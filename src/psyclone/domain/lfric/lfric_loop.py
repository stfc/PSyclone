# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2025, Science and Technology Facilities Council.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# * Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# -----------------------------------------------------------------------------
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified I. Kavcic, A. Coughtrie, L. Turner and O. Brunt, Met Office
# Modified J. Henrichs, Bureau of Meteorology
# Modified A. B. G. Chalk and N. Nobre, STFC Daresbury Lab

''' This module implements the PSyclone LFRic API by specialising the PSyLoop
    base class from psyGen.py.
    '''

from psyclone.configuration import Config
from psyclone.core import AccessType
from psyclone.domain.common.psylayer import PSyLoop
from psyclone.domain.lfric import LFRicConstants, LFRicKern
from psyclone.domain.lfric.lfric_types import LFRicTypes
from psyclone.errors import GenerationError, InternalError
from psyclone.f2pygen import CallGen, CommentGen
from psyclone.psyGen import InvokeSchedule, HaloExchange
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.nodes import (ArrayReference, ACCRegionDirective, DataNode,
                                  Loop, Literal, OMPRegionDirective,
                                  PSyDataNode, Reference, Routine, Schedule)
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE


class LFRicLoop(PSyLoop):
    '''
    The LFRic-specific PSyLoop class. This passes the LFRic-specific
    loop information to the base class so it creates the one
    we require.  Creates LFRic-specific loop bounds when the code is
    being generated.

    :param str loop_type: the type (iteration space) of this loop.
    :param kwargs: additional keyword arguments provided to the PSyIR node.
    :type kwargs: unwrapped dict.

    :raises InternalError: if an unrecognised loop_type is specified.

    '''
    # pylint: disable=too-many-instance-attributes
    def __init__(self, loop_type="", **kwargs):
        const = LFRicConstants()
        super().__init__(valid_loop_types=const.VALID_LOOP_TYPES, **kwargs)
        self.loop_type = loop_type

        # Set our variable at initialisation as it might be required
        # by other classes before code generation. A 'null' loop does not
        # have an associated variable.
        if self.loop_type != "null":

            if self.loop_type == "colours":
                tag = "colours_loop_idx"
                suggested_name = "colour"
            elif self.loop_type == "colour":
                tag = "cell_loop_idx"
                suggested_name = "cell"
            elif self.loop_type == "dof":
                tag = "dof_loop_idx"
                suggested_name = "df"
            elif self.loop_type == "":
                tag = "cell_loop_idx"
                suggested_name = "cell"
            else:
                raise InternalError(
                    f"Unsupported loop type '{self.loop_type}' found when "
                    f"creating loop variable. Supported values are 'colours', "
                    f"'colour', 'dof' or '' (for cell-columns).")

            self.variable = self.scope.symbol_table.find_or_create_tag(
                tag, root_name=suggested_name, symbol_type=DataSymbol,
                datatype=LFRicTypes("LFRicIntegerScalarDataType")())

        # Pre-initialise the Loop children  # TODO: See issue #440
        self.addchild(Literal("NOT_INITIALISED", INTEGER_TYPE,
                              parent=self))  # start
        self.addchild(Literal("NOT_INITIALISED", INTEGER_TYPE,
                              parent=self))  # stop
        self.addchild(Literal("1", INTEGER_TYPE, parent=self))  # step
        self.addchild(Schedule(parent=self))  # loop body

        # At this stage we don't know what our loop bounds are
        self._lower_bound_name = None
        self._lower_bound_index = None
        self._upper_bound_name = None
        self._upper_bound_halo_depth = None

    def lower_to_language_level(self):
        '''In-place replacement of DSL or high-level concepts into generic
        PSyIR constructs. This function replaces a LFRicLoop with a PSyLoop
        and inserts the loop boundaries into the new PSyLoop, or removes
        the loop node in case of a domain kernel. Once TODO #1731 is done
        (which should fix the loop boundaries, which atm rely on index of
        the loop in the schedule, i.e. can change when transformations are
        applied), this function can likely be removed.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        if self._loop_type != "null":

            # This is not a 'domain' loop (i.e. there is a real loop). First
            # check that there isn't any validation issues with the node.
            for child in self.loop_body.children:
                child.validate_global_constraints()

            # Then generate the loop bounds, this needs to be done BEFORE
            # lowering the loop body because it needs kernel information.
            start = self.start_expr.copy()
            stop = self.stop_expr.copy()
            step = self.step_expr.copy()

            # Now we can lower the nodes in the loop body
            for child in self.loop_body.children:
                child.lower_to_language_level()

            # Finally create the new lowered Loop and replace the domain one
            loop = Loop.create(self._variable, start, stop, step, [])
            loop.loop_body._symbol_table = \
                self.loop_body.symbol_table.shallow_copy()
            loop.children[3] = self.loop_body.copy()
            self.replace_with(loop)
            lowered_node = loop
        else:
            # If loop_type is "null" we do not need a loop at all, just the
            # kernel in its loop_body
            for child in self.loop_body.children:
                child.lower_to_language_level()
            # TODO #1010: This restriction can be removed when also lowering
            # the parent InvokeSchedule
            if len(self.loop_body.children) > 1:
                raise NotImplementedError(
                    f"Lowering LFRic domain loops that produce more than one "
                    f"children is not yet supported, but found:\n "
                    f"{self.view()}")
            lowered_node = self.loop_body[0].detach()
            self.replace_with(lowered_node)

        return lowered_node

    def node_str(self, colour=True):
        ''' Creates a text summary of this loop node. We override this
        method from the Loop class because, in LFRic, the function
        space is now an object and we need to call orig_name on it. We
        also include the upper loop bound as this can now be modified.

        :param bool colour: whether or not to include control codes for colour.

        :returns: text summary of this node, optionally with control codes \
                  for colour highlighting.
        :rtype: str

        '''
        if self._loop_type == "null":
            return f"{self.coloured_name(colour)}[type='null']"

        if self._upper_bound_halo_depth:
            upper_bound = (f"{self._upper_bound_name}"
                           f"({self._upper_bound_halo_depth})")
        else:
            upper_bound = self._upper_bound_name
        return (f"{self.coloured_name(colour)}[type='{self._loop_type}', "
                f"field_space='{self._field_space.orig_name}', "
                f"it_space='{self.iteration_space}', "
                f"upper_bound='{upper_bound}']")

    def load(self, kern):
        '''
        Load the state of this Loop using the supplied Kernel
        object. This method is provided so that we can individually
        construct Loop objects for a given kernel call.

        :param kern: Kernel object to use to populate state of Loop
        :type kern: :py:class:`psyclone.domain.lfric.LFRicKern`

        :raises GenerationError: if the field updated by the kernel has an \
            unexpected function space or if the kernel's 'operates-on' is \
            not consistent with the loop type.

        '''
        self._kern = kern

        self._field = kern.arguments.iteration_space_arg()
        self._field_name = self._field.name
        self._field_space = self._field.function_space

        if self.loop_type == "null" and kern.iterates_over != "domain":
            raise GenerationError(
                f"A LFRicLoop of type 'null' can only contain a kernel that "
                f"operates on the 'domain' but kernel '{kern.name}' operates "
                f"on '{kern.iterates_over}'.")
        self._iteration_space = kern.iterates_over  # cell_columns etc.

        # Loop bounds
        self.set_lower_bound("start")
        const = LFRicConstants()
        if kern.iterates_over == "dof":
            # This loop must be over DoFs
            if Config.get().api_conf("lfric").compute_annexed_dofs \
               and Config.get().distributed_memory \
               and not kern.is_reduction:
                self.set_upper_bound("nannexed")
            else:
                self.set_upper_bound("ndofs")
            return

        if "halo" in kern.iterates_over:
            if Config.get().distributed_memory:
                if kern.iterates_over == "halo_cell_column":
                    # In LFRic, the local cell-indexing scheme is set up such
                    # that owned cells have lower indices than halo cells, the
                    # first halo cell starts immediately after the last owned
                    # cell, and the cell indices are contiguous.
                    self.set_lower_bound("cell_halo_start")
                self.set_upper_bound("cell_halo", halo_depth=kern.halo_depth)
                return

        if not Config.get().distributed_memory:
            # Sequential
            self.set_upper_bound("ncells")
            return

        # Otherwise, distributed memory is enabled.
        if self._field.is_operator:
            # We always compute operators redundantly out to the L1
            # halo
            self.set_upper_bound("cell_halo", halo_depth=1)
            return
        if (self.field_space.orig_name in
                const.VALID_DISCONTINUOUS_NAMES):
            # Iterate to ncells for all discontinuous quantities,
            # including any_discontinuous_space
            self.set_upper_bound("ncells")
            return
        if (self.field_space.orig_name in
                const.CONTINUOUS_FUNCTION_SPACES):
            # Must iterate out to L1 halo for continuous quantities
            # unless the only arguments that are updated all have
            # 'GH_WRITE' access. The only time such an access is
            # permitted for a field on a continuous space is when the
            # kernel is implemented such that any writes to a given
            # shared dof are guaranteed to write the same value. There
            # is therefore no need to iterate into the L1 halo in order
            # to get correct values for annexed dofs.
            if not kern.all_updates_are_writes:
                self.set_upper_bound("cell_halo", halo_depth=1)
                return
            self.set_upper_bound("ncells")
            return
        if (self.field_space.orig_name in
                const.VALID_ANY_SPACE_NAMES):
            # We don't know whether any_space is continuous or not
            # so we have to err on the side of caution and assume that
            # it is. Again, if the only arguments that are updated have
            # 'GH_WRITE' access then we can relax this condition.
            if not kern.all_updates_are_writes:
                self.set_upper_bound("cell_halo", halo_depth=1)
                return
            self.set_upper_bound("ncells")
            return

        raise GenerationError(
            f"Unexpected function space found. Expecting one of "
            f"{const.VALID_FUNCTION_SPACES} but found "
            f"'{self.field_space.orig_name}'")

    def set_lower_bound(self, name, index=None):
        ''' Set the lower bounds of this loop '''
        const = LFRicConstants()
        if name not in const.VALID_LOOP_BOUNDS_NAMES:
            raise GenerationError(
                "The specified lower bound loop name is invalid")
        if name in ["inner"] + const.HALO_ACCESS_LOOP_BOUNDS and index < 1:
            raise GenerationError(
                "The specified index '{index}' for this lower loop bound is "
                "invalid")
        self._lower_bound_name = name
        self._lower_bound_index = index

    def set_upper_bound(self, name, halo_depth=None):
        '''Set the upper bound of this loop.

        :param str name: Loop upper-bound name. Must be a supported name.
        :param halo_depth: An optional argument indicating the depth of halo
                           that this loop accesses.
        :type halo_depth: Optional[:py:class:`psyclone.psyir.nodes.DataNode` |
                                   int]

        :raises GenerationError: if supplied with an invalid upper-bound name.
        :raises GenerationError: if supplied with a halo depth < 1.
        :raises TypeError: if the supplied halo_depth value is neither an int
                           or DataNode.
        '''
        const = LFRicConstants()
        if name not in const.VALID_LOOP_BOUNDS_NAMES:
            raise GenerationError(
                f"The specified upper loop bound name is invalid. Expected "
                f"one of {const.VALID_LOOP_BOUNDS_NAMES} but found '{name}'")
        if name == "start":
            raise GenerationError("'start' is not a valid upper bound")
        # Only halo bounds and inner may have an index. We could just
        # test for index here and assume that index is None for other
        # types of bounds, but checking the type of bound as well is a
        # safer option.
        if (name in (["inner"] + const.HALO_ACCESS_LOOP_BOUNDS) and
                isinstance(halo_depth, int)):
            if halo_depth < 1:
                raise GenerationError(
                    f"The specified halo depth '{halo_depth}' for this loop "
                    f"upper bound is < 1 which is invalid.")
        self._upper_bound_name = name
        if halo_depth and isinstance(halo_depth, int):
            # We support specifying depth as an int as a convenience but we
            # now convert it to a PSyIR literal.
            psyir = Literal(f"{halo_depth}", INTEGER_TYPE)
            self._upper_bound_halo_depth = psyir
        else:
            if halo_depth is not None and not isinstance(halo_depth, DataNode):
                raise TypeError(f"When setting the upper bound of a loop, any "
                                f"halo depth must be supplied as an int or "
                                f"PSyIR DataNode but got {type(halo_depth)}")
            self._upper_bound_halo_depth = halo_depth

    @property
    def upper_bound_name(self):
        ''' Returns the name of the upper loop bound '''
        return self._upper_bound_name

    @property
    def upper_bound_halo_depth(self):
        '''Returns the index of the upper loop bound. This is None if the upper
        bound name is not in HALO_ACCESS_LOOP_BOUNDS.

        :returns: the depth of the halo for a loops upper bound. If it \
            is None then a depth has not been provided. The depth value is \
            only valid when the upper-bound name is associated with a halo \
            e.g. 'cell_halo'.
        :rtype: int

        '''
        return self._upper_bound_halo_depth

    def _lower_bound_fortran(self):
        '''
        Create the associated Fortran code for the type of lower bound.

        TODO: Issue #440. lower_bound_fortran should generate PSyIR.

        :returns: the Fortran code for the lower bound.
        :rtype: str

        :raises GenerationError: if self._lower_bound_name is not "start"
                                 for sequential code.
        :raises GenerationError: if self._lower_bound_name is unrecognised.

        '''
        if (not Config.get().distributed_memory and
                self._lower_bound_name != "start"):
            raise GenerationError(
                f"The lower bound must be 'start' if we are sequential but "
                f"found '{self._upper_bound_name}'")
        if self._lower_bound_name == "start":
            return "1"

        # the start of our space is the end of the previous space +1
        if self._lower_bound_name == "inner":
            prev_space_name = self._lower_bound_name
            prev_space_index_str = str(self._lower_bound_index + 1)
        elif self._lower_bound_name == "ncells":
            prev_space_name = "inner"
            prev_space_index_str = "1"
        elif (self._lower_bound_name == "cell_halo" and
              self._lower_bound_index == 1):
            prev_space_name = "ncells"
            prev_space_index_str = ""
        elif self._lower_bound_name == "cell_halo_start":
            prev_space_name = "edge"
            prev_space_index_str = ""
        elif (self._lower_bound_name == "cell_halo" and
              self._lower_bound_index > 1):
            prev_space_name = self._lower_bound_name
            prev_space_index_str = str(self._lower_bound_index - 1)
        else:
            raise GenerationError(
                f"Unsupported lower bound name '{self._lower_bound_name}' "
                f"found")
        # Use InvokeSchedule SymbolTable to share the same symbol for all
        # Loops in the Invoke.
        mesh_obj_name = self.ancestor(InvokeSchedule).symbol_table.\
            find_or_create_tag("mesh").name
        return mesh_obj_name + "%get_last_" + prev_space_name + "_cell(" \
            + prev_space_index_str + ")+1"

    @property
    def _mesh_name(self):
        '''
        :returns: the name of the mesh variable from which to get the bounds \
                  for this loop.
        :rtype: str
        '''
        # We must allow for self._kern being None (as it will be for
        # a built-in).
        if self._kern and self._kern.is_intergrid:
            # We have more than one mesh object to choose from and we
            # want the coarse one because that determines the iteration
            # space. _field_name holds the name of the argument that
            # determines the iteration space of this kernel and that
            # is set-up to be the one on the coarse mesh (in
            # DynKernelArguments.iteration_space_arg()).
            tag_name = "mesh_" + self._field_name
        else:
            # It's not an inter-grid kernel so there's only one mesh
            tag_name = "mesh"

        # The symbol for the mesh will already have been added to the
        # symbol table associated with the InvokeSchedule.
        return self.ancestor(InvokeSchedule).symbol_table.\
            lookup_with_tag(tag_name).name

    def _upper_bound_fortran(self):
        ''' Create the Fortran code that gives the appropriate upper bound
        value for this type of loop.

        TODO: Issue #440. upper_bound_fortran should generate PSyIR.

        :returns: Fortran code for the upper bound of this loop.
        :rtype: str

        '''
        # pylint: disable=too-many-branches, too-many-return-statements
        # precompute halo_index as a string as we use it in more than
        # one of the if clauses
        halo_index = ""
        if self._upper_bound_halo_depth:
            halo_index = FortranWriter()(self._upper_bound_halo_depth)

        if self._upper_bound_name == "ncolours":
            # Loop over colours
            kernels = self.walk(LFRicKern)
            if not kernels:
                raise InternalError(
                    "Failed to find a kernel within a loop over colours.")
            # Check that all kernels have been coloured. We can't check the
            # number of colours since that is only known at runtime.
            for kern in kernels:
                if not kern.ncolours_var:
                    raise InternalError(
                        f"All kernels within a loop over colours must have "
                        f"been coloured but kernel '{kern.name}' has not")
            return kernels[0].ncolours_var

        if self._upper_bound_name == "ncolour":
            # Loop over cells of a particular colour when DM is disabled.
            # We use the same, DM API as that returns sensible values even
            # when running without MPI.
            root_name = "last_edge_cell_all_colours"
            if self._kern.is_intergrid:
                root_name += "_" + self._field_name
            sym = self.ancestor(
                InvokeSchedule).symbol_table.find_or_create_tag(root_name)
            return f"{sym.name}(colour)"
        if self._upper_bound_name == "colour_halo":
            # Loop over cells of a particular colour when DM is enabled. The
            # LFRic API used here allows for colouring with redundant
            # computation.
            sym_tab = self.ancestor(InvokeSchedule).symbol_table
            if halo_index:
                # The colouring API provides a 2D array that holds the last
                # halo cell for a given colour and halo depth.
                depth = halo_index
            else:
                # If no depth is specified then we go to the full halo depth
                depth = sym_tab.find_or_create_tag(
                    f"max_halo_depth_{self._mesh_name}").name
            root_name = "last_halo_cell_all_colours"
            if self._kern.is_intergrid:
                root_name += "_" + self._field_name
            sym = sym_tab.find_or_create_tag(root_name)
            return f"{sym.name}(colour, {depth})"
        if self._upper_bound_name in ["ndofs", "nannexed"]:
            if Config.get().distributed_memory:
                if self._upper_bound_name == "ndofs":
                    result = (
                        f"{self.field.proxy_name_indexed}%"
                        f"{self.field.ref_name()}%get_last_dof_owned()")
                else:  # nannexed
                    result = (
                        f"{self.field.proxy_name_indexed}%"
                        f"{self.field.ref_name()}%get_last_dof_annexed()")
            else:
                result = self._kern.undf_name
            return result
        if self._upper_bound_name == "ncells":
            if Config.get().distributed_memory:
                result = f"{self._mesh_name}%get_last_edge_cell()"
            else:
                result = (f"{self.field.proxy_name_indexed}%"
                          f"{self.field.ref_name()}%get_ncell()")
            return result
        if self._upper_bound_name == "cell_halo":
            if Config.get().distributed_memory:
                return f"{self._mesh_name}%get_last_halo_cell({halo_index})"
            raise GenerationError(
                "'cell_halo' is not a valid loop upper bound for "
                "sequential/shared-memory code")
        if self._upper_bound_name == "dof_halo":
            if Config.get().distributed_memory:
                return (f"{self.field.proxy_name_indexed}%"
                        f"{self.field.ref_name()}%get_last_dof_halo("
                        f"{halo_index})")
            raise GenerationError(
                "'dof_halo' is not a valid loop upper bound for "
                "sequential/shared-memory code")
        if self._upper_bound_name == "inner":
            if Config.get().distributed_memory:
                return f"{self._mesh_name}%get_last_inner_cell({halo_index})"
            raise GenerationError(
                "'inner' is not a valid loop upper bound for "
                "sequential/shared-memory code")
        raise GenerationError(
            f"Unsupported upper bound name '{self._upper_bound_name}' found "
            f"in lfricloop.upper_bound_fortran()")

    def _halo_read_access(self, arg):
        '''
        Determines whether the supplied argument has (or might have) its
        halo data read within this loop. Returns True if it does, or if
        it might and False if it definitely does not.

        :param arg: an argument contained within this loop.
        :type arg: :py:class:`psyclone.dynamo0p3.DynArgument`

        :returns: True if the argument reads, or might read from the \
            halo and False otherwise.
        :rtype: bool

        :raises GenerationError: if an unsupported upper loop bound name is \
            provided for kernels with stencil access.
        :raises InternalError: if an unsupported field access is found.
        :raises InternalError: if an unsupported argument type is found.

        '''
        const = LFRicConstants()
        if arg.is_scalar or arg.is_operator:
            # Scalars and operators do not have halos
            return False
        if arg.is_field:
            # This is a field so might read from a halo
            if arg.access in [AccessType.WRITE]:
                # This is not a read access
                return False
            if arg.access in AccessType.all_read_accesses():
                # This is a read access
                if arg.descriptor.stencil:
                    if self._upper_bound_name not in ["cell_halo", "ncells"]:
                        raise GenerationError(
                            f"Loop bounds other than 'cell_halo' and 'ncells' "
                            f"are currently unsupported for kernels with "
                            f"stencil accesses. Found "
                            f"'{self._upper_bound_name}'.")
                    # An upper bound of 'cell_halo' means that the
                    # halo might be accessed irrespective of the
                    # stencil and a stencil read access with upper
                    # bound 'ncells' might read from the
                    # halo due to the stencil.
                    return True
                # This is a non-stencil read access
                if self._upper_bound_name in const.HALO_ACCESS_LOOP_BOUNDS:
                    # An upper bound that is part of the halo means
                    # that the halo might be accessed.
                    return True
                if (not arg.discontinuous and
                        self.kernel.iterates_over == "cell_column" and
                        self.kernel.all_updates_are_writes and
                        self._upper_bound_name == "ncells"):
                    # This is the special case of a kernel that guarantees to
                    # write the same value to any given dof, irrespective of
                    # cell column.
                    return False
                if not arg.discontinuous and \
                   self._upper_bound_name in ["ncells", "nannexed"]:
                    # Annexed dofs may be accessed. Return False if we
                    # always compute annexed dofs and True if we don't
                    # (as annexed dofs are part of the level 1 halo).
                    return not Config.get().api_conf("lfric").\
                        compute_annexed_dofs
                # The halo is not accessed.
                return False
            raise InternalError(
                f"Unexpected field access type '{arg.access}' found for arg "
                f"'{arg.name}'.")
        raise InternalError(
            f"Expecting arg '{arg.name}' to be an operator, scalar or field, "
            f"but found '{arg.argument_type}'.")

    def _add_field_component_halo_exchange(self, halo_field, idx=None):
        '''An internal helper method to add the halo exchange call immediately
        before this loop using the halo_field argument for the
        associated field information and the optional idx argument if
        the field is a vector field.

        In certain situations the halo exchange will not be
        required. This is dealt with by adding the halo exchange,
        asking it if it is required and then removing it if it is
        not. This may seem strange but the logic for determining
        whether a halo exchange is required is within the halo
        exchange class so it is simplest to do it this way

        :param halo_field: the argument requiring a halo exchange
        :type halo_field: :py:class:`psyclone.dynamo0p3.DynArgument`
        :param index: optional argument providing the vector index.
        :type index: Optional[int]

        :raises InternalError: if there are two forward write
            dependencies and they are both associated with halo
            exchanges.

        '''
        # Avoid circular import
        # pylint: disable=import-outside-toplevel
        from psyclone.dynamo0p3 import LFRicHaloExchange
        exchange = LFRicHaloExchange(halo_field,
                                     parent=self.parent,
                                     vector_index=idx)
        self.parent.children.insert(self.position,
                                    exchange)

        # Is this halo exchange required? The halo exchange being
        # added may replace an existing halo exchange, which would
        # then be returned as a halo exchange dependence and an
        # exception raised (as a halo exchange should not have another
        # halo exchange as a dependence). Therefore, halo exchange
        # dependencies are ignored here by setting the ignore_hex_dep
        # optional argument.
        required, _ = exchange.required(ignore_hex_dep=True)
        if not required:
            exchange.detach()
        else:
            # The halo exchange we have added may be replacing an
            # existing one. If so, the one being replaced will be the
            # first and only write dependence encountered and must be
            # removed.
            results = exchange.field.forward_write_dependencies()
            if results:
                first_dep_call = results[0].call
                if isinstance(first_dep_call, HaloExchange):
                    # Sanity check. If the first dependence is a field
                    # accessed within a halo exchange then the
                    # subsequent one must not be.
                    next_results = results[0].forward_write_dependencies()
                    if next_results and any(tmp for tmp in next_results
                                            if isinstance(tmp.call,
                                                          HaloExchange)):
                        raise InternalError(
                            f"When replacing a halo exchange with another one "
                            f"for field {exchange.field.name}, a subsequent "
                            f"dependent halo exchange was found. This should "
                            f"never happen.")
                    first_dep_call.detach()

    def _add_halo_exchange(self, halo_field):
        '''Internal helper method to add (a) halo exchange call(s) immediately
        before this loop using the halo_field argument for the
        associated field information. If the field is a vector then
        add the appropriate number of halo exchange calls.

        :param halo_field: the argument requiring a halo exchange
        :type halo_field: :py:class:`psyclone.dynamo0p3.DynArgument`

        '''
        if halo_field.vector_size > 1:
            # the range function below returns values from
            # 1 to the vector size which is what we
            # require in our Fortran code
            for idx in range(1, halo_field.vector_size+1):
                self._add_field_component_halo_exchange(halo_field, idx)
        else:
            self._add_field_component_halo_exchange(halo_field)

    def update_halo_exchanges(self):
        '''add and/or remove halo exchanges due to changes in the loops
        bounds'''
        # this call adds any new halo exchanges that are
        # required. This is done by adding halo exchanges before this
        # loop for any fields in the loop that require a halo exchange
        # and don't already have one
        self.create_halo_exchanges()
        # Now remove any existing halo exchanges that are no longer
        # required. This is done by removing halo exchanges after this
        # loop where a field in this loop previously had a forward
        # dependence on a halo exchange but no longer does
        # pylint: disable=too-many-nested-blocks
        # Avoid circular import
        # pylint: disable=import-outside-toplevel
        from psyclone.dynamo0p3 import LFRicHaloExchange
        for call in self.kernels():
            for arg in call.arguments.args:
                if arg.access in AccessType.all_write_accesses():
                    dep_arg_list = arg.forward_read_dependencies()
                    for dep_arg in dep_arg_list:
                        if isinstance(dep_arg.call, LFRicHaloExchange):
                            # found a halo exchange as a forward dependence
                            # ask the halo exchange if it is required
                            halo_exchange = dep_arg.call
                            required, _ = halo_exchange.required()
                            if not required:
                                halo_exchange.detach()

    def create_halo_exchanges(self):
        '''Add halo exchanges before this loop as required by fields within
        this loop. To keep the logic simple we assume that any field
        that accesses the halo will require a halo exchange and then
        remove the halo exchange if this is not the case (when
        previous writers perform sufficient redundant computation). It
        is implemented this way as the halo exchange class determines
        whether it is required or not so a halo exchange needs to
        exist in order to find out. The appropriate logic is coded in
        the _add_halo_exchange helper method. In some cases a new halo
        exchange will replace an existing one. In this situation that
        routine also removes the old one.

        '''
        for halo_field in self.unique_fields_with_halo_reads():
            # for each unique field in this loop that has its halo
            # read (including annexed dofs), find the previous update
            # of this field
            prev_arg_list = halo_field.backward_write_dependencies()
            if not prev_arg_list:
                # field has no previous dependence so create new halo
                # exchange(s) as we don't know the state of the fields
                # halo on entry to the invoke
                self._add_halo_exchange(halo_field)
            else:
                # field has one or more previous dependencies
                if len(prev_arg_list) > 1:
                    # field has more than one previous dependencies so
                    # should be a vector
                    if halo_field.vector_size <= 1:
                        raise GenerationError(
                            f"Error in create_halo_exchanges. Expecting field "
                            f"'{halo_field.name}' to be a vector as it has "
                            f"multiple previous dependencies")
                    if len(prev_arg_list) != halo_field.vector_size:
                        raise GenerationError(
                            f"Error in create_halo_exchanges. Expecting a "
                            f"dependence for each vector index for field "
                            f"'{halo_field.name}' but the number of "
                            f"dependencies is '{halo_field.vector_size}' and "
                            f"the vector size is '{len(prev_arg_list)}'.")
                    for arg in prev_arg_list:
                        # Avoid circular import
                        # pylint: disable=import-outside-toplevel
                        from psyclone.dynamo0p3 import LFRicHaloExchange
                        if not isinstance(arg.call, LFRicHaloExchange):
                            raise GenerationError(
                                "Error in create_halo_exchanges. Expecting "
                                "all dependent nodes to be halo exchanges")
                prev_node = prev_arg_list[0].call
                # Avoid circular import
                # pylint: disable=import-outside-toplevel
                from psyclone.dynamo0p3 import LFRicHaloExchange
                if not isinstance(prev_node, LFRicHaloExchange):
                    # previous dependence is not a halo exchange so
                    # call the add halo exchange logic which
                    # determines whether a halo exchange is required
                    # or not
                    self._add_halo_exchange(halo_field)

    @property
    def start_expr(self):
        '''
        :returns: the PSyIR for the lower bound of this loop.
        :rtype: :py:class:`psyclone.psyir.Node`

        '''
        inv_sched = self.ancestor(Routine)
        sym_table = inv_sched.symbol_table
        loops = inv_sched.loops()
        posn = None
        for index, loop in enumerate(loops):
            if loop is self:
                posn = index
                break
        root_name = f"loop{posn}_start"
        lbound = sym_table.find_or_create_integer_symbol(root_name,
                                                         tag=root_name)
        self.children[0] = Reference(lbound)
        return self.children[0]

    @property
    def stop_expr(self):
        '''
        :returns: the PSyIR for the upper bound of this loop.
        :rtype: :py:class:`psyclone.psyir.Node`

        '''
        inv_sched = self.ancestor(Routine)
        sym_table = inv_sched.symbol_table

        if self._loop_type == "colour":
            # If this loop is over all cells of a given colour then we must
            # lookup the loop bound as it depends on the current colour.
            parent_loop = self.ancestor(Loop)
            colour_var = parent_loop.variable

            asym = self.kernel.last_cell_all_colours_symbol
            const = LFRicConstants()

            if self.upper_bound_name in const.HALO_ACCESS_LOOP_BOUNDS:
                if self._upper_bound_halo_depth:
                    halo_depth = self._upper_bound_halo_depth.copy()
                else:
                    # We need to go to the full depth of the halo.
                    root_name = "mesh"
                    if self.kernels()[0].is_intergrid:
                        root_name += f"_{self._field_name}"
                    depth_sym = sym_table.lookup_with_tag(
                        f"max_halo_depth_{root_name}")
                    halo_depth = Reference(depth_sym)

                return ArrayReference.create(asym, [Reference(colour_var),
                                                    halo_depth])
            return ArrayReference.create(asym, [Reference(colour_var)])

        # This isn't a 'colour' loop so we have already set-up a
        # variable that holds the upper bound.
        loops = inv_sched.loops()
        posn = None
        for index, loop in enumerate(loops):
            if loop is self:
                posn = index
                break
        root_name = f"loop{posn}_stop"
        ubound = sym_table.find_or_create_integer_symbol(root_name,
                                                         tag=root_name)
        self.children[1] = Reference(ubound)
        return self.children[1]

    def gen_code(self, parent):
        ''' Call the base class to generate the code and then add any
        required halo exchanges.

        :param parent: an f2pygen object that will be the parent of \
            f2pygen objects created in this method.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`


        '''
        # pylint: disable=too-many-statements, too-many-branches
        if (not Config.get().distributed_memory and
            all(kern.iterates_over == "halo_cell_column" for
                kern in self.kernels())):
            # No distributed memory and thus no halo cells but all kernels
            # only operate on halo cells => nothing to do.
            return

        # Check that we're not within an OpenMP parallel region if
        # we are a loop over colours.
        if self._loop_type == "colours" and self.is_openmp_parallel():
            raise GenerationError("Cannot have a loop over colours within an "
                                  "OpenMP parallel region.")

        super().gen_code(parent)

        for psydata_node in self.walk(PSyDataNode):
            psydata_node.fix_gen_code(parent)

        # TODO #1010: gen_code of this loop calls the PSyIR lowering version,
        # but that method can not currently provide sibling nodes because the
        # ancestor is not PSyIR, so for now we leave the remainder of the
        # gen_code logic here instead of removing the whole method.

        if not (Config.get().distributed_memory and
                self._loop_type != "colour"):
            # No need to add halo exchanges so we are done.
            return

        # Set halo clean/dirty for all fields that are modified
        if not self.unique_modified_args("gh_field"):
            return

        if self.ancestor((ACCRegionDirective, OMPRegionDirective)):
            # We cannot include calls to set halos dirty/clean within OpenACC
            # or OpenMP regions. This is handled by the appropriate Directive
            # class instead.
            # TODO #1755 can this check be made more general (e.g. to include
            # Extraction regions)?
            return

        parent.add(CommentGen(parent, ""))
        if self._loop_type != "null":
            prev_node_name = "loop"
        else:
            prev_node_name = "kernel"
        parent.add(CommentGen(parent, f" Set halos dirty/clean for fields "
                              f"modified in the above {prev_node_name}"))
        parent.add(CommentGen(parent, ""))

        self.gen_mark_halos_clean_dirty(parent)

        parent.add(CommentGen(parent, ""))

    def gen_mark_halos_clean_dirty(self, parent):
        '''
        Generates the necessary code to mark halo regions for all modified
        fields as clean or dirty following execution of this loop.

        :param parent: the node in the f2pygen AST to which to add content.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        '''
        # Set halo clean/dirty for all fields that are modified
        fields = self.unique_modified_args("gh_field")

        # First set all of the halo dirty unless we are
        # subsequently going to set all of the halo clean
        for field in fields:
            # Avoid circular import
            # pylint: disable=import-outside-toplevel
            from psyclone.dynamo0p3 import HaloWriteAccess
            # The HaloWriteAccess class provides information about how the
            # supplied field is accessed within its parent loop
            hwa = HaloWriteAccess(field, self)
            if not hwa.max_depth or hwa.dirty_outer:
                # output set dirty as some of the halo will not be set to clean
                if field.vector_size > 1:
                    # the range function below returns values from 1 to the
                    # vector size which is what we require in our Fortran code
                    for index in range(1, field.vector_size+1):
                        parent.add(CallGen(parent, name=field.proxy_name +
                                           f"({index})%set_dirty()"))
                else:
                    parent.add(CallGen(parent, name=field.proxy_name +
                                       "%set_dirty()"))
            # Now set appropriate parts of the halo clean where redundant
            # computation has been performed or a kernel is written to operate
            # on halo cells.
            clean_depth = hwa.clean_depth
            if clean_depth:
                depth_str = FortranWriter()(clean_depth)
                if field.vector_size > 1:
                    # The range function below returns values from 1 to the
                    # vector size, as required in our Fortran code.
                    for index in range(1, field.vector_size+1):
                        parent.add(CallGen(
                            parent, name=f"{field.proxy_name}({index})%"
                            f"set_clean({depth_str})"))
                else:
                    parent.add(CallGen(
                        parent, name=f"{field.proxy_name}%set_clean("
                        f"{depth_str})"))

    def independent_iterations(self,
                               test_all_variables=False,
                               signatures_to_ignore=None,
                               dep_tools=None):
        '''
        This function is an LFRic-specific override of the default method
        in the Loop class. It allows domain-specific rules to be applied when
        determining whether or not loop iterations are independent.

        :param bool test_all_variables: if True, it will test if all variable
            accesses are independent, otherwise it will stop after the first
            variable access is found that isn't.
        :param signatures_to_ignore: list of signatures for which to skip
            the access checks.
        :type signatures_to_ignore: Optional[
            List[:py:class:`psyclone.core.Signature`]]
        :param dep_tools: an optional instance of DependencyTools so that the
            caller can access any diagnostic messages detailing why the loop
            iterations are not independent.
        :type dep_tools: Optional[
            :py:class:`psyclone.psyir.tools.DependencyTools`]

        :returns: True if the loop iterations are independent, False otherwise.
        :rtype: bool

        '''
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.tools import DependencyTools, DTCode
        if not dep_tools:
            dtools = DependencyTools()
        else:
            dtools = dep_tools

        if self.loop_type in ["null", "colours"]:
            # We know we can't parallelise these loops. ("null" means there
            # is no actual loop and "colours" is the *outer* loop over the
            # different colours used - it is the inner, "colour" loop over
            # cells of a single colour which can be parallelised.)
            return False

        try:
            stat = dtools.can_loop_be_parallelised(
                self, test_all_variables=test_all_variables,
                signatures_to_ignore=signatures_to_ignore)
            if stat:
                return True
        except (InternalError, KeyError):
            # LFRic still has symbols that don't exist in the symbol_table
            # until the gen_code() step, so the dependency analysis raises
            # errors in some cases.
            # TODO #1648 - when a transformation colours a loop we must
            # ensure "last_[halo]_cell_all_colours" is added to the symbol
            # table.
            return True

        # The generic DA says that this loop cannot be parallelised. However,
        # we use domain-specific information to qualify this.
        if self.loop_type == "colour":
            # This loop is either over cells of a single colour.
            # According to LFRic rules this is safe to parallelise.
            return True

        if self.loop_type == "dof":
            # The generic DA can't see the PSyIR of this Builtin (because it
            # hasn't been lowered to language level) so we use
            # domain-specific knowledge about its properties.
            if self.kernel.is_reduction:
                dtools._add_message(
                    f"Builtin '{self.kernel.name}' performs a reduction",
                    DTCode.WARN_SCALAR_REDUCTION)
                return False
            return True

        if self.loop_type == "":
            # We can parallelise a non-coloured loop if it only updates
            # quantities on discontinuous function spaces. If an LFRic kernel
            # updates quantities on a continuous function space then it must
            # have at least one argument with GH_INC access. Therefore, we
            # can simply check whether or not it has such an argument in order
            # to infer the continuity of the space.
            if self.has_inc_arg():
                dtools._add_message(
                    f"Kernel '{self.kernel.name}' performs an INC update",
                    DTCode.ERROR_WRITE_WRITE_RACE)
                return False
            return True

        raise InternalError(f"independent_iterations: loop of type "
                            f"'{self.loop_type}' is not supported.")


# ---------- Documentation utils -------------------------------------------- #
# The list of module members that we wish AutoAPI to generate
# documentation for. (See https://psyclone-ref.readthedocs.io)
__all__ = ['LFRicLoop']
