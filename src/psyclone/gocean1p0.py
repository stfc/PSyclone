# pylint: disable=too-many-lines
# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2024, Science and Technology Facilities Council.
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
# Modified J. Henrichs, Bureau of Meteorology
# Modified I. Kavcic, Met Office


'''This module implements the PSyclone GOcean 1.0 API by specialising
    the required base classes for both code generation (PSy, Invokes,
    Invoke, InvokeSchedule, Loop, Kern, Arguments and KernelArgument)
    and parsing (Descriptor and KernelType). It adds a
    GOKernelGridArgument class to capture information on kernel arguments
    that supply properties of the grid (and are generated in the PSy
    layer).

'''

import re

from fparser.two.Fortran2003 import NoMatchError, Nonlabel_Do_Stmt
from fparser.two.parser import ParserFactory

from psyclone.configuration import Config, ConfigurationError
from psyclone.core import Signature
from psyclone.domain.common.psylayer import PSyLoop
from psyclone.domain.gocean import GOceanConstants, GOSymbolTable
from psyclone.errors import GenerationError, InternalError
import psyclone.expression as expr
from psyclone.f2pygen import (
    DeclGen, UseGen, ModuleGen, SubroutineGen, TypeDeclGen, PSyIRGen)
from psyclone.parse.algorithm import Arg
from psyclone.parse.kernel import Descriptor, KernelType
from psyclone.parse.utils import ParseError
from psyclone.psyGen import (
    PSy, Invokes, Invoke, InvokeSchedule, CodedKern, Arguments, Argument,
    KernelArgument, args_filter, AccessType, HaloExchange)
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import (
    Literal, Schedule, KernelSchedule, StructureReference, IntrinsicCall,
    Reference, Call, Assignment, ACCEnterDataDirective, ACCParallelDirective,
    ACCKernelsDirective, Container, ACCUpdateDirective, Routine,
    BinaryOperation)
from psyclone.psyir.symbols import (
    ScalarType, INTEGER_TYPE, DataSymbol, RoutineSymbol, ContainerSymbol,
    UnresolvedType, DataTypeSymbol, UnresolvedInterface, BOOLEAN_TYPE,
    REAL_TYPE)
from psyclone.psyir.tools import DependencyTools


class GOPSy(PSy):
    '''
    The GOcean 1.0 specific PSy class. This creates a GOcean specific
    invokes object (which controls all the required invocation calls).
    Also overrides the PSy gen method so that we generate GOcean-
    specific PSy module code.

    :param invoke_info: An object containing the required invocation \
                        information for code optimisation and generation.
    :type invoke_info: :py:class:`psyclone.parse.FileInfo`

    '''
    def __init__(self, invoke_info):
        Config.get().api = "gocean"
        PSy.__init__(self, invoke_info)

        # Add GOcean infrastructure-specific libraries
        field_sym = ContainerSymbol("field_mod")
        field_sym.wildcard_import = True
        self.container.symbol_table.add(field_sym)
        kind_params_sym = ContainerSymbol("kind_params_mod")
        kind_params_sym.wildcard_import = True
        self.container.symbol_table.add(kind_params_sym)

        # Create invokes
        self._invokes = GOInvokes(invoke_info.calls, self)

    @property
    def gen(self):
        '''
        Generate PSy code for the GOcean api v.1.0.

        :rtype: ast

        '''
        # create an empty PSy layer module
        psy_module = ModuleGen(self.name)
        # include the kind_params module
        psy_module.add(UseGen(psy_module, name="kind_params_mod"))
        # include the field_mod module
        psy_module.add(UseGen(psy_module, name="field_mod"))
        self.invokes.gen_code(psy_module)
        return psy_module.root


class GOInvokes(Invokes):
    '''
    The GOcean specific invokes class. This passes the GOcean specific
    invoke class to the base class so it creates the one we require.

    :param alg_calls: The Invoke calls discovered in the Algorithm layer.
    :type alg_calls: OrderedDict of :py:class:`psyclone.parse.InvokeCall` \
        objects.
    :param psy: the PSy object containing this GOInvokes object.
    :type psy: :py:class:`psyclone.gocean1p0.GOPSy`

    '''
    def __init__(self, alg_calls, psy):
        self._0_to_n = GOInvoke(None, None, None)  # for pyreverse
        Invokes.__init__(self, alg_calls, GOInvoke, psy)

        index_offsets = []
        # Loop over all of the kernels in all of the invoke() calls
        # and check that they work on compatible grid-index offsets.
        # Strictly speaking this check should be done in the parsing
        # code since it is a check on the correctness of the meta-data.
        # However, that would require a fundamental change to the parsing
        # code since it requires information  on all of the invokes and
        # kernels in an application. Therefore it is much simpler to
        # do it here where we have easy access to that information.
        for invoke in self.invoke_list:
            for kern_call in invoke.schedule.coded_kernels():
                # We only care if the index offset is not offset_any (since
                # that is compatible with any other offset)
                if kern_call.index_offset != "go_offset_any":
                    # Loop over the offsets we've seen so far
                    for offset in index_offsets:
                        if offset != kern_call.index_offset:
                            raise GenerationError(
                                f"Meta-data error in kernel {kern_call.name}: "
                                f"INDEX_OFFSET of '{kern_call.index_offset}' "
                                f"does not match that ({offset}) of other "
                                f"kernels. This is not supported.")
                    # Append the index-offset of this kernel to the list of
                    # those seen so far
                    index_offsets.append(kern_call.index_offset)

    def gen_code(self, parent):
        '''
        GOcean redefines the Invokes.gen_code() to start using the PSyIR
        backend when possible. In cases where the backend can not be used yet
        (e.g. OpenCL and PSyDataNodes) the parent class will be called. This
        is a temporary workaround to avoid modifying the generator file while
        other APIs still use the f2pygen module for code generation.
        Once the PSyIR backend has generated an output, this is added into a
        f2pygen PSyIRGen block in the f2pygen AST for each Invoke in the
        PSy layer.

        :param parent: the parent node in the f2pygen AST to which to add \
                       content.
        :type parent: `psyclone.f2pygen.ModuleGen`
        '''
        if self.invoke_list:
            # We just need one invoke as they all have a common root.
            invoke = self.invoke_list[0]

            # Lower the GOcean PSyIR to language level so it can be visited
            # by the backends
            invoke.schedule.root.lower_to_language_level()
            # Then insert it into a f2pygen AST as a PSyIRGen node.
            # Note that other routines besides the Invoke could have been
            # inserted during the lowering (e.g. module-inlined kernels),
            # so have to iterate over all current children of root.
            for child in invoke.schedule.root.children:
                parent.add(PSyIRGen(parent, child))


class GOInvoke(Invoke):
    '''
    The GOcean specific invoke class. This passes the GOcean specific
    schedule class to the base class so it creates the one we require.
    A set of GOcean infrastructure reserved names are also passed to
    ensure that there are no name clashes. Also overrides the gen_code
    method so that we generate GOcean specific invocation code and
    provides three methods which separate arguments that are arrays from
    arguments that are {integer, real} scalars.

    :param alg_invocation: Node in the AST describing the invoke call.
    :type alg_invocation: :py:class:`psyclone.parse.InvokeCall`
    :param int idx: The position of the invoke in the list of invokes \
        contained in the Algorithm.
    :param invokes: the Invokes object containing this GOInvoke \
        object.
    :type invokes: :py:class:`psyclone.gocean1p0.GOInvokes`

    '''
    def __init__(self, alg_invocation, idx, invokes):
        self._schedule = GOInvokeSchedule('name', None)  # for pyreverse
        Invoke.__init__(self, alg_invocation, idx, GOInvokeSchedule, invokes)

        if Config.get().distributed_memory:
            # Insert halo exchange calls
            for loop in self.schedule.loops():
                loop.create_halo_exchanges()

    @property
    def unique_args_arrays(self):
        ''' find unique arguments that are arrays (defined as those that are
            field objects as opposed to scalars or properties of the grid). '''
        result = []
        for call in self._schedule.kernels():
            for arg in call.arguments.args:
                if arg.argument_type == 'field' and arg.name not in result:
                    result.append(arg.name)
        return result

    @property
    def unique_args_iscalars(self):
        '''
        :returns: the unique arguments that are scalars of type integer \
                  (defined as those that are i_scalar 'space').
        :rtype: list of str.

        '''
        result = []
        for call in self._schedule.kernels():
            for arg in args_filter(call.arguments.args, arg_types=["scalar"],
                                   include_literals=False):
                if arg.space.lower() == "go_i_scalar" and \
                   arg.name not in result:
                    result.append(arg.name)
        return result

    def gen_code(self, parent):
        # pylint: disable=too-many-locals
        '''
        Generates GOcean specific invocation code (the subroutine called
        by the associated invoke call in the algorithm layer). This
        consists of the PSy invocation subroutine and the declaration of
        its arguments.

        :param parent: the node in the generated AST to which to add content.
        :type parent: :py:class:`psyclone.f2pygen.ModuleGen`

        '''
        # TODO 1010: GOcean doesn't use this method anymore and it can be
        # deleted, but some tests still call it directly.

        # Create the subroutine
        invoke_sub = SubroutineGen(parent, name=self.name,
                                   args=self.psy_unique_var_names)
        parent.add(invoke_sub)

        # Generate the code body of this subroutine
        self.schedule.gen_code(invoke_sub)

        # Add the subroutine argument declarations for fields
        if self.unique_args_arrays:
            my_decl_arrays = TypeDeclGen(invoke_sub, datatype="r2d_field",
                                         intent="inout",
                                         entity_decls=self.unique_args_arrays)
            invoke_sub.add(my_decl_arrays)

        # Add the subroutine argument declarations for integer and real scalars
        i_args = []
        for argument in self.schedule.symbol_table.argument_datasymbols:
            if argument.name in self.unique_args_iscalars:
                i_args.append(argument.name)

        if i_args:
            my_decl_iscalars = DeclGen(invoke_sub, datatype="INTEGER",
                                       intent="inout",
                                       entity_decls=i_args)
            invoke_sub.add(my_decl_iscalars)

        # Add remaining local scalar symbols using the symbol table
        for symbol in self.schedule.symbol_table.automatic_datasymbols:
            if isinstance(symbol.datatype, ScalarType):
                invoke_sub.add(DeclGen(
                    invoke_sub,
                    datatype=symbol.datatype.intrinsic.name,
                    entity_decls=[symbol.name]))


class GOInvokeSchedule(InvokeSchedule):
    ''' The GOcean specific InvokeSchedule sub-class. We call the base class
    constructor and pass it factories to create GO-specific calls to both
    user-supplied kernels and built-ins.

    :param str name: name of the Invoke.
    :param alg_calls: list of KernelCalls parsed from the algorithm layer.
    :type alg_calls: list of :py:class:`psyclone.parse.algorithm.KernelCall`
    :param reserved_names: optional list of names that are not allowed in the \
                           new InvokeSchedule SymbolTable.
    :type reserved_names: list of str
    :param parent: the parent of this node in the PSyIR.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`

    '''
    # Textual description of the node.
    _text_name = "GOInvokeSchedule"

    def __init__(self, name, alg_calls, reserved_names=None, parent=None):
        InvokeSchedule.__init__(self, name, GOKernCallFactory,
                                GOBuiltInCallFactory,
                                alg_calls, reserved_names, parent=parent)


# pylint: disable=too-many-instance-attributes
class GOLoop(PSyLoop):
    ''' The GOcean specific PSyLoop class. This passes the GOcean specific
        single loop information to the base class so it creates the one we
        require. Adds a GOcean specific setBounds method which tells the loop
        what to iterate over. Need to harmonise with the topology_name method
        in the Dynamo api.

        :param parent: optional parent node (default None).
        :type parent: :py:class:`psyclone.psyir.nodes.Node`
        :param str loop_type: loop type - must be 'inner' or 'outer'.
        :param str field_name: name of the field this loop iterates on.
        :param str field_space: space of the field this loop iterates on.
        :param str iteration_space: iteration space of the loop.

        :raises GenerationError: if the loop is not inserted inside a \
            GOInvokeSchedule region.

    '''
    _bounds_lookup = {}

    def __init__(self, parent, loop_type="", field_name="", field_space="",
                 iteration_space="", index_offset=""):
        # pylint: disable=too-many-arguments
        const = GOceanConstants()

        super().__init__(parent=parent,
                         valid_loop_types=const.VALID_LOOP_TYPES)

        # The following attributes are validated in the respective setters
        self.loop_type = loop_type
        self.field_name = field_name
        self.field_space = field_space
        self.iteration_space = iteration_space
        self.index_offset = index_offset

        # Check that the GOLoop is inside the GOcean PSy-layer
        if not self.ancestor(GOInvokeSchedule):
            raise GenerationError(
                "GOLoops must always be constructed with a parent which is"
                " inside (directly or indirectly) of a GOInvokeSchedule")

        # We set the loop variable name in the constructor so that it is
        # available when we're determining which vars should be OpenMP
        # PRIVATE (which is done *before* code generation is performed)
        if self.loop_type == "inner":
            tag = "contiguous_kidx"
            suggested_name = "i"
        elif self.loop_type == "outer":
            tag = "noncontiguous_kidx"
            suggested_name = "j"
        else:
            raise InternalError(f"While the loop type '{self._loop_type}' is "
                                f"valid, it is not yet supported.")

        # In the GOcean API the loop iteration variables are declared in the
        # Invoke routine scope in order to share them between all GOLoops.
        # This is important because some transformations/scripts work with
        # this assumption when moving or fusing loops.
        symtab = self.ancestor(InvokeSchedule).symbol_table
        try:
            self.variable = symtab.lookup_with_tag(tag)
        except KeyError:
            self.variable = symtab.new_symbol(
                suggested_name, tag, symbol_type=DataSymbol,
                datatype=INTEGER_TYPE)

        # Initialise bounds lookup map if it is not already
        if not GOLoop._bounds_lookup:
            GOLoop.setup_bounds()

    @staticmethod
    def create(parent, loop_type, field_name="", field_space="",
               iteration_space="", index_offset=""):
        # pylint: disable=too-many-arguments,arguments-renamed
        '''
        Create a new instance of a GOLoop with the expected children to
        represent the bounds given by the loop properties.

        :param parent: parent node of this GOLoop.
        :type parent: :py:class:`psyclone.psyir.nodes.Node`
        :param str loop_type: loop type - must be 'inner' or 'outer'.
        :param str field_name: name of the field this loop iterates on.
        :param str field_space: space of the field this loop iterates on.
        :param str iteration_space: iteration space of the loop.
        :param str index_offset: the grid index offset used by the kernel(s) \
            within this loop.

        :returns: a new GOLoop node (with appropriate child nodes).
        :rtype: :py:class:`psyclone.gocean1p0.GOLoop`
        '''

        # Create loop node
        node = GOLoop(parent, loop_type, field_name, field_space,
                      iteration_space, index_offset)

        # Add start, stop, step and body and Loop children
        node.addchild(node.lower_bound())
        node.addchild(node.upper_bound())
        node.addchild(Literal("1", INTEGER_TYPE))
        node.addchild(Schedule())

        return node

    @property
    def field_space(self):
        '''
        :returns: the loop's field space (e.g. CU, CV...).
        :rtype: str
        '''
        return self._field_space

    @field_space.setter
    def field_space(self, my_field_space):
        ''' Sets new value for the field_space and updates the Loop bounds,
        if these exist, to match the given field_space.

        :param str my_field_space: new field_space value.

        :raises TypeError: if the provided field_space is not a string.
        :raises ValueError: if the provided field_space is not a valid GOcean \
                            field_space.

        '''
        # TODO 1393: This could call the super setter if the validations are
        # generic
        if not isinstance(my_field_space, str):
            raise TypeError(
                f"Field space must be a 'str' but found "
                f"'{type(my_field_space).__name__}' instead.")
        valid_fs = GOceanConstants().VALID_FIELD_GRID_TYPES + ['']
        if my_field_space not in valid_fs:
            raise ValueError(
                f"Invalid string '{my_field_space}' provided for a GOcean "
                f"field_space. The valid values are {valid_fs}")

        self._field_space = my_field_space
        if len(self.children) > 1:
            self.start_expr.replace_with(self.lower_bound())
        if len(self.children) > 2:
            self.stop_expr.replace_with(self.upper_bound())

    @property
    def iteration_space(self):
        '''
        :returns: the loop's iteration space (e.g. 'go_internal_pts', \
                  'go_all_pts', ...).
        :rtype: str
        '''
        return self._iteration_space

    @iteration_space.setter
    def iteration_space(self, it_space):
        ''' Sets new value for the iteration_space and updates the Loop bounds,
        if these exist, to match the given iteration_space.

        :param str it_space: new iteration_space value.

        :raises TypeError: if the provided it_space is not a string.

        '''
        if not isinstance(it_space, str):
            raise TypeError(
                f"Iteration space must be a 'str' but found "
                f"'{type(it_space).__name__}' instead.")

        # TODO 1393: We could validate also the value, but at the moment there
        # are some ambiguities to resolve.

        self._iteration_space = it_space
        if len(self.children) > 1:
            self.start_expr.replace_with(self.lower_bound())
        if len(self.children) > 2:
            self.stop_expr.replace_with(self.upper_bound())

    @property
    def bounds_lookup(self):
        '''
        :returns: the GOcean loop bounds lookup table. This is a \
                  5-dimensional dictionary with index-offset, field-space, \
                  iteration-space, loop-type, and boundary-side lookup keys \
                  which provides information about how to construct the \
                  loop boundaries for a kernel with such parameters.
        :rtype: dict
        '''
        return self._bounds_lookup

    def independent_iterations(self,
                               test_all_variables=False,
                               signatures_to_ignore=None,
                               dep_tools=None):
        '''
        This function is a GOcean-specific override of the default method
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
        if not dep_tools:
            dtools = DependencyTools()
        else:
            dtools = dep_tools

        try:
            stat = dtools.can_loop_be_parallelised(
                self, test_all_variables=test_all_variables,
                signatures_to_ignore=signatures_to_ignore)
            return stat
        except InternalError:
            # The dependence analysis in GOcean doesn't yet use PSyIR
            # consistently and that causes failures - TODO #845.
            return True

    # -------------------------------------------------------------------------
    def _halo_read_access(self, arg):
        '''Determines whether the supplied argument has (or might have) its
        halo data read within this loop. Returns True if it does, or if
        it might and False if it definitely does not.

        :param arg: an argument contained within this loop.
        :type arg: :py:class:`psyclone.gocean1p0.GOKernelArgument`

        :return: True if the argument reads, or might read from the \
                 halo and False otherwise.
        :rtype: bool

        '''
        return arg.argument_type == 'field' and arg.stencil.has_stencil and \
            arg.access in [AccessType.READ, AccessType.READWRITE,
                           AccessType.INC]

    def create_halo_exchanges(self):
        '''Add halo exchanges before this loop as required by fields within
        this loop. The PSyIR insertion logic is coded in the _add_halo_exchange
        helper method. '''

        for halo_field in self.unique_fields_with_halo_reads():
            # for each unique field in this loop that has its halo
            # read, find the previous update of this field.
            prev_arg_list = halo_field.backward_write_dependencies()
            if not prev_arg_list:
                # field has no previous dependence so create new halo
                # exchange(s) as we don't know the state of the fields
                # halo on entry to the invoke
                # TODO 856: If dl_es_inf supported an is_dirty flag, we could
                # be more selective on which HaloEx are needed. This
                # function then will be the same as in LFRic and therefore
                # the code can maybe be generalised.
                self._add_halo_exchange(halo_field)
            else:
                prev_node = prev_arg_list[0].call
                if not isinstance(prev_node, HaloExchange):
                    # Previous dependence is not a halo exchange so one needs
                    # to be added to satisfy the dependency in distributed
                    # memory.
                    self._add_halo_exchange(halo_field)

    def _add_halo_exchange(self, halo_field):
        '''An internal helper method to add the halo exchange call immediately
        before this loop using the halo_field argument for the associated
        field information.

        :param halo_field: the argument requiring a halo exchange
        :type halo_field: :py:class:`psyclone.gocean1p0.GOKernelArgument`

        '''
        exchange = GOHaloExchange(halo_field, parent=self.parent)
        self.parent.children.insert(self.position, exchange)

    # -------------------------------------------------------------------------
    @staticmethod
    def setup_bounds():
        '''Populates the GOLoop._bounds_lookup dictionary. This is
        used by PSyclone to look up the loop boundaries for each loop
        it creates.

        '''
        const = GOceanConstants()
        for grid_offset in const.SUPPORTED_OFFSETS:
            GOLoop._bounds_lookup[grid_offset] = {}
            for gridpt_type in const.VALID_FIELD_GRID_TYPES:
                GOLoop._bounds_lookup[grid_offset][gridpt_type] = {}
                for itspace in const.VALID_ITERATES_OVER:
                    GOLoop._bounds_lookup[grid_offset][gridpt_type][
                        itspace] = {}

        # Loop bounds for a mesh with NE offset
        GOLoop._bounds_lookup['go_offset_ne']['go_ct']['go_all_pts'] = \
            {'inner': {'start': "{start}-1", 'stop': "{stop}+1"},
             'outer': {'start': "{start}-1", 'stop': "{stop}+1"}}
        GOLoop._bounds_lookup['go_offset_ne']['go_ct']['go_internal_pts'] = \
            {'inner': {'start': "{start}", 'stop': "{stop}"},
             'outer': {'start': "{start}", 'stop': "{stop}"}}
        GOLoop._bounds_lookup['go_offset_ne']['go_cu']['go_all_pts'] = \
            {'inner': {'start': "{start}-1", 'stop': "{stop}"},
             'outer': {'start': "{start}-1", 'stop': "{stop}+1"}}
        GOLoop._bounds_lookup['go_offset_ne']['go_cu']['go_internal_pts'] = \
            {'inner': {'start': "{start}", 'stop': "{stop}-1"},
             'outer': {'start': "{start}", 'stop': "{stop}"}}
        GOLoop._bounds_lookup['go_offset_ne']['go_cv']['go_all_pts'] = \
            {'inner': {'start': "{start}-1", 'stop': "{stop}+1"},
             'outer': {'start': "{start}-1", 'stop': "{stop}"}}
        GOLoop._bounds_lookup['go_offset_ne']['go_cv']['go_internal_pts'] = \
            {'inner': {'start': "{start}", 'stop': "{stop}"},
             'outer': {'start': "{start}", 'stop': "{stop}-1"}}
        GOLoop._bounds_lookup['go_offset_ne']['go_cf']['go_all_pts'] = \
            {'inner': {'start': "{start}-1", 'stop': "{stop}"},
             'outer': {'start': "{start}-1", 'stop': "{stop}"}}
        GOLoop._bounds_lookup['go_offset_ne']['go_cf']['go_internal_pts'] = \
            {'inner': {'start': "{start}-1", 'stop': "{stop}-1"},
             'outer': {'start': "{start}-1", 'stop': "{stop}-1"}}
        # Loop bounds for a mesh with SE offset
        GOLoop._bounds_lookup['go_offset_sw']['go_ct']['go_all_pts'] = \
            {'inner': {'start': "{start}-1", 'stop': "{stop}+1"},
             'outer': {'start': "{start}-1", 'stop': "{stop}+1"}}
        GOLoop._bounds_lookup['go_offset_sw']['go_ct']['go_internal_pts'] = \
            {'inner': {'start': "{start}", 'stop': "{stop}"},
             'outer': {'start': "{start}", 'stop': "{stop}"}}
        GOLoop._bounds_lookup['go_offset_sw']['go_cu']['go_all_pts'] = \
            {'inner': {'start': "{start}-1", 'stop': "{stop}+1"},
             'outer': {'start': "{start}-1", 'stop': "{stop}+1"}}
        GOLoop._bounds_lookup['go_offset_sw']['go_cu']['go_internal_pts'] = \
            {'inner': {'start': "{start}", 'stop': "{stop}+1"},
             'outer': {'start': "{start}", 'stop': "{stop}"}}
        GOLoop._bounds_lookup['go_offset_sw']['go_cv']['go_all_pts'] = \
            {'inner': {'start': "{start}-1", 'stop': "{stop}+1"},
             'outer': {'start': "{start}-1", 'stop': "{stop}+1"}}
        GOLoop._bounds_lookup['go_offset_sw']['go_cv']['go_internal_pts'] = \
            {'inner': {'start': "{start}", 'stop': "{stop}"},
             'outer': {'start': "{start}", 'stop': "{stop}+1"}}
        GOLoop._bounds_lookup['go_offset_sw']['go_cf']['go_all_pts'] = \
            {'inner': {'start': "{start}-1", 'stop': "{stop}+1"},
             'outer': {'start': "{start}-1", 'stop': "{stop}+1"}}
        GOLoop._bounds_lookup['go_offset_sw']['go_cf']['go_internal_pts'] = \
            {'inner': {'start': "{start}", 'stop': "{stop}+1"},
             'outer': {'start': "{start}", 'stop': "{stop}+1"}}
        # For offset 'any'
        for gridpt_type in const.VALID_FIELD_GRID_TYPES:
            for itspace in const.VALID_ITERATES_OVER:
                GOLoop._bounds_lookup['go_offset_any'][gridpt_type][itspace] =\
                    {'inner': {'start': "{start}-1", 'stop': "{stop}"},
                     'outer': {'start': "{start}-1", 'stop': "{stop}"}}
        # For 'every' grid-point type
        for offset in const.SUPPORTED_OFFSETS:
            for itspace in const.VALID_ITERATES_OVER:
                GOLoop._bounds_lookup[offset]['go_every'][itspace] = \
                    {'inner': {'start': "{start}-1", 'stop': "{stop}+1"},
                     'outer': {'start': "{start}-1", 'stop': "{stop}+1"}}

    # -------------------------------------------------------------------------
    @staticmethod
    def add_bounds(bound_info):
        # pylint: disable=too-many-locals
        '''
        Adds a new iteration space to PSyclone. An iteration space in the
        gocean API is for a certain offset type and field type. It defines
        the loop boundaries for the outer and inner loop. The format is a
        ":" separated tuple:

        >>> bound_info = offset-type:field-type:iteration-space:outer-start:
                         outer-stop:inner-start:inner-stop

        Example:

        >>> bound_info = go_offset_ne:go_ct:go_all_pts:
                         {start}-1:{stop}+1:{start}:{stop}

        The expressions {start} and {stop} will be replaced with the loop
        indices that correspond to the inner points (i.e. non-halo or
        boundary points) of the field. So the index {start}-1 is actually
        on the halo / boundary.

        :param str bound_info: A string that contains a ":" separated \
                               tuple with the iteration space definition.

        :raises ValueError: if bound_info is not a string.
        :raises ConfigurationError: if bound_info is not formatted correctly.

        '''
        if not isinstance(bound_info, str):
            raise InternalError(f"The parameter 'bound_info' must be a "
                                f"string, got '{bound_info}' "
                                f"(type {type(bound_info)})")

        data = bound_info.split(":")
        if len(data) != 7:
            raise ConfigurationError(f"An iteration space must be in the form "
                                     f"\"offset-type:field-type:"
                                     f"iteration-space:outer-start:"
                                     f"outer-stop:inner-start:inner-stop\"\n"
                                     f"But got \"{bound_info}\"")

        if not GOLoop._bounds_lookup:
            GOLoop.setup_bounds()

        # Check that all bound specifications (min and max index) are valid.
        # ------------------------------------------------------------------
        # Regular expression that finds stings surrounded by {}
        bracket_regex = re.compile("{[^}]+}")
        for bound in data[3:7]:
            all_expr = bracket_regex.findall(bound)
            for bracket_expr in all_expr:
                if bracket_expr not in ["{start}", "{stop}"]:
                    raise ConfigurationError(f"Only '{{start}}' and "
                                             f"'{{stop}}' are allowed as "
                                             f"bracketed expression in an "
                                             f"iteration space. But got "
                                             f"{bracket_expr}")

        # We need to make sure the fparser is properly initialised, which
        # typically has not yet happened when the config file is read.
        # Otherwise the Nonlabel_Do_Stmt cannot parse valid expressions.
        ParserFactory().create(std="f2008")

        # Test both the outer loop indices (index 3 and 4) and inner
        # indices (index 5 and 6):
        for bound in data[3:7]:
            do_string = f"do i=1, {bound}"
            # Now replace any {start}/{stop} expression in the loop
            # with a valid integer value:
            do_string = do_string.format(start='15', stop='25')
            # Check if the do loop can be parsed as a nonlabel do loop
            try:
                _ = Nonlabel_Do_Stmt(do_string)
            except NoMatchError as err:
                raise ConfigurationError(f"Expression '{bound}' is not a "
                                         f"valid do loop boundary. Error "
                                         f"message: '{err}'.") from err

        # All tests successful, so add the new bounds:
        # --------------------------------------------
        current_bounds = GOLoop._bounds_lookup   # Shortcut
        # Check offset-type exists
        if not data[0] in current_bounds:
            current_bounds[data[0]] = {}

        # Check field-type exists
        if not data[1] in current_bounds[data[0]]:
            current_bounds[data[0]][data[1]] = {}

        const = GOceanConstants()
        # Check iteration space exists:
        if not data[2] in current_bounds[data[0]][data[1]]:
            current_bounds[data[0]][data[1]][data[2]] = {}
            const.VALID_ITERATES_OVER.append(data[2])

        current_bounds[data[0]][data[1]][data[2]] = \
            {'outer': {'start': data[3], 'stop': data[4]},
             'inner': {'start': data[5], 'stop': data[6]}}

    def get_custom_bound_string(self, side):
        '''
        Get the string that represents a customized custom bound for this
        GOLoop (provided by the add_bounds() method). It can provide the
        'start' or 'stop' side of the bounds.

        :param str side: 'start' or 'stop' side of the bound.

        :returns: the string that represents the loop bound.
        :rtype: str

        :raises GenerationError: if this node can not find a field in \
            the Invoke to be the base of the infrastructure call.
        :raises GenerationError: if no expression is known to obtain the \
            boundaries for a loop of this characteristics, because they \
            are not in the GOcean lookup table or the loop type is not \
            `inner` or `outer`.
        '''
        api_config = Config.get().api_conf("gocean")
        # Get a field argument from the argument list
        field = None
        invoke = self.ancestor(InvokeSchedule)
        if invoke:
            for arg in invoke.symbol_table.argument_list:
                if isinstance(arg.datatype, DataTypeSymbol):
                    if arg.datatype.name == "r2d_field":
                        field = arg
                        break

        if field is None:
            raise GenerationError(
                f"Cannot generate custom loop bound for loop {self}. "
                f"Couldn't find any suitable field.")

        if self.loop_type == "inner":
            prop_access = api_config.grid_properties["go_grid_xstop"]
        elif self.loop_type == "outer":
            prop_access = api_config.grid_properties["go_grid_ystop"]
        else:
            raise GenerationError(
                f"Invalid loop type of '{self.loop_type}'. Expected one of "
                f"{GOceanConstants().VALID_LOOP_TYPES}")

        stop_expr = prop_access.fortran.format(field.name)
        try:
            bound = self.bounds_lookup[self.index_offset][self.field_space][
                self.iteration_space][self.loop_type][side].format(
                    start='2', stop=stop_expr)
        except KeyError as err:
            raise GenerationError(
                f"Cannot generate custom loop bound for a loop with an index-"
                f"offset of '{self.index_offset}', a field-space of "
                f"'{self.field_space}', an iteration-space of "
                f"'{self.iteration_space}' and a loop-type of "
                f"'{self.loop_type}', for the side '{side}' because "
                f"this keys combination does not exist in the "
                f"GOLoop.bounds_lookup table.") from err

        return bound

    # -------------------------------------------------------------------------
    def _grid_property_psyir_expression(self, grid_property):
        '''
        Create a PSyIR reference expression using the supplied grid-property
        information (which will have been read from the config file).

        :param str grid_property: the property of the grid for which to \
            create a reference. This is the format string read from the \
            config file or just a simple name.

        :returns: the PSyIR expression for the grid-property access.
        :rtype: :py:class:`psyclone.psyir.nodes.Reference` or sub-class

        '''
        members = grid_property.split("%")
        if len(members) == 1:
            # We don't have a derived-type reference so create a Reference to
            # a data symbol.
            try:
                sym = self.scope.symbol_table.lookup(members[0])
            except KeyError:
                sym = self.scope.symbol_table.new_symbol(
                    members[0], symbol_type=DataSymbol, datatype=INTEGER_TYPE)
            return Reference(sym, parent=self)

        if members[0] != "{0}":
            raise NotImplementedError(
                f"Supplied grid property is a derived-type reference but "
                f"does not begin with '{{0}}': '{grid_property}'")

        fld_sym = self.scope.symbol_table.lookup(self.field_name)
        return StructureReference.create(fld_sym, members[1:])

    def upper_bound(self):
        ''' Creates the PSyIR of the upper bound of this loop.

        :returns: the PSyIR for the upper bound of this loop.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        '''
        if self.field_space == "go_every":
            # Bounds are independent of the grid-offset convention in use
            # We look-up the upper bounds by enquiring about the SIZE of
            # the array itself
            stop = IntrinsicCall(IntrinsicCall.Intrinsic.SIZE)
            # Use the data property to access the member of the field that
            # contains the actual grid points.
            api_config = Config.get().api_conf("gocean")
            sref = self._grid_property_psyir_expression(
                api_config.grid_properties["go_grid_data"].fortran)
            stop.addchild(sref)
            if self._loop_type == "inner":
                stop.addchild(Literal("1", INTEGER_TYPE, parent=stop))
            elif self._loop_type == "outer":
                stop.addchild(Literal("2", INTEGER_TYPE, parent=stop))
            return stop

        # Loop bounds are pulled from a infrastructure call from a field
        # object. For 'go_internal_pts' and 'go_all_points' we use the
        # 'internal' and 'whole' structures respectively. For other
        # iteration_spaces we look if a custom expression is defined in the
        # lookup table.
        props = Config.get().api_conf("gocean").grid_properties
        if self.iteration_space.lower() == "go_internal_pts":
            return self._grid_property_psyir_expression(
                props[f"go_grid_internal_{self._loop_type}_stop"].fortran)
        if self.iteration_space.lower() == "go_all_pts":
            return self._grid_property_psyir_expression(
                props[f"go_grid_whole_{self._loop_type}_stop"].fortran)
        bound_str = self.get_custom_bound_string("stop")
        return FortranReader().psyir_from_expression(
                    bound_str, self.ancestor(GOInvokeSchedule).symbol_table)

    def lower_bound(self):
        ''' Returns the lower bound of this loop as a string.

        :returns: root of PSyIR sub-tree describing this lower bound.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        '''
        if self.field_space == "go_every":
            # Bounds are independent of the grid-offset convention in use
            return Literal("1", INTEGER_TYPE)

        # Loop bounds are pulled from a infrastructure call from a field
        # object. For 'go_internal_pts' and 'go_all_points' we use the
        # 'internal' and 'whole' structures respectively. For other
        # iteration_spaces we look if a custom expression is defined in the
        # lookup table.
        props = Config.get().api_conf("gocean").grid_properties
        if self.iteration_space.lower() == "go_internal_pts":
            return self._grid_property_psyir_expression(
                props[f"go_grid_internal_{self._loop_type}_start"].fortran)
        if self.iteration_space.lower() == "go_all_pts":
            return self._grid_property_psyir_expression(
                props[f"go_grid_whole_{self._loop_type}_start"].fortran)
        bound_str = self.get_custom_bound_string("start")
        return FortranReader().psyir_from_expression(
                    bound_str, self.ancestor(GOInvokeSchedule).symbol_table)

    def _validate_loop(self):
        ''' Validate that the GOLoop has all necessary boundaries information
        to lower or gen_code to f2pygen.

        :raises GenerationError: if we can't find an enclosing Schedule.
        :raises GenerationError: if this loop does not enclose a Kernel.
        :raises GenerationError: if constant loop bounds are enabled but are \
                                 not supported for the current grid offset.
        :raises GenerationError: if the kernels within this loop expect \
                                 different different grid offsets.

        '''
        # Our schedule holds the names to use for the loop bounds.
        # Climb up the tree looking for our enclosing GOInvokeSchedule
        schedule = self.ancestor(GOInvokeSchedule)
        if schedule is None:
            raise GenerationError("Cannot find a GOInvokeSchedule ancestor "
                                  "for this GOLoop.")

        # Walk down the tree looking for a kernel so that we can
        # look-up what index-offset convention we are to use
        go_kernels = self.walk(GOKern)
        if not go_kernels:
            raise GenerationError("Cannot find the "
                                  "GOcean Kernel enclosed by this loop")
        index_offset = go_kernels[0].index_offset

        # Check that all kernels enclosed by this loop expect the same
        # grid offset
        for kernel in go_kernels:
            if kernel.index_offset != index_offset:
                raise GenerationError(f"All Kernels must expect the same "
                                      f"grid offset but kernel "
                                      f"'{kernel.name}' has offset"
                                      f" '{kernel.index_offset}' which does "
                                      f"not match '{index_offset}'.")

    def gen_code(self, parent):
        ''' Create the f2pygen AST for this loop (and update the PSyIR
        representing the loop bounds if necessary).

        :param parent: the node in the f2pygen AST to which to add content.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        # Check that it is a properly formed GOLoop
        self._validate_loop()

        super().gen_code(parent)


# pylint: disable=too-few-public-methods
class GOBuiltInCallFactory():
    ''' A GOcean-specific built-in call factory. No built-ins
        are supported in GOcean at the moment. '''

    @staticmethod
    def create():
        ''' Placeholder to create a GOocean-specific built-in call.
        This will require us to create a doubly-nested loop and then create
        the body of the particular built-in operation. '''
        raise GenerationError(
            "Built-ins are not supported for the GOcean 1.0 API")


# pylint: disable=too-few-public-methods
class GOKernCallFactory():
    ''' A GOcean-specific kernel-call factory. A standard kernel call in
    GOcean consists of a doubly-nested loop (over i and j) and a call to
    the user-supplied kernel routine. '''
    @staticmethod
    def create(call, parent=None):
        ''' Create a new instance of a call to a GO kernel. Includes the
        looping structure as well as the call to the kernel itself.

        :param parent: node where the kernel call structure will be inserted.
        :type parent: :py:class:`psyclone.psyir.nodes.Node`

        :returns: new PSyIR tree representing the kernel call loop.
        :rtype: :py:class:`psyclone.gocean1p0.GOLoop`

        '''
        # Add temporary parent as the GOKern constructor needs to find its
        # way to the top-level InvokeSchedule but we still don't have the
        # PSyIR loops to place it in the appropriate place. We can't create
        # the loops first because those depend on information provided by
        # this kernel.
        gocall = GOKern(call, parent=parent)

        # Determine Loop information from the enclosed Kernel
        iteration_space = gocall.iterates_over
        field_space = gocall.arguments.iteration_space_arg().function_space
        field_name = gocall.arguments.iteration_space_arg().name
        index_offset = gocall.index_offset

        # Create the double loop structure
        outer_loop = GOLoop.create(loop_type="outer",
                                   iteration_space=iteration_space,
                                   field_space=field_space,
                                   field_name=field_name,
                                   index_offset=index_offset,
                                   parent=parent)
        inner_loop = GOLoop.create(loop_type="inner",
                                   iteration_space=iteration_space,
                                   field_space=field_space,
                                   field_name=field_name,
                                   index_offset=index_offset,
                                   parent=outer_loop.loop_body)
        outer_loop.loop_body.addchild(inner_loop)
        # Remove temporary parent
        # pylint: disable=protected-access
        gocall._parent = None
        inner_loop.loop_body.addchild(gocall)
        return outer_loop


class GOKern(CodedKern):
    '''
    Stores information about GOcean Kernels as specified by the Kernel
    metadata. Uses this information to generate appropriate PSy layer
    code for the Kernel instance. Specialises the gen_code method to
    create the appropriate GOcean specific kernel call.

    :param call: information on the way in which this kernel is called \
                 from the Algorithm layer.
    :type call: :py:class:`psyclone.parse.algorithm.KernelCall`
    :param parent: optional node where the kernel call will be inserted.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`

    '''
    def __init__(self, call, parent=None):
        super().__init__(GOKernelArguments, call, parent, check=False)
        # Store the name of this kernel type (i.e. the name of the
        # Fortran derived type containing its metadata).
        self._metadata_name = call.ktype.name
        # Pull out the grid index-offset that this kernel expects and
        # store it here. This is used to check that all of the kernels
        # invoked by an application are using compatible index offsets.
        self._index_offset = call.ktype.index_offset

    @staticmethod
    def _create_psyir_for_access(symbol, var_value, depth):
        '''This function creates the PSyIR of an index-expression:
        - if `var_value` is negative, it returns 'symbol-depth'.
        - if `var_value` is positive, it returns 'symbol+depth`
        - otherwise it just returns a Reference to `symbol`.
        This is used to create artificial stencil accesses for GOKernels.

        :param symbol: the symbol to use.
        :type symbol: :py:class:`psyclone.psyir.symbols.Symbol`
        :param int var_value: value of the variable, which determines the \
            direction (adding or subtracting depth).
        :param int depth: the depth of the access (>0).

        :returns: the index expression for an access in the given direction.
        :rtype: union[:py:class:`psyclone.psyir.nodes.Reference`,
                      :py:class:`psyclone.psyir.nodes.BinaryOperation`]

        '''
        if var_value == 0:
            return Reference(symbol)
        if var_value > 0:
            operator = BinaryOperation.Operator.ADD
        else:
            operator = BinaryOperation.Operator.SUB

        return BinaryOperation.create(operator,
                                      Reference(symbol),
                                      Literal(str(depth), INTEGER_TYPE))

    def _record_stencil_accesses(self, signature, arg, var_accesses):
        '''This function adds accesses to a field depending on the
        meta-data declaration for this argument (i.e. accounting for
        any stencil accesses).

        :param signature: signature of the variable.
        :type signature: :py:class:`psyclone.core.Signature`
        :param arg:  the meta-data information for this argument.
        :type arg: :py:class:`psyclone.gocean1p0.GOKernelArgument`
        :param var_accesses: VariablesAccessInfo instance that stores the\
            information about the field accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        # TODO #2530: if we parse the actual kernel code, it might not
        # be required anymore to add these artificial accesses, instead
        # the actual kernel accesses could be added.
        sym_tab = self.ancestor(GOInvokeSchedule).symbol_table
        symbol_i = sym_tab.lookup_with_tag("contiguous_kidx")
        symbol_j = sym_tab.lookup_with_tag("noncontiguous_kidx")
        # Query each possible stencil direction and add corresponding
        # variable accesses. Note that if (i,j) itself is accessed, the
        # depth will be 1, so one access to (i,j) is then added.
        for j in [-1, 0, 1]:
            for i in [-1, 0, 1]:
                depth = arg.stencil.depth(i, j)
                for current_depth in range(1, depth+1):
                    # Create PSyIR expressions for the required
                    # i+/- and j+/- expressions
                    i_expr = GOKern._create_psyir_for_access(symbol_i, i,
                                                             current_depth)
                    j_expr = GOKern._create_psyir_for_access(symbol_j, j,
                                                             current_depth)
                    # Even if a GOKern argument is declared to be written, it
                    # can only ever write to (i,j), so any other references
                    # must be read:
                    if i == 0 and j == 0:
                        acc = arg.access
                    else:
                        acc = AccessType.READ

                    var_accesses.add_access(signature, acc, self,
                                            [i_expr, j_expr])

    def reference_accesses(self, var_accesses):
        '''Get all variable access information. All accesses are marked
        according to the kernel metadata.

        :param var_accesses: VariablesAccessInfo instance that stores the\
            information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.VariablesAccessInfo`

        '''
        # Grid properties are accessed using one of the fields. This stores
        # the field used to avoid repeatedly determining the best field:
        field_for_grid_property = None
        for arg in self.arguments.args:
            if arg.argument_type == "grid_property":
                if not field_for_grid_property:
                    field_for_grid_property = \
                        self._arguments.find_grid_access()
                var_name = arg.dereference(field_for_grid_property.name)
            else:
                var_name = arg.name

            signature = Signature(var_name.split("%"))
            if arg.is_scalar:
                # The argument is only a variable if it is not a constant:
                if not arg.is_literal:
                    var_accesses.add_access(signature, arg.access, self)
            else:
                if arg.argument_type == "field":
                    # Now add 'artificial' accesses to this field depending
                    # on meta-data (access-mode and stencil information):
                    self._record_stencil_accesses(signature, arg,
                                                  var_accesses)
                else:
                    # In case of an array for now add an arbitrary array
                    # reference to (i,j) so it is properly recognised as
                    # an array access.
                    sym_tab = self.ancestor(GOInvokeSchedule).symbol_table
                    symbol_i = sym_tab.lookup_with_tag("contiguous_kidx")
                    symbol_j = sym_tab.lookup_with_tag("noncontiguous_kidx")
                    var_accesses.add_access(signature, arg.access,
                                            self, [Reference(symbol_i),
                                                   Reference(symbol_j)])
        super().reference_accesses(var_accesses)
        var_accesses.next_location()

    def local_vars(self):
        '''Return a list of the variable (names) that are local to this loop
        (and must therefore be e.g. threadprivate if doing OpenMP)

        '''
        return []

    @property
    def index_offset(self):
        ''' The grid index-offset convention that this kernel expects '''
        return self._index_offset

    def get_kernel_schedule(self):
        '''
        :returns: a schedule representing the GOcean kernel code.
        :rtype: :py:class:`psyclone.gocean1p0.GOKernelSchedule`

        :raises GenerationError: if there is a problem raising the language- \
                                 level PSyIR of this kernel to GOcean PSyIR.
        '''
        if self._kern_schedule:
            return self._kern_schedule

        # Construct the PSyIR of the Fortran parse tree.
        astp = Fparser2Reader()
        psyir = astp.generate_psyir(self.ast)
        # pylint: disable=import-outside-toplevel
        from psyclone.domain.gocean.transformations import (
            RaisePSyIR2GOceanKernTrans)
        raise_trans = RaisePSyIR2GOceanKernTrans(self._metadata_name)
        try:
            raise_trans.apply(psyir)
        except Exception as err:
            raise GenerationError(
                f"Failed to raise the PSyIR for kernel '{self.name}' "
                f"to GOcean PSyIR. Error was:\n{err}") from err
        for routine in psyir.walk(Routine):
            if routine.name == self.name:
                break
        # We know the above loop will find the named routine because the
        # previous raising transformation would have failed otherwise.
        # pylint: disable=undefined-loop-variable
        self._kern_schedule = routine

        return self._kern_schedule


class GOKernelArguments(Arguments):
    '''Provides information about GOcean kernel-call arguments
    collectively, as specified by the kernel argument metadata. This
    class ensures that initialisation is performed correctly. It also
    overrides the iteration_space_arg method to supply a
    GOcean-specific dictionary for the mapping of argument-access
    types.

    :param call: the kernel meta-data for which to extract argument info.
    :type call: :py:class:`psyclone.parse.KernelCall`
    :param parent_call: the kernel-call object.
    :type parent_call: :py:class:`psyclone.gocean1p0.GOKern`
    :param bool check: whether to check for consistency between the \
        kernel metadata and the algorithm layer. Defaults to \
        True. Currently does nothing in this API.

    '''
    def __init__(self, call, parent_call, check=True):
        # pylint: disable=unused-argument
        if False:  # pylint: disable=using-constant-test
            self._0_to_n = GOKernelArgument(None, None, None)  # for pyreverse
        Arguments.__init__(self, parent_call)

        self._args = []
        # Loop over the kernel arguments obtained from the meta data
        for (idx, arg) in enumerate(call.ktype.arg_descriptors):
            # arg is a GO1p0Descriptor object
            if arg.argument_type == "grid_property":
                # This is an argument supplied by the psy layer
                self._args.append(GOKernelGridArgument(arg, parent_call))
            elif arg.argument_type in ["scalar", "field"]:
                # This is a kernel argument supplied by the Algorithm layer
                self._args.append(GOKernelArgument(arg, call.args[idx],
                                                   parent_call))
            else:
                raise ParseError(f"Invalid kernel argument type. Found "
                                 f"'{arg.argument_type}' but must be one of "
                                 f"['grid_property', 'scalar', 'field'].")
        self._dofs = []

    def psyir_expressions(self):
        '''
        :returns: the PSyIR expressions representing this Argument list.
        :rtype: list of :py:class:`psyclone.psyir.nodes.Node`

        '''
        symtab = self._parent_call.scope.symbol_table
        symbol1 = symtab.lookup_with_tag("contiguous_kidx")
        symbol2 = symtab.lookup_with_tag("noncontiguous_kidx")
        return ([Reference(symbol1), Reference(symbol2)] +
                [arg.psyir_expression() for arg in self.args])

    def find_grid_access(self):
        '''
        Determine the best kernel argument from which to get properties of
        the grid. For this, an argument must be a field (i.e. not
        a scalar) and must be supplied by the algorithm layer
        (i.e. not a grid property). If possible it should also be
        a field that is read-only as otherwise compilers can get
        confused about data dependencies and refuse to SIMD
        vectorise.
        :returns: the argument object from which to get grid properties.
        :rtype: :py:class:`psyclone.gocean1p0.GOKernelArgument` or None
        '''
        for access in [AccessType.READ, AccessType.READWRITE,
                       AccessType.WRITE]:
            for arg in self._args:
                if arg.argument_type == "field" and arg.access == access:
                    return arg
        # We failed to find any kernel argument which could be used
        # to access the grid properties. This will only be a problem
        # if the kernel requires a grid-property argument.
        return None

    @property
    def dofs(self):
        ''' Currently required for invoke base class although this makes no
            sense for GOcean. Need to refactor the Invoke base class and
            remove the need for this property (#279). '''
        return self._dofs

    @property
    def acc_args(self):
        '''
        Provide the list of references (both objects and arrays) that must
        be present on an OpenACC device before the kernel associated with
        this Arguments object may be launched.

        :returns: list of (Fortran) quantities
        :rtype: list of str
        '''
        arg_list = []

        # First off, specify the field object which we will de-reference in
        # order to get any grid properties (if this kernel requires them).
        # We do this as some compilers do less optimisation if we get (read-
        # -only) grid properties from a field object that has read-write
        # access.
        grid_fld = self.find_grid_access()
        grid_ptr = grid_fld.name + "%grid"
        api_config = Config.get().api_conf("gocean")
        # TODO: #676 go_grid_data is actually a field property
        data_fmt = api_config.grid_properties["go_grid_data"].fortran
        arg_list.extend([grid_fld.name, data_fmt.format(grid_fld.name)])
        for arg in self._args:
            if arg.argument_type == "scalar":
                arg_list.append(arg.name)
            elif arg.argument_type == "field" and arg != grid_fld:
                # The remote device will need the reference to the field
                # object *and* the reference to the array within that object.
                arg_list.extend([arg.name, data_fmt.format(arg.name)])
            elif arg.argument_type == "grid_property":
                if grid_ptr not in arg_list:
                    # This kernel needs a grid property and therefore the
                    # pointer to the grid object must be copied to the device.
                    arg_list.append(grid_ptr)
                arg_list.append(grid_ptr+"%"+arg.name)
        return arg_list

    @property
    def fields(self):
        '''
        Provides the list of names of field objects that are required by
        the kernel associated with this Arguments object.

        :returns: List of names of (Fortran) field objects.
        :rtype: list of str
        '''
        args = args_filter(self._args, arg_types=["field"])
        return [arg.name for arg in args]

    @property
    def scalars(self):
        '''
        Provides the list of names of scalar arguments required by the
        kernel associated with this Arguments object. If there are none
        then the returned list is empty.

        :returns: A list of the names of scalar arguments in this object.
        :rtype: list of str
        '''
        args = args_filter(self._args, arg_types=["scalar"])
        return [arg.name for arg in args]

    def append(self, name, argument_type):
        ''' Create and append a GOKernelArgument to the Argument list.

        :param str name: name of the appended argument.
        :param str argument_type: type of the appended argument.

        :raises TypeError: if the given name is not a string.

        '''
        if not isinstance(name, str):
            raise TypeError(
                f"The name parameter given to GOKernelArguments.append "
                f"method should be a string, but found "
                f"'{type(name).__name__}' instead.")

        # Create a descriptor with the given type. `len(self.args)` gives the
        # position in the argument list of the argument to which this
        # descriptor corresponds. (This argument is appended in the code
        # below.)
        descriptor = Descriptor(None, argument_type, len(self.args))

        # Create the argument and append it to the argument list
        arg = Arg("variable", name)
        argument = GOKernelArgument(descriptor, arg, self._parent_call)
        self.args.append(argument)


class GOKernelArgument(KernelArgument):
    ''' Provides information about individual GOcean kernel call arguments
        as specified by the kernel argument metadata. '''
    def __init__(self, arg, arg_info, call):

        self._arg = arg
        KernelArgument.__init__(self, arg, arg_info, call)
        # Complete the argument initialisation as in some APIs it
        # needs to be separated.
        self._complete_init(arg_info)

    def psyir_expression(self):
        '''
        :returns: the PSyIR expression represented by this Argument.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        :raises InternalError: if this Argument type is not "field" or \
                               "scalar".

        '''
        # If the argument name is just a number (e.g. '0') we return a
        # constant Literal expression
        if self.name.isnumeric():
            return Literal(self.name, INTEGER_TYPE)

        # Now try for a real value. The constructor will raise an exception
        # if the string is not a valid floating point number.
        try:
            return Literal(self.name, REAL_TYPE)
        except ValueError:
            pass

        # Otherwise it's some form of Reference
        symbol = self._call.scope.symbol_table.lookup(self.name)

        # Gocean field arguments are StructureReferences to the %data attribute
        if self.argument_type == "field":
            return StructureReference.create(symbol, ["data"])

        # Gocean scalar arguments are References to the variable
        if self.argument_type == "scalar":
            return Reference(symbol)

        raise InternalError(f"GOcean expects the Argument.argument_type() to "
                            f"be 'field' or 'scalar' but found "
                            f"'{self.argument_type}'.")

    def infer_datatype(self):
        ''' Infer the datatype of this argument using the API rules.

        :returns: the datatype of this argument.
        :rtype: :py:class::`psyclone.psyir.symbols.DataType`

        :raises InternalError: if this Argument type is not "field" or \
                               "scalar".
        :raises InternalError: if this argument is scalar but its space \
                               property is not 'go_r_scalar' or 'go_i_scalar'.

        '''
        # All GOcean fields are r2d_field
        if self.argument_type == "field":
            # r2d_field can have UnresolvedType and UnresolvedInterface because
            # it is an unnamed import from a module.
            type_symbol = self._call.root.symbol_table.find_or_create_tag(
                "r2d_field", symbol_type=DataTypeSymbol,
                datatype=UnresolvedType(), interface=UnresolvedInterface())
            return type_symbol

        # Gocean scalars can be REAL or INTEGER
        if self.argument_type == "scalar":
            if self.space.lower() == "go_r_scalar":
                go_wp = self._call.root.symbol_table.find_or_create_tag(
                    "go_wp", symbol_type=DataSymbol, datatype=UnresolvedType(),
                    interface=UnresolvedInterface())
                return ScalarType(ScalarType.Intrinsic.REAL, go_wp)
            if self.space.lower() == "go_i_scalar":
                return INTEGER_TYPE
            raise InternalError(f"GOcean expects scalar arguments to be of "
                                f"'go_r_scalar' or 'go_i_scalar' type but "
                                f"found '{self.space.lower()}'.")

        raise InternalError(f"GOcean expects the Argument.argument_type() "
                            f"to be 'field' or 'scalar' but found "
                            f"'{self.argument_type}'.")

    @property
    def intrinsic_type(self):
        '''
        :returns: the intrinsic type of this argument. If it's not a scalar \
            integer or real it will return an empty string.
        :rtype: str

        '''
        if self.argument_type == "scalar":
            if self.space.lower() == "go_r_scalar":
                return "real"
            if self.space.lower() == "go_i_scalar":
                return "integer"
        return ""

    @property
    def argument_type(self):
        '''
        Return the type of this kernel argument - whether it is a field,
        a scalar or a grid_property (to be supplied by the PSy layer).
        If it has no type it defaults to scalar.

        :returns: the type of the argument.
        :rtype: str

        '''
        if self._arg.argument_type:
            return self._arg.argument_type
        return "scalar"

    @property
    def function_space(self):
        ''' Returns the expected finite difference space for this
            argument as specified by the kernel argument metadata.'''
        return self._arg.function_space

    @property
    def is_scalar(self):
        ''':return: whether this variable is a scalar variable or not.
        :rtype: bool'''
        return self.argument_type == "scalar"


class GOKernelGridArgument(Argument):
    '''
    Describes arguments that supply grid properties to a kernel.
    These arguments are provided by the PSy layer rather than in
    the Algorithm layer.

    :param arg: the meta-data entry describing the required grid property.
    :type arg: :py:class:`psyclone.gocean1p0.GO1p0Descriptor`
    :param kernel_call: the kernel call node that this Argument belongs to.
    :type kernel_call: :py:class:`psyclone.gocean1p0.GOKern`

    :raises GenerationError: if the grid property is not recognised.

    '''
    def __init__(self, arg, kernel_call):
        super().__init__(None, None, arg.access)
        # Complete the argument initialisation as in some APIs it
        # needs to be separated.
        self._complete_init(None)

        api_config = Config.get().api_conf("gocean")
        try:
            deref_name = api_config.grid_properties[arg.grid_prop].fortran
        except KeyError as err:
            all_keys = str(api_config.grid_properties.keys())
            raise GenerationError(f"Unrecognised grid property specified. "
                                  f"Expected one of {all_keys} but found "
                                  f"'{arg.grid_prop}'") from err

        # Each entry is a pair (name, type). Name can be subdomain%internal...
        # so only take the last part after the last % as name.
        self._name = deref_name.split("%")[-1]
        # Store the original property name for easy lookup in is_scalar
        self._property_name = arg.grid_prop

        # This object always represents an argument that is a grid_property
        self._argument_type = "grid_property"

        # Reference to the Call this argument belongs to
        self._call = kernel_call

    @property
    def name(self):
        ''' Returns the Fortran name of the grid property, which is used
        in error messages etc.'''
        return self._name

    def psyir_expression(self):
        '''
        :returns: the PSyIR expression represented by this Argument.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        '''
        # Find field from which to access grid properties
        base_field = self._call.arguments.find_grid_access().name
        tag = "AlgArgs_" + base_field
        symbol = self._call.scope.symbol_table.find_or_create_tag(tag)

        # Get aggregate grid type accessors without the base name
        access = self.dereference(base_field).split('%')[1:]

        # Construct the PSyIR reference
        return StructureReference.create(symbol, access)

    def dereference(self, fld_name):
        '''Returns a Fortran string to dereference a grid property of the
        specified field. It queries the current config file settings for
        getting the proper dereference string, which is a format string
        where {0} represents the field name.

        :param str fld_name: The name of the field which is used to \
            dereference a grid property.

        :returns: the dereference string required to access a grid property
            in a dl_esm field (e.g. "subdomain%internal%xstart"). The name
            must contains a "{0}" which is replaced by the field name.
        :rtype: str'''
        api_config = Config.get().api_conf("gocean")
        deref_name = api_config.grid_properties[self._property_name].fortran
        return deref_name.format(fld_name)

    @property
    def argument_type(self):
        ''' The type of this argument. We have this for compatibility with
            GOKernelArgument objects since, for this class, it will always be
            "grid_property". '''
        return self._argument_type

    @property
    def intrinsic_type(self):
        '''
        :returns: the intrinsic_type of this argument.
        :rtype: str

        '''
        api_config = Config.get().api_conf("gocean")
        return api_config.grid_properties[self._property_name].intrinsic_type

    @property
    def is_scalar(self):
        '''
        :returns: if this variable is a scalar variable or not.
        :rtype: bool
        '''
        # The constructor guarantees that _property_name is a valid key!
        api_config = Config.get().api_conf("gocean")
        return api_config.grid_properties[self._property_name].type \
            == "scalar"

    @property
    def text(self):
        ''' The raw text used to pass data from the algorithm layer
            for this argument. Grid properties are not passed from the
            algorithm layer so None is returned.'''
        return None

    def forward_dependence(self):
        '''
        A grid-property argument is read-only and supplied by the
        PSy layer so has no dependencies

        :returns: None to indicate no dependencies
        :rtype: NoneType
        '''
        return None

    def backward_dependence(self):
        '''
        A grid-property argument is read-only and supplied by the
        PSy layer so has no dependencies

        :returns: None to indicate no dependencies
        :rtype: NoneType
        '''
        return None


class GOStencil():
    '''GOcean 1.0 stencil information for a kernel argument as obtained by
    parsing the kernel meta-data. The expected structure of the
    metadata and its meaning is provided in the description of the
    load method

    '''
    def __init__(self):
        ''' Set up any internal variables. '''
        self._has_stencil = None
        self._stencil = [[0 for _ in range(3)] for _ in range(3)]
        self._name = None
        self._initialised = False

    # pylint: disable=too-many-branches
    def load(self, stencil_info, kernel_name):
        '''Take parsed stencil metadata information, check it is valid and
        store it in a convenient form. The kernel_name argument is
        only used to provide the location if there is an error.

        The stencil information should either be a name which
        indicates a particular type of stencil or in the form
        stencil(xxx,yyy,zzz) which explicitly specifies a stencil
        shape where xxx, yyy and zzz are triplets of integers
        indicating whether there is a stencil access in a particular
        direction and the depth of that access. For example:

        go_stencil(010,  !   N
                   212,  !  W E
                   010)  !   S

        indicates that there is a stencil access of depth 1 in the
        "North" and "South" directions and stencil access of depth 2
        in the "East" and "West" directions. The value at the centre
        of the stencil will not be used by PSyclone but can be 0 or 1
        and indicates whether the local field value is accessed.

        The convention is for the associated arrays to be
        2-dimensional. If we denote the first dimension as "i" and the
        second dimension as "j" then the following directions are
        assumed:


        > j
        > ^
        > |
        > |
        > ---->i

        For example a stencil access like:

        a(i,j) + a(i+1,j) + a(i,j-1)

        would be stored as:

        go_stencil(000,
                   011,
                   010)

        :param stencil_info: contains the appropriate part of the parser AST
        :type stencil_info: :py:class:`psyclone.expression.FunctionVar`
        :param string kernel_name: the name of the kernel from where this \
                                   stencil information came from.

        :raises ParseError: if the supplied stencil information is invalid.

        '''
        self._initialised = True

        if not isinstance(stencil_info, expr.FunctionVar):
            # the stencil information is not in the expected format
            raise ParseError(
                f"Meta-data error in kernel '{kernel_name}': 3rd descriptor "
                f"(stencil) of field argument is '{stencil_info}' but "
                f"expected either a name or the format 'go_stencil(...)'")

        # Get the name
        name = stencil_info.name.lower()
        const = GOceanConstants()

        if stencil_info.args:
            # The stencil info is of the form 'name(a,b,...), so the
            # name should be 'stencil' and there should be 3
            # arguments'
            self._has_stencil = True
            args = stencil_info.args
            if name != "go_stencil":
                raise ParseError(
                    f"Meta-data error in kernel '{kernel_name}': 3rd "
                    f"descriptor (stencil) of field argument is '{name}' but "
                    f"must be 'go_stencil(...)")
            if len(args) != 3:
                raise ParseError(
                    f"Meta-data error in kernel '{kernel_name}': 3rd "
                    f"descriptor (stencil) of field argument with format "
                    f"'go_stencil(...)', has {len(args)} arguments but should "
                    f"have 3")
            # Each of the 3 args should be of length 3 and each
            # character should be a digit from 0-9. Whilst we are
            # expecting numbers, the parser represents these numbers
            # as strings so we have to perform string manipulation to
            # check and that extract them
            for arg_idx in range(3):
                arg = args[arg_idx]
                if not isinstance(arg, str):
                    raise ParseError(
                        f"Meta-data error in kernel '{kernel_name}': 3rd "
                        f"descriptor (stencil) of field argument with format "
                        f"'go_stencil(...)'. Argument index {arg_idx} should "
                        f"be a number but found '{arg}'.")
                if len(arg) != 3:
                    raise ParseError(
                        f"Meta-data error in kernel '{kernel_name}': 3rd "
                        f"descriptor (stencil) of field argument with format "
                        f"'go_stencil(...)'. Argument index {arg_idx} should "
                        f"consist of 3 digits but found {len(arg)}.")
            # The central value is constrained to be 0 or 1
            if args[1][1] not in ["0", "1"]:
                raise ParseError(
                    f"Meta-data error in kernel '{kernel_name}': 3rd "
                    f"descriptor (stencil) of field argument with format "
                    f"'go_stencil(...)'. Argument index 1 position 1 "
                    f"should be a number from 0-1 but found {args[1][1]}.")
            # It is not valid to specify a zero stencil. This is
            # indicated by the 'pointwise' name
            if args[0] == "000" and \
               (args[1] == "000" or args[1] == "010") and \
               args[2] == "000":
                raise ParseError(
                    f"Meta-data error in kernel '{kernel_name}': 3rd "
                    f"descriptor (stencil) of field argument with format "
                    f"'go_stencil(...)'. A zero sized stencil has been "
                    f"specified. This should be specified with the "
                    f"'go_pointwise' keyword.")
            # store the values in an internal array as integers in i,j
            # order
            for idx0 in range(3):
                for idx1 in range(3):
                    # The j coordinate needs to be 'reversed': the first
                    # row (index 0 in args) is 'top', which should be
                    # accessed using '+1', and the last row (index 2 in args)
                    # needs to be accessed using '-1' (see depth()). Using
                    # 2-idx1 mirrors the rows appropriately.
                    self._stencil[idx0][2-idx1] = int(args[idx1][idx0])
        else:
            # stencil info is of the form 'name' so should be one of
            # our valid names
            if name not in const.VALID_STENCIL_NAMES:
                raise ParseError(
                    f"Meta-data error in kernel '{kernel_name}': 3rd "
                    f"descriptor (stencil) of field argument is '{name}' "
                    f"but must be one of {const.VALID_STENCIL_NAMES} or "
                    f"go_stencil(...)")
            self._name = name
            # We currently only support one valid name ('pointwise')
            # which indicates that there is no stencil
            self._has_stencil = False
            # Define a 'stencil' for pointwise so that depth() can be used for
            # pointwise kernels without handling pointwise as special case
            self._stencil = [[0, 0, 0], [0, 1, 0], [0, 0, 0]]

    def _check_init(self):
        '''Internal method which checks that the stencil information has been
        loaded.

        :raises GenerationError: if the GOStencil object has not been
        initialised i.e. the load() method has not been called

        '''
        if not self._initialised:
            raise GenerationError(
                "Error in class GOStencil: the object has not yet been "
                "initialised. Please ensure the load() method is called.")

    @property
    def has_stencil(self):
        '''Specifies whether this argument has stencil information or not. The
        only case when this is False is when the stencil information
        specifies 'pointwise' as this indicates that there is no
        stencil access.

        :returns: True if this argument has stencil information and False \
                  if not.
        :rtype: bool

        '''
        self._check_init()
        return self._has_stencil

    @property
    def name(self):
        '''Provides the stencil name if one is provided

        :returns: the name of the type of stencil if this is provided \
                  and 'None' if not.
        :rtype: str

        '''
        self._check_init()
        return self._name

    def depth(self, index0, index1):
        '''Provides the depth of the stencil in the 8 possible stencil
        directions in a 2d regular grid (see the description in the
        load class for more information). Values must be between -1
        and 1 as they are considered to be relative to the centre of
        the stencil For example:

        stencil(234,
                915,
                876)

        returns

        depth(-1,0) = 9
        depth(1,1) = 4

        :param int index0: the relative stencil offset for the first \
                           index of the associated array. This value \
                           must be between -1 and 1.
        :param int index1: the relative stencil offset for the second \
                           index of the associated array. This value \
                           must be between -1 and 1

        :returns: the depth of the stencil in the specified direction.
        :rtype: int

        :raises GenerationError: if the indices are out-of-bounds.

        '''
        self._check_init()
        if index0 < -1 or index0 > 1 or index1 < -1 or index1 > 1:
            raise GenerationError(
                f"The indices arguments to the depth method in the GOStencil "
                f"object must be between -1 and 1 but found "
                f"({index0},{index1})")
        return self._stencil[index0+1][index1+1]


class GO1p0Descriptor(Descriptor):
    ''' Description of a GOcean 1.0 kernel argument, as obtained by
    parsing the kernel metadata.

    :param str kernel_name: the name of the kernel metadata type \
                            that contains this metadata.
    :param kernel_arg: the relevant part of the parser's AST.
    :type kernel_arg: :py:class:`psyclone.expression.FunctionVar`
    :param int metadata_index: the postion of this argument in the list of \
                               arguments specified in the metadata.

    :raises ParseError: if a kernel argument has an invalid grid-point type.
    :raises ParseError: for an unrecognised grid property.
    :raises ParseError: for an invalid number of arguments.
    :raises ParseError: for an invalid access argument.

    '''
    def __init__(self, kernel_name, kernel_arg, metadata_index):
        # pylint: disable=too-many-locals
        nargs = len(kernel_arg.args)
        stencil_info = None

        const = GOceanConstants()
        if nargs == 3:
            # This kernel argument is supplied by the Algorithm layer
            # and is either a field or a scalar

            access = kernel_arg.args[0].name
            funcspace = kernel_arg.args[1].name
            stencil_info = GOStencil()
            stencil_info.load(kernel_arg.args[2],
                              kernel_name)

            # Valid values for the grid-point type that a kernel argument
            # may have. (We use the funcspace argument for this as it is
            # similar to the space in Finite-Element world.)
            valid_func_spaces = const.VALID_FIELD_GRID_TYPES + \
                const.VALID_SCALAR_TYPES

            self._grid_prop = ""
            if funcspace.lower() in const.VALID_FIELD_GRID_TYPES:
                self._argument_type = "field"
            elif funcspace.lower() in const.VALID_SCALAR_TYPES:
                self._argument_type = "scalar"
            else:
                raise ParseError(f"Meta-data error in kernel {kernel_name}: "
                                 f"argument grid-point type is '{funcspace}' "
                                 f"but must be one of {valid_func_spaces}")

        elif nargs == 2:
            # This kernel argument is a property of the grid. The grid
            # properties are special because they must be supplied by
            # the PSy layer.
            access = kernel_arg.args[0].name
            grid_var = kernel_arg.args[1].name
            funcspace = ""

            self._grid_prop = grid_var
            self._argument_type = "grid_property"
            api_config = Config.get().api_conf("gocean")

            if grid_var.lower() not in api_config.grid_properties:
                valid_keys = str(api_config.grid_properties.keys())
                raise ParseError(
                    f"Meta-data error in kernel {kernel_name}: un-recognised "
                    f"grid property '{grid_var}' requested. Must be one of "
                    f"{valid_keys}")
        else:
            raise ParseError(
                f"Meta-data error in kernel {kernel_name}: 'arg' type "
                f"expects 2 or 3 arguments but found '{len(kernel_arg.args)}' "
                f"in '{kernel_arg.args}'")

        api_config = Config.get().api_conf("gocean")
        access_mapping = api_config.get_access_mapping()
        try:
            access_type = access_mapping[access]
        except KeyError as err:
            valid_names = api_config.get_valid_accesses_api()
            raise ParseError(
                f"Meta-data error in kernel {kernel_name}: argument access is "
                f"given as '{access}' but must be one of {valid_names}"
            ) from err

        # Finally we can call the __init__ method of our base class
        super().__init__(access_type, funcspace, metadata_index,
                         stencil=stencil_info,
                         argument_type=self._argument_type)

    def __str__(self):
        return repr(self)

    @property
    def grid_prop(self):
        '''
        :returns: the name of the grid-property that this argument is to \
                  supply to the kernel.
        :rtype: str

        '''
        return self._grid_prop


class GOKernelType1p0(KernelType):
    ''' Description of a kernel including the grid index-offset it
        expects and the region of the grid that it expects to
        operate upon '''

    def __str__(self):
        return ('GOcean 1.0 kernel ' + self._name + ', index-offset = ' +
                self._index_offset + ', iterates-over = ' +
                self._iterates_over)

    def __init__(self, ast, name=None):
        # Initialise the base class
        KernelType.__init__(self, ast, name=name)

        # What grid offset scheme this kernel expects
        self._index_offset = self._ktype.get_variable('index_offset').init

        const = GOceanConstants()
        if self._index_offset is None:
            raise ParseError(f"Meta-data error in kernel {name}: an "
                             f"INDEX_OFFSET must be specified and must be "
                             f"one of {const.VALID_OFFSET_NAMES}")

        if self._index_offset.lower() not in const.VALID_OFFSET_NAMES:
            raise ParseError(f"Meta-data error in kernel {name}: "
                             f"INDEX_OFFSET has value '{self._index_offset}'"
                             f" but must be one of {const.VALID_OFFSET_NAMES}")

        const = GOceanConstants()
        # Check that the meta-data for this kernel is valid
        if self._iterates_over is None:
            raise ParseError(f"Meta-data error in kernel {name}: "
                             f"ITERATES_OVER is missing. (Valid values are: "
                             f"{const.VALID_ITERATES_OVER})")

        if self._iterates_over.lower() not in const.VALID_ITERATES_OVER:
            raise ParseError(f"Meta-data error in kernel {name}: "
                             f"ITERATES_OVER has value '"
                             f"{self._iterates_over.lower()}' but must be "
                             f"one of {const.VALID_ITERATES_OVER}")

        # The list of kernel arguments
        self._arg_descriptors = []
        have_grid_prop = False
        for idx, init in enumerate(self._inits):
            if init.name != 'go_arg':
                raise ParseError(f"Each meta_arg value must be of type "
                                 f"'go_arg' for the gocean api, but "
                                 f"found '{init.name}'")
            # Pass in the name of this kernel for the purposes
            # of error reporting
            new_arg = GO1p0Descriptor(name, init, idx)
            # Keep track of whether this kernel requires any
            # grid properties
            have_grid_prop = (have_grid_prop or
                              (new_arg.argument_type == "grid_property"))
            self._arg_descriptors.append(new_arg)

        # If this kernel expects a grid property then check that it
        # has at least one field object as an argument (which we
        # can use to access the grid)
        if have_grid_prop:
            have_fld = False
            for arg in self.arg_descriptors:
                if arg.argument_type == "field":
                    have_fld = True
                    break
            if not have_fld:
                raise ParseError(
                    f"Kernel {name} requires a property of the grid but does "
                    f"not have any field objects as arguments.")

    # Override nargs from the base class so that it returns the no.
    # of args specified in the algorithm layer (and thus excludes those
    # that must be added in the PSy layer). This is done to simplify the
    # check on the no. of arguments supplied in any invoke of the kernel.
    @property
    def nargs(self):
        ''' Count and return the number of arguments that this kernel
            expects the Algorithm layer to provide '''
        count = 0
        for arg in self.arg_descriptors:
            if arg.argument_type != "grid_property":
                count += 1
        return count

    @property
    def index_offset(self):
        ''' Return the grid index-offset that this kernel expects '''
        return self._index_offset


class GOACCEnterDataDirective(ACCEnterDataDirective):
    '''
    Sub-classes ACCEnterDataDirective to provide the dl_esm_inf infrastructure-
    specific interfaces to flag and update when data is on a device.

    '''
    def _read_from_device_routine(self):
        ''' Return the symbol of the routine that reads data from the OpenACC
        device, if it doesn't exist create the Routine and the Symbol.

        :returns: the symbol representing the read_from_device routine.
        :rtype: :py:class:`psyclone.psyir.symbols.symbol`
        '''
        symtab = self.root.symbol_table
        try:
            return symtab.lookup_with_tag("openacc_read_func")
        except KeyError:
            # If the subroutines does not exist, it needs to be
            # generated first.
            pass

        # Create the symbol for the routine and add it to the symbol table.
        subroutine_name = symtab.new_symbol(
            "read_from_device", symbol_type=RoutineSymbol,
            tag="openacc_read_func").name

        code = '''
            subroutine read_openacc(from, to, startx, starty, nx, ny, blocking)
                use iso_c_binding, only: c_ptr
                use kind_params_mod, only: go_wp
                type(c_ptr), intent(in) :: from
                real(go_wp), dimension(:,:), intent(inout), target :: to
                integer, intent(in) :: startx, starty, nx, ny
                logical, intent(in) :: blocking
            end subroutine read_openacc
            '''

        # Obtain the PSyIR representation of the code above
        fortran_reader = FortranReader()
        container = fortran_reader.psyir_from_source(code)
        subroutine = container.children[0]
        # Add an ACCUpdateDirective inside the subroutine
        subroutine.addchild(ACCUpdateDirective([Signature("to")], "host",
                                               if_present=False))

        # Rename subroutine
        subroutine.name = subroutine_name

        # Insert the routine as a child of the ancestor Container
        if not self.ancestor(Container):
            raise GenerationError(
                f"The GOACCEnterDataDirective can only be generated/lowered "
                f"inside a Container in order to insert a sibling "
                f"subroutine, but '{self}' is not inside a Container.")
        self.ancestor(Container).addchild(subroutine.detach())

        return symtab.lookup_with_tag("openacc_read_func")

    def lower_to_language_level(self):
        '''
        In-place replacement of DSL or high-level concepts into generic PSyIR
        constructs. In addition to calling this method in the base class, the
        GOACCEnterDataDirective sets up the 'data_on_device' flag for
        each of the fields accessed.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        self._acc_dirs = self.ancestor(InvokeSchedule).walk(
                (ACCParallelDirective, ACCKernelsDirective))
        obj_list = []
        for pdir in self._acc_dirs:
            for var in pdir.fields:
                if var not in obj_list:
                    obj_list.append(var)

        read_routine_symbol = self._read_from_device_routine()

        for var in obj_list:
            symbol = self.scope.symbol_table.lookup(var)
            assignment = Assignment.create(
                StructureReference.create(symbol, ['data_on_device']),
                Literal("true", BOOLEAN_TYPE))
            self.parent.children.insert(self.position, assignment)

            # Use a CodeBlock to encode a Fortran pointer assignment
            reader = FortranReader()
            codeblock = reader.psyir_from_statement(
                f"{symbol.name}%read_from_device_f => "
                f"{read_routine_symbol.name}\n",
                self.scope.symbol_table)

            self.parent.children.insert(self.position, codeblock)

        return super().lower_to_language_level()


class GOKernelSchedule(KernelSchedule):
    '''
    Sub-classes KernelSchedule to provide a GOcean-specific implementation.

    :param str name: Kernel subroutine name
    '''
    # Polymorphic parameter to initialize the Symbol Table of the Schedule
    _symbol_table_class = GOSymbolTable


class GOHaloExchange(HaloExchange):
    '''GOcean specific halo exchange class which can be added to and
    manipulated in a schedule.

    :param field: the field that this halo exchange will act on.
    :type field: :py:class:`psyclone.gocean1p0.GOKernelArgument`
    :param bool check_dirty: optional argument default False (contrary to \
        its generic class - revisit in #856) indicating whether this halo \
        exchange should be subject to a run-time check for clean/dirty halos.
    :param parent: optional PSyIR parent node (default None) of this object.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`
    '''
    def __init__(self, field, check_dirty=False, parent=None):
        super().__init__(field, check_dirty=check_dirty, parent=parent)

        # Name of the HaloExchange method in the GOcean infrastructure.
        self._halo_exchange_name = "halo_exchange"

    def lower_to_language_level(self):
        '''
        In-place replacement of DSL or high-level concepts into generic
        PSyIR constructs. A GOHaloExchange is replaced by a call to the
        appropriate library method.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # TODO 856: Wrap Halo call with an is_dirty flag when necessary.

        # TODO 886: Currently only stencils of depth 1 are accepted by this
        # API, so the HaloExchange is hardcoded to depth 1.

        # Call the halo_exchange routine with depth argument to 1
        # Currently we create an symbol name with % as a workaround of not
        # having type bound routines.
        rsymbol = RoutineSymbol(self.field.name + "%" +
                                self._halo_exchange_name)
        call_node = Call.create(rsymbol, [Literal("1", INTEGER_TYPE)])
        self.replace_with(call_node)
        return call_node


# For Sphinx AutoAPI documentation generation
__all__ = ['GOPSy', 'GOInvokes', 'GOInvoke', 'GOInvokeSchedule', 'GOLoop',
           'GOBuiltInCallFactory', 'GOKernCallFactory', 'GOKern',
           'GOKernelArguments', 'GOKernelArgument',
           'GOKernelGridArgument', 'GOStencil', 'GO1p0Descriptor',
           'GOKernelType1p0', 'GOACCEnterDataDirective',
           'GOKernelSchedule', 'GOHaloExchange']
