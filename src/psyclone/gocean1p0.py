# pylint: disable=too-many-lines
# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2021, Science and Technology Facilities Council.
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

from __future__ import print_function
import re
import six

from fparser.common.readfortran import FortranStringReader
from fparser.common.sourceinfo import FortranFormat
from fparser.two.Fortran2003 import NoMatchError, Nonlabel_Do_Stmt, \
    Pointer_Assignment_Stmt
from fparser.two.parser import ParserFactory

from psyclone.configuration import Config, ConfigurationError
from psyclone.core import Signature
from psyclone.domain.gocean import GOceanConstants
from psyclone.errors import GenerationError, InternalError
import psyclone.expression as expr
from psyclone.f2pygen import CallGen, DeclGen, AssignGen, CommentGen, \
    IfThenGen, UseGen, ModuleGen, SubroutineGen, TypeDeclGen, PSyIRGen
from psyclone.parse.algorithm import Arg
from psyclone.parse.kernel import Descriptor, KernelType
from psyclone.parse.utils import ParseError
from psyclone.psyGen import PSy, Invokes, Invoke, InvokeSchedule, \
    CodedKern, Arguments, Argument, KernelArgument, args_filter, \
    AccessType, HaloExchange
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import Loop, Literal, Schedule, KernelSchedule, \
    StructureReference, BinaryOperation, Reference, Call, Assignment, \
    PSyDataNode, ACCEnterDataDirective, ACCParallelDirective, CodeBlock, \
    ACCKernelsDirective, Container, ACCUpdateDirective
from psyclone.psyir.symbols import SymbolTable, ScalarType, ArrayType, \
    INTEGER_TYPE, DataSymbol, RoutineSymbol, ContainerSymbol, DeferredType, \
    DataTypeSymbol, UnresolvedInterface, UnknownFortranType, BOOLEAN_TYPE, \
    REAL_TYPE


# Specify which OpenCL command queue to use for management operations like
# data transfers when generating an OpenCL PSy-layer
# TODO #1134: This value should be moved to the GOOpenCLTrans when the
# transformation logic is also moved there.
_OCL_MANAGEMENT_QUEUE = 1


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
                                "Meta-data error in kernel {0}: "
                                "INDEX_OFFSET of '{1}' does not match that "
                                "({2}) of other kernels. This is not "
                                "supported.".format(kern_call.name,
                                                    kern_call.index_offset,
                                                    offset))
                    # Append the index-offset of this kernel to the list of
                    # those seen so far
                    index_offsets.append(kern_call.index_offset)

    def gen_rank_expression(self, scope):
        ''' Generate the expression to retrieve the process rank.

        :param scope: where the expression is going to be located.
        :type scope: :py:class:`psyclone.f2pygen.BaseGen`
        :return: generate the Fortran expression to retrieve the process rank.
        :rtype: str
        '''
        scope.add(UseGen(scope, name="parallel_mod", only=True,
                         funcnames=["get_rank"]))
        return "get_rank()"

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
        # First check if there is any unsupported invoke, in this case the
        # f2pygen gen_code path is used instead
        for invoke in self.invoke_list:
            # TODO 1134: The opencl path is still largely implemented using
            # the f2pygen and cannot be processed by the backend yet.
            if invoke.schedule.opencl:
                super(GOInvokes, self).gen_code(parent)
                return

            # TODO 1168: PSyDataNodes are not supported by the backend yet.
            if invoke.schedule.root.walk(PSyDataNode):
                super(GOInvokes, self).gen_code(parent)
                return

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
    def unique_args_rscalars(self):
        '''
        :returns: the unique arguments that are scalars of type real \
                  (defined as those that are go_r_scalar 'space').
        :rtype: list of str.

        '''
        result = []
        for call in self._schedule.kernels():
            for arg in args_filter(call.arguments.args, arg_types=["scalar"],
                                   is_literal=False):
                if arg.space.lower() == "go_r_scalar" and \
                   arg.name not in result:
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
                                   is_literal=False):
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
        # Create the subroutine
        invoke_sub = SubroutineGen(parent, name=self.name,
                                   args=self.psy_unique_var_names)
        parent.add(invoke_sub)

        # Generate the code body of this subroutine
        self.schedule.gen_code(invoke_sub)

        # If we're generating an OpenCL routine then the arguments must
        # have the target attribute as we pass pointers to them in to
        # the OpenCL run-time.
        target = bool(self.schedule.opencl)

        # Add the subroutine argument declarations for fields
        if self.unique_args_arrays:
            my_decl_arrays = TypeDeclGen(invoke_sub, datatype="r2d_field",
                                         intent="inout", target=target,
                                         entity_decls=self.unique_args_arrays)
            invoke_sub.add(my_decl_arrays)

        # Add the subroutine argument declarations for integer and real scalars
        r_args = []
        i_args = []
        for argument in self.schedule.symbol_table.argument_datasymbols:
            if argument.name in self.unique_args_rscalars:
                r_args.append(argument.name)
            if argument.name in self.unique_args_iscalars:
                i_args.append(argument.name)

        if r_args:
            my_decl_rscalars = DeclGen(invoke_sub, datatype="REAL",
                                       intent="inout", kind="go_wp",
                                       entity_decls=r_args)
            invoke_sub.add(my_decl_rscalars)

        if i_args:
            my_decl_iscalars = DeclGen(invoke_sub, datatype="INTEGER",
                                       intent="inout",
                                       entity_decls=i_args)
            invoke_sub.add(my_decl_iscalars)

        # Add remaining local symbols using the symbol table
        for symbol in self.schedule.symbol_table.local_datasymbols:
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
class GOLoop(Loop):
    ''' The GOcean specific Loop class. This passes the GOcean specific
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
        const = GOceanConstants()

        Loop.__init__(self, parent=parent,
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
            raise GenerationError(
                "Invalid loop type of '{0}'. Expected one of {1}".
                format(self._loop_type, const.VALID_LOOP_TYPES))

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
        if not isinstance(my_field_space, six.string_types):
            raise TypeError(
                "Field space must be a 'str' but found '{0}' instead.".
                format(type(my_field_space).__name__))
        valid_fs = GOceanConstants().VALID_FIELD_GRID_TYPES + ['']
        if my_field_space not in valid_fs:
            raise ValueError(
                "Invalid string '{0}' provided for a GOcean field_space. The "
                "valid values are {1}".format(my_field_space, valid_fs))

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
        if not isinstance(it_space, six.string_types):
            raise TypeError(
                "Iteration space must be a 'str' but found '{0}' instead.".
                format(type(it_space).__name__))

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
        gocean1.0 API is for a certain offset type and field type. It defines
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
            raise InternalError("The parameter 'bound_info' must be a string, "
                                "got '{0}' (type {1})"
                                .format(bound_info, type(bound_info)))

        data = bound_info.split(":")
        if len(data) != 7:
            raise ConfigurationError("An iteration space must be in the form "
                                     "\"offset-type:field-type:"
                                     "iteration-space:outer-start:"
                                     "outer-stop:inner-start:inner-stop\"\n"
                                     "But got \"{0}\"".format(bound_info))

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
                    raise ConfigurationError("Only '{{start}}' and '{{stop}}' "
                                             "are allowed as bracketed "
                                             "expression in an iteration "
                                             "space. But got "
                                             "{0}".format(bracket_expr))

        # We need to make sure the fparser is properly initialised, which
        # typically has not yet happened when the config file is read.
        # Otherwise the Nonlabel_Do_Stmt cannot parse valid expressions.
        ParserFactory().create(std="f2008")

        # Test both the outer loop indices (index 3 and 4) and inner
        # indices (index 5 and 6):
        for bound in data[3:7]:
            do_string = "do i=1, {0}".format(bound)
            # Now replace any {start}/{stop} expression in the loop
            # with a valid integer value:
            do_string = do_string.format(start='15', stop='25')
            # Check if the do loop can be parsed as a nonlabel do loop
            try:
                _ = Nonlabel_Do_Stmt(do_string)
            except NoMatchError as err:
                six.raise_from(
                    ConfigurationError("Expression '{0}' is not a "
                                       "valid do loop boundary. Error "
                                       "message: '{1}'."
                                       .format(bound, str(err))), err)

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
        api_config = Config.get().api_conf("gocean1.0")
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
                "Cannot generate custom loop bound for loop {0}. Couldn't"
                " find any suitable field.".format(str(self)))

        if self.loop_type == "inner":
            prop_access = api_config.grid_properties["go_grid_xstop"]
        elif self.loop_type == "outer":
            prop_access = api_config.grid_properties["go_grid_ystop"]
        else:
            raise GenerationError(
                "Invalid loop type of '{0}'. Expected one of {1}".
                format(self.loop_type, GOceanConstants().VALID_LOOP_TYPES))

        stop_expr = prop_access.fortran.format(field.name)
        try:
            bound = self.bounds_lookup[self.index_offset][self.field_space][
                self.iteration_space][self.loop_type][side].format(
                    start='2', stop=stop_expr)
        except KeyError as err:
            six.raise_from(GenerationError(
                "Cannot generate custom loop bound for a loop with an index-"
                "offset of '{0}', a field-space of '{1}', an iteration-space "
                "of '{2}' and a loop-type of '{3}', for the side '{4}' because"
                " this keys combination does not exist in the "
                "GOLoop.bounds_lookup table."
                "".format(self.index_offset, self.field_space,
                          self.iteration_space, self.loop_type, side)), err)

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
                "Supplied grid property is a derived-type reference but does "
                "not begin with '{{0}}': '{0}'".format(grid_property))

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
            stop = BinaryOperation(BinaryOperation.Operator.SIZE)
            # Use the data property to access the member of the field that
            # contains the actual grid points.
            api_config = Config.get().api_conf("gocean1.0")
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
        props = Config.get().api_conf("gocean1.0").grid_properties
        if self.iteration_space.lower() == "go_internal_pts":
            return self._grid_property_psyir_expression(
                props["go_grid_{0}_{1}_stop".format(
                    "internal", self._loop_type)].fortran)
        if self.iteration_space.lower() == "go_all_pts":
            return self._grid_property_psyir_expression(
                props["go_grid_{0}_{1}_stop".format(
                    "whole", self._loop_type)].fortran)
        bound_str = self.get_custom_bound_string("stop")
        return FortranReader().psyir_from_expression(
                    bound_str, self.scope.symbol_table)

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
        props = Config.get().api_conf("gocean1.0").grid_properties
        if self.iteration_space.lower() == "go_internal_pts":
            return self._grid_property_psyir_expression(
                props["go_grid_{0}_{1}_start".format(
                    "internal", self._loop_type)].fortran)
        if self.iteration_space.lower() == "go_all_pts":
            return self._grid_property_psyir_expression(
                props["go_grid_{0}_{1}_start".format(
                    "whole", self._loop_type)].fortran)
        bound_str = self.get_custom_bound_string("start")
        return FortranReader().psyir_from_expression(
                    bound_str, self.scope.symbol_table)

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
                raise GenerationError("All Kernels must expect the same "
                                      "grid offset but kernel '{0}' has offset"
                                      " '{1}' which does not match '{2}'.".
                                      format(kernel.name,
                                             kernel.index_offset,
                                             index_offset))

    def gen_code(self, parent):
        ''' Create the f2pygen AST for this loop (and update the PSyIR
        representing the loop bounds if necessary).

        :param parent: the node in the f2pygen AST to which to add content.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        # Check that it is a properly formed GOLoop
        self._validate_loop()

        Loop.gen_code(self, parent)


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
        super(GOKern, self).__init__(GOKernelArguments, call, parent,
                                     check=False)
        # Pull out the grid index-offset that this kernel expects and
        # store it here. This is used to check that all of the kernels
        # invoked by an application are using compatible index offsets.
        self._index_offset = call.ktype.index_offset

    @staticmethod
    def _format_access(var_name, var_value, depth):
        '''This function creates an index-expression: if the value is
        negative, it returns 'varname-depth', if the value is positive,
        it returns 'varname+depth', otherwise it just returns 'varname'.
        This is used to create artificial stencil accesses for GOKernels.
        TODO #845: Return a proper PSyIR expression instead of a string.

        :param str var_name: name of the variable.
        :param int var_value: value of the variable, which determines the \
            direction (adding or subtracting depth).
        :param int depth: the depth of the access (>0).

        :returns: the index expression for an access in the given direction.
        :rtype: str

        '''
        if var_value < 0:
            return var_name + str(-depth)
        if var_value > 0:
            return var_name + "+" + str(depth)
        return var_name

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
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

        '''
        # Query each possible stencil direction and add a corresponding
        # variable accesses. Note that if (i,j) is accessed, the depth
        # returned will be 1, so one access to (i,j) is added.
        for j in [-1, 0, 1]:
            for i in [-1, 0, 1]:
                depth = arg.stencil.depth(i, j)
                for current_depth in range(1, depth+1):
                    i_expr = GOKern._format_access("i", i, current_depth)
                    j_expr = GOKern._format_access("j", j, current_depth)
                    var_accesses.add_access(signature, arg.access,
                                            self, [i_expr, j_expr])

    def reference_accesses(self, var_accesses):
        '''Get all variable access information. All accesses are marked
        according to the kernel metadata.

        :param var_accesses: VariablesAccessInfo instance that stores the\
            information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`

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
                    var_accesses.add_access(signature, arg.access,
                                            self, ["i", "j"])
        super(GOKern, self).reference_accesses(var_accesses)
        var_accesses.next_location()

    def load(self, call, parent=None):
        '''
        Populate the state of this GOKern object.

        :param call: information on the way in which this kernel is called \
                     from the Algorithm layer.
        :type call: :py:class:`psyclone.parse.algorithm.KernelCall`
        :param parent: the parent of this Kernel node in the PSyIR.
        :type parent: :py:class:`psyclone.gocean1p0.GOLoop`

        '''
        super(GOKern, self).__init__(GOKernelArguments, call, parent,
                                     check=False)

        # Pull out the grid index-offset that this kernel expects and
        # store it here. This is used to check that all of the kernels
        # invoked by an application are using compatible index offsets.
        self._index_offset = call.ktype.index_offset

    def local_vars(self):
        '''Return a list of the variable (names) that are local to this loop
        (and must therefore be e.g. threadprivate if doing OpenMP)

        '''
        return []

    def gen_code(self, parent):
        '''
        Generates GOcean v1.0 specific psy code for a call to the
        kernel instance. Also ensures that the kernel is written to file
        if it has been transformed.

        :param parent: parent node in the f2pygen AST being created.
        :type parent: :py:class:`psyclone.f2pygen.LoopGen`

        '''

        if self.ancestor(InvokeSchedule).opencl:
            # OpenCL is completely different so has its own gen method and
            # has to call the rename_and_write to generate to OpenCL files.
            self.rename_and_write()
            self.gen_ocl(parent)
        else:
            super(GOKern, self).gen_code(parent)

    def gen_ocl(self, parent):
        # pylint: disable=too-many-locals, too-many-statements
        '''
        Generates code for the OpenCL invocation of this kernel.

        :param parent: Parent node in the f2pygen AST to which to add content.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        # Include the check_status subroutine if we are in debug_mode
        api_config = Config.get().api_conf("gocean1.0")
        if api_config.debug_mode:
            parent.add(UseGen(parent, name="ocl_utils_mod", only=True,
                              funcnames=["check_status"]))

        # Generate code inside the first_time block to ensure the device
        # buffers are initialised and the data is on the device. A set_args
        # call is also inserted because in some platforms (e.g. Xiling FPGA)
        # knowing which arguments each kernel is going to use allows the write
        # operation to place the data into the appropriate memory bank.
        # This will create duplicate write operation for fields that are
        # repeated in multiple kernels, but the performance penalty is small
        # because it just happens during the first iteration.
        # TODO #1134: Explore removing duplicated write operations.
        ft_block = self.ancestor(InvokeSchedule)._first_time_block
        self.gen_ocl_buffers_initialisation(ft_block)
        self.gen_ocl_set_args_call(ft_block)
        self.gen_ocl_buffers_initial_write(ft_block)

        # Call the set_args again outside the first_time block in case some
        # value has changed between iterations. This doesn't seem to have any
        # performance penalty, but some of these could be removed for nicer
        # code generation.
        # TODO #1134: Explore removing unnecessary set_args calls.
        self.gen_ocl_set_args_call(parent)

        # Create array for the global work size argument of the kernel. Use the
        # InvokeSchedule symbol table to share this symbols for all the kernels
        # in the Invoke.
        symtab = self.ancestor(InvokeSchedule).symbol_table
        garg = self._arguments.find_grid_access()
        glob_size = symtab.new_symbol(
            "globalsize", symbol_type=DataSymbol,
            datatype=ArrayType(INTEGER_TYPE, [2])).name
        parent.add(DeclGen(parent, datatype="integer", target=True,
                           kind="c_size_t", entity_decls=[glob_size + "(2)"]))
        num_x = api_config.grid_properties["go_grid_nx"].fortran\
            .format(garg.name)
        num_y = api_config.grid_properties["go_grid_ny"].fortran\
            .format(garg.name)
        parent.add(AssignGen(parent, lhs=glob_size,
                             rhs="(/{0}, {1}/)".format(num_x, num_y)))

        # Create array for the local work size argument of the kernel
        local_size = symtab.new_symbol(
            "localsize", symbol_type=DataSymbol,
            datatype=ArrayType(INTEGER_TYPE, [2])).name
        parent.add(DeclGen(parent, datatype="integer", target=True,
                           kind="c_size_t", entity_decls=[local_size + "(2)"]))

        local_size_value = self._opencl_options['local_size']
        parent.add(AssignGen(parent, lhs=local_size,
                             rhs="(/{0}, 1/)".format(local_size_value)))

        # Check that the global_size is multiple of the local_size
        if api_config.debug_mode:
            condition = "mod({0}, {1}) .ne. 0".format(num_x, local_size_value)
            ifthen = IfThenGen(parent, condition)
            parent.add(ifthen)
            message = ('"Global size is not a multiple of local size ('
                       'mandatory in OpenCL < 2.0)."')
            # Since there is no print and break functionality in f2pygen, we
            # use the check_status function here, this could be improved when
            # translating to PSyIR.
            ifthen.add(CallGen(ifthen, "check_status", [message, '-1']))

        # Retrieve kernel name
        kernel = symtab.lookup_with_tag("kernel_" + self.name).name

        # Get the name of the list of command queues (set in
        # psyGen.InvokeSchedule)
        qlist = symtab.lookup_with_tag("opencl_cmd_queues").name
        flag = symtab.lookup_with_tag("opencl_error").name

        # Choose the command queue number to which to dispatch this kernel. We
        # have do deal with possible dependencies to kernels dispatched in
        # different command queues as the order of execution is not guaranteed.
        queue_number = self._opencl_options['queue_number']
        cmd_queue = qlist + "({0})".format(queue_number)
        outer_loop = self.parent.parent.parent.parent
        dependency = outer_loop.backward_dependence()

        # If the dependency is a loop containing a kernel, add a barrier if the
        # previous kernels were dispatched in a different command queue
        if dependency and dependency.coded_kernels():
            for kernel_dep in dependency.coded_kernels():
                # TODO #1134: The OpenCL options should not leak from the
                # OpenCL transformation.
                # pylint: disable=protected-access
                previous_queue = kernel_dep._opencl_options['queue_number']
                if previous_queue != queue_number:
                    # If the backward dependency is being executed in another
                    # queue we add a barrier to make sure the previous kernel
                    # has finished before this one starts.
                    parent.add(AssignGen(
                        parent,
                        lhs=flag,
                        rhs="clFinish({0}({1}))".format(
                            qlist, str(previous_queue))))

        # If the dependency is something other than a kernel, currently we
        # dispatch everything else to queue _OCL_MANAGEMENT_QUEUE, so add a
        # barrier if this kernel is not on queue _OCL_MANAGEMENT_QUEUE.
        if dependency and not dependency.coded_kernels() and \
                queue_number != _OCL_MANAGEMENT_QUEUE:
            parent.add(AssignGen(
                parent,
                lhs=flag,
                rhs="clFinish({0}({1}))".format(
                    qlist, str(_OCL_MANAGEMENT_QUEUE))))

        if api_config.debug_mode:
            # Check that everything has succeeded before the kernel launch,
            # since kernel executions are asynchronous, we insert a clFinish
            # command as a barrier to make sure everything until here has been
            # executed.
            parent.add(AssignGen(parent, lhs=flag,
                                 rhs="clFinish(" + cmd_queue + ")"))
            parent.add(CallGen(
                parent, "check_status",
                ["'Errors before {0} launch'".format(self.name), flag]))

        # Then we call clEnqueueNDRangeKernel
        parent.add(CommentGen(parent, " Launch the kernel"))
        cnull = "C_NULL_PTR"
        args = ", ".join([
            # OpenCL Command Queue
            cmd_queue,
            # OpenCL Kernel object
            kernel,
            # Number of work dimensions
            "2",
            # Global offset (if NULL the global IDs start at offset (0,0,0))
            cnull,
            # Global work size
            "C_LOC({0})".format(glob_size),
            # Local work size
            "C_LOC({0})".format(local_size),
            # Number of events in wait list
            "0",
            # Event wait list that need to be completed before this kernel
            cnull,
            # Event that identifies this kernel completion
            cnull])
        parent.add(AssignGen(parent, lhs=flag,
                             rhs="clEnqueueNDRangeKernel({0})".format(args)))
        parent.add(CommentGen(parent, ""))

        # Add additional checks if we are in debug mode
        if api_config.debug_mode:
            parent.add(CallGen(
                parent, "check_status",
                ["'{0} clEnqueueNDRangeKernel'".format(self.name), flag]))

            # Add a barrier and check that the kernel executed successfully
            parent.add(AssignGen(parent, lhs=flag,
                                 rhs="clFinish(" + cmd_queue + ")"))
            parent.add(CallGen(
                parent, "check_status",
                ["'Errors during {0}'".format(self.name), flag]))

    @property
    def index_offset(self):
        ''' The grid index-offset convention that this kernel expects '''
        return self._index_offset

    def gen_arg_setter_code(self, parent):
        '''
        Creates a Fortran routine to set the arguments of the OpenCL
        version of this kernel.

        :param parent: Parent node of the set-kernel-arguments routine
        :type parent: :py:class:`psyclone.f2pygen.moduleGen`
        '''
        # pylint: disable=too-many-locals, too-many-statements
        # The arg_setter code is in a subroutine, so we create a new scope
        argsetter_st = SymbolTable()

        # Add an argument symbol for the kernel object
        kobj = argsetter_st.new_symbol("kernel_obj").name

        # Keep track of the argument names
        argument_names = [arg.name for arg in self.arguments.args]

        # Declare the subroutine in the Invoke SymbolTable and the argsetter
        # subroutine SymbolTable. Subroutine names should be an exact match.
        sub_name = self.name + "_set_args"
        try:
            self.root.symbol_table.lookup(sub_name)
        except KeyError:
            self.root.symbol_table.add(RoutineSymbol(sub_name), tag=sub_name)
        argsetter_st.add(RoutineSymbol(sub_name), tag=sub_name)

        # Create the f2pygen Subroutine node, the subroutine arguments are not
        # provided yet as they have to be processed to avoid name clashes
        sub = SubroutineGen(parent, name=sub_name)
        parent.add(sub)

        def resolve_argument_names(args):
            ''' Utility to declare the given arguments in the argsetter_st and
            if any name is already used, update the argument_names list with
            a new name to avoid the clash.

            :params args: A subset of arguments from self.arguments.args
            :type args: list of :py:class:`psyclone.psyGen.Arguments`

            :returns: Updated name list for the arguments
            :rtype: list of str
            '''
            names = []
            for arg in args:
                name = argsetter_st.new_symbol(arg.name).name
                argument_names[self.arguments.args.index(arg)] = name
                names.append(name)
            return names

        # Add module imports
        sub.add(UseGen(sub, name="ocl_utils_mod", only=True,
                       funcnames=["check_status"]))
        sub.add(UseGen(sub, name="iso_c_binding", only=True,
                       funcnames=["c_sizeof", "c_loc", "c_intptr_t"]))
        sub.add(UseGen(sub, name="clfortran", only=True,
                       funcnames=["clSetKernelArg"]))

        # Declare arguments
        sub.add(DeclGen(sub, datatype="integer", kind="c_intptr_t",
                        target=True, entity_decls=[kobj]))

        # Get all Grid property arguments
        grid_prop_args = args_filter(self._arguments.args,
                                     arg_types=["field", "grid_property"])

        # Array grid properties are c_intptr_t
        args = [x for x in grid_prop_args if not x.is_scalar]
        if args:
            names = resolve_argument_names(args)
            sub.add(DeclGen(sub, datatype="integer", kind="c_intptr_t",
                            intent="in", target=True, entity_decls=names))

        # Scalar integer grid properties
        args = [x for x in grid_prop_args
                if x.is_scalar and x.intrinsic_type == "integer"]

        if args:
            names = resolve_argument_names(args)
            sub.add(DeclGen(sub, datatype="integer", intent="in",
                            target=True, entity_decls=names))

        # Scalar real grid properties
        args = [x for x in grid_prop_args
                if x.is_scalar and x.intrinsic_type == "real"]
        if args:
            names = resolve_argument_names(args)
            sub.add(DeclGen(sub, datatype="real", intent="in", kind="go_wp",
                            target=True, entity_decls=names))

        # Scalar arguments
        scalar_args = args_filter(self._arguments.args, arg_types=["scalar"],
                                  is_literal=False)
        go_r_scalars = []
        other_scalars = []
        for arg in scalar_args:
            # Use symbol table to avoid name clashes
            name = argsetter_st.new_symbol(arg.name).name
            argument_names[self.arguments.args.index(arg)] = name
            if arg.space.lower() == "go_r_scalar":
                go_r_scalars.append(name)
            else:
                other_scalars.append(name)
        if go_r_scalars:
            sub.add(DeclGen(
                sub, datatype="REAL", intent="in", kind="go_wp",
                target=True, entity_decls=go_r_scalars))
        if other_scalars:
            sub.add(DeclGen(sub, datatype="INTEGER", intent="in",
                            target=True, entity_decls=other_scalars))

        # Declare local variables
        err_name = argsetter_st.new_symbol(
            "ierr", symbol_type=DataSymbol, datatype=INTEGER_TYPE).name
        sub.add(DeclGen(sub, datatype="integer", entity_decls=[err_name]))

        # Set kernel arguments
        sub.add(CommentGen(
            sub,
            " Set the arguments for the {0} OpenCL Kernel".format(self.name)))
        index = 0
        # Add a SetKenrelArg for each Argument and check the OpenCL status
        for variable in argument_names:
            sub.add(AssignGen(
                sub, lhs=err_name,
                rhs="clSetKernelArg({0}, {1}, C_SIZEOF({2}), C_LOC({2}))".
                format(kobj, index, variable)))
            sub.add(CallGen(
                sub, "check_status",
                ["'clSetKernelArg: arg {0} of {1}'".format(index, self.name),
                 err_name]))
            index = index + 1

        # Finally we can provide the updated argument list without name clashes
        sub.args = [kobj] + argument_names

    def gen_ocl_buffers_initialisation(self, parent):
        # pylint: disable=too-many-locals
        '''
        Generate code to create data buffers on OpenCL device.

        :param parent: Parent subroutine in f2pygen AST of generated code.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`
        '''
        # Get the InvokeSchedule symbol table and the root f2pygen node
        symtab = self.ancestor(InvokeSchedule).symbol_table
        module = parent
        while module.parent:
            module = module.parent

        # Traverse all arguments and make sure the buffers are initialised
        for arg in self._arguments.args:
            if arg.argument_type == "field":
                # Get the init_buffer routine and insert a call for this field
                init_buf = self.gen_ocl_initialise_buffer(module)
                field = symtab.lookup(arg.name)
                call = Call.create(init_buf, [Reference(field)])

                # TODO #1134: Currently we convert back the PSyIR to f2pygen
                # but when using the PSyIR backend this will be removed.
                parent.add(PSyIRGen(parent, call))

            elif arg.argument_type == "grid_property" and not arg.is_scalar:
                # Get the grid init_buffer routine and insert a call
                init_buf = self.gen_ocl_initialise_grid_buffers(module)
                field = symtab.lookup(self._arguments.find_grid_access().name)
                call = Call.create(init_buf, [Reference(field)])

                # TODO #1134: Currently we convert back the PSyIR to f2pygen
                # but when using the PSyIR backend this will be removed.
                parent.add(PSyIRGen(parent, call))

            if not arg.is_scalar:
                # All buffers will be assigned to a local OpenCL memory object
                # to easily reference them, make sure this local variable is
                # declared in the Invoke.
                name = arg.name + "_cl_mem"
                try:
                    symtab.lookup_with_tag(name)
                except KeyError:
                    symtab.new_symbol(
                        name, tag=name, symbol_type=DataSymbol,
                        # TODO #1134: We could import the kind symbols from a
                        # iso_c_binding global container.
                        datatype=UnknownFortranType("INTEGER(KIND=c_intptr_t)"
                                                    " :: " + name))
                    parent.add(DeclGen(parent, datatype="integer",
                                       kind="c_intptr_t", entity_decls=[name]))

    def gen_ocl_buffers_initial_write(self, parent):
        # pylint: disable=too-many-locals
        '''
        Generate the f2pygen AST for the code to write the initial data into
        the device.

        :param parent: parent subroutine in f2pygen AST of generated code.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`
        '''
        symtab = self.scope.symbol_table
        there_is_a_grid_buffer = False
        for arg in self._arguments.args:
            if arg.argument_type == "field":
                # Insert call to write_to_device method
                call = Call.create(
                    RoutineSymbol(arg.name+"%write_to_device()"), [])
                parent.add(PSyIRGen(parent, call))
            elif arg.argument_type == "grid_property" and not arg.is_scalar:
                there_is_a_grid_buffer = True

        if there_is_a_grid_buffer:
            module = parent
            while module.parent:
                module = module.parent
            grid_write_routine = self.gen_ocl_write_grid_buffers(module)

            # Insert grid writing call
            field = symtab.lookup(self._arguments.find_grid_access().name)
            call = Call.create(grid_write_routine, [Reference(field)])
            parent.add(PSyIRGen(parent, call))

    def gen_ocl_set_args_call(self, parent):
        '''
        Generate the f2pygen AST for the code to call the set_args subroutine
        for this kernel.

        :param parent: parent subroutine in f2pygen AST of generated code.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        # pylint: disable=too-many-locals, too-many-branches
        # Retrieve symbol table and kernel name
        symtab = self.scope.symbol_table
        kernel = symtab.lookup_with_tag("kernel_" + self.name).name

        # Find the symbol that defines each boundary for this kernel.
        # In OpenCL the iteration boundaries are passed as arguments to the
        # kernel because the global work size may exceed the dimensions and
        # therefore the updates outside the boundaries should be masked.
        # If any of the boundaries is not found, it can not proceed.
        boundaries = []
        try:
            for boundary in ["xstart", "xstop", "ystart", "ystop"]:
                tag = boundary + "_" + self.name
                symbol = symtab.lookup_with_tag(tag)
                boundaries.append(symbol.name)
        except KeyError as err:
            six.raise_from(GenerationError(
                "Boundary symbol tag '{0}' not found while generating the "
                "OpenCL code for kernel '{1}'. Make sure to apply the "
                "GOMoveIterationBoundariesInsideKernelTrans before attempting"
                " the OpenCL code generation.".format(tag, self.name)), err)

        # If this is an IfBlock, make a copy of boundary assignments statements
        # to make sure they are initialised before calling the set_args routine
        # that have them as a parameter.
        # TODO #1134: If everything was PSyIR we could probably move this
        # assignment before the IfBlock instead of duplicating it inside.
        if isinstance(parent, IfThenGen):
            for node in self.ancestor(InvokeSchedule).walk(Assignment):
                if node.lhs.symbol.name in boundaries:
                    parent.add(PSyIRGen(parent, node.copy()))

        # Prepare the argument list for the set_args routine
        arguments = [kernel]
        for arg in self._arguments.args:
            if arg.argument_type == "scalar":
                if arg.name in boundaries:
                    # Boundary values are 0-indexed in OpenCL
                    arguments.append(arg.name + " - 1")
                else:
                    arguments.append(arg.name)
            elif arg.argument_type == "field":
                # Cast buffer to cl_mem type expected by OpenCL
                field = symtab.lookup(arg.name)
                symbol = symtab.lookup_with_tag(arg.name + "_cl_mem")
                source = StructureReference.create(field, ['device_ptr'])
                dest = Reference(symbol)
                bop = BinaryOperation.create(BinaryOperation.Operator.CAST,
                                             source, dest)
                assig = Assignment.create(dest.copy(), bop)
                parent.add(PSyIRGen(parent, assig))
                arguments.append(symbol.name)
            elif arg.argument_type == "grid_property":
                garg = self._arguments.find_grid_access()
                if arg.is_scalar:
                    arguments.append(arg.dereference(garg.name))
                else:
                    # Cast grid buffer to cl_mem type expected by OpenCL
                    device_grid_property = arg.name + "_device"
                    field = symtab.lookup(garg.name)
                    source = StructureReference.create(
                                field, ['grid', device_grid_property])
                    symbol = symtab.lookup_with_tag(arg.name + "_cl_mem")
                    dest = Reference(symbol)
                    bop = BinaryOperation.create(BinaryOperation.Operator.CAST,
                                                 source, dest)
                    assig = Assignment.create(dest.copy(), bop)
                    parent.add(PSyIRGen(parent, assig))
                    arguments.append(symbol.name)

        sub_name = symtab.lookup_with_tag(self.name + "_set_args").name
        parent.add(CallGen(parent, sub_name, arguments))

    def gen_ocl_write_grid_buffers(self, f2pygen_module):
        '''
        Returns the symbol of a subroutine that writes the values of the grid
        properties into the OpenCL device buffers using FortCL. If the
        subroutine doesn't already exist it is generated in the supplied
        f2pygen module.

        :param f2pygen_module: the module where the new function will be \
                               inserted.
        :param type: :py:class:`psyclone.f2pygen.ModuleGen`

        :returns: the symbol representing the grid buffers writing subroutine.
        :rtype: :py:class:`psyclone.psyir.symbols.RoutineSymbol`

        '''
        # pylint: disable=too-many-locals
        symtab = self.root.symbol_table
        try:
            return symtab.lookup_with_tag("ocl_write_grid_buffers")
        except KeyError:
            # If the Symbol does not exist, the rest of this method
            # will generate it.
            pass

        # Create the symbol for the routine and add it to the symbol table.
        subroutine_name = symtab.new_symbol(
            "write_grid_buffers", symbol_type=RoutineSymbol,
            tag="ocl_write_grid_buffers").name

        # Get the GOcean API property names used in this routine
        api_config = Config.get().api_conf("gocean1.0")
        props = api_config.grid_properties
        num_x = props["go_grid_nx"].fortran.format("field")
        num_y = props["go_grid_ny"].fortran.format("field")

        # Code of the subroutine in Fortran
        code = '''
        subroutine write_device_grid(field)
            USE fortcl, ONLY: get_cmd_queues
            use iso_c_binding, only: c_intptr_t, c_size_t, c_sizeof
            USE clfortran
            USE ocl_utils_mod, ONLY: check_status
            type(r2d_field), intent(inout), target :: field
            integer(kind=c_size_t) size_in_bytes
            INTEGER(c_intptr_t), pointer :: cmd_queues(:)
            integer(c_intptr_t) :: cl_mem
            integer :: ierr
            cmd_queues => get_cmd_queues()
            ! Integer grid buffers
            size_in_bytes = int({0} * {1}, 8) * &
                            c_sizeof(field%grid%tmask(1,1))
            cl_mem = transfer(field%grid%tmask_device, cl_mem)
            ierr = clEnqueueWriteBuffer(cmd_queues({2}), &
                        cl_mem, CL_TRUE, 0_8, size_in_bytes, &
                        C_LOC(field%grid%tmask), 0, C_NULL_PTR, C_NULL_PTR)
            CALL check_status("clEnqueueWriteBuffer tmask", ierr)
            ! Real grid buffers
            size_in_bytes = int({0} * {1}, 8) * &
                            c_sizeof(field%grid%area_t(1,1))
        '''.format(num_x, num_y, _OCL_MANAGEMENT_QUEUE)
        write_str = '''
            cl_mem = transfer(field%grid%{0}_device, cl_mem)
            ierr = clEnqueueWriteBuffer(cmd_queues({1}), &
                       cl_mem, CL_TRUE, 0_8, size_in_bytes, &
                       C_LOC(field%grid%{0}), 0, C_NULL_PTR, C_NULL_PTR)
            CALL check_status("clEnqueueWriteBuffer {0}_device", ierr)
        '''
        for grid_prop in ['area_t', 'area_u', 'area_v', 'dx_u', 'dx_v',
                          'dx_t', 'dy_u', 'dy_v', 'dy_t', 'gphiu', 'gphiv']:
            code += write_str.format(grid_prop, _OCL_MANAGEMENT_QUEUE)
        code += "end subroutine write_device_grid"

        # Obtain the PSyIR representation of the code above
        fortran_reader = FortranReader()
        container = fortran_reader.psyir_from_source(code)
        subroutine = container.children[0]
        # Rename subroutine
        subroutine.name = subroutine_name

        # Insert the code in the invoke module
        f2pygen_module.add(PSyIRGen(f2pygen_module, subroutine))

        return symtab.lookup_with_tag("ocl_write_grid_buffers")

    def gen_ocl_initialise_buffer(self, f2pygen_module):
        '''
        Returns the symbol of a subroutine that initialises a OpenCL buffer in
        the OpenCL device using FortCL. If the subroutine doesn't already exist
        it is generated in the supplied f2pygen module.

        :param f2pygen_module: the module where the new function will be \
                               inserted.
        :param type: :py:class:`psyclone.f2pygen.ModuleGen`

        :returns: the symbol of the buffer initialisation subroutine.
        :rtype: :py:class:`psyclone.psyir.symbols.RoutineSymbol`

        '''
        # pylint: disable=too-many-locals
        symtab = self.root.symbol_table
        try:
            return symtab.lookup_with_tag("ocl_init_buffer_func")
        except KeyError:
            # If the Symbol does not exist, the rest of this method
            # will generate it.
            pass

        # Create the symbol for the routine and add it to the symbol table.
        subroutine_name = symtab.new_symbol(
            "initialise_device_buffer", symbol_type=RoutineSymbol,
            tag="ocl_init_buffer_func").name

        # Get the GOcean API property names used in this routine
        api_config = Config.get().api_conf("gocean1.0")
        host_buff = \
            api_config.grid_properties["go_grid_data"].fortran.format("field")
        props = api_config.grid_properties
        num_x = props["go_grid_nx"].fortran.format("field")
        num_y = props["go_grid_ny"].fortran.format("field")

        # Fields need to provide a function pointer to how the
        # device data is going to be read and written, if it doesn't
        # exist, create the appropriate subroutine first.
        read_fp = self.gen_ocl_read_from_device_function(f2pygen_module).name
        write_fp = self.gen_ocl_write_to_device_function(f2pygen_module).name

        # Code of the subroutine in Fortran
        code = '''
        subroutine initialise_device_buffer(field)
            USE fortcl, ONLY: create_rw_buffer
            use field_mod
            type(r2d_field), intent(inout), target :: field
            integer(kind=c_size_t) size_in_bytes
            IF (.NOT. field%data_on_device) THEN
                size_in_bytes = int({0}*{1}, 8) * &
                                    c_sizeof({2}(1,1))
                ! Create buffer on device, we store it without type information
                ! on the dl_esm_inf pointer (transfer/static_cast to void*)
                field%device_ptr = transfer( &
                    create_rw_buffer(size_in_bytes), &
                    field%device_ptr)
                field%data_on_device = .true.
                field%read_from_device_f => {3}
                field%write_to_device_f => {4}
            END IF
        end subroutine initialise_device_buffer
        '''.format(num_x, num_y, host_buff, read_fp, write_fp)

        # Obtain the PSyIR representation of the code above
        fortran_reader = FortranReader()
        container = fortran_reader.psyir_from_source(code)
        subroutine = container.children[0]
        # Rename subroutine
        subroutine.name = subroutine_name

        # Insert the code in the invoke module
        f2pygen_module.add(PSyIRGen(f2pygen_module, subroutine))

        return symtab.lookup_with_tag("ocl_init_buffer_func")

    def gen_ocl_initialise_grid_buffers(self, f2pygen_module):
        '''
        Returns the symbol of a subroutine that initialises all OpenCL grid
        buffers in the OpenCL device using FortCL. If the subroutine doesn't
        already exist it is generated in the supplied f2pygen module.

        :param f2pygen_module: the module where the new function will be \
                               inserted.
        :param type: :py:class:`psyclone.f2pygen.ModuleGen`

        :returns: the symbol of the grid buffer initialisation subroutine.
        :rtype: :py:class:`psyclone.psyir.symbols.RoutineSymbol`

        '''
        # pylint: disable=too-many-locals
        symtab = self.root.symbol_table
        try:
            return symtab.lookup_with_tag("ocl_init_grid_buffers")
        except KeyError:
            # If the Symbol does not exist, the rest of this method
            # will generate it.
            pass

        # Create the symbol for the routine and add it to the symbol table.
        subroutine_name = symtab.new_symbol(
            "initialise_grid_device_buffers", symbol_type=RoutineSymbol,
            tag="ocl_init_grid_buffers").name

        # Get the GOcean API property names used in this routine
        api_config = Config.get().api_conf("gocean1.0")
        props = api_config.grid_properties
        num_x = props["go_grid_nx"].fortran.format("field")
        num_y = props["go_grid_ny"].fortran.format("field")

        int_arrays = []
        real_arrays = []
        for key, prop in props.items():
            if key == "go_grid_data":
                # TODO #676: Ignore because go_grid_data is actually a field
                # property
                continue
            if prop.type == "array" and prop.intrinsic_type == "integer":
                int_arrays.append(prop.fortran.format("field"))
            elif prop.type == "array" and prop.intrinsic_type == "real":
                real_arrays.append(prop.fortran.format("field"))

        # Code of the subroutine in Fortran
        code = '''
        subroutine initialise_device_grid(field)
            USE fortcl, ONLY: create_ronly_buffer
            use field_mod
            type(r2d_field), intent(inout), target :: field
            integer(kind=c_size_t) size_in_bytes
            IF (.not. c_associated({2}_device)) THEN
                ! Create integer grid fields
                size_in_bytes = int({0}*{1}, 8) * c_sizeof({2}(1,1))
        '''.format(num_x, num_y, int_arrays[0])

        for int_array in int_arrays:
            code += '''
                {0}_device = transfer(create_ronly_buffer(size_in_bytes), &
                                      {0}_device)
            '''.format(int_array)

        code += '''
                ! Create real grid buffers
                size_in_bytes = int({0} * {1}, 8) * c_sizeof({2}(1,1))
        '''.format(num_x, num_y, real_arrays[0])

        for real_array in real_arrays:
            code += '''
                {0}_device = transfer(create_ronly_buffer(size_in_bytes), &
                                      {0}_device)
            '''.format(real_array)

        code += '''
            END IF
        end subroutine initialise_device_grid
        '''

        # Obtain the PSyIR representation of the code above
        fortran_reader = FortranReader()
        container = fortran_reader.psyir_from_source(code)
        subroutine = container.children[0]
        # Rename subroutine
        subroutine.name = subroutine_name

        # Insert the code in the invoke module
        f2pygen_module.add(PSyIRGen(f2pygen_module, subroutine))

        return symtab.lookup_with_tag("ocl_init_grid_buffers")

    def gen_ocl_read_from_device_function(self, f2pygen_module):
        '''
        Returns the symbol of a subroutine that retrieves the data back from
        an OpenCL device using FortCL. If the subroutine doesn't already exist
        it is generated in the supplied f2pygen module.

        :param f2pygen_module: the module where the new function will be \
                               inserted.
        :param type: :py:class:`psyclone.f2pygen.ModuleGen`

        :returns: the symbol of the buffer data retrieving subroutine.
        :rtype: :py:class:`psyclone.psyir.symbols.RoutineSymbol`

        '''
        symtab = self.root.symbol_table
        try:
            return symtab.lookup_with_tag("ocl_read_func")
        except KeyError:
            # If the subroutines does not exist, it needs to be
            # generated first.
            pass

        # Create the symbol for the routine and add it to the symbol table.
        subroutine_name = symtab.new_symbol(
            "read_from_device", symbol_type=RoutineSymbol,
            tag="ocl_read_func").name

        # Code of the subroutine in Fortran
        code = '''
        subroutine read_sub(from, to, startx, starty, nx, ny, blocking)
            USE iso_c_binding, only: c_ptr, c_intptr_t, c_size_t, c_sizeof
            USE ocl_utils_mod, ONLY: check_status
            use kind_params_mod, only: go_wp
            USE clfortran
            USE fortcl, ONLY: get_cmd_queues
            type(c_ptr), intent(in) :: from
            real(go_wp), intent(inout), dimension(:,:), target :: to
            integer, intent(in) :: startx, starty, nx, ny
            logical, intent(in) :: blocking
            INTEGER(c_size_t) :: size_in_bytes, offset_in_bytes
            integer(c_intptr_t) :: cl_mem
            INTEGER(c_intptr_t), pointer :: cmd_queues(:)
            integer :: ierr, i

            ! Give the from pointer the appropriate OpenCL memory object type
            cl_mem = transfer(from, cl_mem)
            cmd_queues => get_cmd_queues()

            ! Two copy strategies depending on how much of the total length
            ! nx covers.
            if (nx < size(to, 1) / 2) then
                ! Dispatch asynchronous copies of just the contiguous data.
                do i = starty, starty+ny
                    size_in_bytes = int(nx, 8) * c_sizeof(to(1,1))
                    offset_in_bytes = int(size(to, 1) * (i-1) + (startx-1)) &
                                      * c_sizeof(to(1,1))
                    ierr = clEnqueueReadBuffer(cmd_queues({0}), cl_mem, &
                        CL_FALSE, offset_in_bytes, size_in_bytes, &
                        C_LOC(to(startx, i)), 0, C_NULL_PTR, C_NULL_PTR)
                    CALL check_status("clEnqueueReadBuffer", ierr)
                enddo
                if (blocking) then
                    CALL check_status("clFinish on read", &
                        clFinish(cmd_queues({0})))
                endif
            else
                ! Copy across the whole starty:starty+ny rows in a single
                ! copy operation.
                size_in_bytes = int(size(to, 1) * ny, 8) * c_sizeof(to(1,1))
                offset_in_bytes = int(size(to,1)*(starty-1), 8) &
                                  * c_sizeof(to(1,1))
                ierr = clEnqueueReadBuffer(cmd_queues({0}), cl_mem, &
                    CL_TRUE, offset_in_bytes, size_in_bytes, &
                    C_LOC(to(1,starty)), 0, C_NULL_PTR, C_NULL_PTR)
                CALL check_status("clEnqueueReadBuffer", ierr)
            endif
        end subroutine read_sub
        '''.format(_OCL_MANAGEMENT_QUEUE)

        # Obtain the PSyIR representation of the code above
        fortran_reader = FortranReader()
        container = fortran_reader.psyir_from_source(code)
        subroutine = container.children[0]

        # Rename subroutine
        subroutine.name = subroutine_name

        # Insert the code in the invoke module
        f2pygen_module.add(PSyIRGen(f2pygen_module, subroutine))

        return symtab.lookup_with_tag("ocl_read_func")

    def gen_ocl_write_to_device_function(self, f2pygen_module):
        '''
        Returns the symbol of a subroutine that writes the buffer data into
        an OpenCL device using FortCL. If the subroutine doesn't already exist
        it is generated in the supplied f2pygen module.

        :param f2pygen_module: the module where the new function will be \
                               inserted.
        :param type: :py:class:`psyclone.f2pygen.ModuleGen`

        :returns: the symbol of the buffer writing subroutine.
        :rtype: :py:class:`psyclone.psyir.symbols.RoutineSymbol`

        '''
        symtab = self.root.symbol_table
        try:
            return symtab.lookup_with_tag("ocl_write_func")
        except KeyError:
            # If the subroutines does not exist, it needs to be
            # generated first.
            pass

        # Create the symbol for the routine and add it to the symbol table.
        subroutine_name = symtab.new_symbol(
            "write_to_device", symbol_type=RoutineSymbol,
            tag="ocl_write_func").name

        # Code of the subroutine in Fortran
        code = '''
        subroutine write_sub(from, to, startx, starty, nx, ny, blocking)
            USE iso_c_binding, only: c_ptr, c_intptr_t, c_size_t, c_sizeof
            USE ocl_utils_mod, ONLY: check_status
            use kind_params_mod, only: go_wp
            USE clfortran
            USE fortcl, ONLY: get_cmd_queues
            real(go_wp), intent(in), dimension(:,:), target :: from
            type(c_ptr), intent(in) :: to
            integer, intent(in) :: startx, starty, nx, ny
            logical, intent(in) :: blocking
            integer(c_intptr_t) :: cl_mem
            INTEGER(c_size_t) :: size_in_bytes, offset_in_bytes
            INTEGER(c_intptr_t), pointer :: cmd_queues(:)
            integer :: ierr, i

            ! Give the to pointer the appropriate OpenCL memory object type
            cl_mem = transfer(to, cl_mem)
            cmd_queues => get_cmd_queues()

            ! Two copy strategies depending on how much of the total length
            ! nx covers.
            if (nx < size(from,1) / 2) then
                ! Dispatch asynchronous copies of just the contiguous data.
                do i=starty, starty+ny
                    size_in_bytes = int(nx, 8) * c_sizeof(from(1,1))
                    offset_in_bytes = int(size(from, 1) * (i-1) + (startx-1)) &
                                      * c_sizeof(from(1,1))
                    ierr = clEnqueueWriteBuffer(cmd_queues({0}), cl_mem, &
                        CL_FALSE, offset_in_bytes, size_in_bytes, &
                        C_LOC(from(startx, i)), 0, C_NULL_PTR, C_NULL_PTR)
                    CALL check_status("clEnqueueWriteBuffer", ierr)
                enddo
                if (blocking) then
                    CALL check_status("clFinish on write", &
                        clFinish(cmd_queues({0})))
                endif
            else
                ! Copy across the whole starty:starty+ny rows in a single
                ! copy operation.
                size_in_bytes = int(size(from,1) * ny, 8) * c_sizeof(from(1,1))
                offset_in_bytes = int(size(from,1) * (starty-1)) &
                                  * c_sizeof(from(1,1))
                ierr = clEnqueueWriteBuffer(cmd_queues({0}), cl_mem, &
                    CL_TRUE, offset_in_bytes, size_in_bytes, &
                    C_LOC(from(1, starty)), 0, C_NULL_PTR, C_NULL_PTR)
                CALL check_status("clEnqueueWriteBuffer", ierr)
            endif
        end subroutine write_sub
        '''.format(_OCL_MANAGEMENT_QUEUE)

        # Obtain the PSyIR representation of the code above
        fortran_reader = FortranReader()
        container = fortran_reader.psyir_from_source(code)
        subroutine = container.children[0]
        # Rename subroutine
        subroutine.name = subroutine_name

        # Insert the code in the invoke module
        f2pygen_module.add(PSyIRGen(f2pygen_module, subroutine))

        return symtab.lookup_with_tag("ocl_write_func")

    def get_kernel_schedule(self):
        '''
        Returns a PSyIR Schedule representing the GOcean kernel code.

        :return: Schedule representing the kernel code.
        :rtype: :py:class:`psyclone.gocean1p0.GOKernelSchedule`
        '''
        if self._kern_schedule is None:
            astp = GOFparser2Reader()
            self._kern_schedule = astp.generate_schedule(self.name, self.ast)
            # TODO: Validate kernel with metadata (issue #288).
        return self._kern_schedule


class GOFparser2Reader(Fparser2Reader):
    '''
    Sub-classes the Fparser2Reader with GOcean 1.0 specific
    functionality.
    '''
    @staticmethod
    def _create_schedule(name):
        '''
        Create an empty KernelSchedule.

        :param str name: Name of the subroutine represented by the kernel.
        :returns: New GOKernelSchedule empty object.
        :rtype: py:class:`psyclone.gocean1p0.GOKernelSchedule`
        '''
        return GOKernelSchedule(name)


class GOKernelArguments(Arguments):
    '''Provides information about GOcean kernel-call arguments
        collectively, as specified by the kernel argument
        metadata. This class ensures that initialisation is performed
        correctly. It also overrides the iteration_space_arg method to
        supply a GOcean-specific dictionary for the mapping of
        argument-access types.

    '''
    def __init__(self, call, parent_call):
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
            elif arg.argument_type == "scalar" or arg.argument_type == "field":
                # This is a kernel argument supplied by the Algorithm layer
                self._args.append(GOKernelArgument(arg, call.args[idx],
                                                   parent_call))
            else:
                raise ParseError("Invalid kernel argument type. Found '{0}' "
                                 "but must be one of {1}".
                                 format(arg.argument_type,
                                        ["grid_property", "scalar", "field"]))
        self._dofs = []

    def raw_arg_list(self):
        '''
        :returns: a list of all of the actual arguments to the \
                  kernel call.
        :rtype: list of str

        :raises GenerationError: if the kernel requires a grid property \
                                 but has no field arguments.
        :raises InternalError: if we encounter a kernel argument with an \
                               unrecognised type.
        '''
        if self._raw_arg_list:
            return self._raw_arg_list

        # Before we do anything else, go through the arguments and
        # determine the best one from which to obtain the grid properties.
        grid_arg = self.find_grid_access()

        # A GOcean 1.0 kernel always requires the [i,j] indices of the
        # grid-point that is to be updated
        arguments = ["i", "j"]
        for arg in self._args:

            if arg.argument_type == "scalar":
                # Scalar arguments require no de-referencing
                arguments.append(arg.name)
            elif arg.argument_type == "field":
                # Field objects are Fortran derived-types
                api_config = Config.get().api_conf("gocean1.0")
                # TODO: #676 go_grid_data is actually a field property
                data = api_config.grid_properties["go_grid_data"].fortran\
                    .format(arg.name)
                arguments.append(data)
            elif arg.argument_type == "grid_property":
                # Argument is a property of the grid which we can access via
                # the grid member of any field object.
                # We use the most suitable field as chosen above.
                if grid_arg is None:
                    raise GenerationError(
                        "Error: kernel {0} requires grid property {1} but "
                        "does not have any arguments that are fields".
                        format(self._parent_call.name, arg.name))
                arguments.append(arg.dereference(grid_arg.name))
            else:
                raise InternalError("Kernel {0}, argument {1} has "
                                    "unrecognised type: '{2}'".
                                    format(self._parent_call.name, arg.name,
                                           arg.argument_type))
        self._raw_arg_list = arguments
        return self._raw_arg_list

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
        api_config = Config.get().api_conf("gocean1.0")
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
                "The name parameter given to GOKernelArguments.append method "
                "should be a string, but found '{0}' instead.".
                format(type(name).__name__))

        # Create a descriptor with the given type
        descriptor = Descriptor(None, argument_type)

        # Create the argument and append it to the argument list
        arg = Arg("variable", name)
        argument = GOKernelArgument(descriptor, arg, self._parent_call)
        self.args.append(argument)
        # self.raw_arg_list().append(name)


class GOKernelArgument(KernelArgument):
    ''' Provides information about individual GOcean kernel call arguments
        as specified by the kernel argument metadata. '''
    def __init__(self, arg, arg_info, call):

        self._arg = arg
        KernelArgument.__init__(self, arg, arg_info, call)

    def psyir_expression(self):
        '''
        :returns: the PSyIR expression represented by this Argument.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        :raises InternalError: if this Argument type is not "field" or \
                               "scalar".

        '''
        # If the argument name is just a number (e.g. '0') we return a
        # constant Literal expression
        # six.text_type is needed in Python2 to use the isnumeric method
        if six.text_type(self.name).isnumeric():
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

        raise InternalError("GOcean expects the Argument.argument_type() to be"
                            " 'field' or 'scalar' but found '{0}'."
                            "".format(self.argument_type))

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
            # r2d_field can have DeferredType and UnresolvedInterface because
            # it is an unnamed import from a module.
            type_symbol = self._call.root.symbol_table.symbol_from_tag(
                "r2d_field", symbol_type=DataTypeSymbol,
                datatype=DeferredType(), interface=UnresolvedInterface())
            return type_symbol

        # Gocean scalars can be REAL or INTEGER
        if self.argument_type == "scalar":
            if self.space.lower() == "go_r_scalar":
                go_wp = self._call.root.symbol_table.symbol_from_tag(
                    "go_wp", symbol_type=DataSymbol, datatype=DeferredType(),
                    interface=UnresolvedInterface())
                return ScalarType(ScalarType.Intrinsic.REAL, go_wp)
            if self.space.lower() == "go_i_scalar":
                return INTEGER_TYPE
            raise InternalError("GOcean expects scalar arguments to be of"
                                " 'go_r_scalar' or 'go_i_scalar' type but "
                                "found '{0}'.".format(self.space.lower()))

        raise InternalError("GOcean expects the Argument.argument_type() to be"
                            " 'field' or 'scalar' but found '{0}'."
                            "".format(self.argument_type))

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
        super(GOKernelGridArgument, self).__init__(None, None, arg.access)

        api_config = Config.get().api_conf("gocean1.0")
        try:
            deref_name = api_config.grid_properties[arg.grid_prop].fortran
        except KeyError as err:
            all_keys = str(api_config.grid_properties.keys())
            six.raise_from(
                GenerationError("Unrecognised grid property "
                                "specified. Expected one of {0} but found "
                                "'{1}'". format(all_keys, arg.grid_prop)),
                err)

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
        symbol = self._call.scope.symbol_table.symbol_from_tag(tag)

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
        api_config = Config.get().api_conf("gocean1.0")
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
        api_config = Config.get().api_conf("gocean1.0")
        return api_config.grid_properties[self._property_name].intrinsic_type

    @property
    def is_scalar(self):
        '''
        :returns: if this variable is a scalar variable or not.
        :rtype: bool
        '''
        # The constructor guarantees that _property_name is a valid key!
        api_config = Config.get().api_conf("gocean1.0")
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
                "Meta-data error in kernel '{0}': 3rd descriptor (stencil) of "
                "field argument is '{1}' but expected either a name or the "
                "format 'go_stencil(...)'".format(kernel_name,
                                                  str(stencil_info)))

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
                    "Meta-data error in kernel '{0}': 3rd descriptor "
                    "(stencil) of field argument is '{1}' but must be "
                    "'go_stencil(...)".format(kernel_name, name))
            if len(args) != 3:
                raise ParseError(
                    "Meta-data error in kernel '{0}': 3rd descriptor "
                    "(stencil) of field argument with format "
                    "'go_stencil(...)', has {1} arguments but should have "
                    "3".format(kernel_name, len(args)))
            # Each of the 3 args should be of length 3 and each
            # character should be a digit from 0-9. Whilst we are
            # expecting numbers, the parser represents these numbers
            # as strings so we have to perform string manipulation to
            # check and that extract them
            for arg_idx in range(3):
                arg = args[arg_idx]
                if not isinstance(arg, six.string_types):
                    raise ParseError(
                        "Meta-data error in kernel '{0}': 3rd descriptor "
                        "(stencil) of field argument with format "
                        "'go_stencil(...)'. Argument index {1} should be a "
                        "number but found "
                        "'{2}'.".format(kernel_name, arg_idx, str(arg)))
                if len(arg) != 3:
                    raise ParseError(
                        "Meta-data error in kernel '{0}': 3rd descriptor "
                        "(stencil) of field argument with format "
                        "'go_stencil(...)'. Argument index {1} should "
                        "consist of 3 digits but found "
                        "{2}.".format(kernel_name, arg_idx, len(arg)))
            # The central value is constrained to be 0 or 1
            if args[1][1] not in ["0", "1"]:
                raise ParseError(
                    "Meta-data error in kernel '{0}': 3rd descriptor "
                    "(stencil) of field argument with format "
                    "'go_stencil(...)'. Argument index 1 position 1 "
                    "should be a number from 0-1 "
                    "but found {1}.".format(kernel_name, args[1][1]))
            # It is not valid to specify a zero stencil. This is
            # indicated by the 'pointwise' name
            if args[0] == "000" and \
               (args[1] == "000" or args[1] == "010") and \
               args[2] == "000":
                raise ParseError(
                    "Meta-data error in kernel '{0}': 3rd descriptor "
                    "(stencil) of field argument with format "
                    "'go_stencil(...)'. A zero sized stencil has been "
                    "specified. This should be specified with the "
                    "'go_pointwise' keyword.".format(kernel_name))
            # store the values in an internal array as integers in i,j
            # order
            for idx0 in range(3):
                for idx1 in range(3):
                    # The j coordincate needs to be 'reversed': the first
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
                    "Meta-data error in kernel '{0}': 3rd descriptor "
                    "(stencil) of field argument is '{1}' but must be one "
                    "of {2} or go_stencil(...)"
                    .format(kernel_name, name, const.VALID_STENCIL_NAMES))
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
                "The indices arguments to the depth method in the GOStencil "
                "object must be between -1 and 1 but found "
                "({0},{1})".format(index0, index1))
        return self._stencil[index0+1][index1+1]


class GO1p0Descriptor(Descriptor):
    ''' Description of a GOcean 1.0 kernel argument, as obtained by
    parsing the kernel meta-data.

    :param str kernel_name: the name of the kernel metadata type \
                            that contains this metadata.
    :param kernel_arg: the relevant part of the parser's AST.
    :type kernel_arg: :py:class:`psyclone.expression.FunctionVar`

    :raises ParseError: if a kernel argument has an invalid grid-point type.
    :raises ParseError: for an unrecognised grid property.
    :raises ParseError: for an invalid number of arguments.
    :raises ParseError: for an invalid access argument.

    '''
    def __init__(self, kernel_name, kernel_arg):
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
                raise ParseError("Meta-data error in kernel {0}: argument "
                                 "grid-point type is '{1}' but must be one "
                                 "of {2} ".format(kernel_name, funcspace,
                                                  valid_func_spaces))

        elif nargs == 2:
            # This kernel argument is a property of the grid. The grid
            # properties are special because they must be supplied by
            # the PSy layer.
            access = kernel_arg.args[0].name
            grid_var = kernel_arg.args[1].name
            funcspace = ""

            self._grid_prop = grid_var
            self._argument_type = "grid_property"
            api_config = Config.get().api_conf("gocean1.0")

            if grid_var.lower() not in api_config.grid_properties:
                valid_keys = str(api_config.grid_properties.keys())
                raise ParseError(
                    "Meta-data error in kernel {0}: un-recognised grid "
                    "property '{1}' requested. Must be one of {2}".
                    format(kernel_name, grid_var, valid_keys))
        else:
            raise ParseError(
                "Meta-data error in kernel {0}: 'arg' type expects 2 or 3 "
                "arguments but found '{1}' in '{2}'".
                format(kernel_name,
                       str(len(kernel_arg.args)),
                       kernel_arg.args))

        api_config = Config.get().api_conf("gocean1.0")
        access_mapping = api_config.get_access_mapping()
        try:
            access_type = access_mapping[access]
        except KeyError as err:
            valid_names = api_config.get_valid_accesses_api()
            six.raise_from(
                ParseError("Meta-data error in kernel {0}: "
                           "argument access  is given as '{1}' but must be "
                           "one of {2}".
                           format(kernel_name, access, valid_names)), err)

        # Finally we can call the __init__ method of our base class
        super(GO1p0Descriptor,
              self).__init__(access_type, funcspace, stencil=stencil_info,
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
            raise ParseError("Meta-data error in kernel {0}: an INDEX_OFFSET "
                             "must be specified and must be one of {1}".
                             format(name, const.VALID_OFFSET_NAMES))

        if self._index_offset.lower() not in const.VALID_OFFSET_NAMES:
            raise ParseError("Meta-data error in kernel {0}: INDEX_OFFSET "
                             "has value '{1}' but must be one of {2}".
                             format(name,
                                    self._index_offset,
                                    const.VALID_OFFSET_NAMES))

        const = GOceanConstants()
        # Check that the meta-data for this kernel is valid
        if self._iterates_over is None:
            raise ParseError("Meta-data error in kernel {0}: ITERATES_OVER "
                             "is missing. (Valid values are: {1})".
                             format(name, const.VALID_ITERATES_OVER))

        if self._iterates_over.lower() not in const.VALID_ITERATES_OVER:
            raise ParseError("Meta-data error in kernel {0}: ITERATES_OVER "
                             "has value '{1}' but must be one of {2}".
                             format(name,
                                    self._iterates_over.lower(),
                                    const.VALID_ITERATES_OVER))

        # The list of kernel arguments
        self._arg_descriptors = []
        have_grid_prop = False
        for init in self._inits:
            if init.name != 'go_arg':
                raise ParseError("Each meta_arg value must be of type " +
                                 "'go_arg' for the gocean1.0 api, but " +
                                 "found '{0}'".format(init.name))
            # Pass in the name of this kernel for the purposes
            # of error reporting
            new_arg = GO1p0Descriptor(name, init)
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
                    "Kernel {0} requires a property of the grid but does "
                    "not have any field objects as arguments.".format(name))

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
    Sub-classes ACCEnterDataDirective to provide an API-specific implementation
    of data_on_device().

    '''
    def data_on_device(self, parent):
        '''
        Adds nodes into the f2pygen AST to flag that each of the
        objects required by the kernels in the data region is now on the
        device. We do this by setting the data_on_device attribute to .true.

        :param parent: The node in the f2pygen AST to which to add the \
                       assignment nodes.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`
        '''

        # Get a reference to the f2pygen module root
        module = parent
        while module.parent:
            module = module.parent

        obj_list = []
        for pdir in self._acc_dirs:
            for var in pdir.fields:
                if var not in obj_list:
                    parent.add(AssignGen(parent,
                                         lhs=var+"%data_on_device",
                                         rhs=".true."))
                    parent.add(AssignGen(
                        parent,
                        lhs=var+"%read_from_device_f",
                        rhs=self._read_from_device_routine(module).name,
                        pointer=True))
                    obj_list.append(var)

    def _read_from_device_routine(self, f2pygen_module=None, psyir=None):
        ''' Return the symbol of the routine that reads data from the OpenACC
        device, if it doesn't exist create the Routine and the Symbol, and if
        either f2pygen_module or psyir are supplied then a suitable node
        representing the routine is inserted.

        :param f2pygen_module: optional f2pygen module where to insert the \
                               generated read_from_device routine.
        :type f2pygen_module: :py:class:`psyclone.f2pygen.ModuleGen` or \
                              NoneType
        :param psyir: optional psyir tree where to insert the generated \
                      read_from_device routine.
        :type psyir: :py:class:`psyclone.psyir.nodes.Node` or NoneType

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
        symbol = subroutine.symbol_table.lookup("to")
        subroutine.addchild(ACCUpdateDirective(symbol, "host"))

        # Rename subroutine
        subroutine.name = subroutine_name

        # If a f2pygen module is provided insert a PSyIRGen node
        if f2pygen_module:
            f2pygen_module.add(PSyIRGen(f2pygen_module, subroutine))

        # If PSyIR is provided insert the routine as a child of the parent
        # Container
        if psyir:
            if not psyir.ancestor(Container):
                raise GenerationError(
                    "The GOACCEnterDataDirective can only be generated/lowered"
                    " inside a Container in order to insert a sibling "
                    "subroutine, but '{0}' is not inside a Container."
                    "".format(psyir))
            psyir.ancestor(Container).addchild(subroutine.detach())

        return symtab.lookup_with_tag("openacc_read_func")

    def lower_to_language_level(self):
        '''
        In-place replacement of DSL or high-level concepts into generic PSyIR
        constructs. In addition to calling this method in the base class, the
        GOACCEnterDataDirective sets up the 'data_on_device' flag for
        each of the fields accessed.

        '''
        self._acc_dirs = self.ancestor(InvokeSchedule).walk(
                (ACCParallelDirective, ACCKernelsDirective))
        obj_list = []
        for pdir in self._acc_dirs:
            for var in pdir.fields:
                if var not in obj_list:
                    obj_list.append(var)

        read_routine_symbol = self._read_from_device_routine(psyir=self)

        for var in obj_list:
            symbol = self.scope.symbol_table.lookup(var)
            assignment = Assignment.create(
                StructureReference.create(symbol, ['data_on_device']),
                Literal("true", BOOLEAN_TYPE))
            self.parent.children.insert(self.position, assignment)

            # Use a CodeBlock to encode a Fortran pointer assignment
            reader = FortranStringReader(
                        "{0}%read_from_device_f => {1}\n"
                        "".format(symbol.name, read_routine_symbol.name))
            reader.set_format(FortranFormat(True, True))
            block = Pointer_Assignment_Stmt(reader)
            codeblock = CodeBlock([block], CodeBlock.Structure.STATEMENT)
            self.parent.children.insert(self.position, codeblock)

        super(GOACCEnterDataDirective, self).lower_to_language_level()


class GOSymbolTable(SymbolTable):
    '''
    Sub-classes SymbolTable to provide a GOcean-specific implementation.
    '''

    def _check_gocean_conformity(self):
        '''
        Checks that the Symbol Table has at least 2 arguments which represent
        the iteration indices (are scalar integers).

        :raises GenerationError: if the Symbol Table does not conform to the \
                rules for a GOcean 1.0 kernel.
        '''
        # Get the kernel name if available for better error messages
        kname_str = ""
        if self._node and isinstance(self._node, KernelSchedule):
            kname_str = " for kernel '{0}'".format(self._node.name)

        # Check that there are at least 2 arguments
        if len(self.argument_list) < 2:
            raise GenerationError(
                "GOcean 1.0 API kernels should always have at least two "
                "arguments representing the iteration indices but the "
                "Symbol Table{0} has only {1} argument(s)."
                "".format(kname_str,
                          str(len(self.argument_list)))
                )

        # Check that first 2 arguments are scalar integers
        for pos, posstr in [(0, "first"), (1, "second")]:
            dtype = self.argument_list[pos].datatype
            if not (isinstance(dtype, ScalarType) and
                    dtype.intrinsic == ScalarType.Intrinsic.INTEGER):
                raise GenerationError(
                    "GOcean 1.0 API kernels {0} argument should be a scalar "
                    "integer but got '{1}'{2}."
                    "".format(posstr, dtype, kname_str))

    @property
    def iteration_indices(self):
        '''In the GOcean API the two first kernel arguments are the iteration
        indices.

        :return: List of symbols representing the iteration indices.
        :rtype: list of :py:class:`psyclone.psyir.symbols.DataSymbol`
        '''
        self._check_gocean_conformity()
        return self.argument_list[:2]

    @property
    def data_arguments(self):
        '''In the GOcean API the data arguments start from the third item in
        the argument list.

        :return: List of symbols representing the data arguments.
        :rtype: list of :py:class:`psyclone.psyir.symbols.DataSymbol`
        '''
        self._check_gocean_conformity()
        return self.argument_list[2:]


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
        super(GOHaloExchange, self).__init__(field, check_dirty=check_dirty,
                                             parent=parent)

        # Name of the HaloExchange method in the GOcean infrastructure.
        self._halo_exchange_name = "halo_exchange"

    def lower_to_language_level(self):
        '''
        In-place replacement of DSL or high-level concepts into generic
        PSyIR constructs. A GOHaloExchange is replaced by a call to the
        appropriate library method.

        '''
        # Call the halo_exchange routine with depth argument to 1
        # Currently we create an symbol name with % as a workaround of not
        # having type bound routines.
        rsymbol = RoutineSymbol(self.field.name + "%" +
                                self._halo_exchange_name)
        call_node = Call.create(rsymbol, [Literal("1", INTEGER_TYPE)])
        self.replace_with(call_node)

    def gen_code(self, parent):
        '''GOcean specific code generation for this class.

        :param parent: an f2pygen object that will be the parent of \
            f2pygen objects created in this method.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        '''
        # TODO 856: Wrap Halo call with an is_dirty flag when necessary.

        # TODO 886: Currently only stencils of depth 1 are accepted by this
        # API, so the HaloExchange is hardcoded to depth 1.

        if self.ancestor(InvokeSchedule).opencl:
            # If the dependency is a Kernel dispatched in a different command
            # queue than _OCL_MANAGEMENT_QUEUE, in order to guarantee that the
            # haloexchange data transfer happens after the kernel is finished,
            # we need to add a barrier to that previous queue.
            dependency = self.backward_dependence()
            if dependency and dependency.coded_kernels():
                for kernel_dep in dependency.coded_kernels():
                    # TODO #1134: The OpenCL options should not leak from the
                    # OpenCL transformation.
                    # pylint: disable=protected-access
                    previous_queue = kernel_dep._opencl_options['queue_number']
                    if previous_queue != _OCL_MANAGEMENT_QUEUE:
                        # If the backward dependency is being executed in
                        # another queue we add a barrier to make sure the
                        # previous kernel has finished.
                        stab = self.scope.symbol_table
                        qlist = stab.lookup_with_tag("opencl_cmd_queues").name
                        flag = stab.lookup_with_tag("opencl_error").name
                        parent.add(AssignGen(
                            parent,
                            lhs=flag,
                            rhs="clFinish({0}({1}))".format(
                                qlist, str(previous_queue))))

        parent.add(
            CallGen(
                parent, name=self._field.name +
                "%" + self._halo_exchange_name +
                "(depth=1)"))
        parent.add(CommentGen(parent, ""))


# For Sphinx AutoAPI documentation generation
__all__ = ['GOPSy', 'GOInvokes', 'GOInvoke', 'GOInvokeSchedule', 'GOLoop',
           'GOBuiltInCallFactory', 'GOKernCallFactory', 'GOKern',
           'GOFparser2Reader', 'GOKernelArguments', 'GOKernelArgument',
           'GOKernelGridArgument', 'GOStencil', 'GO1p0Descriptor',
           'GOKernelType1p0', 'GOACCEnterDataDirective', 'GOSymbolTable',
           'GOKernelSchedule', 'GOHaloExchange']
