# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2022, Science and Technology Facilities Council.
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
# Modified I. Kavcic,    Met Office
#          C.M. Maynard, Met Office / University of Reading
# Modified by J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' This module provides generic support for PSyclone's PSy code optimisation
    and generation. The classes in this method need to be specialised for a
    particular API and implementation. '''

from __future__ import print_function, absolute_import
from collections import OrderedDict
import abc
import six
from psyclone.configuration import Config
from psyclone.core import AccessType
from psyclone.errors import GenerationError, InternalError, FieldNotFoundError
from psyclone.f2pygen import CommentGen, CallGen, PSyIRGen, UseGen
from psyclone.parse.algorithm import BuiltInCall
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.backend.visitor import PSyIRVisitor
from psyclone.psyir.nodes import Node, Schedule, Loop, Statement, Container, \
    Routine, Call, OMPDoDirective
from psyclone.psyir.symbols import DataSymbol, RoutineSymbol, Symbol, \
    ContainerSymbol, ImportInterface, ArgumentInterface, DeferredType
from psyclone.psyir.symbols.datatypes import UnknownFortranType

# The types of 'intent' that an argument to a Fortran subroutine
# may have
FORTRAN_INTENT_NAMES = ["inout", "out", "in"]

# Mapping of access type to operator.
REDUCTION_OPERATOR_MAPPING = {AccessType.SUM: "+"}


def object_index(alist, item):
    '''
    A version of the `list.index()` method that checks object identity
    rather that the content of the object.

    TODO this is a workaround for the fact that fparser2 overrides the
    comparison operator for all nodes in the parse tree. See fparser
    issue 174.

    :param alist: single object or list of objects to search.
    :type alist: list or :py:class:`fparser.two.utils.Base`
    :param obj item: object to search for in the list.
    :returns: index of the item in the list.
    :rtype: int
    :raises ValueError: if object is not in the list.
    '''
    if item is None:
        raise InternalError("Cannot search for None item in list.")
    for idx, entry in enumerate(alist):
        if entry is item:
            return idx
    raise ValueError(
        "Item '{0}' not found in list: {1}".format(str(item), alist))


def get_api(api):
    ''' If no API is specified then return the default. Otherwise, check that
    the supplied API is valid.
    :param str api: The PSyclone API to check or an empty string.
    :returns: The API that is in use.
    :rtype: str
    :raises GenerationError: if the specified API is not supported.

    '''
    if api == "":
        api = Config.get().default_api
    else:
        if api not in Config.get().supported_apis:
            raise GenerationError("get_api: Unsupported API '{0}' "
                                  "specified. Supported types are "
                                  "{1}.".format(api,
                                                Config.get().supported_apis))
    return api


def zero_reduction_variables(red_call_list, parent):
    '''zero all reduction variables associated with the calls in the call
    list'''
    if red_call_list:
        parent.add(CommentGen(parent, ""))
        parent.add(CommentGen(parent, " Zero summation variables"))
        parent.add(CommentGen(parent, ""))
        for call in red_call_list:
            call.zero_reduction_variable(parent)
        parent.add(CommentGen(parent, ""))


def args_filter(arg_list, arg_types=None, arg_accesses=None, arg_meshes=None,
                include_literals=True):
    '''
    Return all arguments in the supplied list that are of type
    arg_types and with access in arg_accesses. If these are not set
    then return all arguments.

    :param arg_list: list of kernel arguments to filter.
    :type arg_list: list of :py:class:`psyclone.parse.kernel.Descriptor`
    :param arg_types: list of argument types (e.g. "GH_FIELD").
    :type arg_types: list of str
    :param arg_accesses: list of access types that arguments must have.
    :type arg_accesses: list of \
        :py:class:`psyclone.core.access_type.AccessType`
    :param arg_meshes: list of meshes that arguments must be on.
    :type arg_meshes: list of str
    :param bool include_literals: whether or not to include literal arguments \
                                  in the returned list.

    :returns: list of kernel arguments matching the requirements.
    :rtype: list of :py:class:`psyclone.parse.kernel.Descriptor`

    '''
    arguments = []
    for argument in arg_list:
        if arg_types:
            if argument.argument_type.lower() not in arg_types:
                continue
        if arg_accesses:
            if argument.access not in arg_accesses:
                continue
        if arg_meshes:
            if argument.mesh not in arg_meshes:
                continue
        if not include_literals:
            # We're not including literal arguments so skip this argument
            # if it is literal.
            if argument.is_literal:
                continue
        arguments.append(argument)
    return arguments


class PSyFactory(object):
    '''
    Creates a specific version of the PSy. If a particular api is not
    provided then the default api, as specified in the psyclone.cfg
    file, is chosen.

    :param str api: name of the PSyclone API (domain) for which to create \
        a factory.
    :param bool distributed_memory: whether or not the PSy object created \
        will include support for distributed-memory parallelism.

    :raises TypeError: if the distributed_memory argument is not a bool.

    '''
    def __init__(self, api="", distributed_memory=None):

        if distributed_memory is None:
            _distributed_memory = Config.get().distributed_memory
        else:
            _distributed_memory = distributed_memory

        if _distributed_memory not in [True, False]:
            raise TypeError(
                "The distributed_memory flag in PSyFactory must be set to"
                " 'True' or 'False'")
        Config.get().distributed_memory = _distributed_memory
        self._type = get_api(api)

    def create(self, invoke_info):
        '''
        Create the API-specific PSy instance.

        :param invoke_info: information on the invoke()s found by parsing \
                            the Algorithm layer or (for NEMO) the fparser2 \
                            parse tree of the source file.
        :type invoke_info: :py:class:`psyclone.parse.algorithm.FileInfo` or \
                           :py:class:`fparser.two.Fortran2003.Program`

        :returns: an instance of the API-specifc sub-class of PSy.
        :rtype: subclass of :py:class:`psyclone.psyGen.PSy`

        :raises InternalError: if this factory is found to have an \
                               unsupported type (API).
        '''
        # Conditional run-time importing is a part of this factory
        # implementation.
        # pylint: disable=import-outside-toplevel
        if self._type == "dynamo0.3":
            from psyclone.dynamo0p3 import DynamoPSy as PSyClass
        elif self._type == "gocean1.0":
            from psyclone.gocean1p0 import GOPSy as PSyClass
        elif self._type == "nemo":
            from psyclone.nemo import NemoPSy as PSyClass
            # For this API, the 'invoke_info' is actually the fparser2 AST
            # of the Fortran file being processed
        else:
            raise InternalError(
                "PSyFactory: Unsupported API type '{0}' found. Expected one "
                "of {1}.".format(self._type, Config.get().supported_apis))
        return PSyClass(invoke_info)


class PSy(object):
    '''
    Base class to help manage and generate PSy code for a single
    algorithm file. Takes the invocation information output from the
    function :func:`parse.algorithm.parse` as its input and stores this in a
    way suitable for optimisation and code generation.

    :param FileInfo invoke_info: An object containing the required \
                                 invocation information for code \
                                 optimisation and generation. Produced \
                                 by the function :func:`parse.algorithm.parse`.
    :type invoke_info: :py:class:`psyclone.parse.algorithm.FileInfo`

    For example:

    >>> from psyclone.parse.algorithm import parse
    >>> ast, info = parse("argspec.F90")
    >>> from psyclone.psyGen import PSyFactory
    >>> api = "..."
    >>> psy = PSyFactory(api).create(info)
    >>> print(psy.gen)

    '''
    def __init__(self, invoke_info):
        self._name = invoke_info.name
        self._invokes = None
        # create an empty PSy layer container
        # TODO 1010: Alternatively the PSy object could be a Container itself
        self._container = Container(self.name)

    @property
    def container(self):
        '''
        :returns: the container associated with this PSy object
        :rtype: :py:class:`psyclone.psyir.nodes.Container`
        '''
        return self._container

    def __str__(self):
        return "PSy"

    @property
    def invokes(self):
        ''':returns: the list of invokes.
        :rtype: :py:class:`psyclone.psyGen.Invokes` or derived class
        '''
        return self._invokes

    @property
    def name(self):
        ''':returns: the name of the PSy object.
        :rtype: str
        '''
        return "psy_"+self._name

    @property
    @abc.abstractmethod
    def gen(self):
        '''Abstract base class for code generation function.

        :returns: root node of generated Fortran AST.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`
        '''


class Invokes(object):
    '''Manage the invoke calls.

    :param alg_calls: a list of invoke metadata extracted by the \
        parser.
    :type alg_calls: list of \
    :py:class:`psyclone.parse.algorithm.InvokeCall`
    :param invoke_cls: an api-specific Invoke class.
    :type invoke_cls: subclass of :py:class:`psyclone.psyGen.Invoke`
    :param psy: the PSy instance containing this Invokes instance.
    :type psy: subclass of :py:class`psyclone.psyGen.PSy`

    '''
    def __init__(self, alg_calls, invoke_cls, psy):
        self._psy = psy
        self.invoke_map = {}
        self.invoke_list = []
        for idx, alg_invocation in enumerate(alg_calls):
            my_invoke = invoke_cls(alg_invocation, idx, self)
            self.invoke_map[my_invoke.name] = my_invoke
            self.invoke_list.append(my_invoke)

    def __str__(self):
        return "Invokes object containing "+str(self.names)

    @property
    def psy(self):
        '''
        :returns: the PSy instance that contains this instance.
        :rtype: subclass of :py:class:`psyclone.psyGen.PSy`

        '''
        return self._psy

    @property
    def names(self):
        return self.invoke_map.keys()

    def get(self, invoke_name):
        # add a try here for keyerror
        try:
            return self.invoke_map[invoke_name]
        except KeyError:
            raise RuntimeError("Cannot find an invoke named '{0}' in {1}".
                               format(invoke_name,
                                      str(self.names)))

    def gen_code(self, parent):
        '''
        Create the f2pygen AST for each Invoke in the PSy layer.

        :param parent: the parent node in the AST to which to add content.
        :type parent: `psyclone.f2pygen.ModuleGen`

        :raises GenerationError: if an invoke_list schedule is not an \
            InvokeSchedule.
        '''
        for invoke in self.invoke_list:
            if not isinstance(invoke.schedule, InvokeSchedule):
                raise GenerationError(
                    "An invoke.schedule element of the invoke_list "
                    "is a '{0}', but it should be an 'InvokeSchedule'."
                    "".format(type(invoke.schedule).__name__))
            invoke.gen_code(parent)


class Invoke(object):
    r'''Manage an individual invoke call.

    :param alg_invocation: metadata from the parsed code capturing \
                           information for this Invoke instance.
    :type alg_invocation: :py:class:`psyclone.parse.algorithm.InvokeCall`
    :param int idx: position/index of this invoke call in the subroutine. \
                    If not None, this number is added to the name ("invoke\_").
    :param schedule_class: the schedule class to create for this invoke.
    :type schedule_class: :py:class:`psyclone.psyGen.InvokeSchedule`
    :param invokes: the Invokes instance that contains this Invoke \
                    instance.
    :type invokes: :py:class:`psyclone.psyGen.Invokes`
    :param reserved_names: optional list of reserved names, i.e. names that \
                           should not be used e.g. as a PSyclone-created \
                           variable name.
    :type reserved_names: list of str

    '''
    def __init__(self, alg_invocation, idx, schedule_class, invokes,
                 reserved_names=None):
        '''Construct an invoke object.'''

        self._invokes = invokes
        self._name = "invoke"
        self._alg_unique_args = []

        if alg_invocation is None and idx is None:
            return

        # create a name for the call if one does not already exist
        if alg_invocation.name is not None:
            # In Python2 unicode strings must be converted to str()
            self._name = str(alg_invocation.name)
        elif len(alg_invocation.kcalls) == 1 and \
                alg_invocation.kcalls[0].type == "kernelCall":
            # use the name of the kernel call with the position appended.
            # Appended position is needed in case we have two separate invokes
            # in the same algorithm code containing the same (single) kernel
            self._name = "invoke_" + str(idx) + "_" + \
                alg_invocation.kcalls[0].ktype.name
        else:
            # use the position of the invoke
            self._name = "invoke_" + str(idx)

        if not reserved_names:
            reserved_names = []

        # Get a reference to the parent container, if any
        container = None
        if self.invokes:
            container = self.invokes.psy.container

        # create the schedule
        self._schedule = schedule_class(self._name, alg_invocation.kcalls,
                                        reserved_names, parent=container)

        # Add the new Schedule to the top-level PSy Container
        if container:
            container.addchild(self._schedule)

        # let the schedule have access to me
        self._schedule.invoke = self

        # extract the argument list for the algorithm call and psy
        # layer subroutine.
        self._alg_unique_args = []
        self._psy_unique_vars = []
        tmp_arg_names = []
        for call in self.schedule.kernels():
            for arg in call.arguments.args:
                if arg.text is not None:
                    if arg.text not in self._alg_unique_args:
                        self._alg_unique_args.append(arg.text)
                    if arg.name not in tmp_arg_names:
                        tmp_arg_names.append(arg.name)
                        self._psy_unique_vars.append(arg)
                else:
                    # literals have no name
                    pass

        # work out the unique dofs required in this subroutine
        self._dofs = {}
        for kern_call in self._schedule.coded_kernels():
            dofs = kern_call.arguments.dofs
            for dof in dofs:
                if dof not in self._dofs:
                    # Only keep the first occurence for the moment. We will
                    # need to change this logic at some point as we need to
                    # cope with writes determining the dofs that are used.
                    self._dofs[dof] = [kern_call, dofs[dof][0]]

    def __str__(self):
        return self._name+"("+", ".join([str(arg) for arg in
                                         self._alg_unique_args])+")"

    @property
    def invokes(self):
        '''
        :returns: the Invokes instance that contains this instance.
        :rtype: :py:class`psyclone.psyGen.Invokes`

        '''
        return self._invokes

    @property
    def name(self):
        return self._name

    @property
    def alg_unique_args(self):
        return self._alg_unique_args

    @property
    def psy_unique_vars(self):
        return self._psy_unique_vars

    @property
    def psy_unique_var_names(self):
        names = []
        for var in self._psy_unique_vars:
            names.append(var.name)
        return names

    @property
    def schedule(self):
        return self._schedule

    @schedule.setter
    def schedule(self, obj):
        self._schedule = obj

    def unique_declarations(self, argument_types, access=None,
                            intrinsic_type=None):
        '''
        Returns a list of all required declarations for the specified
        API argument types. If access is supplied (e.g. "write") then
        only declarations with that access are returned. If an intrinsic
        type is supplied then only declarations with that intrinsic type
        are returned.

        :param argument_types: the types of the kernel argument for the \
                               particular API.
        :type argument_types: list of str
        :param access: optional AccessType that the declaration should have.
        :type access: :py:class:`psyclone.core.access_type.AccessType`
        :param intrinsic_type: optional intrinsic type of argument data.
        :type intrinsic_type: str

        :returns: a list of all declared kernel arguments.
        :rtype: list of :py:class:`psyclone.psyGen.KernelArgument`

        :raises InternalError: if at least one kernel argument type is \
                               not valid for the particular API.
        :raises InternalError: if an invalid access is specified.
        :raises InternalError: if an invalid intrinsic type is specified.

        '''
        # First check for invalid argument types, access and intrinsic type
        const = Config.get().api_conf().get_constants()
        if any(argtype not in const.VALID_ARG_TYPE_NAMES for
               argtype in argument_types):
            raise InternalError(
                "Invoke.unique_declarations() called with at least one "
                "invalid argument type. Expected one of {0} but found {1}.".
                format(str(const.VALID_ARG_TYPE_NAMES), str(argument_types)))

        if access and not isinstance(access, AccessType):
            raise InternalError(
                "Invoke.unique_declarations() called with an invalid "
                "access type. Type is '{0}' instead of AccessType.".
                format(str(access)))

        if (intrinsic_type and intrinsic_type not in
                const.VALID_INTRINSIC_TYPES):
            raise InternalError(
                "Invoke.unique_declarations() called with an invalid "
                "intrinsic argument data type. Expected one of {0} but "
                "found '{1}'.".
                format(str(const.VALID_INTRINSIC_TYPES), intrinsic_type))

        # Initialise dictionary of kernel arguments to get the
        # argument list from
        declarations = OrderedDict()
        # Find unique kernel arguments using their declaration names
        for call in self.schedule.kernels():
            for arg in call.arguments.args:
                if not intrinsic_type or arg.intrinsic_type == intrinsic_type:
                    if not access or arg.access == access:
                        if arg.text is not None:
                            if arg.argument_type in argument_types:
                                test_name = arg.declaration_name
                                if test_name not in declarations:
                                    declarations[test_name] = arg
        return list(declarations.values())

    def first_access(self, arg_name):
        ''' Returns the first argument with the specified name passed to
        a kernel in our schedule '''
        for call in self.schedule.kernels():
            for arg in call.arguments.args:
                if arg.text is not None:
                    if arg.declaration_name == arg_name:
                        return arg
        raise GenerationError("Failed to find any kernel argument with name "
                              "'{0}'".format(arg_name))

    def unique_declns_by_intent(self, argument_types, intrinsic_type=None):
        '''
        Returns a dictionary listing all required declarations for each
        type of intent ('inout', 'out' and 'in').

        :param argument_types: the types of the kernel argument for the \
                               particular API for which the intent is required.
        :type argument_types: list of str
        :param intrinsic_type: optional intrinsic type of argument data.
        :type intrinsic_type: str

        :returns: dictionary containing 'intent' keys holding the kernel \
                  arguments as values for each type of intent.
        :rtype: dict of :py:class:`psyclone.psyGen.KernelArgument`

        :raises InternalError: if at least one kernel argument type is \
                               not valid for the particular API.
        :raises InternalError: if an invalid intrinsic type is specified.

        '''
        # First check for invalid argument types and intrinsic type
        const = Config.get().api_conf().get_constants()
        if any(argtype not in const.VALID_ARG_TYPE_NAMES for
               argtype in argument_types):
            raise InternalError(
                "Invoke.unique_declns_by_intent() called with at least one "
                "invalid argument type. Expected one of {0} but found {1}.".
                format(str(const.VALID_ARG_TYPE_NAMES), str(argument_types)))

        if (intrinsic_type and intrinsic_type not in
                const.VALID_INTRINSIC_TYPES):
            raise InternalError(
                "Invoke.unique_declns_by_intent() called with an invalid "
                "intrinsic argument data type. Expected one of {0} but "
                "found '{1}'.".
                format(str(const.VALID_INTRINSIC_TYPES), intrinsic_type))

        # We will return a dictionary containing as many lists
        # as there are types of intent
        declns = {}
        for intent in FORTRAN_INTENT_NAMES:
            declns[intent] = []

        for arg in self.unique_declarations(argument_types,
                                            intrinsic_type=intrinsic_type):
            first_arg = self.first_access(arg.declaration_name)
            if first_arg.access in [AccessType.WRITE, AccessType.SUM]:
                # If the first access is a write then the intent is
                # out irrespective of any other accesses. Note,
                # sum_args behave as if they are write_args from the
                # PSy-layer's perspective.
                declns["out"].append(arg)
                continue
            # if all accesses are read, then the intent is in,
            # otherwise the intent is inout (as we have already
            # dealt with intent out).
            read_only = True
            for call in self.schedule.kernels():
                for tmp_arg in call.arguments.args:
                    if tmp_arg.text is not None and \
                       tmp_arg.declaration_name == arg.declaration_name:
                        if tmp_arg.access != AccessType.READ:
                            # readwrite_args behave in the
                            # same way as inc_args from the
                            # perspective of intents
                            read_only = False
                            break
                if not read_only:
                    break
            if read_only:
                declns["in"].append(arg)
            else:
                declns["inout"].append(arg)
        return declns

    def gen(self):
        from psyclone.f2pygen import ModuleGen
        module = ModuleGen("container")
        self.gen_code(module)
        return module.root

    @abc.abstractmethod
    def gen_code(self, parent):
        '''
        Generates invocation code (the subroutine called by the associated
        invoke call in the algorithm layer). This consists of the PSy
        invocation subroutine and the declaration of its arguments.

        :param parent: the node in the generated AST to which to add content.
        :type parent: :py:class:`psyclone.f2pygen.ModuleGen`

        '''


class InvokeSchedule(Routine):
    '''
    Stores schedule information for an invocation call. Schedules can be
    optimised using transformations.

    >>> from psyclone.parse.algorithm import parse
    >>> ast, info = parse("algorithm.f90")
    >>> from psyclone.psyGen import PSyFactory
    >>> api = "..."
    >>> psy = PSyFactory(api).create(info)
    >>> invokes = psy.invokes
    >>> invokes.names
    >>> invoke = invokes.get("name")
    >>> schedule = invoke.schedule
    >>> print(schedule.view())

    :param str name: name of the Invoke.
    :param type KernFactory: class instance of the factory to use when \
     creating Kernels. e.g. :py:class:`psyclone.dynamo0p3.DynKernCallFactory`.
    :param type BuiltInFactory: class instance of the factory to use when \
     creating built-ins. e.g. \
     :py:class:`psyclone.domain.lfric.lfric_builtins.LFRicBuiltInCallFactory`.
    :param alg_calls: list of Kernel calls in the schedule.
    :type alg_calls: list of :py:class:`psyclone.parse.algorithm.KernelCall`
    :param kwargs: additional keyword arguments provided to the super class.
    :type kwargs: unwrapped dict.

    '''
    # Textual description of the node.
    _text_name = "InvokeSchedule"

    def __init__(self, name, KernFactory, BuiltInFactory, alg_calls=None,
                 reserved_names=None, **kwargs):
        super().__init__(name, **kwargs)

        self._invoke = None

        # Populate the Schedule Symbol Table with the reserved names.
        if reserved_names:
            for reserved in reserved_names:
                self.symbol_table.add(Symbol(reserved))

        # We need to separate calls into loops (an iteration space really)
        # and calls so that we can perform optimisations separately on the
        # two entities.
        if alg_calls is None:
            alg_calls = []
        for call in alg_calls:
            if isinstance(call, BuiltInCall):
                self.addchild(BuiltInFactory.create(call, parent=self))
            else:
                self.addchild(KernFactory.create(call, parent=self))

    @property
    def symbol_table(self):
        '''
        :returns: Table containing symbol information for the schedule.
        :rtype: :py:class:`psyclone.psyir.symbols.SymbolTable`
        '''
        return self._symbol_table

    @property
    def invoke(self):
        return self._invoke

    @invoke.setter
    def invoke(self, my_invoke):
        self._invoke = my_invoke

    def node_str(self, colour=True):
        '''
        Returns the name of this node with appropriate control codes
        to generate coloured output in a terminal that supports it.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        :rtype: str
        '''
        return "{0}[invoke='{1}']".format(
            self.coloured_name(colour), self.name)

    def __str__(self):
        result = self.coloured_name(False) + ":\n"
        for entity in self._children:
            result += str(entity) + "\n"
        result += "End " + self.coloured_name(False) + "\n"
        return result

    def gen_code(self, parent):
        '''
        Generate the Nodes in the f2pygen AST for this schedule.

        :param parent: the parent Node (i.e. the enclosing subroutine) to \
                       which to add content.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`
        '''
        from psyclone.f2pygen import DeclGen, AssignGen, IfThenGen

        # The gen_code methods may generate new Symbol names, however, we want
        # subsequent calls to invoke.gen_code() to produce the exact same code,
        # including symbol names, and therefore new symbols should not be kept
        # permanently outside the hierarchic gen_code call-chain.
        # To make this possible we create here a duplicate of the symbol table.
        # This duplicate will be used by all recursive gen_code() methods
        # called below this one and thus maintaining a consistent Symbol Table
        # during the whole gen_code() chain, but at the end of this method the
        # original symbol table is restored.
        symbol_table_before_gen = self.symbol_table
        psy_symbol_table_before_gen = self.parent.symbol_table
        # pylint: disable=protected-access
        self._symbol_table = self.symbol_table.shallow_copy()
        self.parent._symbol_table = self.parent.symbol_table.shallow_copy()
        # pylint: enable=protected-access

        # Imported symbols promoted from Kernel imports are in the SymbolTable.
        # First aggregate all variables imported from the same module in a map.
        module_map = {}
        for imported_var in self.symbol_table.imported_symbols:
            module_name = imported_var.interface.container_symbol.name
            if module_name in module_map:
                module_map[module_name].append(imported_var.name)
            else:
                module_map[module_name] = [imported_var.name]

        # Then we can produce the UseGen statements without repeating modules
        for module_name, var_list in module_map.items():
            parent.add(UseGen(parent, name=module_name, only=True,
                              funcnames=var_list))

        for entity in self._children:
            entity.gen_code(parent)

        # Restore symbol table (with a protected access attribute change)
        # pylint: disable=protected-access
        self._symbol_table = symbol_table_before_gen
        self.parent._symbol_table = psy_symbol_table_before_gen
        # pylint: enable=protected-access


class GlobalSum(Statement):
    '''
    Generic Global Sum class which can be added to and manipulated
    in, a schedule.

    :param scalar: the scalar that the global sum is stored into
    :type scalar: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
    :param parent: optional parent (default None) of this object
    :type parent: :py:class:`psyclone.psyir.nodes.Node`

    '''
    # Textual description of the node.
    _children_valid_format = "<LeafNode>"
    _text_name = "GlobalSum"
    _colour = "cyan"

    def __init__(self, scalar, parent=None):
        Node.__init__(self, children=[], parent=parent)
        import copy
        self._scalar = copy.copy(scalar)
        if scalar:
            # Update scalar values appropriately
            # Here "readwrite" denotes how the class GlobalSum
            # accesses/updates a scalar
            self._scalar.access = AccessType.READWRITE
            self._scalar.call = self

    @property
    def scalar(self):
        ''' Return the scalar field that this global sum acts on '''
        return self._scalar

    @property
    def dag_name(self):
        '''
        :returns: the name to use in the DAG for this node.
        :rtype: str
        '''
        return "globalsum({0})_".format(self._scalar.name) + str(self.position)

    @property
    def args(self):
        ''' Return the list of arguments associated with this node. Override
        the base method and simply return our argument.'''
        return [self._scalar]

    def node_str(self, colour=True):
        '''
        Returns a text description of this node with (optional) control codes
        to generate coloured output in a terminal that supports it.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        :rtype: str
        '''
        return "{0}[scalar='{1}']".format(self.coloured_name(colour),
                                          self._scalar.name)


class HaloExchange(Statement):
    '''
    Generic Halo Exchange class which can be added to and
    manipulated in, a schedule.

    :param field: the field that this halo exchange will act on
    :type field: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
    :param check_dirty: optional argument default True indicating whether \
                        this halo exchange should be subject to a run-time \
                        check for clean/dirty halos.
    :type check_dirty: bool
    :param vector_index: optional vector index (default None) to identify \
                         which index of a vector field this halo exchange is \
                         responsible for.
    :type vector_index: int
    :param parent: optional parent (default None) of this object
    :type parent: :py:class:`psyclone.psyir.nodes.Node`

    '''
    # Textual description of the node.
    _children_valid_format = "<LeafNode>"
    _text_name = "HaloExchange"
    _colour = "blue"

    def __init__(self, field, check_dirty=True,
                 vector_index=None, parent=None):
        Node.__init__(self, children=[], parent=parent)
        import copy
        self._field = copy.copy(field)
        if field:
            # Update fields values appropriately
            # Here "readwrite" denotes how the class HaloExchange
            # accesses a field rather than the field's continuity
            self._field.access = AccessType.READWRITE
            self._field.call = self
        self._halo_type = None
        self._halo_depth = None
        self._check_dirty = check_dirty
        self._vector_index = vector_index
        # Keep a reference to the SymbolTable associated with the
        # InvokeSchedule.
        self._symbol_table = None
        isched = self.ancestor(InvokeSchedule)
        if isched:
            self._symbol_table = isched.symbol_table

    @property
    def vector_index(self):
        '''If the field is a vector then return the vector index associated
        with this halo exchange. Otherwise return None'''
        return self._vector_index

    @property
    def halo_depth(self):
        ''' Return the depth of the halo exchange '''
        return self._halo_depth

    @halo_depth.setter
    def halo_depth(self, value):
        ''' Set the depth of the halo exchange '''
        self._halo_depth = value

    @property
    def field(self):
        ''' Return the field that the halo exchange acts on '''
        return self._field

    @property
    def dag_name(self):
        '''
        :returns: the name to use in a dag for this node.
        :rtype: str
        '''
        name = ("{0}({1})_{2}".format(self._text_name, self._field.name,
                                      self.position))
        if self._check_dirty:
            name = "check" + name
        return name

    @property
    def args(self):
        '''Return the list of arguments associated with this node. Overide the
        base method and simply return our argument. '''
        return [self._field]

    def check_vector_halos_differ(self, node):
        '''Helper method which checks that two halo exchange nodes (one being
        self and the other being passed by argument) operating on the
        same field, both have vector fields of the same size and use
        different vector indices. If this is the case then the halo
        exchange nodes do not depend on each other. If this is not the
        case then an internal error will have occured and we raise an
        appropriate exception.

        :param node: a halo exchange which should exchange the same field as \
                     self.
        :type node: :py:class:`psyclone.psyGen.HaloExchange`
        :raises GenerationError: if the argument passed is not a halo exchange.
        :raises GenerationError: if the field name in the halo exchange \
                                 passed in has a different name to the field \
                                 in this halo exchange.
        :raises GenerationError: if the field in this halo exchange is not a \
                                 vector field
        :raises GenerationError: if the vector size of the field in this halo \
                                 exchange is different to vector size of the \
                                 field in the halo exchange passed by argument.
        :raises GenerationError: if the vector index of the field in this \
                                 halo exchange is the same as the vector \
                                 index of the field in the halo exchange \
                                 passed by argument.

        '''

        if not isinstance(node, HaloExchange):
            raise GenerationError(
                "Internal error, the argument passed to "
                "HaloExchange.check_vector_halos_differ() is not "
                "a halo exchange object")

        if self.field.name != node.field.name:
            raise GenerationError(
                "Internal error, the halo exchange object passed to "
                "HaloExchange.check_vector_halos_differ() has a different "
                "field name '{0}' to self "
                "'{1}'".format(node.field.name, self.field.name))

        if self.field.vector_size <= 1:
            raise GenerationError(
                "Internal error, HaloExchange.check_vector_halos_differ() "
                "a halo exchange depends on another halo "
                "exchange but the vector size of field '{0}' is 1".
                format(self.field.name))

        if self.field.vector_size != node.field.vector_size:
            raise GenerationError(
                "Internal error, HaloExchange.check_vector_halos_differ() "
                "a halo exchange depends on another halo "
                "exchange but the vector sizes for field '{0}' differ".
                format(self.field.name))

        if self.vector_index == \
           node.vector_index:
            raise GenerationError(
                "Internal error, HaloExchange.check_vector_halos_differ() "
                "a halo exchange depends on another halo "
                "exchange but both vector id's ('{0}') of field '{1}' are "
                "the same".format(self.vector_index, self.field.name))

    def node_str(self, colour=True):
        '''
        Returns the name of this node with (optional) control codes
        to generate coloured output in a terminal that supports it.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        :rtype: str
        '''
        return ("{0}[field='{1}', type='{2}', depth={3}, "
                "check_dirty={4}]".format(
                    self.coloured_name(colour), self._field.name,
                    self._halo_type, self._halo_depth,
                    self._check_dirty))


class Kern(Statement):
    '''Base class representing a call to a sub-program unit from within the
    PSy layer. It is possible for this unit to be in-lined within the
    PSy layer.

    :param parent: parent of this node in the PSyIR.
    :type parent: sub-class of :py:class:`psyclone.psyir.nodes.Node`
    :param call: information on the call itself, as obtained by parsing \
                 the Algorithm layer code.
    :type call: :py:class:`psyclone.parse.algorithm.KernelCall`
    :param str name: the name of the routine being called.
    :param ArgumentsClass: class to create the object that holds all \
        information on the kernel arguments, as extracted from kernel \
        meta-data (and accessible here via call.ktype).
    :type ArgumentsClass: type of :py:class:`psyclone.psyGen.Arguments`
    :param bool check: whether to check for consistency between the \
        kernel metadata and the algorithm layer. Defaults to True.

    :raises GenerationError: if any of the arguments to the call are \
                             duplicated.

    '''
    # Textual representation of the valid children for this node.
    _children_valid_format = "<LeafNode>"

    def __init__(self, parent, call, name, ArgumentsClass, check=True):
        super(Kern, self).__init__(self, parent=parent)
        self._name = name
        self._iterates_over = call.ktype.iterates_over
        self._arguments = ArgumentsClass(call, self, check=check)

        # check algorithm arguments are unique for a kernel or
        # built-in call
        arg_names = []
        for arg in self._arguments.args:
            if arg.text:
                text = arg.text.lower().replace(" ", "")
                if text in arg_names:
                    raise GenerationError(
                        "Argument '{0}' is passed into kernel '{1}' code more "
                        "than once from the algorithm layer. This is not "
                        "allowed.".format(arg.text, self._name))
                arg_names.append(text)

        self._arg_descriptors = None

        # Initialise any reduction information
        reduction_modes = AccessType.get_valid_reduction_modes()
        const = Config.get().api_conf().get_constants()
        args = args_filter(self._arguments.args,
                           arg_types=const.VALID_SCALAR_NAMES,
                           arg_accesses=reduction_modes)
        if args:
            self._reduction = True
            if len(args) != 1:
                raise GenerationError(
                    "PSyclone currently only supports a single reduction "
                    "in a kernel or builtin")
            self._reduction_arg = args[0]
        else:
            self._reduction = False
            self._reduction_arg = None

    @property
    def args(self):
        '''Return the list of arguments associated with this node. Overide the
        base method and simply return our arguments. '''
        return self.arguments.args

    def node_str(self, colour=True):
        ''' Returns the name of this node with (optional) control codes
        to generate coloured output in a terminal that supports it.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        :rtype: str
        '''
        if self.name:
            return (self.coloured_name(colour) + " " + self.name +
                    "(" + self.arguments.names + ")")
        return self.coloured_name(colour) + "[]"

    def reference_accesses(self, var_accesses):
        '''Get all variable access information. The API specific classes
        add the accesses to the arguments. So the code here only calls
        the baseclass, and increases the location.

        :param var_accesses: VariablesAccessInfo instance that stores the \
            information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`
        '''
        super(Kern, self).reference_accesses(var_accesses)
        var_accesses.next_location()

    @property
    def is_reduction(self):
        '''if this kernel/builtin contains a reduction variable then return
        True, otherwise return False'''
        return self._reduction

    @property
    def reduction_arg(self):
        ''' if this kernel/builtin contains a reduction variable then return
        the variable, otherwise return None'''
        return self._reduction_arg

    @property
    def reprod_reduction(self):
        '''Determine whether this kernel/builtin is enclosed within an OpenMP
        do loop. If so report whether it has the reproducible flag
        set. Note, this also catches OMPParallelDo Directives but they
        have reprod set to False so it is OK.'''
        ancestor = self.ancestor(OMPDoDirective)
        if ancestor:
            return ancestor.reprod
        return False

    @property
    def local_reduction_name(self):
        '''Generate a local variable name that is unique for the current
        reduction argument name. This is used for thread-local
        reductions with reproducible reductions '''
        tag = self._reduction_arg.name
        name = self.ancestor(InvokeSchedule).symbol_table.\
            find_or_create_tag(tag, "l_" + tag).name
        return name

    def zero_reduction_variable(self, parent, position=None):
        '''
        Generate code to zero the reduction variable and to zero the local
        reduction variable if one exists. The latter is used for reproducible
        reductions, if specified.

        :param parent: the Node in the AST to which to add new code.
        :type parent: :py:class:`psyclone.psyir.nodes.Node`
        :param str position: where to position the new code in the AST.

        :raises GenerationError: if the variable to zero is not a scalar.
        :raises GenerationError: if the reprod_pad_size (read from the \
                                 configuration file) is less than 1.
        :raises GenerationError: for a reduction into a scalar that is \
                                 neither 'real' nor 'integer'.

        '''
        from psyclone.f2pygen import AssignGen, DeclGen, AllocateGen
        if not position:
            position = ["auto"]
        var_name = self._reduction_arg.name
        local_var_name = self.local_reduction_name
        var_arg = self._reduction_arg
        # Check for a non-scalar argument
        if not var_arg.is_scalar:
            raise GenerationError(
                "Kern.zero_reduction_variable() should be a scalar but "
                "found '{0}'.".format(var_arg.argument_type))
        # Generate the reduction variable
        var_data_type = var_arg.intrinsic_type
        if var_data_type == "real":
            data_value = "0.0"
        elif var_data_type == "integer":
            data_value = "0"
        else:
            raise GenerationError(
                "Kern.zero_reduction_variable() should be either a 'real' "
                "or an 'integer' scalar but found scalar of type '{0}'.".
                format(var_arg.intrinsic_type))
        # Retrieve the precision information (if set) and append it
        # to the initial reduction value
        if var_arg.precision:
            kind_type = var_arg.precision
            zero_sum_variable = "_".join([data_value, kind_type])
        else:
            kind_type = ""
            zero_sum_variable = data_value
        parent.add(AssignGen(parent, lhs=var_name, rhs=zero_sum_variable),
                   position=position)
        if self.reprod_reduction:
            parent.add(DeclGen(parent, datatype=var_data_type,
                               entity_decls=[local_var_name],
                               allocatable=True, kind=kind_type,
                               dimension=":,:"))
            nthreads = \
                self.scope.symbol_table.lookup_with_tag("omp_num_threads").name
            if Config.get().reprod_pad_size < 1:
                raise GenerationError(
                    "REPROD_PAD_SIZE in {0} should be a positive "
                    "integer, but it is set to '{1}'.".format(
                        Config.get().filename, Config.get().reprod_pad_size))
            pad_size = str(Config.get().reprod_pad_size)
            parent.add(AllocateGen(parent, local_var_name + "(" + pad_size +
                                   "," + nthreads + ")"), position=position)
            parent.add(AssignGen(parent, lhs=local_var_name,
                                 rhs=zero_sum_variable), position=position)

    def reduction_sum_loop(self, parent):
        '''
        Generate the appropriate code to place after the end parallel
        region.

        :param parent: the Node in the f2pygen AST to which to add new code.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        :raises GenerationError: for an unsupported reduction access in \
                                 LFRicBuiltIn.

        '''
        from psyclone.f2pygen import DoGen, AssignGen, DeallocateGen
        var_name = self._reduction_arg.name
        local_var_name = self.local_reduction_name
        local_var_ref = self._reduction_ref(var_name)
        reduction_access = self._reduction_arg.access
        try:
            reduction_operator = REDUCTION_OPERATOR_MAPPING[reduction_access]
        except KeyError as err:
            api_strings = [access.api_specific_name()
                           for access in REDUCTION_OPERATOR_MAPPING]
            six.raise_from(GenerationError(
                "Unsupported reduction access '{0}' found in LFRicBuiltIn:"
                "reduction_sum_loop(). Expected one of {1}.".
                format(reduction_access.api_specific_name(),
                       api_strings)), err)
        symtab = self.scope.symbol_table
        thread_idx = symtab.lookup_with_tag("omp_thread_index").name
        nthreads = symtab.lookup_with_tag("omp_num_threads").name
        do_loop = DoGen(parent, thread_idx, "1", nthreads)
        do_loop.add(AssignGen(do_loop, lhs=var_name, rhs=var_name +
                              reduction_operator + local_var_ref))
        parent.add(do_loop)
        parent.add(DeallocateGen(parent, local_var_name))

    def _reduction_ref(self, name):
        '''Return the name unchanged if OpenMP is set to be unreproducible, as
        we will be using the OpenMP reduction clause. Otherwise we
        will be computing the reduction ourselves and therefore need
        to store values into a (padded) array separately for each
        thread.

        :param str name: original name of the variable to be reduced.

        '''
        symtab = self.scope.symbol_table
        if self.reprod_reduction:
            idx_name = symtab.lookup_with_tag("omp_thread_index").name
            local_name = symtab.find_or_create_tag(name, "l_" + name).name
            return local_name + "(1," + idx_name + ")"
        return name

    @property
    def arg_descriptors(self):
        return self._arg_descriptors

    @arg_descriptors.setter
    def arg_descriptors(self, obj):
        self._arg_descriptors = obj

    @property
    def arguments(self):
        return self._arguments

    @property
    def name(self):
        '''
        :returns: the name of the kernel.
        :rtype: str
        '''
        return self._name

    @name.setter
    def name(self, value):
        '''
        Set the name of the kernel.

        :param str value: The name of the kernel.
        '''
        self._name = value

    def is_coloured(self):
        '''
        :returns: True if this kernel is being called from within a \
                  coloured loop.
        :rtype: bool
        '''
        parent_loop = self.ancestor(Loop)
        while parent_loop:
            if parent_loop.loop_type == "colour":
                return True
            parent_loop = parent_loop.ancestor(Loop)
        return False

    @property
    def iterates_over(self):
        return self._iterates_over

    def local_vars(self):
        raise NotImplementedError("Kern.local_vars should be implemented")

    def gen_code(self, parent):
        raise NotImplementedError("Kern.gen_code should be implemented")


class CodedKern(Kern):
    '''
    Class representing a call to a PSyclone Kernel with a user-provided
    implementation. The kernel may or may not be in-lined.

    :param type KernelArguments: the API-specific sub-class of \
                                 :py:class:`psyclone.psyGen.Arguments` to \
                                 create.
    :param call: Details of the call to this kernel in the Algorithm layer.
    :type call: :py:class:`psyclone.parse.algorithm.KernelCall`.
    :param parent: the parent of this Node (kernel call) in the Schedule.
    :type parent: sub-class of :py:class:`psyclone.psyir.nodes.Node`.
    :param bool check: whether to check for consistency between the \
        kernel metadata and the algorithm layer. Defaults to True.

    '''
    # Textual description of the node.
    _text_name = "CodedKern"
    _colour = "magenta"

    def __init__(self, KernelArguments, call, parent=None, check=True):
        # Set module_name first in case there is an error when
        # processing arguments, as we can then return the module_name
        # from where it happened.
        self._module_name = call.module_name
        super(CodedKern, self).__init__(parent, call,
                                        call.ktype.procedure.name,
                                        KernelArguments, check)
        self._module_code = call.ktype._ast
        self._kernel_code = call.ktype.procedure
        self._fp2_ast = None  # The fparser2 AST for the kernel
        self._kern_schedule = None  # PSyIR schedule for the kernel
        # Whether or not this kernel has been transformed
        self._modified = False
        # Whether or not to in-line this kernel into the module containing
        # the PSy layer
        self._module_inline = False
        self._opencl_options = {'local_size': 64, 'queue_number': 1}
        self.arg_descriptors = call.ktype.arg_descriptors

    def get_kernel_schedule(self):
        '''
        Returns a PSyIR Schedule representing the kernel code. The Schedule
        is just generated on first invocation, this allows us to retain
        transformations that may subsequently be applied to the Schedule.

        :returns: Schedule representing the kernel code.
        :rtype: :py:class:`psyclone.psyir.nodes.KernelSchedule`
        '''
        from psyclone.psyir.frontend.fparser2 import Fparser2Reader
        if self._kern_schedule is None:
            astp = Fparser2Reader()
            self._kern_schedule = astp.generate_schedule(self.name, self.ast)
            # TODO: Validate kernel with metadata (issue #288).
        return self._kern_schedule

    @property
    def opencl_options(self):
        '''
        :returns: dictionary of OpenCL options regarding the kernel.
        :rtype: dictionary
        '''
        return self._opencl_options

    def set_opencl_options(self, options):
        '''
        Validate and store a set of options associated with the Kernel to
        tune the OpenCL code generation.

        :param options: a set of options to tune the OpenCL code.
        :type options: dictionary of <string>:<value>

        '''
        valid_opencl_kernel_options = ['local_size', 'queue_number']

        # Validate that the options given are supported
        for key, value in options.items():
            if key in valid_opencl_kernel_options:
                if key == "local_size":
                    if not isinstance(value, int):
                        raise TypeError(
                            "CodedKern OpenCL option 'local_size' should be "
                            "an integer.")
                if key == "queue_number":
                    if not isinstance(value, int):
                        raise TypeError(
                            "CodedKern OpenCL option 'queue_number' should be "
                            "an integer.")
            else:
                raise AttributeError(
                    "CodedKern does not support the OpenCL option '{0}'. "
                    "The supported options are: {1}."
                    "".format(key, valid_opencl_kernel_options))

            self._opencl_options[key] = value

    def __str__(self):
        return "kern call: " + self._name

    @property
    def module_name(self):
        '''
        :returns: The name of the Fortran module that contains this kernel
        :rtype: string
        '''
        return self._module_name

    @property
    def dag_name(self):
        '''
        :returns: the name to use in the DAG for this node.
        :rtype: str
        '''
        _, position = self._find_position(self.ancestor(Routine))
        return "kernel_{0}_{1}".format(self.name, str(position))

    @property
    def module_inline(self):
        '''
        :returns: whether or not this kernel is being module-inlined.
        :rtype: bool
        '''
        return self._module_inline

    @module_inline.setter
    def module_inline(self, value):
        '''
        Setter for whether or not to module-inline this kernel.

        :param bool value: whether or not to module-inline this kernel.
        '''
        # Check all kernels in the same invoke as this one and set any
        # with the same name to the same value as this one. This is
        # required as inlining (or not) affects all calls to the same
        # kernel within an invoke.
        my_schedule = self.ancestor(InvokeSchedule)
        for kernel in my_schedule.walk(Kern):
            if kernel is self:
                self._module_inline = value
            elif kernel.name == self.name and kernel.module_inline != value:
                kernel.module_inline = value

    def node_str(self, colour=True):
        ''' Returns the name of this node with (optional) control codes
        to generate coloured output in a terminal that supports it.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        :rtype: str
        '''
        return (self.coloured_name(colour) + " " + self.name + "(" +
                self.arguments.names + ") " + "[module_inline=" +
                str(self._module_inline) + "]")

    def lower_to_language_level(self):
        '''
        In-place replacement of CodedKern concept into language level
        PSyIR constructs. The CodedKern is implemented as a Call to a
        routine with the appropriate arguments.

        '''
        # If the kernel has been transformed and it is not module inlined
        # then we rename it.
        if not self.module_inline:
            self.rename_and_write()
        else:
            # Inline the kernel subroutine
            self._insert_module_inlined_kernel()

        # Create the appropriate symbols
        symtab = self.ancestor(InvokeSchedule).symbol_table
        try:
            rsymbol = symtab.lookup(self._name)
        except KeyError:
            rsymbol = RoutineSymbol(self._name)
            symtab.add(rsymbol)
            if not self.module_inline:
                # Import subroutine symbol
                try:
                    csymbol = symtab.lookup(self._module_name)
                except KeyError:
                    csymbol = ContainerSymbol(self._module_name)
                    symtab.add(csymbol)
                rsymbol.interface = ImportInterface(csymbol)

        # Create Call to the rsymbol with the argument expressions as children
        # of the new node
        call_node = Call.create(rsymbol, self.arguments.psyir_expressions())

        # Swap itself with the appropriate Call node
        self.replace_with(call_node)

    def _insert_module_inlined_kernel(self, f2pygen_parent=None):
        ''' Module-inline this kernel into the tree if it hasn't been inlined
        previously yet. Currently this needs to be done for the PSyIR tree and
        the f2pygen tree. If the f2pygen_parent argument is None it will be
        inlined in the PSyIR tree, otherwise it will be inlined in the provided
        parent

        :param parent: The parent of this kernel call in the f2pygen AST.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen` or NoneType

        :raises NotImplementedError: if there is a name clash that prevents \
            the kernel from being module-inlined without changing its name.

        '''
        # Check for name clashes
        try:
            # Disable false positive no-member issue
            # pylint: disable=no-member
            existing_symbol = self.scope.symbol_table.lookup(self._name)
        except KeyError:
            existing_symbol = None

        if not existing_symbol:
            # If it doesn't exist already, module-inline the subroutine by:
            # 1) Registering the subroutine symbol in the Container
            self.root.symbol_table.add(RoutineSymbol(self._name))
            # 2) Insert the relevant code into the tree.
            inlined_code = self.get_kernel_schedule()
            if f2pygen_parent:
                # If we are building a f2pygen tree add it to a PSyIRGen node
                # under the PSy-layer f2pygen module.
                module = f2pygen_parent
                while module.parent:
                    module = module.parent
                module.add(PSyIRGen(module, inlined_code))
            else:
                # Otherwise just add it to the current PSyIR tree
                self.root.addchild(inlined_code.detach())

        else:
            # If the symbol already exists, make sure it refers
            # to the exact same subroutine.
            if not isinstance(existing_symbol, RoutineSymbol):
                raise NotImplementedError(
                    "Can not module-inline subroutine '{0}' because symbol"
                    "'{1}' with the same name already exists and changing"
                    " names of module-inlined subroutines is not "
                    "implemented yet.".format(self._name, existing_symbol))

            # Make sure the generated code is an exact match by creating
            # the f2pygen node (which in turn creates the fparser1) of the
            # kernel_schedule and then compare it to the fparser1 trees of
            # the PSyIRGen f2pygen nodes children of module.
            if f2pygen_parent:
                module = f2pygen_parent
                while module.parent:
                    module = module.parent
                search = PSyIRGen(module, self.get_kernel_schedule()).root
                for child in module.children:
                    if isinstance(child, PSyIRGen):
                        if child.root == search:
                            # If there is an exact match (the implementation is
                            # the same), it is safe to continue.
                            break
                else:
                    raise NotImplementedError(
                        "Can not inline subroutine '{0}' because another, "
                        "different, subroutine with the same name already "
                        "exists and versioning of module-inlined subroutines "
                        "is not implemented yet.".format(self._name))

    def gen_code(self, parent):
        '''
        Generates the f2pygen AST of the Fortran for this kernel call and
        writes the kernel itself to file if it has been transformed.

        :param parent: The parent of this kernel call in the f2pygen AST.
        :type parent: :py:class:`psyclone.f2pygen.LoopGen`

        '''
        # If the kernel has been transformed then we rename it. If it
        # is *not* being module inlined then we also write it to file.
        self.rename_and_write()

        # Add the subroutine call with the necessary arguments
        arguments = self.arguments.raw_arg_list()
        parent.add(CallGen(parent, self._name, arguments))

        # Also add the subroutine declaration, this can just be the import
        # statement, or the whole subroutine inlined into the module.
        if not self.module_inline:
            parent.add(UseGen(parent, name=self._module_name, only=True,
                              funcnames=[self._name]))
        else:
            self._insert_module_inlined_kernel(parent)

    def incremented_arg(self):
        ''' Returns the argument that has INC access. Raises a
        FieldNotFoundError if none is found.

        :rtype: str
        :raises FieldNotFoundError: if none is found.
        :returns: a Fortran argument name.
        '''
        for arg in self.arguments.args:
            if arg.access == AccessType.INC:
                return arg

        raise FieldNotFoundError("Kernel {0} does not have an argument with "
                                 "{1} access".
                                 format(self.name,
                                        AccessType.INC.api_specific_name()))

    @property
    def ast(self):
        '''
        Generate and return the fparser2 AST of the kernel source.

        :returns: fparser2 AST of the Fortran file containing this kernel.
        :rtype: :py:class:`fparser.two.Fortran2003.Program`
        '''
        from fparser.common.readfortran import FortranStringReader
        from fparser.two import parser
        # If we've already got the AST then just return it
        if self._fp2_ast:
            return self._fp2_ast
        # Use the fparser1 AST to generate Fortran source
        fortran = self._module_code.tofortran()
        # Create an fparser2 Fortran2008 parser
        my_parser = parser.ParserFactory().create(std="f2008")
        # Parse that Fortran using our parser
        reader = FortranStringReader(fortran)
        self._fp2_ast = my_parser(reader)
        return self._fp2_ast

    @staticmethod
    def _new_name(original, tag, suffix):
        '''
        Construct a new name given the original, a tag and a suffix (which
        may or may not terminate the original name). If suffix is present
        in the original name then the `tag` is inserted before it.

        :param str original: The original name
        :param str tag: Tag to insert into new name
        :param str suffix: Suffix with which to end new name.
        :returns: New name made of original + tag + suffix
        :rtype: str
        '''
        if original.endswith(suffix):
            return original[:-len(suffix)] + tag + suffix
        return original + tag + suffix

    def rename_and_write(self):
        '''
        Writes the (transformed) AST of this kernel to file and resets the
        'modified' flag to False. By default (config.kernel_naming ==
        "multiple"), the kernel is re-named so as to be unique within
        the kernel output directory stored within the configuration
        object. Alternatively, if config.kernel_naming is "single"
        then no re-naming and output is performed if there is already
        a transformed copy of the kernel in the output dir. (In this
        case a check is performed that the transformed kernel already
        present is identical to the one that we would otherwise write
        to file. If this is not the case then we raise a GenerationError.)

        :raises GenerationError: if config.kernel_naming == "single" and a \
                                 different, transformed version of this \
                                 kernel is already in the output directory.
        :raises NotImplementedError: if the kernel has been transformed but \
                                     is also flagged for module-inlining.

        '''
        import os
        from psyclone.line_length import FortLineLength

        # If this kernel has not been transformed we do nothing
        if not self.modified:
            return

        # Remove any "_mod" if the file follows the PSyclone naming convention
        orig_mod_name = self.module_name[:]
        if orig_mod_name.lower().endswith("_mod"):
            old_base_name = orig_mod_name[:-4]
        else:
            old_base_name = orig_mod_name[:]

        # We could create a hash of a string built from the name of the
        # Algorithm (module), the name/position of the Invoke and the
        # index of this kernel within that Invoke. However, that creates
        # a very long name so we simply ensure that kernel names are unique
        # within the user-supplied kernel-output directory.
        name_idx = -1
        fdesc = None
        while not fdesc:
            name_idx += 1
            new_suffix = ""

            new_suffix += "_{0}".format(name_idx)
            new_name = old_base_name + new_suffix + "_mod.f90"

            try:
                # Atomically attempt to open the new kernel file (in case
                # this is part of a parallel build)
                fdesc = os.open(
                    os.path.join(Config.get().kernel_output_dir, new_name),
                    os.O_CREAT | os.O_WRONLY | os.O_EXCL)
            except (OSError, IOError):
                # The os.O_CREATE and os.O_EXCL flags in combination mean
                # that open() raises an error if the file exists
                if Config.get().kernel_naming == "single":
                    # If the kernel-renaming scheme is such that we only ever
                    # create one copy of a transformed kernel then we're done
                    break
                continue

        # Use the suffix we have determined to rename all relevant quantities
        # within the AST of the kernel code.
        self._rename_psyir(new_suffix)

        # Kernel is now self-consistent so unset the modified flag
        self.modified = False

        # If this kernel is being module in-lined then we do not need to
        # write it to file.
        if self.module_inline:
            # TODO #1013: However, the file is already created (opened) and
            # currently this file is needed for the name versioning, so this
            # will create an unnecessary file.
            os.close(fdesc)
            return

        # If we reach this point the kernel needs to be written out into a
        # file using a PSyIR back-end. At the moment there is no way to choose
        # which back-end to use, so simply use the Fortran one (and limit the
        # line length).
        fortran_writer = FortranWriter()
        # Start from the root of the schedule as we want to output
        # any module information surrounding the kernel subroutine
        # as well as the subroutine itself.
        new_kern_code = fortran_writer(self.get_kernel_schedule().root)
        fll = FortLineLength()
        new_kern_code = fll.process(new_kern_code)

        if not fdesc:
            # If we've not got a file descriptor at this point then that's
            # because the file already exists and the kernel-naming scheme
            # ("single") means we're not creating a new one.
            # Check that what we've got is the same as what's in the file
            with open(os.path.join(Config.get().kernel_output_dir,
                                   new_name), "r") as ffile:
                kern_code = ffile.read()
                if kern_code != new_kern_code:
                    raise GenerationError(
                        "A transformed version of this Kernel '{0}' already "
                        "exists in the kernel-output directory ({1}) but is "
                        "not the same as the current, transformed kernel and "
                        "the kernel-renaming scheme is set to '{2}'. (If you "
                        "wish to generate a new, unique kernel for every "
                        "kernel that is transformed then use "
                        "'--kernel-renaming multiple'.)".
                        format(self._module_name+".f90",
                               Config.get().kernel_output_dir,
                               Config.get().kernel_naming))
        else:
            # Write the modified AST out to file
            os.write(fdesc, new_kern_code.encode())
            # Close the new kernel file
            os.close(fdesc)

    def _rename_psyir(self, suffix):
        '''Rename the PSyIR module and kernel names by adding the supplied
        suffix to the names. This change affects the KernCall and
        KernelSchedule nodes as well as the kernel metadata declaration.

        :param str suffix: the string to insert into the quantity names.

        '''
        # We need to get the kernel schedule before modifying self.name
        kern_schedule = self.get_kernel_schedule()

        # Use the suffix to create a new kernel name.  This will
        # conform to the PSyclone convention of ending in "_code"
        orig_mod_name = self.module_name[:]
        orig_kern_name = self.name[:]

        new_kern_name = self._new_name(orig_kern_name, suffix, "_code")
        new_mod_name = self._new_name(orig_mod_name, suffix, "_mod")

        # Change the name of this kernel and the associated
        # module. These names are used when generating the PSy-layer.
        self.name = new_kern_name[:]
        self._module_name = new_mod_name[:]

        kern_schedule.name = new_kern_name[:]
        kern_schedule.root.name = new_mod_name[:]

        # Change the name of the symbol
        try:
            kern_symbol = kern_schedule.symbol_table.lookup(orig_kern_name)
            kern_schedule.root.symbol_table.rename_symbol(kern_symbol,
                                                          new_kern_name)
        except KeyError:
            # TODO #1013. Right now not all tests have PSyIR symbols because
            # some only expect f2pygen generation.
            pass

        # TODO #1013. This needs re-doing properly - in particular the
        # RoutineSymbol associated with the kernel needs to be replaced. For
        # now we only fix the specific case of the name of the kernel routine
        # in the kernel metadata as otherwise various compilation tests
        # fail.
        container_table = kern_schedule.root.symbol_table
        for sym in container_table.local_datatypesymbols:
            if isinstance(sym.datatype, UnknownFortranType):
                orig_declaration = sym.datatype.declaration
                sym.datatype.declaration = orig_declaration.replace(
                    orig_kern_name, new_kern_name)

    @property
    def modified(self):
        '''
        :returns: Whether or not this kernel has been modified (transformed).
        :rtype: bool
        '''
        return self._modified

    @modified.setter
    def modified(self, value):
        '''
        Setter for whether or not this kernel has been modified.

        :param bool value: True if kernel modified, False otherwise.
        '''
        self._modified = value


class InlinedKern(Kern):
    '''A class representing a kernel that is inlined. This is used by
    the NEMO API, since the NEMO API has no function to call or parameters.
    It has one child which stores the Schedule for the child nodes.

    :param psyir_nodes: the list of PSyIR nodes that represent the body \
                        of this kernel.
    :type psyir_nodes: list of :py:class:`psyclone.psyir.nodes.Node`
    :param parent: the parent of this node in the PSyIR.
    :type parent: sub-class of :py:class:`psyclone.psyir.nodes.Node`
    '''
    # Textual description of the node.
    _children_valid_format = "Schedule"
    _text_name = "InlinedKern"
    _colour = "magenta"

    def __init__(self, psyir_nodes, parent=None):
        # pylint: disable=non-parent-init-called, super-init-not-called
        Node.__init__(self, parent=parent)
        schedule = Schedule(children=psyir_nodes, parent=self)
        self.children = [schedule]
        self._arguments = None

    @staticmethod
    def _validate_child(position, child):
        '''
        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        return position == 0 and isinstance(child, Schedule)

    @abc.abstractmethod
    def local_vars(self):
        '''
        :returns: list of the variable (names) that are local to this kernel \
                  (and must therefore be e.g. threadprivate if doing OpenMP)
        :rtype: list of str
        '''


class BuiltIn(Kern):
    '''
    Parent class for all built-ins (field operations for which the user
    does not have to provide an implementation).
    '''
    # Textual description of the node.
    _text_name = "BuiltIn"
    _colour = "magenta"

    def __init__(self):
        # We cannot call Kern.__init__ as don't have necessary information
        # here. Instead we provide a load() method that can be called once
        # that information is available.
        self._arg_descriptors = None
        self._func_descriptors = None
        self._fs_descriptors = None
        self._reduction = None

    @property
    def dag_name(self):
        '''
        :returns: the name to use in the DAG for this node.
        :rtype: str
        '''
        _, position = self._find_position(self.ancestor(Routine))
        return "builtin_{0}_{1}".format(self.name, str(position))

    def load(self, call, arguments, parent=None):
        ''' Set-up the state of this BuiltIn call '''
        name = call.ktype.name
        super(BuiltIn, self).__init__(parent, call, name, arguments)

    def local_vars(self):
        '''Variables that are local to this built-in and therefore need to be
        made private when parallelising using OpenMP or similar. By default
        builtin's do not have any local variables so set to nothing'''
        return []


class Arguments(object):
    '''
    Arguments abstract base class.

    :param parent_call: kernel call with which the arguments are associated.
    :type parent_call: sub-class of :py:class:`psyclone.psyGen.Kern`
    '''
    def __init__(self, parent_call):
        self._parent_call = parent_call
        # The container object holding information on all arguments
        # (derived from both kernel meta-data and the kernel call
        # in the Algorithm layer).
        self._args = []
        # The actual list of arguments that must be supplied to a
        # subroutine call.
        self._raw_arg_list = []

    def raw_arg_list(self):
        '''
        Abstract method to construct the class-specific argument list for a
        kernel call. Must be overridden in API-specific sub-class.

        :raises NotImplementedError: abstract method.
        '''
        raise NotImplementedError("Arguments.raw_arg_list must be "
                                  "implemented in sub-class")

    @abc.abstractmethod
    def psyir_expressions(self):
        '''
        :returns: the PSyIR expressions representing this Argument list.
        :rtype: list of :py:class:`psyclone.psyir.nodes.Node`

        '''

    @property
    def names(self):
        '''
        :returns: the Algorithm-visible kernel arguments in a \
                  comma-delimited string.
        :rtype: str
        '''
        return ",".join([arg.name for arg in self.args])

    @property
    def args(self):
        return self._args

    def iteration_space_arg(self):
        '''
        Returns an argument that can be iterated over, i.e. modified
        (has WRITE, READWRITE or INC access), but not the result of
        a reduction operation.

        :returns: a Fortran argument name
        :rtype: string
        :raises GenerationError: if none such argument is found.

        '''
        for arg in self._args:
            if arg.access in AccessType.all_write_accesses() and \
                    arg.access not in AccessType.get_valid_reduction_modes():
                return arg
        raise GenerationError("psyGen:Arguments:iteration_space_arg Error, "
                              "we assume there is at least one writer, "
                              "reader/writer, or increment as an argument")

    @property
    def acc_args(self):
        '''
        :returns: the list of quantities that must be available on an \
                  OpenACC device before the associated kernel can be launched
        :rtype: list of str
        '''
        raise NotImplementedError(
            "Arguments.acc_args must be implemented in sub-class")

    @property
    def scalars(self):
        '''
        :returns: the list of scalar quantities belonging to this object
        :rtype: list of str
        '''
        raise NotImplementedError(
            "Arguments.scalars must be implemented in sub-class")

    def append(self, name, argument_type):
        ''' Abstract method to append KernelArguments to the Argument
        list.

        :param str name: name of the appended argument.
        :param str argument_type: type of the appended argument.
        '''
        raise NotImplementedError(
            "Arguments.append must be implemented in sub-class")


class DataAccess(object):
    '''A helper class to simplify the determination of dependencies due to
    overlapping accesses to data associated with instances of the
    Argument class.

    '''

    def __init__(self, arg):
        '''Store the argument associated with the instance of this class and
        the Call, HaloExchange or GlobalSum (or a subclass thereof)
        instance with which the argument is associated.

        :param arg: the argument that we are concerned with. An \
        argument can be found in a `Kern` a `HaloExchange` or a \
        `GlobalSum` (or a subclass thereof)
        :type arg: :py:class:`psyclone.psyGen.Argument`

        '''
        # the `psyclone.psyGen.Argument` we are concerned with
        self._arg = arg
        # The call (Kern, HaloExchange, GlobalSum or subclass)
        # instance with which the argument is associated
        self._call = arg.call
        # initialise _covered and _vector_index_access to keep pylint
        # happy
        self._covered = None
        self._vector_index_access = None
        # Now actually set them to the required initial values
        self.reset_coverage()

    def overlaps(self, arg):
        '''Determine whether the accesses to the provided argument overlap
        with the accesses of the source argument. Overlap means that
        the accesses share at least one memory location. For example,
        the arguments both access the 1st index of the same field.

        We do not currently deal with accesses to a subset of an
        argument (unless it is a vector). This distinction will need
        to be added once loop splitting is supported.

        :param arg: the argument to compare with our internal argument
        :type arg: :py:class:`psyclone.psyGen.Argument`
        :return bool: True if there are overlapping accesses between \
                      arguments (i.e. accesses share at least one memory \
                      location) and False if not.

        '''
        if self._arg.name != arg.name:
            # the arguments are different args so do not overlap
            return False

        if isinstance(self._call, HaloExchange) and \
           isinstance(arg.call, HaloExchange) and \
           (self._arg.vector_size > 1 or arg.vector_size > 1):
            # This is a vector field and both accesses come from halo
            # exchanges. As halo exchanges only access a particular
            # vector, the accesses do not overlap if the vector indices
            # being accessed differ.

            # sanity check
            if self._arg.vector_size != arg.vector_size:
                raise InternalError(
                    "DataAccess.overlaps(): vector sizes differ for field "
                    "'{0}' in two halo exchange calls. Found '{1}' and "
                    "'{2}'".format(arg.name, self._arg.vector_size,
                                   arg.vector_size))
            if self._call.vector_index != arg.call.vector_index:
                # accesses are to different vector indices so do not overlap
                return False
        # accesses do overlap
        return True

    def reset_coverage(self):
        '''Reset internal state to allow re-use of the object for a different
        situation.

        '''
        # False unless all data accessed by our local argument has
        # also been accessed by other arguments.
        self._covered = False
        # Used to store individual vector component accesses when
        # checking that all vector components have been accessed.
        self._vector_index_access = []

    def update_coverage(self, arg):
        '''Record any overlap between accesses to the supplied argument and
        the internal argument. Overlap means that the accesses to the
        two arguments share at least one memory location. If the
        overlap results in all of the accesses to the internal
        argument being covered (either directly or as a combination
        with previous arguments) then ensure that the covered() method
        returns True. Covered means that all memory accesses by the
        internal argument have at least one corresponding access by
        the supplied arguments.

        :param arg: the argument used to compare with our internal \
                    argument in order to update coverage information
        :type arg: :py:class:`psyclone.psyGen.Argument`

        '''

        if not self.overlaps(arg):
            # There is no overlap so there is nothing to update.
            return

        if isinstance(arg.call, HaloExchange) and \
           (hasattr(self._arg, 'vector_size') and self._arg.vector_size > 1):
            # The supplied argument is a vector field coming from a
            # halo exchange and therefore only accesses one of the
            # vectors

            if isinstance(self._call, HaloExchange):
                # I am also a halo exchange so only access one of the
                # vectors. At this point the vector indices of the two
                # halo exchange fields must be the same, which should
                # never happen due to checks in the `overlaps()`
                # method earlier
                raise InternalError(
                    "DataAccess:update_coverage() The halo exchange vector "
                    "indices for '{0}' are the same. This should never "
                    "happen".format(self._arg.name))
            else:
                # I am not a halo exchange so access all components of
                # the vector. However, the supplied argument is a halo
                # exchange so only accesses one of the
                # components. This results in partial coverage
                # (i.e. the overlap in accesses is partial). Therefore
                # record the index that is accessed and check whether
                # all indices are now covered (which would mean `full`
                # coverage).
                if arg.call.vector_index in self._vector_index_access:
                    raise InternalError(
                        "DataAccess:update_coverage() Found more than one "
                        "dependent halo exchange with the same vector index")
                self._vector_index_access.append(arg.call.vector_index)
                if len(self._vector_index_access) != self._arg.vector_size:
                    return
        # This argument is covered i.e. all accesses by the
        # internal argument have a corresponding access in one of the
        # supplied arguments.
        self._covered = True

    @property
    def covered(self):
        '''Returns True if all of the data associated with this argument has
        been covered by the arguments provided in update_coverage

        :return bool: True if all of an argument is covered by \
        previous accesses and False if not.

        '''
        return self._covered


class Argument(object):
    '''
    Argument base class. Captures information on an argument that is passed
    to a Kernel from an Invoke.

    :param call: the kernel call that this argument is associated with.
    :type call: :py:class:`psyclone.psyGen.Kern`
    :param arg_info: Information about this argument collected by \
                     the parser.
    :type arg_info: :py:class:`psyclone.parse.algorithm.Arg`
    :param access: the way in which this argument is accessed in \
                   the 'Kern'. Valid values are specified in the config \
                   object of the current API.
    :type access: str

    '''
    def __init__(self, call, arg_info, access):
        self._call = call
        if arg_info is not None:
            self._text = arg_info.text
            self._orig_name = arg_info.varname
            self._form = arg_info.form
            self._is_literal = arg_info.is_literal()
        else:
            self._text = ""
            self._orig_name = ""
            self._form = ""
            self._is_literal = False
        # Initialise access
        self._access = access
        # Default the precision, data type and module to 'None' (no
        # explicit property specified)
        self._precision = None
        self._data_type = None
        self._module_name = None
        # Default the name to the original name for debugging
        # purposes. This may be updated when _complete_init() is
        # called.
        self._name = self._orig_name

    def _complete_init(self, arg_info):
        '''Provides the initialisation of name, text and the declaration of
        symbols in the symbol table if required. This initialisation
        is not performed in the constructor as subclasses may need to
        perform additional initialisation before infer_datatype is
        called (in order to determine the values of precision,
        data_type and module_name).

        :param arg_info: Information about this argument collected by \
            the parser.
        :type arg_info: :py:class:`psyclone.parse.algorithm.Arg`

        '''
        if self._orig_name is None:
            # this is an infrastructure call literal argument. Therefore
            # we do not want an argument (_text=None) but we do want to
            # keep the value (_name)
            self._name = arg_info.text
            self._text = None
        else:
            # There are unit-tests where we create Arguments without an
            # associated call or InvokeSchedule.
            if self._call and self._call.ancestor(InvokeSchedule):
                symtab = self._call.ancestor(InvokeSchedule).symbol_table

                # Keep original list of arguments
                previous_arguments = symtab.argument_list

                # Find the tag to use
                tag = "AlgArgs_" + self._text

                # Prepare the Argument Interface Access value
                argument_access = ArgumentInterface.Access.READWRITE

                # Find the tag or create a new symbol with expected attributes
                new_argument = symtab.find_or_create_tag(
                    tag, root_name=self._orig_name, symbol_type=DataSymbol,
                    datatype=self.infer_datatype(),
                    interface=ArgumentInterface(argument_access))
                self._name = new_argument.name

                # Unless the argument already exists with another interface
                # (e.g. import) they come from the invoke argument list
                if (isinstance(new_argument.interface, ArgumentInterface) and
                        new_argument not in previous_arguments):
                    symtab.specify_argument_list(previous_arguments +
                                                 [new_argument])

    @abc.abstractmethod
    def psyir_expression(self):
        '''
        :returns: the PSyIR expression represented by this Argument.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        '''

    def infer_datatype(self):
        ''' Infer the datatype of this argument using the API rules. If no
        specialisation of this method has been provided make the type
        DeferredType for now (it may be provided later in the execution).

        :returns: the datatype of this argument.
        :rtype: :py:class::`psyclone.psyir.symbols.DataType`

        '''
        # pylint: disable=no-self-use
        return DeferredType()

    def __str__(self):
        return self._name

    @property
    def name(self):
        return self._name

    @property
    def text(self):
        return self._text

    @property
    def form(self):
        return self._form

    @property
    def is_literal(self):
        return self._is_literal

    @property
    def access(self):
        return self._access

    @access.setter
    def access(self, value):
        '''Set the access type for this argument.

        :param value: new access type.
        :type value: :py:class:`psyclone.core.access_type.AccessType`.

        :raises InternalError: if value is not an AccessType.

        '''
        if not isinstance(value, AccessType):
            raise InternalError("Invalid access type '{0}' of type '{1}."
                                .format(value, type(value)))

        self._access = value

    @property
    def argument_type(self):
        '''
        Returns the type of the argument. APIs that do not have this
        concept can use this base class version which just returns "field"
        in all cases. APIs with this concept can override this method.

        :returns: the API type of the kernel argument.
        :rtype: str

        '''
        return "field"

    @property
    @abc.abstractmethod
    def intrinsic_type(self):
        '''
        Abstract property for the intrinsic type of the argument with
        specific implementations in different APIs.

        :returns: the intrinsic type of this argument.
        :rtype: str

        '''

    @property
    def precision(self):
        '''
        :returns: the precision of this argument. Default value is None, \
                  explicit implementation is left to a specific API.
        :rtype: str or NoneType

        '''
        return self._precision

    @property
    def data_type(self):
        '''
        :returns: the data type of this argument. Default value is None, \
                  explicit implementation is left to a specific API.
        :rtype: str or NoneType

        '''
        return self._data_type

    @property
    def module_name(self):
        '''
        :returns: the name of the Fortran module that contains definitions \
                  for the argument data type. Default value is None, \
                  explicit implementation is left to a specific API.
        :rtype: str or NoneType


        '''
        return self._module_name

    @property
    def call(self):
        ''' Return the call that this argument is associated with '''
        return self._call

    @call.setter
    def call(self, value):
        ''' set the node that this argument is associated with '''
        self._call = value

    def backward_dependence(self):
        '''Returns the preceding argument that this argument has a direct
        dependence with, or None if there is not one. The argument may
        exist in a call, a haloexchange, or a globalsum.

        :returns: the first preceding argument that has a dependence \
            on this argument.
        :rtype: :py:class:`psyclone.psyGen.Argument`

        '''
        nodes = self._call.preceding(reverse=True)
        return self._find_argument(nodes)

    def forward_write_dependencies(self, ignore_halos=False):
        '''Returns a list of following write arguments that this argument has
        dependencies with. The arguments may exist in a call, a
        haloexchange (unless `ignore_halos` is `True`), or a globalsum. If
        none are found then return an empty list. If self is not a
        reader then return an empty list.

        :param bool ignore_halos: if `True` then any write dependencies \
            involving a halo exchange are ignored. Defaults to `False`.

        :returns: a list of arguments that have a following write \
            dependence on this argument.
        :rtype: list of :py:class:`psyclone.psyGen.Argument`

        '''
        nodes = self._call.following()
        results = self._find_write_arguments(nodes, ignore_halos=ignore_halos)
        return results

    def backward_write_dependencies(self, ignore_halos=False):
        '''Returns a list of previous write arguments that this argument has
        dependencies with. The arguments may exist in a call, a
        haloexchange (unless `ignore_halos` is `True`), or a globalsum. If
        none are found then return an empty list. If self is not a
        reader then return an empty list.

        :param ignore_halos: if `True` then any write dependencies \
            involving a halo exchange are ignored. Defaults to `False`.
        :type ignore_halos: bool

        :returns: a list of arguments that have a preceding write \
            dependence on this argument.
        :rtype: list of :py:class:`psyclone.psyGen.Argument`

        '''
        nodes = self._call.preceding(reverse=True)
        results = self._find_write_arguments(nodes, ignore_halos=ignore_halos)
        return results

    def forward_dependence(self):
        '''Returns the following argument that this argument has a direct
        dependence on, or `None` if there is not one. The argument may
        exist in a call, a haloexchange, or a globalsum.

        :returns: the first following argument that has a dependence \
            on this argument.
        :rtype: :py:class:`psyclone.psyGen.Argument`

        '''
        nodes = self._call.following()
        return self._find_argument(nodes)

    def forward_read_dependencies(self):
        '''Returns a list of following read arguments that this argument has
        dependencies with. The arguments may exist in a call, a
        haloexchange, or a globalsum. If none are found then
        return an empty list. If self is not a writer then return an
        empty list.

        :returns: a list of following arguments that have a read \
            dependence on this argument.
        :rtype: list of :py:class:`psyclone.psyGen.Argument`

        '''
        nodes = self._call.following()
        return self._find_read_arguments(nodes)

    def _find_argument(self, nodes):
        '''Return the first argument in the list of nodes that has a
        dependency with self. If one is not found return None

        :param nodes: the list of nodes that this method examines.
        :type nodes: list of :py:class:`psyclone.psyir.nodes.Node`

        :returns: An argument object or None.
        :rtype: :py:class:`psyclone.psyGen.Argument`

        '''
        nodes_with_args = [x for x in nodes if
                           isinstance(x, (Kern, HaloExchange, GlobalSum))]
        for node in nodes_with_args:
            for argument in node.args:
                if self._depends_on(argument):
                    return argument
        return None

    def _find_read_arguments(self, nodes):
        '''Return a list of arguments from the list of nodes that have a read
        dependency with self. If none are found then return an empty
        list. If self is not a writer then return an empty list.

        :param nodes: the list of nodes that this method examines.
        :type nodes: list of :py:class:`psyclone.psyir.nodes.Node`

        :returns: a list of arguments that have a read dependence on \
            this argument.
        :rtype: list of :py:class:`psyclone.psyGen.Argument`

        '''
        if self.access not in AccessType.all_write_accesses():
            # I am not a writer so there will be no read dependencies
            return []

        # We only need consider nodes that have arguments
        nodes_with_args = [x for x in nodes if
                           isinstance(x, (Kern, HaloExchange, GlobalSum))]
        access = DataAccess(self)
        arguments = []
        for node in nodes_with_args:
            for argument in node.args:
                # look at all arguments in our nodes
                if argument.access in AccessType.all_read_accesses() and \
                   access.overlaps(argument):
                    arguments.append(argument)
                if argument.access in AccessType.all_write_accesses():
                    access.update_coverage(argument)
                    if access.covered:
                        # We have now found all arguments upon which
                        # this argument depends so return the list.
                        return arguments

        # we did not find a terminating write dependence in the list
        # of nodes so we return any read dependencies that were found
        return arguments

    def _find_write_arguments(self, nodes, ignore_halos=False):
        '''Return a list of arguments from the list of nodes that have a write
        dependency with self. If none are found then return an empty
        list. If self is not a reader then return an empty list.

        :param nodes: the list of nodes that this method examines.
        :type nodes: list of :py:class:`psyclone.psyir.nodes.Node`

        :param bool ignore_halos: if `True` then any write dependencies \
            involving a halo exchange are ignored. Defaults to `False`.
        :returns: a list of arguments that have a write dependence with \
            this argument.
        :rtype: list of :py:class:`psyclone.psyGen.Argument`

        '''
        if self.access not in AccessType.all_read_accesses():
            # I am not a reader so there will be no write dependencies
            return []

        # We only need consider nodes that have arguments
        nodes_with_args = [x for x in nodes if
                           isinstance(x, (Kern, GlobalSum)) or
                           (isinstance(x, HaloExchange) and not ignore_halos)]
        access = DataAccess(self)
        arguments = []
        for node in nodes_with_args:
            for argument in node.args:
                # look at all arguments in our nodes
                if argument.access not in AccessType.all_write_accesses():
                    # no dependence if not a writer
                    continue
                if not access.overlaps(argument):
                    # Accesses are independent of each other
                    continue
                arguments.append(argument)
                access.update_coverage(argument)
                if access.covered:
                    # sanity check
                    if not isinstance(node, HaloExchange) and \
                       len(arguments) > 1:
                        raise InternalError(
                            "Found a writer dependence but there are already "
                            "dependencies. This should not happen.")
                    # We have now found all arguments upon which this
                    # argument depends so return the list.
                    return arguments
        if arguments:
            raise InternalError(
                "Argument()._field_write_arguments() There are no more nodes "
                "but there are already dependencies. This should not happen.")
        # no dependencies have been found
        return []

    def _depends_on(self, argument):
        '''If there is a dependency between the argument and self then return
        True, otherwise return False. We consider there to be a
        dependency between two arguments if the names are the same and
        if one reads and one writes, or if both write. Dependencies
        are often defined as being read-after-write (RAW),
        write-after-read (WAR) and write after write (WAW). These
        dependencies can be considered to be forward dependencies, in
        the sense that RAW means that the read is after the write in
        the schedule. Similarly for WAR and WAW. We capture these
        dependencies in this method. However we also capture
        dependencies in the opposite direction (backward
        dependencies). These are the same dependencies as forward
        dependencies but are reversed. One could consider these to be
        read-before-write, write-before-read, and
        write-before-write. The terminology of forward and backward to
        indicate whether the argument we depend on is after or before
        us in the schedule is borrowed from loop dependence analysis
        where a forward dependence indicates a dependence in a future
        loop iteration and a backward dependence indicates a
        dependence on a previous loop iteration. Note, we currently
        assume that any read or write to an argument results in a
        dependence i.e. we do not consider the internal structure of
        the argument (e.g. it may be an array). However, this
        assumption is OK as all elements of an array are typically
        accessed. However, we may need to revisit this when we change
        the iteration spaces of loops e.g. for overlapping
        communication and computation.

        :param argument: the argument we will check to see whether \
            there is a dependence on this argument instance (self).
        :type argument: :py:class:`psyclone.psyGen.Argument`

        :returns: True if there is a dependence and False if not.
        :rtype: bool

        '''
        if argument.name == self._name:
            if self.access in AccessType.all_write_accesses() and \
               argument.access in AccessType.all_read_accesses():
                return True
            if self.access in AccessType.all_read_accesses() and \
               argument.access in AccessType.all_write_accesses():
                return True
            if self.access in AccessType.all_write_accesses() and \
               argument.access in AccessType.all_write_accesses():
                return True
        return False


class KernelArgument(Argument):
    def __init__(self, arg, arg_info, call):
        self._arg = arg
        Argument.__init__(self, call, arg_info, arg.access)

    @property
    def space(self):
        return self._arg.function_space

    @property
    def stencil(self):
        return self._arg.stencil

    @property
    @abc.abstractmethod
    def is_scalar(self):
        ''':returns: whether this variable is a scalar variable or not.
        :rtype: bool'''


class TransInfo(object):
    '''
    This class provides information about, and access, to the available
    transformations in this implementation of PSyclone. New transformations
    will be picked up automatically as long as they subclass the abstract
    Transformation class.

    For example:

    >>> from psyclone.psyGen import TransInfo
    >>> t = TransInfo()
    >>> print(t.list)
    There is 1 transformation available:
      1: SwapTrans, A test transformation
    >>> # accessing a transformation by index
    >>> trans = t.get_trans_num(1)
    >>> # accessing a transformation by name
    >>> trans = t.get_trans_name("SwapTrans")

    '''

    def __init__(self, module=None, base_class=None):
        ''' if module and/or baseclass are provided then use these else use
            the default module "Transformations" and the default base_class
            "Transformation"'''

        if False:
            self._0_to_n = DummyTransformation()  # only here for pyreverse!

        # TODO #620: This need to be improved to support the new
        # layout, where transformations are in different directories and files.
        # Leaving local imports so they will be removed once TransInfo is
        # replaced.
        # pylint: disable=import-outside-toplevel
        from psyclone import transformations
        if module is None:
            # default to the transformation module
            module = transformations
        if base_class is None:
            base_class = Transformation
        # find our transformations
        self._classes = self._find_subclasses(module, base_class)

        # create our transformations
        self._objects = []
        self._obj_map = {}
        for my_class in self._classes:
            my_object = my_class()
            self._objects.append(my_object)
            self._obj_map[my_object.name] = my_object
        # TODO #620:
        # Transformations that are in psyir and other subdirectories
        # are not found by TransInfo, so we add some that are used in
        # tests and examples explicitly. I'm leaving this import here
        # so it is obvious it can be removed.
        from psyclone.psyir.transformations import LoopFuseTrans
        my_object = LoopFuseTrans()
        # Only add the loop-fuse statement if base_class and module
        # match for the loop fusion transformation.
        if isinstance(my_object, base_class) and module == transformations:
            self._objects.append(LoopFuseTrans())
            self._obj_map["LoopFuseTrans"] = self._objects[-1]

    @property
    def list(self):
        ''' return a string with a human readable list of the available
            transformations '''
        import os
        if len(self._objects) == 1:
            result = "There is 1 transformation available:"
        else:
            result = "There are {0} transformations available:".format(
                len(self._objects))
        result += os.linesep
        for idx, my_object in enumerate(self._objects):
            result += "  " + str(idx+1) + ": " + my_object.name + ": " + \
                      str(my_object) + os.linesep
        return result

    @property
    def num_trans(self):
        ''' return the number of transformations available '''
        return len(self._objects)

    def get_trans_num(self, number):
        ''' return the transformation with this number (use list() first to
            see available transformations) '''
        if number < 1 or number > len(self._objects):
            raise GenerationError("Invalid transformation number supplied")
        return self._objects[number-1]

    def get_trans_name(self, name):
        ''' return the transformation with this name (use list() first to see
            available transformations) '''
        try:
            return self._obj_map[name]
        except KeyError:
            raise GenerationError("Invalid transformation name: got {0} "
                                  "but expected one of {1}".
                                  format(name, self._obj_map.keys()))

    def _find_subclasses(self, module, base_class):
        ''' return a list of classes defined within the specified module that
            are a subclass of the specified baseclass. '''
        import inspect
        return [cls for name, cls in inspect.getmembers(module)
                if inspect.isclass(cls) and not inspect.isabstract(cls) and
                issubclass(cls, base_class) and cls is not base_class]


@six.add_metaclass(abc.ABCMeta)
class Transformation():
    '''Abstract baseclass for a transformation. Uses the abc module so it
    can not be instantiated.

    :param writer: optional argument to set the type of writer to \
        provide to a transformation for use when constructing error \
        messages. Defaults to FortranWriter().
    :type writer: :py:class:`psyclone.psyir.backend.visitor.PSyIRVisitor`

    '''
    def __init__(self, writer=None):

        if writer:
            if not isinstance(writer, PSyIRVisitor):
                raise TypeError(
                    f"The writer argument to a transformation should be a "
                    f"PSyIRVisitor, but found '{type(writer).__name__}'.")
            self._writer = writer
        else:
            self._writer = FortranWriter()

    @property
    def name(self):
        '''
        :returns: the transformation's class name.
        :rtype: str

        '''
        return type(self).__name__

    @abc.abstractmethod
    def apply(self, node, options=None):
        '''Abstract method that applies the transformation. This function
        must be implemented by each transform. As a minimum each apply
        function must take a node to which the transform is applied, and
        a dictionary of additional options, which will also be passed on
        to the validate functions. This dictionary is used to provide
        optional parameters, and also to modify the behaviour of
        validation of transformations: for example, if the user knows that
        a transformation can correctly be applied in a specific case, but
        the more generic code validation would not allow this. Validation
        functions should check for a key in the options dictionary to
        disable certain tests. Those keys will be documented in each
        apply() and validate() function.

        Note that some apply() functions might take a slightly different
        set of parameters.

        :param node: The node (or list of nodes) for the transformation \
                - specific to the actual transform used.
        :type node: depends on actual transformation
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        '''

    def validate(self, node, options=None):
        '''Method that validates that the input data is correct.
        It will raise exceptions if the input data is incorrect. This
        function needs to be implemented by each transformation.

        The validate function can be called by the user independent of
        the apply() function, but it will automatically be executed as
        part of an apply() call.

        As minimum each validate function must take a node to which the
        transform is applied and a dictionary of additional options.
        This dictionary is used to provide optional parameters and also
        to modify the behaviour of validation: for example, if the user
        knows that a transformation can correctly be applied in a specific
        case but the more generic code validation would not allow this.
        Validation functions should check for particular keys in the options
        dict in order to disable certain tests. Those keys will be documented
        in each apply() and validate() function as 'options["option-name"]'.

        Note that some validate functions might take a slightly different
        set of parameters.

        :param node: The node (or list of nodes) for the transformation \
                - specific to the actual transform used.
        :type node: depends on actual transformation
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None
        '''
        # pylint: disable=no-self-use, unused-argument


class DummyTransformation(Transformation):
    '''Dummy transformation use elsewhere to keep pyreverse happy.'''

    @property
    def name(self):
        return

    def apply(self, node, options=None):
        pass


# For Sphinx AutoAPI documentation generation
__all__ = ['PSyFactory', 'PSy', 'Invokes', 'Invoke', 'InvokeSchedule',
           'GlobalSum', 'HaloExchange', 'Kern', 'CodedKern', 'InlinedKern',
           'BuiltIn', 'Arguments', 'DataAccess', 'Argument', 'KernelArgument',
           'TransInfo', 'Transformation', 'DummyTransformation']
