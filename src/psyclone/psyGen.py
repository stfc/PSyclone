# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2020, Science and Technology Facilities Council.
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
# Modified I. Kavcic, Met Office
# -----------------------------------------------------------------------------

''' This module provides generic support for PSyclone's PSy code optimisation
    and generation. The classes in this method need to be specialised for a
    particular API and implementation. '''

from __future__ import print_function, absolute_import
import abc
import six
from fparser.two import Fortran2003
from psyclone.configuration import Config
from psyclone.f2pygen import DirectiveGen
from psyclone.core.access_info import VariablesAccessInfo, AccessType
from psyclone.psyir.symbols import SymbolTable, DataSymbol, ArrayType, \
    Symbol, INTEGER_TYPE, BOOLEAN_TYPE
from psyclone.psyir.nodes import Node, Schedule, Loop, Statement
from psyclone.errors import GenerationError, InternalError, FieldNotFoundError
from psyclone.parse.algorithm import BuiltInCall


# The types of 'intent' that an argument to a Fortran subroutine
# may have
FORTRAN_INTENT_NAMES = ["inout", "out", "in"]

# OMP_OPERATOR_MAPPING is used to determine the operator to use in the
# reduction clause of an OpenMP directive. All code for OpenMP
# directives exists in psyGen.py so this mapping should not be
# overidden.
OMP_OPERATOR_MAPPING = {AccessType.SUM: "+"}

# Names of types of scalar variable
MAPPING_SCALARS = {"iscalar": "iscalar", "rscalar": "rscalar"}


# Valid types of argument to a kernel call
VALID_ARG_TYPE_NAMES = []

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
        from psyclone.f2pygen import CommentGen
        parent.add(CommentGen(parent, ""))
        parent.add(CommentGen(parent, " Zero summation variables"))
        parent.add(CommentGen(parent, ""))
        for call in red_call_list:
            call.zero_reduction_variable(parent)
        parent.add(CommentGen(parent, ""))


def args_filter(arg_list, arg_types=None, arg_accesses=None, arg_meshes=None,
                is_literal=True):
    '''
    Return all arguments in the supplied list that are of type
    arg_types and with access in arg_accesses. If these are not set
    then return all arguments.

    :param arg_list: List of kernel arguments to filter
    :type arg_list: list of :py:class:`psyclone.parse.algorithm.Descriptor`
    :param arg_types: List of argument types (e.g. "GH_FIELD")
    :type arg_types: list of str
    :param arg_accesses: List of access types that arguments must have
    :type arg_accesses: List of \
        :py:class:`psyclone.core.access_type.AccessType`.
    :param arg_meshes: List of meshes that arguments must be on
    :type arg_meshes: list of str
    :param bool is_literal: Whether or not to include literal arguments in \
                            the returned list.
    :returns: list of kernel arguments matching the requirements
    :rtype: list of :py:class:`psyclone.parse.algorithm.Descriptor`
    '''
    arguments = []
    for argument in arg_list:
        if arg_types:
            if argument.type.lower() not in arg_types:
                continue
        if arg_accesses:
            if argument.access not in arg_accesses:
                continue
        if arg_meshes:
            if argument.mesh not in arg_meshes:
                continue
        if not is_literal:
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
    '''
    def __init__(self, api="", distributed_memory=None):
        '''Initialises a factory which can create API specific PSY objects.
        :param str api: Name of the API to use.
        :param bool distributed_memory: True if distributed memory should be \
                                        supported.
        '''
        if distributed_memory is None:
            _distributed_memory = Config.get().distributed_memory
        else:
            _distributed_memory = distributed_memory

        if _distributed_memory not in [True, False]:
            raise GenerationError(
                "The distributed_memory flag in PSyFactory must be set to"
                " 'True' or 'False'")
        Config.get().distributed_memory = _distributed_memory
        self._type = get_api(api)

    def create(self, invoke_info):
        '''
        Create the API-specific PSy instance.

        :param invoke_info: information on the invoke()s found by parsing
                            the Algorithm layer.
        :type invoke_info: :py:class:`psyclone.parse.algorithm.FileInfo`

        :returns: an instance of the API-specifc sub-class of PSy.
        :rtype: subclass of :py:class:`psyclone.psyGen.PSy`
        '''
        if self._type == "dynamo0.1":
            from psyclone.dynamo0p1 import DynamoPSy as PSyClass
        elif self._type == "dynamo0.3":
            from psyclone.dynamo0p3 import DynamoPSy as PSyClass
        elif self._type == "gocean0.1":
            from psyclone.gocean0p1 import GOPSy as PSyClass
        elif self._type == "gocean1.0":
            from psyclone.gocean1p0 import GOPSy as PSyClass
        elif self._type == "nemo":
            from psyclone.nemo import NemoPSy as PSyClass
            # For this API, the 'invoke_info' is actually the fparser2 AST
            # of the Fortran file being processed
        else:
            raise GenerationError("PSyFactory: Internal Error: Unsupported "
                                  "api type '{0}' found. Should not be "
                                  "possible.".format(self._type))
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

    def __str__(self):
        return "PSy"

    @property
    def invokes(self):
        return self._invokes

    @property
    def name(self):
        return "psy_"+self._name

    @property
    @abc.abstractmethod
    def gen(self):
        '''Abstract base class for code generation function.
        :param parent: the parent of this Node in the PSyIR.
        :type parent: :py:class:`psyclone.psyir.nodes.Node`.
        '''

    def inline(self, module):
        ''' inline all kernel subroutines into the module that are marked for
            inlining. Avoid inlining the same kernel more than once. '''
        inlined_kernel_names = []
        for invoke in self.invokes.invoke_list:
            schedule = invoke.schedule
            for kernel in schedule.walk(CodedKern):
                if kernel.module_inline:
                    if kernel.name.lower() not in inlined_kernel_names:
                        inlined_kernel_names.append(kernel.name.lower())
                        module.add_raw_subroutine(kernel._kernel_code)


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
        '''
        opencl_kernels = []
        opencl_num_queues = 1
        generate_ocl_init = False
        for invoke in self.invoke_list:
            # If we are generating OpenCL for an Invoke then we need to
            # create routine(s) to set the arguments of the Kernel(s) it
            # calls. We do it here as this enables us to prevent
            # duplication.
            if invoke.schedule.opencl:
                generate_ocl_init = True
                for kern in invoke.schedule.coded_kernels():
                    if kern.name not in opencl_kernels:
                        # Compute the maximum number of command queues that
                        # will be needed.
                        opencl_num_queues = max(
                            opencl_num_queues,
                            kern.opencl_options['queue_number'])
                        opencl_kernels.append(kern.name)
                        kern.gen_arg_setter_code(parent)
            invoke.gen_code(parent)
        if generate_ocl_init:
            self.gen_ocl_init(parent, opencl_kernels, opencl_num_queues)

    @staticmethod
    def gen_ocl_init(parent, kernels, num_queues):
        '''
        Generates a subroutine to initialise the OpenCL environment and
        construct the list of OpenCL kernel objects used by this PSy layer.

        :param parent: the node in the f2pygen AST representing the module \
                       that will contain the generated subroutine.
        :type parent: :py:class:`psyclone.f2pygen.ModuleGen`
        :param kernels: List of kernel names called by the PSy layer.
        :type kernels: list of str
        :param int num_queues: total number of queues needed for the OpenCL \
                               implementation.
        '''
        from psyclone.f2pygen import SubroutineGen, DeclGen, AssignGen, \
            CallGen, UseGen, CommentGen, CharDeclGen, IfThenGen

        sub = SubroutineGen(parent, "psy_init")
        parent.add(sub)
        sub.add(UseGen(sub, name="fortcl", only=True,
                       funcnames=["ocl_env_init", "add_kernels"]))
        # Add a logical variable used to ensure that this routine is only
        # executed once.
        sub.add(DeclGen(sub, datatype="logical", save=True,
                        entity_decls=["initialised"],
                        initial_values=[".False."]))
        # Check whether or not this is our first time in the routine
        sub.add(CommentGen(sub, " Check to make sure we only execute this "
                           "routine once"))
        ifthen = IfThenGen(sub, ".not. initialised")
        sub.add(ifthen)
        ifthen.add(AssignGen(ifthen, lhs="initialised", rhs=".True."))

        # Initialise the OpenCL environment
        ifthen.add(CommentGen(ifthen,
                              " Initialise the OpenCL environment/device"))
        ifthen.add(CallGen(ifthen, "ocl_env_init", [num_queues]))

        # Create a list of our kernels
        ifthen.add(CommentGen(ifthen,
                              " The kernels this PSy layer module requires"))
        nkernstr = str(len(kernels))

        # Declare array of character strings
        ifthen.add(CharDeclGen(
            ifthen, length="30",
            entity_decls=["kernel_names({0})".format(nkernstr)]))
        for idx, kern in enumerate(kernels):
            ifthen.add(AssignGen(ifthen, lhs="kernel_names({0})".format(idx+1),
                                 rhs='"{0}"'.format(kern)))
        ifthen.add(CommentGen(ifthen,
                              " Create the OpenCL kernel objects. Expects "
                              "to find all of the compiled"))
        ifthen.add(CommentGen(ifthen, " kernels in PSYCLONE_KERNELS_FILE."))
        ifthen.add(CallGen(ifthen, "add_kernels", [nkernstr, "kernel_names"]))


class Invoke(object):
    '''Manage an individual invoke call.

    :param alg_invocation: metadata from the parsed code capturing \
        information for this Invoke instance.
    :type alg_invocation: :py:class`psyclone.parse.algorithm.InvokeCall`
    :param int idx: position/index of this invoke call in the subroutine.
        If not None, this number is added to the name ("invoke_").
    :param schedule_class: the schedule class to create for this invoke.
    :type schedule_class: :py:class:`psyclone.psyGen.InvokeSchedule`
    :param invokes: the Invokes instance that contains this Invoke \
        instance.
    :type invokes: :py:class:`psyclone.psyGen.invokes`
    :param reserved_names: optional argument: list of reserved names,
        i.e. names that should not be used e.g. as a PSyclone-created
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
            self._name = alg_invocation.name
        elif len(alg_invocation.kcalls) == 1 and \
                alg_invocation.kcalls[0].type == "kernelCall":
            # use the name of the kernel call with the position appended.
            # Appended position is needed in case we have two separate invokes
            # in the same algorithm code containing the same (single) kernel
            self._name = "invoke_" + str(idx) + "_" + \
                alg_invocation.kcalls[0].ktype.name
        else:
            # use the position of the invoke
            self._name = "invoke_"+str(idx)

        if not reserved_names:
            reserved_names = []
        reserved_names.append(self._name)

        # create the schedule
        self._schedule = schedule_class(alg_invocation.kcalls, reserved_names)

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

    def unique_declarations(self, datatype, access=None):
        ''' Returns a list of all required declarations for the
        specified datatype. If access is supplied (e.g. "write") then
        only declarations with that access are returned.
        :param string datatype: The type of the kernel argument for the \
                                particular API for which the intent is \
                                required
        :param access: Optional AccessType that the declaration should have.
        :returns: List of all declared names.
        :rtype: A list of strings.
        :raises: GenerationError if an invalid datatype is given.
        :raises: InternalError if an invalid access is specified.
        '''
        if datatype not in VALID_ARG_TYPE_NAMES:
            raise GenerationError(
                "unique_declarations called with an invalid datatype. "
                "Expected one of '{0}' but found '{1}'".
                format(str(VALID_ARG_TYPE_NAMES), datatype))

        if access and not isinstance(access, AccessType):
            raise InternalError(
                "unique_declarations called with an invalid access type. "
                "Type is {0} instead of AccessType".
                format(type(access)))

        declarations = []
        for call in self.schedule.kernels():
            for arg in call.arguments.args:
                if not access or arg.access == access:
                    if arg.text is not None:
                        if arg.type == datatype:
                            test_name = arg.declaration_name
                            if test_name not in declarations:
                                declarations.append(test_name)
        return declarations

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

    def unique_declns_by_intent(self, datatype):
        '''
        Returns a dictionary listing all required declarations for each
        type of intent ('inout', 'out' and 'in').

        :param string datatype: the type of the kernel argument for the \
                                particular API for which the intent is \
                                required
        :returns: dictionary containing 'intent' keys holding the kernel \
                  argument intent and declarations of all kernel arguments \
                  for each type of intent
        :rtype: dict
        :raises GenerationError: if the kernel argument is not a valid \
                                 datatype for the particular API.

        '''
        if datatype not in VALID_ARG_TYPE_NAMES:
            raise GenerationError(
                "unique_declns_by_intent called with an invalid datatype. "
                "Expected one of '{0}' but found '{1}'".
                format(str(VALID_ARG_TYPE_NAMES), datatype))

        # Get the lists of all kernel arguments that are accessed as
        # inc (shared update), write, read and readwrite (independent
        # update). A single argument may be accessed in different ways
        # by different kernels.
        inc_args = self.unique_declarations(datatype, access=AccessType.INC)
        write_args = self.unique_declarations(datatype,
                                              access=AccessType.WRITE)
        read_args = self.unique_declarations(datatype, access=AccessType.READ)
        readwrite_args = self.unique_declarations(datatype,
                                                  AccessType.READWRITE)
        sum_args = self.unique_declarations(datatype, access=AccessType.SUM)
        # sum_args behave as if they are write_args from
        # the PSy-layer's perspective.
        write_args += sum_args
        # readwrite_args behave in the same way as inc_args
        # from the perspective of first access and intents
        inc_args += readwrite_args
        # Rationalise our lists so that any fields that are updated
        # (have inc or readwrite access) do not appear in the list
        # of those that are only written to
        for arg in write_args[:]:
            if arg in inc_args:
                write_args.remove(arg)
        # Fields that are only ever read by any kernel that
        # accesses them
        for arg in read_args[:]:
            if arg in write_args or arg in inc_args:
                read_args.remove(arg)

        # We will return a dictionary containing as many lists
        # as there are types of intent
        declns = {}
        for intent in FORTRAN_INTENT_NAMES:
            declns[intent] = []

        for name in inc_args:
            # For every arg that is updated ('inc'd' or readwritten)
            # by at least one kernel, identify the type of the first
            # access. If it is 'write' then the arg is only
            # intent(out), otherwise it is intent(inout)
            first_arg = self.first_access(name)
            if first_arg.access != AccessType.WRITE:
                if name not in declns["inout"]:
                    declns["inout"].append(name)
            else:
                if name not in declns["out"]:
                    declns["out"].append(name)

        for name in write_args:
            # For every argument that is written to by at least one kernel,
            # identify the type of the first access - if it is read
            # or inc'd before it is written then it must have intent(inout).
            # However, we deal with inc and readwrite args separately so we
            # do not consider those here.
            first_arg = self.first_access(name)
            if first_arg.access == AccessType.READ:
                if name not in declns["inout"]:
                    declns["inout"].append(name)
            else:
                if name not in declns["out"]:
                    declns["out"].append(name)

        for name in read_args:
            # Anything we have left must be declared as intent(in)
            if name not in declns["in"]:
                declns["in"].append(name)

        return declns

    def gen(self):
        from psyclone.f2pygen import ModuleGen
        module = ModuleGen("container")
        self.gen_code(module)
        return module.root

    def gen_code(self, parent):
        from psyclone.f2pygen import SubroutineGen, TypeDeclGen, DeclGen, \
            SelectionGen, AssignGen
        # create the subroutine
        invoke_sub = SubroutineGen(parent, name=self.name,
                                   args=self.psy_unique_vars)
        # add the subroutine argument declarations
        my_typedecl = TypeDeclGen(invoke_sub, datatype="field_type",
                                  entity_decls=self.psy_unique_vars,
                                  intent="inout")
        invoke_sub.add(my_typedecl)
        # declare field-type, column topology and function-space types
        column_topology_name = "topology"
        my_typedecl = TypeDeclGen(invoke_sub, datatype="ColumnTopology",
                                  entity_decls=[column_topology_name],
                                  pointer=True)
        invoke_sub.add(my_typedecl)
        # declare any basic types required
        my_decl = DeclGen(invoke_sub, datatype="integer",
                          entity_decls=["nlayers"])
        invoke_sub.add(my_decl)

        for (idx, dof) in enumerate(self._dofs):
            call = self._dofs[dof][0]
            arg = self._dofs[dof][1]
            # declare a type select clause which is used to map from a base
            # class to FunctionSpace_type
            type_select = SelectionGen(invoke_sub,
                                       expr=arg.name + "_space=>" + arg.name +
                                       "%function_space", typeselect=True)
            invoke_sub.add(type_select)

            my_typedecl = TypeDeclGen(invoke_sub,
                                      datatype="FunctionSpace_type",
                                      entity_decls=[arg.name+"_space"],
                                      pointer=True)
            invoke_sub.add(my_typedecl)

            content = []
            if idx == 0:
                # use the first model to provide nlayers
                # *** assumption that all fields operate over the same number
                # of layers
                assign_1 = AssignGen(type_select, lhs="topology",
                                     rhs=arg.name+"_space%topology",
                                     pointer=True)
                assign_2 = AssignGen(type_select, lhs="nlayers",
                                     rhs="topology%layer_count()")
                content.append(assign_1)
                content.append(assign_2)
            iterates_over = call.iterates_over
            stencil = arg.stencil
            assign_3 = AssignGen(type_select, lhs=dof+"dofmap",
                                 rhs=arg.name +
                                 "_space%dof_map(" + iterates_over + ", " +
                                 stencil + ")",
                                 pointer=True)
            content.append(assign_3)
            type_select.addcase(["FunctionSpace_type"], content=content)
            # declare our dofmap
            my_decl = DeclGen(invoke_sub, datatype="integer",
                              entity_decls=[dof+"dofmap(:,:)"], pointer=True)
            invoke_sub.add(my_decl)

        # create the subroutine kernel call content
        self.schedule.gen_code(invoke_sub)
        parent.add(invoke_sub)


class InvokeSchedule(Schedule):
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
    >>> schedule.view()

    :param type KernFactory: class instance of the factory to use when \
     creating Kernels. e.g. :py:class:`psyclone.dynamo0p3.DynKernCallFactory`.
    :param type BuiltInFactory: class instance of the factory to use when \
     creating built-ins. e.g. \
     :py:class:`psyclone.dynamo0p3_builtins.DynBuiltInCallFactory`.
    :param alg_calls: list of Kernel calls in the schedule.
    :type alg_calls: list of :py:class:`psyclone.parse.algorithm.KernelCall`

    '''
    # Textual description of the node.
    _text_name = "InvokeSchedule"

    def __init__(self, KernFactory, BuiltInFactory, alg_calls=None,
                 reserved_names=None):
        super(InvokeSchedule, self).__init__()

        self._invoke = None
        self._opencl = False  # Whether or not to generate OpenCL

        # InvokeSchedule opencl_options default values
        self._opencl_options = {"end_barrier": True}

        # Populate the Schedule Symbol Table with the reserved names.
        if reserved_names:
            for name in reserved_names:
                self.symbol_table.add(Symbol(name))

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

    def set_opencl_options(self, options):
        '''
        Validate and store a set of options associated with the InvokeSchedule
        to tune the OpenCL code generation.

        :param options: a set of options to tune the OpenCL code.
        :type options: dictionary of <string>:<value>

        '''
        valid_opencl_options = ['end_barrier']

        # Validate that the options given are supported and store them
        for key, value in options.items():
            if key in valid_opencl_options:
                if key == "end_barrier":
                    if not isinstance(value, bool):
                        raise TypeError(
                            "InvokeSchedule opencl_option 'end_barrier' "
                            "should be a boolean.")
            else:
                raise AttributeError(
                    "InvokeSchedule does not support the opencl_option '{0}'. "
                    "The supported options are: {1}."
                    "".format(key, valid_opencl_options))

            self._opencl_options[key] = value

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
            self.coloured_name(colour), self.invoke.name)

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
        from psyclone.f2pygen import UseGen, DeclGen, AssignGen, CommentGen, \
            IfThenGen, CallGen

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
        self._symbol_table = self.symbol_table.shallow_copy()

        # Global symbols promoted from Kernel Globals are in the SymbolTable
        # First aggregate all globals variables from the same module in a map
        module_map = {}
        for globalvar in self.symbol_table.global_datasymbols:
            module_name = globalvar.interface.container_symbol.name
            if module_name in module_map:
                module_map[module_name].append(globalvar.name)
            else:
                module_map[module_name] = [globalvar.name]

        # Then we can produce the UseGen statements without repeating modules
        for module_name, var_list in module_map.items():
            parent.add(UseGen(parent, name=module_name, only=True,
                              funcnames=var_list))

        if self._opencl:
            parent.add(UseGen(parent, name="iso_c_binding"))
            parent.add(UseGen(parent, name="clfortran"))
            parent.add(UseGen(parent, name="fortcl", only=True,
                              funcnames=["get_num_cmd_queues",
                                         "get_cmd_queues",
                                         "get_kernel_by_name"]))

            # Declare variables needed on a OpenCL PSy-layer invoke
            nqueues = self.symbol_table.new_symbol_name("num_cmd_queues")
            self.symbol_table.add(DataSymbol(nqueues, INTEGER_TYPE),
                                  tag="opencl_num_cmd_queues")
            qlist = self.symbol_table.new_symbol_name("cmd_queues")
            self.symbol_table.add(
                DataSymbol(qlist,
                           ArrayType(INTEGER_TYPE,
                                     [ArrayType.Extent.ATTRIBUTE])),
                tag="opencl_cmd_queues")
            first = self.symbol_table.new_symbol_name("first_time")
            self.symbol_table.add(
                DataSymbol(first, BOOLEAN_TYPE), tag="first_time")
            flag = self.symbol_table.new_symbol_name("ierr")
            self.symbol_table.add(
                DataSymbol(flag, INTEGER_TYPE), tag="opencl_error")
            nbytes = self.root.symbol_table.new_symbol_name(
                "size_in_bytes")
            self.symbol_table.add(
                DataSymbol(nbytes, INTEGER_TYPE), tag="opencl_bytes")
            wevent = self.root.symbol_table.new_symbol_name("write_event")
            self.symbol_table.add(
                DataSymbol(wevent, INTEGER_TYPE), tag="opencl_wevent")

            parent.add(DeclGen(parent, datatype="integer", save=True,
                               entity_decls=[nqueues]))
            parent.add(DeclGen(parent, datatype="integer", save=True,
                               pointer=True, kind="c_intptr_t",
                               entity_decls=[qlist + "(:)"]))
            parent.add(DeclGen(parent, datatype="integer",
                               entity_decls=[flag]))
            parent.add(DeclGen(parent, datatype="logical", save=True,
                               entity_decls=[first],
                               initial_values=[".true."]))
            if_first = IfThenGen(parent, first)
            parent.add(if_first)
            if_first.add(AssignGen(if_first, lhs=first, rhs=".false."))
            if_first.add(CommentGen(if_first,
                                    " Ensure OpenCL run-time is initialised "
                                    "for this PSy-layer module"))
            if_first.add(CallGen(if_first, "psy_init"))
            if_first.add(AssignGen(if_first, lhs=nqueues,
                                   rhs="get_num_cmd_queues()"))
            if_first.add(AssignGen(if_first, lhs=qlist, pointer=True,
                                   rhs="get_cmd_queues()"))
            # Kernel pointers
            kernels = self.walk(Kern)
            for kern in kernels:
                base = "kernel_" + kern.name
                kernel = self.root.symbol_table.new_symbol_name(base)
                self.symbol_table.add(Symbol(kernel), tag=kernel)
                parent.add(
                    DeclGen(parent, datatype="integer", kind="c_intptr_t",
                            save=True, target=True, entity_decls=[kernel]))
                if_first.add(
                    AssignGen(
                        if_first, lhs=kernel,
                        rhs='get_kernel_by_name("{0}")'.format(kern.name)))

        for entity in self._children:
            entity.gen_code(parent)

        if self.opencl and self._opencl_options['end_barrier']:

            parent.add(CommentGen(parent,
                                  " Block until all kernels have finished"))

            # We need a clFinish for all the queues in the implementation
            opencl_num_queues = 1
            for kern in self.coded_kernels():
                opencl_num_queues = max(
                    opencl_num_queues,
                    kern.opencl_options['queue_number'])
            for queue_number in range(1, opencl_num_queues + 1):
                parent.add(
                    AssignGen(parent, lhs=flag,
                              rhs="clFinish({0}({1}))".format(qlist,
                                                              queue_number)))

        # Restore symbol table
        self._symbol_table = symbol_table_before_gen

    @property
    def opencl(self):
        '''
        :returns: Whether or not we are generating OpenCL for this \
            InvokeSchedule.
        :rtype: bool
        '''
        return self._opencl

    @opencl.setter
    def opencl(self, value):
        '''
        Setter for whether or not to generate the OpenCL version of this
        schedule.

        :param bool value: whether or not to generate OpenCL.
        '''
        if not isinstance(value, bool):
            raise ValueError(
                "InvokeSchedule.opencl must be a bool but got {0}".
                format(type(value)))
        self._opencl = value


class Directive(Statement):
    '''
    Base class for all Directive statements.

    All classes that generate Directive statements (e.g. OpenMP,
    OpenACC, compiler-specific) inherit from this class.

    :param ast: the entry in the fparser2 parse tree representing the code \
                contained within this directive or None.
    :type ast: :py:class:`fparser.two.Fortran2003.Base` or NoneType
    :param children: list of PSyIR nodes that will be children of this \
                     Directive node or None.
    :type children: list of :py:class:`psyclone.psyir.nodes.Node` or NoneType
    :param parent: PSyIR node that is the parent of this Directive or None.
    :type parent: :py:class:`psyclone.psyir.nodes.Node` or NoneType

    '''
    # The prefix to use when constructing this directive in Fortran
    # (e.g. "OMP"). Must be set by sub-class.
    _PREFIX = ""
    # Textual description of the node.
    _children_valid_format = "Schedule"
    _text_name = "Directive"
    _colour_key = "Directive"

    def __init__(self, ast=None, children=None, parent=None):
        # A Directive always contains a Schedule
        sched = self._insert_schedule(children, ast)
        super(Directive, self).__init__(ast, children=[sched], parent=parent)

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

    @property
    def dir_body(self):
        '''
        :returns: the Schedule associated with this directive.
        :rtype: :py:class:`psyclone.psyir.nodes.Schedule`

        :raises InternalError: if this node does not have a single Schedule as\
                               its child.
        '''
        if len(self.children) != 1 or not \
           isinstance(self.children[0], Schedule):
            raise InternalError(
                "Directive malformed or incomplete. It should have a single "
                "Schedule as a child but found: {0}".format(
                    [type(child).__name__ for child in self.children]))
        return self.children[0]

    @property
    def dag_name(self):
        ''' return the base dag name for this node '''
        return "directive_" + str(self.abs_position)

    def _add_region(self, start_text, end_text=None, data_movement=None):
        '''
        Modifies the underlying fparser2 parse tree to include a subset
        of nodes within a region. (e.g. a 'kernels' or 'data' region.)

        :param str start_text: the directive body to insert at the \
                               beginning of the region. "!$"+self._PREFIX+" " \
                               is prepended to the supplied text.
        :param str end_text: the directive body to insert at the end of \
                             the region (or None). "!$"+self._PREFIX+" " is \
                             prepended to the supplied text.
        :param str data_movement: whether to include data-movement clauses and\
                               if so, whether to determine them by analysing \
                               the code within the region ("analyse") or to \
                               specify 'default(present)' ("present").

        :raises InternalError: if either start_text or end_text already
                               begin with '!'.
        :raises InternalError: if data_movement is not None and not one of \
                               "present" or "analyse".
        :raises InternalError: if data_movement=="analyse" and this is an \
                               OpenMP directive.
        '''
        from fparser.common.readfortran import FortranStringReader
        from fparser.two.Fortran2003 import Comment
        from psyclone.psyir.frontend.fparser2 import Fparser2Reader
        valid_data_movement = ["present", "analyse"]

        # Ensure the fparser2 AST is up-to-date for all of our children
        Node.update(self)

        # Check that we haven't already been called
        if self.ast:
            return

        # Sanity check the supplied begin/end text
        if start_text.lstrip()[0] == "!":
            raise InternalError(
                "_add_region: start_text must be a plain label without "
                "directive or comment characters but got: '{0}'".
                format(start_text))
        if end_text and end_text.lstrip()[0] == "!":
            raise InternalError(
                "_add_region: end_text must be a plain label without directive"
                " or comment characters but got: '{0}'".format(end_text))
        # We only deal with data movement if this is an OpenACC directive
        if data_movement and data_movement == "analyse" and \
           not isinstance(self, ACCDirective):
            raise InternalError(
                "_add_region: the data_movement='analyse' option is only valid"
                " for an OpenACC directive.")

        # Find a reference to the fparser2 parse tree that belongs to
        # the contents of this region. Then go back up one level in the
        # parse tree to find the node to which we will add directives as
        # children. (We do this because our parent PSyIR node may be a
        # directive which has no associated entry in the fparser2 parse tree.)
        first_child = self.children[0][0]
        last_child = self.children[0][-1]
        content_ast = first_child.ast
        fp_parent = content_ast.parent

        try:
            # Find the location of the AST of our first child node in the
            # list of child nodes of our parent in the fparser parse tree.
            ast_start_index = object_index(fp_parent.content,
                                           content_ast)
            if end_text:
                if last_child.ast_end:
                    ast_end_index = object_index(fp_parent.content,
                                                 last_child.ast_end)
                else:
                    ast_end_index = object_index(fp_parent.content,
                                                 last_child.ast)

                text = "!$" + self._PREFIX + " " + end_text
                directive = Comment(FortranStringReader(text,
                                                        ignore_comments=False))
                directive.parent = fp_parent
                fp_parent.content.insert(ast_end_index+1, directive)
                # Ensure this end directive is included with the set of
                # statements belonging to this PSyIR node.
                self.ast_end = directive
                self.dir_body.ast_end = directive
        except (IndexError, ValueError):
            raise InternalError("Failed to find locations to insert "
                                "begin/end directives.")

        text = "!$" + self._PREFIX + " " + start_text

        if data_movement:
            if data_movement == "analyse":
                # Identify the inputs and outputs to the region (variables that
                # are read and written).
                processor = Fparser2Reader()
                readers, writers, readwrites = processor.get_inputs_outputs(
                    fp_parent.content[ast_start_index:ast_end_index+1])

                if readers:
                    text += " COPYIN({0})".format(",".join(readers))
                if writers:
                    text += " COPYOUT({0})".format(",".join(writers))
                if readwrites:
                    text += " COPY({0})".format(",".join(readwrites))

            elif data_movement == "present":
                text += " DEFAULT(PRESENT)"
            else:
                raise InternalError(
                    "_add_region: the optional data_movement argument must be "
                    "one of {0} but got '{1}'".format(valid_data_movement,
                                                      data_movement))
        directive = Comment(FortranStringReader(text,
                                                ignore_comments=False))
        directive.parent = fp_parent
        fp_parent.content.insert(ast_start_index, directive)

        self.ast = directive
        self.dir_body.ast = directive
        # If this is a directive applied to a Loop then update the ast_end
        # for this Node to point to the parse tree for the loop. We have to
        # do this because the loop is a sibling (rather than a child) of the
        # directive in the parse tree.
        if not end_text and isinstance(first_child, Loop):
            self.ast_end = fp_parent.content[ast_start_index+1]


class ACCDirective(Directive):
    ''' Base class for all OpenACC directive statements. '''
    _PREFIX = "ACC"

    @property
    def dag_name(self):
        ''' Return the name to use in a dag for this node.

        :returns: Name of corresponding node in DAG
        :rtype: str
        '''
        return "ACC_directive_" + str(self.abs_position)


@six.add_metaclass(abc.ABCMeta)
class ACCEnterDataDirective(ACCDirective):
    '''
    Abstract class representing a "!$ACC enter data" OpenACC directive in
    an InvokeSchedule. Must be sub-classed for a particular API because the way
    in which fields are marked as being on the remote device is API-
    -dependent.

    :param children: list of nodes which this directive should \
                     have as children.
    :type children: list of :py:class:`psyclone.psyir.nodes.Node`.
    :param parent: the node in the InvokeSchedule to which to add this \
                   directive as a child.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`.
    '''
    def __init__(self, children=None, parent=None):
        super(ACCEnterDataDirective, self).__init__(children=children,
                                                    parent=parent)
        self._acc_dirs = None  # List of parallel directives

    def node_str(self, colour=True):
        '''
        Returns the name of this node with appropriate control codes
        to generate coloured output in a terminal that supports it.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        :rtype: str
        '''
        return self.coloured_name(colour) + "[ACC enter data]"

    @property
    def dag_name(self):
        '''
        :returns: the name to use for this Node in a DAG
        :rtype: str
        '''
        return "ACC_data_" + str(self.abs_position)

    def gen_code(self, parent):
        '''Generate the elements of the f2pygen AST for this Node in the
        Schedule.

        :param parent: node in the f2pygen AST to which to add node(s).
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        :raises GenerationError: if no data is found to copy in.

        '''
        from psyclone.f2pygen import CommentGen

        # We must generate a list of all of the fields accessed by
        # OpenACC kernels (calls within an OpenACC parallel directive)
        # 1. Find all parallel directives. We store this list for later
        #    use in any sub-class.
        self._acc_dirs = self.root.walk(ACCParallelDirective)
        # 2. For each directive, loop over each of the fields used by
        #    the kernels it contains (this list is given by var_list)
        #    and add it to our list if we don't already have it
        var_list = []
        # TODO grid properties are effectively duplicated in this list (but
        # the OpenACC deep-copy support should spot this).
        for pdir in self._acc_dirs:
            for var in pdir.ref_list:
                if var not in var_list:
                    var_list.append(var)
        # 3. Convert this list of objects into a comma-delimited string
        var_str = ",".join(var_list)
        # 4. Add the enter data directive.
        if var_str:
            copy_in_str = "copyin("+var_str+")"
        else:
            # There should be at least one variable to copyin.
            raise GenerationError(
                "ACCEnterData directive did not find any data to copyin. "
                "Perhaps there are no ACCParallel directives within the "
                "region.")
        parent.add(DirectiveGen(parent, "acc", "begin", "enter data",
                                copy_in_str))
        # 5. Call an API-specific subclass of this class in case
        # additional declarations are required.
        self.data_on_device(parent)
        parent.add(CommentGen(parent, ""))

    @abc.abstractmethod
    def data_on_device(self, parent):
        '''
        Adds nodes into an InvokeSchedule to flag that the data required by the
        kernels in the data region is now on the device.

        :param parent: the node in the InvokeSchedule to which to add nodes
        :type parent: :py:class:`psyclone.psyir.nodes.Node`
        '''


class ACCParallelDirective(ACCDirective):
    '''
    Class representing the !$ACC PARALLEL directive of OpenACC
    in the PSyIR.

    '''
    def node_str(self, colour=True):
        '''
        Returns the name of this node with appropriate control codes
        to generate coloured output in a terminal that supports it.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        :rtype: str
        '''
        return self.coloured_name(colour) + "[ACC Parallel]"

    @property
    def dag_name(self):
        '''
        :returns: the name to use for this Node in a DAG
        :rtype: str
        '''
        return "ACC_parallel_" + str(self.abs_position)

    def gen_code(self, parent):
        '''
        Generate the elements of the f2pygen AST for this Node in the Schedule.

        :param parent: node in the f2pygen AST to which to add node(s).
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`
        '''

        # Since we use "default(present)" the Schedule must contain an
        # 'enter data' directive. We don't mandate the order in which
        # transformations are applied so we have to check for that here.
        # We can't use Node.ancestor() because the data directive does
        # not have children. Instead, we go back up to the Schedule and
        # walk down from there.
        nodes = self.root.walk(ACCEnterDataDirective)
        if len(nodes) != 1:
            raise GenerationError(
                "A Schedule containing an ACC parallel region must also "
                "contain an ACC enter data directive but none was found for "
                "{0}".format(self.root.invoke.name))
        # Check that the enter-data directive comes before this parallel
        # directive
        if nodes[0].abs_position > self.abs_position:
            raise GenerationError(
                "An ACC parallel region must be preceeded by an ACC enter-"
                "data directive but in {0} this is not the case.".
                format(self.root.invoke.name))

        # "default(present)" means that the compiler is to assume that
        # all data required by the parallel region is already present
        # on the device. If we've made a mistake and it isn't present
        # then we'll get a run-time error.
        parent.add(DirectiveGen(parent, "acc", "begin", "parallel",
                                "default(present)"))

        for child in self.children:
            child.gen_code(parent)

        parent.add(DirectiveGen(parent, "acc", "end", "parallel", ""))

    @property
    def ref_list(self):
        '''
        Returns a list of the references (whether to arrays or objects)
        required by the Kernel call(s) that are children of this
        directive. This is the list of quantities that must be
        available on the remote device (probably a GPU) before
        the parallel region can be begun.

        :returns: list of variable names
        :rtype: list of str
        '''
        variables = []

        # Look-up the kernels that are children of this node
        for call in self.kernels():
            for arg in call.arguments.acc_args:
                if arg not in variables:
                    variables.append(arg)
        return variables

    @property
    def fields(self):
        '''
        Returns a list of the names of field objects required by the Kernel
        call(s) that are children of this directive.

        :returns: list of names of field arguments.
        :rtype: list of str
        '''
        # Look-up the kernels that are children of this node
        fld_list = []
        for call in self.kernels():
            for arg in call.arguments.fields:
                if arg not in fld_list:
                    fld_list.append(arg)
        return fld_list

    @property
    def scalars(self):
        '''
        Returns a list of the scalar quantities required by the Kernels in
        this region.

        :returns: list of names of scalar arguments.
        :rtype: list of str
        '''
        scalars = []
        for call in self.kernels():
            for arg in call.arguments.scalars:
                if arg not in scalars:
                    scalars.append(arg)
        return scalars

    def update(self):
        '''
        Update the underlying fparser2 parse tree with nodes for the start
        and end of this parallel region.
        '''
        self._add_region(start_text="PARALLEL", end_text="END PARALLEL")


class ACCLoopDirective(ACCDirective):
    '''
    Class managing the creation of a '!$acc loop' OpenACC directive.

    :param children: list of nodes that will be children of this directive.
    :type children: list of :py:class:`psyclone.psyir.nodes.Node`.
    :param parent: the node in the Schedule to which to add this directive.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`.
    :param int collapse: Number of nested loops to collapse into a single \
                         iteration space or None.
    :param bool independent: Whether or not to add the `independent` clause \
                             to the loop directive.
    '''
    def __init__(self, children=None, parent=None, collapse=None,
                 independent=True, sequential=False):
        self._collapse = collapse
        self._independent = independent
        self._sequential = sequential
        super(ACCLoopDirective, self).__init__(children=children,
                                               parent=parent)

    @property
    def dag_name(self):
        '''
        :returns: the name to use for this Node in a DAG
        :rtype: str
        '''
        return "ACC_loop_" + str(self.abs_position)

    def node_str(self, colour=True):
        '''
        Returns the name of this node with (optional) control codes
        to generate coloured output in a terminal that supports it.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        :rtype: str
        '''
        text = self.coloured_name(colour) + "[ACC Loop"
        if self._sequential:
            text += ", seq"
        else:
            if self._collapse:
                text += ", collapse={0}".format(self._collapse)
            if self._independent:
                text += ", independent"
        text += "]"
        return text

    def gen_code(self, parent):
        '''
        Generate the f2pygen AST entries in the Schedule for this OpenACC
        loop directive.

        :param parent: the parent Node in the Schedule to which to add our
                       content.
        :type parent: sub-class of :py:class:`psyclone.f2pygen.BaseGen`
        :raises GenerationError: if this "!$acc loop" is not enclosed within \
                                 an ACC Parallel region.
        '''

        # It is only at the point of code generation that we can check for
        # correctness (given that we don't mandate the order that a user can
        # apply transformations to the code). As an orphaned loop directive,
        # we must have an ACCParallelDirective as an ancestor somewhere
        # back up the tree.
        if not self.ancestor(ACCParallelDirective):
            raise GenerationError(
                "ACCLoopDirective must have an ACCParallelDirective as an "
                "ancestor in the Schedule")

        # Add any clauses to the directive
        options = []
        if self._sequential:
            options.append("seq")
        else:
            if self._collapse:
                options.append("collapse({0})".format(self._collapse))
            if self._independent:
                options.append("independent")
        options_str = " ".join(options)

        parent.add(DirectiveGen(parent, "acc", "begin", "loop", options_str))

        for child in self.children:
            child.gen_code(parent)

    def update(self):
        '''
        Update the existing fparser2 parse tree with the code associated with
        this ACC LOOP directive.
        '''
        text = "LOOP"
        if self._sequential:
            text += " SEQ"
        else:
            if self._independent:
                text += " INDEPENDENT"
            if self._collapse:
                text += " COLLAPSE({0})".format(self._collapse)
        self._add_region(start_text=text)


class OMPDirective(Directive):
    '''
    Base class for all OpenMP-related directives

    '''
    _PREFIX = "OMP"

    @property
    def dag_name(self):
        '''
        :returns: the name to use in a dag for this node
        :rtype: str
        '''
        return "OMP_directive_" + str(self.abs_position)

    def node_str(self, colour=True):
        '''
        Returns the name of this node with (optional) control codes
        to generate coloured output in a terminal that supports it.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        :rtype: str
        '''
        return self.coloured_name(colour) + "[OMP]"

    def _get_reductions_list(self, reduction_type):
        '''Return the name of all scalars within this region that require a
        reduction of type reduction_type. Returned names will be unique.
        :param reduction_type: The reduction type (e.g. AccessType.SUM) to \
            search for.
        :type reduction_type: :py:class:`psyclone.core.access_type.AccessType`
        '''
        result = []
        for call in self.kernels():
            for arg in call.arguments.args:
                if arg.type in MAPPING_SCALARS.values():
                    if arg.descriptor.access == reduction_type:
                        if arg.name not in result:
                            result.append(arg.name)
        return result


class OMPParallelDirective(OMPDirective):

    @property
    def dag_name(self):
        ''' Return the name to use in a dag for this node'''
        return "OMP_parallel_" + str(self.abs_position)

    def node_str(self, colour=True):
        '''
        Returns the name of this node with (optional) control codes
        to generate coloured output in a terminal that supports it.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        :rtype: str
        '''
        return self.coloured_name(colour) + "[OMP parallel]"

    def gen_code(self, parent):
        '''Generate the fortran OMP Parallel Directive and any associated
        code'''
        from psyclone.f2pygen import AssignGen, UseGen, \
            CommentGen, DeclGen

        private_list = self._get_private_list()

        reprod_red_call_list = self.reductions(reprod=True)
        if reprod_red_call_list:
            # we will use a private thread index variable
            thread_idx = \
                self.root.symbol_table.lookup_with_tag("omp_thread_index").name
            private_list.append(thread_idx)
            # declare the variable
            parent.add(DeclGen(parent, datatype="integer",
                               entity_decls=[thread_idx]))
        private_str = ",".join(private_list)

        # We're not doing nested parallelism so make sure that this
        # omp parallel region is not already within some parallel region
        self._not_within_omp_parallel_region()

        # Check that this OpenMP PARALLEL directive encloses other
        # OpenMP directives. Although it is valid OpenMP if it doesn't,
        # this almost certainly indicates a user error.
        self._encloses_omp_directive()

        calls = self.reductions()

        # first check whether we have more than one reduction with the same
        # name in this Schedule. If so, raise an error as this is not
        # supported for a parallel region.
        names = []
        for call in calls:
            name = call.reduction_arg.name
            if name in names:
                raise GenerationError(
                    "Reduction variables can only be used once in an invoke. "
                    "'{0}' is used multiple times, please use a different "
                    "reduction variable".format(name))
            else:
                names.append(name)

        zero_reduction_variables(calls, parent)

        parent.add(DirectiveGen(parent, "omp", "begin", "parallel",
                                "default(shared), private({0})".
                                format(private_str)))

        if reprod_red_call_list:
            # add in a local thread index
            parent.add(UseGen(parent, name="omp_lib", only=True,
                              funcnames=["omp_get_thread_num"]))
            parent.add(AssignGen(parent, lhs=thread_idx,
                                 rhs="omp_get_thread_num()+1"))

        first_type = type(self.dir_body[0])
        for child in self.dir_body.children:
            if first_type != type(child):
                raise NotImplementedError("Cannot correctly generate code"
                                          " for an OpenMP parallel region"
                                          " containing children of "
                                          "different types")
            child.gen_code(parent)

        parent.add(DirectiveGen(parent, "omp", "end", "parallel", ""))

        if reprod_red_call_list:
            parent.add(CommentGen(parent, ""))
            parent.add(CommentGen(parent, " sum the partial results "
                                  "sequentially"))
            parent.add(CommentGen(parent, ""))
            for call in reprod_red_call_list:
                call.reduction_sum_loop(parent)

    def begin_string(self):
        '''Returns the beginning statement of this directive, i.e.
        "omp parallel". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the opening statement of this directive.
        :rtype: str

        '''
        result = "omp parallel"
        # TODO #514: not yet working with NEMO, so commented out for now
        # if not self._reprod:
        #     result += self._reduction_string()
        private_list = self._get_private_list()
        private_str = ",".join(private_list)

        if private_str:
            result = "{0} private({1})".format(result, private_str)
        return result

    def end_string(self):
        '''Returns the end (or closing) statement of this directive, i.e.
        "omp end parallel". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the end statement for this directive.
        :rtype: str

        '''
        # pylint: disable=no-self-use
        return "omp end parallel"

    def _get_private_list(self):
        '''
        Returns the variable names used for any loops within a directive
        and any variables that have been declared private by a Kernel
        within the directive.

        :returns: list of variables to declare as thread private.
        :rtype: list of str

        :raises InternalError: if a Kernel has local variable(s) but they \
                               aren't named.
        '''
        result = set()
        # get variable names from all calls that are a child of this node
        for call in self.kernels():
            for variable_name in call.local_vars():
                if variable_name == "":
                    raise InternalError(
                        "call '{0}' has a local variable but its "
                        "name is not set.".format(call.name))
                result.add(variable_name.lower())

        # Now determine scalar variables that must be private:
        var_accesses = VariablesAccessInfo()
        self.reference_accesses(var_accesses)
        for var_name in var_accesses.all_vars:
            accesses = var_accesses[var_name].all_accesses
            # Ignore variables that have indices, we only look at scalar
            if accesses[0].indices is not None:
                continue

            # If a variable is only accessed once, it is either an error
            # or a shared variable - anyway it is not private
            if len(accesses) == 1:
                continue

            # We have at least two accesses. If the first one is a write,
            # assume the variable should be private:
            if accesses[0].access_type == AccessType.WRITE:
                # Check if the write access is inside the parallel loop. If
                # the write is outside of a loop, it is an assignment to
                # a shared variable. Example where jpk is likely used
                # outside of the parallel section later, so it must be
                # declared as shared in order to have its value in other loops:
                # !$omp parallel
                # jpk = 100
                # !omp do
                # do ji = 1, jpk

                # TODO #598: improve the handling of scalar variables.

                # Go up the tree till we either find the InvokeSchedule,
                # which is at the top, or a Loop statement (or no parent,
                # which means we have reached the end of a called kernel).
                parent = accesses[0].node.ancestor((Loop, InvokeSchedule),
                                                   include_self=True)

                if parent and isinstance(parent, Loop):
                    # The assignment to the variable is inside a loop, so
                    # declare it to be private
                    result.add(var_name.lower())

        # Convert the set into a list and sort it, so that we get
        # reproducible results
        list_result = list(result)
        list_result.sort()
        return list_result

    def _not_within_omp_parallel_region(self):
        ''' Check that this Directive is not within any other
            parallel region '''
        if self.ancestor(OMPParallelDirective) is not None:
            raise GenerationError("Cannot nest OpenMP parallel regions.")

    def _encloses_omp_directive(self):
        ''' Check that this Parallel region contains other OpenMP
            directives. While it doesn't have to (in order to be valid
            OpenMP), it is likely that an absence of directives
            is an error on the part of the user. '''
        # We need to recurse down through all our children and check
        # whether any of them are an OMPDirective.
        node_list = self.walk(OMPDirective)
        if not node_list:
            # TODO raise a warning here so that the user can decide
            # whether or not this is OK.
            pass
            # raise GenerationError("OpenMP parallel region does not enclose "
            #                       "any OpenMP directives. This is probably "
            #                       "not what you want.")

    def update(self):
        '''
        Updates the fparser2 AST by inserting nodes for this OpenMP
        parallel region.

        '''
        # TODO #435: Remove this function once this is fixed
        self._add_region(
            start_text="parallel default(shared), private({0})".format(
                ",".join(self._get_private_list())),
            end_text="end parallel")


class OMPDoDirective(OMPDirective):
    '''
    Class representing an OpenMP DO directive in the PSyclone AST.

    :param list children: list of Nodes that are children of this Node.
    :param parent: the Node in the AST that has this directive as a child.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`
    :param str omp_schedule: the OpenMP schedule to use.
    :param bool reprod: whether or not to generate code for run-reproducible \
                        OpenMP reductions.

    '''
    def __init__(self, children=None, parent=None, omp_schedule="static",
                 reprod=None):

        if children is None:
            children = []

        if reprod is None:
            self._reprod = Config.get().reproducible_reductions
        else:
            self._reprod = reprod

        self._omp_schedule = omp_schedule

        # Call the init method of the base class once we've stored
        # the OpenMP schedule
        super(OMPDoDirective, self).__init__(children=children,
                                             parent=parent)

    @property
    def dag_name(self):
        ''' Return the name to use in a dag for this node'''
        return "OMP_do_" + str(self.abs_position)

    def node_str(self, colour=True):
        '''
        Returns the name of this node with (optional) control codes
        to generate coloured output in a terminal that supports it.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        :rtype: str
        '''
        if self.reductions():
            reprod = "[reprod={0}]".format(self._reprod)
        else:
            reprod = ""
        return "{0}[OMP do]{1}".format(self.coloured_name(colour), reprod)

    def _reduction_string(self):
        ''' Return the OMP reduction information as a string '''
        reduction_str = ""
        for reduction_type in AccessType.get_valid_reduction_modes():
            reductions = self._get_reductions_list(reduction_type)
            for reduction in reductions:
                reduction_str += ", reduction({0}:{1})".format(
                    OMP_OPERATOR_MAPPING[reduction_type], reduction)
        return reduction_str

    @property
    def reprod(self):
        ''' returns whether reprod has been set for this object or not '''
        return self._reprod

    def gen_code(self, parent):
        '''
        Generate the f2pygen AST entries in the Schedule for this OpenMP do
        directive.

        :param parent: the parent Node in the Schedule to which to add our \
                       content.
        :type parent: sub-class of :py:class:`psyclone.f2pygen.BaseGen`
        :raises GenerationError: if this "!$omp do" is not enclosed within \
                                 an OMP Parallel region.

        '''
        # It is only at the point of code generation that we can check for
        # correctness (given that we don't mandate the order that a user
        # can apply transformations to the code). As an orphaned loop
        # directive, we must have an OMPRegionDirective as an ancestor
        # somewhere back up the tree.
        if not self.ancestor(OMPParallelDirective,
                             excluding=OMPParallelDoDirective):
            raise GenerationError("OMPOrphanLoopDirective must have an "
                                  "OMPRegionDirective as ancestor")

        if self._reprod:
            local_reduction_string = ""
        else:
            local_reduction_string = self._reduction_string()

        # As we're an orphaned loop we don't specify the scope
        # of any variables so we don't have to generate the
        # list of private variables
        options = "schedule({0})".format(self._omp_schedule) + \
                  local_reduction_string
        parent.add(DirectiveGen(parent, "omp", "begin", "do", options))

        for child in self.children:
            child.gen_code(parent)

        # make sure the directive occurs straight after the loop body
        position = parent.previous_loop()
        parent.add(DirectiveGen(parent, "omp", "end", "do", ""),
                   position=["after", position])

    def begin_string(self):
        '''Returns the beginning statement of this directive, i.e.
        "omp do ...". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the beginning statement for this directive.
        :rtype: str

        '''
        return "omp do schedule({0})".format(self._omp_schedule)

    def end_string(self):
        '''Returns the end (or closing) statement of this directive, i.e.
        "omp end do". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the end statement for this directive.
        :rtype: str

        '''
        return "omp end do"

    def update(self):
        '''
        Updates the fparser2 AST by inserting nodes for this OpenMP do.

        :raises GenerationError: if the existing AST doesn't have the \
                                 correct structure to permit the insertion \
                                 of the OpenMP parallel do.
        '''
        # Since this is an OpenMP do, it can only be applied
        # to a single loop.
        if len(self.dir_body.children) != 1:
            raise GenerationError(
                "An OpenMP DO can only be applied to a single loop "
                "but this Node has {0} children: {1}".
                format(len(self.dir_body.children), self.dir_body.children))

        self._add_region(start_text="do schedule({0})".format(
            self._omp_schedule), end_text="end do")


class OMPParallelDoDirective(OMPParallelDirective, OMPDoDirective):
    ''' Class for the !$OMP PARALLEL DO directive. This inherits from
        both OMPParallelDirective (because it creates a new OpenMP
        thread-parallel region) and OMPDoDirective (because it
        causes a loop to be parallelised). '''

    def __init__(self, children=[], parent=None, omp_schedule="static"):
        OMPDoDirective.__init__(self,
                                children=children,
                                parent=parent,
                                omp_schedule=omp_schedule)

    @property
    def dag_name(self):
        ''' Return the name to use in a dag for this node'''
        return "OMP_parallel_do_" + str(self.abs_position)

    def node_str(self, colour=True):
        '''
        Returns the name of this node with (optional) control codes
        to generate coloured output in a terminal that supports it.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        :rtype: str
        '''
        return self.coloured_name(colour) + "[OMP parallel do]"

    def gen_code(self, parent):

        # We're not doing nested parallelism so make sure that this
        # omp parallel do is not already within some parallel region
        self._not_within_omp_parallel_region()

        calls = self.reductions()
        zero_reduction_variables(calls, parent)
        private_str = ",".join(self._get_private_list())
        parent.add(DirectiveGen(parent, "omp", "begin", "parallel do",
                                "default(shared), private({0}), "
                                "schedule({1})".
                                format(private_str, self._omp_schedule) +
                                self._reduction_string()))
        for child in self.children:
            child.gen_code(parent)

        # make sure the directive occurs straight after the loop body
        position = parent.previous_loop()
        parent.add(DirectiveGen(parent, "omp", "end", "parallel do", ""),
                   position=["after", position])

    def update(self):
        '''
        Updates the fparser2 AST by inserting nodes for this OpenMP
        parallel do.

        :raises GenerationError: if the existing AST doesn't have the \
                                 correct structure to permit the insertion \
                                 of the OpenMP parallel do.
        '''
        # Since this is an OpenMP (parallel) do, it can only be applied
        # to a single loop.
        if len(self.dir_body.children) != 1:
            raise GenerationError(
                "An OpenMP PARALLEL DO can only be applied to a single loop "
                "but this Node has {0} children: {1}".
                format(len(self.dir_body.children), self.dir_body.children))

        self._add_region(
            start_text="parallel do default(shared), private({0}), "
            "schedule({1})".format(",".join(self._get_private_list()),
                                   self._omp_schedule),
            end_text="end parallel do")


class GlobalSum(Statement):
    '''
    Generic Global Sum class which can be added to and manipulated
    in, a schedule.

    :param scalar: the scalar that the global sum is stored into
    :type scalar: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
    :param parent: optional parent (default None) of this object
    :type parent: :py:class:`psyclone.psyGen.node`

    '''
    # Textual description of the node.
    _children_valid_format = "<LeafNode>"
    _text_name = "GlobalSum"
    _colour_key = "GlobalSum"

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
        ''' Return the name to use in a dag for this node'''
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

    def __str__(self):
        return self.node_str(False)


class HaloExchange(Statement):
    '''
    Generic Halo Exchange class which can be added to and
    manipulated in, a schedule.

    :param field: the field that this halo exchange will act on
    :type field: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
    :param check_dirty: optional argument default True indicating
    whether this halo exchange should be subject to a run-time check
    for clean/dirty halos.
    :type check_dirty: bool
    :param vector_index: optional vector index (default None) to
    identify which index of a vector field this halo exchange is
    responsible for
    :type vector_index: int
    :param parent: optional parent (default None) of this object
    :type parent: :py:class:`psyclone.psyGen.node`

    '''
    # Textual description of the node.
    _children_valid_format = "<LeafNode>"
    _text_name = "HaloExchange"
    _colour_key = "HaloExchange"

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
        '''helper method which checks that two halo exchange nodes (one being
        self and the other being passed by argument) operating on the
        same field, both have vector fields of the same size and use
        different vector indices. If this is the case then the halo
        exchange nodes do not depend on each other. If this is not the
        case then an internal error will have occured and we raise an
        appropriate exception.

        :param node: a halo exchange which should exchange the same
        field as self
        :type node: :py:class:`psyclone.psyGen.HaloExchange`
        :raises GenerationError: if the argument passed is not a halo exchange
        :raises GenerationError: if the field name in the halo
        exchange passed in has a different name to the field in this
        halo exchange
        :raises GenerationError: if the field in this halo exchange is
        not a vector field
        :raises GenerationError: if the vector size of the field in
        this halo exchange is different to vector size of the field in
        the halo exchange passed by argument.
        :raises GenerationError: if the vector index of the field in
        this halo exchange is the same as the vector index of the
        field in the halo exchange passed by argument.

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

    def __str__(self):
        return self.node_str(False)


class Kern(Statement):
    '''
    Base class representing a call to a sub-program unit from within the
    PSy layer. It is possible for this unit to be in-lined within the
    PSy layer.

    :param parent: parent of this node in the PSyIR.
    :type parent: sub-class of :py:class:`psyclone.psyir.nodes.Node`
    :param call: information on the call itself, as obtained by parsing \
                 the Algorithm layer code.
    :type call: :py:class:`psyclone.parse.algorithm.KernelCall`
    :param str name: the name of the routine being called.
    :param arguments: object holding information on the kernel arguments, \
                      as extracted from kernel meta-data.
    :type arguments: :py:class:`psyclone.psyGen.Arguments`

    :raises GenerationError: if any of the arguments to the call are \
                             duplicated.
    '''
    # Textual representation of the valid children for this node.
    _children_valid_format = "<LeafNode>"

    def __init__(self, parent, call, name, arguments):
        super(Kern, self).__init__(self, parent=parent)
        self._arguments = arguments
        self._name = name
        self._iterates_over = call.ktype.iterates_over

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

        # initialise any reduction information
        reduction_modes = AccessType.get_valid_reduction_modes()
        args = args_filter(arguments.args,
                           arg_types=MAPPING_SCALARS.values(),
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
        return (self.coloured_name(colour) + " " + self.name +
                "(" + self.arguments.names + ")")

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
        else:
            return False

    @property
    def local_reduction_name(self):
        '''Generate a local variable name that is unique for the current
        reduction argument name. This is used for thread-local
        reductions with reproducible reductions '''
        tag = self._reduction_arg.name
        # TODO #720: Deprecate name_from_tag method
        name = self.root.symbol_table.name_from_tag(tag, "l_" + tag)
        return name

    def zero_reduction_variable(self, parent, position=None):
        '''
        Generate code to zero the reduction variable and to zero the local
        reduction variable if one exists. The latter is used for reproducible
        reductions, if specified.

        :param parent: the Node in the AST to which to add new code.
        :type parent: :py:class:`psyclone.psyir.nodes.Node`
        :param str position: where to position the new code in the AST.
        :raises GenerationError: if the variable to zero is not of type \
                                 gh_real or gh_integer.
        :raises GenerationError: if the reprod_pad_size (read from the \
                                 configuration file) is less than 1.

        '''
        from psyclone.f2pygen import AssignGen, DeclGen, AllocateGen
        if not position:
            position = ["auto"]
        var_name = self._reduction_arg.name
        local_var_name = self.local_reduction_name
        var_type = self._reduction_arg.type
        if var_type == "gh_real":
            data_type = "real"
            data_value = "0.0"
            kind_type = \
                Config.get().api_conf("dynamo0.3").default_kind[data_type]
            zero = "_".join([data_value, kind_type])
        elif var_type == "gh_integer":
            data_type = "integer"
            data_value = "0"
            kind_type = \
                Config.get().api_conf("dynamo0.3").default_kind[data_type]
            zero = "_".join([data_value, kind_type])
        else:
            raise GenerationError(
                "zero_reduction variable should be one of ['gh_real', "
                "'gh_integer'] but found '{0}'".format(var_type))

        parent.add(AssignGen(parent, lhs=var_name, rhs=zero),
                   position=position)
        if self.reprod_reduction:
            parent.add(DeclGen(parent, datatype=data_type,
                               entity_decls=[local_var_name],
                               allocatable=True, kind=kind_type,
                               dimension=":,:"))
            nthreads = \
                self.root.symbol_table.lookup_with_tag("omp_num_threads").name
            if Config.get().reprod_pad_size < 1:
                raise GenerationError(
                    "REPROD_PAD_SIZE in {0} should be a positive "
                    "integer, but it is set to '{1}'.".format(
                        Config.get().filename, Config.get().reprod_pad_size))
            pad_size = str(Config.get().reprod_pad_size)
            parent.add(AllocateGen(parent, local_var_name + "(" + pad_size +
                                   "," + nthreads + ")"), position=position)
            parent.add(AssignGen(parent, lhs=local_var_name,
                                 rhs=zero), position=position)

    def reduction_sum_loop(self, parent):
        '''generate the appropriate code to place after the end parallel
        region'''
        from psyclone.f2pygen import DoGen, AssignGen, DeallocateGen
        var_name = self._reduction_arg.name
        local_var_name = self.local_reduction_name
        local_var_ref = self._reduction_ref(var_name)
        reduction_access = self._reduction_arg.access
        try:
            reduction_operator = REDUCTION_OPERATOR_MAPPING[reduction_access]
        except KeyError:
            api_strings = [access.api_specific_name()
                           for access in REDUCTION_OPERATOR_MAPPING]
            raise GenerationError(
                "unsupported reduction access '{0}' found in DynBuiltin:"
                "reduction_sum_loop(). Expected one of '{1}'".
                format(reduction_access.api_specific_name(), api_strings))
        symtab = self.root.symbol_table
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
        thread.'''
        if self.reprod_reduction:
            idx_name = \
                self.root.symbol_table.lookup_with_tag("omp_thread_index").name
            # TODO #720: Deprecate name_from_tag method
            local_name = \
                self.root.symbol_table.name_from_tag(name, "l_" + name)
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
        return self.parent.parent.loop_type == "colour"

    def clear_cached_data(self):
        '''This function is called to remove all cached data (which
        then forces all functions to recompute their results). At this
        stage it supports gen_code by enforcing all arguments to
        be recomputed.
        '''
        self.arguments.clear_cached_data()

    @property
    def iterates_over(self):
        return self._iterates_over

    def local_vars(self):
        raise NotImplementedError("Kern.local_vars should be implemented")

    def __str__(self):
        raise NotImplementedError("Kern.__str__ should be implemented")

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
    :param bool check: Whether or not to check that the number of arguments \
                       specified in the kernel meta-data matches the number \
                       provided by the call in the Algorithm layer.

    :raises GenerationError: if(check) and the number of arguments in the \
                             call does not match that in the meta-data.

    '''
    # Textual description of the node.
    _text_name = "CodedKern"
    _colour_key = "CodedKern"

    def __init__(self, KernelArguments, call, parent=None, check=True):
        self._parent = parent
        super(CodedKern, self).__init__(parent, call,
                                        call.ktype.procedure.name,
                                        KernelArguments(call, self))
        self._module_name = call.module_name
        self._module_code = call.ktype._ast
        self._kernel_code = call.ktype.procedure
        self._fp2_ast = None  # The fparser2 AST for the kernel
        self._kern_schedule = None  # PSyIR schedule for the kernel
        # Whether or not this kernel has been transformed
        self._modified = False
        # Whether or not to in-line this kernel into the module containing
        # the PSy layer
        self._module_inline = False
        self._opencl_options = {'local_size': 1, 'queue_number': 1}
        if check and len(call.ktype.arg_descriptors) != len(call.args):
            raise GenerationError(
                "error: In kernel '{0}' the number of arguments specified "
                "in the kernel metadata '{1}', must equal the number of "
                "arguments in the algorithm layer. However, I found '{2}'".
                format(call.ktype.procedure.name,
                       len(call.ktype.arg_descriptors),
                       len(call.args)))
        self.arg_descriptors = call.ktype.arg_descriptors

    def get_kernel_schedule(self):
        '''
        Returns a PSyIR Schedule representing the kernel code. The Schedule
        is just generated on first invocation, this allows us to retain
        transformations that may subsequently be applied to the Schedule
        (but will not adapt to transformations applied to the fparser2 AST).

        :returns: Schedule representing the kernel code.
        :rtype: :py:class:`psyclone.psyGen.KernelSchedule`
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
                            "CodedKern opencl_option 'local_size' should be "
                            "an integer.")
                if key == "queue_number":
                    if not isinstance(value, int):
                        raise TypeError(
                            "CodedKern opencl_option 'queue_number' should be "
                            "an integer.")
            else:
                raise AttributeError(
                    "CodedKern does not support the opencl_option '{0}'. "
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
        ''' Return the name to use in a dag for this node'''
        return "kernel_{0}_{1}".format(self.name, str(self.abs_position))

    @property
    def module_inline(self):
        return self._module_inline

    @module_inline.setter
    def module_inline(self, value):
        '''
        Setter for whether or not to module-inline this kernel.

        :param bool value: Whether or not to module-inline this kernel.
        :raises NotImplementedError: if module-inlining is enabled and the \
                                     kernel has been transformed.
        '''
        # Check all kernels in the same invoke as this one and set any
        # with the same name to the same value as this one. This is
        # required as inlining (or not) affects all calls to the same
        # kernel within an invoke. Note, this will set this kernel as
        # well so there is no need to set it locally.
        if value and self.modified:
            # TODO #229. Kernel in-lining is currently implemented via
            # manipulation of the fparser1 Parse Tree while
            # transformations work with the fparser2 Parse Tree-derived
            # PSyIR.  Therefore there is presently no way to inline a
            # transformed kernel.
            raise NotImplementedError(
                "Cannot module-inline a transformed kernel ({0}).".
                format(self.name))
        my_schedule = self.ancestor(InvokeSchedule)
        for kernel in my_schedule.walk(Kern):
            if kernel.name == self.name:
                kernel._module_inline = value

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

    def gen_code(self, parent):
        '''
        Generates the f2pygen AST of the Fortran for this kernel call and
        writes the kernel itself to file if it has been transformed.

        :param parent: The parent of this kernel call in the f2pygen AST.
        :type parent: :py:calls:`psyclone.f2pygen.LoopGen`
        '''
        from psyclone.f2pygen import CallGen, UseGen

        # If the kernel has been transformed then we rename it. If it
        # is *not* being module inlined then we also write it to file.
        self.rename_and_write()

        parent.add(CallGen(parent, self._name,
                           self.arguments.raw_arg_list()))

        if not self.module_inline:
            parent.add(UseGen(parent, name=self._module_name, only=True,
                              funcnames=[self._name]))

    def gen_arg_setter_code(self, parent):
        '''
        Creates a Fortran routine to set the arguments of the OpenCL
        version of this kernel.

        :param parent: Parent node of the set-kernel-arguments routine.
        :type parent: :py:class:`psyclone.f2pygen.ModuleGen`
        '''
        raise NotImplementedError("gen_arg_setter_code must be implemented "
                                  "by sub-class.")

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
        # Create an fparser2 Fortran2003 parser
        my_parser = parser.ParserFactory().create()
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
        if not self.modified and not self.root.opencl:
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

            # GOcean OpenCL needs to differentiate between kernels generated
            # from the same module file, so we include the kernelname into the
            # output filename.
            # TODO: Issue 499, this works as an OpenCL quickfix but it needs
            # to be generalized and be consistent with the '--kernel-renaming'
            # conventions.
            if self.root.opencl:
                if self.name.lower().endswith("_code"):
                    new_suffix += "_" + self.name[:-5]
                else:
                    new_suffix += "_" + self.name

            new_suffix += "_{0}".format(name_idx)

            # Choose file extension
            if self.root.opencl:
                new_name = old_base_name + new_suffix + ".cl"
            else:
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
        # We can't rename OpenCL kernels as the Invoke set_args functions
        # have already been generated. The link to an specific kernel
        # implementation is delayed to run-time in OpenCL. (e.g. FortCL has
        # the  PSYCLONE_KERNELS_FILE environment variable)
        if not self.root.opencl:
            if self._kern_schedule:
                # A PSyIR kernel schedule has been created. This means
                # that the PSyIR has been modified and will be used to
                # generate modified kernel code. Therefore the PSyIR
                # should be modified rather than the parse tree. This
                # if test, and the associated else, are only required
                # whilst old style (direct fp2) transformations still
                # exist - #490.

                # Rename PSyIR module and kernel names.
                self._rename_psyir(new_suffix)
            else:
                self._rename_ast(new_suffix)

        # Kernel is now self-consistent so unset the modified flag
        self.modified = False

        # If this kernel is being module in-lined then we do not need to
        # write it to file.
        if self.module_inline:
            # TODO #229. We cannot currently inline transformed kernels
            # (because that requires an fparser1 AST and we only have an
            # fparser2 AST of the modified kernel) so raise an error.
            raise NotImplementedError("Cannot module-inline a transformed "
                                      "kernel ({0})".format(self.name))

        if self.root.opencl:
            from psyclone.psyir.backend.opencl import OpenCLWriter
            ocl_writer = OpenCLWriter(
                kernels_local_size=self._opencl_options['local_size'])
            new_kern_code = ocl_writer(self.get_kernel_schedule())
        elif self._kern_schedule:
            # A PSyIR kernel schedule has been created. This means
            # that the PSyIR has been modified. Therefore use the
            # chosen PSyIR back-end to write out the modified kernel
            # code. At the moment there is no way to choose which
            # back-end to use, so simply use the Fortran one (and
            # limit the line length). This test is only required
            # whilst old style (direct fp2) transformations still
            # exist.
            from psyclone.psyir.backend.fortran import FortranWriter
            fortran_writer = FortranWriter()
            # Start from the root of the schedule as we want to output
            # any module information surrounding the kernel subroutine
            # as well as the subroutine itself.
            new_kern_code = fortran_writer(self.get_kernel_schedule().root)
            fll = FortLineLength()
            new_kern_code = fll.process(new_kern_code)
        else:
            # This is an old style transformation which modifes the
            # fp2 parse tree directly. Therefore use the fp2
            # representation to generate the Fortran for this
            # transformed kernel, ensuring that the line length is
            # limited.
            fll = FortLineLength()
            new_kern_code = fll.process(str(self.ast))

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
        KernelSchedule nodes.

        :param str suffix: the string to insert into the quantity names.

        '''
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

        kern_schedule = self.get_kernel_schedule()
        kern_schedule.name = new_kern_name[:]
        kern_schedule.root.name = new_mod_name[:]

    def _rename_ast(self, suffix):
        '''
        Renames all quantities (module, kernel routine, kernel derived type)
        in the kernel AST by inserting the supplied suffix. The resulting
        names follow the PSyclone naming convention (modules end with "_mod",
        types with "_type" and kernels with "_code").

        :param str suffix: the string to insert into the quantity names.
        '''
        from fparser.two.utils import walk

        # Use the suffix we have determined to create a new kernel name.
        # This will conform to the PSyclone convention of ending in "_code"
        orig_mod_name = self.module_name[:]
        orig_kern_name = self.name[:]

        new_kern_name = self._new_name(orig_kern_name, suffix, "_code")
        new_mod_name = self._new_name(orig_mod_name, suffix, "_mod")

        # Query the fparser2 AST to determine the name of the type that
        # contains the kernel subroutine as a type-bound procedure
        orig_type_name = ""
        new_type_name = ""
        dtypes = walk(self.ast.content, Fortran2003.Derived_Type_Def)
        for dtype in dtypes:
            tbound_proc = walk(dtype.content,
                               Fortran2003.Type_Bound_Procedure_Part)
            names = walk(tbound_proc[0].content, Fortran2003.Name)
            if str(names[-1]) == self.name:
                # This is the derived type for this kernel. Now we need
                # its name...
                tnames = walk(dtype.content, Fortran2003.Type_Name)
                orig_type_name = str(tnames[0])

                # The new name for the type containing kernel metadata will
                # conform to the PSyclone convention of ending in "_type"
                new_type_name = self._new_name(orig_type_name, suffix, "_type")
                # Rename the derived type. We do this here rather than
                # search for Type_Name in the AST again below. We loop over
                # the list of type names so as to ensure we rename the type
                # in the end-type statement too.
                for name in tnames:
                    if str(name) == orig_type_name:
                        name.string = new_type_name

        # Change the name of this kernel and the associated module
        self.name = new_kern_name[:]
        self._module_name = new_mod_name[:]

        # Construct a dictionary for mapping from old kernel/type/module
        # names to the corresponding new ones
        rename_map = {orig_mod_name: new_mod_name,
                      orig_kern_name: new_kern_name,
                      orig_type_name: new_type_name}

        # Re-write the values in the AST
        names = walk(self.ast.content, Fortran2003.Name)
        for name in names:
            try:
                new_value = rename_map[str(name)]
                name.string = new_value[:]
            except KeyError:
                # This is not one of the names we are looking for
                continue

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
    '''
    # Textual description of the node.
    _children_valid_format = "Schedule"
    _text_name = "InlinedKern"
    _colour_key = "InlinedKern"

    def __init__(self, psyir_nodes):
        # pylint: disable=non-parent-init-called, super-init-not-called
        Node.__init__(self)
        schedule = Schedule(children=psyir_nodes, parent=self)
        self.children = [schedule]
        # Update the parent info for each node we've moved
        for node in schedule.children:
            node.parent = schedule

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

    def node_str(self, colour=True):
        '''
        Creates a class-specific text description of this node, optionally
        including colour control codes (for coloured output in a terminal).

        :param bool colour: whether or not to include colour control codes.

        :returns: the class-specific text describing this node.
        :rtype: str
        '''
        return self.coloured_name(colour) + "[]"

    def __str__(self):
        return self.coloured_name(False)

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
    _colour_key = "BuiltIn"

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
        ''' Return the name to use in a dag for this node'''
        return "builtin_{0}_".format(self.name) + str(self.abs_position)

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

    def clear_cached_data(self):
        '''This function is called to clear all cached data, which
        enforces that raw_arg_list is recomputed.'''
        self._raw_arg_list = []

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
           self._arg.vector_size > 1:
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
    ''' Argument base class '''

    def __init__(self, call, arg_info, access):
        '''
        :param call: the call that this argument is associated with.
        :type call: :py:class:`psyclone.psyGen.Kern`
        :param arg_info: Information about this argument collected by \
                         the parser.
        :type arg_info: :py:class:`psyclone.parse.algorithm.Arg`
        :param access: the way in which this argument is accessed in \
                 the 'Kern'. Valid values are specified in the config object \
                 of the current API.
        :type access: str

        '''
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
        self._access = access

        if self._orig_name is None:
            # this is an infrastructure call literal argument. Therefore
            # we do not want an argument (_text=None) but we do want to
            # keep the value (_name)
            self._name = arg_info.text
            self._text = None
        else:
            # There are unit-tests where we create Arguments without an
            # associated call.
            if self._call:
                tag = "AlgArgs_" + self._text
                # TODO #720: Deprecate name_from_tag method
                self._name = self._call.root.symbol_table.name_from_tag(
                    tag, self._orig_name)

        self._vector_size = 1

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
        :param value: New access type.
        :type value: :py:class:`psyclone.core.access_type.AccessType`.
        :raisesInternalError if value is not an AccessType.
        '''
        if not isinstance(value, AccessType):
            raise InternalError("Invalid access type '{0}' of type '{1}."
                                .format(value, type(value)))

        self._access = value

    @property
    def type(self):
        '''Return the type of the argument. API's that do not have this
        concept (such as gocean0.1 and dynamo0.1) can use this
        baseclass version which just returns "field" in all
        cases. API's with this concept can override this method '''
        return "field"

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

        :returns: the first preceding argument this argument has a
        dependence with
        :rtype: :py:class:`psyclone.psyGen.Argument`

        '''
        nodes = self._call.preceding(reverse=True)
        return self._find_argument(nodes)

    def backward_write_dependencies(self, ignore_halos=False):
        '''Returns a list of previous write arguments that this argument has
        dependencies with. The arguments may exist in a call, a
        haloexchange (unless `ignore_halos` is `True`), or a globalsum. If
        none are found then return an empty list. If self is not a
        reader then return an empty list.

        :param: ignore_halos: An optional, default `False`, boolean flag
        :type: ignore_halos: bool
        :returns: a list of arguments that this argument has a dependence with
        :rtype: :func:`list` of :py:class:`psyclone.psyGen.Argument`

        '''
        nodes = self._call.preceding(reverse=True)
        results = self._find_write_arguments(nodes, ignore_halos=ignore_halos)
        return results

    def forward_dependence(self):
        '''Returns the following argument that this argument has a direct
        dependence with, or `None` if there is not one. The argument may
        exist in a call, a haloexchange, or a globalsum.

        :returns: the first following argument this argument has a
        dependence with
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

        :returns: a list of arguments that this argument has a dependence with
        :rtype: :func:`list` of :py:class:`psyclone.psyGen.Argument`

        '''
        nodes = self._call.following()
        return self._find_read_arguments(nodes)

    def _find_argument(self, nodes):
        '''Return the first argument in the list of nodes that has a
        dependency with self. If one is not found return None

        :param: the list of nodes that this method examines
        :type: :func:`list` of :py:class:`psyclone.psyir.nodes.Node`
        :returns: An argument object or None
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

        :param: the list of nodes that this method examines
        :type: :func:`list` of :py:class:`psyclone.psyir.nodes.Node`
        :returns: a list of arguments that this argument has a dependence with
        :rtype: :func:`list` of :py:class:`psyclone.psyGen.Argument`

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

        :param: the list of nodes that this method examines
        :type: :func:`list` of :py:class:`psyclone.psyir.nodes.Node`
        :param: ignore_halos: An optional, default `False`, boolean flag
        :type: ignore_halos: bool
        :returns: a list of arguments that this argument has a dependence with
        :rtype: :func:`list` of :py:class:`psyclone.psyGen.Argument`

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

        :param argument: the argument we will check to see whether
        there is a dependence with this argument instance (self)
        :type argument: :py:class:`psyclone.psyGen.Argument`
        :returns: True if there is a dependence and False if not
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
        # layout, where transformations are in different directories and files
        if module is None:
            # default to the transformation module
            from psyclone import transformations
            module = transformations
        if base_class is None:
            from psyclone import psyGen
            base_class = psyGen.Transformation
        # find our transformations
        self._classes = self._find_subclasses(module, base_class)

        # create our transformations
        self._objects = []
        self._obj_map = {}
        for my_class in self._classes:
            my_object = my_class()
            self._objects.append(my_object)
            self._obj_map[my_object.name] = my_object

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
class Transformation(object):
    '''Abstract baseclass for a transformation. Uses the abc module so it
        can not be instantiated. '''

    @abc.abstractproperty
    def name(self):
        '''Returns the name of the transformation.'''
        return

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

        :returns: 2-tuple of new schedule and memento of transform.
        :rtype: (:py:class:`psyclone.dynamo0p3.DynInvokeSchedule`, \
                 :py:class:`psyclone.undoredo.Memento`)

        '''
        # pylint: disable=no-self-use
        schedule = None
        memento = None
        return schedule, memento

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
    def name(self):
        return

    def apply(self, node, options=None):
        return None, None


class ACCKernelsDirective(ACCDirective):
    '''
    Class representing the !$ACC KERNELS directive in the PSyIR.

    :param children: the PSyIR nodes to be enclosed in the Kernels region \
                     and which are therefore children of this node.
    :type children: list of sub-classes of \
                    :py:class:`psyclone.psyir.nodes.Node`
    :param parent: the parent of this node in the PSyIR.
    :type parent: sub-class of :py:class:`psyclone.psyir.nodes.Node`
    :param bool default_present: whether or not to add the "default(present)" \
                                 clause to the kernels directive.

    :raises NotImplementedError: if default_present is False.

    '''
    def __init__(self, children=None, parent=None, default_present=True):
        super(ACCKernelsDirective, self).__init__(children=children,
                                                  parent=parent)
        self._default_present = default_present

    @property
    def dag_name(self):
        '''
        :returns: the name to use for this node in a dag.
        :rtype: str
        '''
        return "ACC_kernels_" + str(self.abs_position)

    def node_str(self, colour=True):
        ''' Returns the name of this node with (optional) control codes
        to generate coloured output in a terminal that supports it.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        :rtype: str
        '''
        return self.coloured_name(colour) + "[ACC Kernels]"

    def gen_code(self, parent):
        '''
        Generate the f2pygen AST entries in the Schedule for this
        OpenACC Kernels directive.

        :param parent: the parent Node in the Schedule to which to add this \
                       content.
        :type parent: sub-class of :py:class:`psyclone.f2pygen.BaseGen`

        '''
        data_movement = ""
        if self._default_present:
            data_movement = "default(present)"
        parent.add(DirectiveGen(parent, "acc", "begin", "kernels",
                                data_movement))
        for child in self.children:
            child.gen_code(parent)
        parent.add(DirectiveGen(parent, "acc", "end", "kernels", ""))

    def update(self):
        '''
        Updates the fparser2 AST by inserting nodes for this ACC kernels
        directive.
        '''
        data_movement = None
        if self._default_present:
            data_movement = "present"
        self._add_region(start_text="KERNELS", end_text="END KERNELS",
                         data_movement=data_movement)


class ACCDataDirective(ACCDirective):
    '''
    Class representing the !$ACC DATA ... !$ACC END DATA directive
    in the PSyIR.

    '''
    @property
    def dag_name(self):
        '''
        :returns: the name to use in a dag for this node.
        :rtype: str
        '''
        return "ACC_data_" + str(self.abs_position)

    def node_str(self, colour=True):
        ''' Returns the name of this node with (optional) control codes
        to generate coloured output in a terminal that supports it.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        :rtype: str
        '''
        return self.coloured_name(colour) + "[ACC DATA]"

    def gen_code(self, _):
        '''
        :raises InternalError: the ACC data directive is currently only \
                               supported for the NEMO API and that uses the \
                               update() method to alter the underlying \
                               fparser2 parse tree.
        '''
        raise InternalError(
            "ACCDataDirective.gen_code should not have been called.")

    def update(self):
        '''
        Updates the fparser2 AST by inserting nodes for this OpenACC Data
        directive.

        '''
        self._add_region(start_text="DATA", end_text="END DATA",
                         data_movement="analyse")


class KernelSchedule(Schedule):
    '''
    A kernelSchedule is the parent node of the PSyIR for Kernel source code.

    :param str name: Kernel subroutine name.
    :param parent: Parent of the KernelSchedule, defaults to None.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`

    '''
    def __init__(self, name, parent=None):
        super(KernelSchedule, self).__init__(children=None, parent=parent)
        self._name = name

    @staticmethod
    def create(name, symbol_table, children):
        '''Create a KernelSchedule instance given a name, a symbol table and a
        list of child nodes.

        :param str name: the name of the KernelSchedule.
        :param symbol_table: the symbol table associated with this \
            KernelSchedule.
        :type symbol_table: :py:class:`psyclone.psyGen.SymbolTable`
        :param children: a list of PSyIR nodes contained in the \
            KernelSchedule.
        :type children: list of :py:class:`psyclone.psyir.nodes.Node`

        :returns: a KernelSchedule instance.
        :rtype: :py:class:`psyclone.psyGen.KernelInstance`

        :raises GenerationError: if the arguments to the create method \
            are not of the expected type.

        '''
        if not isinstance(name, str):
            raise GenerationError(
                "name argument in create method of KernelSchedule class "
                "should be a string but found '{0}'."
                "".format(type(name).__name__))
        if not isinstance(symbol_table, SymbolTable):
            raise GenerationError(
                "symbol_table argument in create method of KernelSchedule "
                "class should be a SymbolTable but found '{0}'."
                "".format(type(symbol_table).__name__))
        if not isinstance(children, list):
            raise GenerationError(
                "children argument in create method of KernelSchedule class "
                "should be a list but found '{0}'."
                "".format(type(children).__name__))
        for child in children:
            if not isinstance(child, Node):
                raise GenerationError(
                    "child of children argument in create method of "
                    "KernelSchedule class should be a PSyIR Node but "
                    "found '{0}'.".format(type(child).__name__))

        kern = KernelSchedule(name)
        kern._symbol_table = symbol_table
        symbol_table._schedule = kern
        for child in children:
            child.parent = kern
        kern.children = children
        return kern

    @property
    def name(self):
        '''
        :returns: Name of the Kernel
        :rtype: str
        '''
        return self._name

    @name.setter
    def name(self, new_name):
        '''
        Sets a new name for the kernel.

        :param str new_name: New name for the kernel.
        '''
        self._name = new_name

    def node_str(self, colour=True):
        ''' Returns the name of this node with (optional) control codes
        to generate coloured output in a terminal that supports it.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        :rtype: str
        '''
        return self.coloured_name(colour) + "[name:'" + self._name + "']"

    def __str__(self):
        result = self.node_str(False) + ":\n"
        for entity in self._children:
            result += str(entity)
        result += "End KernelSchedule\n"
        return result
