# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2019, Science and Technology Facilities Council.
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
from enum import Enum
import abc
from collections import OrderedDict
import re
import six
from fparser.two import Fortran2003
from psyclone.configuration import Config
from psyclone.f2pygen import DirectiveGen
from psyclone.core.access_info import VariablesAccessInfo, AccessType

# We use the termcolor module (if available) to enable us to produce
# coloured, textual representations of Invoke schedules. If it's not
# available then we don't use colour.
try:
    from termcolor import colored
except ImportError:
    # We don't have the termcolor package available so provide
    # alternative routine
    def colored(text, _):
        '''
        Returns the supplied text argument unchanged. This is a swap-in
        replacement for when termcolor.colored is not available.

        :param text: Text to return
        :type text: string
        :param _: Fake argument, only required to match interface
                  provided by termcolor.colored
        :returns: The supplied text, unchanged
        :rtype: string
        '''
        return text


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

# Colour map to use when writing Invoke schedule to terminal. (Requires
# that the termcolor package be installed. If it isn't then output is not
# coloured.) See https://pypi.python.org/pypi/termcolor for details.
SCHEDULE_COLOUR_MAP = {"Schedule": "white",
                       "Loop": "red",
                       "GlobalSum": "cyan",
                       "Directive": "green",
                       "HaloExchange": "blue",
                       "HaloExchangeStart": "yellow",
                       "HaloExchangeEnd": "yellow",
                       "BuiltIn": "magenta",
                       "CodedKern": "magenta",
                       "Profile": "green",
                       "Extract": "green",
                       "If": "red",
                       "Assignment": "blue",
                       "Reference": "yellow",
                       "Operation": "blue",
                       "Literal": "yellow",
                       "Return": "yellow",
                       "CodeBlock": "red",
                       "Container": "green"}

# Default indentation string
INDENTATION_STRING = "    "


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


class GenerationError(Exception):
    ''' Provides a PSyclone specific error class for errors found during PSy
        code generation. '''
    def __init__(self, value):
        Exception.__init__(self, value)
        self.value = "Generation Error: "+value

    def __str__(self):
        return str(self.value)


class FieldNotFoundError(Exception):
    ''' Provides a PSyclone-specific error class when a field with the
    requested property/ies is not found '''
    def __init__(self, value):
        Exception.__init__(self, value)
        self.value = "Field not found error: "+value

    def __str__(self):
        return str(self.value)


class InternalError(Exception):
    '''
    PSyclone-specific exception for use when an internal error occurs (i.e.
    something that 'should not happen').

    :param str value: the message associated with the error.
    '''
    def __init__(self, value):
        Exception.__init__(self, value)
        self.value = "PSyclone internal error: "+value

    def __str__(self):
        return str(self.value)


class SymbolError(Exception):
    '''
    PSyclone-specific exception for use with errors relating to the SymbolTable
    in the PSyIR.

    :param str value: the message associated with the error.
    '''
    def __init__(self, value):
        Exception.__init__(self, value)
        self.value = "PSyclone SymbolTable error: "+value

    def __str__(self):
        return str(self.value)


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
        :type parent: :py:class:`psyclone.psyGen.Node`.
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
    '''Manage the invoke calls

    :param alg_calls: A list of invoke metadata extracted by the \
    parser.
    :type alg_calls: list of \
    :py:class:`psyclone.parse.algorithm.InvokeCall`
    :param Invoke: An api-specific Invoke class
    :type Invoke: Specialisation of :py:class:`psyclone.psyGen.Invoke`

    '''
    def __init__(self, alg_calls, Invoke):
        self.invoke_map = {}
        self.invoke_list = []
        for idx, alg_invocation in enumerate(alg_calls):
            my_invoke = Invoke(alg_invocation, idx)
            self.invoke_map[my_invoke.name] = my_invoke
            self.invoke_list.append(my_invoke)

    def __str__(self):
        return "Invokes object containing "+str(self.names)

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
            invoke.gen_code(parent)
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


class NameSpaceFactory(object):
    # storage for the instance reference
    _instance = None

    def __init__(self, reset=False):
        """ Create singleton instance """
        # Check whether we already have an instance
        if NameSpaceFactory._instance is None or reset:
            # Create and remember instance
            NameSpaceFactory._instance = NameSpace()

    def create(self):
        return NameSpaceFactory._instance


class NameSpace(object):
    '''keeps a record of reserved names and used names for clashes and
        provides a new name if there is a clash. '''

    def __init__(self, case_sensitive=False):
        self._reserved_names = []
        self._added_names = []
        self._context = {}
        self._case_sensitive = case_sensitive

    def create_name(self, root_name=None, context=None, label=None):
        '''Returns a unique name. If root_name is supplied, the name returned
            is based on this name, otherwise one is made up.  If
            context and label are supplied and a previous create_name
            has been called with the same context and label then the
            name provided by the previous create_name is returned.
        '''
        # make up a base name if one has not been supplied
        if root_name is None:
            root_name = "anon"
        # if not case sensitive then make the name lower case
        if not self._case_sensitive:
            lname = root_name.lower()
        else:
            lname = root_name
        # check context and label validity
        if context is None and label is not None or \
                context is not None and label is None:
            raise RuntimeError(
                "NameSpace:create_name() requires both context and label to "
                "be set")

        # if the same context and label have already been supplied
        # then return the previous name
        if context is not None and label is not None:
            # labels may have spurious white space
            label = label.strip()
            if not self._case_sensitive:
                label = label.lower()
                context = context.lower()
            if context in self._context:
                if label in self._context[context]:
                    # context and label have already been supplied
                    return self._context[context][label]
            else:
                # initialise the context so we can add the label value later
                self._context[context] = {}

        # create our name
        if lname not in self._reserved_names and \
                lname not in self._added_names:
            proposed_name = lname
        else:
            count = 1
            proposed_name = lname + "_" + str(count)
            while proposed_name in self._reserved_names or \
                    proposed_name in self._added_names:
                count += 1
                proposed_name = lname+"_"+str(count)

        # store our name
        self._added_names.append(proposed_name)
        if context is not None and label is not None:
            self._context[context][label] = proposed_name

        return proposed_name

    def add_reserved_name(self, name):
        ''' adds a reserved name. create_name() will not return this name '''
        if not self._case_sensitive:
            lname = name.lower()
        else:
            lname = name
        # silently ignore if this is already a reserved name
        if lname not in self._reserved_names:
            if lname in self._added_names:
                raise RuntimeError(
                    "attempted to add a reserved name to a namespace that"
                    " has already used that name")
            self._reserved_names.append(lname)

    def add_reserved_names(self, names):
        ''' adds a list of reserved names '''
        for name in names:
            self.add_reserved_name(name)


class Invoke(object):
    ''' Manage an individual invoke call '''

    def __str__(self):
        return self._name+"("+", ".join([str(arg) for arg in
                                         self._alg_unique_args])+")"

    def __init__(self, alg_invocation, idx, schedule_class,
                 reserved_names=None):
        '''Constructs an invoke object. Parameters:

        :param alg_invocation:
        :type alg_invocation:
        :param idx: Position/index of this invoke call in the subroutine.
            If not None, this number is added to the name ("invoke_").
        :type idx: Integer.
        :param schedule_class: The schedule class to create for this invoke.
        :type schedule_class: :py:class:`psyclone.psyGen.InvokeSchedule`.
        :param reserved_names: Optional argument: list of reserved names,
               i.e. names that should not be used e.g. as psyclone created
               variable name.
        :type reserved_names: List of strings.
        '''

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

        # create our namespace manager - must be done before creating the
        # schedule
        self._name_space_manager = NameSpaceFactory(reset=True).create()

        # Add the name for the call to the list of reserved names. This
        # ensures we don't get a name clash with any variables we subsequently
        # generate.
        if reserved_names:
            reserved_names.append(self._name)
        else:
            reserved_names = [self._name]
        self._name_space_manager.add_reserved_names(reserved_names)

        # create the schedule
        self._schedule = schedule_class(alg_invocation.kcalls)

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


class Node(object):
    '''
    Base class for a node in the PSyIR (schedule).

    :param ast: reference into the fparser2 AST corresponding to this node.
    :type ast: sub-class of :py:class:`fparser.two.Fortran2003.Base`
    :param children: the PSyIR nodes that are children of this node.
    :type children: list of :py:class:`psyclone.psyGen.Node`
    :param parent: that parent of this node in the PSyIR tree.
    :type parent: :py:class:`psyclone.psyGen.Node`

    '''
    # Define two class constants: START_DEPTH and START_POSITION
    # START_DEPTH is used to calculate depth of all Nodes in the tree
    # (1 for main Nodes and increasing for their descendants).
    START_DEPTH = 0
    # START_POSITION is used to to calculate position of all Nodes in
    # the tree (absolute or relative to a parent).
    START_POSITION = 0

    def __init__(self, ast=None, children=None, parent=None):
        if not children:
            self._children = []
        else:
            self._children = children
        self._parent = parent
        # Reference into fparser2 AST (if any)
        self._ast = ast
        # Ref. to last fparser2 parse tree node associated with this Node.
        # This is required when adding directives.
        self._ast_end = None
        # List of tags that provide additional information about this Node.
        self._annotations = []
        # Name to use for this Node type. By default we use the name of
        # the class but this can be overridden by a sub-class.
        self._text_name = self.__class__.__name__
        # Which colour to use from the SCHEDULE_COLOUR_MAP
        self._colour_key = self.__class__.__name__

    def coloured_name(self, colour=True):
        '''
        Returns the display name of this Node, optionally with colour control
        codes (requires that the termcolor package be installed).

        :param bool colour: whether or not to include colour control codes \
                            in the result.

        :returns: the name of this node, optionally with colour control codes.
        :rtype: str
        '''
        if colour:
            try:
                return colored(self._text_name,
                               SCHEDULE_COLOUR_MAP[self._colour_key])
            except KeyError:
                pass
        return self._text_name

    def node_str(self, colour=True):
        '''
        :param bool colour: whether or not to include control codes for \
                            coloured text.

        :returns: a text description of this node. Will typically be \
                  overridden by sub-class.
        :rtype: str
        '''
        return self.coloured_name(colour) + "[]"

    def __str__(self):
        return self.node_str(False)

    def math_equal(self, other):
        '''Returns True if the self has the same results as other. The
        implementation in the base class just confirms that the type is the
        same, and the number of children as well.

        :param other: the node to compare self with.
        :type other: py:class:`psyclone.psyGen.Node`.

        :returns: whether self has the same result as other.
        :rtype: bool
        '''

        # pylint: disable=unidiomatic-typecheck
        if type(self) != type(other):
            return False

        if len(self.children) != len(other.children):
            return False

        for i, entity in enumerate(self.children):
            if not entity.math_equal(other.children[i]):
                return False
        return True

    @property
    def ast(self):
        '''
        :returns: a reference to that part of the fparser2 parse tree that \
                  this node represents or None.
        :rtype: sub-class of :py:class:`fparser.two.utils.Base`
        '''
        return self._ast

    @property
    def ast_end(self):
        '''
        :returns: a reference to the last node in the fparser2 parse tree \
                  that represents a child of this PSyIR node or None.
        :rtype: sub-class of :py:class:`fparser.two.utils.Base`
        '''
        return self._ast_end

    @ast.setter
    def ast(self, ast):
        '''
        Set a reference to the fparser2 node associated with this Node.

        :param ast: fparser2 node associated with this Node.
        :type ast: :py:class:`fparser.two.utils.Base`
        '''
        self._ast = ast

    @ast_end.setter
    def ast_end(self, ast_end):
        '''
        Set a reference to the last fparser2 node associated with this Node.

        :param ast: last fparser2 node associated with this Node.
        :type ast: :py:class:`fparser.two.utils.Base`
        '''
        self._ast_end = ast_end

    @property
    def annotations(self):
        ''' Return the list of annotations attached to this Node.

        :returns: List of anotations
        :rtype: list of str
        '''
        return self._annotations

    def dag(self, file_name='dag', file_format='svg'):
        '''Create a dag of this node and its children.'''
        try:
            import graphviz as gv
        except ImportError:
            # todo: add a warning to a log file here
            # silently return if graphviz bindings are not installed
            return
        try:
            graph = gv.Digraph(format=file_format)
        except ValueError:
            raise GenerationError(
                "unsupported graphviz file format '{0}' provided".
                format(file_format))
        self.dag_gen(graph)
        graph.render(filename=file_name)

    def dag_gen(self, graph):
        '''Output my node's graph (dag) information and call any
        children. Nodes with children are represented as two vertices,
        a start and an end. Forward dependencies are represented as
        green edges, backward dependencies are represented as red
        edges (but their direction is reversed so the layout looks
        reasonable) and parent child dependencies are represented as
        blue edges.'''
        # names to append to my default name to create start and end vertices
        start_postfix = "_start"
        end_postfix = "_end"
        if self.children:
            # I am represented by two vertices, a start and an end
            graph.node(self.dag_name+start_postfix)
            graph.node(self.dag_name+end_postfix)
        else:
            # I am represented by a single vertex
            graph.node(self.dag_name)
        # first deal with forward dependencies
        remote_node = self.forward_dependence()
        local_name = self.dag_name
        if self.children:
            # edge will come from my end vertex as I am a forward dependence
            local_name += end_postfix
        if remote_node:
            # this node has a forward dependence
            remote_name = remote_node.dag_name
            if remote_node.children:
                # the remote node has children so I will connect to
                # its start vertex
                remote_name += start_postfix
            # Create the forward dependence edge in green
            graph.edge(local_name, remote_name, color="green")
        elif self.parent:
            # this node is a child of another node and has no forward
            # dependence. Therefore connect it to the the end vertex
            # of its parent. Use blue to indicate a parent child
            # relationship.
            remote_name = self.parent.dag_name + end_postfix
            graph.edge(local_name, remote_name, color="blue")
        # now deal with backward dependencies. When creating the edges
        # we reverse the direction of the dependence (place
        # remote_node before local_node) to help with the graph
        # layout
        remote_node = self.backward_dependence()
        local_name = self.dag_name
        if self.children:
            # the edge will come from my start vertex as I am a
            # backward dependence
            local_name += start_postfix
        if remote_node:
            # this node has a backward dependence.
            remote_name = remote_node.dag_name
            if remote_node.children:
                # the remote node has children so I will connect to
                # its end vertex
                remote_name += end_postfix
            # Create the backward dependence edge in red.
            graph.edge(remote_name, local_name, color="red")
        elif self.parent:
            # this node has a parent and has no backward
            # dependence. Therefore connect it to the the start vertex
            # of its parent. Use blue to indicate a parent child
            # relationship.
            remote_name = self.parent.dag_name + start_postfix
            graph.edge(remote_name, local_name, color="blue")
        # now call any children so they can add their information to
        # the graph
        for child in self.children:
            child.dag_gen(graph)

    @property
    def dag_name(self):
        '''Return the base dag name for this node.'''
        return "node_" + str(self.abs_position)

    @property
    def args(self):
        '''Return the list of arguments associated with this Node. The default
        implementation assumes the Node has no directly associated
        arguments (i.e. is not a Kern class or subclass). Arguments of
        any of this nodes descendants are considered to be
        associated. '''
        args = []
        for call in self.kernels():
            args.extend(call.args)
        return args

    def backward_dependence(self):
        '''Returns the closest preceding Node that this Node has a direct
        dependence with or None if there is not one. Only Nodes with
        the same parent as self are returned. Nodes inherit their
        descendants' dependencies. The reason for this is that for
        correctness a node must maintain its parent if it is
        moved. For example a halo exchange and a kernel call may have
        a dependence between them but it is the loop body containing
        the kernel call that the halo exchange must not move beyond
        i.e. the loop body inherits the dependencies of the routines
        within it.'''
        dependence = None
        # look through all the backward dependencies of my arguments
        for arg in self.args:
            dependent_arg = arg.backward_dependence()
            if dependent_arg:
                # this argument has a backward dependence
                node = dependent_arg.call
                # if the remote node is deeper in the tree than me
                # then find the ancestor that is at the same level of
                # the tree as me.
                while node.depth > self.depth:
                    node = node.parent
                if self.sameParent(node):
                    # The remote node (or one of its ancestors) shares
                    # the same parent as me
                    if not dependence:
                        # this is the first dependence found so keep it
                        dependence = node
                    else:
                        # we have already found a dependence
                        if dependence.position < node.position:
                            # the new dependence is closer to me than
                            # the previous dependence so keep it
                            dependence = node
        return dependence

    def forward_dependence(self):
        '''Returns the closest following Node that this Node has a direct
        dependence with or None if there is not one. Only Nodes with
        the same parent as self are returned. Nodes inherit their
        descendants' dependencies. The reason for this is that for
        correctness a node must maintain its parent if it is
        moved. For example a halo exchange and a kernel call may have
        a dependence between them but it is the loop body containing
        the kernel call that the halo exchange must not move beyond
        i.e. the loop body inherits the dependencies of the routines
        within it.'''
        dependence = None
        # look through all the forward dependencies of my arguments
        for arg in self.args:
            dependent_arg = arg.forward_dependence()
            if dependent_arg:
                # this argument has a forward dependence
                node = dependent_arg.call
                # if the remote node is deeper in the tree than me
                # then find the ancestor that is at the same level of
                # the tree as me.
                while node.depth > self.depth:
                    node = node.parent
                if self.sameParent(node):
                    # The remote node (or one of its ancestors) shares
                    # the same parent as me
                    if not dependence:
                        # this is the first dependence found so keep it
                        dependence = node
                    else:
                        if dependence.position > node.position:
                            # the new dependence is closer to me than
                            # the previous dependence so keep it
                            dependence = node
        return dependence

    def is_valid_location(self, new_node, position="before"):
        '''If this Node can be moved to the new_node
        (where position determines whether it is before of after the
        new_node) without breaking any data dependencies then return True,
        otherwise return False.

        :param new_node: Node to which this node should be moved.
        :type new_node: :py:class:`psyclone.psyGen.Node`
        :param str position: either 'before' or 'after'.

        :raises GenerationError: if new_node is not an\
                instance of :py:class:`psyclone.psyGen.Node`.
        :raises GenerationError: if position is not 'before' or 'after'.
        :raises GenerationError: if self and new_node do not have the same\
                parent.
        :raises GenerationError: self and new_node are the same Node.

        :returns: whether or not the specified location is valid for this node.
        :rtype: bool

        '''
        # First perform correctness checks
        # 1: check new_node is a Node
        if not isinstance(new_node, Node):
            raise GenerationError(
                "In the psyGen.Node.is_valid_location() method the "
                "supplied argument is not a Node, it is a '{0}'.".
                format(type(new_node).__name__))

        # 2: check position has a valid value
        valid_positions = ["before", "after"]
        if position not in valid_positions:
            raise GenerationError(
                "The position argument in the psyGenNode.is_valid_location() "
                "method must be one of {0} but found '{1}'".format(
                    valid_positions, position))

        # 3: check self and new_node have the same parent
        if not self.sameParent(new_node):
            raise GenerationError(
                "In the psyGen.Node.is_valid_location() method "
                "the node and the location do not have the same parent")

        # 4: check proposed new position is not the same as current position
        new_position = new_node.position
        if new_position < self.position and position == "after":
            new_position += 1
        elif new_position > self.position and position == "before":
            new_position -= 1

        if self.position == new_position:
            raise GenerationError(
                "In the psyGen.Node.is_valid_location() method, the "
                "node and the location are the same so this transformation "
                "would have no effect.")

        # Now determine whether the new location is valid in terms of
        # data dependencies
        # Treat forward and backward dependencies separately
        if new_position < self.position:
            # the new_node is before this node in the schedule
            prev_dep_node = self.backward_dependence()
            if not prev_dep_node:
                # There are no backward dependencies so the move is valid
                return True
            else:
                # return (is the dependent node before the new_position?)
                return prev_dep_node.position < new_position
        else:  # new_node.position > self.position
            # the new_node is after this node in the schedule
            next_dep_node = self.forward_dependence()
            if not next_dep_node:
                # There are no forward dependencies so the move is valid
                return True
            else:
                # return (is the dependent node after the new_position?)
                return next_dep_node.position > new_position

    @property
    def depth(self):
        '''
        Returns this Node's depth in the tree: 1 for the Schedule
        and increasing for its descendants at each level.
        :returns: depth of the Node in the tree
        :rtype: int
        '''
        my_depth = self.START_DEPTH
        node = self
        while node is not None:
            node = node.parent
            my_depth += 1
        return my_depth

    def view(self, indent=0, index=None):
        ''' Print out description of current node to stdout and
        then call view() on all child nodes.

        :param int indent: depth of indent for output text.
        :param int index: the position of this Node wrt its siblings or None.

        '''
        # TODO #542 remove ProfileNode and ExtractNode from this check once
        # they each have a Schedule.
        from psyclone.profiler import ProfileNode
        from psyclone.extractor import ExtractNode
        if not isinstance(self.parent, (Schedule, ProfileNode, ExtractNode)) \
           or index is None:
            print("{0}{1}".format(self.indent(indent),
                                  self.node_str(colour=True)))
        else:
            print("{0}{1}: {2}".format(self.indent(indent), index,
                                       self.node_str(colour=True)))
        for idx, entity in enumerate(self._children):
            entity.view(indent=indent + 1, index=idx)

    @staticmethod
    def indent(count, indent=INDENTATION_STRING):
        '''
        Helper function to produce indentation strings.

        :param int count: Number of indentation levels.
        :param str indent: String representing one indentation level.
        :returns: Complete indentation string.
        :rtype: str
        '''
        return count * indent

    def list(self, indent=0):
        result = ""
        for entity in self._children:
            result += str(entity)+"\n"
        return result

    def addchild(self, child, index=None):
        if index is not None:
            self._children.insert(index, child)
        else:
            self._children.append(child)

    @property
    def children(self):
        return self._children

    @children.setter
    def children(self, my_children):
        self._children = my_children

    @property
    def parent(self):
        return self._parent

    @parent.setter
    def parent(self, my_parent):
        self._parent = my_parent

    @property
    def position(self):
        '''
        Find a Node's position relative to its parent Node (starting
        with 0 if it does not have a parent).

        :returns: relative position of a Node to its parent
        :rtype: int
        '''
        if self.parent is None:
            return self.START_POSITION
        return self.parent.children.index(self)

    @property
    def abs_position(self):
        '''
        Find a Node's absolute position in the tree (starting with 0 if
        it is the root). Needs to be computed dynamically from the
        starting position (0) as its position may change.

        :returns: absolute position of a Node in the tree
        :rtype: int

        :raises InternalError: if the absolute position cannot be found
        '''
        if self.root == self and isinstance(self.root, Schedule):
            return self.START_POSITION
        found, position = self._find_position(self.root.children,
                                              self.START_POSITION)
        if not found:
            raise InternalError("Error in search for Node position "
                                "in the tree")
        return position

    def _find_position(self, children, position):
        '''
        Recurse through the tree depth first returning position of
        a Node if found.
        :param children: list of Nodes which are children of this Node
        :type children: list of :py:class:`psyclone.psyGen.Node`
        :returns: position of the Node in the tree
        :rtype: int
        :raises InternalError: if the starting position is < 0
        '''
        if position < self.START_POSITION:
            raise InternalError(
                "Search for Node position started from {0} "
                "instead of {1}.".format(position, self.START_POSITION))
        for child in children:
            position += 1
            if child == self:
                return True, position
            if child.children:
                found, position = self._find_position(child.children, position)
                if found:
                    return True, position
        return False, position

    @property
    def root(self):
        node = self
        while node.parent is not None:
            node = node.parent
        return node

    def sameRoot(self, node_2):
        if self.root == node_2.root:
            return True
        return False

    def sameParent(self, node_2):
        if self.parent is None or node_2.parent is None:
            return False
        if self.parent == node_2.parent:
            return True
        return False

    def walk(self, my_type):
        ''' Recurse through the PSyIR tree and return all objects that are
        an instance of 'my_type', which is either a single class or a tuple
        of classes. In the latter case all nodes are returned that are
        instances of any classes in the tuple.

        :param my_type: the class(es) for which the instances are collected.
        :type my_type: either a single :py:class:`psyclone.Node` class\
            or a tuple of such classes.

        :returns: list with all nodes that are instances of my_type \
            starting at and including this node.
        :rtype: list of :py:class:`psyclone.Node` instances.
        '''
        local_list = []
        if isinstance(self, my_type):
            local_list.append(self)
        for child in self.children:
            local_list += child.walk(my_type)
        return local_list

    def ancestor(self, my_type, excluding=None):
        '''
        Search back up tree and check whether we have an ancestor that is
        an instance of the supplied type. If we do then we return
        it otherwise we return None. A list of (sub-) classes to ignore
        may be provided via the `excluding` argument.

        :param type my_type: Class to search for.
        :param list excluding: list of (sub-)classes to ignore or None.
        :returns: First ancestor Node that is an instance of the requested \
                  class or None if not found.
        '''
        myparent = self.parent
        while myparent is not None:
            if isinstance(myparent, my_type):
                matched = True
                if excluding:
                    # We have one or more sub-classes we must exclude
                    for etype in excluding:
                        if isinstance(myparent, etype):
                            matched = False
                            break
                if matched:
                    return myparent
            myparent = myparent.parent
        return None

    def kernels(self):
        '''
        :returns: all kernels that are descendants of this node in the PSyIR.
        :rtype: list of :py:class:`psyclone.psyGen.Kern` sub-classes.
        '''
        return self.walk(Kern)

    def following(self):
        '''Return all :py:class:`psyclone.psyGen.Node` nodes after me in the
        schedule. Ordering is depth first.

        :returns: a list of nodes
        :rtype: :func:`list` of :py:class:`psyclone.psyGen.Node`

        '''
        all_nodes = self.root.walk(Node)
        position = all_nodes.index(self)
        return all_nodes[position+1:]

    def preceding(self, reverse=None):
        '''Return all :py:class:`psyclone.psyGen.Node` nodes before me in the
        schedule. Ordering is depth first. If the `reverse` argument
        is set to `True` then the node ordering is reversed
        i.e. returning the nodes closest to me first

        :param: reverse: An optional, default `False`, boolean flag
        :type: reverse: bool
        :returns: A list of nodes
        :rtype: :func:`list` of :py:class:`psyclone.psyGen.Node`

        '''
        all_nodes = self.root.walk(Node)
        position = all_nodes.index(self)
        nodes = all_nodes[:position]
        if reverse:
            nodes.reverse()
        return nodes

    def coded_kernels(self):
        '''
        Returns a list of all of the user-supplied kernels that are beneath
        this node in the PSyIR.

        :returns: all user-supplied kernel calls below this node.
        :rtype: list of :py:class:`psyclone.psyGen.CodedKern`
        '''
        return self.walk(CodedKern)

    def loops(self):
        '''Return all loops currently in this schedule.'''
        return self.walk(Loop)

    def reductions(self, reprod=None):
        '''Return all calls that have reductions and are decendents of this
        node. If reprod is not provided, all reductions are
        returned. If reprod is False, all builtin reductions that are
        not set to reproducible are returned. If reprod is True, all
        builtins that are set to reproducible are returned.'''

        call_reduction_list = []
        for call in self.walk(Kern):
            if call.is_reduction:
                if reprod is None:
                    call_reduction_list.append(call)
                elif reprod:
                    if call.reprod_reduction:
                        call_reduction_list.append(call)
                else:
                    if not call.reprod_reduction:
                        call_reduction_list.append(call)
        return call_reduction_list

    def is_openmp_parallel(self):
        ''':returns: True if this Node is within an OpenMP parallel region.

        '''
        omp_dir = self.ancestor(OMPParallelDirective)
        if omp_dir:
            return True
        return False

    def gen_code(self, parent):
        '''Abstract base class for code generation function.

        :param parent: the parent of this Node in the PSyIR.
        :type parent: :py:class:`psyclone.psyGen.Node`
        '''
        raise NotImplementedError("Please implement me")

    def update(self):
        ''' By default we assume there is no need to update the existing
        fparser2 AST which this Node represents. We simply call the update()
        method of any children. '''
        for child in self._children:
            child.update()

    def reference_accesses(self, var_accesses):
        '''Get all variable access information. The default implementation
        just recurses down to all children.

        :param var_accesses: Stores the output results.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`
        '''
        for child in self._children:
            child.reference_accesses(var_accesses)


class Schedule(Node):
    ''' Stores schedule information for a sequence of statements (supplied
    as a list of children).

    :param children: the sequence of PSyIR nodes that make up the Schedule.
    :type children: list of :py:class:`psyclone.psyGen.Node`
    :param parent: that parent of this node in the PSyIR tree.
    :type parent: :py:class:`psyclone.psyGen.Node`

    '''
    def __init__(self, children=None, parent=None):
        Node.__init__(self, children=children, parent=parent)
        self._text_name = "Schedule"
        self._colour_key = "Schedule"

    @property
    def dag_name(self):
        '''
        :returns: The name of this node in the dag.
        :rtype: str
        '''
        return self._text_name

    def __getitem__(self, index):
        '''
        Overload the subscript notation ([int]) to access specific statements
        in the Schedule.

        :param int index: index of the statement to access.
        :returns: statement in a given position in the Schedule sequence.
        :rtype: :py:class:`psyclone.psyGen.Node`
        '''
        return self._children[index]

    def __str__(self):
        result = "Schedule:\n"
        for entity in self._children:
            result += str(entity) + "\n"
        result += "End Schedule"
        return result

    def gen_code(self, parent):
        '''
        A Schedule does not have any direct Fortran representation. We just
        call gen_code() for all of its children.

        :param parent: node in the f2pygen AST to which to add content.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`
        '''
        for child in self.children:
            child.gen_code(parent)


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
    def __init__(self, KernFactory, BuiltInFactory, alg_calls=None):
        # we need to separate calls into loops (an iteration space really)
        # and calls so that we can perform optimisations separately on the
        # two entities.
        if alg_calls is None:
            alg_calls = []
        sequence = []
        from psyclone.parse.algorithm import BuiltInCall
        for call in alg_calls:
            if isinstance(call, BuiltInCall):
                sequence.append(BuiltInFactory.create(call, parent=self))
            else:
                sequence.append(KernFactory.create(call, parent=self))
        Schedule.__init__(self, children=sequence, parent=None)
        self._invoke = None
        self._opencl = False  # Whether or not to generate OpenCL
        # InvokeSchedule opencl_options default values
        self._opencl_options = {"end_barrier": True}
        self._name_space_manager = NameSpaceFactory().create()
        self._text_name = "InvokeSchedule"

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
        result = "InvokeSchedule:\n"
        for entity in self._children:
            result += str(entity) + "\n"
        result += "End InvokeSchedule\n"
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

        if self._opencl:
            parent.add(UseGen(parent, name="iso_c_binding"))
            parent.add(UseGen(parent, name="clfortran"))
            parent.add(UseGen(parent, name="fortcl", only=True,
                              funcnames=["get_num_cmd_queues",
                                         "get_cmd_queues",
                                         "get_kernel_by_name"]))
            # Command queues
            nqueues = self._name_space_manager.create_name(
                root_name="num_cmd_queues", context="PSyVars",
                label="num_cmd_queues")
            qlist = self._name_space_manager.create_name(
                root_name="cmd_queues", context="PSyVars", label="cmd_queues")
            first = self._name_space_manager.create_name(
                root_name="first_time", context="PSyVars", label="first_time")
            flag = self._name_space_manager.create_name(
                root_name="ierr", context="PSyVars", label="ierr")
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
                kernel = self._name_space_manager.create_name(
                    root_name=base, context="PSyVars", label=base)
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


class Directive(Node):
    '''
    Base class for all Directive statements.

    All classes that generate Directive statements (e.g. OpenMP,
    OpenACC, compiler-specific) inherit from this class.

    :param ast: the entry in the fparser2 parse tree representing the code \
                contained within this directive or None.
    :type ast: :py:class:`fparser.two.Fortran2003.Base` or NoneType
    :param children: list of PSyIR nodes that will be children of this \
                     Directive node or None.
    :type children: list of :py:class:`psyclone.psyGen.Node` or NoneType
    :param parent: PSyIR node that is the parent of this Directive or None.
    :type parent: :py:class:`psyclone.psyGen.Node` or NoneType

    '''
    # The prefix to use when constructing this directive in Fortran
    # (e.g. "OMP"). Must be set by sub-class.
    _PREFIX = ""

    def __init__(self, ast=None, children=None, parent=None):
        # A Directive always contains a Schedule
        sched = Schedule(children=children, parent=self)
        if children:
            # If we have children then set the Schedule's AST pointer to
            # point to the AST associated with them.
            sched.ast = children[0].ast
            for child in children:
                child.parent = sched
        else:
            sched.ast = ast
        super(Directive, self).__init__(ast, children=[sched], parent=parent)
        self._text_name = "Directive"
        self._colour_key = "Directive"

    @property
    def dir_body(self):
        '''
        :returns: the Schedule associated with this directive.
        :rtype: :py:class:`psyclone.psyGen.Schedule`

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
        # TODO this should be simplified/improved once
        # the fparser2 parse tree has parent information (fparser/#102).
        first_child = self.children[0][0]
        last_child = self.children[0][-1]
        content_ast = first_child.ast
        fp_parent = content_ast._parent

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
                fp_parent.content.insert(ast_end_index+1, directive)
                # Retro-fit parent information. # TODO remove/modify this once
                # fparser/#102 is done (i.e. probably supply parent info as
                # option to the Comment() constructor).
                directive._parent = fp_parent
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
        fp_parent.content.insert(ast_start_index, directive)
        # Retro-fit parent information. # TODO remove/modify this once
        # fparser/#102 is done (i.e. probably supply parent info as option
        # to the Comment() constructor).
        directive._parent = fp_parent

        self.ast = directive
        self.dir_body.ast = directive


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
    :type children: list of :py:class:`psyclone.psyGen.Node`.
    :param parent: the node in the InvokeSchedule to which to add this \
                   directive as a child.
    :type parent: :py:class:`psyclone.psyGen.Node`.
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
        :type parent: :py:class:`psyclone.psyGen.Node`
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
    :type children: list of :py:class:`psyclone.psyGen.Node`.
    :param parent: the node in the Schedule to which to add this directive.
    :type parent: :py:class:`psyclone.psyGen.Node`.
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
            name_space_manager = NameSpaceFactory().create()
            thread_idx = name_space_manager.create_name(
                root_name="th_idx", context="PSyVars", label="thread_index")
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
    :type parent: :py:class:`psyclone.psyGen.Node`
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
                             excluding=[OMPParallelDoDirective]):
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
        if len(self._children) != 1:
            raise GenerationError(
                "An OpenMP DO can only be applied to a single loop "
                "but this Node has {0} children: {1}".
                format(len(self._children), self._children))

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
        if len(self._children) != 1:
            raise GenerationError(
                "An OpenMP PARALLEL DO can only be applied to a single loop "
                "but this Node has {0} children: {1}".
                format(len(self._children), self._children))

        self._add_region(
            start_text="parallel do default(shared), private({0}), "
            "schedule({1})".format(",".join(self._get_private_list()),
                                   self._omp_schedule),
            end_text="end parallel do")


class GlobalSum(Node):
    '''
    Generic Global Sum class which can be added to and manipulated
    in, a schedule.

    :param scalar: the scalar that the global sum is stored into
    :type scalar: :py:class:`psyclone.dynamo0p3.DynKernelArgument`
    :param parent: optional parent (default None) of this object
    :type parent: :py:class:`psyclone.psyGen.node`

    '''
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
        self._text_name = "GlobalSum"
        self._colour_key = "GlobalSum"

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


class HaloExchange(Node):
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
        self._text_name = "HaloExchange"
        self._colour_key = "HaloExchange"

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


class Loop(Node):
    '''
    Node representing a loop within the PSyIR. It has 4 mandatory children:
    the first one represents the loop lower bound, the second one represents
    the loop upper bound, the third one represents the step value and the
    fourth one is always a PSyIR Schedule node containing the statements inside
    the loop body.

    (Note: currently this loop only represents the equivalent to Fortran do
    loops. This means the loop is bounded by start/stop/step expressions
    evaluated before the loop starts.)

    :param parent: parent of this node in the PSyIR.
    :type parent: sub-class of :py:class:`psyclone.psyGen.Node`
    :param str variable_name: optional name of the loop iterator \
        variable. Defaults to an empty string.
    :param valid_loop_types: a list of loop types that are specific \
        to a particular API.
    :type valid_loop_types: list of str

    '''

    def __init__(self, parent=None, variable_name="", valid_loop_types=None):
        Node.__init__(self, parent=parent)

        # we need to determine whether this is a built-in or kernel
        # call so our schedule can do the right thing.

        if valid_loop_types is None:
            self._valid_loop_types = []
        else:
            self._valid_loop_types = valid_loop_types
        self._loop_type = None        # inner, outer, colour, colours, ...
        self._field = None
        self._field_name = None       # name of the field
        self._field_space = None      # v0, v1, ...,     cu, cv, ...
        self._iteration_space = None  # cells, ...,      cu, cv, ...
        self._kern = None             # Kernel associated with this loop
        self._text_name = "Loop"
        self._colour_key = "Loop"

        # TODO replace iterates_over with iteration_space
        self._iterates_over = "unknown"

        self._variable_name = variable_name
        self._id = ""

    def _check_completeness(self):
        ''' Check that the Loop has 4 children and the 4th is a Schedule.

        :raises InternalError: If the loop does not have 4 children or the
            4th one is not a Schedule
        '''
        if len(self.children) < 4:
            raise InternalError(
                "Loop malformed or incomplete. It should have exactly 4 "
                "children, but found loop with '{0}'.".format(str(self)))

        if not isinstance(self.children[3], Schedule):
            raise InternalError(
                "Loop malformed or incomplete. Fourth child should be a "
                "Schedule node, but found loop with '{0}'.".format(str(self)))

    @property
    def start_expr(self):
        '''
        :returns: the PSyIR Node representing the Loop start expression.
        :rtype: :py:class:`psyclone.psyGen.Node`

        '''
        self._check_completeness()
        return self._children[0]

    @start_expr.setter
    def start_expr(self, expr):
        ''' Setter for Loop start_expr attribute.

        :param expr: New PSyIR start expression.
        :type expr: :py:class:`psyclone.psyGen.Node`

        :raises TypeError: if expr is not a PSyIR node.

        '''
        if not isinstance(expr, Node):
            raise TypeError(
                "Only PSyIR nodes can be assigned as the Loop start expression"
                ", but found '{0}' instead".format(type(expr)))
        self._check_completeness()
        self._children[0] = expr

    @property
    def stop_expr(self):
        '''
        :returns: the PSyIR Node representing the Loop stop expression.
        :rtype: :py:class:`psyclone.psyGen.Node`

        '''
        self._check_completeness()
        return self._children[1]

    @stop_expr.setter
    def stop_expr(self, expr):
        ''' Setter for Loop stop_expr attribute.

        :param expr: New PSyIR stop expression.
        :type expr: :py:class:`psyclone.psyGen.Node`

        :raises TypeError: if expr is not a PSyIR node.

        '''
        if not isinstance(expr, Node):
            raise TypeError(
                "Only PSyIR nodes can be assigned as the Loop stop expression"
                ", but found '{0}' instead".format(type(expr)))
        self._check_completeness()
        self._children[1] = expr

    @property
    def step_expr(self):
        '''
        :returns: the PSyIR Node representing the Loop step expression.
        :rtype: :py:class:`psyclone.psyGen.Node`

        '''
        self._check_completeness()
        return self._children[2]

    @step_expr.setter
    def step_expr(self, expr):
        ''' Setter for Loop step_expr attribute.

        :param expr: New PSyIR step expression.
        :type expr: :py:class:`psyclone.psyGen.Node`

        :raises TypeError: if expr is not a PSyIR node.

        '''
        if not isinstance(expr, Node):
            raise TypeError(
                "Only PSyIR nodes can be assigned as the Loop step expression"
                ", but found '{0}' instead".format(type(expr)))
        self._check_completeness()
        self._children[2] = expr

    @property
    def loop_body(self):
        '''
        :returns: the PSyIR Schedule with the loop body statements.
        :rtype: :py:class:`psyclone.psyGen.Schedule`

        '''
        self._check_completeness()
        return self._children[3]

    @property
    def dag_name(self):
        ''' Return the name to use in a dag for this node

        :returns: Return the dag name for this loop
        :rtype: string

        '''
        if self.loop_type:
            name = "loop_[{0}]_".format(self.loop_type) + \
                   str(self.abs_position)
        else:
            name = "loop_" + str(self.abs_position)
        return name

    @property
    def loop_type(self):
        return self._loop_type

    @loop_type.setter
    def loop_type(self, value):
        '''
        Set the type of this Loop.

        :param str value: the type of this loop.
        :raises GenerationError: if the specified value is not a recognised \
                                 loop type.
        '''
        if value not in self._valid_loop_types:
            raise GenerationError(
                "Error, loop_type value ({0}) is invalid. Must be one of "
                "{1}.".format(value, self._valid_loop_types))
        self._loop_type = value

    def node_str(self, colour=True):
        '''
        Returns the name of this node with (optional) control codes
        to generate coloured output in a terminal that supports it.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        :rtype: str
        '''
        return ("{0}[type='{1}', field_space='{2}', it_space='{3}']".
                format(colored("Loop", SCHEDULE_COLOUR_MAP["Loop"]),
                       self._loop_type, self._field_space,
                       self.iteration_space))

    @property
    def field_space(self):
        return self._field_space

    @field_space.setter
    def field_space(self, my_field_space):
        self._field_space = my_field_space

    @property
    def field_name(self):
        return self._field_name

    @property
    def field(self):
        return self._field

    @field_name.setter
    def field_name(self, my_field_name):
        self._field_name = my_field_name

    @property
    def iteration_space(self):
        return self._iteration_space

    @iteration_space.setter
    def iteration_space(self, it_space):
        self._iteration_space = it_space

    @property
    def kernel(self):
        '''
        :returns: the kernel object associated with this Loop (if any).
        :rtype: :py:class:`psyclone.psyGen.Kern`
        '''
        return self._kern

    @kernel.setter
    def kernel(self, kern):
        '''
        Setter for kernel object associated with this loop.

        :param kern: a kernel object.
        :type kern: :py:class:`psyclone.psyGen.Kern`
        '''
        self._kern = kern

    @property
    def variable_name(self):
        '''
        :returns: the name of the control variable for this loop.
        :rtype: str
        '''
        return self._variable_name

    def __str__(self):
        # Give Loop sub-classes a specialised name
        name = self.__class__.__name__
        result = name + "["
        result += "id:'" + self._id
        result += "', variable:'" + self._variable_name
        if self.loop_type:
            result += "', loop_type:'" + self._loop_type
        result += "']\n"
        for entity in self._children:
            result += str(entity) + "\n"
        result += "End " + name
        return result

    def reference_accesses(self, var_accesses):
        '''Get all variable access information. It combines the data from
        the loop bounds (start, stop and step), as well as the loop body.
        The loop variable is marked as 'READ+WRITE' and references in start,
        stop and step are marked as 'READ'.

        :param var_accesses: VariablesAccessInfo instance that stores the \
            information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`
        '''

        # It is important to first add the WRITE access, since this way
        # the dependency analysis for declaring openmp private variables
        # will automatically declare the loop variables to be private
        # (write access before read)
        var_accesses.add_access(self.variable_name, AccessType.WRITE, self)
        var_accesses.add_access(self.variable_name, AccessType.READ, self)

        # Accesses of the start/stop/step expressions
        self.start_expr.reference_accesses(var_accesses)
        self.stop_expr.reference_accesses(var_accesses)
        self.step_expr.reference_accesses(var_accesses)
        var_accesses.next_location()

        for child in self.loop_body.children:
            child.reference_accesses(var_accesses)
            var_accesses.next_location()

    def has_inc_arg(self):
        ''' Returns True if any of the Kernels called within this
        loop have an argument with INC access. Returns False otherwise '''
        for kern_call in self.coded_kernels():
            for arg in kern_call.arguments.args:
                if arg.access == AccessType.INC:
                    return True
        return False

    def unique_modified_args(self, arg_type):
        '''Return all unique arguments of the given type from kernels inside
        this loop that are modified.

        :param str arg_type: the type of kernel argument (e.g. field, \
                             operator) to search for.
        :returns: all unique arguments of the given type from kernels inside \
            this loop that are modified.
        :rtype: list of :py:class:`psyclone.psyGen.DynKernelArgument`
        '''
        arg_names = []
        args = []
        for call in self.kernels():
            for arg in call.arguments.args:
                if arg.type.lower() == arg_type:
                    if arg.access != AccessType.READ:
                        if arg.name not in arg_names:
                            arg_names.append(arg.name)
                            args.append(arg)
        return args

    def args_filter(self, arg_types=None, arg_accesses=None, unique=False):
        '''Return all arguments of type arg_types and arg_accesses. If these
        are not set then return all arguments. If unique is set to
        True then only return uniquely named arguments'''
        all_args = []
        all_arg_names = []
        for call in self.kernels():
            call_args = args_filter(call.arguments.args, arg_types,
                                    arg_accesses)
            if unique:
                for arg in call_args:
                    if arg.name not in all_arg_names:
                        all_args.append(arg)
                        all_arg_names.append(arg.name)
            else:
                all_args.extend(call_args)
        return all_args

    def gen_code(self, parent):
        '''
        Generate the Fortran Loop and any associated code.

        :param parent: the node in the f2pygen AST to which to add content.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        def is_unit_literal(expr):
            ''' Check if the given expression is equal to the literal '1'.

            :param expr: a PSyIR expression.
            :type expr: :py:class:`psyclone.psyGen.Node`

            :returns: True if it is equal to the literal '1', false otherwise.
            '''
            return isinstance(expr, Literal) and expr.value == '1'

        if not self.is_openmp_parallel():
            calls = self.reductions()
            zero_reduction_variables(calls, parent)

        if self.root.opencl or (is_unit_literal(self.start_expr) and
                                is_unit_literal(self.stop_expr)):
            # no need for a loop
            for child in self.loop_body:
                child.gen_code(parent)
        else:
            from psyclone.psyir.backend.fortran import FortranWriter
            from psyclone.f2pygen import DoGen, DeclGen
            # start/stop/step_expr are generated with the FortranWriter
            # backend, the rest of the loop with f2pygen.
            fwriter = FortranWriter()
            if is_unit_literal(self.step_expr):
                step_str = None
            else:
                step_str = fwriter(self.step_expr)

            do = DoGen(parent, self._variable_name,
                       fwriter(self.start_expr),
                       fwriter(self.stop_expr),
                       step_str)
            # need to add do loop before children as children may want to add
            # info outside of do loop
            parent.add(do)
            for child in self.loop_body:
                child.gen_code(do)
            my_decl = DeclGen(parent, datatype="integer",
                              entity_decls=[self._variable_name])
            parent.add(my_decl)


class Kern(Node):
    '''
    Base class representing a call to a sub-program unit from within the
    PSy layer. It is possible for this unit to be in-lined within the
    PSy layer.

    :param parent: parent of this node in the PSyIR.
    :type parent: sub-class of :py:class:`psyclone.psyGen.Node`
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
    def __init__(self, parent, call, name, arguments):
        Node.__init__(self, children=[], parent=parent)
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
                else:
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
        var_name = self._reduction_arg.name
        return self._name_space_manager.\
            create_name(root_name="l_"+var_name,
                        context="PSyVars",
                        label=var_name)

    def zero_reduction_variable(self, parent, position=None):
        '''
        Generate code to zero the reduction variable and to zero the local
        reduction variable if one exists. The latter is used for reproducible
        reductions, if specified.

        :param parent: the Node in the AST to which to add new code.
        :type parent: :py:class:`psyclone.psyGen.Node`
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
            zero = "0.0_r_def"
            kind_type = "r_def"
            data_type = "real"
        elif var_type == "gh_integer":
            zero = "0"
            kind_type = None
            data_type = "integer"
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
            nthreads = self._name_space_manager.create_name(
                root_name="nthreads", context="PSyVars", label="nthreads")
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
        # TODO we should initialise self._name_space_manager in the
        # constructor!
        self._name_space_manager = NameSpaceFactory().create()
        thread_idx = self._name_space_manager.create_name(
            root_name="th_idx", context="PSyVars", label="thread_index")
        nthreads = self._name_space_manager.create_name(
            root_name="nthreads", context="PSyVars", label="nthreads")
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
            idx_name = self._name_space_manager.create_name(
                root_name="th_idx",
                context="PSyVars",
                label="thread_index")
            local_name = self._name_space_manager.create_name(
                root_name="l_"+name,
                context="PSyVars",
                label=name)
            return local_name + "(1," + idx_name + ")"
        else:
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
    :type parent: sub-class of :py:class:`psyclone.psyGen.Node`.
    :param bool check: Whether or not to check that the number of arguments \
                       specified in the kernel meta-data matches the number \
                       provided by the call in the Algorithm layer.

    :raises GenerationError: if(check) and the number of arguments in the \
                             call does not match that in the meta-data.

    '''
    def __init__(self, KernelArguments, call, parent=None, check=True):
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
        self._text_name = "CodedKern"
        self._colour_key = "CodedKern"

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
        from fparser.two.utils import walk_ast

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
        dtypes = walk_ast(self.ast.content, [Fortran2003.Derived_Type_Def])
        for dtype in dtypes:
            tbound_proc = walk_ast(dtype.content,
                                   [Fortran2003.Type_Bound_Procedure_Part])
            names = walk_ast(tbound_proc[0].content, [Fortran2003.Name])
            if str(names[-1]) == self.name:
                # This is the derived type for this kernel. Now we need
                # its name...
                tnames = walk_ast(dtype.content, [Fortran2003.Type_Name])
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
        names = walk_ast(self.ast.content, [Fortran2003.Name])
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


class BuiltIn(Kern):
    '''
    Parent class for all built-ins (field operations for which the user
    does not have to provide an implementation).
    '''
    def __init__(self):
        # We cannot call Kern.__init__ as don't have necessary information
        # here. Instead we provide a load() method that can be called once
        # that information is available.
        self._arg_descriptors = None
        self._func_descriptors = None
        self._fs_descriptors = None
        self._reduction = None
        self._text_name = "BuiltIn"
        self._colour_key = "BuiltIn"

    @property
    def dag_name(self):
        ''' Return the name to use in a dag for this node'''
        return "builtin_{0}_".format(self.name) + str(self.abs_position)

    def load(self, call, arguments, parent=None):
        ''' Set-up the state of this BuiltIn call '''
        name = call.ktype.name
        super(BuiltIn, self).__init__(parent, call, name, arguments)
        self._text_name = "BuiltIn"
        self._colour_key = "BuiltIn"

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
        self._name_space_manager = NameSpaceFactory().create()

        if self._orig_name is None:
            # this is an infrastructure call literal argument. Therefore
            # we do not want an argument (_text=None) but we do want to
            # keep the value (_name)
            self._name = arg_info.text
            self._text = None
        else:
            # Use our namespace manager to create a unique name unless
            # the context and label match in which case return the
            # previous name.
            self._name = self._name_space_manager.create_name(
                root_name=self._orig_name, context="AlgArgs", label=self._text)
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

    def set_kernel_arg(self, parent, index, kname):
        '''
        Generate the code to set this argument for an OpenCL kernel.

        :param parent: the node in the Schedule to which to add the code.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`
        :param int index: the (zero-based) index of this argument in the \
                          list of kernel arguments.
        :param str kname: the name of the OpenCL kernel.
        '''
        from psyclone.f2pygen import AssignGen, CallGen
        # Look up variable names from name-space manager
        err_name = self._name_space_manager.create_name(
            root_name="ierr", context="PSyVars", label="ierr")
        kobj = self._name_space_manager.create_name(
            root_name="kernel_obj", context="ArgSetter", label="kernel_obj")
        parent.add(AssignGen(
            parent, lhs=err_name,
            rhs="clSetKernelArg({0}, {1}, C_SIZEOF({2}), C_LOC({2}))".
            format(kobj, index, self.name)))
        parent.add(CallGen(
            parent, "check_status",
            ["'clSetKernelArg: arg {0} of {1}'".format(index, kname),
             err_name]))

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
        :type: :func:`list` of :py:class:`psyclone.psyGen.Node`
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
        :type: :func:`list` of :py:class:`psyclone.psyGen.Node`
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
        :type: :func:`list` of :py:class:`psyclone.psyGen.Node`
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
        momento = None
        return schedule, momento

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


class IfBlock(Node):
    '''
    Node representing an if-block within the PSyIR. It has two mandatory
    children: the first one represents the if-condition and the second one
    the if-body; and an optional third child representing the else-body.

    :param parent: the parent of this node within the PSyIR tree.
    :type parent: :py:class:`psyclone.psyGen.Node`
    :param str annotation: Tags that provide additional information about \
        the node. The node should still be functionally correct when \
        ignoring these tags. Currently, it includes: 'was_elseif' to tag
        nested ifs originally written with the 'else if' languague syntactic \
        constructs, 'was_single_stmt' to tag ifs with a 1-statement body \
        which were originally written in a single line, and 'was_case' to \
        tag an conditional structure which was originally written with the \
        Fortran 'case' or C 'switch' syntactic constructs.
    :raises InternalError: when initialised with invalid parameters.
    '''
    valid_annotations = ('was_elseif', 'was_single_stmt', 'was_case')

    def __init__(self, parent=None, annotation=None):
        super(IfBlock, self).__init__(parent=parent)
        if annotation in IfBlock.valid_annotations:
            self._annotations.append(annotation)
        elif annotation:
            raise InternalError(
                "IfBlock with unrecognized annotation '{0}', valid annotations"
                " are: {1}.".format(annotation, IfBlock.valid_annotations))
        self._text_name = "If"
        self._colour_key = "If"

    @property
    def condition(self):
        ''' Return the PSyIR Node representing the conditional expression
        of this IfBlock.

        :returns: IfBlock conditional expression.
        :rtype: :py:class:`psyclone.psyGen.Node`
        :raises InternalError: If the IfBlock node does not have the correct \
            number of children.
        '''
        if len(self.children) < 2:
            raise InternalError(
                "IfBlock malformed or incomplete. It should have at least 2 "
                "children, but found {0}.".format(len(self.children)))
        return self._children[0]

    @property
    def if_body(self):
        ''' Return the Schedule executed when the IfBlock evaluates to True.

        :returns: Schedule to be executed when IfBlock evaluates to True.
        :rtype: :py:class:`psyclone.psyGen.Schedule`
        :raises InternalError: If the IfBlock node does not have the correct \
            number of children.
        '''

        if len(self.children) < 2:
            raise InternalError(
                "IfBlock malformed or incomplete. It should have at least 2 "
                "children, but found {0}.".format(len(self.children)))

        return self._children[1]

    @property
    def else_body(self):
        ''' If available return the Schedule executed when the IfBlock
        evaluates to False, otherwise return None.

        :returns: Schedule to be executed when IfBlock evaluates \
            to False, if it doesn't exist returns None.
        :rtype: :py:class:`psyclone.psyGen.Schedule` or NoneType
        '''
        if len(self._children) == 3:
            return self._children[2]
        return None

    def node_str(self, colour=True):
        ''' Returns the name of this node with (optional) control codes
        to generate coloured output in a terminal that supports it.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        :rtype: str
        '''
        text = self.coloured_name(colour) + "["
        if self.annotations:
            text += "annotations='" + ','.join(self.annotations) + "'"
        text += "]"
        return text

    def __str__(self):
        result = "If[]\n"
        for entity in self._children:
            result += str(entity)
        return result

    def reference_accesses(self, var_accesses):
        '''Get all variable access information. It combines the data from
        the condition, if-body and (if available) else-body. This could
        later be extended to handle cases where a variable is only written
        in one of the two branches.

        :param var_accesses: VariablesAccessInfo instance that stores the \
            information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`
        '''

        # The first child is the if condition - all variables are read-only
        self.condition.reference_accesses(var_accesses)
        var_accesses.next_location()
        self.if_body.reference_accesses(var_accesses)
        var_accesses.next_location()

        if self.else_body:
            self.else_body.reference_accesses(var_accesses)
            var_accesses.next_location()


class ACCKernelsDirective(ACCDirective):
    '''
    Class representing the !$ACC KERNELS directive in the PSyIR.

    :param children: the PSyIR nodes to be enclosed in the Kernels region \
                     and which are therefore children of this node.
    :type children: list of sub-classes of :py:class:`psyclone.psyGen.Node`
    :param parent: the parent of this node in the PSyIR.
    :type parent: sub-class of :py:class:`psyclone.psyGen.Node`
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


@six.add_metaclass(abc.ABCMeta)
class SymbolInterface(object):
    '''
    Abstract base class for capturing the access mechanism for symbols that
    represent data that exists outside the section of code being represented
    in the PSyIR.

    :param access: How the symbol is accessed within the section of code or \
                   None (if unknown).
    :type access: :py:class:`psyclone.psyGen.SymbolAccess`
    '''
    def __init__(self, access=None):
        self._access = None
        # Use the setter as that has error checking
        if not access:
            self.access = Symbol.Access.UNKNOWN
        else:
            self.access = access

    @property
    def access(self):
        '''
        :returns: the access-type for this symbol.
        :rtype: :py:class:`psyclone.psyGen.Symbol.Access`
        '''
        return self._access

    @access.setter
    def access(self, value):
        '''
        Setter for the access type of this symbol.

        :param value: the new access type.
        :type value: :py:class:`psyclon.psyGen.SymbolAccess`

        :raises TypeError: if the supplied value is not of the correct type.
        '''
        if not isinstance(value, Symbol.Access):
            raise TypeError("SymbolInterface.access must be a 'Symbol.Access' "
                            "but got '{0}'.".format(type(value)))
        self._access = value


class Symbol(object):
    '''
    Symbol item for the Symbol Table. It contains information about: the name,
    the datatype, the shape (in column-major order) and, for a symbol
    representing data that exists outside of the local scope, the interface
    to that symbol (i.e. the mechanism by which it is accessed).

    :param str name: Name of the symbol.
    :param str datatype: Data type of the symbol. (One of \
                     :py:attr:`psyclone.psyGen.Symbol.valid_data_types`.)
    :param list shape: Shape of the symbol in column-major order (leftmost \
                       index is contiguous in memory). Each entry represents \
                       an array dimension. If it is 'None' the extent of that \
                       dimension is unknown, otherwise it holds an integer \
                       literal or a reference to an integer symbol with the \
                       extent. If it is an empty list then the symbol \
                       represents a scalar.
    :param interface: Object describing the interface to this symbol (i.e. \
                      whether it is passed as a routine argument or accessed \
                      in some other way) or None if the symbol is local.
    :type interface: :py:class:`psyclone.psyGen.SymbolInterface` or NoneType.
    :param constant_value: Sets a fixed known value for this \
                           Symbol. If the value is None (the default) \
                           then this symbol is not a constant. The \
                           datatype of the constant value must be \
                           compatible with the datatype of the symbol.
    :type constant_value: int, str or bool

    :raises NotImplementedError: Provided parameters are not supported yet.
    :raises TypeError: Provided parameters have invalid error type.
    :raises ValueError: Provided parameters contain invalid values.

    '''
    ## Tuple with the valid datatypes.
    valid_data_types = ('real',  # Floating point
                        'integer',
                        'character',
                        'boolean',
                        'deferred')  # Type of this symbol not yet determined
    ## Mapping from supported data types for constant values to
    #  internal Python types
    mapping = {'integer': int, 'character': str, 'boolean': bool}

    class Access(Enum):
        '''
        Enumeration for the different types of access that a Symbol is
        permitted to have.

        '''
        ## The symbol is only ever read within the current scoping block.
        READ = 1
        ## The first access of the symbol in the scoping block is a write and
        # therefore any value that it may have had upon entry is discarded.
        WRITE = 2
        ## The first access of the symbol in the scoping block is a read but
        # it is subsequently written to.
        READWRITE = 3
        ## The way in which the symbol is accessed in the scoping block is
        # unknown
        UNKNOWN = 4

    class Argument(SymbolInterface):
        '''
        Captures the interface to a symbol that is accessed as a routine
        argument.

        :param access: how the symbol is accessed within the local scope.
        :type access: :py:class:`psyclone.psyGen.Symbol.Access`
        '''
        def __init__(self, access=None):
            super(Symbol.Argument, self).__init__(access=access)
            self._pass_by_value = False

        def __str__(self):
            return "Argument(pass-by-value={0})".format(self._pass_by_value)

    class FortranGlobal(SymbolInterface):
        '''
        Describes the interface to a Fortran Symbol representing data that
        is supplied as some sort of global variable. Currently only supports
        data accessed via a module 'USE' statement.

        :param str module_use: the name of the Fortran module from which the \
                               symbol is imported.
        :param access: the manner in which the Symbol is accessed in the \
                       associated code section. If None is supplied then the \
                       access is Symbol.Access.UNKNOWN.
        :type access: :py:class:`psyclone.psyGen.Symbol.Access` or None.
        '''
        def __init__(self, module_use, access=None):
            self._module_name = ""
            super(Symbol.FortranGlobal, self).__init__(access=access)
            self.module_name = module_use

        def __str__(self):
            return "FortranModule({0})".format(self.module_name)

        @property
        def module_name(self):
            '''
            :returns: the name of the Fortran module from which the symbol is \
                      imported or None if it is not a module variable.
            :rtype: str or None
            '''
            return self._module_name

        @module_name.setter
        def module_name(self, value):
            '''
            Setter for the name of the Fortran module from which this symbol
            is imported.

            :param str value: the name of the Fortran module.

            :raises TypeError: if the supplied value is not a str.
            :raises ValueError: if the supplied string is not at least one \
                                character long.
            '''
            if not isinstance(value, str):
                raise TypeError("module_name must be a str but got '{0}'".
                                format(type(value)))
            if not value:
                raise ValueError("module_name must be one or more characters "
                                 "long")
            self._module_name = value

    def __init__(self, name, datatype, shape=None, constant_value=None,
                 interface=None):

        self._name = name

        if datatype not in Symbol.valid_data_types:
            raise NotImplementedError(
                "Symbol can only be initialised with {0} datatypes but found "
                "'{1}'.".format(str(Symbol.valid_data_types), datatype))
        self._datatype = datatype

        if shape is None:
            shape = []
        elif not isinstance(shape, list):
            raise TypeError("Symbol shape attribute must be a list.")

        for dimension in shape:
            if isinstance(dimension, Symbol):
                if dimension.datatype != "integer" or dimension.shape:
                    raise TypeError(
                        "Symbols that are part of another symbol shape can "
                        "only be scalar integers, but found '{0}'."
                        "".format(str(dimension)))
            elif not isinstance(dimension, (type(None), int)):
                raise TypeError("Symbol shape list elements can only be "
                                "'Symbol', 'integer' or 'None'.")
        self._shape = shape
        # The following attributes have setter methods (with error checking)
        self._constant_value = None
        self._interface = None
        # If an interface is specified for this symbol then the data with
        # which it is associated must exist outside of this kernel.
        self.interface = interface
        self.constant_value = constant_value

    @property
    def name(self):
        '''
        :returns: Name of the Symbol.
        :rtype: str
        '''
        return self._name

    @property
    def datatype(self):
        '''
        :returns: Datatype of the Symbol.
        :rtype: str
        '''
        return self._datatype

    @property
    def access(self):
        '''
        :returns: How this symbol is accessed (read, readwrite etc.) within \
                  the local scope.
        :rtype: :py:class:`psyclone.psyGen.Symbol.Access` or NoneType.
        '''
        if self._interface:
            return self._interface.access
        # This symbol has no interface info and therefore is local
        return None

    @property
    def shape(self):
        '''
        :returns: Shape of the symbol in column-major order (leftmost \
                  index is contiguous in memory). Each entry represents \
                  an array dimension. If it is 'None' the extent of that \
                  dimension is unknown, otherwise it holds an integer \
                  literal or a reference to an integer symbol with the \
                  extent. If it is an empty list then the symbol \
                  represents a scalar.
        :rtype: list
        '''
        return self._shape

    @property
    def scope(self):
        '''
        :returns: Whether the symbol is 'local' (just exists inside the \
                  kernel scope) or 'global' (data also lives outside the \
                  kernel). Global-scoped symbols must have an associated \
                  'interface' that specifies the mechanism by which the \
                  kernel accesses the associated data.
        :rtype: str
        '''
        if self._interface:
            return "global"
        return "local"

    @property
    def interface(self):
        '''
        :returns: the an object describing the external interface to \
                  this Symbol or None (if it is local).
        :rtype: Sub-class of :py:class:`psyclone.psyGen.SymbolInterface` or \
                NoneType.
        '''
        return self._interface

    @interface.setter
    def interface(self, value):
        '''
        Setter for the Interface associated with this Symbol.

        :param value: an Interface object describing how the Symbol is \
                      accessed by the code or None if it is local.
        :type value: Sub-class of :py:class:`psyclone.psyGen.SymbolInterface` \
                     or NoneType.

        :raises TypeError: if the supplied `value` is of the wrong type.
        '''
        if value is not None and not isinstance(value, SymbolInterface):
            raise TypeError("The interface to a Symbol must be a "
                            "SymbolInterface or None but got '{0}'".
                            format(type(value)))
        self._interface = value

    @property
    def is_constant(self):
        '''
        :returns: Whether the symbol is a constant with a fixed known \
        value (True) or not (False).
        :rtype: bool

        '''
        return self._constant_value is not None

    @property
    def is_scalar(self):
        '''
        :returns: True if this symbol is a scalar and False otherwise.
        :rtype: bool

        '''
        # If the shape variable is an empty list then this symbol is a
        # scalar.
        return self.shape == []

    @property
    def is_array(self):
        '''
        :returns: True if this symbol is an array and False otherwise.
        :rtype: bool

        '''
        # The assumption in this method is that if this symbol is not
        # a scalar then it is an array. If this assumption becomes
        # invalid then this logic will need to be changed
        # appropriately.
        return not self.is_scalar

    @property
    def constant_value(self):
        '''
        :returns: The fixed known value for this symbol if one has \
        been set or None if not.
        :rtype: int, str, bool or NoneType

        '''
        return self._constant_value

    @constant_value.setter
    def constant_value(self, new_value):
        '''
        :param constant_value: Set or change the fixed known value of \
        the constant for this Symbol. If the value is None then this \
        symbol does not have a fixed constant. The datatype of \
        new_value must be compatible with the datatype of the symbol.
        :type constant_value: int, str or bool

        :raises ValueError: If a non-None value is provided and 1) \
        this Symbol instance does not have local scope, or 2) this \
        Symbol instance is not a scalar (as the shape attribute is not \
        empty), or 3) a constant value is provided but the type of the \
        value does not support this, or 4) the type of the value \
        provided is not compatible with the datatype of this Symbol \
        instance.

        '''
        if new_value is not None:
            if self.scope != "local":
                raise ValueError(
                    "Symbol with a constant value is currently limited to "
                    "having local scope but found '{0}'.".format(self.scope))
            if self.is_array:
                raise ValueError(
                    "Symbol with a constant value must be a scalar but the "
                    "shape attribute is not empty.")
            try:
                lookup = Symbol.mapping[self.datatype]
            except KeyError:
                raise ValueError(
                    "A constant value is not currently supported for "
                    "datatype '{0}'.".format(self.datatype))
            if not isinstance(new_value, lookup):
                raise ValueError(
                    "This Symbol instance's datatype is '{0}' which means "
                    "the constant value is expected to be '{1}' but found "
                    "'{2}'.".format(self.datatype,
                                    Symbol.mapping[self.datatype],
                                    type(new_value)))
        self._constant_value = new_value

    def __str__(self):
        ret = self.name + ": <" + self.datatype + ", "
        if self.is_array:
            ret += "Array["
            for dimension in self.shape:
                if isinstance(dimension, Symbol):
                    ret += dimension.name
                elif isinstance(dimension, int):
                    ret += str(dimension)
                elif dimension is None:
                    ret += "'Unknown bound'"
                else:
                    raise InternalError(
                        "Symbol shape list elements can only be 'Symbol', "
                        "'integer' or 'None', but found '{0}'."
                        "".format(type(dimension)))
                ret += ", "
            ret = ret[:-2] + "]"  # Deletes last ", " and adds "]"
        else:
            ret += "Scalar"
        if self.interface:
            ret += ", global=" + str(self.interface)
        else:
            ret += ", local"
        if self.is_constant:
            ret += ", constant_value={0}".format(self.constant_value)
        return ret + ">"

    def copy(self):
        '''Create and return a copy of this object. Any references to the
        original will not be affected so the copy will not be referred
        to by any other object.

        :returns: A symbol object with the same properties as this \
                  symbol object.
        :rtype: :py:class:`psyclone.psyGen.Symbol`

        '''
        return Symbol(self.name, self.datatype, shape=self.shape[:],
                      constant_value=self.constant_value,
                      interface=self.interface)

    def copy_properties(self, symbol_in):
        '''Replace all properties in this object with the properties from
        symbol_in, apart from the name which is immutable.

        :param symbol_in: The symbol from which the properties are \
                          copied from.
        :type symbol_in: :py:class:`psyclone.psyGen.Symbol`

        :raises TypeError: If the argument is not the expected type.

        '''
        if not isinstance(symbol_in, Symbol):
            raise TypeError("Argument should be of type 'Symbol' but found "
                            "'{0}'.".format(type(symbol_in).__name__))

        self._datatype = symbol_in.datatype
        self._shape = symbol_in.shape[:]
        self._constant_value = symbol_in.constant_value
        self._interface = symbol_in.interface


class SymbolTable(object):
    '''
    Encapsulates the symbol table and provides methods to add new symbols
    and look up existing symbols. It is implemented as a single scope
    symbol table (nested scopes not supported).

    :param kernel: Reference to the KernelSchedule to which this symbol table \
        belongs.
    :type kernel: :py:class:`psyclone.psyGen.KernelSchedule` or NoneType
    '''
    # TODO: (Issue #321) Explore how the SymbolTable overlaps with the
    # NameSpace class functionality.
    def __init__(self, kernel=None):
        # Dict of Symbol objects with the symbol names as keys. Make
        # this ordered so that different versions of Python always
        # produce code with declarations in the same order.
        self._symbols = OrderedDict()
        # Ordered list of the arguments.
        self._argument_list = []
        # Reference to KernelSchedule to which this symbol table belongs.
        self._kernel = kernel

    def add(self, new_symbol):
        '''Add a new symbol to the symbol table.

        :param new_symbol: The symbol to add to the symbol table.
        :type new_symbol: :py:class:`psyclone.psyGen.Symbol`

        :raises KeyError: If the symbol name is already in use.

        '''
        if new_symbol.name in self._symbols:
            raise KeyError("Symbol table already contains a symbol with"
                           " name '{0}'.".format(new_symbol.name))
        self._symbols[new_symbol.name] = new_symbol

    def swap_symbol_properties(self, symbol1, symbol2):
        '''Swaps the properties of symbol1 and symbol2 apart from the symbol
        name. Argument list positions are also updated appropriately.

        :param symbol1: The first symbol.
        :type symbol1: :py:class:`psyclone.psyGen.Symbol`
        :param symbol2: The second symbol.
        :type symbol2: :py:class:`psyclone.psyGen.Symbol`

        :raises KeyError: If either of the supplied symbols are not in \
                          the symbol table.
        :raises TypeError: If the supplied arguments are not symbols, \
                 or the names of the symbols are the same in the SymbolTable \
                 instance.

        '''
        for symbol in [symbol1, symbol2]:
            if not isinstance(symbol, Symbol):
                raise TypeError("Arguments should be of type 'Symbol' but "
                                "found '{0}'.".format(type(symbol).__name__))
            if symbol.name not in self._symbols:
                raise KeyError("Symbol '{0}' is not in the symbol table."
                               "".format(symbol.name))
        if symbol1.name == symbol2.name:
            raise ValueError("The symbols should have different names, but "
                             "found '{0}' for both.".format(symbol1.name))

        tmp_symbol = symbol1.copy()
        symbol1.copy_properties(symbol2)
        symbol2.copy_properties(tmp_symbol)

        # Update argument list if necessary
        index1 = None
        if symbol1 in self._argument_list:
            index1 = self._argument_list.index(symbol1)
        index2 = None
        if symbol2 in self._argument_list:
            index2 = self._argument_list.index(symbol2)
        if index1 is not None:
            self._argument_list[index1] = symbol2
        if index2 is not None:
            self._argument_list[index2] = symbol1

    def specify_argument_list(self, argument_symbols):
        '''
        Sets-up the internal list storing the order of the arguments to this
        kernel.

        :param list argument_symbols: Ordered list of the Symbols representing\
                                      the kernel arguments.

        :raises ValueError: If the new argument_list is not consistent with \
                            the existing entries in the SymbolTable.

        '''
        self._validate_arg_list(argument_symbols)
        self._argument_list = argument_symbols[:]

    def lookup(self, name):
        '''
        Look up a symbol in the symbol table.

        :param str name: Name of the symbol
        :raises KeyError: If the given name is not in the Symbol Table.

        '''
        try:
            return self._symbols[name]
        except KeyError:
            raise KeyError("Could not find '{0}' in the Symbol Table."
                           "".format(name))

    def __contains__(self, key):
        '''Check if the given key is part of the Symbol Table.

        :param str key: key to check for existance.
        :returns: Whether the Symbol Table contains the given key.
        :rtype: bool
        '''
        return key in self._symbols

    @property
    def argument_list(self):
        '''
        Checks that the contents of the SymbolTable are self-consistent
        and then returns the list of kernel arguments.

        :returns: Ordered list of arguments.
        :rtype: list of :py:class:`psyclone.psyGen.Symbol`

        :raises InternalError: If the entries of the SymbolTable are not \
                               self-consistent.

        '''
        try:
            self._validate_arg_list(self._argument_list)
            self._validate_non_args()
        except ValueError as err:
            # If the SymbolTable is inconsistent at this point then
            # we have an InternalError.
            raise InternalError(str(err.args))
        return self._argument_list

    @staticmethod
    def _validate_arg_list(arg_list):
        '''
        Checks that the supplied list of Symbols are valid kernel arguments.

        :param arg_list: the proposed kernel arguments.
        :type param_list: list of :py:class:`psyclone.psyGen.Symbol`

        :raises TypeError: if any item in the supplied list is not a Symbol.
        :raises ValueError: if any of the symbols has no Interface.
        :raises ValueError: if any of the symbols has an Interface that is \
                            not a :py:class:`psyclone.psyGen.Symbol.Argument`.

        '''
        for symbol in arg_list:
            if not isinstance(symbol, Symbol):
                raise TypeError("Expected a list of Symbols but found an "
                                "object of type '{0}'.".format(type(symbol)))
            # All symbols in the argument list must have a
            # 'Symbol.Argument' interface
            if symbol.scope == 'local':
                raise ValueError(
                    "Symbol '{0}' is listed as a kernel argument but has "
                    "no associated Interface.".format(str(symbol)))
            if not isinstance(symbol.interface, Symbol.Argument):
                raise ValueError(
                    "Symbol '{0}' is listed as a kernel argument but has "
                    "an interface of type '{1}' rather than "
                    "Symbol.Argument".format(str(symbol),
                                             type(symbol.interface)))

    def _validate_non_args(self):
        '''
        Performs internal consistency checks on the current entries in the
        SymbolTable that do not represent kernel arguments.

        :raises ValueError: If a symbol that is not in the argument list \
                            has a Symbol.Argument interface.

        '''
        for symbol in self._symbols.values():
            if symbol not in self._argument_list:
                # Symbols not in the argument list must not have a
                # Symbol.Argument interface
                if symbol.interface and isinstance(symbol.interface,
                                                   Symbol.Argument):
                    raise ValueError(
                        "Symbol '{0}' is not listed as a kernel argument and "
                        "yet has a Symbol.Argument interface.".format(
                            str(symbol)))

    @property
    def symbols(self):
        '''
        :returns:  List of symbols.
        :rtype: list of :py:class:`psyclone.psyGen.Symbol`
        '''
        return list(self._symbols.values())

    @property
    def local_symbols(self):
        '''
        :returns:  List of local symbols.
        :rtype: list of :py:class:`psyclone.psyGen.Symbol`
        '''
        return [sym for sym in self._symbols.values() if sym.scope == "local"]

    @property
    def global_symbols(self):
        '''
        :returns: list of symbols that are not routine arguments but \
                  still have 'global' scope - i.e. are associated with \
                  data that exists outside the current scope.
        :rtype: list of :py:class:`psyclone.psyGen.Symbol`

        '''
        return [sym for sym in self._symbols.values() if sym.scope == "global"
                and not isinstance(sym.interface, Symbol.Argument)]

    @property
    def iteration_indices(self):
        '''
        :returns: List of symbols representing kernel iteration indices.
        :rtype: list of :py:class:`psyclone.psyGen.Symbol`

        :raises NotImplementedError: this method is abstract.
        '''
        raise NotImplementedError(
            "Abstract property. Which symbols are iteration indices is"
            " API-specific.")

    @property
    def data_arguments(self):
        '''
        :returns: List of symbols representing kernel data arguments.
        :rtype: list of :py:class:`psyclone.psyGen.Symbol`

        :raises NotImplementedError: this method is abstract.
        '''
        raise NotImplementedError(
            "Abstract property. Which symbols are data arguments is"
            " API-specific.")

    def view(self):
        '''
        Print a representation of this Symbol Table to stdout.
        '''
        print(str(self))

    def __str__(self):
        return ("Symbol Table:\n" +
                "\n".join(map(str, self._symbols.values())) +
                "\n")


class KernelSchedule(Schedule):
    '''
    A kernelSchedule inherits the functionality from Schedule and adds a symbol
    table to keep a record of the declared variables and their attributes.

    :param str name: Kernel subroutine name

    '''
    def __init__(self, name):
        super(KernelSchedule, self).__init__(children=None, parent=None)
        self._name = name
        self._symbol_table = SymbolTable(self)

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

    @property
    def symbol_table(self):
        '''
        :returns: Table containing symbol information for the kernel.
        :rtype: :py:class:`psyclone.psyGen.SymbolTable`
        '''
        return self._symbol_table

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


class CodeBlock(Node):
    '''Node representing some generic Fortran code that PSyclone does not
    attempt to manipulate. As such it is a leaf in the PSyIR and therefore
    has no children.

    :param fp2_nodes: list of fparser2 AST nodes representing the Fortran \
                      code constituting the code block.
    :type fp2_nodes: list of :py:class:`fparser.two.utils.Base`
    :param structure: argument indicating whether this code block is a \
    statement or an expression.
    :type structure: :py:class:`psyclone.psyGen.CodeBlock.Structure`
    :param parent: the parent node of this code block in the PSyIR.
    :type parent: :py:class:`psyclone.psyGen.Node`

    '''
    class Structure(Enum):
        '''
        Enumeration that captures the structure of the code block which
        may be required when processing.

        '''
        # The Code Block comprises one or more Fortran statements
        # (which themselves may contain expressions).
        STATEMENT = 1
        # The Code Block comprises one or more Fortran expressions.
        EXPRESSION = 2

    def __init__(self, fp2_nodes, structure, parent=None):
        super(CodeBlock, self).__init__(parent=parent)
        # Store a list of the parser objects holding the code associated
        # with this block. We make a copy of the contents of the list because
        # the list itself is a temporary product of the process of converting
        # from the fparser2 AST to the PSyIR.
        self._fp2_nodes = fp2_nodes[:]
        # Store references back into the fparser2 AST
        if fp2_nodes:
            self.ast = self._fp2_nodes[0]
            self.ast_end = self._fp2_nodes[-1]
        else:
            self.ast = None
            self.ast_end = None
        # Store the structure of the code block.
        self._structure = structure

    @property
    def structure(self):
        '''
        :returns: whether this code block is a statement or an expression.
        :rtype: :py:class:`psyclone.psyGen.CodeBlock.Structure`

        '''
        return self._structure

    @property
    def get_ast_nodes(self):
        '''
        :returns: the list of nodes associated with this code block in \
        the original AST.
        :rtype: list of subclass of \
        `:py:classfparser.two.Fortran2003.Base`

        '''
        return self._fp2_nodes

    def node_str(self, colour=True):
        ''' Create a text description of this node in the schedule, optionally
        including control codes for colour.

        :param bool colour: whether or not to include control codes for colour.

        :return: text description of this node.
        :rtype: str
        '''
        return self.coloured_name(colour) + \
            "[" + str(list(map(type, self._fp2_nodes))) + "]"

    def __str__(self):
        return "CodeBlock[{0} nodes]".format(len(self._fp2_nodes))


class Assignment(Node):
    '''
    Node representing an Assignment statement. As such it has a LHS and RHS
    as children 0 and 1 respectively.

    :param ast: node in the fparser2 AST representing the assignment.
    :type ast: :py:class:`fparser.two.Fortran2003.Assignment_Stmt.
    :param parent: the parent node of this Assignment in the PSyIR.
    :type parent: :py:class:`psyclone.psyGen.Node`
    '''
    def __init__(self, ast=None, parent=None):
        super(Assignment, self).__init__(ast=ast, parent=parent)

    @property
    def lhs(self):
        '''
        :returns: the child node representing the Left-Hand Side of the \
            assignment.
        :rtype: :py:class:`psyclone.psyGen.Node`

        :raises InternalError: Node has fewer children than expected.
        '''
        if not self._children:
            raise InternalError(
                "Assignment '{0}' malformed or incomplete. It "
                "needs at least 1 child to have a lhs.".format(repr(self)))

        return self._children[0]

    @property
    def rhs(self):
        '''
        :returns: the child node representing the Right-Hand Side of the \
            assignment.
        :rtype: :py:class:`psyclone.psyGen.Node`

        :raises InternalError: Node has fewer children than expected.
        '''
        if len(self._children) < 2:
            raise InternalError(
                "Assignment '{0}' malformed or incomplete. It "
                "needs at least 2 children to have a rhs.".format(repr(self)))

        return self._children[1]

    def __str__(self):
        result = "Assignment[]\n"
        for entity in self._children:
            result += str(entity)
        return result

    def reference_accesses(self, var_accesses):
        '''Get all variable access information from this node. The assigned-to
        variable will be set to 'WRITE'.

        :param var_accesses: VariablesAccessInfo instance that stores the \
            information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`
        '''

        # It is important that a new instance is used to handle the LHS,
        # since a check in 'change_read_to_write' makes sure that there
        # is only one access to the variable!
        accesses_left = VariablesAccessInfo()
        self.lhs.reference_accesses(accesses_left)

        # Now change the (one) access to the assigned variable to be WRITE:
        if isinstance(self.lhs, CodeBlock):
            # TODO #363: Assignment to user defined type, not supported yet.
            # Here an absolute hack to get at least some information out
            # from the AST - though indices are just strings, which will
            # likely cause problems later as well.
            name = str(self.lhs.ast)
            # A regular expression that tries to find the last parenthesis
            # pair in the name ("a(i,j)" --> "(i,j)")
            ind = re.search(r"\([^\(]+\)$", name)
            if ind:
                # Remove the index part of the name
                name = name.replace(ind.group(0), "")
                # The index must be added as a list
                accesses_left.add_access(name, AccessType.WRITE, self,
                                         [ind.group(0)])
            else:
                accesses_left.add_access(name, AccessType.WRITE, self)
        else:
            var_info = accesses_left[self.lhs.name]
            try:
                var_info.change_read_to_write()
            except InternalError:
                # An internal error typically indicates that the same variable
                # is used twice on the LHS, e.g.: g(g(1)) = ... This is not
                # supported in PSyclone.
                from psyclone.parse.utils import ParseError
                raise ParseError("The variable '{0}' appears more than once "
                                 "on the left-hand side of an assignment."
                                 .format(self.lhs.name))

        # Merge the data (that shows now WRITE for the variable) with the
        # parameter to this function. It is important that first the
        # RHS is added, so that in statements like 'a=a+1' the read on
        # the RHS comes before the write on the LHS (they have the same
        # location otherwise, but the order is still important)
        self.rhs.reference_accesses(var_accesses)
        var_accesses.merge(accesses_left)
        var_accesses.next_location()


class Reference(Node):
    '''
    Node representing a Reference Expression.

    :param ast: node in the fparser2 AST representing the reference.
    :type ast: :py:class:`fparser.two.Fortran2003.Name.
    :param parent: the parent node of this Reference in the PSyIR.
    :type parent: :py:class:`psyclone.psyGen.Node`
    '''
    def __init__(self, reference_name, parent):
        super(Reference, self).__init__(parent=parent)
        self._reference = reference_name

    @property
    def name(self):
        ''' Return the name of the referenced symbol.

        :returns: Name of the referenced symbol.
        :rtype: str
        '''
        return self._reference

    def node_str(self, colour=True):
        ''' Create a text description of this node in the schedule, optionally
        including control codes for colour.

        :param bool colour: whether or not to include colour control codes.

        :return: text description of this node.
        :rtype: str
        '''
        return self.coloured_name(colour) + "[name:'" + self._reference + "']"

    def __str__(self):
        return self.node_str(False)

    def math_equal(self, other):
        ''':param other: the node to compare self with.
        :type other: py:class:`psyclone.psyGen.Node`

        :returns: True if the self has the same results as other.
        :rtype: bool
        '''
        if not super(Reference, self).math_equal(other):
            return False
        return self.name == other.name

    def reference_accesses(self, var_accesses):
        '''Get all variable access information from this node, i.e.
        it sets this variable to be read.

        :param var_accesses: VariablesAccessInfo instance that stores the \
            information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`
        '''
        var_accesses.add_access(self._reference, AccessType.READ, self)

    def check_declared(self):
        '''Check whether this reference has an associated symbol table entry.

        raises SymbolError: if one or more ancestor symbol table(s) \
        are found and the name of this reference is not found in any \
        of them.

        '''
        found_symbol_table = False
        test_node = self.parent
        while test_node:
            if hasattr(test_node, 'symbol_table'):
                found_symbol_table = True
                symbol_table = test_node.symbol_table
                if self.name in symbol_table:
                    return
            test_node = test_node.parent

        # TODO: remove this if test, remove the initialisation of the
        # found_symbol_table boolean variable and update the doc
        # string when SymbolTables are suppported in the NEMO API, see
        # issue #500. After this change has been made this method could
        # make use of the symbol method to determine
        # whether the reference has been declared (or not).
        if found_symbol_table:
            raise SymbolError(
                "Undeclared reference '{0}' found.".format(self.name))

    def symbol(self, scope_limit=None):
        '''Returns the symbol from a symbol table associated with this
        reference or None is it is not found. The scope_limit variable
        limits the symbol table search to nodes within the scope.

        :param scope_limit: optional Node which limits the symbol \
        search space to the symbol tables of the nodes within the \
        given scope. If it is None (the default), the whole scope (all \
        symbol tables in ancestor nodes) is searched.
        :type scope_limit: :py:class:`psyclone.psyGen.Node` or `None`

        :returns: the Symbol associated with this reference if one is \
        found or None if not.
        :rtype: :py:class:`psyclone.psyGen.Symbol` or `None`

        '''
        if scope_limit:

            if not isinstance(scope_limit, Node):
                raise TypeError(
                    "The scope_limit argument '{0}' provided to the symbol "
                    "method, is not of type `Node`."
                    "".format(str(scope_limit), str(self)))

            # Check that the scope_limit Node is an ancestor of this
            # Reference Node and raise an exception if not.
            found = False
            mynode = self.parent
            while mynode is not None:
                if mynode is scope_limit:
                    found = True
                    break
                mynode = mynode.parent
            if not found:
                # The scope_limit node is not an ancestor of this reference
                # so raise an exception.
                raise ValueError(
                    "The scope_limit node '{0}' provided to the symbol "
                    "method, is not an ancestor of this reference node '{1}'."
                    "".format(str(scope_limit), str(self)))
        test_node = self.parent
        # Iterate over ancestor Nodes of this Reference Node.
        while test_node:
            # For simplicity, test every Node for the existence of a
            # SymbolTable (rather than checking for the particular
            # Node types which we know to have SymbolTables).
            if hasattr(test_node, 'symbol_table'):
                # This Node does have a SymbolTable.
                symbol_table = test_node.symbol_table
                try:
                    # If the reference matches a Symbol in this
                    # SymbolTable then return the Symbol.
                    return symbol_table.lookup(self.name)
                except KeyError:
                    # The Reference Node does not match any Symbols in
                    # this SymbolTable.
                    pass
            if test_node is scope_limit:
                # The ancestor scope Node has been reached and nothing
                # has matched so return with None.
                return None
            # Move on to the next ancestor.
            test_node = test_node.parent
        # scope has not been set and all Nodes have been checked (up
        # to the root Node) but there has been no match so return with
        # None.
        return None


@six.add_metaclass(abc.ABCMeta)
class Operation(Node):
    '''
    Abstract base class for PSyIR nodes representing operators.

    :param operator: the operator used in the operation.
    :type operator: :py:class:`psyclone.psyGen.UnaryOperation.Operator` or \
                    :py:class:`psyclone.psyGen.BinaryOperation.Operator` or \
                    :py:class:`psyclone.psyGen.NaryOperation.Operator`
    :param parent: the parent node of this Operation in the PSyIR.
    :type parent: :py:class:`psyclone.psyGen.Node`

    :raises TypeError: if the supplied operator is not an instance of \
                       self.Operator.

    '''
    # Must be overridden in sub-class to hold an Enumeration of the Operators
    # that it can represent.
    Operator = None

    def __init__(self, operator, parent=None):
        super(Operation, self).__init__(parent=parent)

        if not isinstance(operator, self.Operator):
            raise TypeError(
                "{0} operator argument must be of type "
                "{0}.Operator but found {1}.".format(type(self).__name__,
                                                     type(operator).__name__))
        self._operator = operator
        self._text_name = "Operation"
        self._colour_key = "Operation"

    @property
    def operator(self):
        '''
        Return the operator.

        :returns: Enumerated type capturing the operator.
        :rtype: :py:class:`psyclone.psyGen.UnaryOperation.Operator` or \
                :py:class:`psyclone.psyGen.BinaryOperation.Operator` or \
                :py:class:`psyclone.psyGen.NaryOperation.Operator`

        '''
        return self._operator

    def node_str(self, colour=True):
        '''
        Construct a text representation of this node, optionally with control
        codes for coloured display in a suitable terminal.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this PSyIR node.
        :rtype: str
        '''
        return self.coloured_name(colour) + \
            "[operator:'" + self._operator.name + "']"

    def __str__(self):
        result = self.node_str(False) + "\n"
        for entity in self._children:
            result += str(entity) + "\n"

        # Delete last line break
        if result[-1] == "\n":
            result = result[:-1]
        return result


class UnaryOperation(Operation):
    '''
    Node representing a UnaryOperation expression. As such it has one operand
    as child 0, and an attribute with the operator type.

    :param operator: Enumerated type capturing the unary operator.
    :type operator: :py:class:`psyclone.psyGen.UnaryOperation.Operator`
    :param parent: the parent node of this UnaryOperation in the PSyIR.
    :type parent: :py:class:`psyclone.psyGen.Node`

    '''
    Operator = Enum('Operator', [
        # Arithmetic Operators
        'MINUS', 'PLUS', 'SQRT', 'EXP', 'LOG', 'LOG10',
        # Logical Operators
        'NOT',
        # Trigonometric Operators
        'COS', 'SIN', 'TAN', 'ACOS', 'ASIN', 'ATAN',
        # Other Maths Operators
        'ABS', 'CEIL',
        # Casting Operators
        'REAL', 'INT'
        ])

    def __init__(self, operation, parent=None):
        super(UnaryOperation, self).__init__(operation, parent)
        self._text_name = "UnaryOperation"


class BinaryOperation(Operation):
    '''
    Node representing a BinaryOperation expression. As such it has two operands
    as children 0 and 1, and an attribute with the operator type.

    :param operator: the operator used in the operation.
    :type operator: :py:class:`psyclone.psyGen.BinaryOperation.Operator`
    :param parent: the parent node of this Operation in the PSyIR.
    :type parent: :py:class:`psyclone.psyGen.Node`

    '''
    Operator = Enum('Operator', [
        # Arithmetic Operators. ('REM' is remainder AKA 'MOD' in Fortran.)
        'ADD', 'SUB', 'MUL', 'DIV', 'REM', 'POW', 'SUM',
        # Relational Operators
        'EQ', 'NE', 'GT', 'LT', 'GE', 'LE',
        # Logical Operators
        'AND', 'OR',
        # Other Maths Operators
        'SIGN', 'MIN', 'MAX',
        # Query Operators
        'SIZE'
        ])

    def __init__(self, operator, parent=None):
        super(BinaryOperation, self).__init__(operator, parent)
        self._text_name = "BinaryOperation"

    def math_equal(self, other):
        ''':param other: the node to compare self with.
        :type other: py:class:`psyclone.psyGen.Node`
        :returns: True if the self has the same results as other.
        :rtype: bool
        '''
        if not super(BinaryOperation, self).math_equal(other):
            # Support some commutative law, unfortunately we now need
            # to repeat some tests already done in super(), since we
            # don't know why the above test failed
            # TODO #533 for documenting restrictions
            # pylint: disable=unidiomatic-typecheck
            if type(self) != type(other):
                return False
            if self.operator != other.operator:
                return False
            if self.operator not in [self.Operator.ADD, self.Operator.MUL,
                                     self.Operator.AND, self.Operator.OR,
                                     self.Operator.EQ]:
                return False
            return self._children[0].math_equal(other.children[1]) and \
                self._children[1].math_equal(other.children[0])
        return self.operator == other.operator


class NaryOperation(Operation):
    '''
    Node representing a n-ary operation expression. The n operands are the
    stored as the 0 - n-1th children of this node and the type of the operator
    is held in an attribute.


    :param operator: the operator used in the operation.
    :type operator: :py:class:`psyclone.psyGen.NaryOperation.Operator`
    :param parent: the parent node of this Operation in the PSyIR.
    :type parent: :py:class:`psyclone.psyGen.Node`

    '''
    Operator = Enum('Operator', [
        # Arithmetic Operators
        'MAX', 'MIN', 'SUM'
        ])

    def __init__(self, operator, parent=None):
        super(NaryOperation, self).__init__(operator, parent)
        self._text_name = "NaryOperation"


class Array(Reference):
    '''
    Node representing an Array reference. As such it has a reference and a
    subscript list as children 0 and 1, respectively.

    :param reference_name: node in the fparser2 parse tree representing array.
    :type reference_name: :py:class:`fparser.two.Fortran2003.Part_Ref.
    :param parent: the parent node of this Array in the PSyIR.
    :type parent: :py:class:`psyclone.psyGen.Node`

    '''
    def __init__(self, reference_name, parent):
        super(Array, self).__init__(reference_name, parent=parent)
        self._text_name = "ArrayReference"
        self._colour_key = "Reference"

    def __str__(self):
        result = "Array" + super(Array, self).__str__() + "\n"
        for entity in self._children:
            result += str(entity) + "\n"
        return result

    def reference_accesses(self, var_accesses):
        '''Get all variable access information. All variables used as indices
        in the access of the array will be added as READ.
        :param var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`
        '''

        # This will set the array-name as READ
        super(Array, self).reference_accesses(var_accesses)

        # Now add all children: Note that the class Reference
        # does not recurse to the children (which store the indices), so at
        # this stage no index information has been stored:
        list_indices = []
        for child in self._children:
            child.reference_accesses(var_accesses)
            list_indices.append(child)

        if list_indices:
            var_info = var_accesses[self._reference]
            # The last entry in all_accesses is the one added above
            # in super(Array...). Add the indices to that entry.
            var_info.all_accesses[-1].indices = list_indices


class Literal(Node):
    '''
    Node representing a Literal

    :param str value: String representing the literal value.
    :param parent: the parent node of this Literal in the PSyIR.
    :type parent: :py:class:`psyclone.psyGen.Node`
    '''
    def __init__(self, value, parent=None):
        super(Literal, self).__init__(parent=parent)
        self._value = value

    @property
    def value(self):
        '''
        Return the value of the literal.

        :returns: String representing the literal value.
        :rtype: str
        '''
        return self._value

    def node_str(self, colour=True):
        '''
        Construct a text representation of this node, optionally containing
        colour control codes.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this PSyIR node.
        :rtype: str
        '''
        return "{0}[value:'{1}']".format(self.coloured_name(colour),
                                         self._value)

    def math_equal(self, other):
        ''':param other: the node to compare self with.
        :type other: py:class:`psyclone.psyGen.Node`

        :return: if the self has the same results as other.
        :type: bool
        '''

        return self.value == other.value


class Return(Node):
    '''
    Node representing a Return statement (subroutine break without return
    value).

    :param parent: the parent node of this Return in the PSyIR.
    :type parent: :py:class:`psyclone.psyGen.Node`
    '''
    def __init__(self, parent=None):
        super(Return, self).__init__(parent=parent)

    def __str__(self):
        return "Return[]\n"


class Container(Node):
    '''Node representing a set of KernelSchedule and/or Container nodes,
    as well as a name and a SymbolTable. This construct can be used to
    scope symbols of variables, KernelSchedule names and Container
    names. In Fortran a container would naturally represent a module
    or a submodule.

    :param str name: the name of the container.
    :param parent: optional parent node of this Container in the PSyIR.
    :type parent: :py:class:`psyclone.psyGen.Node`

    '''
    def __init__(self, name, parent=None):
        super(Container, self).__init__(parent=parent)
        self._name = name
        self._symbol_table = SymbolTable(self)

    @property
    def name(self):
        '''
        :returns: name of the container.
        :rtype: str

        '''
        return self._name

    @name.setter
    def name(self, new_name):
        '''Sets a new name for the container.

        :param str new_name: new name for the container.

        '''
        self._name = new_name

    @property
    def symbol_table(self):
        '''
        :returns: table containing symbol information for the container.
        :rtype: :py:class:`psyclone.psyGen.SymbolTable`

        '''
        return self._symbol_table

    def node_str(self, colour=True):
        '''
        Returns the name of this node with appropriate control codes
        to generate coloured output in a terminal that supports it.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        :rtype: str
        '''
        return self.coloured_name(colour) + "[{0}]".format(self.name)

    def __str__(self):
        return "Container[{0}]\n".format(self.name)


__all__ = ['UnaryOperation', 'BinaryOperation', 'NaryOperation',
           'Literal', 'Return', 'Container']
