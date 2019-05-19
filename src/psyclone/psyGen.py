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
import abc
from enum import Enum
import six
from psyclone.configuration import Config
from psyclone.core.access_type import AccessType

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

# The list of Fortran instrinsic functions that we know about (and can
# therefore distinguish from array accesses). These should really be
# provided by the parser (github.com/stfc/fparser/issues/189).
FORTRAN_INTRINSICS = ["MIN", "MAX", "ABS", "SIGN", "MOD", "SUM",
                      "CEILING", "REAL", "KIND", "EXP", "SQRT",
                      "SIN", "COS", "TAN", "ASIN", "ACOS", "ATAN",
                      "LOG", "LOG10", "NINT",
                      "MINVAL", "MAXVAL", "MINLOC", "MAXLOC", "TRIM",
                      "RESHAPE"]

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
                       "Call": "magenta",
                       "KernCall": "magenta",
                       "Profile": "green",
                       "Extract": "green",
                       "If": "red",
                       "Assignment": "blue",
                       "Reference": "yellow",
                       "BinaryOperation": "blue",
                       "UnaryOperation": "blue",
                       "Literal": "yellow",
                       "Return": "yellow",
                       "CodeBlock": "red"}

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
            for kernel in schedule.walk(schedule.children, Kern):
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
        from psyclone.profiler import Profiler
        for idx, alg_invocation in enumerate(alg_calls):
            my_invoke = Invoke(alg_invocation, idx)
            self.invoke_map[my_invoke.name] = my_invoke
            self.invoke_list.append(my_invoke)
            # Add profiling nodes to schedule if automatic profiling has been
            # requested.
            Profiler.add_profile_nodes(my_invoke.schedule, Loop)

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
        for invoke in self.invoke_list:
            invoke.gen_code(parent)
            # If we are generating OpenCL for an Invoke then we need to
            # create routine(s) to set the arguments of the Kernel(s) it
            # calls. We do it here as this enables us to prevent
            # duplication.
            if invoke.schedule.opencl:
                for kern in invoke.schedule.kern_calls():
                    if kern.name not in opencl_kernels:
                        opencl_kernels.append(kern.name)
                        kern.gen_arg_setter_code(parent)
                # We must also ensure that we have a kernel object for
                # each kernel called from the PSy layer
                self.gen_ocl_init(parent, opencl_kernels)

    @staticmethod
    def gen_ocl_init(parent, kernels):
        '''
        Generates a subroutine to initialise the OpenCL environment and
        construct the list of OpenCL kernel objects used by this PSy layer.

        :param parent: the node in the f2pygen AST representing the module \
                       that will contain the generated subroutine.
        :type parent: :py:class:`psyclone.f2pygen.ModuleGen`
        :param kernels: List of kernel names called by the PSy layer.
        :type kernels: list of str
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
        ifthen.add(CallGen(ifthen, "ocl_env_init"))

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
        for call in self.schedule.calls():
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
        for kern_call in self._schedule.kern_calls():
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
        for call in self.schedule.calls():
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
        for call in self.schedule.calls():
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

    def __str__(self):
        raise NotImplementedError("Please implement me")

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

        :return: List of anotations
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
        arguments (i.e. is not a Call class or subclass). Arguments of
        any of this nodes descendants are considered to be
        associated. '''
        args = []
        for call in self.calls():
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
        otherwise return False. '''
        # First perform correctness checks
        # 1: check new_node is a Node
        if not isinstance(new_node, Node):
            raise GenerationError(
                "In the psyGen Call class is_valid_location() method the "
                "supplied argument is not a Node, it is a '{0}'.".
                format(type(new_node).__name__))

        # 2: check position has a valid value
        valid_positions = ["before", "after"]
        if position not in valid_positions:
            raise GenerationError(
                "The position argument in the psyGen Call class "
                "is_valid_location() method must be one of {0} but "
                "found '{1}'".format(valid_positions, position))

        # 3: check self and new_node have the same parent
        if not self.sameParent(new_node):
            raise GenerationError(
                "In the psyGen Call class is_valid_location() method "
                "the node and the location do not have the same parent")

        # 4: check proposed new position is not the same as current position
        new_position = new_node.position
        if new_position < self.position and position == "after":
            new_position += 1
        elif new_position > self.position and position == "before":
            new_position -= 1

        if self.position == new_position:
            raise GenerationError(
                "In the psyGen Call class is_valid_location() method, the "
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

    @abc.abstractmethod
    def view(self, indent=0):
        '''Abstract function to prints a text representation of the node.
        :param int indent: depth of indent for output text.
        '''

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

    def list_to_string(self, my_list):
        result = ""
        for idx, value in enumerate(my_list):
            result += str(value)
            if idx < (len(my_list) - 1):
                result += ","
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
        :raises InternalError: if the absolute position cannot be found
        :rtype: int
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

    def walk(self, children, my_type):
        ''' Recurse through tree and return objects of 'my_type'. '''
        local_list = []
        for child in children:
            if isinstance(child, my_type):
                local_list.append(child)
            local_list += self.walk(child.children, my_type)
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

    def calls(self):
        '''Return all calls that are descendants of this node.'''
        return self.walk(self.children, Call)

    def following(self):
        '''Return all :py:class:`psyclone.psyGen.Node` nodes after me in the
        schedule. Ordering is depth first.

        :returns: a list of nodes
        :rtype: :func:`list` of :py:class:`psyclone.psyGen.Node`

        '''
        all_nodes = self.walk(self.root.children, Node)
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
        all_nodes = self.walk(self.root.children, Node)
        position = all_nodes.index(self)
        nodes = all_nodes[:position]
        if reverse:
            nodes.reverse()
        return nodes

    @property
    def following_calls(self):
        '''Return all calls after me in the schedule.'''
        all_calls = self.root.calls()
        position = all_calls.index(self)
        return all_calls[position+1:]

    @property
    def preceding_calls(self):
        '''Return all calls before me in the schedule.'''
        all_calls = self.root.calls()
        position = all_calls.index(self)
        return all_calls[:position-1]

    def kern_calls(self):
        '''Return all user-supplied kernel calls in this schedule.'''
        return self.walk(self._children, Kern)

    def loops(self):
        '''Return all loops currently in this schedule.'''
        return self.walk(self._children, Loop)

    def reductions(self, reprod=None):
        '''Return all calls that have reductions and are decendents of this
        node. If reprod is not provided, all reductions are
        returned. If reprod is False, all builtin reductions that are
        not set to reproducible are returned. If reprod is True, all
        builtins that are set to reproducible are returned.'''

        call_reduction_list = []
        for call in self.walk(self.children, Call):
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
        '''Returns true if this Node is within an OpenMP parallel region.

        '''
        omp_dir = self.ancestor(OMPParallelDirective)
        if omp_dir:
            return True
        return False

    def gen_code(self, parent):
        '''Abstract base class for code generation function.
        :param parent: the parent of this Node in the PSyIR.
        :type parent: :py:class:`psyclone.psyGen.Node`.
        '''
        raise NotImplementedError("Please implement me")

    def gen_c_code(self, indent=0):
        '''Abstract method for the generation of C source code

        :param int indent: Depth of indent for the output string.
        :raises NotImplementedError: is an abstract method.
        '''
        raise NotImplementedError("Please implement me")

    def update(self):
        ''' By default we assume there is no need to update the existing
        fparser2 AST which this Node represents. We simply call the update()
        method of any children. '''
        for child in self._children:
            child.update()


class Schedule(Node):
    ''' Stores schedule information for a sequence of statements.

    :param sequence: the sequence of PSyIR nodes that make up the schedule.
    :type sequence: list of :py:class:`psyclone.psyGen.Node`
    :param parent: that parent of this node in the PSyIR tree.
    :type parent:  :py:class:`psyclone.psyGen.Node`
    '''

    def __init__(self, sequence=None, parent=None):
        Node.__init__(self, children=sequence, parent=parent)

    @property
    def dag_name(self):
        '''
        :returns: The name of this node in the dag.
        :rtype: str
        '''
        return "schedule"

    def view(self, indent=0):
        '''
        Print a text representation of this node to stdout and then
        call the view() method of any children.

        :param int indent: Depth of indent for output text.
        '''
        print(self.indent(indent) + self.coloured_text + "[]")
        for entity in self._children:
            entity.view(indent=indent + 1)

    @property
    def coloured_text(self):
        '''
        Returns the name of this node with appropriate control codes
        to generate coloured output in a terminal that supports it.

        :return: Text containing the name of this node, possibly coloured.
        :rtype: str
        '''
        return colored("Schedule", SCHEDULE_COLOUR_MAP["Schedule"])

    def __str__(self):
        result = "Schedule:\n"
        for entity in self._children:
            result += str(entity)+"\n"
        result += "End Schedule"
        return result


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
        Schedule.__init__(self, sequence=sequence, parent=None)
        self._invoke = None
        self._opencl = False  # Whether or not to generate OpenCL
        self._name_space_manager = NameSpaceFactory().create()

    @property
    def invoke(self):
        return self._invoke

    @invoke.setter
    def invoke(self, my_invoke):
        self._invoke = my_invoke

    def view(self, indent=0):
        '''
        Print a text representation of this node to stdout and then
        call the view() method of any children.

        :param indent: Depth of indent for output text
        :type indent: integer
        '''
        print(self.indent(indent) + self.coloured_text +
              "[invoke='" + self.invoke.name + "']")
        for entity in self._children:
            entity.view(indent=indent + 1)

    @property
    def coloured_text(self):
        '''
        Returns the name of this node with appropriate control codes
        to generate coloured output in a terminal that supports it.

        :returns: Text containing the name of this node, possibly coloured
        :rtype: string
        '''
        return colored("InvokeSchedule", SCHEDULE_COLOUR_MAP["Schedule"])

    def __str__(self):
        result = "InvokeSchedule:\n"
        for entity in self._children:
            result += str(entity)+"\n"
        result += "End Schedule"
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
            kernels = self.walk(self._children, Call)
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

        if self.opencl:
            # Ensure we block at the end of the invoke to ensure all
            # kernels have completed before we return.
            # This code ASSUMES only the first command queue is used for
            # executing kernels.
            parent.add(CommentGen(parent,
                                  " Block until all kernels have finished"))
            parent.add(AssignGen(parent, lhs=flag,
                                 rhs="clFinish(" + qlist + "(1))"))

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

    All classes that generate Directive statments (e.g. OpenMP,
    OpenACC, compiler-specific) inherit from this class.

    '''

    def view(self, indent=0):
        '''
        Print a text representation of this node to stdout and then
        call the view() method of any children.

        :param indent: Depth of indent for output text
        :type indent: integer
        '''
        print(self.indent(indent) + self.coloured_text)
        for entity in self._children:
            entity.view(indent=indent + 1)

    @property
    def coloured_text(self):
        '''
        Returns a string containing the name of this element with
        control codes for colouring in terminals that support it.

        :returns: Text containing the name of this node, possibly coloured
        :rtype: string
        '''
        return colored("Directive", SCHEDULE_COLOUR_MAP["Directive"])

    @property
    def dag_name(self):
        ''' return the base dag name for this node '''
        return "directive_" + str(self.abs_position)


class ACCDirective(Directive):
    ''' Base class for all OpenACC directive statements. '''

    @abc.abstractmethod
    def view(self, indent=0):
        '''
        Print text representation of this node to stdout.

        :param int indent: size of indent to use for output
        '''

    @property
    def dag_name(self):
        ''' Return the name to use in a dag for this node.

        :returns: Name of corresponding node in DAG
        :rtype: str
        '''
        return "ACC_directive_" + str(self.abs_position)

    def _add_region(self, start_text, end_text=None, data_movement=None):
        '''
        Modifies the underlying fparser2 parse tree to include a subset
        of nodes within a region. (e.g. a 'kernels' or 'data' region.)

        :param str start_text: the directive body to insert at the \
                               beginning of the region. "!$ACC " is \
                               prepended to the supplied text.
        :param str end_text: the directive body to insert at the end of \
                             the region (or None). "!$ACC " is \
                             prepended to the supplied text.
        :param str data_movement: whether to include data-movement clauses and\
                               if so, whether to determine them by analysing \
                               the code within the region ("analyse") or to \
                               specify 'default(present)' ("present").

        :raises InternalError: if either start_text or end_text already
                               begin with '!'.
        :raises InternalError: if data_movement is not None and not one of \
                               "present" or "analyse".
        '''
        from fparser.common.readfortran import FortranStringReader
        from fparser.two.Fortran2003 import Comment
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

        # Find a reference to the fparser2 parse tree that belongs to
        # the contents of this region. Then go back up one level in the
        # parse tree to find the node to which we will add directives as
        # children. (We do this because our parent PSyIR node may be a
        # directive which has no associated entry in the fparser2 parse tree.)
        # TODO this should be simplified/improved once
        # the fparser2 parse tree has parent information (fparser/#102).
        content_ast = self.children[0].ast
        fp_parent = content_ast._parent

        # Find the location of the AST of our first child node in the
        # list of child nodes of our parent in the fparser parse tree.
        ast_start_index = object_index(fp_parent.content,
                                       content_ast)
        if end_text:
            if self.children[-1].ast_end:
                ast_end_index = object_index(fp_parent.content,
                                             self.children[-1].ast_end)
            else:
                ast_end_index = object_index(fp_parent.content,
                                             self.children[-1].ast)

            text = "!$ACC " + end_text
            directive = Comment(FortranStringReader(text,
                                                    ignore_comments=False))
            fp_parent.content.insert(ast_end_index+1, directive)
            # Retro-fit parent information. # TODO remove/modify this once
            # fparser/#102 is done (i.e. probably supply parent info as option
            # to the Comment() constructor).
            directive._parent = fp_parent
            # Ensure this end directive is included with the set of statements
            # belonging to this PSyIR node.
            self.ast_end = directive

        text = "!$ACC " + start_text

        if data_movement:
            if data_movement == "analyse":
                # Identify the inputs and outputs to the region (variables that
                # are read and written).
                processor = Fparser2ASTProcessor()
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

    def view(self, indent=0):
        '''
        Print a text representation of this Node to stdout.

        :param int indent: the amount by which to indent the output.
        '''
        print(self.indent(indent)+self.coloured_text+"[ACC enter data]")
        for entity in self._children:
            entity.view(indent=indent + 1)

    @property
    def dag_name(self):
        '''
        :returns: the name to use for this Node in a DAG
        :rtype: str
        '''
        return "ACC_data_" + str(self.abs_position)

    def gen_code(self, parent):
        '''
        Generate the elements of the f2pygen AST for this Node in the Schedule.

        :param parent: node in the f2pygen AST to which to add node(s).
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`
        '''
        from psyclone.f2pygen import DeclGen, DirectiveGen, CommentGen, \
            IfThenGen, AssignGen, CallGen, UseGen

        # We must generate a list of all of the fields accessed by
        # OpenACC kernels (calls within an OpenACC parallel directive)
        # 1. Find all parallel directives. We store this list for later
        #    use in any sub-class.
        self._acc_dirs = self.walk(self.root.children, ACCParallelDirective)
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
        var_str = self.list_to_string(var_list)

        # 4. Declare and initialise a logical variable to keep track of
        #    whether this is the first time we've entered this Invoke
        name_space_manager = NameSpaceFactory().create()
        first_time = name_space_manager.create_name(
            root_name="first_time", context="PSyVars", label="first_time")
        parent.add(DeclGen(parent, datatype="logical",
                           entity_decls=[first_time],
                           initial_values=[".True."],
                           save=True))
        parent.add(CommentGen(parent,
                              " Ensure all fields are on the device and"))
        parent.add(CommentGen(parent, " copy them over if not."))
        # 5. Put the enter data directive inside an if-block so that we
        #    only ever do it once
        ifthen = IfThenGen(parent, first_time)
        parent.add(ifthen)
        ifthen.add(DirectiveGen(ifthen, "acc", "begin", "enter data",
                                "copyin("+var_str+")"))
        # 6. Flag that we have now entered this routine at least once
        ifthen.add(AssignGen(ifthen, lhs=first_time, rhs=".false."))
        # 7. Flag that the data is now on the device. This calls down
        #    into the API-specific subclass of this class.
        self.data_on_device(ifthen)
        parent.add(CommentGen(parent, ""))

        # 8. Ensure that any scalars are up-to-date
        var_list = []
        for pdir in self._acc_dirs:
            for var in pdir.scalars:
                if var not in var_list:
                    var_list.append(var)
        if var_list:
            # We need to 'use' the openacc module in order to access
            # the OpenACC run-time library
            parent.add(UseGen(parent, name="openacc", only=True,
                              funcnames=["acc_update_device"]))
            parent.add(
                CommentGen(parent,
                           " Ensure all scalars on the device are up-to-date"))
            for var in var_list:
                parent.add(CallGen(parent, "acc_update_device", [var, "1"]))
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
    def view(self, indent=0):
        '''
        Print a text representation of this Node to stdout.

        :param int indent: the amount by which to indent the output.
        '''
        print(self.indent(indent)+self.coloured_text+"[ACC Parallel]")
        for entity in self._children:
            entity.view(indent=indent + 1)

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
        from psyclone.f2pygen import DirectiveGen

        # Since we use "default(present)" the Schedule must contain an
        # 'enter data' directive. We don't mandate the order in which
        # transformations are applied so we have to check for that here.
        # We can't use Node.ancestor() because the data directive does
        # not have children. Instead, we go back up to the Schedule and
        # walk down from there.
        nodes = self.root.walk(self.root.children, ACCEnterDataDirective)
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

        # Look-up the calls that are children of this node
        for call in self.calls():
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
        # Look-up the calls that are children of this node
        fld_list = []
        for call in self.calls():
            for arg in call.arguments.fields:
                if arg not in fld_list:
                    fld_list.append(arg)
        return fld_list

    @property
    def scalars(self):
        '''
        Returns a list of the scalar quantities required by the Calls in
        this region.

        :returns: list of names of scalar arguments.
        :rtype: list of str
        '''
        scalars = []
        for call in self.calls():
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

    def view(self, indent=0):
        '''
        Print a textual representation of this Node to stdout.

        :param int indent: amount to indent output by
        '''
        text = self.indent(indent)+self.coloured_text+"[ACC Loop"
        if self._sequential:
            text += ", seq"
        else:
            if self._collapse:
                text += ", collapse={0}".format(self._collapse)
            if self._independent:
                text += ", independent"
        text += "]"
        print(text)
        for entity in self._children:
            entity.view(indent=indent + 1)

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
        from psyclone.f2pygen import DirectiveGen

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
    @property
    def dag_name(self):
        '''
        :returns: the name to use in a dag for this node
        :rtype: str
        '''
        return "OMP_directive_" + str(self.abs_position)

    def view(self, indent=0):
        '''
        Print a text representation of this node to stdout and then
        call the view() method of any children.

        :param indent: Depth of indent for output text
        :type indent: integer
        '''
        print(self.indent(indent) + self.coloured_text + "[OMP]")
        for entity in self._children:
            entity.view(indent=indent + 1)

    def _get_reductions_list(self, reduction_type):
        '''Return the name of all scalars within this region that require a
        reduction of type reduction_type. Returned names will be unique.
        :param reduction_type: The reduction type (e.g. AccessType.SUM) to \
            search for.
        :type reduction_type: :py:class:`psyclone.core.access_type.AccessType`
        '''
        result = []
        for call in self.calls():
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

    def view(self, indent=0):
        '''
        Print a text representation of this node to stdout and then
        call the view() method of any children.

        :param indent: Depth of indent for output text
        :type indent: integer
        '''
        print(self.indent(indent) + self.coloured_text + "[OMP parallel]")
        for entity in self._children:
            entity.view(indent=indent + 1)

    def gen_code(self, parent):
        '''Generate the fortran OMP Parallel Directive and any associated
        code'''
        from psyclone.f2pygen import DirectiveGen, AssignGen, UseGen, \
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
        private_str = self.list_to_string(private_list)

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

        first_type = type(self.children[0])
        for child in self.children:
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

    def _get_private_list(self):
        '''
        Returns the variable names used for any loops within a directive
        and any variables that have been declared private by a Call
        within the directive.

        :returns: list of variables to declare as thread private.
        :rtype: list of str

        :raises InternalError: if a Call has local variable(s) but they \
                               aren't named.
        '''
        result = []
        # get variable names from all loops that are a child of this node
        for loop in self.loops():
            # We must allow for implicit loops (e.g. in the NEMO API) that
            # have no associated variable name
            if loop.variable_name and \
               loop.variable_name.lower() not in result:
                result.append(loop.variable_name.lower())
        # get variable names from all calls that are a child of this node
        for call in self.calls():
            for variable_name in call.local_vars():
                if variable_name == "":
                    raise InternalError(
                        "call '{0}' has a local variable but its "
                        "name is not set.".format(call.name))
                if variable_name.lower() not in result:
                    result.append(variable_name.lower())
        return result

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
        node_list = self.walk(self.children, OMPDirective)
        if len(node_list) == 0:
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

        :raises InternalError: if the existing AST doesn't have the \
                               correct structure to permit the insertion \
                               of the OpenMP parallel region.
        '''
        from fparser.common.readfortran import FortranStringReader
        from fparser.two.Fortran2003 import Comment

        # Ensure the fparser2 AST is up-to-date for all of our children
        Node.update(self)

        # Check that we haven't already been called
        if self.ast:
            return

        # Find the locations in which we must insert the begin/end
        # directives...
        # Find the children of this node in the AST of our parent node
        try:
            start_idx = object_index(self._parent.ast.content,
                                     self._children[0].ast)
            end_idx = object_index(self._parent.ast.content,
                                   self._children[-1].ast)
        except (IndexError, ValueError):
            raise InternalError("Failed to find locations to insert "
                                "begin/end directives.")
        # Create the start directive
        text = "!$omp parallel default(shared), private({0})".format(
            ",".join(self._get_private_list()))
        startdir = Comment(FortranStringReader(text,
                                               ignore_comments=False))
        # Create the end directive and insert it after the node in
        # the AST representing our last child
        enddir = Comment(FortranStringReader("!$omp end parallel",
                                             ignore_comments=False))
        self.ast_end = enddir
        # If end_idx+1 takes us beyond the range of the list then the
        # element is appended to the list
        self._parent.ast.content.insert(end_idx+1, enddir)

        # Insert the start directive (do this second so we don't have
        # to correct end_idx)
        self.ast = startdir
        self._parent.ast.content.insert(start_idx, self.ast)


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

    def view(self, indent=0):
        '''
        Write out a textual summary of the OpenMP Do Directive and then
        call the view() method of any children.

        :param indent: Depth of indent for output text
        :type indent: integer
        '''
        if self.reductions():
            reprod = "[reprod={0}]".format(self._reprod)
        else:
            reprod = ""
        print(self.indent(indent) + self.coloured_text +
              "[OMP do]{0}".format(reprod))

        for entity in self._children:
            entity.view(indent=indent + 1)

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
        from psyclone.f2pygen import DirectiveGen

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

    def view(self, indent=0):
        '''
        Write out a textual summary of the OpenMP Parallel Do Directive
        and then call the view() method of any children.

        :param indent: Depth of indent for output text
        :type indent: integer
        '''
        print(self.indent(indent) + self.coloured_text +
              "[OMP parallel do]")
        for entity in self._children:
            entity.view(indent=indent + 1)

    def gen_code(self, parent):
        from psyclone.f2pygen import DirectiveGen

        # We're not doing nested parallelism so make sure that this
        # omp parallel do is not already within some parallel region
        self._not_within_omp_parallel_region()

        calls = self.reductions()
        zero_reduction_variables(calls, parent)
        private_str = self.list_to_string(self._get_private_list())
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
        from fparser.common.readfortran import FortranStringReader
        from fparser.two.Fortran2003 import Comment

        # Ensure the fparser2 AST is up-to-date for all of our children
        Node.update(self)

        # Check that we haven't already been called
        if self.ast:
            return

        # Since this is an OpenMP (parallel) do, it can only be applied
        # to a single loop.
        if len(self._children) != 1:
            raise GenerationError(
                "An OpenMP PARALLEL DO can only be applied to a single loop "
                "but this Node has {0} children: {1}".
                format(len(self._children), self._children))

        # Find the locations in which we must insert the begin/end
        # directives...
        # Find the child of this node in the AST of our parent node
        # TODO make this robust by using the new 'children' method to
        # be introduced in fparser#105
        # We have to take care to find a parent node (in the fparser2 AST)
        # that has 'content'. This is because If-else-if blocks have their
        # 'content' as siblings of the If-then and else-if nodes.
        parent = self._parent.ast
        while parent:
            if hasattr(parent, "content") and \
               parent is not self.children[0].ast:
                break
            parent = parent._parent
        if not parent:
            raise InternalError("Failed to find parent node in which to "
                                "insert OpenMP parallel do directive")
        start_idx = object_index(parent.content,
                                 self._children[0].ast)

        # Create the start directive
        text = ("!$omp parallel do default(shared), private({0}), "
                "schedule({1})".format(",".join(self._get_private_list()),
                                       self._omp_schedule))
        startdir = Comment(FortranStringReader(text,
                                               ignore_comments=False))

        # Create the end directive and insert it after the node in
        # the AST representing our last child
        enddir = Comment(FortranStringReader("!$omp end parallel do",
                                             ignore_comments=False))
        if start_idx == len(parent.content) - 1:
            parent.content.append(enddir)
        else:
            parent.content.insert(start_idx+1, enddir)
        self.ast_end = enddir

        # Insert the start directive (do this second so we don't have
        # to correct the location)
        self.ast = startdir
        parent.content.insert(start_idx, self.ast)


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

    def view(self, indent):
        '''
        Print text describing this object to stdout and then
        call the view() method of any children.

        :param indent: Depth of indent for output text
        :type indent: integer
        '''
        print(self.indent(indent) + (
            "{0}[scalar='{1}']".format(self.coloured_text, self._scalar.name)))

    def __str__(self):
        return "GlobalSum[scalar='" + self._scalar.name + "']\n"

    @property
    def coloured_text(self):
        '''
        Return a string containing the (coloured) name of this node
        type

        :returns: A string containing the name of this node, possibly with
                  control codes for colour
        :rtype: string
        '''
        return colored("GlobalSum", SCHEDULE_COLOUR_MAP["GlobalSum"])


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
        self._colour_map_name = "HaloExchange"
        self._dag_name = "haloexchange"

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
        ''' Return the name to use in a dag for this node'''
        name = ("{0}({1})_{2}".format(self._dag_name, self._field.name,
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

    def view(self, indent=0):
        '''
        Write out a textual summary of the OpenMP Parallel Do Directive
        and then call the view() method of any children.

        :param indent: Depth of indent for output text
        :type indent: integer
        '''
        print(self.indent(indent) + (
            "{0}[field='{1}', type='{2}', depth={3}, "
            "check_dirty={4}]".format(self.coloured_text, self._field.name,
                                      self._halo_type,
                                      self._halo_depth, self._check_dirty)))

    def __str__(self):
        result = "HaloExchange["
        result += "field='" + str(self._field.name) + "', "
        result += "type='" + str(self._halo_type) + "', "
        result += "depth='" + str(self._halo_depth) + "', "
        result += "check_dirty='" + str(self._check_dirty) + "']\n"
        return result

    @property
    def coloured_text(self):
        '''
        Return a string containing the (coloured) name of this node type

        :returns: Name of this node type, possibly with colour control codes
        :rtype: string
        '''
        return colored(
            self._text_name, SCHEDULE_COLOUR_MAP[self._colour_map_name])


class Loop(Node):
    '''Represents a loop in the PSyIR.

    :param parent: Parent of this node in the PSyIR.
    :type parent: sub-class of :py:class:`psyclone.psyGen.Node`
    :param str variable_name: Optional name of the loop iterator \
    variable. Defaults to an empty string.
    :param valid_loop_types: A list of loop types that are specific \
    to a particular API.
    :type valid_loop_types: list of str

    '''

    def __init__(self, parent=None,
                 variable_name="",
                 valid_loop_types=None):

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

        # TODO replace iterates_over with iteration_space
        self._iterates_over = "unknown"

        Node.__init__(self, parent=parent)

        self._variable_name = variable_name

        self._start = ""
        self._stop = ""
        self._step = ""
        self._id = ""

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

    def view(self, indent=0):
        '''
        Write out a textual summary of this Loop node to stdout
        and then call the view() method of any children.

        :param indent: Depth of indent for output text
        :type indent: integer
        '''
        print(self.indent(indent) + self.coloured_text +
              "[type='{0}',field_space='{1}',it_space='{2}']".
              format(self._loop_type, self._field_space, self.iteration_space))
        for entity in self._children:
            entity.view(indent=indent + 1)

    @property
    def coloured_text(self):
        '''
        Returns a string containing the name of this node along with
        control characters for colouring in terminals that support it.

        :returns: The name of this node, possibly with control codes for
                  colouring
        :rtype: string
        '''
        return colored("Loop", SCHEDULE_COLOUR_MAP["Loop"])

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
        result = "Loop[" + self._id + "]: " + self._variable_name + "=" + \
            self._id + " lower=" + self._start + "," + self._stop + "," + \
            self._step + "\n"
        for entity in self._children:
            result += str(entity) + "\n"
        result += "EndLoop"
        return result

    def has_inc_arg(self):
        ''' Returns True if any of the Kernels called within this
        loop have an argument with INC access. Returns False otherwise '''
        for kern_call in self.kern_calls():
            for arg in kern_call.arguments.args:
                if arg.access == AccessType.INC:
                    return True
        return False

    def unique_modified_args(self, arg_type):
        '''Return all unique arguments of type arg_type from Kernels in this
        loop that are modified.
        :param str arg_type: the type of kernel argument (e.g. field, \
                             operator) to search for.
        :returns: all unique arguments of type arg_type from Kernels in this
        loop that are modified.
        :rtype: List of :py:class:`psyclone.psyGen.DynKernelArgument`.
        '''
        arg_names = []
        args = []
        for call in self.calls():
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
        for call in self.calls():
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
        if not self.is_openmp_parallel():
            calls = self.reductions()
            zero_reduction_variables(calls, parent)

        if self.root.opencl or (self._start == "1" and self._stop == "1"):
            # no need for a loop
            for child in self.children:
                child.gen_code(parent)
        else:
            from psyclone.f2pygen import DoGen, DeclGen
            do = DoGen(parent, self._variable_name, self._start, self._stop)
            # need to add do loop before children as children may want to add
            # info outside of do loop
            parent.add(do)
            for child in self.children:
                child.gen_code(do)
            my_decl = DeclGen(parent, datatype="integer",
                              entity_decls=[self._variable_name])
            parent.add(my_decl)


class Call(Node):
    '''
    Represents a call to a sub-program unit from within the PSy layer.

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

    def view(self, indent=0):
        '''
        Write out a textual summary of this Call node to stdout
        and then call the view() method of any children.

        :param indent: Depth of indent for output text
        :type indent: integer
        '''
        print(self.indent(indent) + self.coloured_text,
              self.name + "(" + self.arguments.names + ")")
        for entity in self._children:
            entity.view(indent=indent + 1)

    @property
    def coloured_text(self):
        ''' Return a string containing the (coloured) name of this node
        type '''
        return colored("Call", SCHEDULE_COLOUR_MAP["Call"])

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
        :returns: the name of the kernel associated with this call.
        :rtype: str
        '''
        return self._name

    @name.setter
    def name(self, value):
        '''
        Set the name of the kernel that this call is for.

        :param str value: The name of the kernel.
        '''
        self._name = value

    @property
    def iterates_over(self):
        return self._iterates_over

    def local_vars(self):
        raise NotImplementedError("Call.local_vars should be implemented")

    def __str__(self):
        raise NotImplementedError("Call.__str__ should be implemented")

    def gen_code(self, parent):
        raise NotImplementedError("Call.gen_code should be implemented")


class Kern(Call):
    '''
    Class representing a call to a PSyclone Kernel.

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
        Call.__init__(self, parent, call, call.ktype.procedure.name,
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
        if self._kern_schedule is None:
            astp = Fparser2ASTProcessor()
            self._kern_schedule = astp.generate_schedule(self.name, self.ast)
        return self._kern_schedule

    def __str__(self):
        return "kern call: "+self._name

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
        # check all kernels in the same invoke as this one and set any
        # with the same name to the same value as this one. This is
        # required as inlining (or not) affects all calls to the same
        # kernel within an invoke. Note, this will set this kernel as
        # well so there is no need to set it locally.
        if value and self._fp2_ast:
            # TODO #229. We take the existence of an fparser2 AST for
            # this kernel to mean that it has been transformed. Since
            # kernel in-lining is currently implemented via
            # manipulation of the fparser1 AST, there is at present no
            # way to inline such a kernel.
            raise NotImplementedError(
                "Cannot module-inline a transformed kernel ({0}).".
                format(self.name))
        my_schedule = self.ancestor(Schedule)
        for kernel in self.walk(my_schedule.children, Kern):
            if kernel.name == self.name:
                kernel._module_inline = value

    def view(self, indent=0):
        '''
        Write out a textual summary of this Kernel-call node to stdout
        and then call the view() method of any children.

        :param indent: Depth of indent for output text
        :type indent: integer
        '''
        print(self.indent(indent) + self.coloured_text,
              self.name + "(" + self.arguments.names + ")",
              "[module_inline=" + str(self._module_inline) + "]")
        for entity in self._children:
            entity.view(indent=indent + 1)

    @property
    def coloured_text(self):
        '''
        Return text containing the (coloured) name of this node type

        :returns: the name of this node type, possibly with control codes
                  for colour
        :rtype: string
        '''
        return colored("KernCall", SCHEDULE_COLOUR_MAP["KernCall"])

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

    def is_coloured(self):
        ''' Returns true if this kernel is being called from within a
        coloured loop '''
        return self.parent.loop_type == "colour"

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
        if not self.modified:
            return

        # Remove any "_mod" if the file follows the PSyclone naming convention
        orig_mod_name = self.module_name[:]
        if orig_mod_name.endswith("_mod"):
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
            new_suffix = "_{0}".format(name_idx)
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
        # within the AST of the kernel code
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

        # Generate the Fortran for this transformed kernel, ensuring that
        # we limit the line lengths
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

    def _rename_ast(self, suffix):
        '''
        Renames all quantities (module, kernel routine, kernel derived type)
        in the kernel AST by inserting the supplied suffix. The resulting
        names follow the PSyclone naming convention (modules end with "_mod",
        types with "_type" and kernels with "_code").

        :param str suffix: the string to insert into the quantity names.
        '''
        from fparser.two.utils import walk_ast
        from fparser.two import Fortran2003

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


class BuiltIn(Call):
    ''' Parent class for all built-ins (field operations for which the user
    does not have to provide a kernel). '''
    def __init__(self):
        # We cannot call Call.__init__ as don't have necessary information
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
        Call.__init__(self, parent, call, name, arguments)

    def local_vars(self):
        '''Variables that are local to this built-in and therefore need to be
        made private when parallelising using OpenMP or similar. By default
        builtin's do not have any local variables so set to nothing'''
        return []


class Arguments(object):
    '''
    Arguments abstract base class.

    :param parent_call: the call with which the arguments are associated.
    :type parent_call: sub-class of :py:class:`psyclone.psyGen.Call`
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
        argument can be found in a `Call` a `HaloExchange` or a \
        `GlobalSum` (or a subclass thereof)
        :type arg: :py:class:`psyclone.psyGen.Argument`

        '''
        # the `psyclone.psyGen.Argument` we are concerned with
        self._arg = arg
        # the call (Call, HaloExchange, or GlobalSum (or subclass)
        # instance to which the argument is associated
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
        '''Returns true if all of the data associated with this argument has
        been covered by the arguments provided in update_coverage

        :return bool: True if all of an argument is covered by \
        previous accesses and False if not.

        '''
        return self._covered


class Argument(object):
    ''' Argument base class '''

    def __init__(self, call, arg_info, access):
        '''
        :param call: the call that this argument is associated with
        :type call: :py:class:`psyclone.psyGen.Call`
        :param arg_info: Information about this argument collected by
        the parser
        :type arg_info: :py:class:`psyclone.parse.algorithm.Arg`
        :param access: the way in which this argument is accessed in
        the 'Call'. Valid values are specified in the config object
        of the current API.
        :type access: str

        '''
        self._call = call
        self._text = arg_info.text
        self._orig_name = arg_info.varname
        self._form = arg_info.form
        self._is_literal = arg_info.is_literal()
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
                           isinstance(x, (Call, HaloExchange, GlobalSum))]
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
                           isinstance(x, (Call, HaloExchange, GlobalSum))]
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
                           isinstance(x, (Call, GlobalSum)) or
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
    def apply(self, *args):
        '''Abstract method that applies the transformation. This function
        must be implemented by each transform.

        :param args: Arguments for the transformation - specific to\
                    the actual transform used.
        :type args: Type depends on actual transformation.
        :returns: A tuple of the new schedule, and a momento.
        :rtype: Tuple.
        '''
        # pylint: disable=no-self-use
        schedule = None
        momento = None
        return schedule, momento

    def _validate(self, *args):
        '''Method that validates that the input data is correct.
        It will raise exceptions if the input data is incorrect. This function
        needs to be implemented by each transformation.

        :param args: Arguments for the applying the transformation - specific\
                    to the actual transform used.
        :type args: Type depends on actual transformation.
        '''
        # pylint: disable=no-self-use, unused-argument
        return


class DummyTransformation(Transformation):
    '''Dummy transformation use elsewhere to keep pyreverse happy.'''
    def name(self):
        return

    def apply(self):
        return None, None


class IfBlock(Node):
    '''
    Class representing an if-block within the PSyIR. It has two mandatory
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

    @property
    def condition(self):
        ''' Return the PSyIR Node representing the conditional expression
        of this IfBlock.

        :return: IfBlock conditional expression.
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
        ''' Return children of the Schedule executed when the IfBlock
        evaluates to True.

        :return: Statements to be executed when IfBlock evaluates to True.
        :rtype: list of :py:class:`psyclone.psyGen.Node`
        :raises InternalError: If the IfBlock node does not have the correct \
            number of children.
        '''

        if len(self.children) < 2:
            raise InternalError(
                "IfBlock malformed or incomplete. It should have at least 2 "
                "children, but found {0}.".format(len(self.children)))

        return self._children[1]._children

    @property
    def else_body(self):
        ''' Return children of the Schedule executed when the IfBlock
        evaluates to False.

        :return: Statements to be executed when IfBlock evaluates to False.
        :rtype: list of :py:class:`psyclone.psyGen.Node`
        '''
        if len(self._children) == 3:
            return self._children[2]._children
        return []

    @property
    def coloured_text(self):
        '''
        Return text containing the (coloured) name of this node type.

        :returns: the name of this node type, possibly with control codes \
                  for colour.
        :rtype: str
        '''
        return colored("If", SCHEDULE_COLOUR_MAP["If"])

    def view(self, indent=0):
        '''
        Print representation of this node to stdout.

        :param int indent: the level to which to indent the output.
        '''
        print(self.indent(indent) + self.coloured_text + "[", end='')
        if self.annotations:
            print("annotations='" + ','.join(self.annotations) + "'", end='')
        print("]")
        for entity in self._children:
            entity.view(indent=indent + 1)

    def __str__(self):
        result = "If[]\n"
        for entity in self._children:
            result += str(entity)
        return result

    def gen_c_code(self, indent=0):
        '''
        Generate a string representation of this node using C language.

        :param int indent: Depth of indent for the output string.
        :return: C language code representing the node.
        :rtype: str
        :raises InternalError: If any mandatory children of the IfBlock \
            node are missing.
        '''
        if len(self.children) < 2:
            raise InternalError("IfBlock malformed or "
                                "incomplete. It should have at least 2 "
                                "children, but found {0}."
                                "".format(len(self.children)))

        retval = self.indent(indent) + "if ("
        retval += self.condition.gen_c_code() + ") {\n"
        for statement in self.if_body:
            retval += statement.gen_c_code(indent + 1) + "\n"

        if len(self.children) == 3:
            retval += self.indent(indent) + "} else {\n"
            for statement in self.else_body:
                retval += statement.gen_c_code(indent + 1) + "\n"

        retval += self.indent(indent) + "}\n"

        return retval


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

    def view(self, indent=0):
        '''
        Write out a textual summary of the OpenMP Parallel Do Directive
        and then call the view() method of any children.

        :param indent: Depth of indent for output text
        :type indent: integer
        '''
        print(self.indent(indent) + self.coloured_text +
              "[ACC Kernels]")
        for entity in self._children:
            entity.view(indent=indent + 1)

    def gen_code(self, _):
        '''
        :raises InternalError: the ACC Kernels directive is currently only \
                               supported for the NEMO API and that uses the \
                               update() method to alter the underlying \
                               fparser2 parse tree.
        '''
        raise InternalError(
            "ACCKernelsDirective.gen_code should not have been called.")

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

    def view(self, indent=0):
        '''
        Write out a textual summary of the OpenMP Parallel Do Directive
        and then call the view() method of any children.

        :param indent: Depth of indent for output text
        :type indent: integer
        '''
        print(self.indent(indent) + self.coloured_text +
              "[ACC DATA]")
        for entity in self._children:
            entity.view(indent=indent + 1)

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


class Fparser2ASTProcessor(object):
    '''
    Class to encapsulate the functionality for processing the fparser2 AST and
    convert the nodes to PSyIR.
    '''

    def __init__(self):
        from fparser.two import Fortran2003, utils
        # Map of fparser2 node types to handlers (which are class methods)
        self.handlers = {
            Fortran2003.Assignment_Stmt: self._assignment_handler,
            Fortran2003.Name: self._name_handler,
            Fortran2003.Parenthesis: self._parenthesis_handler,
            Fortran2003.Part_Ref: self._part_ref_handler,
            Fortran2003.If_Stmt: self._if_stmt_handler,
            utils.NumberBase: self._number_handler,
            utils.BinaryOpBase: self._binary_op_handler,
            Fortran2003.End_Do_Stmt: self._ignore_handler,
            Fortran2003.End_Subroutine_Stmt: self._ignore_handler,
            Fortran2003.If_Construct: self._if_construct_handler,
            Fortran2003.Case_Construct: self._case_construct_handler,
            Fortran2003.Return_Stmt: self._return_handler,
            Fortran2003.UnaryOpBase: self._unary_op_handler,
        }

    @staticmethod
    def nodes_to_code_block(parent, statements):
        '''
        Create a CodeBlock for the supplied list of statements
        and then wipe the list of statements. A CodeBlock is a node
        in the PSyIR (Schedule) that represents a sequence of one or more
        Fortran statements which PSyclone does not attempt to handle.

        :param parent: Node in the PSyclone AST to which to add this code \
                       block.
        :type parent: :py:class:`psyclone.psyGen.Node`
        :param list statements: List of fparser2 AST nodes constituting the \
                                code block.
        :rtype: :py:class:`psyclone.CodeBlock`
        '''
        if not statements:
            return None

        code_block = CodeBlock(statements, parent=parent)
        parent.addchild(code_block)
        del statements[:]
        return code_block

    @staticmethod
    def get_inputs_outputs(nodes):
        '''
        Identify variables that are inputs and outputs to the section of
        Fortran code represented by the supplied list of nodes in the
        fparser2 parse tree. Loop variables are ignored.

        :param nodes: list of Nodes in the fparser2 AST to analyse.
        :type nodes: list of :py:class:`fparser.two.utils.Base`

        :return: 3-tuple of list of inputs, list of outputs, list of in-outs
        :rtype: (list of str, list of str, list of str)
        '''
        from fparser.two.Fortran2003 import Assignment_Stmt, Part_Ref, \
            Data_Ref, If_Then_Stmt, Array_Section
        from fparser.two.utils import walk_ast
        readers = set()
        writers = set()
        readwrites = set()
        # A dictionary of all array accesses that we encounter - used to
        # sanity check the readers and writers we identify.
        all_array_refs = {}

        # Loop over a flat list of all the nodes in the supplied region
        for node in walk_ast(nodes):

            if isinstance(node, Assignment_Stmt):
                # Found lhs = rhs
                structure_name_str = None

                lhs = node.items[0]
                rhs = node.items[2]
                # Do RHS first as we cull readers after writers but want to
                # keep a = a + ... as the RHS is computed before assigning
                # to the LHS
                for node2 in walk_ast([rhs]):
                    if isinstance(node2, Part_Ref):
                        name = node2.items[0].string
                        if name.upper() not in FORTRAN_INTRINSICS:
                            if name not in writers:
                                readers.add(name)
                    if isinstance(node2, Data_Ref):
                        # TODO we need a robust implementation - issue #309.
                        raise NotImplementedError(
                            "get_inputs_outputs: derived-type references on "
                            "the RHS of assignments are not yet supported.")
                # Now do LHS
                if isinstance(lhs, Data_Ref):
                    # This is a structure which contains an array access.
                    structure_name_str = lhs.items[0].string
                    writers.add(structure_name_str)
                    lhs = lhs.items[1]
                if isinstance(lhs, (Part_Ref, Array_Section)):
                    # This is an array reference
                    name_str = lhs.items[0].string
                    if structure_name_str:
                        # Array ref is part of a derived type
                        name_str = "{0}%{1}".format(structure_name_str,
                                                    name_str)
                        structure_name_str = None
                    writers.add(name_str)
            elif isinstance(node, If_Then_Stmt):
                # Check for array accesses in IF statements
                array_refs = walk_ast([node], [Part_Ref])
                for ref in array_refs:
                    name = ref.items[0].string
                    if name.upper() not in FORTRAN_INTRINSICS:
                        if name not in writers:
                            readers.add(name)
            elif isinstance(node, Part_Ref):
                # Keep a record of all array references to check that we
                # haven't missed anything. Once #309 is done we should be
                # able to get rid of this check.
                name = node.items[0].string
                if name.upper() not in FORTRAN_INTRINSICS and \
                   name not in all_array_refs:
                    all_array_refs[name] = node
            elif node:
                # TODO #309 handle array accesses in other contexts, e.g. as
                # loop bounds in DO statements.
                pass

        # Sanity check that we haven't missed anything. To be replaced when
        # #309 is done.
        accesses = list(readers) + list(writers)
        for name, node in all_array_refs.items():
            if name not in accesses:
                # A matching bare array access hasn't been found but it
                # might have been part of a derived-type access so check
                # for that.
                found = False
                for access in accesses:
                    if "%"+name in access:
                        found = True
                        break
                if not found:
                    raise InternalError(
                        "Array '{0}' present in source code ('{1}') but not "
                        "identified as being read or written.".
                        format(name, str(node)))
        # Now we check for any arrays that are both read and written
        readwrites = readers & writers
        # Remove them from the readers and writers sets
        readers = readers - readwrites
        writers = writers - readwrites
        # Convert sets to lists and sort so that we get consistent results
        # between Python versions (for testing)
        rlist = list(readers)
        rlist.sort()
        wlist = list(writers)
        wlist.sort()
        rwlist = list(readwrites)
        rwlist.sort()

        return (rlist, wlist, rwlist)

    @staticmethod
    def _create_schedule(name):
        '''
        Create an empty KernelSchedule.

        :param str name: Name of the subroutine represented by the kernel.
        :returns: New KernelSchedule empty object.
        :rtype: py:class:`psyclone.psyGen.KernelSchedule`
        '''
        return KernelSchedule(name)

    def generate_schedule(self, name, module_ast):
        '''
        Create a KernelSchedule from the supplied fparser2 AST.

        :param str name: Name of the subroutine represented by the kernel.
        :param module_ast: fparser2 AST of the full module where the kernel \
                           code is located.
        :type module_ast: :py:class:`fparser.two.Fortran2003.Program`
        :raises GenerationError: Unable to generate a kernel schedule from the
                                 provided fpaser2 parse tree.
        '''
        from fparser.two import Fortran2003

        def first_type_match(nodelist, typekind):
            '''
            Returns the first instance of the specified type in the given
            node list.

            :param list nodelist: List of fparser2 nodes.
            :param type typekind: The fparse2 Type we are searching for.
            '''
            for node in nodelist:
                if isinstance(node, typekind):
                    return node
            raise ValueError  # Type not found

        def search_subroutine(nodelist, searchname):
            '''
            Returns the first instance of the specified subroutine in the given
            node list.

            :param list nodelist: List of fparser2 nodes.
            :param str searchname: Name of the subroutine we are searching for.
            '''
            for node in nodelist:
                if (isinstance(node, Fortran2003.Subroutine_Subprogram) and
                        str(node.content[0].get_name()) == searchname):
                    return node
            raise ValueError  # Subroutine not found

        new_schedule = self._create_schedule(name)

        # Assume just 1 Fortran module definition in the file
        if len(module_ast.content) > 1:
            raise GenerationError("Unexpected AST when generating '{0}' "
                                  "kernel schedule. Just one "
                                  "module definition per file supported."
                                  "".format(name))

        # TODO: Metadata can be also accessed for validation (issue #288)

        try:
            mod_content = module_ast.content[0].content
            subroutines = first_type_match(mod_content,
                                           Fortran2003.Module_Subprogram_Part)
            subroutine = search_subroutine(subroutines.content, name)
        except (ValueError, IndexError):
            raise GenerationError("Unexpected kernel AST. Could not find "
                                  "subroutine: {0}".format(name))

        try:
            sub_spec = first_type_match(subroutine.content,
                                        Fortran2003.Specification_Part)
            decl_list = sub_spec.content
            arg_list = subroutine.content[0].items[2].items
        except ValueError:
            # Subroutine without declarations, continue with empty lists.
            decl_list = []
            arg_list = []
        except (IndexError, AttributeError):
            # Subroutine without argument list, continue with empty list.
            arg_list = []
        finally:
            self.process_declarations(new_schedule, decl_list, arg_list)

        try:
            sub_exec = first_type_match(subroutine.content,
                                        Fortran2003.Execution_Part)
        except ValueError:
            pass
        else:
            self.process_nodes(new_schedule, sub_exec.content, sub_exec)

        return new_schedule

    @staticmethod
    def _parse_dimensions(dimensions, symbol_table):
        '''
        Parse the fparser dimension attribute into a shape list with
        the extent of each dimension.

        :param dimensions: fparser dimension attribute
        :type dimensions: \
            :py:class:`fparser.two.Fortran2003.Dimension_Attr_Spec`
        :param symbol_table: Symbol table of the declaration context.
        :type symbol_table: :py:class:`psyclone.psyGen.SymbolTable`
        :returns: Shape of the attribute in column-major order (leftmost \
                  index is contiguous in memory). Each entry represents \
                  an array dimension. If it is 'None' the extent of that \
                  dimension is unknown, otherwise it holds an integer \
                  with the extent. If it is an empty list then the symbol \
                  represents a scalar.
        :rtype: list
        '''
        from fparser.two.utils import walk_ast
        from fparser.two import Fortran2003
        shape = []

        # Traverse shape specs in Depth-first-search order
        for dim in walk_ast([dimensions], [Fortran2003.Assumed_Shape_Spec,
                                           Fortran2003.Explicit_Shape_Spec,
                                           Fortran2003.Assumed_Size_Spec]):

            if isinstance(dim, Fortran2003.Assumed_Size_Spec):
                raise NotImplementedError(
                    "Could not process {0}. Assumed-size arrays"
                    " are not supported.".format(dimensions))

            elif isinstance(dim, Fortran2003.Assumed_Shape_Spec):
                shape.append(None)

            elif isinstance(dim, Fortran2003.Explicit_Shape_Spec):
                def _unsupported_type_error(dimensions):
                    raise NotImplementedError(
                        "Could not process {0}. Only scalar integer literals"
                        " or symbols are supported for explicit shape array "
                        "declarations.".format(dimensions))
                if isinstance(dim.items[1],
                              Fortran2003.Int_Literal_Constant):
                    shape.append(int(dim.items[1].items[0]))
                elif isinstance(dim.items[1], Fortran2003.Name):
                    sym = symbol_table.lookup(dim.items[1].string)
                    if sym.datatype != 'integer' or sym.shape:
                        _unsupported_type_error(dimensions)
                    shape.append(sym)
                else:
                    _unsupported_type_error(dimensions)

            else:
                raise InternalError(
                    "Reached end of loop body and {0} has"
                    " not been handled.".format(type(dim)))

        return shape

    def process_declarations(self, parent, nodes, arg_list):
        '''
        Transform the variable declarations in the fparser2 parse tree into
        symbols in the PSyIR parent node symbol table.

        :param parent: PSyIR node in which to insert the symbols found.
        :type parent: :py:class:`psyclone.psyGen.KernelSchedule`
        :param nodes: fparser2 AST nodes to search for declaration statements.
        :type nodes: list of :py:class:`fparser.two.utils.Base`
        :param arg_list: fparser2 AST node containing the argument list.
        :type arg_list: :py:class:`fparser.Fortran2003.Dummy_Arg_List`
        :raises NotImplementedError: The provided declarations contain
                                     attributes which are not supported yet.
        '''
        from fparser.two.utils import walk_ast
        from fparser.two import Fortran2003

        def iterateitems(nodes):
            '''
            At the moment fparser nodes can be of type None, a single element
            or a list of elements. This helper function provide a common
            iteration interface. This could be improved when fpaser/#170 is
            fixed.
            :param nodes: fparser2 AST node.
            :type nodes: None or List or :py:class:`fparser.two.utils.Base`
            :returns: Returns nodes but always encapsulated in a list
            :rtype: list
            '''
            if nodes is None:
                return []
            if type(nodes).__name__.endswith("_List"):
                return nodes.items
            return [nodes]

        for decl in walk_ast(nodes, [Fortran2003.Type_Declaration_Stmt]):
            (type_spec, attr_specs, entities) = decl.items

            # Parse type_spec, currently just 'real', 'integer', 'logical' and
            # 'character' intrinsic types are supported.
            datatype = None
            if isinstance(type_spec, Fortran2003.Intrinsic_Type_Spec):
                if str(type_spec.items[0]).lower() == 'real':
                    datatype = 'real'
                elif str(type_spec.items[0]).lower() == 'integer':
                    datatype = 'integer'
                elif str(type_spec.items[0]).lower() == 'character':
                    datatype = 'character'
                elif str(type_spec.items[0]).lower() == 'logical':
                    datatype = 'boolean'
            if datatype is None:
                raise NotImplementedError(
                    "Could not process {0}. Only 'real', 'integer', "
                    "'logical' and 'character' intrinsic types are "
                    "supported.".format(str(decl.items)))

            # Parse declaration attributes:
            # 1) If no dimension attribute is provided, it defaults to scalar.
            attribute_shape = []
            # 2) If no intent attribute is provided, it is provisionally
            # marked as a local variable (when the argument list is parsed,
            # arguments with no explicit intent are updated appropriately).
            scope = 'local'
            is_input = False
            is_output = False
            for attr in iterateitems(attr_specs):
                if isinstance(attr, Fortran2003.Attr_Spec):
                    normalized_string = str(attr).lower().replace(' ', '')
                    if "intent(in)" in normalized_string:
                        scope = 'global_argument'
                        is_input = True
                    elif "intent(out)" in normalized_string:
                        scope = 'global_argument'
                        is_output = True
                    elif "intent(inout)" in normalized_string:
                        scope = 'global_argument'
                        is_input = True
                        is_output = True
                    else:
                        raise NotImplementedError(
                            "Could not process {0}. Unrecognized attribute "
                            "'{1}'.".format(decl.items, str(attr)))
                elif isinstance(attr, Fortran2003.Dimension_Attr_Spec):
                    attribute_shape = \
                        self._parse_dimensions(attr, parent.symbol_table)
                else:
                    raise NotImplementedError(
                        "Could not process {0}. Unrecognized attribute "
                        "type {1}.".format(decl.items, str(type(attr))))

            # Parse declarations RHS and declare new symbol into the
            # parent symbol table for each entity found.
            for entity in iterateitems(entities):
                (name, array_spec, char_len, initialisation) = entity.items

                # If the entity has an array-spec shape, it has priority.
                # Otherwise use the declaration attribute shape.
                if array_spec is not None:
                    entity_shape = \
                        self._parse_dimensions(array_spec, parent.symbol_table)
                else:
                    entity_shape = attribute_shape

                if initialisation is not None:
                    raise NotImplementedError(
                        "Could not process {0}. Initialisations on the"
                        " declaration statements are not supported."
                        "".format(decl.items))

                if char_len is not None:
                    raise NotImplementedError(
                        "Could not process {0}. Character length "
                        "specifications are not supported."
                        "".format(decl.items))

                parent.symbol_table.declare(
                    str(name), datatype, entity_shape, scope, is_input,
                    is_output)

        try:
            arg_strings = [x.string for x in arg_list]
            parent.symbol_table.specify_argument_list(arg_strings)
        except KeyError:
            raise InternalError("The kernel argument "
                                "list '{0}' does not match the variable "
                                "declarations for fparser nodes {1}."
                                "".format(str(arg_list), nodes))

    # TODO remove nodes_parent argument once fparser2 AST contains
    # parent information (fparser/#102).
    def process_nodes(self, parent, nodes, nodes_parent):
        '''
        Create the PSyIR of the supplied list of nodes in the
        fparser2 AST. Currently also inserts parent information back
        into the fparser2 AST. This is a workaround until fparser2
        itself generates and stores this information.

        :param parent: Parent node in the PSyIR we are constructing.
        :type parent: :py:class:`psyclone.psyGen.Node`
        :param nodes: List of sibling nodes in fparser2 AST.
        :type nodes: list of :py:class:`fparser.two.utils.Base`
        :param nodes_parent: the parent of the supplied list of nodes in \
                             the fparser2 AST.
        :type nodes_parent: :py:class:`fparser.two.utils.Base`
        '''
        code_block_nodes = []
        for child in nodes:
            # TODO remove this line once fparser2 contains parent
            # information (fparser/#102)
            child._parent = nodes_parent  # Retro-fit parent info

            try:
                psy_child = self._create_child(child, parent)
            except NotImplementedError:
                # If child type implementation not found, add them on the
                # ongoing code_block node list.
                code_block_nodes.append(child)
            else:
                if psy_child:
                    self.nodes_to_code_block(parent, code_block_nodes)
                    parent.addchild(psy_child)
                # If psy_child is not initialised but it didn't produce a
                # NotImplementedError, it means it is safe to ignore it.

        # Complete any unfinished code-block
        self.nodes_to_code_block(parent, code_block_nodes)

    def _create_child(self, child, parent=None):
        '''
        Create a PSyIR node representing the supplied fparser 2 node.

        :param child: node in fparser2 AST.
        :type child: :py:class:`fparser.two.utils.Base`
        :param parent: Parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyGen.Node`
        :raises NotImplementedError: There isn't a handler for the provided \
                child type.
        :returns: Returns the PSyIR representation of child, which can be a \
                  single node, a tree of nodes or None if the child can be \
                  ignored.
        :rtype: :py:class:`psyclone.psyGen.Node` or NoneType
        '''
        handler = self.handlers.get(type(child))
        if handler is None:
            # If the handler is not found then check with the first
            # level parent class. This is done to simplify the
            # handlers map when multiple fparser2 types can be
            # processed with the same handler. (e.g. Subclasses of
            # BinaryOpBase: Mult_Operand, Add_Operand, Level_2_Expr,
            # ... can use the same handler.)
            generic_type = type(child).__bases__[0]
            handler = self.handlers.get(generic_type)
            if not handler:
                raise NotImplementedError()
        return handler(child, parent)

    def _ignore_handler(self, *_):
        '''
        This handler returns None indicating that the associated
        fparser2 node can be ignored.

        Note that this method contains ignored arguments to comform with
        the handler(node, parent) method interface.

        :returns: None
        :rtype: NoneType
        '''
        return None

    def _if_construct_handler(self, node, parent):
        '''
        Transforms an fparser2 If_Construct to the PSyIR representation.

        :param node: node in fparser2 tree.
        :type node: :py:class:`fparser.two.Fortran2003.If_Construct`
        :param parent: Parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyGen.Node`
        :returns: PSyIR representation of node
        :rtype: :py:class:`psyclone.psyGen.IfBlock`
        :raises InternalError: If the fparser2 tree has an unexpected \
            structure.
        '''
        from fparser.two import Fortran2003

        # Check that the fparser2 parsetree has the expected structure
        if not isinstance(node.content[0], Fortran2003.If_Then_Stmt):
            raise InternalError(
                "Failed to find opening if then statement in: "
                "{0}".format(str(node)))
        if not isinstance(node.content[-1], Fortran2003.End_If_Stmt):
            raise InternalError(
                "Failed to find closing end if statement in: "
                "{0}".format(str(node)))

        # Search for all the conditional clauses in the If_Construct
        clause_indices = []
        for idx, child in enumerate(node.content):
            child._parent = node  # Retrofit parent info
            if isinstance(child, (Fortran2003.If_Then_Stmt,
                                  Fortran2003.Else_Stmt,
                                  Fortran2003.Else_If_Stmt,
                                  Fortran2003.End_If_Stmt)):
                clause_indices.append(idx)

        # Deal with each clause: "if", "else if" or "else".
        ifblock = None
        currentparent = parent
        num_clauses = len(clause_indices) - 1
        for idx in range(num_clauses):
            start_idx = clause_indices[idx]
            end_idx = clause_indices[idx+1]
            clause = node.content[start_idx]

            if isinstance(clause, (Fortran2003.If_Then_Stmt,
                                   Fortran2003.Else_If_Stmt)):
                # If it's an 'IF' clause just create an IfBlock, otherwise
                # it is an 'ELSE' clause and it needs an IfBlock annotated
                # with 'was_elseif' inside a Schedule.
                newifblock = None
                if isinstance(clause, Fortran2003.If_Then_Stmt):
                    ifblock = IfBlock(parent=currentparent)
                    ifblock.ast = node  # Keep pointer to fpaser2 AST
                    newifblock = ifblock
                else:
                    elsebody = Schedule(parent=currentparent)
                    currentparent.addchild(elsebody)
                    newifblock = IfBlock(parent=elsebody,
                                         annotation='was_elseif')
                    elsebody.addchild(newifblock)

                    # Keep pointer to fpaser2 AST
                    elsebody.ast = node.content[start_idx]
                    newifblock.ast = node.content[start_idx]

                # Create condition as first child
                self.process_nodes(parent=newifblock,
                                   nodes=[clause.items[0]],
                                   nodes_parent=node)

                # Create if-body as second child
                ifbody = Schedule(parent=ifblock)
                ifbody.ast = node.content[start_idx + 1]
                ifbody.ast_end = node.content[end_idx - 1]
                newifblock.addchild(ifbody)
                self.process_nodes(parent=ifbody,
                                   nodes=node.content[start_idx + 1:end_idx],
                                   nodes_parent=node)

                currentparent = newifblock

            elif isinstance(clause, Fortran2003.Else_Stmt):
                if not idx == num_clauses - 1:
                    raise InternalError(
                        "Else clause should only be found next to last "
                        "clause, but found {0}".format(node.content))
                elsebody = Schedule(parent=currentparent)
                currentparent.addchild(elsebody)
                elsebody.ast = node.content[start_idx]
                elsebody.ast_end = node.content[end_idx]
                self.process_nodes(parent=elsebody,
                                   nodes=node.content[start_idx + 1:end_idx],
                                   nodes_parent=node)
            else:
                raise InternalError(
                    "Only fparser2 If_Then_Stmt, Else_If_Stmt and Else_Stmt "
                    "are expected, but found {0}.".format(clause))

        return ifblock

    def _if_stmt_handler(self, node, parent):
        '''
        Transforms an fparser2 If_Stmt to the PSyIR representation.

        :param node: node in fparser2 AST.
        :type node: :py:class:`fparser.two.Fortran2003.If_Stmt`
        :param parent: Parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyGen.Node`
        :returns: PSyIR representation of node
        :rtype: :py:class:`psyclone.psyGen.IfBlock`
        '''
        ifblock = IfBlock(parent=parent, annotation='was_single_stmt')
        ifblock.ast = node
        self.process_nodes(parent=ifblock, nodes=[node.items[0]],
                           nodes_parent=node)
        ifbody = Schedule(parent=ifblock)
        ifblock.addchild(ifbody)
        self.process_nodes(parent=ifbody, nodes=[node.items[1]],
                           nodes_parent=node)
        return ifblock

    def _case_construct_handler(self, node, parent):
        '''
        Transforms an fparser2 Case_Construct to the PSyIR representation.

        :param node: node in fparser2 tree.
        :type node: :py:class:`fparser.two.Fortran2003.Case_Construct`
        :param parent: Parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyGen.Node`
        :returns: PSyIR representation of node
        :rtype: :py:class:`psyclone.psyGen.IfBlock`
        :raises InternalError: If the fparser2 tree has an unexpected \
            structure.
        :raises NotImplementedError: If the fparser2 tree contains an \
            unsupported structure and should be placed in a CodeBlock.
        '''
        from fparser.two import Fortran2003
        # Check that the fparser2 parsetree has the expected structure
        if not isinstance(node.content[0], Fortran2003.Select_Case_Stmt):
            raise InternalError(
                "Failed to find opening case statement in: "
                "{0}".format(str(node)))
        if not isinstance(node.content[-1], Fortran2003.End_Select_Stmt):
            raise InternalError(
                "Failed to find closing case statement in: "
                "{0}".format(str(node)))

        # Search for all the CASE clauses in the Case_Construct
        clause_indices = []
        selector = None
        for idx, child in enumerate(node.content):
            child._parent = node  # Retrofit parent info
            if isinstance(child, Fortran2003.Select_Case_Stmt):
                selector = child.items[0]
            if isinstance(child, Fortran2003.Case_Stmt):
                # Case Default and value Ranges not supported yet, if found
                # we raise a NotImplementedError that the process_node() will
                # catch and generate a CodeBlock instead.
                case_expression = child.items[0].items[0]
                if isinstance(case_expression,
                              (Fortran2003.Case_Value_Range,
                               Fortran2003.Case_Value_Range_List)):
                    raise NotImplementedError("Case Value Range Statement")
                elif case_expression is None:
                    raise NotImplementedError("Case Default Statement")
                clause_indices.append(idx)
            if isinstance(child, Fortran2003.End_Select_Stmt):
                clause_indices.append(idx)

        # Deal with each Case_Stmt
        rootif = None
        currentparent = parent
        num_clauses = len(clause_indices) - 1
        for idx in range(num_clauses):
            start_idx = clause_indices[idx]
            end_idx = clause_indices[idx+1]
            clause = node.content[start_idx]

            if isinstance(clause, Fortran2003.Case_Stmt):
                case = clause.items[0]
                if isinstance(case, Fortran2003.Case_Selector):
                    ifblock = IfBlock(parent=currentparent,
                                      annotation='was_case')
                    ifblock.ast = node.content[start_idx]
                    ifblock.ast_end = node.content[end_idx - 1]

                    # Add condition: selector == case
                    bop = BinaryOperation(parent=ifblock, operator='==')
                    self.process_nodes(parent=bop,
                                       nodes=[selector],
                                       nodes_parent=node)
                    self.process_nodes(parent=bop,
                                       nodes=[case.items[0]],
                                       nodes_parent=node)
                    ifblock.addchild(bop)

                    # Add If_body
                    ifbody = Schedule(parent=ifblock)
                    self.process_nodes(parent=ifbody,
                                       nodes=node.content[start_idx + 1:
                                                          end_idx],
                                       nodes_parent=node)
                    ifblock.addchild(ifbody)
                    ifbody.ast = node.content[start_idx + 1]
                    ifbody.ast_end = node.content[end_idx - 1]

                    if rootif:
                        # If rootif is already initialised we chain the new
                        # case in the last else branch.
                        elsebody = Schedule(parent=currentparent)
                        currentparent.addchild(elsebody)
                        elsebody.addchild(ifblock)
                        elsebody.ast = node.content[start_idx]
                        elsebody.ast_end = node.content[end_idx]
                    else:
                        rootif = ifblock

                    currentparent = ifblock

        return rootif

    def _return_handler(self, _, parent):
        '''
        Transforms an fparser2 Return_Stmt to the PSyIR representation.

        Note that this method contains ignored arguments to comform with
        the handler(node, parent) method interface.

        :param parent: Parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyGen.Node`
        :return: PSyIR representation of node
        :rtype: :py:class:`psyclone.psyGen.Return`
        '''
        return Return(parent=parent)

    def _assignment_handler(self, node, parent):
        '''
        Transforms an fparser2 Assignment_Stmt to the PSyIR representation.

        :param node: node in fparser2 AST.
        :type node: :py:class:`fparser.two.Fortran2003.Assignment_Stmt`
        :param parent: Parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyGen.Node`

        :returns: PSyIR representation of node.
        :rtype: :py:class:`psyclone.psyGen.Assignment`
        '''
        assignment = Assignment(node, parent=parent)
        self.process_nodes(parent=assignment, nodes=[node.items[0]],
                           nodes_parent=node)
        self.process_nodes(parent=assignment, nodes=[node.items[2]],
                           nodes_parent=node)

        return assignment

    def _unary_op_handler(self, node, parent):
        '''
        Transforms an fparser2 UnaryOpBase to the PSyIR representation.

        :param node: node in fparser2 AST.
        :type node: :py:class:`fparser.two.utils.UnaryOpBase`
        :param parent: Parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyGen.Node`
        :return: PSyIR representation of node
        :rtype: :py:class:`psyclone.psyGen.UnaryOperation`
        '''
        # Get the operator
        operator = node.items[0]

        unary_op = UnaryOperation(operator, parent=parent)
        self.process_nodes(parent=unary_op, nodes=[node.items[1]],
                           nodes_parent=node)

        return unary_op

    def _binary_op_handler(self, node, parent):
        '''
        Transforms an fparser2 BinaryOp to the PSyIR representation.

        :param node: node in fparser2 AST.
        :type node: :py:class:`fparser.two.utils.BinaryOpBase`
        :param parent: Parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyGen.Node`
        :returns: PSyIR representation of node
        :rtype: :py:class:`psyclone.psyGen.BinaryOperation`
        '''
        # Get the operator
        operator = node.items[1]

        binary_op = BinaryOperation(operator, parent=parent)
        self.process_nodes(parent=binary_op, nodes=[node.items[0]],
                           nodes_parent=node)
        self.process_nodes(parent=binary_op, nodes=[node.items[2]],
                           nodes_parent=node)

        return binary_op

    def _name_handler(self, node, parent):
        '''
        Transforms an fparser2 Name to the PSyIR representation.

        :param node: node in fparser2 AST.
        :type node: :py:class:`fparser.two.Fortran2003.Name`
        :param parent: Parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyGen.Node`
        :returns: PSyIR representation of node
        :rtype: :py:class:`psyclone.psyGen.Reference`
        '''
        return Reference(node.string, parent)

    def _parenthesis_handler(self, node, parent):
        '''
        Transforms an fparser2 Parenthesis to the PSyIR representation.
        This means ignoring the parentheis and process the fparser2 children
        inside.

        :param node: node in fparser2 AST.
        :type node: :py:class:`fparser.two.Fortran2003.Parenthesis`
        :param parent: Parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyGen.Node`
        :returns: PSyIR representation of node
        :rtype: :py:class:`psyclone.psyGen.Node`
        '''
        # Use the items[1] content of the node as it contains the required
        # information (items[0] and items[2] just contain the left and right
        # brackets as strings so can be disregarded.
        return self._create_child(node.items[1], parent)

    def _part_ref_handler(self, node, parent):
        '''
        Transforms an fparser2 Part_Ref to the PSyIR representation.

        :param node: node in fparser2 AST.
        :type node: :py:class:`fparser.two.Fortran2003.Part_Ref`
        :param parent: Parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyGen.Node`
        :returns: PSyIR representation of node
        :rtype: :py:class:`psyclone.psyGen.Array`
        '''
        from fparser.two import Fortran2003

        reference_name = node.items[0].string
        array = Array(reference_name, parent)

        if isinstance(node.items[1], Fortran2003.Section_Subscript_List):
            subscript_list = node.items[1].items

            self.process_nodes(parent=array, nodes=subscript_list,
                               nodes_parent=node.items[1])
        else:
            # When there is only one dimension fparser does not have
            # a Subscript_List
            self.process_nodes(parent=array, nodes=[node.items[1]],
                               nodes_parent=node)

        return array

    def _number_handler(self, node, parent):
        '''
        Transforms an fparser2 NumberBase to the PSyIR representation.

        :param node: node in fparser2 AST.
        :type node: :py:class:`fparser.two.utils.NumberBase`
        :param parent: Parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyGen.Node`
        :returns: PSyIR representation of node
        :rtype: :py:class:`psyclone.psyGen.Literal`
        '''
        return Literal(str(node.items[0]), parent=parent)


class Symbol(object):
    '''
    Symbol item for the Symbol Table. It contains information about: the name,
    the datatype, the shape (in column-major order), the scope and for
    global-scoped symbols whether the data is already defined and/or survives
    after the kernel.

    :param str name: Name of the symbol.
    :param str datatype: Data type of the symbol.
    :param list shape: Shape of the symbol in column-major order (leftmost \
                       index is contiguous in memory). Each entry represents \
                       an array dimension. If it is 'None' the extent of that \
                       dimension is unknown, otherwise it holds an integer \
                       literal or a reference to an integer symbol with the \
                       extent. If it is an empty list then the symbol \
                       represents a scalar.
    :param str scope: It is 'local' if the symbol just exists inside the \
                      kernel scope or 'global_*' if the data survives outside \
                      of the kernel scope. Note that global-scoped symbols \
                      also have postfixed information about the sharing \
                      mechanism, at the moment just 'global_argument' is \
                      available for variables passed in/out of the kernel \
                      by argument.
    :param bool is_input: Whether the symbol represents data that exists \
                          before the kernel is entered and that is passed \
                          into the kernel.
    :param bool is_output: Whether the symbol represents data that is passed \
                           outside the kernel upon exit.
    :raises NotImplementedError: Provided parameters are not supported yet.
    :raises TypeError: Provided parameters have invalid error type.
    :raises ValueError: Provided parameters contain invalid values.
    '''

    # Tuple with the valid values for the access attribute.
    valid_scope_types = ('local', 'global_argument')
    # Tuple with the valid datatypes.
    valid_data_types = ('real', 'integer', 'character', 'boolean')

    def __init__(self, name, datatype, shape=[], scope='local',
                 is_input=False, is_output=False):

        self._name = name

        if datatype not in Symbol.valid_data_types:
            raise NotImplementedError(
                "Symbol can only be initialised with {0} datatypes."
                "".format(str(Symbol.valid_data_types)))
        self._datatype = datatype

        if not isinstance(shape, list):
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
        self._scope = None
        self._is_input = None
        self._is_output = None
        self.scope = scope
        self.is_input = is_input
        self.is_output = is_output

    @property
    def name(self):
        '''
        :returns: Name of the Symbol.
        :rtype: string
        '''
        return self._name

    @property
    def datatype(self):
        '''
        :returns: Datatype of the Symbol.
        :rtype: string
        '''
        return self._datatype

    @property
    def is_input(self):
        '''
        :returns: Whether the symbol represents data that already exists \
                  before kernel and is passed into upon entry.
        :rtype: bool
        '''
        return self._is_input

    @is_input.setter
    def is_input(self, new_is_input):
        '''
        :param bool new_is_input: Whether the symbol represents data that \
                                  exists before the kernel is entered and \
                                  that is passed into the kernel.
        :raises TypeError: Provided parameters have invalid error type.
        :raises ValueError: 'new_is_input' contains an invalid value.
        '''
        if not isinstance(new_is_input, bool):
            raise TypeError("Symbol 'is_input' attribute must be a boolean.")
        if self.scope == 'local' and new_is_input is True:
            raise ValueError("Symbol with 'local' scope can not have "
                             "'is_input' attribute set to True.")
        self._is_input = new_is_input

    @property
    def is_output(self):
        '''
        :returns: Whether the variable respresented by this symbol survives \
                  outside the kernel upon exit.
        :rtype: bool
        '''
        return self._is_output

    @is_output.setter
    def is_output(self, new_is_output):
        '''
        :param bool new_is_output: Whether the variable represented by this \
                                   symbol survives outside the kernel \
                                   upon exit.
        :raises TypeError: Provided parameters have invalid error type.
        :raises ValueError: 'new_is_output' contains an invalid value.
        '''
        if not isinstance(new_is_output, bool):
            raise TypeError("Symbol 'is_output' attribute must be a boolean.")
        if self.scope == 'local' and new_is_output is True:
            raise ValueError("Symbol with 'local' scope can not have "
                             "'is_output' attribute set to True.")
        self._is_output = new_is_output

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
                  kernel scope) or 'global_*' (data also lives outside the \
                  kernel). Global-scoped symbols also have postfixed \
                  information about the sharing mechanism, at the moment \
                  just 'global_argument' is available for variables passed \
                  in/out of the kernel by argument.
        :rtype: str
        '''
        return self._scope

    @scope.setter
    def scope(self, new_scope):
        '''
        :param str scope: It is 'local' if the symbol just exists inside the \
                          kernel scope or 'global_*' if the data survives \
                          outside of the kernel scope. Note that \
                          global-scoped symbols also have postfixed \
                          information about the sharing mechanism, at the \
                          moment just 'global_argument' is available for \
                          variables passed in/out of the kernel by argument.
        :raises ValueError: New scope parameter has an invalid value.
        '''
        if new_scope not in Symbol.valid_scope_types:
            raise ValueError("Symbol scope attribute can only be one of {0}"
                             " but got '{1}'."
                             "".format(str(Symbol.valid_scope_types),
                                       str(new_scope)))

        self._scope = new_scope

    def gen_c_definition(self):
        '''
        Generates string representing the C language definition of the symbol.

        :returns: The C definition of the symbol.
        :rtype: str
        :raises NotImplementedError: if there are some symbol types or nodes \
                                     which are not implemented yet.
        '''
        code = ""
        if self.datatype == "real":
            code = code + "double "
        elif self.datatype == "integer":
            code = code + "int "
        elif self.datatype == "character":
            code = code + "char "
        elif self.datatype == "boolean":
            code = code + "bool "
        else:
            raise NotImplementedError(
                "Could not generate the C definition for the variable '{0}', "
                "type '{1}' is currently not supported."
                "".format(self.name, self.datatype))

        # If the argument is an array, in C language we define it
        # as an unaliased pointer.
        if self.shape:
            code += "* restrict "

        code += self.name
        return code

    def __str__(self):
        ret = self.name + ": <" + self.datatype + ", " + self.scope + ", "
        if self.shape:
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
        return ret + ">"


class SymbolTable(object):
    '''
    Encapsulates the symbol table and provides methods to declare new symbols
    and look up existing symbols. It is implemented as a single scope
    symbol table (nested scopes not supported).

    :param kernel: Reference to the KernelSchedule to which this symbol table \
        belongs.
    :type kernel: :py:class:`psyclone.psyGen.KernelSchedule` or NoneType
    '''
    # TODO: (Issue #321) Explore how the SymbolTable overlaps with the
    # NameSpace class functionality.
    def __init__(self, kernel=None):
        # Dict of Symbol objects with the symbol names as keys.
        self._symbols = {}
        # Ordered list of the arguments.
        self._argument_list = []
        # Reference to KernelSchedule to which this symbol table belongs.
        self._kernel = kernel

    def declare(self, name, datatype, shape=[], scope='local',
                is_input=False, is_output=False):
        '''
        Declare a new symbol in the symbol table.

        :param str name: Name of the symbol.
        :param str datatype: Datatype of the symbol.
        :param list shape: Shape of the symbol in column-major order \
                           (leftmost index is contiguous in memory). Each \
                           entry represents an array dimension. If it is \
                           'None' the extent of that dimension is unknown, \
                           otherwise it holds an integer literal or a \
                           reference to an integer symbol with the extent. \
                           If it is an empty list then the symbol represents \
                           a scalar.
        :param str scope: It is 'local' if the symbol just exists inside the \
                          kernel scope or 'global_*' if the data survives \
                          outside of the kernel scope. Note that \
                          global-scoped symbols also have postfixed \
                          information about the sharing mechanism, at the \
                          moment just 'global_argument' is available for \
                          variables passed in/out of the kernel by argument.
        :param bool is_input: Whether the symbol represents data that exists \
                              before the kernel is entered and that is passed \
                              into the kernel.
        :param bool is_output: Whether the symbol represents data that is \
                               survives outside the kernel upon exit.
        :raises KeyError: The provided name can not be used as key in the
                          table.
        '''
        if name in self._symbols:
            raise KeyError("Symbol table already contains a symbol with"
                           " name '{0}'.".format(name))

        self._symbols[name] = Symbol(name, datatype, shape, scope, is_input,
                                     is_output)

    def specify_argument_list(self, argument_name_list):
        '''
        Keep track of the order of the arguments and provide the scope,
        is_input and is_ouput information if it was not available on the
        variable declaration.

        :param list argument_name_list: Ordered list of the argument names.
        '''
        self._argument_list = []
        for name in argument_name_list:
            symbol = self.lookup(name)
            # Declarations without explicit intent are provisionally identified
            # as 'local', but if they appear in the argument list the scope and
            # input/output attributes need to be updated.
            if symbol.scope == 'local':
                symbol.scope = 'global_argument'
                symbol.is_input = True
                symbol.is_output = True
            self._argument_list.append(symbol)

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
        :returns: Ordered list of arguments.
        :rtype: list of :py:class:`psyclone.psyGen.Symbol`
        '''
        return self._argument_list

    @property
    def local_symbols(self):
        '''
        :returns:  List of local symbols.
        :rtype: list of :py:class:`psyclone.psyGen.Symbol`
        '''
        return [sym for sym in self._symbols.values() if sym.scope == "local"]

    def gen_c_local_variables(self, indent=0):
        '''
        Generate C code that defines all local symbols in the Symbol Table.

        :param int indent: Indentation level
        :returns: C languague definition of the local symbols.
        :rtype: str
        '''
        code = ""
        for symbol in self.local_symbols:
            code += Node.indent(indent) + symbol.gen_c_definition() + ";\n"
        return code

    def gen_ocl_argument_list(self, indent=0):
        '''
        Generate OpenCL argument list.

        :raises NotImplementedError: is an abstract method.
        '''
        raise NotImplementedError(
            "A generic implementation of this method is not available.")

    def gen_ocl_iteration_indices(self, indent=0):
        '''
        Generate OpenCL iteration indices declaration.

        :raises NotImplementedError: is an abstract method.
        '''
        raise NotImplementedError(
            "A generic implementation of this method is not available.")

    def gen_ocl_array_length(self, indent=0):
        '''
        Generate OpenCL array length variable declarations.

        :raises NotImplementedError: is an abstract method.
        '''
        raise NotImplementedError(
            "A generic implementation of this method is not available.")

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
        super(KernelSchedule, self).__init__(sequence=None, parent=None)
        self._name = name
        self._symbol_table = SymbolTable(self)

    @property
    def name(self):
        '''
        :returns: Name of the Kernel
        :rtype: str
        '''
        return self._name

    @property
    def symbol_table(self):
        '''
        :returns: Table containing symbol information for the kernel.
        :rtype: :py:class:`psyclone.psyGen.SymbolTable`
        '''
        return self._symbol_table

    def view(self, indent=0):
        '''
        Print a text representation of this node to stdout and then
        call the view() method of any children.

        :param int indent: Depth of indent for output text
        '''
        print(self.indent(indent) + self.coloured_text + "[name:'" + self._name
              + "']")
        for entity in self._children:
            entity.view(indent=indent + 1)

    def gen_ocl(self, indent=0):
        '''
        Generate a string representation of this node in the OpenCL language.

        :param int indent: Depth of indent for the output string.
        :returns: OpenCL language code representing the node.
        :rtype: str
        '''
        raise NotImplementedError(
            "A generic implementation of this method is not available.")

    def __str__(self):
        result = "KernelSchedule[name:'" + self._name + "']:\n"
        for entity in self._children:
            result += str(entity)+"\n"
        result += "End Schedule"
        return result


class CodeBlock(Node):
    '''
    Node representing some generic Fortran code that PSyclone does not attempt
    to manipulate. As such it is a leaf in the PSyIR and therefore has no
    children.

    :param statements: list of fparser2 AST nodes representing the Fortran \
                       code constituting the code block.
    :type statements: list of :py:class:`fparser.two.utils.Base`
    :param parent: the parent node of this code block in the PSyIR.
    :type parent: :py:class:`psyclone.psyGen.Node`
    '''
    def __init__(self, statements, parent=None):
        super(CodeBlock, self).__init__(parent=parent)
        # Store a list of the parser objects holding the code associated
        # with this block. We make a copy of the contents of the list because
        # the list itself is a temporary product of the process of converting
        # from the fparser2 AST to the PSyIR.
        self._statements = statements[:]
        # Store references back into the fparser2 AST
        if statements:
            self.ast = self._statements[0]
            self.ast_end = self._statements[-1]
        else:
            self.ast = None
            self.ast_end = None

    @property
    def coloured_text(self):
        '''
        Return the name of this node type with control codes for
        terminal colouring.

        :returns: Name of node + control chars for colour.
        :rtype: str
        '''
        return colored("CodeBlock", SCHEDULE_COLOUR_MAP["CodeBlock"])

    def view(self, indent=0):
        '''
        Print a representation of this node in the schedule to stdout.

        :param int indent: level to which to indent output.
        '''
        print(self.indent(indent) + self.coloured_text + "[" +
              str(list(map(type, self._statements))) + "]")

    def __str__(self):
        return "CodeBlock[{0} statements]".format(len(self._statements))

    def gen_c_code(self, indent=0):
        '''
        Generate a string representation of this node using C language.

        :param int indent: Depth of indent for the output string.
        :raises GenerationError: gen_c_code always fails for CodeBlocks.
        '''
        raise GenerationError("CodeBlock can not be translated to C")


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
    def coloured_text(self):
        '''
        Return the name of this node type with control codes for
        terminal colouring.

        :returns: Name of node + control chars for colour.
        :rtype: str
        '''
        return colored("Assignment", SCHEDULE_COLOUR_MAP["Assignment"])

    def view(self, indent=0):
        '''
        Print a representation of this node in the schedule to stdout.

        :param int indent: level to which to indent output.
        '''
        print(self.indent(indent) + self.coloured_text + "[]")
        for entity in self._children:
            entity.view(indent=indent + 1)

    def __str__(self):
        result = "Assignment[]\n"
        for entity in self._children:
            result += str(entity)
        return result

    def gen_c_code(self, indent=0):
        '''
        Generate a string representation of this node using C language.

        :param int indent: Depth of indent for the output string.
        :returns: C language code representing the node.
        :rtype: str
        '''
        if len(self.children) != 2:
            raise GenerationError("Assignment malformed or "
                                  "incomplete. It should have exactly 2 "
                                  "children, but found {0}."
                                  "".format(len(self.children)))

        return self.indent(indent) \
            + self.children[0].gen_c_code() + " = " \
            + self.children[1].gen_c_code() + ";"


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

        :return: Name of the referenced symbol.
        :rtype: str
        '''
        return self._reference

    @property
    def coloured_text(self):
        '''
        Return the name of this node type with control codes for
        terminal colouring.

        :returns: Name of node + control chars for colour.
        :rtype: str
        '''
        return colored("Reference", SCHEDULE_COLOUR_MAP["Reference"])

    def view(self, indent=0):
        '''
        Print a representation of this node in the schedule to stdout.

        :param int indent: level to which to indent output.
        '''
        print(self.indent(indent) + self.coloured_text + "[name:'"
              + self._reference + "']")

    def __str__(self):
        return "Reference[name:'" + self._reference + "']\n"

    def gen_c_code(self, indent=0):
        '''
        Generate a string representation of this node using C language.

        :param int indent: Depth of indent for the output string.
        :returns: C language code representing the node.
        :rtype: str
        '''
        return self._reference


class UnaryOperation(Node):
    '''
    Node representing a UnaryOperation expression. As such it has one operand
    as child 0, and an attribute with the operator type.

    :param str operator: string representing the unary operator.
    :param parent: the parent node of this UnaryOperation in the PSyIR.
    :type parent: :py:class:`psyclone.psyGen.Node`
    '''
    def __init__(self, operator, parent=None):
        super(UnaryOperation, self).__init__(parent=parent)
        # TODO: (Issue #339) Create an Operator entity to have more robust
        # operators than the current string.
        self._operator = operator

    @property
    def coloured_text(self):
        '''
        Return the name of this node type with control codes for
        terminal colouring.

        :return: Name of node + control chars for colour.
        :rtype: str
        '''
        return colored("UnaryOperation",
                       SCHEDULE_COLOUR_MAP["UnaryOperation"])

    def view(self, indent=0):
        '''
        Print a representation of this node in the schedule to stdout.

        :param int indent: level to which to indent output.
        '''
        print(self.indent(indent) + self.coloured_text + "[operator:'" +
              self._operator + "']")
        for entity in self._children:
            entity.view(indent=indent + 1)

    def __str__(self):
        result = "UnaryOperation[operator:'" + self._operator + "']\n"
        for entity in self._children:
            result += str(entity)
        return result

    def gen_c_code(self, indent=0):
        '''
        Generate a string representation of this node using C language.

        :param int indent: Depth of indent for the output string.
        :return: C language code representing the node.
        :rtype: str
        :raises GenerationError: if the node or its children are invalid.
        '''
        if len(self.children) != 1:
            raise GenerationError("UnaryOperation malformed or "
                                  "incomplete. It should have exactly 1 "
                                  "child, but found {0}."
                                  "".format(len(self.children)))

        return "(" + self._operator + " " \
            + self._children[0].gen_c_code() + ")"


class BinaryOperation(Node):
    '''
    Node representing a BinaryOperator expression. As such it has two operands
    as children 0 and 1, and a attribute with the operator type.

    :param operator: node in the fparser2 AST representing the binary operator.
    :type operator: :py:class:`fparser.two.Fortran2003.BinaryOpBase.
    :param parent: the parent node of this BinaryOperator in the PSyIR.
    :type parent: :py:class:`psyclone.psyGen.Node`
    '''
    def __init__(self, operator, parent=None):
        super(BinaryOperation, self).__init__(parent=parent)
        self.ast = operator
        self._operator = operator

    @property
    def coloured_text(self):
        '''
        Return the name of this node type with control codes for
        terminal colouring.

        :returns: Name of node + control chars for colour.
        :rtype: str
        '''
        return colored("BinaryOperation",
                       SCHEDULE_COLOUR_MAP["BinaryOperation"])

    def view(self, indent=0):
        '''
        Print a representation of this node in the schedule to stdout.

        :param int indent: level to which to indent output.
        '''
        print(self.indent(indent) + self.coloured_text + "[operator:'" +
              self._operator + "']")
        for entity in self._children:
            entity.view(indent=indent + 1)

    def __str__(self):
        result = "BinaryOperation[operator:'" + self._operator + "']\n"
        for entity in self._children:
            result += str(entity)
        return result

    def gen_c_code(self, indent=0):
        '''
        Generate a string representation of this node using C language.

        :param int indent: Depth of indent for the output string.
        :returns: C language code representing the node.
        :rtype: str
        :raises GenerationError: if the node or its children are invalid.
        '''

        if len(self.children) != 2:
            raise GenerationError("BinaryOperation malformed or "
                                  "incomplete. It should have exactly 2 "
                                  "children, but found {0}."
                                  "".format(len(self.children)))

        return "(" + self._children[0].gen_c_code() + " " \
            + self._operator + " " \
            + self._children[1].gen_c_code() + ")"


class Array(Reference):
    '''
    Node representing an Array reference. As such it has a reference and a
    subscript list as children 0 and 1, respectively.

    :param ast: node in the fparser2 AST representing array.
    :type ast: :py:class:`fparser.two.Fortran2003.Part_Ref.
    :param parent: the parent node of this Array in the PSyIR.
    :type parent: :py:class:`psyclone.psyGen.Node`
    '''
    def __init__(self, reference_name, parent):
        super(Array, self).__init__(reference_name, parent=parent)

    @property
    def coloured_text(self):
        '''
        Return the name of this node type with control codes for
        terminal colouring.

        :returns: Name of node + control chars for colour.
        :rtype: str
        '''
        return colored("ArrayReference", SCHEDULE_COLOUR_MAP["Reference"])

    def view(self, indent=0):
        '''
        Print a representation of this node in the schedule to stdout.

        :param int indent: level to which to indent output.
        '''
        super(Array, self).view(indent)
        for entity in self._children:
            entity.view(indent=indent + 1)

    def __str__(self):
        result = "Array" + super(Array, self).__str__()
        for entity in self._children:
            result += str(entity)
        return result

    def gen_c_code(self, indent=0):
        '''
        Generate a string representation of this node using C language.

        :param int indent: Depth of indent for the output string.
        :returns: C language code representing the node.
        :rtype: str
        '''
        code = super(Array, self).gen_c_code() + "["

        dimensions_remaining = len(self._children)
        if dimensions_remaining < 1:
            raise GenerationError("Array must have at least 1 dimension.")

        # In C array expressions should be reversed from the PSyIR order
        # (column-major to row-major order) and flattened (1D).
        for child in reversed(self._children):
            code = code + child.gen_c_code()
            # For each dimension bigger than one, it needs to write the
            # appropriate operation to flatten the array. By convention,
            # the array dimensions are <name>LEN<DIM>.
            # (e.g. A[3,5,2] -> A[3 * ALEN2 * ALEN1 + 5 * ALEN1 + 2])
            for dim in reversed(range(1, dimensions_remaining)):
                dimstring = self._reference + "LEN" + str(dim)
                code = code + " * " + dimstring
            dimensions_remaining = dimensions_remaining - 1
            code = code + " + "

        code = code[:-3] + "]"  # Delete last ' + ' and close bracket
        return code


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
    def coloured_text(self):
        '''
        Return the name of this node type with control codes for
        terminal colouring.

        :returns: Name of node + control chars for colour.
        :rtype: str
        '''
        return colored("Literal", SCHEDULE_COLOUR_MAP["Literal"])

    def view(self, indent=0):
        '''
        Print a representation of this node in the schedule to stdout.

        :param int indent: level to which to indent output.
        '''
        print(self.indent(indent) + self.coloured_text + "["
              + "value:'"+self._value + "']")

    def __str__(self):
        return "Literal[value:'" + self._value + "']\n"

    def gen_c_code(self, indent=0):
        '''
        Generate a string representation of this node using C language.

        :param int indent: Depth of indent for the output string.
        :returns: C language code representing the node.
        :rtype: str
        '''
        return self._value


class Return(Node):
    '''
    Node representing a Return statement (subroutine break without return
    value).

    :param parent: the parent node of this Return in the PSyIR.
    :type parent: :py:class:`psyclone.psyGen.Node`
    '''
    def __init__(self, parent=None):
        super(Return, self).__init__(parent=parent)

    @property
    def coloured_text(self):
        '''
        Return the name of this node type with control codes for
        terminal colouring.

        :return: Name of node + control chars for colour.
        :rtype: str
        '''
        return colored("Return", SCHEDULE_COLOUR_MAP["Return"])

    def view(self, indent=0):
        '''
        Print a representation of this node in the schedule to stdout.

        :param int indent: level to which to indent output.
        '''
        print(self.indent(indent) + self.coloured_text + "[]")

    def __str__(self):
        return "Return[]\n"

    def gen_c_code(self, indent=0):
        '''
        Generate a string representation of this node using C language.

        :param int indent: Depth of indent for the output string.
        :return: C language code representing the node.
        :rtype: str
        '''
        return self.indent(indent) + "return;"
