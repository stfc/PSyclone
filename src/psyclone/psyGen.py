# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-18, Science and Technology Facilities Council
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
# Authors R. W. Ford and A. R. Porter, STFC Daresbury Lab
# Modified I. Kavcic, Met Office
# -----------------------------------------------------------------------------

''' This module provides generic support for PSyclone's PSy code optimisation
    and generation. The classes in this method need to be specialised for a
    particular API and implementation. '''

import abc
from psyclone import config

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
        :return: The supplied text, unchanged
        :rtype: string
        '''
        return text

# The types of 'intent' that an argument to a Fortran subroutine
# may have
FORTRAN_INTENT_NAMES = ["inout", "out", "in"]

# The following mappings will be set by a particular API if supported
# and required. We provide a default here for API's which do not have
# their own mapping (or support this mapping). This allows codes with
# no support to run.
# MAPPING_REDUCTIONS gives the names of reduction operations
MAPPING_REDUCTIONS = {"sum": "sum"}
# OMP_OPERATOR_MAPPING is used to determine the operator to use in the
# reduction clause of an OpenMP directive. All code for OpenMP
# directives exists in psyGen.py so this mapping should not be
# overidden.
OMP_OPERATOR_MAPPING = {"sum": "+"}
# REDUCTION_OPERATOR_MAPPING is used to determine the operator to use
# when creating a loop to sum partial sums sequentially, in order to
# get reproducible results. The LHS is the datatype of the field in
# question so needs to be overidden by the particular API.
REDUCTION_OPERATOR_MAPPING = {"sum": "+"}
# Names of types of scalar variable
MAPPING_SCALARS = {"iscalar": "iscalar", "rscalar": "rscalar"}
# Types of access for a kernel argument
MAPPING_ACCESSES = {"inc": "inc", "write": "write",
                    "read": "read", "readwrite": "readwrite"}
# Valid types of argument to a kernel call
VALID_ARG_TYPE_NAMES = []
# List of all valid access types for a kernel argument
VALID_ACCESS_DESCRIPTOR_NAMES = []

# Colour map to use when writing Invoke schedule to terminal. (Requires
# that the termcolor package be installed. If it isn't then output is not
# coloured.) See https://pypi.python.org/pypi/termcolor for details.
SCHEDULE_COLOUR_MAP = {"Schedule": "yellow",
                       "Loop": "white",
                       "GlobalSum": "cyan",
                       "Directive": "green",
                       "HaloExchange": "blue",
                       "Call": "red",
                       "KernCall": "magenta"}


def get_api(api):
    ''' If no API is specified then return the default. Otherwise, check that
    the supplied API is valid. '''
    if api == "":
        from psyclone.config import DEFAULTAPI
        api = DEFAULTAPI
    else:
        from psyclone.config import SUPPORTEDAPIS as supported_types
        if api not in supported_types:
            raise GenerationError("get_api: Unsupported API '{0}' "
                                  "specified. Supported types are "
                                  "{1}.".format(api,
                                                supported_types))
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


def args_filter(arg_list, arg_types=None, arg_accesses=None, arg_meshes=None):
    '''
    Return all arguments in the supplied list that are of type
    arg_types and with access in arg_accesses. If these are not set
    then return all arguments.

    :param arg_list: List of kernel arguments to filter
    :type arg_list: list of :py:class:`psyclone.parse.Descriptor`
    :param arg_types: List of argument types (e.g. "GH_FIELD")
    :type arg_types: list of str
    :param arg_accesses: List of access types that arguments must have
    :type arg_accesses: list of str
    :param arg_meshes: List of meshes that arguments must be on
    :type arg_meshes: list of str

    :returns: list of kernel arguments matching the requirements
    :rtype: list of :py:class:`psyclone.parse.Descriptor`
    '''
    arguments = []
    for argument in arg_list:
        if arg_types:
            if argument.type.lower() not in arg_types:
                continue
        if arg_accesses:
            if argument.access.lower() not in arg_accesses:
                continue
        if arg_meshes:
            if argument.mesh not in arg_meshes:
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
        return repr(self.value)


class FieldNotFoundError(Exception):
    ''' Provides a PSyclone-specific error class when a field with the
    requested property/ies is not found '''
    def __init__(self, value):
        Exception.__init__(self, value)
        self.value = "Field not found error: "+value

    def __str__(self):
        return repr(self.value)


class PSyFactory(object):
    '''Creates a specific version of the PSy. If a particular api is not
        provided then the default api, as specified in the config.py
        file, is chosen. Note, for pytest to work we need to set
        distributed_memory to the same default as the value found in
        config.DISTRIBUTED_MEMORY. If we set it to None and then test
        the value, it then fails. I've no idea why. '''

    def __init__(self, api="", distributed_memory=config.DISTRIBUTED_MEMORY):
        if distributed_memory not in [True, False]:
            raise GenerationError(
                "The distributed_memory flag in PSyFactory must be set to"
                " 'True' or 'False'")
        config.DISTRIBUTED_MEMORY = distributed_memory
        self._type = get_api(api)

    def create(self, invoke_info):
        ''' Return the specified version of PSy. '''
        if self._type == "gunghoproto":
            from psyclone.ghproto import GHProtoPSy
            return GHProtoPSy(invoke_info)
        elif self._type == "dynamo0.1":
            from psyclone.dynamo0p1 import DynamoPSy
            return DynamoPSy(invoke_info)
        elif self._type == "dynamo0.3":
            from psyclone.dynamo0p3 import DynamoPSy
            return DynamoPSy(invoke_info)
        elif self._type == "gocean0.1":
            from psyclone.gocean0p1 import GOPSy
            return GOPSy(invoke_info)
        elif self._type == "gocean1.0":
            from psyclone.gocean1p0 import GOPSy
            return GOPSy(invoke_info)
        else:
            raise GenerationError("PSyFactory: Internal Error: Unsupported "
                                  "api type '{0}' found. Should not be "
                                  "possible.".format(self._type))


class PSy(object):
    '''
        Base class to help manage and generate PSy code for a single
        algorithm file. Takes the invocation information output from the
        function :func:`parse.parse` as its input and stores this in a
        way suitable for optimisation and code generation.

        :param FileInfo invoke_info: An object containing the required
                                     invocation information for code
                                     optimisation and generation. Produced
                                     by the function :func:`parse.parse`.

        For example:

        >>> import psyclone
        >>> from psyclone.parse import parse
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
    def gen(self):
        raise NotImplementedError("Error: PSy.gen() must be implemented "
                                  "by subclass")

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
    ''' Manage the invoke calls '''
    def __init__(self, alg_calls, Invoke):
        self.invoke_map = {}
        self.invoke_list = []
        for idx, alg_invocation in enumerate(alg_calls.values()):
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
        for invoke in self.invoke_list:
            invoke.gen_code(parent)


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
        :type schedule_class: Schedule class.
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
        specified datatype. If access is supplied (e.g. "gh_write") then
        only declarations with that access are returned. '''
        if datatype not in VALID_ARG_TYPE_NAMES:
            raise GenerationError(
                "unique_declarations called with an invalid datatype. "
                "Expected one of '{0}' but found '{1}'".
                format(str(VALID_ARG_TYPE_NAMES), datatype))
        if access and access not in VALID_ACCESS_DESCRIPTOR_NAMES:
            raise GenerationError(
                "unique_declarations called with an invalid access type. "
                "Expected one of '{0}' but got '{1}'".
                format(VALID_ACCESS_DESCRIPTOR_NAMES, access))
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

        :param string datatype: the type of the kernel argument for the
                                particular API for which the intent is
                                required
        :return: dictionary containing 'intent' keys holding the kernel
                 argument intent and declarations of all kernel arguments
                 for each type of intent
        :rtype: dict
        :raises GenerationError: if the kernel argument is not a valid
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
        inc_args = self.unique_declarations(datatype,
                                            access=MAPPING_ACCESSES["inc"])
        write_args = self.unique_declarations(datatype,
                                              access=MAPPING_ACCESSES["write"])
        read_args = self.unique_declarations(datatype,
                                             access=MAPPING_ACCESSES["read"])
        readwrite_args = self.unique_declarations(
            datatype, access=MAPPING_ACCESSES["readwrite"])
        sum_args = self.unique_declarations(datatype,
                                            access=MAPPING_REDUCTIONS["sum"])
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
            if first_arg.access != MAPPING_ACCESSES["write"]:
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
            if first_arg.access == MAPPING_ACCESSES["read"]:
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
    ''' baseclass for a node in a schedule '''

    def dag(self, file_name='dag', file_format='svg'):
        '''Create a dag of this node and its children'''
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
        '''output my node's graph (dag) information and call any
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
        ''' return the base dag name for this node '''
        return "node_" + str(self.abs_position)

    @property
    def args(self):
        '''Return the list of arguments associated with this node. The default
        implementation assumes the node has no directly associated
        arguments (i.e. is not a Call class or subclass). Arguments of
        any of this nodes descendents are considered to be
        associated. '''
        args = []
        for call in self.calls():
            args.extend(call.args)
        return args

    def backward_dependence(self):
        '''Returns the closest preceding Node that this Node has a direct
        dependence with or None if there is not one. Only Nodes with
        the same parent as self are returned. Nodes inherit their
        descendents dependencies. The reason for this is that for
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
        descendents dependencies. The reason for this is that for
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
        ''' Returns this Node's depth in the tree. '''
        my_depth = 0
        node = self
        while node is not None:
            node = node.parent
            my_depth += 1
        return my_depth

    def view(self):
        raise NotImplementedError("BaseClass of a Node must implement the "
                                  "view method")

    def indent(self, count, indent="    "):
        result = ""
        for i in range(count):
            result += indent
        return result

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

    def __init__(self, children=None, parent=None):
        if not children:
            self._children = []
        else:
            self._children = children
        self._parent = parent

    def __str__(self):
        raise NotImplementedError("Please implement me")

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
        if self.parent is None:
            return 0
        return self.parent.children.index(self)

    @property
    def abs_position(self):
        ''' Find my position in the schedule. Needs to be computed
            dynamically as my position may change. '''

        if self.root == self:
            return 0
        found, position = self._find_position(self.root.children, 0)
        if not found:
            raise Exception("Error in search for my position in "
                            "the tree")
        return position

    def _find_position(self, children, position):
        ''' Recurse through the tree depth first returning position if
            found.'''
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
        ''' recurse through tree and return objects of mytype '''
        local_list = []
        for child in children:
            if isinstance(child, my_type):
                local_list.append(child)
            local_list += self.walk(child.children, my_type)
        return local_list

    def ancestor(self, my_type):
        ''' Search back up tree and check whether we have an
        ancestor of the supplied type. If we do then we return
        it otherwise we return None '''
        myparent = self.parent
        while myparent is not None:
            if isinstance(myparent, my_type):
                return myparent
            myparent = myparent.parent
        return None

    def calls(self):
        ''' return all calls that are descendents of this node '''
        return self.walk(self.children, Call)

    def following(self):
        '''Return all :py:class:`psyclone.psyGen.Node` nodes after me in the
        schedule. Ordering is depth first.

        :return: a list of nodes
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
        :return: A list of nodes
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
        ''' return all calls after me in the schedule '''
        all_calls = self.root.calls()
        position = all_calls.index(self)
        return all_calls[position+1:]

    @property
    def preceding_calls(self):
        ''' return all calls before me in the schedule '''
        all_calls = self.root.calls()
        position = all_calls.index(self)
        return all_calls[:position-1]

    def kern_calls(self):
        '''return all user-supplied kernel calls in this schedule'''
        return self.walk(self._children, Kern)

    def loops(self):
        ''' return all loops currently in this schedule '''
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
        '''Returns true if this Node is within an OpenMP parallel region

        '''
        omp_dir = self.ancestor(OMPParallelDirective)
        if omp_dir:
            return True
        return False

    def gen_code(self):
        raise NotImplementedError("Please implement me")


class Schedule(Node):

    ''' Stores schedule information for an invocation call. Schedules can be
        optimised using transformations.

        >>> from parse import parse
        >>> ast, info = parse("algorithm.f90")
        >>> from psyGen import PSyFactory
        >>> api = "..."
        >>> psy = PSyFactory(api).create(info)
        >>> invokes = psy.invokes
        >>> invokes.names
        >>> invoke = invokes.get("name")
        >>> schedule = invoke.schedule
        >>> schedule.view()

    '''

    @property
    def dag_name(self):
        ''' Return the name to use in a dag for this node'''
        return "schedule"

    def tkinter_delete(self):
        for entity in self._children:
            entity.tkinter_delete()

    def tkinter_display(self, canvas, x, y):
        y_offset = 0
        for entity in self._children:
            entity.tkinter_display(canvas, x, y+y_offset)
            y_offset = y_offset+entity.height

    @property
    def invoke(self):
        return self._invoke

    @invoke.setter
    def invoke(self, my_invoke):
        self._invoke = my_invoke

    def __init__(self, KernFactory, BuiltInFactory, alg_calls=[]):

        # we need to separate calls into loops (an iteration space really)
        # and calls so that we can perform optimisations separately on the
        # two entities.
        sequence = []
        from psyclone.parse import BuiltInCall
        for call in alg_calls:
            if isinstance(call, BuiltInCall):
                sequence.append(BuiltInFactory.create(call, parent=self))
            else:
                sequence.append(KernFactory.create(call, parent=self))
        Node.__init__(self, children=sequence)
        self._invoke = None

    def view(self, indent=0):
        '''
        Print a text representation of this node to stdout and then
        call the view() method of any children.

        :param indent: Depth of indent for output text
        :type indent: integer
        '''
        print self.indent(indent) + self.coloured_text + \
            "[invoke='" + self.invoke.name + "']"
        for entity in self._children:
            entity.view(indent=indent + 1)

    @property
    def coloured_text(self):
        '''
        Returns the name of this node with appropriate control codes
        to generate coloured output in a terminal that supports it.

        :return: Text containing the name of this node, possibly coloured
        :rtype: string
        '''
        return colored("Schedule", SCHEDULE_COLOUR_MAP["Schedule"])

    def __str__(self):
        result = "Schedule:\n"
        for entity in self._children:
            result += str(entity)+"\n"
        result += "End Schedule"
        return result

    def gen_code(self, parent):
        for entity in self._children:
            entity.gen_code(parent)


class Directive(Node):

    def view(self, indent=0):
        '''
        Print a text representation of this node to stdout and then
        call the view() method of any children.

        :param indent: Depth of indent for output text
        :type indent: integer
        '''
        print self.indent(indent) + self.coloured_text
        for entity in self._children:
            entity.view(indent=indent + 1)

    @property
    def coloured_text(self):
        '''
        Returns a string containing the name of this element with
        control codes for colouring in terminals that support it.

        :return: Text containing the name of this node, possibly coloured
        :rtype: string
        '''
        return colored("Directive", SCHEDULE_COLOUR_MAP["Directive"])

    @property
    def dag_name(self):
        ''' return the base dag name for this node '''
        return "directive_" + str(self.abs_position)


class OMPDirective(Directive):

    @property
    def dag_name(self):
        ''' Return the name to use in a dag for this node'''
        return "OMP_directive_" + str(self.abs_position)

    def view(self, indent=0):
        '''
        Print a text representation of this node to stdout and then
        call the view() method of any children.

        :param indent: Depth of indent for output text
        :type indent: integer
        '''
        print self.indent(indent) + self.coloured_text + "[OMP]"
        for entity in self._children:
            entity.view(indent=indent + 1)

    def _get_reductions_list(self, reduction_type):
        '''Return the name of all scalars within this region that require a
        reduction of type reduction_type. Returned names will be unique. '''
        result = []
        for call in self.calls():
            for arg in call.arguments.args:
                if arg.type in MAPPING_SCALARS.values():
                    if arg.descriptor.access == \
                       MAPPING_REDUCTIONS[reduction_type]:
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
        print self.indent(indent) + self.coloured_text + "[OMP parallel]"
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
        '''Returns the variable names used for any loops within a directive
        and any variables that have been declared private by a Call
        within the directive.

        '''
        result = []
        # get variable names from all loops that are a child of this node
        for loop in self.loops():
            if loop._variable_name == "":
                raise GenerationError("Internal error: name of loop "
                                      "variable not set.")
            if loop._variable_name.lower() not in result:
                result.append(loop._variable_name.lower())
        # get variable names from all calls that are a child of this node
        for call in self.calls():
            for variable_name in call.local_vars():
                if variable_name == "":
                    raise GenerationError("Internal error: call has a "
                                          "local variable but its name "
                                          "is not set.")
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


class OMPDoDirective(OMPDirective):

    def __init__(self, children=None, parent=None, omp_schedule="static",
                 reprod=None):

        if children is None:
            children = []
        if reprod is None:
            reprod = config.REPRODUCIBLE_REDUCTIONS

        self._omp_schedule = omp_schedule
        self._reprod = reprod
        # Call the init method of the base class once we've stored
        # the OpenMP schedule
        OMPDirective.__init__(self,
                              children=children,
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
        print self.indent(indent) + self.coloured_text + \
            "[OMP do]{0}".format(reprod)

        for entity in self._children:
            entity.view(indent=indent + 1)

    def _reduction_string(self):
        ''' Return the OMP reduction information as a string '''
        reduction_str = ""
        for reduction_type in MAPPING_REDUCTIONS.keys():
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
        from psyclone.f2pygen import DirectiveGen

        # It is only at the point of code generation that
        # we can check for correctness (given that we don't
        # mandate the order that a user can apply transformations
        # to the code). As an orphaned loop directive, we must
        # have an OMPRegionDirective as a parent somewhere
        # back up the tree.
        self._within_omp_region()

        if self._reprod:
            local_reduction_string = ""
        else:
            local_reduction_string = self._reduction_string()

        # As we're an orphaned loop we don't specify the scope
        # of any variables so we don't have to generate the
        # list of private variables
        parent.add(DirectiveGen(parent,
                                "omp", "begin", "do",
                                "schedule({0})".
                                format(self._omp_schedule) +
                                local_reduction_string))

        for child in self.children:
            child.gen_code(parent)

        # make sure the directive occurs straight after the loop body
        position = parent.previous_loop()
        parent.add(DirectiveGen(parent, "omp", "end", "do", ""),
                   position=["after", position])

    def _within_omp_region(self):
        ''' Check that this orphaned OMP Loop Directive is actually
            within an OpenMP Parallel Region '''
        myparent = self.parent
        while myparent is not None:
            if isinstance(myparent, OMPParallelDirective) and\
               not isinstance(myparent, OMPParallelDoDirective):
                return
            myparent = myparent.parent
        raise GenerationError("OMPOrphanLoopDirective must have an "
                              "OMPRegionDirective as ancestor")


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
        print self.indent(indent) + self.coloured_text + \
            "[OMP parallel do]"
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
            self._scalar.access = MAPPING_ACCESSES["readwrite"]
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
        print self.indent(indent) + (
            "{0}[scalar='{1}']".format(self.coloured_text, self._scalar.name))

    @property
    def coloured_text(self):
        '''
        Return a string containing the (coloured) name of this node
        type

        :return: A string containing the name of this node, possibly with
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
            self._field.access = MAPPING_ACCESSES["readwrite"]
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
        ''' Return the name to use in a dag for this node'''
        name = ("haloexchange({0})_".format(self._field.name) +
                str(self.position))
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
        print self.indent(indent) + (
            "{0}[field='{1}', type='{2}', depth={3}, "
            "check_dirty={4}]".format(self.coloured_text, self._field.name,
                                      self._halo_type,
                                      self._halo_depth, self._check_dirty))

    @property
    def coloured_text(self):
        '''
        Return a string containing the (coloured) name of this node type

        :return: Name of this node type, possibly with colour control codes
        :rtype: string
        '''
        return colored("HaloExchange", SCHEDULE_COLOUR_MAP["HaloExchange"])


class Loop(Node):

    @property
    def dag_name(self):
        ''' Return the name to use in a dag for this node

        :return: Return the dag name for this loop
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
        assert value in self._valid_loop_types, \
            "Error, loop_type value is invalid"
        self._loop_type = value

    def __init__(self, parent=None,
                 variable_name="",
                 topology_name="topology",
                 valid_loop_types=[]):

        # we need to determine whether this is a built-in or kernel
        # call so our schedule can do the right thing.

        self._valid_loop_types = valid_loop_types
        self._loop_type = None        # inner, outer, colour, colours, ...
        self._field = None
        self._field_name = None       # name of the field
        self._field_space = None      # v0, v1, ...,     cu, cv, ...
        self._iteration_space = None  # cells, ...,      cu, cv, ...

        # TODO replace iterates_over with iteration_space
        self._iterates_over = "unknown"

        Node.__init__(self, parent=parent)

        self._variable_name = variable_name

        self._start = ""
        self._stop = ""
        self._step = ""
        self._id = ""

        # visual properties
        self._width = 30
        self._height = 30
        self._shape = None
        self._text = None
        self._canvas = None

    def view(self, indent=0):
        '''
        Write out a textual summary of this Loop node to stdout
        and then call the view() method of any children.

        :param indent: Depth of indent for output text
        :type indent: integer
        '''
        print self.indent(indent) + self.coloured_text + \
            "[type='{0}',field_space='{1}',it_space='{2}']".\
            format(self._loop_type, self._field_space, self.iteration_space)
        for entity in self._children:
            entity.view(indent=indent + 1)

    @property
    def coloured_text(self):
        '''
        Returns a string containing the name of this node along with
        control characters for colouring in terminals that support it.

        :return: The name of this node, possibly with control codes for
                 colouring
        :rtype: string
        '''
        return colored("Loop", SCHEDULE_COLOUR_MAP["Loop"])

    @property
    def height(self):
        calls_height = 0
        for child in self.children:
            calls_height += child.height
        return self._height+calls_height

    def tkinter_delete(self):
        if self._shape is not None:
            assert self._canvas is not None, "Error"
            self._canvas.delete(self._shape)
        if self._text is not None:
            assert self._canvas is not None, "Error"
            self._canvas.delete(self._text)
        for child in self.children:
            child.tkinter_delete()

    def tkinter_display(self, canvas, x, y):
        self.tkinter_delete()
        self._canvas = canvas
        from Tkinter import ROUND
        name = "Loop"
        min_call_width = 100
        max_calls_width = min_call_width
        calls_height = 0
        for child in self.children:
            calls_height += child.height
            max_calls_width = max(max_calls_width, child.width)

        self._shape = canvas.create_polygon(
            x, y, x+self._width+max_calls_width, y,
            x+self._width+max_calls_width, y+self._height,
            x+self._width, y+self._height,
            x+self._width, y+self._height+calls_height,
            x, y+self._height+calls_height,
            outline="red", fill="green", width=2,
            activeoutline="blue", joinstyle=ROUND)
        self._text = canvas.create_text(x+(self._width+max_calls_width)/2,
                                        y+self._height/2, text=name)

        call_height = 0
        for child in self.children:
            child.tkinter_display(canvas, x+self._width,
                                  y+self._height+call_height)
            call_height += child.height

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

    def __str__(self):
        result = "Loop[" + self._id + "]: " + self._variable_name + "=" + \
            self._id + " lower=" + self._start + "," + self._stop + "," + \
            self._step + "\n"
        for entity in self._children:
            result += str(entity) + "\n"
        result += "EndLoop"
        return result

    def has_inc_arg(self, mapping={}):
        ''' Returns True if any of the Kernels called within this
        loop have an argument with INC access. Returns False otherwise '''
        assert mapping != {}, "psyGen:Loop:has_inc_arg: Error - a mapping "\
            "must be provided"
        for kern_call in self.kern_calls():
            for arg in kern_call.arguments.args:
                if arg.access.lower() == mapping["inc"]:
                    return True
        return False

    def unique_modified_args(self, mapping, arg_type):
        '''Return all unique arguments of type arg_type from Kernels in this
        loop that are modified'''
        arg_names = []
        args = []
        for call in self.calls():
            for arg in call.arguments.args:
                if arg.type.lower() == arg_type:
                    if arg.access.lower() != mapping["read"]:
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
        '''Generate the fortran Loop and any associated code '''
        if not self.is_openmp_parallel():
            calls = self.reductions()
            zero_reduction_variables(calls, parent)

        if self._start == "1" and self._stop == "1":  # no need for a loop
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
        print self.indent(indent) + self.coloured_text, \
            self.name + "(" + str(self.arguments.raw_arg_list) + ")"
        for entity in self._children:
            entity.view(indent=indent + 1)

    @property
    def coloured_text(self):
        ''' Return a string containing the (coloured) name of this node
        type '''
        return colored("Call", SCHEDULE_COLOUR_MAP["Call"])

    @property
    def width(self):
        return self._width

    @property
    def height(self):
        return self._height

    def tkinter_delete(self):
        if self._shape is not None:
            assert self._canvas is not None, "Error"
            self._canvas.delete(self._shape)
        if self._text is not None:
            assert self._canvas is not None, "Error"
            self._canvas.delete(self._text)

    def tkinter_display(self, canvas, x, y):
        self.tkinter_delete()
        self._canvas = canvas
        self._x = x
        self._y = y
        self._shape = self._canvas.create_rectangle(
            self._x, self._y, self._x+self._width, self._y+self._height,
            outline="red", fill="yellow", activeoutline="blue", width=2)
        self._text = self._canvas.create_text(self._x+self._width/2,
                                              self._y+self._height/2,
                                              text=self._name)

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

        # visual properties
        self._width = 250
        self._height = 30
        self._shape = None
        self._text = None
        self._canvas = None
        self._arg_descriptors = None

        # initialise any reduction information
        args = args_filter(arguments.args,
                           arg_types=MAPPING_SCALARS.values(),
                           arg_accesses=MAPPING_REDUCTIONS.values())
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
        '''Generate code to zero the reduction variable and to zero the local
        reduction variable if one exists. The latter is used for reproducible
        reductions, if specified.'''
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
            if config.REPROD_PAD_SIZE < 1:
                raise GenerationError(
                    "REPROD_PAD_SIZE in config.py should be a positive "
                    "integer, but it is set to '{0}'.".format(
                        config.REPROD_PAD_SIZE))
            pad_size = str(config.REPROD_PAD_SIZE)
            parent.add(AllocateGen(parent, local_var_name + "(" + pad_size +
                                   "," + nthreads + ")"), position=position)
            parent.add(AssignGen(parent, lhs=local_var_name,
                                 rhs=zero), position=position)

    def reduction_sum_loop(self, parent):
        '''generate the appropriate code to place after the end parallel
        region'''
        from psyclone.f2pygen import DoGen, AssignGen, DeallocateGen
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
            raise GenerationError(
                "unsupported reduction access '{0}' found in DynBuiltin:"
                "reduction_sum_loop(). Expected one of '{1}'".
                format(reduction_access, REDUCTION_OPERATOR_MAPPING.keys()))
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
        return self._name

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
    def __init__(self, KernelArguments, call, parent=None, check=True):
        Call.__init__(self, parent, call, call.ktype.procedure.name,
                      KernelArguments(call, self))
        self._module_name = call.module_name
        self._module_code = call.ktype._ast
        self._kernel_code = call.ktype.procedure
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

    def __str__(self):
        return "kern call: "+self._name

    @property
    def module_name(self):
        '''
        :return: The name of the Fortran module that contains this kernel
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
        # check all kernels in the same invoke as this one and set any
        # with the same name to the same value as this one. This is
        # required as inlining (or not) affects all calls to the same
        # kernel within an invoke. Note, this will set this kernel as
        # well so there is no need to set it locally.
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
        print self.indent(indent) + self.coloured_text, \
            self.name + "(" + str(self.arguments.raw_arg_list) + ")", \
            "[module_inline=" + str(self._module_inline) + "]"
        for entity in self._children:
            entity.view(indent=indent + 1)

    @property
    def coloured_text(self):
        '''
        Return text containing the (coloured) name of this node type

        :return: the name of this node type, possibly with control codes
                 for colour
        :rtype: string
        '''
        return colored("KernCall", SCHEDULE_COLOUR_MAP["KernCall"])

    def gen_code(self, parent):
        from psyclone.f2pygen import CallGen, UseGen
        parent.add(CallGen(parent, self._name, self._arguments.arglist))
        parent.add(UseGen(parent, name=self._module_name, only=True,
                          funcnames=[self._name]))

    def incremented_arg(self, mapping={}):
        ''' Returns the argument that has INC access. Raises a
        FieldNotFoundError if none is found.

        :param mapping: dictionary of access types (here INC) associated
                        with arguments with their metadata strings as keys
        :type mapping: dict
        :return: a Fortran argument name
        :rtype: string
        :raises FieldNotFoundError: if none is found.

        '''
        assert mapping != {}, "psyGen:Kern:incremented_arg: Error - a "\
            "mapping must be provided"
        for arg in self.arguments.args:
            if arg.access.lower() == mapping["inc"]:
                return arg
        raise FieldNotFoundError("Kernel {0} does not have an argument with "
                                 "{1} access".
                                 format(self.name, mapping["inc"]))

    def written_arg(self, mapping={}):
        '''
        Returns an argument that has WRITE or READWRITE access. Raises a
        FieldNotFoundError if none is found.

        :param mapping: dictionary of access types (here WRITE or
                        READWRITE) associated with arguments with their
                        metadata strings as keys
        :type mapping: dict
        :return: a Fortran argument name
        :rtype: string
        :raises FieldNotFoundError: if none is found.

        '''
        assert mapping != {}, "psyGen:Kern:written_arg: Error - a "\
            "mapping must be provided"
        for access in ["write", "readwrite"]:
            for arg in self.arguments.args:
                if arg.access.lower() == mapping[access]:
                    return arg
        raise FieldNotFoundError("Kernel {0} does not have an argument with "
                                 "{1} or {2} access".
                                 format(self.name, mapping["write"],
                                        mapping["readwrite"]))

    def is_coloured(self):
        ''' Returns true if this kernel is being called from within a
        coloured loop '''
        return self.parent.loop_type == "colour"


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
    ''' arguments abstract base class '''
    def __init__(self, parent_call):
        self._parent_call = parent_call
        self._args = []

    @property
    def raw_arg_list(self):
        ''' returns a comma separated list of the field arguments to the
            kernel call '''
        result = ""
        for idx, arg in enumerate(self.args):
            result += arg.name
            if idx < (len(self.args) - 1):
                result += ","
        return result

    @property
    def args(self):
        return self._args

    def iteration_space_arg(self, mapping={}):
        '''
        Returns an argument that can be iterated over, i.e. modified
        (has WRITE, READWRITE or INC access).

        :param mapping: dictionary of access types associated with arguments
                        with their metadata strings as keys
        :type mapping: dict
        :return: a Fortran argument name
        :rtype: string
        :raises GenerationError: if none such argument is found.

        '''
        assert mapping != {}, "psyGen:Arguments:iteration_space_arg: Error "
        "a mapping needs to be provided"
        for arg in self._args:
            if arg.access.lower() == mapping["write"] or \
               arg.access.lower() == mapping["readwrite"] or \
               arg.access.lower() == mapping["inc"]:
                return arg
        raise GenerationError("psyGen:Arguments:iteration_space_arg Error, "
                              "we assume there is at least one writer, "
                              "reader/writer, or increment as an argument")


class Argument(object):
    ''' Argument base class '''

    def __init__(self, call, arg_info, access):
        '''
        :param call: the call that this argument is associated with
        :type call: :py:class:`psyclone.psyGen.Call`
        :param arg_info: Information about this argument collected by
        the parser
        :type arg_info: :py:class:`psyclone.parse.Arg`
        :param access: the way in which this argument is accessed in
        the 'Call'. Valid values are specified in 'MAPPING_ACCESSES'
        (and may be modified by the particular API).
        :type access: str

        '''
        self._call = call
        self._text = arg_info.text
        self._orig_name = arg_info.varName
        self._form = arg_info.form
        self._is_literal = arg_info.is_literal()
        self._access = access
        if self._orig_name is None:
            # this is an infrastructure call literal argument. Therefore
            # we do not want an argument (_text=None) but we do want to
            # keep the value (_name)
            self._name = arg_info.text
            self._text = None
        else:
            self._name_space_manager = NameSpaceFactory().create()
            # Use our namespace manager to create a unique name unless
            # the context and label match in which case return the
            # previous name.
            self._name = self._name_space_manager.create_name(
                root_name=self._orig_name, context="AlgArgs", label=self._text)
        # _writers and _readers need to be instances of this class,
        # rather than static variables, as the mapping that is used
        # depends on the API and this is only known when a subclass of
        # Argument is created (as the local MAPPING_ACCESSES will be
        # used). For example, a dynamo0p3 api instantiation of a
        # DynArgument (subclass of Argument) will use the
        # MAPPING_ACCESSES specified in the dynamo0p3 file which
        # overide the default ones in this file.
        self._write_access_types = [MAPPING_ACCESSES["write"],
                                    MAPPING_ACCESSES["readwrite"],
                                    MAPPING_ACCESSES["inc"],
                                    MAPPING_REDUCTIONS["sum"]]
        self._read_access_types = [MAPPING_ACCESSES["read"],
                                   MAPPING_ACCESSES["readwrite"],
                                   MAPPING_ACCESSES["inc"]]
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
        ''' set the access type for this argument '''
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

        :return: the first preceding argument this argument has a
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
        :return: a list of arguments that this argument has a dependence with
        :rtype: :func:`list` of :py:class:`psyclone.psyGen.Argument`

        '''
        nodes = self._call.preceding(reverse=True)
        results = self._find_write_arguments(nodes, ignore_halos=ignore_halos)
        return results

    def forward_dependence(self):
        '''Returns the following argument that this argument has a direct
        dependence with, or `None` if there is not one. The argument may
        exist in a call, a haloexchange, or a globalsum.

        :return: the first following argument this argument has a
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

        :return: a list of arguments that this argument has a dependence with
        :rtype: :func:`list` of :py:class:`psyclone.psyGen.Argument`

        '''
        nodes = self._call.following()
        return self._find_read_arguments(nodes)

    def _find_argument(self, nodes):
        '''Return the first argument in the list of nodes that has a
        dependency with self. If one is not found return None

        :param: the list of nodes that this method examines
        :type: :func:`list` of :py:class:`psyclone.psyGen.Node`
        :return: An argument object or None
        :rtype: :py:class:`psyclone.psyGen.Argument`

        '''
        nodes_with_args = [x for x in nodes if isinstance(x, Call) or
                           isinstance(x, HaloExchange) or
                           isinstance(x, GlobalSum)]
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
        :return: a list of arguments that this argument has a dependence with
        :rtype: :func:`list` of :py:class:`psyclone.psyGen.Argument`

        '''
        if self.access not in self._write_access_types:
            # I am not a writer so there will be no read dependencies
            return []

        # We only need consider nodes that have arguments
        nodes_with_args = [x for x in nodes if isinstance(x, Call) or
                           isinstance(x, HaloExchange) or
                           isinstance(x, GlobalSum)]
        arguments = []
        for node in nodes_with_args:
            for argument in node.args:
                # look at all arguments in our nodes
                if argument.name != self.name:
                    # different names so there is no dependence
                    continue
                if isinstance(node, HaloExchange) and \
                   isinstance(self.call, HaloExchange):
                    # no dependence if both nodes are halo exchanges
                    # (as they act on different vector indices).
                    self.call.check_vector_halos_differ(node)
                    continue
                if argument.access in self._read_access_types:
                    # there is a read dependence so append to list
                    arguments.append(argument)
                if argument.access in self._write_access_types:
                    # there is a write dependence so finish our search
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
        :return: a list of arguments that this argument has a dependence with
        :rtype: :func:`list` of :py:class:`psyclone.psyGen.Argument`

        '''
        if self.access not in self._read_access_types:
            # I am not a reader so there will be no write dependencies
            return []

        # We only need consider nodes that have arguments
        nodes_with_args = [x for x in nodes if isinstance(x, Call) or
                           (isinstance(x, HaloExchange) and
                            not ignore_halos) or
                           isinstance(x, GlobalSum)]
        arguments = []
        vector_count = 0
        for node in nodes_with_args:
            for argument in node.args:
                # look at all arguments in our nodes
                if argument.name != self.name:
                    # different names so there is no dependence
                    continue
                if argument.access not in self._write_access_types:
                    # no dependence if not a writer
                    continue
                if isinstance(node, HaloExchange):
                    if isinstance(self.call, HaloExchange):
                        # no dependence if both nodes are halo
                        # exchanges (as they act on different vector
                        # indices).
                        self.call.check_vector_halos_differ(node)
                        continue
                    else:
                        # a vector read will depend on more than one
                        # halo exchange (a dependence for each vector
                        # index) as halo exchanges only act on a
                        # single vector index
                        vector_count += 1
                        arguments.append(argument)
                        if vector_count == self._vector_size:
                            # found all of the halo exchange
                            # dependencies. As they are writers
                            # we now return
                            return arguments
                else:
                    # this argument is a writer so add it and return
                    if arguments:
                        raise GenerationError(
                            "Internal error, found a writer dependence but "
                            "there are already dependencies. This should not "
                            "happen.")
                    return [argument]
        if arguments:
            raise GenerationError(
                "Internal error, no more nodes but there are "
                "already dependencies. This should not happen.")
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
        :return: True if there is a dependence and False if not
        :rtype: bool

        '''
        if argument.name == self._name:
            if self.access in self._write_access_types and \
               argument.access in self._read_access_types:
                return True
            if self.access in self._read_access_types and \
               argument.access in self._write_access_types:
                return True
            if self.access in self._write_access_types and \
               argument.access in self._write_access_types:
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
    >>> print t.list
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
                if inspect.isclass(cls) and
                issubclass(cls, base_class) and cls is not base_class]


class Transformation(object):
    ''' abstract baseclass for a transformation. Uses the abc module so it
        can not be instantiated. '''
    __metaclass__ = abc.ABCMeta

    @abc.abstractproperty
    def name(self):
        return

    @abc.abstractmethod
    def apply(self):
        schedule = None
        momento = None
        return schedule, momento


class DummyTransformation(Transformation):
    def name(self):
        return

    def apply(self):
        return None, None
