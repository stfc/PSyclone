# ------------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2015.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
# ------------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab

''' This module implements the PSyclone Dynamo 0.3 API by 1)
    specialising the required base classes in parser.py (Descriptor,
    KernelType) and adding a new class (DynFuncDescriptor03) to
    capture function descriptor metadata and 2) specialising the
    required base classes in psyGen.py (PSy, Invokes, Invoke, Schedule,
    Loop, Kern, Inf, Arguments and Argument). '''

# imports
from parse import Descriptor, KernelType, ParseError
import expression as expr
import fparser
import os
from psyGen import PSy, Invokes, Invoke, Schedule, Loop, Kern, Arguments, \
    Argument, Inf, NameSpaceFactory, GenerationError, FieldNotFoundError


# first section : Parser specialisations and classes

# constants
VALID_FUNCTION_SPACES = ["w0", "w1", "w2", "w3", "wtheta", "w2h", "w2v"]

VALID_ANY_SPACE_NAMES = ["any_space_1", "any_space_2", "any_space_3",
                         "any_space_4", "any_space_5", "any_space_6",
                         "any_space_7", "any_space_8", "any_space_9"]

VALID_FUNCTION_SPACE_NAMES = VALID_FUNCTION_SPACES + VALID_ANY_SPACE_NAMES

VALID_OPERATOR_NAMES = ["gh_basis", "gh_diff_basis", "gh_orientation"]

VALID_ARG_TYPE_NAMES = ["gh_field", "gh_operator"]

VALID_ACCESS_DESCRIPTOR_NAMES = ["gh_read", "gh_write", "gh_inc"]

# The mapping from meta-data strings to field-access types
# used in this API.
FIELD_ACCESS_MAP = {"write": "gh_write", "read": "gh_read",
                    "readwrite": "gh_rw", "inc": "gh_inc"}

# classes


class DynFuncDescriptor03(object):
    ''' The Dynamo 0.3 API includes a function-space descriptor as
    well as an argument descriptor which is not supported by the base
    classes. This class captures the information specified in a
    function-space descriptor. '''

    def __init__(self, func_type):
        self._func_type = func_type
        if func_type.name != 'func_type':
            raise ParseError(
                "In the dynamo0.3 API each meta_func entry must be of type "
                "'func_type' but found '{0}'".format(func_type.name))
        if len(func_type.args) < 2:
            raise ParseError(
                "In the dynamo0.3 API each meta_func entry must have at "
                "least 2 args, but found '{0}'".format(len(func_type.args)))
        self._operator_names = []
        for idx, arg in enumerate(func_type.args):
            if idx == 0:  # first func_type arg
                if arg.name not in VALID_FUNCTION_SPACE_NAMES:
                    raise ParseError(
                        "In the dynamo0p3 API the 1st argument of a "
                        "meta_func entry should be a valid function space "
                        "name (one of {0}), but found '{1}' in '{2}'".format(
                            VALID_FUNCTION_SPACE_NAMES, arg.name, func_type))
                self._function_space_name = arg.name
            else:  # subsequent func_type args
                if arg.name not in VALID_OPERATOR_NAMES:
                    raise ParseError(
                        "In the dynamo0.3 API, the 2nd argument and all "
                        "subsequent arguments of a meta_func entry should "
                        "be a valid operator name (one of {0}), but found "
                        "'{1}' in '{2}".format(VALID_OPERATOR_NAMES,
                                               arg.name, func_type))
                if arg.name in self._operator_names:
                    raise ParseError(
                        "In the dynamo0.3 API, it is an error to specify an "
                        "operator name more than once in a meta_func entry, "
                        "but '{0}' is replicated in '{1}".format(arg.name,
                                                                 func_type))
                self._operator_names.append(arg.name)
        self._name = func_type.name

    @property
    def function_space_name(self):
        ''' Returns the name of the descriptors function space '''
        return self._function_space_name

    @property
    def operator_names(self):
        ''' Returns a list of operators that are associated with this
        descriptors function space '''
        return self._operator_names

    def __repr__(self):
        return "DynFuncDescriptor03({0})".format(self._func_type)

    def __str__(self):
        res = "DynFuncDescriptor03 object" + os.linesep
        res += "  name='{0}'".format(self._name) + os.linesep
        res += "  nargs={0}".format(len(self._operator_names)+1) + os.linesep
        res += "  function_space_name[{0}] = '{1}'".\
               format(0, self._function_space_name) + os.linesep
        for idx, arg in enumerate(self._operator_names):
            res += "  operator_name[{0}] = '{1}'".format(idx+1, arg) + \
                   os.linesep
        return res


class DynArgDescriptor03(Descriptor):
    ''' This class captures the information specified in an argument
    descriptor.'''

    def __init__(self, arg_type):
        self._arg_type = arg_type
        if arg_type.name != 'arg_type':
            raise ParseError(
                "In the dynamo0.3 API aach meta_arg entry must be of type "
                "'arg_type', but found '{0}'".format(arg_type.name))
        # we require at least 3 args
        if len(arg_type.args) < 3:
            raise ParseError(
                "In the dynamo0.3 API each meta_arg entry must have at least "
                "3 args, but found '{0}'".format(len(arg_type.args)))
        # the first arg is the type of field, possibly with a *n appended
        self._vector_size = 1
        if isinstance(arg_type.args[0], expr.BinaryOperator):
            # we expect 'field_type * n' to have been specified
            self._type = arg_type.args[0].toks[0].name
            operator = arg_type.args[0].toks[1]
            try:
                self._vector_size = int(arg_type.args[0].toks[2])
            except TypeError:
                raise ParseError(
                    "In the dynamo0.3 API vector notation expects the format "
                    "(field*n) where n is an integer, but the following was "
                    "found '{0}' in '{1}'.".
                    format(str(arg_type.args[0].toks[2]), arg_type))
            if self._type not in VALID_ARG_TYPE_NAMES:
                raise ParseError(
                    "In the dynamo0.3 API the 1st argument of a meta_arg "
                    "entry should be a valid argument type (one of {0}), but "
                    "found '{1}' in '{2}'".format(VALID_ARG_TYPE_NAMES,
                                                  self._type, arg_type))
            if not operator == "*":
                raise ParseError(
                    "In the dynamo0.3 API the 1st argument of a meta_arg "
                    "entry may be a vector but if so must use '*' as the "
                    "separator in the format (field*n), but found '{0}' in "
                    "'{1}'".format(operator, arg_type))
            if not self._vector_size > 1:
                raise ParseError(
                    "In the dynamo0.3 API the 1st argument of a meta_arg "
                    "entry may be a vector but if so must contain a valid "
                    "integer vector size in the format (field*n where n>1), "
                    "but found '{0}' in '{1}'".format(self._vector_size,
                                                      arg_type))

        elif isinstance(arg_type.args[0], expr.FunctionVar):
            # we expect 'field_type' to have been specified
            if arg_type.args[0].name not in VALID_ARG_TYPE_NAMES:
                raise ParseError(
                    "In the dynamo0.3 API Each the 1st argument of a "
                    "meta_arg entry should be a valid argument type (one of "
                    "{0}), but found '{1}' in '{2}'".
                    format(VALID_ARG_TYPE_NAMES, arg_type.args[0].name,
                           arg_type))
            self._type = arg_type.args[0].name
        else:
            raise ParseError(
                "Internal error in DynArgDescriptor03.__init__, (1) should "
                "not get to here")
        # The 2nd arg is an access descriptor
        if arg_type.args[1].name not in VALID_ACCESS_DESCRIPTOR_NAMES:
            raise ParseError(
                "In the dynamo0.3 API the 2nd argument of a meta_arg entry "
                "must be a valid access descriptor (one of {0}), but found "
                "'{1}' in '{2}'".format(VALID_ACCESS_DESCRIPTOR_NAMES,
                                        arg_type.args[1].name, arg_type))
        self._access_descriptor = arg_type.args[1]
        if self._type == "gh_field":
            # we expect 3 arguments in total with the 3rd being a
            # function space
            if len(arg_type.args) != 3:
                raise ParseError(
                    "In the dynamo0.3 API each meta_arg entry must have 3 "
                    "arguments if its first argument is gh_field, but found "
                    "{0} in '{1}'").format(len(arg_type.args), arg_type)
            if arg_type.args[2].name not in VALID_FUNCTION_SPACE_NAMES:
                raise ParseError(
                    "In the dynamo0.3 API the 3rd argument of a meta_arg "
                    "entry must be a valid function space name (one of {0}), "
                    "but found '{1}' in '{2}".
                    format(VALID_FUNCTION_SPACE_NAMES, arg_type.args[2].name,
                           arg_type))
            self._function_space1 = arg_type.args[2].name
        elif self._type == "gh_operator":
            # we expect 4 arguments with the 3rd and 4th each being a
            # function space
            if len(arg_type.args) != 4:
                raise ParseError(
                    "In the dynamo0.3 API each meta_arg entry must have 4 "
                    "arguments if its first argument is gh_operator, but "
                    "found {0} in '{1}'".format(len(arg_type.args), arg_type))
            if arg_type.args[2].name not in VALID_FUNCTION_SPACE_NAMES:
                raise ParseError(
                    "In the dynamo0.3 API the 3rd argument of a meta_arg "
                    "entry must be a valid function space name (one of {0}), "
                    "but found '{1}' in '{2}".
                    format(VALID_FUNCTION_SPACE_NAMES, arg_type.args[2].name,
                           arg_type))
            self._function_space1 = arg_type.args[2].name
            if arg_type.args[3].name not in VALID_FUNCTION_SPACE_NAMES:
                raise ParseError(
                    "In the dynamo0.3 API the 4th argument of a meta_arg "
                    "entry must be a valid function space name (one of {0}), "
                    "but found '{1}' in '{2}".
                    format(VALID_FUNCTION_SPACE_NAMES, arg_type.args[2].name,
                           arg_type))
            self._function_space2 = arg_type.args[3].name
        else:  # we should never get to here
            raise ParseError(
                "Internal error in DynArgDescriptor03.__init__, (2) should "
                "not get to here")
        Descriptor.__init__(self, self._access_descriptor.name,
                            self._function_space1, None)

    @property
    def function_space_to(self):
        ''' Return the "to" function space for a gh_operator. This is
        the first function space specified in the metadata. Raise an
        error if this is not an operator. '''
        if self._type == "gh_operator":
            return self._function_space1
        else:
            raise RuntimeError(
                "function_space_to only makes sense for a gh_operator, but "
                "this is a '{0}'".format(self._type))

    @property
    def function_space_from(self):
        ''' Return the "from" function space for a gh_operator. This is
        the second function space specified in the metadata. Raise an
        error if this is not an operator. '''
        if self._type == "gh_operator":
            return self._function_space2
        else:
            raise RuntimeError(
                "function_space_from only makes sense for a gh_operator, but "
                "this is a '{0}'".format(self._type))

    @property
    def function_space(self):
        ''' Return the function space name that this instance operates
        on. In the case of a gh_operator, where there are 2 function
        spaces, return function_space_from. '''
        if self._type == "gh_field":
            return self._function_space1
        elif self._type == "gh_operator":
            return self._function_space2
        else:
            raise RuntimeError(
                "Internal error, DynArgDescriptor03:function_space(), should "
                "not get to here.")

    @property
    def is_any_space(self):
        ''' Returns True if this descriptor is of type any_space. This
        could be any on the any_space spaces, i.e. any of any_space_1,
        any_space_2, ... any_space_9, otherwise returns False. For
        operators, returns True if the source descriptor is of type
        any_space, else returns False. '''
        if self.function_space in VALID_ANY_SPACE_NAMES:
            return True
        else:
            return False

    @property
    def vector_size(self):
        ''' Returns the vector size of the argument. This will be 1 if *n
        has not been specified. '''
        return self._vector_size

    @property
    def type(self):
        ''' returns the type of the argument (gh_field, gh_operator, ...). '''
        return self._type

    def __str__(self):
        res = "DynArgDescriptor03 object" + os.linesep
        res += "  argument_type[0]='{0}'".format(self._type)
        if self._vector_size > 1:
            res += "*"+str(self._vector_size)
        res += os.linesep
        res += "  access_descriptor[1]='{0}'".format(self._access_descriptor) \
               + os.linesep
        if self._type == "gh_field":
            res += "  function_space[2]='{0}'".format(self._function_space1) \
                   + os.linesep
        elif self._type == "gh_operator":
            res += "  function_space_to[2]='{0}'".\
                   format(self._function_space1) + os.linesep
            res += "  function_space_from[3]='{0}'".\
                   format(self._function_space2) + os.linesep
        else:  # we should never get to here
            raise ParseError("Internal error in DynArgDescriptor03.__str__")
        return res

    def __repr__(self):
        return "DynArgDescriptor03({0})".format(self._arg_type)


class DynKernMetadata(KernelType):
    ''' Captures the Kernel subroutine code and metadata describing
    the subroutine for the Dynamo 0.3 API. '''

    def __init__(self, ast, name=None):
        KernelType.__init__(self, ast, name=name)
        # parse the arg_type metadata
        self._arg_descriptors = []
        for arg_type in self._inits:
            self._arg_descriptors.append(DynArgDescriptor03(arg_type))
        # parse the func_type metadata if it exists
        found = False
        for line in self._ktype.content:
            if isinstance(line, fparser.typedecl_statements.Type):
                for entry in line.selector:
                    if entry == "func_type":
                        if line.entity_decls[0].split()[0].split("(")[0] == \
                                "meta_funcs":
                            found = True
                            break
        if not found:
            func_types = []
        else:
            # use the base class method to extract the information
            func_types = self.getkerneldescriptors(self._ktype,
                                                   var_name="meta_funcs")
        self._func_descriptors = []
        # populate a list of function descriptor objects which we
        # return via the func_descriptors method.
        arg_fs_names = []
        for descriptor in self._arg_descriptors:
            arg_fs_names.append(descriptor.function_space)
        used_fs_names = []
        for func_type in func_types:
            descriptor = DynFuncDescriptor03(func_type)
            fs_name = descriptor.function_space_name
            # check that function space names in meta_funcs are specified in
            # meta_args
            if fs_name not in arg_fs_names:
                raise ParseError(
                    "In the dynamo0.3 API all function spaces specified in "
                    "meta_funcs must exist in meta_args, but '{0}' breaks "
                    "this rule in ...\n'{1}'.".
                    format(fs_name, self._ktype.content))
            if fs_name not in used_fs_names:
                used_fs_names.append(fs_name)
            else:
                raise ParseError(
                    "In the dynamo0.3 API function spaces specified in "
                    "meta_funcs must be unique, but '{0}' is replicated in "
                    "...\n'{1}'.".format(fs_name, self._ktype.content))
            self._func_descriptors.append(descriptor)

    @property
    def func_descriptors(self):
        ''' Returns metadata about the function spaces within a
        Kernel. This metadata is provided within Kernel code via the
        meta_funcs variable. Information is returned as a list of
        DynFuncDescriptor03 objects, one for each function space. '''
        return self._func_descriptors

# Second section : PSy specialisations

# classes


class DynamoPSy(PSy):
    ''' The Dynamo specific PSy class. This creates a Dynamo specific
    invokes object (which controls all the required invocation calls).
    It also overrides the PSy gen method so that we generate dynamo
    specific PSy module code. '''

    def __init__(self, invoke_info):
        PSy.__init__(self, invoke_info)
        self._invokes = DynamoInvokes(invoke_info.calls)

    @property
    def gen(self):
        '''
        Generate PSy code for the Dynamo0.3 api.

        :rtype: ast

        '''
        from f2pygen import ModuleGen, UseGen
        # create an empty PSy layer module
        psy_module = ModuleGen(self.name)
        # include required infrastructure modules
        psy_module.add(UseGen(psy_module, name="field_mod", only=True,
                              funcnames=["field_type", "field_proxy_type"]))
        psy_module.add(UseGen(psy_module, name="operator_mod", only=True,
                              funcnames=["operator_type",
                                         "operator_proxy_type"]))
        psy_module.add(UseGen(psy_module, name="quadrature_mod", only=True,
                              funcnames=["quadrature_type"]))
        psy_module.add(UseGen(psy_module, name="constants_mod", only=True,
                              funcnames=["r_def"]))
        # add all invoke specific information
        self.invokes.gen_code(psy_module)
        # inline kernels where requested
        self.inline(psy_module)
        # return the generated code
        return psy_module.root


class DynamoInvokes(Invokes):
    ''' The Dynamo specific invokes class. This passes the Dynamo
    specific invoke class to the base class so it creates the one we
    require. '''

    def __init__(self, alg_calls):
        if False:
            self._0_to_n = DynInvoke(None, None)  # for pyreverse
        Invokes.__init__(self, alg_calls, DynInvoke)


class DynInvoke(Invoke):
    ''' The Dynamo specific invoke class. This passes the Dynamo
    specific schedule class to the base class so it creates the one we
    require.  Also overrides the gen_code method so that we generate
    dynamo specific invocation code. '''

    def __init__(self, alg_invocation, idx):
        if False:
            self._schedule = DynSchedule(None)  # for pyreverse
        Invoke.__init__(self, alg_invocation, idx, DynSchedule)
        # check whether we have more than one kernel call within this
        # invoke which specifies any_space. This is not supported at
        # the moment so we raise an error.  any_space with different
        # kernels in an invoke must either inherit the space from the
        # variable (which needs analysis) or have a unique name for
        # the space used by each kernel and at the moment neither of
        # these has been coded for.
        any_space_call_count = 0
        for call in self.schedule.calls():
            found_any_space = False
            for arg_descriptor in call.arg_descriptors:
                if arg_descriptor.is_any_space:
                    found_any_space = True
                    break
            if found_any_space:
                any_space_call_count += 1
        if any_space_call_count > 1:
            raise GenerationError(
                "Error, there are multiple kernels within this invoke with "
                "kernel arguments declared as any_space. This is not yet "
                "supported.")
        # the baseclass works out the algorithms codes unique argument
        # list and stores it in the self._alg_unique_args
        # list. However, the base class currently ignores any qr
        # arguments so we need to add them in.
        self._alg_unique_qr_args = []
        for call in self.schedule.calls():
            if call.qr_required:
                if call.qr_text not in self._alg_unique_qr_args:
                    self._alg_unique_qr_args.append(call.qr_text)
        self._alg_unique_args.extend(self._alg_unique_qr_args)
        # we also need to work out the names to use for the qr
        # arguments within the psy layer. These are stored in the
        # _psy_unique_qr_vars list
        self._psy_unique_qr_vars = []
        for call in self.schedule.calls():
            if call.qr_required:
                if call.qr_name not in self._psy_unique_qr_vars:
                    self._psy_unique_qr_vars.append(call.qr_name)

    @property
    def qr_required(self):
        ''' Returns True if at least one of the kernels in this invoke
        requires QR, otherwise returns False. '''
        required = False
        for call in self.schedule.calls():
            if call.qr_required:
                required = True
                break
        return required

    def unique_declarations(self, datatype, proxy=False):
        ''' Returns a list of all required declarations for the
        specified datatype. If proxy is set to True then the
        equivalent proxy declarations are returned instead. '''
        if datatype not in VALID_ARG_TYPE_NAMES:
            raise GenerationError(
                "unique_declarations called with an invalid datatype. "
                "Expected one of '{0}' but found '{1}'".
                format(str(VALID_ARG_TYPE_NAMES), datatype))
        declarations = []
        for call in self.schedule.calls():
            for arg in call.arguments.args:
                if arg.text is not None:
                    if arg.type == datatype:
                        if proxy:
                            test_name = arg.proxy_declaration_name
                        else:
                            test_name = arg.declaration_name
                        if test_name not in declarations:
                            declarations.append(test_name)
        return declarations

    def arg_for_funcspace(self, fs_name):
        ''' Returns an argument object which is on the requested
        function space. Searches through all Kernel calls in this
        invoke. Currently the first argument object that is found is
        used. Throws an exception if no argument exists. '''
        for kern_call in self.schedule.kern_calls():
            if fs_name in kern_call.arguments.unique_fss:
                for arg in kern_call.arguments.args:
                    if arg.function_space == fs_name:
                        return arg
        raise GenerationError("Functionspace name not found")

    def unique_fss(self):
        ''' Returns the unique function space names over all kernel
        calls in this invoke. '''
        unique_fs_names = []
        for kern_call in self.schedule.kern_calls():
            for fs_name in kern_call.arguments.unique_fss:
                if fs_name not in unique_fs_names:
                    unique_fs_names.append(fs_name)
        return unique_fs_names

    def basis_required(self, func_space):
        ''' Returns true if at least one of the kernels in this invoke
        requires a basis function for this function space, otherwise
        it returns False. '''
        # look in each kernel
        for kern_call in self.schedule.kern_calls():
            # is there a descriptor for this function space?
            if kern_call.fs_descriptors.exists(func_space):
                descriptor = kern_call.fs_descriptors.get_descriptor(
                    func_space)
                # does this descriptor specify that a basis function
                # is required?
                if descriptor.requires_basis:
                    # found a kernel that requires a basis function
                    # for this function space
                    return True
        # none of my kernels require a basis function for this function space
        return False

    def diff_basis_required(self, func_space):
        ''' Returns true if at least one of the kernels in this invoke
        requires a differential basis function for this function
        space, otherwise it returns False.'''
        # look in each kernel
        for kern_call in self.schedule.kern_calls():
            # is there a descriptor for this function space?
            if kern_call.fs_descriptors.exists(func_space):
                descriptor = kern_call.fs_descriptors.get_descriptor(
                    func_space)
                # does this descriptor specify that a basis function
                # is required?
                if descriptor.requires_diff_basis:
                    # found a kernel that requires a diff basis
                    # function for this function space
                    return True
        # none of my kernels require a diff basis function for this
        # function space
        return False

    def is_coloured(self):
        ''' Returns true if at least one of the loops in the
        schedule of this invoke has been coloured '''
        for loop in self.schedule.loops():
            if loop.loop_type == "colours":
                return True
        return False

    def ndf_name(self, func_space):
        ''' A convenience method that returns an ndf name for a
        particular function space. These names are specified in
        function_space_descriptors objects contained within Kernel
        objects. The first Kernel in the invoke is used to return the
        name. If no Kernel exist in this invoke an error is thrown. '''
        kern_calls = self.schedule.kern_calls()
        if len(kern_calls) == 0:
            raise GenerationError(
                "ndf_name makes no sense if there are no kernel calls")
        return kern_calls[0].fs_descriptors.ndf_name(func_space)

    def undf_name(self, func_space):
        ''' A convenience method that returns an undf name for a
        particular function space. These names are specified in
        function_space_descriptors objects contained within Kernel
        objects. The first Kernel in the invoke is used to return the
        name. If no Kernel exists in this invoke an error is thrown. '''
        kern_calls = self.schedule.kern_calls()
        if len(kern_calls) == 0:
            raise GenerationError(
                "undf_name makes no sense if there are no kernel calls")
        return kern_calls[0].fs_descriptors.undf_name(func_space)

    def get_operator_name(self, operator_name, function_space):
        ''' A convenience method that returns an operator name for a
        particular operator on a particular function space. These
        names are specified in function_space_descriptors objects
        contained within Kernel objects. The first Kernel which uses
        the specified function space is used to return the name. If no
        Kernel using this function space exists in this invoke, an
        error is thrown. '''
        for kern_call in self.schedule.kern_calls():
            if kern_call.fs_descriptors.exists(function_space):
                descriptor = kern_call.fs_descriptors.get_descriptor(
                    function_space)
                return descriptor.name(operator_name)
        raise GenerationError(
            "Dyn_invoke:get_operator_name: no kern call with function space "
            "'{0}' and operator '{1}'".format(function_space, operator_name))

    def field_on_space(self, func_space):
        ''' Returns true if a field exists on this space for any
        kernel in this invoke. '''
        for kern_call in self.schedule.kern_calls():
            if kern_call.field_on_space(func_space):
                return True
        return False

    def gen_code(self, parent):
        ''' Generates Dynamo specific invocation code (the subroutine
        called by the associated invoke call in the algorithm
        layer). This consists of the PSy invocation subroutine and the
        declaration of its arguments. '''
        from f2pygen import SubroutineGen, TypeDeclGen, AssignGen, DeclGen, \
            AllocateGen, DeallocateGen, CallGen, CommentGen
        # create a namespace manager so we can avoid name clashes
        self._name_space_manager = NameSpaceFactory().create()
        # create the subroutine
        invoke_sub = SubroutineGen(parent, name=self.name,
                                   args=self.psy_unique_var_names +
                                   self._psy_unique_qr_vars)
        # add the subroutine argument declarations fields
        field_declarations = self.unique_declarations("gh_field")
        if len(field_declarations) > 0:
            invoke_sub.add(TypeDeclGen(invoke_sub, datatype="field_type",
                                       entity_decls=field_declarations,
                                       intent="inout"))
        # operators
        operator_declarations = self.unique_declarations("gh_operator")
        if len(operator_declarations) > 0:
            invoke_sub.add(TypeDeclGen(invoke_sub, datatype="operator_type",
                                       entity_decls=operator_declarations,
                                       intent="inout"))
        # qr
        if len(self._psy_unique_qr_vars) > 0:
            invoke_sub.add(TypeDeclGen(invoke_sub, datatype="quadrature_type",
                                       entity_decls=self._psy_unique_qr_vars,
                                       intent="in"))
        # declare and initialise proxies for each of the arguments
        invoke_sub.add(CommentGen(invoke_sub, ""))
        invoke_sub.add(CommentGen(invoke_sub, " Initialise field proxies"))
        invoke_sub.add(CommentGen(invoke_sub, ""))
        for arg in self.psy_unique_vars:
            if arg.vector_size > 1:
                for idx in range(1, arg.vector_size+1):
                    invoke_sub.add(AssignGen(invoke_sub,
                                   lhs=arg.proxy_name+"("+str(idx)+")",
                                   rhs=arg.name+"("+str(idx)+")%get_proxy()"))
            else:
                invoke_sub.add(AssignGen(invoke_sub, lhs=arg.proxy_name,
                                         rhs=arg.name+"%get_proxy()"))

        field_proxy_decs = self.unique_declarations("gh_field", proxy=True)
        if len(field_proxy_decs) > 0:
            invoke_sub.add(TypeDeclGen(invoke_sub,
                           datatype="field_proxy_type",
                           entity_decls=field_proxy_decs))
        op_proxy_decs = self.unique_declarations("gh_operator", proxy=True)
        if len(op_proxy_decs) > 0:
            invoke_sub.add(TypeDeclGen(invoke_sub,
                           datatype="operator_proxy_type",
                           entity_decls=op_proxy_decs))
        # Initialise the number of layers
        invoke_sub.add(CommentGen(invoke_sub, ""))
        invoke_sub.add(CommentGen(invoke_sub, " Initialise number of layers"))
        invoke_sub.add(CommentGen(invoke_sub, ""))
        # use the first argument
        first_var = self.psy_unique_vars[0]
        # use our namespace manager to create a unique name unless
        # the context and label match and in this case return the
        # previous name
        nlayers_name = self._name_space_manager.create_name(
            root_name="nlayers", context="PSyVars", label="nlayers")
        invoke_sub.add(AssignGen(invoke_sub, lhs=nlayers_name,
                       rhs=first_var.proxy_name_indexed + "%" +
                       first_var.ref_name + "%get_nlayers()"))
        invoke_sub.add(DeclGen(invoke_sub, datatype="integer",
                               entity_decls=[nlayers_name]))
        if self.qr_required:
            # declare and initialise qr values
            invoke_sub.add(CommentGen(invoke_sub, ""))
            invoke_sub.add(CommentGen(invoke_sub, " Initialise qr values"))
            invoke_sub.add(CommentGen(invoke_sub, ""))
            invoke_sub.add(DeclGen(invoke_sub, datatype="integer",
                           entity_decls=["nqp_h", "nqp_v"]))
            invoke_sub.add(DeclGen(invoke_sub, datatype="real", pointer=True,
                           kind="r_def", entity_decls=["xp(:,:) => null()"]))
            decl_list = ["zp(:) => null()", "wh(:) => null()",
                         "wv(:) => null()"]
            invoke_sub.add(DeclGen(invoke_sub, datatype="real", pointer=True,
                           kind="r_def", entity_decls=decl_list))
            if len(self._psy_unique_qr_vars) > 1:
                raise GenerationError(
                    "Oops, not yet coded for multiple qr values")
            qr_var_name = self._psy_unique_qr_vars[0]
            qr_ptr_vars = {"zp": "xqp_v", "xp": "xqp_h", "wh": "wqp_h",
                           "wv": "wqp_v"}
            qr_vars = ["nqp_h", "nqp_v"]
            for qr_var in qr_ptr_vars.keys():
                invoke_sub.add(AssignGen(invoke_sub, pointer=True, lhs=qr_var,
                               rhs=qr_var_name + "%get_" +
                               qr_ptr_vars[qr_var] + "()"))
            for qr_var in qr_vars:
                invoke_sub.add(AssignGen(invoke_sub, lhs=qr_var,
                               rhs=qr_var_name + "%get_" + qr_var + "()"))
        operator_declarations = []
        var_list = []
        var_dim_list = []
        # loop over all function spaces used by the kernels in this invoke
        for function_space in self.unique_fss():
            # Initialise information associated with this function space
            invoke_sub.add(CommentGen(invoke_sub, ""))
            invoke_sub.add(CommentGen(invoke_sub, " Initialise sizes and "
                           "allocate any basis arrays for "+function_space))
            invoke_sub.add(CommentGen(invoke_sub, ""))
            # Find an argument on this space to use to dereference
            arg = self.arg_for_funcspace(function_space)
            name = arg.proxy_name_indexed
            # initialise ndf for this function space and add name to
            # list to declare later
            ndf_name = self.ndf_name(function_space)
            var_list.append(ndf_name)
            invoke_sub.add(AssignGen(invoke_sub, lhs=ndf_name,
                                     rhs=name+"%"+arg.ref_name+"%get_ndf()"))
            # if there is a field on this space then initialise undf
            # for this function space and add name to list to declare
            # later
            if self.field_on_space(function_space):
                undf_name = self.undf_name(function_space)
                var_list.append(undf_name)
                invoke_sub.add(AssignGen(invoke_sub, lhs=undf_name,
                               rhs=name+"%"+arg.ref_name+"%get_undf()"))
            if self.basis_required(function_space):
                # initialise 'dim' variable for this function space
                # and add name to list to declare later
                lhs = "dim_"+function_space
                var_dim_list.append(lhs)
                rhs = name+"%"+arg.ref_name+"%get_dim_space()"
                invoke_sub.add(AssignGen(invoke_sub, lhs=lhs, rhs=rhs))
                # allocate the basis function variable
                alloc_args = "dim_" + function_space + ", " + \
                             self.ndf_name(function_space) + ", nqp_h, nqp_v"
                op_name = self.get_operator_name("gh_basis", function_space)
                invoke_sub.add(AllocateGen(invoke_sub,
                                           op_name+"("+alloc_args+")"))
                # add basis function variable to list to declare later
                operator_declarations.append(op_name+"(:,:,:,:)")
            if self.diff_basis_required(function_space):
                # initialise 'diff_dim' variable for this function
                # space and add name to list to declare later
                lhs = "diff_dim_"+function_space
                var_dim_list.append(lhs)
                rhs = name+"%"+arg.ref_name+"%get_dim_space_diff()"
                invoke_sub.add(AssignGen(invoke_sub, lhs=lhs, rhs=rhs))
                # allocate the diff basis function variable
                alloc_args = "diff_dim_" + function_space + ", " + \
                             self.ndf_name(function_space) + ", nqp_h, nqp_v"
                op_name = self.get_operator_name("gh_diff_basis",
                                                 function_space)
                invoke_sub.add(AllocateGen(invoke_sub,
                                           op_name+"("+alloc_args+")"))
                # add diff basis function variable to list to declare later
                operator_declarations.append(op_name+"(:,:,:,:)")
        if not var_list == []:
            # declare ndf and undf for all function spaces
            invoke_sub.add(DeclGen(invoke_sub, datatype="integer",
                                   entity_decls=var_list))
        if not var_dim_list == []:
            # declare dim and diff_dim for all function spaces
            invoke_sub.add(DeclGen(invoke_sub, datatype="integer",
                                   entity_decls=var_dim_list))
        if not operator_declarations == []:
            # declare the basis function operators
            invoke_sub.add(DeclGen(invoke_sub, datatype="real",
                                   allocatable=True,
                                   kind="r_def",
                                   entity_decls=operator_declarations))

        if self.is_coloured():
            # Add declarations of the colour map and array holding the
            # no. of cells of each colour
            invoke_sub.add(DeclGen(parent, datatype="integer",
                                   pointer=True,
                                   entity_decls=["cmap(:,:)",
                                                 "ncp_colour(:)"]))
            # Declaration of variable to hold the number of colours
            invoke_sub.add(DeclGen(parent, datatype="integer",
                                   entity_decls=["ncolour"]))

        if self.qr_required:
            # add calls to compute the values of any basis arrays
            invoke_sub.add(CommentGen(invoke_sub, ""))
            invoke_sub.add(CommentGen(invoke_sub, " Compute basis arrays"))
            invoke_sub.add(CommentGen(invoke_sub, ""))
            # only look at function spaces that are used by the
            # kernels in this invoke
            for function_space in self.unique_fss():
                # see if a basis function is needed for this function space
                if self.basis_required(function_space):
                    # Create the argument list
                    args = []
                    op_name = self.get_operator_name("gh_basis",
                                                     function_space)
                    args.append(op_name)
                    args.append(self.ndf_name(function_space))
                    args.extend(["nqp_h", "nqp_v", "xp", "zp"])
                    # find an appropriate field to access
                    arg = self.arg_for_funcspace(function_space)
                    name = arg.proxy_name_indexed
                    # insert the basis array call
                    invoke_sub.add(CallGen(invoke_sub,
                                   name=name + "%" + arg.ref_name +
                                   "%compute_basis_function", args=args))
                if self.diff_basis_required(function_space):
                    # Create the argument list
                    args = []
                    op_name = self.get_operator_name("gh_diff_basis",
                                                     function_space)
                    args.append(op_name)
                    args.append(self.ndf_name(function_space))
                    args.extend(["nqp_h", "nqp_v", "xp", "zp"])
                    # find an appropriate field to access
                    arg = self.arg_for_funcspace(function_space)
                    name = arg.proxy_name_indexed
                    # insert the diff basis array call
                    invoke_sub.add(CallGen(invoke_sub, name=name + "%" +
                                   arg.ref_name +
                                   "%compute_diff_basis_function", args=args))
        invoke_sub.add(CommentGen(invoke_sub, ""))
        invoke_sub.add(CommentGen(invoke_sub, " Call our kernels"))
        invoke_sub.add(CommentGen(invoke_sub, ""))
        # add content from the schedule
        self.schedule.gen_code(invoke_sub)
        if self.qr_required:
            # deallocate all allocated basis function arrays
            invoke_sub.add(CommentGen(invoke_sub, ""))
            invoke_sub.add(CommentGen(invoke_sub, " Deallocate basis arrays"))
            invoke_sub.add(CommentGen(invoke_sub, ""))
            func_space_var_names = []
            # loop over all function spaces used by the kernels in this invoke
            for function_space in self.unique_fss():
                if self.basis_required(function_space):
                    # add the basis array name to the list to use later
                    op_name = self.get_operator_name("gh_basis",
                                                     function_space)
                    func_space_var_names.append(op_name)
                if self.diff_basis_required(function_space):
                    # add the diff_basis array name to the list to use later
                    op_name = self.get_operator_name("gh_diff_basis",
                                                     function_space)
                    func_space_var_names.append(op_name)
            # add the required deallocate call
            invoke_sub.add(DeallocateGen(invoke_sub, func_space_var_names))
        invoke_sub.add(CommentGen(invoke_sub, ""))
        # finally, add me to my parent
        parent.add(invoke_sub)


class DynSchedule(Schedule):
    ''' The Dynamo specific schedule class. This passes the Dynamo
    specific loop and infrastructure classes to the base class so it
    creates the ones we require. '''

    def __init__(self, arg):
        Schedule.__init__(self, DynLoop, DynInf, arg)


class DynLoop(Loop):
    ''' The Dynamo specific Loop class. This passes the Dynamo
    specific loop information to the base class so it creates the one
    we require.  Creates Dynamo specific loop bounds when the code is
    being generated. '''

    def __init__(self, call=None, parent=None,
                 loop_type=""):
        Loop.__init__(self, DynInf, DynKern, call=call, parent=parent,
                      valid_loop_types=["colours", "colour", ""])
        self.loop_type = loop_type

        if self._loop_type == "colours":
            self._variable_name = "colour"
        elif self._loop_type == "colour":
            self._variable_name = "cell"
        else:
            self._variable_name = "cell"

    def has_inc_arg(self, mapping=None):
        ''' Returns True if any of the Kernels called within this loop
        have an argument with INC access. Returns False otherwise. '''
        if mapping is not None:
            my_mapping = mapping
        else:
            my_mapping = FIELD_ACCESS_MAP
        return Loop.has_inc_arg(self, my_mapping)

    def gen_code(self, parent):
        ''' Work out the appropriate loop bounds and variable name
        depending on the loop type and then call the base class to
        generate the code. '''

        # Check that we're not within an OpenMP parallel region if
        # we are a loop over colours.
        if self._loop_type == "colours" and self.is_openmp_parallel():
                    raise GenerationError("Cannot have a loop over "
                                          "colours within an OpenMP "
                                          "parallel region.")
        # Set-up loop bounds
        self._start = "1"
        if self._loop_type == "colours":
            self._stop = "ncolour"
        elif self._loop_type == "colour":
            self._stop = "ncp_colour(colour)"
        else:
            self._stop = self.field.proxy_name_indexed + "%" + \
                self.field.ref_name + "%get_ncell()"
        Loop.gen_code(self, parent)


class DynInf(Inf):
    ''' A Dynamo 0.3 specific infrastructure call factory. No
    infrastructure calls are supported in Dynamo at the moment so we
    just call the base class (which currently recognises the set()
    infrastructure call). '''

    @staticmethod
    def create(call, parent=None):
        ''' Creates a specific infrastructure call. Currently just calls
            the base class method. '''
        return Inf.create(call, parent)


class DynKern(Kern):
    ''' Stores information about Dynamo Kernels as specified by the
    Kernel metadata and associated algorithm call. Uses this
    information to generate appropriate PSy layer code for the Kernel
    instance or to generate a Kernel stub'''

    def __init__(self):
        if False:
            self._arguments = DynKernelArguments(None, None)  # for pyreverse

    def load(self, call, parent=None):
        ''' sets up kernel information with the call object which is
        created by the parser. This object includes information about
        the invoke call and the associated kernel'''
        self._setup_qr(call.ktype.func_descriptors)
        self._setup(call.ktype, call.module_name, call.args, parent)

    def load_meta(self, ktype):
        ''' sets up kernel information with the kernel type object
        which is created by the parser. The object includes the
        metadata describing the kernel code '''

        # create a name for each argument
        from parse import Arg
        args = []
        for idx, descriptor in enumerate(ktype.arg_descriptors):
            pre = None
            if descriptor.type.lower() == "gh_operator":
                pre = "op_"
            elif descriptor.type.lower() == "gh_field":
                pre = "field_"
            else:
                raise GenerationError(
                    "load_meta expected one of 'gh_field, gh_operator' but "
                    "found '{0}'".format(descriptor.type))
            args.append(Arg("variable", pre+str(idx+1)))
        # initialise qr so we can test whether it is required
        self._setup_qr(ktype.func_descriptors)
        if self._qr_required:
            # it is required so add a qr algorithm argument
            args.append(Arg("variable", "qr"))
        self._setup(ktype, "dummy_name", args, None)

    def _setup_qr(self, func_descriptors):
        ''' initialisation of the qr information. This may be needed before
        general setup so is computed in a separate method. '''
        self._qr_required = False
        for descriptor in func_descriptors:
            if len(descriptor.operator_names) > 0:
                self._qr_required = True
                break

    def _setup(self, ktype, module_name, args, parent):
        ''' internal setup of kernel information. '''
        from parse import KernelCall
        Kern.__init__(self, DynKernelArguments,
                      KernelCall(module_name, ktype, args),
                      parent, check=False)
        self._func_descriptors = ktype.func_descriptors
        self._fs_descriptors = FSDescriptors(ktype.func_descriptors)
        # dynamo 0.3 api kernels require quadrature rule arguments to be
        # passed in if one or more basis functions are used by the kernel.
        self._qr_args = {"nh": "nqp_h", "nv": "nqp_v", "h": "wh", "v": "wv"}
        # perform some consistency checks as we have switched these
        # off in the base class
        if self._qr_required:
            # check we have an extra argument in the algorithm call
            if len(ktype.arg_descriptors)+1 != len(args):
                raise GenerationError(
                    "error: QR is required for kernel '{0}' which means that "
                    "a QR argument must be passed by the algorithm layer. "
                    "Therefore the number of arguments specified in the "
                    "kernel metadata '{1}', must be one less than the number "
                    "of arguments in the algorithm layer. However, I found "
                    "'{2}'".format(ktype.procedure.name,
                                   len(ktype.arg_descriptors),
                                   len(args)))
        else:
            # check we have the same number of arguments in the
            # algorithm call and the kernel metadata
            if len(ktype.arg_descriptors) != len(args):
                raise GenerationError(
                    "error: QR is not required for kernel '{0}'. Therefore "
                    "the number of arguments specified in the kernel "
                    "metadata '{1}', must equal the number of arguments in "
                    "the algorithm layer. However, I found '{2}'".
                    format(ktype.procedure.name,
                           len(ktype.arg_descriptors), len(args)))
        # if there is a quadrature rule, what is the name of the
        # algorithm argument?
        self._qr_text = ""
        self._qr_name = ""
        if self._qr_required:
            qr_arg = args[-1]
            self._qr_text = qr_arg.text
            self._name_space_manager = NameSpaceFactory().create()
            # use our namespace manager to create a unique name unless
            # the context and label match and in this case return the
            # previous name
            self._qr_name = self._name_space_manager.create_name(
                root_name=qr_arg.varName, context="AlgArgs",
                label=self._qr_text)

    @property
    def fs_descriptors(self):
        ''' Returns a list of function space descriptor objects of
        type FSDescriptor which contain information about the function
        spaces. '''
        return self._fs_descriptors

    @property
    def qr_required(self):
        ''' Returns True if this kernel makes use of a quadrature
        rule, else returns False. '''
        return self._qr_required

    @property
    def qr_text(self):
        ''' Returns the QR argument-text used by the algorithm layer
        in the calling argument list. '''
        return self._qr_text

    @property
    def qr_name(self):
        ''' Returns a Quadrature-rule name for this Kernel. '''
        return self._qr_name

    def local_vars(self):
        ''' Returns the names used by the Kernel that vary from one
        invocation to the next and therefore require privatisation
        when parallelised. '''
        lvars = []
        # Dof maps for fields
        for unique_fs in self.arguments.unique_fss:
            if self.field_on_space(unique_fs):
                # A map is required as there is a field on this space
                lvars.append(self._fs_descriptors.map_name(unique_fs))
        # Orientation maps
        for unique_fs in self.arguments.unique_fss:
            if self._fs_descriptors.exists(unique_fs):
                fs_descriptor = self._fs_descriptors.get_descriptor(unique_fs)
                if fs_descriptor.orientation:
                    lvars.append(fs_descriptor.orientation_name)
        return lvars

    def field_on_space(self, func_space):
        ''' Returns True if a field exists on this space for this kernel. '''
        if func_space in self.arguments.unique_fss:
            for arg in self.arguments.args:
                if arg.function_space == func_space and \
                        arg.type == "gh_field":
                    return True
        return False

    def _create_arg_list(self, parent, my_type="call"):
        ''' creates the kernel call or kernel stub subroutine argument
        list. For kernel stubs it also creates the data
        declarations. '''
        from f2pygen import DeclGen, AssignGen, UseGen
        if my_type == "subroutine":
            # add in any required USE associations
            parent.add(UseGen(parent, name="constants_mod", only="True",
                              funcnames=["r_def"]))
        # create the argument list
        arglist = []
        if self._arguments.has_operator:
            # 0.5: provide cell position
            arglist.append("cell")
            if my_type == "subroutine":
                parent.add(DeclGen(parent, datatype="integer", intent="in",
                                   entity_decls=["cell"]))
        # 1: provide mesh height
        arglist.append("nlayers")
        if my_type == "subroutine":
            parent.add(DeclGen(parent, datatype="integer", intent="in",
                               entity_decls=["nlayers"]))
        # 2: Provide data associated with fields in the order
        #    specified in the metadata.  If we have a vector field
        #    then generate the appropriate number of arguments.
        first_arg = True
        first_arg_decl = None
        for arg in self._arguments.args:
            undf_name = self._fs_descriptors.undf_name(arg.function_space)
            if arg.type == "gh_field":
                dataref = "%data"
                if arg.vector_size > 1:
                    for idx in range(1, arg.vector_size+1):
                        if my_type == "subroutine":
                            text = arg.name + "_" + arg.function_space + \
                                "_v" + str(idx)
                            intent = arg.intent
                            decl = DeclGen(parent, datatype="real",
                                           kind="r_def", dimension=undf_name,
                                           intent=intent, entity_decls=[text])
                            parent.add(decl)
                            if first_arg:
                                first_arg = False
                                first_arg_decl = decl
                        else:
                            text = arg.proxy_name + "(" + str(idx) + ")" + \
                                dataref
                        arglist.append(text)
                else:
                    if my_type == "subroutine":
                        text = arg.name + "_" + arg.function_space
                        intent = arg.intent
                        decl = DeclGen(parent, datatype="real",
                                       kind="r_def", dimension=undf_name,
                                       intent=intent, entity_decls=[text])
                        parent.add(decl)
                        if first_arg:
                            first_arg = False
                            first_arg_decl = decl
                    else:
                        text = arg.proxy_name+dataref
                    arglist.append(text)
            elif arg.type == "gh_operator":
                if my_type == "subroutine":
                    # check whether the operator works on the same space.
                    if arg.descriptor.function_space_from != \
                       arg.descriptor.function_space_to:
                        raise GenerationError(
                            "Stub gen currently assumes that operators work "
                            "on the same function space, therefore the "
                            "declarations will be incorrect. The generator"
                            "needs to know which dimension is 'from' and "
                            "which is 'to' before we can generate the "
                            "correct code.")
                    size = arg.name+"_ncell_3d"
                    arglist.append(size)
                    decl = DeclGen(parent, datatype="integer", intent="in",
                                   entity_decls=[size])
                    parent.add(decl)
                    if first_arg:
                        first_arg = False
                        first_arg_decl = decl
                    text = arg.name
                    arglist.append(text)
                    intent = arg.intent
                    ndf_name = self._fs_descriptors.ndf_name(
                        arg.function_space)
                    parent.add(DeclGen(parent, datatype="real",
                                       kind="r_def",
                                       dimension=ndf_name + "," + ndf_name +
                                       "," + size,
                                       intent=intent, entity_decls=[text]))
                else:
                    arglist.append(arg.proxy_name_indexed+"%ncell_3d")
                    arglist.append(arg.proxy_name_indexed+"%local_stencil")
            else:
                raise GenerationError(
                    "Unexpected arg type found in "
                    "dynamo0p3.py:DynKern:gen_code(). Expected one of"
                    " [gh_field, gh_operator] but found " + arg.type)
        # 3: For each function space (in the order they appear in the
        # metadata arguments)
        for unique_fs in self.arguments.unique_fss:
            # 3.1 Provide compulsory arguments common to operators and
            # fields on a space. There is one: "ndf".
            ndf_name = self._fs_descriptors.ndf_name(unique_fs)
            arglist.append(ndf_name)
            if my_type == "subroutine":
                parent.add(DeclGen(parent, datatype="integer", intent="in",
                           entity_decls=[ndf_name]))
            # 3.1.1 Provide additional compulsory arguments if there
            # is a field on this space
            if self.field_on_space(unique_fs):
                undf_name = self._fs_descriptors.undf_name(unique_fs)
                arglist.append(undf_name)
                map_name = self._fs_descriptors.map_name(unique_fs)
                arglist.append(map_name)
                if my_type == "subroutine":
                    # ndf* declarations need to be before argument
                    # declarations as some compilers don't like
                    # declarations after they have been used. We place
                    # ndf* before the first argument declaration
                    # (field or operator) (rather than after nlayers)
                    # as this keeps the declarations in the order
                    # specified in the metadata and first used by
                    # fields/operators.
                    parent.add(DeclGen(parent, datatype="integer", intent="in",
                                       entity_decls=[undf_name]),
                               position=["before", first_arg_decl.root])
                    parent.add(DeclGen(parent, datatype="integer", intent="in",
                                       dimension=ndf_name,
                                       entity_decls=[map_name]))
            # 3.2 Provide any optional arguments. These arguments are
            # associated with the keyword arguments (basis function,
            # differential basis function and orientation) for a
            # function space.
            if self._fs_descriptors.exists(unique_fs):
                descriptor = self._fs_descriptors.get_descriptor(unique_fs)
                if descriptor.requires_basis:
                    basis_name = descriptor.basis_name
                    arglist.append(basis_name)
                    if my_type == "subroutine":
                        # the size of the first dimension for a
                        # basis array depends on the
                        # function space. The values are
                        # w0=1, w1=3, w2=3, w3=1, wtheta=1, w2h=3, w2v=3
                        first_dim = None
                        if unique_fs.lower() in ["w0", "w3", "wtheta"]:
                            first_dim = "1"
                        elif unique_fs.lower() in ["w1", "w2", "w2h", "w2v"]:
                            first_dim = "3"
                        else:
                            raise GenerationError(
                                "Unsupported space for basis function, "
                                "expecting one of {0} but found "
                                "'{1}'".format(VALID_FUNCTION_SPACES,
                                               unique_fs))
                        parent.add(DeclGen(parent, datatype="real",
                                           kind="r_def", intent="in",
                                           dimension=first_dim + "," +
                                           ndf_name + "," +
                                           self._qr_args["nh"] + "," +
                                           self._qr_args["nv"],
                                           entity_decls=[basis_name]))
                if descriptor.requires_diff_basis:
                    diff_basis_name = descriptor.diff_basis_name
                    arglist.append(diff_basis_name)
                    if my_type == "subroutine":
                        # the size of the first dimension for a
                        # differential basis array depends on the
                        # function space. The values are
                        # w0=3, w1=3, w2=1, w3=1, wtheta=3, w2h=1, w2v=1
                        first_dim = None
                        if unique_fs.lower() in ["w2", "w3", "w2h", "w2v"]:
                            first_dim = "1"
                        elif unique_fs.lower() in ["w0", "w1", "wtheta"]:
                            first_dim = "3"
                        else:
                            raise GenerationError(
                                "Unsupported space for differential basis "
                                "function, expecting one of {0} but found "
                                "'{1}'".format(VALID_FUNCTION_SPACES,
                                               unique_fs))
                        parent.add(DeclGen(parent, datatype="real",
                                           kind="r_def", intent="in",
                                           dimension=first_dim + "," +
                                           ndf_name + "," +
                                           self._qr_args["nh"] + "," +
                                           self._qr_args["nv"],
                                           entity_decls=[diff_basis_name]))
                if descriptor.requires_orientation:
                    orientation_name = descriptor.orientation_name
                    arglist.append(orientation_name)
                    if my_type == "subroutine":
                        parent.add(DeclGen(parent, datatype="integer",
                                           intent="in", dimension=ndf_name,
                                           entity_decls=[orientation_name]))
            # 3.3 Fix for boundary_dofs array to the boundary
            # condition kernel (enforce_bc_kernel) arguments
            if self.name.lower() == "enforce_bc_code" and \
               unique_fs.lower() == "any_space_1":
                arglist.append("boundary_dofs")
                if my_type == "subroutine":
                    ndf_name = self._fs_descriptors.ndf_name("any_space_1")
                    parent.add(DeclGen(parent, datatype="integer", intent="in",
                                       dimension=ndf_name+",2",
                                       entity_decls=["boundary_dofs"]))
                if my_type == "call":
                    parent.add(DeclGen(parent, datatype="integer",
                                       pointer=True, entity_decls=[
                                           "boundary_dofs(:,:) => null()"]))
                    proxy_name = self._arguments.get_field("any_space_1").\
                        proxy_name
                    new_parent, position = parent.start_parent_loop()
                    new_parent.add(AssignGen(new_parent, pointer=True,
                                             lhs="boundary_dofs",
                                             rhs=proxy_name +
                                             "%vspace%get_boundary_dofs()"),
                                   position=["before", position])
        # 4: Provide qr arguments if required
        if self._qr_required:
            arglist.extend([self._qr_args["nh"], self._qr_args["nv"],
                            self._qr_args["h"], self._qr_args["v"]])
            if my_type == "subroutine":
                parent.add(DeclGen(parent, datatype="integer", intent="in",
                                   entity_decls=[self._qr_args["nh"],
                                                 self._qr_args["nv"]]))
                parent.add(DeclGen(parent, datatype="real", kind="r_def",
                                   intent="in", dimension=self._qr_args["nh"],
                                   entity_decls=[self._qr_args["h"]]))
                parent.add(DeclGen(parent, datatype="real", kind="r_def",
                                   intent="in", dimension=self._qr_args["nv"],
                                   entity_decls=[self._qr_args["v"]]))
        return arglist

    @property
    def gen_stub(self):
        ''' output a kernel stub '''
        from f2pygen import ModuleGen, SubroutineGen

        # remove "_code" from the name if it exists to determine the
        # base name which (if dynamo0.3 naming conventions are
        # followed) is used as the root for the module and subroutine
        # names.
        if self.name.lower().endswith("_code"):
            base_name = self.name[:-5]
        else:
            # TODO: add a warning here when logging is added
            base_name = self.name

        # create an empty PSy layer module
        psy_module = ModuleGen(base_name+"_mod")

        # create the subroutine
        sub_stub = SubroutineGen(psy_module, name=base_name+"_code",
                                 implicitnone=True)
        # create the arglist and declarations
        arglist = self._create_arg_list(sub_stub, my_type="subroutine")
        # add the arglist
        sub_stub.args = arglist
        # add the subroutine to the parent module
        psy_module.add(sub_stub)
        return psy_module.root

    @property
    def incremented_field(self, mapping=None):
        ''' Returns the argument corresponding to a field that has
        INC access.  '''
        if mapping is None:
            my_mapping = FIELD_ACCESS_MAP
        else:
            my_mapping = mapping
        return Kern.incremented_field(self, my_mapping)

    @property
    def written_field(self, mapping=None):
        ''' Returns the argument corresponding to a field that has
        WRITE access '''
        if mapping is None:
            my_mapping = FIELD_ACCESS_MAP
        else:
            my_mapping = mapping
        return Kern.written_field(self, my_mapping)

    def gen_code(self, parent):
        ''' Generates dynamo version 0.3 specific psy code for a call to
            the dynamo kernel instance. '''
        from f2pygen import CallGen, DeclGen, AssignGen, UseGen, CommentGen, \
            IfThenGen
        parent.add(DeclGen(parent, datatype="integer",
                           entity_decls=["cell"]))

        # If this kernel is being called from within a coloured
        # loop then we have to look-up the colour map
        if self.is_coloured():

            # Find which argument object has INC access in order to look-up
            # the colour map
            try:
                arg = self.incremented_field
            except FieldNotFoundError:
                # TODO Warn that we're colouring a kernel that has
                # no field object with INC access
                arg = self.written_field

            new_parent, position = parent.start_parent_loop()
            # Add the look-up of the colouring map for this kernel
            # call
            new_parent.add(CommentGen(new_parent, ""),
                           position=["before", position])
            new_parent.add(CommentGen(new_parent, " Look-up colour map"),
                           position=["before", position])
            new_parent.add(CommentGen(new_parent, ""),
                           position=["before", position])
            name = arg.proxy_name_indexed + \
                "%" + arg.ref_name + "%get_colours"
            new_parent.add(CallGen(new_parent,
                                   name=name,
                                   args=["ncolour", "ncp_colour", "cmap"]),
                           position=["before", position])
            new_parent.add(CommentGen(new_parent, ""),
                           position=["before", position])

            # We must pass the colour map to the dofmap/orientation
            # lookup rather than just the cell
            dofmap_args = "cmap(colour, cell)"
        else:
            # This kernel call has not been coloured
            #  - is it OpenMP parallel, i.e. are we a child of
            # an OpenMP directive?
            if self.is_openmp_parallel():
                try:
                    # It is OpenMP parallel - does it have an argument
                    # with INC access?
                    arg = self.incremented_field
                except FieldNotFoundError:
                    arg = None
                if arg:
                    raise GenerationError("Kernel {0} has an argument with "
                                          "INC access and therefore must "
                                          "be coloured in order to be "
                                          "parallelised with OpenMP".
                                          format(self._name))
            dofmap_args = "cell"

        # create a maps_required logical which we can use to add in
        # spacer comments if necessary
        maps_required = False
        for unique_fs in self.arguments.unique_fss:
            if self.field_on_space(unique_fs):
                maps_required = True

        # function-space maps initialisation and their declarations
        if maps_required:
            parent.add(CommentGen(parent, ""))
        for unique_fs in self.arguments.unique_fss:
            if self.field_on_space(unique_fs):
                # A map is required as there is a field on this space
                map_name = self._fs_descriptors.map_name(unique_fs)
                field = self._arguments.get_field(unique_fs)
                parent.add(AssignGen(parent, pointer=True, lhs=map_name,
                                     rhs=field.proxy_name_indexed +
                                     "%" + field.ref_name +
                                     "%get_cell_dofmap("+dofmap_args+")"))
        if maps_required:
            parent.add(CommentGen(parent, ""))
        decl_map_names = []
        for unique_fs in self.arguments.unique_fss:
            if self.field_on_space(unique_fs):
                # A map is required as there is a field on this space
                map_name = self._fs_descriptors.map_name(unique_fs)
                decl_map_names.append(map_name+"(:) => null()")
        if len(decl_map_names) > 0:
            parent.add(DeclGen(parent, datatype="integer", pointer=True,
                               entity_decls=decl_map_names))
        # orientation arrays initialisation and their declarations
        for unique_fs in self.arguments.unique_fss:
            if self._fs_descriptors.exists(unique_fs):
                fs_descriptor = self._fs_descriptors.get_descriptor(unique_fs)
                if fs_descriptor.orientation:
                    field = self._arguments.get_field(unique_fs)
                    parent.add(AssignGen(parent, pointer=True,
                               lhs=fs_descriptor.orientation_name,
                               rhs=field.proxy_name_indexed + "%" +
                                         field.ref_name +
                                         "%get_cell_orientation(" +
                                         dofmap_args + ")"))
        if self._fs_descriptors.orientation:
            orientation_decl_names = []
            for orientation_name in self._fs_descriptors.orientation_names:
                orientation_decl_names.append(orientation_name +
                                              "(:) => null()")
            parent.add(DeclGen(parent, datatype="integer", pointer=True,
                               entity_decls=orientation_decl_names))
            parent.add(CommentGen(parent, ""))

        arglist = self._create_arg_list(parent)

        # generate the kernel call and associated use statement
        parent.add(CallGen(parent, self._name, arglist))
        if not self.module_inline:
            parent.add(UseGen(parent, name=self._module_name,
                              only=True, funcnames=[self._name]))
        # 5: Fix for boundary_dofs array in matrix_vector_mm_code
        if self.name == "matrix_vector_mm_code":
            # In matrix_vector_mm_code, all fields are on the same
            # (unknown) space. Therefore we can use any field to
            # dereference. We choose the 2nd one as that is what is
            # done in the manual implementation.
            reference_arg = self.arguments.args[1]
            enforce_bc_arg = self.arguments.args[0]
            space_name = "w2"
            kern_func_space_name = enforce_bc_arg.function_space
            ndf_name = self.fs_descriptors.ndf_name(kern_func_space_name)
            undf_name = self.fs_descriptors.undf_name(kern_func_space_name)
            map_name = self.fs_descriptors.map_name(kern_func_space_name)
            w2_proxy_name = reference_arg.proxy_name
            self._name_space_manager = NameSpaceFactory().create()
            fs_name = self._name_space_manager.create_name(root_name="fs")
            boundary_dofs_name = self._name_space_manager.create_name(
                root_name="boundary_dofs_"+space_name)
            parent.add(UseGen(parent, name="function_space_mod",
                              only=True, funcnames=[space_name]))
            parent.add(DeclGen(parent, datatype="integer", pointer=True,
                               entity_decls=[boundary_dofs_name +
                                             "(:,:) => null()"]))
            parent.add(DeclGen(parent, datatype="integer",
                               entity_decls=[fs_name]))
            new_parent, position = parent.start_parent_loop()
            new_parent.add(AssignGen(new_parent, lhs=fs_name,
                                     rhs=reference_arg.name +
                                     "%which_function_space()"),
                           position=["before", position])
            if_then = IfThenGen(new_parent, fs_name+" .eq. "+space_name)
            new_parent.add(if_then, position=["before", position])
            if_then.add(AssignGen(if_then, pointer=True,
                                  lhs=boundary_dofs_name,
                                  rhs=w2_proxy_name +
                                  "%vspace%get_boundary_dofs()"))
            parent.add(CommentGen(parent, ""))
            if_then = IfThenGen(parent, fs_name+" .eq. "+space_name)
            parent.add(if_then)
            nlayers_name = self._name_space_manager.create_name(
                root_name="nlayers", context="PSyVars", label="nlayers")
            parent.add(UseGen(parent, name="enforce_bc_kernel_mod", only=True,
                              funcnames=["enforce_bc_code"]))
            if_then.add(CallGen(if_then, "enforce_bc_code",
                                [nlayers_name,
                                 enforce_bc_arg.proxy_name+"%data",
                                 ndf_name, undf_name, map_name,
                                 boundary_dofs_name]))
            parent.add(CommentGen(parent, ""))


class FSDescriptor(object):
    ''' Provides information about a particular function space. '''

    def __init__(self, descriptor):
        self._descriptor = descriptor

    @property
    def requires_basis(self):
        ''' Returns True if a basis function is associated with this
        function space, otherwise it returns False. '''
        if "gh_basis" in self._descriptor.operator_names:
            return True
        else:
            return False

    @property
    def requires_diff_basis(self):
        ''' Returns True if a differential basis function is
        associated with this function space, otherwise it returns
        False. '''
        if "gh_diff_basis" in self._descriptor.operator_names:
            return True
        else:
            return False

    @property
    def requires_orientation(self):
        ''' Returns True if an orientation function is
        associated with this function space, otherwise it returns
        False. '''
        if "gh_orientation" in self._descriptor.operator_names:
            return True
        else:
            return False

    def name(self, operator_name):
        ''' Returns the names of the specified operator for this
        function space. The name is unique to the function space, it
        is not the raw metadata value. '''
        if operator_name == "gh_orientation":
            return self.orientation_name
        elif operator_name == "gh_basis":
            return self.basis_name
        elif operator_name == "gh_diff_basis":
            return self.diff_basis_name
        else:
            raise GenerationError("FSDescriptor:name: unsupported name '{0}'"
                                  " found".format(operator_name))

    @property
    def basis_name(self):
        ''' Returns a name for the basis function on this function
        space. The name is unique to the function space, it is not the
        raw metadata value. '''

        return "basis"+"_"+self._descriptor.function_space_name

    @property
    def diff_basis_name(self):
        ''' Returns a name for the differential basis function on this
        function space. The name is unique to the function space, it
        is not the raw metadata value. '''
        return "diff_basis"+"_"+self._descriptor.function_space_name

    @property
    def fs_name(self):
        ''' Returns the raw metadata value of this function space. '''
        return self._descriptor.function_space_name

    @property
    def orientation_name(self):
        ''' Returns a name for orientation on this function space. The
        name is unique to the function space, it is not the raw
        metadata value. '''
        for operator_name in self._descriptor.operator_names:
            if operator_name == "gh_orientation":
                return "orientation"+"_"+self._descriptor.function_space_name
        raise GenerationError(
            "Internal logic error: FS-Descriptor:orientation_name: This "
            "descriptor has no orientation so can not have a name")

    @property
    def orientation(self):
        ''' Returns True if orientation is associated with this
        function space, otherwise it returns False. '''
        for operator_name in self._descriptor.operator_names:
            if operator_name == "gh_orientation":
                return True
        return False


class FSDescriptors(object):
    ''' Contains a collection of FSDescriptor objects and methods
    that provide information across these objects. '''

    def __init__(self, descriptors):
        self._orig_descriptors = descriptors
        self._descriptors = []
        for descriptor in descriptors:
            self._descriptors.append(FSDescriptor(descriptor))

    def ndf_name(self, func_space):
        ''' Returns a ndf name for this function space. '''
        return "ndf_"+func_space

    def undf_name(self, func_space):
        ''' Returns a undf name for this function space. '''
        return "undf_"+func_space

    def map_name(self, func_space):
        ''' Returns a dofmap name for this function space. '''
        return "map_"+func_space

    @property
    def orientation(self):
        ''' Return True if at least one descriptor specifies
        orientation, otherwise return False. '''
        for descriptor in self._descriptors:
            if descriptor.orientation:
                return True
        return False

    @property
    def orientation_names(self):
        ''' Returns a list of all orientation names used in this
        objects collection of FSDescriptor objects. '''
        names = []
        for descriptor in self._descriptors:
            if descriptor.orientation:
                names.append(descriptor.orientation_name)
        return names

    def exists(self, fs_name):
        ''' Return True if a descriptor with the specified function
        space name exists, otherwise return False. '''
        for descriptor in self._descriptors:
            if descriptor.fs_name == fs_name:
                return True
        return False

    def get_descriptor(self, fs_name):
        ''' Return the descriptor with the specified function space
        name. If it does not exist raise an error.'''
        for descriptor in self._descriptors:
            if descriptor.fs_name == fs_name:
                return descriptor
        raise GenerationError(
            "FSDescriptors:get_descriptor: there is no descriptor for "
            "function space {0}".format(fs_name))


class DynKernelArguments(Arguments):
    ''' Provides information about Dynamo kernel call arguments
    collectively, as specified by the kernel argument metadata. '''

    def __init__(self, call, parent_call):
        if False:  # for pyreverse
            self._0_to_n = DynKernelArgument(None, None, None)
        Arguments.__init__(self, parent_call)
        self._args = []
        for (idx, arg) in enumerate(call.ktype.arg_descriptors):
            self._args.append(DynKernelArgument(arg, call.args[idx],
                                                parent_call))
        self._dofs = []

    def get_field(self, func_space):
        ''' Returns the first field found that is on the specified
        function space. If no field is found an exception is raised. '''
        for arg in self._args:
            if arg.function_space == func_space:
                return arg
        raise FieldNotFoundError("DynKernelArguments:get_field: there is no"
                                 " field with function space {0}".
                                 format(func_space))

    @property
    def has_operator(self):
        ''' Returns true if at least one of the arguments is an operator. '''
        for arg in self._args:
            if arg.type == "gh_operator":
                return True
        return False

    @property
    def unique_fss(self):
        ''' Returns a unique list of function spaces used by the
        arguments. '''
        func_space = []
        for arg in self._args:
            if arg.function_space not in func_space:
                func_space.append(arg.function_space)
        return func_space

    def iteration_space_arg(self, mapping=None):
        ''' Returns the first argument that is written to. This can be
        used to dereference for the iteration space. '''
        if mapping is not None:
            my_mapping = mapping
        else:
            my_mapping = FIELD_ACCESS_MAP
        arg = Arguments.iteration_space_arg(self, my_mapping)
        return arg

    @property
    def dofs(self):
        ''' Currently required for invoke base class although this
        makes no sense for dynamo. Need to refactor the invoke class
        and pull out dofs into the gunghoproto api. '''
        return self._dofs


class DynKernelArgument(Argument):
    ''' Provides information about individual Dynamo kernel call
    arguments as specified by the kernel argument metadata. '''

    def __init__(self, arg, arg_info, call):
        self._arg = arg
        Argument.__init__(self, call, arg_info, arg.access)
        self._vector_size = arg.vector_size
        self._type = arg.type

    @property
    def descriptor(self):
        return self._arg

    @property
    def ref_name(self):
        ''' Returns the name used to dereference this type of argument. '''
        if self._type == "gh_field":
            return "vspace"
        elif self._type == "gh_operator":
            return "fs_from"
        else:
            raise GenerationError(
                "ref_name: Error, unsupported arg type '{0}' found".
                format(self._type))

    @property
    def type(self):
        ''' Returns the type of this argument. '''
        return self._type

    @property
    def vector_size(self):
        ''' Returns the vector size of this argument as specified in
        the Kernel metadata. '''
        return self._vector_size

    @property
    def proxy_name(self):
        ''' Returns the proxy name for this argument. '''
        return self._name+"_proxy"

    @property
    def proxy_declaration_name(self):
        ''' Returns the proxy name for this argument with the array
        dimensions added if required. '''
        if self._vector_size > 1:
            return self.proxy_name+"("+str(self._vector_size)+")"
        else:
            return self.proxy_name

    @property
    def declaration_name(self):
        ''' Returns the name for this argument with the array
        dimensions added if required. '''
        if self._vector_size > 1:
            return self._name+"("+str(self._vector_size)+")"
        else:
            return self._name

    @property
    def proxy_name_indexed(self):
        ''' Returns the proxy name for this argument with an
        additional index which accesses the first element for a vector
        argument. '''
        if self._vector_size > 1:
            return self._name+"_proxy(1)"
        else:
            return self._name+"_proxy"

    @property
    def function_space(self):
        ''' Returns the expected finite element function space for this
            argument as specified by the kernel argument metadata. '''
        return self._arg.function_space

    @property
    def intent(self):
        if self.access == "gh_read":
            return "in"
        elif self.access == "gh_write":
            return "out"
        elif self.access == "gh_inc":
            return "inout"
        else:
            raise GenerationError(
                "Expecting argument access to be one of 'gh_read, gh_write, "
                "gh_inc' but found '{0}'".format(self.access))
