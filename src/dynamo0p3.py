# -----------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2015.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
# -----------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab

''' This module implements the PSyclone Dynamo 0.3 API by 1)
    specialising the required base classes in parser.py (Descriptor,
    KernelType) and adding a new class (DynFuncDescriptor03) to
    capture function descriptor metadata and 2) specialising the
    required base classes in psyGen.py (PSy, Invokes, Invoke, Schedule,
    Loop, Kern, Inf, Arguments and Argument). '''

# imports
import os
from parse import Descriptor, KernelType, ParseError
import expression as expr
import fparser
from psyGen import PSy, Invokes, Invoke, Schedule, Loop, Kern, \
    Arguments, KernelArgument, NameSpaceFactory, GenerationError, \
    FieldNotFoundError, HaloExchange, FORTRAN_INTENT_NAMES
import psyGen
import config

# first section : Parser specialisations and classes

# constants
DISCONTINUOUS_FUNCTION_SPACES = ["w3"]
CONTINUOUS_FUNCTION_SPACES = ["w0", "w1", "w2", "wtheta", "w2h", "w2v"]
VALID_FUNCTION_SPACES = DISCONTINUOUS_FUNCTION_SPACES + \
    CONTINUOUS_FUNCTION_SPACES

VALID_ANY_SPACE_NAMES = ["any_space_1", "any_space_2", "any_space_3",
                         "any_space_4", "any_space_5", "any_space_6",
                         "any_space_7", "any_space_8", "any_space_9"]

VALID_FUNCTION_SPACE_NAMES = VALID_FUNCTION_SPACES + VALID_ANY_SPACE_NAMES

VALID_OPERATOR_NAMES = ["gh_basis", "gh_diff_basis", "gh_orientation"]

VALID_SCALAR_NAMES = ["gh_real", "gh_integer"]
VALID_ARG_TYPE_NAMES = ["gh_field", "gh_operator"] + VALID_SCALAR_NAMES

VALID_REDUCTION_NAMES = ["gh_sum"]
VALID_ACCESS_DESCRIPTOR_NAMES = ["gh_read", "gh_write", "gh_inc"] + \
    VALID_REDUCTION_NAMES

VALID_STENCIL_TYPES = ["x1d", "y1d", "xory1d", "cross", "region"]
# Note, can't use VALID_STENCIL_DIRECTIONS at all locations in this
# file as it causes failures with py.test 2.8.7. Therefore some parts
# of the code do not use the VALID_STENCIL_DIRECTIONS variable.
VALID_STENCIL_DIRECTIONS = ["x_direction", "y_direction"]
# Note, xory1d does not have a direct mapping in STENCIL_MAPPING as it
# indicates either x1d or y1d.
# Note, the LFRic infrastructure currently does not have 'region' as
# an option in stencil_dofmap_mod.F90 so it is not included in
# STENCIL_MAPPING.
STENCIL_MAPPING = {"x1d": "STENCIL_1DX", "y1d": "STENCIL_1DY",
                   "cross": "STENCIL_CROSS"}

VALID_LOOP_BOUNDS_NAMES = ["start", "inner", "edge", "halo", "ncolour",
                           "ncolours", "cells", "dofs"]

# The mapping from meta-data strings to field-access types
# used in this API.
FIELD_ACCESS_MAP = {"write": "gh_write", "read": "gh_read",
                    "readwrite": "gh_rw", "inc": "gh_inc"}

# Valid Dynamo loop types. The default is "" which is over cells (in the
# horizontal plane).
VALID_LOOP_TYPES = ["dofs", "colours", "colour", ""]

# Mappings used by non-API-Specific code in psyGen
psyGen.MAPPING_REDUCTIONS = {"sum": "gh_sum"}
psyGen.MAPPING_SCALARS = {"iscalar": "gh_integer", "rscalar": "gh_real"}
psyGen.MAPPING_ACCESSES = {"inc": "gh_inc", "write": "gh_write",
                           "read": "gh_read"}
psyGen.VALID_ARG_TYPE_NAMES = VALID_ARG_TYPE_NAMES
psyGen.VALID_ACCESS_DESCRIPTOR_NAMES = VALID_ACCESS_DESCRIPTOR_NAMES

# Functions


def get_fs_map_name(function_space):
    ''' Returns a dofmap name for the supplied FunctionSpace. '''
    return "map_" + function_space.mangled_name


def get_fs_ndf_name(function_space):
    ''' Returns a ndf name for this FunctionSpace object. '''
    return "ndf_" + function_space.mangled_name


def get_fs_undf_name(function_space):
    ''' Returns a undf name for this FunctionSpace object. '''
    return "undf_" + function_space.mangled_name


def get_fs_orientation_name(function_space):
    ''' Returns an orientation name for a function space with the
    supplied name '''
    return "orientation" + "_" + function_space.mangled_name


def get_fs_basis_name(function_space):
    ''' Returns a name for the basis function on this FunctionSpace.
    The name is unique to the function space, it is not the
    raw metadata value. '''
    return "basis" + "_" + function_space.mangled_name


def get_fs_diff_basis_name(function_space):
    ''' Returns a name for the differential basis function on the
    supplied FunctionSpace. The name is unique to the function space, it
    is not the raw metadata value. '''
    return "diff_basis" + "_" + function_space.mangled_name


def get_fs_operator_name(operator_name, function_space):
    ''' Returns the name of the specified operator for the supplied
    FunctionSpace. The name is unique to the function space, it
    is not the raw metadata value. '''
    if operator_name == "gh_orientation":
        return get_fs_orientation_name(function_space)
    elif operator_name == "gh_basis":
        return get_fs_basis_name(function_space)
    elif operator_name == "gh_diff_basis":
        return get_fs_diff_basis_name(function_space)
    else:
        raise GenerationError(
            "Unsupported name '{0}' found. Expected one of {1}".
            format(operator_name, VALID_OPERATOR_NAMES))


def mangle_fs_name(args, fs_name):
    ''' Construct the mangled version of a function-space name given
    a list of kernel arguments '''
    if fs_name not in VALID_ANY_SPACE_NAMES:
        # If the supplied function-space name is not any any-space then
        # we don't need to mangle the name
        return fs_name
    for arg in args:
        for fspace in arg.function_spaces:
            if fspace and fspace.orig_name.lower() == fs_name.lower():
                return fs_name.lower() + "_" + arg.name
    raise FieldNotFoundError("No kernel argument found for function space "
                             "'{0}'".format(fs_name))


def field_on_space(function_space, arguments):
    ''' Returns True if the supplied list of arguments contains a field
    that exists on the specified space. '''
    if function_space.mangled_name in arguments.unique_fs_names:
        for arg in arguments.args:
            # First, test that arg is a field as some argument objects won't
            # have function spaces, e.g. scalars
            if arg.type == "gh_field" and \
               arg.function_space.orig_name == function_space.orig_name:
                return True
    return False

# Classes


class FunctionSpace(object):
    ''' Manages the name of a function space. If it is an any-space
    then its name is mangled such that it is unique within the scope
    of an Invoke '''

    def __init__(self, name, kernel_args):
        self._orig_name = name
        self._kernel_args = kernel_args
        if self._orig_name not in VALID_ANY_SPACE_NAMES:
            # We only need to name-mangle any-space spaces
            self._mangled_name = self._orig_name
        else:
            # We do not construct the name-mangled name at this point
            # as the full list of kernel arguments may still be under
            # construction.
            self._mangled_name = None

    @property
    def orig_name(self):
        ''' Returns the name of this function space as declared in the
        kernel meta-data '''
        return self._orig_name

    @property
    def mangled_name(self):
        ''' Returns the mangled name of this function space such that
        it is unique within the scope of an invoke. If the mangled
        name has not been generated then we do that the first time we're
        called. '''
        if self._mangled_name:
            return self._mangled_name
        else:
            # Cannot use kernel_args.field_on_space(x) here because that
            # routine itself requires the mangled name in order to identify
            # whether the space is present in the kernel call.
            self._mangled_name = mangle_fs_name(self._kernel_args.args,
                                                self._orig_name)
            return self._mangled_name


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
                "In the dynamo0.3 API each meta_arg entry must be of type "
                "'arg_type', but found '{0}'".format(arg_type.name))
        # we require at least 2 args
        if len(arg_type.args) < 2:
            raise ParseError(
                "In the dynamo0.3 API each meta_arg entry must have at least "
                "2 args, but found '{0}'".format(len(arg_type.args)))
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
            if self._type in VALID_SCALAR_NAMES and self._vector_size > 1:
                raise ParseError(
                    "In the dynamo0.3 API vector notation is not supported "
                    "for scalar arguments (found '{0}')".
                    format(arg_type.args[0]))
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
                    "In the dynamo0.3 API the 1st argument of a "
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
        # Reduction access descriptors are only valid for scalar arguments
        if self._type not in VALID_SCALAR_NAMES and \
           self._access_descriptor.name in VALID_REDUCTION_NAMES:
            raise ParseError(
                "In the dynamo0.3 API a reduction access '{0}' is only valid "
                "with a scalar argument, but '{1}' was found".
                format(self._access_descriptor.name, self._type))
        # Scalars can only be read_only or reductions
        if self._type in VALID_SCALAR_NAMES:
            if self._access_descriptor.name not in ["gh_read"] + \
               VALID_REDUCTION_NAMES:
                raise ParseError(
                    "In the dynamo0.3 API scalar arguments must be "
                    "read-only (gh_read) or a reduction ({0}) but found "
                    "'{1}' in '{2}'".format(VALID_REDUCTION_NAMES,
                                            self._access_descriptor.name,
                                            arg_type))
        # Scalars with reductions do not work with Distributed Memory
        if self._type in VALID_SCALAR_NAMES and \
           self._access_descriptor.name in VALID_REDUCTION_NAMES and \
           config.DISTRIBUTED_MEMORY:
            raise ParseError(
                "Scalar reductions are not yet supported with distributed "
                "memory.")
        stencil = None
        if self._type == "gh_field":
            if len(arg_type.args) < 3:
                raise ParseError(
                    "In the dynamo0.3 API each meta_arg entry must have at "
                    "least 3 arguments if its first argument is gh_field, but "
                    "found {0} in '{1}'".format(len(arg_type.args), arg_type)
                    )
            # There must be at most 4 arguments.
            if len(arg_type.args) > 4:
                raise ParseError(
                    "In the dynamo0.3 API each meta_arg entry must have at "
                    "most 4 arguments if its first argument is gh_field, but "
                    "found {0} in '{1}'".format(len(arg_type.args), arg_type))
            # The 3rd argument must be a function space name
            if arg_type.args[2].name not in VALID_FUNCTION_SPACE_NAMES:
                raise ParseError(
                    "In the dynamo0.3 API the 3rd argument of a meta_arg "
                    "entry must be a valid function space name if its first "
                    "argument is gh_field (one of {0}), but found '{1}' in "
                    "'{2}".format(VALID_FUNCTION_SPACE_NAMES,
                                  arg_type.args[2].name, arg_type))
            self._function_space1 = arg_type.args[2].name

            # The optional 4th argument is a stencil specification
            if len(arg_type.args) == 4:
                try:
                    stencil = self._get_stencil(arg_type.args[3],
                                                VALID_STENCIL_TYPES)
                except ParseError as err:
                    raise ParseError(
                        "In the dynamo0.3 API the 4th argument of a meta_arg "
                        "entry must be a valid stencil specification but "
                        "entry '{0}' raised the following error:".
                        format(arg_type) + str(err))

            if self._function_space1.lower() == "w3" and \
               self._access_descriptor.name.lower() == "gh_inc":
                raise ParseError(
                    "it does not make sense for a 'w3' space to have a "
                    "'gh_inc' access")
            if stencil and self._access_descriptor.name.lower() != \
                    "gh_read":
                raise ParseError("a stencil must be read only so its access"
                                 "should be gh_read")

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
        elif self._type in VALID_SCALAR_NAMES:
            if len(arg_type.args) != 2:
                raise ParseError(
                    "In the dynamo0.3 API each meta_arg entry must have 2 "
                    "arguments if its first argument is gh_{{r,i}}scalar, but "
                    "found {0} in '{1}'".format(len(arg_type.args), arg_type))
            # Scalars don't have a function space
            self._function_space1 = None
        else:  # we should never get to here
            raise ParseError(
                "Internal error in DynArgDescriptor03.__init__, (2) should "
                "not get to here")
        Descriptor.__init__(self, self._access_descriptor.name,
                            self._function_space1, stencil=stencil)

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
        elif self._type in VALID_SCALAR_NAMES:
            return None
        else:
            raise RuntimeError(
                "Internal error, DynArgDescriptor03:function_space(), should "
                "not get to here.")

    @property
    def function_spaces(self):
        ''' Return the function space names that this instance operates
        on as a list. In the case of a gh_operator, where there are 2 function
        spaces, we return both. '''
        if self._type == "gh_field":
            return [self.function_space]
        elif self._type == "gh_operator":
            # return to before from to maintain expected ordering
            return [self.function_space_to, self.function_space_from]
        elif self._type in VALID_SCALAR_NAMES:
            return []
        else:
            raise RuntimeError(
                "Internal error, DynArgDescriptor03:function_spaces(), should "
                "not get to here.")

    @property
    def is_any_space(self):
        ''' Returns True if this descriptor is of type any_space. This
        could be any on the any_space spaces, i.e. any of any_space_1,
        any_space_2, ... any_space_9, otherwise returns False. For
        operators, returns True if the source descriptor is of type
        any_space, else returns False. '''
        return self.function_space in VALID_ANY_SPACE_NAMES

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
        elif self._type in VALID_SCALAR_NAMES:
            pass  # we have nothing to add if we're a scalar
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
            arg_fs_names.extend(descriptor.function_spaces)
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
                    "meta_funcs must be unique, but '{0}' is replicated."
                    .format(fs_name))
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
        self._name_space_manager = NameSpaceFactory().create()
        if False:
            self._0_to_n = DynInvoke(None, None)  # for pyreverse
        Invokes.__init__(self, alg_calls, DynInvoke)


def stencil_extent_value(field):
    '''Returns the content of the stencil extent. This may be a literal
    value (a number) or a variable name. This function simplifies this
    problem by returning a string in either case'''
    if field.stencil.extent_arg.is_literal():
        extent = field.stencil.extent_arg.text
    else:
        extent = field.stencil.extent_arg.varName
    return extent


def stencil_unique_str(arg, context):
    '''Returns a string that uniquely identifies a stencil. As a stencil
    differs due to the function space it operates on, type of
    stencil and extent of stencil, we concatenate these things together
    to return a unique string '''
    unique = context
    unique += arg.function_space.mangled_name
    unique += arg.descriptor.stencil['type']
    if arg.descriptor.stencil['extent']:
        raise GenerationError(
            "found a stencil with an extent specified in the metadata. This "
            "is not coded for.")
    unique += arg.stencil.extent_arg.text.lower()
    if arg.descriptor.stencil['type'] == 'xory1d':
        unique += arg.stencil.direction_arg.text.lower()
    return unique


def stencil_map_name(arg):
    ''' returns a valid unique map name for a stencil in the PSy layer '''
    root_name = arg.name + "_stencil_map"
    unique = stencil_unique_str(arg, "map")
    name_space_manager = NameSpaceFactory().create()
    return name_space_manager.create_name(
        root_name=root_name, context="PSyVars", label=unique)


def stencil_dofmap_name(arg):
    ''' returns a valid unique dofmap name for a stencil in the PSy layer '''
    root_name = arg.name + "_stencil_dofmap"
    unique = stencil_unique_str(arg, "dofmap")
    name_space_manager = NameSpaceFactory().create()
    return name_space_manager.create_name(
        root_name=root_name, context="PSyVars", label=unique)


class DynInvokeStencil(object):
    ''' stencil information associated with a DynInvoke call '''

    def __init__(self, schedule):

        self._name_space_manager = NameSpaceFactory().create()
        # list of arguments which have an extent value passed to this
        # invoke routine from the algorithm layer
        self._unique_extent_args = []
        extent_names = []
        for call in schedule.calls():
            for arg in call.arguments.args:
                if arg.stencil:
                    if not arg.stencil.extent:
                        if not arg.stencil.extent_arg.is_literal():
                            if arg.stencil.extent_arg.text not in extent_names:
                                extent_names.append(
                                    arg.stencil.extent_arg.text)
                                self._unique_extent_args.append(arg)

        # list of lists, one for each call, within which has all args
        # which require a stencil access
        self._unique_extent_kern_args = []
        for idx, call in enumerate(schedule.calls()):
            for arg in call.arguments.args:
                if arg.stencil:
                    if not arg.stencil.extent:
                        self._unique_extent_kern_args.append(arg)

        self._unique_direction_args = []
        direction_names = []
        for call in schedule.calls():
            for idx, arg in enumerate(call.arguments.args):
                if arg.stencil and arg.stencil.direction_arg:
                    if arg.stencil.direction_arg.is_literal():
                        raise GenerationError(
                            "Kernel {0}, metadata arg {1}, a literal is not "
                            "a valid value for a stencil direction".
                            format(call.name, str(idx)))
                    if arg.stencil.direction_arg.text.lower() not in \
                       ["x_direction", "y_direction"]:
                        if arg.stencil.direction_arg.text not in \
                           direction_names:
                            direction_names.append(
                                arg.stencil.direction_arg.text)
                            self._unique_direction_args.append(arg)

    @property
    def _unique_extent_vars(self):
        names = []
        for arg in self._unique_extent_args:
            names.append(arg.stencil.extent_arg.varName)
        return names

    def _declare_unique_extent_vars(self, parent):
        from f2pygen import DeclGen
        if self._unique_extent_vars:
            parent.add(DeclGen(parent, datatype="integer",
                               entity_decls=self._unique_extent_vars,
                               intent="in"))

    @property
    def _unique_direction_vars(self):
        names = []
        for arg in self._unique_direction_args:
            names.append(arg.stencil.direction_arg.varName)
        return names

    def _declare_unique_direction_vars(self, parent):
        from f2pygen import DeclGen
        if self._unique_direction_vars:
            parent.add(DeclGen(parent, datatype="integer",
                               entity_decls=self._unique_direction_vars,
                               intent="in"))

    @property
    def unique_alg_vars(self):
        '''returns a list of the names of the extent and direction arguments
        specified in the algorithm layer'''
        return self._unique_extent_vars + self._unique_direction_vars

    def declare_unique_alg_vars(self, parent):
        '''declares all extent and direction arguments passed into the PSy
        layer'''
        self._declare_unique_extent_vars(parent)
        self._declare_unique_direction_vars(parent)

    def initialise_stencil_maps(self, parent):
        '''adds in the required stencil dofmap code to the PSy layer'''
        from f2pygen import AssignGen, IfThenGen, TypeDeclGen, UseGen, \
            CommentGen, DeclGen
        if self._unique_extent_kern_args:
            parent.add(CommentGen(parent, ""))
            parent.add(CommentGen(parent, " Initialise stencil dofmaps"))
            parent.add(CommentGen(parent, ""))
            parent.add(UseGen(parent, name="stencil_dofmap_mod", only=True,
                              funcnames=["stencil_dofmap_type"]))
            stencil_map_names = []
            for arg in self._unique_extent_kern_args:
                map_name = stencil_map_name(arg)
                if map_name not in stencil_map_names:
                    # only initialise maps once
                    stencil_map_names.append(map_name)
                    parent.add(
                        TypeDeclGen(parent, pointer=True,
                                    datatype="stencil_dofmap_type",
                                    entity_decls=[map_name+" => null()"]))
                    stencil_type = arg.descriptor.stencil['type']
                    if stencil_type == "xory1d":
                        parent.add(UseGen(parent, name="flux_direction_mod",
                                          only=True,
                                          funcnames=
                                          ["x_direction", "y_direction"]))
                        parent.add(UseGen(parent, name="stencil_dofmap_mod",
                                          only=True,
                                          funcnames=
                                          ["STENCIL_1DX", "STENCIL_1DY"]))
                        direction_name = arg.stencil.direction_arg.varName
                        for direction in ["x", "y"]:
                            if_then = IfThenGen(parent, direction_name +
                                                " .eq. " + direction +
                                                "_direction")
                            if_then.add(
                                AssignGen(if_then, pointer=True,
                                          lhs=map_name, rhs=arg.proxy_name +
                                          "%vspace%get_stencil_dofmap("
                                          "STENCIL_1D" + direction.upper() +
                                          ","+stencil_extent_value(arg)+")"))
                            parent.add(if_then)
                    else:
                        try:
                            stencil_name = STENCIL_MAPPING[stencil_type]
                        except KeyError:
                            raise GenerationError(
                                "Unsupported stencil type '{0}' supplied. "
                                "Supported mappings are {1}".
                                format(arg.descriptor.stencil['type'],
                                       str(STENCIL_MAPPING)))
                        parent.add(UseGen(parent, name="stencil_dofmap_mod",
                                          only=True,
                                          funcnames=[stencil_name]))
                        parent.add(
                            AssignGen(parent, pointer=True, lhs=map_name,
                                      rhs=arg.proxy_name +
                                      "%vspace%get_stencil_dofmap(" +
                                      stencil_name + "," +
                                      stencil_extent_value(arg) + ")"))
                    parent.add(DeclGen(parent, datatype="integer",
                                       pointer=True,
                                       entity_decls=[stencil_dofmap_name(arg) +
                                                     "(:,:,:) => null()"]))
                    parent.add(AssignGen(parent, pointer=True,
                                         lhs=stencil_dofmap_name(arg),
                                         rhs=map_name + "%get_dofmap()"))


class DynInvoke(Invoke):
    ''' The Dynamo specific invoke class. This passes the Dynamo
    specific schedule class to the base class so it creates the one we
    require.  Also overrides the gen_code method so that we generate
    dynamo specific invocation code. '''

    def __init__(self, alg_invocation, idx):
        if False:
            self._schedule = DynSchedule(None)  # for pyreverse
        reserved_names_list = []
        reserved_names_list.extend(STENCIL_MAPPING.values())
        reserved_names_list.extend(VALID_STENCIL_DIRECTIONS)
        Invoke.__init__(self, alg_invocation, idx, DynSchedule,
                        reserved_names=reserved_names_list)

        # The baseclass works out the algorithm code's unique argument
        # list and stores it in the self._alg_unique_args
        # list. However, the base class currently ignores any stencil and qr
        # arguments so we need to add them in.

        # initialise our invoke stencil information
        self.stencil = DynInvokeStencil(self.schedule)

        # extend arg list
        self._alg_unique_args.extend(self.stencil.unique_alg_vars)

        # adding in qr arguments
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

        # lastly, add in halo exchange calls if required. We only need to
        # do this for fields since operators are assembled in place
        # and scalars don't have halos.
        if config.DISTRIBUTED_MEMORY:
            # for the moment just add them before each loop as required
            for loop in self.schedule.loops():
                inc = loop.has_inc_arg()
                for halo_field in loop.unique_fields_with_halo_reads():
                    if halo_field.vector_size > 1:
                        # the range function below returns values from
                        # 1 to the vector size which is what we
                        # require in our Fortran code
                        for idx in range(1, halo_field.vector_size+1):
                            exchange = DynHaloExchange(
                                halo_field, parent=loop, vector_index=idx,
                                inc=inc)
                            loop.parent.children.insert(loop.position,
                                                        exchange)
                    else:
                        exchange = DynHaloExchange(halo_field, parent=loop,
                                                   inc=inc)
                        loop.parent.children.insert(loop.position, exchange)

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

    def unique_proxy_declarations(self, datatype, access=None):
        ''' Returns a list of all required proxy declarations for the
        specified datatype.  If access is supplied (e.g. "gh_write")
        then only declarations with that access are returned. '''
        if datatype not in VALID_ARG_TYPE_NAMES:
            raise GenerationError(
                "unique_proxy_declarations called with an invalid datatype. "
                "Expected one of '{0}' but found '{1}'".
                format(str(VALID_ARG_TYPE_NAMES), datatype))
        if access and access not in VALID_ACCESS_DESCRIPTOR_NAMES:
            raise GenerationError(
                "unique_proxy_declarations called with an invalid access "
                "type. Expected one of '{0}' but got '{1}'".
                format(VALID_ACCESS_DESCRIPTOR_NAMES, access))
        declarations = []
        for call in self.schedule.calls():
            for arg in call.arguments.args:
                if not access or arg.access == access:
                    if arg.text and arg.type == datatype:
                        if arg.proxy_declaration_name not in declarations:
                            declarations.append(arg.proxy_declaration_name)
        return declarations

    def arg_for_funcspace(self, fspace):
        ''' Returns an argument object which is on the requested
        function space. Searches through all Kernel calls in this
        invoke. Currently the first argument object that is found is
        used. Throws an exception if no argument exists. '''
        for kern_call in self.schedule.calls():
            try:
                return kern_call.arguments.get_arg_on_space(fspace)
            except FieldNotFoundError:
                pass
        raise GenerationError(
            "No argument found on '{0}' space".format(fspace.mangled_name))

    def unique_fss(self):
        ''' Returns the unique function space *objects* over all kernel
        calls in this invoke. '''
        unique_fs = []
        unique_fs_names = []
        for kern_call in self.schedule.calls():
            kern_fss = kern_call.arguments.unique_fss
            for fspace in kern_fss:
                if fspace.mangled_name not in unique_fs_names:
                    unique_fs.append(fspace)
                    unique_fs_names.append(fspace.mangled_name)
        return unique_fs

    def basis_required(self, func_space):
        ''' Returns true if at least one of the kernels in this invoke
        requires a basis function for this function space, otherwise
        it returns False. '''
        # look in each kernel
        for kern_call in self.schedule.kern_calls():
            # is there a descriptor for this function space?
            if kern_call.fs_descriptors.exists(func_space):
                descriptor = kern_call.fs_descriptors.\
                    get_descriptor(func_space)
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
                descriptor = kern_call.fs_descriptors.\
                    get_descriptor(func_space)
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

    def get_fs_operator_name(self, operator_name, function_space):
        ''' A convenience method that returns an operator name for a
        particular operator on a particular function space. These
        names are specified in function_space_descriptors objects
        contained within Kernel objects. The first Kernel which uses
        the specified function space is used to return the name. If no
        Kernel using this function space exists in this invoke, an
        error is thrown. '''
        for kern_call in self.schedule.kern_calls():
            if kern_call.fs_descriptors.exists(function_space):
                return get_fs_operator_name(operator_name, function_space)
        raise GenerationError(
            "Dyn_invoke:get_fs_operator_name: no kern call with function "
            "space '{0}' and operator '{1}'".format(function_space,
                                                    operator_name))

    def field_on_space(self, func_space):
        ''' Returns true if a field exists on this space for any
        kernel in this invoke. '''
        for kern_call in self.schedule.calls():
            if field_on_space(func_space, kern_call.arguments):
                return True
        return False

    def gen_code(self, parent):
        ''' Generates Dynamo specific invocation code (the subroutine
        called by the associated invoke call in the algorithm
        layer). This consists of the PSy invocation subroutine and the
        declaration of its arguments. '''
        from f2pygen import SubroutineGen, TypeDeclGen, AssignGen, DeclGen, \
            AllocateGen, DeallocateGen, CallGen, CommentGen
        # Create a namespace manager so we can avoid name clashes
        self._name_space_manager = NameSpaceFactory().create()
        # Create the subroutine
        invoke_sub = SubroutineGen(parent, name=self.name,
                                   args=self.psy_unique_var_names +
                                   self.stencil.unique_alg_vars +
                                   self._psy_unique_qr_vars)
        # Add the subroutine argument declarations for real scalars that
        # are read - we don't currently support any other access type
        r_declarations = self.unique_declarations("gh_real",
                                                  access="gh_read")
        if r_declarations:
            invoke_sub.add(DeclGen(invoke_sub, datatype="real",
                                   kind="r_def", entity_decls=r_declarations,
                                   intent="in"))
        # Add the subroutine argument declarations for integer scalars
        # that are read - we don't currently support any other access type
        i_declarations = self.unique_declarations("gh_integer",
                                                  access="gh_read")
        if i_declarations:
            invoke_sub.add(DeclGen(invoke_sub, datatype="integer",
                                   entity_decls=i_declarations,
                                   intent="in"))

        # declare any stencil arguments
        self.stencil.declare_unique_alg_vars(invoke_sub)

        fld_args = self.unique_declns_by_intent("gh_field")
        # Add the subroutine argument declarations for fields
        for intent in FORTRAN_INTENT_NAMES:
            if fld_args[intent]:
                if intent == "out":
                    # The data part of a field might have intent(out) but
                    # in order to preserve the state of the whole derived-type
                    # object it must be declared as inout.
                    fort_intent = "inout"
                else:
                    fort_intent = intent
                invoke_sub.add(TypeDeclGen(invoke_sub, datatype="field_type",
                                           entity_decls=fld_args[intent],
                                           intent=fort_intent))

        # Add the subroutine argument declarations for operators that
        # are read or written (operators are always on discontinous spaces
        # and therefore are never 'inc')
        op_declarations_dict = self.unique_declns_by_intent("gh_operator")
        for intent in FORTRAN_INTENT_NAMES:
            if op_declarations_dict[intent]:
                if intent == "out":
                    # The data part of an operator might have intent(out) but
                    # in order to preserve the state of the whole derived-type
                    # object it must be declared as inout.
                    fort_intent = "inout"
                else:
                    fort_intent = intent
                invoke_sub.add(
                    TypeDeclGen(invoke_sub, datatype="operator_type",
                                entity_decls=op_declarations_dict[intent],
                                intent=fort_intent))

        # Add the subroutine argument declarations for qr (quadrature
        # rules)
        if len(self._psy_unique_qr_vars) > 0:
            invoke_sub.add(TypeDeclGen(invoke_sub, datatype="quadrature_type",
                                       entity_decls=self._psy_unique_qr_vars,
                                       intent="in"))

        # Zero any scalar arguments that are GH_SUM
        zero_args = self.unique_declarations("gh_real", access="gh_sum")
        if zero_args:
            invoke_sub.add(CommentGen(invoke_sub, ""))
            invoke_sub.add(CommentGen(invoke_sub, " Zero summation variables"))
            invoke_sub.add(CommentGen(invoke_sub, ""))
            for arg in zero_args:
                invoke_sub.add(AssignGen(invoke_sub,
                                         lhs=arg, rhs="0.0_r_def"))

        # declare and initialise proxies for each of the (non-scalar)
        # arguments
        invoke_sub.add(CommentGen(invoke_sub, ""))
        invoke_sub.add(CommentGen(invoke_sub, " Initialise field proxies"))
        invoke_sub.add(CommentGen(invoke_sub, ""))
        for arg in self.psy_unique_vars:
            # We don't have proxies for scalars
            if arg.type in VALID_SCALAR_NAMES:
                continue
            if arg.vector_size > 1:
                # the range function below returns values from
                # 1 to the vector size which is what we
                # require in our Fortran code
                for idx in range(1, arg.vector_size+1):
                    invoke_sub.add(
                        AssignGen(invoke_sub,
                                  lhs=arg.proxy_name+"("+str(idx)+")",
                                  rhs=arg.name+"("+str(idx)+")%get_proxy()"))
            else:
                invoke_sub.add(AssignGen(invoke_sub, lhs=arg.proxy_name,
                                         rhs=arg.name+"%get_proxy()"))

        field_proxy_decs = self.unique_proxy_declarations("gh_field")
        if len(field_proxy_decs) > 0:
            invoke_sub.add(
                TypeDeclGen(invoke_sub,
                            datatype="field_proxy_type",
                            entity_decls=field_proxy_decs))
        op_proxy_decs = self.unique_proxy_declarations("gh_operator")
        if len(op_proxy_decs) > 0:
            invoke_sub.add(
                TypeDeclGen(invoke_sub,
                            datatype="operator_proxy_type",
                            entity_decls=op_proxy_decs))
        # Initialise the number of layers
        invoke_sub.add(CommentGen(invoke_sub, ""))
        invoke_sub.add(CommentGen(invoke_sub, " Initialise number of layers"))
        invoke_sub.add(CommentGen(invoke_sub, ""))

        # Use the first argument that is not a scalar
        first_var = None
        for var in self.psy_unique_vars:
            if var.type in ["gh_field", "gh_operator"]:
                first_var = var
                break
        if not first_var:
            raise GenerationError(
                "Cannot create an Invoke with no field/operator arguments")

        # Use our namespace manager to create a unique name unless
        # the context and label match and in this case return the
        # previous name
        nlayers_name = self._name_space_manager.create_name(
            root_name="nlayers", context="PSyVars", label="nlayers")
        invoke_sub.add(
            AssignGen(invoke_sub, lhs=nlayers_name,
                      rhs=first_var.proxy_name_indexed + "%" +
                      first_var.ref_name() + "%get_nlayers()"))
        invoke_sub.add(DeclGen(invoke_sub, datatype="integer",
                               entity_decls=[nlayers_name]))

        # declare and initialise a mesh object if required
        if config.DISTRIBUTED_MEMORY:
            from f2pygen import UseGen
            # we will need a mesh object for any loop bounds
            mesh_obj_name = self._name_space_manager.create_name(
                root_name="mesh", context="PSyVars", label="mesh")
            invoke_sub.add(UseGen(invoke_sub, name="mesh_mod", only=True,
                                  funcnames=["mesh_type"]))
            invoke_sub.add(TypeDeclGen(invoke_sub, datatype="mesh_type",
                                       entity_decls=[mesh_obj_name]))
            rhs = first_var.name_indexed + "%get_mesh()"
            invoke_sub.add(CommentGen(invoke_sub, ""))
            invoke_sub.add(CommentGen(invoke_sub, " Create a mesh object"))
            invoke_sub.add(CommentGen(invoke_sub, ""))
            invoke_sub.add(AssignGen(invoke_sub, lhs=mesh_obj_name, rhs=rhs))

        # declare and initialise stencil maps
        self.stencil.initialise_stencil_maps(invoke_sub)

        if self.qr_required:
            # declare and initialise qr values
            invoke_sub.add(CommentGen(invoke_sub, ""))
            invoke_sub.add(CommentGen(invoke_sub, " Initialise qr values"))
            invoke_sub.add(CommentGen(invoke_sub, ""))
            invoke_sub.add(
                DeclGen(invoke_sub, datatype="integer",
                        entity_decls=["nqp_h", "nqp_v"]))
            invoke_sub.add(
                DeclGen(invoke_sub, datatype="real", pointer=True,
                        kind="r_def", entity_decls=["xp(:,:) => null()"]))
            decl_list = ["zp(:) => null()", "wh(:) => null()",
                         "wv(:) => null()"]
            invoke_sub.add(
                DeclGen(invoke_sub, datatype="real", pointer=True,
                        kind="r_def", entity_decls=decl_list))
            if len(self._psy_unique_qr_vars) > 1:
                raise GenerationError(
                    "Oops, not yet coded for multiple qr values")
            qr_var_name = self._psy_unique_qr_vars[0]
            qr_ptr_vars = {"zp": "xqp_v", "xp": "xqp_h", "wh": "wqp_h",
                           "wv": "wqp_v"}
            qr_vars = ["nqp_h", "nqp_v"]
            for qr_var in qr_ptr_vars.keys():
                invoke_sub.add(
                    AssignGen(invoke_sub, pointer=True, lhs=qr_var,
                              rhs=qr_var_name + "%get_" +
                              qr_ptr_vars[qr_var] + "()"))
            for qr_var in qr_vars:
                invoke_sub.add(
                    AssignGen(invoke_sub, lhs=qr_var,
                              rhs=qr_var_name + "%get_" + qr_var + "()"))
        operator_declarations = []
        var_list = []
        var_dim_list = []
        # loop over all unique function spaces used by the kernels in this
        # invoke
        for function_space in self.unique_fss():
            # Initialise information associated with this function space
            invoke_sub.add(CommentGen(invoke_sub, ""))
            invoke_sub.add(
                CommentGen(invoke_sub, " Initialise sizes and "
                           "allocate any basis arrays for " +
                           function_space.mangled_name))
            invoke_sub.add(CommentGen(invoke_sub, ""))
            # Find an argument on this space to use to dereference
            arg = self.arg_for_funcspace(function_space)
            name = arg.proxy_name_indexed
            # initialise ndf for this function space and add name to
            # list to declare later
            ndf_name = get_fs_ndf_name(function_space)
            var_list.append(ndf_name)
            invoke_sub.add(AssignGen(invoke_sub, lhs=ndf_name,
                                     rhs=name +
                                     "%" + arg.ref_name(function_space) +
                                     "%get_ndf()"))
            # if there is a field on this space then initialise undf
            # for this function space and add name to list to declare
            # later
            if self.field_on_space(function_space):
                undf_name = get_fs_undf_name(function_space)
                var_list.append(undf_name)
                invoke_sub.add(AssignGen(invoke_sub, lhs=undf_name,
                                         rhs=name + "%" +
                                         arg.ref_name(function_space) +
                                         "%get_undf()"))
            if self.basis_required(function_space):
                # initialise 'dim' variable for this function space
                # and add name to list to declare later
                lhs = "dim_"+function_space.mangled_name
                var_dim_list.append(lhs)
                rhs = name+"%"+arg.ref_name(function_space)+"%get_dim_space()"
                invoke_sub.add(AssignGen(invoke_sub, lhs=lhs, rhs=rhs))
                # allocate the basis function variable
                alloc_args = "dim_" + function_space.mangled_name + ", " + \
                             get_fs_ndf_name(function_space) + ", nqp_h, nqp_v"
                op_name = self.get_fs_operator_name("gh_basis", function_space)
                invoke_sub.add(AllocateGen(invoke_sub,
                                           op_name+"("+alloc_args+")"))
                # add basis function variable to list to declare later
                operator_declarations.append(op_name+"(:,:,:,:)")
            if self.diff_basis_required(function_space):
                # initialise 'diff_dim' variable for this function
                # space and add name to list to declare later
                lhs = "diff_dim_" + function_space.mangled_name
                var_dim_list.append(lhs)
                rhs = name+"%" + arg.ref_name(function_space) + \
                    "%get_dim_space_diff()"
                invoke_sub.add(AssignGen(invoke_sub, lhs=lhs, rhs=rhs))
                # allocate the diff basis function variable
                alloc_args = ("diff_dim_" + function_space.mangled_name +
                              ", " + get_fs_ndf_name(function_space) +
                              ", nqp_h, nqp_v")
                op_name = self.get_fs_operator_name("gh_diff_basis",
                                                    function_space)
                invoke_sub.add(AllocateGen(invoke_sub,
                                           op_name+"("+alloc_args+")"))
                # add diff basis function variable to list to declare later
                operator_declarations.append(op_name+"(:,:,:,:)")
        if var_list:
            # declare ndf and undf for all function spaces
            invoke_sub.add(DeclGen(invoke_sub, datatype="integer",
                                   entity_decls=var_list))
        if var_dim_list:
            # declare dim and diff_dim for all function spaces
            invoke_sub.add(DeclGen(invoke_sub, datatype="integer",
                                   entity_decls=var_dim_list))
        if operator_declarations:
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
                    op_name = self.get_fs_operator_name("gh_basis",
                                                        function_space)
                    args.append(op_name)
                    args.append(get_fs_ndf_name(function_space))
                    args.extend(["nqp_h", "nqp_v", "xp", "zp"])
                    # find an appropriate field to access
                    arg = self.arg_for_funcspace(function_space)
                    name = arg.proxy_name_indexed
                    # insert the basis array call
                    invoke_sub.add(CallGen(invoke_sub,
                                           name=name + "%" +
                                           arg.ref_name(function_space) +
                                           "%compute_basis_function",
                                           args=args))
                if self.diff_basis_required(function_space):
                    # Create the argument list
                    args = []
                    op_name = self.get_fs_operator_name("gh_diff_basis",
                                                        function_space)
                    args.append(op_name)
                    args.append(get_fs_ndf_name(function_space))
                    args.extend(["nqp_h", "nqp_v", "xp", "zp"])
                    # find an appropriate field to access
                    arg = self.arg_for_funcspace(function_space)
                    name = arg.proxy_name_indexed
                    # insert the diff basis array call
                    invoke_sub.add(
                        CallGen(invoke_sub, name=name + "%" +
                                arg.ref_name(function_space) +
                                "%compute_diff_basis_function", args=args))
        invoke_sub.add(CommentGen(invoke_sub, ""))
        if config.DISTRIBUTED_MEMORY:
            invoke_sub.add(CommentGen(invoke_sub, " Call kernels and "
                                      "communication routines"))
        else:
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
                    op_name = self.get_fs_operator_name("gh_basis",
                                                        function_space)
                    func_space_var_names.append(op_name)
                if self.diff_basis_required(function_space):
                    # add the diff_basis array name to the list to use later
                    op_name = self.get_fs_operator_name("gh_diff_basis",
                                                        function_space)
                    func_space_var_names.append(op_name)
            # add the required deallocate call
            invoke_sub.add(DeallocateGen(invoke_sub, func_space_var_names))
        invoke_sub.add(CommentGen(invoke_sub, ""))
        # finally, add me to my parent
        parent.add(invoke_sub)


class DynSchedule(Schedule):
    ''' The Dynamo specific schedule class. This passes the Dynamo-
    specific factories for creating kernel and infrastructure calls
    to the base class so it creates the ones we require. '''

    def __init__(self, arg):
        from dynamo0p3_builtins import DynBuiltInCallFactory
        Schedule.__init__(self, DynKernCallFactory, DynBuiltInCallFactory, arg)

    def view(self, indent=0):
        '''a method implemented by all classes in a schedule which display the
        tree in a textual form. This method overrides the default view
        method to include distributed memory information '''
        print self.indent(indent) + "Schedule[invoke='" + self.invoke.name + \
            "' dm="+str(config.DISTRIBUTED_MEMORY)+"]"
        for entity in self._children:
            entity.view(indent=indent + 1)


class DynHaloExchange(HaloExchange):

    ''' Dynamo specific halo exchange class which can be added to and
    manipulated in, a schedule '''

    def __init__(self, field, check_dirty=True, parent=None,
                 vector_index=None, inc=False):

        self._vector_index = vector_index
        if field.descriptor.stencil:
            halo_type = field.descriptor.stencil['type']
            halo_depth = field.descriptor.stencil['extent']
            if not halo_depth:
                # halo_depth is provided by the algorithm layer
                halo_depth = stencil_extent_value(field)
            else:
                halo_depth = str(halo_depth)
            if inc:
                # there is an inc writer which needs redundant
                # computation so our halo depth must be increased by 1
                halo_depth += "+1"
        else:
            halo_type = 'region'
            halo_depth = "1"
        HaloExchange.__init__(self, field, halo_type, halo_depth,
                              check_dirty, parent=parent)

    def gen_code(self, parent):
        ''' Dynamo specific code generation for this class '''
        from f2pygen import IfThenGen, CallGen, CommentGen
        if self._vector_index:
            ref = "(" + str(self._vector_index) + ")"
        else:
            ref = ""
        if self._check_dirty:
            if_then = IfThenGen(parent, self._field.proxy_name + ref +
                                "%is_dirty(depth=" + self._halo_depth +
                                ")")
            parent.add(if_then)
            halo_parent = if_then
        else:
            halo_parent = parent
        halo_parent.add(
            CallGen(
                halo_parent, name=self._field.proxy_name + ref +
                "%halo_exchange(depth=" + self._halo_depth + ")"))
        parent.add(CommentGen(parent, ""))


class DynLoop(Loop):
    ''' The Dynamo specific Loop class. This passes the Dynamo
    specific loop information to the base class so it creates the one
    we require.  Creates Dynamo specific loop bounds when the code is
    being generated. '''

    def __init__(self, parent=None, loop_type=""):
        Loop.__init__(self, parent=parent,
                      valid_loop_types=VALID_LOOP_TYPES)
        self.loop_type = loop_type
        self._kern = None

        # Get the namespace manager instance so we can look-up
        # the name of the nlayers and ndf variables
        self._name_space_manager = NameSpaceFactory().create()

        # set our variable name at initialisation as it might be
        # required by other classes before code generation
        if self._loop_type == "colours":
            self._variable_name = "colour"
        elif self._loop_type == "colour":
            self._variable_name = "cell"
        elif self._loop_type == "dofs":
            self._variable_name = self._name_space_manager.\
                create_name(root_name="df",
                            context="PSyVars",
                            label="dof_loop_idx")
        else:
            self._variable_name = "cell"

        # At this stage we don't know what our loop bounds are
        self._lower_bound_name = None
        self._lower_bound_index = None
        self._upper_bound_name = None
        self._upper_bound_index = None

    def view(self, indent=0):
        ''' Print out a textual representation of this loop. We override
        this method from the Loop class because, in Dynamo0.3, the
        function space is now an object and we need to call orig_name on
        it '''
        print self.indent(indent) +\
            "Loop[type='{0}',field_space='{1}',it_space='{2}']".\
            format(self._loop_type, self._field_space.orig_name,
                   self.iteration_space)
        for entity in self._children:
            entity.view(indent=indent + 1)

    def load(self, kern):
        ''' Load the state of this Loop using the supplied Kernel
        object. This method is provided so that we can individually
        construct Loop objects for a given kernel call. '''
        self._kern = kern

        self._field = kern.arguments.iteration_space_arg()
        self._field_name = self._field.name
        self._field_space = self._field.function_space
        self._iteration_space = kern.iterates_over  # cells etc.

        # Loop bounds
        self.set_lower_bound("start")

        from dynamo0p3_builtins import DynBuiltIn
        if isinstance(kern, DynBuiltIn):
            # If the kernel is a built-in/pointwise operation
            # then this loop must be over DoFs
            self.set_upper_bound("dofs")
        else:
            if config.DISTRIBUTED_MEMORY:
                if self.field_space.orig_name in DISCONTINUOUS_FUNCTION_SPACES:
                    self.set_upper_bound("edge")
                elif self.field_space.orig_name in CONTINUOUS_FUNCTION_SPACES:
                    self.set_upper_bound("halo", index=1)
                elif self.field_space.orig_name in VALID_ANY_SPACE_NAMES:
                    self.set_upper_bound("halo", index=1)
                else:
                    raise GenerationError(
                        "Unexpected function space found. Expecting one of "
                        "{0} but found '{1}'".format(
                            str(VALID_FUNCTION_SPACES),
                            self.field_space.orig_name))
            else:  # sequential
                self.set_upper_bound("cells")

    def set_lower_bound(self, name, index=None):
        ''' Set the lower bounds of this loop '''
        if name not in VALID_LOOP_BOUNDS_NAMES:
            raise GenerationError(
                "The specified lower bound loop name is invalid")
        if name in ["inner", "halo"] and index < 1:
            raise GenerationError(
                "The specified index '{0}' for this lower loop bound is "
                "invalid".format(str(index)))
        self._lower_bound_name = name
        self._lower_bound_index = index

    def set_upper_bound(self, name, index=None):
        ''' Set the upper bounds of this loop '''
        if name not in VALID_LOOP_BOUNDS_NAMES:
            raise GenerationError(
                "The specified upper bound loop name is invalid")
        if name == "start":
            raise GenerationError("'start' is not a valid upper bound")
        if name in ["inner", "halo"] and index < 1:
            raise GenerationError(
                "The specified index '{0}' for this upper loop bound is "
                "invalid".format(str(index)))
        self._upper_bound_name = name
        self._upper_bound_index = index

    def _lower_bound_fortran(self):
        ''' Create the associated fortran code for the type of lower bound '''
        if not config.DISTRIBUTED_MEMORY and self._lower_bound_name != "start":
            raise GenerationError(
                "The lower bound must be 'start' if we are sequential but "
                "found '{0}'".format(self._upper_bound_name))
        if self._lower_bound_name == "start":
            return "1"
        else:
            # the start of our space is the end of the previous space +1
            if self._lower_bound_name == "inner":
                prev_space_name = self._lower_bound_name
                prev_space_index_str = str(self._lower_bound_index + 1)
            elif self._lower_bound_name == "edge":
                prev_space_name = "inner"
                prev_space_index_str = "1"
            elif (self._lower_bound_name == "halo" and
                  self._lower_bound_index == 1):
                prev_space_name = "edge"
                prev_space_index_str = ""
            elif (self._lower_bound_name == "halo" and
                  self._lower_bound_index > 1):
                prev_space_name = self._lower_bound_name
                prev_space_index_str = str(self._lower_bound_index - 1)
            else:
                raise GenerationError("Unsupported lower bound name found")
            mesh_obj_name = self._name_space_manager.create_name(
                root_name="mesh", context="PSyVars", label="mesh")
            return mesh_obj_name + "%get_last_" + prev_space_name + "_cell(" \
                + prev_space_index_str + ")+1"

    def _upper_bound_fortran(self):
        ''' Create the associated fortran code for the type of upper bound '''
        if self._upper_bound_name == "ncolours":
            return "ncolour"
        elif self._upper_bound_name == "ncolour":
            return "ncp_colour(colour)"
        elif not config.DISTRIBUTED_MEMORY:
            if self._upper_bound_name == "cells":
                return self.field.proxy_name_indexed + "%" + \
                    self.field.ref_name() + "%get_ncell()"
            # keep ncolours and ncolour here as options as we will
            # need them again when the DM colouring API is implemented
            elif self._upper_bound_name == "ncolours":
                return "ncolour"
            elif self._upper_bound_name == "ncolour":
                return "ncp_colour(colour)"
            elif self._upper_bound_name == "dofs":
                return self._kern.undf_name
            else:
                raise GenerationError(
                    "For sequential/shared-memory code, the upper loop "
                    "bound must be one of ncolours, ncolour, cells or dofs "
                    "but got '{0}'".format(self._upper_bound_name))
        else:
            if self._upper_bound_name in ["inner", "halo"]:
                index = self._upper_bound_index
            else:
                index = ""
            mesh_obj_name = self._name_space_manager.create_name(
                root_name="mesh", context="PSyVars", label="mesh")
            return mesh_obj_name + "%get_last_" + self._upper_bound_name + \
                "_cell(" + str(index) + ")"

    def has_inc_arg(self, mapping=None):
        ''' Returns True if any of the Kernels called within this loop
        have an argument with INC access. Returns False otherwise. '''
        if mapping is not None:
            my_mapping = mapping
        else:
            my_mapping = FIELD_ACCESS_MAP
        return Loop.has_inc_arg(self, my_mapping)

    def unique_fields_with_halo_reads(self):
        ''' Returns all fields in this loop that require at least some
        of their halo to be clean to work correctly. '''

        unique_fields = []
        unique_field_names = []

        for call in self.calls():
            for arg in call.arguments.args:
                if self._halo_read_access(arg):
                    if arg.name not in unique_field_names:
                        unique_field_names.append(arg.name)
                        unique_fields.append(arg)
        return unique_fields

    def _halo_read_access(self, arg):
        '''Determines whether this argument reads from the halo for this
        loop'''
        if arg.descriptor.stencil:
            # TODO: check assumption that "inner" includes the stencil
            return self._upper_bound_name in ["halo", "edge"]
        if arg.type in VALID_SCALAR_NAMES:
            # scalars do not have halos
            return False
        elif arg.type == "gh_operator":
            # operators do not have halos
            return False
        elif arg.discontinuous and arg.access.lower() == "gh_read":
            # there are no shared dofs so access to inner and edge are
            # local so we only care about reads in the halo
            return self._upper_bound_name == "halo"
        elif arg.access.lower() in ["gh_read", "gh_inc"]:
            # it is either continuous or we don't know (any_space_x)
            # and we need to assume it may be continuous for
            # correctness. There may be shared dofs so only access to
            # inner is local so we care about reads in both the edge
            # (annexed dofs) and the halo
            return self._upper_bound_name in ["halo", "edge"]
        else:
            # access is neither a read nor an inc so does not need halo
            return False

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

        # get fortran loop bounds
        self._start = self._lower_bound_fortran()
        self._stop = self._upper_bound_fortran()
        Loop.gen_code(self, parent)

        if config.DISTRIBUTED_MEMORY and self._loop_type != "colour":
            # Set halo dirty for all fields that are modified
            from f2pygen import CallGen, CommentGen
            fields = self.unique_modified_args(FIELD_ACCESS_MAP, "gh_field")
            if fields:
                parent.add(CommentGen(parent, ""))
                parent.add(CommentGen(parent,
                                      " Set halos dirty for fields modified "
                                      "in the above loop"))
                parent.add(CommentGen(parent, ""))
                for field in fields:
                    if field.vector_size > 1:
                        # the range function below returns values from
                        # 1 to the vector size which is what we
                        # require in our Fortran code
                        for index in range(1, field.vector_size+1):
                            parent.add(CallGen(parent, name=field.proxy_name +
                                               "(" + str(index) +
                                               ")%set_dirty()"))
                    else:
                        parent.add(CallGen(parent, name=field.proxy_name +
                                           "%set_dirty()"))
                parent.add(CommentGen(parent, ""))


class DynKern(Kern):
    ''' Stores information about Dynamo Kernels as specified by the
    Kernel metadata and associated algorithm call. Uses this
    information to generate appropriate PSy layer code for the Kernel
    instance or to generate a Kernel stub'''

    def __init__(self):
        if False:
            self._arguments = DynKernelArguments(None, None)  # for pyreverse
        self._func_descriptors = None
        self._fs_descriptors = None
        self._qr_required = False
        self._qr_text = ""
        self._qr_name = ""
        self._qr_args = None
        self._name_space_manager = NameSpaceFactory().create()

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
            elif descriptor.type.lower() == "gh_real":
                pre = "rscalar_"
            elif descriptor.type.lower() == "gh_integer":
                pre = "iscalar_"
            else:
                raise GenerationError(
                    "load_meta expected one of '{0}' but "
                    "found '{1}'".format(VALID_ARG_TYPE_NAMES,
                                         descriptor.type))
            args.append(Arg("variable", pre+str(idx+1)))

            if descriptor.stencil:
                if not descriptor.stencil["extent"]:
                    # extent is passed in
                    args.append(Arg("variable", pre+str(idx+1)+"_extent"))
                if descriptor.stencil["type"] == "xory1d":
                    # direction is passed in
                    args.append(Arg("variable", pre+str(idx+1)+"_direction"))

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

        # if there is a quadrature rule, what is the name of the
        # algorithm argument?
        self._qr_text = ""
        self._qr_name = ""
        if self._qr_required:
            qr_arg = args[-1]
            self._qr_text = qr_arg.text
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
            if field_on_space(unique_fs, self.arguments):
                # A map is required as there is a field on this space
                lvars.append(get_fs_map_name(unique_fs))
        # Orientation maps
        for unique_fs in self.arguments.unique_fss:
            if self._fs_descriptors.exists(unique_fs):
                fs_descriptor = self._fs_descriptors.get_descriptor(unique_fs)
                if fs_descriptor.requires_orientation:
                    lvars.append(get_fs_orientation_name(unique_fs))
        return lvars

    def _create_arg_list(self, parent, my_type="call"):
        ''' creates the kernel call or kernel stub subroutine argument
        list. For kernel stubs it also creates the data
        declarations. '''
        from f2pygen import DeclGen, AssignGen, UseGen
        if my_type == "subroutine":
            # add in any required USE associations
            parent.add(UseGen(parent, name="constants_mod", only=True,
                              funcnames=["r_def"]))
        # create the argument list
        arglist = []
        if self._arguments.has_operator:
            # 0.5: provide cell position
            if my_type == "call":
                if self.is_coloured():
                    arglist.append("cmap(colour, cell)")
                else:
                    arglist.append("cell")
            else:
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
        #    then generate the appropriate number of arguments.  If
        #    the field is accessed with a stencil operation then add
        #    in any required additional arguments.
        first_arg = True
        first_arg_decl = None
        for arg in self._arguments.args:
            if arg.type == "gh_field":
                undf_name = get_fs_undf_name(arg.function_space)
                dataref = "%data"
                if arg.vector_size > 1:
                    # the range function below returns values from
                    # 1 to the vector size which is what we
                    # require in our Fortran code
                    for idx in range(1, arg.vector_size+1):
                        if my_type == "subroutine":
                            text = (arg.name + "_" +
                                    arg.function_space.mangled_name +
                                    "_v" + str(idx))
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
                        text = arg.name + "_" + arg.function_space.mangled_name
                        intent = arg.intent
                        decl = DeclGen(parent, datatype="real",
                                       kind="r_def", dimension=undf_name,
                                       intent=intent, entity_decls=[text])
                        parent.add(decl)
                        if first_arg:
                            first_arg = False
                            first_arg_decl = decl
                    else:
                        text = arg.proxy_name + dataref
                    arglist.append(text)
                # add in any required stencil arguments
                if arg.descriptor.stencil:
                    if not arg.descriptor.stencil['extent']:
                        # the extent is not specified in the metadata
                        # so pass the value in
                        if my_type == "subroutine":
                            name = arg.name+"_extent"
                            parent.add(DeclGen(parent, datatype="integer",
                                               intent="in",
                                               entity_decls=[name]))
                        else:
                            name = stencil_extent_value(arg)
                        arglist.append(name)
                    if arg.descriptor.stencil['type'] == "xory1d":
                        # the direction of the stencil is not known so
                        # pass the value in
                        if my_type == "subroutine":
                            name = arg.name+"_direction"
                            parent.add(DeclGen(parent, datatype="integer",
                                               intent="in",
                                               entity_decls=[name]))
                        else:
                            name = arg.stencil.direction_arg.varName
                        arglist.append(name)
                    if my_type != "subroutine":
                        # add in stencil dofmap
                        var_name = stencil_dofmap_name(arg)
                        name = var_name+"(:,:,"
                        if self.is_coloured():
                            name += "cmap(colour, cell)"
                        else:
                            name += "cell"
                        name += ")"
                        arglist.append(name)

            elif arg.type == "gh_operator":
                if my_type == "subroutine":
                    size = arg.name + "_ncell_3d"
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
                    ndf_name_to = get_fs_ndf_name(arg.function_space_to)
                    ndf_name_from = get_fs_ndf_name(arg.function_space_from)
                    parent.add(DeclGen(parent, datatype="real",
                                       kind="r_def",
                                       dimension=ndf_name_to + "," +
                                       ndf_name_from + "," + size,
                                       intent=intent, entity_decls=[text]))
                else:
                    arglist.append(arg.proxy_name_indexed+"%ncell_3d")
                    arglist.append(arg.proxy_name_indexed+"%local_stencil")

            elif arg.type in VALID_SCALAR_NAMES:
                if my_type == "subroutine":
                    if arg.type == "gh_real":
                        decl = DeclGen(parent, datatype="real", kind="r_def",
                                       intent=arg.intent,
                                       entity_decls=[arg.name])
                    elif arg.type == "gh_integer":
                        decl = DeclGen(parent, datatype="integer",
                                       intent=arg.intent,
                                       entity_decls=[arg.name])
                    else:
                        raise GenerationError(
                            "Internal error: expected arg type to be one "
                            "of '{0}' but got '{1}'".format(VALID_SCALAR_NAMES,
                                                            arg.type))
                    parent.add(decl)
                arglist.append(arg.name)

            else:
                raise GenerationError(
                    "Unexpected arg type found in "
                    "dynamo0p3.py:DynKern:gen_code(). Expected one of '{0}' "
                    "but found '{1}'".format(VALID_ARG_TYPE_NAMES, arg.type))
        # 3: For each function space (in the order they appear in the
        # metadata arguments)
        for unique_fs in self.arguments.unique_fss:
            # 3.1 Provide compulsory arguments common to operators and
            # fields on a space. There is one: "ndf".
            ndf_name = get_fs_ndf_name(unique_fs)
            arglist.append(ndf_name)
            if my_type == "subroutine":
                parent.add(
                    DeclGen(parent, datatype="integer", intent="in",
                            entity_decls=[ndf_name]))
            # 3.1.1 Provide additional compulsory arguments if there
            # is a field on this space
            if field_on_space(unique_fs, self.arguments):
                undf_name = get_fs_undf_name(unique_fs)
                arglist.append(undf_name)
                map_name = get_fs_map_name(unique_fs)
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
                    basis_name = get_fs_basis_name(unique_fs)
                    arglist.append(basis_name)
                    if my_type == "subroutine":
                        # the size of the first dimension for a
                        # basis array depends on the
                        # function space. The values are
                        # w0=1, w1=3, w2=3, w3=1, wtheta=1, w2h=3, w2v=3
                        first_dim = None
                        if unique_fs.orig_name.lower() in \
                           ["w0", "w3", "wtheta"]:
                            first_dim = "1"
                        elif (unique_fs.orig_name.lower() in
                              ["w1", "w2", "w2h", "w2v"]):
                            first_dim = "3"
                        else:
                            raise GenerationError(
                                "Unsupported space for basis function, "
                                "expecting one of {0} but found "
                                "'{1}'".format(VALID_FUNCTION_SPACES,
                                               unique_fs.orig_name))
                        parent.add(DeclGen(parent, datatype="real",
                                           kind="r_def", intent="in",
                                           dimension=first_dim + "," +
                                           ndf_name + "," +
                                           self._qr_args["nh"] + "," +
                                           self._qr_args["nv"],
                                           entity_decls=[basis_name]))
                if descriptor.requires_diff_basis:
                    diff_basis_name = get_fs_diff_basis_name(unique_fs)
                    arglist.append(diff_basis_name)
                    if my_type == "subroutine":
                        # the size of the first dimension for a
                        # differential basis array depends on the
                        # function space. The values are
                        # w0=3, w1=3, w2=1, w3=1, wtheta=3, w2h=1, w2v=1
                        first_dim = None
                        if unique_fs.orig_name.lower() in \
                           ["w2", "w3", "w2h", "w2v"]:
                            first_dim = "1"
                        elif (unique_fs.orig_name.lower() in
                              ["w0", "w1", "wtheta"]):
                            first_dim = "3"
                        else:
                            raise GenerationError(
                                "Unsupported space for differential basis "
                                "function, expecting one of {0} but found "
                                "'{1}'".format(VALID_FUNCTION_SPACES,
                                               unique_fs.orig_name))
                        parent.add(DeclGen(parent, datatype="real",
                                           kind="r_def", intent="in",
                                           dimension=first_dim + "," +
                                           ndf_name + "," +
                                           self._qr_args["nh"] + "," +
                                           self._qr_args["nv"],
                                           entity_decls=[diff_basis_name]))
                if descriptor.requires_orientation:
                    orientation_name = get_fs_orientation_name(unique_fs)
                    arglist.append(orientation_name)
                    if my_type == "subroutine":
                        parent.add(DeclGen(parent, datatype="integer",
                                           intent="in", dimension=ndf_name,
                                           entity_decls=[orientation_name]))
            # 3.3 Fix for boundary_dofs array to the boundary
            # condition kernel (enforce_bc_kernel) arguments
            if self.name.lower() == "enforce_bc_code" and \
               unique_fs.orig_name.lower() == "any_space_1":
                arglist.append("boundary_dofs")
                if my_type == "subroutine":
                    ndf_name = get_fs_ndf_name(unique_fs)
                    parent.add(DeclGen(parent, datatype="integer", intent="in",
                                       dimension=ndf_name+",2",
                                       entity_decls=["boundary_dofs"]))
                if my_type == "call":
                    parent.add(DeclGen(parent, datatype="integer",
                                       pointer=True, entity_decls=[
                                           "boundary_dofs(:,:) => null()"]))
                    fspace = None
                    for fspace in self._arguments.unique_fss:
                        if fspace.orig_name == "any_space_1":
                            break
                    proxy_name = (self._arguments.get_arg_on_space(fspace).
                                  proxy_name)
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
    def incremented_arg(self, mapping=None):
        ''' Returns the argument corresponding to a field or operator that has
        INC access.  '''
        if mapping is None:
            my_mapping = FIELD_ACCESS_MAP
        else:
            my_mapping = mapping
        return Kern.incremented_arg(self, my_mapping)

    @property
    def written_arg(self, mapping=None):
        ''' Returns the argument corresponding to a field or operator that has
        WRITE access '''
        if mapping is None:
            my_mapping = FIELD_ACCESS_MAP
        else:
            my_mapping = mapping
        return Kern.written_arg(self, my_mapping)

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
                arg = self.incremented_arg
            except FieldNotFoundError:
                # TODO Warn that we're colouring a kernel that has
                # no field object with INC access
                arg = self.written_arg

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
                "%" + arg.ref_name() + "%get_colours"
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
                    arg = self.incremented_arg
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
            if field_on_space(unique_fs, self.arguments):
                maps_required = True

        # function-space maps initialisation and their declarations
        if maps_required:
            parent.add(CommentGen(parent, ""))
        for unique_fs in self.arguments.unique_fss:
            if field_on_space(unique_fs, self.arguments):
                # A map is required as there is a field on this space
                map_name = get_fs_map_name(unique_fs)
                field = self._arguments.get_arg_on_space(unique_fs)
                parent.add(AssignGen(parent, pointer=True, lhs=map_name,
                                     rhs=field.proxy_name_indexed +
                                     "%" + field.ref_name(unique_fs) +
                                     "%get_cell_dofmap("+dofmap_args+")"))
        if maps_required:
            parent.add(CommentGen(parent, ""))
        decl_map_names = []
        for unique_fs in self.arguments.unique_fss:
            if field_on_space(unique_fs, self.arguments):
                # A map is required as there is a field on this space
                map_name = get_fs_map_name(unique_fs)
                decl_map_names.append(map_name+"(:) => null()")
        if len(decl_map_names) > 0:
            parent.add(DeclGen(parent, datatype="integer", pointer=True,
                               entity_decls=decl_map_names))
        # orientation arrays initialisation and their declarations
        orientation_decl_names = []
        for unique_fs in self.arguments.unique_fss:
            if self._fs_descriptors.exists(unique_fs):
                fs_descriptor = self._fs_descriptors.get_descriptor(unique_fs)
                if fs_descriptor.requires_orientation:
                    field = self._arguments.get_arg_on_space(unique_fs)
                    oname = get_fs_orientation_name(unique_fs)
                    orientation_decl_names.append(oname+"(:) => null()")
                    parent.add(
                        AssignGen(parent, pointer=True,
                                  lhs=oname,
                                  rhs=field.proxy_name_indexed + "%" +
                                  field.ref_name(unique_fs) +
                                  "%get_cell_orientation(" +
                                  dofmap_args + ")"))
        if orientation_decl_names:
            parent.add(DeclGen(parent, datatype="integer", pointer=True,
                               entity_decls=orientation_decl_names))
            parent.add(CommentGen(parent, ""))

        arglist = self._create_arg_list(parent)

        # generate the kernel call and associated use statement
        parent.add(CallGen(parent, self._name, arglist))
        if not self.module_inline:
            parent.add(UseGen(parent, name=self._module_name,
                              only=True, funcnames=[self._name]))
        # 5: Fix for boundary_dofs array in matrix_vector_code
        if self.name == "matrix_vector_code":
            # In matrix_vector_code, all fields are on the same
            # (unknown) space. Therefore we can use any field to
            # dereference. We choose the 2nd one as that is what is
            # done in the manual implementation.
            reference_arg = self.arguments.args[1]
            enforce_bc_arg = self.arguments.args[0]
            space_names = ["w1", "w2"]
            kern_func_space_name = enforce_bc_arg.function_space
            ndf_name = get_fs_ndf_name(kern_func_space_name)
            undf_name = get_fs_undf_name(kern_func_space_name)
            map_name = get_fs_map_name(kern_func_space_name)
            proxy_name = reference_arg.proxy_name
            self._name_space_manager = NameSpaceFactory().create()
            fs_name = self._name_space_manager.create_name(root_name="fs")
            boundary_dofs_name = self._name_space_manager.create_name(
                root_name="boundary_dofs")
            parent.add(UseGen(parent, name="function_space_mod",
                              only=True, funcnames=space_names))
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
            test_str = ""
            for idx, space_name in enumerate(space_names):
                test_str += "("+fs_name+" .eq. "+space_name+")"
                if idx < (len(space_names)-1):
                    test_str += " .or. "
            if_then = IfThenGen(new_parent, test_str)
            new_parent.add(if_then, position=["before", position])
            if_then.add(AssignGen(if_then, pointer=True,
                                  lhs=boundary_dofs_name,
                                  rhs=proxy_name +
                                  "%vspace%get_boundary_dofs()"))
            parent.add(CommentGen(parent, ""))
            if_then = IfThenGen(parent, test_str)
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
    ''' Provides information about a particular function space used by
    a meta-funcs entry in the kernel metadata. '''

    def __init__(self, descriptor):
        self._descriptor = descriptor

    @property
    def requires_basis(self):
        ''' Returns True if a basis function is associated with this
        function space, otherwise it returns False. '''
        return "gh_basis" in self._descriptor.operator_names

    @property
    def requires_diff_basis(self):
        ''' Returns True if a differential basis function is
        associated with this function space, otherwise it returns
        False. '''
        return "gh_diff_basis" in self._descriptor.operator_names

    @property
    def requires_orientation(self):
        ''' Returns True if an orientation function is
        associated with this function space, otherwise it returns
        False. '''
        return "gh_orientation" in self._descriptor.operator_names

    @property
    def fs_name(self):
        ''' Returns the raw metadata value of this function space. '''
        return self._descriptor.function_space_name


class FSDescriptors(object):
    ''' Contains a collection of FSDescriptor objects and methods
    that provide information across these objects. We have one
    FSDescriptor for each meta-funcs entry in the kernel
    meta-data '''

    def __init__(self, descriptors):
        self._orig_descriptors = descriptors
        self._descriptors = []
        for descriptor in descriptors:
            self._descriptors.append(FSDescriptor(descriptor))

    def exists(self, fspace):
        ''' Return True if a descriptor with the specified function
        space exists, otherwise return False. '''
        for descriptor in self._descriptors:
            # FS descriptors hold information taken from the kernel
            # metadata and therefore it is the original name of
            # the supplied function space that we must look at
            if descriptor.fs_name == fspace.orig_name:
                return True
        return False

    def get_descriptor(self, fspace):
        ''' Return the descriptor with the specified function space
        name. If it does not exist raise an error.'''
        for descriptor in self._descriptors:
            if descriptor.fs_name == fspace.orig_name:
                return descriptor
        raise GenerationError(
            "FSDescriptors:get_descriptor: there is no descriptor for "
            "function space {0}".format(fspace.orig_name))


def check_args(call):
    '''checks that the kernel arguments provided via the invoke call are
    consistent with the information expected, as specified by the
    kernel metadata '''

    # stencil arguments
    stencil_arg_count = 0
    for arg_descriptor in call.ktype.arg_descriptors:
        if arg_descriptor.stencil:
            if not arg_descriptor.stencil['extent']:
                # an extent argument must be provided
                stencil_arg_count += 1
            if arg_descriptor.stencil['type'] == 'xory1d':
                # a dimension argument must be provided
                stencil_arg_count += 1

    # qr_argument
    qr_required = False
    for descriptor in call.ktype.func_descriptors:
        if len(descriptor.operator_names) > 0:
            qr_required = True
    if qr_required:
        qr_arg_count = 1
    else:
        qr_arg_count = 0

    expected_arg_count = len(call.ktype.arg_descriptors) + \
        stencil_arg_count + qr_arg_count

    if expected_arg_count != len(call.args):
        raise GenerationError(
            "error: expected '{0}' arguments in the algorithm layer but "
            "found '{1}'. Expected '{2}' standard arguments, '{3}' "
            "stencil arguments and '{4}' qr_arguments'".format(
                expected_arg_count, len(call.args),
                len(call.ktype.arg_descriptors), stencil_arg_count,
                qr_arg_count))


class DynStencil(object):
    ''' Provides stencil information about a Dynamo argument '''
    def __init__(self, name):
        self._name = name
        self._extent = None
        self._extent_arg = None
        self._direction_arg = None

    @property
    def extent(self):
        ''' returns the extent of the stencil if it is known '''
        return self._extent

    @property
    def extent_arg(self):
        '''returns the algorithm argument associated with the extent value if
        it is not known'''
        return self._extent_arg

    @extent_arg.setter
    def extent_arg(self, value):
        ''' sets the extent_arg value '''
        self._extent_arg = value

    @property
    def direction_arg(self):
        '''returns the direction argument associated with the direction of
        the stencil if it is not known'''
        return self._direction_arg

    @direction_arg.setter
    def direction_arg(self, value):
        ''' sets the direction_arg value '''
        self._direction_arg = value


class DynKernelArguments(Arguments):
    ''' Provides information about Dynamo kernel call arguments
    collectively, as specified by the kernel argument metadata. '''

    def __init__(self, call, parent_call):
        if False:  # for pyreverse
            self._0_to_n = DynKernelArgument(None, None, None, None)

        Arguments.__init__(self, parent_call)

        # check that the arguments provided by the algorithm layer are
        # consistent with those expected by the kernel(s)
        check_args(call)

        # create our arguments and add in stencil information where
        # appropriate.
        self._args = []
        idx = 0
        for arg in call.ktype.arg_descriptors:

            dyn_argument = DynKernelArgument(self, arg, call.args[idx],
                                             parent_call)
            idx += 1
            if dyn_argument.descriptor.stencil:
                stencil = DynStencil(dyn_argument.descriptor.stencil['type'])
                # we can not cover the if test below as the specification of
                # stencil extent in metadata is not supported
                if dyn_argument.descriptor.stencil['extent']:
                    raise GenerationError("extent metadata not yet supported")
                    # if supported we would add the following
                    # line. However, note there is currently no setter
                    # for extent in DynStencil.
                    # stencil.extent = dyn_argument.descriptor.stencil['extent']
                else:
                    # an extent argument has been added
                    stencil.extent_arg = call.args[idx]
                    idx += 1
                if dyn_argument.descriptor.stencil['type'] == 'xory1d':
                    # a direction argument has been added
                    stencil.direction_arg = call.args[idx]
                    idx += 1
                dyn_argument.stencil = stencil
            self._args.append(dyn_argument)

        self._dofs = []

        # Generate a static list of unique function-space names used
        # by the set of arguments: store the mangled names as these
        # are what we use at the level of an Invoke
        self._unique_fs_names = []
        # List of corresponding unique function-space objects
        self._unique_fss = []
        for arg in self._args:
            for function_space in arg.function_spaces:
                # We check that function_space is not None because scalar
                # args don't have one and fields only have one (only
                # operators have two).
                if function_space and \
                   function_space.mangled_name not in self._unique_fs_names:
                    self._unique_fs_names.append(function_space.mangled_name)
                    self._unique_fss.append(function_space)

    def get_arg_on_space(self, func_space):
        '''Returns the first argument (field or operator) found that
        is on the specified function space. If no field or operator is
        found an exception is raised.'''
        for arg in self._args:
            for function_space in arg.function_spaces:
                if function_space and \
                   func_space.mangled_name == function_space.mangled_name:
                    return arg
        raise FieldNotFoundError("DynKernelArguments:get_arg_on_space: there "
                                 "is no field or operator with function space "
                                 "{0}".format(func_space.mangled_name))

    @property
    def has_operator(self):
        ''' Returns true if at least one of the arguments is an operator. '''
        for arg in self._args:
            if arg.type == "gh_operator":
                return True
        return False

    @property
    def unique_fss(self):
        ''' Returns a unique list of function space objects used by the
        arguments of this kernel '''
        return self._unique_fss

    @property
    def unique_fs_names(self):
        ''' Return the list of unique function space names used by the
        arguments of this kernel. The names are unmangled (i.e. as
        specified in the kernel metadata) '''
        return self._unique_fs_names

    def iteration_space_arg(self, mapping=None):
        '''Returns the first argument we can use to dereference the iteration
        space. This can be a field or operator that is modified or
        alternatively a field that is read if one or more scalars
        are modified. '''

        # first look for known function spaces then try any_space
        for spaces in [VALID_FUNCTION_SPACES, VALID_ANY_SPACE_NAMES]:

            # do we have a field or operator that is modified?
            for arg in self._args:
                if arg.type in ["gh_field", "gh_operator"] and \
                   arg.access in ["gh_write", "gh_inc"] \
                   and arg.function_space.orig_name in spaces:
                    return arg

        # no modified fields or operators. Check for unmodified fields
        for arg in self._args:
            if arg.type == "gh_field" and \
               arg.access == "gh_read":
                return arg

        # it is an error if we get to here
        raise GenerationError(
            "iteration_space_arg(). The dynamo0.3 api must have a modified "
            "field, a modified operator, or an unmodified field (in the case "
            "of a modified scalar). None of these were found.")

    @property
    def dofs(self):
        ''' Currently required for invoke base class although this
        makes no sense for dynamo. Need to refactor the invoke class
        and pull out dofs into the gunghoproto api. '''
        return self._dofs


class DynKernelArgument(KernelArgument):
    ''' Provides information about individual Dynamo kernel call
    arguments as specified by the kernel argument metadata. '''

    def __init__(self, kernel_args, arg_meta_data, arg_info, call):
        KernelArgument.__init__(self, arg_meta_data, arg_info, call)
        # Keep a reference to DynKernelArguments object that contains
        # this argument. This permits us to manage name-mangling for
        # any-space function spaces.
        self._kernel_args = kernel_args
        self._vector_size = arg_meta_data.vector_size
        self._type = arg_meta_data.type
        self._stencil = None

        # The list of function-space objects for this argument. Each
        # object can be queried for its original name and for the
        # mangled name (used to make any-space arguments distinct
        # within an invoke). The argument will only have more than
        # one function-space associated with it if it is an operator.
        fs1 = None
        fs2 = None

        if self._type == "gh_operator":

            fs1 = FunctionSpace(arg_meta_data.function_space_to,
                                self._kernel_args)
            fs2 = FunctionSpace(arg_meta_data.function_space_from,
                                self._kernel_args)
        else:
            if arg_meta_data.function_space:
                fs1 = FunctionSpace(arg_meta_data.function_space,
                                    self._kernel_args)
        self._function_spaces = [fs1, fs2]

    @property
    def descriptor(self):
        ''' return a descriptor object which contains Kernel
        metadata about this argument '''
        return self._arg

    def ref_name(self, function_space=None):
        ''' Returns the name used to dereference this type of argument. '''
        if not function_space:
            if self._type == "gh_operator":
                # For an operator we use the 'from' FS
                function_space = self._function_spaces[1]
            else:
                function_space = self._function_spaces[0]
        else:
            # Check that the supplied function space is valid for this
            # argument
            found = False
            for fspace in self.function_spaces:
                if fspace and fspace.orig_name == function_space.orig_name:
                    found = True
                    break
            if not found:
                raise GenerationError(
                    "DynKernelArgument:ref_name(fs). The supplied function "
                    "space (fs='{0}') is not one of the function spaces "
                    "associated with this argument (fss='{1}')".format(
                        function_space.orig_name,
                        self.function_space_names))
        if self._type == "gh_field":
            return "vspace"
        elif self._type == "gh_operator":
            if function_space.orig_name == self.descriptor.function_space_from:
                return "fs_from"
            elif function_space.orig_name == self.descriptor.function_space_to:
                return "fs_to"
            else:
                raise GenerationError(
                    "ref_name: Error, function space '{0}' is one of the "
                    "gh_operator function spaces '{1}' but is not being "
                    "returned by either function_space from '{2}' or "
                    "function_space_to '{3}'".format(
                        function_space.orig_name, self.function_spaces,
                        self.descriptor.function_space_from,
                        self.descriptor.function_space_to))
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
    def name_indexed(self):
        ''' Returns the name for this argument with an
        additional index which accesses the first element for a vector
        argument. '''
        if self._vector_size > 1:
            return self._name+"(1)"
        else:
            return self._name

    @property
    def function_space(self):
        ''' Returns the expected finite element function space for this
            argument as specified by the kernel argument metadata. '''
        if self._type == "gh_operator":
            return self.function_spaces[1]
        else:
            return self._function_spaces[0]

    @property
    def function_space_to(self):
        ''' Returns the 'to' function space of an operator '''
        return self._function_spaces[0]

    @property
    def function_space_from(self):
        ''' Returns the 'from' function space of an operator '''
        return self._function_spaces[1]

    @property
    def function_spaces(self):
        ''' Returns the expected finite element function spaces for this
        argument as a list as specified by the kernel argument
        metadata. We have more than one function space when dealing
        with operators. '''
        return self._function_spaces

    @property
    def function_space_names(self):
        ''' Returns a list of the names of the function spaces associated
        with this argument. We have more than one function space when
        dealing with operators. '''
        fs_names = []
        for fspace in self._function_spaces:
            if fspace:
                fs_names.append(fspace.orig_name)
        return fs_names

    @property
    def intent(self):
        ''' Returns the fortran intent of this argument. '''
        if self.access == "gh_read":
            return "in"
        elif self.access == "gh_write":
            return "out"
        elif self.access in ["gh_inc"] + VALID_REDUCTION_NAMES:
            return "inout"
        else:
            raise GenerationError(
                "Expecting argument access to be one of 'gh_read, gh_write, "
                "gh_inc' or one of {0}, but found '{1}'".
                format(str(VALID_REDUCTION_NAMES), self.access))

    @property
    def discontinuous(self):
        '''Returns True if this argument is known to be on a discontinuous
        function space, otherwise returns False.'''
        if self.function_space in DISCONTINUOUS_FUNCTION_SPACES:
            return True
        elif self.function_space in VALID_ANY_SPACE_NAMES:
            # we will eventually look this up based on our dependence
            # analysis but for the moment we assume the worst
            return False
        else:  # must be a continuous function space
            return False

    @property
    def stencil(self):
        ''' Return a stencil information object if it exists '''
        return self._stencil

    @stencil.setter
    def stencil(self, value):
        ''' Set our stencil information '''
        self._stencil = value


class DynKernCallFactory(object):
    ''' Create the necessary framework for a Dynamo kernel call.
    This consists of a Loop over cells containing a call to the
    user-supplied kernel routine. '''
    @staticmethod
    def create(call, parent=None):
        ''' Create the objects needed for a call to the kernel
        described in the call object '''

        # Loop over cells
        cloop = DynLoop(parent=parent)

        # The kernel itself
        kern = DynKern()
        kern.load(call, cloop)

        # Add the kernel as a child of the loop
        cloop.addchild(kern)

        # Set-up the loop now we have the kernel object
        cloop.load(kern)

        # Return the outermost loop
        return cloop
