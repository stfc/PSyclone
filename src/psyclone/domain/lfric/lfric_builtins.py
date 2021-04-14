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
# Author A. R. Porter, STFC Daresbury Lab
# Modified I. Kavcic, Met Office

''' This module implements the support for 'built-in' operations in the
    PSyclone LFRic (Dynamo 0.3) API. Each supported built-in is implemented
    as a different Python class, all inheriting from the LFRicBuiltIn class.
    The LFRicBuiltInCallFactory creates the Python object required for
    a given built-in call. '''

from __future__ import absolute_import
from psyclone.core.access_type import AccessType
from psyclone.psyGen import BuiltIn
from psyclone.parse.utils import ParseError
from psyclone.domain.lfric import LFRicArgDescriptor
from psyclone.f2pygen import AssignGen
from psyclone.configuration import Config

# The name of the file containing the meta-data describing the
# built-in operations for this API
BUILTIN_DEFINITIONS_FILE = "lfric_builtins_mod.f90"

# The types of argument that are valid for built-in kernels in the
# LFRic API
VALID_BUILTIN_ARG_TYPES = LFRicArgDescriptor.VALID_FIELD_NAMES + \
    LFRicArgDescriptor.VALID_SCALAR_NAMES

# Valid LFRic iteration spaces for built-in kernels
BUILTIN_ITERATION_SPACES = ["dof"]


# Function to return the built-in operations that we support for this API.
# The meta-data describing these kernels is in lfric_builtins_mod.f90.
# The built-in operations F90 capitalised names are dictionary keys and need
# to be converted to lower case for invoke-generation purpose.
def get_lowercase_builtin_map(builtin_map_capitalised_dict):
    '''
    Convert the names of the supported built-in operations to lowercase
    for comparison and invoke-generation purpose.

    :param builtin_map_capitalised_dict: a dictionary of built-in names.
    :type builtin_map_capitalised_dict: dict of str

    :returns: a dictionary of lowercase Fortran built-in names as keys \
              and case-sensitive Python built-in names as values.
    :rtype: dict of str

    '''
    builtin_map_dict = {}
    for fortran_name in builtin_map_capitalised_dict:
        python_name = builtin_map_capitalised_dict[fortran_name]
        builtin_map_dict[fortran_name.lower()] = python_name
    return builtin_map_dict


class LFRicBuiltInCallFactory(object):
    '''
    Creates the necessary framework for a call to an LFRic built-in,
    This consists of the operation itself and the loop over unique DoFs.

    '''

    def __str__(self):
        return "Factory for a call to an LFRic built-in."

    @staticmethod
    def create(call, parent=None):
        '''
        Create the objects needed for a call to the built-in described in
        the call (BuiltInCall) object.

        :param call: details of the call to this built-in in the \
                     Algorithm layer.
        :type call: :py:class:`psyclone.parse.algorithm.BuiltInCall`
        :param parent: the schedule instance to which the built-in call \
                       belongs.
        :type parent: :py:class:`psyclone.dynamo0p3.DynInvokeSchedule`

        :raises ParseError: if the name of the function being called is \
                            not a recognised built-in.

        '''
        if call.func_name not in BUILTIN_MAP:
            raise ParseError(
                "Unrecognised built-in call in LFRic API: found '{0}' but "
                "expected one of {1}.".
                format(call.func_name, list(BUILTIN_MAP_CAPITALISED.keys())))

        # Use our dictionary to get the correct Python object for
        # this built-in.
        builtin = BUILTIN_MAP[call.func_name]()

        # Create the loop over DoFs
        from psyclone.dynamo0p3 import DynLoop
        dofloop = DynLoop(parent=parent,
                          loop_type=BUILTIN_ITERATION_SPACES[0])

        # Use the call object (created by the parser) to set-up the state
        # of the infrastructure kernel
        builtin.load(call, parent=dofloop.loop_body)

        # Set-up its state
        dofloop.load(builtin)

        # As it is the innermost loop it has the kernel as a loop_body
        # child.
        dofloop.loop_body.addchild(builtin)

        # Return the outermost loop
        return dofloop


# TODO #1140 'LFRicBuiltIn' should be an abstract class.
class LFRicBuiltIn(BuiltIn):
    '''
    Parent class for a call to an LFRic Built-in.
    '''
    def __init__(self):
        # Builtins do not accept quadrature
        self.qr_rules = {}
        # Builtins cannot request mesh properties
        self.mesh = None
        super(LFRicBuiltIn, self).__init__()

    def __str__(self):
        ''' :raises NotImplementedError: if the method is called. '''
        raise NotImplementedError("LFRicBuiltIn.__str__ must be overridden")

    def load(self, call, parent=None):
        '''
        Populate the state of this object using the supplied call object.

        :param call: The BuiltIn object from which to extract information
                     about this built-in call.
        :type call: :py:class:`psyclone.parse.algorithm.BuiltInCall`
        :param parent: The parent node of the kernel call in the AST
                       we are constructing. This will be a loop.
        :type parent: :py:class:`psyclone.dynamo0p3.DynLoop`

        '''
        from psyclone.dynamo0p3 import FSDescriptors, DynKernelArguments
        BuiltIn.load(self, call, DynKernelArguments, parent)
        self.arg_descriptors = call.ktype.arg_descriptors
        self._func_descriptors = call.ktype.func_descriptors
        self._fs_descriptors = FSDescriptors(call.ktype.func_descriptors)
        self._idx_name = \
            self.root.symbol_table.symbol_from_tag("dof_loop_idx", "df").name
        # Check that this built-in kernel is valid
        self._validate()

    def _validate(self):
        '''
        Check that this built-in conforms to the LFRic API.

        :raises ParseError: if a built-in call does not iterate over DoFs.
        :raises ParseError: if an argument to a built-in kernel is not \
                            one of valid argument types.
        :raises ParseError: if a built-in kernel writes to more than \
                            one argument.
        :raises ParseError: if a built-in kernel does not have at least \
                            one field argument.
        :raises ParseError: if all field arguments are not on the same space.
        :raises ParseError: if all field arguments of a non-conversion \
                            built-in do not have the same data type.

        '''
        # Check that our assumption that we're looping over DoFs is valid
        if self.iterates_over not in BUILTIN_ITERATION_SPACES:
            raise ParseError(
                "In the LFRic API built-in calls must operate on "
                "DoFs but found '{0}' for {1}.".
                format(self.iterates_over, str(self)))
        # Check write count, field arguments and spaces
        write_count = 0  # Only one argument must be written to
        field_count = 0  # We must have one or more fields as arguments
        spaces = set()   # All field arguments must be on the same space
        # Field data types must be the same except for the conversion built-ins
        data_types = set()
        for arg in self.arg_descriptors:
            # Check valid argument types
            if arg.argument_type not in VALID_BUILTIN_ARG_TYPES:
                raise ParseError(
                    "In the LFRic API an argument to a built-in kernel "
                    "must be one of {0} but kernel '{1}' has an argument of "
                    "type '{2}'.".format(VALID_BUILTIN_ARG_TYPES, self.name,
                                         arg.argument_type))
            # Built-ins update fields DoF by DoF and therefore can have
            # WRITE/READWRITE access
            if arg.access in [AccessType.WRITE, AccessType.SUM,
                              AccessType.READWRITE]:
                write_count += 1
            if arg.argument_type in LFRicArgDescriptor.VALID_FIELD_NAMES:
                field_count += 1
                spaces.add(arg.function_space)
                data_types.add(arg.data_type)

        if write_count != 1:
            raise ParseError("A built-in kernel in the LFRic API must "
                             "have one and only one argument that is written "
                             "to but found {0} for kernel '{1}'.".
                             format(write_count, self.name))
        if field_count == 0:
            raise ParseError("A built-in kernel in the LFRic API "
                             "must have at least one field as an argument but "
                             "kernel '{0}' has none.".format(self.name))
        if len(spaces) != 1:
            spaces_str = [str(x) for x in sorted(spaces)]
            raise ParseError(
                "All field arguments to a built-in in the LFRic API "
                "must be on the same space. However, found spaces {0} for "
                "arguments to '{1}'".format(spaces_str, self.name))

        conversion_builtins = ["int_X", "real_X"]
        conversion_builtins_lower = [x.lower() for x in conversion_builtins]
        if len(data_types) != 1 and self.name not in conversion_builtins_lower:
            data_types_str = [str(x) for x in sorted(data_types)]
            raise ParseError(
                "In the LFRic API only the data type conversion built-ins "
                "{0} are allowed to have field arguments of different "
                "data types. However, found different data types "
                "{1} for field arguments to '{2}'.".
                format(conversion_builtins, data_types_str, self.name))

    def array_ref(self, fld_name):
        '''
        :returns: the array reference for a proxy with the supplied name.
        :rtype: str

        '''
        return fld_name + "%data(" + self._idx_name + ")"

    @property
    def undf_name(self):
        '''
        Dynamically looks up the name of the undf variable for the
        space that this kernel updates.

        :returns: the name of the undf variable.
        :rtype: str

        '''
        field = self._arguments.iteration_space_arg()
        return field.function_space.undf_name

    @property
    def qr_required(self):
        '''
        Built-ins do not currently require quadrature.

        :returns: False
        :rtype: bool

        '''
        return False

    @property
    def reference_element(self):
        '''
        Built-ins do not require reference-element properties.

        :returns: None
        :rtype: NoneType

        '''
        return None

    def gen_code(self, parent):
        raise NotImplementedError("LFRicBuiltIn.gen_code must be overridden")

    def cma_operation(self):
        '''
        Built-ins do not perform operations with Column-Matrix-Assembly
        operators.

        :returns: None
        :rtype: NoneType

        '''

        return None

    @property
    def is_intergrid(self):
        '''
        We don't have any inter-grid Built-ins.

        :returns: False
        :rtype: bool

        '''
        return False

    @property
    def fs_descriptors(self):
        '''
        :returns: a list of function space descriptor objects which \
                  contain information about the function spaces.
        :rtype: list of :py:class:`psyclone.dynamo0p3.FSDescriptor`

        '''
        return self._fs_descriptors


# ******************************************************************* #
# ************** Built-ins for real-valued fields ******************* #
# ******************************************************************* #

# ------------------------------------------------------------------- #
# ============== Adding (scaled) real fields ======================== #
# ------------------------------------------------------------------- #


class LFRicXPlusYKern(LFRicBuiltIn):
    ''' Add one, real-valued, field to another and return the result as
    a third, real-valued, field.

    '''
    def __str__(self):
        return "Built-in: Add real-valued fields"

    def gen_code(self, parent):
        '''
        Generates LFRic API specific PSy code for a call to the
        X_plus_Y Built-in.

        :param parent: Node in f2pygen tree to which to add call.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        '''
        # We add each element of f2 to the corresponding element of f1
        # and store the result in f3 (real-valued fields).
        field_name3 = self.array_ref(self._arguments.args[0].proxy_name)
        field_name1 = self.array_ref(self._arguments.args[1].proxy_name)
        field_name2 = self.array_ref(self._arguments.args[2].proxy_name)
        parent.add(AssignGen(parent, lhs=field_name3,
                             rhs=field_name1 + " + " + field_name2))


class LFRicIncXPlusYKern(LFRicBuiltIn):
    ''' Add the second, real-valued, field to the first field and return it.

    '''
    def __str__(self):
        return "Built-in: Increment a real-valued field"

    def gen_code(self, parent):
        '''
        Generates LFRic API specific PSy code for a call to the
        inc_X_plus_Y Built-in.

        :param parent: Node in f2pygen tree to which to add call.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        '''
        # We add each element of f1 to the corresponding element of f2
        # and store the result back in f1 (real-valued fields).
        field_name1 = self.array_ref(self._arguments.args[0].proxy_name)
        field_name2 = self.array_ref(self._arguments.args[1].proxy_name)
        parent.add(AssignGen(parent, lhs=field_name1,
                             rhs=field_name1 + " + " + field_name2))


class LFRicAPlusXKern(LFRicBuiltIn):
    ''' `Y = a + X` where `a` is a real scalar and `X` and `Y` are
    real-valued fields (DoF-wise addition of a scalar value).

    '''
    def __str__(self):
        return "Built-in: a_plus_X (real-valued fields)"

    def gen_code(self, parent):
        '''
        Generates LFRic API specific PSy code for a call to the
        a_plus_X Built-in.

        :param parent: Node in f2pygen tree to which to add call.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        '''
        # We add a scalar value to each element of f1 and store the
        # result in f2 (real-valued fields).
        field_name2 = self.array_ref(self._arguments.args[0].proxy_name)
        scalar_name = self._arguments.args[1].name
        field_name1 = self.array_ref(self._arguments.args[2].proxy_name)
        parent.add(AssignGen(parent, lhs=field_name2,
                             rhs=scalar_name + " + " + field_name1))


class LFRicIncAPlusXKern(LFRicBuiltIn):
    ''' `X = a + X` where `a` is a real scalar and `X` is a real-valued
    field (DoF-wise addition of a scalar value).

    '''
    def __str__(self):
        return "Built-in: inc_a_plus_X (real-valued field)"

    def gen_code(self, parent):
        '''
        Generates LFRic API specific PSy code for a call to the
        inc_a_plus_X Built-in.

        :param parent: Node in f2pygen tree to which to add call.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        '''
        # We add a scalar value to each element of f1 and return the
        # result in f1 (real-valued fields).
        field_name = self.array_ref(self._arguments.args[1].proxy_name)
        scalar_name = self._arguments.args[0].name
        parent.add(AssignGen(parent, lhs=field_name,
                             rhs=scalar_name + " + " + field_name))


class LFRicAXPlusYKern(LFRicBuiltIn):
    ''' `Z = a*X + Y` where `a` is a real scalar and `Z`, `X` and
    `Y` are real-valued fields.

    '''
    def __str__(self):
        return "Built-in: aX_plus_Y (real-valued fields)"

    def gen_code(self, parent):
        '''
        Generates LFRic API specific PSy code for a call to the
        aX_plus_Y Built-in.

        :param parent: Node in f2pygen tree to which to add call.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        '''
        # We multiply one element of field f1 (3rd arg) by a real scalar
        # (2nd arg), add it to the corresponding element of a second
        # field (4th arg)  and write the value to the corresponding
        # element of field f3 (1st arg) (real-valed fields).
        field_name3 = self.array_ref(self._arguments.args[0].proxy_name)
        scalar_name = self._arguments.args[1].name
        field_name1 = self.array_ref(self._arguments.args[2].proxy_name)
        field_name2 = self.array_ref(self._arguments.args[3].proxy_name)
        rhs_expr = scalar_name + "*" + field_name1 + " + " + field_name2
        parent.add(AssignGen(parent, lhs=field_name3, rhs=rhs_expr))


class LFRicIncAXPlusYKern(LFRicBuiltIn):
    ''' `X = a*X + Y` where `a` is a real scalar and `X` and `Y` are
    real-valued fields.

    '''
    def __str__(self):
        return "Built-in: inc_aX_plus_Y (real-valued fields)"

    def gen_code(self, parent):
        '''
        Generates LFRic API specific PSy code for a call to the
        inc_aX_plus_Y Built-in.

        :param parent: Node in f2pygen tree to which to add call.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        '''
        # We multiply one element of field f1 (2nd arg) by a real scalar
        # (1st arg), add it to the corresponding element of a second
        # field (3rd arg) and write the value back into the element
        # of field f1 (real-valued fields).
        scalar_name = self._arguments.args[0].name
        field_name1 = self.array_ref(self._arguments.args[1].proxy_name)
        field_name2 = self.array_ref(self._arguments.args[2].proxy_name)
        rhs_expr = scalar_name + "*" + field_name1 + " + " + field_name2
        parent.add(AssignGen(parent, lhs=field_name1, rhs=rhs_expr))


class LFRicIncXPlusBYKern(LFRicBuiltIn):
    ''' `X = X + b*Y` where `b` is a real scalar and `X` and `Y` are
    real-valued fields.

    '''
    def __str__(self):
        return "Built-in: inc_X_plus_bY (real-valued fields)"

    def gen_code(self, parent):
        '''
        Generates LFRic API specific PSy code for a call to the
        inc_X_plus_bY Built-in.

        :param parent: Node in f2pygen tree to which to add call.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        '''
        # We multiply one element of field f2 (3rd arg) by a real scalar
        # (2nd arg), add it to the corresponding element of a first field
        # f1 (1st arg) and write the value back into the element of field
        # f1 (real-valued fields).
        scalar_name = self._arguments.args[1].name
        field_name1 = self.array_ref(self._arguments.args[0].proxy_name)
        field_name2 = self.array_ref(self._arguments.args[2].proxy_name)
        rhs_expr = field_name1 + " + " + scalar_name + "*" + field_name2
        parent.add(AssignGen(parent, lhs=field_name1, rhs=rhs_expr))


class LFRicAXPlusBYKern(LFRicBuiltIn):
    ''' `Z = a*X + b*Y` where `a` and `b` are real scalars and `Z`, `X` and
    `Y` are real-valued fields.

    '''
    def __str__(self):
        return "Built-in: aX_plus_bY (real-valued fields)"

    def gen_code(self, parent):
        '''
        Generates LFRic API specific PSy code for a call to the
        aX_plus_bY Built-in.

        :param parent: Node in f2pygen tree to which to add call.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        '''
        # We multiply one element of field f1 (3rd arg) by the first, real,
        # scalar (2nd arg), add it to the product of the corresponding
        # element of a second field (5th arg) with the second, real, scalar
        # (4th arg) and write the value to the corresponding element of
        # field f3 (1st arg) (real-valued fields).
        field_name3 = self.array_ref(self._arguments.args[0].proxy_name)
        scalar_name1 = self._arguments.args[1].name
        scalar_name2 = self._arguments.args[3].name
        field_name1 = self.array_ref(self._arguments.args[2].proxy_name)
        field_name2 = self.array_ref(self._arguments.args[4].proxy_name)
        rhs_expr = (scalar_name1 + "*" + field_name1 + " + " +
                    scalar_name2 + "*" + field_name2)
        parent.add(AssignGen(parent, lhs=field_name3, rhs=rhs_expr))


class LFRicIncAXPlusBYKern(LFRicBuiltIn):
    ''' `X = a*X + b*Y` where `a` and `b` are real scalars and `X` and `Y`
    are real-valued fields.

    '''
    def __str__(self):
        return "Built-in: inc_aX_plus_bY (real-valued fields)"

    def gen_code(self, parent):
        '''
        Generates LFRic API specific PSy code for a call to the
        inc_aX_plus_bY Built-in.

        :param parent: Node in f2pygen tree to which to add call.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        '''
        # We multiply one element of field f1 (2nd arg) by the first, real,
        # scalar (1st arg), add it to the product of the corresponding
        # element of a second field (4th arg) with the second, real, scalar
        # (4rd arg) and write the value back into the element of field f1
        # (real-valued fields).
        scalar_name1 = self._arguments.args[0].name
        scalar_name2 = self._arguments.args[2].name
        field_name1 = self.array_ref(self._arguments.args[1].proxy_name)
        field_name2 = self.array_ref(self._arguments.args[3].proxy_name)
        rhs_expr = (scalar_name1 + "*" + field_name1 + " + " +
                    scalar_name2 + "*" + field_name2)
        parent.add(AssignGen(parent, lhs=field_name1, rhs=rhs_expr))


class LFRicAXPlusAYKern(LFRicBuiltIn):
    ''' `Z = a*X + a*Y = a*(X + Y)` where `a` is a real scalar and `Z`,
    `X` and `Y` are real-valued fields.

    '''
    def __str__(self):
        return "Built-in: aX_plus_aY (real-valued fields)"

    def gen_code(self, parent):
        '''
        Generates LFRic API specific PSy code for a call to the
        aX_plus_aY Built-in.

        :param parent: Node in f2pygen tree to which to add call.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        '''
        # We add each element of field f1 (3rd arg) to the corresponding
        # element of field f2 (4th arg), multiply the sum by a real
        # scalar (2nd arg) and write the value to the corresponding
        # element of field f3 (1st arg) (real-valued fields).
        field_name3 = self.array_ref(self._arguments.args[0].proxy_name)
        scalar_name = self._arguments.args[1].name
        field_name1 = self.array_ref(self._arguments.args[2].proxy_name)
        field_name2 = self.array_ref(self._arguments.args[3].proxy_name)
        rhs_expr = (scalar_name + " * (" + field_name1 + " + " +
                    field_name2 + ")")
        parent.add(AssignGen(parent, lhs=field_name3, rhs=rhs_expr))


# ------------------------------------------------------------------- #
# ============== Subtracting (scaled) real fields =================== #
# ------------------------------------------------------------------- #


class LFRicXMinusYKern(LFRicBuiltIn):
    ''' Subtract one, real-valued, field from another and return the
    result as a third, real-valued, field.

    '''
    def __str__(self):
        return "Built-in: Subtract real-valued fields"

    def gen_code(self, parent):
        '''
        Generates LFRic API specific PSy code for a call to the
        X_minus_Y Built-in.

        :param parent: Node in f2pygen tree to which to add call.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        '''
        # We subtract each element of f2 from the corresponding element
        # of f1 and store the result in f3 (real-valued fields).
        field_name3 = self.array_ref(self._arguments.args[0].proxy_name)
        field_name1 = self.array_ref(self._arguments.args[1].proxy_name)
        field_name2 = self.array_ref(self._arguments.args[2].proxy_name)
        assign = AssignGen(parent, lhs=field_name3,
                           rhs=field_name1 + " - " + field_name2)
        parent.add(assign)


class LFRicIncXMinusYKern(LFRicBuiltIn):
    ''' Subtract the second, real-valued, field from the first field
    and return it.

    '''
    def __str__(self):
        return "Built-in: Decrement a real-valued field"

    def gen_code(self, parent):
        '''
        Generates LFRic API specific PSy code for a call to the
        inc_X_minus_Y Built-in.

        :param parent: Node in f2pygen tree to which to add call.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        '''
        # We subtract each element of f1 from the corresponding element of f2
        # and store the result back in f1 (real-valued fields).
        field_name1 = self.array_ref(self._arguments.args[0].proxy_name)
        field_name2 = self.array_ref(self._arguments.args[1].proxy_name)
        parent.add(AssignGen(parent, lhs=field_name1,
                             rhs=field_name1 + " - " + field_name2))


class LFRicAXMinusYKern(LFRicBuiltIn):
    ''' `Z = a*X - Y` where `a` is a real scalar and `Z`, `X` and
    `Y` are real-valued fields.

    '''
    def __str__(self):
        return "Built-in: aX_minus_Y (real-valued fields)"

    def gen_code(self, parent):
        '''
        Generates LFRic API specific PSy code for a call to the
        aX_minus_Y Built-in.

        :param parent: Node in f2pygen tree to which to add call.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        '''
        # We multiply one element of field f1 (3rd arg) by a real scalar
        # (2nd arg), subtract it from the corresponding element of a
        # second field (4th arg) and write the value to the corresponding
        # element of field f3 (1st arg) (real-valued fields).
        field_name3 = self.array_ref(self._arguments.args[0].proxy_name)
        scalar_name = self._arguments.args[1].name
        field_name1 = self.array_ref(self._arguments.args[2].proxy_name)
        field_name2 = self.array_ref(self._arguments.args[3].proxy_name)
        rhs_expr = scalar_name + "*" + field_name1 + " - " + field_name2
        parent.add(AssignGen(parent, lhs=field_name3, rhs=rhs_expr))


class LFRicXMinusBYKern(LFRicBuiltIn):
    ''' `Z = X - b*Y` where `b` is a real scalar and `Z`, `X` and
    `Y` are real-valued fields.

    '''
    def __str__(self):
        return "Built-in: X_minus_bY (real-valued fields)"

    def gen_code(self, parent):
        '''
        Generates LFRic API specific PSy code for a call to the
        X_minus_bY Built-in.

        :param parent: Node in f2pygen tree to which to add call.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        '''
        # We multiply one element of field f2 (4th arg) by a real scalar
        # (3rd arg), subtract it from the corresponding element of a first
        # field f1 (2nd arg) and write the value to the corresponding
        # element of field f3 (1st arg) (real-valued fields).
        field_name3 = self.array_ref(self._arguments.args[0].proxy_name)
        scalar_name = self._arguments.args[2].name
        field_name1 = self.array_ref(self._arguments.args[1].proxy_name)
        field_name2 = self.array_ref(self._arguments.args[3].proxy_name)
        rhs_expr = field_name1 + " - " + scalar_name + "*" + field_name2
        parent.add(AssignGen(parent, lhs=field_name3, rhs=rhs_expr))


class LFRicIncXMinusBYKern(LFRicBuiltIn):
    ''' `X = X - b*Y` where `b` is a real scalar and `X` and `Y` are
    real-valued fields.

    '''
    def __str__(self):
        return "Built-in: inc_X_minus_bY (real-valued fields)"

    def gen_code(self, parent):
        '''
        Generates LFRic API specific PSy code for a call to the
        inc_X_minus_bY Built-in.

        :param parent: Node in f2pygen tree to which to add call.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        '''
        # We multiply one element of field f2 (3rd arg) by a real scalar
        # (2nd arg), subtract it from the corresponding element of field
        # f1 (1st arg) and write the value back into the element of
        # field f1 (real-valued fields).
        scalar_name = self._arguments.args[1].name
        field_name1 = self.array_ref(self._arguments.args[0].proxy_name)
        field_name2 = self.array_ref(self._arguments.args[2].proxy_name)
        rhs_expr = field_name1 + " - " + scalar_name + "*" + field_name2
        parent.add(AssignGen(parent, lhs=field_name1, rhs=rhs_expr))


class LFRicAXMinusBYKern(LFRicBuiltIn):
    ''' `Z = a*X - b*Y` where `a` and `b` are real scalars and `Z`, `X` and
    `Y` are real-valued fields.

    '''
    def __str__(self):
        return "Built-in: aX_minus_bY (real-valued fields)"

    def gen_code(self, parent):
        '''
        Generates LFRic API specific PSy code for a call to the
        aX_minus_bY Built-in.

        :param parent: Node in f2pygen tree to which to add call.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        '''
        # We multiply one element of field f2 (5th arg) by the second, real,
        # scalar (4th arg), subtract it from the product of the corresponding
        # element of the first field (3rd arg) with the first, real, scalar
        # (2nd arg) and write the value to the corresponding element of
        # field f3 (1st arg) (real-valued fields).
        field_name3 = self.array_ref(self._arguments.args[0].proxy_name)
        scalar_name1 = self._arguments.args[1].name
        scalar_name2 = self._arguments.args[3].name
        field_name1 = self.array_ref(self._arguments.args[2].proxy_name)
        field_name2 = self.array_ref(self._arguments.args[4].proxy_name)
        rhs_expr = (scalar_name1 + "*" + field_name1 + " - " +
                    scalar_name2 + "*" + field_name2)
        parent.add(AssignGen(parent, lhs=field_name3, rhs=rhs_expr))


# ------------------------------------------------------------------- #
# ============== Multiplying (scaled) real fields =================== #
# ------------------------------------------------------------------- #


class LFRicXTimesYKern(LFRicBuiltIn):
    ''' DoF-wise product of one, real-valued, field with another with
    the result returned as a third, real-valued, field.

    '''
    def __str__(self):
        return "Built-in: Multiply real-valued fields"

    def gen_code(self, parent):
        '''
        Generates LFRic API specific PSy code for a call to the
        X_times_Y Built-in.

        :param parent: Node in f2pygen tree to which to add call.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        '''
        # We multiply each element of f1 by the corresponding element
        # of f2 and store the result in f3 (real-valued fields).
        field_name3 = self.array_ref(self._arguments.args[0].proxy_name)
        field_name1 = self.array_ref(self._arguments.args[1].proxy_name)
        field_name2 = self.array_ref(self._arguments.args[2].proxy_name)
        assign = AssignGen(parent, lhs=field_name3,
                           rhs=field_name1 + " * " + field_name2)
        parent.add(assign)


class LFRicIncXTimesYKern(LFRicBuiltIn):
    ''' Multiply the first, real-valued, field by the second and return it.

    '''
    def __str__(self):
        return "Built-in: Multiply one real-valued field by another"

    def gen_code(self, parent):
        '''
        Generates LFRic API specific PSy code for a call to the
        inc_X_times_Y Built-in.

        :param parent: Node in f2pygen tree to which to add call.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        '''
        # We multiply each element of f1 by the corresponding element of
        # f2 and store the result back in f1 (real-valued fields).
        field_name1 = self.array_ref(self._arguments.args[0].proxy_name)
        field_name2 = self.array_ref(self._arguments.args[1].proxy_name)
        parent.add(AssignGen(parent, lhs=field_name1,
                             rhs=field_name1 + " * " + field_name2))


class LFRicIncAXTimesYKern(LFRicBuiltIn):
    ''' `X = a*X*Y` where `a` is a real scalar and `X` and `Y` are
    real-valued fields.

    '''
    def __str__(self):
        return "Built-in: inc_aX_times_Y (real-valued fields)"

    def gen_code(self, parent):
        '''
        Generates LFRic API specific PSy code for a call to the
        inc_aX_times_Y Built-in.

        :param parent: Node in f2pygen tree to which to add call.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        '''
        # We multiply a real scalar (1st arg) by a DoF-wise product of
        # (real-valued) fields f1 (2nd arg) and f2 (3rd arg) and write
        # the value back into the element of field f1.
        scalar_name = self._arguments.args[0].name
        field_name1 = self.array_ref(self._arguments.args[1].proxy_name)
        field_name2 = self.array_ref(self._arguments.args[2].proxy_name)
        rhs_expr = scalar_name + " * " + field_name1 + " * " + field_name2
        parent.add(AssignGen(parent, lhs=field_name1, rhs=rhs_expr))


# ------------------------------------------------------------------- #
# ============== Scaling real fields ================================ #
# ------------------------------------------------------------------- #


class LFRicATimesXKern(LFRicBuiltIn):
    ''' Multiply the first, real-valued, field by a real scalar and
    return the result as a second, real-valued, field (`Y = a*X`).

    '''
    def __str__(self):
        return "Built-in: Copy a scaled real-valued field"

    def gen_code(self, parent):
        '''
        Generates LFRic API specific PSy code for a call to the
        a_times_X Built-in.

        :param parent: Node in f2pygen tree to which to add call.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        '''
        # We multiply each element of f1 by the real scalar argument and
        # store the result in f2 (real-valued fields).
        field_name2 = self.array_ref(self._arguments.args[0].proxy_name)
        scalar_name = self._arguments.args[1].name
        field_name1 = self.array_ref(self._arguments.args[2].proxy_name)
        parent.add(AssignGen(parent, lhs=field_name2,
                             rhs=scalar_name + " * " + field_name1))


class LFRicIncATimesXKern(LFRicBuiltIn):
    ''' Multiply a real-valued field by a real scalar and return it.

    '''
    def __str__(self):
        return "Built-in: Scale a real-valued field"

    def gen_code(self, parent):
        '''
        Generates LFRic API specific PSy code for a call to the
        inc_a_times_X Built-in.

        :param parent: Node in f2pygen tree to which to add call.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        '''
        # In this case we're multiplying each element of a real-valued
        # field by the supplied real scalar value.
        field_name = self.array_ref(self._arguments.args[1].proxy_name)
        scalar_name = self._arguments.args[0].name
        parent.add(AssignGen(parent, lhs=field_name,
                             rhs=scalar_name + " * " + field_name))


# ------------------------------------------------------------------- #
# ============== Dividing real fields =============================== #
# ------------------------------------------------------------------- #


class LFRicXDividebyYKern(LFRicBuiltIn):
    ''' Divide the first, real-valued, field by the second and return
    the result as a third, real-valued, field.

    '''
    def __str__(self):
        return "Built-in: Divide real-valued fields"

    def gen_code(self, parent):
        '''
        Generates LFRic API specific PSy code for a call to the
        X_divideby_Y Built-in.

        :param parent: Node in f2pygen tree to which to add call.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        '''
        # We divide each element of f1 by the corresponding element of
        # f2 and store the result in f3 (real-valued fields).
        field_name3 = self.array_ref(self._arguments.args[0].proxy_name)
        field_name1 = self.array_ref(self._arguments.args[1].proxy_name)
        field_name2 = self.array_ref(self._arguments.args[2].proxy_name)
        parent.add(AssignGen(parent, lhs=field_name3,
                             rhs=field_name1 + " / " + field_name2))


class LFRicIncXDividebyYKern(LFRicBuiltIn):
    ''' Divide the first, real-valued, field by the second and return it.

    '''
    def __str__(self):
        return "Built-in: Divide one real-valued field by another"

    def gen_code(self, parent):
        '''
        Generates LFRic API specific PSy code for a call to the
        inc_X_divideby_Y Built-in.

        :param parent: Node in f2pygen tree to which to add call.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        '''
        # We divide each element of f1 by the corresponding element of
        # f2 and store the result back in f1 (real-valued fields).
        field_name1 = self.array_ref(self._arguments.args[0].proxy_name)
        field_name2 = self.array_ref(self._arguments.args[1].proxy_name)
        parent.add(AssignGen(parent, lhs=field_name1,
                             rhs=field_name1 + " / " + field_name2))


# ------------------------------------------------------------------- #
# ============== Inverse scaling of real fields ===================== #
# ------------------------------------------------------------------- #


class LFRicADividebyXKern(LFRicBuiltIn):
    ''' DoF-wise division of a scalar value `a` by the elements
    of a real-valued field, `X`, storing the result in another,
    real-valued, field, `Y` (`Y = a/X`).

    '''
    def __str__(self):
        return "Built-in: Inverse scaling of a real-valued field (Y = a/X)"

    def gen_code(self, parent):
        '''
        Generates LFRic API specific PSy code for a call to the
        a_divideby_X Built-in.

        :param parent: Node in f2pygen tree to which to add call.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        '''
        # We divide the real scalar argument by each element of f1 and
        # store the result in f2 (real-valued fields).
        field_name2 = self.array_ref(self._arguments.args[0].proxy_name)
        scalar_name = self._arguments.args[1].name
        field_name1 = self.array_ref(self._arguments.args[2].proxy_name)
        parent.add(AssignGen(parent, lhs=field_name2,
                             rhs=scalar_name + " / " + field_name1))


class LFRicIncADividebyXKern(LFRicBuiltIn):
    ''' DoF-wise division of a scalar value `a` by the elements
    of a real-valued field, `X`, storing the result in the same
    field (`X = a/X`).

    '''
    def __str__(self):
        return "Built-in: Inverse scaling of a real-valued field (X = a/X)"

    def gen_code(self, parent):
        '''
        Generates LFRic API specific PSy code for a call to the
        inc_a_divideby_X Built-in.

        :param parent: Node in f2pygen tree to which to add call.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        '''
        # We divide the real scalar argument by each element of f1 and
        # return the result in f1 (real-valued field).
        field_name = self.array_ref(self._arguments.args[1].proxy_name)
        scalar_name = self._arguments.args[0].name
        parent.add(AssignGen(parent, lhs=field_name,
                             rhs=scalar_name + " / " + field_name))


# ------------------------------------------------------------------- #
# ============== Raising a real field to a scalar =================== #
# ------------------------------------------------------------------- #


class LFRicIncXPowrealAKern(LFRicBuiltIn):
    ''' Raise a real-valued field to a real power and return it.

    '''
    def __str__(self):
        return "Built-in: Raise a real-valued field to a real power"

    def gen_code(self, parent):
        '''
        Generates LFRic API specific PSy code for a call to the
        X_powreal_a Built-in.

        :param parent: Node in f2pygen tree to which to add call.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        '''
        # In this case we're raising each element of a real-valued field
        # to a supplied real scalar value.
        field_name = self.array_ref(self._arguments.args[0].proxy_name)
        real_power = self._arguments.args[1].name
        parent.add(AssignGen(parent, lhs=field_name,
                             rhs=field_name + "**" + real_power))


class LFRicIncXPowintNKern(LFRicBuiltIn):
    ''' Raise a real-valued field to an integer power and return it.

    '''
    def __str__(self):
        return "Built-in: Raise a real-valued field to an integer power"

    def gen_code(self, parent):
        '''
        Generates LFRic API specific PSy code for a call to the
        X_powint_n Built-in.

        :param parent: Node in f2pygen tree to which to add call.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        '''
        # In this case we're raising each element of a real-valued field
        # to a supplied integer scalar value.
        field_name = self.array_ref(self._arguments.args[0].proxy_name)
        integer_power = self._arguments.args[1].name
        parent.add(AssignGen(parent, lhs=field_name,
                             rhs=field_name + "**" + integer_power))


# ------------------------------------------------------------------- #
# ============== Setting real field elements to a value  ============ #
# ------------------------------------------------------------------- #


class LFRicSetvalCKern(LFRicBuiltIn):
    ''' Set a real-valued field equal to a real scalar value.

    '''
    def __str__(self):
        return "Built-in: Set a real-valued field to a real scalar value"

    def gen_code(self, parent):
        '''
        Generates LFRic API specific PSy code for a call to the
        setval_c Built-in.

        :param parent: Node in f2pygen tree to which to add call.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        '''
        # In this case we're assigning a single real scalar value to all
        # elements of a real-valued field.
        field_name = self.array_ref(self._arguments.args[0].proxy_name)
        scalar_value = self._arguments.args[1]
        parent.add(AssignGen(parent, lhs=field_name, rhs=scalar_value))


class LFRicSetvalXKern(LFRicBuiltIn):
    ''' Set a real-valued field equal to another, real-valued, field.

    '''
    def __str__(self):
        return "Built-in: Set a real-valued field equal to another such field"

    def gen_code(self, parent):
        '''
        Generates LFRic API specific PSy code for a call to the
        setval_X Built-in.

        :param parent: Node in f2pygen tree to which to add call.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        '''
        # We copy one element of field X (second arg) to the corresponding
        # element of field Y (first arg) (real-valued fields).
        field_name2 = self.array_ref(self._arguments.args[0].proxy_name)
        field_name1 = self.array_ref(self._arguments.args[1].proxy_name)
        parent.add(AssignGen(parent, lhs=field_name2, rhs=field_name1))


# ------------------------------------------------------------------- #
# ============== Inner product of real fields ======================= #
# ------------------------------------------------------------------- #


class LFRicXInnerproductYKern(LFRicBuiltIn):
    ''' Calculates the inner product of two real-valued fields,
    `innprod = SUM( X(:)*Y(:) )`.

    '''
    def __str__(self):
        return "Built-in: X_innerproduct_Y (real-valued fields)"

    def gen_code(self, parent):
        '''
        Generates LFRic API specific PSy code for a call to the
        X_innerproduct_Y Built-in.

        :param parent: Node in f2pygen tree to which to add call.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        '''
        # We sum the DoF-wise product of the supplied real-valued fields.
        # The real scalar variable holding the sum is initialised to zero
        # in the PSy layer.
        innprod_name = self._reduction_ref(self._arguments.args[0].name)
        field_name1 = self.array_ref(self._arguments.args[1].proxy_name)
        field_name2 = self.array_ref(self._arguments.args[2].proxy_name)
        rhs_expr = innprod_name + " + " + field_name1 + "*" + field_name2
        parent.add(AssignGen(parent, lhs=innprod_name, rhs=rhs_expr))


class LFRicXInnerproductXKern(LFRicBuiltIn):
    ''' Calculates the inner product of one real-valued field by itself,
    `innprod = SUM( X(:)*X(:) )`.

    '''
    def __str__(self):
        return "Built-in: X_innerproduct_X (real-valued fields)"

    def gen_code(self, parent):
        '''
        Generates LFRic API specific PSy code for a call to the
        X_innerproduct_X Built-in.

        :param parent: Node in f2pygen tree to which to add call.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        '''
        # We sum the DoF-wise product of the supplied real-valued fields.
        # The real scalar variable holding the sum is initialised to zero
        # in the PSy layer.
        innprod_name = self._reduction_ref(self._arguments.args[0].name)
        field_name = self.array_ref(self._arguments.args[1].proxy_name)
        rhs_expr = innprod_name + " + " + field_name + "*" + field_name
        parent.add(AssignGen(parent, lhs=innprod_name, rhs=rhs_expr))


# ------------------------------------------------------------------- #
# ============== Sum real field elements ============================ #
# ------------------------------------------------------------------- #


class LFRicSumXKern(LFRicBuiltIn):
    ''' Computes the sum of the elements of a real-valued field.

    '''
    def __str__(self):
        return "Built-in: Sum a real-valued field"

    def gen_code(self, parent):
        '''
        Generates LFRic API specific PSy code for a call to the
        sum_X Built-in.

        :param parent: Node in f2pygen tree to which to add call.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        '''
        # Sum all the elements of a real-valued field. The real scalar
        # variable holding the sum is initialised to zero in the PSy layer.
        field_name = self.array_ref(self._arguments.args[1].proxy_name)
        sum_name = self._reduction_ref(self._arguments.args[0].name)
        rhs_expr = sum_name + " + " + field_name
        parent.add(AssignGen(parent, lhs=sum_name, rhs=rhs_expr))


# ------------------------------------------------------------------- #
# ============== Sign of real field elements ======================== #
# ------------------------------------------------------------------- #


class LFRicSignXKern(LFRicBuiltIn):
    ''' Returns the sign of a real-valued field elements using the
    Fortran intrinsic `sign` function, `Y = sign(a, X)`.

    '''
    def __str__(self):
        return "Built-in: Sign of a real-valued field"

    def gen_code(self, parent):
        '''
        Generates LFRic API specific PSy code for a call to the
        sign_X Built-in.

        :param parent: Node in f2pygen tree to which to add call.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        '''
        # Return the sign of all the elements of a real-valued field
        # applied to the supplied real scalar.
        field_name2 = self.array_ref(self._arguments.args[0].proxy_name)
        scalar_name = self._arguments.args[1].name
        field_name1 = self.array_ref(self._arguments.args[2].proxy_name)
        rhs_expr = ("sign(" + scalar_name + ", " + field_name1 + ")")
        parent.add(AssignGen(parent, lhs=field_name2, rhs=rhs_expr))


# ------------------------------------------------------------------- #
# ============== Converting real to integer field elements ========== #
# ------------------------------------------------------------------- #


class LFRicIntXKern(LFRicBuiltIn):
    ''' Converts real-valued field elements to integer-valued
    field elements using the Fortran intrinsic `int` function,
    `Y = int(X, i_def)`. Here `Y` is an integer-valued field and
    `X` is the real-valued field being converted. The correct `kind`
    is read from the PSyclone configuration file.

    '''
    def __str__(self):
        return "Built-in: Convert a real-valued to an integer-valued field"

    def gen_code(self, parent):
        '''
        Generates LFRic API specific PSy code for a call to the
        int_X Built-in.

        :param parent: Node in f2pygen tree to which to add call.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        '''
        api_config = Config.get().api_conf("dynamo0.3")
        # Convert all the elements of a real-valued field to the
        # corresponding elements of an integer-valued field using
        # the PSyclone configuration for the correct 'kind'.
        field_name2 = self.array_ref(self._arguments.args[0].proxy_name)
        field_name1 = self.array_ref(self._arguments.args[1].proxy_name)
        rhs_expr = ("int(" + field_name1 + ", " +
                    api_config.default_kind["integer"] + ")")
        parent.add(AssignGen(parent, lhs=field_name2, rhs=rhs_expr))


# ******************************************************************* #
# ************** Built-ins for integer-valued fields **************** #
# ******************************************************************* #

# ------------------------------------------------------------------- #
# ============== Adding integer fields ============================== #
# ------------------------------------------------------------------- #

class LFRicIntXPlusYKern(LFRicXPlusYKern):
    ''' Add corresponding elements of two, integer-valued, fields, `X`
    and `Y`, and return the result as a third, integer-valued, field, `Z`.
    Inherits the `gen_code` method from the real-valued built-in
    equivalent `LFRicXPlusYKern`.

    '''
    def __str__(self):
        return "Built-in: Add integer-valued fields"


class LFRicIntIncXPlusYKern(LFRicIncXPlusYKern):
    ''' Add each element of an integer-valued field, `X`, to the
    corresponding element of another integer-valued field, `Y`, and
    store the result back in `X`.
    Inherits the `gen_code` method from the real-valued built-in
    equivalent `LFRicIncXPlusYKern`.

    '''
    def __str__(self):
        return "Built-in: Increment an integer-valued field"


class LFRicIntAPlusXKern(LFRicAPlusXKern):
    ''' Add an integer scalar value, `a`, to each element of an
    integer-valued field, `X`, and return the result as a second,
    integer-valued, field, `Y`.
    Inherits the `gen_code` method from the real-valued built-in
    equivalent `LFRicAPlusXKern`.

    '''
    def __str__(self):
        return "Built-in: int_a_plus_X (integer-valued fields)"


class LFRicIntIncAPlusXKern(LFRicIncAPlusXKern):
    ''' Add an integer scalar value, `a`, to each element of an
    integer-valued field, `X`, and return the result in the
    same field.
    Inherits the `gen_code` method from the real-valued built-in
    equivalent `LFRicIncAPlusXKern`.

    '''
    def __str__(self):
        return "Built-in: int_inc_a_plus_X (integer-valued field)"


# ------------------------------------------------------------------- #
# ============== Subtracting integer fields ========================= #
# ------------------------------------------------------------------- #


class LFRicIntXMinusYKern(LFRicXMinusYKern):
    ''' Subtract each element of an integer-valued field, `Y`, from
    the corresponding element of another, integer-valued, field, `X`,
    and return the result as a third, integer-valued, field, `Z`.
    Inherits the `gen_code` method from the real-valued built-in
    equivalent `LFRicXMinusYKern`.

    '''
    def __str__(self):
        return "Built-in: Subtract integer-valued fields"


class LFRicIntIncXMinusYKern(LFRicIncXMinusYKern):
    ''' Subtract each element of an integer-valued field, `Y`, from
    the corresponding element of another, integer-valued, field, `X`,
    and store the result back in `X`.
    Inherits the `gen_code` method from the real-valued built-in
    equivalent `LFRicXMinusYKern`.

    '''
    def __str__(self):
        return "Built-in: Decrement an integer-valued field"


# ------------------------------------------------------------------- #
# ============== Multiplying integer fields ========================= #
# ------------------------------------------------------------------- #


class LFRicIntXTimesYKern(LFRicXTimesYKern):
    ''' Multiply each element of one, integer-valued, field, `X`, by
    the corresponding element of another, integer-valued, field, `Y`,
    and return the result as a third, integer-valued, field, `Z`.
    Inherits the `gen_code` method from the real-valued built-in
    equivalent `LFRicXTimesYKern`.

    '''
    def __str__(self):
        return "Built-in: Multiply integer-valued fields"


class LFRicIntIncXTimesYKern(LFRicIncXTimesYKern):
    ''' Multiply each element of one, integer-valued, field, `X`, by
    the corresponding element of another, integer-valued, field, `Y`,
    and store the result back in `X`.
    Inherits the `gen_code` method from the real-valued built-in
    equivalent `LFRicIncXTimesYKern`.

    '''
    def __str__(self):
        return "Built-in: Multiply one integer-valued field by another"


# ------------------------------------------------------------------- #
# ============== Scaling integer fields ============================= #
# ------------------------------------------------------------------- #


class LFRicIntATimesXKern(LFRicATimesXKern):
    ''' Multiply each element of the first, integer-valued, field, `X`,
     by an integer scalar, `a`, and return the result as a second,
    integer-valued, field `Y` (`Y = a*X`).
    Inherits the `gen_code` method from the real-valued built-in
    equivalent `LFRicATimesXKern`.

    '''
    def __str__(self):
        return "Built-in: Copy a scaled integer-valued field"


class LFRicIntIncATimesXKern(LFRicIncATimesXKern):
    ''' Multiply each element of an integer-valued field, `X` by
    an integer scalar, `a`, and store the result back in `X`.
    Inherits the `gen_code` method from the real-valued built-in
    equivalent `LFRicIncATimesXKern`.

    '''
    def __str__(self):
        return "Built-in: Scale an integer-valued field"


# ------------------------------------------------------------------- #
# ============== Setting integer field elements to a value  ========= #
# ------------------------------------------------------------------- #


class LFRicIntSetvalCKern(LFRicSetvalCKern):
    ''' Assign a single constant integer scalar value, `c`, to all
    elements of an integer-valued field, `X`.
    Inherits the `gen_code` method from the real-valued built-in
    equivalent `LFRicSetvalCKern`.

    '''
    def __str__(self):
        return ("Built-in: Set an integer-valued field to an integer "
                "scalar value")


class LFRicIntSetvalXKern(LFRicSetvalXKern):
    ''' Copy one element of an integer-valued field (second argument),
    `X`, to the corresponding element of another, integer-valued,
    field (first argument), `Y`.
    Inherits the `gen_code` method from the real-valued built-in
    equivalent `LFRicSetvalXKern`.

    '''
    def __str__(self):
        return ("Built-in: Set an integer-valued field equal to another "
                "such field")


# ------------------------------------------------------------------- #
# ============== Sign of integer field elements ===================== #
# ------------------------------------------------------------------- #


class LFRicIntSignXKern(LFRicSignXKern):
    ''' Returns the sign of an integer-valued field elements using the
    Fortran intrinsic `sign` function, `Y = sign(a, X)`.
    Inherits the `gen_code` method from the real-valued built-in
    equivalent `LFRicSignXKern`.

    '''
    def __str__(self):
        return "Built-in: Sign of an integer-valued field"


# ------------------------------------------------------------------- #
# ============== Converting integer to real field elements ========== #
# ------------------------------------------------------------------- #


class LFRicRealXKern(LFRicBuiltIn):
    ''' Converts integer-valued field elements to real-valued
    field elements using the Fortran intrinsic `real` function,
    `Y = real(X, r_def)`. Here `Y` is a real-valued field and `X`
    is the integer-valued field being converted. The correct `kind`
    is read from the PSyclone configuration file.

    '''
    def __str__(self):
        return "Built-in: Convert an integer-valued to a real-valued field"

    def gen_code(self, parent):
        '''
        Generates LFRic API specific PSy code for a call to the
        real_X Built-in.

        :param parent: Node in f2pygen tree to which to add call.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        '''
        api_config = Config.get().api_conf("dynamo0.3")
        # Convert all the elements of an integer-valued field to
        # the corresponding elements of a real-valued field using
        # the PSyclone configuration for the correct 'kind'.
        field_name2 = self.array_ref(self._arguments.args[0].proxy_name)
        field_name1 = self.array_ref(self._arguments.args[1].proxy_name)
        rhs_expr = ("real(" + field_name1 + ", " +
                    api_config.default_kind["real"] + ")")
        parent.add(AssignGen(parent, lhs=field_name2, rhs=rhs_expr))


# The built-in operations that we support for this API. The meta-data
# describing these kernels is in lfric_builtins_mod.f90. This dictionary
# can only be defined after all of the necessary 'class' statements have
# been executed (happens when this module is imported into another).
# Built-ins for real-valued fields
REAL_BUILTIN_MAP_CAPITALISED = {
    # Adding (scaled) real fields
    "X_plus_Y": LFRicXPlusYKern,
    "inc_X_plus_Y": LFRicIncXPlusYKern,
    "a_plus_X": LFRicAPlusXKern,
    "inc_a_plus_X": LFRicIncAPlusXKern,
    "aX_plus_Y": LFRicAXPlusYKern,
    "inc_aX_plus_Y": LFRicIncAXPlusYKern,
    "inc_X_plus_bY": LFRicIncXPlusBYKern,
    "aX_plus_bY": LFRicAXPlusBYKern,
    "inc_aX_plus_bY": LFRicIncAXPlusBYKern,
    "aX_plus_aY": LFRicAXPlusAYKern,
    # Subtracting (scaled) real fields
    "X_minus_Y": LFRicXMinusYKern,
    "inc_X_minus_Y": LFRicIncXMinusYKern,
    "aX_minus_Y": LFRicAXMinusYKern,
    "X_minus_bY": LFRicXMinusBYKern,
    "inc_X_minus_bY": LFRicIncXMinusBYKern,
    "aX_minus_bY": LFRicAXMinusBYKern,
    # Multiplying (scaled) real fields
    "X_times_Y": LFRicXTimesYKern,
    "inc_X_times_Y": LFRicIncXTimesYKern,
    "inc_aX_times_Y": LFRicIncAXTimesYKern,
    # Multiplying real fields by a real scalar (scaling fields)
    "a_times_X": LFRicATimesXKern,
    "inc_a_times_X": LFRicIncATimesXKern,
    # Dividing real fields
    "X_divideby_Y": LFRicXDividebyYKern,
    "inc_X_divideby_Y": LFRicIncXDividebyYKern,
    # Dividing a real scalar by elements of a real field
    # (inverse scaling of fields)
    "a_divideby_X": LFRicADividebyXKern,
    "inc_a_divideby_X": LFRicIncADividebyXKern,
    # Raising a real field to a scalar
    "inc_X_powreal_a": LFRicIncXPowrealAKern,
    "inc_X_powint_n": LFRicIncXPowintNKern,
    # Setting real field elements to scalar or other
    # real field's values
    "setval_c": LFRicSetvalCKern,
    "setval_X": LFRicSetvalXKern,
    # Inner product of real fields
    "X_innerproduct_Y": LFRicXInnerproductYKern,
    "X_innerproduct_X": LFRicXInnerproductXKern,
    # Sum values of a real field
    "sum_X": LFRicSumXKern,
    # Sign of real field elements applied to a scalar value
    "sign_X": LFRicSignXKern,
    # Converting real to integer field elements
    "int_X": LFRicIntXKern}

# Built-ins for integer-valued fields
INT_BUILTIN_MAP_CAPITALISED = {
    # Adding integer fields
    "int_X_plus_Y": LFRicIntXPlusYKern,
    "int_inc_X_plus_Y": LFRicIntIncXPlusYKern,
    "int_a_plus_X": LFRicIntAPlusXKern,
    "int_inc_a_plus_X": LFRicIntIncAPlusXKern,
    # Subtracting integer fields
    "int_X_minus_Y": LFRicIntXMinusYKern,
    "int_inc_X_minus_Y": LFRicIntIncXMinusYKern,
    # Multiplying (scaled) real fields
    "int_X_times_Y": LFRicIntXTimesYKern,
    "int_inc_X_times_Y": LFRicIntIncXTimesYKern,
    # Multiplying integer fields by an integer scalar (scaling fields)
    "int_a_times_X": LFRicIntATimesXKern,
    "int_inc_a_times_X": LFRicIntIncATimesXKern,
    # Setting an integer field elements to an integer scalar
    # or other integer field's values
    "int_setval_c": LFRicIntSetvalCKern,
    "int_setval_X": LFRicIntSetvalXKern,
    # Sign of integer field elements applied to a scalar value
    "int_sign_X": LFRicIntSignXKern,
    # Converting integer to real field elements
    "real_X": LFRicRealXKern}

# Built-in map dictionary for all built-ins
BUILTIN_MAP_CAPITALISED = REAL_BUILTIN_MAP_CAPITALISED
BUILTIN_MAP_CAPITALISED.update(INT_BUILTIN_MAP_CAPITALISED)

# Built-in map dictionary in lowercase keys for invoke generation and
# comparison purposes. This does not enforce case sensitivity to Fortran
# built-in names.
BUILTIN_MAP = get_lowercase_builtin_map(BUILTIN_MAP_CAPITALISED)
