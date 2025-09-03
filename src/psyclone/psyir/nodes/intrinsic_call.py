# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2025, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Lab
# Modified: R. W. Ford, STFC Daresbury Lab
# Modified: S. Siso, STFC Daresbury Lab
# Modified: A. B. G. Chalk, STFC Daresbury Lab
# -----------------------------------------------------------------------------

"""This module contains the IntrinsicCall node implementation."""

from collections import namedtuple
from collections.abc import Iterable
from enum import Enum
from typing import Callable

from psyclone.core import AccessType, VariablesAccessMap
from psyclone.psyir.nodes.operation import BinaryOperation
from psyclone.psyir.nodes.call import Call
from psyclone.psyir.nodes.datanode import DataNode
from psyclone.psyir.nodes.literal import Literal
from psyclone.psyir.nodes.reference import Reference
from psyclone.psyir.symbols import IntrinsicSymbol
from psyclone.psyir.symbols.datatypes import (
    CHARACTER_TYPE,
    BOOLEAN_TYPE,
    INTEGER_TYPE,
    REAL_DOUBLE_TYPE,
    REAL8_TYPE,
    REAL_TYPE,
    DataType,
    ArrayType,
    ScalarType,
    UnresolvedType,
    NoType,
)
from psyclone.psyir.symbols.datasymbol import DataSymbol

# pylint: disable=too-many-branches

# Named tuple for describing the attributes of each intrinsic
IAttr = namedtuple(
    "IAttr",
    "name is_pure is_elemental is_inquiry required_args optional_args"
    " return_type reference_accesses",
)
# Alternatively we could use an Enum to decrive the intrinsic types
# IntrinsicType = Enum('IntrinsicType',
#    'Atomic Collective Elemental Inquiry Pure Impure Transformational'
# )
# And let the IntrinsicCall is_pure, is_elemental, ... do the conversion

# Named tuple for describing the properties of the required arguments to
# a particular intrinsic. If there's no limit on the number of arguments
# then `max_count` will be None.
ArgDesc = namedtuple("ArgDesc", "min_count max_count types")


def _get_first_argument_type(node) -> DataType:
    """Helper function for the common IntrinsicCall case where
    the return type matches exactly the datatype of the first argument.

    :param node: The IntrinsicCall whose return type to compute.
    :type node: :py:class:`psyclone.psyir.nodes.IntrinsicCall`

    :returns: the datatype of the first argument of the IntrinsicCall.
    """
    return node.arguments[0].datatype


def _get_first_argument_type_with_optional_kind(node) -> DataType:
    """Helper function for the common IntrinsicCall case where the
    return type is the Intrinsic of the first argument, with an optional
    kind parameter which may override the precision.

    :param node: The IntrinsicCall whose return type to compute.
    :type node: :py:class:`psyclone.psyir.nodes.IntrinsicCall`

    :returns: the datatype of the first argument of the IntrinsicCall.
    """
    if "kind" not in node.argument_names:
        return node.arguments[0].datatype
    else:
        kind = node.arguments[node.argument_names.index("kind")]
        return_type = node.arguments[0].datatype.copy()
        return_type._precision = kind
        return return_type


def _get_first_argument_intrinsic_with_optional_kind_and_dim(node) -> DataType:
    """Helper function for IntrinsicCalls like MAXLOC where they have optional
    Kind and Dim options but the intrinsic is that of the first argument.

    :param node: The IntrinsicCall whose return type to compute.
    :type node: :py:class:`psyclone.psyir.nodes.IntrinsicCall`

    :returns: the computed datatype for the IntrinsicCall.
    """
    if "kind" in node.argument_names:
        dtype = ScalarType(
            node.arguments[0].datatype.datatype.intrinsic,
            node.arguments[node.argument_names.index("kind")],
        )
    else:
        # PSyclone has the UNDEFINED Precision as the default kind for all
        # supported inbuilt datatypes.
        dtype = ScalarType(
            node.arguments[0].datatype.intrinsic,
            ScalarType.Precision.UNDEFINED,
        )
    if "dim" not in node.argument_names:
        return ArrayType(
            dtype,
            [
                ArrayType.ArrayBounds(
                    Literal("1", INTEGER_TYPE),
                    Literal(
                        str(len(node.arguments[0].datatype.shape)),
                        INTEGER_TYPE,
                    ),
                )
            ],
        )
    # Always have dim from here.
    # If array has rank 1, the result is scalar.
    arg = node.arguments[0]
    shape = arg.datatype.shape
    if len(shape) == 1:
        return dtype
    # For now we don't attempt to work out the shape.
    new_shape = [ArrayType.Extent.DEFERRED] * (len(shape) - 1)
    return ArrayType(dtype, new_shape)


def _get_first_argument_specified_kind_with_optional_dim(
        node, intrinsic: ScalarType.Intrinsic = ScalarType.Intrinsic.BOOLEAN
        ) -> DataType:
    """Helper function for the common IntrinsicCall case where the
    return type is a Scalar with the kind of the first argument,
    unless an option dim parameter is given in which case an array with
    rank is given instead.

    :param node: The IntrinsicCall whose return type to compute.
    :type node: :py:class:`psyclone.psyir.nodes.IntrinsicCall`
    :param intrinsic: The type of the intrinsic of the resulting datatype.
                      Default is ScalarType.Intrinsic.BOOLEAN

    :returns: the computed datatype for the IntrinsicCall.
    """
    dtype = ScalarType(
        intrinsic, node.arguments[0].datatype.precision
    )
    if "dim" not in node.argument_names:
        return dtype
    else:
        # If dim is given then this should return an array, but we
        # don't necessarily know the dimensions of the resulting array
        # at compile time. It will have one fewer dimension than the
        # input.
        arg = node.arguments[0]
        shape = arg.datatype.shape
        if len(shape) == 1:
            return dtype
        # For now we don't attempt to work out the shape.
        new_shape = [ArrayType.Extent.DEFERRED] * (len(shape) - 1)
        return ArrayType(dtype, new_shape)


def _get_integer_with_optional_kind(node) -> DataType:
    """Helper function for the common case where the return type is a
    Scalar integer with an optional kind argument.

    :param node: The IntrinsicCall whose return type to compute.
    :type node: :py:class:`psyclone.psyir.nodes.IntrinsicCall`

    :returns: the computed datatype for the IntrinsicCall.
    """
    return (
        ScalarType(
            ScalarType.Intrinsic.INTEGER,
            node.arguments[node.argument_names.index("kind")],
        )
        if "kind" in node.argument_names
        else INTEGER_TYPE
    )


def _get_integer_of_kind_with_optional_dim(node) -> DataType:
    """Helper function for a type of Integer with optional dim and
    kind options.

    :param node: The IntrinsicCall whose return type to compute.
    :type node: :py:class:`psyclone.psyir.nodes.IntrinsicCall`

    :returns: the computed datatype for the IntrinsicCall.
    """
    dtype = ScalarType(
        ScalarType.Intrinsic.INTEGER,
        (
            ScalarType.Precision.UNDEFINED
            if "kind" not in node.argument_names
            else node.arguments[node.argument_names.index("kind")]
        ),
    )
    if "dim" not in node.argument_names:
        return dtype
    else:
        # If dim is given then this should return an array, but we
        # don't necessarily know the dimensions of the resulting array
        # at compile time. It will have one fewer dimension than the
        # input.
        arg = node.arguments[0]
        shape = arg.datatype.shape
        if len(shape) == 1:
            return dtype
        else:
            # For now we don't attempt to work out the shape.
            new_shape = [ArrayType.Extent.DEFERRED] * (len(shape) - 1)
            return ArrayType(dtype, new_shape)


def _get_real_with_argone_kind(node) -> DataType:
    """Helper function for the common IntrinsicCall case where the
    return type is a Scalar REAL with the kind of the first argument.

    :param node: The IntrinsicCall whose return type to compute.
    :type node: :py:class:`psyclone.psyir.nodes.IntrinsicCall`

    :returns: the computed datatype for the IntrinsicCall.
    """
    return ScalarType(
        ScalarType.Intrinsic.REAL, node.arguments[0].datatype.precision
    )


def _get_real_with_x_kind(node) -> DataType:
    """Helper function for the BESSEL_.N cases, where the return type is
    a Scalar REAL with the kind of the X argument (the final argument).

    :param node: The IntrinsicCall whose return type to compute.
    :type node: :py:class:`psyclone.psyir.nodes.IntrinsicCall`

    :returns: the computed datatype for the IntrinsicCall.
    """
    return ScalarType(
        ScalarType.Intrinsic.REAL, node.arguments[-1].datatype.precision
    )


def _findloc_return_type(node) -> DataType:
    """Helper function for the FINDLOC case.

    :param node: The IntrinsicCall whose return type to compute.
    :type node: :py:class:`psyclone.psyir.nodes.IntrinsicCall`

    :returns: the computed datatype for the IntrinsicCall.
    """
    if "kind" in node.argument_names:
        dtype = ScalarType(
            node.arguments[0].intrinsic,
            node.arguments[node.argument_names.index("kind")],
        )
    else:
        dtype = node.arguments[0].datatype.copy()
    if "dim" in node.argument_names:
        if len(node.arguments[0].datatype.shape) == 1:
            return dtype
        else:
            # We can't get the sizes correct since we don't know
            # dim, so use deferred.
            return ArrayType(
                dtype,
                [ArrayType.Extent.DEFERRED]
                * (len(node.arguments[0].datatype.shape) - 1),
            )
    else:
        return ArrayType(
            dtype,
            [
                ArrayType.ArrayBounds(
                    Literal("1", INTEGER_TYPE),
                    Literal(
                        str(len(node.arguments[0].datatype.shape)),
                        INTEGER_TYPE,
                    ),
                )
            ],
        )


def _int_return_type(node) -> DataType:
    """Helper function for the INT case.

    :param node: The IntrinsicCall whose return type to compute.
    :type node: :py:class:`psyclone.psyir.nodes.IntrinsicCall`

    :returns: the computed datatype for the IntrinsicCall.
    """
    if "kind" in node.argument_names:
        dtype = ScalarType(
            ScalarType.Intrinsic.INTEGER,
            node.arguments[node.argument_names.index("kind")],
        )
    else:
        dtype = INTEGER_TYPE

    if isinstance(node.arguments[0].datatype, ArrayType):
        return ArrayType(
            dtype,
            [
                (
                    index.copy()
                    if not isinstance(index, ArrayType.ArrayBounds)
                    else ArrayType.ArrayBounds(index.lower, index.upper)
                )
                for index in node.arguments[0].datatype.shape
            ],
        )
    else:
        return dtype


def _iparity_return_type(node) -> DataType:
    """Helper function for the IPARITY case.

    :param node: The IntrinsicCall whose return type to compute.
    :type node: :py:class:`psyclone.psyir.nodes.IntrinsicCall`

    :returns: the computed datatype for the IntrinsicCall.
    """
    dtype = ScalarType(
        node.arguments[0].datatype.intrinsic,
        node.arguments[0].datatype.precision,
    )
    if len(node.arguments) == 1 or (
        len(node.arguments) == 2 and "mask" in node.argument_names
    ):
        return dtype
    # We have a dimension specified. We don't know the resultant shape
    # in any detail as its dependent on the value of dim
    return ArrayType(
        dtype,
        [ArrayType.Extent.DEFERRED]
        * (len(node.arguments[0].datatype.shape) - 1),
    )


def _get_bound_function_return_type(node) -> DataType:
    """Helper function for the return types of functions like LBOUND and
    LCOBOUND etc.

    :param node: The IntrinsicCall whose return type to compute.
    :type node: :py:class:`psyclone.psyir.nodes.IntrinsicCall`

    :returns: the computed datatype for the IntrinsicCall.
    """
    if "kind" in node.argument_names:
        dtype = ScalarType(
            ScalarType.Intrinsic.INTEGER,
            node.arguments[node.argument_names.index("kind")],
        )
    else:
        dtype = INTEGER_TYPE
    if "dim" in node.argument_names:
        return dtype
    return ArrayType(
        dtype,
        [
            ArrayType.ArrayBounds(
                Literal("1", INTEGER_TYPE),
                Literal(
                    str(len(node.arguments[0].datatype.shape)), INTEGER_TYPE
                ),
            )
        ],
    )


def _matmul_return_type(node) -> DataType:
    """Helper function for the return type of MATMUL.

    :param node: The IntrinsicCall whose return type to compute.
    :type node: :py:class:`psyclone.psyir.nodes.IntrinsicCall`

    :returns: the computed datatype for the IntrinsicCall.
    """
    argtype1 = node.arguments[0].datatype
    argtype2 = node.arguments[1].datatype
    shape1 = argtype1.shape
    shape2 = argtype2.shape
    stype1 = ScalarType(argtype1.intrinsic, argtype1.precision)
    stype2 = ScalarType(argtype2.intrinsic, argtype2.precision)
    # Create a temporary BinaryOperation to use get_result_scalar_type
    arg1 = Reference(DataSymbol("a", stype1))
    arg2 = Reference(DataSymbol("b", stype2))
    binop = BinaryOperation.create(BinaryOperation.Operator.MUL,
                                   arg1, arg2)
    # TODO - make this a public method?
    stype = binop._get_result_scalar_type([stype1, stype2])
    #  a11 a12 x b1 = a11*b1 + a12*b2
    #  a21 a22   b2   a21*b1 + a22*b2
    #  a31 a32        a31*b1 + a32*b2
    #  3 x 2 * 2 x 1 = 3 x 1
    #  rank 2  rank 1  rank 1
    if len(shape1) == 1:
        extent = IntrinsicCall.create(
            IntrinsicCall.Intrinsic.SIZE,
            [node.arguments[1].copy(),
             ("dim", Literal("1", INTEGER_TYPE))])
        shape = [extent]
    elif len(shape2) == 1:
        extent = IntrinsicCall.create(
            IntrinsicCall.Intrinsic.SIZE,
            [node.arguments[0].copy(),
             ("dim", Literal("1", INTEGER_TYPE))])
        shape = [extent]
    else:
        # matrix-matrix. Result is size(arg0, 1) x size(arg1, 2)
        extent1 = IntrinsicCall.create(
            IntrinsicCall.Intrinsic.SIZE,
            [node.arguments[0].copy(),
             ("dim", Literal("1", INTEGER_TYPE))])
        extent2 = IntrinsicCall.create(
            IntrinsicCall.Intrinsic.SIZE,
            [node.arguments[1].copy(),
             ("dim", Literal("2", INTEGER_TYPE))])
        shape = [extent1, extent2]
    return ArrayType(stype, shape)


def _maxval_return_type(node) -> DataType:
    """ Helper function for the MAXVAL (and similar) intrinsic return
    types.

    :param node: The IntrinsicCall whose return type to compute.
    :type node: :py:class:`psyclone.psyir.nodes.IntrinsicCall`

    :returns: the computed datatype for the IntrinsicCall.
    """
    dtype = ScalarType(node.arguments[0].datatype.intrinsic,
                       node.arguments[0].datatype.precision)
    if ("dim" not in node.argument_names
            or len(node.arguments[0].datatype.shape) == 1):
        return dtype
    # We have a dimension specified. We don't know the resultant shape
    # in any detail as its dependent on the value of dim
    return ArrayType(
        dtype,
        [ArrayType.Extent.DEFERRED]
        * (len(node.arguments[0].datatype.shape) - 1),
    )


def _reduce_return_type(node) -> DataType:
    """ Helper function for the REDUCE intrinsic return type.

    :param node: The IntrinsicCall whose return type to compute.
    :type node: :py:class:`psyclone.psyir.nodes.IntrinsicCall`

    :returns: the computed datatype for the IntrinsicCall.
    """
    # Check if we have dim
    have_dim = (len(node.arguments) > 2 and
                node.argument_names[2] is None)
    dtype = ScalarType(node.arguments[0].datatype.intrinsic,
                       node.arguments[0].datatype.precision)
    if not have_dim:
        return dtype
    if len(node.arguments[0].datatype.shape) == 1:
        return dtype
    else:
        # We can't get the sizes correct since we don't know
        # dim, so use deferred.
        return ArrayType(
            dtype,
            [ArrayType.Extent.DEFERRED]
            * (len(node.arguments[0].datatype.shape) - 1),
        )


class IntrinsicCall(Call):
    """Node representing a call to an intrinsic routine (function or
    subroutine). This can be found as a standalone statement
    or an expression.

    :param intrinsic: the type of Intrinsic being created.
    :type intrinsic: py:class:`psyclone.psyir.IntrinsicCall.Intrinsic`
    :param kwargs: additional keyword arguments provided to the PSyIR node.
    :type kwargs: unwrapped dict.

    :raises TypeError: if the 'intrinsic' argument is not an Intrinsic type.

    """

    # Textual description of the node.
    _children_valid_format = "[DataNode]*"
    _text_name = "IntrinsicCall"
    _colour = "cyan"

    #: The type of Symbol this Call must refer to. Used for type checking in
    #: the constructor (of the parent class).
    _symbol_type = IntrinsicSymbol

    class Intrinsic(IAttr, Enum):
        """Enum of all intrinsics with their attributes as values using the
        IAttr namedtuple format:

            NAME = IAttr(name, is_pure, is_elemental, is_inquiry,
                         required_args, optional_args, return_type,
                         reference_accesses)

        Note that name is duplicated inside IAttr because each item in the
        Enum must have a different value, and without the name that would
        not be guaranteed.

        """

        # Fortran special-case statements (technically not Fortran intrinsics
        # but in PSyIR they are represented as Intrinsics)
        # TODO 3060 reference_accesses
        ALLOCATE = IAttr(
            "ALLOCATE",
            False,
            False,
            False,
            ArgDesc(1, None, Reference),
            {
                "mold": Reference,
                "source": Reference,
                "stat": Reference,
                "errmsg": Reference,
            },
            None,
            None,
        )
        DEALLOCATE = IAttr(
            "DEALLOCATE",
            False,
            False,
            False,
            ArgDesc(1, None, Reference),
            {"stat": Reference},
            None,
            None,
        )
        NULLIFY = IAttr(
            "NULLIFY",
            False,
            False,
            False,
            ArgDesc(1, None, Reference),
            {},
            None,
            None,
        )

        # Fortran Intrinsics (from Fortran 2018 standard table 16.1)
        ABS = IAttr(
            "ABS",
            True,
            True,
            False,
            ArgDesc(1, 1, DataNode),
            {},
            # TODO 1590 Complex conversion unsupported.
            _get_first_argument_type,
            None,
        )
        ACHAR = IAttr(
            "ACHAR",
            True,
            True,
            False,
            ArgDesc(1, 1, DataNode),
            {"kind": DataNode},
            CHARACTER_TYPE,
            None,
        )
        ACOS = IAttr(
            "ACOS",
            True,
            True,
            False,
            ArgDesc(1, 1, DataNode),
            {},
            _get_first_argument_type,
            None,
        )
        ACOSH = IAttr(
            "ACOSH",
            True,
            True,
            False,
            ArgDesc(1, 1, DataNode),
            {},
            _get_first_argument_type,
            None,
        )
        ADJUSTL = IAttr(
            "ADJUSTL",
            True,
            True,
            False,
            ArgDesc(1, 1, DataNode),
            {},
            # TODO 2612 This may be more complex if we support character len
            _get_first_argument_type,
            None,
        )
        ADJUSTR = IAttr(
            "ADJUSTR",
            True,
            True,
            False,
            ArgDesc(1, 1, DataNode),
            {},
            # TODO 2612 This may be more complex if we support character len
            _get_first_argument_type,
            None,
        )
        AIMAG = IAttr(
            "AIMAG",
            True,
            True,
            False,
            ArgDesc(1, 1, DataNode),
            {},
            # TODO #1590 Complex numbers' precision unsupported.
            lambda node: UnresolvedType(),
            None,
        )
        AINT = IAttr(
            "AINT",
            True,
            True,
            False,
            ArgDesc(1, 1, DataNode),
            {"kind": DataNode},
            lambda node: (
                ScalarType(
                    ScalarType.Intrinsic.REAL,
                    (
                        node.arguments[node.argument_names.index("kind")]
                        if "kind" in node.argument_names
                        else node.arguments[0].datatype.precision
                    ),
                )
            ),
            None,
        )
        ALL = IAttr(
            "ALL",
            True,
            False,
            False,
            ArgDesc(1, 1, DataNode),
            {"dim": DataNode},
            _get_first_argument_specified_kind_with_optional_dim,
            None,
        )
        ALLOCATED = IAttr(
            "ALLOCATED",
            True,
            False,
            True,
            ArgDesc(1, 1, DataNode),
            {},
            BOOLEAN_TYPE,
            None,
        )
        ANINT = IAttr(
            "ANINT",
            True,
            True,
            False,
            ArgDesc(1, 1, DataNode),
            {"kind": DataNode},
            lambda node: (
                ScalarType(
                    ScalarType.Intrinsic.REAL,
                    (
                        node.arguments[node.argument_names.index("kind")]
                        if "kind" not in node.argument_names
                        else node.arguments[0].datatype.precision
                    ),
                )
            ),
            None,
        )
        ANY = IAttr(
            "ANY",
            True,
            False,
            False,
            ArgDesc(1, 1, DataNode),
            {"dim": DataNode},
            _get_first_argument_specified_kind_with_optional_dim,
            None,
        )
        ASIN = IAttr(
            "ASIN",
            True,
            True,
            False,
            ArgDesc(1, 1, DataNode),
            {},
            _get_first_argument_type,
            None,
        )
        ASINH = IAttr(
            "ASINH",
            True,
            True,
            False,
            ArgDesc(1, 1, DataNode),
            {},
            _get_first_argument_type,
            None,
        )
        ASSOCIATED = IAttr(
            "ASSOCIATED",
            False,
            False,
            True,
            ArgDesc(1, 1, DataNode),
            {"target": DataNode},
            BOOLEAN_TYPE,
            None,
        )
        ATAN = IAttr(
            "ATAN",
            True,
            True,
            False,
            ArgDesc(1, 2, DataNode),
            {},
            # N. B. If this has 2 arguments then the return value
            # is the of the second argument, however the standard defines
            # the type and kind type of both arguments must be the same.
            _get_first_argument_type,
            None,
        )
        ATAN2 = IAttr(
            "ATAN2",
            True,
            True,
            False,
            ArgDesc(2, 2, DataNode),
            {},
            _get_first_argument_type,
            None,
        )
        ATANH = IAttr(
            "ATANH",
            True,
            True,
            False,
            ArgDesc(1, 1, DataNode),
            {},
            _get_first_argument_type,
            None,
        )
        ATOMIC_ADD = IAttr(
            "ATOMIC_ADD",
            True,
            True,
            False,
            ArgDesc(2, 2, DataNode),
            {"stat": DataNode},
            None,
            None,
        )
        ATOMIC_AND = IAttr(
            "ATOMIC_AND",
            True,
            True,
            False,
            ArgDesc(2, 2, DataNode),
            {"stat": DataNode},
            None,
            None,
        )
        ATOMIC_CAS = IAttr(
            "ATOMIC_CAS",
            True,
            True,
            False,
            ArgDesc(2, 2, DataNode),
            {"stat": DataNode},
            None,
            None,
        )
        ATOMIC_DEFINE = IAttr(
            "ATOMIC_DEFINE",
            True,
            True,
            False,
            ArgDesc(2, 2, DataNode),
            {"stat": DataNode},
            None,
            None,
        )
        ATOMIC_FETCH_ADD = IAttr(
            "ATOMIC_FETCH_ADD",
            True,
            True,
            False,
            ArgDesc(3, 3, DataNode),
            {"stat": DataNode},
            None,
            None,
        )
        ATOMIC_FETCH_AND = IAttr(
            "ATOMIC_FETCH_AND",
            True,
            True,
            False,
            ArgDesc(3, 3, DataNode),
            {"stat": DataNode},
            None,
            None,
        )
        ATOMIC_FETCH_OR = IAttr(
            "ATOMIC_FETCH_OR",
            True,
            True,
            False,
            ArgDesc(3, 3, DataNode),
            {"stat": DataNode},
            None,
            None,
        )
        ATOMIC_FETCH_XOR = IAttr(
            "ATOMIC_FETCH_XOR",
            True,
            True,
            False,
            ArgDesc(3, 3, DataNode),
            {"stat": DataNode},
            None,
            None,
        )
        ATOMIC_OR = IAttr(
            "ATOMIC_OR",
            True,
            True,
            False,
            ArgDesc(2, 2, DataNode),
            {"stat": DataNode},
            None,
            None,
        )
        ATOMIC_REF = IAttr(
            "ATOMIC_REF",
            True,
            True,
            False,
            ArgDesc(2, 2, DataNode),
            {"stat": DataNode},
            None,
            None,
        )
        ATOMIC_XOR = IAttr(
            "ATOMIC_XOR",
            True,
            True,
            False,
            ArgDesc(2, 2, DataNode),
            {"stat": DataNode},
            None,
            None,
        )
        BESSEL_J0 = IAttr(
            "BESSEL_J0",
            True,
            True,
            False,
            ArgDesc(1, 1, DataNode),
            {},
            _get_real_with_argone_kind,
            None,
        )
        BESSEL_J1 = IAttr(
            "BESSEL_J1",
            True,
            True,
            False,
            ArgDesc(1, 1, DataNode),
            {},
            _get_real_with_argone_kind,
            None,
        )
        BESSEL_JN = IAttr(
            "BESSEL_JN",
            True,
            None,
            False,
            ArgDesc(2, 3, DataNode),
            {},
            _get_real_with_x_kind,
            None,
        )
        BESSEL_Y0 = IAttr(
            "BESSEL_Y0",
            True,
            True,
            False,
            ArgDesc(1, 1, DataNode),
            {},
            _get_real_with_argone_kind,
            None,
        )
        BESSEL_Y1 = IAttr(
            "BESSEL_Y1",
            True,
            True,
            False,
            ArgDesc(1, 1, DataNode),
            {},
            _get_real_with_argone_kind,
            None,
        )
        BESSEL_YN = IAttr(
            "BESSEL_YN",
            True,
            None,
            False,
            ArgDesc(2, 3, DataNode),
            {},
            _get_real_with_x_kind,
            None,
        )
        BGE = IAttr(
            "BGE",
            True,
            True,
            False,
            ArgDesc(2, 2, DataNode),
            {},
            BOOLEAN_TYPE,
            None,
        )
        BGT = IAttr(
            "BGT",
            True,
            True,
            False,
            ArgDesc(2, 2, DataNode),
            {},
            BOOLEAN_TYPE,
            None,
        )
        BIT_SIZE = IAttr(
            "BIT_SIZE",
            True,
            False,
            True,
            ArgDesc(1, 1, DataNode),
            {},
            INTEGER_TYPE,
            None,
        )
        BLE = IAttr(
            "BLE",
            True,
            True,
            False,
            ArgDesc(2, 2, DataNode),
            {},
            BOOLEAN_TYPE,
            None,
        )
        BLT = IAttr(
            "BLT",
            True,
            True,
            False,
            ArgDesc(2, 2, DataNode),
            {},
            BOOLEAN_TYPE,
            None,
        )
        BTEST = IAttr(
            "BTEST",
            True,
            True,
            False,
            ArgDesc(2, 2, DataNode),
            {},
            BOOLEAN_TYPE,
            None,
        )
        CEILING = IAttr(
            "CEILING",
            True,
            True,
            False,
            ArgDesc(1, 1, DataNode),
            {"kind": DataNode},
            _get_integer_with_optional_kind,
            None,
        )
        CHAR = IAttr(
            "CHAR",
            True,
            True,
            False,
            ArgDesc(1, 1, DataNode),
            {"kind": DataNode},
            CHARACTER_TYPE,
            None,
        )
        CMPLX = IAttr(
            "CMPLX",
            True,
            True,
            False,
            ArgDesc(1, 1, DataNode),
            {"Y": DataNode, "kind": DataNode},
            # TODO #1590 Complex numbers unsupported.
            lambda node: UnresolvedType(),
            None,
        )
        CO_BROADCAST = IAttr(
            "CO_BROADCAST",
            True,
            False,
            False,
            ArgDesc(1, 2, DataNode),
            {"stat": DataNode, "errmsg": DataNode},
            None,
            None,
        )
        CO_MAX = IAttr(
            "CO_MAX",
            True,
            False,
            False,
            ArgDesc(1, 1, DataNode),
            {"result_image": DataNode, "stat": DataNode, "errmsg": DataNode},
            None,
            None,
        )
        CO_MIN = IAttr(
            "CO_MIN",
            True,
            False,
            False,
            ArgDesc(1, 1, DataNode),
            {"result_image": DataNode, "stat": DataNode, "errmsg": DataNode},
            None,
            None,
        )
        CO_REDUCE = IAttr(
            "CO_REDUCE",
            True,
            False,
            False,
            ArgDesc(1, 2, DataNode),
            {"result_image": DataNode, "stat": DataNode, "errmsg": DataNode},
            None,
            None,
        )
        CO_SUM = IAttr(
            "CO_SUM",
            True,
            False,
            False,
            ArgDesc(1, 1, DataNode),
            {"result_image": DataNode, "stat": DataNode, "errmsg": DataNode},
            None,
            None,
        )
        COMMAND_ARGUMENT_COUNT = IAttr(
            "COMMAND_ARGUMENT_COUNT",
            True,
            False,
            False,
            ArgDesc(0, 0, None),
            {},
            INTEGER_TYPE,
            None,
        )
        CONJG = IAttr(
            "CONJG",
            True,
            True,
            False,
            ArgDesc(1, 1, DataNode),
            {},
            # TODO #1590 Complex numbers unsupported.
            lambda node: UnresolvedType(),
            None,
        )
        COS = IAttr(
            "COS",
            True,
            True,
            False,
            ArgDesc(1, 1, DataNode),
            {},
            _get_first_argument_type,
            None,
        )
        COSH = IAttr(
            "COSH",
            True,
            True,
            False,
            ArgDesc(1, 1, DataNode),
            {},
            _get_first_argument_type,
            None,
        )
        COSHAPE = IAttr(
            "COSHAPE",
            True,
            False,
            True,
            ArgDesc(1, 1, DataNode),
            {"kind": DataNode},
            # FIXME Return type
            lambda node: ArrayType(
                ScalarType(
                    ScalarType.Intrinsic.INTEGER,
                    (
                        node.arguments[0].datatype.precision
                        if "kind" not in node.argument_names
                        else node.arguments[node.argument_names.index("kind")]
                    ),
                ),
                [
                    (
                        index.copy()
                        if not isinstance(index, ArrayType.ArrayBounds)
                        else ArrayType.ArrayBounds(index.lower, index.upper)
                    )
                    for index in node.arguments[0].datatype.shape
                ],
            ),
            None,
        )
        COUNT = IAttr(
            "COUNT",
            True,
            False,
            False,
            ArgDesc(1, 1, DataNode),
            {"dim": DataNode, "kind": DataNode},
            _get_integer_of_kind_with_optional_dim,
            None,
        )
        CPU_TIME = IAttr(
            "CPU_TIME",
            False,
            False,
            False,
            ArgDesc(1, 1, DataNode),
            {},
            None,
            None,
        )
        CSHIFT = IAttr(
            "CSHIFT",
            True,
            False,
            False,
            ArgDesc(2, 2, DataNode),
            {"dim": DataNode},
            _get_first_argument_type,  # FIXME Wait on Sergi reply
            None,
        )
        DATE_AND_TIME = IAttr(
            "DATE_AND_TIME",
            False,
            False,
            False,
            ArgDesc(0, 0, DataNode),
            {
                "date": DataNode,
                "time": DataNode,
                "zone": DataNode,
                "values": DataNode,
            },
            None,
            None,
        )
        DBLE = IAttr(
            "DBLE",
            True,
            True,
            False,
            ArgDesc(1, 1, DataNode),
            {},
            REAL_DOUBLE_TYPE,
            None,
        )
        DIGITS = IAttr(
            "DIGITS",
            True,
            False,
            True,
            ArgDesc(1, 1, DataNode),
            {},
            INTEGER_TYPE,
            None,
        )
        DIM = IAttr(
            "DIM",
            True,
            True,
            False,
            ArgDesc(2, 2, DataNode),
            {},
            _get_first_argument_type,
            None,
        )
        DOT_PRODUCT = IAttr(
            "DOT_PRODUCT",
            True,
            False,
            False,
            ArgDesc(2, 2, DataNode),
            {},
            lambda node: ScalarType(
                node.arguments[0].datatype.intrinsic,
                node.arguments[0].datatype.precision,
            ),
            None,
        )
        DPROD = IAttr(
            "DPROD",
            True,
            True,
            False,
            ArgDesc(2, 2, DataNode),
            {},
            REAL8_TYPE,
            None,
        )
        DSHIFTL = IAttr(
            "DSHIFTL",
            True,
            True,
            False,
            ArgDesc(3, 3, DataNode),
            {},
            lambda node: (
                node.arguments[0].datatype.copy()
                if not isinstance(node.arguments[0], Literal)
                else node.arguments[1].datatype.copy()
            ),
            None,
        )
        DSHIFTR = IAttr(
            "DSHIFTR",
            True,
            True,
            False,
            ArgDesc(3, 3, DataNode),
            {},
            lambda node: (
                node.arguments[0].datatype.copy()
                if not isinstance(node.arguments[0], Literal)
                else node.arguments[1].datatype.copy()
            ),
            None,
        )
        EOSHIFT = IAttr(
            "EOSHIFT",
            True,
            False,
            False,
            ArgDesc(2, 2, DataNode),
            {"boundary": DataNode, "dim": DataNode},
            _get_first_argument_type,  # FIXME Wait for Sergi reply.
            None,
        )
        EPSILON = IAttr(
            "EPSILON",
            True,
            False,
            True,
            ArgDesc(1, 1, DataNode),
            {},
            _get_first_argument_type,
            None,
        )
        ERF = IAttr(
            "ERF",
            True,
            True,
            False,
            ArgDesc(1, 1, DataNode),
            {},
            _get_real_with_argone_kind,
            None,
        )
        ERFC = IAttr(
            "ERFC",
            True,
            True,
            False,
            ArgDesc(1, 1, DataNode),
            {},
            _get_real_with_argone_kind,
            None,
        )
        ERFC_SCALED = IAttr(
            "ERFC_SCALED",
            True,
            True,
            False,
            ArgDesc(1, 1, DataNode),
            {},
            _get_real_with_argone_kind,
            None,
        )
        EVENT_QUERY = IAttr(
            "EVENT_QUERY",
            False,
            False,
            False,
            ArgDesc(2, 2, DataNode),
            {"stat": DataNode},
            None,
            None,
        )
        EXECUTE_COMMAND_LINE = IAttr(
            "EXECUTE_COMMAND_LINE",
            False,
            False,
            False,
            ArgDesc(2, 2, DataNode),
            {
                "wait": DataNode,
                "exitstat": DataNode,
                "cmdstat": DataNode,
                "cmdmsg": DataNode,
            },
            None,
            None,
        )
        EXP = IAttr(
            "EXP",
            True,
            True,
            False,
            ArgDesc(1, 1, DataNode),
            {},
            _get_first_argument_type,
            None,
        )
        EXPONENT = IAttr(
            "EXPONENT",
            True,
            True,
            False,
            ArgDesc(1, 1, DataNode),
            {},
            INTEGER_TYPE,
            None,
        )
        EXTENDS_TYPE_OF = IAttr(
            "EXTENDS_TYPE_OF",
            True,
            False,
            True,
            ArgDesc(2, 2, DataNode),
            {},
            BOOLEAN_TYPE,
            None,
        )
        FAILED_IMAGES = IAttr(
            "FAILED_IMAGES",
            False,
            False,
            False,
            ArgDesc(0, 0, DataNode),
            {"team": DataNode, "kind": DataNode},
            lambda node: ArrayType(
                ScalarType(
                    ScalarType.Intrinsic.INTEGER,
                    (
                        node.arguments[node.argument_names.index("kind")]
                        if "kind" in node.argument_names
                        else ScalarType.Precision.UNDEFINED
                    ),
                ),
                [ArrayType.Extent.DEFERRED],
            ),
            None,
        )
        FINDLOC = IAttr(
            "FINDLOC",
            True,
            False,
            False,
            ArgDesc(2, 3, DataNode),
            {"mask": DataNode, "kind": DataNode, "back": DataNode},
            _findloc_return_type,
            None,
        )
        FLOAT = IAttr(
            "FLOAT",
            True,
            True,
            False,
            ArgDesc(1, 1, DataNode),
            {},
            REAL_TYPE,
            None,
        )
        FLOOR = IAttr(
            "FLOOR",
            True,
            True,
            False,
            ArgDesc(1, 1, DataNode),
            {"kind": DataNode},
            _get_integer_with_optional_kind,
            None,
        )
        FRACTION = IAttr(
            "FRACTION",
            True,
            True,
            False,
            ArgDesc(1, 1, DataNode),
            {},
            _get_first_argument_type,
            None,
        )
        GAMMA = IAttr(
            "GAMMA",
            True,
            True,
            False,
            ArgDesc(1, 1, DataNode),
            {},
            _get_first_argument_type,
            None,
        )
        GET_COMMAND = IAttr(
            "GET_COMMAND",
            False,
            False,
            False,
            ArgDesc(0, 0, DataNode),
            {
                "command": DataNode,
                "length": DataNode,
                "status": DataNode,
                "errmsg": DataNode,
            },
            None,
            None,
        )
        GET_COMMAND_ARGUMENT = IAttr(
            "GET_COMMAND_ARGUMENT",
            False,
            False,
            False,
            ArgDesc(1, 1, DataNode),
            {
                "value": DataNode,
                "length": DataNode,
                "status": DataNode,
                "errmsg": DataNode,
            },
            None,
            None,
        )
        GET_ENVIRONMENT_VARIABLE = IAttr(
            "GET_ENVIRONMENT_VARIABLE",
            False,
            False,
            False,
            ArgDesc(1, 1, DataNode),
            {
                "value": DataNode,
                "length": DataNode,
                "status": DataNode,
                "trim_name": DataNode,
                "errmsg": DataNode,
            },
            None,
            None,
        )
        GET_TEAM = IAttr(
            "GET_TEAM",
            True,
            False,
            False,
            ArgDesc(0, 0, DataNode),
            {"level": DataNode},
            # Unsupported return type (TEAM_TYPE from ISO_FORTRAN_ENV).
            lambda node: UnresolvedType(),
            None,
        )
        HUGE = IAttr(
            "HUGE",
            True,
            False,
            True,
            ArgDesc(1, 1, (Reference, Literal)),
            {},
            _get_first_argument_type,
            None,
        )
        HYPOT = IAttr(
            "HYPOT",
            True,
            True,
            False,
            ArgDesc(2, 2, (DataNode)),
            {},
            _get_first_argument_type,
            None,
        )
        IACHAR = IAttr(
            "IACHAR",
            True,
            True,
            False,
            ArgDesc(1, 1, (DataNode)),
            {"kind": DataNode},
            _get_integer_with_optional_kind,
            None,
        )
        IALL = IAttr(
            "IALL",
            True,
            False,
            False,
            # FIXME Note to reviewer I think this should be
            # ArgDesc(1, 2, (DataNode)), {"mask": DataNode}
            # See https://gcc.gnu.org/onlinedocs/gfortran/IALL.html
            # If this changes and "dim" is no longer a named argument, the
            # return type function will not be correct.
            ArgDesc(1, 1, (DataNode)),
            {"dim": DataNode, "mask": DataNode},
            # There is no kind, but this call will work.
            _get_integer_of_kind_with_optional_dim,
            None,
        )
        IAND = IAttr(
            "IAND",
            True,
            True,
            False,
            ArgDesc(2, 2, (DataNode)),
            {},
            lambda node: (
                node.arguments[0].datatype.copy()
                if not isinstance(node.arguments[0], Literal)
                else node.arguments[1].datatype.copy()
            ),
            None,
        )
        IANY = IAttr(
            "IANY",
            True,
            False,
            False,
            # FIXME Note to reviewer I think this should be
            # ArgDesc(1, 2, (DataNode)), {"mask": DataNode}
            # See https://gcc.gnu.org/onlinedocs/gfortran/IANY.html
            # If this changes and "dim" is no longer a named argument, the
            # return type function will not be correct.
            ArgDesc(1, 1, (DataNode)),
            {"dim": DataNode, "mask": DataNode},
            # There is no kind, but this call will work.
            _get_integer_of_kind_with_optional_dim,
            None,
        )
        IBCLR = IAttr(
            "IBCLR",
            True,
            True,
            False,
            ArgDesc(2, 2, (DataNode)),
            {},
            _get_first_argument_type,
            None,
        )
        IBITS = IAttr(
            "IBITS",
            True,
            True,
            False,
            ArgDesc(3, 3, (DataNode)),
            {},
            _get_first_argument_type,
            None,
        )
        IBSET = IAttr(
            "IBSET",
            True,
            True,
            False,
            ArgDesc(2, 2, (DataNode)),
            {},
            _get_first_argument_type,
            None,
        )
        ICHAR = IAttr(
            "ICHAR",
            True,
            True,
            False,
            ArgDesc(1, 1, (DataNode)),
            {"kind": DataNode},
            _get_integer_with_optional_kind,
            None,
        )
        IEOR = IAttr(
            "IEOR",
            True,
            True,
            False,
            ArgDesc(2, 2, (DataNode)),
            {},
            lambda node: (
                node.arguments[0].datatype.copy()
                if not isinstance(node.arguments[0], Literal)
                else node.arguments[1].datatype.copy()
            ),
            None,
        )
        IMAGE_INDEX = IAttr(
            "IMAGE_INDEX",
            True,
            False,
            True,
            ArgDesc(2, 3, (DataNode)),
            {},
            INTEGER_TYPE,
            None,
        )
        IMAGE_STATUS = IAttr(
            "IMAGE_STATUS",
            True,
            False,
            False,
            ArgDesc(1, 1, (DataNode)),
            {"team": DataNode},
            INTEGER_TYPE,
            None,
        )
        INDEX = IAttr(
            "INDEX",
            True,
            True,
            False,
            ArgDesc(2, 2, (DataNode)),
            {"back": DataNode, "kind": DataNode},
            _get_integer_with_optional_kind,
            None,
        )
        INT = IAttr(
            "INT",
            True,
            True,
            False,
            ArgDesc(1, 1, (DataNode)),
            {"kind": DataNode},
            _int_return_type,
            None,
        )
        IOR = IAttr(
            "IOR",
            True,
            True,
            False,
            ArgDesc(2, 2, (DataNode)),
            {},
            lambda node: ScalarType(
                ScalarType.Intrinsic.INTEGER,
                (
                    node.arguments[0].datatype.precision
                    if not isinstance(node.arguments[0], Literal)
                    else node.arguments[1].datatype.precision
                ),
            ),
            None,
        )
        IPARITY = IAttr(
            "IPARITY",
            True,
            False,
            False,
            ArgDesc(1, 2, (DataNode)),
            {"mask": DataNode},
            _iparity_return_type,
            None,
        )
        IS_CONTIGUOUS = IAttr(
            "IS_CONTIGUOUS",
            True,
            False,
            True,
            ArgDesc(1, 1, (DataNode)),
            {},
            BOOLEAN_TYPE,
            None,
        )
        IS_IOSTAT_END = IAttr(
            "IS_IOSTAT_END",
            True,
            True,
            False,
            ArgDesc(1, 1, (DataNode)),
            {},
            BOOLEAN_TYPE,
            None,
        )
        IS_IOSTAT_EOR = IAttr(
            "IS_IOSTAT_EOR",
            True,
            True,
            False,
            ArgDesc(1, 1, (DataNode)),
            {},
            BOOLEAN_TYPE,
            None,
        )
        ISHFT = IAttr(
            "ISHFT",
            True,
            True,
            False,
            ArgDesc(2, 2, (DataNode)),
            {},
            _get_first_argument_type,
            None,
        )
        ISHFTC = IAttr(
            "ISHFTC",
            True,
            True,
            False,
            ArgDesc(2, 2, (DataNode)),
            {"size": DataNode},
            _get_first_argument_type,
            None,
        )
        KIND = IAttr(
            "KIND",
            True,
            False,
            True,
            ArgDesc(1, 1, (DataNode)),
            {},
            INTEGER_TYPE,
            None,
        )
        LBOUND = IAttr(
            "LBOUND",
            True,
            False,
            True,
            ArgDesc(1, 1, (DataNode)),
            {"dim": DataNode, "kind": DataNode},
            _get_bound_function_return_type,
            None,
        )
        LCOBOUND = IAttr(
            "LCOBOUND",
            True,
            False,
            True,
            ArgDesc(1, 1, (DataNode)),
            {"dim": DataNode, "kind": DataNode},
            _get_bound_function_return_type,
            None,
        )
        LEADZ = IAttr(
            "LEADZ",
            True,
            True,
            False,
            ArgDesc(1, 1, (DataNode)),
            {},
            INTEGER_TYPE,
            None,
        )
        LEN = IAttr(
            "LEN",
            True,
            False,
            True,
            ArgDesc(1, 1, (DataNode)),
            {"kind": DataNode},
            _get_integer_with_optional_kind,
            None,
        )
        LEN_TRIM = IAttr(
            "LEN_TRIM",
            True,
            True,
            False,
            ArgDesc(1, 1, (DataNode)),
            {"kind": DataNode},
            _get_integer_with_optional_kind,
            None,
        )
        LGE = IAttr(
            "LGE",
            True,
            True,
            False,
            ArgDesc(2, 2, (DataNode)),
            {},
            BOOLEAN_TYPE,
            None,
        )
        LGT = IAttr(
            "LGT",
            True,
            True,
            False,
            ArgDesc(2, 2, (DataNode)),
            {},
            BOOLEAN_TYPE,
            None,
        )
        LLE = IAttr(
            "LLE",
            True,
            True,
            False,
            ArgDesc(2, 2, (DataNode)),
            {},
            BOOLEAN_TYPE,
            None,
        )
        LLT = IAttr(
            "LLT",
            True,
            True,
            False,
            ArgDesc(2, 2, (DataNode)),
            {},
            BOOLEAN_TYPE,
            None,
        )
        LOG = IAttr(
            "LOG",
            True,
            True,
            False,
            ArgDesc(1, 1, (DataNode)),
            {},
            _get_first_argument_type,
            None,
        )
        LOG_GAMMA = IAttr(
            "LOG_GAMMA",
            True,
            True,
            False,
            ArgDesc(1, 1, (DataNode)),
            {},
            _get_first_argument_type,
            None,
        )
        LOG10 = IAttr(
            "LOG10",
            True,
            True,
            False,
            ArgDesc(1, 1, (DataNode)),
            {},
            _get_first_argument_type,
            None,
        )
        LOGICAL = IAttr(
            "LOGICAL",
            True,
            True,
            False,
            ArgDesc(1, 1, (DataNode)),
            {"kind": DataNode},
            _get_first_argument_type_with_optional_kind,
            None,
        )
        MASKL = IAttr(
            "MASKL",
            True,
            True,
            False,
            ArgDesc(1, 1, (DataNode)),
            {"kind": DataNode},
            _get_integer_with_optional_kind,
            None,
        )
        MASKR = IAttr(
            "MASKR",
            True,
            True,
            False,
            ArgDesc(1, 1, (DataNode)),
            {"kind": DataNode},
            _get_integer_with_optional_kind,
            None,
        )
        MATMUL = IAttr(
            "MATMUL",
            True,
            False,
            False,
            ArgDesc(2, 2, DataNode),
            {},
            _matmul_return_type,
            None,
        )
        MAX = IAttr(
            "MAX",
            True,
            True,
            False,
            ArgDesc(2, None, DataNode),
            {},
            _get_first_argument_type,
            None,
        )
        MAXEXPONENT = IAttr(
            "MAXEXPONENT",
            True,
            False,
            True,
            ArgDesc(1, 1, DataNode),
            {},
            INTEGER_TYPE,
            None,
        )
        MAXLOC = IAttr(
            "MAXLOC",
            True,
            False,
            False,
            ArgDesc(1, 2, DataNode),
            {
                "dim": DataNode,
                "mask": DataNode,
                "kind": DataNode,
                "back": DataNode,
            },
            _get_first_argument_intrinsic_with_optional_kind_and_dim,
            None,
        )
        MAXVAL = IAttr(
            "MAXVAL",
            True,
            False,
            False,
            ArgDesc(1, 1, DataNode),
            {"dim": DataNode, "mask": DataNode},
            _maxval_return_type,
            None,
        )
        MERGE = IAttr(
            "MERGE",
            True,
            True,
            False,
            ArgDesc(3, 3, DataNode),
            {},
            _get_first_argument_type,
            None
        )
        MERGE_BITS = IAttr(
            "MERGE_BITS",
            True,
            True,
            False,
            ArgDesc(3, 3, DataNode),
            {},
            _get_first_argument_type,
            None,
        )
        MIN = IAttr(
            "MIN",
            True,
            True,
            False,
            ArgDesc(2, None, DataNode),
            {},
            _get_first_argument_type,
            None,
        )
        MINEXPONENT = IAttr(
            "MINEXPONENT",
            True,
            False,
            True,
            ArgDesc(1, 1, DataNode),
            {},
            INTEGER_TYPE,
            None,
        )
        MINLOC = IAttr(
            "MINLOC",
            True,
            False,
            False,
            ArgDesc(1, 2, DataNode),
            {
                "dim": DataNode,
                "mask": DataNode,
                "kind": DataNode,
                "back": DataNode,
            },
            _get_first_argument_intrinsic_with_optional_kind_and_dim,
            None,
        )
        MINVAL = IAttr(
            "MINVAL",
            True,
            False,
            False,
            ArgDesc(1, 1, DataNode),
            {"dim": DataNode, "mask": DataNode},
            _maxval_return_type,
            None,
        )
        MOD = IAttr(
            "MOD",
            True,
            True,
            False,
            ArgDesc(2, 2, DataNode),
            {},
            _get_first_argument_type,
            None
        )
        MODULO = IAttr(
            "MODULO",
            True,
            True,
            False,
            ArgDesc(2, 2, DataNode),
            {},
            _get_first_argument_type,
            None,
        )
        MOVE_ALLOC = IAttr(
            "MOVE_ALLOC",
            False,
            False,
            False,
            ArgDesc(2, 2, DataNode),
            {"stat": DataNode, "errmsg": DataNode},
            None,
            None,
        )
        MVBITS = IAttr(
            "MVBITS",
            True,
            True,
            False,
            ArgDesc(5, 5, DataNode),
            {},
            None,
            None,
        )
        NEAREST = IAttr(
            "NEAREST",
            True,
            True,
            False,
            ArgDesc(2, 2, DataNode),
            {},
            _get_first_argument_type,
            None,
        )
        NEW_LINE = IAttr(
            "NEW_LINE",
            True,
            True,
            False,
            ArgDesc(1, 1, DataNode),
            {},
            CHARACTER_TYPE,
            None,
        )
        NINT = IAttr(
            "NINT",
            True,
            True,
            False,
            ArgDesc(1, 1, DataNode),
            {"kind": DataNode},
            INTEGER_TYPE,
            None,
        )
        NORM2 = IAttr(
            "NORM2",
            True,
            False,
            False,
            ArgDesc(1, 2, DataNode),
            {},
            # No kind on NORM2 but this function works for return type.
            # FIXME Check this is correct.
            _get_first_argument_intrinsic_with_optional_kind_and_dim,
            None,
        )
        NOT = IAttr(
            "NOT",
            True,
            True,
            False,
            ArgDesc(1, 1, DataNode),
            {},
            lambda node: ScalarType(
                ScalarType.Intrinsic.INTEGER,
                node.arguments[0].datatype.precision
            ),
            None
        )
        NULL = IAttr(
            "NULL",
            True,
            False,
            False,
            ArgDesc(0, 0, DataNode),
            {"mold": DataNode},
            # Returns a dissociated pointed - not supported.
            lambda node: UnresolvedType(),
            None,
        )
        NUM_IMAGES = IAttr(
            "NUM_IMAGES",
            True,
            False,
            False,
            ArgDesc(0, 1, DataNode),
            {},
            INTEGER_TYPE,
            None,
        )
        OUT_OF_RANGE = IAttr(
            "OUT_OF_RANGE",
            True,
            True,
            False,
            ArgDesc(2, 2, DataNode),
            {"round": DataNode},
            BOOLEAN_TYPE,
            None,
        )
        PACK = IAttr(
            "PACK",
            True,
            False,
            False,
            ArgDesc(2, 2, DataNode),
            {"vector": DataNode},
            lambda node: ArrayType(
                ScalarType(
                    node.arguments[0].datatype.intrinsic,
                    node.arguments[0].datatype.precision),
                [ArrayType.Extent.DEFERRED]
            ),
            None,
        )
        PARITY = IAttr(
            "PARITY",
            True,
            False,
            False,
            ArgDesc(1, 2, DataNode),
            {},
            _get_first_argument_type,
            None,
        )
        POPCNT = IAttr(
            "POPCNT",
            True,
            True,
            False,
            ArgDesc(1, 1, DataNode),
            {},
            INTEGER_TYPE,
            None,
        )
        POPPAR = IAttr(
            "POPPAR",
            True,
            True,
            False,
            ArgDesc(1, 1, DataNode),
            {},
            INTEGER_TYPE,
            None,
        )
        PRECISION = IAttr(
            "PRECISION",
            True,
            False,
            True,
            ArgDesc(1, 1, DataNode),
            {},
            INTEGER_TYPE,
            None,
        )
        PRESENT = IAttr(
            "PRESENT",
            True,
            False,
            True,
            ArgDesc(1, 1, DataNode),
            {},
            BOOLEAN_TYPE,
            None,
        )
        PRODUCT = IAttr(
            "PRODUCT",
            True,
            False,
            False,
            ArgDesc(1, 1, DataNode),
            {"dim": DataNode, "mask": DataNode},
            lambda node: (
                _get_first_argument_specified_kind_with_optional_dim(
                    node, node.arguments[0].datatype.intrinsic
                )
            ),
            None,
        )
        RADIX = IAttr(
            "RADIX",
            True,
            False,
            True,
            ArgDesc(1, 1, DataNode),
            {},
            INTEGER_TYPE,
            None
        )
        RANDOM_INIT = IAttr(
            "RANDOM_INIT",
            False,
            False,
            False,
            ArgDesc(2, 2, DataNode),
            {},
            None,
            None,
        )
        RANDOM_NUMBER = IAttr(
            "RANDOM_NUMBER",
            False,
            False,
            False,
            ArgDesc(1, 1, Reference),
            {},
            None,
            None,
        )
        RANDOM_SEED = IAttr(
            "RANDOM_SEED",
            False,
            False,
            False,
            ArgDesc(0, 0, Reference),
            {"size": DataNode, "put": DataNode, "Get": DataNode},
            None,
            None,
        )
        RANGE = IAttr(
            "RANGE",
            True,
            False,
            True,
            ArgDesc(1, 1, Reference),
            {},
            INTEGER_TYPE,
            None,
        )
        RANK = IAttr(
            "RANK",
            True,
            False,
            True,
            ArgDesc(1, 1, Reference),
            {},
            INTEGER_TYPE,
            None
        )
        REAL = IAttr(
            "REAL",
            True,
            True,
            False,
            ArgDesc(1, 1, Reference),
            {"kind": DataNode},
            lambda node: (
                ScalarType(
                    ScalarType.Intrinsic.REAL,
                    (
                        node.arguments[node.argument_names.index("kind")]
                        if "kind" in node.argument_names
                        else node.arguments[0].datatype.precision
                    ),
                )
            ),
            None,
        )
        REDUCE = IAttr(
            "REDUCE",
            True,
            False,
            False,
            ArgDesc(2, 3, DataNode),
            {"mask": DataNode, "identity": DataNode, "ordered": DataNode},
            _reduce_return_type,
            None,
        )
        REPEAT = IAttr(
            "REPEAT",
            True,
            False,
            False,
            ArgDesc(2, 2, Reference),
            {},
            CHARACTER_TYPE,
            None,
        )
        RESHAPE = IAttr(
            "RESHAPE",
            True,
            False,
            False,
            ArgDesc(2, 2, Reference),
            {"pad": DataNode, "order": DataNode},
            # I went with unresolved for now as the result depends on
            # argument 2 (even the dimensionality).
            lambda node: UnresolvedType(),
            None,
        )
        RRSPACING = IAttr(
            "RRSPACING",
            True,
            True,
            False,
            ArgDesc(1, 1, Reference),
            {},
            _get_first_argument_type,
            None,
        )
        SAME_TYPE_AS = IAttr(
            "SAME_TYPE_AS",
            True,
            False,
            True,
            ArgDesc(2, 2, Reference),
            {},
            BOOLEAN_TYPE,
            None,
        )
        SCALE = IAttr(
            "SCALE",
            True,
            True,
            False,
            ArgDesc(2, 2, Reference),
            {},
            _get_first_argument_type,
            None,
        )
        SCAN = IAttr(
            "SCAN",
            True,
            True,
            False,
            ArgDesc(2, 2, Reference),
            {"back": DataNode, "kind": DataNode},
            _get_integer_with_optional_kind,
            None,
        )
        SELECTED_CHAR_KIND = IAttr(
            "SELECTED_CHAR_KIND",
            True,
            False,
            False,
            ArgDesc(1, 1, Reference),
            {},
            INTEGER_TYPE,
            None,
        )
        SELECTED_INT_KIND = IAttr(
            "SELECTED_INT_KIND",
            True,
            False,
            False,
            ArgDesc(1, 1, Reference),
            {},
            INTEGER_TYPE,
            None,
        )
        SELECTED_REAL_KIND = IAttr(
            "SELECTED_REAL_KIND",
            True,
            False,
            False,
            ArgDesc(0, 0, Reference),
            {"P": DataNode, "R": DataNode, "radix": DataNode},
            INTEGER_TYPE,
            None,
        )
        SET_EXPONENT = IAttr(
            "SET_EXPONENT",
            True,
            True,
            False,
            ArgDesc(2, 2, Reference),
            {},
            _get_first_argument_type,
            None,
        )
        SHAPE = IAttr(
            "SHAPE",
            True,
            False,
            True,
            ArgDesc(1, 1, Reference),
            {"kind": DataNode},
            lambda node: (
                ArrayType(ScalarType(
                    ScalarType.Intrinsic.INTEGER,
                    (ScalarType.Precision.UNDEFINED if "kind" not in
                     node.argument_names else
                     node.arguments[node.argument_names.index("kind")])),
                    [ArrayType.ArrayBounds(
                        Literal("1", INTEGER_TYPE),
                        Literal(str(len(node.arguments[0].datatype.shape)),
                                INTEGER_TYPE))])
            ),
            None,
        )
        SHIFTA = IAttr(
            "SHIFTA",
            True,
            True,
            False,
            ArgDesc(2, 2, Reference),
            {},
            _get_first_argument_type,
            None,
        )
        SHIFTL = IAttr(
            "SHIFTL",
            True,
            True,
            False,
            ArgDesc(2, 2, Reference),
            {},
            _get_first_argument_type,
            None,
        )
        SHIFTR = IAttr(
            "SHIFTR",
            True,
            True,
            False,
            ArgDesc(2, 2, Reference),
            {},
            _get_first_argument_type,
            None,
        )
        SIGN = IAttr(
            "SIGN",
            True,
            True,
            False,
            ArgDesc(2, 2, DataNode),
            {},
            _get_first_argument_type,
            None
        )
        SIN = IAttr(
            "SIN",
            True,
            True,
            False,
            ArgDesc(1, 1, DataNode),
            {},
            _get_first_argument_type,
            None
        )
        SINH = IAttr(
            "SINH",
            True,
            True,
            False,
            ArgDesc(1, 1, DataNode),
            {},
            _get_first_argument_type,
            None
        )
        SIZE = IAttr(
            "SIZE",
            True,
            False,
            True,
            ArgDesc(1, 1, DataNode),
            {"dim": DataNode, "kind": DataNode},
            _get_integer_with_optional_kind,
            None,
        )
        SPACING = IAttr(
            "SPACING",
            True,
            True,
            False,
            ArgDesc(1, 1, DataNode),
            {},
            _get_first_argument_type,
            None,
        )
        SPREAD = IAttr(
            "SPREAD",
            True,
            False,
            False,
            ArgDesc(3, 3, DataNode),
            {},
            lambda node: ArrayType(
                ScalarType(
                    node.arguments[0].datatype.intrinsic,
                    node.arguments[0].datatype.precision),
                ([ArrayType.Extent.DEFERRED] *
                 (len(node.arguments[0].datatype.shape) + 1)
                 if isinstance(node.arguments[0].datatype, ArrayType) else
                 [ArrayType.Extent.DEFERRED])
            ),
            None,
        )
        SQRT = IAttr(
            "SQRT",
            True,
            True,
            False,
            ArgDesc(1, 1, DataNode),
            {},
            # FIXME For reviewer - I put unresolved type because it can return
            # COMPLEX depending on input.
            lambda node: UnresolvedType(),
            None
        )
        STOPPED_IMAGES = IAttr(
            "STOPPED_IMAGES",
            False,
            False,
            False,
            ArgDesc(0, 0, DataNode),
            {"team": DataNode, "kind": DataNode},
            lambda node: ArrayType(
                _get_integer_with_optional_kind(node),
                [ArrayType.Extent.DEFERRED]
            ),
            None,
        )
        STORAGE_SIZE = IAttr(
            "STORAGE_SIZE",
            True,
            False,
            True,
            ArgDesc(1, 1, DataNode),
            {"kind": DataNode},
            _get_integer_with_optional_kind,
            None,
        )
        SUM = IAttr(
            "SUM",
            True,
            False,
            False,
            # FIXME Think this is wrong again - 2nd argument can be non-named
            # dim?
            ArgDesc(1, 1, DataNode),
            {"dim": DataNode, "mask": DataNode},
            lambda node: _get_first_argument_specified_kind_with_optional_dim(
                node, node.arguments[0].datatype.intrinsic
            ),
            None,
        )
        SYSTEM_CLOCK = IAttr(
            "SYSTEM_CLOCK",
            False,
            False,
            False,
            ArgDesc(0, 0, DataNode),
            {"count": DataNode, "count_rate": DataNode, "count_max": DataNode},
            None,
            None,
        )
        TAN = IAttr(
            "TAN",
            True,
            True,
            False,
            ArgDesc(1, 1, DataNode),
            {},
            _get_first_argument_type,
            None
        )
        TANH = IAttr(
            "TANH",
            True,
            True,
            False,
            ArgDesc(1, 1, DataNode),
            {},
            _get_first_argument_type,
            None
        )
        # FIXME I'm not sure this exists?
        TEAM_IMAGE = IAttr(
            "TEAM_IMAGE",
            True,
            False,
            False,
            ArgDesc(0, 0, DataNode),
            {"team": DataNode},
            None,
            None,
        )
        THIS_IMAGE = IAttr(
            "THIS_IMAGE",
            True,
            False,
            False,
            # FIXME Again not sure this is correct. Have used unresolved
            # type for the return value - can improve it if wanted.
            ArgDesc(0, 0, DataNode),
            {"coarray": DataNode, "team": DataNode, "dim": DataNode},
            lambda node: UnresolvedType(),
            None,
        )
        TINY = IAttr(
            "TINY",
            True,
            False,
            True,
            ArgDesc(1, 1, (Reference, Literal)),
            {},
            _get_first_argument_type,
            None,
        )
        TRAILZ = IAttr(
            "TRAILZ",
            True,
            True,
            False,
            ArgDesc(1, 1, DataNode),
            {},
            INTEGER_TYPE,
            None,
        )
        TRANSFER = IAttr(
            "TRANSFER",
            True,
            False,
            False,
            ArgDesc(2, 2, DataNode),
            {"size": DataNode},
            lambda node: (
                node.arguments[1].datatype if
                ("size" not in node.argument_names and
                 not isinstance(node.arguments[1].datatype, ArrayType))
                else ArrayType(
                    ScalarType(node.arguments[1].datatype.intrinsic,
                               node.arguments[1].datatype.precision),
                    [ArrayType.Extent.DEFERRED])

            ),
            None,
        )
        TRANSPOSE = IAttr(
            "TRANSPOSE",
            True,
            False,
            False,
            ArgDesc(1, 1, DataNode),
            {},
            lambda node: ArrayType(ScalarType(
                node.arguments[0].datatype.intrinsic,
                node.arguments[0].datatype.precision),
                [node.arguments[0].datatype.shape[1],
                 node.arguments[0].datatype.shape[0]]
            ),
            None,
        )
        TRIM = IAttr(
            "TRIM",
            True,
            False,
            False,
            ArgDesc(1, 1, DataNode),
            {},
            CHARACTER_TYPE,
            None
        )
        UBOUND = IAttr(
            "UBOUND",
            True,
            False,
            True,
            ArgDesc(1, 1, DataNode),
            {"dim": DataNode, "kind": DataNode},
            _get_bound_function_return_type,
            None,
        )
        UCOBOUND = IAttr(
            "UCOBOUND",
            True,
            False,
            True,
            ArgDesc(1, 1, DataNode),
            {"dim": DataNode, "kind": DataNode},
            _get_bound_function_return_type,
            None,
        )
        UNPACK = IAttr(
            "UNPACK",
            True,
            False,
            False,
            ArgDesc(3, 3, DataNode),
            {},
            _get_first_argument_type,
            None,
        )
        VERIFY = IAttr(
            "VERIFY",
            True,
            True,
            False,
            ArgDesc(2, 2, DataNode),
            {"back": DataNode, "kind": DataNode},
            _get_integer_with_optional_kind,
            None,
        )

        def __hash__(self):
            return hash(self.name)

    def __init__(self, intrinsic, **kwargs):
        if not isinstance(intrinsic, Enum) or intrinsic not in self.Intrinsic:
            raise TypeError(
                f"IntrinsicCall 'intrinsic' argument should be an "
                f"instance of IntrinsicCall.Intrinsic, but found "
                f"'{type(intrinsic).__name__}'."
            )

        # A Call expects a Reference to a Symbol, so give it a Reference
        # to an Intrinsicsymbol of the given intrinsic.
        super().__init__(**kwargs)
        self.addchild(
            Reference(
                IntrinsicSymbol(
                    intrinsic.name,
                    intrinsic,
                    is_elemental=intrinsic.is_elemental,
                    is_pure=intrinsic.is_pure,
                )
            )
        )

    @property
    def intrinsic(self):
        """Return the type of intrinsic.

        :returns: enumerated type capturing the type of intrinsic.
        :rtype: :py:class:`psyclone.psyir.nodes.IntrinsicCall.Intrinsic`

        """
        return self.routine.symbol.intrinsic

    @property
    def datatype(self) -> DataType:
        """Return the datatype of this IntrinsicCall.

        :returns: The datatype corresponding to this IntrinsicCall.
        """
        # If the return type is None then return NoType
        if not self.intrinsic.return_type:
            return NoType()
        if isinstance(self.intrinsic.return_type, Callable):
            try:
                return self.intrinsic.return_type(self)
            except Exception:
                # If we don't know what a type is correctly then many
                # of the computed types will fail, so we should give
                # an UnresolvedType in those cases.
                return UnresolvedType()
        else:
            return self.intrinsic.return_type

    def is_available_on_device(self, device_string: str = "") -> bool:
        """
        :param device_string: optional string to identify the offloading
            device (or its compiler-platform family).
        :returns: whether this intrinsic is available on an accelerated device.

        :raises ValueError: if the provided 'device_string' is not one of the
            supported values.

        """
        # Reduction operations that have more than a single argument sometimes
        # fail, so we avoid putting them on the accelerator device
        if self.intrinsic in REDUCTION_INTRINSICS:
            if len(self.arguments) > 1:
                return False

        if not device_string:
            return self.intrinsic in DEFAULT_DEVICE_INTRINISCS
        if device_string == "nvfortran-all":
            return self.intrinsic in NVFORTRAN_ALL
        if device_string == "nvfortran-uniform":
            return self.intrinsic in NVFORTRAN_UNIFORM

        raise ValueError(
            f"Unsupported device_string value '{device_string}', the supported"
            " values are '' (default), 'nvfortran-all', 'nvfortran-uniform'"
        )

    @classmethod
    def create(cls, intrinsic, arguments=()):
        """Create an instance of this class given the type of intrinsic and a
        list of nodes (or name-and-node tuples) for its arguments. Any
        named arguments *must* come after any required arguments.

        :param intrinsic: the Intrinsic being called.
        :type intrinsic: py:class:`psyclone.psyir.IntrinsicCall.Intrinsic`
        :param arguments: list of arguments for this intrinsic, these
            can be PSyIR nodes or tuples of string,Node for named arguments.
        :type arguments: Optional[Iterable[\
            Union[:py:class:`psyclone.psyir.nodes.DataNode`,\
                  Tuple[str, :py:class:`psyclone.psyir.nodes.DataNode`]]]]

        :returns: an instance of this class.
        :rtype: :py:class:`psyclone.psyir.nodes.IntrinsicCall`

        :raises TypeError: if any of the arguments are of the wrong type.
        :raises ValueError: if any optional arguments have incorrect names
            or if a positional argument is listed *after* a named argument.
        :raises ValueError: if the number of supplied arguments is not valid
            for the specified intrinsic.

        """
        call = IntrinsicCall(intrinsic)

        if not isinstance(arguments, Iterable):
            raise TypeError(
                f"IntrinsicCall.create() 'arguments' argument should be an "
                f"Iterable but found '{type(arguments).__name__}'"
            )

        # Validate the supplied arguments.
        last_named_arg = None
        pos_arg_count = 0
        for arg in arguments:
            if isinstance(arg, tuple):
                if not isinstance(arg[0], str):
                    raise TypeError(
                        f"Optional arguments to an IntrinsicCall must be "
                        f"specified by a (str, Reference) tuple but got "
                        f"a {type(arg[0]).__name__} instead of a str."
                    )
                name = arg[0].lower()
                last_named_arg = name
                # TODO #2302: For now we disable the positional arguments
                # checks because this does not consider that positional
                # arguments can be also found by name, and we don't have
                # sufficient information to validate them.
                # if not optional_arg_names:
                #     raise ValueError(
                #         f"The '{intrinsic.name}' intrinsic does not support "
                #         f"any optional arguments but got '{name}'.")
                # if name not in optional_arg_names:
                #     raise ValueError(
                #         f"The '{intrinsic.name}' intrinsic supports the "
                #         f"optional arguments {optional_arg_names} but got "
                #         f"'{name}'")
                if name in intrinsic.optional_args:
                    if not isinstance(arg[1], intrinsic.optional_args[name]):
                        raise TypeError(
                            f"The optional argument '{name}' to intrinsic "
                            f"'{intrinsic.name}' must be of type "
                            f"'{intrinsic.optional_args[name].__name__}' but "
                            f"got '{type(arg[1]).__name__}'"
                        )
                else:
                    # If it not in the optional_args list it must be positional
                    pos_arg_count += 1
            else:
                if last_named_arg:
                    raise ValueError(
                        f"Found a positional argument *after* a named "
                        f"argument ('{last_named_arg}'). This is invalid.'"
                    )
                if not isinstance(arg, intrinsic.required_args.types):
                    raise TypeError(
                        f"The '{intrinsic.name}' intrinsic requires that "
                        f"positional arguments be of type "
                        f"'{intrinsic.required_args.types}' "
                        f"but got a '{type(arg).__name__}'"
                    )
                pos_arg_count += 1

        if (
            intrinsic.required_args.max_count is not None
            and pos_arg_count > intrinsic.required_args.max_count
        ) or pos_arg_count < intrinsic.required_args.min_count:
            msg = f"The '{intrinsic.name}' intrinsic requires "
            if (
                intrinsic.required_args.max_count is not None
                and intrinsic.required_args.max_count > 0
            ):
                msg += (
                    f"between {intrinsic.required_args.min_count} and "
                    f"{intrinsic.required_args.max_count} "
                )
            else:
                msg += f"at least {intrinsic.required_args.min_count} "
            msg += f"arguments but got {len(arguments)}."
            raise ValueError(msg)

        # Create an intrinsic call and add the arguments
        # afterwards. We can't call the parent create method as it
        # assumes the intrinsic argument is a symbol and therefore tries
        # to create an intrinsic call with this symbol, rather than
        # the intrinsic enum.
        call._add_args(call, arguments)

        return call

    def reference_accesses(self) -> VariablesAccessMap:
        """
        :returns: a map of all the symbol accessed inside this node, the
            keys are Signatures (unique identifiers to a symbol and its
            structure acccessors) and the values are AccessSequence
            (a sequence of AccessTypes).

        """
        var_accesses = VariablesAccessMap()
        if self.intrinsic.is_inquiry and isinstance(
            self.arguments[0], Reference
        ):
            # If this is an inquiry access (which doesn't actually access the
            # value) then make sure we use the correct access type for the
            # inquired variable, which is always the first argument.
            sig, indices = self.arguments[0].get_signature_and_indices()
            var_accesses.add_access(sig, AccessType.INQUIRY, self.arguments[0])
            for idx_list in indices:
                for idx in idx_list:
                    var_accesses.update(idx.reference_accesses())
        elif self.arguments:
            var_accesses.update(self.arguments[0].reference_accesses())

        for child in self.arguments[1:]:
            var_accesses.update(child.reference_accesses())
        return var_accesses

    # TODO #2102: Maybe the three properties below can be removed if intrinsic
    # is a symbol, as they would act as the super() implementation.
    @property
    def is_elemental(self):
        """
        :returns: whether the routine being called is elemental (provided with
            an input array it will apply the operation individually to each of
            the array elements and return an array with the results). If this
            information is not known then it returns None.
        :rtype: NoneType | bool
        """
        return self.intrinsic.is_elemental

    @property
    def is_pure(self):
        """
        :returns: whether the routine being called is pure (guaranteed to
            return the same result when provided with the same argument
            values).  If this information is not known then it returns None.
        :rtype: NoneType | bool
        """
        return self.intrinsic.is_pure

    @property
    def is_inquiry(self):
        """
        :returns: whether the routine being called is a query function (i.e.
            returns information about its argument rather than accessing any
            data referenced by the argument). If this information is not known
            then it returns None.
        :rtype: NoneType | bool
        """
        return self.intrinsic.is_inquiry


# Intrinsics available on nvidia gpus with uniform (CPU and GPU) results when
# compiled with the nvfortran "-gpu=uniform_math" flag
NVFORTRAN_UNIFORM = (
    IntrinsicCall.Intrinsic.ABS,
    IntrinsicCall.Intrinsic.ACOS,
    IntrinsicCall.Intrinsic.AINT,
    IntrinsicCall.Intrinsic.ANINT,
    IntrinsicCall.Intrinsic.ASIN,
    IntrinsicCall.Intrinsic.ATAN,
    IntrinsicCall.Intrinsic.ATAN2,
    IntrinsicCall.Intrinsic.COS,
    IntrinsicCall.Intrinsic.COSH,
    IntrinsicCall.Intrinsic.DBLE,
    IntrinsicCall.Intrinsic.DPROD,
    IntrinsicCall.Intrinsic.EXP,
    IntrinsicCall.Intrinsic.IAND,
    IntrinsicCall.Intrinsic.IEOR,
    IntrinsicCall.Intrinsic.INT,
    IntrinsicCall.Intrinsic.IOR,
    IntrinsicCall.Intrinsic.LOG,
    IntrinsicCall.Intrinsic.NOT,
    IntrinsicCall.Intrinsic.MAX,
    IntrinsicCall.Intrinsic.MIN,
    IntrinsicCall.Intrinsic.MOD,
    IntrinsicCall.Intrinsic.NINT,
    IntrinsicCall.Intrinsic.SIGN,
    IntrinsicCall.Intrinsic.SIN,
    IntrinsicCall.Intrinsic.SINH,
    IntrinsicCall.Intrinsic.SQRT,
    IntrinsicCall.Intrinsic.TAN,
    IntrinsicCall.Intrinsic.TANH,
    IntrinsicCall.Intrinsic.UBOUND,
    IntrinsicCall.Intrinsic.MERGE,
    IntrinsicCall.Intrinsic.PRODUCT,
    IntrinsicCall.Intrinsic.SIZE,
    IntrinsicCall.Intrinsic.SUM,
    IntrinsicCall.Intrinsic.LBOUND,
    IntrinsicCall.Intrinsic.MAXVAL,
    IntrinsicCall.Intrinsic.MINVAL,
    IntrinsicCall.Intrinsic.TINY,
    IntrinsicCall.Intrinsic.HUGE,
)
# MATMUL can fail at link time depending on the precision of
# its arguments.
# IntrinsicCall.Intrinsic.MATMUL,

# All nvfortran intrinsics available on GPUs
NVFORTRAN_ALL = NVFORTRAN_UNIFORM + (
    IntrinsicCall.Intrinsic.LOG10,
    IntrinsicCall.Intrinsic.REAL,
)

# For now the default intrinsics availabe on GPU are the same as nvfortran-all
DEFAULT_DEVICE_INTRINISCS = NVFORTRAN_ALL

# TODO #658 this can be removed once we have support for determining the
# type of a PSyIR expression.
# Intrinsics that perform a reduction on an array.
REDUCTION_INTRINSICS = [
    IntrinsicCall.Intrinsic.SUM,
    IntrinsicCall.Intrinsic.MINVAL,
    IntrinsicCall.Intrinsic.MAXVAL,
]
