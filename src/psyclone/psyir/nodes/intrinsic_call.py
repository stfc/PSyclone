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
from psyclone.errors import InternalError
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


def _convert_argument_to_type_info(argument: DataNode,
                                   access_info: VariablesAccessMap) -> None:
    """Helper function for the common case where an argument needs to have
    a TYPE_INFO access map in access_info instead of a read access.

    :param argument: The argument whose access needs changing.
    :param access_info: The access map containing the access.
    """
    # If the argument isn't a Reference then we don't do anything.
    if isinstance(argument, Reference):
        sig, _ = argument.get_signature_and_indices()
        var_info = access_info[sig]
        try:
            var_info.change_read_to_type_info()
        except InternalError:
            # The argument here is also used in some other way
            # so we do nothing as the other usage has precedence.
            pass


def _reference_accesses_all_reads_with_optional_kind(
        node
) -> VariablesAccessMap:
    """Helper function for the common IntrinsicCall case where all
    arguments are read only, with the exception of an optional kind named
    argument which is instead TYPE_INFO.

    :param node: The IntrinsicCall whose reference_accesses to compute.
    :type node:  :py:class:`psyclone.psyir.nodes.IntrinsicCall`

    :returns: the reference accesses of node.
    """
    kind_index = (node.argument_names.index("kind") if
                  "kind" in node.argument_names else None)
    reference_accesses = VariablesAccessMap()
    for i, arg in enumerate(node.arguments):
        accesses = arg.reference_accesses()
        if kind_index == i:
            if isinstance(arg, Reference):
                _convert_argument_to_type_info(arg, accesses)
        reference_accesses.update(accesses)

    return reference_accesses


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
            name="ALLOCATE",
            is_pure=False,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, None, Reference),
            optional_args={
                "mold": Reference,
                "source": Reference,
                "stat": Reference,
                "errmsg": Reference,
            },
            return_type=None,
            reference_accesses=None,
        )
        DEALLOCATE = IAttr(
            name="DEALLOCATE",
            is_pure=False,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, None, Reference),
            optional_args={"stat": Reference},
            return_type=None,
            reference_accesses=None,
        )
        NULLIFY = IAttr(
            name="NULLIFY",
            is_pure=False,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, None, Reference),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )

        # Fortran Intrinsics (from Fortran 2018 standard table 16.1)
        ABS = IAttr(
            name="ABS",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            # TODO 1590 Complex conversion unsupported.
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        ACHAR = IAttr(
            name="ACHAR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"kind": DataNode},
            return_type=CHARACTER_TYPE,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        ACOS = IAttr(
            name="ACOS",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        ACOSH = IAttr(
            name="ACOSH",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        ADJUSTL = IAttr(
            name="ADJUSTL",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            # TODO 2612 This may be more complex if we support character len
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        ADJUSTR = IAttr(
            name="ADJUSTR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            # TODO 2612 This may be more complex if we support character len
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        AIMAG = IAttr(
            name="AIMAG",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            # TODO #1590 Complex numbers' precision unsupported.
            return_type=lambda node: UnresolvedType(),
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        AINT = IAttr(
            name="AINT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"kind": DataNode},
            return_type=lambda node: (
                ScalarType(
                    ScalarType.Intrinsic.REAL,
                    (
                        node.arguments[node.argument_names.index("kind")]
                        if "kind" in node.argument_names
                        else node.arguments[0].datatype.precision
                    ),
                )
            ),
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        ALL = IAttr(
            name="ALL",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"dim": DataNode},
            return_type=_get_first_argument_specified_kind_with_optional_dim,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        ALLOCATED = IAttr(
            name="ALLOCATED",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=BOOLEAN_TYPE,
            reference_accesses=None,  # FIXME Inquiry function.
        )
        ANINT = IAttr(
            name="ANINT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"kind": DataNode},
            return_type=lambda node: (
                ScalarType(
                    ScalarType.Intrinsic.REAL,
                    (
                        node.arguments[node.argument_names.index("kind")]
                        if "kind" not in node.argument_names
                        else node.arguments[0].datatype.precision
                    ),
                )
            ),
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        ANY = IAttr(
            name="ANY",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"dim": DataNode},
            return_type=_get_first_argument_specified_kind_with_optional_dim,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        ASIN = IAttr(
            name="ASIN",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        ASINH = IAttr(
            name="ASINH",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        ASSOCIATED = IAttr(
            name="ASSOCIATED",
            is_pure=False,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"target": DataNode},
            return_type=BOOLEAN_TYPE,
            reference_accesses=None,  # FIXME Inquiry function
        )
        ATAN = IAttr(
            name="ATAN",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 2, DataNode),
            optional_args={},
            # N. B. If this has 2 arguments then the return value
            # is the of the second argument, however the standard defines
            # the type and kind type of both arguments must be the same.
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        ATAN2 = IAttr(
            name="ATAN2",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={},
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        ATANH = IAttr(
            name="ATANH",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        ATOMIC_ADD = IAttr(
            name="ATOMIC_ADD",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={"stat": DataNode},
            return_type=None,
            reference_accesses=None,  # FIXME Write read Write
        )
        ATOMIC_AND = IAttr(
            name="ATOMIC_AND",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={"stat": DataNode},
            return_type=None,
            reference_accesses=None,  # FIXME Write read Write
        )
        ATOMIC_CAS = IAttr(
            name="ATOMIC_CAS",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={"stat": DataNode},
            return_type=None,
            reference_accesses=None,  # FIXME Write Write read read Write
        )
        ATOMIC_DEFINE = IAttr(
            name="ATOMIC_DEFINE",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={"stat": DataNode},
            return_type=None,
            reference_accesses=None,  # FIXME Write read Write
        )
        ATOMIC_FETCH_ADD = IAttr(
            name="ATOMIC_FETCH_ADD",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(3, 3, DataNode),
            optional_args={"stat": DataNode},
            return_type=None,
            reference_accesses=None,  # FIXME Write read write write
        )
        ATOMIC_FETCH_AND = IAttr(
            name="ATOMIC_FETCH_AND",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(3, 3, DataNode),
            optional_args={"stat": DataNode},
            return_type=None,
            reference_accesses=None,  # FIXME Write read write write
        )
        ATOMIC_FETCH_OR = IAttr(
            name="ATOMIC_FETCH_OR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(3, 3, DataNode),
            optional_args={"stat": DataNode},
            return_type=None,
            reference_accesses=None,  # FIXME Write read write write
        )
        ATOMIC_FETCH_XOR = IAttr(
            name="ATOMIC_FETCH_XOR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(3, 3, DataNode),
            optional_args={"stat": DataNode},
            return_type=None,
            reference_accesses=None,  # FIXME Write read write write
        )
        ATOMIC_OR = IAttr(
            name="ATOMIC_OR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={"stat": DataNode},
            return_type=None,
            reference_accesses=None,  # FIXME Write read Write
        )
        ATOMIC_REF = IAttr(
            name="ATOMIC_REF",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={"stat": DataNode},
            return_type=None,
            reference_accesses=None,  # FIXME Write read write
        )
        ATOMIC_XOR = IAttr(
            name="ATOMIC_XOR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={"stat": DataNode},
            return_type=None,
            reference_accesses=None,  # FIXME Write read write
        )
        BESSEL_J0 = IAttr(
            name="BESSEL_J0",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=_get_real_with_argone_kind,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        BESSEL_J1 = IAttr(
            name="BESSEL_J1",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=_get_real_with_argone_kind,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        BESSEL_JN = IAttr(
            name="BESSEL_JN",
            is_pure=True,
            is_elemental=None,
            is_inquiry=False,
            required_args=ArgDesc(2, 3, DataNode),
            optional_args={},
            return_type=_get_real_with_x_kind,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        BESSEL_Y0 = IAttr(
            name="BESSEL_Y0",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=_get_real_with_argone_kind,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        BESSEL_Y1 = IAttr(
            name="BESSEL_Y1",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=_get_real_with_argone_kind,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        BESSEL_YN = IAttr(
            name="BESSEL_YN",
            is_pure=True,
            is_elemental=None,
            is_inquiry=False,
            required_args=ArgDesc(2, 3, DataNode),
            optional_args={},
            return_type=_get_real_with_x_kind,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        BGE = IAttr(
            name="BGE",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={},
            return_type=BOOLEAN_TYPE,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        BGT = IAttr(
            name="BGT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={},
            return_type=BOOLEAN_TYPE,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        BIT_SIZE = IAttr(
            name="BIT_SIZE",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=INTEGER_TYPE,
            reference_accesses=None,  # FIXME Inquiry function.
        )
        BLE = IAttr(
            name="BLE",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={},
            return_type=BOOLEAN_TYPE,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        BLT = IAttr(
            name="BLT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={},
            return_type=BOOLEAN_TYPE,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        BTEST = IAttr(
            name="BTEST",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={},
            return_type=BOOLEAN_TYPE,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        CEILING = IAttr(
            name="CEILING",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"kind": DataNode},
            return_type=_get_integer_with_optional_kind,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        CHAR = IAttr(
            name="CHAR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"kind": DataNode},
            return_type=CHARACTER_TYPE,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        CMPLX = IAttr(
            name="CMPLX",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"Y": DataNode, "kind": DataNode},
            # TODO #1590 Complex numbers unsupported.
            return_type=lambda node: UnresolvedType(),
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        CO_BROADCAST = IAttr(
            name="CO_BROADCAST",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, 2, DataNode),
            optional_args={"stat": DataNode, "errmsg": DataNode},
            return_type=None,
            reference_accesses=None,  # FIXME ReadWrite, READ, Write, Write
        )
        CO_MAX = IAttr(
            name="CO_MAX",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"result_image": DataNode,
                           "stat": DataNode,
                           "errmsg": DataNode},
            return_type=None,
            reference_accesses=None,  # FIXME readwrite, read, write, write
        )
        CO_MIN = IAttr(
            name="CO_MIN",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"result_image": DataNode,
                           "stat": DataNode,
                           "errmsg": DataNode},
            return_type=None,
            reference_accesses=None,  # FIXME readwrite, read, write, write
        )
        CO_REDUCE = IAttr(
            name="CO_REDUCE",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, 2, DataNode),
            optional_args={"result_image": DataNode,
                           "stat": DataNode,
                           "errmsg": DataNode},
            return_type=None,
            reference_accesses=None,
            # FIXME readwrite, inquiry?, read, write, write
        )
        CO_SUM = IAttr(
            name="CO_SUM",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"result_image": DataNode,
                           "stat": DataNode,
                           "errmsg": DataNode},
            return_type=None,
            reference_accesses=None,  # FIXME readwrite, read, write, write
        )
        COMMAND_ARGUMENT_COUNT = IAttr(
            name="COMMAND_ARGUMENT_COUNT",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(0, 0, None),
            optional_args={},
            return_type=INTEGER_TYPE,
            reference_accesses=None,  # FIXME None
        )
        CONJG = IAttr(
            name="CONJG",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            # TODO #1590 Complex numbers unsupported.
            return_type=lambda node: UnresolvedType(),
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        COS = IAttr(
            name="COS",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        COSH = IAttr(
            name="COSH",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        COSHAPE = IAttr(
            name="COSHAPE",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"kind": DataNode},
            # FIXME Return type
            return_type=lambda node: ArrayType(
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
            reference_accesses=None,  # FIXME inquiry, kind
        )
        COUNT = IAttr(
            name="COUNT",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"dim": DataNode, "kind": DataNode},
            return_type=_get_integer_of_kind_with_optional_dim,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        CPU_TIME = IAttr(
            name="CPU_TIME",
            is_pure=False,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,  # FIXME write
        )
        CSHIFT = IAttr(
            name="CSHIFT",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={"dim": DataNode},
            return_type=_get_first_argument_type,  # FIXME Wait on Sergi reply
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        DATE_AND_TIME = IAttr(
            name="DATE_AND_TIME",
            is_pure=False,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(0, 0, DataNode),
            optional_args={
                "date": DataNode,
                "time": DataNode,
                "zone": DataNode,
                "values": DataNode,
            },
            return_type=None,
            reference_accesses=None,  # FIXME All write arguments
        )
        DBLE = IAttr(
            name="DBLE",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=REAL_DOUBLE_TYPE,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        DIGITS = IAttr(
            name="DIGITS",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=INTEGER_TYPE,
            reference_accesses=None,  # FIXME Inquiry
        )
        DIM = IAttr(
            name="DIM",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={},
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        DOT_PRODUCT = IAttr(
            name="DOT_PRODUCT",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={},
            return_type=lambda node: ScalarType(
                node.arguments[0].datatype.intrinsic,
                node.arguments[0].datatype.precision,
            ),
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        DPROD = IAttr(
            name="DPROD",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={},
            return_type=REAL8_TYPE,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        DSHIFTL = IAttr(
            name="DSHIFTL",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(3, 3, DataNode),
            optional_args={},
            return_type=lambda node: (
                node.arguments[0].datatype.copy()
                if not isinstance(node.arguments[0], Literal)
                else node.arguments[1].datatype.copy()
            ),
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        DSHIFTR = IAttr(
            name="DSHIFTR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(3, 3, DataNode),
            optional_args={},
            return_type=lambda node: (
                node.arguments[0].datatype.copy()
                if not isinstance(node.arguments[0], Literal)
                else node.arguments[1].datatype.copy()
            ),
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        EOSHIFT = IAttr(
            name="EOSHIFT",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={"boundary": DataNode, "dim": DataNode},
            return_type=_get_first_argument_type,
            # FIXME Wait for Sergi reply.
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        EPSILON = IAttr(
            name="EPSILON",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=_get_first_argument_type,
            reference_accesses=None,  # FIXME inquiry
        )
        ERF = IAttr(
            name="ERF",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=_get_real_with_argone_kind,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        ERFC = IAttr(
            name="ERFC",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=_get_real_with_argone_kind,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        ERFC_SCALED = IAttr(
            name="ERFC_SCALED",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=_get_real_with_argone_kind,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        EVENT_QUERY = IAttr(
            name="EVENT_QUERY",
            is_pure=False,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={"stat": DataNode},
            return_type=None,
            reference_accesses=None,  # FIXME read write write
        )
        EXECUTE_COMMAND_LINE = IAttr(
            name="EXECUTE_COMMAND_LINE",
            is_pure=False,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={
                "wait": DataNode,
                "exitstat": DataNode,
                "cmdstat": DataNode,
                "cmdmsg": DataNode,
            },
            return_type=None,
            reference_accesses=None,  # FIXME read read write write write
        )
        EXP = IAttr(
            name="EXP",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        EXPONENT = IAttr(
            name="EXPONENT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=INTEGER_TYPE,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        EXTENDS_TYPE_OF = IAttr(
            name="EXTENDS_TYPE_OF",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={},
            return_type=BOOLEAN_TYPE,
            reference_accesses=None,  # FIXME Inquiry
        )
        FAILED_IMAGES = IAttr(
            name="FAILED_IMAGES",
            is_pure=False,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(0, 0, DataNode),
            optional_args={"team": DataNode, "kind": DataNode},
            return_type=lambda node: ArrayType(
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
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        FINDLOC = IAttr(
            name="FINDLOC",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(2, 3, DataNode),
            optional_args={"mask": DataNode,
                           "kind": DataNode,
                           "back": DataNode},
            return_type=_findloc_return_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        FLOAT = IAttr(
            name="FLOAT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=REAL_TYPE,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        FLOOR = IAttr(
            name="FLOOR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"kind": DataNode},
            return_type=_get_integer_with_optional_kind,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        FRACTION = IAttr(
            name="FRACTION",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        GAMMA = IAttr(
            name="GAMMA",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        GET_COMMAND = IAttr(
            name="GET_COMMAND",
            is_pure=False,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(0, 0, DataNode),
            optional_args={
                "command": DataNode,
                "length": DataNode,
                "status": DataNode,
                "errmsg": DataNode,
            },
            return_type=None,
            reference_accesses=None,  # FIXME WRITE, WRITE, WRITE
        )
        GET_COMMAND_ARGUMENT = IAttr(
            name="GET_COMMAND_ARGUMENT",
            is_pure=False,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={
                "value": DataNode,
                "length": DataNode,
                "status": DataNode,
                # FIXME THis doesn't take errmsg.
                "errmsg": DataNode,
            },
            return_type=None,
            reference_accesses=None,  # FIXME READ, Write, Write, Write
        )
        GET_ENVIRONMENT_VARIABLE = IAttr(
            name="GET_ENVIRONMENT_VARIABLE",
            is_pure=False,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={
                "value": DataNode,
                "length": DataNode,
                "status": DataNode,
                "trim_name": DataNode,
                # FIXME This doesn't take errmsg.
                "errmsg": DataNode,
            },
            return_type=None,
            reference_accesses=None,  # FIXME read, write, read, write, read
        )
        GET_TEAM = IAttr(
            name="GET_TEAM",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(0, 0, DataNode),
            optional_args={"level": DataNode},
            # Unsupported return type (TEAM_TYPE from ISO_FORTRAN_ENV).
            return_type=lambda node: UnresolvedType(),
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        HUGE = IAttr(
            name="HUGE",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, (Reference, Literal)),
            optional_args={},
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        HYPOT = IAttr(
            name="HYPOT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, (DataNode)),
            optional_args={},
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        IACHAR = IAttr(
            name="IACHAR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, (DataNode)),
            optional_args={"kind": DataNode},
            return_type=_get_integer_with_optional_kind,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        IALL = IAttr(
            name="IALL",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            # FIXME Note to reviewer I think this should be
            # required_args=ArgDesc(1, 2, (DataNode)),
            # optional_args={"mask": DataNode}
            # See https://gcc.gnu.org/onlinedocs/gfortran/IALL.html
            # If this changes and "dim" is no longer a named argument, the
            # return type function will not be correct.
            required_args=ArgDesc(1, 1, (DataNode)),
            optional_args={"dim": DataNode, "mask": DataNode},
            # There is no kind, but this call will work.
            return_type=_get_integer_of_kind_with_optional_dim,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        IAND = IAttr(
            name="IAND",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, (DataNode)),
            optional_args={},
            return_type=lambda node: (
                node.arguments[0].datatype.copy()
                if not isinstance(node.arguments[0], Literal)
                else node.arguments[1].datatype.copy()
            ),
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        IANY = IAttr(
            name="IANY",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            # FIXME Note to reviewer I think this should be
            # required_args=ArgDesc(1, 2, (DataNode)),
            # optional_args={"mask": DataNode}
            # See https://gcc.gnu.org/onlinedocs/gfortran/IANY.html
            # If this changes and "dim" is no longer a named argument, the
            # return type function will not be correct.
            required_args=ArgDesc(1, 1, (DataNode)),
            optional_args={"dim": DataNode, "mask": DataNode},
            # There is no kind, but this call will work.
            return_type=_get_integer_of_kind_with_optional_dim,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        IBCLR = IAttr(
            name="IBCLR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, (DataNode)),
            optional_args={},
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        IBITS = IAttr(
            name="IBITS",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(3, 3, (DataNode)),
            optional_args={},
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        IBSET = IAttr(
            name="IBSET",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, (DataNode)),
            optional_args={},
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        ICHAR = IAttr(
            name="ICHAR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, (DataNode)),
            optional_args={"kind": DataNode},
            return_type=_get_integer_with_optional_kind,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        IEOR = IAttr(
            name="IEOR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, (DataNode)),
            optional_args={},
            return_type=lambda node: (
                node.arguments[0].datatype.copy()
                if not isinstance(node.arguments[0], Literal)
                else node.arguments[1].datatype.copy()
            ),
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        IMAGE_INDEX = IAttr(
            name="IMAGE_INDEX",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(2, 3, (DataNode)),
            optional_args={},
            return_type=INTEGER_TYPE,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        IMAGE_STATUS = IAttr(
            name="IMAGE_STATUS",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, (DataNode)),
            optional_args={"team": DataNode},
            return_type=INTEGER_TYPE,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        INDEX = IAttr(
            name="INDEX",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, (DataNode)),
            optional_args={"back": DataNode, "kind": DataNode},
            return_type=_get_integer_with_optional_kind,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        INT = IAttr(
            name="INT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, (DataNode)),
            optional_args={"kind": DataNode},
            return_type=_int_return_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        IOR = IAttr(
            name="IOR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, (DataNode)),
            optional_args={},
            return_type=lambda node: ScalarType(
                ScalarType.Intrinsic.INTEGER,
                (
                    node.arguments[0].datatype.precision
                    if not isinstance(node.arguments[0], Literal)
                    else node.arguments[1].datatype.precision
                ),
            ),
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        IPARITY = IAttr(
            name="IPARITY",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, 2, (DataNode)),
            optional_args={"mask": DataNode},
            return_type=_iparity_return_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        IS_CONTIGUOUS = IAttr(
            name="IS_CONTIGUOUS",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, (DataNode)),
            optional_args={},
            return_type=BOOLEAN_TYPE,
            reference_accesses=None,  # FIXME Inquiry
        )
        IS_IOSTAT_END = IAttr(
            name="IS_IOSTAT_END",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, (DataNode)),
            optional_args={},
            return_type=BOOLEAN_TYPE,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        IS_IOSTAT_EOR = IAttr(
            name="IS_IOSTAT_EOR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, (DataNode)),
            optional_args={},
            return_type=BOOLEAN_TYPE,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        ISHFT = IAttr(
            name="ISHFT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, (DataNode)),
            optional_args={},
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        ISHFTC = IAttr(
            name="ISHFTC",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, (DataNode)),
            optional_args={"size": DataNode},
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        KIND = IAttr(
            name="KIND",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, (DataNode)),
            optional_args={},
            return_type=INTEGER_TYPE,
            reference_accesses=None,  # FIXME Inquiry
        )
        LBOUND = IAttr(
            name="LBOUND",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, (DataNode)),
            optional_args={"dim": DataNode, "kind": DataNode},
            return_type=_get_bound_function_return_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        LCOBOUND = IAttr(
            name="LCOBOUND",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, (DataNode)),
            optional_args={"dim": DataNode, "kind": DataNode},
            return_type=_get_bound_function_return_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        LEADZ = IAttr(
            name="LEADZ",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, (DataNode)),
            optional_args={},
            return_type=INTEGER_TYPE,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        LEN = IAttr(
            name="LEN",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, (DataNode)),
            optional_args={"kind": DataNode},
            return_type=_get_integer_with_optional_kind,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        LEN_TRIM = IAttr(
            name="LEN_TRIM",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, (DataNode)),
            optional_args={"kind": DataNode},
            return_type=_get_integer_with_optional_kind,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        LGE = IAttr(
            name="LGE",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, (DataNode)),
            optional_args={},
            return_type=BOOLEAN_TYPE,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        LGT = IAttr(
            name="LGT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, (DataNode)),
            optional_args={},
            return_type=BOOLEAN_TYPE,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        LLE = IAttr(
            name="LLE",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, (DataNode)),
            optional_args={},
            return_type=BOOLEAN_TYPE,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        LLT = IAttr(
            name="LLT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, (DataNode)),
            optional_args={},
            return_type=BOOLEAN_TYPE,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        LOG = IAttr(
            name="LOG",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, (DataNode)),
            optional_args={},
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        LOG_GAMMA = IAttr(
            name="LOG_GAMMA",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, (DataNode)),
            optional_args={},
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        LOG10 = IAttr(
            name="LOG10",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, (DataNode)),
            optional_args={},
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        LOGICAL = IAttr(
            name="LOGICAL",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, (DataNode)),
            optional_args={"kind": DataNode},
            return_type=_get_first_argument_type_with_optional_kind,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        MASKL = IAttr(
            name="MASKL",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, (DataNode)),
            optional_args={"kind": DataNode},
            return_type=_get_integer_with_optional_kind,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        MASKR = IAttr(
            name="MASKR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, (DataNode)),
            optional_args={"kind": DataNode},
            return_type=_get_integer_with_optional_kind,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        MATMUL = IAttr(
            name="MATMUL",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={},
            return_type=_matmul_return_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        MAX = IAttr(
            name="MAX",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, None, DataNode),
            optional_args={},
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        MAXEXPONENT = IAttr(
            name="MAXEXPONENT",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=INTEGER_TYPE,
            reference_accesses=None,  # FIXME Inquiry
        )
        MAXLOC = IAttr(
            name="MAXLOC",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, 2, DataNode),
            optional_args={
                "dim": DataNode,
                "mask": DataNode,
                "kind": DataNode,
                "back": DataNode,
            },
            return_type=(
                _get_first_argument_intrinsic_with_optional_kind_and_dim
            ),
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        MAXVAL = IAttr(
            name="MAXVAL",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"dim": DataNode, "mask": DataNode},
            return_type=_maxval_return_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        MERGE = IAttr(
            name="MERGE",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(3, 3, DataNode),
            optional_args={},
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        MERGE_BITS = IAttr(
            name="MERGE_BITS",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(3, 3, DataNode),
            optional_args={},
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        MIN = IAttr(
            name="MIN",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, None, DataNode),
            optional_args={},
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        MINEXPONENT = IAttr(
            name="MINEXPONENT",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=INTEGER_TYPE,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
            reference_accesses=None,  # FIXME Inquiry function
        )
        MINLOC = IAttr(
            name="MINLOC",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, 2, DataNode),
            optional_args={
                "dim": DataNode,
                "mask": DataNode,
                "kind": DataNode,
                "back": DataNode,
            },
            return_type=(
                _get_first_argument_intrinsic_with_optional_kind_and_dim
            ),
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        MINVAL = IAttr(
            name="MINVAL",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"dim": DataNode, "mask": DataNode},
            return_type=_maxval_return_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        MOD = IAttr(
            name="MOD",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={},
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        MODULO = IAttr(
            name="MODULO",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={},
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        MOVE_ALLOC = IAttr(
            name="MOVE_ALLOC",
            is_pure=False,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            # FIXME No stat or errmsg arguments...
            optional_args={"stat": DataNode, "errmsg": DataNode},
            return_type=None,
            reference_accesses=None,  # FIXME Readwrite, write
        )
        MVBITS = IAttr(
            name="MVBITS",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(5, 5, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,  # FIXME read read read write read
        )
        NEAREST = IAttr(
            name="NEAREST",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={},
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        NEW_LINE = IAttr(
            name="NEW_LINE",
            is_pure=True,
            is_elemental=True,
            # FIXME This is inquiry
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=CHARACTER_TYPE,
            reference_accesses=None,  # FIXME inquiry
        )
        NINT = IAttr(
            name="NINT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"kind": DataNode},
            return_type=INTEGER_TYPE,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        NORM2 = IAttr(
            name="NORM2",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, 2, DataNode),
            optional_args={},
            # No kind on NORM2 but this function works for return type.
            # FIXME Check this is correct.
            return_type=(
                _get_first_argument_intrinsic_with_optional_kind_and_dim
            ),
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        NOT = IAttr(
            name="NOT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=lambda node: ScalarType(
                ScalarType.Intrinsic.INTEGER,
                node.arguments[0].datatype.precision
            ),
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        NULL = IAttr(
            name="NULL",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(0, 0, DataNode),
            optional_args={"mold": DataNode},
            # Returns a dissociated pointed - not supported.
            return_type=lambda node: UnresolvedType(),
            reference_accesses=None,  # FIXME type info
        )
        NUM_IMAGES = IAttr(
            name="NUM_IMAGES",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(0, 1, DataNode),
            optional_args={},
            return_type=INTEGER_TYPE,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        OUT_OF_RANGE = IAttr(
            name="OUT_OF_RANGE",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={"round": DataNode},
            return_type=BOOLEAN_TYPE,
            reference_accesses=None,  # FIXME Read, typeinfo, read
        )
        PACK = IAttr(
            name="PACK",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={"vector": DataNode},
            return_type=lambda node: ArrayType(
                ScalarType(
                    node.arguments[0].datatype.intrinsic,
                    node.arguments[0].datatype.precision),
                [ArrayType.Extent.DEFERRED]
            ),
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        PARITY = IAttr(
            name="PARITY",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"dim": DataNode},
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        POPCNT = IAttr(
            name="POPCNT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=INTEGER_TYPE,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        POPPAR = IAttr(
            name="POPPAR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=INTEGER_TYPE,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        PRECISION = IAttr(
            name="PRECISION",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=INTEGER_TYPE,
            reference_accesses=None,  # FIXME Inquiry
        )
        PRESENT = IAttr(
            name="PRESENT",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=BOOLEAN_TYPE,
            reference_accesses=None,  # FIXME Inquiry
        )
        PRODUCT = IAttr(
            name="PRODUCT",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"dim": DataNode, "mask": DataNode},
            return_type=lambda node: (
                _get_first_argument_specified_kind_with_optional_dim(
                    node, node.arguments[0].datatype.intrinsic
                )
            ),
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        RADIX = IAttr(
            name="RADIX",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=INTEGER_TYPE,
            reference_accesses=None,  # FIXME Inquiry
        )
        RANDOM_INIT = IAttr(
            name="RANDOM_INIT",
            is_pure=False,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        RANDOM_NUMBER = IAttr(
            name="RANDOM_NUMBER",
            is_pure=False,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, Reference),
            optional_args={},
            return_type=None,
            reference_accesses=None,  # FIXME write
        )
        RANDOM_SEED = IAttr(
            name="RANDOM_SEED",
            is_pure=False,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(0, 0, Reference),
            optional_args={"size": DataNode, "put": DataNode, "Get": DataNode},
            return_type=None,
            reference_accesses=None,  # FIXME write, read, write
        )
        RANGE = IAttr(
            name="RANGE",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, Reference),
            optional_args={},
            return_type=INTEGER_TYPE,
            reference_accesses=None,  # FIXME inquiry
        )
        RANK = IAttr(
            name="RANK",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, Reference),
            optional_args={},
            return_type=INTEGER_TYPE,
            reference_accesses=None,  # FIXME Inquiry
        )
        REAL = IAttr(
            name="REAL",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, Reference),
            optional_args={"kind": DataNode},
            return_type=lambda node: (
                ScalarType(
                    ScalarType.Intrinsic.REAL,
                    (
                        node.arguments[node.argument_names.index("kind")]
                        if "kind" in node.argument_names
                        else node.arguments[0].datatype.precision
                    ),
                )
            ),
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        REDUCE = IAttr(
            name="REDUCE",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(2, 3, DataNode),
            optional_args={"mask": DataNode,
                           "identity": DataNode,
                           "ordered": DataNode},
            return_type=_reduce_return_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        REPEAT = IAttr(
            name="REPEAT",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, Reference),
            optional_args={},
            return_type=CHARACTER_TYPE,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        RESHAPE = IAttr(
            name="RESHAPE",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, Reference),
            optional_args={"pad": DataNode, "order": DataNode},
            # I went with unresolved for now as the result depends on
            # argument 2 (even the dimensionality).
            return_type=lambda node: UnresolvedType(),
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        RRSPACING = IAttr(
            name="RRSPACING",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, Reference),
            optional_args={},
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        SAME_TYPE_AS = IAttr(
            name="SAME_TYPE_AS",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(2, 2, Reference),
            optional_args={},
            return_type=BOOLEAN_TYPE,
            reference_accesses=None,  # FIXME Inquiry
        )
        SCALE = IAttr(
            name="SCALE",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, Reference),
            optional_args={},
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        SCAN = IAttr(
            name="SCAN",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, Reference),
            optional_args={"back": DataNode, "kind": DataNode},
            return_type=_get_integer_with_optional_kind,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        SELECTED_CHAR_KIND = IAttr(
            name="SELECTED_CHAR_KIND",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            # FIXME this should be DataNode?
            required_args=ArgDesc(1, 1, Reference),
            optional_args={},
            return_type=INTEGER_TYPE,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        SELECTED_INT_KIND = IAttr(
            name="SELECTED_INT_KIND",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            # FIXME this should be DataNode?
            required_args=ArgDesc(1, 1, Reference),
            optional_args={},
            return_type=INTEGER_TYPE,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        SELECTED_REAL_KIND = IAttr(
            name="SELECTED_REAL_KIND",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(0, 0, Reference),
            optional_args={"P": DataNode, "R": DataNode, "radix": DataNode},
            return_type=INTEGER_TYPE,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        SET_EXPONENT = IAttr(
            name="SET_EXPONENT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, Reference),
            optional_args={},
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        SHAPE = IAttr(
            name="SHAPE",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, Reference),
            optional_args={"kind": DataNode},
            return_type=lambda node: (
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
            reference_accesses=None,  # FIXME Inquiry, typeinfo
        )
        SHIFTA = IAttr(
            name="SHIFTA",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, Reference),
            optional_args={},
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        SHIFTL = IAttr(
            name="SHIFTL",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, Reference),
            optional_args={},
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        SHIFTR = IAttr(
            name="SHIFTR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, Reference),
            optional_args={},
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        SIGN = IAttr(
            name="SIGN",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={},
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        SIN = IAttr(
            name="SIN",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        SINH = IAttr(
            name="SINH",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        SIZE = IAttr(
            name="SIZE",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"dim": DataNode, "kind": DataNode},
            return_type=_get_integer_with_optional_kind,
            reference_accesses=None,  # FIXME Inquiry, read, type_info
        )
        SPACING = IAttr(
            name="SPACING",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=_get_first_argument_type,
            reference_accesses=None,  # FIXME inquiry
        )
        SPREAD = IAttr(
            name="SPREAD",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(3, 3, DataNode),
            optional_args={},
            return_type=lambda node: ArrayType(
                ScalarType(
                    node.arguments[0].datatype.intrinsic,
                    node.arguments[0].datatype.precision),
                ([ArrayType.Extent.DEFERRED] *
                 (len(node.arguments[0].datatype.shape) + 1)
                 if isinstance(node.arguments[0].datatype, ArrayType) else
                 [ArrayType.Extent.DEFERRED])
            ),
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        SQRT = IAttr(
            name="SQRT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            # FIXME For reviewer - I put unresolved type because it can return
            # COMPLEX depending on input.
            return_type=lambda node: UnresolvedType(),
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        STOPPED_IMAGES = IAttr(
            name="STOPPED_IMAGES",
            is_pure=False,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(0, 0, DataNode),
            optional_args={"team": DataNode, "kind": DataNode},
            return_type=lambda node: ArrayType(
                _get_integer_with_optional_kind(node),
                [ArrayType.Extent.DEFERRED]
            ),
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        STORAGE_SIZE = IAttr(
            name="STORAGE_SIZE",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"kind": DataNode},
            return_type=_get_integer_with_optional_kind,
            reference_accesses=None,  # FIXME Inquiry
        )
        SUM = IAttr(
            name="SUM",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            # FIXME Think this is wrong again - 2nd argument can be non-named
            # dim?
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"dim": DataNode, "mask": DataNode},
            return_type=lambda node: (
                _get_first_argument_specified_kind_with_optional_dim(
                    node, node.arguments[0].datatype.intrinsic
                )
            ),
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        SYSTEM_CLOCK = IAttr(
            name="SYSTEM_CLOCK",
            is_pure=False,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(0, 0, DataNode),
            optional_args={"count": DataNode,
                           "count_rate": DataNode,
                           "count_max": DataNode},
            return_type=None,
            reference_accesses=None,  # FIXME write, write, write
        )
        TAN = IAttr(
            name="TAN",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        TANH = IAttr(
            name="TANH",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        # FIXME I'm not sure this exists?
        TEAM_IMAGE = IAttr(
            name="TEAM_IMAGE",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(0, 0, DataNode),
            optional_args={"team": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        THIS_IMAGE = IAttr(
            name="THIS_IMAGE",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            # FIXME Again not sure this is correct. Have used unresolved
            # type for the return value - can improve it if wanted.
            required_args=ArgDesc(0, 0, DataNode),
            optional_args={"coarray": DataNode,
                           "team": DataNode,
                           "dim": DataNode},
            return_type=lambda node: UnresolvedType(),
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        TINY = IAttr(
            name="TINY",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, (Reference, Literal)),
            optional_args={},
            return_type=_get_first_argument_type,
            reference_accesses=None,  # FIXME Inquiry
        )
        TRAILZ = IAttr(
            name="TRAILZ",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=INTEGER_TYPE,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        TRANSFER = IAttr(
            name="TRANSFER",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={"size": DataNode},
            return_type=lambda node: (
                node.arguments[1].datatype if
                ("size" not in node.argument_names and
                 not isinstance(node.arguments[1].datatype, ArrayType))
                else ArrayType(
                    ScalarType(node.arguments[1].datatype.intrinsic,
                               node.arguments[1].datatype.precision),
                    [ArrayType.Extent.DEFERRED])

            ),
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        TRANSPOSE = IAttr(
            name="TRANSPOSE",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=lambda node: ArrayType(ScalarType(
                node.arguments[0].datatype.intrinsic,
                node.arguments[0].datatype.precision),
                [node.arguments[0].datatype.shape[1],
                 node.arguments[0].datatype.shape[0]]
            ),
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        TRIM = IAttr(
            name="TRIM",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=CHARACTER_TYPE,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        UBOUND = IAttr(
            name="UBOUND",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"dim": DataNode, "kind": DataNode},
            return_type=_get_bound_function_return_type,
            reference_accesses=None,  # FIXME inquiry, read, type_info
        )
        UCOBOUND = IAttr(
            name="UCOBOUND",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"dim": DataNode, "kind": DataNode},
            return_type=_get_bound_function_return_type,
            reference_accesses=None,  # FIXME inquiry, read, type_info
        )
        UNPACK = IAttr(
            name="UNPACK",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(3, 3, DataNode),
            optional_args={},
            return_type=_get_first_argument_type,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
        )
        VERIFY = IAttr(
            name="VERIFY",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={"back": DataNode, "kind": DataNode},
            return_type=_get_integer_with_optional_kind,
            reference_accesses=(
                _reference_accesses_all_reads_with_optional_kind
            ),
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
