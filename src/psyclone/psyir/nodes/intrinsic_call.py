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

from __future__ import annotations

from collections import namedtuple
from collections.abc import Iterable
from enum import Enum
from typing import List, Tuple, Callable

from psyclone.core import AccessType, VariablesAccessMap
from psyclone.errors import InternalError
from psyclone.psyir.nodes.operation import BinaryOperation
from psyclone.psyir.nodes.call import Call
from psyclone.psyir.nodes.datanode import DataNode
from psyclone.psyir.nodes.literal import Literal
from psyclone.psyir.nodes.reference import Reference
from psyclone.psyir.symbols import IntrinsicSymbol, Symbol
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
    UnsupportedFortranType,
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

# Named tuple for describing the properties of the required arguments to
# a particular intrinsic. If there's no limit on the number of arguments
# then `max_count` will be None. If max_count is not None, then arg_names
# will contain a list of the argument names of the required arguments, in
# the order defined by the standard. If max_count is None, arg_names will
# be a tuple containing None to ensure the argument name computation logic
# still works.
ArgDesc = namedtuple('ArgDesc', 'min_count max_count types arg_names')


def _get_named_argument_type(node: IntrinsicCall,
                             argument_name: str) -> DataType:
    """Helper function for the common IntrinsicCall case where
    the return type matches exactly the datatype of the
    argument with the provided argument_name.

    :param node: The IntrinsicCall whose return type to compute.
    :param argument_name: The name of the argument whose datatype to return.

    :returns: the datatype of the named argument of the IntrinsicCall.
    """
    return node.argument_by_name(argument_name).datatype


def _type_of_named_arg_with_optional_kind_and_dim(
        node: IntrinsicCall, arg_name: str
) -> DataType:
    """Helper function for IntrinsicCalls like MAXLOC where they have optional
    Kind and Dim options but the intrinsic type is that of the argument with
    the provided arg_name.

    :param node: The IntrinsicCall whose return type to compute.
    :param arg_name: The name of the argument that provides the intrinsic
                     type.

    :returns: the computed datatype for the IntrinsicCall.
    """
    arg = node.argument_by_name(arg_name)
    if "kind" in node.argument_names:
        dtype = ScalarType(
            arg.datatype.intrinsic,
            node.argument_by_name("kind").copy(),
        )
    else:
        # PSyclone has the UNDEFINED Precision as the default kind for all
        # supported inbuilt datatypes.
        dtype = ScalarType(
            arg.datatype.intrinsic,
            ScalarType.Precision.UNDEFINED,
        )
    # If "dim" argument isn't present then the result is an array of the same
    # dimension as the arg_name argument.
    if "dim" not in node.argument_names:
        return ArrayType(
            dtype,
            [
                Literal(
                    str(len(
                        arg.datatype.shape)),
                    INTEGER_TYPE,
                ),
            ],
        )
    # Always have dim argument from here.
    # If array has rank 1, the result is scalar.
    shape = arg.datatype.shape
    if len(shape) == 1:
        return dtype
    # Otherwise the result is an array with rank one less than the
    # arg_name argument.
    new_shape = [ArrayType.Extent.DEFERRED] * (len(shape) - 1)
    return ArrayType(dtype, new_shape)


def _type_with_specified_precision_and_optional_dim(
        node: IntrinsicCall, argument_name: str,
        intrinsic: ScalarType.Intrinsic = ScalarType.Intrinsic.BOOLEAN
        ) -> DataType:
    """Helper function for the common IntrinsicCall case where the
    return type is a Scalar with the precision of a named argument,
    unless an optional argument named 'dim' exists, in which case an array
    with rank one less than the input node is given instead.

    :param node: The IntrinsicCall whose return type to compute.
    :param argument_name: The name of the argument whose precision to be used.
    :param intrinsic: The type of the intrinsic of the resulting datatype.
                      Default is ScalarType.Intrinsic.BOOLEAN

    :returns: the computed datatype for the IntrinsicCall.
    """
    dtype = ScalarType(
        intrinsic, node.argument_by_name(argument_name).datatype.precision
    )
    # If dim is not present, or the rank of the
    # array argument is 1 then this returns a scalar.
    arg = node.argument_by_name(argument_name)
    shape = arg.datatype.shape
    if "dim" not in node.argument_names or len(shape) == 1:
        return dtype
    # If dim is given then this should return an array, but we
    # don't necessarily know the dimensions of the resulting array
    # at compile time. It will have one fewer dimension than the
    # input.
    # For now we don't attempt to work out the shape.
    new_shape = [ArrayType.Extent.DEFERRED] * (len(shape) - 1)
    return ArrayType(dtype, new_shape)


def _type_of_scalar_with_optional_kind(
        node: IntrinsicCall, intrinsic: ScalarType.Intrinsic,
        arg_name: str
) -> DataType:
    """Helper function for the common case where the return type is a
    scalar intrinsic type with an optional kind argument.

    :param node: The IntrinsicCall whose return type to compute.
    :param intrinsic: The intrinsic type for the datatype.
    :param arg_name: The name of the optional argument to use for the kind.

    :returns: the computed datatype for the IntrinsicCall.
    """
    return (
        ScalarType(
            intrinsic,
            node.argument_by_name(arg_name).copy(),
        )
        if arg_name in node.argument_names
        # Otherwise, use the default Precision (in PSyclone this is the
        # UNDEFINED precision)
        else ScalarType(intrinsic, ScalarType.Precision.UNDEFINED)
    )


def _get_intrinsic_of_argname_kind_with_optional_dim(
        node: IntrinsicCall, intrinsic: ScalarType.Intrinsic,
        array_arg_name: str, kind_arg_name: str) -> DataType:
    """Helper function for a datatype of type intrinsic with optional dim and
    optional arg_name kind option. If the dim argument exists, then the
    dimensionality of the result is based on the argument that has the
    array_arg_name.

    :param node: The IntrinsicCall whose return type to compute.
    :param intrinsic: The intrinsic type of the resultant datatype.
    :param array_arg_name: The name of the array type to use to compute
                           dimensionality if dim is requested.
    :param kind_arg_name: The name of the optional argument to base the
                          precision of the resultant datatype on.

    :returns: the computed datatype for the IntrinsicCall.
    """
    dtype = ScalarType(
        intrinsic,
        (
            ScalarType.Precision.UNDEFINED
            if kind_arg_name not in node.argument_names
            else node.argument_by_name(kind_arg_name).copy()
        ),
    )
    # If dim is not present then we return a scalar.
    if "dim" not in node.argument_names:
        return dtype
    # If dim is given then this should return an array, but we
    # don't necessarily know the dimensions of the resulting array
    # at compile time. It will have one fewer dimension than the
    # input.
    arg = node.argument_by_name(array_arg_name)
    shape = arg.datatype.shape
    if len(shape) == 1:
        return dtype
    # For now we don't attempt to work out the shape.
    new_shape = [ArrayType.Extent.DEFERRED] * (len(shape) - 1)
    return ArrayType(dtype, new_shape)


def _get_intrinsic_with_named_arg_precision(
        node: IntrinsicCall, intrinsic: ScalarType.Intrinsic,
        argument_name: str
) -> DataType:
    """Helper function for the common IntrinsicCall case where the
    return type is a scalar of the type of the supplied intrinsic,
    with the kind of the first argument.

    :param node: The IntrinsicCall whose return type to compute.
    :param intrinsic: The datatype intrinsic type to use.
    :param argument_name: The name of the argument to use for the precision
                          of the datatype.

    :returns: the computed datatype for the IntrinsicCall.
    """
    return ScalarType(
        intrinsic, node.argument_by_name(argument_name).datatype.precision
    )


def _findloc_return_type(node: IntrinsicCall) -> DataType:
    """Helper function for the FINDLOC case.

    The datatype of FINDLOC is a rank-one array of dimension equal to
    the "array" named argument, unless dim is present in which case
    the datatype is an N-1 dimension array.
    If the optional argument
    "kind" is present, then the precision of the resulting datatype
    is equal to the kind argument.

    :param node: The IntrinsicCall whose return type to compute.

    :returns: the computed datatype for the IntrinsicCall.
    """
    if "kind" in node.argument_names:
        dtype = ScalarType(
            node.argument_by_name("array").datatype.intrinsic,
            node.argument_by_name("kind").copy(),
        )
    else:
        dtype = node.argument_by_name("array").datatype.copy()
    # If dim argument is given.
    if "dim" in node.argument_names:
        # Return a scalar if the array has rank 1.
        if len(node.argument_by_name("array").datatype.shape) == 1:
            return dtype
        # Otherwise return an array of rank n-1.
        return ArrayType(
            dtype,
            [ArrayType.Extent.DEFERRED]
            * (len(node.argument_by_name("array").datatype.shape) - 1),
        )
    # Otherwise return an array with same rank as the "array"
    # argument.
    return ArrayType(
        dtype,
        [
            Literal(
                str(len(node.argument_by_name(
                            "array"
                        ).datatype.shape)),
                INTEGER_TYPE,
            ),
        ],
    )


def _int_return_type(node: IntrinsicCall) -> DataType:
    """Helper function for the INT case.

    The resulting datatype is an scalar integer of default kind
    (unless the "kind" argument is provided, in which case the kind
    specified by the argument) if "a" is a scalar.

    If "a" is an array, then it is an ArrayType of the same size as
    the input, with the datatype of the scalar type specified in
    the previous paragraph.

    :param node: The IntrinsicCall whose return type to compute.

    :returns: the computed datatype for the IntrinsicCall.
    """
    if "kind" in node.argument_names:
        dtype = ScalarType(
            ScalarType.Intrinsic.INTEGER,
            node.argument_by_name("kind").copy(),
        )
    else:
        dtype = INTEGER_TYPE

    if not isinstance(node.argument_by_name("a").datatype, ArrayType):
        return dtype
    return ArrayType(
        dtype,
        [
            index.copy()
            for index in node.argument_by_name("a").datatype.shape
        ],
    )


def _iparity_return_type(node: IntrinsicCall) -> DataType:
    """Helper function for the IPARITY case.

    The result is the same type as the "array" argument. If the
    "dim" argument is not present, a scalar of that type is returned.
    Otherwise an ArrayType of rank n-1 (where n is the rank of "array") of
    that type is returned instead.

    :param node: The IntrinsicCall whose return type to compute.

    :returns: the computed datatype for the IntrinsicCall.
    """
    dtype = ScalarType(
        node.argument_by_name("array").datatype.intrinsic,
        node.argument_by_name("array").datatype.precision,
    )
    # If dim is not present then we return a scalar.
    if "dim" not in node.argument_names:
        return dtype
    # We have a dimension specified. We don't know the resultant shape
    # in any detail as its dependent on the value of dim
    return ArrayType(
        dtype,
        [ArrayType.Extent.DEFERRED]
        * (len(node.argument_by_name("array").datatype.shape) - 1),
    )


def _get_bound_function_return_type(node: IntrinsicCall) -> DataType:
    """Helper function for the return types of functions like LBOUND and
    LCOBOUND etc.

    The return type is of type integer and of the kind specified by the
    "kind" argument if present, or the default
    (ScalarType.Precision.UNDEFINED) otherwise.
    If the "dim" argument is present, then the result is that ScalarType.
    Otherwise, it's an array of that ScalarType with extent equal to the
    rank of the "array" argument.

    :param node: The IntrinsicCall whose return type to compute.

    :returns: the computed datatype for the IntrinsicCall.
    """
    if "kind" in node.argument_names:
        dtype = ScalarType(
            ScalarType.Intrinsic.INTEGER,
            node.argument_by_name("kind").copy(),
        )
    else:
        dtype = INTEGER_TYPE
    # If "dim" is in the arguments, then return a Scalar.
    if "dim" in node.argument_names:
        return dtype
    # Otherwise return an array with rank equal to the "array" argument.
    return ArrayType(
        dtype,
        [
            Literal(
                str(len(node.argument_by_name("array").datatype.shape)),
                INTEGER_TYPE
            ),
        ],
    )


def _matmul_return_type(node: IntrinsicCall) -> DataType:
    """Helper function for the return type of MATMUL.

    If matrix_a is a vector and matrix_b is a matrix, then the
    result is an array of shape of the first dimension of matrix_b.
    If matrix_b is a vector and matrix_a is a matrix, then the
    result is an array of shape of the first dimension of matrix_a.
    If matrix_a and matrix_b are both matrices, then the result
    is an array of shape [matrix_a dim 1, matrix_b dim 2].

    :param node: The IntrinsicCall whose return type to compute.

    :returns: the computed datatype for the IntrinsicCall.
    """
    argtype1 = node.argument_by_name("matrix_a").datatype
    argtype2 = node.argument_by_name("matrix_b").datatype
    shape1 = argtype1.shape
    shape2 = argtype2.shape
    stype1 = ScalarType(argtype1.intrinsic, argtype1.precision)
    stype2 = ScalarType(argtype2.intrinsic, argtype2.precision)
    # Create a temporary BinaryOperation to use get_result_scalar_type
    arg1 = Reference(DataSymbol("a", stype1))
    arg2 = Reference(DataSymbol("b", stype2))
    binop = BinaryOperation.create(BinaryOperation.Operator.MUL,
                                   arg1, arg2)
    stype = binop.get_result_scalar_type([stype1, stype2])
    #  a11 a12 x b1 = a11*b1 + a12*b2
    #  a21 a22   b2   a21*b1 + a22*b2
    #  a31 a32        a31*b1 + a32*b2
    #  3 x 2 * 2 x 1 = 3 x 1
    #  rank 2  rank 1  rank 1
    # Vector-matrix case.
    if len(shape1) == 1:
        extent = IntrinsicCall.create(
            IntrinsicCall.Intrinsic.SIZE,
            [node.argument_by_name("matrix_b").copy(),
             ("dim", Literal("1", INTEGER_TYPE))])
        shape = [extent]
    # Matrix-vector case.
    elif len(shape2) == 1:
        extent = IntrinsicCall.create(
            IntrinsicCall.Intrinsic.SIZE,
            [node.argument_by_name("matrix_a").copy(),
             ("dim", Literal("1", INTEGER_TYPE))])
        shape = [extent]
    else:
        # matrix-matrix. Result is size(arg0, 1) x size(arg1, 2)
        extent1 = IntrinsicCall.create(
            IntrinsicCall.Intrinsic.SIZE,
            [node.argument_by_name("matrix_a").copy(),
             ("dim", Literal("1", INTEGER_TYPE))])
        extent2 = IntrinsicCall.create(
            IntrinsicCall.Intrinsic.SIZE,
            [node.argument_by_name("matrix_b").copy(),
             ("dim", Literal("2", INTEGER_TYPE))])
        shape = [extent1, extent2]
    return ArrayType(stype, shape)


def _maxval_return_type(node: IntrinsicCall) -> DataType:
    """ Helper function for the MAXVAL (and similar) intrinsic return
    types.

    If the "dim" argument is absent, or the "array" argument has rank one
    then the result is a ScalarType of the type of the "array" argument.
    Otherwise the result is an ArrayType of rank n-1 (where n is the rank of
    the "array" argument) with the same datatype of the "array" argument.

    :param node: The IntrinsicCall whose return type to compute.

    :returns: the computed datatype for the IntrinsicCall.
    """
    dtype = ScalarType(node.argument_by_name("array").datatype.intrinsic,
                       node.argument_by_name("array").datatype.precision)
    if ("dim" not in node.argument_names
            or len(node.argument_by_name("array").datatype.shape) == 1):
        return dtype
    # We have a dimension specified. We don't know the resultant shape
    # in any detail as its dependent on the value of dim
    return ArrayType(
        dtype,
        [ArrayType.Extent.DEFERRED]
        * (len(node.argument_by_name("array").datatype.shape) - 1),
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

        All argument names (i.e. required_args.arg_names and
        optional_args) should be lower case.

        """

        # Fortran special-case statements (technically not Fortran intrinsics
        # but in PSyIR they are represented as Intrinsics)
        # TODO 3060 reference_accesses NYI on Intrinsics, they are currently
        # all set to None.
        ALLOCATE = IAttr(
            name="ALLOCATE",
            is_pure=False,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=None,
                types=Reference,
                arg_names=((None,),)),
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
            required_args=ArgDesc(
                min_count=1,
                max_count=None,
                types=Reference,
                arg_names=((None,),)),
            optional_args={"stat": Reference},
            return_type=None,
            reference_accesses=None,
        )
        NULLIFY = IAttr(
            name="NULLIFY",
            is_pure=False,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=None,
                types=Reference,
                arg_names=((None,),)),
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
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("a",),)),
            optional_args={},
            # TODO 1590 Complex to real conversion unsupported.
            return_type=lambda node: _get_named_argument_type(node, "a"),
            reference_accesses=None,
        )
        ACHAR = IAttr(
            name="ACHAR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("i",),)),
            optional_args={"kind": DataNode},
            return_type=CHARACTER_TYPE,
            reference_accesses=None,
        )
        ACOS = IAttr(
            name="ACOS",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("x",),)),
            optional_args={},
            return_type=lambda node: _get_named_argument_type(node, "x"),
            reference_accesses=None,
        )
        ACOSH = IAttr(
            name="ACOSH",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("x",),)),
            optional_args={},
            return_type=lambda node: _get_named_argument_type(node, "x"),
            reference_accesses=None,
        )
        ADJUSTL = IAttr(
            name="ADJUSTL",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("string",),)),
            optional_args={},
            # TODO 2612 This may be more complex if we support character len
            return_type=lambda node: _get_named_argument_type(node, "string"),
            reference_accesses=None,
        )
        ADJUSTR = IAttr(
            name="ADJUSTR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("string",),)),
            optional_args={},
            # TODO 2612 This may be more complex if we support character len
            return_type=lambda node: _get_named_argument_type(node, "string"),
            reference_accesses=None,
        )
        AIMAG = IAttr(
            name="AIMAG",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("z",),)),
            optional_args={},
            # TODO #1590 Complex numbers' precision unsupported.
            return_type=lambda node: UnsupportedFortranType(""),
            reference_accesses=None,
        )
        AINT = IAttr(
            name="AINT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("a",),)),
            optional_args={"kind": DataNode},
            return_type=lambda node: ScalarType(
                ScalarType.Intrinsic.REAL,
                (
                    node.argument_by_name("kind").copy()
                    if "kind" in node.argument_names
                    else node.argument_by_name("a").datatype.precision
                ),
            ),
            reference_accesses=None,
        )
        ALL = IAttr(
            name="ALL",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("mask",),)),
            optional_args={"dim": DataNode},
            return_type=(
                lambda node:
                _type_with_specified_precision_and_optional_dim(
                    node, "mask"
                )
            ),
            reference_accesses=None,
        )
        ALLOCATED = IAttr(
            name="ALLOCATED",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            # Argname of allocated depends on the input.
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("",),)),
            optional_args={},
            return_type=BOOLEAN_TYPE,
            reference_accesses=None,
        )
        ANINT = IAttr(
            name="ANINT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("a",),)),
            optional_args={"kind": DataNode},
            return_type=lambda node: ScalarType(
                ScalarType.Intrinsic.REAL,
                (
                    node.argument_by_name("kind").copy()
                    if "kind" in node.argument_names
                    else node.argument_by_name("a").datatype.precision
                ),
            ),
            reference_accesses=None,
        )
        ANY = IAttr(
            name="ANY",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("mask",),)),
            optional_args={"dim": DataNode},
            return_type=(
                lambda node:
                _type_with_specified_precision_and_optional_dim(
                    node, "mask"
                )
            ),
            reference_accesses=None,
        )
        ASIN = IAttr(
            name="ASIN",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("x",),)),
            optional_args={},
            return_type=lambda node: _get_named_argument_type(node, "x"),
            reference_accesses=None,
        )
        ASINH = IAttr(
            name="ASINH",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("x",),)),
            optional_args={},
            return_type=lambda node: _get_named_argument_type(node, "x"),
            reference_accesses=None,
        )
        ASSOCIATED = IAttr(
            name="ASSOCIATED",
            is_pure=False,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("pointer",),)),
            optional_args={"target": DataNode},
            return_type=BOOLEAN_TYPE,
            reference_accesses=None,
        )
        ATAN = IAttr(
            name="ATAN",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=2,
                types=DataNode,
                arg_names=(("x",), ("y", "x"))),
            optional_args={},
            return_type=lambda node: _get_named_argument_type(node, "x"),
            reference_accesses=None,
        )
        ATAN2 = IAttr(
            name="ATAN2",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(("y", "x"),)),
            optional_args={},
            return_type=lambda node: _get_named_argument_type(node, "y"),
            reference_accesses=None,
        )
        ATANH = IAttr(
            name="ATANH",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("x",),)),
            optional_args={},
            return_type=lambda node: _get_named_argument_type(node, "x"),
            reference_accesses=None,
        )
        ATOMIC_ADD = IAttr(
            name="ATOMIC_ADD",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(("atom", "value"),)),
            optional_args={"stat": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        ATOMIC_AND = IAttr(
            name="ATOMIC_AND",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(("atom", "value"),)),
            optional_args={"stat": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        ATOMIC_CAS = IAttr(
            name="ATOMIC_CAS",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=4,
                max_count=4,
                types=DataNode,
                arg_names=(
                    ("atom", "old", "compare", "new"),
                )
            ),
            optional_args={"stat": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        ATOMIC_DEFINE = IAttr(
            name="ATOMIC_DEFINE",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(("atom", "value"),)),
            optional_args={"stat": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        ATOMIC_FETCH_ADD = IAttr(
            name="ATOMIC_FETCH_ADD",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=3,
                max_count=3,
                types=DataNode,
                arg_names=(
                    ("atom", "value", "old"),
                )
            ),
            optional_args={"stat": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        ATOMIC_FETCH_AND = IAttr(
            name="ATOMIC_FETCH_AND",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=3,
                max_count=3,
                types=DataNode,
                arg_names=(
                    ("atom", "value", "old"),
                )
            ),
            optional_args={"stat": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        ATOMIC_FETCH_OR = IAttr(
            name="ATOMIC_FETCH_OR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=3,
                max_count=3,
                types=DataNode,
                arg_names=(
                    ("atom", "value", "old"),
                )
            ),
            optional_args={"stat": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        ATOMIC_FETCH_XOR = IAttr(
            name="ATOMIC_FETCH_XOR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=3,
                max_count=3,
                types=DataNode,
                arg_names=(
                    ("atom", "value", "old"),
                )
            ),
            optional_args={"stat": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        ATOMIC_OR = IAttr(
            name="ATOMIC_OR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(("atom", "value"),)),
            optional_args={"stat": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        ATOMIC_REF = IAttr(
            name="ATOMIC_REF",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(("value", "atom"),)),
            optional_args={"stat": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        ATOMIC_XOR = IAttr(
            name="ATOMIC_XOR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(("atom", "value"),)),
            optional_args={"stat": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        BESSEL_J0 = IAttr(
            name="BESSEL_J0",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("x",),)),
            optional_args={},
            return_type=lambda node:
                _get_intrinsic_with_named_arg_precision(
                    node, ScalarType.Intrinsic.REAL,
                    "x"
                ),
            reference_accesses=None,
        )
        BESSEL_J1 = IAttr(
            name="BESSEL_J1",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("x",),)),
            optional_args={},
            return_type=lambda node:
                _get_intrinsic_with_named_arg_precision(
                    node, ScalarType.Intrinsic.REAL,
                    "x"
                ),
            reference_accesses=None,
        )
        BESSEL_JN = IAttr(
            name="BESSEL_JN",
            is_pure=True,
            # TODO 3141 The elemental status is dependent on the
            # structure of the IntrinsicCall.
            is_elemental=None,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=3,
                types=DataNode,
                arg_names=(
                    ("n", "x"),
                    ("n1", "n2", "x"),
                )
            ),
            optional_args={},
            return_type=lambda node:
                _get_intrinsic_with_named_arg_precision(
                    node, ScalarType.Intrinsic.REAL,
                    "x"
                ),
            reference_accesses=None,
        )
        BESSEL_Y0 = IAttr(
            name="BESSEL_Y0",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("x",),)),
            optional_args={},
            return_type=lambda node:
                _get_intrinsic_with_named_arg_precision(
                    node, ScalarType.Intrinsic.REAL,
                    "x"
                ),
            reference_accesses=None,
        )
        BESSEL_Y1 = IAttr(
            name="BESSEL_Y1",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("x",),)),
            optional_args={},
            return_type=lambda node:
                _get_intrinsic_with_named_arg_precision(
                    node, ScalarType.Intrinsic.REAL,
                    "x"
                ),
            reference_accesses=None,
        )
        BESSEL_YN = IAttr(
            name="BESSEL_YN",
            is_pure=True,
            # TODO 3141 The elemental status is dependent on the
            # structure of the IntrinsicCall.
            is_elemental=None,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=3,
                types=DataNode,
                arg_names=(
                    ("n", "x"),
                    ("n1", "n2", "x"),
                )
            ),
            optional_args={},
            return_type=lambda node:
                _get_intrinsic_with_named_arg_precision(
                    node, ScalarType.Intrinsic.REAL,
                    "x"
                ),
            reference_accesses=None,
        )
        BGE = IAttr(
            name="BGE",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(("i", "j"),)),
            optional_args={},
            return_type=BOOLEAN_TYPE,
            reference_accesses=None,
        )
        BGT = IAttr(
            name="BGT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(("i", "j"),)),
            optional_args={},
            return_type=BOOLEAN_TYPE,
            reference_accesses=None,
        )
        BIT_SIZE = IAttr(
            name="BIT_SIZE",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("i",),)),
            optional_args={},
            return_type=INTEGER_TYPE,
            reference_accesses=None,
        )
        BLE = IAttr(
            name="BLE",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(("i", "j"),)),
            optional_args={},
            return_type=BOOLEAN_TYPE,
            reference_accesses=None,
        )
        BLT = IAttr(
            name="BLT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(("i", "j"),)),
            optional_args={},
            return_type=BOOLEAN_TYPE,
            reference_accesses=None,
        )
        BTEST = IAttr(
            name="BTEST",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(("i", "pos"),)),
            optional_args={},
            return_type=BOOLEAN_TYPE,
            reference_accesses=None,
        )
        CEILING = IAttr(
            name="CEILING",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("a",),)),
            optional_args={"kind": DataNode},
            return_type=lambda node:
                _type_of_scalar_with_optional_kind(
                    node, ScalarType.Intrinsic.INTEGER,
                    "kind"
                ),
            reference_accesses=None,
        )
        CHAR = IAttr(
            name="CHAR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("i",),)),
            optional_args={"kind": DataNode},
            return_type=CHARACTER_TYPE,
            reference_accesses=None,
        )
        CMPLX = IAttr(
            name="CMPLX",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("x",),)),
            optional_args={"y": DataNode, "kind": DataNode},
            # TODO #1590 Complex numbers unsupported.
            return_type=lambda node: UnsupportedFortranType(""),
            reference_accesses=None,
        )
        CO_BROADCAST = IAttr(
            name="CO_BROADCAST",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(
                    ("a", "source_image"),
                )
             ),
            optional_args={"stat": DataNode, "errmsg": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        CO_MAX = IAttr(
            name="CO_MAX",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("a",),)),
            optional_args={"result_image": DataNode,
                           "stat": DataNode,
                           "errmsg": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        CO_MIN = IAttr(
            name="CO_MIN",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("a",),)),
            optional_args={"result_image": DataNode,
                           "stat": DataNode,
                           "errmsg": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        CO_REDUCE = IAttr(
            name="CO_REDUCE",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(
                    ("a", "operation"),
                )
            ),
            optional_args={"result_image": DataNode,
                           "stat": DataNode,
                           "errmsg": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        CO_SUM = IAttr(
            name="CO_SUM",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("a",),)),
            optional_args={"result_image": DataNode,
                           "stat": DataNode,
                           "errmsg": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        COMMAND_ARGUMENT_COUNT = IAttr(
            name="COMMAND_ARGUMENT_COUNT",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=0,
                max_count=0,
                types=None,
                arg_names=()),
            optional_args={},
            return_type=INTEGER_TYPE,
            reference_accesses=None,
        )
        CONJG = IAttr(
            name="CONJG",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("z",),)),
            optional_args={},
            # TODO #1590 Complex numbers unsupported.
            return_type=lambda node: UnsupportedFortranType(""),
            reference_accesses=None,
        )
        COS = IAttr(
            name="COS",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("x",),)),
            optional_args={},
            return_type=lambda node: _get_named_argument_type(node, "x"),
            reference_accesses=None,
        )
        COSH = IAttr(
            name="COSH",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("x",),)),
            optional_args={},
            return_type=lambda node: _get_named_argument_type(node, "x"),
            reference_accesses=None,
        )
        COSHAPE = IAttr(
            name="COSHAPE",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("coarray",),)),
            optional_args={"kind": DataNode},
            return_type=lambda node: ArrayType(
                ScalarType(
                    ScalarType.Intrinsic.INTEGER,
                    (
                        node.argument_by_name("coarray").datatype.precision
                        if "kind" not in node.argument_names
                        else node.argument_by_name("kind").copy()
                    ),
                ),
                [
                    index.copy()
                    for index in node.argument_by_name(
                        "coarray"
                    ).datatype.shape
                ],
            ),
            reference_accesses=None,
        )
        COUNT = IAttr(
            name="COUNT",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("mask",),)),
            optional_args={"dim": DataNode, "kind": DataNode},
            return_type=lambda node:
                _get_intrinsic_of_argname_kind_with_optional_dim(
                    node, ScalarType.Intrinsic.INTEGER,
                    "mask", "kind"
                ),
            reference_accesses=None,
        )
        CPU_TIME = IAttr(
            name="CPU_TIME",
            is_pure=False,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("time",),)),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        CSHIFT = IAttr(
            name="CSHIFT",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(("array", "shift"),)),
            optional_args={"dim": DataNode},
            return_type=lambda node: _get_named_argument_type(node, "array"),
            reference_accesses=None,
        )
        DATE_AND_TIME = IAttr(
            name="DATE_AND_TIME",
            is_pure=False,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=0,
                max_count=0,
                types=DataNode,
                arg_names=()),
            optional_args={
                "date": DataNode,
                "time": DataNode,
                "zone": DataNode,
                "values": DataNode,
            },
            return_type=None,
            reference_accesses=None,
        )
        DBLE = IAttr(
            name="DBLE",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("a",),)),
            optional_args={},
            return_type=REAL_DOUBLE_TYPE,
            reference_accesses=None,
        )
        DIGITS = IAttr(
            name="DIGITS",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("x",),)),
            optional_args={},
            return_type=INTEGER_TYPE,
            reference_accesses=None,
        )
        DIM = IAttr(
            name="DIM",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(("x", "y"),)),
            optional_args={},
            return_type=lambda node: _get_named_argument_type(node, "x"),
            reference_accesses=None,
        )
        DOT_PRODUCT = IAttr(
            name="DOT_PRODUCT",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(("vector_a", "vector_b"),)
            ),
            optional_args={},
            return_type=lambda node: ScalarType(
                node.argument_by_name("vector_a").datatype.intrinsic,
                node.argument_by_name("vector_a").datatype.precision
            ),
            reference_accesses=None,
        )
        DPROD = IAttr(
            name="DPROD",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(("x", "y"),)),
            optional_args={},
            return_type=REAL8_TYPE,
            reference_accesses=None,
        )
        DSHIFTL = IAttr(
            name="DSHIFTL",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=3,
                max_count=3,
                types=DataNode,
                arg_names=(("i", "j", "shift"),)),
            optional_args={},
            return_type=lambda node: (_get_named_argument_type(
                node,
                "j" if isinstance(node.argument_by_name("i"), Literal) else
                "i"
                )
            ),
            reference_accesses=None,
        )
        DSHIFTR = IAttr(
            name="DSHIFTR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=3,
                max_count=3,
                types=DataNode,
                arg_names=(("i", "j", "shift"),)),
            optional_args={},
            return_type=lambda node: (_get_named_argument_type(
                node,
                "j" if isinstance(node.argument_by_name("i"), Literal) else
                "i"
                )
            ),
            reference_accesses=None,
        )
        EOSHIFT = IAttr(
            name="EOSHIFT",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(("array", "shift"),)),
            optional_args={"boundary": DataNode, "dim": DataNode},
            return_type=lambda node: _get_named_argument_type(node, "array"),
            reference_accesses=None,
        )
        EPSILON = IAttr(
            name="EPSILON",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("x",),)),
            optional_args={},
            return_type=lambda node: _get_named_argument_type(node, "x"),
            reference_accesses=None,
        )
        ERF = IAttr(
            name="ERF",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("x",),)),
            optional_args={},
            return_type=lambda node:
                _get_intrinsic_with_named_arg_precision(
                    node, ScalarType.Intrinsic.REAL,
                    "x"
                ),
            reference_accesses=None,
        )
        ERFC = IAttr(
            name="ERFC",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("x",),)),
            optional_args={},
            return_type=lambda node:
                _get_intrinsic_with_named_arg_precision(
                    node, ScalarType.Intrinsic.REAL,
                    "x"
                ),
            reference_accesses=None,
        )
        ERFC_SCALED = IAttr(
            name="ERFC_SCALED",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("x",),)),
            optional_args={},
            return_type=lambda node:
                _get_intrinsic_with_named_arg_precision(
                    node, ScalarType.Intrinsic.REAL,
                    "x"
                ),
            reference_accesses=None,
        )
        EVENT_QUERY = IAttr(
            name="EVENT_QUERY",
            is_pure=False,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(("event", "count"),)),
            optional_args={"stat": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        EXECUTE_COMMAND_LINE = IAttr(
            name="EXECUTE_COMMAND_LINE",
            is_pure=False,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("command",),)),
            optional_args={
                "wait": DataNode,
                "exitstat": DataNode,
                "cmdstat": DataNode,
                "cmdmsg": DataNode,
            },
            return_type=None,
            reference_accesses=None,
        )
        EXP = IAttr(
            name="EXP",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("x",),)),
            optional_args={},
            return_type=lambda node: _get_named_argument_type(node, "x"),
            reference_accesses=None,
        )
        EXPONENT = IAttr(
            name="EXPONENT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("x",),)),
            optional_args={},
            return_type=INTEGER_TYPE,
            reference_accesses=None,
        )
        EXTENDS_TYPE_OF = IAttr(
            name="EXTENDS_TYPE_OF",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(("a", "mold"),)),
            optional_args={},
            return_type=BOOLEAN_TYPE,
            reference_accesses=None,
        )
        FAILED_IMAGES = IAttr(
            name="FAILED_IMAGES",
            is_pure=False,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=0,
                max_count=0,
                types=DataNode,
                arg_names=()),
            optional_args={"team": DataNode, "kind": DataNode},
            return_type=lambda node: ArrayType(
                _type_of_scalar_with_optional_kind(
                    node, ScalarType.Intrinsic.INTEGER,
                    "kind"
                ),
                [ArrayType.Extent.DEFERRED],
            ),
            reference_accesses=None,
        )
        FINDLOC = IAttr(
            name="FINDLOC",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=3,
                types=DataNode,
                arg_names=(
                    ("array", "value", "dim"),
                    ("array", "value")
                )
            ),
            optional_args={"mask": DataNode,
                           "kind": DataNode,
                           "back": DataNode},
            return_type=_findloc_return_type,
            reference_accesses=None,
        )
        FLOAT = IAttr(
            name="FLOAT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                # FLOAT is a language extension, and not all compilers
                # (e.g. nvfortran) can handle a keyword argument.
                arg_names=(("",),)),
            optional_args={},
            return_type=REAL_TYPE,
            reference_accesses=None,
        )
        FLOOR = IAttr(
            name="FLOOR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("a",),)),
            optional_args={"kind": DataNode},
            return_type=lambda node:
                _type_of_scalar_with_optional_kind(
                    node, ScalarType.Intrinsic.INTEGER,
                    "kind"
                ),
            reference_accesses=None,
        )
        FRACTION = IAttr(
            name="FRACTION",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("x",),)),
            optional_args={},
            return_type=lambda node: _get_named_argument_type(node, "x"),
            reference_accesses=None,
        )
        GAMMA = IAttr(
            name="GAMMA",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("x",),)),
            optional_args={},
            return_type=lambda node: _get_named_argument_type(node, "x"),
            reference_accesses=None,
        )
        GET_COMMAND = IAttr(
            name="GET_COMMAND",
            is_pure=False,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=0,
                max_count=0,
                types=DataNode,
                arg_names=()),
            optional_args={
                "command": DataNode,
                "length": DataNode,
                "status": DataNode,
                "errmsg": DataNode,
            },
            return_type=None,
            reference_accesses=None,
        )
        GET_COMMAND_ARGUMENT = IAttr(
            name="GET_COMMAND_ARGUMENT",
            is_pure=False,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("number",),)),
            optional_args={
                "value": DataNode,
                "length": DataNode,
                "status": DataNode,
                "errmsg": DataNode,
            },
            return_type=None,
            reference_accesses=None,
        )
        GET_ENVIRONMENT_VARIABLE = IAttr(
            name="GET_ENVIRONMENT_VARIABLE",
            is_pure=False,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("name",),)),
            optional_args={
                "value": DataNode,
                "length": DataNode,
                "status": DataNode,
                "trim_name": DataNode,
                "errmsg": DataNode,
            },
            return_type=None,
            reference_accesses=None,
        )
        GET_TEAM = IAttr(
            name="GET_TEAM",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=0,
                max_count=0,
                types=DataNode,
                arg_names=()),
            optional_args={"level": DataNode},
            # Unsupported return type (TEAM_TYPE from ISO_FORTRAN_ENV).
            return_type=lambda node: UnsupportedFortranType("TEAM_TYPE"),
            reference_accesses=None,
        )
        HUGE = IAttr(
            name="HUGE",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(
                 min_count=1,
                 max_count=1,
                 types=(Reference, Literal),
                 arg_names=(("x",),)),
            optional_args={},
            return_type=lambda node: _get_named_argument_type(node, "x"),
            reference_accesses=None,
        )
        HYPOT = IAttr(
            name="HYPOT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(("x", "y"),)),
            optional_args={},
            return_type=lambda node: _get_named_argument_type(node, "x"),
            reference_accesses=None,
        )
        IACHAR = IAttr(
            name="IACHAR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("c",),)),
            optional_args={"kind": DataNode},
            return_type=lambda node:
                _type_of_scalar_with_optional_kind(
                    node, ScalarType.Intrinsic.INTEGER,
                    "kind"
                ),
            reference_accesses=None,
        )
        IALL = IAttr(
            name="IALL",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=2,
                types=DataNode,
                arg_names=(
                    ("array",),
                    ("array", "dim")
                )
            ),
            optional_args={"mask": DataNode},
            return_type=lambda node:
                # No kind option is available, however if we provide a fake
                # argument name as the final parameter this function will
                # provide the correct result.
                _get_intrinsic_of_argname_kind_with_optional_dim(
                    node, ScalarType.Intrinsic.INTEGER,
                    "array", "no_kind"
                ),
            reference_accesses=None,
        )
        IAND = IAttr(
            name="IAND",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(("i", "j"),)),
            optional_args={},
            return_type=lambda node: (_get_named_argument_type(
                node,
                "j" if isinstance(node.argument_by_name("i"), Literal) else
                "i"
                )
            ),
            reference_accesses=None,
        )
        IANY = IAttr(
            name="IANY",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=2,
                types=DataNode,
                arg_names=(
                    ("array",),
                    ("array", "dim")
                )
            ),
            optional_args={"mask": DataNode},
            return_type=lambda node:
                # No kind option is available, however if we provide a fake
                # argument name as the final parameter this function will
                # provide the correct result.
                _get_intrinsic_of_argname_kind_with_optional_dim(
                    node, ScalarType.Intrinsic.INTEGER,
                    "array", "no_kind"
                ),
            reference_accesses=None,
        )
        IBCLR = IAttr(
            name="IBCLR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(("i", "pos"),)),
            optional_args={},
            return_type=lambda node: _get_named_argument_type(node, "i"),
            reference_accesses=None,
        )
        IBITS = IAttr(
            name="IBITS",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=3,
                max_count=3,
                types=DataNode,
                arg_names=(("i", "pos", "len"),)),
            optional_args={},
            return_type=lambda node: _get_named_argument_type(node, "i"),
            reference_accesses=None,
        )
        IBSET = IAttr(
            name="IBSET",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(("i", "pos"),)),
            optional_args={},
            return_type=lambda node: _get_named_argument_type(node, "i"),
            reference_accesses=None,
        )
        ICHAR = IAttr(
            name="ICHAR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("c",),)),
            optional_args={"kind": DataNode},
            return_type=lambda node:
                _type_of_scalar_with_optional_kind(
                    node, ScalarType.Intrinsic.INTEGER,
                    "kind"
                ),
            reference_accesses=None,
        )
        IEOR = IAttr(
            name="IEOR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(("i", "j"),)),
            optional_args={},
            return_type=lambda node: (_get_named_argument_type(
                node,
                "j" if isinstance(node.argument_by_name("i"), Literal) else
                "i"
                )
            ),
            reference_accesses=None,
        )
        IMAGE_INDEX = IAttr(
            name="IMAGE_INDEX",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            # Argument names depend on input, as TEAM vs TEAM_NUMBER
            # are not distinguishable without context.
            required_args=ArgDesc(
                min_count=2,
                max_count=3,
                types=DataNode,
                arg_names=(("",),)),
            optional_args={},
            return_type=INTEGER_TYPE,
            reference_accesses=None,
        )
        IMAGE_STATUS = IAttr(
            name="IMAGE_STATUS",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("image",),)),
            optional_args={"team": DataNode},
            return_type=INTEGER_TYPE,
            reference_accesses=None,
        )
        INDEX = IAttr(
            name="INDEX",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(("string", "substring"),)),
            optional_args={"back": DataNode, "kind": DataNode},
            return_type=lambda node:
                _type_of_scalar_with_optional_kind(
                    node, ScalarType.Intrinsic.INTEGER,
                    "kind"
                ),
            reference_accesses=None,
        )
        INT = IAttr(
            name="INT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("a",),)),
            optional_args={"kind": DataNode},
            return_type=_int_return_type,
            reference_accesses=None,
        )
        IOR = IAttr(
            name="IOR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(("i", "j"),)),
            optional_args={},
            return_type=lambda node: (_get_intrinsic_with_named_arg_precision(
                node,
                ScalarType.Intrinsic.INTEGER,
                "j" if isinstance(node.argument_by_name("i"), Literal) else
                "i"
                )
            ),
            reference_accesses=None,
        )
        IPARITY = IAttr(
            name="IPARITY",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=2,
                types=DataNode,
                arg_names=(
                    ("array",),
                    ("array", "dim")
                )
            ),
            optional_args={"mask": DataNode},
            return_type=_iparity_return_type,
            reference_accesses=None,
        )
        IS_CONTIGUOUS = IAttr(
            name="IS_CONTIGUOUS",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("array",),)),
            optional_args={},
            return_type=BOOLEAN_TYPE,
            reference_accesses=None,
        )
        IS_IOSTAT_END = IAttr(
            name="IS_IOSTAT_END",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("i",),)),
            optional_args={},
            return_type=BOOLEAN_TYPE,
            reference_accesses=None,
        )
        IS_IOSTAT_EOR = IAttr(
            name="IS_IOSTAT_EOR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("i",),)),
            optional_args={},
            return_type=BOOLEAN_TYPE,
            reference_accesses=None,
        )
        ISHFT = IAttr(
            name="ISHFT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(("i", "shift"),)),
            optional_args={},
            return_type=lambda node: _get_named_argument_type(node, "i"),
            reference_accesses=None,
        )
        ISHFTC = IAttr(
            name="ISHFTC",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(("i", "shift"),)),
            optional_args={"size": DataNode},
            return_type=lambda node: _get_named_argument_type(node, "i"),
            reference_accesses=None,
        )
        KIND = IAttr(
            name="KIND",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("x",),)),
            optional_args={},
            return_type=INTEGER_TYPE,
            reference_accesses=None,
        )
        LBOUND = IAttr(
            name="LBOUND",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("array",),)),
            optional_args={"dim": DataNode, "kind": DataNode},
            return_type=_get_bound_function_return_type,
            reference_accesses=None,
        )
        LCOBOUND = IAttr(
            name="LCOBOUND",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("coarray",),)),
            optional_args={"dim": DataNode, "kind": DataNode},
            return_type=_get_bound_function_return_type,
            reference_accesses=None,
        )
        LEADZ = IAttr(
            name="LEADZ",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("i",),)),
            optional_args={},
            return_type=INTEGER_TYPE,
            reference_accesses=None,
        )
        LEN = IAttr(
            name="LEN",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("string",),)),
            optional_args={"kind": DataNode},
            return_type=lambda node:
                _type_of_scalar_with_optional_kind(
                    node, ScalarType.Intrinsic.INTEGER,
                    "kind"
                ),
            reference_accesses=None,
        )
        LEN_TRIM = IAttr(
            name="LEN_TRIM",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("string",),)),
            optional_args={"kind": DataNode},
            return_type=lambda node:
                _type_of_scalar_with_optional_kind(
                    node, ScalarType.Intrinsic.INTEGER,
                    "kind"
                ),
            reference_accesses=None,
        )
        LGE = IAttr(
            name="LGE",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(("string_a", "string_b"),)
            ),
            optional_args={},
            return_type=BOOLEAN_TYPE,
            reference_accesses=None,
        )
        LGT = IAttr(
            name="LGT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(("string_a", "string_b"),)
            ),
            optional_args={},
            return_type=BOOLEAN_TYPE,
            reference_accesses=None,
        )
        LLE = IAttr(
            name="LLE",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(("string_a", "string_b"),)
            ),
            optional_args={},
            return_type=BOOLEAN_TYPE,
            reference_accesses=None,
        )
        LLT = IAttr(
            name="LLT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(("string_a", "string_b"),)
            ),
            optional_args={},
            return_type=BOOLEAN_TYPE,
            reference_accesses=None,
        )
        LOG = IAttr(
            name="LOG",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("x",),)),
            optional_args={},
            return_type=lambda node: _get_named_argument_type(node, "x"),
            reference_accesses=None,
        )
        LOG_GAMMA = IAttr(
            name="LOG_GAMMA",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("x",),)),
            optional_args={},
            return_type=lambda node: _get_named_argument_type(node, "x"),
            reference_accesses=None,
        )
        LOG10 = IAttr(
            name="LOG10",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("x",),)),
            optional_args={},
            return_type=lambda node: _get_named_argument_type(node, "x"),
            reference_accesses=None,
        )
        LOGICAL = IAttr(
            name="LOGICAL",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("l",),)),
            optional_args={"kind": DataNode},
            return_type=lambda node: (
                _type_of_scalar_with_optional_kind(
                    node, node.argument_by_name("l").datatype.intrinsic,
                    "kind",
                ) if "kind" in node.argument_names else
                _get_named_argument_type(node, "l")
            ),
            reference_accesses=None,
        )
        MASKL = IAttr(
            name="MASKL",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("i",),)),
            optional_args={"kind": DataNode},
            return_type=lambda node:
                _type_of_scalar_with_optional_kind(
                    node, ScalarType.Intrinsic.INTEGER,
                    "kind"
                ),
            reference_accesses=None,
        )
        MASKR = IAttr(
            name="MASKR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("i",),)),
            optional_args={"kind": DataNode},
            return_type=lambda node:
                _type_of_scalar_with_optional_kind(
                    node, ScalarType.Intrinsic.INTEGER,
                    "kind"
                ),
            reference_accesses=None,
        )
        MATMUL = IAttr(
            name="MATMUL",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(("matrix_a", "matrix_b"),)
            ),
            optional_args={},
            return_type=_matmul_return_type,
            reference_accesses=None,
        )
        MAX = IAttr(
            name="MAX",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            # No upper limit on argument type so we don't store an
            # argument list of names.
            required_args=ArgDesc(
                min_count=2,
                max_count=None,
                types=DataNode,
                arg_names=((None,),)),
            optional_args={},
            return_type=lambda node: node.arguments[0].datatype,
            reference_accesses=None,
        )
        MAXEXPONENT = IAttr(
            name="MAXEXPONENT",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("x",),)),
            optional_args={},
            return_type=INTEGER_TYPE,
            reference_accesses=None,
        )
        MAXLOC = IAttr(
            name="MAXLOC",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=2,
                types=DataNode,
                arg_names=(
                    ("array",),
                    ("array", "dim")
                )
            ),
            optional_args={
                "mask": DataNode,
                "kind": DataNode,
                "back": DataNode,
            },
            return_type=lambda node: (
                _type_of_named_arg_with_optional_kind_and_dim(
                    node, "array"
                )
            ),
            reference_accesses=None,
        )
        MAXVAL = IAttr(
            name="MAXVAL",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=2,
                types=DataNode,
                arg_names=(
                    ("array",),
                    ("array", "dim")
                )
            ),
            optional_args={"mask": DataNode},
            return_type=_maxval_return_type,
            reference_accesses=None,
        )
        MERGE = IAttr(
            name="MERGE",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=3,
                max_count=3,
                types=DataNode,
                arg_names=(("tsource", "fsource", "mask"),)
            ),
            optional_args={},
            return_type=lambda node: _get_named_argument_type(node,
                                                              "tsource"),
            reference_accesses=None
        )
        MERGE_BITS = IAttr(
            name="MERGE_BITS",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=3,
                max_count=3,
                types=DataNode,
                arg_names=(("i", "j", "mask"),)),
            optional_args={},
            return_type=lambda node: _get_named_argument_type(node, "i"),
            reference_accesses=None,
        )
        MIN = IAttr(
            name="MIN",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            # No upper limit on argument type so we don't store an
            # argument list of names.
            required_args=ArgDesc(
                min_count=2,
                max_count=None,
                types=DataNode,
                arg_names=((None,),)),
            optional_args={},
            return_type=lambda node: node.arguments[0].datatype,
            reference_accesses=None,
        )
        MINEXPONENT = IAttr(
            name="MINEXPONENT",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("x",),)),
            optional_args={},
            return_type=INTEGER_TYPE,
            reference_accesses=None,
        )
        MINLOC = IAttr(
            name="MINLOC",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=2,
                types=DataNode,
                arg_names=(
                    ("array",),
                    ("array", "dim")
                )
            ),
            optional_args={
                "mask": DataNode,
                "kind": DataNode,
                "back": DataNode,
            },
            return_type=lambda node: (
                _type_of_named_arg_with_optional_kind_and_dim(
                    node, "array"
                )
            ),
            reference_accesses=None,
        )
        MINVAL = IAttr(
            name="MINVAL",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=2,
                types=DataNode,
                arg_names=(
                    ("array",),
                    ("array", "dim")
                )
            ),
            optional_args={"mask": DataNode},
            return_type=_maxval_return_type,
            reference_accesses=None,
        )
        MOD = IAttr(
            name="MOD",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(("a", "p"),)),
            optional_args={},
            return_type=lambda node: _get_named_argument_type(node, "a"),
            reference_accesses=None
        )
        MODULO = IAttr(
            name="MODULO",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(("a", "p"),)),
            optional_args={},
            return_type=lambda node: _get_named_argument_type(node, "a"),
            reference_accesses=None,
        )
        MOVE_ALLOC = IAttr(
            name="MOVE_ALLOC",
            is_pure=False,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(("from", "to"),)),
            optional_args={"stat": DataNode, "errmsg": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        MVBITS = IAttr(
            name="MVBITS",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=5,
                max_count=5,
                types=DataNode,
                arg_names=(
                    ("from", "frompos", "len", "to", "topos"),
                )
            ),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        NEAREST = IAttr(
            name="NEAREST",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(("x", "s"),)),
            optional_args={},
            return_type=lambda node: _get_named_argument_type(node, "x"),
            reference_accesses=None,
        )
        NEW_LINE = IAttr(
            name="NEW_LINE",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("c"),)),
            optional_args={},
            return_type=CHARACTER_TYPE,
            reference_accesses=None,
        )
        NINT = IAttr(
            name="NINT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("a",),)),
            optional_args={"kind": DataNode},
            return_type=INTEGER_TYPE,
            reference_accesses=None,
        )
        NORM2 = IAttr(
            name="NORM2",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=2,
                types=DataNode,
                arg_names=(
                    ("x",),
                    ("x", "dim")
                )
            ),
            optional_args={},
            # No kind on NORM2 but this function works for return type.
            return_type=lambda node: (
                _type_of_named_arg_with_optional_kind_and_dim(
                    node, "x"
                )
            ),
            reference_accesses=None,
        )
        NOT = IAttr(
            name="NOT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("i",),)),
            optional_args={},
            return_type=lambda node: ScalarType(
                ScalarType.Intrinsic.INTEGER,
                node.argument_by_name("i").datatype.precision
            ),
            reference_accesses=None
        )
        NULL = IAttr(
            name="NULL",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=0,
                max_count=0,
                types=DataNode,
                arg_names=()),
            optional_args={"mold": DataNode},
            # Returns a dissociated pointed - not supported.
            return_type=lambda node: UnsupportedFortranType(""),
            reference_accesses=None,
        )
        NUM_IMAGES = IAttr(
            name="NUM_IMAGES",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            # Argnames depends on the input.
            required_args=ArgDesc(
                min_count=0,
                max_count=1,
                types=DataNode,
                arg_names=(("",),)),
            optional_args={},
            return_type=INTEGER_TYPE,
            reference_accesses=None,
        )
        OUT_OF_RANGE = IAttr(
            name="OUT_OF_RANGE",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(("x", "mold",),)),
            optional_args={"round": DataNode},
            return_type=BOOLEAN_TYPE,
            reference_accesses=None,
        )
        PACK = IAttr(
            name="PACK",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(("array", "mask"),)),
            optional_args={"vector": DataNode},
            return_type=lambda node: ArrayType(
                ScalarType(
                    node.argument_by_name("array").datatype.intrinsic,
                    node.argument_by_name("array").datatype.precision),
                [ArrayType.Extent.DEFERRED]
            ),
            reference_accesses=None,
        )
        PARITY = IAttr(
            name="PARITY",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=2,
                types=DataNode,
                arg_names=(
                    ("mask",),
                    ("mask", "dim")
                )
            ),
            optional_args={},
            return_type=lambda node: _get_named_argument_type(node, "mask"),
            reference_accesses=None,
        )
        POPCNT = IAttr(
            name="POPCNT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("i",),)),
            optional_args={},
            return_type=INTEGER_TYPE,
            reference_accesses=None,
        )
        POPPAR = IAttr(
            name="POPPAR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("i",),)),
            optional_args={},
            return_type=INTEGER_TYPE,
            reference_accesses=None,
        )
        PRECISION = IAttr(
            name="PRECISION",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("x",),)),
            optional_args={},
            return_type=INTEGER_TYPE,
            reference_accesses=None,
        )
        PRESENT = IAttr(
            name="PRESENT",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("a",),)),
            optional_args={},
            return_type=BOOLEAN_TYPE,
            reference_accesses=None,
        )
        PRODUCT = IAttr(
            name="PRODUCT",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=2,
                types=DataNode,
                arg_names=(
                    ("array",),
                    ("array", "dim")
                )
            ),
            optional_args={"mask": DataNode},
            return_type=(
                lambda node:
                _type_with_specified_precision_and_optional_dim(
                    node, "array",
                    node.argument_by_name("array").datatype.intrinsic
                )
            ),
            reference_accesses=None,
        )
        RADIX = IAttr(
            name="RADIX",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("x",),)),
            optional_args={},
            return_type=INTEGER_TYPE,
            reference_accesses=None
        )
        RANDOM_INIT = IAttr(
            name="RANDOM_INIT",
            is_pure=False,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(("repeatable", "image_distinct"),)
            ),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        RANDOM_NUMBER = IAttr(
            name="RANDOM_NUMBER",
            is_pure=False,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=Reference,
                arg_names=(("harvest",),)),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        RANDOM_SEED = IAttr(
            name="RANDOM_SEED",
            is_pure=False,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=0,
                max_count=0,
                types=Reference,
                arg_names=()),
            optional_args={"size": DataNode, "put": DataNode, "Get": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        RANGE = IAttr(
            name="RANGE",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=Reference,
                arg_names=(("x",),)),
            optional_args={},
            return_type=INTEGER_TYPE,
            reference_accesses=None,
        )
        RANK = IAttr(
            name="RANK",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=Reference,
                arg_names=(("a",),)),
            optional_args={},
            return_type=INTEGER_TYPE,
            reference_accesses=None
        )
        REAL = IAttr(
            name="REAL",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=Reference,
                arg_names=(("a",),)),
            optional_args={"kind": DataNode},
            return_type=lambda node: (
                ScalarType(
                    ScalarType.Intrinsic.REAL,
                    (
                        node.argument_by_name("kind").copy()
                        if "kind" in node.argument_names
                        else node.argument_by_name("a").datatype.precision
                    ),
                )
            ),
            reference_accesses=None,
        )
        REDUCE = IAttr(
            name="REDUCE",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=3,
                types=DataNode,
                arg_names=(
                    ("array", "operation"),
                    ("array", "operation", "dim")
                )
            ),
            optional_args={"mask": DataNode,
                           "identity": DataNode,
                           "ordered": DataNode},
            return_type=_maxval_return_type,
            reference_accesses=None,
        )
        REPEAT = IAttr(
            name="REPEAT",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=Reference,
                arg_names=(("string", "ncopies"),)),
            optional_args={},
            return_type=CHARACTER_TYPE,
            reference_accesses=None,
        )
        RESHAPE = IAttr(
            name="RESHAPE",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=Reference,
                arg_names=(("source", "shape"),)),
            optional_args={"pad": DataNode, "order": DataNode},
            # I went with unresolved for now as the result depends on
            # argument 2 (even the dimensionality).
            return_type=lambda node: UnresolvedType(),
            reference_accesses=None,
        )
        RRSPACING = IAttr(
            name="RRSPACING",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=Reference,
                arg_names=(("x",),)),
            optional_args={},
            return_type=lambda node: _get_named_argument_type(node, "x"),
            reference_accesses=None,
        )
        SAME_TYPE_AS = IAttr(
            name="SAME_TYPE_AS",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=Reference,
                arg_names=(("a", "b"),)),
            optional_args={},
            return_type=BOOLEAN_TYPE,
            reference_accesses=None,
        )
        SCALE = IAttr(
            name="SCALE",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=Reference,
                arg_names=(("x", "i"),)),
            optional_args={},
            return_type=lambda node: _get_named_argument_type(node, "x"),
            reference_accesses=None,
        )
        SCAN = IAttr(
            name="SCAN",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=Reference,
                arg_names=(("string", "set"),)),
            optional_args={"back": DataNode, "kind": DataNode},
            return_type=lambda node:
                _type_of_scalar_with_optional_kind(
                    node, ScalarType.Intrinsic.INTEGER,
                    "kind"
                ),
            reference_accesses=None,
        )
        SELECTED_CHAR_KIND = IAttr(
            name="SELECTED_CHAR_KIND",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=Reference,
                arg_names=(("name",),)),
            optional_args={},
            return_type=INTEGER_TYPE,
            reference_accesses=None,
        )
        SELECTED_INT_KIND = IAttr(
            name="SELECTED_INT_KIND",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=Reference,
                arg_names=(("r",),)),
            optional_args={},
            return_type=INTEGER_TYPE,
            reference_accesses=None,
        )
        SELECTED_REAL_KIND = IAttr(
            name="SELECTED_REAL_KIND",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=0,
                max_count=0,
                types=Reference,
                arg_names=()),
            optional_args={"p": DataNode, "r": DataNode, "radix": DataNode},
            return_type=INTEGER_TYPE,
            reference_accesses=None,
        )
        SET_EXPONENT = IAttr(
            name="SET_EXPONENT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=Reference,
                arg_names=(("x", "i"),)),
            optional_args={},
            return_type=lambda node: _get_named_argument_type(node, "x"),
            reference_accesses=None,
        )
        SHAPE = IAttr(
            name="SHAPE",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=Reference,
                arg_names=(("source",),)),
            optional_args={"kind": DataNode},
            return_type=lambda node: (
                ArrayType(ScalarType(
                    ScalarType.Intrinsic.INTEGER,
                    (ScalarType.Precision.UNDEFINED if "kind" not in
                     node.argument_names else
                     node.argument_by_name("kind").copy())),
                    [Literal(str(len(
                        node.argument_by_name("source").datatype.shape)),
                            INTEGER_TYPE)])
            ),
            reference_accesses=None,
        )
        SHIFTA = IAttr(
            name="SHIFTA",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=Reference,
                arg_names=(("i", "shift"),)),
            optional_args={},
            return_type=lambda node: _get_named_argument_type(node, "i"),
            reference_accesses=None,
        )
        SHIFTL = IAttr(
            name="SHIFTL",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=Reference,
                arg_names=(("i", "shift"),)),
            optional_args={},
            return_type=lambda node: _get_named_argument_type(node, "i"),
            reference_accesses=None,
        )
        SHIFTR = IAttr(
            name="SHIFTR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=Reference,
                arg_names=(("i", "shift"),)),
            optional_args={},
            return_type=lambda node: _get_named_argument_type(node, "i"),
            reference_accesses=None,
        )
        SIGN = IAttr(
            name="SIGN",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(("a", "b"),)),
            optional_args={},
            return_type=lambda node: _get_named_argument_type(node, "a"),
            reference_accesses=None
        )
        SIN = IAttr(
            name="SIN",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("x",),)),
            optional_args={},
            return_type=lambda node: _get_named_argument_type(node, "x"),
            reference_accesses=None
        )
        SINH = IAttr(
            name="SINH",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("x",),)),
            optional_args={},
            return_type=lambda node: _get_named_argument_type(node, "x"),
            reference_accesses=None
        )
        SIZE = IAttr(
            name="SIZE",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("array",),)),
            optional_args={"dim": DataNode, "kind": DataNode},
            return_type=lambda node:
                _type_of_scalar_with_optional_kind(
                    node, ScalarType.Intrinsic.INTEGER,
                    "kind"
                ),
            reference_accesses=None,
        )
        SPACING = IAttr(
            name="SPACING",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("x",),)),
            optional_args={},
            return_type=lambda node: _get_named_argument_type(node, "x"),
            reference_accesses=None,
        )
        SPREAD = IAttr(
            name="SPREAD",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=3,
                max_count=3,
                types=DataNode,
                arg_names=(("source", "dim", "ncopies"),)
            ),
            optional_args={},
            return_type=lambda node: ArrayType(
                ScalarType(
                    node.argument_by_name("source").datatype.intrinsic,
                    node.argument_by_name("source").datatype.precision),
                ([ArrayType.Extent.DEFERRED] *
                 (len(node.argument_by_name("source").datatype.shape) + 1)
                 if isinstance(node.argument_by_name("source").datatype,
                               ArrayType) else
                 [ArrayType.Extent.DEFERRED])
            ),
            reference_accesses=None,
        )
        SQRT = IAttr(
            name="SQRT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("x",),)),
            optional_args={},
            # TODO 1590 Complex conversion unsupported.
            return_type=lambda node: UnsupportedFortranType(""),
            reference_accesses=None
        )
        STOPPED_IMAGES = IAttr(
            name="STOPPED_IMAGES",
            is_pure=False,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=0,
                max_count=0,
                types=DataNode,
                arg_names=()),
            optional_args={"team": DataNode, "kind": DataNode},
            return_type=lambda node: ArrayType(
                _type_of_scalar_with_optional_kind(
                    node, ScalarType.Intrinsic.INTEGER,
                    "kind"
                )
                [ArrayType.Extent.DEFERRED]
            ),
            reference_accesses=None,
        )
        STORAGE_SIZE = IAttr(
            name="STORAGE_SIZE",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("a",),)),
            optional_args={"kind": DataNode},
            return_type=lambda node:
                _type_of_scalar_with_optional_kind(
                    node, ScalarType.Intrinsic.INTEGER,
                    "kind"
                ),
            reference_accesses=None,
        )
        SUM = IAttr(
            name="SUM",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=2,
                types=DataNode,
                arg_names=(
                    ("array",),
                    ("array", "dim")
                )
            ),
            optional_args={"mask": DataNode},
            return_type=(
                lambda node:
                _type_with_specified_precision_and_optional_dim(
                    node, "array",
                    node.argument_by_name("array").datatype.intrinsic
                )
            ),
            reference_accesses=None,
        )
        SYSTEM_CLOCK = IAttr(
            name="SYSTEM_CLOCK",
            is_pure=False,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=0,
                max_count=0,
                types=DataNode,
                arg_names=()),
            optional_args={"count": DataNode,
                           "count_rate": DataNode,
                           "count_max": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        TAN = IAttr(
            name="TAN",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("x",),)),
            optional_args={},
            return_type=lambda node: _get_named_argument_type(node, "x"),
            reference_accesses=None
        )
        TANH = IAttr(
            name="TANH",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("x",),)),
            optional_args={},
            return_type=lambda node: _get_named_argument_type(node, "x"),
            reference_accesses=None
        )
        TEAM_NUMBER = IAttr(
            name="TEAM_NUMBER",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=0,
                max_count=0,
                types=DataNode,
                arg_names=()),
            optional_args={"team": DataNode},
            return_type=INTEGER_TYPE,
            reference_accesses=None,
        )
        THIS_IMAGE = IAttr(
            name="THIS_IMAGE",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=0,
                max_count=2,
                types=DataNode,
                arg_names=(
                    (),
                    ("coarray",),
                    ("coarray", "dim")
                )
            ),
            optional_args={"team": DataNode},
            # Support for this is not currentl implemented, however it is
            # an integer or array of integer's depending on arguments, so
            # could be added later. See
            # https://gcc.gnu.org/onlinedocs/gfortran/THIS_005fIMAGE.html
            return_type=lambda node: UnresolvedType(),
            reference_accesses=None,
        )
        TINY = IAttr(
            name="TINY",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=(Reference, Literal),
                arg_names=(("x",),)
            ),
            optional_args={},
            return_type=lambda node: _get_named_argument_type(node, "x"),
            reference_accesses=None,
        )
        TRAILZ = IAttr(
            name="TRAILZ",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("i",),)),
            optional_args={},
            return_type=INTEGER_TYPE,
            reference_accesses=None,
        )
        TRANSFER = IAttr(
            name="TRANSFER",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(("source", "mold"),)),
            optional_args={"size": DataNode},
            return_type=lambda node: (
                node.arguments[1].datatype.copy() if
                ("size" not in node.argument_names and
                 not isinstance(node.argument_by_name("mold").datatype,
                                ArrayType))
                else ArrayType(
                    ScalarType(
                        node.argument_by_name("mold").datatype.intrinsic,
                        node.argument_by_name("mold").datatype.precision
                    ),
                    [ArrayType.Extent.DEFERRED])
            ),
            reference_accesses=None,
        )
        TRANSPOSE = IAttr(
            name="TRANSPOSE",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("matrix",),)),
            optional_args={},
            return_type=lambda node: ArrayType(ScalarType(
                node.argument_by_name("matrix").datatype.intrinsic,
                node.argument_by_name("matrix").datatype.precision),
                [node.argument_by_name("matrix").datatype.shape[1],
                 node.argument_by_name("matrix").datatype.shape[0]]
            ),
            reference_accesses=None,
        )
        TRIM = IAttr(
            name="TRIM",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("string",),)),
            optional_args={},
            return_type=CHARACTER_TYPE,
            reference_accesses=None
        )
        UBOUND = IAttr(
            name="UBOUND",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("array",),)),
            optional_args={"dim": DataNode, "kind": DataNode},
            return_type=_get_bound_function_return_type,
            reference_accesses=None,
        )
        UCOBOUND = IAttr(
            name="UCOBOUND",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(
                min_count=1,
                max_count=1,
                types=DataNode,
                arg_names=(("coarray",),)),
            optional_args={"dim": DataNode, "kind": DataNode},
            return_type=_get_bound_function_return_type,
            reference_accesses=None,
        )
        UNPACK = IAttr(
            name="UNPACK",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=3,
                max_count=3,
                types=DataNode,
                arg_names=(("vector", "mask", "field"),)
            ),
            optional_args={},
            return_type=lambda node: _get_named_argument_type(node, "vector"),
            reference_accesses=None,
        )
        VERIFY = IAttr(
            name="VERIFY",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(("string", "set"),)),
            optional_args={"back": DataNode, "kind": DataNode},
            return_type=lambda node:
                _type_of_scalar_with_optional_kind(
                    node, ScalarType.Intrinsic.INTEGER,
                    "kind"
                ),
            reference_accesses=None,
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
        if self.intrinsic.return_type is None:
            return NoType()
        if isinstance(self.intrinsic.return_type, Callable):
            try:
                return self.intrinsic.return_type(self)
            except AttributeError as err:
                # If we get an attribute error, and its because of attempting
                # to lookup the precision or intrinsic, then it is likely
                # due to looking up the datatype elements of an Unresolved
                # or UnsupportedType - in those cases then we should
                # return an UnresolvedType and not error.
                if (("has no attribute 'precision'" or
                     "has no attribute 'intrinsic'" in str(err))
                    and "NoneType" not in
                        str(err)):
                    return UnresolvedType()
                # Can't use debug string due to this being a potentially
                # incomplete IntrinsicCall
                raise InternalError(
                    f"Failed to compute the datatype of a "
                    f"'{self.intrinsic.name}' intrinsic. This is likely due "
                    f"to not fully initialising the intrinsic correctly."
                ) from err
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

    def _find_matching_interface(self) -> Tuple[str]:
        '''
        Finds the matching required argument interface for this node to
        add argument names from.

        :raises NotImplementedError: if there is not exactly one argument
                                     interface that matches this
                                     IntrinsicCall.
        '''
        if len(self.intrinsic.required_args.arg_names) > 1:
            # Pull out the list of optional argument names.
            optional_names = list(self.intrinsic.optional_args.keys())
            # Create a list of all the possible interface's argument lists.
            potential_interfaces: List[Tuple[str]] = [
                    names for names in self.intrinsic.required_args.arg_names
            ]
            # Remove any of the interfaces that don't contain
            # a named non-optional argument from the list of potential
            # candidate interfaces.
            for name in self.argument_names:
                if not name:
                    continue
                # Need to check lower case.
                lname = name.lower()
                # Optional argument names are skipped over as they don't
                # affect which interface is being used.
                if lname in optional_names:
                    continue
                for arglist in potential_interfaces:
                    if lname not in arglist:
                        potential_interfaces.remove(arglist)

            # Remove any of the interfaces that have too many or
            # too few *total* arguments to be candidates.
            for choice in potential_interfaces[:]:
                min_args = len(choice)
                max_args = min_args + len(optional_names)
                if (len(self.arguments) < min_args or
                        len(self.arguments) > max_args):
                    potential_interfaces.remove(choice)

            # Remove any of the interfaces that have too many or
            # too few *required* arguments to be candidates.
            # At this point the total arguments must be valid for all
            # remaining choices, and all named arguments must also be
            # present.
            for choice in potential_interfaces[:]:
                required_args = len(choice)
                # Check if the number of unnamed arguments is greater
                # than the number of required arguments. If so then
                # this choice is still acceptable (because optional
                # arguments can also be positional).
                num_positional_arguments = len(
                        [x for x in self.argument_names if x is None]
                )
                if num_positional_arguments >= required_args:
                    continue
                # Otherwise we need to check if all the
                # required arguments are present as named arguments.
                # This operation pulls all the argument names from the
                # potential interface that are not already matched to a
                # positional argument in this IntrinsicCall. These must
                # be matched to named arguments in this IntrinsicCall, else
                # this interface cannot be a candidate for argument names.
                remaining_required = choice[num_positional_arguments:]
                for name in remaining_required:
                    lname = name.lower()
                    if lname not in self.argument_names:
                        potential_interfaces.remove(choice)
                        break

            # If we didn't reduce the number of potential interfaces to a
            # single interface then we can't add argument names.
            if (len(potential_interfaces) > 1 or
                    len(potential_interfaces) == 0):
                raise NotImplementedError(
                    f"Cannot add argument names to '{self.intrinsic.name}' "
                    f"IntrinsicCall as PSyclone can't determine which "
                    f"argument set it should use. This can be resolved by "
                    f"using named arguments in the Fortran source."
                )
            return potential_interfaces[0]
        elif len(self.intrinsic.required_args.arg_names) == 1:
            # This intrinsic only has a single possible interface.
            return self.intrinsic.required_args.arg_names[0]
        else:
            # This intrinsic has no required arguments.
            return ()

    def compute_argument_names(self):
        '''Computes the argument names that correspond to the arguments
        of this IntrinsicCall, and add those argument names. If the interface
        is ambiguous, this function will raise an error.

        A small number of intrinsics (e.g. ALLOCATE, MAX) never have ambiguity
        and no argument limits, in which case no argument names are added.

        :raises ValueError: If the number of arguments or argument names
            are not valid for this IntrinsicCall.
        :raises NotImplementedError: If there is argument ambiguity and
            computation of argument names is not possible.
        '''
        # First step is to convert all the argument names in the
        # intrinsic call to lower case. This also avoids constant
        # need to convert argument names to lower case when doing
        # comparisons.
        argument_names = self.argument_names
        for i, name in enumerate(argument_names):
            if name:
                self._argument_names[i] = (self._argument_names[i][0],
                                           name.lower())

        # Get the optional argument names
        optional_names = list(self.intrinsic.optional_args.keys())

        # If PSyclone can't handle the required args due
        # to them being non-finite or context sensitive, then skip
        # checking argument names (This is if [0][0] is None or '').
        if (not (len(self.intrinsic.required_args.arg_names) == 1 and
                 not self.intrinsic.required_args.arg_names[0][0])):
            # Get all valid argument names.
            all_valid_names = [
                name for tupl in self.intrinsic.required_args.arg_names for
                name in tupl
            ]
            all_valid_names.extend(optional_names)
            # Check that all arguments names provided to this call are valid.
            # Raise ValueError if not.
            for name in self.argument_names:
                if not name:
                    continue
                if name not in all_valid_names:
                    raise ValueError(
                        f"Found invalid argument name '{name}' when "
                        f"computing argument names for the "
                        f"'{self.intrinsic.name}' IntrinsicCall. Allowed "
                        f"argument names are '{sorted(set(all_valid_names))}'."
                    )

        # Check that this call has a valid number of arguments
        if len(self.arguments) < self.intrinsic.required_args.min_count:
            raise ValueError(
                f"Found too few arguments when computing argument names for "
                f"the '{self.intrinsic.name}' IntrinsicCall. Requires at "
                f"least {self.intrinsic.required_args.min_count} "
                f"arguments but found {len(self.arguments)}."
            )

        # If there is no maximum number of required arguments then we
        # can skip the rest of argument name computation, as this Intrinsic
        # can never have ambiguity.
        if self.intrinsic.required_args.max_count is None:
            return

        if (len(self.arguments) > (self.intrinsic.required_args.max_count +
                                   len(optional_names))):
            max_args = (self.intrinsic.required_args.max_count +
                        len(optional_names))
            raise ValueError(
                f"Found too many arguments when computing argument names "
                f"for the '{self.intrinsic.name}' IntrinsicCall. Requires at "
                f"most {max_args} arguments but found {len(self.arguments)}."
            )

        # Find which intrinsic call interface we matching arguments to.
        interface_arg_names = self._find_matching_interface()

        # Handle cases where None or "" is in the interface_arg_names,
        # as this implies context sensitive argument naming which PSyclone
        # cannot handle.
        if interface_arg_names and not interface_arg_names[0]:
            # If we find any named non-optional named arguments for these
            # intrinsics then we can't add argument names to this
            # IntrinsicCall.
            # N.B. With currently supported intrinsics there are no
            # optional argument on these context-sensitive intrinsics
            # that have a finite argument count, but we keep the check
            # in case we need the support in future, and it still handles
            # what we currently need to check (i.e. if we have a named
            # argument here we can't add argument names to it safely).
            for name in self.argument_names:
                if not name:
                    continue
                if name not in optional_names:
                    raise NotImplementedError(
                        f"Cannot add argument names to "
                        f"'{self.intrinsic.name}' "
                        f"as non-optional argument name '{name}' found "
                        f"but the Intrinsic has context-sensitive argument "
                        f"names which is unsupported by PSyclone."
                    )

        # The following rules are defined by the Fortran standard.
        # 1. Unnamed arguments must be in the order defined in the standard,
        #    i.e. you cannot have LBOUND(1, 8, array=i).
        # 2. If all arguments are named, the order is entirely flexible, so
        #    LBOUND(kind=8, dim=1, array=i) is allowed.
        # 3. All unnamed arguments will occur before any named arguments.

        # Name any unnamed arguments.
        for i, name in enumerate(self.argument_names):
            # If we find a named arg then we can exit this section.
            if name:
                break
            if i < len(interface_arg_names):
                # We found a required argument without a name.
                # Update the argument_names tuple with the corresponding
                # name from the matched interface.
                # If the argument name is '' then it needs to be None.
                if interface_arg_names[i]:
                    self._argument_names[i] = (self._argument_names[i][0],
                                               interface_arg_names[i])
                else:
                    self._argument_names[i] = (self._argument_names[i][0],
                                               None)
                continue
            # Otherwise we found an optional argument, which will always
            # be in order if unnamed.
            self._argument_names[i] = (self._argument_names[i][0],
                                       optional_names[i - len(
                                           interface_arg_names)])

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
        # Get all valid required argument names.
        valid_req_names = [
            name for tupl in intrinsic.required_args.arg_names for
            name in tupl
        ]
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
                if name in intrinsic.optional_args:
                    if not isinstance(arg[1], intrinsic.optional_args[name]):
                        raise TypeError(
                            f"The optional argument '{name}' to intrinsic "
                            f"'{intrinsic.name}' must be of type "
                            f"'{intrinsic.optional_args[name].__name__}' but "
                            f"got '{type(arg[1]).__name__}'")
                elif name in valid_req_names:
                    if not isinstance(arg[1], intrinsic.required_args.types):
                        raise TypeError(
                            f"The argument '{name}' to intrinsic "
                            f"'{intrinsic.name}' must be of type "
                            f"'{intrinsic.required_args.types.__name__}' but "
                            f"got '{type(arg[1]).__name__}'")
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
                        f"but got a '{type(arg).__name__}'")

        # Create an intrinsic call and add the arguments
        # afterwards. We can't call the parent create method as it
        # assumes the intrinsic argument is a symbol and therefore tries
        # to create an intrinsic call with this symbol, rather than
        # the intrinsic enum.
        call._add_args(call, arguments)

        # Error check and add argument names to the call
        try:
            call.compute_argument_names()
        except (ValueError, NotImplementedError):
            # Since we fail adding argument names, we need to undo any links
            # created between nodes and return all inputs to their original
            # state before raising the error to the caller.
            for child in call.children:
                child.detach()
            # Rereaise the error.
            raise

        return call

    def get_all_accessed_symbols(self) -> set[Symbol]:
        '''
        :returns: a set of all the symbols accessed inside this IntrinsicCall.
        '''
        symbols = set()
        for child in self.arguments:
            symbols.update(child.get_all_accessed_symbols())
        return symbols

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
    IntrinsicCall.Intrinsic.ABS,  IntrinsicCall.Intrinsic.ACOS,
    IntrinsicCall.Intrinsic.AINT, IntrinsicCall.Intrinsic.ANINT,
    IntrinsicCall.Intrinsic.ASIN, IntrinsicCall.Intrinsic.ATAN,
    IntrinsicCall.Intrinsic.ATAN2, IntrinsicCall.Intrinsic.COS,
    IntrinsicCall.Intrinsic.COSH, IntrinsicCall.Intrinsic.DBLE,
    IntrinsicCall.Intrinsic.DPROD, IntrinsicCall.Intrinsic.EXP,
    IntrinsicCall.Intrinsic.IAND, IntrinsicCall.Intrinsic.IEOR,
    IntrinsicCall.Intrinsic.INT, IntrinsicCall.Intrinsic.IOR,
    IntrinsicCall.Intrinsic.LOG, IntrinsicCall.Intrinsic.NOT,
    IntrinsicCall.Intrinsic.MAX, IntrinsicCall.Intrinsic.MIN,
    IntrinsicCall.Intrinsic.MOD, IntrinsicCall.Intrinsic.NINT,
    IntrinsicCall.Intrinsic.SIGN, IntrinsicCall.Intrinsic.SIN,
    IntrinsicCall.Intrinsic.SINH, IntrinsicCall.Intrinsic.SQRT,
    IntrinsicCall.Intrinsic.TAN, IntrinsicCall.Intrinsic.TANH,
    IntrinsicCall.Intrinsic.UBOUND, IntrinsicCall.Intrinsic.MERGE,
    IntrinsicCall.Intrinsic.PRODUCT, IntrinsicCall.Intrinsic.SIZE,
    IntrinsicCall.Intrinsic.SUM, IntrinsicCall.Intrinsic.LBOUND,
    IntrinsicCall.Intrinsic.MAXVAL, IntrinsicCall.Intrinsic.MINVAL,
    IntrinsicCall.Intrinsic.TINY, IntrinsicCall.Intrinsic.HUGE,
    IntrinsicCall.Intrinsic.CEILING,
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
# Intrinsics that perform operations on an array.
REDUCTION_INTRINSICS = [
    IntrinsicCall.Intrinsic.SUM, IntrinsicCall.Intrinsic.MINVAL,
    IntrinsicCall.Intrinsic.MAXVAL, IntrinsicCall.Intrinsic.PACK,
    IntrinsicCall.Intrinsic.COUNT
]
