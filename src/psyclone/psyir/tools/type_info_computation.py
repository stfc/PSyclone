# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' This module contains the implementation functions that computes the
promotion of precisions or datatypes for Fortran operations.'''

from typing import Union

from psyclone.errors import InternalError
from psyclone.psyir.nodes import Reference
from psyclone.psyir.symbols.datatypes import (
    ScalarType, UnresolvedType, DataType, ArrayType
)


def compute_precision(
        precisions: list[Union[int, ScalarType.Precision, Reference]]
) -> Union[int, ScalarType.Precision, Reference]:
    '''
    Compares the input precisions to determine the precision of the result
    of a numerical operation involving them.

    If the precisions are the same, then that value is returned.
    Otherwise, Section 7.1.9.3 of the Fortran2008 standard says that in
    this case, the precision of the result is the greatest of the set.
    If the precision cannot be determined then
    `ScalarType.Precision.UNDEFINED` is returned.

    :param precisions: the precisions of the operands.

    :returns: the precision of the resulting expression.

    :raises InternalError: if an unsupported Precision value is encountered
        (this is to defend against any future extension of
        ScalarType.Precision).
    '''
    # If all precisions are equal, then we can return the first.
    # This will handle the case where we have all precisions as References
    # to the same parameter as well.
    if all(x == precisions[0] for x in precisions):
        return precisions[0]

    # We have different precisions.
    if all(isinstance(prec, int) for prec in precisions):
        # All precisions are integer.
        return max(precisions)

    if all(isinstance(prec, ScalarType.Precision) for
           prec in precisions):
        # All precisions are of ScalarType.Precision type.
        # TODO 3271 - at the moment this is wrong as default reals are defined
        # as UNDEFINED. This means that the precision of an operation
        # involving a default REAL and a DOUBLE PRECISION will be computed
        # to be UNDEFINED instead of DOUBLE.
        if ScalarType.Precision.UNDEFINED in precisions:
            return ScalarType.Precision.UNDEFINED
        if ScalarType.Precision.DOUBLE in precisions:
            return ScalarType.Precision.DOUBLE
        raise InternalError(
            f"Could not compute precision for inputs "
            f"'{precisions}' due to unknown Precisions being supplied."
        )

    # We can't reason about the precision of the result.
    return ScalarType.Precision.UNDEFINED


def compute_scalar_type(
    argtypes: list[DataType]
) -> ScalarType:
    '''
    Examines the argtypes to determine the base type of the result of a
    numerical operation with them as operands. Uses the rules in Section 7.2
    of the Fortran2008 standard. If the type cannot be determined then an
    instance of `UnresolvedType` is returned.

    :param argtypes: the types of the arguments.

    :returns: the elemental type of the result of the input arguments.

    :raises InternalError: If more than two argument types are provided.
    :raises TypeError: If the types differ and any are not a numeric datatype.
    '''

    if len(argtypes) > 2:
        raise InternalError(
            f"Can't compute the scalar type of more than 2 inputs but "
            f"{len(argtypes)} were provided."
        )

    if any(isinstance(atype, UnresolvedType) or
           isinstance(atype.intrinsic, UnresolvedType) for atype in argtypes):
        # If any of the input intrinsics are UnresolvedTypes then we can't do
        # better than UnresolvedType
        return UnresolvedType()

    # If all the datatypes are the same then we can return the first.
    if argtypes[0] == argtypes[1]:
        if isinstance(argtypes[0], ArrayType):
            return argtypes[0].elemental_type
        return argtypes[0]

    # If the arguments are the same type but have different precisions then
    # we need to compute the resulting precision.
    if argtypes[0].intrinsic == argtypes[1].intrinsic:
        # Operands are of the same intrinsic type.
        precision = compute_precision([argtypes[0].precision,
                                       argtypes[1].precision])
        return ScalarType(argtypes[0].intrinsic, precision)

    # If either has COMPLEX intrinsic type, the result is a COMPLEX.
    # Otherwise, if either has REAL intrinsic type, the result is a REAL.
    for intrin in [ScalarType.Intrinsic.COMPLEX,
                   ScalarType.Intrinsic.REAL]:
        for argtype in argtypes:
            if argtype.intrinsic == intrin:
                if isinstance(argtype, ArrayType):
                    return argtype.elemental_type
                return argtype

    # Otherwise, the type of the result is not consistent with
    # a numerical operation
    raise TypeError(
        f"Couldn't compute the type of an operation as the types of the "
        "arguments differ and one is non-numeric. Provided "
        f"arguments were '{argtypes[0]}' and '{argtypes[1]}'."
    )
