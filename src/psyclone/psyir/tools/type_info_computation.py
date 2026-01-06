# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2026, Science and Technology Facilities Council.
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
# Authors A. B. G. Chalk, A. R. Porter, S. Siso, STFC Daresbury Lab
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' This module contains the implementation of a function that computes the
promotion of precisions or datatype for Fortran operations.'''

from typing import List, Union

from psyclone.errors import InternalError
from psyclone.psyir.nodes import Reference
from psyclone.psyir.symbols.datatypes import (
    ScalarType, UnresolvedType, DataType
)


def compute_precision(
        precisions: List[Union[int, ScalarType.Precision, Reference]]
) -> Union[int, ScalarType.Precision, Reference]:
    '''
    Compares the input precisions to determine the precision of the result
    of the operation.

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
        # TODO 3271 - at the moment this is wrong as reals are defined as
        # UNDEFINED, wheras it should keep as a DOUBLE with real + double
        # precision.
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
    argtypes: List[DataType]
) -> ScalarType.Intrinsic:
    '''
    Examines the argtypes to determine the base type of the
    operation using the rules in Section 7.2 of the Fortran2008 standard.
    If the type cannot be determined then an instance of `UnresolvedType`
    is returned.

    :param argtypes: the types of the arguments.

    :returns: the base type of the result of the input arguments.

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
    if (argtypes[0].intrinsic == argtypes[1].intrinsic and
            argtypes[0].precision == argtypes[1].precision):
        return argtypes[0]

    # TODO 1590 - ensure support for complex numbers here in the future.

    # If the arguments are the same type but have different precisions then
    # we need to compute the resulting precision.
    if argtypes[0].intrinsic == argtypes[1].intrinsic:
        # Operands are of the same intrinsic type.
        precision = compute_precision([argtypes[0].precision,
                                       argtypes[1].precision])
        return ScalarType(argtypes[0].intrinsic, precision)

    # If either has REAL intrinsic type, the result is a REAL.
    if argtypes[0].intrinsic == ScalarType.Intrinsic.REAL:
        return argtypes[0]
    if argtypes[1].intrinsic == ScalarType.Intrinsic.REAL:
        return argtypes[1]

    # Otherwise, the type of the result is not consistent with
    # a numerical operation
    raise TypeError(
        f"Couldn't compute the type of an operation as one or more of "
        f"the arguments have a non-numeric non-shared datatype. Provided "
        f"arguments were '{argtypes[0]}' and '{argtypes[1]}'."
    )
