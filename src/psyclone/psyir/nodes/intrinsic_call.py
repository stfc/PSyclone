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
# -----------------------------------------------------------------------------

''' This module contains the IntrinsicCall node implementation.'''

from collections import namedtuple
from collections.abc import Iterable
from enum import Enum

from psyclone.core import AccessType, VariablesAccessMap
from psyclone.psyir.nodes.call import Call
from psyclone.psyir.nodes.datanode import DataNode
from psyclone.psyir.nodes.literal import Literal
from psyclone.psyir.nodes.reference import Reference
from psyclone.psyir.symbols import IntrinsicSymbol, NoType, UnresolvedType

# pylint: disable=too-many-branches

# Named tuple for describing the attributes of each intrinsic
IAttr = namedtuple(
    'IAttr', ('name is_pure is_elemental is_inquiry required_args '
              'optional_args datatype')
)
# Alternatively we could use an Enum to decrive the intrinsic types
# IntrinsicType = Enum('IntrinsicType',
#    'Atomic Collective Elemental Inquiry Pure Impure Transformational'
# )
# And let the IntrinsicCall is_pure, is_elemental, ... do the conversion

# Named tuple for describing the properties of the required arguments to
# a particular intrinsic. If there's no limit on the number of arguments
# then `max_count` will be None.
ArgDesc = namedtuple('ArgDesc', 'min_count max_count types')


def maxval_datatype(intrinsic: Call):
    '''
    '''
    array = intrinsic.arguments[0]
    # Look to see whether we have the 'dim' argument.
    return NoType()


class IntrinsicCall(Call):
    ''' Node representing a call to an intrinsic routine (function or
    subroutine). This can be found as a standalone statement
    or an expression.

    :param intrinsic: the type of Intrinsic being created.
    :type intrinsic: py:class:`psyclone.psyir.IntrinsicCall.Intrinsic`
    :param kwargs: additional keyword arguments provided to the PSyIR node.
    :type kwargs: unwrapped dict.

    :raises TypeError: if the 'intrinsic' argument is not an Intrinsic type.

    '''
    # Textual description of the node.
    _children_valid_format = "[DataNode]*"
    _text_name = "IntrinsicCall"
    _colour = "cyan"

    #: The type of Symbol this Call must refer to. Used for type checking in
    #: the constructor (of the parent class).
    _symbol_type = IntrinsicSymbol

    class Intrinsic(IAttr, Enum):
        ''' Enum of all intrinsics with their attributes as values using the
        IAttr namedtuple format:

            NAME = IAttr(name, is_pure, is_elemental, is_inquiry,
                         required_args, optional_args)

        Note that name is duplicated inside IAttr because each item in the
        Enum must have a different value, and without the name that would
        not be guaranteed.

        '''
        # Fortran special-case statements (technically not Fortran intrinsics
        # but in PSyIR they are represented as Intrinsics)
        ALLOCATE = IAttr(
            'ALLOCATE', False, False, False,
            ArgDesc(1, None, Reference),
            {"mold": Reference, "source": Reference, "stat": Reference,
             "errmsg": Reference}, lambda self: NoType())
        DEALLOCATE = IAttr(
            'DEALLOCATE', False, False, False,
            ArgDesc(1, None, Reference), {"stat": Reference},
            lambda self: NoType())
        NULLIFY = IAttr(
            'NULLIFY', False, False, False,
            ArgDesc(1, None, Reference), {}, lambda self: NoType())

        # Fortran Intrinsics (from Fortran 2018 standard table 16.1)
        ABS = IAttr(
            'ABS', True, True, False,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        ACHAR = IAttr(
            'ACHAR', True, True, False,
            ArgDesc(1, 1, DataNode), {"kind": DataNode},
            lambda self: UnresolvedType())
        ACOS = IAttr(
            'ACOS', True, True, False,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        ACOSH = IAttr(
            'ACOSH', True, True, False,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        ADJUSTL = IAttr(
            'ADJUSTL', True, True, False,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        ADJUSTR = IAttr(
            'ADJUSTR', True, True, False,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        AIMAG = IAttr(
            'AIMAG', True, True, False,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        AINT = IAttr(
            'AINT', True, True, False,
            ArgDesc(1, 1, DataNode), {"kind": DataNode},
            lambda self: UnresolvedType())
        ALL = IAttr(
            'ALL', True, False, False,
            ArgDesc(1, 1, DataNode), {"dim": DataNode},
            lambda self: UnresolvedType())  # ?
        ALLOCATED = IAttr(
            'ALLOCATED', True, False, True,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        ANINT = IAttr(
            'ANINT', True, True, False,
            ArgDesc(1, 1, DataNode), {"kind": DataNode},
            lambda self: UnresolvedType())
        ANY = IAttr(
            'ANY', True, False, False,
            ArgDesc(1, 1, DataNode), {"dim": DataNode},
            lambda self: UnresolvedType())  # ?
        ASIN = IAttr(
            'ASIN', True, True, False,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        ASINH = IAttr(
            'ASINH', True, True, False,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        ASSOCIATED = IAttr(
            'ASSOCIATED', False, False, True,
            ArgDesc(1, 1, DataNode), {"target": DataNode},
            lambda self: UnresolvedType())
        ATAN = IAttr(
            'ATAN', True, True, False,
            ArgDesc(1, 2, DataNode), {}, lambda self: UnresolvedType())
        ATAN2 = IAttr(
            'ATAN2', True, True, False,
            ArgDesc(2, 2, DataNode), {}, lambda self: UnresolvedType())
        ATANH = IAttr(
            'ATANH', True, True, False,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        ATOMIC_ADD = IAttr(
            'ATOMIC_ADD', True, True, False,
            ArgDesc(2, 2, DataNode), {"stat": DataNode},
            lambda self: UnresolvedType())
        ATOMIC_AND = IAttr(
            'ATOMIC_AND', True, True, False,
            ArgDesc(2, 2, DataNode), {"stat": DataNode},
            lambda self: UnresolvedType())
        ATOMIC_CAS = IAttr(
            'ATOMIC_CAS', True, True, False,
            ArgDesc(2, 2, DataNode), {"stat": DataNode},
            lambda self: UnresolvedType())
        ATOMIC_DEFINE = IAttr(
            'ATOMIC_DEFINE', True, True, False,
            ArgDesc(2, 2, DataNode), {"stat": DataNode},
            lambda self: UnresolvedType())
        ATOMIC_FETCH_ADD = IAttr(
            'ATOMIC_FETCH_ADD', True, True, False,
            ArgDesc(3, 3, DataNode), {"stat": DataNode},
            lambda self: UnresolvedType())
        ATOMIC_FETCH_AND = IAttr(
            'ATOMIC_FETCH_AND', True, True, False,
            ArgDesc(3, 3, DataNode), {"stat": DataNode},
            lambda self: UnresolvedType())
        ATOMIC_FETCH_OR = IAttr(
            'ATOMIC_FETCH_OR', True, True, False,
            ArgDesc(3, 3, DataNode), {"stat": DataNode},
            lambda self: UnresolvedType())
        ATOMIC_FETCH_XOR = IAttr(
            'ATOMIC_FETCH_XOR', True, True, False,
            ArgDesc(3, 3, DataNode), {"stat": DataNode},
            lambda self: UnresolvedType())
        ATOMIC_OR = IAttr(
            'ATOMIC_OR', True, True, False,
            ArgDesc(2, 2, DataNode), {"stat": DataNode},
            lambda self: UnresolvedType())
        ATOMIC_REF = IAttr(
            'ATOMIC_REF', True, True, False,
            ArgDesc(2, 2, DataNode), {"stat": DataNode},
            lambda self: UnresolvedType())
        ATOMIC_XOR = IAttr(
            'ATOMIC_XOR', True, True, False,
            ArgDesc(2, 2, DataNode), {"stat": DataNode},
            lambda self: UnresolvedType())
        BESSEL_J0 = IAttr(
            'BESSEL_J0', True, True, False,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        BESSEL_J1 = IAttr(
            'BESSEL_J1', True, True, False,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        BESSEL_JN = IAttr(
            'BESSEL_JN', True, None, False,
            ArgDesc(2, 3, DataNode), {}, lambda self: UnresolvedType())
        BESSEL_Y0 = IAttr(
            'BESSEL_Y0', True, True, False,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        BESSEL_Y1 = IAttr(
            'BESSEL_Y1', True, True, False,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        BESSEL_YN = IAttr(
            'BESSEL_YN', True, None, False,
            ArgDesc(2, 3, DataNode), {}, lambda self: UnresolvedType())
        BGE = IAttr(
            'BGE', True, True, False,
            ArgDesc(2, 2, DataNode), {}, lambda self: UnresolvedType())
        BGT = IAttr(
            'BGT', True, True, False,
            ArgDesc(2, 2, DataNode), {}, lambda self: UnresolvedType())
        BIT_SIZE = IAttr(
            'BIT_SIZE', True, False, True,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        BLE = IAttr(
            'BLE', True, True, False,
            ArgDesc(2, 2, DataNode), {}, lambda self: UnresolvedType())
        BLT = IAttr(
            'BLT', True, True, False,
            ArgDesc(2, 2, DataNode), {}, lambda self: UnresolvedType())
        BTEST = IAttr(
            'BTEST', True, True, False,
            ArgDesc(2, 2, DataNode), {}, lambda self: UnresolvedType())
        CEILING = IAttr(
            'CEILING', True, True, False,
            ArgDesc(1, 1, DataNode), {"kind": DataNode},
            lambda self: UnresolvedType())
        CHAR = IAttr(
            'CHAR', True, True, False,
            ArgDesc(1, 1, DataNode), {"kind": DataNode},
            lambda self: UnresolvedType())
        CMPLX = IAttr(
            'CMPLX', True, True, False,
            ArgDesc(1, 1, DataNode), {"Y": DataNode, "kind": DataNode},
            lambda self: UnresolvedType())
        CO_BROADCAST = IAttr(
            'CO_BROADCAST', True, False, False,
            ArgDesc(1, 2, DataNode), {"stat": DataNode, "errmsg": DataNode},
            lambda self: UnresolvedType())
        CO_MAX = IAttr(
            'CO_MAX', True, False, False,
            ArgDesc(1, 1, DataNode),
            {"result_image": DataNode, "stat": DataNode, "errmsg": DataNode},
            lambda self: UnresolvedType())
        CO_MIN = IAttr(
            'CO_MIN', True, False, False,
            ArgDesc(1, 1, DataNode),
            {"result_image": DataNode, "stat": DataNode, "errmsg": DataNode},
            lambda self: UnresolvedType())
        CO_REDUCE = IAttr(
            'CO_REDUCE', True, False, False,
            ArgDesc(1, 2, DataNode),
            {"result_image": DataNode, "stat": DataNode, "errmsg": DataNode},
            lambda self: UnresolvedType())
        CO_SUM = IAttr(
            'CO_SUM', True, False, False,
            ArgDesc(1, 1, DataNode),
            {"result_image": DataNode, "stat": DataNode, "errmsg": DataNode},
            lambda self: UnresolvedType())
        COMMAND_ARGUMENT_COUNT = IAttr(
            'COMMAND_ARGUMENT_COUNT', True, False, False,
            ArgDesc(0, 0, None), {}, lambda self: UnresolvedType())
        CONJG = IAttr(
            'CONJG', True, True, False,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        COS = IAttr(
            'COS', True, True, False,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        COSH = IAttr(
            'COSH', True, True, False,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        COSHAPE = IAttr(
            'COSHAPE', True, False, True,
            ArgDesc(1, 1, DataNode), {"kind": DataNode},
            lambda self: UnresolvedType())
        COUNT = IAttr(
            'COUNT', True, False, False,
            ArgDesc(1, 1, DataNode), {"dim": DataNode, "kind": DataNode},
            lambda self: UnresolvedType())
        CPU_TIME = IAttr(
            'CPU_TIME', False, False, False,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        CSHIFT = IAttr(
            'CSHIFT', True, False, False,
            ArgDesc(2, 2, DataNode), {"dim": DataNode},
            lambda self: UnresolvedType())
        DATE_AND_TIME = IAttr(
            'DATE_AND_TIME', False, False, False,
            ArgDesc(0, 0, DataNode),
            {"date": DataNode, "time": DataNode,
             "zone": DataNode, "values": DataNode},
            lambda self: UnresolvedType())
        DBLE = IAttr(
            'DBLE', True, True, False,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        DIGITS = IAttr(
            'DIGITS', True, False, True,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        DIM = IAttr(
            'DIM', True, True, False,
            ArgDesc(2, 2, DataNode), {}, lambda self: UnresolvedType())
        DOT_PRODUCT = IAttr(
            'DOT_PRODUCT', True, False, False,
            ArgDesc(2, 2, DataNode), {}, lambda self: UnresolvedType())
        DPROD = IAttr(
            'DPROD', True, True, False,
            ArgDesc(2, 2, DataNode), {}, lambda self: UnresolvedType())
        DSHIFTL = IAttr(
            'DSHIFTL', True, True, False,
            ArgDesc(3, 3, DataNode), {}, lambda self: UnresolvedType())
        DSHIFTR = IAttr(
            'DSHIFTR', True, True, False,
            ArgDesc(3, 3, DataNode), {}, lambda self: UnresolvedType())
        EOSHIFT = IAttr(
            'EOSHIFT', True, False, False,
            ArgDesc(2, 2, DataNode), {"boundary": DataNode, "dim": DataNode},
            lambda self: UnresolvedType())
        EPSILON = IAttr(
            'EPSILON', True, False, True,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        ERF = IAttr(
            'ERF', True, True, False,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        ERFC = IAttr(
            'ERFC', True, True, False,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        ERFC_SCALED = IAttr(
            'ERFC_SCALED', True, True, False,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        EVENT_QUERY = IAttr(
            'EVENT_QUERY', False, False, False,
            ArgDesc(2, 2, DataNode), {"stat": DataNode},
            lambda self: UnresolvedType())
        EXECUTE_COMMAND_LINE = IAttr(
            'EXECUTE_COMMAND_LINE', False, False, False,
            ArgDesc(2, 2, DataNode),
            {"wait": DataNode, "exitstat": DataNode,
             "cmdstat": DataNode, "cmdmsg": DataNode},
            lambda self: UnresolvedType())
        EXP = IAttr(
            'EXP', True, True, False,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        EXPONENT = IAttr(
            'EXPONENT', True, True, False,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        EXTENDS_TYPE_OF = IAttr(
            'EXTENDS_TYPE_OF', True, False, True,
            ArgDesc(2, 2, DataNode), {}, lambda self: UnresolvedType())
        FAILED_IMAGES = IAttr(
            'FAILED_IMAGES', False, False, False,
            ArgDesc(0, 0, DataNode), {"team": DataNode, "kind": DataNode},
            lambda self: UnresolvedType())
        FINDLOC = IAttr(
            'FINDLOC', True, False, False,
            ArgDesc(2, 3, DataNode),
            {"mask": DataNode, "kind": DataNode, "back": DataNode},
            lambda self: UnresolvedType())
        FLOAT = IAttr(
            'FLOAT', True, True, False,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        FLOOR = IAttr(
            'FLOOR', True, True, False,
            ArgDesc(1, 1, DataNode), {"kind": DataNode},
            lambda self: UnresolvedType())
        FRACTION = IAttr(
            'FRACTION', True, True, False,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        GAMMA = IAttr(
            'GAMMA', True, True, False,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        GET_COMMAND = IAttr(
            'GET_COMMAND', False, False, False,
            ArgDesc(0, 0, DataNode),
            {"command": DataNode, "length": DataNode,
             "status": DataNode, "errmsg": DataNode},
            lambda self: UnresolvedType())
        GET_COMMAND_ARGUMENT = IAttr(
            'GET_COMMAND_ARGUMENT', False, False, False,
            ArgDesc(1, 1, DataNode),
            {"value": DataNode, "length": DataNode,
             "status": DataNode, "errmsg": DataNode},
            lambda self: UnresolvedType())
        GET_ENVIRONMENT_VARIABLE = IAttr(
            'GET_ENVIRONMENT_VARIABLE', False, False, False,
            ArgDesc(1, 1, DataNode),
            {"value": DataNode, "length": DataNode, "status": DataNode,
             "trim_name": DataNode, "errmsg": DataNode},
            lambda self: UnresolvedType())
        GET_TEAM = IAttr(
            'GET_TEAM', True, False, False,
            ArgDesc(0, 0, DataNode), {"level": DataNode},
            lambda self: UnresolvedType())
        HUGE = IAttr(
            'HUGE', True, False, True,
            ArgDesc(1, 1, (Reference, Literal)), {},
            lambda self: UnresolvedType())
        HYPOT = IAttr(
            'HYPOT', True, True, False,
            ArgDesc(2, 2, (DataNode)), {}, lambda self: UnresolvedType())
        IACHAR = IAttr(
            'IACHAR', True, True, False,
            ArgDesc(1, 1, (DataNode)), {"kind": DataNode},
            lambda self: UnresolvedType())
        IALL = IAttr(
            'IALL', True, False, False,
            ArgDesc(1, 1, (DataNode)), {"dim": DataNode, "kind": DataNode},
            lambda self: UnresolvedType())
        IAND = IAttr(
            'IAND', True, True, False,
            ArgDesc(2, 2, (DataNode)), {}, lambda self: UnresolvedType())
        IANY = IAttr(
            'IANY', True, False, False,
            ArgDesc(1, 1, (DataNode)), {"dim": DataNode, "kind": DataNode},
            lambda self: UnresolvedType())
        IBCLR = IAttr(
            'IBCLR', True, True, False,
            ArgDesc(2, 2, (DataNode)), {}, lambda self: UnresolvedType())
        IBITS = IAttr(
            'IBITS', True, True, False,
            ArgDesc(3, 3, (DataNode)), {}, lambda self: UnresolvedType())
        IBSET = IAttr(
            'IBSET', True, True, False,
            ArgDesc(2, 2, (DataNode)), {}, lambda self: UnresolvedType())
        ICHAR = IAttr(
            'ICHAR', True, True, False,
            ArgDesc(1, 1, (DataNode)), {"kind": DataNode},
            lambda self: UnresolvedType())
        IEOR = IAttr(
            'IEOR', True, True, False,
            ArgDesc(2, 2, (DataNode)), {}, lambda self: UnresolvedType())
        IMAGE_INDEX = IAttr(
            'IMAGE_INDEX', True, False, True,
            ArgDesc(2, 3, (DataNode)), {}, lambda self: UnresolvedType())
        IMAGE_STATUS = IAttr(
            'IMAGE_STATUS', True, False, False,
            ArgDesc(1, 1, (DataNode)), {"team": DataNode},
            lambda self: UnresolvedType())
        INDEX = IAttr(
            'INDEX', True, True, False,
            ArgDesc(2, 2, (DataNode)), {"back": DataNode, "kind": DataNode},
            lambda self: UnresolvedType())
        INT = IAttr(
            'INT', True, True, False,
            ArgDesc(1, 1, (DataNode)), {"kind": DataNode},
            lambda self: UnresolvedType())
        IOR = IAttr(
            'IOR', True, True, False,
            ArgDesc(2, 2, (DataNode)), {}, lambda self: UnresolvedType())
        IPARITY = IAttr(
            'IPARITY', True, False, False,
            ArgDesc(1, 2, (DataNode)), {"mask": DataNode},
            lambda self: UnresolvedType())
        ISHFT = IAttr(
            'ISHFT', True, True, False,
            ArgDesc(2, 2, (DataNode)), {}, lambda self: UnresolvedType())
        ISHFTC = IAttr(
            'ISHFTC', True, True, False,
            ArgDesc(2, 2, (DataNode)), {"size": DataNode},
            lambda self: UnresolvedType())
        IS_CONTIGUOUS = IAttr(
            'IS_CONTIGUOUS', True, False, True,
            ArgDesc(1, 1, (DataNode)), {}, lambda self: UnresolvedType())
        IS_IOSTAT_END = IAttr(
            'IS_IOSTAT_END', True, True, False,
            ArgDesc(1, 1, (DataNode)), {}, lambda self: UnresolvedType())
        IS_IOSTAT_EOR = IAttr(
            'IS_IOSTAT_EOR', True, True, False,
            ArgDesc(1, 1, (DataNode)), {}, lambda self: UnresolvedType())
        KIND = IAttr(
            'KIND', True, False, True,
            ArgDesc(1, 1, (DataNode)), {}, lambda self: UnresolvedType())
        LBOUND = IAttr(
            'LBOUND', True, False, True,
            ArgDesc(1, 1, (DataNode)), {"dim": DataNode, "kind": DataNode},
            lambda self: UnresolvedType())
        LCOBOUND = IAttr(
            'LCOBOUND', True, False, True,
            ArgDesc(1, 1, (DataNode)), {"dim": DataNode, "kind": DataNode},
            lambda self: UnresolvedType())
        LEADZ = IAttr(
            'LEADZ', True, True, False,
            ArgDesc(1, 1, (DataNode)), {}, lambda self: UnresolvedType())
        LEN = IAttr(
            'LEN', True, False, True,
            ArgDesc(1, 1, (DataNode)), {"kind": DataNode},
            lambda self: UnresolvedType())
        LEN_TRIM = IAttr(
            'LEN_TRIM', True, True, False,
            ArgDesc(1, 1, (DataNode)), {"kind": DataNode},
            lambda self: UnresolvedType())
        LGE = IAttr(
            'LGE', True, True, False,
            ArgDesc(2, 2, (DataNode)), {}, lambda self: UnresolvedType())
        LGT = IAttr(
            'LGT', True, True, False,
            ArgDesc(2, 2, (DataNode)), {}, lambda self: UnresolvedType())
        LLE = IAttr(
            'LLE', True, True, False,
            ArgDesc(2, 2, (DataNode)), {}, lambda self: UnresolvedType())
        LLT = IAttr(
            'LLT', True, True, False,
            ArgDesc(2, 2, (DataNode)), {}, lambda self: UnresolvedType())
        LOG = IAttr(
            'LOG', True, True, False,
            ArgDesc(1, 1, (DataNode)), {}, lambda self: UnresolvedType())
        LOG_GAMMA = IAttr(
            'LOG_GAMMA', True, True, False,
            ArgDesc(1, 1, (DataNode)), {}, lambda self: UnresolvedType())
        LOG10 = IAttr(
            'LOG10', True, True, False,
            ArgDesc(1, 1, (DataNode)), {}, lambda self: UnresolvedType())
        LOGICAL = IAttr(
            'LOGICAL', True, True, False,
            ArgDesc(1, 1, (DataNode)), {"kind": DataNode},
            lambda self: UnresolvedType())
        MASKL = IAttr(
            'MASKL', True, True, False,
            ArgDesc(1, 1, (DataNode)), {"kind": DataNode},
            lambda self: UnresolvedType())
        MASKR = IAttr(
            'MASKR', True, True, False,
            ArgDesc(1, 1, (DataNode)), {"kind": DataNode},
            lambda self: UnresolvedType())
        MATMUL = IAttr(
            'MATMUL', True, False, False,
            ArgDesc(2, 2, DataNode), {}, lambda self: UnresolvedType())
        MAX = IAttr(
            'MAX', True, True, False,
            ArgDesc(2, None, DataNode), {}, lambda self: UnresolvedType())
        MAXEXPONENT = IAttr(
            'MAXEXPONENT', True, False, True,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        MAXLOC = IAttr(
            'MAXLOC', True, False, False,
            ArgDesc(1, 2, DataNode),
            {"dim": DataNode, "mask": DataNode, "kind": DataNode,
             "back": DataNode}, lambda self: UnresolvedType())
        MAXVAL = IAttr(
            'MAXVAL', True, False, False,
            ArgDesc(1, 1, DataNode),
            {"dim": DataNode, "mask": DataNode}, maxval_datatype)
        MERGE = IAttr(
            'MERGE', True, True, False,
            ArgDesc(3, 3, DataNode), {}, lambda self: UnresolvedType())
        MERGE_BITS = IAttr(
            'MERGE_BITS', True, True, False,
            ArgDesc(3, 3, DataNode), {}, lambda self: UnresolvedType())
        MIN = IAttr(
            'MIN', True, True, False,
            ArgDesc(2, None, DataNode), {}, lambda self: UnresolvedType())
        MINEXPONENT = IAttr(
            'MINEXPONENT', True, False, True,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        MINLOC = IAttr(
            'MINLOC', True, False, False,
            ArgDesc(1, 2, DataNode),
            {"dim": DataNode, "mask": DataNode, "kind": DataNode,
             "back": DataNode}, lambda self: UnresolvedType())
        MINVAL = IAttr(
            'MINVAL', True, False, False,
            ArgDesc(1, 1, DataNode),
            {"dim": DataNode, "mask": DataNode}, lambda self: UnresolvedType())
        MOD = IAttr(
            'MOD', True, True, False,
            ArgDesc(2, 2, DataNode), {}, lambda self: UnresolvedType())
        MODULO = IAttr(
            'MODULO', True, True, False,
            ArgDesc(2, 2, DataNode), {}, lambda self: UnresolvedType())
        MOVE_ALLOC = IAttr(
            'MOVE_ALLOC', False, False, False,
            ArgDesc(2, 2, DataNode), {"stat": DataNode, "errmsg": DataNode},
            lambda self: UnresolvedType())
        MVBITS = IAttr(
            'MVBITS', True, True, False,
            ArgDesc(5, 5, DataNode), {}, lambda self: UnresolvedType())
        NEAREST = IAttr(
            'NEAREST', True, True, False,
            ArgDesc(2, 2, DataNode), {}, lambda self: UnresolvedType())
        NEW_LINE = IAttr(
            'NEW_LINE', True, True, False,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        NINT = IAttr(
            'NINT', True, True, False,
            ArgDesc(1, 1, DataNode), {"kind": DataNode},
            lambda self: UnresolvedType())
        NORM2 = IAttr(
            'NORM2', True, False, False,
            ArgDesc(1, 2, DataNode), {}, lambda self: UnresolvedType())
        NOT = IAttr(
            'NOT', True, True, False,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        NULL = IAttr(
            'NULL', True, False, False,
            ArgDesc(0, 0, DataNode), {"mold": DataNode},
            lambda self: UnresolvedType())
        NUM_IMAGES = IAttr(
            'NUM_IMAGES', True, False, False,
            ArgDesc(0, 1, DataNode), {}, lambda self: UnresolvedType())
        OUT_OF_RANGE = IAttr(
            'OUT_OF_RANGE', True, True, False,
            ArgDesc(2, 2, DataNode), {"round": DataNode},
            lambda self: UnresolvedType())
        PACK = IAttr(
            'PACK', True, False, False,
            ArgDesc(2, 2, DataNode), {"vector": DataNode},
            lambda self: UnresolvedType())
        PARITY = IAttr(
            'PARITY', True, False, False,
            ArgDesc(1, 2, DataNode), {}, lambda self: UnresolvedType())
        POPCNT = IAttr(
            'POPCNT', True, True, False,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        POPPAR = IAttr(
            'POPPAR', True, True, False,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        PRECISION = IAttr(
            'PRECISION', True, False, True,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        PRESENT = IAttr(
            'PRESENT', True, False, True,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        PRODUCT = IAttr(
            'PRODUCT', True, False, False,
            ArgDesc(1, 1, DataNode), {"dim": DataNode, "mask": DataNode},
            lambda self: UnresolvedType())
        RADIX = IAttr(
            'RADIX', True, False, True,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        RANDOM_INIT = IAttr(
            'RANDOM_INIT', False, False, False,
            ArgDesc(2, 2, DataNode), {}, lambda self: UnresolvedType())
        RANDOM_NUMBER = IAttr(
            'RANDOM_NUMBER', False, False, False,
            ArgDesc(1, 1, Reference), {}, lambda self: UnresolvedType())
        RANDOM_SEED = IAttr(
            'RANDOM_SEED', False, False, False,
            ArgDesc(0, 0, Reference),
            {"size": DataNode, "put": DataNode, "Get": DataNode},
            lambda self: UnresolvedType())
        RANGE = IAttr(
            'RANGE', True, False, True,
            ArgDesc(1, 1, Reference), {}, lambda self: UnresolvedType())
        RANK = IAttr(
            'RANK', True, False, True,
            ArgDesc(1, 1, Reference), {}, lambda self: UnresolvedType())
        REAL = IAttr(
            'REAL', True, True, False,
            ArgDesc(1, 1, Reference), {"kind": DataNode},
            lambda self: UnresolvedType())
        REDUCE = IAttr(
            'REDUCE', True, False, False,
            ArgDesc(2, 3, Reference),
            {"mask": DataNode, "identity": DataNode, "ordered": DataNode},
            lambda self: UnresolvedType())
        REPEAT = IAttr(
            'REPEAT', True, False, False,
            ArgDesc(2, 2, Reference), {}, lambda self: UnresolvedType())
        RESHAPE = IAttr(
            'RESHAPE', True, False, False,
            ArgDesc(2, 2, Reference), {"pad": DataNode, "order": DataNode},
            lambda self: UnresolvedType())
        RRSPACING = IAttr(
            'RRSPACING', True, True, False,
            ArgDesc(1, 1, Reference), {}, lambda self: UnresolvedType())
        SAME_TYPE_AS = IAttr(
            'SAME_TYPE_AS', True, False, True,
            ArgDesc(2, 2, Reference), {}, lambda self: UnresolvedType())
        SCALE = IAttr(
            'SCALE', True, True, False,
            ArgDesc(2, 2, Reference), {}, lambda self: UnresolvedType())
        SCAN = IAttr(
            'SCAN', True, True, False,
            ArgDesc(2, 2, Reference), {"back": DataNode, "kind": DataNode},
            lambda self: UnresolvedType())
        SELECTED_CHAR_KIND = IAttr(
            'SELECTED_CHAR_KIND', True, False, False,
            ArgDesc(1, 1, Reference), {}, lambda self: UnresolvedType())
        SELECTED_INT_KIND = IAttr(
            'SELECTED_INT_KIND', True, False, False,
            ArgDesc(1, 1, Reference), {}, lambda self: UnresolvedType())
        SELECTED_REAL_KIND = IAttr(
            'SELECTED_REAL_KIND', True, False, False,
            ArgDesc(0, 0, Reference),
            {"P": DataNode, "R": DataNode, "radix": DataNode},
            lambda self: UnresolvedType())
        SET_EXPONENT = IAttr(
            'SET_EXPONENT', True, True, False,
            ArgDesc(2, 2, Reference), {}, lambda self: UnresolvedType())
        SHAPE = IAttr(
            'SHAPE', True, False, True,
            ArgDesc(1, 1, Reference), {"kind": DataNode},
            lambda self: UnresolvedType())
        SHIFTA = IAttr(
            'SHIFTA', True, True, False,
            ArgDesc(2, 2, Reference), {}, lambda self: UnresolvedType())
        SHIFTL = IAttr(
            'SHIFTL', True, True, False,
            ArgDesc(2, 2, Reference), {}, lambda self: UnresolvedType())
        SHIFTR = IAttr(
            'SHIFTR', True, True, False,
            ArgDesc(2, 2, Reference), {}, lambda self: UnresolvedType())
        SIGN = IAttr(
            'SIGN', True, True, False,
            ArgDesc(2, 2, DataNode), {}, lambda self: UnresolvedType())
        SIN = IAttr(
            'SIN', True, True, False,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        SINH = IAttr(
            'SINH', True, True, False,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        SIZE = IAttr(
            'SIZE', True, False, True,
            ArgDesc(1, 1, DataNode), {"dim": DataNode, "kind": DataNode},
            lambda self: UnresolvedType())
        SPACING = IAttr(
            'SPACING', True, True, False,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        SPREAD = IAttr(
            'SPREAD', True, False, False,
            ArgDesc(3, 3, DataNode), {}, lambda self: UnresolvedType())
        SQRT = IAttr(
            'SQRT', True, True, False,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        STOPPED_IMAGES = IAttr(
            'STOPPED_IMAGES', False, False, False,
            ArgDesc(0, 0, DataNode), {"team": DataNode, "kind": DataNode},
            lambda self: UnresolvedType())
        STORAGE_SIZE = IAttr(
            'STORAGE_SIZE', True, False, True,
            ArgDesc(1, 1, DataNode), {"kind": DataNode},
            lambda self: UnresolvedType())
        SUM = IAttr(
            'SUM', True, False, False,
            ArgDesc(1, 1, DataNode), {"dim": DataNode, "mask": DataNode},
            lambda self: UnresolvedType())
        SYSTEM_CLOCK = IAttr(
            'SYSTEM_CLOCK', False, False, False,
            ArgDesc(0, 0, DataNode),
            {"count": DataNode, "count_rate": DataNode, "count_max": DataNode},
            lambda self: UnresolvedType())
        TAN = IAttr(
            'TAN', True, True, False,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        TANH = IAttr(
            'TANH', True, True, False,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        TEAM_IMAGE = IAttr(
            'TEAM_IMAGE', True, False, False,
            ArgDesc(0, 0, DataNode), {"team": DataNode},
            lambda self: UnresolvedType())
        THIS_IMAGE = IAttr(
            'THIS_IMAGE', True, False, False,
            ArgDesc(0, 0, DataNode),
            {"coarray": DataNode, "team": DataNode, "dim": DataNode},
            lambda self: UnresolvedType())
        TINY = IAttr(
            'TINY', True, False, True,
            ArgDesc(1, 1, (Reference, Literal)), {},
            lambda self: UnresolvedType())
        TRAILZ = IAttr(
            'TRAILZ', True, True, False,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        TRANSFER = IAttr(
            'TRANSFER', True, False, False,
            ArgDesc(2, 2, DataNode), {"size": DataNode},
            lambda self: UnresolvedType())
        TRANSPOSE = IAttr(
            'TRANSPOSE', True, False, False,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        TRIM = IAttr(
            'TRIM', True, False, False,
            ArgDesc(1, 1, DataNode), {}, lambda self: UnresolvedType())
        UBOUND = IAttr(
            'UBOUND', True, False, True,
            ArgDesc(1, 1, DataNode), {"dim": DataNode, "kind": DataNode},
            lambda self: UnresolvedType())
        UCOBOUND = IAttr(
            'UCOBOUND', True, False, True,
            ArgDesc(1, 1, DataNode), {"dim": DataNode, "kind": DataNode},
            lambda self: UnresolvedType())
        UNPACK = IAttr(
            'UNPACK', True, False, False,
            ArgDesc(3, 3, DataNode), {}, lambda self: UnresolvedType())
        VERIFY = IAttr(
            'VERIFY', True, True, False,
            ArgDesc(2, 2, DataNode), {"back": DataNode, "kind": DataNode},
            lambda self: UnresolvedType())

        def __hash__(self):
            return hash(self.name)

    def __init__(self, intrinsic, **kwargs):
        if not isinstance(intrinsic, Enum) or intrinsic not in self.Intrinsic:
            raise TypeError(
                f"IntrinsicCall 'intrinsic' argument should be an "
                f"instance of IntrinsicCall.Intrinsic, but found "
                f"'{type(intrinsic).__name__}'.")

        # A Call expects a Reference to a Symbol, so give it a Reference
        # to an Intrinsicsymbol of the given intrinsic.
        super().__init__(**kwargs)
        self.addchild(Reference(IntrinsicSymbol(
            intrinsic.name,
            intrinsic,
            is_elemental=intrinsic.is_elemental,
            is_pure=intrinsic.is_pure
        )))
        self._intrinsic = intrinsic

    @property
    def datatype(self):
        '''
        '''
        return self._intrinsic.datatype(self)

    @property
    def intrinsic(self):
        ''' Return the type of intrinsic.

        :returns: enumerated type capturing the type of intrinsic.
        :rtype: :py:class:`psyclone.psyir.nodes.IntrinsicCall.Intrinsic`

        '''
        return self.routine.symbol.intrinsic

    def is_available_on_device(self, device_string: str = "") -> bool:
        '''
        :param device_string: optional string to identify the offloading
            device (or its compiler-platform family).
        :returns: whether this intrinsic is available on an accelerated device.

        :raises ValueError: if the provided 'device_string' is not one of the
            supported values.

        '''
        if not device_string:
            return self.intrinsic in DEFAULT_DEVICE_INTRINISCS
        if device_string == "nvfortran-all":
            return self.intrinsic in NVFORTRAN_ALL
        if device_string == "nvfortran-uniform":
            return self.intrinsic in NVFORTRAN_UNIFORM

        raise ValueError(
            f"Unsupported device_string value '{device_string}', the supported"
            " values are '' (default), 'nvfortran-all', 'nvfortran-uniform'")

    @classmethod
    def create(cls, intrinsic, arguments=()):
        '''Create an instance of this class given the type of intrinsic and a
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

        '''
        call = IntrinsicCall(intrinsic)

        if not isinstance(arguments, Iterable):
            raise TypeError(
                f"IntrinsicCall.create() 'arguments' argument should be an "
                f"Iterable but found '{type(arguments).__name__}'")

        # Validate the supplied arguments.
        last_named_arg = None
        pos_arg_count = 0
        for arg in arguments:
            if isinstance(arg, tuple):
                if not isinstance(arg[0], str):
                    raise TypeError(
                        f"Optional arguments to an IntrinsicCall must be "
                        f"specified by a (str, Reference) tuple but got "
                        f"a {type(arg[0]).__name__} instead of a str.")
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
                            f"got '{type(arg[1]).__name__}'")
                else:
                    # If it not in the optional_args list it must be positional
                    pos_arg_count += 1
            else:
                if last_named_arg:
                    raise ValueError(
                        f"Found a positional argument *after* a named "
                        f"argument ('{last_named_arg}'). This is invalid.'")
                if not isinstance(arg, intrinsic.required_args.types):
                    raise TypeError(
                        f"The '{intrinsic.name}' intrinsic requires that "
                        f"positional arguments be of type "
                        f"'{intrinsic.required_args.types}' "
                        f"but got a '{type(arg).__name__}'")
                pos_arg_count += 1

        if ((intrinsic.required_args.max_count is not None and
             pos_arg_count > intrinsic.required_args.max_count)
                or pos_arg_count < intrinsic.required_args.min_count):
            msg = f"The '{intrinsic.name}' intrinsic requires "
            if (intrinsic.required_args.max_count is not None and
                    intrinsic.required_args.max_count > 0):
                msg += (f"between {intrinsic.required_args.min_count} and "
                        f"{intrinsic.required_args.max_count} ")
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
        '''
        :returns: a map of all the symbol accessed inside this node, the
            keys are Signatures (unique identifiers to a symbol and its
            structure acccessors) and the values are SingleVariableAccessInfo
            (a sequence of AccessTypes).

        '''
        var_accesses = VariablesAccessMap()
        if self.intrinsic.is_inquiry and isinstance(self.arguments[0],
                                                    Reference):
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
        '''
        :returns: whether the routine being called is elemental (provided with
            an input array it will apply the operation individually to each of
            the array elements and return an array with the results). If this
            information is not known then it returns None.
        :rtype: NoneType | bool
        '''
        return self.intrinsic.is_elemental

    @property
    def is_pure(self):
        '''
        :returns: whether the routine being called is pure (guaranteed to
            return the same result when provided with the same argument
            values).  If this information is not known then it returns None.
        :rtype: NoneType | bool
        '''
        return self.intrinsic.is_pure

    @property
    def is_inquiry(self):
        '''
        :returns: whether the routine being called is a query function (i.e.
            returns information about its argument rather than accessing any
            data referenced by the argument). If this information is not known
            then it returns None.
        :rtype: NoneType | bool
        '''
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
    IntrinsicCall.Intrinsic.TINY, IntrinsicCall.Intrinsic.HUGE
)

# All nvfortran intrinsics available on GPUs
NVFORTRAN_ALL = NVFORTRAN_UNIFORM + (
    IntrinsicCall.Intrinsic.LOG10, IntrinsicCall.Intrinsic.REAL)

# For now the default intrinsics availabe on GPU are the same as nvfortran-all
DEFAULT_DEVICE_INTRINISCS = NVFORTRAN_ALL

# TODO #658 this can be removed once we have support for determining the
# type of a PSyIR expression.
# Intrinsics that perform a reduction on an array.
REDUCTION_INTRINSICS = [
    IntrinsicCall.Intrinsic.SUM, IntrinsicCall.Intrinsic.MINVAL,
    IntrinsicCall.Intrinsic.MAXVAL]
