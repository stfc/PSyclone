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
from psyclone.psyir.symbols import IntrinsicSymbol

# pylint: disable=too-many-branches

# Named tuple for describing the attributes of each intrinsic
IAttr = namedtuple(
    'IAttr', 'name is_pure is_elemental is_inquiry required_args optional_args'
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
             "errmsg": Reference})
        DEALLOCATE = IAttr(
            'DEALLOCATE', False, False, False,
            ArgDesc(1, None, Reference), {"stat": Reference})
        NULLIFY = IAttr(
            'NULLIFY', False, False, False,
            ArgDesc(1, None, Reference), {})

        # Fortran Intrinsics (from Fortran 2018 standard table 16.1)
        ABS = IAttr(
            'ABS', True, True, False,
            ArgDesc(1, 1, DataNode), {})
        ACHAR = IAttr(
            'ACHAR', True, True, False,
            ArgDesc(1, 1, DataNode), {"kind": DataNode})
        ACOS = IAttr(
            'ACOS', True, True, False,
            ArgDesc(1, 1, DataNode), {})
        ACOSH = IAttr(
            'ACOSH', True, True, False,
            ArgDesc(1, 1, DataNode), {})
        ADJUSTL = IAttr(
            'ADJUSTL', True, True, False,
            ArgDesc(1, 1, DataNode), {})
        ADJUSTR = IAttr(
            'ADJUSTR', True, True, False,
            ArgDesc(1, 1, DataNode), {})
        AIMAG = IAttr(
            'AIMAG', True, True, False,
            ArgDesc(1, 1, DataNode), {})
        AINT = IAttr(
            'AINT', True, True, False,
            ArgDesc(1, 1, DataNode), {"kind": DataNode})
        ALL = IAttr(
            'ALL', True, False, False,
            ArgDesc(1, 1, DataNode), {"dim": DataNode})  # ?
        ALLOCATED = IAttr(
            'ALLOCATED', True, False, True,
            ArgDesc(1, 1, DataNode), {})
        ANINT = IAttr(
            'ANINT', True, True, False,
            ArgDesc(1, 1, DataNode), {"kind": DataNode})
        ANY = IAttr(
            'ANY', True, False, False,
            ArgDesc(1, 1, DataNode), {"dim": DataNode})  # ?
        ASIN = IAttr(
            'ASIN', True, True, False,
            ArgDesc(1, 1, DataNode), {})
        ASINH = IAttr(
            'ASINH', True, True, False,
            ArgDesc(1, 1, DataNode), {})
        ASSOCIATED = IAttr(
            'ASSOCIATED', False, False, True,
            ArgDesc(1, 1, DataNode), {"target": DataNode})
        ATAN = IAttr(
            'ATAN', True, True, False,
            ArgDesc(1, 2, DataNode), {})
        ATAN2 = IAttr(
            'ATAN2', True, True, False,
            ArgDesc(2, 2, DataNode), {})
        ATANH = IAttr(
            'ATANH', True, True, False,
            ArgDesc(1, 1, DataNode), {})
        ATOMIC_ADD = IAttr(
            'ATOMIC_ADD', True, True, False,
            ArgDesc(2, 2, DataNode), {"stat": DataNode})
        ATOMIC_AND = IAttr(
            'ATOMIC_AND', True, True, False,
            ArgDesc(2, 2, DataNode), {"stat": DataNode})
        ATOMIC_CAS = IAttr(
            'ATOMIC_CAS', True, True, False,
            ArgDesc(2, 2, DataNode), {"stat": DataNode})
        ATOMIC_DEFINE = IAttr(
            'ATOMIC_DEFINE', True, True, False,
            ArgDesc(2, 2, DataNode), {"stat": DataNode})
        ATOMIC_FETCH_ADD = IAttr(
            'ATOMIC_FETCH_ADD', True, True, False,
            ArgDesc(3, 3, DataNode), {"stat": DataNode})
        ATOMIC_FETCH_AND = IAttr(
            'ATOMIC_FETCH_AND', True, True, False,
            ArgDesc(3, 3, DataNode), {"stat": DataNode})
        ATOMIC_FETCH_OR = IAttr(
            'ATOMIC_FETCH_OR', True, True, False,
            ArgDesc(3, 3, DataNode), {"stat": DataNode})
        ATOMIC_FETCH_XOR = IAttr(
            'ATOMIC_FETCH_XOR', True, True, False,
            ArgDesc(3, 3, DataNode), {"stat": DataNode})
        ATOMIC_OR = IAttr(
            'ATOMIC_OR', True, True, False,
            ArgDesc(2, 2, DataNode), {"stat": DataNode})
        ATOMIC_REF = IAttr(
            'ATOMIC_REF', True, True, False,
            ArgDesc(2, 2, DataNode), {"stat": DataNode})
        ATOMIC_XOR = IAttr(
            'ATOMIC_XOR', True, True, False,
            ArgDesc(2, 2, DataNode), {"stat": DataNode})
        BESSEL_J0 = IAttr(
            'BESSEL_J0', True, True, False,
            ArgDesc(1, 1, DataNode), {})
        BESSEL_J1 = IAttr(
            'BESSEL_J1', True, True, False,
            ArgDesc(1, 1, DataNode), {})
        BESSEL_JN = IAttr(
            'BESSEL_JN', True, None, False,
            ArgDesc(2, 3, DataNode), {})
        BESSEL_Y0 = IAttr(
            'BESSEL_Y0', True, True, False,
            ArgDesc(1, 1, DataNode), {})
        BESSEL_Y1 = IAttr(
            'BESSEL_Y1', True, True, False,
            ArgDesc(1, 1, DataNode), {})
        BESSEL_YN = IAttr(
            'BESSEL_YN', True, None, False,
            ArgDesc(2, 3, DataNode), {})
        BGE = IAttr(
            'BGE', True, True, False,
            ArgDesc(2, 2, DataNode), {})
        BGT = IAttr(
            'BGT', True, True, False,
            ArgDesc(2, 2, DataNode), {})
        BIT_SIZE = IAttr(
            'BIT_SIZE', True, False, True,
            ArgDesc(1, 1, DataNode), {})
        BLE = IAttr(
            'BLE', True, True, False,
            ArgDesc(2, 2, DataNode), {})
        BLT = IAttr(
            'BLT', True, True, False,
            ArgDesc(2, 2, DataNode), {})
        BTEST = IAttr(
            'BTEST', True, True, False,
            ArgDesc(2, 2, DataNode), {})
        CEILING = IAttr(
            'CEILING', True, True, False,
            ArgDesc(1, 1, DataNode), {"kind": DataNode})
        CHAR = IAttr(
            'CHAR', True, True, False,
            ArgDesc(1, 1, DataNode), {"kind": DataNode})
        CMPLX = IAttr(
            'CMPLX', True, True, False,
            ArgDesc(1, 1, DataNode), {"Y": DataNode, "kind": DataNode})
        CO_BROADCAST = IAttr(
            'CO_BROADCAST', True, False, False,
            ArgDesc(1, 2, DataNode), {"stat": DataNode, "errmsg": DataNode})
        CO_MAX = IAttr(
            'CO_MAX', True, False, False,
            ArgDesc(1, 1, DataNode),
            {"result_image": DataNode, "stat": DataNode, "errmsg": DataNode})
        CO_MIN = IAttr(
            'CO_MIN', True, False, False,
            ArgDesc(1, 1, DataNode),
            {"result_image": DataNode, "stat": DataNode, "errmsg": DataNode})
        CO_REDUCE = IAttr(
            'CO_REDUCE', True, False, False,
            ArgDesc(1, 2, DataNode),
            {"result_image": DataNode, "stat": DataNode, "errmsg": DataNode})
        CO_SUM = IAttr(
            'CO_SUM', True, False, False,
            ArgDesc(1, 1, DataNode),
            {"result_image": DataNode, "stat": DataNode, "errmsg": DataNode})
        COMMAND_ARGUMENT_COUNT = IAttr(
            'COMMAND_ARGUMENT_COUNT', True, False, False,
            ArgDesc(0, 0, None), {})
        CONJG = IAttr(
            'CONJG', True, True, False,
            ArgDesc(1, 1, DataNode), {})
        COS = IAttr(
            'COS', True, True, False,
            ArgDesc(1, 1, DataNode), {})
        COSH = IAttr(
            'COSH', True, True, False,
            ArgDesc(1, 1, DataNode), {})
        COSHAPE = IAttr(
            'COSHAPE', True, False, True,
            ArgDesc(1, 1, DataNode), {"kind": DataNode})
        COUNT = IAttr(
            'COUNT', True, False, False,
            ArgDesc(1, 1, DataNode), {"dim": DataNode, "kind": DataNode})
        CPU_TIME = IAttr(
            'CPU_TIME', False, False, False,
            ArgDesc(1, 1, DataNode), {})
        CSHIFT = IAttr(
            'CSHIFT', True, False, False,
            ArgDesc(2, 2, DataNode), {"dim": DataNode})
        DATE_AND_TIME = IAttr(
            'DATE_AND_TIME', False, False, False,
            ArgDesc(0, 0, DataNode),
            {"date": DataNode, "time": DataNode,
             "zone": DataNode, "values": DataNode})
        DBLE = IAttr(
            'DBLE', True, True, False,
            ArgDesc(1, 1, DataNode), {})
        DIGITS = IAttr(
            'DIGITS', True, False, True,
            ArgDesc(1, 1, DataNode), {})
        DIM = IAttr(
            'DIM', True, True, False,
            ArgDesc(2, 2, DataNode), {})
        DOT_PRODUCT = IAttr(
            'DOT_PRODUCT', True, False, False,
            ArgDesc(2, 2, DataNode), {})
        DPROD = IAttr(
            'DPROD', True, True, False,
            ArgDesc(2, 2, DataNode), {})
        DSHIFTL = IAttr(
            'DSHIFTL', True, True, False,
            ArgDesc(3, 3, DataNode), {})
        DSHIFTR = IAttr(
            'DSHIFTR', True, True, False,
            ArgDesc(3, 3, DataNode), {})
        EOSHIFT = IAttr(
            'EOSHIFT', True, False, False,
            ArgDesc(2, 2, DataNode), {"boundary": DataNode, "dim": DataNode})
        EPSILON = IAttr(
            'EPSILON', True, False, True,
            ArgDesc(1, 1, DataNode), {})
        ERF = IAttr(
            'ERF', True, True, False,
            ArgDesc(1, 1, DataNode), {})
        ERFC = IAttr(
            'ERFC', True, True, False,
            ArgDesc(1, 1, DataNode), {})
        ERFC_SCALED = IAttr(
            'ERFC_SCALED', True, True, False,
            ArgDesc(1, 1, DataNode), {})
        EVENT_QUERY = IAttr(
            'EVENT_QUERY', False, False, False,
            ArgDesc(2, 2, DataNode), {"stat": DataNode})
        EXECUTE_COMMAND_LINE = IAttr(
            'EXECUTE_COMMAND_LINE', False, False, False,
            ArgDesc(2, 2, DataNode),
            {"wait": DataNode, "exitstat": DataNode,
             "cmdstat": DataNode, "cmdmsg": DataNode})
        EXP = IAttr(
            'EXP', True, True, False,
            ArgDesc(1, 1, DataNode), {})
        EXPONENT = IAttr(
            'EXPONENT', True, True, False,
            ArgDesc(1, 1, DataNode), {})
        EXTENDS_TYPE_OF = IAttr(
            'EXTENDS_TYPE_OF', True, False, True,
            ArgDesc(2, 2, DataNode), {})
        FAILED_IMAGES = IAttr(
            'FAILED_IMAGES', False, False, False,
            ArgDesc(0, 0, DataNode), {"team": DataNode, "kind": DataNode})
        FINDLOC = IAttr(
            'FINDLOC', True, False, False,
            ArgDesc(2, 3, DataNode),
            {"mask": DataNode, "kind": DataNode, "back": DataNode})
        FLOAT = IAttr(
            'FLOAT', True, True, False,
            ArgDesc(1, 1, DataNode), {})
        FLOOR = IAttr(
            'FLOOR', True, True, False,
            ArgDesc(1, 1, DataNode), {"kind": DataNode})
        FRACTION = IAttr(
            'FRACTION', True, True, False,
            ArgDesc(1, 1, DataNode), {})
        GAMMA = IAttr(
            'GAMMA', True, True, False,
            ArgDesc(1, 1, DataNode), {})
        GET_COMMAND = IAttr(
            'GET_COMMAND', False, False, False,
            ArgDesc(0, 0, DataNode),
            {"command": DataNode, "length": DataNode,
             "status": DataNode, "errmsg": DataNode})
        GET_COMMAND_ARGUMENT = IAttr(
            'GET_COMMAND_ARGUMENT', False, False, False,
            ArgDesc(1, 1, DataNode),
            {"value": DataNode, "length": DataNode,
             "status": DataNode, "errmsg": DataNode})
        GET_ENVIRONMENT_VARIABLE = IAttr(
            'GET_ENVIRONMENT_VARIABLE', False, False, False,
            ArgDesc(1, 1, DataNode),
            {"value": DataNode, "length": DataNode, "status": DataNode,
             "trim_name": DataNode, "errmsg": DataNode})
        GET_TEAM = IAttr(
            'GET_TEAM', True, False, False,
            ArgDesc(0, 0, DataNode), {"level": DataNode})
        HUGE = IAttr(
            'HUGE', True, False, True,
            ArgDesc(1, 1, (Reference, Literal)), {})
        HYPOT = IAttr(
            'HYPOT', True, True, False,
            ArgDesc(2, 2, (DataNode)), {})
        IACHAR = IAttr(
            'IACHAR', True, True, False,
            ArgDesc(1, 1, (DataNode)), {"kind": DataNode})
        IALL = IAttr(
            'IALL', True, False, False,
            ArgDesc(1, 1, (DataNode)), {"dim": DataNode, "kind": DataNode})
        IAND = IAttr(
            'IAND', True, True, False,
            ArgDesc(2, 2, (DataNode)), {})
        IANY = IAttr(
            'IANY', True, False, False,
            ArgDesc(1, 1, (DataNode)), {"dim": DataNode, "kind": DataNode})
        IBCLR = IAttr(
            'IBCLR', True, True, False,
            ArgDesc(2, 2, (DataNode)), {})
        IBITS = IAttr(
            'IBITS', True, True, False,
            ArgDesc(3, 3, (DataNode)), {})
        IBSET = IAttr(
            'IBSET', True, True, False,
            ArgDesc(2, 2, (DataNode)), {})
        ICHAR = IAttr(
            'ICHAR', True, True, False,
            ArgDesc(1, 1, (DataNode)), {"kind": DataNode})
        IEOR = IAttr(
            'IEOR', True, True, False,
            ArgDesc(2, 2, (DataNode)), {})
        IMAGE_INDEX = IAttr(
            'IMAGE_INDEX', True, False, True,
            ArgDesc(2, 3, (DataNode)), {})
        IMAGE_STATUS = IAttr(
            'IMAGE_STATUS', True, False, False,
            ArgDesc(1, 1, (DataNode)), {"team": DataNode})
        INDEX = IAttr(
            'INDEX', True, True, False,
            ArgDesc(2, 2, (DataNode)), {"back": DataNode, "kind": DataNode})
        INT = IAttr(
            'INT', True, True, False,
            ArgDesc(1, 1, (DataNode)), {"kind": DataNode})
        IOR = IAttr(
            'IOR', True, True, False,
            ArgDesc(2, 2, (DataNode)), {})
        IPARITY = IAttr(
            'IPARITY', True, False, False,
            ArgDesc(1, 2, (DataNode)), {"mask": DataNode})
        ISHFT = IAttr(
            'ISHFT', True, True, False,
            ArgDesc(2, 2, (DataNode)), {})
        ISHFTC = IAttr(
            'ISHFTC', True, True, False,
            ArgDesc(2, 2, (DataNode)), {"size": DataNode})
        IS_CONTIGUOUS = IAttr(
            'IS_CONTIGUOUS', True, False, True,
            ArgDesc(1, 1, (DataNode)), {})
        IS_IOSTAT_END = IAttr(
            'IS_IOSTAT_END', True, True, False,
            ArgDesc(1, 1, (DataNode)), {})
        IS_IOSTAT_EOR = IAttr(
            'IS_IOSTAT_EOR', True, True, False,
            ArgDesc(1, 1, (DataNode)), {})
        KIND = IAttr(
            'KIND', True, False, True,
            ArgDesc(1, 1, (DataNode)), {})
        LBOUND = IAttr(
            'LBOUND', True, False, True,
            ArgDesc(1, 1, (DataNode)), {"dim": DataNode, "kind": DataNode})
        LCOBOUND = IAttr(
            'LCOBOUND', True, False, True,
            ArgDesc(1, 1, (DataNode)), {"dim": DataNode, "kind": DataNode})
        LEADZ = IAttr(
            'LEADZ', True, True, False,
            ArgDesc(1, 1, (DataNode)), {})
        LEN = IAttr(
            'LEN', True, False, True,
            ArgDesc(1, 1, (DataNode)), {"kind": DataNode})
        LEN_TRIM = IAttr(
            'LEN_TRIM', True, True, False,
            ArgDesc(1, 1, (DataNode)), {"kind": DataNode})
        LGE = IAttr(
            'LGE', True, True, False,
            ArgDesc(2, 2, (DataNode)), {})
        LGT = IAttr(
            'LGT', True, True, False,
            ArgDesc(2, 2, (DataNode)), {})
        LLE = IAttr(
            'LLE', True, True, False,
            ArgDesc(2, 2, (DataNode)), {})
        LLT = IAttr(
            'LLT', True, True, False,
            ArgDesc(2, 2, (DataNode)), {})
        LOG = IAttr(
            'LOG', True, True, False,
            ArgDesc(1, 1, (DataNode)), {})
        LOG_GAMMA = IAttr(
            'LOG_GAMMA', True, True, False,
            ArgDesc(1, 1, (DataNode)), {})
        LOG10 = IAttr(
            'LOG10', True, True, False,
            ArgDesc(1, 1, (DataNode)), {})
        LOGICAL = IAttr(
            'LOGICAL', True, True, False,
            ArgDesc(1, 1, (DataNode)), {"kind": DataNode})
        MASKL = IAttr(
            'MASKL', True, True, False,
            ArgDesc(1, 1, (DataNode)), {"kind": DataNode})
        MASKR = IAttr(
            'MASKR', True, True, False,
            ArgDesc(1, 1, (DataNode)), {"kind": DataNode})
        MATMUL = IAttr(
            'MATMUL', True, False, False,
            ArgDesc(2, 2, DataNode), {})
        MAX = IAttr(
            'MAX', True, True, False,
            ArgDesc(2, None, DataNode), {})
        MAXEXPONENT = IAttr(
            'MAXEXPONENT', True, False, True,
            ArgDesc(1, 1, DataNode), {})
        MAXLOC = IAttr(
            'MAXLOC', True, False, False,
            ArgDesc(1, 2, DataNode),
            {"dim": DataNode, "mask": DataNode, "kind": DataNode,
             "back": DataNode})
        MAXVAL = IAttr(
            'MAXVAL', True, False, False,
            ArgDesc(1, 1, DataNode),
            {"dim": DataNode, "mask": DataNode})
        MERGE = IAttr(
            'MERGE', True, True, False,
            ArgDesc(3, 3, DataNode), {})
        MERGE_BITS = IAttr(
            'MERGE_BITS', True, True, False,
            ArgDesc(3, 3, DataNode), {})
        MIN = IAttr(
            'MIN', True, True, False,
            ArgDesc(2, None, DataNode), {})
        MINEXPONENT = IAttr(
            'MINEXPONENT', True, False, True,
            ArgDesc(1, 1, DataNode), {})
        MINLOC = IAttr(
            'MINLOC', True, False, False,
            ArgDesc(1, 2, DataNode),
            {"dim": DataNode, "mask": DataNode, "kind": DataNode,
             "back": DataNode})
        MINVAL = IAttr(
            'MINVAL', True, False, False,
            ArgDesc(1, 1, DataNode),
            {"dim": DataNode, "mask": DataNode})
        MOD = IAttr(
            'MOD', True, True, False,
            ArgDesc(2, 2, DataNode), {})
        MODULO = IAttr(
            'MODULO', True, True, False,
            ArgDesc(2, 2, DataNode), {})
        MOVE_ALLOC = IAttr(
            'MOVE_ALLOC', False, False, False,
            ArgDesc(2, 2, DataNode), {"stat": DataNode, "errmsg": DataNode})
        MVBITS = IAttr(
            'MVBITS', True, True, False,
            ArgDesc(5, 5, DataNode), {})
        NEAREST = IAttr(
            'NEAREST', True, True, False,
            ArgDesc(2, 2, DataNode), {})
        NEW_LINE = IAttr(
            'NEW_LINE', True, True, False,
            ArgDesc(1, 1, DataNode), {})
        NINT = IAttr(
            'NINT', True, True, False,
            ArgDesc(1, 1, DataNode), {"kind": DataNode})
        NORM2 = IAttr(
            'NORM2', True, False, False,
            ArgDesc(1, 2, DataNode), {})
        NOT = IAttr(
            'NOT', True, True, False,
            ArgDesc(1, 1, DataNode), {})
        NULL = IAttr(
            'NULL', True, False, False,
            ArgDesc(0, 0, DataNode), {"mold": DataNode})
        NUM_IMAGES = IAttr(
            'NUM_IMAGES', True, False, False,
            ArgDesc(0, 1, DataNode), {})
        OUT_OF_RANGE = IAttr(
            'OUT_OF_RANGE', True, True, False,
            ArgDesc(2, 2, DataNode), {"round": DataNode})
        PACK = IAttr(
            'PACK', True, False, False,
            ArgDesc(2, 2, DataNode), {"vector": DataNode})
        PARITY = IAttr(
            'PARITY', True, False, False,
            ArgDesc(1, 2, DataNode), {})
        POPCNT = IAttr(
            'POPCNT', True, True, False,
            ArgDesc(1, 1, DataNode), {})
        POPPAR = IAttr(
            'POPPAR', True, True, False,
            ArgDesc(1, 1, DataNode), {})
        PRECISION = IAttr(
            'PRECISION', True, False, True,
            ArgDesc(1, 1, DataNode), {})
        PRESENT = IAttr(
            'PRESENT', True, False, True,
            ArgDesc(1, 1, DataNode), {})
        PRODUCT = IAttr(
            'PRODUCT', True, False, False,
            ArgDesc(1, 1, DataNode), {"dim": DataNode, "mask": DataNode})
        RADIX = IAttr(
            'RADIX', True, False, True,
            ArgDesc(1, 1, DataNode), {})
        RANDOM_INIT = IAttr(
            'RANDOM_INIT', False, False, False,
            ArgDesc(2, 2, DataNode), {})
        RANDOM_NUMBER = IAttr(
            'RANDOM_NUMBER', False, False, False,
            ArgDesc(1, 1, Reference), {})
        RANDOM_SEED = IAttr(
            'RANDOM_SEED', False, False, False,
            ArgDesc(0, 0, Reference),
            {"size": DataNode, "put": DataNode, "Get": DataNode})
        RANGE = IAttr(
            'RANGE', True, False, True,
            ArgDesc(1, 1, Reference), {})
        RANK = IAttr(
            'RANK', True, False, True,
            ArgDesc(1, 1, Reference), {})
        REAL = IAttr(
            'REAL', True, True, False,
            ArgDesc(1, 1, Reference), {"kind": DataNode})
        REDUCE = IAttr(
            'REDUCE', True, False, False,
            ArgDesc(2, 3, Reference),
            {"mask": DataNode, "identity": DataNode, "ordered": DataNode})
        REPEAT = IAttr(
            'REPEAT', True, False, False,
            ArgDesc(2, 2, Reference), {})
        RESHAPE = IAttr(
            'RESHAPE', True, False, False,
            ArgDesc(2, 2, Reference), {"pad": DataNode, "order": DataNode})
        RRSPACING = IAttr(
            'RRSPACING', True, True, False,
            ArgDesc(1, 1, Reference), {})
        SAME_TYPE_AS = IAttr(
            'SAME_TYPE_AS', True, False, True,
            ArgDesc(2, 2, Reference), {})
        SCALE = IAttr(
            'SCALE', True, True, False,
            ArgDesc(2, 2, Reference), {})
        SCAN = IAttr(
            'SCAN', True, True, False,
            ArgDesc(2, 2, Reference), {"back": DataNode, "kind": DataNode})
        SELECTED_CHAR_KIND = IAttr(
            'SELECTED_CHAR_KIND', True, False, False,
            ArgDesc(1, 1, Reference), {})
        SELECTED_INT_KIND = IAttr(
            'SELECTED_INT_KIND', True, False, False,
            ArgDesc(1, 1, Reference), {})
        SELECTED_REAL_KIND = IAttr(
            'SELECTED_REAL_KIND', True, False, False,
            ArgDesc(0, 0, Reference),
            {"P": DataNode, "R": DataNode, "radix": DataNode})
        SET_EXPONENT = IAttr(
            'SET_EXPONENT', True, True, False,
            ArgDesc(2, 2, Reference), {})
        SHAPE = IAttr(
            'SHAPE', True, False, True,
            ArgDesc(1, 1, Reference), {"kind": DataNode})
        SHIFTA = IAttr(
            'SHIFTA', True, True, False,
            ArgDesc(2, 2, Reference), {})
        SHIFTL = IAttr(
            'SHIFTL', True, True, False,
            ArgDesc(2, 2, Reference), {})
        SHIFTR = IAttr(
            'SHIFTR', True, True, False,
            ArgDesc(2, 2, Reference), {})
        SIGN = IAttr(
            'SIGN', True, True, False,
            ArgDesc(2, 2, DataNode), {})
        SIN = IAttr(
            'SIN', True, True, False,
            ArgDesc(1, 1, DataNode), {})
        SINH = IAttr(
            'SINH', True, True, False,
            ArgDesc(1, 1, DataNode), {})
        SIZE = IAttr(
            'SIZE', True, False, True,
            ArgDesc(1, 1, DataNode), {"dim": DataNode, "kind": DataNode})
        SPACING = IAttr(
            'SPACING', True, True, False,
            ArgDesc(1, 1, DataNode), {})
        SPREAD = IAttr(
            'SPREAD', True, False, False,
            ArgDesc(3, 3, DataNode), {})
        SQRT = IAttr(
            'SQRT', True, True, False,
            ArgDesc(1, 1, DataNode), {})
        STOPPED_IMAGES = IAttr(
            'STOPPED_IMAGES', False, False, False,
            ArgDesc(0, 0, DataNode), {"team": DataNode, "kind": DataNode})
        STORAGE_SIZE = IAttr(
            'STORAGE_SIZE', True, False, True,
            ArgDesc(1, 1, DataNode), {"kind": DataNode})
        SUM = IAttr(
            'SUM', True, False, False,
            ArgDesc(1, 1, DataNode), {"dim": DataNode, "mask": DataNode})
        SYSTEM_CLOCK = IAttr(
            'SYSTEM_CLOCK', False, False, False,
            ArgDesc(0, 0, DataNode),
            {"count": DataNode, "count_rate": DataNode, "count_max": DataNode})
        TAN = IAttr(
            'TAN', True, True, False,
            ArgDesc(1, 1, DataNode), {})
        TANH = IAttr(
            'TANH', True, True, False,
            ArgDesc(1, 1, DataNode), {})
        TEAM_IMAGE = IAttr(
            'TEAM_IMAGE', True, False, False,
            ArgDesc(0, 0, DataNode), {"team": DataNode})
        THIS_IMAGE = IAttr(
            'THIS_IMAGE', True, False, False,
            ArgDesc(0, 0, DataNode),
            {"coarray": DataNode, "team": DataNode, "dim": DataNode})
        TINY = IAttr(
            'TINY', True, False, True,
            ArgDesc(1, 1, (Reference, Literal)), {})
        TRAILZ = IAttr(
            'TRAILZ', True, True, False,
            ArgDesc(1, 1, DataNode), {})
        TRANSFER = IAttr(
            'TRANSFER', True, False, False,
            ArgDesc(2, 2, DataNode), {"size": DataNode})
        TRANSPOSE = IAttr(
            'TRANSPOSE', True, False, False,
            ArgDesc(1, 1, DataNode), {})
        TRIM = IAttr(
            'TRIM', True, False, False,
            ArgDesc(1, 1, DataNode), {})
        UBOUND = IAttr(
            'UBOUND', True, False, True,
            ArgDesc(1, 1, DataNode), {"dim": DataNode, "kind": DataNode})
        UCOBOUND = IAttr(
            'UCOBOUND', True, False, True,
            ArgDesc(1, 1, DataNode), {"dim": DataNode, "kind": DataNode})
        UNPACK = IAttr(
            'UNPACK', True, False, False,
            ArgDesc(3, 3, DataNode), {})
        VERIFY = IAttr(
            'VERIFY', True, True, False,
            ArgDesc(2, 2, DataNode), {"back": DataNode, "kind": DataNode})

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

    @property
    def intrinsic(self):
        ''' Return the type of intrinsic.

        :returns: enumerated type capturing the type of intrinsic.
        :rtype: :py:class:`psyclone.psyir.nodes.IntrinsicCall.Intrinsic`

        '''
        return self.routine.symbol.intrinsic

    # This is not part of the intrinsic enum, because its ValueError could
    # change for different devices, and in the future we may want to pass
    # a device/arch/compiler parameter or look at the configuration file.
    # Currently it is inspired from: https://docs.nvidia.com/hpc-sdk/
    # compilers/hpc-compilers-user-guide/#acc-fort-intrin-sum
    # But that list is incomplete (e.g. SUM is supported and not listed)
    def is_available_on_device(self):
        '''
        :returns: whether this intrinsic is available on an accelerated device.
        :rtype: bool

        '''
        return self.intrinsic in (
            IntrinsicCall.Intrinsic.ABS,  IntrinsicCall.Intrinsic.ACOS,
            IntrinsicCall.Intrinsic.AINT, IntrinsicCall.Intrinsic.ANINT,
            IntrinsicCall.Intrinsic.ASIN, IntrinsicCall.Intrinsic.ATAN,
            IntrinsicCall.Intrinsic.ATAN2, IntrinsicCall.Intrinsic.COS,
            IntrinsicCall.Intrinsic.COSH, IntrinsicCall.Intrinsic.DBLE,
            IntrinsicCall.Intrinsic.DPROD, IntrinsicCall.Intrinsic.EXP,
            IntrinsicCall.Intrinsic.IAND, IntrinsicCall.Intrinsic.IEOR,
            IntrinsicCall.Intrinsic.INT, IntrinsicCall.Intrinsic.IOR,
            IntrinsicCall.Intrinsic.LOG,
            IntrinsicCall.Intrinsic.MAX, IntrinsicCall.Intrinsic.MIN,
            IntrinsicCall.Intrinsic.MOD, IntrinsicCall.Intrinsic.NINT,
            IntrinsicCall.Intrinsic.NOT,
            IntrinsicCall.Intrinsic.SIGN, IntrinsicCall.Intrinsic.SIN,
            IntrinsicCall.Intrinsic.SINH, IntrinsicCall.Intrinsic.SQRT,
            IntrinsicCall.Intrinsic.TAN, IntrinsicCall.Intrinsic.TANH,
            IntrinsicCall.Intrinsic.UBOUND, IntrinsicCall.Intrinsic.MERGE,
            # The ones below can be offloaded but provide numerical differences
            # even with the -gpu=uniform_math flag, ideally it should be
            # configurable if these are allowed or not.
            # IntrinsicCall.Intrinsic.LOG10, IntrinsicCall.Intrinsic.REAL,
            # The one below are not documented on nvidia compiler
            IntrinsicCall.Intrinsic.PRODUCT, IntrinsicCall.Intrinsic.SIZE,
            IntrinsicCall.Intrinsic.SUM, IntrinsicCall.Intrinsic.LBOUND,
            IntrinsicCall.Intrinsic.MAXVAL, IntrinsicCall.Intrinsic.MINVAL,
            IntrinsicCall.Intrinsic.TINY, IntrinsicCall.Intrinsic.HUGE)

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


# TODO #658 this can be removed once we have support for determining the
# type of a PSyIR expression.
# Intrinsics that perform operations on an array.
ARRAY_INTRINSICS = [
    IntrinsicCall.Intrinsic.SUM, IntrinsicCall.Intrinsic.MINVAL,
    IntrinsicCall.Intrinsic.MAXVAL, IntrinsicCall.Intrinsic.PACK,
    IntrinsicCall.Intrinsic.COUNT]
