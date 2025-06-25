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
from psyclone.psyir.symbols import IntrinsicSymbol, UnresolvedType

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
             "errmsg": Reference}, UnresolvedType())
        DEALLOCATE = IAttr(
            'DEALLOCATE', False, False, False,
            ArgDesc(1, None, Reference), {"stat": Reference}, UnresolvedType())
        NULLIFY = IAttr(
            'NULLIFY', False, False, False,
            ArgDesc(1, None, Reference), {}, UnresolvedType())

        # Fortran Intrinsics (from Fortran 2018 standard table 16.1)
        ABS = IAttr(
            'ABS', True, True, False,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        ACHAR = IAttr(
            'ACHAR', True, True, False,
            ArgDesc(1, 1, DataNode), {"kind": DataNode}, UnresolvedType())
        ACOS = IAttr(
            'ACOS', True, True, False,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        ACOSH = IAttr(
            'ACOSH', True, True, False,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        ADJUSTL = IAttr(
            'ADJUSTL', True, True, False,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        ADJUSTR = IAttr(
            'ADJUSTR', True, True, False,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        AIMAG = IAttr(
            'AIMAG', True, True, False,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        AINT = IAttr(
            'AINT', True, True, False,
            ArgDesc(1, 1, DataNode), {"kind": DataNode}, UnresolvedType())
        ALL = IAttr(
            'ALL', True, False, False,
            ArgDesc(1, 1, DataNode), {"dim": DataNode}, UnresolvedType())  # ?
        ALLOCATED = IAttr(
            'ALLOCATED', True, False, True,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        ANINT = IAttr(
            'ANINT', True, True, False,
            ArgDesc(1, 1, DataNode), {"kind": DataNode}, UnresolvedType())
        ANY = IAttr(
            'ANY', True, False, False,
            ArgDesc(1, 1, DataNode), {"dim": DataNode}, UnresolvedType())  # ?
        ASIN = IAttr(
            'ASIN', True, True, False,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        ASINH = IAttr(
            'ASINH', True, True, False,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        ASSOCIATED = IAttr(
            'ASSOCIATED', False, False, True,
            ArgDesc(1, 1, DataNode), {"target": DataNode}, UnresolvedType())
        ATAN = IAttr(
            'ATAN', True, True, False,
            ArgDesc(1, 2, DataNode), {}, UnresolvedType())
        ATAN2 = IAttr(
            'ATAN2', True, True, False,
            ArgDesc(2, 2, DataNode), {}, UnresolvedType())
        ATANH = IAttr(
            'ATANH', True, True, False,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        ATOMIC_ADD = IAttr(
            'ATOMIC_ADD', True, True, False,
            ArgDesc(2, 2, DataNode), {"stat": DataNode}, UnresolvedType())
        ATOMIC_AND = IAttr(
            'ATOMIC_AND', True, True, False,
            ArgDesc(2, 2, DataNode), {"stat": DataNode}, UnresolvedType())
        ATOMIC_CAS = IAttr(
            'ATOMIC_CAS', True, True, False,
            ArgDesc(2, 2, DataNode), {"stat": DataNode}, UnresolvedType())
        ATOMIC_DEFINE = IAttr(
            'ATOMIC_DEFINE', True, True, False,
            ArgDesc(2, 2, DataNode), {"stat": DataNode}, UnresolvedType())
        ATOMIC_FETCH_ADD = IAttr(
            'ATOMIC_FETCH_ADD', True, True, False,
            ArgDesc(3, 3, DataNode), {"stat": DataNode}, UnresolvedType())
        ATOMIC_FETCH_AND = IAttr(
            'ATOMIC_FETCH_AND', True, True, False,
            ArgDesc(3, 3, DataNode), {"stat": DataNode}, UnresolvedType())
        ATOMIC_FETCH_OR = IAttr(
            'ATOMIC_FETCH_OR', True, True, False,
            ArgDesc(3, 3, DataNode), {"stat": DataNode}, UnresolvedType())
        ATOMIC_FETCH_XOR = IAttr(
            'ATOMIC_FETCH_XOR', True, True, False,
            ArgDesc(3, 3, DataNode), {"stat": DataNode}, UnresolvedType())
        ATOMIC_OR = IAttr(
            'ATOMIC_OR', True, True, False,
            ArgDesc(2, 2, DataNode), {"stat": DataNode}, UnresolvedType())
        ATOMIC_REF = IAttr(
            'ATOMIC_REF', True, True, False,
            ArgDesc(2, 2, DataNode), {"stat": DataNode}, UnresolvedType())
        ATOMIC_XOR = IAttr(
            'ATOMIC_XOR', True, True, False,
            ArgDesc(2, 2, DataNode), {"stat": DataNode}, UnresolvedType())
        BESSEL_J0 = IAttr(
            'BESSEL_J0', True, True, False,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        BESSEL_J1 = IAttr(
            'BESSEL_J1', True, True, False,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        BESSEL_JN = IAttr(
            'BESSEL_JN', True, None, False,
            ArgDesc(2, 3, DataNode), {}, UnresolvedType())
        BESSEL_Y0 = IAttr(
            'BESSEL_Y0', True, True, False,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        BESSEL_Y1 = IAttr(
            'BESSEL_Y1', True, True, False,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        BESSEL_YN = IAttr(
            'BESSEL_YN', True, None, False,
            ArgDesc(2, 3, DataNode), {}, UnresolvedType())
        BGE = IAttr(
            'BGE', True, True, False,
            ArgDesc(2, 2, DataNode), {}, UnresolvedType())
        BGT = IAttr(
            'BGT', True, True, False,
            ArgDesc(2, 2, DataNode), {}, UnresolvedType())
        BIT_SIZE = IAttr(
            'BIT_SIZE', True, False, True,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        BLE = IAttr(
            'BLE', True, True, False,
            ArgDesc(2, 2, DataNode), {}, UnresolvedType())
        BLT = IAttr(
            'BLT', True, True, False,
            ArgDesc(2, 2, DataNode), {}, UnresolvedType())
        BTEST = IAttr(
            'BTEST', True, True, False,
            ArgDesc(2, 2, DataNode), {}, UnresolvedType())
        CEILING = IAttr(
            'CEILING', True, True, False,
            ArgDesc(1, 1, DataNode), {"kind": DataNode}, UnresolvedType())
        CHAR = IAttr(
            'CHAR', True, True, False,
            ArgDesc(1, 1, DataNode), {"kind": DataNode}, UnresolvedType())
        CMPLX = IAttr(
            'CMPLX', True, True, False,
            ArgDesc(1, 1, DataNode), {"Y": DataNode, "kind": DataNode},
            UnresolvedType())
        CO_BROADCAST = IAttr(
            'CO_BROADCAST', True, False, False,
            ArgDesc(1, 2, DataNode), {"stat": DataNode, "errmsg": DataNode},
            UnresolvedType())
        CO_MAX = IAttr(
            'CO_MAX', True, False, False,
            ArgDesc(1, 1, DataNode),
            {"result_image": DataNode, "stat": DataNode, "errmsg": DataNode},
            UnresolvedType())
        CO_MIN = IAttr(
            'CO_MIN', True, False, False,
            ArgDesc(1, 1, DataNode),
            {"result_image": DataNode, "stat": DataNode, "errmsg": DataNode},
            UnresolvedType())
        CO_REDUCE = IAttr(
            'CO_REDUCE', True, False, False,
            ArgDesc(1, 2, DataNode),
            {"result_image": DataNode, "stat": DataNode, "errmsg": DataNode},
            UnresolvedType())
        CO_SUM = IAttr(
            'CO_SUM', True, False, False,
            ArgDesc(1, 1, DataNode),
            {"result_image": DataNode, "stat": DataNode, "errmsg": DataNode},
            UnresolvedType())
        COMMAND_ARGUMENT_COUNT = IAttr(
            'COMMAND_ARGUMENT_COUNT', True, False, False,
            ArgDesc(0, 0, None), {}, UnresolvedType())
        CONJG = IAttr(
            'CONJG', True, True, False,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        COS = IAttr(
            'COS', True, True, False,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        COSH = IAttr(
            'COSH', True, True, False,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        COSHAPE = IAttr(
            'COSHAPE', True, False, True,
            ArgDesc(1, 1, DataNode), {"kind": DataNode}, UnresolvedType())
        COUNT = IAttr(
            'COUNT', True, False, False,
            ArgDesc(1, 1, DataNode), {"dim": DataNode, "kind": DataNode},
            UnresolvedType())
        CPU_TIME = IAttr(
            'CPU_TIME', False, False, False,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        CSHIFT = IAttr(
            'CSHIFT', True, False, False,
            ArgDesc(2, 2, DataNode), {"dim": DataNode}, UnresolvedType())
        DATE_AND_TIME = IAttr(
            'DATE_AND_TIME', False, False, False,
            ArgDesc(0, 0, DataNode),
            {"date": DataNode, "time": DataNode,
             "zone": DataNode, "values": DataNode}, UnresolvedType())
        DBLE = IAttr(
            'DBLE', True, True, False,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        DIGITS = IAttr(
            'DIGITS', True, False, True,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        DIM = IAttr(
            'DIM', True, True, False,
            ArgDesc(2, 2, DataNode), {}, UnresolvedType())
        DOT_PRODUCT = IAttr(
            'DOT_PRODUCT', True, False, False,
            ArgDesc(2, 2, DataNode), {}, UnresolvedType())
        DPROD = IAttr(
            'DPROD', True, True, False,
            ArgDesc(2, 2, DataNode), {}, UnresolvedType())
        DSHIFTL = IAttr(
            'DSHIFTL', True, True, False,
            ArgDesc(3, 3, DataNode), {}, UnresolvedType())
        DSHIFTR = IAttr(
            'DSHIFTR', True, True, False,
            ArgDesc(3, 3, DataNode), {}, UnresolvedType())
        EOSHIFT = IAttr(
            'EOSHIFT', True, False, False,
            ArgDesc(2, 2, DataNode), {"boundary": DataNode, "dim": DataNode},
            UnresolvedType())
        EPSILON = IAttr(
            'EPSILON', True, False, True,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        ERF = IAttr(
            'ERF', True, True, False,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        ERFC = IAttr(
            'ERFC', True, True, False,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        ERFC_SCALED = IAttr(
            'ERFC_SCALED', True, True, False,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        EVENT_QUERY = IAttr(
            'EVENT_QUERY', False, False, False,
            ArgDesc(2, 2, DataNode), {"stat": DataNode}, UnresolvedType())
        EXECUTE_COMMAND_LINE = IAttr(
            'EXECUTE_COMMAND_LINE', False, False, False,
            ArgDesc(2, 2, DataNode),
            {"wait": DataNode, "exitstat": DataNode,
             "cmdstat": DataNode, "cmdmsg": DataNode}, UnresolvedType())
        EXP = IAttr(
            'EXP', True, True, False,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        EXPONENT = IAttr(
            'EXPONENT', True, True, False,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        EXTENDS_TYPE_OF = IAttr(
            'EXTENDS_TYPE_OF', True, False, True,
            ArgDesc(2, 2, DataNode), {}, UnresolvedType())
        FAILED_IMAGES = IAttr(
            'FAILED_IMAGES', False, False, False,
            ArgDesc(0, 0, DataNode), {"team": DataNode, "kind": DataNode},
            UnresolvedType())
        FINDLOC = IAttr(
            'FINDLOC', True, False, False,
            ArgDesc(2, 3, DataNode),
            {"mask": DataNode, "kind": DataNode, "back": DataNode},
            UnresolvedType())
        FLOAT = IAttr(
            'FLOAT', True, True, False,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        FLOOR = IAttr(
            'FLOOR', True, True, False,
            ArgDesc(1, 1, DataNode), {"kind": DataNode}, UnresolvedType())
        FRACTION = IAttr(
            'FRACTION', True, True, False,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        GAMMA = IAttr(
            'GAMMA', True, True, False,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        GET_COMMAND = IAttr(
            'GET_COMMAND', False, False, False,
            ArgDesc(0, 0, DataNode),
            {"command": DataNode, "length": DataNode,
             "status": DataNode, "errmsg": DataNode}, UnresolvedType())
        GET_COMMAND_ARGUMENT = IAttr(
            'GET_COMMAND_ARGUMENT', False, False, False,
            ArgDesc(1, 1, DataNode),
            {"value": DataNode, "length": DataNode,
             "status": DataNode, "errmsg": DataNode}, UnresolvedType())
        GET_ENVIRONMENT_VARIABLE = IAttr(
            'GET_ENVIRONMENT_VARIABLE', False, False, False,
            ArgDesc(1, 1, DataNode),
            {"value": DataNode, "length": DataNode, "status": DataNode,
             "trim_name": DataNode, "errmsg": DataNode}, UnresolvedType())
        GET_TEAM = IAttr(
            'GET_TEAM', True, False, False,
            ArgDesc(0, 0, DataNode), {"level": DataNode}, UnresolvedType())
        HUGE = IAttr(
            'HUGE', True, False, True,
            ArgDesc(1, 1, (Reference, Literal)), {}, UnresolvedType())
        HYPOT = IAttr(
            'HYPOT', True, True, False,
            ArgDesc(2, 2, (DataNode)), {}, UnresolvedType())
        IACHAR = IAttr(
            'IACHAR', True, True, False,
            ArgDesc(1, 1, (DataNode)), {"kind": DataNode}, UnresolvedType())
        IALL = IAttr(
            'IALL', True, False, False,
            ArgDesc(1, 1, (DataNode)), {"dim": DataNode, "kind": DataNode},
            UnresolvedType())
        IAND = IAttr(
            'IAND', True, True, False,
            ArgDesc(2, 2, (DataNode)), {}, UnresolvedType())
        IANY = IAttr(
            'IANY', True, False, False,
            ArgDesc(1, 1, (DataNode)), {"dim": DataNode, "kind": DataNode},
            UnresolvedType())
        IBCLR = IAttr(
            'IBCLR', True, True, False,
            ArgDesc(2, 2, (DataNode)), {}, UnresolvedType())
        IBITS = IAttr(
            'IBITS', True, True, False,
            ArgDesc(3, 3, (DataNode)), {}, UnresolvedType())
        IBSET = IAttr(
            'IBSET', True, True, False,
            ArgDesc(2, 2, (DataNode)), {}, UnresolvedType())
        ICHAR = IAttr(
            'ICHAR', True, True, False,
            ArgDesc(1, 1, (DataNode)), {"kind": DataNode}, UnresolvedType())
        IEOR = IAttr(
            'IEOR', True, True, False,
            ArgDesc(2, 2, (DataNode)), {}, UnresolvedType())
        IMAGE_INDEX = IAttr(
            'IMAGE_INDEX', True, False, True,
            ArgDesc(2, 3, (DataNode)), {}, UnresolvedType())
        IMAGE_STATUS = IAttr(
            'IMAGE_STATUS', True, False, False,
            ArgDesc(1, 1, (DataNode)), {"team": DataNode}, UnresolvedType())
        INDEX = IAttr(
            'INDEX', True, True, False,
            ArgDesc(2, 2, (DataNode)), {"back": DataNode, "kind": DataNode},
            UnresolvedType())
        INT = IAttr(
            'INT', True, True, False,
            ArgDesc(1, 1, (DataNode)), {"kind": DataNode}, UnresolvedType())
        IOR = IAttr(
            'IOR', True, True, False,
            ArgDesc(2, 2, (DataNode)), {}, UnresolvedType())
        IPARITY = IAttr(
            'IPARITY', True, False, False,
            ArgDesc(1, 2, (DataNode)), {"mask": DataNode}, UnresolvedType())
        ISHFT = IAttr(
            'ISHFT', True, True, False,
            ArgDesc(2, 2, (DataNode)), {}, UnresolvedType())
        ISHFTC = IAttr(
            'ISHFTC', True, True, False,
            ArgDesc(2, 2, (DataNode)), {"size": DataNode}, UnresolvedType())
        IS_CONTIGUOUS = IAttr(
            'IS_CONTIGUOUS', True, False, True,
            ArgDesc(1, 1, (DataNode)), {}, UnresolvedType())
        IS_IOSTAT_END = IAttr(
            'IS_IOSTAT_END', True, True, False,
            ArgDesc(1, 1, (DataNode)), {}, UnresolvedType())
        IS_IOSTAT_EOR = IAttr(
            'IS_IOSTAT_EOR', True, True, False,
            ArgDesc(1, 1, (DataNode)), {}, UnresolvedType())
        KIND = IAttr(
            'KIND', True, False, True,
            ArgDesc(1, 1, (DataNode)), {}, UnresolvedType())
        LBOUND = IAttr(
            'LBOUND', True, False, True,
            ArgDesc(1, 1, (DataNode)), {"dim": DataNode, "kind": DataNode},
            UnresolvedType())
        LCOBOUND = IAttr(
            'LCOBOUND', True, False, True,
            ArgDesc(1, 1, (DataNode)), {"dim": DataNode, "kind": DataNode},
            UnresolvedType())
        LEADZ = IAttr(
            'LEADZ', True, True, False,
            ArgDesc(1, 1, (DataNode)), {}, UnresolvedType())
        LEN = IAttr(
            'LEN', True, False, True,
            ArgDesc(1, 1, (DataNode)), {"kind": DataNode}, UnresolvedType())
        LEN_TRIM = IAttr(
            'LEN_TRIM', True, True, False,
            ArgDesc(1, 1, (DataNode)), {"kind": DataNode}, UnresolvedType())
        LGE = IAttr(
            'LGE', True, True, False,
            ArgDesc(2, 2, (DataNode)), {}, UnresolvedType())
        LGT = IAttr(
            'LGT', True, True, False,
            ArgDesc(2, 2, (DataNode)), {}, UnresolvedType())
        LLE = IAttr(
            'LLE', True, True, False,
            ArgDesc(2, 2, (DataNode)), {}, UnresolvedType())
        LLT = IAttr(
            'LLT', True, True, False,
            ArgDesc(2, 2, (DataNode)), {}, UnresolvedType())
        LOG = IAttr(
            'LOG', True, True, False,
            ArgDesc(1, 1, (DataNode)), {}, UnresolvedType())
        LOG_GAMMA = IAttr(
            'LOG_GAMMA', True, True, False,
            ArgDesc(1, 1, (DataNode)), {}, UnresolvedType())
        LOG10 = IAttr(
            'LOG10', True, True, False,
            ArgDesc(1, 1, (DataNode)), {}, UnresolvedType())
        LOGICAL = IAttr(
            'LOGICAL', True, True, False,
            ArgDesc(1, 1, (DataNode)), {"kind": DataNode}, UnresolvedType())
        MASKL = IAttr(
            'MASKL', True, True, False,
            ArgDesc(1, 1, (DataNode)), {"kind": DataNode}, UnresolvedType())
        MASKR = IAttr(
            'MASKR', True, True, False,
            ArgDesc(1, 1, (DataNode)), {"kind": DataNode}, UnresolvedType())
        MATMUL = IAttr(
            'MATMUL', True, False, False,
            ArgDesc(2, 2, DataNode), {}, UnresolvedType())
        MAX = IAttr(
            'MAX', True, True, False,
            ArgDesc(2, None, DataNode), {}, UnresolvedType())
        MAXEXPONENT = IAttr(
            'MAXEXPONENT', True, False, True,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        MAXLOC = IAttr(
            'MAXLOC', True, False, False,
            ArgDesc(1, 2, DataNode),
            {"dim": DataNode, "mask": DataNode, "kind": DataNode,
             "back": DataNode}, UnresolvedType())
        MAXVAL = IAttr(
            'MAXVAL', True, False, False,
            ArgDesc(1, 1, DataNode),
            {"dim": DataNode, "mask": DataNode}, UnresolvedType())
        MERGE = IAttr(
            'MERGE', True, True, False,
            ArgDesc(3, 3, DataNode), {}, UnresolvedType())
        MERGE_BITS = IAttr(
            'MERGE_BITS', True, True, False,
            ArgDesc(3, 3, DataNode), {}, UnresolvedType())
        MIN = IAttr(
            'MIN', True, True, False,
            ArgDesc(2, None, DataNode), {}, UnresolvedType())
        MINEXPONENT = IAttr(
            'MINEXPONENT', True, False, True,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        MINLOC = IAttr(
            'MINLOC', True, False, False,
            ArgDesc(1, 2, DataNode),
            {"dim": DataNode, "mask": DataNode, "kind": DataNode,
             "back": DataNode}, UnresolvedType())
        MINVAL = IAttr(
            'MINVAL', True, False, False,
            ArgDesc(1, 1, DataNode),
            {"dim": DataNode, "mask": DataNode}, UnresolvedType())
        MOD = IAttr(
            'MOD', True, True, False,
            ArgDesc(2, 2, DataNode), {}, UnresolvedType())
        MODULO = IAttr(
            'MODULO', True, True, False,
            ArgDesc(2, 2, DataNode), {}, UnresolvedType())
        MOVE_ALLOC = IAttr(
            'MOVE_ALLOC', False, False, False,
            ArgDesc(2, 2, DataNode), {"stat": DataNode, "errmsg": DataNode},
            UnresolvedType())
        MVBITS = IAttr(
            'MVBITS', True, True, False,
            ArgDesc(5, 5, DataNode), {}, UnresolvedType())
        NEAREST = IAttr(
            'NEAREST', True, True, False,
            ArgDesc(2, 2, DataNode), {}, UnresolvedType())
        NEW_LINE = IAttr(
            'NEW_LINE', True, True, False,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        NINT = IAttr(
            'NINT', True, True, False,
            ArgDesc(1, 1, DataNode), {"kind": DataNode}, UnresolvedType())
        NORM2 = IAttr(
            'NORM2', True, False, False,
            ArgDesc(1, 2, DataNode), {}, UnresolvedType())
        NOT = IAttr(
            'NOT', True, True, False,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        NULL = IAttr(
            'NULL', True, False, False,
            ArgDesc(0, 0, DataNode), {"mold": DataNode}, UnresolvedType())
        NUM_IMAGES = IAttr(
            'NUM_IMAGES', True, False, False,
            ArgDesc(0, 1, DataNode), {}, UnresolvedType())
        OUT_OF_RANGE = IAttr(
            'OUT_OF_RANGE', True, True, False,
            ArgDesc(2, 2, DataNode), {"round": DataNode}, UnresolvedType())
        PACK = IAttr(
            'PACK', True, False, False,
            ArgDesc(2, 2, DataNode), {"vector": DataNode}, UnresolvedType())
        PARITY = IAttr(
            'PARITY', True, False, False,
            ArgDesc(1, 2, DataNode), {}, UnresolvedType())
        POPCNT = IAttr(
            'POPCNT', True, True, False,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        POPPAR = IAttr(
            'POPPAR', True, True, False,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        PRECISION = IAttr(
            'PRECISION', True, False, True,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        PRESENT = IAttr(
            'PRESENT', True, False, True,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        PRODUCT = IAttr(
            'PRODUCT', True, False, False,
            ArgDesc(1, 1, DataNode), {"dim": DataNode, "mask": DataNode},
            UnresolvedType())
        RADIX = IAttr(
            'RADIX', True, False, True,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        RANDOM_INIT = IAttr(
            'RANDOM_INIT', False, False, False,
            ArgDesc(2, 2, DataNode), {}, UnresolvedType())
        RANDOM_NUMBER = IAttr(
            'RANDOM_NUMBER', False, False, False,
            ArgDesc(1, 1, Reference), {}, UnresolvedType())
        RANDOM_SEED = IAttr(
            'RANDOM_SEED', False, False, False,
            ArgDesc(0, 0, Reference),
            {"size": DataNode, "put": DataNode, "Get": DataNode},
            UnresolvedType())
        RANGE = IAttr(
            'RANGE', True, False, True,
            ArgDesc(1, 1, Reference), {}, UnresolvedType())
        RANK = IAttr(
            'RANK', True, False, True,
            ArgDesc(1, 1, Reference), {}, UnresolvedType())
        REAL = IAttr(
            'REAL', True, True, False,
            ArgDesc(1, 1, Reference), {"kind": DataNode}, UnresolvedType())
        REDUCE = IAttr(
            'REDUCE', True, False, False,
            ArgDesc(2, 3, Reference),
            {"mask": DataNode, "identity": DataNode, "ordered": DataNode},
            UnresolvedType())
        REPEAT = IAttr(
            'REPEAT', True, False, False,
            ArgDesc(2, 2, Reference), {}, UnresolvedType())
        RESHAPE = IAttr(
            'RESHAPE', True, False, False,
            ArgDesc(2, 2, Reference), {"pad": DataNode, "order": DataNode},
            UnresolvedType())
        RRSPACING = IAttr(
            'RRSPACING', True, True, False,
            ArgDesc(1, 1, Reference), {}, UnresolvedType())
        SAME_TYPE_AS = IAttr(
            'SAME_TYPE_AS', True, False, True,
            ArgDesc(2, 2, Reference), {}, UnresolvedType())
        SCALE = IAttr(
            'SCALE', True, True, False,
            ArgDesc(2, 2, Reference), {}, UnresolvedType())
        SCAN = IAttr(
            'SCAN', True, True, False,
            ArgDesc(2, 2, Reference), {"back": DataNode, "kind": DataNode},
            UnresolvedType())
        SELECTED_CHAR_KIND = IAttr(
            'SELECTED_CHAR_KIND', True, False, False,
            ArgDesc(1, 1, Reference), {}, UnresolvedType())
        SELECTED_INT_KIND = IAttr(
            'SELECTED_INT_KIND', True, False, False,
            ArgDesc(1, 1, Reference), {}, UnresolvedType())
        SELECTED_REAL_KIND = IAttr(
            'SELECTED_REAL_KIND', True, False, False,
            ArgDesc(0, 0, Reference),
            {"P": DataNode, "R": DataNode, "radix": DataNode},
            UnresolvedType())
        SET_EXPONENT = IAttr(
            'SET_EXPONENT', True, True, False,
            ArgDesc(2, 2, Reference), {}, UnresolvedType())
        SHAPE = IAttr(
            'SHAPE', True, False, True,
            ArgDesc(1, 1, Reference), {"kind": DataNode}, UnresolvedType())
        SHIFTA = IAttr(
            'SHIFTA', True, True, False,
            ArgDesc(2, 2, Reference), {}, UnresolvedType())
        SHIFTL = IAttr(
            'SHIFTL', True, True, False,
            ArgDesc(2, 2, Reference), {}, UnresolvedType())
        SHIFTR = IAttr(
            'SHIFTR', True, True, False,
            ArgDesc(2, 2, Reference), {}, UnresolvedType())
        SIGN = IAttr(
            'SIGN', True, True, False,
            ArgDesc(2, 2, DataNode), {}, UnresolvedType())
        SIN = IAttr(
            'SIN', True, True, False,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        SINH = IAttr(
            'SINH', True, True, False,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        SIZE = IAttr(
            'SIZE', True, False, True,
            ArgDesc(1, 1, DataNode), {"dim": DataNode, "kind": DataNode},
            UnresolvedType())
        SPACING = IAttr(
            'SPACING', True, True, False,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        SPREAD = IAttr(
            'SPREAD', True, False, False,
            ArgDesc(3, 3, DataNode), {}, UnresolvedType())
        SQRT = IAttr(
            'SQRT', True, True, False,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        STOPPED_IMAGES = IAttr(
            'STOPPED_IMAGES', False, False, False,
            ArgDesc(0, 0, DataNode), {"team": DataNode, "kind": DataNode},
            UnresolvedType())
        STORAGE_SIZE = IAttr(
            'STORAGE_SIZE', True, False, True,
            ArgDesc(1, 1, DataNode), {"kind": DataNode}, UnresolvedType())
        SUM = IAttr(
            'SUM', True, False, False,
            ArgDesc(1, 1, DataNode), {"dim": DataNode, "mask": DataNode},
            UnresolvedType())
        SYSTEM_CLOCK = IAttr(
            'SYSTEM_CLOCK', False, False, False,
            ArgDesc(0, 0, DataNode),
            {"count": DataNode, "count_rate": DataNode, "count_max": DataNode},
            UnresolvedType())
        TAN = IAttr(
            'TAN', True, True, False,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        TANH = IAttr(
            'TANH', True, True, False,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        TEAM_IMAGE = IAttr(
            'TEAM_IMAGE', True, False, False,
            ArgDesc(0, 0, DataNode), {"team": DataNode}, UnresolvedType())
        THIS_IMAGE = IAttr(
            'THIS_IMAGE', True, False, False,
            ArgDesc(0, 0, DataNode),
            {"coarray": DataNode, "team": DataNode, "dim": DataNode},
            UnresolvedType())
        TINY = IAttr(
            'TINY', True, False, True,
            ArgDesc(1, 1, (Reference, Literal)), {}, UnresolvedType())
        TRAILZ = IAttr(
            'TRAILZ', True, True, False,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        TRANSFER = IAttr(
            'TRANSFER', True, False, False,
            ArgDesc(2, 2, DataNode), {"size": DataNode}, UnresolvedType())
        TRANSPOSE = IAttr(
            'TRANSPOSE', True, False, False,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        TRIM = IAttr(
            'TRIM', True, False, False,
            ArgDesc(1, 1, DataNode), {}, UnresolvedType())
        UBOUND = IAttr(
            'UBOUND', True, False, True,
            ArgDesc(1, 1, DataNode), {"dim": DataNode, "kind": DataNode},
            UnresolvedType())
        UCOBOUND = IAttr(
            'UCOBOUND', True, False, True,
            ArgDesc(1, 1, DataNode), {"dim": DataNode, "kind": DataNode},
            UnresolvedType())
        UNPACK = IAttr(
            'UNPACK', True, False, False,
            ArgDesc(3, 3, DataNode), {}, UnresolvedType())
        VERIFY = IAttr(
            'VERIFY', True, True, False,
            ArgDesc(2, 2, DataNode), {"back": DataNode, "kind": DataNode},
            UnresolvedType())

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

    @property
    def datatype(self):
        '''
        '''
        if self.intrinsic == IntrinsicCall.Intrinsic.MAXVAL:
            pass
        return super().datatype


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
