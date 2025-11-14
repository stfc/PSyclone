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

''' This module contains the IntrinsicCall node implementation.'''

from collections import namedtuple
from collections.abc import Iterable
from enum import Enum

from psyclone.core import AccessType, VariablesAccessMap
from psyclone.psyir.nodes.call import Call
from psyclone.psyir.nodes.datanode import DataNode
from psyclone.psyir.nodes.literal import Literal
from psyclone.psyir.nodes.reference import Reference
from psyclone.psyir.symbols import IntrinsicSymbol, Symbol

# pylint: disable=too-many-branches

# Named tuple for describing the attributes of each intrinsic
IAttr = namedtuple(
    "IAttr",
    "name is_pure is_elemental is_inquiry required_args optional_args "
    "return_type reference_accesses",
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
                         required_args, optional_args, return_type,
                         reference_accesses)

        Note that name is duplicated inside IAttr because each item in the
        Enum must have a different value, and without the name that would
        not be guaranteed.

        '''
        # Fortran special-case statements (technically not Fortran intrinsics
        # but in PSyIR they are represented as Intrinsics)
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
            return_type=None,
            reference_accesses=None,
        )
        ACHAR = IAttr(
            name="ACHAR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"kind": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        ACOS = IAttr(
            name="ACOS",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        ACOSH = IAttr(
            name="ACOSH",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        ADJUSTL = IAttr(
            name="ADJUSTL",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        ADJUSTR = IAttr(
            name="ADJUSTR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        AIMAG = IAttr(
            name="AIMAG",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        AINT = IAttr(
            name="AINT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"kind": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        ALL = IAttr(
            name="ALL",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"dim": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        ALLOCATED = IAttr(
            name="ALLOCATED",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        ANINT = IAttr(
            name="ANINT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"kind": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        ANY = IAttr(
            name="ANY",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"dim": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        ASIN = IAttr(
            name="ASIN",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        ASINH = IAttr(
            name="ASINH",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        ASSOCIATED = IAttr(
            name="ASSOCIATED",
            is_pure=False,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"target": DataNode},
            return_type=None,
            reference_accesses=None,
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
            return_type=None,
            reference_accesses=None,
        )
        ATAN2 = IAttr(
            name="ATAN2",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        ATANH = IAttr(
            name="ATANH",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        ATOMIC_ADD = IAttr(
            name="ATOMIC_ADD",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={"stat": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        ATOMIC_AND = IAttr(
            name="ATOMIC_AND",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={"stat": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        ATOMIC_CAS = IAttr(
            name="ATOMIC_CAS",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={"stat": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        ATOMIC_DEFINE = IAttr(
            name="ATOMIC_DEFINE",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={"stat": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        ATOMIC_FETCH_ADD = IAttr(
            name="ATOMIC_FETCH_ADD",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(3, 3, DataNode),
            optional_args={"stat": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        ATOMIC_FETCH_AND = IAttr(
            name="ATOMIC_FETCH_AND",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(3, 3, DataNode),
            optional_args={"stat": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        ATOMIC_FETCH_OR = IAttr(
            name="ATOMIC_FETCH_OR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(3, 3, DataNode),
            optional_args={"stat": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        ATOMIC_FETCH_XOR = IAttr(
            name="ATOMIC_FETCH_XOR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(3, 3, DataNode),
            optional_args={"stat": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        ATOMIC_OR = IAttr(
            name="ATOMIC_OR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={"stat": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        ATOMIC_REF = IAttr(
            name="ATOMIC_REF",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={"stat": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        ATOMIC_XOR = IAttr(
            name="ATOMIC_XOR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={"stat": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        BESSEL_J0 = IAttr(
            name="BESSEL_J0",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        BESSEL_J1 = IAttr(
            name="BESSEL_J1",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        BESSEL_JN = IAttr(
            name="BESSEL_JN",
            is_pure=True,
            # TODO 3141 The elemental status is dependent on the
            # structure of the IntrinsicCall.
            is_elemental=None,
            is_inquiry=False,
            required_args=ArgDesc(2, 3, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        BESSEL_Y0 = IAttr(
            name="BESSEL_Y0",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        BESSEL_Y1 = IAttr(
            name="BESSEL_Y1",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        BESSEL_YN = IAttr(
            name="BESSEL_YN",
            is_pure=True,
            # TODO 3141 The elemental status is dependent on the
            # structure of the IntrinsicCall.
            is_elemental=None,
            is_inquiry=False,
            required_args=ArgDesc(2, 3, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        BGE = IAttr(
            name="BGE",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        BGT = IAttr(
            name="BGT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        BIT_SIZE = IAttr(
            name="BIT_SIZE",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        BLE = IAttr(
            name="BLE",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        BLT = IAttr(
            name="BLT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        BTEST = IAttr(
            name="BTEST",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        CEILING = IAttr(
            name="CEILING",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"kind": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        CHAR = IAttr(
            name="CHAR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"kind": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        CMPLX = IAttr(
            name="CMPLX",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"Y": DataNode, "kind": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        CO_BROADCAST = IAttr(
            name="CO_BROADCAST",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, 2, DataNode),
            optional_args={"stat": DataNode, "errmsg": DataNode},
            return_type=None,
            reference_accesses=None,
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
            reference_accesses=None,
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
            reference_accesses=None,
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
            reference_accesses=None,
        )
        COMMAND_ARGUMENT_COUNT = IAttr(
            name="COMMAND_ARGUMENT_COUNT",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(0, 0, None),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        CONJG = IAttr(
            name="CONJG",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        COS = IAttr(
            name="COS",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        COSH = IAttr(
            name="COSH",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        COSHAPE = IAttr(
            name="COSHAPE",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"kind": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        COUNT = IAttr(
            name="COUNT",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"dim": DataNode, "kind": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        CPU_TIME = IAttr(
            name="CPU_TIME",
            is_pure=False,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        CSHIFT = IAttr(
            name="CSHIFT",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={"dim": DataNode},
            return_type=None,
            reference_accesses=None,
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
            reference_accesses=None,
        )
        DBLE = IAttr(
            name="DBLE",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        DIGITS = IAttr(
            name="DIGITS",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        DIM = IAttr(
            name="DIM",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        DOT_PRODUCT = IAttr(
            name="DOT_PRODUCT",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        DPROD = IAttr(
            name="DPROD",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        DSHIFTL = IAttr(
            name="DSHIFTL",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(3, 3, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        DSHIFTR = IAttr(
            name="DSHIFTR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(3, 3, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        EOSHIFT = IAttr(
            name="EOSHIFT",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={"boundary": DataNode, "dim": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        EPSILON = IAttr(
            name="EPSILON",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        ERF = IAttr(
            name="ERF",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        ERFC = IAttr(
            name="ERFC",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        ERFC_SCALED = IAttr(
            name="ERFC_SCALED",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        EVENT_QUERY = IAttr(
            name="EVENT_QUERY",
            is_pure=False,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={"stat": DataNode},
            return_type=None,
            reference_accesses=None,
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
            reference_accesses=None,
        )
        EXP = IAttr(
            name="EXP",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        EXPONENT = IAttr(
            name="EXPONENT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        EXTENDS_TYPE_OF = IAttr(
            name="EXTENDS_TYPE_OF",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        FAILED_IMAGES = IAttr(
            name="FAILED_IMAGES",
            is_pure=False,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(0, 0, DataNode),
            optional_args={"team": DataNode, "kind": DataNode},
            return_type=None,
            reference_accesses=None,
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
            return_type=None,
            reference_accesses=None,
        )
        FLOAT = IAttr(
            name="FLOAT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        FLOOR = IAttr(
            name="FLOOR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"kind": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        FRACTION = IAttr(
            name="FRACTION",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        GAMMA = IAttr(
            name="GAMMA",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
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
            reference_accesses=None,
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
            required_args=ArgDesc(1, 1, DataNode),
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
            required_args=ArgDesc(0, 0, DataNode),
            optional_args={"level": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        HUGE = IAttr(
            name="HUGE",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, (Reference, Literal)),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        HYPOT = IAttr(
            name="HYPOT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        IACHAR = IAttr(
            name="IACHAR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"kind": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        IALL = IAttr(
            name="IALL",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"dim": DataNode, "mask": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        IAND = IAttr(
            name="IAND",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        IANY = IAttr(
            name="IANY",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"dim": DataNode, "mask": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        IBCLR = IAttr(
            name="IBCLR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        IBITS = IAttr(
            name="IBITS",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(3, 3, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        IBSET = IAttr(
            name="IBSET",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        ICHAR = IAttr(
            name="ICHAR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"kind": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        IEOR = IAttr(
            name="IEOR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        IMAGE_INDEX = IAttr(
            name="IMAGE_INDEX",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(2, 3, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        IMAGE_STATUS = IAttr(
            name="IMAGE_STATUS",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"team": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        INDEX = IAttr(
            name="INDEX",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={"back": DataNode, "kind": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        INT = IAttr(
            name="INT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"kind": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        IOR = IAttr(
            name="IOR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        IPARITY = IAttr(
            name="IPARITY",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, 2, DataNode),
            optional_args={"mask": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        IS_CONTIGUOUS = IAttr(
            name="IS_CONTIGUOUS",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        IS_IOSTAT_END = IAttr(
            name="IS_IOSTAT_END",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        IS_IOSTAT_EOR = IAttr(
            name="IS_IOSTAT_EOR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        ISHFT = IAttr(
            name="ISHFT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        ISHFTC = IAttr(
            name="ISHFTC",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={"size": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        KIND = IAttr(
            name="KIND",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        LBOUND = IAttr(
            name="LBOUND",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"dim": DataNode, "kind": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        LCOBOUND = IAttr(
            name="LCOBOUND",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"dim": DataNode, "kind": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        LEADZ = IAttr(
            name="LEADZ",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        LEN = IAttr(
            name="LEN",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"kind": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        LEN_TRIM = IAttr(
            name="LEN_TRIM",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"kind": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        LGE = IAttr(
            name="LGE",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        LGT = IAttr(
            name="LGT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        LLE = IAttr(
            name="LLE",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        LLT = IAttr(
            name="LLT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        LOG = IAttr(
            name="LOG",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        LOG_GAMMA = IAttr(
            name="LOG_GAMMA",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        LOG10 = IAttr(
            name="LOG10",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        LOGICAL = IAttr(
            name="LOGICAL",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"kind": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        MASKL = IAttr(
            name="MASKL",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"kind": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        MASKR = IAttr(
            name="MASKR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"kind": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        MATMUL = IAttr(
            name="MATMUL",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        MAX = IAttr(
            name="MAX",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, None, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        MAXEXPONENT = IAttr(
            name="MAXEXPONENT",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
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
            return_type=None,
            reference_accesses=None,
        )
        MAXVAL = IAttr(
            name="MAXVAL",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"dim": DataNode, "mask": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        MERGE = IAttr(
            name="MERGE",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(3, 3, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None
        )
        MERGE_BITS = IAttr(
            name="MERGE_BITS",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(3, 3, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        MIN = IAttr(
            name="MIN",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, None, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        MINEXPONENT = IAttr(
            name="MINEXPONENT",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
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
            return_type=None,
            reference_accesses=None,
        )
        MINVAL = IAttr(
            name="MINVAL",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"dim": DataNode, "mask": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        MOD = IAttr(
            name="MOD",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None
        )
        MODULO = IAttr(
            name="MODULO",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        MOVE_ALLOC = IAttr(
            name="MOVE_ALLOC",
            is_pure=False,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={"stat": DataNode, "errmsg": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        MVBITS = IAttr(
            name="MVBITS",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(5, 5, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        NEAREST = IAttr(
            name="NEAREST",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        NEW_LINE = IAttr(
            name="NEW_LINE",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        NINT = IAttr(
            name="NINT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"kind": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        NORM2 = IAttr(
            name="NORM2",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, 2, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        NOT = IAttr(
            name="NOT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None
        )
        NULL = IAttr(
            name="NULL",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(0, 0, DataNode),
            optional_args={"mold": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        NUM_IMAGES = IAttr(
            name="NUM_IMAGES",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(0, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        OUT_OF_RANGE = IAttr(
            name="OUT_OF_RANGE",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={"round": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        PACK = IAttr(
            name="PACK",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={"vector": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        PARITY = IAttr(
            name="PARITY",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, 2, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        POPCNT = IAttr(
            name="POPCNT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        POPPAR = IAttr(
            name="POPPAR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        PRECISION = IAttr(
            name="PRECISION",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        PRESENT = IAttr(
            name="PRESENT",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        PRODUCT = IAttr(
            name="PRODUCT",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"dim": DataNode, "mask": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        RADIX = IAttr(
            name="RADIX",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None
        )
        RANDOM_INIT = IAttr(
            name="RANDOM_INIT",
            is_pure=False,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        RANDOM_NUMBER = IAttr(
            name="RANDOM_NUMBER",
            is_pure=False,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, Reference),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        RANDOM_SEED = IAttr(
            name="RANDOM_SEED",
            is_pure=False,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(0, 0, Reference),
            optional_args={"size": DataNode, "put": DataNode, "Get": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        RANGE = IAttr(
            name="RANGE",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, Reference),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        RANK = IAttr(
            name="RANK",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, Reference),
            optional_args={},
            return_type=None,
            reference_accesses=None
        )
        REAL = IAttr(
            name="REAL",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, Reference),
            optional_args={"kind": DataNode},
            return_type=None,
            reference_accesses=None,
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
            return_type=None,
            reference_accesses=None,
        )
        REPEAT = IAttr(
            name="REPEAT",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, Reference),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        RESHAPE = IAttr(
            name="RESHAPE",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, Reference),
            optional_args={"pad": DataNode, "order": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        RRSPACING = IAttr(
            name="RRSPACING",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, Reference),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        SAME_TYPE_AS = IAttr(
            name="SAME_TYPE_AS",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(2, 2, Reference),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        SCALE = IAttr(
            name="SCALE",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, Reference),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        SCAN = IAttr(
            name="SCAN",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, Reference),
            optional_args={"back": DataNode, "kind": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        SELECTED_CHAR_KIND = IAttr(
            name="SELECTED_CHAR_KIND",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, Reference),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        SELECTED_INT_KIND = IAttr(
            name="SELECTED_INT_KIND",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, Reference),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        SELECTED_REAL_KIND = IAttr(
            name="SELECTED_REAL_KIND",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(0, 0, Reference),
            optional_args={"P": DataNode, "R": DataNode, "radix": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        SET_EXPONENT = IAttr(
            name="SET_EXPONENT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, Reference),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        SHAPE = IAttr(
            name="SHAPE",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, Reference),
            optional_args={"kind": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        SHIFTA = IAttr(
            name="SHIFTA",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, Reference),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        SHIFTL = IAttr(
            name="SHIFTL",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, Reference),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        SHIFTR = IAttr(
            name="SHIFTR",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, Reference),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        SIGN = IAttr(
            name="SIGN",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None
        )
        SIN = IAttr(
            name="SIN",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None
        )
        SINH = IAttr(
            name="SINH",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None
        )
        SIZE = IAttr(
            name="SIZE",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"dim": DataNode, "kind": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        SPACING = IAttr(
            name="SPACING",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        SPREAD = IAttr(
            name="SPREAD",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(3, 3, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        SQRT = IAttr(
            name="SQRT",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None
        )
        STOPPED_IMAGES = IAttr(
            name="STOPPED_IMAGES",
            is_pure=False,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(0, 0, DataNode),
            optional_args={"team": DataNode, "kind": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        STORAGE_SIZE = IAttr(
            name="STORAGE_SIZE",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"kind": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        SUM = IAttr(
            name="SUM",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"dim": DataNode, "mask": DataNode},
            return_type=None,
            reference_accesses=None,
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
            reference_accesses=None,
        )
        TAN = IAttr(
            name="TAN",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None
        )
        TANH = IAttr(
            name="TANH",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None
        )
        TEAM_NUMBER = IAttr(
            name="TEAM_NUMBER",
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
            required_args=ArgDesc(0, 0, DataNode),
            optional_args={"coarray": DataNode,
                           "team": DataNode,
                           "dim": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        TINY = IAttr(
            name="TINY",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, (Reference, Literal)),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        TRAILZ = IAttr(
            name="TRAILZ",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        TRANSFER = IAttr(
            name="TRANSFER",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={"size": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        TRANSPOSE = IAttr(
            name="TRANSPOSE",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        TRIM = IAttr(
            name="TRIM",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None
        )
        UBOUND = IAttr(
            name="UBOUND",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"dim": DataNode, "kind": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        UCOBOUND = IAttr(
            name="UCOBOUND",
            is_pure=True,
            is_elemental=False,
            is_inquiry=True,
            required_args=ArgDesc(1, 1, DataNode),
            optional_args={"dim": DataNode, "kind": DataNode},
            return_type=None,
            reference_accesses=None,
        )
        UNPACK = IAttr(
            name="UNPACK",
            is_pure=True,
            is_elemental=False,
            is_inquiry=False,
            required_args=ArgDesc(3, 3, DataNode),
            optional_args={},
            return_type=None,
            reference_accesses=None,
        )
        VERIFY = IAttr(
            name="VERIFY",
            is_pure=True,
            is_elemental=True,
            is_inquiry=False,
            required_args=ArgDesc(2, 2, DataNode),
            optional_args={"back": DataNode, "kind": DataNode},
            return_type=None,
            reference_accesses=None,
        )

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

    def get_all_accessed_symbols(self) -> set[Symbol]:
        '''
        :returns: a set of all the symbols accessed inside this IntrinsicCall.
        '''
        symbols = set()
        for child in self.arguments:
            symbols.update(child.get_all_accessed_symbols())
        return symbols

    def reference_accesses(self) -> VariablesAccessMap:
        '''
        :returns: a map of all the symbol accessed inside this node, the
            keys are Signatures (unique identifiers to a symbol and its
            structure acccessors) and the values are AccessSequence
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
# MATMUL can fail at link time depending on the precision of
# its arguments.
# IntrinsicCall.Intrinsic.MATMUL,

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
