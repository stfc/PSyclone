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
from typing import List, Tuple

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
# then `max_count` will be None. If max_count is not None, then arg_names
# will contain a list of the argument names of the required arguments, in
# the order defined by the standard. If max_count is None, arg_names will
# be a tuple containing None to ensure the canonicalisation logic still
# works.
ArgDesc = namedtuple('ArgDesc', 'min_count max_count types arg_names')


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

        All argument names (i.e. required_args.arg_names and 
        optional_args) should be lower case.

        '''
        # Fortran special-case statements (technically not Fortran intrinsics
        # but in PSyIR they are represented as Intrinsics)
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            required_args=ArgDesc(
                min_count=2,
                max_count=2,
                types=DataNode,
                arg_names=(("y", "x"),)),
            optional_args={},
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            optional_args={"Y": DataNode, "kind": DataNode},
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            optional_args={"P": DataNode, "R": DataNode, "radix": DataNode},
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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
            return_type=None,
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

    def _find_matching_interface(self) -> Tuple[str]:
        '''
        Finds the matching required argument interface for this node to
        canonicalise to.

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
                # this interface cannot be a candidate for canonicalisation.
                remaining_required = choice[num_positional_arguments:]
                for name in remaining_required:
                    lname = name.lower()
                    if lname not in self.argument_names:
                        potential_interfaces.remove(choice)
                        break

            # If we didn't reduce the number of potential interfacfes to a
            # single interface then we can't canonicalise.
            if (len(potential_interfaces) > 1 or
                    len(potential_interfaces) == 0):
                raise NotImplementedError(
                    f"Cannot canonicalise '{self.intrinsic.name}' "
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

    def canonicalise(self):
        '''Canonicalise an IntrinsicCall in the PSyIR. Upon successful
        canonicalisation, all arguments will become named arguments.

        A small number of intrinsics (e.g. ALLOCATE) never have ambiguity
        and no argument limits, in which case no canonicalisation is done.

        :raises ValueError: If the number of arguments or argument names
            are not valid for this IntrinsicCall.
        :raises NotImplementedError: If there is argument ambiguity and
            canonicalisation is not possible.
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
                        f"canonicalising the '{self.intrinsic.name}' "
                        f"IntrinsicCall. Allowed argument names are "
                        f"'{sorted(set(all_valid_names))}'."
                    )

        # Check that this call has a valid number of arguments
        if len(self.arguments) < self.intrinsic.required_args.min_count:
            raise ValueError(
                f"Found too few arguments when canonicalising the "
                f"'{self.intrinsic.name}' IntrinsicCall. Requires at "
                f"least {self.intrinsic.required_args.min_count} "
                f"arguments but found {len(self.arguments)}."
            )

        # If there is no maximum number of required arguments then we
        # can skip the rest of canonicalisation, as this Intrinsic can never
        # have ambiguity.
        if self.intrinsic.required_args.max_count is None:
            return

        if (len(self.arguments) > (self.intrinsic.required_args.max_count +
                                   len(optional_names))):
            max_args = (self.intrinsic.required_args.max_count +
                        len(optional_names))
            raise ValueError(
                f"Found too many arguments when canonicalising the "
                f"'{self.intrinsic.name}' IntrinsicCall. Requires at most "
                f"{max_args} arguments but found {len(self.arguments)}."
            )

        # Find which intrinsic call interface we are canonicalising with.
        interface_arg_names = self._find_matching_interface()

        # Handle cases where None or "" is in the interface_arg_names,
        # as this implies context sensitive argument naming which PSyclone
        # cannot handle.
        if interface_arg_names and not interface_arg_names[0]:
            # If we find any named non-optional named arguments for these
            # intrinsics then we can't canonicalise this IntrinsicCall.
            # N.B. With currently supported intrinsic there are no
            # optional argument on these context-sensitive intrinsics
            # that have a finite argument count, but we keep the check
            # in case we need the support in future, and it still handles
            # what we currently need to check (i.e. if we have a named
            # argument here we can't canonicalise it safely).
            for name in self.argument_names:
                if not name:
                    continue
                if name not in optional_names:
                    raise NotImplementedError(
                        f"Cannot canonicalise '{self.intrinsic.name}' "
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
                        f"a {type(arg[0]).__name__} instead of a str.")
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
                        f"argument ('{last_named_arg}'). This is invalid.'")
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

        # Error check and canonicalise the call
        try:
            call.canonicalise()
        except (ValueError, NotImplementedError):
            # Since we fail canonicalisation, we need to undo any links
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
