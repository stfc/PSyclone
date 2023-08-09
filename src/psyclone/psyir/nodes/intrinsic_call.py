# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2023, Science and Technology Facilities Council.
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
from enum import Enum

from psyclone.psyir.nodes.call import Call
from psyclone.psyir.nodes.datanode import DataNode
from psyclone.psyir.nodes.literal import Literal
from psyclone.psyir.nodes.reference import Reference
from psyclone.psyir.symbols import IntrinsicSymbol

# pylint: disable=too-many-branches

# Named tuple for describing the attributes of each intrinsic
IAttr = namedtuple(
    'IAttr',
    'name is_pure is_elemental is_inquiry is_available_gpu '
    'required_args optional_args'
)


#: Named tuple for describing the properties of the required arguments to
#: a particular intrinsic. If there's no limit on the number of arguments
#: then `max_count` will be None.
ArgDesc = namedtuple('ArgDesc', 'min_count max_count types')


class IntrinsicCall(Call):
    ''' Node representing a call to an intrinsic routine (function or
    subroutine). This can be found as a standalone statement
    or an expression.

    :param routine: the type of Intrinsic being created.
    :type routine: py:class:`psyclone.psyir.IntrinsicCall.Intrinsic`
    :param kwargs: additional keyword arguments provided to the PSyIR node.
    :type kwargs: unwrapped dict.

    :raises TypeError: if the routine argument is not an Intrinsic type.

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
                         is_available_gpu, required_args, optional_args)

        Note that name is duplicated inside IAttr because each item in the
        Enum must have a different value, and without the name that would
        not be guaranteed.

        '''
        # Fortran special-case statements (technically not Fortran intrinsics
        # but in PSyIR they are represented as Intrinsics)
        ALLOCATE = IAttr('ALLOCATE', False, False, False, False,
                         ArgDesc(1, None, Reference),
                         {"mold": Reference, "source": Reference,
                          "stat": Reference, "errmsg": Reference})
        DEALLOCATE = IAttr('DEALLOCATE', False, False, False, False,
                           ArgDesc(1, None, Reference), {"stat": Reference})

        # Fortran Intrinsics (from Fortran 2018 standard table 16.1)
        ABS = IAttr('ABS', True, True, False, True,
                    ArgDesc(1, 1, DataNode), {})
        ACHAR = IAttr('ACHAR', True, True, False, False,
                      ArgDesc(1, 1, DataNode), {"kind": DataNode})
        ACOS = IAttr('ACOS', True, True, False, True,
                     ArgDesc(1, 1, DataNode), {})
        ACOSH = IAttr('ACOS', True, True, False, True,
                      ArgDesc(1, 1, DataNode), {})
        ADJUSTL = IAttr('ADJUSTL', True, True, False, False,
                        ArgDesc(1, 1, DataNode), {})
        ADJUSTR = IAttr('ADJUSTR', True, True, False, False,
                        ArgDesc(1, 1, DataNode), {})
        AIMAG = IAttr('AIMAG', True, True, False, False,
                      ArgDesc(1, 1, DataNode), {})
        AINT = IAttr('AINT', True, True, False, True,
                     ArgDesc(1, 1, DataNode), {"kind": DataNode})
        ALL = IAttr('ALL', True, False, False, False,
                    ArgDesc(1, 1, DataNode), {"dim": DataNode})  # ?
        ALLOCATED = IAttr('ALLOCATED', True, False, True, False,
                          ArgDesc(1, 1, DataNode), {})
        ANINT = IAttr('ANINT', True, True, False, True,
                      ArgDesc(1, 1, DataNode), {"kind": DataNode})
        ANY = IAttr('ANY', True, False, False, False,
                    ArgDesc(1, 1, DataNode), {"dim": DataNode})  # ?
        ASIN = IAttr('ASIN', True, True, False, True,
                     ArgDesc(1, 1, DataNode), {})
        ASINH = IAttr('ASINH', True, True, False, False,
                      ArgDesc(1, 1, DataNode), {})
        ASSOCIATED = IAttr('ASSOCIATED', False, False, True, False,
                           ArgDesc(1, 1, DataNode), {"target": DataNode})
        ATAN = IAttr('ATAN', True, True, False, True,
                     ArgDesc(1, 2, DataNode), {})
        ATAN2 = IAttr('ATAN2', True, True, False, True,
                      ArgDesc(2, 2, DataNode), {})
        ATANH = IAttr('ATANH', True, True, False, False,
                      ArgDesc(1, 1, DataNode), {})
        # Are atomic elemental? Are they available for GPU?
        ATOMIC_ADD = IAttr('ATOMIC_ADD', True, True, False, False,
                           ArgDesc(2, 2, DataNode), {"stat": DataNode})
        ATOMIC_AND = IAttr('ATOMIC_AND', True, True, False, False,
                           ArgDesc(2, 2, DataNode), {"stat": DataNode})
        ATOMIC_CAS = IAttr('ATOMIC_CAS', True, True, False, False,
                           ArgDesc(2, 2, DataNode), {"stat": DataNode})
        ATOMIC_DEFINE = IAttr('ATOMIC_DEFINE', True, True, False, False,
                              ArgDesc(2, 2, DataNode), {"stat": DataNode})
        ATOMIC_FETCH_ADD = IAttr('ATOMIC_FETCH_ADD', True, True, False, False,
                                 ArgDesc(3, 3, DataNode), {"stat": DataNode})
        ATOMIC_FETCH_AND = IAttr('ATOMIC_FETCH_AND', True, True, False, False,
                                 ArgDesc(3, 3, DataNode), {"stat": DataNode})
        ATOMIC_FETCH_OR = IAttr('ATOMIC_FETCH_OR', True, True, False, False,
                                ArgDesc(3, 3, DataNode), {"stat": DataNode})
        ATOMIC_FETCH_XOR = IAttr('ATOMIC_FETCH_XOR', True, True, False, False,
                                 ArgDesc(3, 3, DataNode), {"stat": DataNode})
        ATOMIC_OR = IAttr('ATOMIC_OR', True, True, False, False,
                          ArgDesc(2, 2, DataNode), {"stat": DataNode})
        ATOMIC_REF = IAttr('ATOMIC_REF', True, True, False, False,
                           ArgDesc(2, 2, DataNode), {"stat": DataNode})
        ATOMIC_XOR = IAttr('ATOMIC_XOR', True, True, False, False,
                           ArgDesc(2, 2, DataNode), {"stat": DataNode})
        BESSEL_J0 = IAttr('BESSEL_J0', True, True, False, False,
                          ArgDesc(1, 1, DataNode), {})
        BESSEL_J1 = IAttr('BESSEL_J1', True, True, False, False,
                          ArgDesc(1, 1, DataNode), {})
        BESSEL_JN = IAttr('BESSEL_JN', True, None, False, False,
                          ArgDesc(2, 3, DataNode), {})
        BESSEL_Y0 = IAttr('BESSEL_Y0', True, True, False, False,
                          ArgDesc(1, 1, DataNode), {})
        BESSEL_Y1 = IAttr('BESSEL_Y1', True, True, False, False,
                          ArgDesc(1, 1, DataNode), {})
        BESSEL_YN = IAttr('BESSEL_YN', True, None, False, False,
                          ArgDesc(2, 3, DataNode), {})
        BGE = IAttr('BGE', True, True, False, False,
                    ArgDesc(2, 2, DataNode), {})
        BGT = IAttr('BGT', True, True, False, False,
                    ArgDesc(2, 2, DataNode), {})
        BIT_SIZE = IAttr('BIT_SIZE', True, False, True, False,
                         ArgDesc(1, 1, DataNode), {})
        BLE = IAttr('BLE', True, True, False, False,
                    ArgDesc(2, 2, DataNode), {})
        BLT = IAttr('BLT', True, True, False, False,
                    ArgDesc(2, 2, DataNode), {})
        BTEST = IAttr('BTEST', True, True, False, False,
                      ArgDesc(2, 2, DataNode), {})
        CEILING = IAttr('CEILING', True, True, False, False,
                        ArgDesc(1, 1, DataNode), {"kind": DataNode})
        CHAR = IAttr('CHAR', True, True, False, False,
                     ArgDesc(1, 1, DataNode), {"kind": DataNode})
        CMPLX = IAttr('CMPLX', True, True, False, False,
                      ArgDesc(1, 1, DataNode),
                      {"Y": DataNode, "kind": DataNode})
        # Collective intrinsics attributes?
        CO_BROADCAST = IAttr('CO_BROADCAST', True, True, False, False,
                             ArgDesc(1, 2, DataNode),
                             {"stat": DataNode, "errmsg": DataNode})
        CO_MAX = IAttr('CO_MAX', True, True, False, False,
                       ArgDesc(1, 1, DataNode),
                       {"result_image": DataNode, "stat": DataNode,
                        "errmsg": DataNode})
        CO_MIN = IAttr('CO_MIN', True, True, False, False,
                       ArgDesc(1, 1, DataNode),
                       {"result_image": DataNode, "stat": DataNode,
                        "errmsg": DataNode})
        CO_REDUCE = IAttr('CO_REDUCE', True, True, False, False,
                          ArgDesc(1, 2, DataNode),
                          {"result_image": DataNode, "stat": DataNode,
                           "errmsg": DataNode})
        CO_SUM = IAttr('CO_SUM', True, True, False, False,
                       ArgDesc(1, 1, DataNode),
                       {"result_image": DataNode, "stat": DataNode,
                        "errmsg": DataNode})
        COMMAND_ARGUMENT_COUNT = IAttr('COMMAND_ARGUMENT_COUNT',
                                       True, False, False, False,
                                       ArgDesc(0, 0, None), {})
        CONJG = IAttr('CONJG', True, True, False, False,
                      ArgDesc(1, 1, DataNode), {})
        COS = IAttr('COS', True, True, False, True,
                    ArgDesc(1, 1, DataNode), {})
        COSH = IAttr('COSH', True, True, False, True,
                     ArgDesc(1, 1, DataNode), {})
        COSHAPE = IAttr('COSHAPE', True, False, True, False,
                        ArgDesc(1, 1, DataNode), {"kind": DataNode})
        COUNT = IAttr('COUNT', True, False, False, False,
                      ArgDesc(1, 1, DataNode),
                      {"dim": DataNode, "kind": DataNode})
        CPU_TIME = IAttr('CPU_TIME', False, False, False, False,
                         ArgDesc(1, 1, DataNode), {})
        CSHIFT = IAttr('CSHIFT', True, False, False, False,
                       ArgDesc(2, 2, DataNode), {"dim": DataNode})
        DATE_AND_TIME = IAttr('DATE_AND_TIME', False, False, False, False,
                              ArgDesc(0, 0, DataNode),
                              {"date": DataNode, "time": DataNode,
                               "zone": DataNode, "values": DataNode})
        DBLE = IAttr('DBLE', True, True, False, True,
                     ArgDesc(1, 1, DataNode), {})
        DIGITS = IAttr('DIGITS', True, False, True, False,
                       ArgDesc(1, 1, DataNode), {})
        DIM = IAttr('DIM', True, True, False, False,
                    ArgDesc(2, 2, DataNode), {})
        DOT_PRODUCT = IAttr('DOT_PRODUCT', True, False, False, False,
                            ArgDesc(2, 2, DataNode), {})
        DPROD = IAttr('DPROD', True, True, False, True,
                      ArgDesc(2, 2, DataNode), {})
        DSHIFTL = IAttr('DSHIFTL', True, True, False, False,
                        ArgDesc(3, 3, DataNode), {})
        DSHIFTR = IAttr('DSHIFTR', True, True, False, False,
                        ArgDesc(3, 3, DataNode), {})
        EOSHIFT = IAttr('EOSHIFT', True, False, False, False,
                        ArgDesc(2, 2, DataNode),
                        {"boundary": DataNode, "dim": DataNode})
        EPSILON = IAttr('EPSILON', True, True, False, False,
                        ArgDesc(1, 1, DataNode), {})
        ERF = IAttr('ERF', True, True, False, False,
                    ArgDesc(1, 1, DataNode), {})
        ERFC = IAttr('ERFC', True, True, False, False,
                     ArgDesc(1, 1, DataNode), {})
        ERFC_SCALED = IAttr('ERFC_SCALED', True, True, False, False,
                            ArgDesc(1, 1, DataNode), {})
        EVENT_QUERY = IAttr('EVENT_QUERY', False, False, False, False,
                            ArgDesc(2, 2, DataNode), {"stat": DataNode})
        EXECUTE_COMMAND_LINE = IAttr('EXECUTE_COMMAND_LINE',
                                     False, False, False, False,
                                     ArgDesc(2, 2, DataNode),
                                     {"wait": DataNode, "exitstat": DataNode,
                                      "cmdstat": DataNode, "cmdmsg": DataNode})
        EXP = IAttr('EXP', True, True, False, True,
                    ArgDesc(1, 1, DataNode), {})
        EXPONENT = IAttr('EXPONENT', True, True, False, False,
                         ArgDesc(1, 1, DataNode), {})
        EXTENDS_TYPE_OF = IAttr('EXTENDS_TYPE_OF', True, False, True, False,
                                ArgDesc(2, 2, DataNode), {})
        FAILED_IMAGES = IAttr('FAILED_IMAGES', True, False, False, False,
                              ArgDesc(0, 0, DataNode),
                              {"team": DataNode, "kind": DataNode})
        FINDLOC = IAttr('FINDLOC', True, False, False, False,
                        ArgDesc(2, 3, DataNode),
                        {"mask": DataNode, "kind": DataNode,
                         "back": DataNode})
        FLOOR = IAttr('FLOOR', True, True, False, False,
                      ArgDesc(1, 1, DataNode), {"kind": DataNode})
        FRACTION = IAttr('FRACTION', True, True, False, False,
                         ArgDesc(1, 1, DataNode), {})
        GAMMA = IAttr('GAMMA', True, True, False, False,
                      ArgDesc(1, 1, DataNode), {})

        MIN = IAttr('MIN', True, True, False, True,
                    ArgDesc(1, None, DataNode), {})
        MAX = IAttr('MAX', True, True, False, True,
                    ArgDesc(1, None, DataNode), {})
        MATMUL = IAttr('MATMUL', True, False, False, False,
                       ArgDesc(2, 2, DataNode), {})
        SIGN = IAttr('SIGN', True, True, False, True,
                     ArgDesc(2, 2, DataNode), {})

        MOD = IAttr('MOD', True, True, False, True,
                     ArgDesc(2, 2, DataNode), {})

        TRANSPOSE = IAttr('TRANSPOSE', True, False, False, False,
                     ArgDesc(1, 1, DataNode), {})
        RANDOM_NUMBER = IAttr('RANDOM_NUMBER', False, False, False, False,
                              ArgDesc(1, 1, Reference), {})
        MINVAL = IAttr('MINVAL', True, False, False, False,
                       ArgDesc(1, 1, DataNode),
                       {"dim": DataNode, "mask": DataNode})
        MAXVAL = IAttr('MAXVAL', True, False, False, False,
                       ArgDesc(1, 1, DataNode),
                       {"dim": DataNode, "mask": DataNode})
        SUM = IAttr('SUM', True, False, False, False,
                    ArgDesc(1, 1, DataNode),
                    {"dim": DataNode, "mask": DataNode})
        TINY = IAttr('TINY', True, False, False, False,
                     ArgDesc(1, 1, (Reference, Literal)), {})
        HUGE = IAttr('HUGE', True, False, False, False,
                     ArgDesc(1, 1, (Reference, Literal)), {})

        SQRT = IAttr('SQRT', True, True, False, True,
                     ArgDesc(1, 1, DataNode), {})

        def __hash__(self):
            return hash(self.name)


    def __init__(self, routine, **kwargs):
        if not isinstance(routine, Enum) or routine not in self.Intrinsic:
            raise TypeError(
                f"IntrinsicCall 'routine' argument should be an "
                f"instance of IntrinsicCall.Intrinsic, but found "
                f"'{type(routine).__name__}'.")

        # A Call expects a symbol, so give it an intrinsic symbol.
        super().__init__(
            IntrinsicSymbol(
                routine.name,
                is_elemental=routine.is_elemental,
                is_pure=routine.is_pure),
            **kwargs)
        self._intrinsic = routine

    @property
    def intrinsic(self):
        ''' Return the type of intrinsic.

        :returns: enumerated type capturing the type of intrinsic.
        :rtype: :py:class:`psyclone.psyir.nodes.IntrinsicCall.Intrinsic`

        '''
        return self._intrinsic

    @classmethod
    def create(cls, routine, arguments):
        '''Create an instance of this class given the type of routine and a
        list of nodes (or name-and-node tuples) for its arguments. Any
        named arguments *must* come after any required arguments.

        :param routine: the Intrinsic being called.
        :type routine: py:class:`psyclone.psyir.IntrinsicCall.Intrinsic`
        :param arguments: the arguments to this routine, and/or \
            2-tuples containing an argument name and the \
            argument. Arguments are added as child nodes.
        :type arguments: List[ \
            Union[:py:class:`psyclone.psyir.nodes.DataNode`, \
                  Tuple[str, :py:class:`psyclone.psyir.nodes.DataNode`]]]

        :returns: an instance of this class.
        :rtype: :py:class:`psyclone.psyir.nodes.IntrinsicCall`

        :raises TypeError: if any of the arguments are of the wrong type.
        :raises ValueError: if any optional arguments have incorrect names \
            or if a positional argument is listed *after* a named argument.
        :raises ValueError: if the number of supplied arguments is not valid \
            for the specified intrinsic routine.

        '''
        if not isinstance(routine, Enum) or routine not in cls.Intrinsic:
            raise TypeError(
                f"IntrinsicCall create() 'routine' argument should be an "
                f"instance of IntrinsicCall.Intrinsic but found "
                f"'{type(routine).__name__}'.")

        if not isinstance(arguments, list):
            raise TypeError(
                f"IntrinsicCall.create() 'arguments' argument should be a "
                f"list but found '{type(arguments).__name__}'")

        if routine.optional_args:
            optional_arg_names = sorted(list(routine.optional_args.keys()))
        else:
            optional_arg_names = []

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
                if not optional_arg_names:
                    raise ValueError(
                        f"The '{routine.name}' intrinsic does not support "
                        f"any optional arguments but got '{name}'.")
                if name not in optional_arg_names:
                    raise ValueError(
                        f"The '{routine.name}' intrinsic supports the "
                        f"optional arguments {optional_arg_names} but got "
                        f"'{name}'")
                if not isinstance(arg[1], routine.optional_args[name]):
                    raise TypeError(
                        f"The optional argument '{name}' to intrinsic "
                        f"'{routine.name}' must be of type "
                        f"'{ routine.optional_args[name].__name__}' but got "
                        f"'{type(arg[1]).__name__}'")
            else:
                if last_named_arg:
                    raise ValueError(
                        f"Found a positional argument *after* a named "
                        f"argument ('{last_named_arg}'). This is invalid.'")
                if not isinstance(arg, routine.required_args.types):
                    raise TypeError(
                        f"The '{routine.name}' intrinsic requires that "
                        f"positional arguments be of type "
                        f"'{routine.required_args.types}' "
                        f"but got a '{type(arg).__name__}'")
                pos_arg_count += 1

        if ((routine.required_args.max_count is not None and
             pos_arg_count > routine.required_args.max_count)
                or pos_arg_count < routine.required_args.min_count):
            msg = f"The '{routine.name}' intrinsic requires "
            if (routine.required_args.max_count is not None and
                    routine.required_args.max_count > 0):
                msg += (f"between {routine.required_args.min_count} and "
                        f"{routine.required_args.max_count} ")
            else:
                msg += f"at least {routine.required_args.min_count} "
            msg += f"arguments but got {len(arguments)}."
            raise ValueError(msg)

        # Create an intrinsic call and add the arguments
        # afterwards. We can't call the parent create method as it
        # assumes the routine argument is a symbol and therefore tries
        # to create an intrinsic call with this symbol, rather than
        # the intrinsic enum.
        call = IntrinsicCall(routine)
        call._add_args(call, arguments)
        call._intrinsic = routine

        return call


# TODO #658 this can be removed once we have support for determining the
# type of a PSyIR expression.
# Intrinsics that perform a reduction on an array.
REDUCTION_INTRINSICS = [
    IntrinsicCall.Intrinsic.SUM, IntrinsicCall.Intrinsic.MINVAL,
    IntrinsicCall.Intrinsic.MAXVAL]
