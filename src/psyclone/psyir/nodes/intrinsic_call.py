# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council.
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
# -----------------------------------------------------------------------------

''' This module contains the IntrinsicCall node implementation.'''

from enum import Enum

from psyclone.errors import GenerationError
from psyclone.psyir.nodes.array_reference import ArrayReference
from psyclone.psyir.nodes.call import Call
from psyclone.psyir.nodes.reference import Reference
from psyclone.psyir.symbols import IntrinsicSymbol


class IntrinsicCall(Call):
    ''' Node representing a call to an intrinsic routine (function or
    subroutine). This can be found as a standalone statement
    or an expression.

    '''
    # Textual description of the node.
    _children_valid_format = "[DataNode]*"
    _text_name = "IntrinsicCall"
    _colour = "cyan"
    _min_arg_count = 0
    _max_arg_count = 0

    #: The intrinsics that can be represented by this node.
    Intrinsic = Enum('Intrinsic', [
        'ALLOCATE', 'DEALLOCATE', 'RANDOM'
        ])

    #: List of required arguments, indexed by intrinsic name.
    _required_args = {}
    #: Dict of optional arguments, indexed by intrinsic name.
    _optional_args = {}
    _required_args[Intrinsic.ALLOCATE.name] = [ArrayReference]
    _optional_args[Intrinsic.ALLOCATE.name] = {"mold": Reference,
                                               "status": Reference}
    _required_args[Intrinsic.DEALLOCATE.name] = [Reference]
    _optional_args[Intrinsic.DEALLOCATE.name] = {}
    _required_args[Intrinsic.RANDOM.name] = [Reference]
    _optional_args[Intrinsic.RANDOM.name] = {}

    @classmethod
    def create(cls, intrinsic, arguments):
        '''Create an instance of class cls given valid instances of a routine
        symbol, and a list of child nodes (or name and node tuple) for
        its arguments.

        :param intrinsic: the Intrinsic being called.
        :type intrinsic: py:class:`psyclone.psyir.IntrinsicCall.Intrinsic`
        :param arguments: the arguments to this routine, and/or \
            2-tuples containing an argument name and the \
            argument. Arguments are added as child nodes.
        :type arguments: List[ \
            Union[:py:class:``psyclone.psyir.nodes.DataNode``, \
                  Tuple[str, :py:class:``psyclone.psyir.nodes.DataNode``]]]

        :returns: an instance of cls.
        :rtype: :py:class:`psyclone.psyir.nodes.IntrinsicCall`

        :raises TypeError: if the intrinsic argument is not an \
                           IntrinsicCall.Intrinsic.
        :raises TypeErrpr: if the arguments argument is not a list.
        :raises GenerationError: if the contents of the arguments \
            argument are not the expected type.

        '''
        if not isinstance(intrinsic, IntrinsicCall.Intrinsic):
            raise TypeError(
                f"IntrinsicCall create() 'intrinsic' argument should be an "
                f"instance of IntrinsicCall.Intrinsic but found "
                f"'{type(intrinsic).__name__}'.")

        if not isinstance(arguments, list):
            raise TypeError(
                f"IntrinsicCall.create() 'arguments' argument should be a "
                f"list but found '{type(arguments).__name__}'")

        # Create a call, supplying an IntrinsicSymbol in place of a
        # RoutineSymbol.
        call = super().create(IntrinsicSymbol(intrinsic.name), arguments)

        reqd_args = cls._required_args[intrinsic.name]
        if len(arguments) < len(reqd_args):
            raise GenerationError(
                f"The '{intrinsic.name}' intrinsic requires {len(reqd_args)} "
                f"arguments but got {len(arguments)}.")

        if cls._optional_args[intrinsic.name]:
            optional_arg_names = sorted(list(
                cls._optional_args[intrinsic.name].keys()))
        else:
            optional_arg_names = []

        # Validate the supplied arguments.
        arg_pos = 0
        for idx, arg in enumerate(call.children):
            name = call._argument_names[idx][1]
            if name:
                if name not in optional_arg_names:
                    raise GenerationError(
                        f"The '{intrinsic.name}' intrinsic only supports "
                        f"the optional arguments {optional_arg_names} but "
                        f"got '{name}'.")
                if type(arg) != cls._optional_args[intrinsic.name][name]:
                    raise TypeError("arp2")
            else:
                if type(arg) != reqd_args[arg_pos]:
                    raise GenerationError(
                        f"The '{intrinsic.name}' intrinsic requires an "
                        f"argument of type '{reqd_args[arg_pos].__name__}' at "
                        f"position {arg_pos} but got a '{type(arg).__name__}'")
                arg_pos += 1

        return call
