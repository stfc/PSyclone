# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council.
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

''' This module contains the implementations of the various OpenACC Directive
Clause nodes.'''

from psyclone.psyir.nodes.clause import Clause
from psyclone.psyir.nodes.reference import Reference


class ACCCopyClause(Clause):
    '''
    OpenACC copy clause. Specifies a list of variables that are to be copied
    to the device at the start of the associated region and back to the host
    at the end.

    '''
    _children_valid_format = "Reference"
    _clause_string = "copy"

    @staticmethod
    def _validate_child(position, child):
        '''
        Decides whether a given child and position are valid for this node.
        Any number of children are allowed, all of type Reference.

        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        return isinstance(child, Reference)


class ACCCopyInClause(Clause):
    '''
    OpenACC copy clause. Specifies a list of variables that are to be copied
    to the device at the start of a region.

    '''
    _children_valid_format = "Reference"
    _clause_string = "copyin"

    @staticmethod
    def _validate_child(position, child):
        '''
        Decides whether a given child and position are valid for this node.
        Any number of children are allowed, all of type Reference.

        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        return isinstance(child, Reference)


class ACCCopyOutClause(Clause):
    '''
    OpenACC copy clause. Specifies a list of variables that are to be copied
    from the device to the host at the end of the associated region.

    '''
    _children_valid_format = "Reference"
    _clause_string = "copyout"

    @staticmethod
    def _validate_child(position, child):
        '''
        Decides whether a given child and position are valid for this node.
        Any number of children are allowed, all of type Reference.

        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        return isinstance(child, Reference)


__all__ = ["ACCCopyClause", "ACCCopyInClause", "ACCCopyOutClause"]
