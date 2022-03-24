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
# Authors A. B. G. Chalk, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module contains the implementations of the various OpenMP Directive
nodes.'''

from psyclone.psyir.nodes.clause import Clause
from psyclone.psyir.nodes.literal import Literal


class OMPNowaitClause(Clause):
    '''
    OpenMP nowait clause. Disable the implicit barrier at the end of the
    associated directive.
    '''
    _children_valid_format = None
    _text_name = "NowaitClause"
    _clause_string = "nowait"


class OMPGrainsizeClause(Clause):
    '''
    OpenMP grainsize clause, used by OMPTaskloopDirective. Controls the
    grainsize of the associated directive.
    '''
    _children_valid_format = "Literal"
    _text_name = "GrainsizeClause"
    _clause_string = "grainsize"

    @staticmethod
    def _validate_child(position, child):
        '''
         Decides whether a given child and position are valid for this node.
         One child allowed, of type Literal.

        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        if position == 0:
            return isinstance(child, Literal)
        return False


class OMPNumTasksClause(Clause):
    '''
    OpenMP numtasks clause, used by OMPTaskloopDirective. Controls the number
    of tasks created by OpenMP for the associated directive.
    '''
    _children_valid_format = "Literal"
    _text_name = "NumTasksClause"
    _clause_string = "num_tasks"

    @staticmethod
    def _validate_child(position, child):
        '''
         Decides whether a given child and position are valid for this node.
         One child allowed, of type Literal.

        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        if position == 0:
            return isinstance(child, Literal)
        return False


class OMPNogroupClause(Clause):
    '''
    OpenMP nogroup clause, used by OMPTaskloopDirective to disable the
    implicit Taskgroup associated with a Taskloop.
    '''
    _children_valid_format = None
    _text_name = "NogroupClause"
    _clause_string = "nogroup"
