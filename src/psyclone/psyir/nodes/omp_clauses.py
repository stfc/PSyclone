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

from enum import Enum
from psyclone.psyir.nodes.clause import Clause, OperandClause
from psyclone.psyir.nodes.literal import Literal
from psyclone.psyir.nodes.reference import Reference


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


class OMPSharedClause(Clause):
    '''
    OpenMP shared clause. This is used to declare variables as shared in an
    OpenMP region
    '''
    _children_valid_format = "[Reference]*"
    _text_name = "SharedClause"

    @property
    def _clause_string(self):
        if len(self.children) > 0:
            return "shared"
        return ""


    @staticmethod
    def _validate_child(position, child):
        '''
        Decides whether a given child and position are valid for this node.
        Any number of Reference nodes are allowed.

        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        return isinstance(child, Reference)


class OMPPrivateClause(Clause):
    '''
    OpenMP private clause. This is used to declare variables as private
    to an OpenMP region.
    '''
    _children_valid_format = "[Reference]*"
    _text_name = "PrivateClause"

    @property
    def _clause_string(self):
        if len(self.children) > 0:
            return "private"
        return ""

    @staticmethod
    def _validate_child(position, child):
        '''
        Decides whether a given child and position are valid for this node.
        Any number of Reference nodes are allowed.

        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        return isinstance(child, Reference)


class OMPFirstprivateClause(Clause):
    '''
    OpenMP firstprivate clause. This is used to declare variables as
    firstprivate to an OpenMP region.
    '''
    _children_valid_format = "[Reference]*"
    _text_name = "FirstprivateClause"

    @property
    def _clause_string(self):
        if len(self.children) > 0:
            return "firstprivate"
        return ""

    @staticmethod
    def _validate_child(position, child):
        '''
        Decides whether a given child and position are valid for this node.
        Any number of Reference nodes are allowed.

        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        return isinstance(child, Reference)


class OMPDefaultClause(Clause):
    '''
    OpenMP Default clause. Used to determine the default declaration for
    variables used in an OpenMP region.
    '''

    class DefaultClauseTypes(Enum):
        '''Enumeration of the different types of OMPDefaultClause supported
        in PSyclone'''
        SHARED=0
        NONE=1
        FIRSTPRIVATE=2

    _children_valid_format = None
    _text_name = "OMPDefaultClause"

    @property
    def _clause_string(self):
        clause_string = "default(" + str(self._clause_type.name).lower() + ")"
        return clause_string


    def __init__(self, clause_type=DefaultClauseTypes.SHARED):
        if not isinstance(clause_type, OMPDefaultClause.DefaultClauseTypes):
            raise TypeError(
                    "OMPDefaultClause expected 'clause_type' argument of type "
                    "OMPDefaultClause.DefaultClauseTypes but found '{0}'"
                    .format(type(clause_type).__name__))
        self._clause_type = clause_type
        super(OMPDefaultClause, self).__init__()

class OMPScheduleClause(Clause):
    '''
    OpenMP Schedule clause used for OMP Do Directives.
    '''
    _children_valid_format = "None"
    _text_name = "OMPScheduleClause"
    _schedule = ""

    @property
    def _clause_string(self):
        return f"schedule({self._schedule})"

    def set_schedule(self, schedule):
        self._schedule = schedule

    def __init__(self, schedule="static"):
        self._schedule = schedule
        super(OMPScheduleClause, self).__init__()

    @property
    def schedule(self):
        return self._schedule

    def __eq__(self, other):
        '''Two OMPScheduleClause are equal if they have the same schedule
        '''
        is_eq = super().__eq__(other)
        is_eq = is_eq and (self.schedule == other.schedule)
        return is_eq

class OMPDependClause(OperandClause):
    '''
    OpenMP Depend clause used for OpenMP Task directives.
    '''
    _children_valid_format = "[Reference]*"
    _text_name = "OMPDependClause"
    _clause_string = "depend"

    class DependClauseTypes(Enum):
        '''Enumeration of the different types of OMPDependClause supported
        in PSyclone'''
        IN=0
        OUT=1
        INOUT=2

    def __init__(self, depend_type=DependClauseTypes.INOUT):
        if not isinstance(depend_type, OMPDependClause.DependClauseTypes):
            raise TypeError(
                    "OMPDependClause expected 'depend_type' argument of type "
                    "OMPDependClause.DependClauseTypes but found '{0}'"
                    .format(type(depend_type).__name__))
        self._operand = depend_type
        super(OMPDependClause, self).__init__()

    @staticmethod
    def _validate_child(position, child):
        '''
         Decides whether a given child and position are valid for this node.
         Any number of children allowed, but must be Reference.

        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        return isinstance(child, Reference)

    @property
    def operand(self):
        return str(self._operand.name).lower()

    def __eq__(self, other):
        '''Two OMPDependClause are equal if:
        1. Same type (OMPDependClause).
        2. Same Operand
        3. Same number of children.
        4. Their children are equal.
        '''
        is_eq = super().__eq__(other)
        is_eq = is_eq and (self.operand == other.operand)
        return is_eq

class OMPReductionClause(OperandClause):
    '''
    OpenMP Reduction clause. Not yet used
    '''
    _children_valid_format = "[Reference]+"
    _text_name = "OMPReductionClause"
    # FIXME Reduction string and operator 
