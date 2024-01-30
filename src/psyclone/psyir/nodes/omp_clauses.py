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
# Authors A. B. G. Chalk, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module contains the implementations of the various OpenMP Clause
nodes.'''

from enum import Enum
from psyclone.psyir.nodes.clause import Clause, OperandClause
from psyclone.psyir.nodes.literal import Literal
from psyclone.psyir.nodes.reference import Reference
from psyclone.psyir.symbols import Symbol


class OMPNowaitClause(Clause):
    '''
    OpenMP nowait clause. Disable the implicit barrier at the end of the
    associated directive.
    '''
    _children_valid_format = None
    _clause_string = "nowait"


class OMPGrainsizeClause(Clause):
    '''
    OpenMP grainsize clause, used by OMPTaskloopDirective. Controls the
    grainsize of the associated directive.
    '''
    _children_valid_format = "Literal"
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
    _clause_string = "nogroup"


class OMPSharedClause(Clause):
    '''
    OpenMP shared clause. This is used to declare variables as shared in an
    OpenMP region.
    '''
    _children_valid_format = "Reference*"

    @property
    def _clause_string(self):
        '''
        :returns: the string that represents this clause in OpenMP (i.e. \
                "shared"). Returns an empty string to avoid generation of \
                code if this clause has no children.
        :rtype: str
        '''
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
    _children_valid_format = "Reference*"

    @staticmethod
    def create(symbols):
        ''' Create a OMPPrivateClause containing a Reference to each of the
        provided symbols as children.

        :param symbols: List of symbols to reference in the private clause.
        :type symbols: List[:py:class:`psyclone.psyir.symbols.Symbol`]

        :returns: A OMPPrivateClause referencing the provided symbols.
        :rtype: py:class:`psyclone.psyir.nodes.OMPPrivateClause`

        :raises TypeError: If the symbols argument is not a List that \
            contains only PSyIR Symbols.

        '''
        if not isinstance(symbols, list):
            raise TypeError(
                f"OMPPrivateClause expected the 'symbols' argument to be a "
                f"list, but found '{type(symbols).__name__}' instead.")
        for symbol in symbols:
            if not isinstance(symbol, Symbol):
                raise TypeError(
                    f"OMPPrivateClause expected all the items in the 'symbols'"
                    f" list to be PSyIR Symbols, but found a "
                    f"'{type(symbol).__name__}'.")

        references = [Reference(symbol) for symbol in symbols]
        return OMPPrivateClause(children=references)

    @property
    def _clause_string(self):
        '''
        :returns: the string that represents this clause in OpenMP (i.e.\
                "private"). Returns an empty string to avoid generation of\
                code if this clause has no children.
        :rtype: str
        '''
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
    _children_valid_format = "Reference*"

    @staticmethod
    def create(symbols):
        ''' Create an OMPFirstprivateClause containing a Reference to each of
        the provided symbols as children.

        :param symbols: List of symbols to reference in the firstprivate \
            clause.
        :type symbols: List[:py:class:`psyclone.psyir.symbols.Symbol`]

        :returns: A OMPFirstprivateClause referencing the provided symbols.
        :rtype: py:class:`psyclone.psyir.nodes.OMPFirstprivateClause`

        :raises TypeError: If the symbols argument is not a List that \
            contains only PSyIR Symbols.

        '''
        if not isinstance(symbols, list):
            raise TypeError(
                f"OMPFirstprivateClause expected the 'symbols' argument to be "
                f"a list, but found '{type(symbols).__name__}' instead.")
        for symbol in symbols:
            if not isinstance(symbol, Symbol):
                raise TypeError(
                    f"OMPFirstprivateClause expected all the items in the "
                    f"'symbols' list to be PSyIR Symbols, but found a "
                    f"'{type(symbol).__name__}'.")

        references = [Reference(symbol) for symbol in symbols]
        return OMPFirstprivateClause(children=references)

    @property
    def _clause_string(self):
        '''
        :returns: the string that represents this clause in OpenMP (i.e.\
                "firstprivate"). Returns an empty string to avoid generation\
                of code if this clause has no children.
        :rtype: str
        '''
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

    :param clause_type: The default data-sharing attribute to be described\
                        by this clause. The default value is\
                        OMPDefaultClause.DefaultClauseTypes.SHARED.
    :type clause_type: \
        :py:class:`psyclone.psyir.nodes.OMPDefaultClause.DefaultClauseTypes`
    :param kwargs: additional keyword arguments provided to the PSyIR node.
    :type kwargs: unwrapped dict.

    :raises TypeError: if the supplied clause_type is not the correct type.
    '''

    class DefaultClauseTypes(Enum):
        '''Enumeration of the different types of OMPDefaultClause supported
        in PSyclone.'''
        SHARED = 0
        NONE = 1
        FIRSTPRIVATE = 2

    _children_valid_format = None

    def __init__(self, clause_type=DefaultClauseTypes.SHARED, **kwargs):
        if not isinstance(clause_type, OMPDefaultClause.DefaultClauseTypes):
            raise TypeError(
                    "OMPDefaultClause expected 'clause_type' argument of type "
                    "OMPDefaultClause.DefaultClauseTypes but found "
                    f"'{type(clause_type).__name__}'")
        self._clause_type = clause_type
        super().__init__(**kwargs)

    @property
    def clause_type(self):
        '''
        Gets the clause type value of this OMPDefaultClause

        :returns: the clause type for this OMPDefaultClause.
        :rtype: \
            :py:class:`psyclone.psyir.nodes.OMPDefaultClause.\
                       DefaultClauseTypes`
        '''
        return self._clause_type

    @property
    def _clause_string(self):
        '''
        :returns: the string that represents this clause in OpenMP (e.g.\
                "default(shared)"). The value in parentheses is dependent on\
                the DefaultClauseTypes value in _clause_type.
        :rtype: str
        '''
        clause_string = "default(" + str(self._clause_type.name).lower() + ")"
        return clause_string

    def node_str(self, colour=True):
        '''
        :param bool colour: whether or not to include control codes for \
                            coloured text.

        :returns: a text description of this node.
        :rtype: str
        '''
        return self.coloured_name(colour) + f"[default={self._clause_type}]"


class OMPScheduleClause(Clause):
    '''
    OpenMP Schedule clause used for OMP Do Directives.

    :param str schedule: The OpenMP schedule to use with this directive. \
        The default value is "none" which means that no explicit schedule \
        is specified.
    :param kwargs: additional keyword arguments provided to the PSyIR node.
    :type kwargs: unwrapped dict.

    '''
    _children_valid_format = "None"

    VALID_OMP_SCHEDULES = ["runtime", "static", "dynamic", "guided", "auto",
                           "none"]

    def __init__(self, schedule="none", **kwargs):
        self.schedule = schedule
        super().__init__(**kwargs)

    @property
    def _clause_string(self):
        '''
        :returns: the string that represents this clause in OpenMP (e.g. \
            "schedule(static)"). The value inside parentheses is \
            set to the value of the schedule of this clause unless \
            that value is 'none' in which case an empty string is returned.
        :rtype: str
        '''
        if self._schedule != "none":
            return f"schedule({self._schedule})"
        return ""

    @property
    def schedule(self):
        '''
        :returns: the schedule for this OMPScheduleClause.
        :rtype: str
        '''
        return self._schedule

    @schedule.setter
    def schedule(self, schedule):
        '''
        :param str schedule: the schedule to use for this clause.

        :raises ValueError: if the supplied value is not a recognised \
                            OpenMP schedule.
        '''
        if schedule not in self.VALID_OMP_SCHEDULES:
            raise ValueError(
                f"Schedule must be one of {self.VALID_OMP_SCHEDULES}. "
                f"Found '{schedule}'.")
        self._schedule = schedule

    def __eq__(self, other):
        '''
        Two OMPScheduleClause are equal if they have the same schedule.

        :param object other: the object to check equality to.

        :returns: whether other is equal to self.
        :rtype: bool
        '''
        is_eq = super().__eq__(other)
        is_eq = is_eq and (self.schedule == other.schedule)
        return is_eq

    def node_str(self, colour=True):
        '''
        :param bool colour: whether or not to include control codes for \
                            coloured text.

        :returns: a text description of this node.
        :rtype: str
        '''
        return self.coloured_name(colour) + f"[schedule={self._schedule}]"


class OMPDependClause(OperandClause):
    '''
    OpenMP Depend clause used for OpenMP Task directives.

    :param depend_type: The dependency type to use for this clause. The \
                        default value is \
                        OMPDependClause.DependClauseTypes.INOUT.
    :type depend_type: \
            :py:class:`psyclone.psyir.nodes.OMPDependClause.DependClauseTypes`
    :param kwargs: additional keyword arguments provided to the PSyIR node.
    :type kwargs: unwrapped dict.

    :raises TypeError: if the supplied depend_type argument is the wrong type.
    '''
    _children_valid_format = "Reference*"
    _clause_string = "depend"

    class DependClauseTypes(Enum):
        '''Enumeration of the different types of OMPDependClause supported
        in PSyclone.'''
        IN = 0
        OUT = 1
        INOUT = 2

    def __init__(self, depend_type=DependClauseTypes.INOUT, **kwargs):
        if not isinstance(depend_type, OMPDependClause.DependClauseTypes):
            raise TypeError(
                    "OMPDependClause expected 'depend_type' argument of type "
                    "OMPDependClause.DependClauseTypes but found "
                    f"'{type(depend_type).__name__}'")
        self._operand = depend_type
        super().__init__(**kwargs)

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
        '''
        :returns: the string representation of the operand of this clause.
        :rtype: str
        '''
        return str(self._operand.name).lower()

    def __eq__(self, other):
        '''Two OMPDependClause are equal if:
        1. Same type (OMPDependClause).
        2. Same Operand
        3. Same number of children.
        4. Their children are equal.

        :param object other: the object to check equality to.

        :returns: whether other is equal to self.
        :rtype: bool
        '''
        is_eq = super().__eq__(other)
        is_eq = is_eq and (self.operand == other.operand)
        return is_eq

    def node_str(self, colour=True):
        '''
        :param bool colour: whether or not to include control codes for \
                            coloured text.

        :returns: a text description of this node.
        :rtype: str
        '''
        return self.coloured_name(colour) + f"[operand={self._operand}]"


class OMPReductionClause(OperandClause):
    '''
    OpenMP Reduction clause.
    '''
    _children_valid_format = "Reference+"
    # TODO: #1812 Reduction string and operator
