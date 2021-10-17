# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
#         I. Kavcic,    Met Office
#         C.M. Maynard, Met Office / University of Reading
#         J. Henrichs, Bureau of Meteorology
# Modified A. B. G. Chalk, STFC Daresbury Lab.
# -----------------------------------------------------------------------------

''' This module contains the Directive, RegionDirective, StandaloneDirective
    node implementation.'''

from __future__ import absolute_import
import abc
import six
from psyclone.psyir.nodes.statement import Statement
from psyclone.psyir.nodes.schedule import Schedule
from psyclone.errors import InternalError


@six.add_metaclass(abc.ABCMeta)
class Directive(Statement):
    '''
    Abstract base class for all Directive statements.

    '''
    # The prefix to use when code-generating this directive
    # (e.g. "OMP") must be set by a mixin or sub-class.
    _PREFIX = ""
    _colour = "green"


class RegionDirective(Directive):
    '''
    Base class for all Directive nodes that have an associated
    region of code with them.

    All classes that generate RegionDirective statements (e.g. OpenMP,
    OpenACC, compiler-specific) inherit from this class.

    :param ast: the entry in the fparser2 parse tree representing the code \
                contained within this directive or None.
    :type ast: :py:class:`fparser.two.Fortran2003.Base` or NoneType
    :param children: list of PSyIR nodes that will be children of this \
                     Directive node or None.
    :type children: list of :py:class:`psyclone.psyir.nodes.Node` or NoneType
    :param parent: PSyIR node that is the parent of this Directive or None.
    :type parent: :py:class:`psyclone.psyir.nodes.Node` or NoneType

    '''
    # Textual description of the node.
    _children_valid_format = "Schedule"

    def __init__(self, ast=None, children=None, parent=None):
        # A Directive always contains a Schedule
        sched = Schedule(children=children, parent=self)
        super(RegionDirective, self).__init__(ast, children=[sched],
                                              parent=parent)

    @staticmethod
    def _validate_child(position, child):
        '''
        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        return position == 0 and isinstance(child, Schedule)

    @property
    def dir_body(self):
        '''
        :returns: the Schedule associated with this directive.
        :rtype: :py:class:`psyclone.psyir.nodes.Schedule`

        :raises InternalError: if this node does not have a single Schedule as\
                               its child.
        '''
        if len(self.children) != 1 or not \
           isinstance(self.children[0], Schedule):
            raise InternalError(
                "Directive malformed or incomplete. It should have a single "
                "Schedule as a child but found: {0}".format(
                    [type(child).__name__ for child in self.children]))
        return self.children[0]


class StandaloneDirective(Directive):
    '''
    Base class for all StandaloneDirective statements. This class is
    designed for directives which do not have code associated with
    them, e.g. OpenMP's taskwait.

    All classes that generate StandaloneDirective statements
    (e.g. OpenMP, OpenACC, compiler-specific) inherit from this class.

    '''
    # Textual description of the node.
    _children_valid_format = None

    @staticmethod
    def _validate_child(position, child):
        '''
        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        # Children are not allowed for StandaloneDirective
        return False


# For automatic API documentation generation
__all__ = ["Directive", "RegionDirective", "StandaloneDirective"]
