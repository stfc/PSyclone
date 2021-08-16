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
# -----------------------------------------------------------------------------

''' This module contains the Directive node implementation.'''

from __future__ import absolute_import
from fparser.common.readfortran import FortranStringReader
from fparser.two.Fortran2003 import Comment
from psyclone.psyir.nodes.statement import Statement
from psyclone.psyir.nodes.schedule import Schedule
from psyclone.psyir.nodes.routine import Routine
from psyclone.psyir.nodes.loop import Loop
from psyclone.psyir.nodes.node import Node
from psyclone.errors import InternalError


class Directive(Statement):
    '''
    Base class for all Directive statements.

    All classes that generate Directive statements (e.g. OpenMP,
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
    # The prefix to use when constructing this directive in Fortran
    # (e.g. "OMP"). Must be set by sub-class.
    _PREFIX = ""
    # Textual description of the node.
    _children_valid_format = "Schedule"
    _text_name = "Directive"
    _colour = "green"

    def __init__(self, ast=None, children=None, parent=None):
        # A Directive always contains a Schedule
        sched = self._insert_schedule(children, ast)
        super(Directive, self).__init__(ast, children=[sched], parent=parent)

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

    @property
    def dag_name(self):
        '''
        :returns: the name to use in the DAG for this node.
        :rtype: str
        '''
        _, position = self._find_position(self.ancestor(Routine))
        return "directive_" + str(position)

    def _add_region(self, start_text, end_text=None, data_movement=None):
        '''
        Modifies the underlying fparser2 parse tree to include a subset
        of nodes within a region. (e.g. a 'kernels' or 'data' region.)

        :param str start_text: the directive body to insert at the \
                               beginning of the region. "!$"+self._PREFIX+" " \
                               is prepended to the supplied text.
        :param str end_text: the directive body to insert at the end of \
                             the region (or None). "!$"+self._PREFIX+" " is \
                             prepended to the supplied text.
        :param str data_movement: whether to include data-movement clauses and\
                               if so, whether to determine them by analysing \
                               the code within the region ("analyse") or to \
                               specify 'default(present)' ("present").

        :raises InternalError: if either start_text or end_text already
                               begin with '!'.
        :raises InternalError: if data_movement is not None and not one of \
                               "present" or "analyse".
        :raises InternalError: if data_movement=="analyse" and this is an \
                               OpenMP directive.
        '''
        from psyclone.psyir.frontend.fparser2 import Fparser2Reader
        from psyclone.psyir.nodes.acc_directives import ACCDirective
        from psyclone.psyGen import object_index
        valid_data_movement = ["present", "analyse"]

        # Ensure the fparser2 AST is up-to-date for all of our children
        Node.update(self)

        # Check that we haven't already been called
        if self.ast:
            return

        # Sanity check the supplied begin/end text
        if start_text.lstrip()[0] == "!":
            raise InternalError(
                "_add_region: start_text must be a plain label without "
                "directive or comment characters but got: '{0}'".
                format(start_text))
        if end_text and end_text.lstrip()[0] == "!":
            raise InternalError(
                "_add_region: end_text must be a plain label without directive"
                " or comment characters but got: '{0}'".format(end_text))
        # We only deal with data movement if this is an OpenACC directive
        if data_movement and data_movement == "analyse" and \
           not isinstance(self, ACCDirective):
            raise InternalError(
                "_add_region: the data_movement='analyse' option is only valid"
                " for an OpenACC directive.")

        # Find a reference to the fparser2 parse tree that belongs to
        # the contents of this region. Then go back up one level in the
        # parse tree to find the node to which we will add directives as
        # children. (We do this because our parent PSyIR node may be a
        # directive which has no associated entry in the fparser2 parse tree.)
        first_child = self.children[0][0]
        last_child = self.children[0][-1]
        content_ast = first_child.ast
        fp_parent = content_ast.parent

        try:
            # Find the location of the AST of our first child node in the
            # list of child nodes of our parent in the fparser parse tree.
            ast_start_index = object_index(fp_parent.content,
                                           content_ast)
            if end_text:
                if last_child.ast_end:
                    ast_end_index = object_index(fp_parent.content,
                                                 last_child.ast_end)
                else:
                    ast_end_index = object_index(fp_parent.content,
                                                 last_child.ast)

                text = "!$" + self._PREFIX + " " + end_text
                directive = Comment(FortranStringReader(text,
                                                        ignore_comments=False))
                directive.parent = fp_parent
                fp_parent.content.insert(ast_end_index+1, directive)
                # Ensure this end directive is included with the set of
                # statements belonging to this PSyIR node.
                self.ast_end = directive
                self.dir_body.ast_end = directive
        except (IndexError, ValueError):
            raise InternalError("Failed to find locations to insert "
                                "begin/end directives.")

        text = "!$" + self._PREFIX + " " + start_text

        if data_movement:
            if data_movement == "analyse":
                # Identify the inputs and outputs to the region (variables that
                # are read and written).
                processor = Fparser2Reader()
                readers, writers, readwrites = processor.get_inputs_outputs(
                    fp_parent.content[ast_start_index:ast_end_index+1])

                if readers:
                    text += " COPYIN({0})".format(",".join(readers))
                if writers:
                    text += " COPYOUT({0})".format(",".join(writers))
                if readwrites:
                    text += " COPY({0})".format(",".join(readwrites))

            elif data_movement == "present":
                text += " DEFAULT(PRESENT)"
            else:
                raise InternalError(
                    "_add_region: the optional data_movement argument must be "
                    "one of {0} but got '{1}'".format(valid_data_movement,
                                                      data_movement))
        directive = Comment(FortranStringReader(text,
                                                ignore_comments=False))
        directive.parent = fp_parent
        fp_parent.content.insert(ast_start_index, directive)

        self.ast = directive
        self.dir_body.ast = directive
        # If this is a directive applied to a Loop then update the ast_end
        # for this Node to point to the parse tree for the loop. We have to
        # do this because the loop is a sibling (rather than a child) of the
        # directive in the parse tree.
        if not end_text and isinstance(first_child, Loop):
            self.ast_end = fp_parent.content[ast_start_index+1]


class ChildlessDirective(Statement):
    '''
    Base class for all ChildlessDirective statements. This class is
    designed for directives which do not have code associated with
    them, e.g. OpenMP's taskwait.

    All classes that generate ChildlessDirective statements
    (e.g. OpenMP, OpenACC, compiler-specific) inherit from this class.

    :param ast: None.
    :type ast: NoneType
    :param children: None.
    :type children: NoneType
    :param parent: PSyIR node that is the parent of this Directive or None.
    :type parent: :py:class:`psyclone.psyir.nodes.Node` or NoneType

    '''
    # The prefix to use when constructing this directive in Fortran
    # (e.g. "OMP"). Must be set by sub-class.
    _PREFIX = ""
    # Textual description of the node.
    _children_valid_format = None
    _text_name = "ChildlessDirective"
    _colour = "green"

    def __init__(self, ast=None, children=None, parent=None):
        super(ChildlessDirective, self).__init__(ast, children=children, parent=parent)

    @staticmethod
    def _validate_child(position, child):
        '''
        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        # Children are not allowed for ChildlessDirective
        return False

    @property
    def dag_name(self):
        '''
        :returns: the name to use in the DAG for this node.
        :rtype: str
        '''
        _, position = self._find_position(self.ancestor(Routine))
        return "childless_directive_" + str(position)

# For automatic API documentation generation
__all__ = ["Directive", "ChildlessDirective"]
