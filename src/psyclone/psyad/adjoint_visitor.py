# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2022, Science and Technology Facilities Council.
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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab

'''A PSyIR visitor for PSyAD : the PSyclone Adjoint support. Applies
transformations to tangent-linear PSyIR to return its PSyIR adjoint.

'''
from __future__ import print_function
import logging

from fparser.two import Fortran2003
from psyclone.psyad.transformations import AssignmentTrans
from psyclone.psyad.utils import node_is_passive, node_is_active, negate_expr
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.backend.language_writer import LanguageWriter
from psyclone.psyir.backend.visitor import PSyIRVisitor, VisitorError
from psyclone.psyir.nodes import (Routine, Schedule, Reference, Node, Literal,
                                  CodeBlock, BinaryOperation, Assignment,
                                  Container)
from psyclone.psyir.symbols import ArgumentInterface
from psyclone.psyir.tools import DependencyTools


class AdjointVisitor(PSyIRVisitor):
    '''An Adjoint Visitor that translates the PSyIR of a tangent-linear
    code into its adjoint form.

    :param active_variable_names: a list of the active variables.
    :type active_variable_names: list of str
    :param writer: the writer to use when outputting PSyIR in error or \
        logging messages. Defaults to FortranWriter.
    :type writer: \
        :py:class:`psyclone.psyir.backend.language_writer.LanguageWriter`

    :raises ValueError: if no active variables are supplied.

    '''
    def __init__(self, active_variable_names, writer=FortranWriter()):
        super(AdjointVisitor, self).__init__()
        if not active_variable_names:
            raise ValueError(
                "There should be at least one active variable supplied to "
                "an AdjointVisitor.")
        if not isinstance(writer, LanguageWriter):
            raise TypeError(
                "The writer argument should be a subclass of LanguageWriter "
                "but found '{0}'.".format(type(writer).__name__))
        self._active_variable_names = active_variable_names
        self._active_variables = None
        self._logger = logging.getLogger(__name__)
        self._writer = writer

    def container_node(self, node):
        '''This method is called if the visitor finds a Container node. A copy
        of the container is returned (as this does not change when
        converting from tangent linear to adjoint) containing
        processed descendants.

        :param node: a Container PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Container`

        :returns: a new PSyIR tree containing the adjoint of this node \
            and its descendant nodes.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        '''
        self._logger.debug("Copying Container")
        return self._copy_and_process(node)

    def schedule_node(self, node):
        '''This method is called if the visitor finds a Schedule node. A copy
        of the schedule is returned, as this does not change when
        converting from tangent linear to adjoint and its children are
        re-ordered and sorted dependending on whether they are active
        or passive nodes.

        As a schedule contains variable scoping information, i.e. a
        symbol table, the symbols representing the active variable
        names supplied to the visitor are found and added to an
        internal list so they are available when processing any
        descendants.

        :param node: a Schedule PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Schedule`

        :returns: a new PSyIR tree containing the adjoint equivalent \
            of this node and its descendants.
        :rtype: :py:class:`psyclone.psyir.nodes.Schedule`

        '''
        self._logger.debug("Transforming Schedule")

        # A schedule has a scope so determine and store active variables
        symbol_table = node.scope.symbol_table
        self._active_variables = []
        for variable_name in self._active_variable_names:
            self._active_variables.append(symbol_table.lookup(variable_name))

        # We only need to copy this node. Issue #1440 will address
        # this.
        node_copy = node.copy()
        node_copy.children = []

        if isinstance(node, Routine):
            # Zero local active variables.
            self._logger.debug("Zero-ing any local active variables")
            for active_variable in self._active_variables:
                if active_variable.is_local:
                    if not (active_variable.is_scalar or
                            active_variable.is_array):
                        # Issue #1627 structures are not allowed.
                        raise NotImplementedError(
                            f"Active local variables can only be scalars and "
                            f"arrays, but found '{active_variable}'.")
                    datatype = active_variable.datatype.intrinsic.name
                    if datatype == "REAL":
                        value = "0.0"
                    elif datatype == "INTEGER":
                        value = "0"
                    else:
                        raise NotImplementedError(
                            f"Datatype '{datatype}' is not supported (for "
                            f"active local variable "
                            f"'{active_variable.name}'). Supported types are "
                            f"'REAL' and 'INTEGER'.")
                    node_copy.children.append(
                        Assignment.create(
                            Reference(active_variable),
                            Literal(value, active_variable.datatype)))

        # Split active and passive nodes.
        self._logger.debug("Adding passive code into new schedule")
        active_nodes = []
        for child in node.children:
            if node_is_passive(child, self._active_variables):
                # Add passive nodes back into the schedule as they do
                # not change.
                node_copy.children.append(child.copy())
            else:
                # Store active nodes for further processing.
                active_nodes.append(child)

        # Reverse active nodes.
        self._logger.debug("Reversing order of active code")
        active_nodes.reverse()

        # Process active nodes.
        self._logger.debug(
            "Processing active code and adding results into new schedule")
        for child in active_nodes:
            result = self._visit(child)
            if isinstance(result, list):
                node_copy.children.extend(result)
            else:
                node_copy.children.append(result)

        # Creating the adjoint may have altered the way variables are
        # accessed within the code. If any of the active variables are
        # subroutine arguments then we must update the access property of
        # the associated ArgumentInterface.

        # Since a piece of code could contain many Schedules, ensure we are
        # currently handling the one representing the routine.
        if isinstance(node, Routine):
            dtools = DependencyTools()
            # Input signatures ('in_sigs') are those whose first access is a
            # read.
            # Output signatures ('out_sigs') are those that are written to at
            # some point.
            in_sigs, out_sigs = dtools.get_in_out_parameters(
                node_copy.children)
            # Get the variable name associated with each of these signatures.
            in_names = [sig.var_name for sig in in_sigs]
            out_names = [sig.var_name for sig in out_sigs]

            # We must update the symbols in the table of the new tree
            adj_table = node_copy.symbol_table

            for vname in self._active_variable_names:
                sym = adj_table.lookup(vname)
                if not sym.is_argument:
                    continue
                # Ensure that the interface we modify is private to this
                # symbol.
                # TODO #1544 how do we ensure that an interface is only
                # referred to by one symbol?
                new_interface = sym.interface.copy()
                sym.interface = new_interface
                if vname in in_names:
                    if vname in out_names:
                        sym.interface.access = \
                            ArgumentInterface.Access.READWRITE
                    else:
                        sym.interface.access = ArgumentInterface.Access.READ
                else:
                    sym.interface.access = ArgumentInterface.Access.WRITE

        return node_copy

    def assignment_node(self, node):
        '''This method is called if the visitor finds an Assignment node. The
        adjoint of this tangent-linear assignment is returned via the
        AssignmentTrans transformation. As the adjoint of a single
        tangent-linear assignment can consist of multiple assignments,
        a list of nodes is returned.

        :param node: an Assignment PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Assignment`

        :returns: a list of PSyIR nodes containing the adjoint \
            of this node.
        :rtype: list of :py:class:`psyclone.psyir.nodes.Node`

        :raises VisitorError: if the schedule_node method has not been \
            called previously.

        '''
        self._logger.debug("Transforming active assignment")
        if self._active_variables is None:
            raise VisitorError(
                "An assignment node should not be visited before a schedule, "
                "as the latter sets up the active variables.")
        assign_trans = AssignmentTrans(self._active_variables)
        new_node = node.copy()
        # Temporary parent schedule required by the transformation.
        dummy_schedule = Schedule()
        dummy_schedule.children.append(new_node)
        assign_trans.apply(new_node)
        return dummy_schedule.pop_all_children()

    def loop_node(self, node):
        '''This method is called if the visitor finds a Loop node. If the loop
        (including any descendants) contains active variables then a
        new loop is returned which iterates in the reverse order of
        the original loop and the body of the new loop is the result
        of processing the body of the original loop. If the loop does
        not contain any active variables then an exception is raised
        as this case should have been dealt with by the
        schedule_node() method.

        :param node: a Loop PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Loop`

        :returns: a new PSyIR tree containing the adjoint equivalent \
            of this node and its descendants.
        :rtype: :py:class:`psyclone.psyir.nodes.Loop`

        '''
        if self._active_variables is None:
            raise VisitorError(
                "A loop node should not be visited before a schedule, "
                "as the latter sets up the active variables.")

        # Check that variables in loop bounds and the iterator are
        # passive, unless they are part of the 1st argument to an
        # LBOUND or UBOUND function, as this is used to determine
        # the size of the array, not modify its content.
        for expr, description in [(node.start_expr, "lower bound"),
                                  (node.stop_expr, "upper bound"),
                                  (node.step_expr, "step")]:
            for ref in expr.walk(Reference):
                if ref.symbol in self._active_variables:
                    # Ignore LBOUND and UBOUND
                    if not (isinstance(ref.parent, BinaryOperation) and
                            ref.position == 0 and
                            ref.parent.operator in [
                                BinaryOperation.Operator.LBOUND,
                                BinaryOperation.Operator.UBOUND]):
                        raise VisitorError(
                            f"The {description} of a loop should not contain "
                            f"active variables, but found '{ref.name}' in "
                            f"'{self._writer(expr)}'.")

        if node_is_active(Reference(node.variable), self._active_variables):
            raise VisitorError(
                "The loop iterator '{0}' should not be an active "
                "variable.".format(node.variable.name))

        if node_is_passive(node, self._active_variables):
            raise VisitorError(
                "A passive loop node should not be processed by the "
                "loop_node() method within the AdjointVisitor() class, as "
                "it should have been dealt with by the schedule_node() "
                "method.")

        self._logger.debug("Transforming active loop")

        # The approach taken is to swap the loop bounds and multiply
        # the step by minus one. However, the loop step might not
        # align with the loop stop and in this case an offset needs to
        # be computed e.g. 1 to 4 step 2 gives 1,3, but 4 to 1 step -2
        # gives 4,2, which is not correct. In this example the offset
        # (hi-lo mod step) is 1, so we will have (4-1) to 1 step -2,
        # giving the expected 3,1.

        # Default to no offset required for the loop starting point
        # (on the assumption that the step is unitary, which it is in
        # most cases).
        offset = None
        if not(isinstance(node.step_expr, Literal) and
               node.step_expr.value.strip() in ["1", "-1"]):
            # The loop step might not be unitary so compute an offset:
            # stop-start mod step
            fortran_writer = FortranWriter()
            hi_str = fortran_writer(node.stop_expr)
            lo_str = fortran_writer(node.start_expr)
            step_str = fortran_writer(node.step_expr)
            # TODO: use language independent PSyIR, see issue #1345
            ptree = Fortran2003.Intrinsic_Function_Reference(
                "mod({0}-{1},{2})".format(hi_str, lo_str, step_str))
            offset = CodeBlock([ptree], CodeBlock.Structure.EXPRESSION)

        # We only need to copy this node and its bounds. Issue #1440
        # will address this.
        new_node = node.copy()

        # Reverse loop order
        start_expr = new_node.start_expr.copy()
        if offset:
            new_node.start_expr = BinaryOperation.create(
                BinaryOperation.Operator.SUB, new_node.stop_expr.copy(),
                offset)
        else:
            new_node.start_expr = new_node.stop_expr.copy()
        new_node.stop_expr = start_expr
        new_node.step_expr = negate_expr(new_node.step_expr.copy())

        # Determine the adjoint of the loop body
        new_node.children[3] = self._visit(node.children[3])
        return new_node

    def _copy_and_process(self, node):
        '''Utility function to return a copy the current node containing the
        result of processing all descendants.

        :param node: a PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Node`

        :returns: a new PSyIR tree containing a copy of this node \
            which contains the result of processing all of its \
            descendants.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        '''
        # We only need to copy this node. Issue #1440 will address
        # this.
        node_copy = node.copy()
        node_copy.children = []
        for child in node.children:
            result = self._visit(child)
            # Result may be a node or a list of nodes
            if isinstance(result, Node):
                result = [result]
            node_copy.children.extend(result)
        return node_copy


# =============================================================================
# Documentation utils: The list of module members that we wish AutoAPI to
# generate documentation for (see https://psyclone-ref.readthedocs.io).
__all__ = ["AdjointVisitor"]
