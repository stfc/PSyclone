from __future__ import print_function
import logging

from psyclone.psyad.transformations import AssignmentTrans
from psyclone.psyir.backend.visitor import PSyIRVisitor, VisitorError
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import Schedule, Reference, UnaryOperation, \
    BinaryOperation, Literal
from psyclone.psyir.symbols import REAL_TYPE


def active(node, active_variables):
    ''' Determines whether this node contains variables that are active.

    :param node: the PSyIR node that is being evaluated.
    :type node: :py:class:`psyclone.psyir.nodes.Node`
    :param active_variables: a list of active variables.
    :type active_variables: :py:class:`psyclone.psyir.symbols.DataSymbol`

    :returns: True if active and False otherwise.
    :rtype: bool

    '''
    for reference in node.walk(Reference):
        if reference.symbol in active_variables:
            return True
    return False


def passive(node, active_variables):
    '''Determines whether this node contains only variables that are
    passive.

    :param node: the PSyIR node that is being evaluated.
    :type node: :py:class:`psyclone.psyir.nodes.Node`
    :param active_variables: a list of active variables.
    :type active_variables: :py:class:`psyclone.psyir.symbols.DataSymbol`

    :returns: True if passive and False otherwise.
    :rtype: bool

    '''
    return not active(node, active_variables)


def negate(expr):
    ''' xxx '''

    if isinstance(expr, Literal):
        return UnaryOperation.create(UnaryOperation.Operator.MINUS, expr)
    return BinaryOperation.create(
        BinaryOperation.Operator.MUL, Literal("-1", INTEGER_TYPE), expr)


class AdjointVisitor(PSyIRVisitor):
    '''An Adjoint Visitor that translates a PSyIR tree representation of a
    tangent linear code to its adjoint.

    :param active_variables_names: a list of the active variables.
    :type active_variables_names: list of str

    :raises TypeError: if not active variables are supplied.

    '''

    def __init__(self, active_variable_names):
        super(AdjointVisitor, self).__init__()
        if not active_variable_names:
            raise TypeError("There should be at least one active variable supplied.")
        self._active_variable_names = active_variable_names
        self._active_variables = None
        self._logger = logging.getLogger(__name__)

    def filecontainer_node(self, node):
        '''This method is called if the visitor finds a FileContainer node.

        :param node: a FileContainer PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.FileContainer`

        :returns: a new PSyIR tree containing the adjoint equivalent \
            of this node and its children nodes.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        '''
        self._logger.debug("Copying FileContainer")
        node_copy = node.copy()
        node_copy.children = []
        for child in node.children:
            result = self._visit(child)
            node_copy.children.append(result)
        return node_copy

    def schedule_node(self, node):
        '''This method is called if the visitor finds a Schedule node.'''

        self._logger.debug("Transforming Schedule")
        # A schedule has a scope so determine and store active variables
        symbol_table = node.scope.symbol_table
        self._active_variables = []
        for variable_name in self._active_variable_names:
            self._active_variables.append(symbol_table.lookup(variable_name))

        node_copy = node.copy()
        node_copy.children = []

        # split active and passive nodes.
        self._logger.debug("Adding passive code")
        active_nodes = []
        for child in node.children:
            if passive(child, self._active_variables):
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
        self._logger.debug("Processing active code")
        for child in active_nodes:
            result = self._visit(child)
            if isinstance(result, list):
                node_copy.children.extend(result)
            else:
                node_copy.children.append(result)

        return node_copy

    def assignment_node(self, node):
        '''This method is called if the visitor finds an Assignment node.'''

        self._logger.debug("Transforming active assignment")
        if not self._active_variables:
            raise VisitorError(
                "An assignment node should not be called without a schedule "
                "being called beforehand as the latter sets up the active "
                "variables.")
        assign_trans = AssignmentTrans(self._active_variables)
        new_node = node.copy()
        # Temporary parent schedule required by the transformation.
        dummy_schedule = Schedule()
        dummy_schedule.children.append(new_node)
        assign_trans.apply(new_node)
        return dummy_schedule.pop_all_children()

    def loop_node(self, node):
        '''This method is called if the visitor finds a Loop node.'''

        self._logger.debug("Transforming active loop")
        new_node = node.copy()
        # Reverse loop order
        start_expr = new_node.start_expr.copy()
        new_node.start_expr = new_node.stop_expr.copy()
        new_node.stop_expr = start_expr
        new_node.step_expr = negate(new_node.step_expr.copy())
        new_node.children[3] = self._visit(node.children[3])
        return new_node
