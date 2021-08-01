from __future__ import print_function

from psyclone.psyad.transformations import AssignmentTrans
from psyclone.psyir.backend.visitor import PSyIRVisitor
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import Schedule


def active(node, active_variables):
    ''' Determines whether this node contains variables that are active.

    :param node: the PSyIR node that is being evaluated.
    :type node: :py:class:`psyclone.psyir.nodes.Node`
    :param active_variables: a list of active variables.
    :type active_variables: :py:class:`psyclone.psyir.symbols.DataSymbol`

    :returns: True if active and False otherwise.
    :rtype: bool

    '''
    from psyclone.psyir.nodes import Reference
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


class AdjointVisitor(PSyIRVisitor):
    ''' Visitor implementing an Adjoint Visitor. '''

    def __init__(self, active_variable_names):
        super(AdjointVisitor, self).__init__()
        self._active_variable_names = active_variable_names
        self._active_variables = None

    def filecontainer_node(self, node):
        '''This method is called if the visitor finds a FileContainer node.'''
        print("Copying FileContainer")
        node_copy = node.copy()
        node_copy.children = []
        for child in node.children:
            result = self._visit(child)
            if result:
                node_copy.children.append(result)
        return node_copy
        
    def schedule_node(self, node):
        '''This method is called if the visitor finds a Schedule node.'''
        print ("Transforming Schedule")
        # A schedule has a scope so determine and store active variables
        symbol_table = node.scope.symbol_table
        self._active_variables = []
        for variable_name in self._active_variable_names:
            self._active_variables.append(symbol_table.lookup(variable_name))

        node_copy = node.copy()
        node_copy.children = []

        # split active and passive nodes.
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
        active_nodes.reverse()

        # Process active nodes.
        for child in active_nodes:
            result = self._visit(child)
            if result:
                if isinstance(result, list):
                    node_copy.children.extend(result)
                else:
                    node_copy.children.append(result)

        return node_copy

    def assignment_node(self, node):
        '''This method is called if the visitor finds an Assignment node.'''
        print ("Transforming active assignment")
        assign_trans = AssignmentTrans(self._active_variables)
        new_node = node.copy()
        # Temporary parent schedule required by the transformation.
        dummy_schedule = Schedule()
        dummy_schedule.children.append(new_node)
        assign_trans.apply(new_node)
        return dummy_schedule.pop_all_children()

    def loop_node(self, node):
        '''This method is called if the visitor finds a Loop node.'''
        print ("Transforming active loop")
        new_node = node.copy()
        # TODO. Reverse loop order
        new_node.children[3] = self._visit(node.children[3])
        return new_node
