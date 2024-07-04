'''
'''
from psyclone.psyir.backend.visitor import PSyIRVisitor


class Relink(PSyIRVisitor):
    '''
    '''
    # We want to modify the supplied tree.
    _DISABLE_LOWERING = True

    def __init__(self, table, skip_nodes=True, check_global_constraints=False):
        super().__init__(skip_nodes=skip_nodes,
                         check_global_constraints=check_global_constraints)
        self._table = table

    def reference_node(self, ref_node):
        '''
        '''
        try:
            ref_node.symbol = self._table.lookup(ref_node.symbol.name)
        except KeyError:
            # This symbol isn't in the table.
            pass

    def literal_node(self, lit_node):
        '''
        '''
        lit_node.datatype.relink(self._table)
