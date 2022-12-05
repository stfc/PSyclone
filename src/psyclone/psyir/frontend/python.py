import ast


class PSyIRNodeVisitor(ast.NodeVisitor):
    ''' xxx '''
    def generic_visit(self, node):
        ''' xxx '''
        print(type(node))
        super().generic_visit(node)


class PythonReader():
    '''PSyIR Python frontend. This frontend translates Python into
    language-level PSyIR.

    '''
    def psyir_from_source(self, source_code):
        ''' xxx '''
        tree = ast.parse(source_code)
        # print(ast.dump(tree))
        node_visitor = PSyIRNodeVisitor()
        node_visitor.visit(tree)
