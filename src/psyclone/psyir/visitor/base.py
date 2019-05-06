''' xxx '''


class PSyIRVisitor():
    ''' xxx '''

    def __init__(self):
        self._code = ""

    def visit(self, node):
        ''' xxx '''
        try:
            exec("self.{0}_start(node)".format(type(node).__name__.lower()))
        except AttributeError as excinfo:
            raise RuntimeError("Unsupported node found: {0}"
                               "".format(str(excinfo)))
        for child in node.children:
            self.visit(child)
        try:
            exec("self.{0}_end(node)".format(type(node).__name__.lower()))
        except AttributeError as excinfo:
            raise RuntimeError("Unsupported node found: {0}"
                               "".format(str(excinfo)))

    @property
    def code(self):
        ''' xxx '''
        return self._code

    def kernelschedule_start(self, node):
        ''' xxx '''
        raise NotImplementedError("Implement me")

    def kernelschedule_end(self, node):
        ''' xxx '''
        raise NotImplementedError("Implement me")

    def codeblock_start(self, node):
        ''' xxx '''
        raise NotImplementedError("Implement me")

    def codeblock_end(self, node):
        ''' xxx '''
        raise NotImplementedError("Implement me")
