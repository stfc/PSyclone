'''Generic PSyIR visitor code'''

# pylint: disable=exec-used


class PSyIRVisitor(object):
    '''An abstract generic PSyIR visitor'''

    def __init__(self):
        self._code = ""

    def visit(self, node):
        '''Implements the PSyIR tree walk and call back. Call backs are
        implemented by using the name of the PSyIR tree as a key. Two
        call back methods are used for each node in the tree, one
        before any children are visited (start) and one after (end).

        :param node: A PSyIR node.
        :type node: xxx

        :raises VisitorError: if a node is found that does not have
        associated (start or end) call back methods.

        '''
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
        '''
        :returns: the transformed Fortran code.
        :rtype: str

        '''
        return self._code

    def kernelschedule_start(self, node):
        ''' xxx '''
        raise NotImplementedError("Implement the kernelschedule_start method.")

    def kernelschedule_end(self, node):
        ''' xxx '''
        raise NotImplementedError("Implement the kernelschedule_end method.")

    def codeblock_start(self, node):
        ''' xxx '''
        raise NotImplementedError("Implement the codeblock_start method.")

    def codeblock_end(self, node):
        ''' xxx '''
        raise NotImplementedError("Implement the codeblock_end method.")

    def assignment_start(self, node):
        ''' xxx '''
        raise NotImplementedError("Implement the assignment_start method.")

    def assignment_end(self, node):
        ''' xxx '''
        raise NotImplementedError("Implement the assignment_end method.")
