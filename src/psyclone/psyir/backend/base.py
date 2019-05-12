# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019, Science and Technology Facilities Council
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
# Author R. W. Ford, STFC Daresbury Lab.

'''Generic PSyIR visitor code that can be specialised by different
back ends.

'''
# pylint: disable=exec-used


class VisitorError(Exception):
    '''Provides a PSyclone-specific error class for errors found within a
    PSyIR visitor.

    :param str value: error message.

    '''
    def __init__(self, value):
        Exception.__init__(self, value)
        self.value = "Visitor Error: "+value

    def __str__(self):
        return str(self.value)


class PSyIRVisitor(object):
    '''An abstract generic PSyIR visitor. This is designed to be
    specialised by particular back ends. Assumes that a back end only
    need receive a method call back for each node before and after any
    children nodes are processed.

    :param bool skip_nodes: If skip_nodes is False then an exception \
    is raised if a visitor method for a PSyIR node has not been \
    implemented, otherwise the visitor silently continues. This is an \
    optional argument which defaults to False.

    '''
    def __init__(self, skip_nodes=False):
        self._skip_nodes = skip_nodes
        self._code = ""

    def visit(self, node):
        '''Implements the PSyIR tree walk and call back. Call backs are
        implemented by using the name of class of the object in the
        PSyIR tree as a key. Two call back methods are used for each
        node in the tree, one before any children are visited (start)
        and one after (end).

        :param node: A PSyIR node.
        :type node: :py:class:`psyclone.psyGen.Node`

        :raises VisitorError: if a node is found that does not have
        associated (start or end) call back methods.

        '''
        try:
            exec("self.{0}_start(node)".format(type(node).__name__.lower()))
        except AttributeError as excinfo:
            if not self._skip_nodes:
                raise VisitorError("Unsupported node found: {0}"
                                   "".format(str(excinfo)))
        for child in node.children:
            self.visit(child)
        try:
            if not self._skip_nodes:
                exec("self.{0}_end(node)".format(type(node).__name__.lower()))
        except AttributeError as excinfo:
            raise VisitorError("Unsupported node found: {0}"
                               "".format(str(excinfo)))

    @property
    def code(self):
        '''
        :returns: the transformed Fortran code.
        :rtype: str

        '''
        return self._code
