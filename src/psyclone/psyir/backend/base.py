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
# pylint: disable=eval-used

from __future__ import print_function


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
    '''A generic PSyIR visitor. This is designed to be specialised by
    a particular back end.

    :param bool skip_nodes: If skip_nodes is False then an exception \
    is raised if a visitor method for a PSyIR node has not been \
    implemented, otherwise the visitor silently continues. This is an \
    optional argument which defaults to False.
    :param indent: Specifies how what to use for indentation. This is \
    an optional argument that defaults to None, which indicates that \
    two spaces should be used.
    :type indent: str or NoneType
    :param int start_depth: Specifies how much indentation to start \
    with. This is an optional argument that defaults to 0.

    :raises TypeError: if skip_nodes is not a boolean, indent is not a \
    string or start_depth is not an integer.

    '''
    def __init__(self, skip_nodes=False, indent=None, start_depth=0):

        if not isinstance(skip_nodes, bool):
            raise TypeError(
                "skip_nodes should be a boolean but found '{0}'"
                "".format(type(skip_nodes).__name__))
        if indent is not None and not isinstance(indent, str):
            raise TypeError(
                "indent should be a str but found '{0}'"
                "".format(type(indent).__name__))
        if not isinstance(start_depth, int):
            raise TypeError(
                "start_depth should be an integer but found '{0}'"
                "".format(type(start_depth).__name__))

        self._skip_nodes = skip_nodes
        if indent is None:
            self._indent = "  "
        else:
            self._indent = indent
        self._depth = start_depth

    @property
    def _nindent(self):
        '''
        :returns: the current indentation string.
        :rtype: str

        '''
        return self._depth * self._indent

    def visit(self, node):
        '''Implements the PSyIR callbacks. Callbacks are implemented by
        using the name of class of the object in the PSyIR tree as the
        method name. Names are not modified, other than making them
        lower case, apart from the `Return` class which is changed to
        `return_node` because `return` is a Python keyword.

        :param node: A PSyIR node.
        :type node: :py:class:`psyclone.psyGen.Node`

        :raises VisitorError: if a node is found that does not have \
        associated call back method (and skip_nodes is not set).

        '''
        node_name = type(node).__name__.lower()
        if node_name == "return":
            node_name = "return_node"
        try:
            return eval("self.{0}(node)".format(node_name))
        except AttributeError as excinfo:
            message = "Unsupported node found: {0}".format(str(excinfo))
            if self._skip_nodes:
                print(message)
                for child in node.children:
                    self.visit(child)
            else:
                raise VisitorError(message)
