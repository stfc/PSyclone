# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2024, Science and Technology Facilities Council
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
# Author I. Kavcic, Met Office
# Modified A. R. Porter, S. Siso, R. W. Ford and N. Nobre, STFC Daresbury Lab
# Modified J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

'''
This module provides support for extraction of code within a specified
invoke. The extracted code may be a single kernel, multiple occurrences of a
kernel in an invoke, nodes in an invoke or the entire invoke (extraction
applied to all Nodes).

There is currently only one class in this module: ExtractNode (see below).

Another class which contains helper functions for code extraction, such as
wrapping up settings for generating driver for the extracted code, will
be added in Issue #298.
'''

from psyclone.f2pygen import CommentGen
from psyclone.psyir.nodes.psy_data_node import PSyDataNode


class ExtractNode(PSyDataNode):
    '''
    This class can be inserted into a Schedule to mark Nodes for
    code extraction using the ExtractRegionTrans transformation. By
    applying the transformation the Nodes marked for extraction become
    children of (the Schedule of) an ExtractNode.

    :param ast: reference into the fparser2 parse tree corresponding to
        this node.
    :type ast: sub-class of :py:class:`fparser.two.Fortran2003.Base`
    :param children: the PSyIR nodes that are children of this node.
    :type children: list of :py:class:`psyclone.psyir.nodes.Node`
    :param parent: the parent of this node in the PSyIR tree.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`
    :param options: a dictionary with options provided via transformations.
    :type options: Optional[Dict[str, Any]]
    :param str options["prefix"]: a prefix to use for the PSyData module name
        (``prefix_psy_data_mod``) and the PSyDataType
        (``prefix_PSyDataType``) - a "_" will be added automatically.
        It defaults to "extract", which means the module name used will be
        ``extract_psy_data_mode``, and the data type ``extract_PSyDataType``.
    :param str options["post_var_postfix"]: a postfix to be used when
        creating names to store values of output variable. A variable 'a'
        would store its value as 'a', and its output values as 'a_post' with
        the default post_var_postfix of '_post'.
    :param options["read_write_info"]: information about variables that are
        read and/or written in the instrumented code.
    :type options["read_write_info"]:
        py:class:`psyclone.psyir.tools.ReadWriteInfo`

    '''
    # Textual description of the node.
    _text_name = "Extract"
    _colour = "green"
    # The default prefix to add to the PSyData module name and PSyDataType
    _default_prefix = "extract"

    def __init__(self, ast=None, children=None, parent=None, options=None):
        super().__init__(ast=ast, children=children,
                         parent=parent, options=options)

        # Define a postfix that will be added to variable that are
        # modified to make sure the names can be distinguished between pre-
        # and post-variables (i.e. here input and output). A variable
        # "myvar" will be stored as "myvar" with its input value, and
        # "myvar_post" with its output value. It is the responsibility
        # of the transformation that inserts this node to make sure this
        # name is consistent with the name used when creating the driver
        # (otherwise the driver will not be able to read in the dumped
        # valued), and also to handle any potential name clashes (e.g. a
        # variable 'a' exists, which creates 'a_out' for the output variable,
        # which would clash with a variable 'a_out' used in the program unit).

        if options is None:
            options = {}

        self._post_name = options.get("post_var_postfix", "_post")

        # Keep a copy of the argument list:
        self._read_write_info = options.get("read_write_info")

    def __eq__(self, other):
        '''
        Checks whether two nodes are equal. Two ExtractNodes are equal if
        their extract_body members are equal.

        :param object other: the object to check equality to.

        :returns: whether other is equal to self.
        :rtype: bool
        '''
        is_eq = super().__eq__(other)
        is_eq = is_eq and self.post_name == other.post_name
        return is_eq

    @property
    def extract_body(self):
        '''
        :returns: the Schedule associated with this ExtractNode.
        :rtype: :py:class:`psyclone.psyir.nodes.Schedule`

        '''
        return super().psy_data_body

    @property
    def post_name(self):
        '''
        :returns: the _post_name member of this ExtractNode.
        :rtype: str
        '''
        return self._post_name

    def gen_code(self, parent):
        # pylint: disable=arguments-differ
        '''
        Generates the code required for extraction of one or more Nodes.
        It uses the PSyData API (via the base class PSyDataNode) to create
        the required callbacks that will allow a library to write the
        kernel data to a file.

        :param parent: the parent of this Node in the PSyIR.
        :type parent: :py:class:`psyclone.psyir.nodes.Node`.

        '''
        if self._read_write_info is None:
            # Typically, _read_write_info should be set at the constructor,
            # but some tests do not provide the required information. To
            # support these tests, allow creation of the read_write info here.
            # We cannot do this in the constructor, since at construction
            # time of this node it is not yet part of the PSyIR tree, so it
            # does not have children from which we can collect the input/output
            # parameters.

            # Avoid circular dependency
            # pylint: disable=import-outside-toplevel
            from psyclone.psyir.tools.call_tree_utils import CallTreeUtils
            # Determine the variables to write:
            ctu = CallTreeUtils()
            self._read_write_info = \
                ctu.get_in_out_parameters(self, options=self.options)

        options = {'pre_var_list': self._read_write_info.read_list,
                   'post_var_list': self._read_write_info.write_list,
                   'post_var_postfix': self._post_name}

        parent.add(CommentGen(parent, ""))
        parent.add(CommentGen(parent, " ExtractStart"))
        parent.add(CommentGen(parent, ""))
        super().gen_code(parent, options)
        parent.add(CommentGen(parent, ""))
        parent.add(CommentGen(parent, " ExtractEnd"))
        parent.add(CommentGen(parent, ""))

    def lower_to_language_level(self):
        # pylint: disable=arguments-differ
        '''
        Lowers this node (and all children) to language-level PSyIR. The
        PSyIR tree is modified in-place.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        if self._read_write_info is None:
            # Typically, _read_write_info should be set at the constructor,
            # but some tests do not provide the required information. To
            # support these tests, allow creation of the read_write info
            # here (it can't be done in the constructor, since this node
            # is not yet integrated into the PSyIR, so the dependency tool
            # cannot determine variable usage at that time):

            # Avoid circular dependency
            # pylint: disable=import-outside-toplevel
            from psyclone.psyir.tools.call_tree_utils import CallTreeUtils
            # Determine the variables to write:
            ctu = CallTreeUtils()
            self._read_write_info = \
                ctu.get_in_out_parameters(self, options=self.options)

        options = {'pre_var_list': self._read_write_info.read_list,
                   'post_var_list': self._read_write_info.write_list,
                   'post_var_postfix': self._post_name}

        return super().lower_to_language_level(options)


# For AutoAPI documentation generation
__all__ = ['ExtractNode']
