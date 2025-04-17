# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023, Science and Technology Facilities Council.
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

"""
    Module containing Fortran2008 Block_Stmt rule R808
"""
from fparser.two.utils import StmtBase, WORDClsBase, ScopingRegionMixin


class Block_Stmt(StmtBase, WORDClsBase, ScopingRegionMixin):
    """
    Fortran 2008 Rule 808.

    block-stmt is [ block-construct-name : ] BLOCK

    """

    subclass_names = []
    use_names = ["Block_Construct_Name"]
    counter = 0

    @staticmethod
    def match(string):
        """
        Attempts to match the supplied text with this rule.

        :param str string: the text to match.

        :returns: a tuple of the matched node and instance of Counter or \
                  None if there is no match.
        :rtype: Tuple["BLOCK", \
                      :py:class:`fparser.two.Fortran2008.Block_Stmt.Counter`] \
                | NoneType
        """
        found = WORDClsBase.match("BLOCK", None, string)
        if not found:
            return None
        block, _ = found
        # Construct a unique name for this BLOCK (in case it isn't named). We
        # ensure the name is not a valid Fortran name so that it can't clash
        # with any regions named in the code.
        scope_name = f"block:{Block_Stmt.counter}"
        Block_Stmt.counter += 1
        # TODO #397. Ideally we'd have the name associated with the Block
        # Construct here (if any) so that it could be displayed in repr.
        # As it is, repr will show scope_name which will not be the same
        # as any explicit name given to the Block. (This name *is* shown
        # in the repr of the End_Block_Stmt.) This problem is common to
        # other block constructs such as Block_Nonlabel_Do_Construct.
        return block, scope_name

    def get_scope_name(self):
        """
        :returns: the name of this scoping region.
        :rtype: str
        """
        if self.item.name:
            return self.item.name
        return self.items[1]

    def get_start_name(self):
        """
        :returns: the name associated with this Block construct or None.
        :rtype: str | NoneType
        """
        return self.item.name

    def tostr(self):
        """
        :returns: the string representation of this node.
        :rtype: str
        """
        return "BLOCK"
