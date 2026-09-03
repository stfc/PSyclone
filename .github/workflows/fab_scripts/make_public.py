#!/usr/bin/env python

# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2024-2025, Science and Technology Facilities Council.
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
# ------------------------------------------------------------------------------
# Author: Joerg Henrichs, Bureau of Meteorology

"""This file contains a script that will remove any private or protected
declaration in a Fortran file. This is used by PSyclone's kernel extraction
and driver creation feature, see
https://psyclone.readthedocs.io/en/latest/psyke.html.
for details.

Usage:  make_public.py  file1.f90
"""

import sys

from fparser.common.readfortran import FortranFileReader
from fparser.two.Fortran2003 import (
    Access_Stmt,
    Access_Spec,
    Attr_Spec,
    Binding_Private_Stmt,
    Private_Components_Stmt,
    Protected_Stmt,
)
from fparser.two.parser import ParserFactory
from fparser.two.Fortran2003 import Type_Declaration_Stmt
from fparser.two.utils import walk


# -----------------------------------------------------------------------------
def remove_private(filename: str) -> ParserFactory:
    """
    Simple function that removes all private and protected declarations.

    :param str filename: the file in which to remove private and protected
    :return :py:class:`fparser.ParserFactory`: the modified parse tree
    """
    # Do not filter out comments, since this would remove UM-style
    # 'depends on' comments for dependencies that are otherwise not detected.
    reader = FortranFileReader(filename, ignore_comments=False)
    parser = ParserFactory().create(std="f2008")
    parse_tree = parser(reader)
    # A useful print to see the actual rules
    # print(repr(parse_tree))

    # Loop over all access and protected statements. Note that a
    # `protected_stmt` is not an access statement, so it needs to
    # be listed additionally:
    for node in walk(
        parse_tree,
        (Access_Stmt, Protected_Stmt, Private_Components_Stmt,
         Binding_Private_Stmt),
    ):
        # A Private_Components_Stms has no items:
        if isinstance(
            node, (Binding_Private_Stmt, Private_Components_Stmt)
        ) or node.items[0] in ["PRIVATE", "PROTECTED"]:
            # Find the node in the parent, and remove it:
            node.parent.children.remove(node)

    for node in walk(parse_tree, Access_Spec):
        if str(node) != "PRIVATE":
            continue

        # In some cases it might be required to leave certain declarations
        # private. One example is the variable `ModuleName`` which is used
        # with DrHook to specify the name of the module. If at the same time
        # another modules does a wild-card import, this importing module will
        # have two definitions of ModuleName, its own and the imported one.
        # The following code can be uncommented to test for this type of
        # definition and leave it private.
        names_to_ignore = ["modulename"]
        if isinstance(node.parent.parent, Type_Declaration_Stmt):
            # type-declaration-stmt is declaration-type-spec [
            #     [ , attr-spec ]... :: ] entity-decl-list
            entity_decl = node.parent.parent.items[-1].items[0]
            if str(entity_decl.items[0]).lower() in names_to_ignore:
                # Don't change a variable in the names_to_ignore list
                continue
        node.string = "PUBLIC"

    all_nodes = list(walk(parse_tree, Attr_Spec))
    for node in all_nodes:
        if str(node) == "PROTECTED":
            # This is a tuple, so we can't simply remove the attribute
            node.parent.items = tuple(i for i in node.parent.items
                                      if i is not node)
            # If all items in the Attr_Spec are removed, we need to replace the
            # Attr_Spec in the parent-parent (Type_Declaration) with None,
            # otherwise fparser will create e.g. `real, :: a`
            if len(node.parent.items) == 0:
                # Again all tuples, which we can't modify, so we need to
                # recreate the tuple but replace the attr_spec with None
                type_decl = node.parent.parent
                type_decl.items = (type_decl.items[0], None,
                                   type_decl.items[2])

    return parse_tree


# -----------------------------------------------------------------------------
if __name__ == "__main__":
    user_filename = sys.argv[1]
    modified_parse_tree = remove_private(user_filename)
    print(modified_parse_tree)
