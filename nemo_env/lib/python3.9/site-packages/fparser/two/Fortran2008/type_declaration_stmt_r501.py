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
    Module containing Fortran2008 Type_Declaration_Stmt rule R501
"""
from fparser.two.Fortran2003 import Type_Declaration_Stmt as Type_Declaration_Stmt_2003


class Type_Declaration_Stmt(Type_Declaration_Stmt_2003):  # R501
    """
    Fortran 2008 rule 501.

    .. code-block:: fortran

        type-declaration-stmt is declaration-type-spec [ [ , attr-spec ] ... :: ]
                                 entity-decl-list

    The implementation of this rule does not add anything to the Fortran 2003
    variant but overwrites :py:meth:`get_attr_spec_list_cls` to use
    the Fortran 2008 variant of :py:class:`Attr_Spec_List`.

    Associated constraints are:

    "C501 (R501)  The same attr-spec shall not appear more than once in a given
          type-declaration-stmt."
    "C502 (R501)  If a language-binding-spec with a NAME= specifier appears,
          the entity-decl-list shall consist of a single entity-decl."
    "C503 (R501)  If a language-binding-spec is specified, the entity-decl-list
          shall not contain any procedure names."
    "C505 (R501)  If initialization appears, a double-colon separator shall
          appear before the entity-decl-list."

    C501-C503, C505 are currently not checked - issue #259.

    """

    @staticmethod
    def get_attr_spec_list_cls():
        """Return the type used to match the attr-spec-list

        This overwrites the Fortran 2003 type with the Fortran 2008 variant.

        """
        # Avoid circular dependencies by importing here.
        # pylint: disable=import-outside-toplevel
        from fparser.two.Fortran2008 import Attr_Spec_List

        return Attr_Spec_List
