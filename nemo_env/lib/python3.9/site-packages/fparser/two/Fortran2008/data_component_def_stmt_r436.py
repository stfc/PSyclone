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
    Module containing Fortran2008 Data_Component_Def_Stmt rule R436
"""
from fparser.two.Fortran2003 import (
    Data_Component_Def_Stmt as Data_Component_Def_Stmt_2003,
)
from fparser.two.Fortran2003 import Declaration_Type_Spec, Component_Decl_List
from fparser.two.utils import Type_Declaration_StmtBase


class Data_Component_Def_Stmt(Data_Component_Def_Stmt_2003):  # R436
    """
    Fortran 2008 rule 436.

    .. code-block:: fortran

        data-component-def-stmt is declaration-type-spec [
                 [ , component-attr-spec-list ] :: ] component-decl-list

    The implementation of this rule does not add anything to the Fortran 2003
    variant but reimplements the match method identical to Fortran 2003 as
    otherwise the generated Fortran 2008 variant of `Component_Attr_Spec_List`
    would not be used. Unfortunately, the required `attr_spec_list_cls` can not
    simply be provided as a class property since the relevant class is only
    generated at the end of this file using the `use_names` class property of
    this class.

    Associated constraints are:

    "C439 (R436)  No component-attr-spec shall appear more than once in a given
          component-def-stmt."
    "C440 (R436)  If neither the POINTER nor the ALLOCATABLE attribute is
          specified, the declaration-type-spec in the component-def-stmt shall
          specify an intrinsic type or a previously defined derived type."
    "C441 (R436)  If the POINTER or ALLOCATABLE attribute is specified, each
          component-array-spec shall be a deferred-shape-spec-list."
    "C442 (R436)  If a coarray-spec appears, it shall be a
          deferred-coshape-spec-list and the component shall have the
          ALLOCATABLE attribute."
    "C443 (R436)  If a coarray-spec appears, the component shall not be of type
          C_PTR or C_FUNPTR."
    "C445 (R436)  If neither the POINTER nor the ALLOCATABLE attribute is
          specified, each component-array-spec shall be an
          explicit-shape-spec-list."
    "C447 (R436)  A component shall not have both the ALLOCATABLE and POINTER
          attributes."
    "C448 (R436)  If the CONTIGUOUS attribute is specified, the component shall
          be an array with the POINTER attribute."
    "C457 (R436)  If component-initialization appears, a double-colon separator
          shall appear before the component-decl-list."
    "C458 (R436)  If component-initialization appears, every type parameter and
          array bound of the component shall be a colon or constant expression.
    "C459 (R436)  If => appears in component-initialization, POINTER shall
          appear in the component-attr-spec-list. If = appears in
          component-initialization, neither POINTER nor ALLOCATABLE shall
          appear in the component-attr-spec-list."

    C439-C443, C445, C447-C448, C457-C459 are currently not checked
    - issue #258.

    """

    @staticmethod
    def match(string):
        """Implements the matching of a data component definition statement.

        :param str string: the reader or string to match as a data \
                           component definition statement.

        :return: a 3-tuple containing declaration type specification, \
                 component attribute specification and component declaration \
                 list if there is a match or None if there is no match.
        :rtype: `NoneType` or \
            (:py:class:`fparser.two.Fortran2003.Declaration_Type_Spec`, \
             :py:class:`fparser.two.Fortran2008.Component_Attr_Spec_List`, \
             :py:class:`fparser.two.Fortran2003.Component_Decl_List`)

        """
        # Avoid circular dependencies for generated clas by importing here.
        # pylint: disable=import-outside-toplevel
        from fparser.two.Fortran2008 import Component_Attr_Spec_List

        return Type_Declaration_StmtBase.match(
            Declaration_Type_Spec, Component_Attr_Spec_List, Component_Decl_List, string
        )
