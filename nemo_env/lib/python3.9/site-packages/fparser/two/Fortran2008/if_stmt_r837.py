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
    Module containing Fortran2008 If_Stmt rule R837
"""
from fparser.two.Fortran2003 import If_Stmt as If_Stmt_2003
from fparser.two.Fortran2008.action_stmt_c828 import Action_Stmt_C828


class If_Stmt(If_Stmt_2003):  # R837
    """
    Fortran 2008 rule R837
    if-stmt is IF ( scalar-logical-expr ) action-stmt

    The implementation of this rule only replaces the :py:attr:`use_names` and
    :py:attr:`action_stmt_class` attributes to use the Fortran 2008 variant
    :py:class:`Action_Stmt_C828` instead of
    :py:class:`fparser.two.Fortran2003.Action_Stmt_C802`.

    Associated constraints are:

    C828 (R837) The action-stmt in the if-stmt shall not be an end-function-stmt,
          end-mp-subprogram-stmt, end-program-stmt, end-subroutine-stmt, or if-stmt.

    """

    use_names = ["Scalar_Logical_Expr", "Action_Stmt_C828"]
    action_stmt_cls = Action_Stmt_C828
