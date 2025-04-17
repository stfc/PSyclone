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
    Module containing Fortran2008 Action_Stmt constraint C816
"""
from fparser.two.Fortran2003 import Action_Stmt_C824 as Action_Stmt_C824_2003
from fparser.two.Fortran2008.action_stmt_r214 import Action_Stmt


class Action_Stmt_C816(Action_Stmt_C824_2003):
    """
    action-stmt-c816 is action-stmt
    C816 is applied.
    """

    subclass_names = Action_Stmt.subclass_names[:]
    subclass_names.remove("Arithmetic_If_Stmt")
    subclass_names.remove("Continue_Stmt")
    subclass_names.remove("Cycle_Stmt")
    subclass_names.remove("End_Function_Stmt")
    subclass_names.remove("End_Subroutine_Stmt")
    subclass_names.remove("Error_Stop_Stmt")
    subclass_names.remove("Exit_Stmt")
    subclass_names.remove("Goto_Stmt")
    subclass_names.remove("Return_Stmt")
    subclass_names.remove("Stop_Stmt")
