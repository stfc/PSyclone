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
    Module containing Fortran2008 Action_Stmt rule R214
"""
from fparser.two.Fortran2003 import Action_Stmt as Action_Stmt_2003


class Action_Stmt(Action_Stmt_2003):  # R214
    """
    Fortran 2008 rule R214.

    .. code-block:: fortran

        action-stmt is allocate-stmt
                        or assignment-stmt
                        or backspace-stmt
                        or call-stmt
                        or close-stmt
                        or continue-stmt
                        or cycle-stmt
                        or deallocate-stmt
                        or end-function-stmt
                        or end-mp-subprogram-stmt
                        or end-program-stmt
                        or end-subroutine-stmt
                        or endfile-stmt
                        or error-stop-stmt
                        or exit-stmt
                        or flush-stmt
                        or forall-stmt
                        or goto-stmt
                        or if-stmt
                        or inquire-stmt
                        or lock-stmt
                        or nullify-stmt
                        or open-stmt
                        or pointer-assignment-stmt
                        or print-stmt
                        or read-stmt
                        or return-stmt
                        or rewind-stmt
                        or stop-stmt
                        or sync-all-stmt
                        or sync-images-stmt
                        or sync-memory-stmt
                        or unlock-stmt
                        or wait-stmt
                        or where-stmt
                        or write-stmt
                        or arithmetic-if-stmt
                        or computed-goto-stmt

    The implementation of this rule adds the relevant subclass names
    for new statements added in Fortran 2008.

    Associated constraints are:

    "C201 (R208) An execution-part shall not contain an end-function-stmt,
          end-mp-subprogram-stmt, end-program-stmt, or end-subroutine-stmt."

    NB: The following statements are not yet implemented:
    end-mp-subprogram-stmt, endfile-stmt, lock-stmt, sync-all-stmt,
    sync-images-stmt, sync-memory-stmt, unlock-stmt.

    """

    # Fortran 2008 adds a few additional action-stmt. We therefore
    # extend the Fortran 2003 specification
    subclass_names = Action_Stmt_2003.subclass_names[:]
    subclass_names.append("Error_Stop_Stmt")
