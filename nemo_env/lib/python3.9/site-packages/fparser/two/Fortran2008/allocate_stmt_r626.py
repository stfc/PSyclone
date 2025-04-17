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
    Module containing Fortran2008 Allocate_Stmt rule R626
"""
from fparser.two.Fortran2003 import Allocate_Stmt as Allocate_Stmt_2003


class Allocate_Stmt(Allocate_Stmt_2003):  # R626
    """
    Fortran 2008 rule R626.

    .. code-block:: fortran

        allocate-stmt is ALLOCATE ( [ type-spec :: ] allocation-list
                                    [, alloc-opt-list ] )

    The implementation of this rule simply ensures that the Fortran2008 version
    of Alloc_Opt is used.

    """

    subclass_names = []
    use_names = ["Type_Spec", "Allocation_List", "Alloc_Opt_List"]

    @classmethod
    def alloc_opt_list(cls):
        """
        :returns: the Fortran2008 flavour of Alloc_Opt_List.
        :rtype: type
        """
        # Avoid circular dependencies by importing here.
        # pylint: disable=import-outside-toplevel
        from fparser.two.Fortran2008 import Alloc_Opt_List

        return Alloc_Opt_List
