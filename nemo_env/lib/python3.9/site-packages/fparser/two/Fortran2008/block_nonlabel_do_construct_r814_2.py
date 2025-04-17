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
"""This module provides the Fortran2008-specific version of a partial
implementation of the block-do-construct rule r814. fparser splits
this rule into a label and nonlabel version.

"""

from fparser.two.Fortran2003 import (
    Block_Nonlabel_Do_Construct as Block_Nonlabel_Do_Construct_2003,
)
from fparser.two.Fortran2008.nonlabel_do_stmt_r817 import Nonlabel_Do_Stmt


class Block_Nonlabel_Do_Construct(Block_Nonlabel_Do_Construct_2003):
    """Subclass the 2003 version so that this class will import the
    Fortran2008 Nonlabel_Do_Stmt class

    """

    @staticmethod
    def nonlabel_do_stmt_cls():
        """
        :returns: Fortran2008 Nonlabel_Do_Stmt class.
        :rtype: :py:class:`fparser.two.Fortran2008.Nonlabel_Do_Stmt`

        """
        return Nonlabel_Do_Stmt
