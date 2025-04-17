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
    Module containing Fortran2008 Attr_Spec rule R502
"""
from fparser.two import pattern_tools as pattern
from fparser.two.Fortran2003 import Attr_Spec as Attr_Spec_2003
from fparser.two.utils import STRINGBase


class Attr_Spec(Attr_Spec_2003):  # R502
    """
    Fortran 2008 rule R502.

    .. code-block:: fortran

        attr-spec is access-spec
                     or ALLOCATABLE
                     or ASYNCHRONOUS
                     or CODIMENSION lbracket coarray-spec rbracket
                     or CONTIGUOUS
                     or DIMENSION ( array-spec )
                     or EXTERNAL
                     or INTENT ( intent-spec )
                     or INTRINSIC
                     or language-binding-spec
                     or OPTIONAL
                     or PARAMETER
                     or POINTER
                     or PROTECTED
                     or SAVE
                     or TARGET
                     or VALUE
                     or VOLATILE

    In the spec above, lbracket and rbracket are left and right square
    brackets `[]` but not printed explicitly to avoid misinterpretation
    as optional parts.

    This rule adds CODIMENSION and CONTIGUOUS attributes to Fortran2003's R503.

    """

    subclass_names = Attr_Spec_2003.subclass_names[:]
    subclass_names.append("Codimension_Attr_Spec")
    use_names = []

    @staticmethod
    def match(string):
        """
        Implements the matching for attributes of types.

        :param str string: the string to match as attribute.

        :return: `None` if there is no match, otherwise a 1-tuple \
                 containing the matched string.
        :rtype: `NoneType` or (`str`,)

        """
        return STRINGBase.match(pattern.abs_attr_spec_f08, string)
