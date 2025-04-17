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
    Module containing Fortran2008 Codimension_Attr_Spec rule R502.d
"""
from fparser.two.Fortran2008.coarray_bracket_spec_r502d0 import Coarray_Bracket_Spec
from fparser.two.utils import WORDClsBase


class Codimension_Attr_Spec(WORDClsBase):  # R502.d
    """
    codimension-attr-spec is CODIMENSION lbracket coarray-spec rbracket

    In the spec above, lbracket and rbracket are left and right square
    brackets `[]` but not printed explicitly to avoid misinterpretation
    as optional parts.

    """

    subclass_names = []
    use_names = ["Coarray_Bracket_Spec"]

    @staticmethod
    def match(string):
        """
        Implements the matching for the CODIMENSION attribute.

        :param str string: the string to match as the attribute.

        :return: `None` if there is no match, otherwise a 2-tuple \
                 containing `CODIMENSION` as a string and the matched \
                 coarray-spec..
        :rtype: `NoneType` or \
            (`str`, :py:class:`fparser.two.Fortran2008.Coarray_Bracket_Spec`,)

        """
        return WORDClsBase.match(
            "CODIMENSION", Coarray_Bracket_Spec, string, colons=False, require_cls=True
        )
