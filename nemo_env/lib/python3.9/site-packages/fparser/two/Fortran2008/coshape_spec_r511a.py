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
    Module containing Fortran2008 Coshape_Spec rule R511.a
"""
from fparser.common.splitline import string_replace_map
from fparser.two.Fortran2008.lower_cobound_r512 import Lower_Cobound
from fparser.two.Fortran2008.upper_cobound_r513 import Upper_Cobound
from fparser.two.utils import SeparatorBase


class Coshape_Spec(SeparatorBase):  # R511.a
    """
    coshape-spec is [ lower-cobound : ] upper-cobound

    """

    subclass_names = []
    use_names = ["Lower_Cobound", "Upper_Cobound"]

    @staticmethod
    def match(string):
        """
        Implements the matching for a coarray shape.

        :param str string: the string to match as shape.

        :return: `None` if there is no match, otherwise a 2-tuple with \
                 lower bound if given or `None`, and upper bound.
        :rtype: `NoneType` or \
            (`None`, :py:class:`fparser.two.Fortran2008.Upper_Cobound`) or \
            (:py:class:`fparser.two.Fortran2008.Lower_Cobound`, \
             :py:class:`fparser.two.Fortran2008.Upper_Cobound`)

        """
        line, repmap = string_replace_map(string)
        if ":" not in line:
            return (None, Upper_Cobound(string))
        lower, upper = line.split(":", 1)
        lower = lower.rstrip()
        upper = upper.lstrip()
        if not upper:
            return None
        if not lower:
            return None
        return (Lower_Cobound(repmap(lower)), Upper_Cobound(repmap(upper)))

    def tostr(self):
        """
        Converts the Shape specification to string.

        :return: the shape specification as string.
        :rtype: str

        """
        if self.items[0] is None:
            return str(self.items[1])
        return SeparatorBase.tostr(self)
