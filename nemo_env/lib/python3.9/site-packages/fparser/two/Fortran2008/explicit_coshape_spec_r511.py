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
    Module containing Fortran2008 Explicit_Coshape_Spec rule R511
"""
from fparser.common.splitline import string_replace_map
from fparser.two.Fortran2008.lower_cobound_r512 import Lower_Cobound
from fparser.two.utils import SeparatorBase


class Explicit_Coshape_Spec(SeparatorBase):  # R511
    """
    Fortran 2008 rule R511
    explicit-coshape-spec is [ coshape-spec-list , ] [ lower-cobound : ] *

    Associated constraint is:

    "C529 (R511)  A lower-cobound or upper-cobound that  is  not  a  constant
          expression shall appear only in a subprogram, BLOCK construct, or
          interface body."

    C529 is currently not checked - issue #259.

    """

    subclass_names = []
    use_names = ["Coshape_Spec_List", "Lower_Cobound"]

    @staticmethod
    def match(string):
        """
        Implements the matching for explicit coarray shape specification.

        :param str string: the string to match as deferred shape.

        :return: `None` if there is no match, otherwise a 2-tuple \
                 containing matched coshape-spec-list or `None` and \
                 matched lower-cobound or `None`.
        :rtype: `NoneType` or \
            (:py:class:`fparser.two.Fortran2008.Coshape_Spec_List` or `None`, \
             :py:class:`fparser.two:Fortran2008.Lower_Cobound` or `None`)

        """
        # Avoid circular dependencies by importing here.
        # pylint: disable=import-outside-toplevel
        from fparser.two.Fortran2008 import Coshape_Spec_List

        if not string.endswith("*"):
            return None
        line = string[:-1].rstrip()
        if not line:
            return (None, None)
        if line.endswith(":"):
            line, repmap = string_replace_map(line[:-1].rstrip())
            sep_pos = line.rfind(",")
            if sep_pos == -1:
                return (None, Lower_Cobound(repmap(line)))
            return (
                Coshape_Spec_List(repmap(line[:sep_pos].rstrip())),
                Lower_Cobound(repmap(line[sep_pos + 1 :].lstrip())),
            )
        if not line.endswith(","):
            return None
        line = line[:-1].rstrip()
        return (Coshape_Spec_List(line), None)

    def tostr(self):
        """
        Converts the explicit coarray shape specification to string.

        :return: the shape specification as string.
        :rtype: str

        """
        s = ""
        if self.items[0]:
            s += str(self.items[0]) + ", "
        if self.items[1]:
            s += str(self.items[1]) + " : "
        s += "*"
        return s
