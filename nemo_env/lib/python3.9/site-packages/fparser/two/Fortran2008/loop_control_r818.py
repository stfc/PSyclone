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
    Module containing Fortran2008 Loop_Control rule R818
"""
from fparser.two.Fortran2003 import Loop_Control as Loop_Control_2003, Forall_Header


class Loop_Control(Loop_Control_2003):  # R818
    """Fortran 2008 rule R818

    loop-control is [ , ] do-variable = scalar-int-expr , scalar-int-expr
                       [ , scalar-int-expr ]
                    or [ , ] WHILE ( scalar-logical-expr )
                    or [ , ] CONCURRENT forall-header

    Extends the Fortran2003 rule R830 with the additional CONCURRENT clause.

    The F2003 Loop_Control class would be better and more extensible
    if it called 2 classes, one for each of the above
    expressions. This would then affect the implementation of this
    class. Something like the suggestion below. However, this would
    result in a different fparser tree, see issue #416.

    F2003: While_Loop_Cntl: scalar-logical-expression, delim
    F2003: Counter_Loop_Cntl: var, lower, upper, [step], delim
    F2008: Concurrent_Loop_Cntl: conc_expr, delim
    F2018: Concurrent_Loop_Cntl: conc_expr, local_x, delim

    """

    subclass_names = []
    # This class' match method makes use of the Fortran2003 match
    # method so 'use_names' should include any classes used within
    # there as well as any used here.
    use_names = Loop_Control_2003.use_names[:]
    use_names.append("Forall_Header")

    @staticmethod
    def match(string):
        """Attempts to match the supplied text with this rule.

        :param str string: Fortran code to check for a match.

        :returns: None if there is no match, a tuple with the first \
            entry providing the result of matching the 'WHILE' part of \
            the rule if there is a match, the second entry providing \
            the result of matching the 'COUNTER' part of the rule if \
            there is a match, the third entry indicating whether \
            there is an optional preceding ',' and the fourth entry \
            providing the result of matching the 'CONCURRENT' part of \
            the rule if there is a match.

        :rtype: Optional[Tuple[ \
            Optional[ \
                :py:class:`fparser.two.Fortran2003.Scalar_Logical_Expr`], \
            Optional[Tuple[ \
                :py:class:`fparser.two.Fortran2003.Do_Variable`, List[str]]], \
            Optional[str], \
            Optional[:py:class:`fparser.two.Fortran2003.Forall_Header`]]]

        """
        # Fortran2003 matches all but CONCURRENT so try this first
        result = Loop_Control_2003.match(string)
        if result:
            return result + (None,)
        # Try to match with CONCURRENT
        line = string.lstrip()
        optional_delim = None
        if line.startswith(","):
            line = line[1:].lstrip()
            optional_delim = ","
        if line[:10].upper() != "CONCURRENT":
            return None
        return (None, None, optional_delim, Forall_Header(line[10:].lstrip().rstrip()))

    def tostr(self):
        """
        :returns: the Fortran representation of this object.
        :rtype: str
        """
        if self.items[0] or self.items[1]:
            # Use the F2003 tostr() implementation
            return Loop_Control_2003.tostr(self)
        # Return loop control construct containing "CONCURRENT" clause
        loopctrl = f"CONCURRENT {self.items[3]}"
        # Add optional delimiter to loop control construct if present
        if self.items[2]:
            loopctrl = f"{self.items[2]} {loopctrl}"
        return loopctrl
