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
    Module containing Fortran2008 Component_Attr_Spec rule R437
"""
from fparser.two.Fortran2003 import Component_Attr_Spec as Component_Attr_Spec_2003


class Component_Attr_Spec(Component_Attr_Spec_2003):  # R437
    """
    Fortran 2008 rule R437.

    .. code-block:: fortran

        component-attr-spec is access-spec
                               or ALLOCATABLE
                               or CODIMENSION lbracket coarray-spec rbracket
                               or CONTIGUOUS
                               or DIMENSION ( component-array-spec )
                               or POINTER

    In the spec above, lbracket and rbracket are left and right square
    brackets `[]` but not printed explicitly to avoid misinterpretation
    as optional parts.

    This rule adds CODIMENSION and CONTIGUOUS attributes to Fortran2003's R441.

    """

    subclass_names = Component_Attr_Spec_2003.subclass_names[:]
    subclass_names.append("Codimension_Attr_Spec")
    attributes = Component_Attr_Spec_2003.attributes[:]
    attributes.append("CONTIGUOUS")
