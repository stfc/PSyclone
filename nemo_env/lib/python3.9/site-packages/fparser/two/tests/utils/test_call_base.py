# Copyright (c) 2023 Science and Technology Facilities Council.

# All rights reserved.

# Modifications made as part of the fparser project are distributed
# under the following license:

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:

# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.

# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.

# 3. Neither the name of the copyright holder nor the names of its
# contributors may be used to endorse or promote products derived from
# this software without specific prior written permission.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

""" Module containing pytest tests for the fparser2 CallBase class. """

import pytest
from fparser.two import Fortran2003
from fparser.two import utils


@pytest.mark.usefixtures("f2003_create")
def test_call_base_match():
    """Check that parent information is correctly set-up in the
    parse tree."""
    # Basic match is OK.
    assert utils.CallBase.match(
        Fortran2003.Procedure_Designator, Fortran2003.Actual_Arg_Spec_List, "ogg()"
    )
    # Trailing white space is ignored.
    assert utils.CallBase.match(
        Fortran2003.Procedure_Designator, Fortran2003.Actual_Arg_Spec_List, "ogg()  "
    )
    # String doesn't end with ')'
    assert not utils.CallBase.match(
        Fortran2003.Procedure_Designator, Fortran2003.Actual_Arg_Spec_List, "ogg"
    )
    # Missing opening '('
    assert not utils.CallBase.match(
        Fortran2003.Procedure_Designator, Fortran2003.Actual_Arg_Spec_List, "ogg)"
    )
    # Missing lhs.
    assert not utils.CallBase.match(
        Fortran2003.Procedure_Designator, Fortran2003.Actual_Arg_Spec_List, "(ogg)"
    )
    # Additional, matching parentheses are OK.
    assert utils.CallBase.match(
        Fortran2003.Procedure_Designator,
        Fortran2003.Actual_Arg_Spec_List,
        "ogg((nanny) )",
    )
    # upper_lhs makes no difference when lhs is matched with a class.
    assert utils.CallBase.match(
        Fortran2003.Procedure_Designator,
        Fortran2003.Actual_Arg_Spec_List,
        "ogg()",
        upper_lhs=True,
    )
    # upper_lhs is respected when lhs is matched with a str
    assert not utils.CallBase.match(
        "ogg", Fortran2003.Actual_Arg_Spec_List, "ogg()  ", upper_lhs=True
    )
    assert utils.CallBase.match(
        "OGG", Fortran2003.Actual_Arg_Spec_List, "ogg()  ", upper_lhs=True
    ) == ("OGG", None)
    # rhs can be matched using a str
    assert utils.CallBase.match(
        "ogg",
        "nanny",
        "ogg(nanny)",
    )
    assert not utils.CallBase.match(
        "ogg",
        "nanny",
        "ogg(granny)",
    )
    # require_rhs is respected
    assert utils.CallBase.match("ogg", "nanny", "ogg(nanny)", require_rhs=False) == (
        "ogg",
        "nanny",
    )
    assert utils.CallBase.match("ogg", "nanny", "ogg(nanny)", require_rhs=True) == (
        "ogg",
        "nanny",
    )
    assert not utils.CallBase.match("ogg", "nanny", "ogg()", require_rhs=True)


@pytest.mark.usefixtures("f2003_create")
def test_call_base_tostr():
    """Test the tostr() method of CallBase."""
    fref = Fortran2003.Function_Reference("gytha(ogg)")
    assert fref.tostr() == "gytha(ogg)"
    fref = Fortran2003.Function_Reference("weatherwax( )")
    assert fref.tostr() == "weatherwax()"
