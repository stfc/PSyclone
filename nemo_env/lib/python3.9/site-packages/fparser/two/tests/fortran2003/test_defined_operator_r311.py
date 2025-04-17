# Copyright (c) 2019 Science and Technology Facilities Council

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

"""Test Fortran 2003 rule R311 : This file tests the support for a
defined operator.

"""

import pytest
from fparser.two.Fortran2003 import Defined_Operator
from fparser.two.utils import NoMatchError


def test_defined_operator(f2003_create):
    """Check that correct defined operator input is parsed correctly. No
    need to test all options as they will be tested by the
    subclasses.

    """
    # defined unary or binary op
    myinput = ".inv."
    ast = Defined_Operator(myinput)
    assert myinput.upper() in str(ast)
    assert repr(ast) == "Defined_Op('{0}')".format(myinput.upper())
    # extended intrinsic op
    myinput = "**"
    ast = Defined_Operator(myinput)
    assert myinput.upper() in str(ast)
    assert repr(ast) == "Extended_Intrinsic_Op('{0}')".format(myinput)


def test_parse_errors(f2003_create):
    """The full set of errors will be checked by the subclass tests.  We
    just check a few here to make sure the correct NoMatchError
    exception is raised at this level.

    """
    for myinput in ["", "  ", ".inv", "inv.", ".false.", "***"]:
        with pytest.raises(NoMatchError) as excinfo:
            _ = Defined_Operator(myinput)
        assert "Defined_Operator: '{0}'".format(myinput) in str(excinfo.value)
