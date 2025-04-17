# Modified work Copyright (c) 2017-2022 Science and Technology
# Facilities Council.
# Original work Copyright (c) 1999-2008 Pearu Peterson
#
# All rights reserved.
#
# Modifications made as part of the fparser project are distributed
# under the following license:
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.
#
# 3. Neither the name of the copyright holder nor the names of its
# contributors may be used to endorse or promote products derived from
# this software without specific prior written permission.
#
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

# --------------------------------------------------------------------

# The original software (in the f2py project) was distributed under
# the following license:

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:

#   a. Redistributions of source code must retain the above copyright notice,
#      this list of conditions and the following disclaimer.
#   b. Redistributions in binary form must reproduce the above copyright
#      notice, this list of conditions and the following disclaimer in the
#      documentation and/or other materials provided with the distribution.
#   c. Neither the name of the F2PY project nor the names of its
#      contributors may be used to endorse or promote products derived from
#      this software without specific prior written permission.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR
# ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
# DAMAGE.

import pytest
from fparser.api import get_reader
from fparser.two.Fortran2003 import Associate_Construct
from fparser.two.utils import FortranSyntaxError


@pytest.mark.parametrize(
    "code, expected_string",
    [
        (
            """\
            ASSOCIATE ( Z => EXP(-(X**2+Y**2)) * COS(THETA) )
                PRINT *, A+Z, A-Z
            END ASSOCIATE
            """,
            "ASSOCIATE(Z => EXP(- (X ** 2 + Y ** 2)) * COS(THETA))\n"
            "  PRINT *, A + Z, A - Z\nEND ASSOCIATE",
        ),
        (
            """\
            name:ASSOCIATE ( XC => AX%B(I,J)%C )
                XC%DV = XC%DV + PRODUCT(XC%EV(1:N))
            END ASSOCIATE name
            """,
            "name:ASSOCIATE(XC => AX % B(I, J) % C)\n  XC % DV = XC % DV + "
            "PRODUCT(XC % EV(1 : N))\nEND ASSOCIATE name",
        ),
        (
            """\
            ASSOCIATE ( W => RESULT(I,J)%W, ZX => AX%B(I,J)%D, ZY => AY%B(I,J)%D )
                W = ZX*X + ZY*Y
            END ASSOCIATE
            """,
            "ASSOCIATE(W => RESULT(I, J) % W, ZX => AX % B(I, J) % D, ZY => "
            "AY % B(I, J) % D)\n  W = ZX * X + ZY * Y\nEND ASSOCIATE",
        ),
    ],
)
def test_associate_construct(fake_symbol_table, code, expected_string):
    """Test some basic 'associate' constructs are parsed correctly"""
    obj = Associate_Construct(get_reader(code))
    assert isinstance(obj, Associate_Construct), repr(obj)
    assert str(obj) == expected_string


def test_end_block_missing_name(f2003_create, fake_symbol_table):
    """Check that a named associate block has a name at the end"""
    with pytest.raises(FortranSyntaxError) as exc_info:
        Associate_Construct(
            get_reader(
                """\
                name:associate (xc => ax%b(i,j)%c)
                    xc%dv = xc%dv + product(xc%ev(1:n))
                end associate
                """
            )
        )
    assert exc_info.value.args[0].endswith("Expecting name 'name' but none given")


def test_end_block_wrong_name(f2003_create, fake_symbol_table):
    """Check that a named associate block has the correct name at the end"""
    with pytest.raises(FortranSyntaxError) as exc_info:
        Associate_Construct(
            get_reader(
                """\
                name:associate (xc => ax%b(i,j)%c)
                    xc%dv = xc%dv + product(xc%ev(1:n))
                end associate wrong
                """
            )
        )
    assert exc_info.value.args[0].endswith("Expecting name 'name', got 'wrong'")
