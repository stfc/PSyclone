# Copyright (c) 2022-2024 Science and Technology Facilities Council.

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

"""Test Fortran 2008 rule R904

   open-stmt is OPEN ( connect-spec-list )

   R905 connect-spec is [ UNIT = ] file-unit-number
                    or ACCESS = scalar-default-char-expr
                    or ACTION = scalar-default-char-expr
                    or ASYNCHRONOUS = scalar-default-char-expr
                    or BLANK = scalar-default-char-expr
                    [ or CONVERT = scalar-default-char-expr ]
                    or DECIMAL = scalar-default-char-expr
                    or DELIM = scalar-default-char-expr
                    or ENCODING = scalar-default-char-expr
                    or ERR = label
                    or FILE = file-name-expr
                    or FORM = scalar-default-char-expr
                    or IOMSG = iomsg-variable
                    or IOSTAT = scalar-int-variable
                    or NEWUNIT = scalar-int-variable
                    or PAD = scalar-default-char-expr
                    or POSITION = scalar-default-char-expr
                    or RECL = scalar-int-expr
                    or ROUND = scalar-default-char-expr
                    or SIGN = scalar-default-char-expr
                    or STATUS = scalar-default-char-expr

Note that the CONVERT argument is non-standard but supported by several major
compilers.

R906 file-name-expr is scalar-default-char-expr
R907 iomsg-variable is scalar-default-char-variable

C903 No specifier shall appear more than once in a given connect-spec-list.
C904 (R904) If the NEWUNIT= specifier does not appear, a file-unit-number
     shall be specified; if the optional characters UNIT= are omitted, the
     file-unit-number shall be the first item in the connect-spec-list.
C905 (R904) The label used in the ERR= specifier shall be the statement label
     of a branch target statement that appears in the same inclusive scope as
     the OPEN statement.
C906 (R904) If a NEWUNIT= specifier appears, a file-unit-number shall not
     appear.

"""

import pytest
from fparser.api import get_reader
from fparser.two import Fortran2008
from fparser.two.utils import NoMatchError, walk


@pytest.mark.parametrize(
    "open_args",
    [
        "unit=23, file='hello'",
        "23, file='hello', err=36",
        "23, file='hello', status='OLD'",
        "unit=23, file='hello', action='read'",
        "unit=23, file='hello', convert=endianess, action='read'",
    ],
)
@pytest.mark.usefixtures("f2008_parser")
def test_open_f2003_args(open_args):
    """Check that the Fortran2008 version of Open_Stmt still supports the
    various arguments defined in 2003."""
    obj = Fortran2008.Open_Stmt(f"open({open_args})")
    assert isinstance(obj, Fortran2008.Open_Stmt)


def test_open_newunit(f2008_parser):
    """Test that NEWUNIT is a valid argument to OPEN."""
    tree = f2008_parser(
        get_reader(
            """\
subroutine myopen( unit, file )
  integer, intent(out):: unit
  character(len=*), intent(in):: file
  open( newunit=unit, file=file )
endsubroutine myopen
    """
        )
    )
    open_stmts = walk(tree, Fortran2008.Open_Stmt)
    assert open_stmts
    assert len(open_stmts[0].children) == 2
    assert str(open_stmts[0]) == "OPEN(NEWUNIT = unit, FILE = file)"


@pytest.mark.usefixtures("f2008_parser")
def test_constraint_903():
    """Check that Constraint 903 (no specifier shall appear more than once)
    is applied."""
    with pytest.raises(NoMatchError):
        Fortran2008.Open_Stmt("open(23, unit=24, file='hello')")
    with pytest.raises(NoMatchError):
        Fortran2008.Open_Stmt("open(23, file='hello', file='another')")


@pytest.mark.usefixtures("f2008_parser")
def test_constraint_904():
    """Check that a unit number is specified. We cannot currently check that
    if no UNIT= appears that it is the first argument to open() because the
    Connect_Spec.match() method always adds a UNIT= if one is missing."""
    with pytest.raises(NoMatchError):
        Fortran2008.Open_Stmt("open(file='hello')")


@pytest.mark.usefixtures("f2008_parser")
def test_constraint_906():
    """Check that Constraint 906 is applied (no unit number may appear if
    NEWUNIT is specified)."""
    with pytest.raises(NoMatchError):
        Fortran2008.Open_Stmt("open(23, newunit=10, file='hello')")
    with pytest.raises(NoMatchError):
        Fortran2008.Open_Stmt("open(unit=23, newunit=10, file='hello')")


@pytest.mark.usefixtures("f2008_parser")
def test_open_invalid_arg():
    """Check that there is no match if an invalid argument is supplied."""
    with pytest.raises(NoMatchError) as err:
        Fortran2008.Open_Stmt("open(newunit=10, file='hello', andy='yes')")
    assert "andy='yes'" in str(err)
