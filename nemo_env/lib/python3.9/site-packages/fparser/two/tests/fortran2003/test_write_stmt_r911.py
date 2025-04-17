# This Python file uses the following encoding: utf-8
# Copyright (c) 2020 Science and Technology Facilities Council.
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

""" pytest module for the Fortran2003 Write Statement - R911. """

import pytest
from fparser.two.utils import NoMatchError
from fparser.two.Fortran2003 import Write_Stmt, Io_Control_Spec_List


@pytest.mark.usefixtures("f2003_create")
def test_write_stmt():
    """Tests for various forms of WRITE statement (R911)."""
    tcls = Write_Stmt
    obj = tcls('write (123)"hey"')
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == 'WRITE(123) "hey"'
    assert repr(obj).replace("u'", "'") == (
        "Write_Stmt(Io_Control_Spec_List(',', (Io_Control_Spec(None, "
        "Int_Literal_Constant('123', None)),)), Output_Item_List(',', "
        "(Char_Literal_Constant('\"hey\"', None),)))"
    )

    obj = tcls('WRITE (*,"(I3)") my_int')
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == 'WRITE(*, "(I3)") my_int'
    assert repr(obj).replace("u'", "'") == (
        "Write_Stmt(Io_Control_Spec_List(',', (Io_Control_Spec(None, "
        "Io_Unit('*')), Io_Control_Spec(None, "
        "Char_Literal_Constant('\"(I3)\"', None)))), Output_Item_List(',', "
        "(Name('my_int'),)))"
    )

    obj = tcls("WRITE (*,namtest)")
    assert isinstance(obj, tcls), repr(obj)
    assert str(obj) == "WRITE(*, namtest)"
    assert repr(obj).replace("u'", "'") == (
        "Write_Stmt(Io_Control_Spec_List(',', "
        "(Io_Control_Spec(None, Io_Unit('*')), Io_Control_Spec(None, "
        "Name('namtest')))), None)"
    )

    # Test when format specifier contains an '=' character
    iolist = Io_Control_Spec_List("*,'(5X,\"q_mesh =\",4F12.8)'")
    assert isinstance(iolist, Io_Control_Spec_List)
    obj = tcls("WRITE(*,'(5X,\"q_mesh =\",1F12.8)') 1.d0")
    assert isinstance(obj, tcls)
    assert repr(obj).replace("u'", "'") == (
        "Write_Stmt(Io_Control_Spec_List(',', (Io_Control_Spec(None, "
        "Io_Unit('*')), Io_Control_Spec(None, "
        "Char_Literal_Constant('\\'(5X,\"q_mesh =\",1F12.8)\\'', None)))), "
        "Output_Item_List(',', (Real_Literal_Constant('1.D0', None),)))"
    )

    obj = tcls("WRITE(*,FMT='(5X,\"q_mesh =\",1F12.8)') 1.d0")
    assert isinstance(obj, tcls)
    assert repr(obj).replace("u'", "'") == (
        "Write_Stmt(Io_Control_Spec_List(',', (Io_Control_Spec(None, "
        "Io_Unit('*')), Io_Control_Spec('FMT', "
        "Char_Literal_Constant('\\'(5X,\"q_mesh =\",1F12.8)\\'', None)))), "
        "Output_Item_List(',', (Real_Literal_Constant('1.D0', None),)))"
    )

    # Format specifier contains an '=' and is built using concatenation
    obj = tcls('''WRITE (6, '("write = some=""'//'text'//'""")')''')
    assert isinstance(obj, tcls)
    assert str(obj) == '''WRITE(6, '("write = some=""' // 'text' // '""")')'''
    obj_repr = repr(obj)
    obj_repr = obj_repr.replace('u"', '"')
    assert obj_repr.replace("u'", "'") == (
        "Write_Stmt(Io_Control_Spec_List(',', "
        "(Io_Control_Spec(None, Int_Literal_Constant('6', None)), "
        "Io_Control_Spec(None, Level_3_Expr(Level_3_Expr("
        "Char_Literal_Constant('\\'(\"write = some=\"\"\\'', None), '//', "
        "Char_Literal_Constant(\"'text'\", None)), '//', "
        "Char_Literal_Constant('\\'\"\"\")\\'', None))))), None)"
    )


@pytest.mark.usefixtures("f2003_create")
def test_named_unit_before_fmt_error():
    """Check that we reject a WRITE that names the io-unit argument but
    still has a positional format argument (containing an '=').
    TODO #267. This test needs expanding and probably moving to a file
    dedicated to R913 and its (many) constraints.

    """
    tcls = Write_Stmt
    # Cannot have an un-named (positional) argument after a named argument
    with pytest.raises(NoMatchError):
        tcls('''WRITE (UNIT=6, '("write some=""'//'text'//'""")')''')
