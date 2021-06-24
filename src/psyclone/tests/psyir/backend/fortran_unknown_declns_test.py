# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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
# Author A. R. Porter, STFC Daresbury Lab
# -----------------------------------------------------------------------------

'''Performs pytest tests on the support for declarations of unknown type in
   the psyclone.psyir.backend.fortran module'''

from __future__ import absolute_import
import pytest

from psyclone.psyir.backend.visitor import VisitorError
from psyclone.psyir.nodes import Container
from psyclone.psyir.symbols import Symbol, DataSymbol, RoutineSymbol, \
    UnknownType, UnknownFortranType


def test_fw_unknown_decln_error(monkeypatch, fortran_writer):
    ''' Check that the FortranWriter raises the expected error if it
    encounters an UnknownType that is not an UnknownFortranType. '''
    # We can't create an UnknownType() object directly as it is abstract.
    # Therefore we create a symbol of UnknownFortranType and then
    # monkeypatch it.
    sym = DataSymbol("b", UnknownFortranType("int b;"))
    monkeypatch.setattr(sym.datatype, "__class__", UnknownType)
    with pytest.raises(VisitorError) as err:
        fortran_writer.gen_vardecl(sym)
    assert ("cannot handle the declaration of a symbol of 'UnknownType'" in
            str(err.value))


def test_fw_unknown_decln(fortran_writer):
    ''' Check that the FortranWriter recreates a declaration that is of
    UnknownFortranType. '''
    sym = DataSymbol("b", UnknownFortranType("integer, value :: b"))
    assert "integer, value :: b" in fortran_writer.gen_vardecl(sym)


def test_fw_unknown_interface_decln(fortran_writer):
    ''' Check that the backend recreates an interface declaration stored as
    an unknown type and adds an appropriate access statement. '''
    container = Container("my_mod")
    interface_code = ("interface eos\n"
                      "  module procedure eos1d, eos2d\n"
                      "end interface")
    sym = RoutineSymbol("eos", UnknownFortranType(interface_code),
                        visibility=Symbol.Visibility.PRIVATE)
    assert interface_code in fortran_writer.gen_vardecl(sym)
    container.symbol_table.add(sym)
    code = fortran_writer(container)
    assert interface_code in code
    assert "private :: eos\n" in code
    container.symbol_table.add(
        RoutineSymbol("my_sub", visibility=Symbol.Visibility.PUBLIC))
    code = fortran_writer(container)
    assert "private :: eos\n" in code
    assert "public :: my_sub\n" in code
