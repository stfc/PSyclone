# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2024, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified I. Kavcic, Met Office
# -----------------------------------------------------------------------------

''' Performs py.test tests on the support for KIND parameters in the
    fparser2 PSyIR front-end '''

import pytest

from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003

from psyclone.psyir.frontend.fparser2 import (Fparser2Reader,
                                              _kind_find_or_create)
from psyclone.psyir.nodes import KernelSchedule
from psyclone.psyir.symbols import (
    DataSymbol, ScalarType, UnsupportedFortranType, RoutineSymbol, SymbolTable,
    Symbol, UnresolvedType, ContainerSymbol, UnresolvedInterface)


def process_declarations(code):
    '''
    Utility routine to create PSyIR for Fortran variable declarations.

    :param str code: Fortran declaration statement(s)

    :returns: a 2-tuple consisting of a KernelSchedule with populated Symbol \
              Table and the parse tree for the specification part.
    :rtype: (:py:class:`psyclone.psyir.nodes.KernelSchedule`, \
             :py:class:`fparser.two.Fortran2003.Specification_Part`)
    '''
    sched = KernelSchedule("dummy_schedule")
    # Add a wildcard import to allow us to get away with undeclared symbols
    # in the tests.
    sched.symbol_table.add(ContainerSymbol("some_mod", wildcard_import=True))
    processor = Fparser2Reader()
    reader = FortranStringReader(code)
    fparser2spec = Fortran2003.Specification_Part(reader).content
    processor.process_declarations(sched, fparser2spec, [])
    return sched, fparser2spec


@pytest.mark.usefixtures("f2008_parser")
def test_process_declarations_kind_new_param():
    ''' Test that process_declarations handles variables declared with
    an explicit KIND parameter that has not already been declared. Also
    check that the matching on the variable name is not case sensitive.

    '''
    fake_parent, fp2spec = process_declarations("real(kind=wp) :: var1\n"
                                                "real(kind=Wp) :: var2\n")
    var1_var = fake_parent.symbol_table.lookup("var1")
    assert isinstance(var1_var.datatype.precision, DataSymbol)
    # Check that this has resulted in the creation of a new 'wp' symbol
    wp_var = fake_parent.symbol_table.lookup("wp")
    assert wp_var.datatype.intrinsic == ScalarType.Intrinsic.INTEGER
    assert var1_var.datatype.precision is wp_var
    # Check that, despite the difference in case, the second variable
    # references the same 'wp' symbol.
    var2_var = fake_parent.symbol_table.lookup("var2")
    assert var2_var.datatype.precision is wp_var
    # Check that we get a symbol of unsupported type if the KIND expression has
    # an unexpected structure
    # Break the parse tree by changing Name('wp') into a str
    fp2spec[0].items[0].items[1].items = ("(", "blah", ")")
    # Change the variable name too to prevent a clash
    fp2spec[0].children[2].children[0].items[0].string = "var3"
    processor = Fparser2Reader()
    processor.process_declarations(fake_parent, [fp2spec[0]], [])
    sym = fake_parent.symbol_table.lookup("var3")
    assert isinstance(sym, DataSymbol)
    assert isinstance(sym.datatype, UnsupportedFortranType)


@pytest.mark.xfail(reason="Kind parameter declarations not supported - #569")
@pytest.mark.usefixtures("f2008_parser")
def test_process_declarations_kind_param():
    ''' Test that process_declarations handles the kind attribute when
    it specifies a previously-declared symbol.

    '''
    fake_parent = KernelSchedule("dummy_schedule")
    processor = Fparser2Reader()
    reader = FortranStringReader("integer, parameter :: r_def = KIND(1.0D0)\n"
                                 "real(kind=r_def) :: var2")
    fparser2spec = Fortran2003.Specification_Part(reader)
    processor.process_declarations(fake_parent, fparser2spec.content, [])
    assert isinstance(fake_parent.symbol_table.lookup("var2").precision,
                      DataSymbol)


@pytest.mark.usefixtures("f2008_parser")
def test_process_declarations_kind_use():
    ''' Test that process_declarations handles the kind attribute when
    it specifies a symbol accessed via a USE.

    '''
    fake_parent, _ = process_declarations("use kind_mod, only: r_def\n"
                                          "real(kind=r_def) :: var2")
    var2_var = fake_parent.symbol_table.lookup("var2")
    assert isinstance(var2_var.datatype.precision, DataSymbol)
    assert fake_parent.symbol_table.lookup("r_def") is \
        var2_var.datatype.precision

    # If we change the symbol_table default visibility, this is respected
    # by new kind symbols
    fake_parent.symbol_table.default_visibility = Symbol.Visibility.PRIVATE
    _kind_find_or_create("i_def", fake_parent.symbol_table)
    assert (fake_parent.symbol_table.lookup("i_def").visibility
            is Symbol.Visibility.PRIVATE)


@pytest.mark.usefixtures("f2008_parser")
def test_kind_param_unsupportedtype():
    ''' Check that the _kind_find_or_create() routine preserves the type of
    a Symbol representing a kind parameter if it is of UnsupportedType. '''
    symbol_table = SymbolTable()
    wp_sym = DataSymbol(
        "wp", UnsupportedFortranType("integer, parameter :: wp = dp"))
    symbol_table.add(wp_sym)
    kind_sym = _kind_find_or_create("wp", symbol_table)
    assert kind_sym is wp_sym
    assert wp_sym.datatype.declaration == "integer, parameter :: wp = dp"


@pytest.mark.usefixtures("f2008_parser")
def test_kind_param_unresolvedtype():
    ''' Check that the _kind_find_or_create() routine changes the type of
    a Symbol representing a kind parameter if it is of UnresolvedType. '''
    symbol_table = SymbolTable()
    wp_sym = DataSymbol(
        "wp", UnresolvedType(), interface=UnresolvedInterface())
    symbol_table.add(wp_sym)
    kind_sym = _kind_find_or_create("wp", symbol_table)
    assert kind_sym is wp_sym
    assert wp_sym.datatype.intrinsic == ScalarType.Intrinsic.INTEGER


@pytest.mark.usefixtures("f2008_parser")
def test_wrong_type_kind_param():
    ''' Check that we raise the expected error if a variable used as a KIND
    specifier is not a DataSymbol or has already been declared with non-integer
    type.

    '''
    fake_parent, _ = process_declarations("integer, parameter :: r_def=4\n"
                                          "real(kind=r_def) :: var2")
    r_def = fake_parent.symbol_table.lookup("r_def")
    # Monkeypatch this DataSymbol so that it appears to be a RoutineSymbol
    r_def.__class__ = RoutineSymbol
    with pytest.raises(TypeError) as err:
        _kind_find_or_create("r_def", fake_parent.symbol_table)
    assert ("found an entry of type 'RoutineSymbol' for variable 'r_def'" in
            str(err.value))
    # Repeat but declare r_def as real
    with pytest.raises(TypeError) as err:
        process_declarations("real :: r_def\n"
                             "real(kind=r_def) :: var2")
    assert ("already contains a DataSymbol for variable 'r_def'" in
            str(err.value))


@pytest.mark.parametrize("vartype, kind, precision",
                         [("real", "1.0d0", ScalarType.Precision.DOUBLE),
                          ("real", "1.0D7", ScalarType.Precision.DOUBLE),
                          ("real", "1_t_def", None),
                          ("real", "1.0", ScalarType.Precision.UNDEFINED),
                          ("real", "1.0E3", ScalarType.Precision.SINGLE),
                          # 32-bit integer
                          ("integer", "1", ScalarType.Precision.UNDEFINED),
                          # 64-bit integer
                          ("integer", str(1 << 31 + 4)+"_t_def", None)])
@pytest.mark.usefixtures("f2008_parser")
def test_process_declarations_kind_literals(vartype, kind, precision):
    ''' Test that process_declarations handles variables declared with
    an explicit KIND specified using a literal constant.

    '''
    fake_parent, _ = process_declarations(f"{vartype}(kind=KIND({kind})) :: "
                                          f"var")
    if not precision:
        assert fake_parent.symbol_table.lookup("var").datatype.precision is \
            fake_parent.symbol_table.lookup("t_def")
    else:
        assert (fake_parent.symbol_table.lookup("var").datatype.precision ==
                precision)


@pytest.mark.parametrize("vartype, kind",
                         [("logical", ".false."),
                          ("real", "-1.0D7"),
                          ("real", "kvar"),
                          ("real", "kvar(1)")])
@pytest.mark.usefixtures("f2008_parser")
def test_unsupported_kind(vartype, kind):
    ''' Check that we get an unsupported type for an unsupported kind
        specifier.
        TODO #569 - add support for some/all of these.

    '''
    sched, _ = process_declarations(f"{vartype}(kind=KIND({kind})) :: var")
    assert isinstance(sched.symbol_table.lookup("var").datatype,
                      UnsupportedFortranType)
