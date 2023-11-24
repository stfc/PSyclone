# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2023, Science and Technology Facilities Council.
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
# -----------------------------------------------------------------------------

''' Perform py.test tests on the psygen.psyir.symbols.routinesymbol file '''

import os
import pytest

from psyclone.configuration import Config
from psyclone.psyir import nodes
from psyclone.psyir.symbols import (
    RoutineSymbol, Symbol, SymbolError, UnresolvedInterface,
    NoType, INTEGER_TYPE, DeferredType, DataTypeSymbol)


def test_routinesymbol_init():
    '''Test that a RoutineSymbol instance can be created.'''
    # A RoutineSymbol should be of type NoType by default.
    jo_sym = RoutineSymbol('jo')
    assert isinstance(jo_sym, RoutineSymbol)
    assert isinstance(jo_sym.datatype, NoType)
    # By default we don't know whether a symbol is pure or elemental.
    assert jo_sym.is_pure is None
    assert jo_sym.is_elemental is None
    ellie_sym = RoutineSymbol('ellie', INTEGER_TYPE,
                              visibility=Symbol.Visibility.PRIVATE)
    assert isinstance(ellie_sym, RoutineSymbol)
    assert ellie_sym.datatype == INTEGER_TYPE
    isaac_sym = RoutineSymbol('isaac', DeferredType(),
                              interface=UnresolvedInterface())
    assert isinstance(isaac_sym, RoutineSymbol)
    assert isinstance(isaac_sym.datatype, DeferredType)

    tam_type = DataTypeSymbol('tam_type', DeferredType())
    tam_sym = RoutineSymbol('tam', tam_type)
    assert isinstance(tam_sym, RoutineSymbol)
    assert tam_sym.datatype is tam_type
    # Check that is_pure and is_elemental can be specified.
    marvin_sym = RoutineSymbol('marvin', DeferredType(), is_pure=True)
    assert marvin_sym.is_pure is True
    paranoid_sym = RoutineSymbol('paranoid', DeferredType(),
                                 is_elemental=False)
    assert paranoid_sym.is_elemental is False


def test_routinesymbol_init_error():
    '''Test that the RoutineSymbol raises an error (via the Symbol parent
    class) if there is an invalid argument.

    '''
    with pytest.raises(TypeError) as error:
        _ = RoutineSymbol(None)
    assert ("RoutineSymbol 'name' attribute should be of type 'str' but "
            "'NoneType' found." in str(error.value))
    with pytest.raises(TypeError) as error:
        _ = RoutineSymbol("isaac", "integer")
    assert ("datatype of a RoutineSymbol must be specified using either a "
            "DataType or a DataTypeSymbol but got: 'str'" in str(error.value))
    with pytest.raises(TypeError) as error:
        _ = RoutineSymbol("android", DeferredType(), is_pure="maybe")
    assert ("is_pure for a RoutineSymbol must be a bool or None but got "
            "'str'" in str(error.value))
    with pytest.raises(TypeError) as error:
        _ = RoutineSymbol("android", DeferredType(), is_elemental="maybe")
    assert ("is_elemental for a RoutineSymbol must be a bool or None but got "
            "'str'" in str(error.value))


def test_routinesymbol_specialise_and_process_arguments():
    ''' Tests that a RoutineSymbol created from a specialisation instead of
    the constructor deals with the arguments as expected.'''

    # Try to make a RoutineSymbol without a datatype
    sym1 = Symbol("symbol1")
    sym1.specialise(RoutineSymbol)
    # pylint gets confused because it doesn't know about specialise()
    # pylint: disable=no-member
    assert isinstance(sym1.datatype, NoType)

    # Include a datatype
    sym2 = Symbol("symbol2")
    sym2.specialise(RoutineSymbol, datatype=INTEGER_TYPE)
    assert sym2.datatype is INTEGER_TYPE

    # Include is_pure
    sym3 = Symbol("sym3")
    sym3.specialise(RoutineSymbol, is_pure=False)
    assert sym3.is_pure is False

    # Include is_elemental
    sym4 = Symbol("sym4")
    sym4.specialise(RoutineSymbol, is_elemental=True)
    assert sym4.is_elemental is True


def test_routinesymbol_str():
    '''Test that the __str__ method in routinesymbol behaves as expected.'''
    routine_symbol = RoutineSymbol("roo")
    assert (str(routine_symbol) == "roo: RoutineSymbol<NoType, "
            "pure=unknown, elemental=unknown>")
    routine_symbol = RoutineSymbol("roo", INTEGER_TYPE)
    assert (str(routine_symbol) ==
            "roo: RoutineSymbol<Scalar<INTEGER, UNDEFINED>, pure=unknown, "
            "elemental=unknown>")
    type_sym = DataTypeSymbol("some_type", DeferredType())
    routine_symbol = RoutineSymbol("roo", type_sym, is_elemental=True,
                                   is_pure=True)
    assert (str(routine_symbol) ==
            "roo: RoutineSymbol<some_type: DataTypeSymbol, pure=True, "
            "elemental=True>")
    routine_symbol = RoutineSymbol("eyore", type_sym, is_elemental=False,
                                   is_pure=True)
    assert (str(routine_symbol) ==
            "eyore: RoutineSymbol<some_type: DataTypeSymbol, pure=True, "
            "elemental=False>")


def test_get_routine_recursive(fortran_reader, fortran_writer, tmpdir,
                               monkeypatch):
    '''
    Test get_routine() when the implementation of the routine is not in the
    immediately-imported Container.

    '''
    code = '''\
    module a_mod
      use my_mod, only: my_sub
    contains
      subroutine a_sub()
        real, dimension(10) :: a
        call my_sub(a)
      end subroutine a_sub
    end module a_mod
    '''
    # Create the modules containing the subroutine definition, write it to
    # file and set the search path so that PSyclone can find it.
    path = str(tmpdir)
    monkeypatch.setattr(Config.get(), '_include_paths', [path])

    with open(os.path.join(path, "my_mod.f90"), "w") as mfile:
        mfile.write('''\
    module my_mod
      use my_mod2, only: my_sub
    contains
      subroutine ignore_this()
      end subroutine ignore_this
    end module my_mod
    ''')
    with open(os.path.join(path, "my_mod2.f90"), "w") as mfile:
        mfile.write('''\
    module my_mod2
    contains
      subroutine my_sub(arg)
        real, dimension(10), intent(inout) :: arg
        arg(1:10) = 1.0
      end subroutine my_sub
    end module my_mod2
    ''')
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(nodes.Call)[0]
    psyir = call.routine.get_routine()
    assert isinstance(psyir, nodes.Routine)
    assigns = psyir.walk(nodes.Assignment)
    assert len(assigns) == 1
    assert assigns[0].lhs.symbol.name == "arg"


def test_get_routine_unresolved(fortran_reader):
    '''Test that get_routine() raises the expected error when the routine
    being called is unresolved.'''
    psyir = fortran_reader.psyir_from_source('''\
module my_mod
contains
subroutine my_sub()
    call external_routine()
end subroutine my_sub
end module my_mod
''')
    call = psyir.walk(nodes.Call)[0]
    with pytest.raises(NotImplementedError) as err:
        call.routine.get_routine()
    assert ("RoutineSymbol 'external_routine' is unresolved and searching for "
            "its implementation is not yet supported" in str(err.value))


def test_get_routine_missing_container(fortran_reader):
    '''
    Test that get_routine() raises the expected error if a RoutineSymbol is
    not imported but no Container has been provided in which to search.
    '''
    psyir = fortran_reader.psyir_from_source('''\
module my_mod
contains
subroutine my_sub()
  implicit none
end subroutine my_sub
end module my_mod
''')
    ctr = psyir.walk(nodes.Container)[1]
    rsym = ctr.symbol_table.lookup("my_sub")
    with pytest.raises(SymbolError) as err:
        rsym.get_routine()
    assert ("RoutineSymbol 'my_sub' is not imported so a Container node must "
            "be provided in order to search for its Schedule"
            in str(err.value))


def test_get_routine_missing_implementation(fortran_reader):
    '''Test that get_routine() raises the expected error if the implementation
    of the Routine cannot be found in the supplied Container.'''
    psyir = fortran_reader.psyir_from_source('''\
module my_mod
contains
subroutine my_sub()
  implicit none
end subroutine my_sub
end module my_mod
''')
    ctr = psyir.walk(nodes.Container)[1]
    rsym = RoutineSymbol("missing")
    ctr.symbol_table.add(rsym)
    with pytest.raises(SymbolError) as err:
        rsym.get_routine(ctr)
    assert ("Failed to find a Routine named 'missing' in Container 'my_mod'"
            in str(err.value))


def test_get_routine_interface_name(tmpdir, monkeypatch, fortran_reader):
    '''
    Test get_routine() for  a subroutine that is called via an interface with a
    different name.

    TODO #924 - this is currently unsupported.

    '''
    path = str(tmpdir)
    monkeypatch.setattr(Config.get(), '_include_paths', [path])

    with open(os.path.join(path, "my_mod.f90"), "w") as mfile:
        mfile.write('''\
    module my_mod
      interface manna
        module procedure :: manna_sp, manna_dp
      end interface manna
    contains
      subroutine manna_sp(arg)
        real(kind=kind(1.0)) :: arg
      end subroutine manna_sp
      subroutine manna_dp(arg)
        real(kind=kind(1.0d0)) :: arg
      end subroutine manna_dp
    end module my_mod
    ''')
    code = '''\
    module a_mod
      use my_mod, only: manna
    contains
      subroutine a_sub()
        real, dimension(10) :: a
        call manna(a)
      end subroutine a_sub
    end module a_mod
    '''
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(nodes.Call)[0]
    with pytest.raises(NotImplementedError) as err:
        call.routine.get_routine()
    assert ("RoutineSymbol 'manna' exists in Container 'my_mod' but is of "
            "UnknownFortranType:" in str(err.value))
