# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2026, Science and Technology Facilities Council.
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
# Authors A. B. G. Chalk STFC Daresbury Lab

'''This module contains the DataNodeToTempTrans class.'''

import os
import pytest

from psyclone.configuration import Config
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import (
    Assignment, Reference
)
from psyclone.psyir.symbols import (
    DataSymbol, INTEGER_TYPE
)
from psyclone.psyir.transformations import (
    DataNodeToTempTrans, TransformationError
)
from psyclone.tests.utilities import Compile


def test_datanodetotemptrans_validate(fortran_reader, tmp_path):
    """Tests the non-import related functionality of the validate
    function of the DataNodeToTempTrans."""
    dtrans = DataNodeToTempTrans()
    code = """subroutine test(a, b, c)
        integer, dimension(:,:), intent(inout) :: a, b, c
        c = b + a
    end subroutine test"""
    psyir = fortran_reader.psyir_from_source(code)
    assign = psyir.walk(Assignment)[0]
    with pytest.raises(TransformationError) as err:
        dtrans.validate(assign.rhs)
    assert ("Input node's datatype is an array of unknown size, so the "
            "DataNodeToTempTrans cannot be applied. Input node was "
            "'b + a'" in str(err.value))

    code = """subroutine test
        use some_mod
        c = b + a
    end subroutine test"""
    psyir = fortran_reader.psyir_from_source(code)
    assign = psyir.walk(Assignment)[0]
    with pytest.raises(TransformationError) as err:
        dtrans.validate(assign.rhs)
    assert ("Input node's datatype cannot be computed, so the "
            "DataNodeToTempTrans cannot be applied. Input node "
            "was 'b + a'. The following symbols in the input "
            "node are not resolved in the scope: '['a', 'b']'. "
            "Setting RESOLVE_IMPORTS in the transformation script "
            "may enable resolution of these symbols." in str(err.value))

    code = """subroutine test
        character(len=25) :: a, b

        b = a
    end subroutine test"""
    psyir = fortran_reader.psyir_from_source(code)
    assign = psyir.walk(Assignment)[0]
    with pytest.raises(TransformationError) as err:
        dtrans.validate(assign.rhs)
    assert ("Input node's datatype cannot be computed, so the "
            "DataNodeToTempTrans cannot be applied. Input node "
            "was 'a'" in str(err.value))

    with pytest.raises(TypeError) as err:
        dtrans.validate("abc")
    assert ("Input node to DataNodeToTempTrans should be a "
            "DataNode but got 'str'." in str(err.value))

    with pytest.raises(TypeError) as err:
        dtrans.validate(assign.rhs, storage_name=1)
    assert ("'DataNodeToTempTrans' received options with the wrong types:\n"
            "'storage_name' option expects type 'str' but received '1' of "
            "type 'int'.\nPlease see the documentation and check the "
            "provided types." in str(err.value))

    with pytest.raises(TransformationError) as err:
        dtrans.validate(Reference(DataSymbol("a", INTEGER_TYPE)))
    assert ("Input node to DataNodeToTempTrans has no ancestor Statement "
            "node which is not supported." in str(err.value))

    code = """module some_mod
    contains
    integer function some_func(a, b)
        integer :: a, b
        a = a + b
        some_func = a + b
    end function
    subroutine test()
        integer :: a, b

        a = a + some_func(a,b)
    end subroutine test
    end module"""
    psyir = fortran_reader.psyir_from_source(code)
    assign = psyir.walk(Assignment)[2]
    with pytest.raises(TransformationError) as err:
        dtrans.validate(assign.rhs)
    assert ("Input node to DataNodeToTempTrans contains a call "
            "'some_func(a, b)' that is not "
            "guaranteed to be pure. Input node is 'a + some_func(a, b)'."
            in str(err.value))


def test_datanodetotemptrans_validate_imports(
        fortran_reader, tmp_path, monkeypatch
):
    """Tests the import related functionality of the validate
    function of the DataNodeToTempTrans."""
    dtrans = DataNodeToTempTrans()
    monkeypatch.setattr(Config.get(), '_include_paths', [str(tmp_path)])
    filename = os.path.join(str(tmp_path), "a_mod.f90")
    with open(filename, "w", encoding='UTF-8') as module:
        module.write('''
        module a_mod
            use some_mod, only: i
            integer, dimension(25, i) :: some_var
        end module a_mod
        ''')
    code = """subroutine test()
        use a_mod, only: some_var
        integer, dimension(25, 50) :: b
        integer :: i
        b = some_var
        end subroutine test"""
    psyir = fortran_reader.psyir_from_source(code)
    psyir.children[0].symbol_table.resolve_imports()
    assign = psyir.walk(Assignment)[0]
    with pytest.raises(TransformationError) as err:
        dtrans.validate(assign.rhs)
    assert ("The type of the node supplied to DataNodeToTempTrans depends "
            "upon an imported symbol 'i' which has a name clash with a "
            "symbol in the current scope." in str(err.value))

    # This should work if the i in scope is imported from the
    # some_mod already.
    filename = os.path.join(str(tmp_path), "some_mod.f90")
    with open(filename, "w", encoding='UTF-8') as module:
        module.write('''
        module some_mod
            integer, parameter :: i = 50
        end module some_mod
        ''')
    code = """subroutine test()
        use some_mod, only: i
        use a_mod, only: some_var
        integer, dimension(25, 50) :: b
        b = some_var
        end subroutine test"""
    psyir = fortran_reader.psyir_from_source(code)
    psyir.children[0].symbol_table.resolve_imports()
    assign = psyir.walk(Assignment)[0]
    dtrans.validate(assign.rhs)

    # Check validation works when the shape contains a symbol from an
    # existing module
    filename = os.path.join(str(tmp_path), "a_mod.f90")
    with open(filename, "w", encoding='UTF-8') as module:
        module.write('''
        module a_mod
            use some_mod, only: i
            integer, dimension(25, i) :: some_var
        end module a_mod
        ''')
    filename = os.path.join(str(tmp_path), "some_mod2.f90")
    with open(filename, "w", encoding='UTF-8') as module:
        module.write('''
        module some_mod2
            integer, parameter :: i = 25
            integer :: j
        end module some_mod2
        ''')
    code = """subroutine test()
        use a_mod, only: some_var
        use some_mod2, only: j
        j = some_var(1,3)
        end subroutine test"""
    # We need to resolve the module in the Frontend to avoid some_Var
    # becoming a call.
    psyir = FortranReader(resolve_modules=True).psyir_from_source(code)
    assign = psyir.walk(Assignment)[0]
    dtrans.validate(assign.rhs)

    # Check validation raise an error when the shape contains a symbol from
    # a module that overlaps with a symbol in the scope.
    filename = os.path.join(str(tmp_path), "tmpmod.f90")
    with open(filename, "w", encoding='UTF-8') as module:
        module.write('''
        module tmpmod
            integer, parameter :: i = 25
            integer, parameter :: j = 30
        end module tmpmod
        ''')
    filename = os.path.join(str(tmp_path), "f_mod.f90")
    with open(filename, "w", encoding='UTF-8') as module:
        module.write('''
        module f_mod
            use tmpmod, only: i
            integer, dimension(25, i) :: some_var
        end module f_mod
        ''')
    code = """subroutine test()
        use f_mod, only: some_var
        integer :: tmpmod
        tmpmod = some_var
        end subroutine test"""
    psyir = FortranReader(resolve_modules=True).psyir_from_source(code)
    assign = psyir.walk(Assignment)[0]
    with pytest.raises(TransformationError) as err:
        dtrans.validate(assign.rhs)
    assert ("Input node contains an imported symbol 'i' whose containing "
            "module collides with an existing symbol. Colliding name is "
            "'tmpmod'."
            in str(err.value))

    filename = os.path.join(str(tmp_path), "some_other_mod.f90")
    with open(filename, "w", encoding='UTF-8') as module:
        module.write('''
        module some_other_mod
    integer, parameter :: dim1 = 4, dim2 = 5
    real(kind=wp), dimension(dim1, dim2) :: a_var
    public :: a_var
    private
end module''')
    code = """subroutine test()
    use some_other_mod, only: a_var
    integer, dimension(4, 5) :: b
    b = a_var
end subroutine test
    """
    psyir = FortranReader(resolve_modules=True).psyir_from_source(code)
    assign = psyir.walk(Assignment)[0]
    with pytest.raises(TransformationError) as err:
        dtrans.validate(assign.rhs)
    assert ("The datatype of the node suppled to DataNodeToTempTrans depends "
            "upon an imported symbol 'dim1' that is declared as private in "
            "its containing module, so cannot be imported." in str(err.value))


def test_datanodetotemptrans_apply(fortran_reader, fortran_writer, tmp_path,
                                   monkeypatch):
    """Tests the apply function of the DataNodeToTempTrans without imported
    symbols."""
    dtrans = DataNodeToTempTrans()
    code = """subroutine test()
        integer, dimension(10,100) :: a
        integer, dimension(100,10) :: b
        integer, dimension(10,10) :: c, d
        d = c + MATMUL(a, b)
    end subroutine test"""
    psyir = fortran_reader.psyir_from_source(code)
    assign = psyir.walk(Assignment)[0]
    dtrans.apply(assign.rhs.operands[1])
    out = fortran_writer(psyir)
    assert ("integer, dimension(SIZE(a, dim=1),SIZE(b, dim=2)) :: tmp"
            in out)
    assert "tmp = MATMUL(a, b)" in out
    assert "d = c + tmp" in out
    assert Compile(tmp_path).string_compiles(out)

    code = """subroutine test()
        real :: a
        integer :: b

        b = INT(a)
        end subroutine test"""
    psyir = fortran_reader.psyir_from_source(code)
    assign = psyir.walk(Assignment)[0]
    dtrans.apply(assign.rhs, storage_name="temporary")
    out = fortran_writer(psyir)
    assert "integer :: temporary" in out
    assert "temporary = INT(a)" in out
    assert "b = temporary" in out
    assert Compile(tmp_path).string_compiles(out)

    code = """subroutine test()
        real, dimension(100) :: b
        integer :: i

        do i = 1, 100
          b(i) = REAL(i)
        end do
     end subroutine test"""
    psyir = fortran_reader.psyir_from_source(code)
    assign = psyir.walk(Assignment)[0]
    dtrans.apply(assign.rhs, storage_name="temporary")
    out = fortran_writer(psyir)
    assert "  real :: temporary" in out
    assert """  do i = 1, 100, 1
    temporary = REAL(i)
    b(i) = temporary
  enddo""" in out

    code = """subroutine test()
    integer, dimension(2:6) :: a
    integer, dimension(1:3) :: b

    a(2:4) = 3 * b

    end subroutine test"""
    psyir = fortran_reader.psyir_from_source(code)
    assign = psyir.walk(Assignment)[0]
    dtrans.apply(assign.rhs)
    out = fortran_writer(psyir)
    assert "  integer, dimension(3) :: tmp" in out
    assert """  tmp = 3 * b
  a(:4) = tmp""" in out
    assert Compile(tmp_path).string_compiles(out)


def test_datanodetotemptrans_apply_imports(
        fortran_reader, fortran_writer, tmp_path, monkeypatch
):
    """Tests the apply function of the DataNodeToTempTrans with imported
    symbols.

    TODO #284: Compilation tests for these are blocked as the Compile
    utility doesn't look for imported modules yet.
    """
    dtrans = DataNodeToTempTrans()
    # Test the imports are handled correctly.
    monkeypatch.setattr(Config.get(), '_include_paths', [str(tmp_path)])
    filename = os.path.join(str(tmp_path), "a_mod.f90")
    with open(filename, "w", encoding='UTF-8') as module:
        module.write('''
        module a_mod
            integer :: some_var
        end module a_mod
        ''')
    code = """subroutine test()
        use a_mod
        integer :: b
        b = some_var
        end subroutine test"""
    psyir = fortran_reader.psyir_from_source(code)
    psyir.children[0].symbol_table.resolve_imports()
    assign = psyir.walk(Assignment)[0]
    dtrans.apply(assign.rhs)
    out = fortran_writer(psyir)
    assert """  integer :: tmp

  tmp = some_var
  b = tmp""" in out

    filename = os.path.join(str(tmp_path), "b_mod.f90")
    with open(filename, "w", encoding='UTF-8') as module:
        module.write('''
        module b_mod
            integer, dimension(25, 50) :: some_var
        end module b_mod
        ''')
    code = """subroutine test()
        use b_mod
        integer, dimension(25, 50) :: b
        b = some_var
        end subroutine test"""
    psyir = fortran_reader.psyir_from_source(code)
    psyir.children[0].symbol_table.resolve_imports()
    assign = psyir.walk(Assignment)[0]
    dtrans.apply(assign.rhs)
    out = fortran_writer(psyir)
    assert """  integer, dimension(25,50) :: tmp

  tmp = some_var
  b = tmp""" in out

    filename = os.path.join(str(tmp_path), "c_mod.f90")
    with open(filename, "w", encoding='UTF-8') as module:
        module.write('''
        module c_mod
            use some_mod, only: i
            integer, dimension(25, i) :: some_var
        end module c_mod
        ''')
    code = """subroutine test()
        use c_mod, only: some_var
        integer, dimension(25, 50) :: b
        b = some_var
        end subroutine test"""
    psyir = fortran_reader.psyir_from_source(code)
    psyir.children[0].symbol_table.resolve_imports()
    assign = psyir.walk(Assignment)[0]
    dtrans.apply(assign.rhs)
    out = fortran_writer(psyir)
    assert """  use some_mod, only : i
  integer, dimension(25,50) :: b
  integer, dimension(25,i) :: tmp

  tmp = some_var
  b = tmp""" in out

    # Check that modules in a shape from an imported module are
    # correctly added to the output if the module is already
    # present as a Container.
    filename = os.path.join(str(tmp_path), "f_mod.f90")
    with open(filename, "w", encoding='UTF-8') as module:
        module.write('''
        module f_mod
            use g_mod, only: i
            integer, dimension(25, i) :: some_var
        end module f_mod
        ''')
    filename = os.path.join(str(tmp_path), "g_mod.f90")
    with open(filename, "w", encoding='UTF-8') as module:
        module.write('''
        module g_mod
            integer, parameter :: i = 25
            integer, dimension(25, i) :: j = 30
        end module g_mod
        ''')
    code = """subroutine test()
        use g_mod, only: j
        use f_mod, only: some_var
        j = some_var
        end subroutine test"""
    # We need to resolve the module in the Frontend to avoid some_Var
    # becoming a call.
    psyir = FortranReader(resolve_modules=True).psyir_from_source(code)
    assign = psyir.walk(Assignment)[0]
    dtrans.apply(assign.rhs)
    out = fortran_writer(psyir)
    assert """  use g_mod, only : i, j
  use f_mod, only : some_var
  integer, dimension(25,i) :: tmp

  tmp = some_var
  j = tmp""" in out


def test_datanodetotemptrans_apply_nemo_example(fortran_reader,
                                                fortran_writer):
    '''Takes an example of how a call in NEMO could look and tests the
    apply method of DataNodeToTempTrans.'''
    code = """module some_mod
        use some_mod2, only: iom_put
        integer :: nis0, nie0, njs0, nje0
        real, allocatable, dimension(:,:,:) :: avt_k
        real, allocatable, dimension(:,:,:) :: rn2
        real, allocatable, dimension(:,:,:) :: wmask


    contains
    subroutine test

    call iom_put('estrat_k', -avt_k(:,:,:) * rn2(nis0:nie0,njs0:nje0,:)\
* wmask(nis0:nie0,njs0:nje0,:))
    end subroutine test
    end module"""

    psyir = fortran_reader.psyir_from_source(code)
    dtrans = DataNodeToTempTrans()
    pytest.xfail(
        reason="Issue #3325. PSyclone does not currently give "
        "enough information about the datatype of expressions "
        "involving allocatable arrays for the "
        "DataNodeToTempTrans to be applied for this case yet."
    )
    dtrans.apply(psyir.children[0].children[0].children[0].arguments[1])
