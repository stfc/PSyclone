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

'''This module contains the DataNodeExtractTrans class.'''

import os
import pytest

from psyclone.configuration import Config
from psyclone.psyir.nodes import (
    Assignment, Reference
)
from psyclone.psyir.symbols import (
    DataSymbol, INTEGER_TYPE
)
from psyclone.psyir.transformations import (
    DataNodeExtractTrans, TransformationError
)


def test_datanodeextracttrans_validate(fortran_reader, tmpdir, monkeypatch):
    """Tests the validate function of the DataNodeExtractTrans."""
    dtrans = DataNodeExtractTrans()
    code = """subroutine test(a, b, c)
        integer, dimension(:,:), intent(inout) :: a, b, c
        c = b + a
    end subroutine test"""
    psyir = fortran_reader.psyir_from_source(code)
    assign = psyir.walk(Assignment)[0]
    with pytest.raises(TransformationError) as err:
        dtrans.validate(assign.rhs)
    assert ("Input node's datatype is an array of unknown size, so the "
            "DataNodeExtractTrans cannot be applied. Input node was "
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
            "DataNodeExtractTrans cannot be applied. Input node "
            "was 'b + a'" in str(err.value))

    code = """subroutine test
        character(len=25) :: a, b

        b = a
    end subroutine test"""
    psyir = fortran_reader.psyir_from_source(code)
    assign = psyir.walk(Assignment)[0]
    with pytest.raises(TransformationError) as err:
        dtrans.validate(assign.rhs)
    assert ("Input node's datatype cannot be computed, so the "
            "DataNodeExtractTrans cannot be applied. Input node "
            "was 'a'" in str(err.value))

    with pytest.raises(TypeError) as err:
        dtrans.validate("abc")
    assert ("Input node to DataNodeExtractTrans should be a "
            "DataNode but got 'str'." in str(err.value))

    with pytest.raises(TypeError) as err:
        dtrans.validate(assign.rhs, storage_name=1)
    assert ("'DataNodeExtractTrans' received options with the wrong types:\n"
            "'storage_name' option expects type 'str' but received '1' of "
            "type 'int'.\nPlease see the documentation and check the "
            "provided types." in str(err.value))

    with pytest.raises(TransformationError) as err:
        dtrans.validate(Reference(DataSymbol("a", INTEGER_TYPE)))
    assert ("Input node to DataNodeExtractTrans has no ancestor Statement "
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
    assert ("Input node to DataNodeExtractTrans contains a call that is not "
            "guaranteed to be pure. Input node is 'a + some_func(a, b)'."
            in str(err.value))

    monkeypatch.setattr(Config.get(), '_include_paths', [str(tmpdir)])
    filename = os.path.join(str(tmpdir), "a_mod.f90")
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
    assert ("Input node contains an imported symbol whose name collides "
            "with an existing symbol, so the DataNodeExtractTrans cannot be "
            "applied. Clashing symbol name is 'i'." in str(err.value))


def test_datanodeextractrans_apply(fortran_reader, fortran_writer, tmpdir,
                                   monkeypatch):
    """Tests the apply function of the DataNodeExtractTrans."""
    dtrans = DataNodeExtractTrans()
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

    # Test the imports are handled correctly.
    monkeypatch.setattr(Config.get(), '_include_paths', [str(tmpdir)])
    filename = os.path.join(str(tmpdir), "a_mod.f90")
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

    filename = os.path.join(str(tmpdir), "b_mod.f90")
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

    filename = os.path.join(str(tmpdir), "c_mod.f90")
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
