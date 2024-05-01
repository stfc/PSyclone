# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council.
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
# ----------------------------------------------------------------------------
# Author: A. B. G. Chalk, STFC Daresbury Lab

'''This module tests the scalarization transformation.
'''

from psyclone.core import VariablesAccessInfo
from psyclone.psyir.transformations import ScalarizationTrans
from psyclone.tests.utilities import Compile


def test_scalarizationtrans_potential_array_symbols(fortran_reader):
    ''' Test the possible code paths in the
    _find_potential_scalarizable_array_symbols function.'''
    code = '''subroutine test()
        integer :: i
        integer :: k
        integer, dimension(1:100) :: arr
        integer, dimension(1:100) :: b
        integer, dimension(1:100) :: c

        do i = 1, 100
           arr(i) = i
        end do
    end subroutine
    '''
    strans = ScalarizationTrans()
    psyir = fortran_reader.psyir_from_source(code)
    node = psyir.children[0].children[0]
    var_accesses = VariablesAccessInfo(nodes=node.loop_body)
    potential_targets = strans._find_potential_scalarizable_array_symbols(
            node, var_accesses)
    assert len(potential_targets) == 1
    assert potential_targets[0].var_name == "arr"

    code = '''subroutine test()
        integer :: i
        integer :: k
        integer, dimension(1:100) :: arr
        integer, dimension(1:100) :: b
        integer, dimension(1:100) :: c

        do i = 1, 99
           k = i + 1
           arr(k) = i
        end do
    end subroutine
    '''
    psyir = fortran_reader.psyir_from_source(code)
    node = psyir.children[0].children[0]
    var_accesses = VariablesAccessInfo(nodes=node.loop_body)
    potential_targets = strans._find_potential_scalarizable_array_symbols(
            node, var_accesses)
    assert len(potential_targets) == 0

    code = '''subroutine test()
        integer :: i
        integer :: k
        integer, dimension(1:100) :: arr
        integer, dimension(1:100) :: b
        integer, dimension(1:100) :: c
        k = 3
        do i = 1, 99
           arr(i) = i + 1
           arr(k) = arr(i) + 1
        end do
    end subroutine
    '''
    psyir = fortran_reader.psyir_from_source(code)
    node = psyir.children[0].children[1]
    var_accesses = VariablesAccessInfo(nodes=node.loop_body)
    potential_targets = strans._find_potential_scalarizable_array_symbols(
            node, var_accesses)
    assert len(potential_targets) == 0

    # Ensure that we don't access imports or arguments or unknowns
    # for scalarization
    # Not sure if we should expand this for anything else?
    code = '''subroutine test(b)
        use mymod, only: arr
        integer :: i
        integer :: k
        integer, dimension(1:100) :: b

        do i = 1, 100
           arr(i) = i
           b(i) = i
           c(i) = i
        end do
    end subroutine
    '''
    strans = ScalarizationTrans()
    psyir = fortran_reader.psyir_from_source(code)
    node = psyir.children[0].children[0]
    var_accesses = VariablesAccessInfo(nodes=node.loop_body)
    potential_targets = strans._find_potential_scalarizable_array_symbols(
            node, var_accesses)
    assert len(potential_targets) == 0


def test_scalarization_first_access_is_write(fortran_reader):
    ''' Test the scalarization transformation's
    _check_first_access_is_write function.'''
    code = '''subroutine test()
        integer :: i
        integer :: k
        integer, dimension(1:100) :: arr
        integer, dimension(1:100) :: b
        integer, dimension(1:100) :: c

        do i = 1, 100
           arr(i) = i
        end do
    end subroutine
    '''
    strans = ScalarizationTrans()
    psyir = fortran_reader.psyir_from_source(code)
    node = psyir.children[0].children[0]
    var_accesses = VariablesAccessInfo(nodes=node.loop_body)
    potentials = [var_accesses.all_signatures[0]]
    potential_targets = strans._check_first_access_is_write(
            node, var_accesses, potentials)

    assert len(potential_targets) == 1
    assert potential_targets[0].var_name == "arr"
    code = '''subroutine test()
        integer :: i
        integer :: k
        integer, dimension(1:100) :: arr
        integer, dimension(1:100) :: b
        integer, dimension(1:100) :: c

        do i = 1, 100
           arr(i) = arr(i) + 1
        end do
    end subroutine
    '''
    strans = ScalarizationTrans()
    psyir = fortran_reader.psyir_from_source(code)
    node = psyir.children[0].children[0]
    var_accesses = VariablesAccessInfo(nodes=node.loop_body)
    potentials = [var_accesses.all_signatures[0]]
    potential_targets = strans._check_first_access_is_write(
            node, var_accesses, potentials)

    assert len(potential_targets) == 0


def test_scalarization_trans_check_valid_following_access(fortran_reader):
    ''' Test the scalarization transformation's
    _check_valid_following_access function.'''
    code = '''subroutine test()
        integer :: i
        integer :: k
        integer, dimension(1:100) :: arr
        integer, dimension(1:100) :: b

        do i = 1, 100
           arr(i) = exp(arr(i))
           b(i) = arr(i) * 3
        end do
        do i = 1, 100
           b(i) = b(i) + 1
        end do
    end subroutine
    '''
    strans = ScalarizationTrans()
    psyir = fortran_reader.psyir_from_source(code)
    node = psyir.children[0].children[0]
    var_accesses = VariablesAccessInfo(nodes=node.loop_body)
    # Only arr makes it through the 2 prior stages
    potentials = [var_accesses.all_signatures[0]]
    potential_targets = strans._check_valid_following_access(
            node, var_accesses, potentials)
    assert len(potential_targets) == 1
    assert potential_targets[0].var_name == "arr"

    # Test we ignore array next_access if they're in an if statement
    code = '''subroutine test()
        integer :: i
        integer :: k
        integer, dimension(1:100) :: arr
        integer, dimension(1:100) :: b
        logical :: x = .FALSE.

        do i = 1, 100
           arr(i) = exp(arr(i))
           b(i) = arr(i) * 3
        end do
        if(x) then
          do i = 1, 100
            b(i) = 1
          end do
        end if
        end subroutine test
        '''
    strans = ScalarizationTrans()
    psyir = fortran_reader.psyir_from_source(code)
    node = psyir.children[0].children[0]
    var_accesses = VariablesAccessInfo(nodes=node.loop_body)
    potentials = [var_accesses.all_signatures[0],
                  var_accesses.all_signatures[1]]
    potential_targets = strans._check_valid_following_access(
            node, var_accesses, potentials)
    assert len(potential_targets) == 1
    assert potential_targets[0].var_name == "arr"
    # Test we don't ignore array next_access if they're in an if statement
    # that is an ancestor of the loop we're scalarizing
    code = '''subroutine test()
        integer :: i
        integer :: k
        integer, dimension(1:100) :: arr
        integer, dimension(1:100) :: b

        if(.false.) then
          do i = 1, 100
           arr(i) = exp(arr(i))
           b(i) = arr(i) * 3
          end do
          do i = 1, 100
            b(i) = 1
          end do
        end if
        end subroutine test
        '''
    strans = ScalarizationTrans()
    psyir = fortran_reader.psyir_from_source(code)
    node = psyir.children[0].children[0].if_body.children[0]
    var_accesses = VariablesAccessInfo(nodes=node.loop_body)
    potentials = [var_accesses.all_signatures[0],
                  var_accesses.all_signatures[1]]
    potential_targets = strans._check_valid_following_access(
            node, var_accesses, potentials)
    assert len(potential_targets) == 2
    assert potential_targets[0].var_name == "arr"
    assert potential_targets[1].var_name == "b"

    # Test we don't ignore array next_access if they have an ancestor
    # that is a Call
    code = '''subroutine test()
        use my_mod
        integer :: i
        integer :: k
        integer, dimension(1:100) :: arr
        integer, dimension(1:100) :: b

        if(.false.) then
          do i = 1, 100
           arr(i) = exp(arr(i))
           b(i) = arr(i) * 3
          end do
          do i = 1, 100
            Call some_func(b(i))
          end do
        end if
        end subroutine test
        '''
    strans = ScalarizationTrans()
    psyir = fortran_reader.psyir_from_source(code)
    node = psyir.children[0].children[0].if_body.children[0]
    var_accesses = VariablesAccessInfo(nodes=node.loop_body)
    potentials = [var_accesses.all_signatures[0],
                  var_accesses.all_signatures[1]]
    potential_targets = strans._check_valid_following_access(
            node, var_accesses, potentials)
    assert len(potential_targets) == 1
    assert potential_targets[0].var_name == "arr"


def test_scalarization_trans_apply(fortran_reader, fortran_writer, tmpdir):
    ''' Test the application of the scalarization transformation.'''
    code = '''subroutine test()
         integer :: i
         integer :: k
         integer, dimension(1:100) :: arr
         integer, dimension(1:100) :: b
         integer, dimension(1:100) :: c

         do i = 1, 100
            arr(i) = i
            arr(i) = exp(arr(i))
            k = i
            b(i) = arr(i) * 3
            c(k) = i
         end do
         do i = 1, 100
            b(i) = b(i) + 1
         end do
     end subroutine
    '''
    strans = ScalarizationTrans()
    psyir = fortran_reader.psyir_from_source(code)

    loop = psyir.children[0].children[0]
    strans.apply(loop)
    correct = '''subroutine test()
  integer :: i
  integer :: k
  integer, dimension(100) :: arr
  integer, dimension(100) :: b
  integer, dimension(100) :: c
  integer :: arr_scalar

  do i = 1, 100, 1
    arr_scalar = i
    arr_scalar = EXP(arr_scalar)
    k = i
    b(i) = arr_scalar * 3
    c(k) = i
  enddo
  do i = 1, 100, 1
    b(i) = b(i) + 1
  enddo'''
    out = fortran_writer(psyir)
    assert correct in out
    assert Compile(tmpdir).string_compiles(out)

