# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2024-2025, Science and Technology Facilities Council.
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


def test_scalararizationtrans_is_local_array(fortran_reader):
    code = '''function test(a) result(x)
       use mymod, only: arr, atype
       integer :: i
       integer :: k
       real, dimension(1:100) :: local
       real, dimension(1:100) :: a
       character(2), dimension(1:100) :: b
       real, dimension(1:100) :: x
       type(atype) :: custom
       type(atype), dimension(1:100) :: custom2

       do i = 1, 100
          arr(i) = i
          a(i) = i
          local(i) = i
          b(i) = b(i) // "c"
          x(i) = i
          custom%type(i) = i
          custom2(i)%typeb(i) = i
       end do
       end function'''
    psyir = fortran_reader.psyir_from_source(code)
    node = psyir.children[0].children[0]
    var_accesses = VariablesAccessInfo(nodes=node.loop_body)
    keys = list(var_accesses.keys())
    # Test arr
    assert var_accesses[keys[1]].var_name == "arr"
    assert not ScalarizationTrans._is_local_array(keys[1],
                                                  var_accesses)
    # Test a
    assert var_accesses[keys[2]].var_name == "a"
    assert not ScalarizationTrans._is_local_array(keys[2],
                                                  var_accesses)
    # Test local
    assert var_accesses[keys[3]].var_name == "local"
    assert ScalarizationTrans._is_local_array(keys[3],
                                              var_accesses)

    # Test b - the RHS of the assignment is a codeblock so we do not
    # count it as a local array and invalidate it, as otherwise the
    # local array test can fail. Also we can't safely transform the
    # CodeBlock anyway.
    assert var_accesses[keys[4]].var_name == "b"
    assert not ScalarizationTrans._is_local_array(keys[4],
                                                  var_accesses)

    # Test x - the return value is not classed as a local array.
    assert var_accesses[keys[5]].var_name == "x"
    assert not ScalarizationTrans._is_local_array(keys[5],
                                                  var_accesses)

    # Test custom - we don't scalarize derived types.
    assert var_accesses[keys[6]].var_name == "custom%type"
    assert not ScalarizationTrans._is_local_array(keys[6],
                                                  var_accesses)
    # Test custom2 - we don't scalarize derived types.
    assert var_accesses[keys[7]].var_name == "custom2%typeb"
    assert not ScalarizationTrans._is_local_array(keys[7],
                                                  var_accesses)

    # Test filter behaviour same as used in the transformation
    local_arrays = filter(
            lambda sig: ScalarizationTrans._is_local_array(sig, var_accesses),
            var_accesses)
    local_arrays = list(local_arrays)
    assert len(local_arrays) == 1
    assert local_arrays[0].var_name == "local"


def test_scalarizationtrans_have_same_unmodified_index(fortran_reader):
    code = '''subroutine test()
       integer :: i
       integer :: k
       real, dimension(1:100) :: a
       real, dimension(1:103) :: b
       real, dimension(1:100) :: c
       k = 0
       do i = 1, 100
          a(i) = i
          b(i+2) = i
          b(i+3) = b(i) + b(i+1)
          c(k) = 2
          k = k + 1
       end do
       end subroutine'''
    psyir = fortran_reader.psyir_from_source(code)
    node = psyir.children[0].children[1]
    var_accesses = VariablesAccessInfo(nodes=node.loop_body)
    keys = list(var_accesses.keys())
    # Test a
    assert var_accesses[keys[1]].var_name == "a"
    assert ScalarizationTrans._have_same_unmodified_index(keys[1],
                                                          var_accesses)
    # Test b (differeing indices)
    assert var_accesses[keys[2]].var_name == "b"
    assert not ScalarizationTrans._have_same_unmodified_index(keys[2],
                                                              var_accesses)
    # Test c (k is modified)
    assert var_accesses[keys[3]].var_name == "c"
    assert not ScalarizationTrans._have_same_unmodified_index(keys[3],
                                                              var_accesses)
    # Test filter behaviour same as used in the transformation
    local_arrays = filter(
            lambda sig: ScalarizationTrans._is_local_array(sig, var_accesses),
            var_accesses)
    local_arrays = list(local_arrays)
    assert len(local_arrays) == 3

    unmodified_indices = filter(
            lambda sig: ScalarizationTrans._have_same_unmodified_index(
                sig, var_accesses),
            local_arrays)
    unmodified_indices = list(unmodified_indices)
    assert len(unmodified_indices) == 1
    assert unmodified_indices[0].var_name == "a"


def test_scalarizationtrans_check_first_access_is_write(fortran_reader):
    code = '''subroutine test()
       integer :: i
       integer :: k
       real, dimension(1:100) :: a
       real, dimension(1:100) :: b
       real, dimension(1:100) :: c
       do i = 1, 100
          a(i) = i
          b(i) = b(i) + 1
          c(i) = a(i) + b(i)
       end do
       end subroutine'''
    psyir = fortran_reader.psyir_from_source(code)
    node = psyir.children[0].children[0]
    var_accesses = VariablesAccessInfo(nodes=node.loop_body)
    keys = list(var_accesses.keys())
    # Test a
    assert var_accesses[keys[1]].var_name == "a"
    assert ScalarizationTrans._check_first_access_is_write(keys[1],
                                                           var_accesses)
    # Test b (differeing indices)
    assert var_accesses[keys[2]].var_name == "b"
    assert not ScalarizationTrans._check_first_access_is_write(keys[2],
                                                               var_accesses)
    # Test c (k is modified)
    assert var_accesses[keys[3]].var_name == "c"
    assert ScalarizationTrans._check_first_access_is_write(keys[3],
                                                           var_accesses)

    # Test filter behaviour same as used in the transformation
    local_arrays = filter(
            lambda sig: ScalarizationTrans._is_local_array(sig, var_accesses),
            var_accesses)
    local_arrays = list(local_arrays)
    assert len(local_arrays) == 3

    unmodified_indices = filter(
            lambda sig: ScalarizationTrans._have_same_unmodified_index(
                sig, var_accesses),
            local_arrays)
    unmodified_indices = list(unmodified_indices)
    assert len(unmodified_indices) == 3

    first_write_arrays = filter(
            lambda sig: ScalarizationTrans._check_first_access_is_write(
                sig, var_accesses),
            unmodified_indices)
    first_write_arrays = list(first_write_arrays)
    assert len(first_write_arrays) == 2


def test_scalarizationtrans_value_unused_after_loop(fortran_reader):
    code = '''subroutine test()
        integer :: i
        integer :: k
        real, dimension(1:100) :: arr
        real, dimension(1:100) :: b

        do i = 1, 100
           arr(i) = exp(arr(i))
           b(i) = arr(i) * 3
        end do
        do i = 1, 100
           b(i) = b(i) + 1
        end do
    end subroutine
    '''
    psyir = fortran_reader.psyir_from_source(code)
    node = psyir.children[0].children[0]
    var_accesses = VariablesAccessInfo(nodes=node.loop_body)
    keys = list(var_accesses.keys())
    # Test arr
    assert var_accesses[keys[1]].var_name == "arr"
    assert ScalarizationTrans._value_unused_after_loop(keys[1],
                                                       node.loop_body,
                                                       var_accesses)
    # Test b
    assert var_accesses[keys[2]].var_name == "b"
    assert not ScalarizationTrans._value_unused_after_loop(keys[2],
                                                           node.loop_body,
                                                           var_accesses)

    # Test we ignore array next_access if they're in an if statement
    code = '''subroutine test()
        integer :: i
        integer :: k
        real, dimension(1:100) :: arr
        real, dimension(1:100) :: b
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
    psyir = fortran_reader.psyir_from_source(code)
    node = psyir.children[0].children[0]
    var_accesses = VariablesAccessInfo(nodes=node.loop_body)
    keys = list(var_accesses.keys())
    # Test arr
    assert var_accesses[keys[1]].var_name == "arr"
    assert ScalarizationTrans._value_unused_after_loop(keys[1],
                                                       node.loop_body,
                                                       var_accesses)
    # Test b
    assert var_accesses[keys[2]].var_name == "b"
    assert ScalarizationTrans._value_unused_after_loop(keys[2],
                                                       node.loop_body,
                                                       var_accesses)
    # Test we don't ignore array next_access if they're in an if statement
    # that is an ancestor of the loop we're scalarizing
    code = '''subroutine test()
        integer :: i
        integer :: k
        real, dimension(1:100) :: arr
        real, dimension(1:100) :: b

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
    psyir = fortran_reader.psyir_from_source(code)
    node = psyir.children[0].children[0].if_body.children[0]
    var_accesses = VariablesAccessInfo(nodes=node.loop_body)
    keys = list(var_accesses.keys())
    # Test arr
    assert var_accesses[keys[1]].var_name == "arr"
    assert ScalarizationTrans._value_unused_after_loop(keys[1],
                                                       node.loop_body,
                                                       var_accesses)
    # Test b
    assert var_accesses[keys[2]].var_name == "b"
    assert ScalarizationTrans._value_unused_after_loop(keys[2],
                                                       node.loop_body,
                                                       var_accesses)

    # Test we don't ignore array next_access if they have an ancestor
    # that is a Call
    code = '''subroutine test()
        use my_mod
        integer :: i
        integer :: k
        real, dimension(1:100) :: arr
        real, dimension(1:100) :: b

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
    psyir = fortran_reader.psyir_from_source(code)
    node = psyir.children[0].children[0].if_body.children[0]
    var_accesses = VariablesAccessInfo(nodes=node.loop_body)
    keys = list(var_accesses.keys())
    # Test arr
    assert var_accesses[keys[1]].var_name == "arr"
    assert ScalarizationTrans._value_unused_after_loop(keys[1],
                                                       node.loop_body,
                                                       var_accesses)
    # Test b
    assert var_accesses[keys[2]].var_name == "b"
    assert not ScalarizationTrans._value_unused_after_loop(keys[2],
                                                           node.loop_body,
                                                           var_accesses)

    # Test being a while condition correctly counts as being used.
    code = '''subroutine test()
        use my_mod
        integer :: i
        integer :: k
        real, dimension(1:100) :: arr
        real, dimension(1:100) :: b

          do i = 1, 100
           arr(i) = exp(arr(i))
           b(i) = arr(i) * 3
          end do
          do i = 1, 100
              do while(b(i) < 256)
                b(i) = arr(i) * arr(i)
                arr(i) = arr(i) * 2
              end do
          end do
        end subroutine test
        '''
    psyir = fortran_reader.psyir_from_source(code)
    node = psyir.children[0].children[0]
    var_accesses = VariablesAccessInfo(nodes=node.loop_body)
    keys = list(var_accesses.keys())
    # Test b
    assert var_accesses[keys[2]].var_name == "b"
    assert not ScalarizationTrans._value_unused_after_loop(keys[2],
                                                           node.loop_body,
                                                           var_accesses)

    # Test being a loop start/stop/step condition correctly counts
    # as being used.
    code = '''subroutine test()
        use my_mod
        integer :: i
        integer :: k
        real, dimension(1:100) :: arr
        real, dimension(1:100) :: b
        real, dimension(1:100) :: c
        real, dimension(1:100, 1:100) :: d

          do i = 1, 100
           arr(i) = exp(arr(i))
           b(i) = arr(i) * 3
           c(i) = i
          end do
          do i = 1, 100
              do k = arr(i), b(i), c(i)
                d(i,k) = i
              end do
          end do
        end subroutine test
        '''
    psyir = fortran_reader.psyir_from_source(code)
    node = psyir.children[0].children[0]
    var_accesses = VariablesAccessInfo(nodes=node.loop_body)
    keys = list(var_accesses.keys())
    # Test arr
    assert var_accesses[keys[1]].var_name == "arr"
    assert not ScalarizationTrans._value_unused_after_loop(keys[1],
                                                           node.loop_body,
                                                           var_accesses)
    # Test b
    assert var_accesses[keys[2]].var_name == "b"
    assert not ScalarizationTrans._value_unused_after_loop(keys[2],
                                                           node.loop_body,
                                                           var_accesses)
    # Test c
    assert var_accesses[keys[3]].var_name == "c"
    assert not ScalarizationTrans._value_unused_after_loop(keys[3],
                                                           node.loop_body,
                                                           var_accesses)

    # Test being a symbol in a Codeblock counts as used
    code = '''subroutine test()
        use my_mod
        integer :: i
        integer :: k
        real, dimension(1:100) :: arr
        real, dimension(1:100) :: b
        real, dimension(1:100) :: c
        real, dimension(1:100, 1:100) :: d

          do i = 1, 100
           arr(i) = exp(arr(i))
           b(i) = arr(i) * 3
           c(i) = i
          end do
          do i = 1, 100
            print *, arr(i)
          end do
        end subroutine test
        '''
    psyir = fortran_reader.psyir_from_source(code)
    node = psyir.children[0].children[0]
    var_accesses = VariablesAccessInfo(nodes=node.loop_body)
    keys = list(var_accesses.keys())
    # Test arr
    assert var_accesses[keys[1]].var_name == "arr"
    assert not ScalarizationTrans._value_unused_after_loop(keys[1],
                                                           node.loop_body,
                                                           var_accesses)

    # Test being in an IfBlock condition counts as used.
    code = '''subroutine test()
        use my_mod
        integer :: i
        integer :: k
        real, dimension(1:100) :: arr
        real, dimension(1:100) :: b
        real, dimension(1:100) :: c
        real, dimension(1:100, 1:100) :: d

          do i = 1, 100
           arr(i) = exp(arr(i))
           b(i) = arr(i) * 3
           c(i) = i
          end do
          do i = 1, 100
            if(arr(i) == 1) then
                print *, b(i)
            end if
          end do
        end subroutine test
        '''
    psyir = fortran_reader.psyir_from_source(code)
    node = psyir.children[0].children[0]
    var_accesses = VariablesAccessInfo(nodes=node.loop_body)
    keys = list(var_accesses.keys())
    # Test arr
    assert var_accesses[keys[1]].var_name == "arr"
    assert not ScalarizationTrans._value_unused_after_loop(keys[1],
                                                           node.loop_body,
                                                           var_accesses)


def test_scalarization_trans_apply(fortran_reader, fortran_writer, tmpdir):
    ''' Test the application of the scalarization transformation.'''
    code = '''subroutine test()
         integer :: i
         integer :: k
         real, dimension(1:100) :: arr
         real, dimension(1:100) :: b
         real, dimension(1:100) :: c

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
  real, dimension(100) :: arr
  real, dimension(100) :: b
  real, dimension(100) :: c
  real :: arr_scalar

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

    # Use in if/else where the if has write only followup and
    # the else has a read - shouldn't scalarise b.
    code = '''subroutine test()
         integer :: i
         integer :: k
         real, dimension(1:100) :: arr
         real, dimension(1:100) :: b
         real, dimension(1:100) :: c

         do i = 1, 100
            arr(i) = i
            arr(i) = exp(arr(i))
            k = i
            b(i) = arr(i) * 3
            c(k) = i
         end do
         do i = 1, 100
            if(c(i) > 50) then
                b(i) = c(i)
            else
                b(i) = b(i) + c(i)
            end if
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
  real, dimension(100) :: arr
  real, dimension(100) :: b
  real, dimension(100) :: c
  real :: arr_scalar

  do i = 1, 100, 1
    arr_scalar = i
    arr_scalar = EXP(arr_scalar)
    k = i
    b(i) = arr_scalar * 3
    c(k) = i
  enddo
  do i = 1, 100, 1
    if (c(i) > 50) then
      b(i) = c(i)
    else
      b(i) = b(i) + c(i)
    end if
  enddo'''
    out = fortran_writer(psyir)
    assert correct in out
    assert Compile(tmpdir).string_compiles(out)


def test_scalarization_trans_apply_routinesymbol(fortran_reader,
                                                 fortran_writer, tmpdir):
    ''' Test the application of the scalarization transformation doesn't work
    when applied on an array with a RoutineSymbol as an index.'''
    code = '''subroutine test
        integer, dimension(3) :: j
        integer :: i
        integer, allocatable, dimension(:,:,:) :: k
        do i= 1, 100
            allocate(k(MAXVAL(j(1:3)),1,1))
            deallocate(k)
        end do
    end subroutine test'''
    strans = ScalarizationTrans()
    psyir = fortran_reader.psyir_from_source(code)
    strans.apply(psyir.children[0].children[0])
    correct = '''subroutine test()
  integer, dimension(3) :: j
  integer :: i
  integer, allocatable, dimension(:,:,:) :: k

  do i = 1, 100, 1
    ALLOCATE(k(1:MAXVAL(j(:)),1:1,1:1))
    DEALLOCATE(k)
  enddo

end subroutine test
'''
    out = fortran_writer(psyir)
    assert correct == out
    assert Compile(tmpdir).string_compiles(out)
