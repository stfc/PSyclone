# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2025, Science and Technology Facilities Council.
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
# Authors: R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified S. Valat, Inria / Laboratoire Jean Kuntzmann
#          M. Schreiber, Univ. Grenoble Alpes / Inria / Lab. Jean Kuntzmann

'''Module containing py.test tests for the transformation of the PSyIR
   of generic code using the OpenACC 'kernels' directive.

'''

import pytest

from psyclone.errors import GenerationError
from psyclone.psyir.nodes import Assignment, ACCKernelsDirective, Loop, Routine
from psyclone.psyir.transformations import (
    ACCKernelsTrans, TransformationError, ProfileTrans)
from psyclone.transformations import ACCEnterDataTrans, ACCLoopTrans
from psyclone.tests.utilities import get_invoke

EXPLICIT_LOOP = ("program do_loop\n"
                 "use kind_params_mod\n"
                 "integer :: ji\n"
                 "integer, parameter :: jpj=32\n"
                 "real(kind=wp) :: sto_tmp(jpj)\n"
                 "do ji = 1,jpj\n"
                 "  sto_tmp(ji) = 1.0d0\n"
                 "end do\n"
                 "end program do_loop\n")


def test_kernels_single_node(fortran_reader):
    ''' Check that we can apply the ACCKernelsTrans to a single node
    instead of to a list of nodes. '''
    psyir = fortran_reader.psyir_from_source(EXPLICIT_LOOP)
    schedule = psyir.walk(Routine)[0]
    acc_trans = ACCKernelsTrans()
    acc_trans.apply(schedule[0], {"default_present": True})
    assert isinstance(schedule[0], ACCKernelsDirective)


def test_trigger_async_error(fortran_reader):
    """Check that we can't apply an ACC Kernel Trans with
    a parent using an async queue IDs that is different."""
    psyir = fortran_reader.psyir_from_source(EXPLICIT_LOOP)
    acc_trans = ACCKernelsTrans()

    loop = psyir.walk(Loop)[0]
    acc_trans.apply(loop,
                    {"default_present": True,
                     "async_queue": 2})

    loop = psyir.walk(Loop)[0]

    with pytest.raises(TransformationError) as einfo:
        acc_trans.apply(loop, {"default_present": True,
                        "async_queue": 3})

    correct = ("Cannot apply ACCKernelsTrans with asynchronous"
               " queue '3' because a parent directive specifies"
               " queue '2'")

    assert correct in str(einfo.value)


def test_no_kernels_error(fortran_reader):
    ''' Check that the transformation rejects an attempt to put things
    that aren't kernels inside a kernels region. '''
    psyir = fortran_reader.psyir_from_source(
             "program write_out\n"
             "integer, parameter :: wp = kind(1.0)\n"
             "integer :: ji, jpj\n"
             "real(kind=wp) :: sto_tmp(5)\n"
             "do ji = 1,jpj\n"
             "read(*,*) sto_tmp(ji)\n"
             "end do\n"
             "do ji = 1,jpj\n"
             "write(*,*) sto_tmp(ji)\n"
             "end do\n"
             "end program write_out\n")
    schedule = psyir.walk(Routine)[0]
    acc_trans = ACCKernelsTrans()
    with pytest.raises(TransformationError) as err:
        acc_trans.apply(schedule.children[0:2], {"default_present": True})
    assert ("Nodes of type 'CodeBlock' cannot be enclosed by a "
            "ACCKernelsTrans transformation" in str(err.value))


def test_acc_kernels_gocean_error():
    ''' Check that we refuse to allow the kernels transformation
    for the GOcean DSL. '''
    _, invoke = get_invoke("single_invoke_three_kernels.f90", "gocean",
                           name="invoke_0", dist_mem=False)
    schedule = invoke.schedule
    accktrans = ACCKernelsTrans()
    with pytest.raises(NotImplementedError) as err:
        accktrans.apply(schedule.children)
    assert ("kernels regions are not currently supported for "
            "GOcean InvokeSchedules" in str(err.value))


def test_no_loops(fortran_reader):
    ''' Check that the transformation refuses to generate a kernels region
    if it contains no loops and that this check may also be disabled. '''
    psyir = fortran_reader.psyir_from_source(
                "program no_loop\n"
                "integer :: jpk\n"
                "jpk = 30\n"
                "end program no_loop\n")
    acc_trans = ACCKernelsTrans()
    schedule = psyir.walk(Routine)[0]
    with pytest.raises(TransformationError) as err:
        acc_trans.validate(schedule.children[0:1])
    assert ("must enclose at least one loop or array range but none were "
            "found" in str(err.value))
    # But we can disable this check.
    acc_trans.validate(schedule[0:1], options={"disable_loop_check": True})


def test_implicit_loop(fortran_reader, fortran_writer):
    ''' Check that the transformation generates correct code when applied
    to an implicit loop. '''
    psyir = fortran_reader.psyir_from_source(
                "program implicit_loop\n"
                "use kind_params_mod\n"
                "real(kind=wp) :: sto_tmp(5,5)\n"
                "sto_tmp(:,:) = 0.0_wp\n"
                "end program implicit_loop\n")
    schedule = psyir.walk(Routine)[0]
    acc_trans = ACCKernelsTrans()
    acc_trans.apply(schedule.children[0:1], {"default_present": True})
    gen_code = fortran_writer(psyir)
    assert ("  !$acc kernels default(present)\n"
            "  sto_tmp(:,:) = 0.0_wp\n"
            "  !$acc end kernels\n" in gen_code)


def test_multikern_if(fortran_reader, fortran_writer):
    ''' Check that we can include an if-block containing multiple
    loops within a kernels region. '''
    psyir = fortran_reader.psyir_from_source(
                "program implicit_loop\n"
                "use kind_params_mod\n"
                "logical :: do_this\n"
                "integer :: jk\n"
                "real(kind=wp) :: sto_tmp(5)\n"
                "if(do_this)then\n"
                "do jk = 1, 3\n"
                "  sto_tmp(jk) = jk\n"
                "end do\n"
                "else\n"
                "do jk = 1, 5\n"
                "  sto_tmp(jk) = jk\n"
                "end do\n"
                "end if\n"
                "end program implicit_loop\n")
    schedule = psyir.walk(Routine)[0]
    acc_trans = ACCKernelsTrans()
    acc_trans.apply(schedule.children[0:1], {"default_present": True})
    gen_code = fortran_writer(psyir)
    assert ("  !$acc kernels default(present)\n"
            "  if (do_this) then\n"
            "    do jk = 1, 3, 1\n" in gen_code)
    assert ("    enddo\n"
            "  end if\n"
            "  !$acc end kernels\n"
            "\n"
            "end program implicit_loop" in gen_code)


def test_kernels_within_if(fortran_reader, fortran_writer):
    ''' Check that we can put a kernels region within an if block. '''
    psyir = fortran_reader.psyir_from_source(
                "program if_then\n"
                "logical :: do_this\n"
                "integer :: ji, jpi\n"
                "real :: fld(:), fld2d(:,:)\n"
                "if(do_this)then\n"
                "  do ji=1,jpi\n"
                "    fld(ji) = 1.0\n"
                "  end do\n"
                "else\n"
                "  fld2d(:,:) = 0.0\n"
                "end if\n"
                "end program if_then\n")
    schedule = psyir.walk(Routine)[0]
    acc_trans = ACCKernelsTrans()

    acc_trans.apply(schedule.children[0].if_body, {"default_present": True})
    acc_trans.apply(schedule.children[0].else_body, {"default_present": True})
    new_code = fortran_writer(psyir)
    assert ("  if (do_this) then\n"
            "    !$acc kernels default(present)\n"
            "    do ji = 1, jpi, 1\n" in new_code)
    assert ("    enddo\n"
            "    !$acc end kernels\n"
            "  else\n"
            "    !$acc kernels default(present)\n"
            "    fld2d(:,:) = 0.0\n"
            "    !$acc end kernels\n"
            "  end if\n" in new_code)


def test_no_code_block_kernels(fortran_reader):
    ''' Check that we reject attempts to enclose CodeBlocks within a
    Kernels region. '''
    psyir = fortran_reader.psyir_from_source(
                "program cb_mix\n"
                "  integer :: ji, jpi\n"
                "  real :: fld(:)\n"
                "  do ji=1,jpi\n"
                "    fld(ji) = 1.0\n"
                "  end do\n"
                "  write(*,*) 'Hello'\n"
                "end program cb_mix\n")
    schedule = psyir.walk(Routine)[0]
    acc_trans = ACCKernelsTrans()
    with pytest.raises(TransformationError) as err:
        acc_trans.apply(schedule.children)
    assert ("Nodes of type 'CodeBlock' cannot be enclosed by a "
            "ACCKernelsTrans" in str(err.value))


def test_no_default_present(fortran_reader, fortran_writer):
    ''' Check that we can create a kernels region with no 'default(present)'
    clause (as we will want to do when using managed memory). '''
    psyir = fortran_reader.psyir_from_source(EXPLICIT_LOOP)
    schedule = psyir.walk(Routine)[0]
    acc_trans = ACCKernelsTrans()
    acc_trans.apply(schedule.children, {"default_present": False})
    assert "!$acc kernels\n" in fortran_writer(psyir)


def test_kernels_around_where_construct(fortran_reader, fortran_writer):
    ''' Check that we can put a WHERE construct inside a KERNELS region. '''
    psyir = fortran_reader.psyir_from_source(
                "program where_test\n"
                "  integer :: flag\n"
                "  real :: a(:,:), b(:,:)\n"
                "  where (a(:,:) < flag)\n"
                "    b(:,:) = 0.0\n"
                "  end where\n"
                "end program where_test\n")
    schedule = psyir.walk(Routine)[0]
    acc_trans = ACCKernelsTrans()
    acc_trans.apply(schedule)
    assert isinstance(schedule[0], ACCKernelsDirective)
    assert isinstance(schedule[0].dir_body[0], Loop)
    assert ("  !$acc kernels\n"
            "  do widx2 = 1, SIZE(a(:,:), dim=2), 1\n"
            "    do widx1 = 1, SIZE(a(:,:), dim=1), 1\n"
            "      if (a(LBOUND(a, dim=1) + widx1 - 1,"
            "LBOUND(a, dim=2) + widx2 - 1) < flag) then\n"
            "        b(LBOUND(b, dim=1) + widx1 - 1,"
            "LBOUND(b, dim=2) + widx2 - 1) = 0.0\n"
            "      end if\n"
            "    enddo\n"
            "  enddo\n"
            "  !$acc end kernels\n" in fortran_writer(psyir))


def test_kernels_around_where_stmt(fortran_reader, fortran_writer):
    ''' Check that we can put a WHERE statement inside a KERNELS region. '''
    psyir = fortran_reader.psyir_from_source(
                "program where_test\n"
                "  integer :: flag\n"
                "  real :: a(:,:), b(:,:), c(:,:)\n"
                "  a(:,:) = 1.0\n"
                "  where (a(:,:) < flag) b(:,:) = 0.0\n"
                "  c(:,:) = 1.0\n"
                "end program where_test\n")
    schedule = psyir.walk(Routine)[0]
    acc_trans = ACCKernelsTrans()
    acc_trans.apply([schedule[1]])
    assert ("  a(:,:) = 1.0\n"
            "  !$acc kernels\n"
            "  do widx2 = 1, SIZE(a(:,:), dim=2), 1\n"
            "    do widx1 = 1, SIZE(a(:,:), dim=1), 1\n"
            "      if (a(LBOUND(a, dim=1) + widx1 - 1,"
            "LBOUND(a, dim=2) + widx2 - 1) < flag) then\n"
            "        b(LBOUND(b, dim=1) + widx1 - 1,"
            "LBOUND(b, dim=2) + widx2 - 1) = 0.0\n"
            "      end if\n"
            "    enddo\n"
            "  enddo\n"
            "  !$acc end kernels\n"
            "  c(:,:) = 1.0\n" in fortran_writer(psyir))


def test_loop_inside_kernels(fortran_reader, fortran_writer):
    ''' Check that we can put an ACC LOOP directive inside a KERNELS
    region. '''
    psyir = fortran_reader.psyir_from_source(EXPLICIT_LOOP)
    schedule = psyir.walk(Routine)[0]
    acc_trans = ACCKernelsTrans()
    acc_trans.apply([schedule[0]])
    loop_trans = ACCLoopTrans()
    loop_trans.apply(schedule[0].dir_body[0])
    output = fortran_writer(psyir)
    assert ("  !$acc kernels\n"
            "  !$acc loop independent\n"
            "  do ji = 1, jpj, 1\n" in output)
    assert ("  enddo\n"
            "  !$acc end kernels\n" in output)


def test_two_loops_inside_kernels(fortran_reader, fortran_writer):
    ''' Check that we can mark-up one or both loops inside a KERNELS
    region containing two loops. '''
    psyir = fortran_reader.psyir_from_source(
                "program two_loops\n"
                "  integer :: ji\n"
                "  real :: array(10)\n"
                "  do ji = 1, 10\n"
                "    array(ji) = 1.0\n"
                "  end do\n"
                "  do ji = 1, 5\n"
                "    array(ji) = 2.0*array(ji)\n"
                "  end do\n"
                "end program two_loops\n")
    schedule = psyir.walk(Routine)[0]
    # Enclose both loops within a KERNELS region
    acc_trans = ACCKernelsTrans()
    acc_trans.apply(schedule[0:2])
    # Apply a loop transformation to just the first loop
    loop_trans = ACCLoopTrans()
    loop_trans.apply(schedule[0].dir_body[0])
    output = fortran_writer(psyir)
    assert ("  !$acc kernels\n"
            "  !$acc loop independent\n"
            "  do ji = 1, 10, 1\n" in output)
    assert ("  enddo\n"
            "  !$acc end kernels\n"
            "\n"
            "end program" in output)
    loop_trans.apply(schedule[0].dir_body[1])
    output = fortran_writer(psyir)
    assert ("  !$acc loop independent\n"
            "  do ji = 1, 5, 1\n" in output)
    assert ("  enddo\n"
            "  !$acc end kernels\n"
            "\n"
            "end program" in output)


def test_loop_after_implicit_kernels(fortran_reader, fortran_writer):
    ''' Test the addition of a loop directive after some implicit loops
    within a kernels region. '''
    psyir = fortran_reader.psyir_from_source(
                "program two_loops\n"
                "  integer :: ji\n"
                "  real :: array(10,10)\n"
                "  array(:,:) = -1.0\n"
                "  do ji = 1, 5\n"
                "    array(ji,1) = 2.0*array(ji,2)\n"
                "  end do\n"
                "end program two_loops\n")
    schedule = psyir.walk(Routine)[0]
    acc_trans = ACCKernelsTrans()
    loop_trans = ACCLoopTrans()
    acc_trans.apply(schedule[0:2])
    loop_trans.apply(schedule[0].dir_body[1])
    output = fortran_writer(psyir)
    assert ("  !$acc kernels\n"
            "  array(:,:) = -1.0\n"
            "  !$acc loop independent\n"
            "  do ji = 1, 5, 1\n" in output)
    assert ("  enddo\n"
            "  !$acc end kernels\n"
            "\n"
            "end program" in output)


def test_no_psydata_in_kernels(fortran_reader, monkeypatch):
    ''' Check that we refuse to generate code when a kernels region
    contains PSyData calls. '''
    psyir = fortran_reader.psyir_from_source(EXPLICIT_LOOP)
    schedule = psyir.walk(Routine)[0]
    ptrans = ProfileTrans()
    acc_trans = ACCKernelsTrans()
    acc_trans.apply(schedule[0])
    # Attempt to put a profiling call within the loop
    assign = schedule.walk(Assignment)[0]
    with pytest.raises(TransformationError) as err:
        ptrans.apply(assign)
    assert ("A PSyData node cannot be inserted inside an OpenACC region"
            in str(err.value))
    # Monkeypatch the validate() method so as to avoid the checking
    # that it does
    monkeypatch.setattr(ptrans, "validate", lambda x, y: None)
    ptrans.apply(assign)
    # Check that an appropriate error is raised by the backend
    with pytest.raises(GenerationError) as err:
        psyir.walk(ACCKernelsDirective)[0].validate_global_constraints()
    assert ("Cannot include CodeBlocks or calls to PSyData routines within "
            "OpenACC regions but found [" in str(err.value))


def test_no_assumed_size_char_in_kernels(fortran_reader):
    '''
    Check that the transformation rejects any assumed-size character variables
    or intrinsics that aren't available on GPU.

    '''
    code = '''\
subroutine ice(assumed_size_char, assumed2)
  implicit none
  character(len = *), intent(in) :: assumed_size_char
  character*(*) :: assumed2
  character(len=10) :: explicit_size_char
  real, dimension(10,10) :: my_var

  if (assumed_size_char == 'literal') then
    my_var(:UBOUND(my_var)) = 0.0
    explicit_size_char = 'hello'
  end if

  assumed_size_char(:LEN(explicit_size_char)) = ' '

  explicit_size_char(:) = achar(9)

  if(explicit_size_char == 'literal') then
    my_var(:) = 0.0
  end if

  assumed2(:) = ''

  explicit_size_char = assumed2

end
'''
    psyir = fortran_reader.psyir_from_source(code)
    sub = psyir.walk(Routine)[0]
    acc_trans = ACCKernelsTrans()
    with pytest.raises(TransformationError) as err:
        acc_trans.validate(sub.children[0])
    assert ("Assumed-size character variables cannot be enclosed in an "
            "OpenACC region but found 'if (assumed_size_char == 'literal')"
            in str(err.value))
    with pytest.raises(TransformationError) as err:
        acc_trans.validate(sub.children[1], options={"allow_string": True})
    assert ("Assumed-size character variables cannot be enclosed in an OpenACC"
            " region but found 'assumed_size_char(:LEN(explicit_size_char)) = "
            in str(err.value))
    with pytest.raises(TransformationError) as err:
        acc_trans.validate(sub.children[2], options={"allow_string": True})
    assert ("Cannot include 'ACHAR(9)' in an OpenACC region because "
            "it is not available on GPU" in str(err.value))
    # Check that the character assignment is excluded by default.
    with pytest.raises(TransformationError) as err:
        acc_trans.validate(sub.children[2])
    assert ("ACCKernelsTrans does not permit assignments involving character "
            "variables by default (use the 'allow_string' option to include "
            "them), but found:" in str(err.value))
    # Check the verbose option.
    with pytest.raises(TransformationError) as err:
        acc_trans.validate(sub.children[2], options={"verbose": True})
    assert (sub.children[2].preceding_comment ==
            "ACCKernelsTrans does not permit assignments involving character "
            "variables by default (use the 'allow_string' option to include "
            "them)")

    # String with explicit length is fine.
    acc_trans.validate(sub.children[3], options={})
    # CHARACTER*(*) notation is also rejected.
    with pytest.raises(TransformationError) as err:
        acc_trans.validate(sub.children[4], options={})
    assert ("Assumed-size character variables cannot be enclosed in an OpenACC"
            " region but found 'assumed2(:) = ''" in str(err.value))
    # We don't need there to be a character literal in order to spot a problem.
    with pytest.raises(TransformationError) as err:
        acc_trans.validate(sub.children[5], options={})
    assert ("Assumed-size character variables cannot be enclosed in an OpenACC"
            " region but found 'explicit_size_char = assumed2" in
            str(err.value))


def test_check_async_queue_with_enter_data(fortran_reader):
    '''Tests for the check_async_queue() method.'''
    acc_trans = ACCKernelsTrans()
    acc_edata_trans = ACCEnterDataTrans()
    with pytest.raises(TypeError) as err:
        acc_trans.check_async_queue(None, 3.5)
    assert ("Invalid async_queue value, expect Reference or integer or None "
            "or bool, got : 3.5" in str(err.value))
    psyir = fortran_reader.psyir_from_source(
                "program two_loops\n"
                "  integer :: ji\n"
                "  real :: array(10,10)\n"
                "  do ji = 1, 5\n"
                "    array(ji,1) = 2.0*array(ji,2)\n"
                "  end do\n"
                "end program two_loops\n")
    prog = psyir.walk(Routine)[0]
    acc_edata_trans.apply(prog, {"async_queue": 1})
    with pytest.raises(TransformationError) as err:
        acc_trans.check_async_queue(prog.walk(Loop), 2)
    assert ("Cannot apply ACCKernelsTrans with asynchronous queue '2' because "
            "the containing routine has an ENTER DATA directive specifying "
            "queue '1'" in str(err.value))
