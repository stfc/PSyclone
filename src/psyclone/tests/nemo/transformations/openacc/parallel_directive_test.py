# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Module containing py.test tests for the transformation of the PSy
   representation of NEMO code using the OpenACC parallel directive.

'''

import pytest

from psyclone.psyGen import TransInfo
from psyclone.tests.utilities import Compile
from psyclone.transformations import TransformationError


SINGLE_LOOP = ("program do_loop\n"
               "integer :: ji\n"
               "integer, parameter :: jpj=128\n"
               "real :: sto_tmp(jpj)\n"
               "do ji = 1,jpj\n"
               "  sto_tmp(ji) = 1.0d0\n"
               "end do\n"
               "end program do_loop\n")


def test_parallel_single_loop(fortran_reader, fortran_writer, tmpdir):
    ''' Check that we can apply the transformation to a single, explicit
    loop. '''
    psyir = fortran_reader.psyir_from_source(SINGLE_LOOP)
    schedule = psyir.children[0]
    data_trans = TransInfo().get_trans_name('ACCDataTrans')
    acc_trans = TransInfo().get_trans_name('ACCParallelTrans')
    acc_trans.apply(schedule[0:1])
    data_trans.apply(schedule[0])
    code = fortran_writer(psyir).lower()

    assert ("program do_loop\n"
            "  integer, parameter :: jpj = 128\n"
            "  integer :: ji\n"
            "  real, dimension(jpj) :: sto_tmp\n"
            "\n"
            "  !$acc data copyout(sto_tmp)\n"
            "  !$acc parallel default(present)\n"
            "  do ji = 1, jpj, 1\n"
            "    sto_tmp(ji) = 1.0d0\n"
            "  enddo\n"
            "  !$acc end parallel\n"
            "  !$acc end data\n"
            "\n"
            "end program do_loop" in code)
    assert Compile(tmpdir).string_compiles(code)


def test_parallel_single_loop_with_no_default_present_clause(
        fortran_reader, fortran_writer, tmpdir):
    ''' Check that we can apply the transformation to a single, explicit
    loop, without the default present clause '''
    psyir = fortran_reader.psyir_from_source(SINGLE_LOOP)
    schedule = psyir.children[0]
    acc_trans = TransInfo().get_trans_name('ACCParallelTrans')

    with pytest.raises(TransformationError) as err:
        acc_trans.apply(schedule[0:1], options={"default_present": 3})
    assert ("The provided 'default_present' option must be a boolean, "
            "but found '3'." in str(err.value))

    acc_trans.apply(schedule[0:1], options={"default_present": False})
    code = fortran_writer(psyir).lower()

    assert ("program do_loop\n"
            "  integer, parameter :: jpj = 128\n"
            "  integer :: ji\n"
            "  real, dimension(jpj) :: sto_tmp\n"
            "\n"
            "  !$acc parallel\n"
            "  do ji = 1, jpj, 1\n"
            "    sto_tmp(ji) = 1.0d0\n"
            "  enddo\n"
            "  !$acc end parallel\n"
            "\n"
            "end program do_loop" in code)
    assert Compile(tmpdir).string_compiles(code)


def test_parallel_two_loops(fortran_reader, fortran_writer, tmpdir):
    ''' Check that we can enclose two loops within a parallel region. '''
    psyir = fortran_reader.psyir_from_source(
                "program do_loop\n"
                "integer :: ji\n"
                "integer, parameter :: jpi=11\n"
                "real :: sto_tmp(jpi), sto_tmp2(jpi)\n"
                "do ji = 1,jpi\n"
                "  sto_tmp(ji) = 1.0d0\n"
                "end do\n"
                "do ji = 1,jpi\n"
                "  sto_tmp2(ji) = 1.0d0\n"
                "end do\n"
                "end program do_loop\n")
    schedule = psyir.children[0]
    data_trans = TransInfo().get_trans_name('ACCDataTrans')
    acc_trans = TransInfo().get_trans_name('ACCParallelTrans')
    acc_trans.apply(schedule[0:2])
    data_trans.apply(schedule[0])
    code = fortran_writer(psyir).lower()
    assert ("program do_loop\n"
            "  integer, parameter :: jpi = 11\n"
            "  integer :: ji\n"
            "  real, dimension(jpi) :: sto_tmp\n"
            "  real, dimension(jpi) :: sto_tmp2\n"
            "\n"
            "  !$acc data copyout(sto_tmp,sto_tmp2)\n"
            "  !$acc parallel default(present)\n"
            "  do ji = 1, jpi, 1\n"
            "    sto_tmp(ji) = 1.0d0\n"
            "  enddo\n"
            "  do ji = 1, jpi, 1\n"
            "    sto_tmp2(ji) = 1.0d0\n"
            "  enddo\n"
            "  !$acc end parallel\n"
            "  !$acc end data\n"
            "\n"
            "end program do_loop" in code)
    assert Compile(tmpdir).string_compiles(code)


def test_parallel_if_block(fortran_reader, fortran_writer, tmpdir):
    ''' Check that we can enclose an IF-block within a parallel region. '''
    psyir = fortran_reader.psyir_from_source(
                "program do_loop\n"
                "integer :: ji\n"
                "integer, parameter :: jpi=64\n"
                "logical :: init\n"
                "real :: sto_tmp(jpi), sto_tmp2(jpi)\n"
                "if(init)then\n"
                "  do ji = 1,jpi\n"
                "    sto_tmp(ji) = 1.0d0\n"
                "  end do\n"
                "else\n"
                "  do ji = 1,jpi\n"
                "    sto_tmp2(ji) = 1.0d0\n"
                "  end do\n"
                "end if\n"
                "end program do_loop\n")
    schedule = psyir.children[0]
    data_trans = TransInfo().get_trans_name('ACCDataTrans')
    acc_trans = TransInfo().get_trans_name('ACCParallelTrans')
    acc_trans.apply(schedule[0:1])
    data_trans.apply(schedule[0])
    code = fortran_writer(psyir).lower()
    assert ("  !$acc data copyout(sto_tmp,sto_tmp2)\n"
            "  !$acc parallel default(present)\n"
            "  if (init) then\n"
            "    do ji = 1, jpi, 1\n" in code)
    assert ("    enddo\n"
            "  end if\n"
            "  !$acc end parallel\n"
            "  !$acc end data\n" in code)
    assert Compile(tmpdir).string_compiles(code)
