# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2026, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
#         A. B. G. Chalk, STFC Daresbury Lab
# Modified I. Kavcic, Met Office
# Modified J. Henrichs, Bureau of Meteorology
# Modified M. Naylor, University of Cambridge, UK

'''Contains the tests for the OMPParallelLoopTrans transformations.'''

import logging
import pytest
from psyclone.psyir.nodes import (
    CodeBlock, Literal, Loop)
from psyclone.psyir.symbols import (
     ScalarType, DataSymbol)
from psyclone.psyir.transformations import TransformationError
from psyclone.tests.utilities import Compile
from psyclone.transformations import OMPParallelLoopTrans


def test_omplooptrans_apply_force_private(fortran_reader, fortran_writer,
                                          tmpdir, caplog):
    ''' Test applying the OMPParallelLoopTrans in cases where a list of
    explicit private variables is given. '''

    psyir = fortran_reader.psyir_from_source('''
        module my_mod
            contains
            subroutine my_subroutine()
                integer :: ji, jj, jk, jpkm1, jpjm1, jpim1, scalar1
                real, dimension(10, 10, 10) :: array1, array2
                array2 = 1
                do jk = 2, jpkm1, 1
                  do jj = 2, jpjm1, 1
                    do ji = 2, jpim1, 1
                       array2(ji,jj,jk) = array2(ji,jj,jk) + 1
                       array1(ji,jj,jk) = array2(ji,jj,jk)
                    enddo
                  enddo
                enddo
            end subroutine
        end module my_mod''')
    omplooptrans = OMPParallelLoopTrans()
    loop = psyir.walk(Loop)[0]

    caplog.clear()
    with caplog.at_level(logging.WARNING,
                         logger="psyclone.psyir.transformations"):
        omplooptrans.apply(loop, force_private=['array2', 'other'])

    # 'other' is not used, but this is logged
    assert ("OMPParallelLoopTrans has been provided with the 'other' symbol "
            "name in the 'force_private' option, but there is no such symbol "
            "in this scope." in caplog.text)

    # array2 is requested private, the shared_attibute_inference promotes it to
    # firstprivate because it is read first
    expected = '''\
    !$omp parallel do default(shared) private(ji,jj,jk) firstprivate(array2) \
schedule(auto)
    do jk = 2, jpkm1, 1\n'''

    gen = fortran_writer(psyir)
    assert expected in gen
    assert Compile(tmpdir).string_compiles(gen)


def test_omplooptrans_apply_firstprivate(fortran_reader, fortran_writer,
                                         tmpdir):
    ''' Test applying the OMPLoopTrans in cases where a firstprivate
    clause is needed to generate code that is functionally equivalent to the
    original, serial version.'''

    # Example with a conditional write and a OMPParallelDoDirective
    psyir = fortran_reader.psyir_from_source('''
        module my_mod
            contains
            subroutine my_subroutine()
                integer :: ji, jj, jk, jpkm1, jpjm1, jpim1, scalar1, scalar2
                real, dimension(10, 10, 10) :: zwt, zwd, zwi, zws
                scalar1 = 1
                do jk = 2, jpkm1, 1
                  do jj = 2, jpjm1, 1
                    do ji = 2, jpim1, 1
                       if (.true.) then
                          scalar1 = zwt(ji,jj,jk)
                       endif
                       scalar2 = scalar1 + zwt(ji,jj,jk)
                       zws(ji,jj,jk) = scalar2
                    enddo
                  enddo
                enddo
            end subroutine
        end module my_mod''')
    omplooptrans = OMPParallelLoopTrans()
    loop = psyir.walk(Loop)[0]
    omplooptrans.apply(loop)
    expected = '''\
    !$omp parallel do default(shared) private(ji,jj,jk,scalar2) \
firstprivate(scalar1) schedule(auto)
    do jk = 2, jpkm1, 1
      do jj = 2, jpjm1, 1
        do ji = 2, jpim1, 1
          if (.true.) then
            scalar1 = zwt(ji,jj,jk)
          end if
          scalar2 = scalar1 + zwt(ji,jj,jk)
          zws(ji,jj,jk) = scalar2
        enddo
      enddo
    enddo
    !$omp end parallel do\n'''

    gen = fortran_writer(psyir)
    assert expected in gen
    assert Compile(tmpdir).string_compiles(gen)


def test_omplooptrans_apply_firstprivate_fail(fortran_reader):
    ''' Test applying the OMPLoopTrans in cases where a firstprivate
    clause it is needed to generate functionally equivalent code than
    the starting serial version.

    In some cases the transformation validate dependency analysis reports
    the firstprivate use as a reduction, which is wrong.

    '''

    # Example with a read before write and a OMPParallelDirective
    psyir = fortran_reader.psyir_from_source('''
        subroutine my_subroutine()
            integer :: ji, jj, jk, jpkm1, jpjm1, jpim1, scalar1, scalar2
            real, dimension(10, 10, 10) :: zwt, zwd, zwi, zws
            do jk = 2, jpkm1, 1
              do jj = 2, jpjm1, 1
                do ji = 2, jpim1, 1
                   scalar2 = scalar1 + zwt(ji,jj,jk)
                   scalar1 = 3
                   zws(ji,jj,jk) = scalar2 + scalar1
                enddo
              enddo
            enddo
        end subroutine''')
    omplooptrans = OMPParallelLoopTrans()
    loop = psyir.walk(Loop)[0]
    try:
        omplooptrans.apply(loop)
    except TransformationError:
        # TODO #598: When this is solved, this test can be removed and the
        # "force":True in the previous test can also be removed
        pytest.xfail(reason="Issue #598: This example should be a firstprivate"
                            " but the dependency analysis believes it is a "
                            "reduction.")


def test_parallellooptrans_refuse_codeblock():
    ''' Check that ParallelLoopTrans.validate() rejects a loop nest that
    encloses a CodeBlock. We have to use OMPParallelLoopTrans as
    ParallelLoopTrans is abstract. '''
    otrans = OMPParallelLoopTrans()
    # Construct a valid Loop in the PSyIR with a CodeBlock in its body
    parent = Loop.create(DataSymbol("ji", ScalarType.integer_type()),
                         Literal("1", ScalarType.integer_type()),
                         Literal("10", ScalarType.integer_type()),
                         Literal("1", ScalarType.integer_type()),
                         [CodeBlock([], CodeBlock.Structure.STATEMENT,
                                    None)])
    with pytest.raises(TransformationError) as err:
        otrans.validate(parent)
    assert ("Nodes of type 'CodeBlock' cannot be enclosed "
            "by a OMPParallelLoopTrans transformation" in str(err.value))


@pytest.mark.parametrize("use_options_dict", [True, False])
def test_ompparallellooptrans_reductions(fortran_reader, fortran_writer,
                                         caplog, use_options_dict):
    '''Check that OMPParallelLoopTrans behaves correctly with reductions.
    The implementation is inherited from OMPLooptrans, however the options
    required differ for OMPParallelTrans (enable_reductions instead of
    reduction_ops).'''

    code = """subroutine test
    integer :: a, i

    do i = 1, 100
        a = a + i
    end do
    end subroutine test"""

    psyir = fortran_reader.psyir_from_source(code)
    loop = psyir.walk(Loop)[0]
    if use_options_dict:
        OMPParallelLoopTrans().apply(
            loop, options={
                "enable_reductions": True, "reduction_ops": []
            })
    else:
        # If not using the options dict we should have a logged message too.
        caplog.clear()
        with caplog.at_level(logging.WARNING,
                             "psyclone.psyir.transformations."
                             "omp_parallel_loop_trans"):
            OMPParallelLoopTrans().apply(loop, enable_reductions=True,
                                         reduction_ops=[])
            assert caplog.records[0].levelname == "WARNING"
            assert ("OMPParallelLoopTrans overrides the provided "
                    "reduction_ops keyword argument to those supported by "
                    "PSyclone." in caplog.text)

    correct = """!$omp parallel do default(shared) private(i) schedule(auto) \
reduction(+: a)
  do i = 1, 100, 1
    a = a + i
  enddo
  !$omp end parallel do
"""
    out = fortran_writer(psyir)
    assert correct in out
