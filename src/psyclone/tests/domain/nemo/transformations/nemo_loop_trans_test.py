# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2024, Science and Technology Facilities Council.
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
# Authors: A. R. Porter and N. Nobre, STFC Daresbury Lab

''' Module containing tests for the NEMO-specific loop transformations. '''

from __future__ import absolute_import
import inspect
from importlib import import_module
import pytest
from psyclone.domain.nemo.transformations import NemoLoopFuseTrans, \
                                                 CreateNemoPSyTrans
from psyclone.psyir.nodes import Loop
from psyclone.psyir.transformations import LoopTrans
from psyclone.psyir.transformations.transformation_error import \
        TransformationError
from psyclone.tests.utilities import Compile, get_invoke


def fuse_loops(fortran_code, fortran_reader, fortran_writer):
    '''Helper function that fuses the first two nodes in the given
    Fortran code, and returns the fused Fortran code as string.
    If an error is detected by the used NemoLoopFuseTrans transformation,
    If an error is detected by the used LoopFuseTrans transformation,
    it will raise a TransformationError.
    :param str fortran_code: the Fortran code to loop fuse.
    '''
    psyir = fortran_reader.psyir_from_source(fortran_code)
    psy_trans = CreateNemoPSyTrans()
    fuse = NemoLoopFuseTrans()
    # Raise the language-level PSyIR to NEMO PSyIR
    psy_trans.apply(psyir)
    loop1 = psyir.children[0].children[0]
    loop2 = psyir.children[0].children[1]
    fuse.apply(loop1, loop2)

    return fortran_writer(psyir), psyir


def test_all_nemo_loop_trans_base_validate(monkeypatch):
    ''' Check that all transformations that sub-class LoopTrans call the
    base validate() method. '''
    # First get a valid Loop object that we can pass in.
    _, invoke = get_invoke("explicit_over_implicit.f90", api="nemo", idx=0)
    loop = invoke.schedule.walk(Loop)[0]

    # Get all transformations for the NEMO domain
    transmod = import_module("psyclone.domain.nemo.transformations")
    all_trans_classes = inspect.getmembers(transmod, inspect.isclass)

    # To ensure that we identify that the validate() method in the LoopTrans
    # base class has been called, we monkeypatch it to raise an exception.

    def fake_validate(_1, _2, options=None):
        raise NotImplementedError("validate test exception")
    monkeypatch.setattr(LoopTrans, "validate", fake_validate)

    for name, cls_type in all_trans_classes:
        trans = cls_type()
        if isinstance(trans, LoopTrans):
            # The Loop fuse validation function requires two
            # parameters (the two loops to fuse), so it needs
            # to be tested separately:
            if isinstance(trans, NemoLoopFuseTrans):
                with pytest.raises(NotImplementedError) as err:
                    trans.validate(loop, node2=loop)
            else:
                with pytest.raises(NotImplementedError) as err:
                    trans.validate(loop)
            assert "validate test exception" in str(err.value), \
                   f"{name}.validate() does not call LoopTrans.validate()"


def test_fuse_different_loop_vars(fortran_reader, fortran_writer):
    '''
    Test that loop variables are verified to be identical.
    '''
    code = '''subroutine sub()
              integer :: ji, jj, n
              integer, dimension(10,10) :: s, t
              do jj=1, n
                 do ji=1, 10
                    s(ji, jj)=t(ji, jj)+1
                 enddo
              enddo
              do ji=1, n
                 do jj=1, 10
                    s(ji, jj)=t(ji, jj)+1
                 enddo
              enddo
              end subroutine sub'''
    with pytest.raises(TransformationError) as err:
        fuse_loops(code, fortran_reader, fortran_writer)
    assert ("Loop variables must be the same, but are 'jj' and 'ji'"
            in str(err.value))


def test_fuse_incorrect_bounds_step(tmpdir, fortran_reader, fortran_writer):
    '''
    Test that loop boundaries and step size must be identical.
    '''
    # Lower loop boundary
    code = '''subroutine sub()
              integer :: ji, jj, n
              integer, dimension(10,10) :: s, t
              do jj=1, n
                 do ji=1, 10
                    s(ji, jj)=t(ji, jj)+1
                 enddo
              enddo
              do jj=2, n
                 do ji=1, 10
                    s(ji, jj)=t(ji, jj)+1
                 enddo
              enddo
              end subroutine sub'''
    with pytest.raises(TransformationError) as err:
        fuse_loops(code, fortran_reader, fortran_writer)
    assert "Lower loop bounds must be identical, but are" in str(err.value)

    # Upper loop boundary
    code = '''subroutine sub()
              integer :: ji, jj, n
              integer, dimension(10,10) :: s, t
              do jj=1, n
                 do ji=1, 10
                    s(ji, jj)=t(ji, jj)+1
                 enddo
              enddo
              do jj=1, n+1
                 do ji=1, 10
                    s(ji, jj)=t(ji, jj)+1
                 enddo
              enddo
              end subroutine sub'''
    with pytest.raises(TransformationError) as err:
        fuse_loops(code, fortran_reader, fortran_writer)
    assert "Upper loop bounds must be identical, but are" in str(err.value)

    # Test step size:
    code = '''subroutine sub()
              integer :: ji, jj, n
              integer, dimension(10,10) :: s, t
              do jj=1, n, 2
                 do ji=1, 10
                    s(ji, jj)=t(ji, jj)+1
                 enddo
              enddo
              do jj=1, n
                 do ji=1, 10
                    s(ji, jj)=t(ji, jj)+1
                 enddo
              enddo
              end subroutine sub'''
    with pytest.raises(TransformationError) as err:
        fuse_loops(code, fortran_reader, fortran_writer)
    assert "Step size in loops must be identical, but are" in str(err.value)

    # Test step size - make sure it defaults to 1
    code = '''subroutine sub()
              integer :: ji, jj, n
              integer, dimension(10,10) :: s, t
              do jj=1, n, 1
                 do ji=1, 10
                    s(ji, jj)=t(ji, jj)+1
                 enddo
              enddo
              do jj=1, n
                 do ji=1, 10
                    s(ji, jj)=t(ji, jj)+1
                 enddo
              enddo
              end subroutine sub'''
    out, _ = fuse_loops(code, fortran_reader, fortran_writer)
    assert Compile(tmpdir).string_compiles(out)


def test_fuse_no_symbol(fortran_reader, fortran_writer):
    '''Tests what happens if a variable name is not in the symbol table,
    e.g. because of a wildcard import. It also checks if a name is defined
    in an outer module.
    '''
    # Case 1: assume that the array 't' is imported from mymod. In
    # this case the loop validation will find a Symbol (not a DataSymbol),
    # and cannot test if this variable is an array. It should fall back
    # to use the variable accesses information (which includes indices),
    # knowing this way that this is an array.
    code = '''subroutine sub()
              use mymod
              integer :: ji, jj, n
              integer, dimension(10,10) :: s
              do jj=1, n
                 do ji=1, 10
                    s(ji, jj)=t(ji, jj)+1
                 enddo
              enddo
              do jj=1, n
                 do ji=1, 10
                    t(ji, jj) = s(ji, jj) + t(ji, jj)
                 enddo
              enddo
              end subroutine sub'''
    out, psyir = fuse_loops(code, fortran_reader, fortran_writer)
    assert """
  do jj = 1, n, 1
    do ji = 1, 10, 1
      s(ji,jj) = t(ji,jj) + 1
    enddo
    do ji = 1, 10, 1
      t(ji,jj) = s(ji,jj) + t(ji,jj)
    enddo
  enddo""" in out

    fuse = NemoLoopFuseTrans()
    # Case 2: Symbol 't' is defined in outer module:
    code = '''
    module mymod
        integer, dimension(10, 10) :: t
    contains
        subroutine sub()
            integer :: ji, jj, n
            integer, dimension(10,10) :: s
            do jj=1, n
               do ji=1, 10
                  s(ji, jj)=t(ji, jj)+1
               enddo
            enddo
            do jj=1, n
               do ji=1, 10
                  t(ji, jj) = s(ji, jj) + t(ji, jj)
               enddo
            enddo
        end subroutine sub
    end module mymod'''
    psyir = fortran_reader.psyir_from_source(code)
    # First child is now the subroutine, which has
    # two children which are the two loops:
    loop1 = psyir.children[0].children[0][0]
    loop2 = psyir.children[0].children[0][1]
    fuse.apply(loop1, loop2)
    out = fortran_writer(psyir)
    assert """
    do jj = 1, n, 1
      do ji = 1, 10, 1
        s(ji,jj) = t(ji,jj) + 1
      enddo
      do ji = 1, 10, 1
        t(ji,jj) = s(ji,jj) + t(ji,jj)
      enddo
    enddo""" in out
