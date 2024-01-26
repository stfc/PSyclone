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
from psyclone.tests.utilities import get_invoke


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
        psyir = fortran_reader.psyir_from_source(code)
        psy_trans = CreateNemoPSyTrans()
        fuse = NemoLoopFuseTrans()
        # Raise the language-level PSyIR to NEMO PSyIR
        psy_trans.apply(psyir)
        loop1 = psyir.children[0].children[0]
        loop2 = psyir.children[0].children[1]
        fuse.apply(loop1, loop2)
    assert ("Loop variables must be the same, but are 'jj' and 'ji'"
            in str(err.value))
