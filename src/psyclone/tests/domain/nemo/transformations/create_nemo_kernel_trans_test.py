# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Lab
# Modified by S. Siso, STFC Daresbury Lab
# Modified by R. W. Ford, STFC Daresbury Lab

'''Module containing tests for the CreateNemoKernelTrans transformation.'''

from __future__ import absolute_import

import pytest

from psyclone.domain.nemo.transformations import CreateNemoKernelTrans
from psyclone.errors import LazyString
from psyclone.psyGen import InlinedKern
from psyclone.psyir.nodes import Loop, CodeBlock, Assignment
from psyclone.transformations import Transformation, TransformationError


BASIC_KERN_CODE = '''subroutine basic_kern()
  integer, parameter :: jpi=16
  integer :: ji
  real :: a(jpi), fconst
  do ji = 1, jpi
    a(ji) = fconst
    if(ji == jpi)then
      a(ji) = 0.0
    end if
  end do
end subroutine basic_kern
'''


def test_createkerneltrans_construction():
    ''' Check that we can construct the transformation object. '''
    trans = CreateNemoKernelTrans()
    assert isinstance(trans, Transformation)
    assert trans.name == "CreateNemoKernelTrans"


def test_kern_trans_validation(fortran_reader):
    ''' Test that the validate() method of the transformation correctly
    rejects things that aren't kernels. '''
    trans = CreateNemoKernelTrans()
    # Add a write() to the body of the loop
    code = BASIC_KERN_CODE.replace("  end do\n",
                                   "    write(*,*) ji\n  end do\n")
    psyir = fortran_reader.psyir_from_source(code)
    loop = psyir.walk(Loop)[0]
    # Check that calling apply() also calls validate()
    with pytest.raises(TransformationError) as err:
        trans.apply(loop)
    assert ("supplied node should be a PSyIR Schedule but found 'Loop'" in
            str(err.value))
    # Test validate() directly
    with pytest.raises(TransformationError) as err:
        trans.validate(loop)
    assert ("supplied node should be a PSyIR Schedule but found 'Loop'" in
            str(err.value))
    # We should reject the top-level routine schedule because it is not
    # within a loop
    with pytest.raises(TransformationError) as err:
        trans.validate(psyir.children[0])
    assert "supplied Schedule must be within a Loop" in str(err.value)
    # Loop body should not validate because it contains a Write statement
    # (which ends up as a CodeBlock)
    assert loop.walk(CodeBlock)
    with pytest.raises(TransformationError) as err:
        trans.validate(loop.loop_body)
    assert isinstance(err.value.value, LazyString)
    assert ("A NEMO Kernel cannot contain a node of type: 'CodeBlock'" in
            str(err.value))


def test_no_explicit_loop_in_kernel(fortran_reader):
    ''' Check that the transformation rejects a loop body if it includes
    an explicit loop. '''
    trans = CreateNemoKernelTrans()
    code = ("program fake_kern\n"
            "integer :: ji, jpj, idx\n"
            "real :: sto_tmp(5)\n"
            "do ji = 1,jpj\n"
            "  do idx = 1, 5\n"
            "    sto_tmp(ji) = 1.0\n"
            "  end do\n"
            "end do\n"
            "end program fake_kern\n")
    psyir = fortran_reader.psyir_from_source(code)
    loop = psyir.walk(Loop)[0]
    # 'loop.loop_body' is not a valid kernel because it itself contains a loop
    with pytest.raises(TransformationError) as err:
        trans.apply(loop.loop_body)
    assert "Kernel cannot contain a node of type: 'Loop'" in str(err.value)


def test_no_implicit_loop_in_kernel(fortran_reader):
    ''' Check that the transformation rejects a loop if it includes an implicit
    loop. '''
    trans = CreateNemoKernelTrans()
    code = ("program fake_kern\n"
            "integer :: ji, jpj\n"
            "real :: sto_tmp(5,5)\n"
            "do ji = 1,jpj\n"
            "  sto_tmp(:,:) = 1.0\n"
            "end do\n"
            "end program fake_kern\n")
    psyir = fortran_reader.psyir_from_source(code)
    loop = psyir.walk(Loop)[0]
    assert isinstance(loop.loop_body[0], Assignment)
    # 'loop.loop_body' is not a valid kernel because it contains an
    # assignment to an array range.
    with pytest.raises(TransformationError) as err:
        trans.apply(loop.loop_body)
    assert ("NEMO Kernel cannot contain array assignments but found: "
            "['sto_tmp(:,:) = 1.0']" in str(err.value))


def test_basic_kern(fortran_reader):
    ''' Check that the transformation correctly transforms a very simple
    kernel. '''
    trans = CreateNemoKernelTrans()
    psyir = fortran_reader.psyir_from_source(BASIC_KERN_CODE)
    loop = psyir.walk(Loop)[0]
    assign = loop.loop_body[0]
    trans.apply(loop.loop_body)
    assert isinstance(loop.loop_body[0], InlinedKern)
    # Check that the body of the kernel is still the original assignment
    assert loop.loop_body[0].children[0][0] is assign


def test_no_kernel_in_kernel(fortran_reader):
    ''' Check that the transformation refuses to create a Kernel if the
    supplied PSyIR already contains one. '''
    trans = CreateNemoKernelTrans()
    psyir = fortran_reader.psyir_from_source(BASIC_KERN_CODE)
    loop = psyir.walk(Loop)[0]
    trans.apply(loop.loop_body)
    with pytest.raises(TransformationError) as err:
        trans.apply(loop.loop_body)
    assert ("A NEMO Kernel cannot contain a node of type: 'NemoKern'" in
            str(err.value))
