# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2024, Science and Technology Facilities Council.
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
# Authors R. W. Ford and S. Siso, STFC Daresbury Lab

'''Module containing tests for the NemoOuterArrayRange2LoopTrans
transformation.'''

import pytest

from fparser.common.readfortran import FortranStringReader
from psyclone.psyir.nodes import Assignment, CodeBlock
from psyclone.psyGen import Transformation, PSyFactory
from psyclone.psyir.transformations import TransformationError
from psyclone.domain.nemo.transformations import NemoOuterArrayRange2LoopTrans
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.tests.utilities import get_invoke, Compile

# Constants
API = "nemo"


def test_transform():
    '''Check that it is possible to create an instance of
    NemoArrayRange2LoopTrans and that it is a Transformation.

    '''
    assert NemoOuterArrayRange2LoopTrans()
    assert isinstance(NemoOuterArrayRange2LoopTrans(), Transformation)


def test_transform_apply_mixed_implicit_do(tmpdir):
    '''Check that the PSyIR is transformed as expected for a lat,lon,levs
    loop with some of its indices accessed using array notation and
    some using explicit loops.  The resultant Fortran code is used to
    confirm the transformation has worked correctly.

    '''
    _, invoke_info = get_invoke("explicit_over_implicit.f90", api=API, idx=0)
    schedule = invoke_info.schedule
    assignment = schedule[0].loop_body[0]
    trans = NemoOuterArrayRange2LoopTrans()
    # Apply transformation 2 times, once for each implicit
    # dimension
    trans.apply(assignment)
    writer = FortranWriter()
    result = writer(schedule)
    expected = (
        "  do jk = 1, jpk, 1\n"
        "    do idx = LBOUND(umask, dim=2), UBOUND(umask, dim=2), 1\n"
        "      umask(:,idx,jk) = vmask(:,idx,jk) + 1.0\n"
        "    enddo\n"
        "  enddo")
    assert expected in result
    trans.apply(assignment)
    result = writer(schedule)
    expected = (
        "  do jk = 1, jpk, 1\n"
        "    do idx = LBOUND(umask, dim=2), UBOUND(umask, dim=2), 1\n"
        "      do idx_1 = LBOUND(umask, dim=1), UBOUND(umask, dim=1), 1\n"
        "        umask(idx_1,idx,jk) = vmask(idx_1,idx,jk) + 1.0\n"
        "      enddo\n"
        "    enddo\n"
        "  enddo")
    assert expected in result
    assert Compile(tmpdir).string_compiles(result)


def test_apply_with_structures(fortran_reader, fortran_writer):
    '''Check that the PSyIR is transformed as expected when the
    expressions contain a mix of arrays and structures in the same
    reference.

    '''
    trans = NemoOuterArrayRange2LoopTrans()

    # The outer dimension is already set
    psyir = fortran_reader.psyir_from_source('''
    subroutine test
        use my_variables
        integer, parameter :: constant = 3
        integer :: jk
        base%field(constant)%array(:,:,jk) = 1
    end subroutine test
    ''')
    assignment = psyir.walk(Assignment)[0]
    trans.apply(assignment)
    result = fortran_writer(assignment)
    assert "base%field(constant)%array(:,idx,jk) = 1" in result
    trans.apply(assignment)
    result = fortran_writer(assignment)
    assert "base%field(constant)%array(idx_1,idx,jk) = 1" in result

    # The inner dimension is already set
    psyir = fortran_reader.psyir_from_source('''
    subroutine test
        use my_variables
        integer, parameter :: jf = 3, jpi = 3, jpim1 = 1
        ptab(jf)%pt2d(jpi,:,:) = ptab(jf)%pt2d(jpim1,:,:)
    end subroutine test
    ''')
    assignment = psyir.walk(Assignment)[0]
    trans.apply(assignment)
    result = fortran_writer(assignment)
    assert "ptab(jf)%pt2d(jpi,:,idx) = ptab(jf)%pt2d(jpim1,:,idx)" in result
    trans.apply(assignment)
    result = fortran_writer(assignment)
    assert ("ptab(jf)%pt2d(jpi,idx_1,idx) = "
            "ptab(jf)%pt2d(jpim1,idx_1,idx)" in result)

    # Array index expression includes an array reference.
    psyir = fortran_reader.psyir_from_source('''
    subroutine test
        use my_variables
        integer, parameter :: jf = 3, jpi = 3, jpim1 = 1
        ptab(jf)%pt2d(:,map(jf)+1,:) = ptab(jf)%pt2d(:,3,:)
    end subroutine test
    ''')
    assignment = psyir.walk(Assignment)[0]
    trans.apply(assignment)
    result = fortran_writer(assignment)
    assert ("ptab(jf)%pt2d(:,map(jf) + 1,idx) = ptab(jf)%pt2d(:,3,idx)"
            in result)
    trans.apply(assignment)
    result = fortran_writer(assignment)
    assert ("ptab(jf)%pt2d(idx_1,map(jf) + 1,idx) = ptab(jf)%pt2d(idx_1,3,idx)"
            in result)


def test_apply_calls_validate():
    '''Check that the apply() method calls the validate method.'''
    trans = NemoOuterArrayRange2LoopTrans()
    with pytest.raises(TransformationError) as info:
        trans.apply(None)
    assert ("Error in NemoOuterArrayRange2LoopTrans transformation. The "
            "supplied node argument should be a PSyIR Assignment, but "
            "found 'NoneType'." in str(info.value))


def test_str():
    '''Test that the str of an instance of the NemoArrayRange2LoopTrans class
    returns the expected value.

    '''
    assert (str(NemoOuterArrayRange2LoopTrans()) == "Convert a PSyIR "
            "assignment to the outermost ArrayReference Range into a "
            "PSyIR NemoLoop.")


def test_name():
    '''Check that the name property of the NemoOuterArrayRange2LoopTrans
    class returns the expected value.

    '''
    assert (NemoOuterArrayRange2LoopTrans().name ==
            "NemoOuterArrayRange2LoopTrans")


def test_validate_assignment():
    '''Check that the validate method raises an exception if the supplied
    argument is not an Assignment node.

    '''
    trans = NemoOuterArrayRange2LoopTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(None)
    assert ("Error in NemoOuterArrayRange2LoopTrans transformation. The "
            "supplied node argument should be a PSyIR Assignment, but "
            "found 'NoneType'." in str(info.value))


def test_validate_array_reference(parser):
    '''Check that the validate method raises an exception if the supplied
    argument is not an Assignment node with an array reference on its lhs.

    '''
    code = '''subroutine data_ref()
  use some_mod, only: prof_type
  INTEGER, parameter :: n=16
  INTEGER :: ji
  real :: a(n), fconst
  type(prof_type) :: prof
  do ji = 1, n
     prof%npind(ji) = 2.0*a(ji) + fconst
  end do
END subroutine data_ref
'''
    reader = FortranStringReader(code)
    prog = parser(reader)
    psy = PSyFactory(API, distributed_memory=False).create(prog)
    schedule = psy.invokes.invoke_list[0].schedule
    assignment = schedule.walk(Assignment)[0]
    # Break the PSyIR so that there is a CodeBlock on the LHS
    assignment.children[0] = CodeBlock(prog.children,
                                       CodeBlock.Structure.EXPRESSION)
    trans = NemoOuterArrayRange2LoopTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(assignment)
    assert ("Transformation Error: Error in NemoOuterArrayRange2LoopTrans "
            "transformation. The LHS of the supplied assignment node should be"
            " a Reference that contains an array access somewhere in the "
            "expression, but found 'CodeBlock[1 nodes]'." in str(info.value))


# lhs array reference has a range
def test_validate_range():
    '''Check that the validate method raises an exception if the supplied
    argument is not an Assignment node with an array reference on its
    lhs containing at least one Range node.

    '''
    _, invoke_info = get_invoke("explicit_do.f90", api=API, idx=0)
    schedule = invoke_info.schedule
    assignments = schedule.walk(Assignment)
    assert len(assignments) == 1
    assignment = assignments[0]
    trans = NemoOuterArrayRange2LoopTrans()
    with pytest.raises(TransformationError) as info:
        trans.validate(assignment)
    assert ("Error in NemoOuterArrayRange2LoopTrans transformation. "
            "The LHS of the supplied assignment node should be an expression "
            "with an array that has a Range node, but found 'ArrayReference"
            in str(info.value))
