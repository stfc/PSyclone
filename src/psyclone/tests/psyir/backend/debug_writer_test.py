# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2024, Science and Technology Facilities Council.
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
# Author: S. Siso, STFC Daresbury Lab
# -----------------------------------------------------------------------------

'''Performs pytest tests on the psyclone.psyir.backend.debug_writer module'''


import pytest
from psyclone.psyir.backend.debug_writer import DebugWriter
from psyclone.psyir.nodes import OMPParallelDirective, Routine, Statement, \
    BinaryOperation, DataNode


EXAMPLE_CODE = '''
module example_mod
  implicit none
  contains
  subroutine example_code(i, j, cu, p, u)
    integer,  intent(in) :: I, J
    real, intent(out), dimension(:,:) :: cu
    real, intent(in),  dimension(:,:) :: p, u

    CU(I,J) = 0.5d0*(P(i+1,J)+P(I,J))*U(I,J)

  end subroutine example_code
end module example_mod
'''


def test_debug_writer_fortran(fortran_reader, fortran_writer):
    ''' For language level constructs it generated the same as Fortran, but it
    has the lowering and the validate_global_constraints check disabled. '''
    dwriter = DebugWriter()
    assert dwriter._validate_nodes is False
    assert dwriter._DISABLE_LOWERING is True

    psyir = fortran_reader.psyir_from_source(EXAMPLE_CODE)
    assert psyir.debug_string() == fortran_writer(psyir)


def test_debug_writer_higher_abstraction_nodes(fortran_reader):
    ''' For higer-level constructs (or language level nodes that need to be
    lowered like the OMPParallelDirective to get the private clauses) the
    lowering and globals constraints will not be executed for the DebugWriter.
    If there is no suitable generic visitor for printing it will just output
    the node as < str(node) >'''

    class MyDSLStatement(Statement):
        ''' A dummy DSL Statement that doesn't like being lowered '''
        def lower_to_language_level(self):
            raise NotImplementedError("This should not be called")

        def validate_global_constraints(self):
            raise NotImplementedError("This should not be called")

    class MyDSLDataNode(DataNode):
        ''' A dummy DSL DataNode that doesn't like being lowered '''
        def lower_to_language_level(self):
            raise NotImplementedError("This should not be called")

        def validate_global_constraints(self):
            raise NotImplementedError("This should not be called")

    # We need to call them at least once otherwise Codecov thinks this is
    # uncovered lines, but we purposely do not want them executed after
    # this
    with pytest.raises(NotImplementedError):
        MyDSLStatement().lower_to_language_level()
    with pytest.raises(NotImplementedError):
        MyDSLStatement().validate_global_constraints()
    with pytest.raises(NotImplementedError):
        MyDSLDataNode().validate_global_constraints()
    with pytest.raises(NotImplementedError):
        MyDSLDataNode().lower_to_language_level()

    # Introduce instances of the created DSL nodes into the example code
    psyir = fortran_reader.psyir_from_source(EXAMPLE_CODE)
    subroutine = psyir.walk(Routine)[0]
    content = subroutine.children[0].detach()
    subroutine.addchild(OMPParallelDirective.create())
    subroutine.children[0].dir_body.addchild(content)
    subroutine.addchild(MyDSLStatement())
    operation = psyir.walk(BinaryOperation)[0]
    operation.children[0].replace_with(MyDSLDataNode())

    assert psyir.debug_string() == '''\
module example_mod
  implicit none
  public

  contains
  subroutine example_code(i, j, cu, p, u)
    integer, intent(in) :: i
    integer, intent(in) :: j
    real, dimension(:,:), intent(out) :: cu
    real, dimension(:,:), intent(in) :: p
    real, dimension(:,:), intent(in) :: u

    !$omp parallel default(shared)
    cu(i,j) = < MyDSLDataNode[] > * u(i,j)
    !$omp end parallel
    < MyDSLStatement[] >

  end subroutine example_code

end module example_mod\n'''
