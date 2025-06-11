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
#   contributors may be used to endorse or promote products deribted from
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
# Author: H. Brunie, University of Grenoble Alpes

"""This module tests the RemoveConditionalTrans transformation.
"""

import pytest

from psyclone.psyir.nodes import Literal, Routine
from psyclone.psyir.symbols import INTEGER_TYPE
from psyclone.psyir.transformations import (
    RemoveIfBlockTrans,
    TransformationError,
)


# ----------------------------------------------------------------------------
def test_ribt_general():
    """Test general functionality of the transformation."""

    ribt = RemoveIfBlockTrans()

    assert ribt.name == "RemoveIfBlockTrans"


# ----------------------------------------------------------------------------
def test_ribt_errors():
    """Test errors that should be thrown."""

    ribt = RemoveIfBlockTrans()
    lit = Literal("1", INTEGER_TYPE)
    with pytest.raises(TransformationError) as err:
        ribt.apply(lit)

    assert "Transformation Error: Only handles Routine node." == str(err.value)


# ----------------------------------------------------------------------------
def test_ribt_remove_trivial_ifblock(fortran_reader, fortran_writer):
    """Tests remove trivial ifblock."""
    source = """program test
                integer ::x 
                if(.TRUE.)then
                    x = 3
                else
                    x =4
                endif
                if(.FALSE.)then
                    x = 5
                else
                    x =6
                endif
                end program test"""
    psyir = fortran_reader.psyir_from_source(source)
    # The first child is the assignment to 'invariant'
    routine = psyir.walk(Routine)[0]
    ribt = RemoveIfBlockTrans()
    ribt.apply(routine)
    out_routine = fortran_writer(routine)

    assert "x = 3" in out_routine
    assert "x = 6" in out_routine
    assert "x = 4" not in out_routine
    assert "x = 5" not in out_routine


def test_ribt_remove_trivial_ifblock_after_replace_ref_by_lit(fortran_reader, fortran_writer):
    """Tests remove trivial ifblock."""
    source = """program test
                logical, parameter :: cond1=.TRUE., cond2 = .FALSE.
                integer ::x 
                if(cond1)then
                    x = 3
                else
                    x =4
                endif
                if(cond2)then
                    x = 5
                else
                    x =6
                endif
                end program test"""
    psyir = fortran_reader.psyir_from_source(source)
    # The first child is the assignment to 'invariant'
    routine = psyir.walk(Routine)[0]
    from psyclone.psyir.transformations import ReplaceReferenceByLiteralTrans

    trans = ReplaceReferenceByLiteralTrans()
    trans.apply(routine)
    ribt = RemoveIfBlockTrans()
    ribt.apply(routine)
    out_routine = fortran_writer(routine)

    assert "x = 3" in out_routine
    assert "x = 6" in out_routine
    assert "x = 4" not in out_routine
    assert "x = 5" not in out_routine


# ----------------------------------------------------------------------------
def test_ribt_boolean_expr_involving_int_comparison(fortran_reader, fortran_writer):
    """Tests ifblock that cannot be removed."""
    source = """program test
                integer i
                real :: x
                integer, dimension(10) :: a
                do i = 1, 10
                    if (i < 5) then
                     a(i) = 0
                    else
                     a(i) = 1
                    endif
                    if (.FALSE.) then
                      x = 3
                    endif
                end do
                end program test"""
    psyir = fortran_reader.psyir_from_source(source)
    routine = psyir.walk(Routine)[0]

    # None of the statements can be moved, so the output
    # before and after the transformation should be identical:
    out_before = fortran_writer(routine)
    ribt = RemoveIfBlockTrans()
    ribt.apply(routine)
    out_after = fortran_writer(routine)
    assert "x = 3" in out_before
    assert "x = 3" not in out_after
    assert "a(i) = 0" in out_before
    assert "a(i) = 1" in out_before
    assert "a(i) = 0" in out_after
    assert "a(i) = 1" in out_after


# ----------------------------------------------------------------------------
def test_ribt_with_known_variables(fortran_reader, fortran_writer):
    """Tests removing ifblock with condition in known variables"""
    source = """program test
                logical :: cond
                real ::x
                namelist /MY_NAMELIST/ cond !.TRUE.
                if (cond) then
                    x = 2
                else
                    x = 3
                endif
                end program test"""
    psyir = fortran_reader.psyir_from_source(source)
    routine = psyir.walk(Routine)[0]
    ribt = RemoveIfBlockTrans({"cond": True})
    out_before = fortran_writer(psyir)
    ribt.apply(routine)
    out_after = fortran_writer(psyir)
    assert "x = 2" in out_before
    assert "x = 3" in out_before
    ## After application of the transformation
    assert "x = 2" in out_after
    assert "x = 3" not in out_after

def test_ribt_too_cmplx_boolean_expr(fortran_reader, fortran_writer):
    """"""
    source = """program test
                integer :: x,y
                logical :: b1
                logical, parameter:: b2 = .FALSE.
                x = 0 - 1
                b1 = .FALSE.
                if ((x + 3 - 2 == 0) .AND. b1 == b2) then
                    y = 0
                else
                    y = 1
                endif
                end program test"""
    psyir = fortran_reader.psyir_from_source(source)
    # The first child is the assignment to 'invariant'
    routine = psyir.walk(Routine)[0]

    # None of the statements can be moved, so the output
    # before and after the transformation should be identical:
    out_before = fortran_writer(routine)
    from psyclone.psyir.transformations import ReplaceReferenceByLiteralTrans
    trans = ReplaceReferenceByLiteralTrans()
    trans.apply(routine)
    ribt = RemoveIfBlockTrans({"x": -1, "b1": False})
    ribt.apply(routine)
    out_after = fortran_writer(routine)
    assert "if (x + 3 - 2 == 0 .AND. b1 == b2)" in out_before
    assert "y = 0" in out_before
    assert "y = 1" in out_before

    assert "if (x + 3 - 2 == 0 .AND. b1 == b2)" not in out_after
    assert "y = 0" in out_after
    assert "y = 1" not in out_after
