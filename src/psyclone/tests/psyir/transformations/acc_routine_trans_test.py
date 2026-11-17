# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2024-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Module containing tests for the ACCRoutineTrans transformation.

Note that the implementation itself is still in the 'old' module
psyclone.transformations. This is because it depends on the
 `MarkRoutineForGPUMixin` class in that module.

'''
import pytest

from psyclone.psyir.nodes import ACCRoutineDirective, Routine
from psyclone.transformations import ACCRoutineTrans, TransformationError


def test_accrt_validate_invalid_parallelism():
    '''Check that the validate() method rejects an unrecognised "parallelism"
    option.'''
    artrans = ACCRoutineTrans()
    with pytest.raises(TransformationError) as err:
        artrans.validate(Routine.create("fake"),
                         parallelism="wrong")
    assert ("'wrong' is not a supported level of parallelism. Should be one "
            "of [" in str(err.value))


def test_accrt_apply_calls_validate(monkeypatch):
    '''Check that the apply() method calls the validate() method.'''

    def fake_validate(_1, _2, **kwargs):
        raise NotImplementedError("this one")
    artrans = ACCRoutineTrans()
    monkeypatch.setattr(artrans, "validate", fake_validate)
    with pytest.raises(NotImplementedError) as err:
        artrans.apply(Routine.create("fake"))
    assert "this one" in str(err.value)


def test_accrt_apply(fortran_reader):
    '''Test that the apply() method works as expected.'''
    psyir = fortran_reader.psyir_from_source('''
module my_mod
  implicit none
contains
  subroutine this_one(a)
    real, dimension(:) :: a
    a(:) = 0.0
  end subroutine this_one
end module my_mod''')
    routine = psyir.walk(Routine)[0]
    routine_copy = routine.copy()
    assert routine.name == "this_one"
    artrans = ACCRoutineTrans()
    artrans.apply(routine)
    directives = routine.walk(ACCRoutineDirective)
    assert len(directives) == 1
    # Default level of parallelism is "seq".
    assert directives[0].begin_string() == "acc routine seq"
    assert directives[0].parent is routine
    # Applying it a second time should have no effect.
    artrans.apply(routine)
    directives2 = routine.walk(ACCRoutineDirective)
    assert len(directives2) == 1
    assert directives2[0] is directives[0]
    assert directives2[0].parent is routine
    # Check that we can specify the parallelism clause.
    # TODO #2668 deprecate options dict. Kept for coverage.
    artrans.apply(routine_copy, options={"parallelism": "vector"})
    directives3 = routine_copy.walk(ACCRoutineDirective)
    assert directives3[0].begin_string() == "acc routine vector"
