# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Performs py.test tests on the subscript-triplet handler in the fparser2
    PSyIR front-end. Also tests the associated utility used to create arguments
    for the LBOUND and UBOUND array-query operations. '''

import pytest
from fparser.two.Fortran2003 import Execution_Part
from fparser.common.readfortran import FortranStringReader
from psyclone.errors import InternalError
from psyclone.psyir.frontend.fparser2 import Fparser2Reader, \
    _copy_full_base_reference
from psyclone.psyir.nodes import Schedule, Assignment, Member, ArrayMember, \
    StructureMember, Reference, ArrayReference


@pytest.mark.usefixtures("f2008_parser")
def test_subscript_triplet_handler_error():
    ''' Check that the subscript-triplet handler raises the expected error if
    the parent PSyIR node is not of the correct type. '''
    processor = Fparser2Reader()
    fake_parent = Schedule()
    reader = FortranStringReader("x(:) = a")
    fp2node = Execution_Part.match(reader)
    triplet = fp2node[0][0].children[0].children[1].children[0]
    with pytest.raises(InternalError) as err:
        processor._subscript_triplet_handler(triplet, fake_parent)
    assert ("Expected parent PSyIR node to be either a Reference or a Member "
            "but got 'Schedule' when processing ':'" in str(err.value))


def test_ubound_lbound_arg_error():
    ''' Check that the _copy_full_base_reference utility method raises the
    expected error if the supplied node is of the wrong type. '''
    with pytest.raises(InternalError) as err:
        _copy_full_base_reference(Schedule())
    assert ("supplied node must be an instance of either Reference or "
            "Member but got 'Schedule'" in str(err.value))


def test_ubound_lbound_arg(fortran_reader):
    ''' Tests for the _copy_full_base_reference utility method. '''
    code = ("subroutine my_sub()\n"
            "  use some_mod, only: my_type\n"
            "  type(my_type) :: var, vars(3)\n"
            "  var%region%subgrid(3)%data(:) = 1.0\n"
            "  vars(1)%region%subgrid(3)%data(:) = 1.0\n"
            "  vars(1)%region%subgrid(:)%data(:) = 1.0\n"
            "  vars(:)%region%subgrid(3)%xstop = 1.0\n"
            "end subroutine my_sub\n")
    psyir = fortran_reader.psyir_from_source(code)
    assignments = psyir.walk(Assignment)
    # var%region%subgrid(3)%data(:)
    assign = assignments[0]
    arg = _copy_full_base_reference(assign.lhs.member.member.member)
    assert arg.member.member.member.name == "data"
    assert isinstance(arg.member.member.member, Member)
    assert not isinstance(arg.member.member.member, ArrayMember)
    # vars(1)%region%subgrid(3)%data(:)
    assign = assignments[1]
    arg = _copy_full_base_reference(assign.lhs.member.member.member)
    assert arg.member.member.member.name == "data"
    assert isinstance(arg.member.member.member, Member)
    assert not isinstance(arg.member.member.member, ArrayMember)
    # vars(1)%region%subgrid(:)%data(:)
    assign = assignments[2]
    # For the first colon
    arg = _copy_full_base_reference(assign.lhs.member.member)
    assert arg.member.member.name == "subgrid"
    assert isinstance(arg.member.member, Member)
    assert not isinstance(arg.member.member, (ArrayMember, StructureMember))
    # For the second colon
    arg = _copy_full_base_reference(assign.lhs.member.member.member)
    assert arg.member.member.member.name == "data"
    assert isinstance(arg.member.member.member, Member)
    assert not isinstance(arg.member.member.member, ArrayMember)
    # vars(:)%region%subgrid(3)%xstop
    assign = assignments[3]
    arg = _copy_full_base_reference(assign.lhs)
    assert arg.symbol.name == "vars"
    assert isinstance(arg, Reference)
    assert not isinstance(arg, ArrayReference)
