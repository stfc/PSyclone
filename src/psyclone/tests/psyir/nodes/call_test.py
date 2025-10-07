# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2025, Science and Technology Facilities Council.
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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Performs py.test tests on the Call PSyIR node. '''

import os
import pytest
from psyclone.configuration import Config
from psyclone.core import Signature
from psyclone.errors import GenerationError
from psyclone.psyir.nodes import (
    ArrayReference, BinaryOperation, Call, Literal,
    Node, Reference, Routine, Schedule)
from psyclone.psyir.nodes.call import CallMatchingArgumentsNotFound
from psyclone.psyir.nodes.node import colored
from psyclone.psyir.symbols import (
    ArrayType, INTEGER_TYPE, ContainerSymbol, DataSymbol, NoType,
    RoutineSymbol, REAL_TYPE, SymbolError, UnresolvedInterface)


class SpecialCall(Call):
    '''Test Class specialising the Call class'''


def test_call_init():
    '''Test that a Call can be created as expected. Also test the routine
    property.

    '''
    # Initialise without a RoutineSymbol
    call = Call()
    # By default everything is None
    assert call.routine is None
    assert call.parent is None
    assert len(call.arguments) == 0
    assert call.is_elemental is None
    assert call.is_pure is None

    # Initialise with parent and add routine and argument children
    parent = Schedule()
    routine = RoutineSymbol("jo", NoType())
    call = Call(parent=parent)
    call.addchild(Reference(routine))
    call.addchild(Literal('3', INTEGER_TYPE))
    assert call.routine.symbol is routine
    assert call.parent is parent
    assert call.arguments == (Literal('3', INTEGER_TYPE),)


def test_call_is_elemental():
    '''Test the is_elemental property of a Call is set correctly and can be
    queried.'''
    routine = RoutineSymbol("zaphod", NoType())
    call = Call.create(routine)
    assert call.is_elemental is None
    routine = RoutineSymbol("beeblebrox", NoType(), is_elemental=True)
    call = Call.create(routine)
    assert call.is_elemental is True


def test_call_is_pure():
    '''Test the is_pure property of a Call is set correctly and can be
    queried.'''
    routine = RoutineSymbol("zaphod", NoType())
    call = Call.create(routine)
    assert call.is_pure is None
    routine = RoutineSymbol("beeblebrox", NoType(), is_pure=True)
    call = Call.create(routine)
    assert call.is_pure is True


def test_call_is_available_on_device():
    '''Test the is_available_on_device() method of a Call (currently always
    returns False). '''
    routine = RoutineSymbol("zaphod", NoType())
    call = Call.create(routine)
    assert call.is_available_on_device() is False


def test_call_equality():
    '''Test the __eq__ method of the Call class. '''
    # routine arguments
    routine = RoutineSymbol("j", NoType())
    routine2 = RoutineSymbol("k", NoType())
    call1 = Call.create(routine)
    call2 = Call.create(routine)
    assert call1 == call2

    call3 = Call.create(routine2)
    assert call1 != call3

    # Check with argument names
    call4 = Call.create(routine, [("name", Literal("1.0", REAL_TYPE))])
    call5 = Call.create(routine, [("name", Literal("1.0", REAL_TYPE))])
    assert call4 == call5

    # Check with argument name and no argument name
    call6 = Call.create(routine, [Literal("1.0", REAL_TYPE)])
    assert call4 != call6

    # Check with different argument names
    call7 = Call.create(routine, [("new_name", Literal("1.0", REAL_TYPE))])
    assert call4 != call7

    # Check when a Reference (to the same RoutineSymbol) is provided.
    call8 = Call.create(Reference(routine),
                        [("new_name", Literal("1.0", REAL_TYPE))])
    assert call8 == call7


@pytest.mark.parametrize("cls", [Call, SpecialCall])
def test_call_create(cls):
    '''Test that the create method creates a valid call with arguments,
    some of which are named. Also checks the routine and argument_names
    properties.

    '''
    routine = RoutineSymbol("ellie", INTEGER_TYPE)
    array_type = ArrayType(INTEGER_TYPE, shape=[10, 20])
    arguments = [Reference(DataSymbol("arg1", INTEGER_TYPE)),
                 ArrayReference(DataSymbol("arg2", array_type))]
    call = cls.create(routine, [arguments[0], ("name", arguments[1])])
    # pylint: disable=unidiomatic-typecheck
    assert type(call) is cls
    assert call.routine.symbol is routine
    assert call.argument_names == [None, "name"]
    for idx, child, in enumerate(call.arguments):
        assert child is arguments[idx]
        assert child.parent is call


def test_call_create_error1():
    '''Test that the appropriate exception is raised if the routine
    argument to the create method is not a RoutineSymbol.

    '''
    with pytest.raises(TypeError) as info:
        _ = Call.create(None, [])
    assert ("The Call routine argument should be a Reference to a "
            "RoutineSymbol or a RoutineSymbol, but found "
            "'NoneType'." in str(info.value))


def test_call_create_error2():
    '''Test that the appropriate exception is raised if the arguments
    argument to the create method is not a list'''
    routine = RoutineSymbol("isaac", NoType())
    with pytest.raises(GenerationError) as info:
        _ = Call.create(routine, None)
    assert ("Call.create 'arguments' argument should be an Iterable but found "
            "'NoneType'." in str(info.value))


def test_call_create_error3():
    '''Test that the appropriate exception is raised if one or more of the
    argument names is not valid.'''
    routine = RoutineSymbol("roo", INTEGER_TYPE)
    with pytest.raises(ValueError) as info:
        _ = Call.create(
            routine, [Reference(DataSymbol(
                "arg1", INTEGER_TYPE)), (" a", None)])
    assert "Invalid Fortran name ' a' found." in str(info.value)


def test_call_create_error4():
    '''Test that the appropriate exception is raised if one or more of the
    arguments argument list entries to the create method is not a
    DataNode.

    '''
    routine = RoutineSymbol("roo", INTEGER_TYPE)
    with pytest.raises(GenerationError) as info:
        _ = Call.create(
            routine, [Reference(DataSymbol(
                "arg1", INTEGER_TYPE)), ("name", None)])
    assert ("Item 'NoneType' can't be child 2 of 'Call'. The valid format "
            "is: 'Reference, [DataNode]*'." in str(info.value))


def test_call_add_args():
    '''Test the _add_args method in the Call class.'''

    routine = RoutineSymbol("myeloma", INTEGER_TYPE)
    call = Call.create(routine)
    array_type = ArrayType(INTEGER_TYPE, shape=[10, 20])
    arguments = [Reference(DataSymbol("arg1", INTEGER_TYPE)),
                 ArrayReference(DataSymbol("arg2", array_type))]
    Call._add_args(call, [arguments[0], ("name", arguments[1])])
    assert call.routine.symbol is routine
    assert call.argument_names == [None, "name"]
    for idx, child, in enumerate(call.arguments):
        assert child is arguments[idx]
        assert child.parent is call
    # For some reason pylint thinks that call.children[0,1] are of
    # type Literal and complains about there being no name member,
    # even though they are not.
    # pylint: disable=no-member
    assert call.arguments[0].name == "arg1"
    assert call.arguments[1].name == "arg2"


def test_call_add_args_error1():
    '''Test that the appropriate exception is raised if an entry in the
    arguments argument to the _add_args method is a tuple that does
    not have two elements.

    '''
    routine = RoutineSymbol("isaac", NoType())
    with pytest.raises(GenerationError) as info:
        _ = Call._add_args(routine, [(1, 2, 3)])
    assert ("If a child of the children argument in create method of Call "
            "class is a tuple, it's length should be 2, but found 3."
            in str(info.value))


def test_call_add_args_error2():
    '''Test that the appropriate exception is raised if an entry in the
    arguments argument to the _add_args method is is a tuple with two
    elements and the first element is not a string.'''
    routine = RoutineSymbol("isaac", NoType())
    with pytest.raises(GenerationError) as info:
        _ = Call._add_args(routine, [(1, 2)])
    assert ("If a child of the children argument in create method of Call "
            "class is a tuple, its first argument should be a str, but "
            "found int." in str(info.value))


def test_call_appendnamedarg():
    '''Test the append_named_arg method in the Call class. Check
    it raises the expected exceptions if arguments are invalid and
    that it works as expected when the input is valid.

    '''
    op1 = Literal("1", INTEGER_TYPE)
    op2 = Literal("2", INTEGER_TYPE)
    op3 = Literal("3", INTEGER_TYPE)
    call = Call.create(RoutineSymbol("hello"), [])
    # name arg wrong type
    with pytest.raises(TypeError) as info:
        call.append_named_arg(1, op1)
    assert ("A name should be a string, but found 'int'."
            in str(info.value))
    # invalid name
    with pytest.raises(ValueError) as info:
        call.append_named_arg("_", op1)
    assert "Invalid Fortran name '_' found." in str(info.value)
    # name arg already used
    call.append_named_arg("name1", op1)
    with pytest.raises(ValueError) as info:
        call.append_named_arg("name1", op2)
    assert ("The value of the name argument (name1) in 'append_named_arg' in "
            "the 'Call' node is already used for a named argument."
            in str(info.value))
    # ok
    call.append_named_arg("name2", op2)
    call.append_named_arg(None, op3)
    assert call.arguments == (op1, op2, op3)
    assert call.argument_names == ["name1", "name2", None]


def test_call_insertnamedarg():
    '''Test the insert_named_arg method in the Call class. Check
    it raises the expected exceptions if arguments are invalid and
    that it works as expected when the input is valid.

    '''
    op1 = Literal("1", INTEGER_TYPE)
    op2 = Literal("2", INTEGER_TYPE)
    op3 = Literal("3", INTEGER_TYPE)
    call = Call.create(RoutineSymbol("hello"), [])
    # name arg wrong type
    with pytest.raises(TypeError) as info:
        call.insert_named_arg(1, op1, 0)
    assert ("A name should be a string, but found 'int'."
            in str(info.value))
    # invalid name
    with pytest.raises(ValueError) as info:
        call.insert_named_arg("1", op1, 0)
    assert "Invalid Fortran name '1' found." in str(info.value)
    # name arg already used
    call.insert_named_arg("name1", op1, 0)
    with pytest.raises(ValueError) as info:
        call.insert_named_arg("name1", op2, 0)
    assert ("The value of the name argument (name1) in 'insert_named_arg' in "
            "the 'Call' node is already used for a named argument."
            in str(info.value))
    # invalid index type
    with pytest.raises(TypeError) as info:
        call.insert_named_arg("name2", op2, "hello")
    assert ("The 'index' argument in 'insert_named_arg' in the 'Call' node "
            "should be an int but found str." in str(info.value))
    # ok
    assert call.arguments == (op1,)
    assert call.argument_names == ["name1"]
    call.insert_named_arg("name2", op2, 0)
    assert call.arguments == (op2, op1)
    assert call.argument_names == ["name2", "name1"]
    call.insert_named_arg(None, op3, 0)
    assert call.arguments == (op3, op2, op1)
    assert call.argument_names == [None, "name2", "name1"]


def test_call_replacenamedarg():
    '''Test the replace_named_arg method in the Call class. Check
    it raises the expected exceptions if arguments are invalid and
    that it works as expected when the input is valid.

    '''
    op1 = Literal("1", INTEGER_TYPE)
    op2 = Literal("2", INTEGER_TYPE)
    op3 = Literal("3", INTEGER_TYPE)
    call = Call.create(RoutineSymbol("hello"),
                       [("name1", op1), ("name2", op2)])

    # name arg wrong type
    with pytest.raises(TypeError) as info:
        call.replace_named_arg(1, op3)
    assert ("The 'name' argument in 'replace_named_arg' in the 'Call' "
            "node should be a string, but found int." in str(info.value))
    # name arg is not found
    with pytest.raises(ValueError) as info:
        call.replace_named_arg("new_name", op3)
    assert ("The value of the existing_name argument (new_name) in "
            "'replace_named_arg' in the 'Call' node was not found in the "
            "existing arguments." in str(info.value))
    # ok - including change in case
    assert call.arguments == (op1, op2)
    assert call.argument_names == ["name1", "name2"]
    assert call._argument_names[0][0] == id(op1)
    assert call._argument_names[1][0] == id(op2)
    call.replace_named_arg("nAMe1", op3)
    assert call.arguments == (op3, op2)
    assert call.argument_names == ["nAMe1", "name2"]
    assert call._argument_names[0][0] == id(op3)
    assert call._argument_names[1][0] == id(op2)


def test_call_argument_by_name():
    '''Test the argument_by_name method.'''
    op1 = Literal("1", INTEGER_TYPE)
    op2 = Literal("2", INTEGER_TYPE)
    op3 = Literal("3", INTEGER_TYPE)
    call = Call.create(RoutineSymbol("hello"), [op1, ("a", op2), ("b", op3)])
    assert call.argument_by_name("z") is None
    assert call.argument_by_name("a") is op2
    assert call.argument_by_name("b") is op3


def test_call_reference_accesses():
    '''Test the reference_accesses() method.'''
    rsym = RoutineSymbol("trillian")
    # A call with an argument passed by value.
    call1 = Call.create(rsym, [Literal("1", INTEGER_TYPE)])
    var_info = call1.reference_accesses()
    # The Routine symbol is not considered 'read'.
    assert not var_info.is_read(Signature("trillian"))
    assert var_info.is_called(Signature("trillian"))
    dsym = DataSymbol("beta", INTEGER_TYPE)
    # Simple argument passed by reference.
    call2 = Call.create(rsym, [Reference(dsym)])
    var_info = call2.reference_accesses()
    assert var_info.has_read_write(Signature("beta"))
    assert not var_info.is_called(Signature("beta"))
    # Array access argument. The array should be READWRITE, any variable in
    # the index expression should be READ.
    idx_sym = DataSymbol("ji", INTEGER_TYPE)
    asym = DataSymbol("gamma", ArrayType(INTEGER_TYPE, shape=[10]))
    aref = ArrayReference.create(asym, [Reference(idx_sym)])
    call3 = Call.create(rsym, [aref])
    var_info = call3.reference_accesses()
    assert var_info.has_read_write(Signature("gamma"))
    assert var_info.is_read(Signature("ji"))
    # Argument is a temporary so any inputs to it are READ only.
    expr = BinaryOperation.create(BinaryOperation.Operator.MUL,
                                  Literal("2", INTEGER_TYPE), Reference(dsym))
    call4 = Call.create(rsym, [expr])
    var_info = call4.reference_accesses()
    assert var_info.is_read(Signature("beta"))
    # Argument is itself a function call: call trillian(some_func(gamma(ji)))
    fsym = RoutineSymbol("some_func")
    fcall = Call.create(fsym,
                        [ArrayReference.create(asym, [Reference(idx_sym)])])
    call5 = Call.create(rsym, [fcall])
    var_info = call5.reference_accesses()
    assert var_info.has_read_write(Signature("gamma"))
    assert var_info.is_read(Signature("ji"))
    # Call to a routine - if the definition is not found, they will be RW
    puresym = RoutineSymbol("dirk", is_pure=True)
    call6 = Call.create(puresym, [Reference(dsym)])
    var_info = call6.reference_accesses()
    assert var_info.is_read(Signature("beta"))
    assert var_info.is_written(Signature("beta"))


def test_call_reference_accesses_findable_routine(fortran_reader):
    '''Test the reference_accesses() when the psyir call find the declaration
    of a routine'''
    psyir = fortran_reader.psyir_from_source("""
    subroutine return_scalar(x, y, z)
        integer, intent(in) :: x
        integer, intent(out) :: y
        integer, intent(inout) :: z
        y = x + 1
    end subroutine return_scalar

    subroutine test()
        integer :: x, y, z
        call return_scalar(x, y, z)
    end subroutine test
    """)
    test_routine = psyir.walk(Routine)[1]
    assert test_routine.name == "test"
    call = test_routine.walk(Call)[0]
    vam = call.reference_accesses()
    assert vam.is_read(Signature("x"))
    assert not vam.is_written(Signature("x"))
    assert not vam.is_read(Signature("y"))
    assert vam.is_written(Signature("y"))
    assert vam.is_read(Signature("z"))
    assert vam.is_written(Signature("z"))


def test_type_bound_call_reference_accesses(fortran_reader):
    '''Test the reference_accesses() method for a call to a type-bound
    procedure.

    TODO #2823 - we currently make dangerous assumptions about accesses
    to variables if whether or not they are being used as function
    arguments is ambiguous.

    '''
    code = '''\
    module my_mod
    contains
    pure function get_start(idx) result(start)
      integer, intent(in) :: idx
      integer :: start
      start = idx - 1
    end function get_start
    subroutine my_sub
      use some_mod, only: my_grid, domain
      integer :: i, j, k, f
      ! We know that get_start() is a call to a pure function so 'f' is
      ! read only.
      call my_grid(k)%update(get_start(f), domain%get_start(i),j)
    end subroutine my_sub
    end module my_mod
    '''
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    vam = call.reference_accesses()
    # The type-bound procedure is called.
    assert vam.is_called(Signature("my_grid%update"))
    # As is the function defined in the same module.
    assert vam.is_called(Signature("get_start"))
    # All of the indices are marked as read.
    for var in ["i", "j", "k", "f"]:
        assert vam.is_read(Signature(var))
    # Only the arguments to the calls are marked as read-write.
    assert vam.has_read_write(Signature("j"))
    assert not vam.has_read_write(Signature("k"))
    assert not vam.has_read_write(Signature("f"))
    # We can't tell whether 'domain%get_start(i)' is an array access
    # or a function call. We currently, dangerously, assume it is the former.
    if not vam.has_read_write(Signature("i")):
        pytest.xfail(reason="TODO #2823 - potential array accesses/function "
                     "calls are always assumed to be array accesses. This is "
                     "unsafe.")


def test_call_argumentnames_after_removearg():
    '''Test the argument_names property makes things consistent if a child
    argument is removed. This is used transparently by the class to
    keep things consistent.

    '''
    op1 = Literal("1", INTEGER_TYPE)
    op2 = Literal("1", INTEGER_TYPE)
    call = Call.create(RoutineSymbol("name"), [("name1", op1), ("name2", op2)])
    assert len(call.arguments) == 2
    assert len(call._argument_names) == 2
    assert call.argument_names == ["name1", "name2"]
    call.children.pop(1)
    assert len(call.arguments) == 1
    assert len(call._argument_names) == 2
    # argument_names property makes _argument_names list consistent.
    assert call.argument_names == ["name2"]
    assert len(call._argument_names) == 1


def test_call_argumentnames_after_addarg():
    '''Test the argument_names property makes things consistent if a child
    argument is added. This is used transparently by the class to
    keep things consistent.

    '''
    op1 = Literal("1", INTEGER_TYPE)
    op2 = Literal("1", INTEGER_TYPE)
    op3 = Literal("1", INTEGER_TYPE)
    call = Call.create(RoutineSymbol("name"), [("name1", op1), ("name2", op2)])
    assert len(call.arguments) == 2
    assert len(call._argument_names) == 2
    assert call.argument_names == ["name1", "name2"]
    call.children.append(op3)
    assert len(call.arguments) == 3
    assert len(call._argument_names) == 2
    # argument_names property makes _argument_names list consistent.
    assert call.argument_names == ["name1", "name2", None]
    assert len(call._argument_names) == 3


def test_call_argumentnames_after_replacearg():
    '''Test the argument_names property makes things consistent if a child
    argument is replaced. This is used transparently by the class to
    keep things consistent.

    '''
    op1 = Literal("1", INTEGER_TYPE)
    op2 = Literal("1", INTEGER_TYPE)
    op3 = Literal("1", INTEGER_TYPE)
    call = Call.create(RoutineSymbol("name"), [("name1", op1), ("name2", op2)])
    assert len(call.arguments) == 2
    assert len(call._argument_names) == 2
    assert call.argument_names == ["name1", "name2"]
    call.children[1] = op3
    assert len(call.arguments) == 2
    assert len(call._argument_names) == 2
    # argument_names property makes _argument_names list consistent.
    assert call._argument_names[0][0] != id(call.arguments[0])
    assert call.argument_names == [None, "name2"]
    assert len(call._argument_names) == 2
    assert call._argument_names[0][0] == id(call.arguments[0])


def test_call_argumentnames_after_reorderarg():
    '''Test the argument_names property makes things consistent if a child
    argument is replaced. This is used transparently by the class to
    keep things consistent.

    '''
    op1 = Literal("1", INTEGER_TYPE)
    op2 = Literal("1", INTEGER_TYPE)
    op3 = Literal("1", INTEGER_TYPE)
    call = Call.create(RoutineSymbol("name"), [("name1", op1), ("name2", op2)])
    assert len(call.arguments) == 2
    assert len(call._argument_names) == 2
    assert call.argument_names == ["name1", "name2"]
    call.children[1] = op3
    assert len(call.arguments) == 2
    assert len(call._argument_names) == 2
    # argument_names property makes _argument_names list consistent.
    assert call.argument_names == [None, "name2"]
    assert len(call._argument_names) == 2


def test_call_node_reconcile_add():
    '''Test that the reconcile method behaves as expected. Use an example
    where we add a new arg.

    '''
    op1 = Literal("1", INTEGER_TYPE)
    op2 = Literal("1", INTEGER_TYPE)
    op3 = Literal("1", INTEGER_TYPE)
    call = Call.create(RoutineSymbol("name"), [("name1", op1), ("name2", op2)])
    # consistent
    assert len(call._argument_names) == 2
    assert call._argument_names[0] == (id(call.arguments[0]), "name1")
    assert call._argument_names[1] == (id(call.arguments[1]), "name2")
    call.children.append(op3)
    # inconsistent
    assert len(call._argument_names) == 2
    assert call._argument_names[0] == (id(call.arguments[0]), "name1")
    assert call._argument_names[1] == (id(call.arguments[1]), "name2")
    call._reconcile()
    # consistent
    assert len(call._argument_names) == 3
    assert call._argument_names[0] == (id(call.arguments[0]), "name1")
    assert call._argument_names[1] == (id(call.arguments[1]), "name2")
    assert call._argument_names[2] == (id(call.arguments[2]), None)


def test_call_node_reconcile_reorder():
    '''Test that the reconcile method behaves as expected. Use an example
    where we reorder the arguments.

    '''
    op1 = Literal("1", INTEGER_TYPE)
    op2 = Literal("2", INTEGER_TYPE)
    call = Call.create(RoutineSymbol("name"), [("name1", op1), ("name2", op2)])
    # consistent
    assert len(call._argument_names) == 2
    assert call._argument_names[0] == (id(call.arguments[0]), "name1")
    assert call._argument_names[1] == (id(call.arguments[1]), "name2")

    # Swap position of arguments
    call.children.extend([op2.detach(), op1.detach()])

    # Now the private _argument_names are inconsistent with thir node ids
    assert len(call._argument_names) == 2
    assert call._argument_names[0] != (id(call.arguments[0]), "name1")
    assert call._argument_names[1] != (id(call.arguments[1]), "name2")
    call._reconcile()
    # consistent
    assert len(call._argument_names) == 2
    assert call._argument_names[0] == (id(call.arguments[0]), "name2")
    assert call._argument_names[1] == (id(call.arguments[1]), "name1")


def test_call_node_str():
    ''' Test that the node_str method behaves as expected '''
    routine = RoutineSymbol("isaac", NoType())
    call = Call.create(routine)
    colouredtext = colored("Call", Call._colour)
    assert call.node_str() == colouredtext+"[name='isaac']"


def test_call_str():
    ''' Test that the str method behaves as expected '''
    routine = RoutineSymbol("roo", NoType())
    call = Call.create(routine)
    assert str(call) == "Call[name='roo']"


def test_copy():
    ''' Test that the copy() method behaves as expected. '''
    op1 = Literal("1", INTEGER_TYPE)
    op2 = Literal("2", INTEGER_TYPE)
    call = Call.create(RoutineSymbol("name"), [("name1", op1), ("name2", op2)])
    # Call copy with consitent internal state of _arguments_name
    call2 = call.copy()
    assert call._argument_names[0] == (id(call.arguments[0]), "name1")
    assert call._argument_names[1] == (id(call.arguments[1]), "name2")
    assert call2._argument_names[0] == (id(call2.arguments[0]), "name1")
    assert call2._argument_names[1] == (id(call2.arguments[1]), "name2")
    assert call._argument_names != call2._argument_names

    # Swap position of arguments
    call.children.extend([op2.detach(), op1.detach()])

    # The internal state of the argument_names is now inconsistent:
    # name1=op2, name2=op1 (until we call a public method)
    assert call._argument_names[0] != (id(call.arguments[0]), "name2")
    assert call._argument_names[1] != (id(call.arguments[1]), "name1")

    # Calling the copy method must reconcile the argument names before doing
    # the copy, so name2=op2 is at position 0, and name1=op1 is at position 1
    call2 = call.copy()
    assert call._argument_names[0] == (id(call.arguments[0]), "name2")
    assert call._argument_names[1] == (id(call.arguments[1]), "name1")
    assert call2._argument_names[0] == (id(call2.arguments[0]), "name2")
    assert call2._argument_names[1] == (id(call2.arguments[1]), "name1")

    # And the ids are not the same (each one has their own)
    assert call._argument_names != call2._argument_names


def test_call_get_callees_local(fortran_reader):
    '''
    Check that get_callees() works as expected when the target of the Call
    exists in the same Container as the call site.
    '''
    code = '''
module some_mod
  implicit none
  integer :: luggage
contains
  subroutine top()
    luggage = 0
    call bottom()
  end subroutine top

  subroutine bottom()
    luggage = luggage + 1
  end subroutine bottom
end module some_mod'''
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    result = call.get_callees()
    assert result == [psyir.walk(Routine)[1]]


def test_call_get_callees_local_unresolved_interface(fortran_reader,
                                                     monkeypatch):
    '''
    Check that get_callees() works as expected when the target of the Call
    is an unresolved interface that exists in the same Container as the call
    site. This shouldn't ever occur in practise so we use monkeypatch.

    '''
    code = '''
module some_mod
  implicit none
  integer :: luggage
  interface polymorph
    module procedure :: morph1, morph2
  end interface
contains
  subroutine top()
    luggage = 0
    call polymorph(luggage)
  end subroutine top

  subroutine morph1(arg)
    integer, intent(inout) :: arg
  end subroutine morph1

  subroutine morph2(arg)
    real, intent(inout) :: arg
  end subroutine morph2
end module some_mod'''
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    # Monkeypatch the called symbol so that it appears to be unresolved.
    monkeypatch.setattr(call.routine.symbol, "_interface",
                        UnresolvedInterface())
    result = call.get_callees()
    assert len(result) == 2
    assert result == psyir.walk(Routine)[1:]
    assert isinstance(call.routine.symbol.datatype, NoType)


def test_call_get_callees_local_file_container(fortran_reader):
    '''
    Test that get_callees() succeeds when the called routine is within
    the parent FileContainer.
    '''
    code = '''
subroutine upper()
  write(*,*) "hello"
end subroutine upper

module some_mod
  implicit none
contains
  subroutine top()
    integer :: x = 0
    call upper()
    call bottom(x)
  end subroutine top
end module some_mod

subroutine bottom(luggage)
    integer, intent(inout) :: luggage
    luggage = luggage + 1
end subroutine bottom'''
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    result = call.get_callees()
    assert result == [psyir.walk(Routine)[0]]
    call = psyir.walk(Call)[1]
    result = call.get_callees()
    assert result == [psyir.walk(Routine)[2]]


def test_call_get_callee_1_simple_match(fortran_reader):
    '''
    Check that the right routine has been found for a single routine
    implementation.
    '''
    code = '''
module some_mod
  implicit none
contains

  subroutine main()
    integer :: e, f, g
    call foo(e, f, g)
  end subroutine

  ! Matching routine
  pure subroutine foo(a, b, c)
    integer :: a, b, c
  end subroutine

end module some_mod'''

    psyir = fortran_reader.psyir_from_source(code)

    routine_main: Routine = psyir.walk(Routine)[0]
    assert routine_main.name == "main"

    call_foo: Call = routine_main.walk(Call)[0]
    assert call_foo.routine.symbol.is_pure is True

    (result, _) = call_foo.get_callee()

    routine_match: Routine = psyir.walk(Routine)[1]
    assert result is routine_match


def test_call_get_callee_2_optional_args(fortran_reader):
    '''
    Check that optional arguments have been correlated correctly.
    '''
    code = '''
module some_mod
  implicit none
contains

  subroutine main()
    integer :: e, f, g
    call foo(e, f)
  end subroutine

  ! Matching routine
  subroutine foo(a, b, c)
    integer :: a, b
    integer, optional :: c
  end subroutine

end module some_mod'''

    root_node: Node = fortran_reader.psyir_from_source(code)

    routine_main: Routine = root_node.walk(Routine)[0]
    assert routine_main.name == "main"

    routine_match: Routine = root_node.walk(Routine)[1]
    assert routine_match.name == "foo"

    call_foo: Call = routine_main.walk(Call)[0]
    assert call_foo.routine.name == "foo"

    (result, arg_idx_list) = call_foo.get_callee()
    result: Routine

    assert len(arg_idx_list) == 2
    assert arg_idx_list[0] == 0
    assert arg_idx_list[1] == 1

    assert result is routine_match


def test_call_get_callee_3a_trigger_error(fortran_reader):
    '''
    Test which is supposed to trigger an error when no matching routine
    is found
    '''
    code = '''
module some_mod
  implicit none
contains

  subroutine main()
    integer :: e, f, g
    call foo(e, f, g)
  end subroutine

  ! Matching routine
  subroutine foo(a, b)
    integer :: a, b
  end subroutine

end module some_mod'''

    root_node: Node = fortran_reader.psyir_from_source(code)

    routine_main: Routine = root_node.walk(Routine)[0]
    assert routine_main.name == "main"

    call_foo: Call = routine_main.walk(Call)[0]
    assert call_foo.routine.name == "foo"

    with pytest.raises(CallMatchingArgumentsNotFound) as err:
        call_foo.get_callee()

    assert "No matching routine found for" in str(err.value)


def test_call_get_callee_3c_trigger_error(fortran_reader):
    '''
    Test which is supposed to trigger an error when no matching routine
    is found, but we use the special option check_matching_arguments=False
    to find one.
    '''
    code = '''
module some_mod
  implicit none
contains

  subroutine main()
    integer :: e, f, g
    call foo(e, f, g)
  end subroutine

  ! Matching routine
  subroutine foo(a, b)
    integer :: a, b
  end subroutine

end module some_mod'''

    root_node: Node = fortran_reader.psyir_from_source(code)

    routine_main: Routine = root_node.walk(Routine)[0]
    assert routine_main.name == "main"

    call_foo: Call = routine_main.walk(Call)[0]
    assert call_foo.routine.name == "foo"

    call_foo.get_callee(check_matching_arguments=False)


def test_call_get_callee_4_named_arguments(fortran_reader):
    '''
    Check that named arguments have been correlated correctly
    '''
    code = '''
module some_mod
  implicit none
contains

  subroutine main()
    integer :: e, f, g
    call foo(c=e, a=f, b=g)
  end subroutine

  ! Matching routine
  subroutine foo(a, b, c)
    integer :: a, b, c
  end subroutine

end module some_mod'''

    root_node: Node = fortran_reader.psyir_from_source(code)

    routine_main: Routine = root_node.walk(Routine)[0]
    assert routine_main.name == "main"

    routine_match: Routine = root_node.walk(Routine)[1]
    assert routine_match.name == "foo"

    call_foo: Call = routine_main.walk(Call)[0]
    assert call_foo.routine.name == "foo"

    (result, arg_idx_list) = call_foo.get_callee()
    result: Routine

    assert len(arg_idx_list) == 3
    assert arg_idx_list[0] == 2
    assert arg_idx_list[1] == 0
    assert arg_idx_list[2] == 1

    assert result is routine_match


def test_call_get_callee_5_optional_and_named_arguments(fortran_reader):
    '''
    Check that optional and named arguments have been correlated correctly
    when the call is to a generic interface.
    '''
    code = '''
module some_mod
  implicit none
contains

  subroutine main()
    integer :: e, f, g
    call foo(b=e, a=f)
  end subroutine

  ! Matching routine
  subroutine foo(a, b, c)
    integer :: a, b
    integer, optional :: c
  end subroutine

end module some_mod'''

    root_node: Node = fortran_reader.psyir_from_source(code)

    routine_main: Routine = root_node.walk(Routine)[0]
    assert routine_main.name == "main"

    routine_match: Routine = root_node.walk(Routine)[1]
    assert routine_match.name == "foo"

    call_foo: Call = routine_main.walk(Call)[0]
    assert call_foo.routine.name == "foo"

    (result, arg_idx_list) = call_foo.get_callee()
    result: Routine

    assert len(arg_idx_list) == 2
    assert arg_idx_list[0] == 1
    assert arg_idx_list[1] == 0

    assert result is routine_match


_code_test_get_callee_6 = '''
module some_mod
  implicit none

  interface foo
    procedure foo_a, foo_b, foo_c, foo_optional
  end interface
contains

  subroutine main()
    integer :: e_int, f_int, g_int
    real :: e_real, f_real, g_real

    ! Should match foo_a, test_call_get_callee_6_interfaces_0_0
    call foo(e_int, f_int)

    ! Should match foo_a, test_call_get_callee_6_interfaces_0_1
    call foo(e_int, f_int, g_int)

    ! Should match foo_b, test_call_get_callee_6_interfaces_1_0
    call foo(e_real, f_int)

    ! Should match foo_b, test_call_get_callee_6_interfaces_1_1
    call foo(e_real, f_int, g_int)

    ! Should match foo_b, test_call_get_callee_6_interfaces_1_2
    call foo(e_real, c=f_int, b=g_int)

    ! Should match foo_c, test_call_get_callee_6_interfaces_2_0
    call foo(e_int, f_real, g_int)

    ! Should match foo_c, test_call_get_callee_6_interfaces_2_1
    call foo(b=e_real, a=f_int)

    ! Should match foo_c, test_call_get_callee_6_interfaces_2_2
    call foo(b=e_real, a=f_int, g_int)

    ! Should not match foo_optional because of invalid type,
    ! test_call_get_callee_6_interfaces_3_0_mismatch
    call foo(f_int, e_real, g_int, g_int)
  end subroutine

  subroutine foo_a(a, b, c)
    integer :: a, b
    integer, optional :: c
  end subroutine

  subroutine foo_b(a, b, c)
    real :: a
    integer :: b
    integer, optional :: c
  end subroutine

  subroutine foo_c(a, b, c)
    integer :: a
    real :: b
    integer, optional :: c
  end subroutine

  subroutine foo_optional(a, b, c, d)
    integer :: a
    real :: b
    integer :: c
    real, optional :: d ! real vs. int
  end subroutine


end module some_mod'''


def test_call_get_callee_6_interfaces_0_0(fortran_reader):
    '''
    Check that a non-existing optional argument at the end of the list
    has been correctly determined.
    '''

    root_node: Node = fortran_reader.psyir_from_source(_code_test_get_callee_6)

    routine_main: Routine = root_node.walk(Routine)[0]
    assert routine_main.name == "main"

    routine_foo_a: Routine = root_node.walk(Routine)[1]
    assert routine_foo_a.name == "foo_a"

    call_foo_a: Call = routine_main.walk(Call)[0]
    assert call_foo_a.routine.name == "foo"

    (result, arg_idx_list) = call_foo_a.get_callee()
    result: Routine

    assert len(arg_idx_list) == 2
    assert arg_idx_list[0] == 0
    assert arg_idx_list[1] == 1

    assert result is routine_foo_a


def test_call_get_callee_6_interfaces_0_1(fortran_reader):
    '''
    Check that an existing optional argument at the end of the list
    has been correctly determined.
    '''

    root_node: Node = fortran_reader.psyir_from_source(_code_test_get_callee_6)

    routine_main: Routine = root_node.walk(Routine)[0]
    assert routine_main.name == "main"

    routine_foo_a: Routine = root_node.walk(Routine)[1]
    assert routine_foo_a.name == "foo_a"

    call_foo_a: Call = routine_main.walk(Call)[1]
    assert call_foo_a.routine.name == "foo"

    (result, arg_idx_list) = call_foo_a.get_callee()
    result: Routine

    assert len(arg_idx_list) == 3
    assert arg_idx_list[0] == 0
    assert arg_idx_list[1] == 1
    assert arg_idx_list[2] == 2

    assert result is routine_foo_a


def test_call_get_callee_6_interfaces_1_0(fortran_reader):
    '''
    Check that
    - different argument types and
    - non-existing optional argument at the end of the list
    have been correctly determined.
    '''

    root_node: Node = fortran_reader.psyir_from_source(_code_test_get_callee_6)

    routine_main: Routine = root_node.walk(Routine)[0]
    assert routine_main.name == "main"

    routine_foo_b: Routine = root_node.walk(Routine)[2]
    assert routine_foo_b.name == "foo_b"

    call_foo_b: Call = routine_main.walk(Call)[2]
    assert call_foo_b.routine.name == "foo"

    (result, arg_idx_list) = call_foo_b.get_callee()
    result: Routine

    assert len(arg_idx_list) == 2
    assert arg_idx_list[0] == 0
    assert arg_idx_list[1] == 1

    assert result is routine_foo_b


def test_call_get_callee_6_interfaces_1_1(fortran_reader):
    '''
    Check that
    - different argument types and
    - existing optional argument at the end of the list
    have been correctly determined.
    '''

    root_node: Node = fortran_reader.psyir_from_source(_code_test_get_callee_6)

    routine_main: Routine = root_node.walk(Routine)[0]
    assert routine_main.name == "main"

    routine_foo_b: Routine = root_node.walk(Routine)[2]
    assert routine_foo_b.name == "foo_b"

    call_foo_b: Call = routine_main.walk(Call)[3]
    assert call_foo_b.routine.name == "foo"

    (result, arg_idx_list) = call_foo_b.get_callee()
    result: Routine

    assert len(arg_idx_list) == 3
    assert arg_idx_list[0] == 0
    assert arg_idx_list[1] == 1
    assert arg_idx_list[2] == 2

    assert result is routine_foo_b


def test_call_get_callee_6_interfaces_1_2(fortran_reader):
    '''
    Check that
    - different argument types and
    - naming arguments resulting in a different order
    have been correctly determined.
    '''

    root_node: Node = fortran_reader.psyir_from_source(_code_test_get_callee_6)

    routine_main: Routine = root_node.walk(Routine)[0]
    assert routine_main.name == "main"

    routine_foo_b: Routine = root_node.walk(Routine)[2]
    assert routine_foo_b.name == "foo_b"

    call_foo_b: Call = routine_main.walk(Call)[4]
    assert call_foo_b.routine.name == "foo"

    (result, arg_idx_list) = call_foo_b.get_callee()
    result: Routine

    assert len(arg_idx_list) == 3
    assert arg_idx_list[0] == 0
    assert arg_idx_list[1] == 2
    assert arg_idx_list[2] == 1

    assert result is routine_foo_b


def test_call_get_callee_6_interfaces_2_0(fortran_reader):
    '''
    Check that
    - different argument types (different order than in tests before)
    have been correctly determined.
    '''

    root_node: Node = fortran_reader.psyir_from_source(_code_test_get_callee_6)

    routine_main: Routine = root_node.walk(Routine)[0]
    assert routine_main.name == "main"

    routine_foo_c: Routine = root_node.walk(Routine)[3]
    assert routine_foo_c.name == "foo_c"

    call_foo_c: Call = routine_main.walk(Call)[5]
    assert call_foo_c.routine.name == "foo"

    (result, arg_idx_list) = call_foo_c.get_callee()
    result: Routine

    assert len(arg_idx_list) == 3
    assert arg_idx_list[0] == 0
    assert arg_idx_list[1] == 1
    assert arg_idx_list[2] == 2

    assert result is routine_foo_c


def test_call_get_callee_6_interfaces_2_1(fortran_reader):
    '''
    Check that
    - different argument types (different order than in tests before) and
    - naming arguments resulting in a different order and
    - optional argument
    have been correctly determined.
    '''

    root_node: Node = fortran_reader.psyir_from_source(_code_test_get_callee_6)

    routine_main: Routine = root_node.walk(Routine)[0]
    assert routine_main.name == "main"

    routine_foo_c: Routine = root_node.walk(Routine)[3]
    assert routine_foo_c.name == "foo_c"

    call_foo_c: Call = routine_main.walk(Call)[6]
    assert call_foo_c.routine.name == "foo"

    (result, arg_idx_list) = call_foo_c.get_callee()
    result: Routine

    assert len(arg_idx_list) == 2
    assert arg_idx_list[0] == 1
    assert arg_idx_list[1] == 0

    assert result is routine_foo_c


def test_call_get_callee_6_interfaces_2_2(fortran_reader):
    '''
    Check that
    - different argument types (different order than in tests before) and
    - naming arguments resulting in a different order and
    - last call argument without naming
    have been correctly determined.
    '''

    root_node: Node = fortran_reader.psyir_from_source(_code_test_get_callee_6)

    routine_main: Routine = root_node.walk(Routine)[0]
    assert routine_main.name == "main"

    routine_foo_c: Routine = root_node.walk(Routine)[3]
    assert routine_foo_c.name == "foo_c"

    call_foo_c: Call = routine_main.walk(Call)[7]
    assert call_foo_c.routine.name == "foo"

    (result, arg_idx_list) = call_foo_c.get_callee()
    result: Routine

    assert len(arg_idx_list) == 3
    assert arg_idx_list[0] == 1
    assert arg_idx_list[1] == 0
    assert arg_idx_list[2] == 2

    assert result is routine_foo_c


def test_call_get_callee_6_interfaces_3_0_mismatch(fortran_reader):
    '''
    Check that matching a partial data type can also go wrong.
    '''

    root_node: Node = fortran_reader.psyir_from_source(_code_test_get_callee_6)

    routine_main: Routine = root_node.walk(Routine)[0]
    assert routine_main.name == "main"

    routine_foo_optional: Routine = root_node.walk(Routine)[4]
    assert routine_foo_optional.name == "foo_optional"

    call_foo_optional: Call = routine_main.walk(Call)[8]
    assert call_foo_optional.routine.name == "foo"

    with pytest.raises(CallMatchingArgumentsNotFound) as einfo:
        call_foo_optional.get_callee()

    assert "Argument partial type mismatch of call argument" in (
        str(einfo.value))


def test_call_get_callee_7_matching_arguments_not_found(fortran_reader):
    '''
    Trigger error that matching arguments were not found
    '''
    code = '''
module some_mod
  implicit none
contains

  subroutine main()
    integer :: e, f, g
    ! Use named argument 'd', which doesn't exist
    ! to trigger an error when searching for the matching routine.
    call foo(e, f, d=g)
  end subroutine

  ! Matching routine
  subroutine foo(a, b, c)
    integer :: a, b, c
  end subroutine

end module some_mod'''

    psyir = fortran_reader.psyir_from_source(code)

    routine_main: Routine = psyir.walk(Routine)[0]
    assert routine_main.name == "main"

    call_foo: Call = routine_main.walk(Call)[0]

    with pytest.raises(CallMatchingArgumentsNotFound) as err:
        call_foo.get_callee()

    assert "No matching routine found for 'call foo(e, f, d=g)" in str(
        err.value
    )


def test_call_get_callee_8_arguments_not_handled(fortran_reader):
    '''
    Trigger error that matching arguments were not found.
    In this test, this is caused by omitting the required third non-optional
    argument.
    '''
    code = '''
module some_mod
  implicit none
contains

  subroutine main()
    integer :: e, f
    ! Omit the 3rd required argument
    call foo(e, f)
  end subroutine

  ! Routine matching by 'name', but not by argument matching
  subroutine foo(a, b, c)
    integer :: a, b, c
  end subroutine

end module some_mod'''

    psyir = fortran_reader.psyir_from_source(code)

    routine_main: Routine = psyir.walk(Routine)[0]
    assert routine_main.name == "main"

    call_foo: Call = routine_main.walk(Call)[0]

    with pytest.raises(CallMatchingArgumentsNotFound) as err:
        call_foo.get_callee()

    assert "No matching routine found for 'call foo(e, f)" in str(err.value)


@pytest.mark.usefixtures("clear_module_manager_instance")
def test_call_get_callees_unresolved(fortran_reader, tmpdir, monkeypatch,
                                     config_instance):
    '''
    Test that get_callees() raises the expected error if the called routine
    is unresolved.
    '''
    # Ensure that include_paths in the Config object is empty.
    config_instance.include_paths = []
    code = '''
subroutine top()
  call bottom()
end subroutine top'''
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    with pytest.raises(NotImplementedError) as err:
        _ = call.get_callees()
    assert ("Failed to find the source code of the unresolved routine "
            "'bottom'. There are no wildcard imports that could be bringing "
            "it into scope. It must be an external routine that is only "
            "resolved at link time" in str(err.value))
    # Repeat but in the presence of a wildcard import.
    code = '''
subroutine top()
  use some_mod_somewhere
  call bottom()
end subroutine top'''
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    with pytest.raises(NotImplementedError) as err:
        _ = call.get_callees()
    assert ("Failed to find the source code of the unresolved routine "
            "'bottom'. It may be being brought into scope from one of "
            "['some_mod_somewhere']. You may wish to add the appropriate "
            "module name to the `RESOLVE_IMPORTS` variable in the "
            "transformation script." in str(err.value))
    # Repeat but in the presence of a wildcard import and CodeBlock.
    code = '''
module my_mod
  use some_mod_somewhere
  contains
subroutine top()
  call bottom()
end subroutine top
complex function possibly()
    possibly = 1
end function possibly
end module my_mod
    '''
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    with pytest.raises(NotImplementedError) as err:
        _ = call.get_callees()
    assert ("Failed to find the source code of the unresolved routine "
            "'bottom'. It may be being brought into scope from one of "
            "['some_mod_somewhere'] or it may be within a CodeBlock. If it "
            "isn't, you may" in str(err.value))


@pytest.mark.usefixtures("clear_module_manager_instance")
def test_call_get_callees_resolved_not_found(fortran_reader, monkeypatch):
    '''
    Test get_callees() when the RoutineSymbol is resolved (i.e. we know which
    Container it comes from) but we can't find the source of the Container.

    '''
    code = '''
subroutine top()
  use another_mod, only: this_one
  call this_one()
end subroutine top'''
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    with pytest.raises(NotImplementedError) as err:
        _ = call.get_callees()
    assert ("RoutineSymbol 'this_one' is imported from Container 'another_mod'"
            " but the source defining that container could not be found. The "
            "module search path is set to [" in str(err.value))
    monkeypatch.setattr(ContainerSymbol, "find_container_psyir",
                        lambda _1, local_node=None: None)
    with pytest.raises(NotImplementedError) as err:
        _ = call.get_callees()
    assert ("RoutineSymbol 'this_one' is imported from Container 'another_mod'"
            " but the PSyIR for that container could not be generated."
            in str(err.value))


def test_call_get_callees_interface(fortran_reader):
    '''
    Check that get_callees() works correctly when the target of a call is
    actually a generic interface.
    '''
    code = '''
module my_mod

    interface bottom
      module procedure :: rbottom, ibottom
    end interface bottom
contains
  subroutine top()
    integer :: luggage
    luggage = 0
    call bottom(luggage)
  end subroutine top

  pure subroutine ibottom(luggage)
    integer :: luggage
    luggage = luggage + 1
  end subroutine ibottom

  pure subroutine rbottom(luggage)
    real :: luggage
    luggage = luggage + 1.0
  end subroutine rbottom
end module my_mod
'''
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    assert call.routine.symbol.is_pure is True
    callees = call.get_callees()
    assert len(callees) == 2
    assert isinstance(callees[0], Routine)
    assert callees[0].name == "rbottom"
    assert isinstance(callees[1], Routine)
    assert callees[1].name == "ibottom"


def test_call_get_callees_file_container(fortran_reader):
    '''
    Check that get_callees works if the called routine happens to be in file
    scope, even when there's no Container.
    '''
    code = '''
  subroutine top()
    integer :: luggage
    luggage = 0
    call bottom(luggage)
  end subroutine top

  subroutine bottom(luggage)
    integer :: luggage
    luggage = luggage + 1
  end subroutine bottom
'''
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    result = call.get_callees()
    assert len(result) == 1
    assert isinstance(result[0], Routine)
    assert result[0].name == "bottom"


def test_call_get_callees_no_container(fortran_reader):
    '''
    Check that get_callees() raises the expected error when the Call is not
    within a Container and the target routine cannot be found.
    '''
    # To avoid having the routine symbol immediately dismissed as
    # unresolved, the code that we initially process *does* have a Container.
    code = '''
module my_mod

contains
  subroutine top()
    integer :: luggage
    luggage = 0
    call bottom(luggage)
  end subroutine top

  subroutine bottom(luggage)
    integer :: luggage
    luggage = luggage + 1
  end subroutine bottom

end module my_mod
'''
    psyir = fortran_reader.psyir_from_source(code)
    top_routine = psyir.walk(Routine)[0]
    new_call = Call.create(RoutineSymbol("missing"), [])
    top_routine.addchild(new_call)
    with pytest.raises(SymbolError) as err:
        _ = new_call.get_callees()
    assert ("Failed to find a Routine named 'missing' in Container 'my_mod'"
            in str(err.value))
    # Deliberately make the Routine node an orphan so there's no Container.
    top_routine.detach()
    call = top_routine.walk(Call)[0]
    with pytest.raises(SymbolError) as err:
        _ = call.get_callees()
    assert ("Failed to find a Routine named 'bottom' in code:\n'"
            "subroutine top()" in str(err.value))


def test_call_get_callees_import_local_container(fortran_reader):
    '''
    Check that get_callees() works successfully for a routine accessed via
    a specific import from another module in the same file.
    '''
    code = '''
module some_mod
contains
  subroutine just_do_it()
    write(*,*) "hello"
  end subroutine just_do_it
end module some_mod
module other_mod
  use some_mod, only: just_do_it
contains
  subroutine run_it()
    call just_do_it()
  end subroutine run_it
end module other_mod
'''
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    routines = call.get_callees()
    assert len(routines) == 1
    assert isinstance(routines[0], Routine)
    assert routines[0].name == "just_do_it"


def test_fn_call_get_callees(fortran_reader):
    '''
    Test that get_callees() works for a function call.
    '''
    code = '''
module some_mod
  implicit none
  integer :: luggage
contains
  subroutine top()
    luggage = 0
    luggage = luggage + my_func(1)
  end subroutine top

  function my_func(val)
    integer, intent(in) :: val
    integer :: my_func
    my_func = 1 + val
  end function my_func
end module some_mod'''
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    result = call.get_callees()
    assert result == [psyir.walk(Routine)[1]]


def test_get_callees_code_block(fortran_reader):
    '''Test that get_callees() raises the expected error when the called
    routine is in a CodeBlock.'''
    code = '''
module some_mod
  implicit none
  integer :: luggage
contains
  subroutine top()
    luggage = 0
    luggage = luggage + real(my_func(1))
  end subroutine top

  complex function my_func(val)
    integer, intent(in) :: val
    my_func = CMPLX(1 + val, 1.0)
  end function my_func
end module some_mod'''
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[1]
    with pytest.raises(NotImplementedError) as err:
        _ = call.get_callees()
    assert ("Failed to find the source code of the unresolved routine "
            "'my_func'. There are no wildcard imports that could be bringing "
            "it into scope but it might be within a CodeBlock." in
            str(err.value))


@pytest.mark.usefixtures("clear_module_manager_instance")
def test_get_callees_follow_imports(fortran_reader, tmpdir, monkeypatch):
    '''
    Test that get_callees() follows imports to find the definition of the
    called routine.
    '''
    code = '''
module some_mod
  use other_mod, only: pack_it
  implicit none
contains
  subroutine top()
    integer :: luggage = 0
    call pack_it(luggage)
  end subroutine top
end module some_mod'''
    # Create the module containing an import of the subroutine definition,
    # write it to file and set the search path so that PSyclone can find it.
    path = str(tmpdir)
    monkeypatch.setattr(Config.get(), '_include_paths', [path])

    with open(os.path.join(path, "other_mod.f90"),
              "w", encoding="utf-8") as mfile:
        mfile.write('''\
    module other_mod
        use another_mod, only: pack_it
    contains
    end module other_mod
    ''')
    # Finally, create the module containing the routine definition.
    with open(os.path.join(path, "another_mod.f90"),
              "w", encoding="utf-8") as mfile:
        mfile.write('''\
    module another_mod
    contains
        subroutine pack_it(arg)
          integer, intent(inout) :: arg
          arg = arg + 2
        end subroutine pack_it
    end module another_mod
    ''')
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    result = call.get_callees()
    assert len(result) == 1
    assert isinstance(result[0], Routine)
    assert result[0].name == "pack_it"


@pytest.mark.usefixtures("clear_module_manager_instance")
def test_get_callees_import_private_clash(fortran_reader, tmpdir, monkeypatch):
    '''
    Test that get_callees() raises the expected error if a module from which
    a routine is imported has a private shadow of that routine (and thus we
    don't know where to look for the target routine).
    '''
    code = '''
module some_mod
  use other_mod, only: pack_it
  implicit none
contains
  subroutine top()
    integer :: luggage = 0
    call pack_it(luggage)
  end subroutine top
end module some_mod'''
    # Create the module containing a private routine with the name we are
    # searching for, write it to file and set the search path so that PSyclone
    # can find it.
    path = str(tmpdir)
    monkeypatch.setattr(Config.get(), '_include_paths', [path])

    with open(os.path.join(path, "other_mod.f90"),
              "w", encoding="utf-8") as mfile:
        mfile.write('''\
    module other_mod
        use another_mod
        private pack_it
    contains
        function pack_it(arg)
          integer :: arg
          integer :: pack_it
        end function pack_it
    end module other_mod
    ''')
    psyir = fortran_reader.psyir_from_source(code)
    call = psyir.walk(Call)[0]
    with pytest.raises(NotImplementedError) as err:
        _ = call.get_callees()
    assert ("RoutineSymbol 'pack_it' is imported from Container 'other_mod' "
            "but that Container defines a private Symbol of the same name. "
            "Searching for the Container that defines a public Routine with "
            "that name is not yet supported - TODO #924" in str(err.value))
