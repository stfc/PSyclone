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
from psyclone.core import Signature, VariablesAccessInfo
from psyclone.errors import GenerationError
from psyclone.psyir.nodes import (
    ArrayReference,
    BinaryOperation,
    Call,
    Literal,
    Reference,
    Routine,
    Schedule,
)
from psyclone.psyir.nodes.node import colored
from psyclone.psyir.symbols import (
    ArrayType,
    INTEGER_TYPE,
    DataSymbol,
    NoType,
    RoutineSymbol,
    REAL_TYPE
)

from psyclone.psyir.tools.call_routine_matcher import (
    CallMatchingArgumentsNotFoundError)


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
    assert call.arguments == [Literal('3', INTEGER_TYPE)]


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
    assert call.arguments == [op1, op2, op3]
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
    assert call.arguments == [op1]
    assert call.argument_names == ["name1"]
    call.insert_named_arg("name2", op2, 0)
    assert call.arguments == [op2, op1]
    assert call.argument_names == ["name2", "name1"]
    call.insert_named_arg(None, op3, 0)
    assert call.arguments == [op3, op2, op1]
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
    # ok
    assert call.arguments == [op1, op2]
    assert call.argument_names == ["name1", "name2"]
    assert call._argument_names[0][0] == id(op1)
    assert call._argument_names[1][0] == id(op2)
    call.replace_named_arg("name1", op3)
    assert call.arguments == [op3, op2]
    assert call.argument_names == ["name1", "name2"]
    assert call._argument_names[0][0] == id(op3)
    assert call._argument_names[1][0] == id(op2)


def test_call_reference_accesses():
    '''Test the reference_accesses() method.'''
    rsym = RoutineSymbol("trillian")
    # A call with an argument passed by value.
    call1 = Call.create(rsym, [Literal("1", INTEGER_TYPE)])
    var_info = VariablesAccessInfo()
    assert var_info._location == 0
    call1.reference_accesses(var_info)
    # Check that the current location number is increased after the call:
    assert var_info._location == 1
    # The Routine symbol is not considered 'read'.
    assert not var_info.is_read(Signature("trillian"))
    assert var_info.is_called(Signature("trillian"))
    dsym = DataSymbol("beta", INTEGER_TYPE)
    # Simple argument passed by reference.
    call2 = Call.create(rsym, [Reference(dsym)])
    call2.reference_accesses(var_info)
    assert var_info.has_read_write(Signature("beta"))
    assert not var_info.is_called(Signature("beta"))
    # Array access argument. The array should be READWRITE, any variable in
    # the index expression should be READ.
    idx_sym = DataSymbol("ji", INTEGER_TYPE)
    asym = DataSymbol("gamma", ArrayType(INTEGER_TYPE, shape=[10]))
    aref = ArrayReference.create(asym, [Reference(idx_sym)])
    call3 = Call.create(rsym, [aref])
    call3.reference_accesses(var_info)
    assert var_info.has_read_write(Signature("gamma"))
    assert var_info.is_read(Signature("ji"))
    # Argument is a temporary so any inputs to it are READ only.
    expr = BinaryOperation.create(BinaryOperation.Operator.MUL,
                                  Literal("2", INTEGER_TYPE), Reference(dsym))
    call4 = Call.create(rsym, [expr])
    var_info = VariablesAccessInfo()
    call4.reference_accesses(var_info)
    assert var_info.is_read(Signature("beta"))
    # Argument is itself a function call: call trillian(some_func(gamma(ji)))
    fsym = RoutineSymbol("some_func")
    fcall = Call.create(fsym,
                        [ArrayReference.create(asym, [Reference(idx_sym)])])
    call5 = Call.create(rsym, [fcall])
    call5.reference_accesses(var_info)
    assert var_info.has_read_write(Signature("gamma"))
    assert var_info.is_read(Signature("ji"))
    # Call to a PURE routine - arguments should be READ only.
    puresym = RoutineSymbol("dirk", is_pure=True)
    call6 = Call.create(puresym, [Reference(dsym)])
    var_info = VariablesAccessInfo()
    call6.reference_accesses(var_info)
    assert var_info.is_read(Signature("beta"))
    assert not var_info.is_written(Signature("beta"))


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
    vai = VariablesAccessInfo()
    call.reference_accesses(vai)
    # The type-bound procedure is called.
    assert vai.is_called(Signature("my_grid%update"))
    # As is the function defined in the same module.
    assert vai.is_called(Signature("get_start"))
    # All of the indices are marked as read.
    for var in ["i", "j", "k", "f"]:
        assert vai.is_read(Signature(var))
    # Only the arguments to the calls are marked as read-write.
    assert vai.has_read_write(Signature("j"))
    assert not vai.has_read_write(Signature("k"))
    assert not vai.has_read_write(Signature("f"))
    # We can't tell whether 'domain%get_start(i)' is an array access
    # or a function call. We currently, dangerously, assume it is the former.
    if not vai.has_read_write(Signature("i")):
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


def test_call_get_callee_arguments_not_handled(fortran_reader):
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

    with pytest.raises(CallMatchingArgumentsNotFoundError) as err:
        call_foo.get_callee()

    assert ("CallMatchingArgumentsNotFound: Found routines, but"
            " no routine with matching arguments found for 'call"
            " foo(e, f)':" in str(err.value))

    assert ("CallMatchingArgumentsNotFound: Argument 'c' in"
            " subroutine 'foo' not handled." in str(err.value))


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
