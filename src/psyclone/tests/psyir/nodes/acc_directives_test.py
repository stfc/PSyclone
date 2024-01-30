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
# Authors R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
# Modified I. Kavcic, Met Office
# Modified A. B. G. Chalk, STFC Daresbury Lab
# Modified J. G. Wallwork, Met Office
# -----------------------------------------------------------------------------

''' Performs py.test tests on the OpenACC PSyIR Directive nodes. '''

import os
import pytest

from psyclone.configuration import Config
from psyclone.core import Signature
from psyclone.errors import GenerationError
from psyclone.f2pygen import ModuleGen
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
from psyclone.psyir.nodes import (ACCKernelsDirective,
                                  ACCLoopDirective,
                                  ACCParallelDirective,
                                  ACCRegionDirective,
                                  ACCRoutineDirective,
                                  ACCUpdateDirective,
                                  ACCAtomicDirective,
                                  Assignment,
                                  Literal,
                                  Reference,
                                  Return,
                                  Routine)
from psyclone.psyir.nodes.loop import Loop
from psyclone.psyir.nodes.schedule import Schedule
from psyclone.psyir.symbols import SymbolTable, DataSymbol, INTEGER_TYPE
from psyclone.transformations import (
    ACCDataTrans, ACCEnterDataTrans, ACCKernelsTrans, ACCLoopTrans,
    ACCParallelTrans, ACCRoutineTrans)

BASE_PATH = os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(
    os.path.abspath(__file__)))), "test_files", "dynamo0p3")


@pytest.fixture(scope="module", autouse=True)
def setup():
    '''Make sure that all tests here use a new Config instance.'''
    Config._instance = None
    yield
    Config._instance = None


# Class ACCRegionDirective

class MyACCRegion(ACCRegionDirective):
    '''Concreate test class sub-classed from ACCRegionDirective.'''


def test_accregiondir_validate_global(fortran_reader):
    '''Check that the validate_global_constraints() method rejects PSyDataNode
    and CodeBlock nodes.'''
    accnode = MyACCRegion()
    cblock = fortran_reader.psyir_from_statement("write(*,*) 'hello'",
                                                 SymbolTable())
    accnode.dir_body.addchild(cblock)
    with pytest.raises(GenerationError) as err:
        accnode.validate_global_constraints()
    assert ("Cannot include CodeBlocks or calls to PSyData routines within "
            "OpenACC regions but found ['CodeBlock'] within a region enclosed "
            "by an 'MyACCRegion'" in str(err.value))


def test_accregiondir_signatures():
    '''Test the signatures property of ACCRegionDirective.'''
    routine = Routine("test_prog")
    accnode = MyACCRegion()
    routine.addchild(accnode)
    bob = DataSymbol("bob", INTEGER_TYPE)
    richard = DataSymbol("richard", INTEGER_TYPE)
    routine.symbol_table.add(bob)
    accnode.dir_body.addchild(
        Assignment.create(lhs=Reference(bob), rhs=Literal("1", INTEGER_TYPE)))
    accnode.dir_body.addchild(
        Assignment.create(lhs=Reference(bob), rhs=Literal("1", INTEGER_TYPE)))
    accnode.dir_body.addchild(
        Assignment.create(lhs=Reference(bob), rhs=Literal("1", INTEGER_TYPE)))
    accnode.dir_body.addchild(
        Assignment.create(lhs=Reference(bob), rhs=Reference(richard)))
    # pylint: disable=unbalanced-tuple-unpacking
    reads, writes = accnode.signatures
    assert Signature("richard") in reads
    assert Signature("bob") in writes

# Class ACCEnterDataDirective start


# (1/4) Method gen_code
def test_accenterdatadirective_gencode_1():
    '''Test that an OpenACC Enter Data directive, when added to a schedule
    with a single loop, raises the expected exception as there is no
    following OpenACC Parallel or OpenACC Kernels directive as at
    least one is required. This test uses the dynamo0.3 API.

    '''
    acc_enter_trans = ACCEnterDataTrans()
    _, info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"))
    psy = PSyFactory(distributed_memory=False).create(info)
    sched = psy.invokes.get('invoke_0_testkern_type').schedule
    acc_enter_trans.apply(sched)
    with pytest.raises(GenerationError) as excinfo:
        str(psy.gen)
    assert ("ACCEnterData directive did not find any data to copyin. Perhaps "
            "there are no ACCParallel or ACCKernels directives within the "
            "region?" in str(excinfo.value))

    # Test that the same error is produced by the begin_string() which is used
    # by the PSyIR backend
    sched[0].lower_to_language_level()
    with pytest.raises(GenerationError) as excinfo:
        sched[0].begin_string()
    assert ("ACCEnterData directive did not find any data to copyin. Perhaps "
            "there are no ACCParallel or ACCKernels directives within the "
            "region?" in str(excinfo.value))


# (2/4) Method gen_code
def test_accenterdatadirective_gencode_2():
    '''Test that an OpenACC Enter Data directive, when added to a schedule
    with multiple loops, raises the expected exception, as there is no
    following OpenACC Parallel or OpenACCKernels directive and at
    least one is required. This test uses the dynamo0.3 API.

    '''
    acc_enter_trans = ACCEnterDataTrans()
    _, info = parse(os.path.join(BASE_PATH, "1.2_multi_invoke.f90"))
    psy = PSyFactory(distributed_memory=False).create(info)
    sched = psy.invokes.get('invoke_0').schedule
    acc_enter_trans.apply(sched)
    with pytest.raises(GenerationError) as excinfo:
        str(psy.gen)
    assert ("ACCEnterData directive did not find any data to copyin. Perhaps "
            "there are no ACCParallel or ACCKernels directives within the "
            "region?" in str(excinfo.value))


# (3/4) Method gen_code
@pytest.mark.parametrize("trans", [ACCParallelTrans, ACCKernelsTrans])
def test_accenterdatadirective_gencode_3(trans):
    '''Test that an OpenACC Enter Data directive, when added to a schedule
    with a single loop, produces the expected code (there should be
    "copy in" data as there is a following OpenACC parallel or kernels
    directive). This test uses the dynamo0.3 API.

    '''
    acc_trans = trans()
    acc_enter_trans = ACCEnterDataTrans()
    _, info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"))
    psy = PSyFactory(distributed_memory=False).create(info)
    sched = psy.invokes.get('invoke_0_testkern_type').schedule
    acc_trans.apply(sched.children)
    acc_enter_trans.apply(sched)
    code = str(psy.gen)
    assert (
        "      !$acc enter data copyin(f1_data,f2_data,m1_data,m2_data,"
        "map_w1,map_w2,map_w3,ndf_w1,ndf_w2,ndf_w3,nlayers,"
        "undf_w1,undf_w2,undf_w3)\n" in code)


# (4/4) Method gen_code
@pytest.mark.parametrize("trans1,trans2",
                         [(ACCParallelTrans, ACCParallelTrans),
                          (ACCParallelTrans, ACCKernelsTrans),
                          (ACCKernelsTrans, ACCParallelTrans),
                          (ACCKernelsTrans, ACCKernelsTrans)])
def test_accenterdatadirective_gencode_4(trans1, trans2):
    '''Test that an OpenACC Enter Data directive, when added to a schedule
    with multiple loops and multiple OpenACC parallel and/or Kernel
    directives, produces the expected code (when the same argument is
    used in multiple loops there should only be one entry). This test
    uses the dynamo0.3 API.

    '''
    acc_trans1 = trans1()
    acc_trans2 = trans2()
    acc_enter_trans = ACCEnterDataTrans()
    _, info = parse(os.path.join(BASE_PATH, "1.2_multi_invoke.f90"))
    psy = PSyFactory(distributed_memory=False).create(info)
    sched = psy.invokes.get('invoke_0').schedule
    acc_trans1.apply([sched.children[1]])
    acc_trans2.apply([sched.children[0]])
    acc_enter_trans.apply(sched)
    code = str(psy.gen)
    assert (
        "      !$acc enter data copyin(f1_data,f2_data,f3_data,m1_data,"
        "m2_data,map_w1,map_w2,map_w3,ndf_w1,ndf_w2,ndf_w3,"
        "nlayers,undf_w1,undf_w2,undf_w3)\n" in code)


# Class ACCLoopDirective start

def test_accloopdirective_node_str(monkeypatch):
    ''' Test the node_str() method of ACCLoopDirective node '''
    directive = ACCLoopDirective()

    # Mock the coloured name as this is tested elsewhere
    monkeypatch.setattr(directive, "coloured_name",
                        lambda x: "ACCLoopDirective")

    # Default value output
    expected = ("ACCLoopDirective[sequential=False,gang=False,vector=False,"
                "collapse=None,independent=True]")
    assert directive.node_str() == expected
    assert str(directive) == expected

    # Non-default value output
    directive._sequential = True
    directive._collapse = 2
    directive._independent = False
    directive._gang = True
    directive._vector = True
    expected = ("ACCLoopDirective[sequential=True,gang=True,vector=True,"
                "collapse=2,independent=False]")
    assert directive.node_str() == expected
    assert str(directive) == expected


def test_accloopdirective_collapse_getter_and_setter():
    ''' Test the ACCLoopDirective collapse property setter and getter.'''
    target = ACCLoopDirective()
    assert target.collapse is None
    target.collapse = 3
    assert target.collapse == 3
    target.collapse = None
    assert target.collapse is None

    with pytest.raises(ValueError) as err:
        target.collapse = 0
    assert ("The ACCLoopDirective collapse clause must be a positive integer "
            "or None, but value '0' has been given." in str(err.value))

    with pytest.raises(TypeError) as err:
        target.collapse = 'a'
    assert ("The ACCLoopDirective collapse clause must be a positive integer "
            "or None, but value 'a' has been given." in str(err.value))


def test_accloopdirective_equality():
    ''' Test the __eq__ method of ACCLoopDirective node. '''
    # We need to manually set the same SymbolTable instance in both directives
    # for their equality to be True
    symboltable = SymbolTable()
    directive1 = ACCLoopDirective()
    directive2 = ACCLoopDirective()
    directive1.children[0]._symbol_table = symboltable
    directive2.children[0]._symbol_table = symboltable
    assert directive1 == directive2

    # Check equality fails when collapse is different
    directive2._collapse = 2
    assert directive1 != directive2

    # Check equality fails when independent is different
    directive2._collapse = directive1.collapse
    directive2._independent = False
    assert directive1 != directive2

    # Check equality fails when sequential is different
    directive2._independent = directive1.independent
    directive2._sequential = not directive1._sequential
    assert directive1 != directive2

    # Check equality fails when gang is different
    directive2._sequential = directive1.sequential
    directive2._gang = not directive1._gang
    assert directive1 != directive2

    # Check equality fails when vector is different
    directive2._gang = directive1.gang
    directive2._vector = not directive1._vector
    assert directive1 != directive2


def test_accloopdirective_validate(fortran_reader):
    '''
    Check the validate_global_constraints method of ACCLoopDirective. For
    an ACC loop to validate, it must either be within an 'ACC parallel/kernels'
    region or in a routine with an 'ACC routine' directive.

    '''
    code = '''\
subroutine my_sub()
  implicit none
  real, dimension(10,10) :: var
  integer :: ji, jj
  do jj = 1, 10
    do ji = 1, 10
      var(ji,jj) = ji + jj
    end do
  end do
end subroutine my_sub'''
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    # Add an orphan ACC loop directive.
    acclooptrans = ACCLoopTrans()
    acclooptrans.apply(routine[0])
    # This should be rejected.
    with pytest.raises(GenerationError) as err:
        routine[0].validate_global_constraints()
    assert ("ACCLoopDirective in routine 'my_sub' must either have an "
            "ACCParallelDirective or ACCKernelsDirective as an ancestor in "
            "the Schedule or the routine must contain an ACCRoutineDirective."
            in str(err.value))
    # Add an ACCRoutineDirective.
    accrtrans = ACCRoutineTrans()
    accrtrans.apply(routine)
    routine[0].validate_global_constraints()
    # Remove the ACCRoutineDirective.
    routine.children.pop(index=0)
    with pytest.raises(GenerationError) as err:
        routine[0].validate_global_constraints()
    # Add an ACC Parallel region
    accptrans = ACCParallelTrans()
    accptrans.apply(routine.children)
    routine[0].validate_global_constraints()

# Class ACCLoopDirective end


# Class ACCKernelsDirective start

# (1/1) Method __init__
def test_acckernelsdirective_init():
    '''Test an ACCKernelsDirective can be created and that the optional
    arguments are set and can be set as expected.

    '''
    directive = ACCKernelsDirective()
    assert directive._default_present
    assert directive.parent is None
    assert len(directive.children) == 1
    assert isinstance(directive.children[0], Schedule)
    directive = ACCKernelsDirective(default_present=False)
    assert not directive._default_present


# (1/1) Method gen_code
@pytest.mark.parametrize("default_present", [False, True])
def test_acckernelsdirective_gencode(default_present):
    '''Check that the gen_code method in the ACCKernelsDirective class
    generates the expected code. Use the dynamo0.3 API.

    '''
    _, info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"))
    psy = PSyFactory(distributed_memory=False).create(info)
    sched = psy.invokes.get('invoke_0_testkern_type').schedule

    trans = ACCKernelsTrans()
    trans.apply(sched, {"default_present": default_present})

    code = str(psy.gen)
    string = ""
    if default_present:
        string = " default(present)"
    assert (
        f"      !$acc kernels{string}\n"
        f"      DO cell=loop0_start,loop0_stop\n" in code)
    assert (
        "      END DO\n"
        "      !$acc end kernels\n" in code)


def test_acckerneldirective_equality():
    ''' Test the __eq__ method of ACCKernelsDirective node. '''
    # We need to manually set the same SymbolTable instance in both directives
    # for their equality to be True
    symboltable = SymbolTable()
    directive1 = ACCKernelsDirective()
    directive2 = ACCKernelsDirective()
    directive1.children[0]._symbol_table = symboltable
    directive2.children[0]._symbol_table = symboltable
    assert directive1 == directive2

    # Check equality fails when default_present is different
    directive2._default_present = not directive1._default_present
    assert directive1 != directive2

# Class ACCRoutineDirective


def test_acc_routine_directive_constructor_and_strings():
    ''' Test the ACCRoutineDirective constructor and its output
    strings.'''
    target = ACCRoutineDirective()
    assert target.begin_string() == "acc routine"
    assert str(target) == "ACCRoutineDirective[]"

    temporary_module = ModuleGen("test")
    target.gen_code(temporary_module)
    assert "!$acc routine\n" in str(temporary_module.root)


# Class ACCUpdateDirective

def test_accupdatedirective_init():
    ''' Test the constructor of ACCUpdateDirective node. '''

    # Check argument validations
    with pytest.raises(TypeError) as err:
        _ = ACCUpdateDirective({"invalid"}, "host")
    assert ("The ACCUpdateDirective signatures argument must be a "
            "set of signatures but got {'str'}"
            in str(err.value))

    sig = {Signature("x")}
    with pytest.raises(ValueError) as err:
        _ = ACCUpdateDirective(sig, "invalid")
    assert ("The ACCUpdateDirective direction argument must be a string with "
            "any of the values in '('self', 'host', 'device')' but found "
            "'invalid'." in str(err.value))

    with pytest.raises(TypeError) as err:
        _ = ACCUpdateDirective(sig, "host", if_present=1)
    assert ("The ACCUpdateDirective if_present argument must be a "
            "boolean but got int"
            in str(err.value))

    # Successful init
    directive = ACCUpdateDirective(sig, "host")
    assert directive.sig_set == sig
    assert directive.direction == "host"
    assert directive.if_present is True

    directive = ACCUpdateDirective(sig, "host", if_present=False)
    assert directive.if_present is False


def test_accupdatedirective_begin_string():
    ''' Test the begin_string method of ACCUpdateDirective. '''

    sig = {Signature("x")}
    directive_host = ACCUpdateDirective(sig, "host", if_present=False)
    directive_device = ACCUpdateDirective(sig, "device")
    directive_empty = ACCUpdateDirective(set(), "host", if_present=False)

    assert directive_host.begin_string() == "acc update host(x)"
    assert directive_device.begin_string() == "acc update if_present device(x)"

    with pytest.raises(GenerationError) as err:
        directive_empty.begin_string()
    assert ("ACCUpdate directive did not find any data to update."
            in str(err.value))


def test_accupdatedirective_equality():
    ''' Test the __eq__ method of ACCUpdateDirective node. '''
    sig = {Signature("x")}
    directive1 = ACCUpdateDirective(sig, "device")
    directive2 = ACCUpdateDirective(sig, "device")
    assert directive1 == directive2

    # Check equality fails when different signatures
    directive3 = ACCUpdateDirective({Signature("t")}, "device")
    assert directive1 != directive3

    # Check equality fails when different directions
    directive4 = ACCUpdateDirective(sig, "host")
    assert directive1 != directive4

    # Check equality fails when different if_present settings
    directive5 = ACCUpdateDirective(sig, "device", if_present=False)
    assert directive1 != directive5


def test_accdatadirective_update_data_movement_clauses(fortran_reader,
                                                       fortran_writer):
    '''Test that the data movement clauses are constructed correctly for the
    ACCDataDirective class and that they are updated appropriately when the
    tree beneath them is changed.

    '''
    psyir = fortran_reader.psyir_from_source(
        "program dtype_read\n"
        "use field_mod, only: fld_type\n"
        "integer, parameter :: jpj = 10\n"
        "type(fld_type), dimension(5) :: small_holding\n"
        "real, dimension(jpj) :: sto_tmp\n"
        "integer :: ji, jf\n"
        "real, dimension(jpj) :: sfactor\n"
        "sfactor(:) = 0.1\n"
        "sto_tmp(:) = 0.0\n"
        "jf = 3\n"
        "do ji = 1,jpj\n"
        "  sto_tmp(ji) = sto_tmp(ji) + small_holding(3)%grid(jf)%data(ji)\n"
        "  sto_tmp(ji) = sfactor * sto_tmp(ji)\n"
        "end do\n"
        "end program dtype_read\n")
    loop = psyir.walk(Loop)[0]
    dtrans = ACCDataTrans()
    dtrans.apply(loop)
    output = fortran_writer(psyir)
    expected = ("!$acc data copyin(sfactor,small_holding,small_holding(3)%grid"
                ",small_holding(3)%grid(jf)%data), copy(sto_tmp)")
    assert expected in output
    # Check that calling update_signal() explicitly has no effect as the tree
    # has not changed.
    loop.update_signal()
    output = fortran_writer(psyir)
    assert expected in output
    # Now remove the second statement from the loop body.
    del loop.loop_body.children[1]
    output = fortran_writer(psyir)
    # 'sfactor' should have been removed from the copyin()
    assert ("!$acc data copyin(small_holding,small_holding(3)%grid,"
            "small_holding(3)%grid(jf)%data), copy(sto_tmp)" in output)


def test_accparalleldirective():
    '''
    Test the ACCParallelDirective constructors, property getters and
    setters and string methods.
    '''

    # It can be created
    accpar = ACCParallelDirective()
    assert isinstance(accpar, ACCParallelDirective)
    assert accpar._default_present is True

    # Also without default(present)
    accpar = ACCParallelDirective(default_present=False)
    assert isinstance(accpar, ACCParallelDirective)
    assert accpar._default_present is False

    # But only with boolean values
    with pytest.raises(TypeError) as err:
        _ = ACCParallelDirective(default_present=3)
    assert ("The ACCParallelDirective default_present property must be a "
            "boolean but value '3' has been given." in str(err.value))

    # The default present value has getter and setter
    accpar.default_present = True
    assert accpar.default_present is True

    with pytest.raises(TypeError) as err:
        accpar.default_present = "invalid"
    assert ("The ACCParallelDirective default_present property must be a "
            "boolean but value 'invalid' has been given." in str(err.value))

    # The begin string depends on the default present value
    accpar.default_present = True
    assert accpar.begin_string() == "acc parallel default(present)"
    accpar.default_present = False
    assert accpar.begin_string() == "acc parallel"

    # It has an end_string
    assert accpar.end_string() == "acc end parallel"


def test_acc_atomics_is_valid_atomic_statement(fortran_reader):
    ''' Test the ACCAtomicDirective can identify when a statement is a valid
    expression to support OpenACC atomics. '''

    code = '''
    subroutine my_subroutine()
        integer, dimension(10, 10) :: A = 1
        integer, dimension(10, 10) :: B = 2
        integer :: i, j, val

        A(1,1) = A(1,1) * 2
        A(1,1) = A(1,1) / (2 + 3 - 5)
        A(1,1) = MAX(A(1,1), A(1,2))
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    for stmt in tree.walk(Assignment):
        assert ACCAtomicDirective.is_valid_atomic_statement(stmt)

    code = '''
    subroutine my_subroutine()
        integer, dimension(10, 10) :: A = 1
        integer, dimension(10, 10) :: B = 2
        integer :: i, j, val

        A(1,1) = A(1,1) ** 2  ! Operator is not supported
        A(1,1) = A(2,1) * 2   ! The operands are different that the lhs
        A(1,1) = A(1,1) / 2 + 3 - 5  ! A(1,1) is not a top-level operand
        A(:,1) = A(:,1) / 2      ! It is not a scalar expression
        A(1,1) = MOD(A(1,1), 3)  ! Intrinsic is not supported
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    for stmt in tree.walk(Assignment):
        assert not ACCAtomicDirective.is_valid_atomic_statement(stmt)

    # Its also not valid if its not an Assignment
    assert not ACCAtomicDirective.is_valid_atomic_statement(Return())


def test_acc_atomics_validate_global_constraints(fortran_reader, monkeypatch):
    ''' Test the ACCAtomicDirective can check the globals constraints to
    validate that the directive is correctly formed.'''

    code = '''
    subroutine my_subroutine()
        integer, dimension(10, 10) :: A = 1
        integer, dimension(10, 10) :: B = 2
        integer :: i, j, val

        A(1,1) = A(1,1) * 2
    end subroutine
    '''
    tree = fortran_reader.psyir_from_source(code)
    routine = tree.walk(Routine)[0]
    stmt = routine.children[0]
    atomic = ACCAtomicDirective()
    atomic.dir_body.addchild(stmt.detach())
    routine.addchild(atomic)

    # This is a valid atomic
    atomic.validate_global_constraints()

    # If the statement is invalid (for any reason already tested in a previous
    # test), it raises an error
    monkeypatch.setattr(atomic, "is_valid_atomic_statement", lambda _: False)
    with pytest.raises(GenerationError) as err:
        atomic.validate_global_constraints()
    assert "is not a valid OpenACC Atomic statement." in str(err.value)

    # If it doesn not have an associated statement
    atomic.dir_body[0].detach()
    with pytest.raises(GenerationError) as err:
        atomic.validate_global_constraints()
    assert ("Atomic directives must always have one and only one associated "
            "statement, but found " in str(err.value))


def test_acc_atomics_srtings():
    ''' Test the ACCAtomicDirective begin and end strings '''
    atomic = ACCAtomicDirective()
    assert atomic.begin_string() == "acc atomic"
    assert atomic.end_string() == "acc end atomic"
