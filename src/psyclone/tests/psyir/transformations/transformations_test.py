# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2024, Science and Technology Facilities Council.
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
# ----------------------------------------------------------------------------
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
#         A. B. G. Chalk, STFC Daresbury Lab
# Modified I. Kavcic, Met Office
# Modified J. Henrichs, Bureau of Meteorology

'''
API-agnostic tests for various transformation classes.
'''

import os
import pytest
from fparser.common.readfortran import FortranStringReader
from psyclone.errors import InternalError
from psyclone.psyir.nodes import CodeBlock, IfBlock, Literal, Loop, Node, \
    Reference, Schedule, Statement, ACCLoopDirective, OMPMasterDirective, \
    OMPDoDirective, OMPLoopDirective, Routine
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE, BOOLEAN_TYPE, \
    ImportInterface, ContainerSymbol
from psyclone.psyir.tools import DependencyTools
from psyclone.psyir.transformations import ProfileTrans, RegionTrans, \
    TransformationError
from psyclone.tests.utilities import get_invoke, Compile
from psyclone.transformations import ACCEnterDataTrans, ACCLoopTrans, \
    ACCParallelTrans, OMPLoopTrans, OMPParallelLoopTrans, OMPParallelTrans, \
    OMPSingleTrans, OMPMasterTrans, OMPTaskloopTrans, OMPDeclareTargetTrans
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory

GOCEAN_BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                os.pardir, os.pardir, "test_files",
                                "gocean1p0")


@pytest.fixture(name="sample_psyir")
def sample_psyir_fixture(fortran_reader):
    ''' Snippet of code converted to PSyIR to use during the tests. '''
    code = '''
    subroutine my_subroutine()
        integer, dimension(10, 10) :: A
        integer :: i
        integer :: j
        do i = 1, 10
            do j = 1, 10
                A(i, j) = 0
            end do
        end do
        do i = 1, 10
            do j = 1, 10
                A(i, j) = 0
            end do
        end do
    end subroutine
    '''
    return fortran_reader.psyir_from_source(code)


def test_accloop():
    ''' Generic tests for the ACCLoopTrans transformation class '''
    trans = ACCLoopTrans()
    assert trans.name == "ACCLoopTrans"
    assert str(trans) == "Adds an 'OpenACC loop' directive to a loop"

    cnode = Statement()
    tdir = trans._directive([cnode])
    assert isinstance(tdir, ACCLoopDirective)


def test_accparallel():
    ''' Generic tests for the ACCParallelTrans class '''
    acct = ACCParallelTrans()
    assert acct.name == "ACCParallelTrans"
    assert acct._default_present is True

    acct = ACCParallelTrans(default_present=False)
    assert acct.name == "ACCParallelTrans"
    assert acct._default_present is False

    with pytest.raises(TypeError) as err:
        _ = ACCParallelTrans(default_present=3)
    assert ("The provided 'default_present' argument must be a boolean, "
            "but found '3'." in str(err.value))


def test_accenterdata():
    ''' Generic tests for the ACCEnterDataTrans class '''
    acct = ACCEnterDataTrans()
    assert acct.name == "ACCEnterDataTrans"
    assert str(acct) == "Adds an OpenACC 'enter data' directive"


def test_accenterdata_internalerr(monkeypatch):
    ''' Check that the ACCEnterDataTrans.apply() method raises an internal
    error if the validate method fails to throw out an invalid type of
    Schedule. '''
    acct = ACCEnterDataTrans()
    monkeypatch.setattr(acct, "validate", lambda sched, options: None)
    with pytest.raises(InternalError) as err:
        acct.apply("Not a schedule")
    assert ("validate() has not rejected an (unsupported) schedule"
            in str(err.value))


def test_omptaskloop_no_collapse():
    ''' Check that the OMPTaskloopTrans.directive() method rejects
    the collapse argument '''
    trans = OMPTaskloopTrans()
    cnode = Node()
    with pytest.raises(NotImplementedError) as err:
        trans._directive(cnode, collapse=True)
    assert ("The COLLAPSE clause is not yet supported for "
            "'!$omp taskloop' directives" in str(err.value))


def test_omptaskloop_getters_and_setters():
    ''' Check that the OMPTaskloopTrans getters and setters
    correctly throw TransformationErrors on illegal values '''
    trans = OMPTaskloopTrans()
    with pytest.raises(TransformationError) as err:
        trans.omp_num_tasks = "String"
    assert "num_tasks must be an integer or None, got str" in str(err.value)
    with pytest.raises(TransformationError) as err:
        trans.omp_num_tasks = -1
    assert "num_tasks must be a positive integer, got -1" in str(err.value)
    with pytest.raises(TransformationError) as err:
        trans.omp_grainsize = "String"
    assert "grainsize must be an integer or None, got str" in str(err.value)
    with pytest.raises(TransformationError) as err:
        trans.omp_grainsize = -1
    assert "grainsize must be a positive integer, got -1" in str(err.value)
    trans.omp_num_tasks = 32
    assert trans.omp_num_tasks == 32
    with pytest.raises(TransformationError) as err:
        trans.omp_grainsize = 32
    assert ("The grainsize and num_tasks clauses would both "
            "be specified for this Taskloop transformation"
            in str(err.value))
    trans.omp_num_tasks = None
    assert trans.omp_num_tasks is None
    trans.omp_grainsize = 32
    assert trans.omp_grainsize == 32
    trans.grainsize = None
    assert trans.grainsize is None

    trans = OMPTaskloopTrans(num_tasks=32)
    assert trans.omp_num_tasks == 32
    trans = OMPTaskloopTrans(grainsize=32)
    assert trans.omp_grainsize == 32

    with pytest.raises(TransformationError) as err:
        trans = OMPTaskloopTrans(grainsize=32, num_tasks=32)
    assert ("The grainsize and num_tasks clauses would both "
            "be specified for this Taskloop transformation"
            in str(err.value))

    with pytest.raises(TypeError) as err:
        trans = OMPTaskloopTrans(nogroup=32)
    assert "Expected nogroup to be a bool but got a int" in str(err.value)


def test_omptaskloop_apply(monkeypatch):
    '''Check that the gen_code method in the OMPTaskloopDirective
    class generates the expected code when passing options to
    the OMPTaskloopTrans's apply method and correctly overrides the
    taskloop's inbuilt value. Use the gocean API.
    '''
    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH, "single_invoke.f90"),
                           api="gocean1.0")
    taskloop = OMPTaskloopTrans()
    master = OMPMasterTrans()
    parallel = OMPParallelTrans()
    psy = PSyFactory("gocean1.0", distributed_memory=False).\
        create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule

    # Check that the _nogroup clause isn't changed during apply
    assert taskloop._nogroup is False
    taskloop.apply(schedule.children[0], {"nogroup": True})
    assert taskloop._nogroup is False
    taskloop_node = schedule.children[0]
    master.apply(schedule.children[0])
    parallel.apply(schedule.children[0])

    code = str(psy.gen)

    clauses = " nogroup"
    assert (
        f"    !$omp parallel default(shared), private(i,j)\n"
        f"      !$omp master\n"
        f"      !$omp taskloop{clauses}\n"
        f"      DO" in code)
    assert (
        "      END DO\n"
        "      !$omp end taskloop\n"
        "      !$omp end master\n"
        "      !$omp end parallel" in code)

    assert taskloop_node.begin_string() == "omp taskloop"

    # Create a fake validate function to throw an exception
    def validate(self, options):
        raise TransformationError("Fake error")
    monkeypatch.setattr(taskloop, "validate", validate)
    # Test that the nogroup attribute isn't permanently changed if validate
    # throws an exception
    assert taskloop._nogroup is False
    with pytest.raises(TransformationError) as excinfo:
        _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH,
                               "single_invoke.f90"), api="gocean1.0")
        schedule = psy.invokes.invoke_list[0].schedule
        taskloop.apply(schedule[0], {"nogroup": True})
    assert "Fake error" in str(excinfo.value)
    assert taskloop._nogroup is False


def test_ompdeclaretargettrans(sample_psyir, fortran_writer):
    ''' Test OMPDeclareTargetTrans works as expected.'''

    # Try to insert a OMPDeclareTarget on a wrong node type
    ompdeclaretargettrans = OMPDeclareTargetTrans()
    loop = sample_psyir.walk(Loop)[0]
    with pytest.raises(TransformationError) as err:
        ompdeclaretargettrans.apply(loop)
    assert ("The OMPDeclareTargetTrans must be applied to a Routine, but "
            "found: 'Loop'." in str(err.value))

    # Insert a OMPDeclareTarget on a Routine
    routine = sample_psyir.walk(Routine)[0]
    ompdeclaretargettrans.apply(routine)
    expected = '''\
subroutine my_subroutine()
  integer, dimension(10,10) :: a
  integer :: i
  integer :: j

  !$omp declare target
  do i = 1, 10, 1
'''
    assert expected in fortran_writer(sample_psyir)

    # If the OMPDeclareTarget directive is already there do not repeat it
    previous_num_children = len(routine.children)
    ompdeclaretargettrans.apply(routine)
    assert previous_num_children == len(routine.children)


def test_ompdeclaretargettrans_with_globals(sample_psyir, parser):
    ''' Test that the ompdelcaretarget is not added if there is any global
    symbol'''
    ompdeclaretargettrans = OMPDeclareTargetTrans()
    routine = sample_psyir.walk(Routine)[0]
    ref1 = sample_psyir.walk(Reference)[0]

    # Symbol not defined in the symbol table will be considered global
    ref1.symbol = DataSymbol("new_symbol", INTEGER_TYPE)
    with pytest.raises(TransformationError) as err:
        ompdeclaretargettrans.apply(routine)
    assert ("Kernel 'my_subroutine' contains accesses to data (variable "
            "'new_symbol') that are not present in the Symbol Table(s) within "
            "the scope of this routine. Cannot transform such a kernel."
            in str(err.value))

    # If it is local but comes from an import it is also a global
    routine.symbol_table.add(ref1.symbol)
    ref1.symbol.interface = ImportInterface(ContainerSymbol('my_mod'))
    with pytest.raises(TransformationError) as err:
        ompdeclaretargettrans.apply(routine)
    assert ("The Symbol Table for kernel 'my_subroutine' contains the "
            "following symbol(s) with imported interface: ['new_symbol']. "
            "If these symbols represent data then they must first be "
            "converted to kernel arguments using the KernelImportsToArguments "
            "transformation. If the symbols represent external routines then "
            "PSyclone cannot currently transform this kernel for execution on "
            "an OpenMP target." in str(err.value))

    # If the symbol is inside a CodeBlock it is also captured
    reader = FortranStringReader('''
    subroutine mytest
        not_declared1 = not_declared1 + not_declared2
    end subroutine mytest''')
    prog = parser(reader)
    block = CodeBlock(prog.children[0].children[1].children[0].children,
                      CodeBlock.Structure.EXPRESSION)
    ref1.replace_with(block)
    with pytest.raises(TransformationError) as err:
        ompdeclaretargettrans.apply(routine)
    assert ("Kernel 'my_subroutine' contains accesses to data (variable "
            "'not_declared1') that are not present in the Symbol Table(s) "
            "within the scope of this routine. Cannot transform such a kernel."
            in str(err.value))


def test_omplooptrans_properties():
    ''' Test that the OMPLoopTrans properties assign and return the expected
    values and raise errors when necessary. '''

    # Check default values
    omplooptrans = OMPLoopTrans()
    assert omplooptrans.omp_schedule == "auto"
    assert omplooptrans.omp_directive == "do"

    # Use setters with valid values
    omplooptrans.omp_schedule = "dynamic,2"
    omplooptrans.omp_directive = "paralleldo"
    assert omplooptrans.omp_schedule == "dynamic,2"
    assert omplooptrans.omp_directive == "paralleldo"

    # Setting things at the constructor also works
    omplooptrans = OMPLoopTrans(omp_schedule="dynamic,2",
                                omp_directive="loop")
    assert omplooptrans.omp_schedule == "dynamic,2"
    assert omplooptrans.omp_directive == "loop"

    # Use setters with invalid values
    with pytest.raises(TypeError) as err:
        omplooptrans.omp_directive = "invalid"
    assert ("The OMPLoopTrans.omp_directive property must be a str with "
            "the value of ['do', 'paralleldo', 'teamsdistributeparalleldo', "
            "'loop'] but found a 'str' with value 'invalid'."
            in str(err.value))

    with pytest.raises(TypeError) as err:
        omplooptrans.omp_schedule = 3
    assert ("The OMPLoopTrans.omp_schedule property must be a 'str' but"
            " found a 'int'." in str(err.value))

    with pytest.raises(ValueError) as err:
        omplooptrans.omp_schedule = "invalid"
    assert ("Valid OpenMP schedules are ['runtime', 'static', 'dynamic', "
            "'guided', 'auto', 'none'] but got 'invalid'." in str(err.value))

    with pytest.raises(ValueError) as err:
        omplooptrans.omp_schedule = "auto,3"
    assert ("Cannot specify a chunk size when using an OpenMP schedule "
            "of 'auto'." in str(err.value))

    with pytest.raises(ValueError) as err:
        omplooptrans.omp_schedule = "dynamic,a"
    assert ("Supplied OpenMP schedule 'dynamic,a' has an invalid chunk-size."
            in str(err.value))

    with pytest.raises(ValueError) as err:
        omplooptrans.omp_schedule = "dynamic,"
    assert ("Supplied OpenMP schedule 'dynamic,' has an invalid chunk-size."
            in str(err.value))


def test_parallellooptrans_validate_dependencies(fortran_reader):
    ''' Test that the parallellooptrans validation checks for loop carried
    dependencies. '''

    def create_loops(body):
        psyir = fortran_reader.psyir_from_source(f'''
        subroutine my_subroutine()
            integer :: ji, jj, jk, jpkm1, jpjm1, jpim1
            real, dimension(10, 10, 10) :: zwt, zwd, zwi, zws
            real :: total
            {body}
        end subroutine''')
        return psyir.walk(Loop)

    # Use OMPLoopTrans as a concrete class of ParallelLoopTrans
    omplooptrans = OMPLoopTrans()
    # Example with a loop carried dependency in jk dimension
    loops = create_loops('''
        do jk = 2, jpkm1, 1
          do jj = 2, jpjm1, 1
            do ji = 2, jpim1, 1
              zwt(ji,jj,jk) = zwd(ji,jj,jk) - zwi(ji,jj,jk) * &
                              zws(ji,jj,jk - 1) / zwt(ji,jj,jk - 1)
            enddo
          enddo
        enddo''')

    # Check that the loop can not be parallelised due to the loop-carried
    # dependency.
    with pytest.raises(TransformationError) as err:
        omplooptrans.validate(loops[0])
    assert ("Transformation Error: Dependency analysis failed with the "
            "following messages:\nError: The write access to 'zwt(ji,jj,jk)' "
            "and to 'zwt(ji,jj,jk - 1)' are dependent and cannot be "
            "parallelised" in str(err.value))

    # However, the inner loop can be parallelised because the dependency is
    # just with 'jk' and it is not modified in the inner loops
    omplooptrans.validate(loops[1])

    # Reductions also indicate a data dependency that needs to be handled, so
    # we don't permit the parallelisation of the loop (until we support
    # reduction clauses)
    loops = create_loops('''
        do jk = 2, jpkm1, 1
          do jj = 2, jpjm1, 1
            do ji = 2, jpim1, 1
              total = total + zwt(ji,jj,jk)
            enddo
          enddo
        enddo''')
    with pytest.raises(TransformationError) as err:
        omplooptrans.validate(loops[0])
    assert ("Transformation Error: Dependency analysis failed with the "
            "following messages:\nWarning: Variable 'total' is read first, "
            "which indicates a reduction." in str(err.value))

    # Shared scalars are race conditions but these are accepted because it
    # can be manage with the appropriate clause
    loops = create_loops('''
        do jk = 2, jpkm1, 1
          do jj = 2, jpjm1, 1
            do ji = 2, jpim1, 1
              total = zwt(ji,jj,jk)
            enddo
          enddo
        enddo''')
    assert not DependencyTools().can_loop_be_parallelised(loops[0])
    omplooptrans.validate(loops[0])


def test_omplooptrans_apply_firstprivate(fortran_reader, fortran_writer,
                                         tmpdir):
    ''' Test applying the OMPLoopTrans in cases where a firstprivate
    clause is needed to generate code that is functionally equivalent to the
    original, serial version.'''

    # Example with a conditional write and a OMPParallelDoDirective
    psyir = fortran_reader.psyir_from_source('''
        module my_mod
            contains
            subroutine my_subroutine()
                integer :: ji, jj, jk, jpkm1, jpjm1, jpim1, scalar1, scalar2
                real, dimension(10, 10, 10) :: zwt, zwd, zwi, zws
                scalar1 = 1
                do jk = 2, jpkm1, 1
                  do jj = 2, jpjm1, 1
                    do ji = 2, jpim1, 1
                       if (.true.) then
                          scalar1 = zwt(ji,jj,jk)
                       endif
                       scalar2 = scalar1 + zwt(ji,jj,jk)
                       zws(ji,jj,jk) = scalar2
                    enddo
                  enddo
                enddo
            end subroutine
        end module my_mod''')
    omplooptrans = OMPParallelLoopTrans()
    loop = psyir.walk(Loop)[0]
    omplooptrans.apply(loop)
    expected = '''\
    !$omp parallel do default(shared), private(ji,jj,jk,scalar2), \
firstprivate(scalar1), schedule(auto)
    do jk = 2, jpkm1, 1
      do jj = 2, jpjm1, 1
        do ji = 2, jpim1, 1
          if (.true.) then
            scalar1 = zwt(ji,jj,jk)
          end if
          scalar2 = scalar1 + zwt(ji,jj,jk)
          zws(ji,jj,jk) = scalar2
        enddo
      enddo
    enddo
    !$omp end parallel do\n'''

    gen = fortran_writer(psyir)
    assert expected in gen
    assert Compile(tmpdir).string_compiles(gen)


def test_omplooptrans_apply_firstprivate_fail(fortran_reader):
    ''' Test applying the OMPLoopTrans in cases where a firstprivate
    clause it is needed to generate functionally equivalent code than
    the starting serial version.

    In some cases the transformation validate dependency analysis reports
    the firstprivate use as a reduction, which is wrong.

    '''

    # Example with a read before write and a OMPParallelDirective
    psyir = fortran_reader.psyir_from_source('''
        subroutine my_subroutine()
            integer :: ji, jj, jk, jpkm1, jpjm1, jpim1, scalar1, scalar2
            real, dimension(10, 10, 10) :: zwt, zwd, zwi, zws
            do jk = 2, jpkm1, 1
              do jj = 2, jpjm1, 1
                do ji = 2, jpim1, 1
                   scalar2 = scalar1 + zwt(ji,jj,jk)
                   scalar1 = 3
                   zws(ji,jj,jk) = scalar2 + scalar1
                enddo
              enddo
            enddo
        end subroutine''')
    omplooptrans = OMPParallelLoopTrans()
    loop = psyir.walk(Loop)[0]
    try:
        omplooptrans.apply(loop)
    except TransformationError:
        # TODO #598: When this is solved, this test can be removed and the
        # "force":True in the previous test can also be removed
        pytest.xfail(reason="Issue #598: This example should be a firstprivate"
                            " but the dependency analysis believes it is a "
                            "reduction.")


def test_omplooptrans_apply(sample_psyir, fortran_writer):
    ''' Test OMPLoopTrans works as expected with the different options. '''

    # By default it adds a OMPDoDirective with static schedule
    omplooptrans = OMPLoopTrans()
    tree = sample_psyir.copy()
    omplooptrans.apply(tree.walk(Loop)[0])
    assert isinstance(tree.walk(Loop)[0].parent, Schedule)
    assert isinstance(tree.walk(Loop)[0].parent.parent, OMPDoDirective)
    assert tree.walk(Loop)[0].parent.parent._omp_schedule == 'auto'

    # The omp_schedule can be changed
    omplooptrans = OMPLoopTrans(omp_schedule="dynamic,2")
    ompparalleltrans = OMPParallelTrans()
    tree = sample_psyir.copy()
    loop1 = tree.walk(Loop)[0]
    omplooptrans.apply(loop1)
    assert isinstance(loop1.parent, Schedule)
    assert isinstance(loop1.parent.parent, OMPDoDirective)
    assert loop1.parent.parent._omp_schedule == 'dynamic,2'
    ompparalleltrans.apply(loop1.parent.parent)  # Needed for generation

    # The omp_directive can be changed
    omplooptrans = OMPLoopTrans(omp_directive="loop")
    loop2 = tree.walk(Loop, stop_type=Loop)[1]
    omplooptrans.apply(loop2, {'collapse': 2})
    assert isinstance(loop2.parent, Schedule)
    assert isinstance(loop2.parent.parent, OMPLoopDirective)
    ompparalleltrans.apply(loop2.parent.parent)  # Needed for generation

    # Check that the full resulting code looks like this
    expected = '''
  !$omp parallel default(shared), private(i,j)
  !$omp do schedule(dynamic,2)
  do i = 1, 10, 1
    do j = 1, 10, 1
      a(i,j) = 0
    enddo
  enddo
  !$omp end do
  !$omp end parallel
  !$omp parallel default(shared), private(i,j)
  !$omp loop collapse(2)
  do i = 1, 10, 1
    do j = 1, 10, 1
      a(i,j) = 0
    enddo
  enddo
  !$omp end loop
  !$omp end parallel\n'''

    assert expected in fortran_writer(tree)


def test_ifblock_children_region():
    ''' Check that we reject attempts to transform the conditional part of
    an If statement or to include both the if- and else-clauses in a region
    (without their parent). '''
    acct = ACCParallelTrans()
    # Construct a valid IfBlock
    condition = Reference(DataSymbol('condition', BOOLEAN_TYPE))
    ifblock = IfBlock.create(condition, [], [])

    # Attempt to put all of the children of the IfBlock into a region. This
    # is an error because the first child is the conditional part of the
    # IfBlock.
    with pytest.raises(TransformationError) as err:
        super(ACCParallelTrans, acct).validate([ifblock.children[0]])
    assert ("transformation to the immediate children of a Loop/IfBlock "
            "unless it is to a single Schedule" in str(err.value))
    with pytest.raises(TransformationError) as err:
        super(ACCParallelTrans, acct).validate(ifblock.children[1:])
    assert (" to multiple nodes when one or more is a Schedule. "
            "Either target a single Schedule or " in str(err.value))


def test_regiontrans_wrong_children():
    ''' Check that the validate method raises the expected error if
        passed the wrong children of a Node. (e.g. those representing the
        bounds of a Loop.) '''
    # RegionTrans is abstract so use a concrete sub-class
    rtrans = ACCParallelTrans()
    # Construct a valid Loop in the PSyIR
    parent = Loop()
    parent.addchild(Literal("1", INTEGER_TYPE))
    parent.addchild(Literal("10", INTEGER_TYPE))
    parent.addchild(Literal("1", INTEGER_TYPE))
    parent.addchild(Schedule())
    with pytest.raises(TransformationError) as err:
        RegionTrans.validate(rtrans, parent.children)
    assert ("Cannot apply a transformation to multiple nodes when one or more "
            "is a Schedule" in str(err.value))


def test_parallelregion_refuse_codeblock():
    ''' Check that ParallelRegionTrans.validate() rejects a loop nest that
    encloses a CodeBlock. We use OMPParallelTrans as ParallelRegionTrans
    is abstract. '''
    otrans = OMPParallelTrans()
    # Construct a valid Loop in the PSyIR with a CodeBlock in its body
    parent = Loop.create(DataSymbol("ji", INTEGER_TYPE),
                         Literal("1", INTEGER_TYPE),
                         Literal("10", INTEGER_TYPE),
                         Literal("1", INTEGER_TYPE),
                         [CodeBlock([], CodeBlock.Structure.STATEMENT,
                                    None)])
    with pytest.raises(TransformationError) as err:
        otrans.validate([parent])
    assert ("Nodes of type 'CodeBlock' cannot be enclosed by a "
            "OMPParallelTrans transformation" in str(err.value))


def test_parallellooptrans_refuse_codeblock():
    ''' Check that ParallelLoopTrans.validate() rejects a loop nest that
    encloses a CodeBlock. We have to use OMPParallelLoopTrans as
    ParallelLoopTrans is abstract. '''
    otrans = OMPParallelLoopTrans()
    # Construct a valid Loop in the PSyIR with a CodeBlock in its body
    parent = Loop.create(DataSymbol("ji", INTEGER_TYPE),
                         Literal("1", INTEGER_TYPE),
                         Literal("10", INTEGER_TYPE),
                         Literal("1", INTEGER_TYPE),
                         [CodeBlock([], CodeBlock.Structure.STATEMENT,
                                    None)])
    with pytest.raises(TransformationError) as err:
        otrans.validate(parent)
    assert ("Nodes of type 'CodeBlock' cannot be enclosed "
            "by a OMPParallelLoopTrans transformation" in str(err.value))


# Tests for OMPSingleTrans
def test_ompsingle():
    ''' Generic tests for the OMPSingleTrans transformation class '''
    trans = OMPSingleTrans()
    assert trans.name == "OMPSingleTrans"
    assert str(trans) == "Insert an OpenMP Single region"

    assert trans.omp_nowait is False
    trans.omp_nowait = True
    assert trans.omp_nowait is True


def test_ompsingle_invalid_nowait():
    ''' Tests to check OMPSingle rejects invalid attempts
        to pass nowait argument '''
    trans = OMPSingleTrans()
    with pytest.raises(TypeError) as err:
        trans.omp_nowait = "string"
    assert ("Expected nowait to be a bool but got a str"
            in str(err.value))


def test_ompsingle_nested():
    ''' Tests to check OMPSingle rejects being applied to another OMPSingle '''
    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH, "single_invoke.f90"),
                           api="gocean1.0")
    single = OMPSingleTrans()
    psy = PSyFactory("gocean1.0", distributed_memory=False).\
        create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule

    single.apply(schedule[0])
    with pytest.raises(TransformationError) as err:
        single.apply(schedule[0])
    assert ("Transformation Error: Nodes of type 'OMPSingleDirective' cannot"
            " be enclosed by a OMPSingleTrans transformation"
            in str(err.value))


# Tests for OMPMasterTrans
def test_ompmaster():
    ''' Generic tests for the OMPMasterTrans transformation class '''
    trans = OMPMasterTrans()
    assert trans.name == "OMPMasterTrans"
    assert str(trans) == "Insert an OpenMP Master region"


def test_ompmaster_nested():
    '''Tests to check OMPMasterTrans rejects being applied to another
    OMPMasterTrans'''

    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH, "single_invoke.f90"),
                           api="gocean1.0")
    master = OMPMasterTrans()
    psy = PSyFactory("gocean1.0", distributed_memory=False).\
        create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule

    # Successful transformation test
    node = schedule[0]
    master.apply(node)
    assert isinstance(schedule[0], OMPMasterDirective)
    assert schedule[0].dir_body[0] is node
    with pytest.raises(TransformationError) as err:
        master.apply(schedule[0])
    assert ("Transformation Error: Nodes of type 'OMPMasterDirective' cannot"
            " be enclosed by a OMPMasterTrans transformation"
            in str(err.value))


# Tests for ProfileTrans


@pytest.mark.parametrize("options", [None, {"invalid": "invalid"},
                                     {"region_name": ("mod", "reg")}])
def test_profile_trans_name(options):
    '''Check that providing no option or an option not associated with the
    profile transformation does not result in anything being passed
    into ProfileNode via the name argument and that providing an
    option associated with the profile transformation does result in
    the relevant names being passed into ProfileNode via the name
    argument. This is checked by looking at the variables
    '_module_name' and '_region_name' which are set to the name
    argument values if they are provided, otherwise the variables are
    set to None.

    '''
    _, invoke = get_invoke("1_single_invoke.f90", "dynamo0.3", idx=0)
    schedule = invoke.schedule
    profile_trans = ProfileTrans()
    if options:
        profile_trans.apply(schedule.children, options=options)
    else:
        profile_trans.apply(schedule.children)
    profile_node = schedule[0]
    if options and "region_name" in options:
        assert profile_node._module_name == "mod"
        assert profile_node._region_name == "reg"
    else:
        assert profile_node._module_name is None
        assert profile_node._region_name is None


@pytest.mark.parametrize("value", [None, ["a", "b"], (), ("a",),
                                   ("a", "b", "c"), ("a", []), ([], "a")])
def test_profile_trans_invalid_name(value):
    '''Invalid name supplied to options argument.'''
    profile_trans = ProfileTrans()

    # We need to have a schedule as parent, otherwise the node
    # (with no parent) will not be allowed.
    sched = Schedule()
    node = Statement(parent=sched)
    sched.addchild(node)
    with pytest.raises(TransformationError) as excinfo:
        profile_trans.apply(node, options={"region_name": value})
    assert ("User-supplied region name must be a tuple containing "
            "two non-empty strings." in str(excinfo.value))
