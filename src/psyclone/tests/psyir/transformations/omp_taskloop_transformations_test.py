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
# ----------------------------------------------------------------------------
# Authors: A. B. G. Chalk, N. Nobre and S. Siso, STFC Daresbury Lab
'''
API-agnostic tests for OpenMP task transformation classes.
'''
from __future__ import absolute_import, print_function
import os
import pytest

from psyclone.errors import InternalError
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
from psyclone.psyir.nodes import Loop, Node, OMPTaskwaitDirective, \
    OMPTaskloopDirective, OMPParallelDirective, \
    OMPDoDirective, OMPSingleDirective
from psyclone.psyir.transformations import TransformationError
from psyclone.transformations import OMPLoopTrans, OMPParallelTrans, \
    OMPSingleTrans, OMPMasterTrans, OMPTaskloopTrans, MoveTrans
from psyclone.psyir.transformations import OMPTaskwaitTrans

GOCEAN_BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                os.pardir, os.pardir, "test_files",
                                "gocean1p0")


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
        "    !$omp parallel default(shared), private(i,j)\n" +
        "      !$omp master\n" +
        f"      !$omp taskloop{clauses}\n" +
        "      DO" in code)
    assert (
        "      END DO\n" +
        "      !$omp end taskloop\n" +
        "      !$omp end master\n" +
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


def test_omptaskwait_trans_str():
    '''Test the __str__ method of the OMPTaskwaitTrans'''
    trans = OMPTaskwaitTrans()
    assert str(trans) == ("Adds 'OpenMP TASKWAIT' directives to an OpenMP"
                          " parallel region to satisfy 'OpenMP TASKLOOP' "
                          "dependencies")


def test_omptaskwait_validate_non_parallel():
    '''Test the validate method of the OMPTaskwaitTrans fails when supplied
    a non-OMPParallelDirective node'''
    loop = Loop()
    trans = OMPTaskwaitTrans()
    with pytest.raises(TransformationError) as excinfo:
        trans.validate(loop)
    assert ("OMPTaskwaitTrans was supplied a 'Loop' node, but expected an "
            "OMPParallelDirective") in str(excinfo.value)


def test_omptaskwait_validate_no_taskloop(fortran_reader):
    '''Test the validate method of the OMPTaskwaitTrans fails when supplied
    a parallel region containing no taskloops'''
    code = '''
    subroutine sub()
        integer :: ji, jj, n
        integer, dimension(10, 10) :: t
        integer, dimension(10, 10) :: s
        do jj = 1, n
            do ji = 1, 10
                s(ji, jj) = t(ji, jj)
            end do
        end do
    end subroutine sub
    '''
    psyir = fortran_reader.psyir_from_source(code)
    trans = OMPParallelTrans()
    # Apply parallel trans to the loop
    trans.apply(psyir.children[0].children[0])
    ttrans = OMPTaskwaitTrans()
    with pytest.raises(TransformationError) as excinfo:
        ttrans.validate(psyir.children[0].children[0])
    assert ("OMPTaskwaitTrans was supplied an OMPParallelDirective "
            "that does not contain any OMPTaskloopDirectives")\
        in str(excinfo.value)
    # Check this error can be disabled correctly
    ttrans.validate(psyir.children[0].children[0],
                    options={"fail_on_no_taskloop": False})


def test_omptaskwait_validate_multiple_parallel_regions():
    '''Test the validate method of the OMPTaskwaitTrans succeeds when
    supplied a parallel region containing dependent taskloops in different
    parallel regions
    '''

    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH,
                                        "single_invoke_two_"
                                        "identical_kernels.f90"),
                           api="gocean1.0")
    psy = PSyFactory("gocean1.0", distributed_memory=False).\
        create(invoke_info)
    trans = OMPParallelTrans()
    tloop = OMPTaskloopTrans(nogroup=True)
    sing = OMPSingleTrans(nowait=True)
    ttrans = OMPTaskwaitTrans()
    schedule1 = psy.invokes.invoke_list[0].schedule
    # Construct two parallel regions, each containing
    # one loop parallelised with taskloop. These loops
    # operate on the same data, so we check the dependency
    # is not tracked across the parallel regions
    tloop.apply(schedule1[0])
    tloop.apply(schedule1[1])
    sing.apply(schedule1[0])
    sing.apply(schedule1[1])
    trans.apply(schedule1[0])
    trans.apply(schedule1[1])
    # This should be ok
    ttrans.validate(schedule1[0])
    ttrans.validate(schedule1[1])


def test_omptaskwait_validate_barrierless_single_region():
    '''Test the validate method of the OMPTaskwaitTrans throws an
    error when supplied a dependency across barrierless single regions.
    '''
    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH,
                                        "single_invoke_two_"
                                        "identical_kernels.f90"),
                           api="gocean1.0")
    psy = PSyFactory("gocean1.0", distributed_memory=False).\
        create(invoke_info)
    trans = OMPParallelTrans()
    tloop = OMPTaskloopTrans(nogroup=True)
    sing = OMPSingleTrans(nowait=True)
    ttrans = OMPTaskwaitTrans()
    schedule1 = psy.invokes.invoke_list[0].schedule
    # Construct an example where we have a single parallel region
    # containing 2 single regions with nowait, each containing a
    # loop parallelised with taskloop. Since the single regions
    # have been created with nowait, there is no way to satisfy
    # the dependency between the two taskloops
    tloop.apply(schedule1[0])
    child0 = schedule1[0]
    tloop.apply(schedule1[1])
    child1 = schedule1[1]
    sing.apply(schedule1[0])
    sing.apply(schedule1[1])
    trans.apply(schedule1)
    # Dependency should still exist
    assert (OMPTaskwaitTrans.get_forward_dependence(
            child0, schedule1[0]) is child1)
    correct = "Couldn't satisfy the dependencies due to taskloop dependencies "
    correct = correct + "across barrierless OMP serial regions. Dependency is "
    correct = correct + '''from
!$omp taskloop nogroup
do j = cu_fld%internal%ystart, cu_fld%internal%ystop, 1
  do i = cu_fld%internal%xstart, cu_fld%internal%xstop, 1
    < kern call: compute_cu_code >
  enddo
enddo
!$omp end taskloop
to
!$omp taskloop nogroup
do j = p_fld%internal%ystart, p_fld%internal%ystop, 1
  do i = p_fld%internal%xstart, p_fld%internal%xstop, 1
    < kern call: compute_cu_code >
  enddo
enddo
!$omp end taskloop'''
    # This should raise an exception
    with pytest.raises(TransformationError) as excinfo:
        ttrans.validate(schedule1[0])
    assert correct in str(excinfo.value)


def test_omptaskwait_validate_master_region():
    '''Test the validate method of the OMPTaskwaitTrans throws an
    error when supplied a dependency across master regions
    '''
    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH,
                                        "single_invoke_two_"
                                        "identical_kernels.f90"),
                           api="gocean1.0")
    psy = PSyFactory("gocean1.0", distributed_memory=False).\
        create(invoke_info)
    trans = OMPParallelTrans()
    tloop = OMPTaskloopTrans(nogroup=True)
    sing = OMPMasterTrans()
    ttrans = OMPTaskwaitTrans()
    schedule1 = psy.invokes.invoke_list[0].schedule
    # Construct an example where we have a single parallel region
    # containing 2 master regions, each containing a
    # loop parallelised with taskloop. Since the master regions
    # have no barrier associated, there is no way to satisfy
    # the dependency between the two taskloops
    tloop.apply(schedule1[0])
    child0 = schedule1[0]
    tloop.apply(schedule1[1])
    child1 = schedule1[1]
    sing.apply(schedule1[0])
    sing.apply(schedule1[1])
    trans.apply(schedule1)
    # Dependency should still exist
    assert (OMPTaskwaitTrans.get_forward_dependence(
            child0, schedule1[0]) is child1)
    # This should be raise an error
    correct = "Couldn't satisfy the dependencies due to taskloop dependencies "
    correct = correct + "across barrierless OMP serial regions. Dependency is "
    correct = correct + '''from
!$omp taskloop nogroup
do j = cu_fld%internal%ystart, cu_fld%internal%ystop, 1
  do i = cu_fld%internal%xstart, cu_fld%internal%xstop, 1
    < kern call: compute_cu_code >
  enddo
enddo
!$omp end taskloop
to
!$omp taskloop nogroup
do j = p_fld%internal%ystart, p_fld%internal%ystop, 1
  do i = p_fld%internal%xstart, p_fld%internal%xstop, 1
    < kern call: compute_cu_code >
  enddo
enddo
!$omp end taskloop'''
    with pytest.raises(TransformationError) as excinfo:
        ttrans.validate(schedule1[0])
    assert correct in str(excinfo.value)


def test_omptaskwait_getforwarddep_invalid_type():
    '''Test the get_forward_dependence method of the OMPTaskwaitTrans
    returns an Error when the root node is not a OMPParallelDirective
    '''
    loop = Loop()
    with pytest.raises(TransformationError) as excinfo:
        OMPTaskwaitTrans.get_forward_dependence(None, loop)
    assert ("Expected the root of the tree in which to look for a forward "
            "dependence to be an instance of OMPParallelDirective,"
            " but was supplied an instance of 'Loop'") in str(excinfo.value)


def test_omptaskwait_getforwarddep_required_parent(monkeypatch):
    '''Test the get_forward_dependence method of the OMPTaskwaitTrans
    returns an Error when there is no parent OMPParallelDirective
    '''
    pdir = OMPParallelDirective()

    def validate_global_constraints(self):
        # pylint: disable=unused-argument
        pass
    monkeypatch.setattr(OMPTaskloopDirective, "validate_global_constraints",
                        validate_global_constraints)
    tdir = OMPTaskloopDirective()
    with pytest.raises(InternalError) as excinfo:
        OMPTaskwaitTrans.get_forward_dependence(tdir, pdir)
    assert ("No parent parallel directive was found for the taskloop region"
            ":") in str(excinfo.value)


def test_omptaskwait_get_forward_dependence1():
    '''Test the get_forward_dependence method of the OMPTaskwaitTrans.
    This test checks dependence from OMPTaskloopDirective to
    Loop'''
    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH,
                                        "single_invoke_two_"
                                        "identical_kernels.f90"),
                           api="gocean1.0")
    psy = PSyFactory("gocean1.0", distributed_memory=False).\
        create(invoke_info)
    trans = OMPParallelTrans()
    tloop = OMPTaskloopTrans(nogroup=True)
    sing = OMPSingleTrans()
    # Construct a schedule with two dependent loops inside
    # a single OpenMP region. The forward dependency from the
    # two loops should still be found after transforming the
    # first into an OpenMP task loop.
    schedule1 = psy.invokes.invoke_list[0].schedule
    tloop.apply(schedule1[0])
    child0 = schedule1[0]
    child1 = schedule1[1]
    sing.apply(schedule1)
    trans.apply(schedule1)

    assert isinstance(child0, OMPTaskloopDirective)
    assert isinstance(child1, Loop)
    assert (OMPTaskwaitTrans.get_forward_dependence(
            child0, schedule1[0]) is child1)


def test_omptaskwait_get_forward_dependence2():
    '''Test the get_forward_dependence method of the OMPTaskwaitTrans.
    This test checks dependence from OMPTaskloopDirective to
    OMPTaskloopDirective'''
    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH,
                                        "single_invoke_two_"
                                        "identical_kernels.f90"),
                           api="gocean1.0")
    psy = PSyFactory("gocean1.0", distributed_memory=False).\
        create(invoke_info)
    trans = OMPParallelTrans()
    tloop = OMPTaskloopTrans(nogroup=True)
    sing = OMPSingleTrans()
    # Construct a schedule with two dependent loops inside
    # a single OpenMP region. The forward dependency from the
    # two loops should still be found after transforming
    # both into OpenMP task loops.
    schedule1 = psy.invokes.invoke_list[0].schedule
    tloop.apply(schedule1[0])
    child0 = schedule1[0]
    tloop.apply(schedule1[1])
    child1 = schedule1[1]
    sing.apply(schedule1)
    trans.apply(schedule1)

    assert isinstance(child0, OMPTaskloopDirective)
    assert isinstance(child1, OMPTaskloopDirective)
    assert (OMPTaskwaitTrans.get_forward_dependence(
            child0, schedule1[0]) is child1)


def test_omptaskwait_get_forward_dependence3():
    '''Test the get_forward_dependence method of the OMPTaskwaitTrans.
    This test checks dependence from OMPTaskloopDirective to
    OMPDoDirective'''
    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH,
                                        "single_invoke_two_"
                                        "identical_kernels.f90"),
                           api="gocean1.0")
    psy = PSyFactory("gocean1.0", distributed_memory=False).\
        create(invoke_info)
    trans = OMPParallelTrans()
    tloop = OMPTaskloopTrans(nogroup=True)
    oloop = OMPLoopTrans()
    sing = OMPSingleTrans()
    schedule1 = psy.invokes.invoke_list[0].schedule
    # Construct a schedule with two dependent loops inside
    # a single OpenMP region. The forward dependency from the
    # two loops should still be found after transforming
    # the first into an OpenMP task loops and the second into
    # an OpenMP do loop.
    tloop.apply(schedule1[0])
    child0 = schedule1[0]
    oloop.apply(schedule1[1])
    child1 = schedule1[1]
    sing.apply(schedule1)
    trans.apply(schedule1)

    assert isinstance(child0, OMPTaskloopDirective)
    assert isinstance(child1, OMPDoDirective)
    assert (OMPTaskwaitTrans.get_forward_dependence(
            child0, schedule1[0]) is child1)


def test_omptaskwait_get_forward_dependence4():
    '''Test the get_forward_dependence method of the OMPTaskwaitTrans.
    This test checks dependence from an OMPTaskloopDirective
    to its parent OMPSingleDirective'''
    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH,
                                        "single_invoke_two_"
                                        "identical_kernels.f90"),
                           api="gocean1.0")
    psy = PSyFactory("gocean1.0", distributed_memory=False).\
        create(invoke_info)
    trans = OMPParallelTrans()
    tloop = OMPTaskloopTrans(nogroup=True)
    sing = OMPSingleTrans()
    # Construct a schedule with two dependent loops inside
    # two different OpenMP single regions. The forward dependency
    # from the two loops should be moved to the first node's parent
    # OMPSingleDirective after transforming both loops into
    # OpenMP task loops.
    schedule1 = psy.invokes.invoke_list[0].schedule
    tloop.apply(schedule1[0])
    child0 = schedule1[0]
    tloop.apply(schedule1[1])
    sing.apply(schedule1[0])
    child1 = schedule1[0]
    sing.apply(schedule1[1])
    trans.apply(schedule1)

    assert isinstance(child0, OMPTaskloopDirective)
    assert isinstance(child1, OMPSingleDirective)
    assert (OMPTaskwaitTrans.get_forward_dependence(
            child0, schedule1[0]) is child1)


def test_omptaskwait_get_forward_dependence5():
    '''Test the get_forward_dependence method of the OMPTaskwaitTrans.
    This test checks dependence from OMPTaskloopDirective to None'''
    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH,
                                        "single_invoke_two_"
                                        "identical_kernels.f90"),
                           api="gocean1.0")
    psy = PSyFactory("gocean1.0", distributed_memory=False).\
        create(invoke_info)
    trans = OMPParallelTrans()
    tloop = OMPTaskloopTrans(nogroup=True)
    sing = OMPSingleTrans(nowait=True)
    # Construct a schedule with two dependent loops inside
    # a single OpenMP region. This code checks that no
    # dependency is returned for the second task loop
    # as it has no following loops
    schedule1 = psy.invokes.invoke_list[0].schedule
    tloop.apply(schedule1[0])
    tloop.apply(schedule1[1])
    child0 = schedule1[1]
    sing.apply(schedule1)
    trans.apply(schedule1)

    assert isinstance(child0, OMPTaskloopDirective)
    assert (OMPTaskwaitTrans.get_forward_dependence(
            child0, schedule1[0]) is None)


def test_omptaskwait_get_forward_dependence6():
    '''Test the get_forward_dependence method of the OMPTaskwaitTrans.
    This test checks dependence from OMPTaskloopDirective to
    OMPTaskwaitDirective'''
    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH,
                                        "single_invoke_two_"
                                        "identical_kernels.f90"),
                           api="gocean1.0")
    psy = PSyFactory("gocean1.0", distributed_memory=False).\
        create(invoke_info)
    trans = OMPParallelTrans()
    tloop = OMPTaskloopTrans(nogroup=True)
    sing = OMPSingleTrans()
    # Construct a schedule with two dependent loops inside
    # a single OpenMP region, with an OMPTaskwaitDirective
    # between them.
    schedule1 = psy.invokes.invoke_list[0].schedule
    tloop.apply(schedule1[0])
    tloop.apply(schedule1[1])
    schedule1.addchild(OMPTaskwaitDirective(), 1)
    child0 = schedule1[0]
    child1 = schedule1[1]
    sing.apply(schedule1)
    trans.apply(schedule1)

    assert isinstance(child0, OMPTaskloopDirective)
    assert isinstance(child1, OMPTaskwaitDirective)
    assert (OMPTaskwaitTrans.get_forward_dependence(
            child0, schedule1[0]) is child1)


def test_omptaskwait_apply_simple():
    '''Test the apply method of the OMPTaskwaitTrans works for
    a simple example.
    '''
    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH,
                                        "single_invoke_two_"
                                        "identical_kernels.f90"),
                           api="gocean1.0")
    psy = PSyFactory("gocean1.0", distributed_memory=False).\
        create(invoke_info)
    trans = OMPParallelTrans()
    tloop = OMPTaskloopTrans(nogroup=True)
    sing = OMPSingleTrans()
    ttrans = OMPTaskwaitTrans()
    schedule1 = psy.invokes.invoke_list[0].schedule
    # Apply taskloop transformations to the two loops, and
    # then apply OMPSingleTrans, OMPParallelTrans and
    # OMPTaskwaitTrans to the schedule. Check the apply
    # routine adds one OMPTaskwait directive as child 1
    # of the OMPSingleDirective
    tloop.apply(schedule1[0])
    tloop.apply(schedule1[1])
    sing.apply(schedule1)
    the_sing = schedule1[0]
    trans.apply(schedule1)
    ttrans.apply(schedule1[0])

    assert len(schedule1.walk(OMPTaskwaitDirective)) == 1
    assert (schedule1.walk(OMPTaskwaitDirective)[0] is
            the_sing.dir_body[1])


def test_omptaskwait_apply_multidepend():
    '''Test the apply method of the OMPTaskwaitTrans works for
    two dependency setup. This test uses an ABBA dependence setup,
    i.e. The fourth loop depends on the first, and the third loop
    depends on the second.
    '''
    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH,
                                        "multi_dependent_invoke.f90"),
                           api="gocean1.0")
    psy = PSyFactory("gocean1.0", distributed_memory=False).\
        create(invoke_info)
    trans = OMPParallelTrans()
    tloop = OMPTaskloopTrans(nogroup=True)
    sing = OMPSingleTrans()
    ttrans = OMPTaskwaitTrans()
    # Apply Taskloop transformations to each of the 4 loops
    # in the schedule, and then apply OMPSingleTrans, OMPParallelTrans and
    # OMPTaskwaitTrans to the schedule. Check the apply
    # routine adds one OMPTaskwait directive as child 2
    # of the OMPSingleDirective
    schedule1 = psy.invokes.invoke_list[0].schedule
    tloop.apply(schedule1[0])
    tloop.apply(schedule1[1])
    tloop.apply(schedule1[2])
    tloop.apply(schedule1[3])
    sing.apply(schedule1)
    the_sing = schedule1[0]
    trans.apply(schedule1)
    ttrans.apply(schedule1[0])

    assert len(schedule1.walk(OMPTaskwaitDirective)) == 1
    assert (schedule1.walk(OMPTaskwaitDirective)[0] is
            the_sing.dir_body[2])


def test_omptaskwait_apply_multidepend2():
    '''Test the apply method of the OMPTaskwaitTrans works for
    two dependency setup. This test uses an AABB dependence setup,
    i.e. The fourth loop depends on the third, and the second loop
    depends on the first.
    '''
    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH,
                                        "multi_dependent_invoke.f90"),
                           api="gocean1.0")
    psy = PSyFactory("gocean1.0", distributed_memory=False).\
        create(invoke_info)
    trans = OMPParallelTrans()
    tloop = OMPTaskloopTrans(nogroup=True)
    sing = OMPSingleTrans()
    ttrans = OMPTaskwaitTrans()
    move = MoveTrans()
    # Reorder the loops to setup an AABB dependence setup.
    # Apply Taskloop transformations to each of the 4 loops
    # in the schedule, and then apply OMPSingleTrans, OMPParallelTrans and
    # OMPTaskwaitTrans to the schedule. Check the apply
    # routine adds two OMPTaskwait directive as children 1 and 4
    # of the OMPSingleDirective
    schedule1 = psy.invokes.invoke_list[0].schedule
    move.apply(schedule1[3], schedule1[0],
               {"position": "after"})
    tloop.apply(schedule1[0])
    tloop.apply(schedule1[1])
    tloop.apply(schedule1[2])
    tloop.apply(schedule1[3])
    sing.apply(schedule1)
    the_sing = schedule1[0]
    trans.apply(schedule1)
    ttrans.apply(schedule1[0])

    assert len(schedule1.walk(OMPTaskwaitDirective)) == 2
    assert (schedule1.walk(OMPTaskwaitDirective)[0] is
            the_sing.dir_body[1])
    assert (schedule1.walk(OMPTaskwaitDirective)[1] is
            the_sing.dir_body[4])


def test_omptaskwait_apply_multidepend3():
    '''Test the apply method of the OMPTaskwaitTrans works for
    two dependency setup. This test uses an ABAB dependence setup,
    i.e. The fourth loop depends on the second, and the third loop
    depends on the first.
    '''
    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH,
                                        "multi_dependent_invoke.f90"),
                           api="gocean1.0")
    psy = PSyFactory("gocean1.0", distributed_memory=False).\
        create(invoke_info)
    trans = OMPParallelTrans()
    tloop = OMPTaskloopTrans(nogroup=True)
    sing = OMPSingleTrans()
    ttrans = OMPTaskwaitTrans()
    move = MoveTrans()
    # Reorder the loops to setup an ABAB dependence setup.
    # Apply Taskloop transformations to each of the 4 loops
    # in the schedule, and then apply OMPSingleTrans, OMPParallelTrans and
    # OMPTaskwaitTrans to the schedule. Check the apply
    # routine adds two OMPTaskwait directive as children 1 and 4
    # of the OMPSingleDirective
    schedule1 = psy.invokes.invoke_list[0].schedule
    move.apply(schedule1[3], schedule1[1],
               {"position": "after"})
    tloop.apply(schedule1[0])
    tloop.apply(schedule1[1])
    tloop.apply(schedule1[2])
    tloop.apply(schedule1[3])
    sing.apply(schedule1)
    the_sing = schedule1[0]
    trans.apply(schedule1)
    ttrans.apply(schedule1[0])

    assert len(schedule1.walk(OMPTaskwaitDirective)) == 1
    assert (schedule1.walk(OMPTaskwaitDirective)[0] is
            the_sing.dir_body[2])


def test_omptaskwait_apply_multiloops():
    '''Test the apply method of the OMPTaskwaitTrans works for
    a system with both OMPTaskwaitDirective and OMPDoDirective nodes.
    This test uses an ABBA dependence setup, i.e. The fourth loop depends
    on the first, and the third loop depends on the second.
    '''
    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH,
                                        "multi_dependent_invoke.f90"),
                           api="gocean1.0")
    psy = PSyFactory("gocean1.0", distributed_memory=False).\
        create(invoke_info)
    trans = OMPParallelTrans()
    tloop = OMPTaskloopTrans(nogroup=True)
    oloop = OMPLoopTrans()
    sing = OMPSingleTrans()
    ttrans = OMPTaskwaitTrans()
    # Apply the Taskloop transformation to the first loop, apply the
    # OMPLoop transformation to second, third and fourth loops
    # in the schedule, and then apply OMPSingleTrans, OMPParallelTrans and
    # OMPTaskwaitTrans to the schedule. Check the apply
    # routine adds one OMPTaskwait directive as child 3
    # of the OMPSingleDirective
    schedule1 = psy.invokes.invoke_list[0].schedule
    tloop.apply(schedule1[0])
    oloop.apply(schedule1[1])
    oloop.apply(schedule1[2])
    oloop.apply(schedule1[3])
    sing.apply(schedule1)
    the_sing = schedule1[0]
    trans.apply(schedule1)
    ttrans.apply(schedule1[0])

    assert len(schedule1.walk(OMPTaskwaitDirective)) == 1
    assert (schedule1.walk(OMPTaskwaitDirective)[0] is
            the_sing.dir_body[3])


def test_omptaskwait_apply_multiregion():
    '''Test the apply method of the OMPTaskwaitTrans works for
    a system with dependencies only over the single boundary. In
    this case the single region should end with a taskwait.
    '''
    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH,
                                        "multi_dependent_invoke.f90"),
                           api="gocean1.0")
    psy = PSyFactory("gocean1.0", distributed_memory=False).\
        create(invoke_info)
    trans = OMPParallelTrans()
    tloop = OMPTaskloopTrans(nogroup=True)
    oloop = OMPLoopTrans()
    sing = OMPSingleTrans()
    ttrans = OMPTaskwaitTrans()
    # Apply a taskloop and single transformation to the first loop,
    # apply OMPLoop transformations to the other 3 loops, and finally
    # apply a parallel transformation and taskwait transformation.
    # Check the OMPTaskwaitTrans creates one OMPTaskwaitDirective and
    # the final child of the single region
    schedule1 = psy.invokes.invoke_list[0].schedule
    tloop.apply(schedule1[0])
    oloop.apply(schedule1[1])
    oloop.apply(schedule1[2])
    oloop.apply(schedule1[3])
    sing.apply(schedule1[0])
    the_sing = schedule1[0]
    sing.apply(schedule1[1:])
    trans.apply(schedule1)
    ttrans.apply(schedule1[0])
    assert len(schedule1.walk(OMPTaskwaitDirective)) == 1
    assert isinstance(the_sing.dir_body[-1], OMPTaskwaitDirective)


def test_omptaskwait_apply_multiregion2():
    '''Test the apply method of the OMPTaskwaitTrans works for
    a system with dependencies inside and over the single boundary.
    This test uses an ABBA dependence setup, i.e. The fourth loop depends
    on the first, and the third loop depends on the second.
    In this case the first single region should not end with a taskwait
    as the prior taskwait should satisfy the dependency.
    '''
    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH,
                                        "multi_dependent_invoke.f90"),
                           api="gocean1.0")
    psy = PSyFactory("gocean1.0", distributed_memory=False).\
        create(invoke_info)
    trans = OMPParallelTrans()
    tloop = OMPTaskloopTrans(nogroup=True)
    sing = OMPSingleTrans()
    ttrans = OMPTaskwaitTrans()
    # Applies a taskloop transformation to every loop in the schedule.
    # Apply a single transformation to the first 3 loops, and the last loop.
    # Finally apply a parallel and taskwait transformation to the schedule.
    # The resulting schedule should contain one OMPTaskwaitDirective, which
    # should be the second child of the first single region.
    schedule1 = psy.invokes.invoke_list[0].schedule
    tloop.apply(schedule1[0])
    tloop.apply(schedule1[1])
    tloop.apply(schedule1[2])
    tloop.apply(schedule1[3])
    sing.apply(schedule1[0:3])
    the_sing = schedule1[0]
    sing.apply(schedule1[1])
    trans.apply(schedule1)
    ttrans.apply(schedule1[0])
    assert len(schedule1.walk(OMPTaskwaitDirective)) == 1
    assert isinstance(the_sing.dir_body[2], OMPTaskwaitDirective)
    assert not isinstance(the_sing.dir_body[-1], OMPTaskwaitDirective)


def test_omptaskwait_ignore_nogroup_clause():
    '''Test the apply method of the OMPTaskwaitTrans ignores
    OMPTaskloop nodes with no nogroup clause
    This test uses an ABBA dependence setup, i.e. The fourth loop depends
    on the first, and the third loop depends on the second.
    '''
    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH,
                                        "multi_dependent_invoke.f90"),
                           api="gocean1.0")
    psy = PSyFactory("gocean1.0", distributed_memory=False).\
        create(invoke_info)
    trans = OMPParallelTrans()
    tloop = OMPTaskloopTrans(nogroup=True)
    tloop2 = OMPTaskloopTrans(nogroup=False)
    sing = OMPSingleTrans()
    ttrans = OMPTaskwaitTrans()
    # Apply taskloop transformations to each loop in the schedule.
    # The first and fourth loops will have a nogroup clause.
    # Apply a single, parallel and taskwait transformation to the
    # schedule. The resulting schedule should contain one
    # OMPTaskwaitDirective and it should appear just before the final
    # OMPTaskloopDirective
    schedule1 = psy.invokes.invoke_list[0].schedule
    tloop.apply(schedule1[0])
    tloop2.apply(schedule1[1])
    tloop2.apply(schedule1[2])
    tloop.apply(schedule1[3])
    sing.apply(schedule1)
    the_sing = schedule1[0]
    trans.apply(schedule1)
    ttrans.apply(schedule1[0])

    assert len(schedule1.walk(OMPTaskwaitDirective)) == 1
    assert (schedule1.walk(OMPTaskwaitDirective)[0] is
            the_sing.dir_body[3])
