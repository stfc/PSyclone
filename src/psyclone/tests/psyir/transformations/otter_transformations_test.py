# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council.
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
# Authors A. B. G. Chalk, STFC Daresbury Lab
# -----------------------------------------------------------------------------

import os
import pytest

from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
from psyclone.psyir.transformations import OtterParallelTrans, \
        OtterTaskloopTrans, OtterTraceSetupTrans, OtterTaskSingleTrans, \
        OtterLoopTrans, OtterSynchroniseChildrenTrans, \
        OtterSynchroniseDescendantsTrans, OtterTraceStartEndTrans

GOCEAN_BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                os.pardir, os.pardir, "test_files",
                                "gocean1p0")

def test_ottersetuptrace_trans_str():
    tracetrans = OtterTraceSetupTrans()
    assert (str(tracetrans) == "Adds a Otter Trace setup node to a region of "
            "code to enable Otter instrumentation of PSyclone")

def test_ottersetuptrace_trans_apply():
    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH, "single_invoke.f90"),
                           api="gocean1.0")
    psy = PSyFactory("gocean1.0", distributed_memory=False).\
        create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    tracetrans = OtterTraceSetupTrans()
    tracetrans.apply(schedule.children[:])
    code = str(psy.gen)
    assert ("USE otter_serial, ONLY: fortran_otterTraceFinalise, "
            "fortran_otterTraceInitialise_i" in code)
    assert ("CALL fortran_otterTraceInitialise_i(__FILE__, 'invoke_0_compute_cu'"
            ", __LINE__)" in code)
    assert "CALL fortran_otterTraceFinalise" in code

def test_otterparallel_trans_str():
    paralleltrans = OtterParallelTrans()
    assert (str(paralleltrans) == "Adds a Otter Parallel node to a region of "
            "code")

def test_otterparallel_trans_apply():
    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH, "single_invoke.f90"),
                           api="gocean1.0")
    psy = PSyFactory("gocean1.0", distributed_memory=False).\
        create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    paralleltrans = OtterParallelTrans()
    paralleltrans.apply(schedule.children[:])
    code = str(psy.gen)
    assert ("USE otter_serial, ONLY: fortran_otterThreadsBegin_i, "
            "fortran_otterThreadsEnd" in code)
    assert ("CALL fortran_otterThreadsBegin_i(__FILE__, 'invoke_0_compute_cu'"
            ", __LINE__)" in code)
    assert "CALL fortran_otterThreadsEnd" in code

def test_ottertaskloop_trans_str():
    tlooptrans = OtterTaskloopTrans()
    assert (str(tlooptrans) == "Adds a set of Otter Tasking nodes to a Loop "
            "by chunking.")

def test_ottertaskloop_trans_apply():
    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH, "single_invoke.f90"),
                           api="gocean1.0")
    psy = PSyFactory("gocean1.0", distributed_memory=False).\
        create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    chunktrans = OtterTaskloopTrans()
    chunktrans.apply(schedule.children[0])
    code = str(psy.gen)
    assert ("USE otter_serial, ONLY: fortran_otterTaskBegin_i, "
            "fortran_otterTaskEnd" in code)
    correct = \
        '''DO j_out_var = cu_fld%internal%ystart, cu_fld%internal%ystop, 32
        j_el_inner = MIN(j_out_var + (32 - 1), cu_fld%internal%ystop)
        CALL fortran_otterTaskBegin_i(__FILE__, 'invoke_0_compute_cu', __LINE__)
        DO j = j_out_var, j_el_inner, 1
          DO i = cu_fld%internal%xstart, cu_fld%internal%xstop, 1
    '''
    assert correct in code
    correct = '''END DO
        END DO
        CALL fortran_otterTaskEnd
      END DO'''
    assert correct in code

def test_ottertasksingle_trans_str():
    strans = OtterTaskSingleTrans()
    assert (str(strans) == "Adds a Otter TaskSingle node to a region of code")

def test_ottertasksingle_trans_apply():
    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH, "single_invoke.f90"),
                           api="gocean1.0")
    psy = PSyFactory("gocean1.0", distributed_memory=False).\
        create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    paralleltrans = OtterTaskSingleTrans()
    paralleltrans.apply(schedule.children[:])
    code = str(psy.gen)
    assert ("USE otter_serial, ONLY: fortran_otterTaskSingleBegin_i, "
            "fortran_otterTaskSingleEnd" in code)
    assert ("CALL fortran_otterTaskSingleBegin_i(__FILE__, 'invoke_0_compute_cu'"
            ", __LINE__)" in code)
    assert "CALL fortran_otterTaskSingleEnd" in code


def test_otterloop_trans_str():
    looptrans = OtterLoopTrans()
    assert (str(looptrans) == "Adds an Otter Loop node to a Loop")

def test_otterloop_trans_apply():
    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH, "single_invoke.f90"),
                           api="gocean1.0")
    psy = PSyFactory("gocean1.0", distributed_memory=False).\
        create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    looptrans = OtterLoopTrans()
    looptrans.apply(schedule.children[0])
    code = str(psy.gen)
    assert ("USE otter_serial, ONLY: fortran_otterLoopBegin_i, "
            "fortran_otterLoopEnd, fortran_otterLoopIterationBegin_i, "
            "fortran_otterLoopIterationEnd" in code)
    correct = \
        '''CALL fortran_otterLoopBegin_i()
      DO j = cu_fld%internal%ystart, cu_fld%internal%ystop, 1
        CALL fortran_otterLoopIterationBegin_i()
'''
    assert correct in code

    correct = \
            '''END DO
        CALL fortran_otterLoopIterationEnd
      END DO
      CALL fortran_otterLoopEnd'''
    assert correct in code


def test_ottersyncchild_trans_str():
    synctrans = OtterSynchroniseChildrenTrans()
    assert (str(synctrans) == "Adds an Otter Synchronise Children "
            "Transformation after the supplied node.")


def test_ottersyncchild_trans_apply():
    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH, "single_invoke.f90"),
                           api="gocean1.0")
    psy = PSyFactory("gocean1.0", distributed_memory=False).\
        create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    synctrans = OtterSynchroniseChildrenTrans()
    synctrans.apply(schedule.children[0])
    code = str(psy.gen)
    correct = '''END DO
      CALL fortran_otterSynchroniseTasks_i(0)'''
    assert ("USE otter_serial, ONLY: fortran_otterSynchroniseTasks_i"
             in code)
    assert correct in code


def test_ottersyncdec_trans_str():
    synctrans = OtterSynchroniseDescendantsTrans()
    assert (str(synctrans) == "Adds an Otter Synchronise Descendents node "
            "around the supplied nodes.")


def test_ottersyncdec_trans_apply():
    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH, "single_invoke.f90"),
                           api="gocean1.0")
    psy = PSyFactory("gocean1.0", distributed_memory=False).\
        create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    synctrans = OtterSynchroniseDescendantsTrans()
    synctrans.apply(schedule.children[0])
    code = str(psy.gen)
    assert ("USE otter_serial, ONLY: "
            "fortran_otterSynchroniseDescendantTasksBegin_i, "
            "fortran_otterSynchroniseDescendantTasksEnd" in code)
    correct = \
        '''CALL fortran_otterSynchroniseDescendantTasksBegin_i()
      DO j = cu_fld%internal%ystart, cu_fld%internal%ystop, 1
'''
    assert correct in code

    correct = \
            '''END DO
      CALL fortran_otterSynchroniseDescendantTasksEnd'''
    assert correct in code


def test_ottertracestratend_trans_str():
    tracetrans = OtterTraceStartEndTrans()
    assert (str(tracetrans) == "Adds an Otter Trace Start and End node around "
            "the supplied nodes.")

def test_ottertracestartend_trans_apply():
    _, invoke_info = parse(os.path.join(GOCEAN_BASE_PATH, "single_invoke.f90"),
                           api="gocean1.0")
    psy = PSyFactory("gocean1.0", distributed_memory=False).\
        create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    tracetrans = OtterTraceStartEndTrans()
    tracetrans.apply(schedule.children[0])
    code = str(psy.gen)
    print(code)
    assert ("USE otter_serial, ONLY: fortran_otterTraceStart, "
            "fortran_otterTraceStop" in code)
    correct = \
        '''CALL fortran_otterTraceStart
      DO j = cu_fld%internal%ystart, cu_fld%internal%ystop, 1
'''
    assert correct in code

    correct = \
            '''END DO
      CALL fortran_otterTraceStop'''
    assert correct in code
