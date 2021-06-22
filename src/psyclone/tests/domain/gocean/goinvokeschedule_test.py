# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2021, Science and Technology Facilities Council.
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
# Authors: A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified work Copyright (c) 2018-2019 by J. Henrichs, Bureau of Meteorology
# Modified R. W. Ford, STFC Daresbury Lab
# Modified: I. Kavcic, Met Office

''' pytest tests for the GOInvokeSchedule class. '''

from __future__ import absolute_import, print_function
import os
import pytest
from psyclone.errors import GenerationError
from psyclone.gocean1p0 import GOInvokeSchedule
from psyclone.parse.algorithm import parse
from psyclone.psyir.nodes.node import colored
from psyclone.psyir.nodes import StructureReference, Member, Container, \
    StructureMember, Loop, Schedule, Literal, BinaryOperation
from psyclone.psyGen import PSyFactory, CodedKern, HaloExchange

API = "gocean1.0"
BASE_PATH = os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(
    os.path.abspath(__file__)))), "test_files", "gocean1p0")


def test_goschedule_view(capsys, dist_mem):
    ''' Test that the GOInvokeSchedule::view() method works as expected '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "single_invoke_two_kernels.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]

    # Ensure we check for the correct (colour) control codes in the output
    isched = colored("GOInvokeSchedule", GOInvokeSchedule._colour)
    loop = colored("Loop", Loop._colour)
    call = colored("CodedKern", CodedKern._colour)
    sched = colored("Schedule", Schedule._colour)
    lit = colored("Literal", Literal._colour)
    sref = colored("StructureReference", StructureReference._colour)
    smem = colored("StructureMember", StructureMember._colour)
    mem = colored("Member", Member._colour)
    bop = colored("BinaryOperation", BinaryOperation._colour)
    haloex = colored("HaloExchange", HaloExchange._colour)

    if dist_mem:
        # View without constant loop bounds and with distributed memory
        # where the p field has a stencil access.
        invoke.schedule.view()

        # The view method writes to stdout and this is captured by py.test
        # by default. We have to query this captured output.
        out, _ = capsys.readouterr()

        expected_output = (
            isched + "[invoke='invoke_0', Constant loop bounds=False]\n"
            "    0: " + haloex + "[field='p_fld', type='None', depth=None, "
            "check_dirty=False]\n"
            "    1: " + loop + "[type='outer', field_space='go_cu', "
            "it_space='go_internal_pts']\n"
            "        " + sref + "[name:'cu_fld']\n"
            "            " + smem + "[name:'internal']\n"
            "                " + mem + "[name:'ystart']\n"
            "        " + sref + "[name:'cu_fld']\n"
            "            " + smem + "[name:'internal']\n"
            "                " + mem + "[name:'ystop']\n"
            "        " + lit + "[value:'1', Scalar<INTEGER, UNDEFINED>]\n"
            "        " + sched + "[]\n"
            "            0: " + loop + "[type='inner', field_space='go_cu', "
            "it_space='go_internal_pts']\n"
            "                " + sref + "[name:'cu_fld']\n"
            "                    " + smem + "[name:'internal']\n"
            "                        " + mem + "[name:'xstart']\n"
            "                " + sref + "[name:'cu_fld']\n"
            "                    " + smem + "[name:'internal']\n"
            "                        " + mem + "[name:'xstop']\n"
            "                " + lit + "[value:'1', Scalar<INTEGER, "
            "UNDEFINED>]\n"
            "                " + sched + "[]\n"
            "                    0: " + call +
            " compute_cu_code(cu_fld,p_fld,u_fld) "
            "[module_inline=False]\n"
            "    2: " + loop + "[type='outer', field_space='go_every', "
            "it_space='go_internal_pts']\n"
            "        " + lit + "[value:'1', Scalar<INTEGER, UNDEFINED>]\n"
            "        " + bop + "[operator:'SIZE']\n"
            "            " + sref + "[name:'uold_fld']\n"
            "                " + mem + "[name:'data']\n"
            "            " + lit + "[value:'2', Scalar<INTEGER, UNDEFINED>]\n"
            "        " + lit + "[value:'1', Scalar<INTEGER, UNDEFINED>]\n"
            "        " + sched + "[]\n"
            "            0: " + loop + "[type='inner', field_space='go_every',"
            " it_space='go_internal_pts']\n"
            "                " + lit + "[value:'1', Scalar<INTEGER, "
            "UNDEFINED>]\n"
            "                " + bop + "[operator:'SIZE']\n"
            "                    " + sref + "[name:'uold_fld']\n"
            "                        " + mem + "[name:'data']\n"
            "                    " + lit + "[value:'1', Scalar<INTEGER, "
            "UNDEFINED>]\n"
            "                " + lit + "[value:'1', Scalar<INTEGER, "
            "UNDEFINED>]\n"
            "                " + sched + "[]\n"
            "                    0: " + call +
            " time_smooth_code(u_fld,unew_fld,"
            "uold_fld) [module_inline=False]\n")
    else:
        # View with constant loop bounds and without distributed memory
        invoke.schedule._const_loop_bounds = True
        invoke.schedule.view()

        # The view method writes to stdout and this is captured by py.test
        # by default. We have to query this captured output.
        out, _ = capsys.readouterr()

        expected_output = (
            isched + "[invoke='invoke_0', Constant loop bounds=True]\n"
            "    0: " + loop + "[type='outer', field_space='go_cu', "
            "it_space='go_internal_pts']\n"
            "        " + lit + "[value:'2', Scalar<INTEGER, UNDEFINED>]\n"
            "        " + lit + "[value:'jstop', Scalar<INTEGER, "
            "UNDEFINED>]\n"
            "        " + lit + "[value:'1', Scalar<INTEGER, UNDEFINED>]\n"
            "        " + sched + "[]\n"
            "            0: " + loop + "[type='inner', field_space='go_cu', "
            "it_space='go_internal_pts']\n"
            "                " + lit + "[value:'2', Scalar<INTEGER, "
            "UNDEFINED>]\n"
            "                " + lit + "[value:'istop+1', Scalar<INTEGER, "
            "UNDEFINED>]\n"
            "                " + lit + "[value:'1', Scalar<INTEGER, "
            "UNDEFINED>]\n"
            "                " + sched + "[]\n"
            "                    0: " + call +
            " compute_cu_code(cu_fld,p_fld,u_fld) "
            "[module_inline=False]\n"
            "    1: " + loop + "[type='outer', field_space='go_every', "
            "it_space='go_internal_pts']\n"
            "        " + lit + "[value:'1', Scalar<INTEGER, UNDEFINED>]\n"
            "        " + lit + "[value:'jstop+1', Scalar<INTEGER, "
            "UNDEFINED>]\n"
            "        " + lit + "[value:'1', Scalar<INTEGER, UNDEFINED>]\n"
            "        " + sched + "[]\n"
            "            0: " + loop +
            "[type='inner', field_space='go_every', "
            "it_space='go_internal_pts']\n"
            "                " + lit + "[value:'1', Scalar<INTEGER, "
            "UNDEFINED>]\n"
            "                " + lit + "[value:'istop+1', Scalar<INTEGER, "
            "UNDEFINED>]\n"
            "                " + lit + "[value:'1', Scalar<INTEGER, "
            "UNDEFINED>]\n"
            "                " + sched + "[]\n"
            "                    0: " + call +
            " time_smooth_code(u_fld,unew_fld,"
            "uold_fld) [module_inline=False]\n")
    assert expected_output == out


def test_goschedule_str(dist_mem):
    ''' Test that the GOInvokeSchedule::__str__ method works as expected '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "single_invoke_two_kernels.f90"),
                           api=API)
    psy = PSyFactory(API, distributed_memory=dist_mem).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule

    if dist_mem:
        # str without constant loop bounds and with distributed memory
        # where the p field has a stencil access
        sched_str = str(schedule)
        expected_sched = (
            "GOInvokeSchedule[invoke='invoke_0', Constant loop "
            "bounds=False]:\n"
            "HaloExchange[field='p_fld', type='None', depth=None, "
            "check_dirty=False]\n"
            "GOLoop[id:'', variable:'j', loop_type:'outer']\n"
            "StructureReference[name:'cu_fld']\n"
            "StructureMember[name:'internal']\n"
            "Member[name:'ystart']\n"
            "StructureReference[name:'cu_fld']\n"
            "StructureMember[name:'internal']\n"
            "Member[name:'ystop']\n"
            "Literal[value:'1', Scalar<INTEGER, UNDEFINED>]\n"
            "Schedule:\n"
            "GOLoop[id:'', variable:'i', loop_type:'inner']\n"
            "StructureReference[name:'cu_fld']\n"
            "StructureMember[name:'internal']\n"
            "Member[name:'xstart']\n"
            "StructureReference[name:'cu_fld']\n"
            "StructureMember[name:'internal']\n"
            "Member[name:'xstop']\n"
            "Literal[value:'1', Scalar<INTEGER, UNDEFINED>]\n"
            "Schedule:\n"
            "kern call: compute_cu_code\n"
            "End Schedule\n"
            "End GOLoop\n"
            "End Schedule\n"
            "End GOLoop\n"
            "GOLoop[id:'', variable:'j', loop_type:'outer']\n"
            "Literal[value:'1', Scalar<INTEGER, UNDEFINED>]\n"
            "BinaryOperation[operator:'SIZE']\n"
            "StructureReference[name:'uold_fld']\n"
            "Member[name:'data']\n"
            "Literal[value:'2', Scalar<INTEGER, UNDEFINED>]\n"
            "Literal[value:'1', Scalar<INTEGER, UNDEFINED>]\n"
            "Schedule:\n"
            "GOLoop[id:'', variable:'i', loop_type:'inner']\n"
            "Literal[value:'1', Scalar<INTEGER, UNDEFINED>]\n"
            "BinaryOperation[operator:'SIZE']\n"
            "StructureReference[name:'uold_fld']\n"
            "Member[name:'data']\n"
            "Literal[value:'1', Scalar<INTEGER, UNDEFINED>]\n"
            "Literal[value:'1', Scalar<INTEGER, UNDEFINED>]\n"
            "Schedule:\n"
            "kern call: time_smooth_code\n"
            "End Schedule\n"
            "End GOLoop\n"
            "End Schedule\n"
            "End GOLoop\n"
            "End Schedule")
    else:
        # str with constant loop bounds and without distributed memory
        schedule._const_loop_bounds = True
        sched_str = str(schedule)
        expected_sched = (
            "GOInvokeSchedule[invoke='invoke_0', Constant loop bounds=True]:\n"
            "GOLoop[id:'', variable:'j', loop_type:'outer']\n"
            "Literal[value:'2', Scalar<INTEGER, UNDEFINED>]\n"
            "Literal[value:'jstop', Scalar<INTEGER, UNDEFINED>]\n"
            "Literal[value:'1', Scalar<INTEGER, UNDEFINED>]\n"
            "Schedule:\n"
            "GOLoop[id:'', variable:'i', loop_type:'inner']\n"
            "Literal[value:'2', Scalar<INTEGER, UNDEFINED>]\n"
            "Literal[value:'istop+1', Scalar<INTEGER, UNDEFINED>]\n"
            "Literal[value:'1', Scalar<INTEGER, UNDEFINED>]\n"
            "Schedule:\n"
            "kern call: compute_cu_code\n"
            "End Schedule\n"
            "End GOLoop\n"
            "End Schedule\n"
            "End GOLoop\n"
            "GOLoop[id:'', variable:'j', loop_type:'outer']\n"
            "Literal[value:'1', Scalar<INTEGER, UNDEFINED>]\n"
            "Literal[value:'jstop+1', Scalar<INTEGER, UNDEFINED>]\n"
            "Literal[value:'1', Scalar<INTEGER, UNDEFINED>]\n"
            "Schedule:\n"
            "GOLoop[id:'', variable:'i', loop_type:'inner']\n"
            "Literal[value:'1', Scalar<INTEGER, UNDEFINED>]\n"
            "Literal[value:'istop+1', Scalar<INTEGER, UNDEFINED>]\n"
            "Literal[value:'1', Scalar<INTEGER, UNDEFINED>]\n"
            "Schedule:\n"
            "kern call: time_smooth_code\n"
            "End Schedule\n"
            "End GOLoop\n"
            "End Schedule\n"
            "End GOLoop\n"
            "End Schedule")
    assert sched_str == expected_sched


def test_gosched_parent():
    ''' Check that the GOInvokeSchedule constructor allows the parent node
    to be supplied or omitted. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "single_invoke_two_kernels.f90"),
                           api=API)
    kcalls = invoke_info.calls[0].kcalls
    # With no parent specified
    gsched = GOInvokeSchedule("my_sched", kcalls)
    assert gsched.parent is None
    # With a parent
    cont = Container("my_mod")
    gsched = GOInvokeSchedule("my_sched", kcalls, parent=cont)
    assert gsched.parent is cont


def test_gosched_ijstop():
    ''' Test that the GOInvokeSchedule.{i,j}loop_stop raise an error if
    constant loop bounds are not being used '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "single_invoke_two_kernels.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # Turn off constant loop bounds
    schedule.const_loop_bounds = False
    # Attempt to query the upper bound of the i loop
    with pytest.raises(GenerationError):
        _ = schedule.iloop_stop
    # Attempt to query the upper bound of the j loop
    with pytest.raises(GenerationError):
        _ = schedule.jloop_stop


def test_writetoread_dag(tmpdir, have_graphviz):
    ''' Test that the GOInvokeSchedule::dag() method works as expected when we
    have two kernels with a write -> read dependency '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "single_invoke_write_to_read.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    old_cwd = tmpdir.chdir()
    invoke.schedule.dag()
    if have_graphviz:
        dot_file = os.path.join(str(tmpdir), "dag")
        assert os.path.isfile(dot_file)
        with open(dot_file, "r") as dfile:
            dot = dfile.read()
        assert dot.startswith("digraph")
        # write -> read means that the second loop can only begin once the
        # first loop is complete. Check that we have the correct forwards
        # dependence (green) and backwards dependence (red).
        assert ('"loop_[outer]_1_end" -> "loop_[outer]_12_start" [color=red]'
                in dot or
                '"loop_[outer]_1_end" -> "loop_[outer]_12_start" '
                '[color=#ff0000]' in dot)
        assert ('"loop_[outer]_1_end" -> "loop_[outer]_12_start" [color=green]'
                in dot or
                '"loop_[outer]_1_end" -> "loop_[outer]_12_start" '
                '[color=#00ff00]' in dot)
    old_cwd.chdir()


def test_dag(tmpdir, have_graphviz):
    ''' Test that the GOInvokeSchedule::dag() method works as expected '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "nemolite2d_alg_mod.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    old_cwd = tmpdir.chdir()
    invoke.schedule.dag()
    if have_graphviz:
        assert os.path.isfile(os.path.join(str(tmpdir), "dag.svg"))
        dot_file = os.path.join(str(tmpdir), "dag")
        assert os.path.isfile(dot_file)
        with open(dot_file, "r") as dfile:
            dot = dfile.read()
        # The two kernels in this example are independent so we should
        # have no forwards/backwards dependencies
        for col in ["red", "#ff0000", "green", "#00ff00"]:
            assert '[color={0}]'.format(col) not in dot
    old_cwd.chdir()
