# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2021, Science and Technology Facilities Council.
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
# Modified I. Kavcic, Met Office
# Modified J. Henrichs, Bureau of Meteorology

'''
API-agnostic tests for various transformation classes.
'''

from __future__ import absolute_import, print_function
import os
import pytest

from psyclone.errors import InternalError
from psyclone.psyir.nodes import CodeBlock, IfBlock, Literal, Loop, Node, \
    Reference, Schedule, Statement, ACCLoopDirective
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE, BOOLEAN_TYPE
from psyclone.psyir.transformations import ProfileTrans, RegionTrans, \
    TransformationError
from psyclone.tests.utilities import get_invoke
from psyclone.transformations import ACCEnterDataTrans, ACCLoopTrans, \
    ACCParallelTrans, OMPLoopTrans, OMPParallelLoopTrans, OMPParallelTrans, \
    OMPSingleTrans
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory

GOCEAN_BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                os.pardir, os.pardir, "test_files",
                                "gocean1p0")


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
        _, _ = acct.apply("Not a schedule")
    assert ("validate() has not rejected an (unsupported) schedule"
            in str(err.value))


def test_omploop_no_collapse():
    ''' Check that the OMPLoopTrans.directive() method rejects the
    collapse argument '''
    trans = OMPLoopTrans()
    cnode = Node()
    with pytest.raises(NotImplementedError) as err:
        _ = trans._directive(cnode, collapse=2)
    assert ("The COLLAPSE clause is not yet supported for '!$omp do' "
            "directives" in str(err.value))


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
    assert("Transformation Error: Nodes of type 'OMPSingleDirective' cannot" +
           " be enclosed by a OMPSingleTrans transformation"
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
        _, _ = profile_trans.apply(schedule.children, options=options)
    else:
        _, _ = profile_trans.apply(schedule.children)
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
        _ = profile_trans.apply(node, options={"region_name": value})
    assert ("User-supplied region name must be a tuple containing "
            "two non-empty strings." in str(excinfo.value))
