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
# Modified J. Henrichs, Bureau of Metorology

'''
API-agnostic tests for various transformation classes.
'''

from __future__ import absolute_import, print_function
import pytest

from psyclone.errors import InternalError
from psyclone.psyGen import ACCLoopDirective
from psyclone.psyir.nodes import IfBlock, Literal, Loop, Node, Reference, \
    Schedule, Statement, CodeBlock, Return
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE, BOOLEAN_TYPE
from psyclone.psyir.transformations import LoopFuseTrans, ProfileTrans, \
    RegionTrans, TransformationError
from psyclone.tests.utilities import get_invoke
from psyclone.transformations import ACCEnterDataTrans, ACCLoopTrans, \
    ACCParallelTrans, OMPLoopTrans, OMPParallelLoopTrans, OMPParallelTrans


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


def test_fusetrans_error_incomplete():
    ''' Check that we reject attempts to fuse loops which are incomplete. '''
    sch = Schedule()
    loop1 = Loop(variable=DataSymbol("i", INTEGER_TYPE))
    loop2 = Loop(variable=DataSymbol("j", INTEGER_TYPE))
    sch.addchild(loop1)
    sch.addchild(loop2)

    fuse = LoopFuseTrans()

    # Check first loop
    with pytest.raises(TransformationError) as err:
        fuse.validate(loop1, loop2)
    assert ("Error in LoopFuseTrans transformation. The target loop must have "
            "four children but found: []" in str(err.value))

    loop1.addchild(Literal("start", INTEGER_TYPE))
    loop1.addchild(Literal("stop", INTEGER_TYPE))
    loop1.addchild(Literal("step", INTEGER_TYPE))
    loop1.addchild(Schedule())
    loop1.loop_body.addchild(Return())

    # Check second loop
    with pytest.raises(TransformationError) as err:
        fuse.validate(loop1, loop2)
    assert ("Error in LoopFuseTrans transformation. The target loop must have "
            "four children but found: []" in str(err.value))

    loop2.addchild(Literal("start", INTEGER_TYPE))
    loop2.addchild(Literal("stop", INTEGER_TYPE))
    loop2.addchild(Literal("step", INTEGER_TYPE))
    loop2.addchild(Schedule())
    loop2.loop_body.addchild(Return())

    # Validation should now pass
    fuse.validate(loop1, loop2)


def test_fusetrans_error_not_same_parent():
    ''' Check that we reject attempts to fuse loops which don't share the
    same parent '''

    sch1 = Schedule()
    sch2 = Schedule()
    loop1 = Loop(variable=DataSymbol("i", INTEGER_TYPE))
    loop2 = Loop(variable=DataSymbol("j", INTEGER_TYPE))
    sch1.addchild(loop1)
    sch2.addchild(loop2)

    loop1.addchild(Literal("1", INTEGER_TYPE))  # start
    loop1.addchild(Literal("10", INTEGER_TYPE))  # stop
    loop1.addchild(Literal("1", INTEGER_TYPE))  # step
    loop1.addchild(Schedule())  # loop body

    loop2.addchild(Literal("1", INTEGER_TYPE))  # start
    loop2.addchild(Literal("10", INTEGER_TYPE))  # stop
    loop2.addchild(Literal("1", INTEGER_TYPE))  # step
    loop2.addchild(Schedule())  # loop body

    fuse = LoopFuseTrans()

    # Try to fuse loops with different parents
    with pytest.raises(TransformationError) as err:
        fuse.validate(loop1, loop2)
    assert ("Error in LoopFuseTrans transformation. Loops do not have the "
            "same parent" in str(err.value))


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
