# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018, Science and Technology Facilities Council.
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
# Authors R. W. Ford and A. R. Porter, STFC Daresbury Lab

'''
API-agnostic tests for various transformation classes.
'''

from __future__ import absolute_import, print_function
import pytest


def test_accloop():
    ''' Generic tests for the ACCLoopTrans transformation class '''
    from psyclone.transformations import ACCLoopTrans
    from psyclone.psyGen import Node, ACCLoopDirective
    trans = ACCLoopTrans()
    assert trans.name == "ACCLoopTrans"
    assert str(trans) == "Adds an 'OpenACC loop' directive to a loop"

    pnode = Node()
    cnode = Node()
    tdir = trans._directive(pnode, [cnode])
    assert isinstance(tdir, ACCLoopDirective)


def test_accparallel():
    ''' Generic tests for the ACCParallelTrans class '''
    from psyclone.transformations import ACCParallelTrans
    acct = ACCParallelTrans()
    assert acct.name == "ACCParallelTrans"


def test_accenterdata():
    ''' Generic tests for the ACCEnterDataTrans class '''
    from psyclone.transformations import ACCEnterDataTrans
    acct = ACCEnterDataTrans()
    assert acct.name == "ACCEnterDataTrans"
    assert str(acct) == "Adds an OpenACC 'enter data' directive"


def test_accenterdata_internalerr(monkeypatch):
    ''' Check that the ACCEnterDataTrans.apply() method raises an internal
    error if the _validate method fails to throw out an invalid type of
    Schedule. '''
    from psyclone.transformations import ACCEnterDataTrans
    from psyclone.psyGen import InternalError
    acct = ACCEnterDataTrans()
    monkeypatch.setattr(acct, "_validate", lambda sched: None)
    with pytest.raises(InternalError) as err:
        _, _ = acct.apply("Not a schedule")
    assert "validate() has not rejected an (unsupported) schedule" in str(err)


def test_omploop_no_collapse():
    ''' Check that the OMPLoopTrans.directive() method rejects the
    collapse argument '''
    from psyclone.psyGen import Node
    from psyclone.transformations import OMPLoopTrans
    trans = OMPLoopTrans()
    pnode = Node()
    cnode = Node()
    with pytest.raises(NotImplementedError) as err:
        _ = trans._directive(pnode, cnode, collapse=2)
    assert ("The COLLAPSE clause is not yet supported for '!$omp do' "
            "directives" in str(err))


def test_ifblock_children_region():
    ''' Check that we reject attempts to transform the conditional part of
    an If statement or to include both the if- and else-clauses in a region
    (without their parent). '''
    from psyclone.psyGen import IfBlock, Reference, Schedule
    from psyclone.transformations import ACCParallelTrans, TransformationError
    acct = ACCParallelTrans()
    # Construct a valid IfBlock
    ifblock = IfBlock()
    # Condition
    ref1 = Reference('condition1', parent=ifblock)
    ifblock.addchild(ref1)
    # If-body
    sch = Schedule(parent=ifblock)
    ifblock.addchild(sch)
    # Else-body
    sch2 = Schedule(parent=ifblock)
    ifblock.addchild(sch2)
    # Attempt to put all of the children of the IfBlock into a region. This
    # is an error because the first child is the conditional part of the
    # IfBlock.
    with pytest.raises(TransformationError) as err:
        super(ACCParallelTrans, acct)._validate(ifblock.children)
    assert ("transformation to the conditional expression (first child" in
            str(err))
    with pytest.raises(TransformationError) as err:
        super(ACCParallelTrans, acct)._validate(ifblock.children[1:])
    assert ("Cannot enclose both the if- and else- clauses of an IfBlock by "
            in str(err))


def test_fusetrans_error_incomplete():
    ''' Check that we reject attempts to fuse loops which are incomplete. '''
    from psyclone.psyGen import Loop, Schedule, Literal, Return
    from psyclone.transformations import LoopFuseTrans, TransformationError
    sch = Schedule()
    loop1 = Loop(variable_name="i", parent=sch)
    loop2 = Loop(variable_name="j", parent=sch)
    sch.addchild(loop1)
    sch.addchild(loop2)

    fuse = LoopFuseTrans()

    # Check first loop
    with pytest.raises(TransformationError) as err:
        fuse._validate(loop1, loop2)
    assert "Error in LoopFuse transformation. The first loop does not have " \
        "4 children." in str(err.value)

    loop1.addchild(Literal("start", parent=loop1))
    loop1.addchild(Literal("stop", parent=loop1))
    loop1.addchild(Literal("step", parent=loop1))
    loop1.addchild(Schedule(parent=loop1))
    loop1.loop_body.addchild(Return(parent=loop1.loop_body))

    # Check second loop
    with pytest.raises(TransformationError) as err:
        fuse._validate(loop1, loop2)
    assert "Error in LoopFuse transformation. The second loop does not have " \
        "4 children." in str(err.value)

    loop2.addchild(Literal("start", parent=loop2))
    loop2.addchild(Literal("stop", parent=loop2))
    loop2.addchild(Literal("step", parent=loop2))
    loop2.addchild(Schedule(parent=loop2))
    loop2.loop_body.addchild(Return(parent=loop2.loop_body))

    # Validation should now pass
    fuse._validate(loop1, loop2)
