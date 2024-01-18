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
# Author: A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified: L. Turner, Met Office

''' Module containing tests for the LoopTrans class. Since it is abstract we
have to test it using various sub-classes. '''

from __future__ import absolute_import
import inspect
import pytest
from psyclone.psyir.transformations import LoopFuseTrans, LoopTrans, \
    TransformationError
from psyclone.psyir.nodes import Loop
from psyclone.psyGen import CodedKern
from psyclone.tests.utilities import get_invoke
from psyclone.transformations import OMPParallelLoopTrans
from psyclone import transformations, psyir


def test_loop_trans_name():
    ''' Check that the name method works as expected. '''
    # We have to use sub-classes of LoopTrans as it itself is abstract.
    trans1 = OMPParallelLoopTrans()
    assert trans1.name == "OMPParallelLoopTrans"
    trans2 = LoopFuseTrans()
    assert trans2.name == "LoopFuseTrans"


def test_loop_trans_validate(monkeypatch):
    ''' Test the validation checks on the loop node provided to the
    transformation. '''
    # We have to use sub-class of LoopTrans as it itself is abstract.
    trans = OMPParallelLoopTrans()
    _, invoke = get_invoke("test27_loop_swap.f90", "gocean1.0", idx=1,
                           dist_mem=False)
    with pytest.raises(TransformationError) as err:
        trans.validate(invoke.schedule)
    assert ("Target of OMPParallelLoopTrans transformation must be a sub-"
            "class of Loop but got 'GOInvokeSchedule'" in str(err.value))
    # Check that validate is OK with a valid loop
    loop = invoke.schedule.walk(Loop)[0]
    trans.validate(loop)
    # Pretend that the loop is of 'null' type
    monkeypatch.setattr(loop, "_loop_type", "null")
    with pytest.raises(TransformationError) as err:
        trans.validate(loop)
    assert ("Cannot apply a OMPParallelLoopTrans transformation to a "
            "'null' loop" in str(err.value))
    monkeypatch.undo()
    # Break the contents of the loop
    loop.children = loop.pop_all_children()[0:1]
    with pytest.raises(TransformationError) as err:
        trans.validate(loop)
    assert ("Error in OMPParallelLoopTrans transformation. The target loop "
            "must have four children but found:" in
            str(err.value))


def test_loop_trans_validate_options(monkeypatch):
    ''' Test the options argument to the validate method. '''
    trans = OMPParallelLoopTrans()
    _, invoke = get_invoke("test27_loop_swap.f90", "gocean1.0", idx=1,
                           dist_mem=False)
    loop = invoke.schedule.walk(Loop)[0]
    with pytest.raises(TransformationError) as err:
        trans.validate(loop, options="hello")
    assert ("method 'options' argument must be a dictionary but found 'str'"
            in str(err.value))
    # Monkeypatch the transformation to make it appear that we wish to
    # exclude CodedKern nodes.
    monkeypatch.setattr(trans, "excluded_node_types", (CodedKern, ))
    with pytest.raises(TransformationError) as err:
        trans.validate(loop)
    assert ("Nodes of type 'GOKern' cannot be enclosed by a "
            "OMPParallelLoopTrans transformation" in str(err.value))
    # Now disable this check on excluded node types
    trans.validate(loop, options={"node-type-check": False})


def test_all_loop_trans_base_validate(monkeypatch):
    ''' Check that all transformations that sub-class LoopTrans call the
    base validate() method. '''
    # First get a valid GOLoop and LFRicLoop objects that we can pass in,
    # as appropriate.
    _, invoke = get_invoke("test27_loop_swap.f90", "gocean1.0", idx=1,
                           dist_mem=False)
    goloop = invoke.schedule.walk(Loop)[0]
    _, invoke = get_invoke("1_single_invoke.f90", "dynamo0.3", idx=0,
                           dist_mem=False)
    lfricloop = invoke.schedule.walk(Loop)[0]

    # Find all PSyIR transformations. There are currently two locations for
    # these. Eventually all general transformations will be in
    # psyir.transformations.
    all_trans_classes = inspect.getmembers(psyir.transformations,
                                           inspect.isclass)
    all_trans_classes += inspect.getmembers(transformations, inspect.isclass)

    # To ensure that we identify that the validate() method in the LoopTrans
    # base class has been called, we monkeypatch it to raise an exception.

    def fake_validate(_1, _2, options=None):
        raise NotImplementedError("validate test exception")
    monkeypatch.setattr(LoopTrans, "validate", fake_validate)

    for name, cls_type in all_trans_classes:
        # We can't just instantiate every class as those that aren't
        # transformations sometimes require arguments to the constructor.
        if (not inspect.isabstract(cls_type) and
                name.startswith("Trans") and
                "Error" not in name):
            trans = cls_type()
            if isinstance(trans, LoopTrans):
                # Ensure we use an LFRicLoop for LFRic transformations.
                target = lfricloop if name.beginswith("LFRic") else goloop
                with pytest.raises(NotImplementedError) as err:
                    if isinstance(trans, LoopFuseTrans):
                        trans.validate(target, target)
                    else:
                        trans.validate(target)
                assert "validate test exception" in str(err.value), \
                    f"{name}.validate() does not call LoopTrans.validate()"
