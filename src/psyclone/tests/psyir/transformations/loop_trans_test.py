# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Lab

''' Module containing tests for the LoopTrans class. Since it is abstract we
have to test it using various sub-classes. '''

from __future__ import absolute_import
import inspect
from importlib import import_module
import pytest
from psyclone.psyir.transformations import TransformationError, LoopTrans
from psyclone.psyir.nodes import Loop
from psyclone.psyGen import CodedKern
from psyclone.tests.utilities import get_invoke
from psyclone.transformations import OMPParallelLoopTrans, LoopFuseTrans
from psyclone import transformations, psyir


def test_loop_trans_validate():
    ''' Test the validation checks on the loop node provided to the
    transformation. '''
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
    # Break the loop
    loop.children = loop.children[0:1]
    with pytest.raises(TransformationError) as err:
        trans.validate(loop)
    assert ("Error in OMPParallelLoopTrans transformation. The target loop "
            "must have four children but found: ['Literal']" in
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


def test_loop_trans_validate_nemo_specific(monkeypatch):
    ''' Test the NEMO-specifc part of the validation routine.
    TODO #435 remove this test. '''
    trans = OMPParallelLoopTrans()
    _, invoke_info = get_invoke("explicit_do.f90", api="nemo", idx=0)
    schedule = invoke_info.schedule
    loop = schedule.loops()[0]
    monkeypatch.setattr(loop, "_annotations", ["was_where"])
    with pytest.raises(TransformationError) as err:
        trans.validate(loop)
    assert ("In the NEMO API a transformation cannot be applied to a PSyIR "
            "loop representing a WHERE construct." in str(err.value))


def test_all_loop_trans_base_validate(monkeypatch):
    ''' Check that all transformations that sub-class LoopTrans call the
    base validate() method. '''
    # First get a valid Loop object that we can pass in.
    trans = OMPParallelLoopTrans()
    _, invoke = get_invoke("test27_loop_swap.f90", "gocean1.0", idx=1,
                           dist_mem=False)
    loop = invoke.schedule.walk(Loop)[0]

    # Find all classes defined in our various possible locations for
    # transformations. This highlights the problem of finding transformations
    # which is the subject of TODO #978.
    all_trans_classes = inspect.getmembers(psyir.transformations,
                                           inspect.isclass)
    all_trans_classes += inspect.getmembers(transformations, inspect.isclass)
    # Domain-specific transformations
    for api in ["gocean", "lfric", "nemo"]:
        transmod = import_module("psyclone.domain.{0}.transformations".format(
            api))
        all_trans_classes += inspect.getmembers(transmod, inspect.isclass)
    # To ensure that we identify that the validate() method in the LoopTrans
    # base class has been called, we monkeypatch it to raise an exception.

    def fake_validate(_1, _2, options=None):
        raise NotImplementedError("validate test exception")
    monkeypatch.setattr(LoopTrans, "validate", fake_validate)

    for name, cls_type in all_trans_classes:
        # We can't just instantiate every class as those that aren't
        # transformations sometimes require arguments to the constructor.
        if not inspect.isabstract(cls_type) and name.endswith("Trans"):
            trans = cls_type()
            if isinstance(trans, LoopTrans):
                with pytest.raises(NotImplementedError) as err:
                    if isinstance(trans, LoopFuseTrans):
                        trans.validate(loop, loop)
                    else:
                        trans.validate(loop)
                assert "validate test exception" in str(err.value), \
                    "{0}.validate() does not call LoopTrans.validate()".format(
                        name)
