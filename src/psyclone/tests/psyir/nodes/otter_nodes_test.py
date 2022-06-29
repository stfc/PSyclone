# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council
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
# Author A. B. G. Chalk, STFC Daresbury Lab 

''' Module containing the tests for OtterNodes. '''

import pytest
from psyclone.errors import InternalError, GenerationError
from psyclone.f2pygen import ModuleGen
from psyclone.psyir.nodes import PSyDataNode, Schedule, Return, Routine, \
        Call
from psyclone.psyir.nodes.statement import Statement
from psyclone.psyir.nodes.otter_nodes import OtterTraceSetupNode, \
        OtterParallelNode, OtterTaskNode, OtterTaskSingleNode, \
        OtterLoopNode, OtterLoopIterationNode, OtterSynchroniseChildrenNode, \
        OtterSynchroniseDescendantTasksNode, OtterTraceNode
from psyclone.psyir.symbols import ContainerSymbol, ImportInterface, \
    SymbolTable, DataTypeSymbol, DeferredType, DataSymbol, UnknownFortranType
from psyclone.tests.utilities import get_invoke


def test_ottertracesetup_lower_to_language():
    ''' Test that the OtterTraceSetupNode is lowered as expected. '''

    # Try without an ancestor Routine
    psy_node = OtterTraceSetupNode()
    with pytest.raises(GenerationError) as excinfo:
        psy_node.lower_to_language_level()
    assert ("An OtterNode must be inside a Routine context when"
            " lowering but 'otterTraceSetupNode[]' is not."
            in str(excinfo.value))

    # Add the ancestor Routine and empty body
    routine = Routine("my_routine")
    routine.addchild(psy_node)
    psy_node.lower_to_language_level()

    assert not routine.walk(OtterTraceSetupNode)
    calls = routine.walk(Call)
    assert len(calls) == 2
    assert calls[0].routine.name == "fortran_otterTraceInitialise_i"
    assert len(calls[0].children) == 3
    assert calls[0].children[0].name == "__FILE__"
    assert calls[0].children[1].value == "my_routine"
    assert calls[0].children[2].name == "__LINE__"

    assert calls[1].routine.name == "fortran_otterTraceFinalise"
    assert len(calls[1].children) == 0


def test_otterparallelnode_lower_to_language():
    ''' Test that the OtterParallelNode is lowered as expected. '''

    # Try without an ancestor Routine
    psy_node = OtterParallelNode()
    with pytest.raises(GenerationError) as excinfo:
        psy_node.lower_to_language_level()
    assert ("An OtterNode must be inside a Routine context when"
            " lowering but 'otterParallelNode[]' is not."
            in str(excinfo.value))

    # Add the ancestor Routine and empty body
    routine = Routine("my_routine")
    routine.addchild(psy_node)
    psy_node.lower_to_language_level()

    assert not routine.walk(OtterParallelNode)
    calls = routine.walk(Call)
    assert len(calls) == 2
    assert calls[0].routine.name == "fortran_otterThreadsBegin_i"
    assert len(calls[0].children) == 3
    assert calls[0].children[0].name == "__FILE__"
    assert calls[0].children[1].value == "my_routine"
    assert calls[0].children[2].name == "__LINE__"

    assert calls[1].routine.name == "fortran_otterThreadsEnd"
    assert len(calls[1].children) == 0


def test_ottertasknode_lower_to_language():
    ''' Test that the OtterTaskNode is lowered as expected. '''

    # Try without an ancestor Routine
    psy_node = OtterTaskNode()
    with pytest.raises(GenerationError) as excinfo:
        psy_node.lower_to_language_level()
    assert ("An OtterNode must be inside a Routine context when"
            " lowering but 'otterTaskNode[]' is not."
            in str(excinfo.value))

    # Add the ancestor Routine and empty body
    routine = Routine("my_routine")
    routine.addchild(psy_node)
    psy_node.lower_to_language_level()

    assert not routine.walk(OtterTaskNode)
    calls = routine.walk(Call)
    assert len(calls) == 2
    assert calls[0].routine.name == "fortran_otterTaskBegin_i"
    assert len(calls[0].children) == 3
    assert calls[0].children[0].name == "__FILE__"
    assert calls[0].children[1].value == "my_routine"
    assert calls[0].children[2].name == "__LINE__"

    assert calls[1].routine.name == "fortran_otterTaskEnd"
    assert len(calls[1].children) == 0


def test_ottertasksinglenode_lower_to_language():
    ''' Test that the OtterTaskSingleNode is lowered as expected. '''

    # Try without an ancestor Routine
    psy_node = OtterTaskSingleNode()
    with pytest.raises(GenerationError) as excinfo:
        psy_node.lower_to_language_level()
    assert ("An OtterNode must be inside a Routine context when"
            " lowering but 'otterTaskSingleNode[]' is not."
            in str(excinfo.value))

    # Add the ancestor Routine and empty body
    routine = Routine("my_routine")
    routine.addchild(psy_node)
    psy_node.lower_to_language_level()

    assert not routine.walk(OtterTaskSingleNode)
    calls = routine.walk(Call)
    assert len(calls) == 2
    assert calls[0].routine.name == "fortran_otterTaskSingleBegin_i"
    assert len(calls[0].children) == 3
    assert calls[0].children[0].name == "__FILE__"
    assert calls[0].children[1].value == "my_routine"
    assert calls[0].children[2].name == "__LINE__"

    assert calls[1].routine.name == "fortran_otterTaskSingleEnd"
    assert len(calls[1].children) == 0


def test_otterloopnode_lower_to_language():
    ''' Test that the OtterLoopNode is lowered as expected. '''

    # Try without an ancestor Routine
    psy_node = OtterLoopNode()
    with pytest.raises(GenerationError) as excinfo:
        psy_node.lower_to_language_level()
    assert ("An OtterNode must be inside a Routine context when"
            " lowering but 'otterLoopNode[]' is not."
            in str(excinfo.value))

    # Add the ancestor Routine and empty body
    routine = Routine("my_routine")
    routine.addchild(psy_node)
    psy_node.lower_to_language_level()

    assert not routine.walk(OtterLoopNode)
    calls = routine.walk(Call)
    assert len(calls) == 2
    assert calls[0].routine.name == "fortran_otterLoopBegin_i"
    assert len(calls[0].children) == 3
    assert calls[0].children[0].name == "__FILE__"
    assert calls[0].children[1].value == "my_routine"
    assert calls[0].children[2].name == "__LINE__"

    assert calls[1].routine.name == "fortran_otterLoopEnd"
    assert len(calls[1].children) == 0


def test_otterloopiterationnode_lower_to_language():
    ''' Test that the OtterLoopIterationNode is lowered as expected. '''

    # Try without an ancestor Routine
    psy_node = OtterLoopIterationNode()
    with pytest.raises(GenerationError) as excinfo:
        psy_node.lower_to_language_level()
    assert ("An OtterNode must be inside a Routine context when"
            " lowering but 'otterLoopIterationNode[]' is not."
            in str(excinfo.value))

    # Add the ancestor Routine and empty body
    routine = Routine("my_routine")
    routine.addchild(psy_node)
    psy_node.lower_to_language_level()

    assert not routine.walk(OtterLoopIterationNode)
    calls = routine.walk(Call)
    assert len(calls) == 2
    assert calls[0].routine.name == "fortran_otterLoopIterationBegin_i"
    assert len(calls[0].children) == 3
    assert calls[0].children[0].name == "__FILE__"
    assert calls[0].children[1].value == "my_routine"
    assert calls[0].children[2].name == "__LINE__"

    assert calls[1].routine.name == "fortran_otterLoopIterationEnd"
    assert len(calls[1].children) == 0


def test_ottersyncorhonisechildrennode_lower_to_language():
    ''' Test that the OtterSynchroniseChildrenNode is lowered as expected. '''

    # Try without an ancestor Routine
    psy_node = OtterSynchroniseChildrenNode()
    with pytest.raises(GenerationError) as excinfo:
        psy_node.lower_to_language_level()
    assert ("An OtterNode must be inside a Routine context when"
            " lowering but 'otterSynchroniseChildrenNode[]' is not."
            in str(excinfo.value))

    # Add the ancestor Routine and empty body
    routine = Routine("my_routine")
    routine.addchild(psy_node)
    psy_node.lower_to_language_level()

    assert not routine.walk(OtterSynchroniseChildrenNode)
    calls = routine.walk(Call)
    assert len(calls) == 1
    assert calls[0].routine.name == "fortran_otterSynchroniseTasks_i"
    assert len(calls[0].children) == 3
    assert calls[0].children[0].name == "__FILE__"
    assert calls[0].children[1].value == "my_routine"
    assert calls[0].children[2].name == "__LINE__"


def test_ottersyncorhonisedescendenttasksnode_lower_to_language():
    ''' Test that the OtterSynchroniseDescendantTasksNode is lowered as 
    expected. '''

    # Try without an ancestor Routine
    psy_node = OtterSynchroniseDescendantTasksNode()
    with pytest.raises(GenerationError) as excinfo:
        psy_node.lower_to_language_level()
    assert ("An OtterNode must be inside a Routine context when"
            " lowering but 'otterSynchroniseDescendantTasksNode[]' is not."
            in str(excinfo.value))

    # Add the ancestor Routine and empty body
    routine = Routine("my_routine")
    routine.addchild(psy_node)
    psy_node.lower_to_language_level()

    assert not routine.walk(OtterSynchroniseDescendantTasksNode)
    calls = routine.walk(Call)
    assert len(calls) == 2
    assert (calls[0].routine.name == 
            "fortran_otterSynchroniseDescendantTasksBegin_i")
    assert len(calls[0].children) == 3
    assert calls[0].children[0].name == "__FILE__"
    assert calls[0].children[1].value == "my_routine"
    assert calls[0].children[2].name == "__LINE__"

    assert (calls[1].routine.name == 
            "fortran_otterSynchroniseDescendantTasksEnd")
    assert len(calls[1].children) == 0


def test_ottertracenode_lower_to_language():
    ''' Test that the OtterTraceNode is lowered as expected. '''

    # Try without an ancestor Routine
    psy_node = OtterTraceNode()
    with pytest.raises(GenerationError) as excinfo:
        psy_node.lower_to_language_level()
    assert ("An OtterNode must be inside a Routine context when"
            " lowering but 'otterTraceNode[]' is not."
            in str(excinfo.value))

    # Add the ancestor Routine and empty body
    routine = Routine("my_routine")
    routine.addchild(psy_node)
    psy_node.lower_to_language_level()

    assert not routine.walk(OtterTraceNode)
    calls = routine.walk(Call)
    assert len(calls) == 2
    assert calls[0].routine.name == "fortran_otterTraceStart"
    assert len(calls[0].children) == 0

    assert calls[1].routine.name == "fortran_otterTraceStop"
    assert len(calls[1].children) == 0
