# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Module containing tests for ReadOnlyVerifyTrans and ReadOnlyVerifyNode
'''

import pytest

from psyclone.errors import InternalError
from psyclone.psyir.nodes import Node, ReadOnlyVerifyNode, Schedule
from psyclone.psyir.transformations import (ReadOnlyVerifyTrans,
                                            TransformationError)
from psyclone.tests.utilities import get_invoke
from psyclone.transformations import OMPParallelLoopTrans
from psyclone.utils import colored

# --------------------------------------------------------------------------- #
# ================== ReadOnly Transformation tests ========================== #
# --------------------------------------------------------------------------- #


def test_readonly_trans():
    '''Tests basic functions in ReadOnlyVerifyTrans.'''
    read_only = ReadOnlyVerifyTrans()
    assert str(read_only) == "Create a sub-tree of the PSyIR that has " \
                             "a node of type ReadOnlyVerifyNode at its root."
    assert read_only.name == "ReadOnlyVerifyTrans"


# -----------------------------------------------------------------------------
def test_malformed_readonly_node(monkeypatch):
    ''' Check that we raise the expected error if a ReadOnlyVerifyNode does
    not have a single Schedule node as its child. '''
    read_node = ReadOnlyVerifyNode()
    monkeypatch.setattr(read_node, "_children", [])
    with pytest.raises(InternalError) as err:
        _ = read_node.read_only_verify_body
    assert "malformed or incomplete. It should have a " in str(err.value)
    monkeypatch.setattr(read_node, "_children", [Node(), Node()])
    with pytest.raises(InternalError) as err:
        _ = read_node.read_only_verify_body
    assert "malformed or incomplete. It should have a " in str(err.value)


# -----------------------------------------------------------------------------
def test_read_only_basic():
    '''Check basic functionality: node names, schedule view.
    '''
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           "gocean", idx=0, dist_mem=False)
    read_only = ReadOnlyVerifyTrans()
    read_only.apply(invoke.schedule[0].loop_body[0])
    result = invoke.schedule.view()

    # Create the coloured text (if required)
    read_node = colored("ReadOnlyVerify", ReadOnlyVerifyNode._colour)
    sched_node = colored("Schedule", Schedule._colour)
    assert f"""{sched_node}[]
            0: {read_node}[]
                {sched_node}[]""" in result


# -----------------------------------------------------------------------------
def test_read_only_options():
    '''Check that options are passed to the ReadOnly Node and trigger
    the use of the newly defined names.
    '''
    psy, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                             "gocean", idx=0, dist_mem=False)
    read_only = ReadOnlyVerifyTrans()
    read_only.apply(invoke.schedule[0],
                    options={"region_name": ("a", "b")})
    code = str(psy.gen)

    assert 'CALL read_only_verify_psy_data % PreStart("a", "b", 6, 6)' in code


# -----------------------------------------------------------------------------
def test_invalid_apply():
    '''Test the exceptions that should be raised by ReadOnlyVerifyTrans.

    '''
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           "gocean", idx=0)
    read_only = ReadOnlyVerifyTrans()
    omp = OMPParallelLoopTrans()
    omp.apply(invoke.schedule[0])
    with pytest.raises(TransformationError) as err:
        read_only.apply(invoke.schedule[0].dir_body[0],
                        options={"region_name": ("a", "b")})
    assert "Error in ReadOnlyVerifyTrans: Application to a Loop without its "\
           "parent Directive is not allowed." in str(err.value)

    with pytest.raises(TransformationError) as err:
        read_only.apply(invoke.schedule[0].dir_body[0].loop_body[0],
                        options={"region_name": ("a", "b")})
    assert "Error in ReadOnlyVerifyTrans: Application to Nodes enclosed " \
           "within a thread-parallel region is not allowed." in str(err.value)
