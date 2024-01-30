# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2024, Science and Technology Facilities Council
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
# Author: J. Henrichs, Bureau of Meteorology
# Modified: R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab

''' Module containing tests for generating PSyData hooks'''

from __future__ import absolute_import

import pytest

from psyclone.configuration import Config
from psyclone.errors import InternalError
from psyclone.psyir.nodes import PSyDataNode
from psyclone.psyir.transformations import (ExtractTrans, PSyDataTrans,
                                            ReadOnlyVerifyTrans,
                                            TransformationError)
from psyclone.tests.utilities import get_invoke


# -----------------------------------------------------------------------------
def test_psy_data_trans_empty_list():
    ''' Check that the transformation rejects an empty list of nodes. '''
    data_trans = PSyDataTrans()
    with pytest.raises(TransformationError) as err:
        data_trans.apply([])
    assert "Cannot apply transformation to an empty list" in str(err.value)


# -----------------------------------------------------------------------------
def test_psy_data_trans_basic():
    '''Check basic functionality: node names, schedule view.
    '''
    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           "gocean1.0", idx=0, dist_mem=False)
    schedule = invoke.schedule

    data_trans = PSyDataTrans()
    assert "Create a sub-tree of the PSyIR that has a node of type " \
           "PSyDataNode at its root" in str(data_trans)

    assert data_trans.name == "PSyDataTrans"
    data_trans.apply(schedule)

    assert isinstance(invoke.schedule[0], PSyDataNode)

    # Insert a DataTrans call between outer and inner loop.
    # This tests that we find the subroutine node even
    # if it is not the immediate parent.
    node = invoke.schedule[0].psy_data_body[0].loop_body[0]
    data_trans.apply(node)

    assert isinstance(invoke.schedule[0].psy_data_body[0].loop_body[0],
                      PSyDataNode)
    assert invoke.schedule[0].psy_data_body[0].loop_body[0].children[0].\
        children[0] is node


# -----------------------------------------------------------------------------
def test_class_definitions(fortran_writer):
    '''Tests if the class-prefix can be set and behaves as expected.
    '''

    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           "gocean1.0", idx=0)
    schedule = invoke.schedule

    data_trans = PSyDataTrans()
    data_trans.apply(schedule)
    code = fortran_writer(schedule.root)

    # By default, no prefix should be used:
    assert "use psy_data_mod, only : PSyDataType" in code
    assert "type(PSyDataType), save, target :: psy_data" in code
    assert "CALL psy_data" in code

    # This puts the new PSyDataNode with prefix "extract" around the
    # previous PSyDataNode, but the prefix was not used previously.
    data_trans.apply(schedule, {"prefix": "extract"})
    code = fortran_writer(schedule.root)
    assert "use extract_psy_data_mod, only : extract_PSyDataType" in code
    assert ("type(extract_PSyDataType), save, target :: "
            "extract_psy_data" in code)
    assert "CALL extract_psy_data" in code
    # The old call must still be there (e.g. not somehow be changed
    # by setting the prefix)
    assert "use psy_data_mod, only : PSyDataType" in code
    assert "type(PSyDataType), save, target :: psy_data" in code
    assert "CALL psy_data" in code

    # Now add a third class: "profile", and make sure all previous
    # and new declarations and calls are there:
    data_trans.apply(schedule, {"prefix": "profile"})
    code = fortran_writer(schedule.root)
    assert "use psy_data_mod, only : PSyDataType" in code
    assert "use extract_psy_data_mod, only : extract_PSyDataType" in code
    assert "use profile_psy_data_mod, only : profile_PSyDataType" in code

    assert "type(PSyDataType), save, target :: psy_data" in code
    assert ("type(extract_PSyDataType), save, target :: "
            "extract_psy_data" in code)
    assert ("type(profile_PSyDataType), save, target :: "
            "profile_psy_data" in code)

    assert "CALL psy_data" in code
    assert "CALL extract_psy_data" in code
    assert "CALL profile_psy_data" in code

    with pytest.raises(TransformationError) as err:
        data_trans.apply(schedule, {"prefix": "invalid-prefix"})
    assert "Error in 'prefix' parameter: found 'invalid-prefix', while " \
        "one of " in str(err.value)
    assert "as defined in /" in str(err.value)


# -----------------------------------------------------------------------------
def test_psy_data_get_unique_region_names():
    '''Tests the get_unique_region_names function.'''
    data_trans = PSyDataTrans()
    region_name = data_trans.\
        get_unique_region_name([], {"region_name": ("a", "b")})
    assert region_name == ("a", "b")

    with pytest.raises(InternalError) as err:
        region_name = data_trans.\
            get_unique_region_name([], {"region_name": 1})
    assert "The name must be a tuple containing two non-empty strings." \
        in str(err.value)

    with pytest.raises(InternalError) as err:
        region_name = data_trans.\
            get_unique_region_name([], {"region_name": ("a", "")})
    assert "The name must be a tuple containing two non-empty strings." \
        in str(err.value)

    _, invoke = get_invoke("test11_different_iterates_over_one_invoke.f90",
                           "gocean1.0", idx=0)
    region_name = data_trans.get_unique_region_name(invoke.schedule, {})
    assert region_name == ('psy_single_invoke_different_iterates_over',
                           'invoke_0:r0')

    region_name = data_trans.\
        get_unique_region_name([invoke.schedule[0]], {})
    assert region_name == ('psy_single_invoke_different_iterates_over',
                           'invoke_0:compute_cv_code:r0')


# -----------------------------------------------------------------------------
@pytest.mark.parametrize("transformation",
                         [ExtractTrans(), ReadOnlyVerifyTrans()])
def test_trans_with_shape_function(monkeypatch, fortran_reader,
                                   fortran_writer, transformation):
    '''Tests that extraction of a region that uses an array-shape Fortran
    intrinsic like lbound, ubound, or size do include these references.

    '''
    source = '''program test
                integer :: ji, jk
                integer, parameter :: jpi=10, jpk=10
                real, dimension(jpi,jpi,jpk) :: umask, dummy
                do jk = 1, ubound(dummy,1)
                  umask(1,1,jk) = -1.0d0
                end do
                end program test'''

    psyir = fortran_reader.psyir_from_source(source)
    # Child 0 is the program
    loop = psyir.children[0].children[0]

    # We need to disable distributed_memory for the extraction to work:
    config = Config.get()
    monkeypatch.setattr(config, "distributed_memory", False)

    psyir_copy = psyir.copy()
    transformation.apply(loop)
    out = fortran_writer(psyir)
    assert 'PreDeclareVariable("dummy", dummy)' in out
    assert 'ProvideVariable("dummy", dummy)' in out

    # Now check that the user can overwrite 'COLLECT-ARRAY-SHAPE-READS'
    # =================================================================
    # Since the original tree was modified, we now use the copy:

    loop = psyir_copy.children[0].children[0]
    # Disable array-shape-reads, which means 'dummy' should then not
    # be in the list of input parameters anymore, and therefore
    transformation.apply(loop, options={"COLLECT-ARRAY-SHAPE-READS": False})
    out = fortran_writer(psyir_copy)
    # No reference to 'dummy' should be in the created code:
    assert 'PreDeclareVariable("dummy", dummy)' not in out
    assert 'ProvideVariable("dummy", dummy)' not in out
