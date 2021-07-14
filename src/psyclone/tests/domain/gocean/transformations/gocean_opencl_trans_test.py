# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council
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
# Author S. Siso, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' Module containing tests for PSyclone GOcean OpenCL transformation.
'''

from __future__ import absolute_import

import pytest

from psyclone.domain.gocean.transformations import GOOpenCLTrans
from psyclone.psyir.transformations import TransformationError
from psyclone.tests.utilities import get_invoke

# API names
GOCEAN_API = "gocean1.0"


def test_transformation_name():
    ''' Check that the GOOpenCLTransformation returns the correct name'''
    trans = GOOpenCLTrans()
    assert trans.name == "GOOpenCLTrans"


def test_unsupported_api():
    ''' Check that attempting to apply an OpenCL transformation to a Dynamo
    InvokeSchedule raises the expected error. '''
    _, invoke = get_invoke("1_single_invoke.f90", "dynamo0.3",
                           name="invoke_0_testkern_type", dist_mem=False)
    sched = invoke.schedule
    trans = GOOpenCLTrans()
    with pytest.raises(TransformationError) as err:
        _ = trans.apply(sched)
    assert ("OpenCL generation is currently only supported for the GOcean "
            "API but got an InvokeSchedule of type:" in str(err.value))
