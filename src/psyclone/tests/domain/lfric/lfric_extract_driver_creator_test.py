# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council.
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
# Author J. Henrichs, Bureau of Meteorology

''' This module tests the driver creation for extracted kernels.'''

import pytest

from psyclone.domain.lfric import LFRicConstants, LFRicExtractDriverCreator
from psyclone.errors import InternalError

API = "dynamo0.3"


def test_lfric_driver_constructor():
    '''Tests the constructor of the LFRic driver creator.'''

    driver_creator = LFRicExtractDriverCreator()
    default_precision = {"i_def": "int32", "r_def": "real64",
                         "r_second": "real64", "r_solver": "real32",
                         "r_tran": "real32"}

    assert driver_creator._precision == default_precision

    # Check that we can modify the default precisions:
    new_precision = {"r_solver": "real64", "r_tran": "real64"}
    driver_creator = LFRicExtractDriverCreator(new_precision)
    assert (driver_creator._precision ==
            {"i_def": "int32", "r_def": "real64",
             "r_second": "real64", "r_solver": "real64",
             "r_tran": "real64"})


def test_lfric_driver_field_mapping():
    '''Tests that the mapping of fields to precision is as expected.'''
    mapping = LFRicConstants().DATA_TYPE_MAP
    correct = {}

    for field in ["columnwise_operator",
                  "field", "integer_field",
                  "operator", "r_solver_field",
                  "r_solver_operator", "r_tran_field"]:
        correct[mapping[field]["proxy_type"]] = mapping[field]["kind"]

    driver_creator = LFRicExtractDriverCreator()
    assert driver_creator._map_fields_to_precision == correct


def test_lfric_driver_constructor_error():
    '''Tests the error handling of the constructor of the LFRic driver
    creator.'''

    # Wrong argument type:
    with pytest.raises(InternalError) as err:
        _ = LFRicExtractDriverCreator(precision=1)
    assert ("The precision argument of the LFRic driver creator must be a "
            "dictionary, but got 'int'." in str(err.value))
