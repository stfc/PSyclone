# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council
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
# Author R. W. Ford, STFC Daresbury Lab

'''Module containing tests for the EvaluatorTargetsMetadata class.

'''
import pytest

from fparser.two import Fortran2003

from psyclone.domain.lfric.kernel import EvaluatorTargetsMetadata


def test_init():
    '''Test that an instance of EvaluatorTargetsMetadata can be created
    and that its initial values are stored as expected.

    '''
    values = ["w0", "w1"]
    metadata = EvaluatorTargetsMetadata(values)
    assert isinstance(metadata, EvaluatorTargetsMetadata)
    assert metadata._evaluator_targets == [value.lower() for value in values]


def test_init_error():
    '''Test that invalid input to the constructor causes the expected
    exception to be raised.

    '''
    with pytest.raises(TypeError) as info:
        _ = EvaluatorTargetsMetadata(None)
    assert ("EvaluatorTargetsMetadata values should be provided as a list but "
            "found 'NoneType'." in str(info.value))


def test_fortran_string():
    '''Test that the fortran_string method works as expected.'''

    values = ["w0", "w1", "w2", "w3"]
    metadata = EvaluatorTargetsMetadata(values)
    fortran_string = metadata.fortran_string()
    expected = (
        "INTEGER :: GH_EVALUATOR_TARGETS(4) = (/w0, w1, w2, w3/)\n")
    assert fortran_string == expected


def test_create_from_fortran_string():
    '''Test that the create_from_fortran_string method works as
    expected.

    '''
    fortran_string = "integer :: gh_evaluator_targets(2) = (/wtheta, wchi/)"
    metadata = EvaluatorTargetsMetadata.create_from_fortran_string(
        fortran_string)
    assert metadata.evaluator_targets == ["wtheta", "wchi"]


@pytest.mark.parametrize("fortran_string, expected_list", [
    ("INTEGER, dimension(2) :: gh_evaluator_targets = (/w2, w3/)",
     ["w2", "w3"]),
    ("INTEGER :: gh_evaluator_targets(2) = (/w0, w1/)",
     ["w0", "w1"])])
def test_create_from_fparser2(fortran_string, expected_list):
    '''Test that the create_from_fparser2 method works as expected.'''
    fparser2_tree = EvaluatorTargetsMetadata.create_fparser2(
        fortran_string, Fortran2003.Data_Component_Def_Stmt)
    metadata = EvaluatorTargetsMetadata.create_from_fparser2(fparser2_tree)
    assert isinstance(metadata, EvaluatorTargetsMetadata)
    assert metadata.evaluator_targets == expected_list


def test_setter_getter():
    '''Test that the setters and getters work as expected.'''
    values = ["w0", "w2broken"]
    metadata = EvaluatorTargetsMetadata(values)
    assert metadata.evaluator_targets == values
    # Check that the getter makes a copy of the list
    assert metadata.evaluator_targets is not metadata._evaluator_targets

    values = ["w1", "w2"]
    metadata.evaluator_targets = values
    assert metadata._evaluator_targets == values
    # Check that the setter makes a copy of the list
    assert metadata._evaluator_targets is not values

    # Test that the setter lowers the input values
    values = ["W1", "WCHI"]
    metadata.evaluator_targets = values
    assert metadata._evaluator_targets == [value.lower() for value in values]


def test_setter_errors():
    '''Test that the setter raises the expected exceptions.'''

    values = ["w3", "w2"]
    metadata = EvaluatorTargetsMetadata(values)

    with pytest.raises(TypeError) as info:
        metadata.evaluator_targets = "invalid"
    assert ("EvaluatorTargetsMetadata values should be provided as a list "
            "but found 'str'." in str(info.value))

    with pytest.raises(TypeError) as info:
        metadata.evaluator_targets = []
    assert ("The EvaluatorTargetsMetadata list should contain at least one "
            "entry, but it is empty." in str(info.value))

    with pytest.raises(TypeError) as info:
        metadata.evaluator_targets = [None]
    assert ("The EvaluatorTargetsMetadata list should be a list containing "
            "objects of type str but found 'None', which is of type "
            "'NoneType'." in str(info.value))

    with pytest.raises(ValueError) as info:
        metadata.evaluator_targets = ["invalid"]
    assert ("The 'evaluator_targets' metadata should be a recognised value "
            "(one of ['w3', 'wtheta', 'w2v', 'w2vtrace', 'w2broken', 'w0', "
            "'w1', 'w2', 'w2trace', 'w2h', 'w2htrace', 'any_w2', 'wchi']) "
            "but found 'invalid'." in str(info.value))
