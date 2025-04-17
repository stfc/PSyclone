# Copyright (c) 2022 Science and Technology Facilities Council
#
# All rights reserved.
##
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.
#
# 3. Neither the name of the copyright holder nor the names of its
# contributors may be used to endorse or promote products derived from
# this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

"""Tests for the fparser2_bench script.

"""

import pytest
from fparser.scripts import fparser2_bench


def test_gen_sub():
    """Check the gen_sub() routine works as expected."""
    code = fparser2_bench.gen_sub(3)
    assert "subroutine g3(x)\n" in code
    assert "do i = 3, 12\n" in code
    assert "end subroutine\n" in code


def test_create_bench():
    """Check the create_bench() routine works as expected."""
    code = fparser2_bench.create_bench(3)
    assert "program bench3\n" in code
    assert code.count("end subroutine") == 3
    for idx in range(1, 4):
        assert f"subroutine g{idx}(x)" in code
    assert "end program" in code


def test_runner_invalid_num_routines():
    """Test the checking on the value of the supplied num_routines
    parameter."""
    with pytest.raises(ValueError) as err:
        fparser2_bench.runner(0)
    assert (
        "Number of routines to create must be a positive, non-zero "
        "integer but got: 0" in str(err.value)
    )


def test_runner(capsys):
    """Check that normal usage gives the expected benchmark output."""
    fparser2_bench.runner(3)
    stdout, stderr = capsys.readouterr()
    assert stderr == ""
    assert "Constructing benchmark code with 3 subroutines" in stdout
    assert "Time taken for parse =" in stdout
