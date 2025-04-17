# Copyright (c) 2018 Science and Technology Facilities Council

# All rights reserved.

# Modifications made as part of the fparser project are distributed
# under the following license:

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:

# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.

# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.

# 3. Neither the name of the copyright holder nor the names of its
# contributors may be used to endorse or promote products derived from
# this software without specific prior written permission.

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

"""Test Fortran 2003 rule R213 : This file tests support for the
Exectuable_Construct class.

As this class just uses Base to match with a choice of subclasses we
test that an instance of each subclass can be succesfully
parsed. Detailed checking of the subclass rules are performed by the
subclass tests.

"""

from fparser.two.Fortran2003 import Executable_Construct
from fparser.api import get_reader


def test_action_statement(f2003_create):
    """Test an action statement is supported by the executable construct
    class.

    """
    code = "a = 1.0"
    result = Executable_Construct(code)
    assert str(result) == code


def test_associate_construct(f2003_create):
    """Test an associate construct is supported by the executable
    construct class.

    """
    code = "associate(a => b)\n" "end associate"
    reader = get_reader(code)
    result = Executable_Construct(reader)
    assert str(result).lower() == code


def test_case_construct(f2003_create):
    """Test a case construct is supported by the executable construct
    class.

    """
    code = "select case (a)\n" "end select"
    reader = get_reader(code)
    result = Executable_Construct(reader)
    assert str(result).lower() == code


def test_do_construct(f2003_create):
    """Test a do construct is supported by the executable construct class."""
    code = "do i = 1, n\n" "end do"
    reader = get_reader(code)
    result = Executable_Construct(reader)
    assert str(result).lower() == code


def test_forall_construct(f2003_create):
    """Test a forall construct is supported by the executable construct
    class.

    """
    code = "forall(i = 1 : n)\n" "end forall"
    reader = get_reader(code)
    result = Executable_Construct(reader)
    assert str(result).lower() == code


def test_if_construct(f2003_create):
    """Test an if construct is supported by the executable construct
    class.

    """
    code = "if (a) then\n" "end if"
    reader = get_reader(code)
    result = Executable_Construct(reader)
    assert str(result).lower() == code


def test_select_type_construct(f2003_create):
    """Test a select_type construct is supported by the executable
    construct class.

    """
    code = "select type(a)\n" "end select"
    reader = get_reader(code)
    result = Executable_Construct(reader)
    assert str(result).lower() == code


def test_where_construct(f2003_create):
    """Test a where construct is supported by the executable construct
    class.

    """
    code = "where (a)\n" "end where"
    reader = get_reader(code)
    result = Executable_Construct(reader)
    assert str(result).lower() == code
