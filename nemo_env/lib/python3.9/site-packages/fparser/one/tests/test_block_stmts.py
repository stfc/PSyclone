# -*- coding: utf-8 -*-
# Copyright (c) 2018 Science and Technology Facilities Council.
#
# All rights reserved.
#
# Modifications made as part of the fparser project are distributed
# under the following license:
#
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

"""
File containing tests for the one.block_statements module.
"""

import pytest

from fparser.common.utils import AnalyzeError
from fparser.common.sourceinfo import FortranFormat
from fparser.one.parsefortran import FortranParser
from fparser.common.readfortran import FortranStringReader


def test_get_type_by_name(monkeypatch):
    """Tests for HasImplicitStmt.get_type_by_name()."""
    from fparser.one.typedecl_statements import Real, Integer

    # We can't just create a HasImplicitStmt object so we get the parser
    # to create a module object as that sub-classes HasImplicitStmt (amongst
    # other things).
    string = """\
module some_block
end module some_block
"""
    reader = FortranStringReader(string)
    reader.set_format(FortranFormat(True, False))
    parser = FortranParser(reader)
    parser.parse()
    mod = parser.block.content[0]
    # Now we have a Module object, we can call get_type_by_name()...
    rtype = mod.get_type_by_name("a_real")
    assert isinstance(rtype, Real)
    itype = mod.get_type_by_name("i_int")
    assert isinstance(itype, Integer)
    # Check that we raise the correct error if we don't have any implicit
    # rules set
    monkeypatch.setattr(mod.a, "implicit_rules", None)
    with pytest.raises(AnalyzeError) as err:
        _ = mod.get_type_by_name("i_int")
    assert "Implicit rules mapping is null" in str(err.value)


def test_get_type_by_name_implicit():
    """Tests for HasImplicitStmt.get_type_by_name() when the source code
    contains IMPLICIT statements."""
    from fparser.one.typedecl_statements import Real, Integer

    # We can't just create a HasImplicitStmt object so we get the parser
    # to create a module object as that sub-classes HasImplicitStmt (amongst
    # other things).
    string = """\
module some_block
  implicit real (a-e)
  implicit integer (f-z)
end module some_block
"""
    reader = FortranStringReader(string)
    reader.set_format(FortranFormat(True, False))
    parser = FortranParser(reader)
    parser.parse()
    # Get the module object
    mod = parser.block.content[0]
    # We have to run the analyze method on the Implicit objects
    # produced by the parser in order to populate the implicit_rules
    # of the module.
    mod.content[0].analyze()
    mod.content[1].analyze()
    # Now we can call get_type_by_name()...
    rtype = mod.get_type_by_name("a_real")
    assert isinstance(rtype, Real)
    itype = mod.get_type_by_name("f_int")
    assert isinstance(itype, Integer)


def test_implicit_topyf(monkeypatch):
    """Tests for the topyf() method of HasImplicitStmt."""
    # We can't just create a HasImplicitStmt object so we get the parser
    # to create a module object as that sub-classes HasImplicitStmt (amongst
    # other things).
    string = """\
module some_block
  implicit real (a-e)
  implicit integer (f-z)
end module some_block
"""
    reader = FortranStringReader(string)
    reader.set_format(FortranFormat(True, False))
    parser = FortranParser(reader)
    parser.parse()
    # Get the module object
    mod = parser.block.content[0]
    code = mod.topyf()
    assert "! default IMPLICIT rules apply" not in code
    mod.content[0].analyze()
    mod.content[1].analyze()
    code = mod.topyf()
    assert "REAL (a, b, c, d, e)" in code
    assert "INTEGER (f, g, h" in code
    monkeypatch.setattr(mod.a, "implicit_rules", None)
    code = mod.topyf()
    assert "IMPLICIT NONE" in code
