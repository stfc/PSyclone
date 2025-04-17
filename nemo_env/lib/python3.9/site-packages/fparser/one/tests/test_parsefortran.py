#!/usr/bin/env python
# -*- coding: utf-8 -*-
#############################################################################
# Modified work Copyright (c) 2017-2018 Science and Technology
# Facilities Council
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
##############################################################################
"""
Tests the fparser.one.parsefortran module.
"""

import pytest
import fparser.one.parsefortran
import fparser.common.readfortran


def test_log_empty(log):
    """
    Tests that a reader without next() method causes an event to be logged.
    """

    class EmptyReader:
        """
        A faux reader with no next() method.
        """

        id = "thingumy"

    unit_under_test = fparser.one.parsefortran.FortranParser(EmptyReader())
    unit_under_test.analyze()
    assert log.messages == {
        "debug": [],
        "info": ["Nothing to analyze."],
        "warning": [],
        "error": [],
        "critical": [],
    }
    unit_under_test.cache.clear()


def test_log_cache(log):
    """
    Tests that using a cached reader object logs an event.
    """

    class Readerlike:
        """
        Dummy reader class, the only purpose of which is to have an id and not
        cause the parser to fail.
        """

        id = "thisun"

        def next(self):
            """
            Simple non-failure-causing method.
            """
            yield "NOT A THING"
            raise StopIteration

    # Expect everything to go okay, no log messages.
    log.reset()
    parser = fparser.one.parsefortran.FortranParser(Readerlike())
    assert log.messages == {
        "debug": [],
        "info": [],
        "warning": [],
        "error": [],
        "critical": [],
    }

    # This time we should use a cached log.
    parser = fparser.one.parsefortran.FortranParser(Readerlike())
    assert log.messages == {
        "debug": [],
        "info": ["using cached thisun"],
        "warning": [],
        "error": [],
        "critical": [],
    }
    parser.cache.clear()


def test_log_failure(log, monkeypatch):
    """
    Tests that an unexpected read failure causes an event to be logged.
    """

    def faulty_next(self, ignore_comments=False):
        """
        Raies any old exception.
        """
        raise Exception("That" "s all folks!")

    monkeypatch.setattr(
        "fparser.common.readfortran.FortranStringReader.next", faulty_next
    )
    reader = fparser.common.readfortran.FortranStringReader("")
    unit_under_test = fparser.one.parsefortran.FortranParser(reader)
    with pytest.raises(Exception):
        unit_under_test.parse()
    assert log.messages["debug"][0].startswith("An error occurred during parsing.")
    assert log.messages["info"] == []
    assert log.messages["warning"] == []
    assert log.messages["error"] == []
    assert log.messages["critical"][0].startswith("While processing")
    assert log.messages["critical"][1] == "STOPPED PARSING"
    unit_under_test.cache.clear()


def test_pyf():
    """
    Tests inherited from implementation code.
    """
    string = """
python module foo
  interface tere
    subroutine bar
    real r
    end subroutine bar
  end interface tere
end python module foo
"""
    expected = [
        "  PYTHONMODULE foo",
        "    INTERFACE tere",
        "      SUBROUTINE bar()",
        "        REAL r",
        "      END SUBROUTINE bar",
        "    END INTERFACE tere",
        "  END PYTHONMODULE foo",
    ]

    reader = fparser.common.readfortran.FortranStringReader(string)
    reader.set_format(fparser.common.sourceinfo.FortranFormat.from_mode("pyf"))
    parser = fparser.one.parsefortran.FortranParser(reader)
    parser.parse()
    caught = parser.block.tofortran().splitlines()
    assert caught[0][:13] == "!BEGINSOURCE "
    assert caught[1:] == expected


def test_free90():
    """
    Tests inherited from implementation code.
    """
    string = """
module foo

   subroutine bar
    real r
    if ( pc_get_lun() .ne. 6) &
    write ( pc_get_lun(), '( &
    & /, a, /, " p=", i4, " stopping c_flag=", a, &
    & /, " print unit=", i8)') &
    trim(title), pcpsx_i_pel(), trim(c_flag), pc_get_lun()
    if (.true.) then
      call smth
    end if
    aaa : if (.false.) then
    else if (a) then
    else
    end if aaa
    hey = 1
    end subroutine bar
    abstract interface

    end interface

end module foo
"""
    expected = [
        "  MODULE foo",
        "    SUBROUTINE bar()",
        "      REAL r",
        "      IF (pc_get_lun() .ne. 6)"
        + ' WRITE (pc_get_lun(), \'(  /, A, /, " P = ", i4,'
        + ' " stopping c_flag=", a,  /, " print unit=", i8)\')'
        + " trim(title), pcpsx_i_pel(), trim(c_flag), pc_get_lun()",
        "      IF (.true.) THEN",
        "        CALL smth",
        "      END IF",
        "      aaa: IF (.false.) THEN",
        "      ELSE IF (a) THEN",
        "      ELSE",
        "      END IF aaa",
        "      hey = 1",
        "    END SUBROUTINE bar",
        "    ABSTRACT INTERFACE",
        "    END INTERFACE",
        "  END MODULE foo",
    ]

    reader = fparser.common.readfortran.FortranStringReader(string)
    mode = fparser.common.sourceinfo.FortranFormat.from_mode("free")
    reader.set_format(mode)
    parser = fparser.one.parsefortran.FortranParser(reader)
    parser.parse()
    caught = parser.block.tofortran().splitlines()
    assert caught[0][:13] == "!BEGINSOURCE "
    assert caught[1:] == expected


def test_module_procedure():
    """
    Tests a type that contains a procedure, and makes sure
    it has a module_procedures attribute
    """
    string = """
module foo
    type, public :: grid_type
   contains
        procedure :: get_tmask
   end type grid_type
end module foo
"""

    reader = fparser.common.readfortran.FortranStringReader(string)
    mode = fparser.common.sourceinfo.FortranFormat.from_mode("free")
    reader.set_format(mode)
    parser = fparser.one.parsefortran.FortranParser(reader)
    parser.parse()

    # Fails if the class Type does not have a "module_procedures" attribute
    parser.analyze()


def test_f77():
    """
    Tests inherited from implementation code.
    """
    string = """\
      program foo
      a = 3
      end
      subroutine bar
      end
      pure function foo(a)
      end
      pure real*4 recursive function bar()
      end
"""
    expected = [
        "        PROGRAM foo",
        "          a = 3",
        "        END PROGRAM foo",
        "        SUBROUTINE bar()",
        "        END SUBROUTINE bar",
        "        pure FUNCTION foo(a)",
        "        END FUNCTION foo",
        "        pure recursive REAL*4 FUNCTION bar()",
        "        END FUNCTION bar",
    ]

    reader = fparser.common.readfortran.FortranStringReader(string)
    parser = fparser.one.parsefortran.FortranParser(reader)
    parser.parse()
    assert isinstance(parser.block, fparser.one.block_statements.BeginSource)
    caught = parser.block.tofortran().splitlines()
    assert caught[0][:25] == "      !      BEGINSOURCE "
    assert caught[1:] == expected
