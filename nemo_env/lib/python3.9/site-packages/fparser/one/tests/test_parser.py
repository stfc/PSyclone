# Modified work Copyright (c) 2017-2021 Science and Technology
# Facilities Council
# Original work Copyright (c) 1999-2008 Pearu Peterson

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

# --------------------------------------------------------------------

# The original software (in the f2py project) was distributed under
# the following license:

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:

#   a. Redistributions of source code must retain the above copyright notice,
#      this list of conditions and the following disclaimer.
#   b. Redistributions in binary form must reproduce the above copyright
#      notice, this list of conditions and the following disclaimer in the
#      documentation and/or other materials provided with the distribution.
#   c. Neither the name of the F2PY project nor the names of its
#      contributors may be used to endorse or promote products derived from
#      this software without specific prior written permission.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR
# ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
# DAMAGE.

# Original author: Pearu Peterson <pearu@cens.ioc.ee>
# First version created: May 2006

"""
Test parsing single Fortran lines.

"""

import pytest
from fparser.one.block_statements import (
    Allocatable,
    Allocate,
    ArithmeticIf,
    AssignedGoto,
    Assign,
    Assignment,
    Asynchronous,
    Backspace,
    Bind,
    Call,
    CallProtoArgument,
    CallStatement,
    Check,
    Close,
    Common,
    ComputedGoto,
    Contains,
    Continue,
    Cycle,
    Data,
    Deallocate,
    Depend,
    Dimension,
    Else,
    ElseIf,
    ElseWhere,
    Endfile,
    Entry,
    Enumerator,
    Equivalence,
    Exit,
    External,
    FinalBinding,
    Flush,
    Format,
    ForallStmt,
    FortranName,
    GenericBinding,
    Goto,
    Implicit,
    Import,
    Inquire,
    Intent,
    Intrinsic,
    ModuleProcedure,
    Nullify,
    Open,
    Optional,
    Parameter,
    Pointer,
    PointerAssignment,
    Pause,
    Print,
    Private,
    Protected,
    Public,
    Read,
    Return,
    Rewind,
    Save,
    Sequence,
    SpecificBinding,
    Stop,
    Target,
    Threadsafe,
    Use,
    Value,
    Volatile,
    Wait,
    WhereStmt,
    Write,
)
import fparser.common.sourceinfo
from fparser.one.typedecl_statements import (
    Byte,
    Character,
    Complex,
    DoubleComplex,
    DoublePrecision,
    Integer,
    Logical,
    Real,
)

from fparser.common.readfortran import FortranStringReader
from fparser.common.utils import AnalyzeError, ParseError


def parse(cls, line, label="", isfree=True, isstrict=False):
    """Tries to parse a Fortran line using the given class cls.
    If successful, it then converts the parsed statement back to
    a string. If isstrict is false, it will then try to parse
    this string again (recursively calling itself, with isstrict
    set to true) and make sure that the re-parsed string is
    identical to the input.
    It returns the string representation of the parsed input line.
    """

    if label:
        line = label + " : " + line
    reader = FortranStringReader(line)
    reader.set_format(fparser.common.sourceinfo.FortranFormat(isfree, isstrict))
    item = next(reader)
    if not cls.match(item.get_line()):
        raise ValueError("%r does not match %s pattern" % (line, cls.__name__))
    stmt = cls(item, item)
    if stmt.isvalid:
        # Check that we can successfully parse the string representation
        # of the parsed object
        stmt_string = str(stmt)
        if not isstrict:
            reparsed_stmt_string = parse(cls, stmt_string, isstrict=True)
            if stmt_string != reparsed_stmt_string:
                raise ValueError(
                    "Failed to parse %r with %s pattern in Pyf "
                    "mode, got %r" % (stmt_string, cls.__name__, reparsed_stmt_string)
                )
        return stmt_string
    raise ValueError("parsing %r with %s pattern failed" % (line, cls.__name__))


def test_assignment():
    """Test that assignment statements are parsed correctly."""
    assert parse(Assignment, "a=b") == "a = b"
    assert parse(PointerAssignment, "a=>b") == "a => b"
    assert parse(Assignment, "a (2)=b(n,m)") == "a(2) = b(n,m)"
    assert parse(Assignment, "a % 2(2,4)=b(a(i))") == "a%2(2,4) = b(a(i))"
    assert (
        parse(Assignment, "cu(i,j) = .5d0*(cu(i+1,j) + cu(i-1,j))")
        == "cu(i,j) = .5d0*(cu(i+1,j) + cu(i-1,j))"
    )


def test_assign():
    assert parse(Assign, "assign 10 to a") == "ASSIGN 10 TO a"


def test_call():
    assert parse(Call, "call a") == "CALL a"
    assert parse(Call, "call a()") == "CALL a"
    assert parse(Call, "call a(1)") == "CALL a(1)"
    assert parse(Call, "call a(1,2)") == "CALL a(1, 2)"
    assert parse(Call, "call a % c ( n , a+1 )") == "CALL a % c(n, a+1)"


def test_goto():
    assert parse(Goto, "go to 19") == "GO TO 19"
    assert parse(Goto, "goto 19") == "GO TO 19"
    assert parse(ComputedGoto, "goto (1, 2 ,3) a+b(2)") == "GO TO (1, 2, 3) a+b(2)"
    assert parse(ComputedGoto, "goto (1, 2 ,3) , a+b(2)") == "GO TO (1, 2, 3) a+b(2)"
    assert parse(AssignedGoto, "goto a") == "GO TO a"
    assert parse(AssignedGoto, "goto a ( 1 )") == "GO TO a (1)"
    assert parse(AssignedGoto, "goto a ( 1 ,2)") == "GO TO a (1, 2)"


def test_continue():
    assert parse(Continue, "continue") == "CONTINUE"


def test_return():
    assert parse(Return, "return") == "RETURN"
    assert parse(Return, "return a") == "RETURN a"
    assert parse(Return, "return a+1") == "RETURN a+1"
    assert parse(Return, "return a(c, a)") == "RETURN a(c, a)"


def test_stop():
    assert parse(Stop, "stop") == "STOP"
    assert parse(Stop, "stop 1") == "STOP 1"
    assert parse(Stop, 'stop "a"') == 'STOP "a"'
    assert parse(Stop, 'stop "a b"') == 'STOP "a b"'


def test_print():
    assert parse(Print, "print*") == "PRINT *"
    assert parse(Print, 'print "a b( c )"') == 'PRINT "a b( c )"'
    assert parse(Print, "print 12, a") == "PRINT 12, a"
    assert parse(Print, "print 12, a , b") == "PRINT 12, a, b"
    assert parse(Print, "print 12, a(c,1) , b") == "PRINT 12, a(c,1), b"


def test_read():
    assert parse(Read, "read ( 10 )") == "READ (10)"
    assert parse(Read, "read ( 10 ) a ") == "READ (10) a"
    assert parse(Read, "read ( 10 ) a , b") == "READ (10) a, b"
    assert parse(Read, "read *") == "READ *"
    assert parse(Read, "read 12") == "READ 12"
    assert parse(Read, 'read "a b"') == 'READ "a b"'
    assert parse(Read, 'read "a b",a') == 'READ "a b", a'
    assert parse(Read, "read * , a") == "READ *, a"
    assert parse(Read, 'read "hey a" , a') == 'READ "hey a", a'
    assert parse(Read, "read * , a  , b") == "READ *, a, b"
    assert parse(Read, "read ( unit  =10 )") == "READ (UNIT = 10)"


def test_write():
    assert parse(Write, "write ( 10 )") == "WRITE (10)"
    assert parse(Write, "write ( 10 , a )") == "WRITE (10, a)"
    assert parse(Write, "write ( 10 ) b") == "WRITE (10) b"
    assert parse(Write, "write ( 10 ) a(1) , b+2") == "WRITE (10) a(1), b+2"
    assert parse(Write, "write ( unit=10 )") == "WRITE (UNIT = 10)"


def test_flush():
    assert parse(Flush, "flush 10") == "FLUSH (10)"
    assert parse(Flush, "flush (10)") == "FLUSH (10)"
    assert parse(Flush, "flush (UNIT = 10)") == "FLUSH (UNIT = 10)"
    assert parse(Flush, "flush (10, err=  23)") == "FLUSH (10, ERR = 23)"


def test_wait():
    assert parse(Wait, "wait(10)") == "WAIT (10)"
    assert parse(Wait, "wait(10,err=129)") == "WAIT (10, ERR = 129)"


def test_contains():
    assert parse(Contains, "contains") == "CONTAINS"


def test_allocate():
    """Tests for various forms of ALLOCATE statement"""
    assert parse(Allocate, "allocate (a)") == "ALLOCATE (a)"
    assert parse(Allocate, "allocate (a, stat=b)") == "ALLOCATE (a, STAT = b)"
    assert parse(Allocate, "allocate (a,b(:1))") == "ALLOCATE (a, b(:1))"
    assert parse(Allocate, "allocate (real(8)::a)") == "ALLOCATE (REAL(KIND=8) :: a)"
    # With a type specifier
    assert parse(Allocate, "allocate (real :: a(8))") == "ALLOCATE (REAL :: a(8))"
    assert (
        parse(Allocate, "allocate (real(kind=wp) :: a(8))")
        == "ALLOCATE (REAL(KIND=wp) :: a(8))"
    )
    assert parse(Allocate, "allocate (a_type :: a)") == "ALLOCATE (a_type :: a)"
    with pytest.raises(ParseError) as err:
        parse(Allocate, "allocate(not valid :: a)")
    assert "Unrecognised type-specification in ALLOCATE statement" in str(err.value)


def test_deallocate():
    assert parse(Deallocate, "deallocate (a)") == "DEALLOCATE (a)"
    assert parse(Deallocate, "deallocate (a, stat=b)") == "DEALLOCATE (a, STAT = b)"


def test_moduleprocedure():
    """Test  [ MODULE ] PROCEDURE [::] <procedure-name-list>"""

    assert parse(ModuleProcedure, "Procedure a") == "MODULE PROCEDURE a"
    assert parse(ModuleProcedure, "procedure a , b") == "MODULE PROCEDURE a, b"
    assert parse(ModuleProcedure, "procedure :: a ") == "MODULE PROCEDURE a"
    assert parse(ModuleProcedure, "procedure :: a , b") == "MODULE PROCEDURE a, b"
    assert parse(ModuleProcedure, "ModuleProcedure a") == "MODULE PROCEDURE a"
    assert parse(ModuleProcedure, "module procedure a , b") == "MODULE PROCEDURE a, b"
    assert parse(ModuleProcedure, "module procedure :: a ") == "MODULE PROCEDURE a"
    assert (
        parse(ModuleProcedure, "module procedure :: a , b") == "MODULE PROCEDURE a, b"
    )
    assert parse(ModuleProcedure, "moduleprocedure::a,b") == "MODULE PROCEDURE a, b"


def test_access():
    assert parse(Public, "Public") == "PUBLIC"
    assert parse(Public, "public a") == "PUBLIC a"
    assert parse(Public, "public :: a") == "PUBLIC a"
    assert parse(Public, "public a,b,c") == "PUBLIC a, b, c"
    assert parse(Public, "public :: a(:,:)") == "PUBLIC a(:,:)"
    assert parse(Private, "private") == "PRIVATE"
    assert parse(Private, "private :: a") == "PRIVATE a"


def test_close():
    assert parse(Close, "close (12)") == "CLOSE (12)"
    assert parse(Close, "close (12, err=99)") == "CLOSE (12, ERR = 99)"
    assert parse(Close, "close (12, status = a(1,2))") == "CLOSE (12, STATUS = a(1,2))"


def test_class():
    """Check that we correctly parse and generate a class declaration"""
    from fparser.one.typedecl_statements import Class

    assert (
        parse(Class, "class(runtime_constants_type) :: a")
        == "CLASS(runtime_constants_type) a"
    )


def test_cycle():
    assert parse(Cycle, "cycle") == "CYCLE"
    assert parse(Cycle, "cycle ab") == "CYCLE ab"


def test_rewind():
    assert parse(Rewind, "rewind 1") == "REWIND (1)"
    assert parse(Rewind, "rewind (1)") == "REWIND (1)"
    assert parse(Rewind, "rewind (1, err =  123)") == "REWIND (1, ERR = 123)"


def test_backspace():
    assert parse(Backspace, "backspace 1") == "BACKSPACE (1)"
    assert parse(Backspace, "backspace (1)") == "BACKSPACE (1)"
    assert parse(Backspace, "backspace (1, err =  123)") == "BACKSPACE (1, ERR = 123)"


def test_endfile():
    assert parse(Endfile, "endfile 1") == "ENDFILE (1)"
    assert parse(Endfile, "endfile (1)") == "ENDFILE (1)"
    assert parse(Endfile, "endfile (1, err =  123)") == "ENDFILE (1, ERR = 123)"


def test_open():
    assert parse(Open, "open (1)") == "OPEN (1)"
    assert parse(Open, "open (1, err =  123)") == "OPEN (1, ERR = 123)"


def test_format():
    assert parse(Format, "1 format ()") == "1 FORMAT ()"
    assert parse(Format, "199 format (1)") == "199 FORMAT (1)"
    assert parse(Format, "2 format (1 , SS)") == "2 FORMAT (1, ss)"


def test_save():
    assert parse(Save, "save") == "SAVE"
    assert parse(Save, "save :: a") == "SAVE a"
    assert parse(Save, "save a,b") == "SAVE a, b"


def test_data():
    assert parse(Data, "data a /b/") == "DATA a / b /"
    assert parse(Data, "data a , c /b/") == "DATA a, c / b /"
    assert parse(Data, "data a /b ,c/") == "DATA a / b, c /"
    assert parse(Data, "data a /b/ c,e /d/") == "DATA a / b / c, e / d /"
    assert parse(Data, "data a(1,2) /b/") == "DATA a(1,2) / b /"
    assert parse(Data, "data a /b, c(1)/") == "DATA a / b, c(1) /"


def test_nullify():
    assert parse(Nullify, "nullify(a)") == "NULLIFY (a)"
    assert parse(Nullify, "nullify(a  ,b)") == "NULLIFY (a, b)"


def test_use():
    """Check that we can parse USE statement."""
    assert parse(Use, "use a") == "USE a"
    assert parse(Use, "use :: a") == "USE a"
    assert parse(Use, "use, intrinsic:: a") == "USE, INTRINSIC :: a"
    assert parse(Use, "use, non_intrinsic:: a") == "USE, NON_INTRINSIC :: a"
    assert (
        parse(Use, "use, non_intrinsic:: a , only: b")
        == "USE, NON_INTRINSIC :: a, ONLY: b"
    )
    assert parse(Use, "use :: a ,only: b") == "USE a, ONLY: b"
    assert parse(Use, "use :: a , only: b=>c") == "USE a, ONLY: b=>c"
    assert parse(Use, "use :: a , b=>c") == "USE a, b=>c"
    assert (
        parse(Use, "use :: a , only: operator(+) , b") == "USE a, ONLY: operator(+), b"
    )


def test_exit():
    assert parse(Exit, "exit") == "EXIT"
    assert parse(Exit, "exit ab") == "EXIT ab"


def test_parameter():
    assert parse(Parameter, "parameter (a = b(1,2))") == "PARAMETER (a = b(1,2))"
    assert (
        parse(Parameter, "parameter (a = b(1,2) , b=1)")
        == "PARAMETER (a = b(1,2), b=1)"
    )


def test_equivalence():
    assert parse(Equivalence, "equivalence (a , b)") == "EQUIVALENCE (a, b)"
    assert (
        parse(Equivalence, "equivalence (a , b) , ( c, d(1) , g  )")
        == "EQUIVALENCE (a, b), (c, d(1), g)"
    )


def test_dimension():
    assert parse(Dimension, "dimension a(b)") == "DIMENSION a(b)"
    assert parse(Dimension, "dimension::a(b)") == "DIMENSION a(b)"
    assert parse(Dimension, "dimension a(b)  , c(d)") == "DIMENSION a(b), c(d)"
    assert parse(Dimension, "dimension a(b,c)") == "DIMENSION a(b,c)"


def test_target():
    assert parse(Target, "target a(b)") == "TARGET a(b)"
    assert parse(Target, "target::a(b)") == "TARGET a(b)"
    assert parse(Target, "target a(b)  , c(d)") == "TARGET a(b), c(d)"
    assert parse(Target, "target a(b,c)") == "TARGET a(b,c)"


def test_pointer():
    assert parse(Pointer, "pointer a=b") == "POINTER a=b"
    assert parse(Pointer, "pointer :: a=b") == "POINTER a=b"
    assert parse(Pointer, "pointer a=b, c=d(1,2)") == "POINTER a=b, c=d(1,2)"


def test_protected():
    assert parse(Protected, "protected a") == "PROTECTED a"
    assert parse(Protected, "protected::a") == "PROTECTED a"
    assert parse(Protected, "protected a , b") == "PROTECTED a, b"


def test_volatile():
    assert parse(Volatile, "volatile a") == "VOLATILE a"
    assert parse(Volatile, "volatile::a") == "VOLATILE a"
    assert parse(Volatile, "volatile a , b") == "VOLATILE a, b"


def test_value():
    assert parse(Value, "value a") == "VALUE a"
    assert parse(Value, "value::a") == "VALUE a"
    assert parse(Value, "value a , b") == "VALUE a, b"


def test_arithmeticif():
    assert parse(ArithmeticIf, "if (a) 1,2,3") == "IF (a) 1, 2, 3"
    assert parse(ArithmeticIf, "if (a(1)) 1,2,3") == "IF (a(1)) 1, 2, 3"
    assert parse(ArithmeticIf, "if (a(1,2)) 1,2,3") == "IF (a(1,2)) 1, 2, 3"


def test_intrinsic():
    assert parse(Intrinsic, "intrinsic a") == "INTRINSIC a"
    assert parse(Intrinsic, "intrinsic::a") == "INTRINSIC a"
    assert parse(Intrinsic, "intrinsic a , b") == "INTRINSIC a, b"


def test_inquire():
    assert parse(Inquire, "inquire (1)") == "INQUIRE (1)"
    assert parse(Inquire, "inquire (1, err=123)") == "INQUIRE (1, ERR = 123)"
    assert parse(Inquire, "inquire (iolength=a) b") == "INQUIRE (IOLENGTH = a) b"
    assert (
        parse(Inquire, "inquire (iolength=a) b  ,c(1,2)")
        == "INQUIRE (IOLENGTH = a) b, c(1,2)"
    )


def test_sequence():
    assert parse(Sequence, "sequence") == "SEQUENCE"


def test_external():
    assert parse(External, "external a") == "EXTERNAL a"
    assert parse(External, "external::a") == "EXTERNAL a"
    assert parse(External, "external a , b") == "EXTERNAL a, b"


def test_common():
    assert parse(Common, "common a") == "COMMON a"
    assert parse(Common, "common a , b") == "COMMON a, b"
    assert parse(Common, "common a , b(1,2)") == "COMMON a, b(1,2)"
    assert parse(Common, "common // a") == "COMMON a"
    assert parse(Common, "common / name/ a") == "COMMON / name / a"
    assert parse(Common, "common / name/ a  , c") == "COMMON / name / a, c"
    assert (
        parse(Common, "common / name/ a /foo/ c(1) ,d")
        == "COMMON / name / a / foo / c(1), d"
    )
    assert (
        parse(Common, "common / name/ a, /foo/ c(1) ,d")
        == "COMMON / name / a / foo / c(1), d"
    )


def test_optional():
    assert parse(Optional, "optional a") == "OPTIONAL a"
    assert parse(Optional, "optional::a") == "OPTIONAL a"
    assert parse(Optional, "optional a , b") == "OPTIONAL a, b"


def test_intent():
    assert parse(Intent, "intent (in) a") == "INTENT (IN) a"
    assert parse(Intent, "intent(in)::a") == "INTENT (IN) a"
    assert parse(Intent, "intent(in) a , b") == "INTENT (IN) a, b"
    assert parse(Intent, "intent (in, out) a") == "INTENT (IN, OUT) a"


def test_entry():
    assert parse(Entry, "entry a") == "ENTRY a"
    assert parse(Entry, "entry a()") == "ENTRY a"
    assert parse(Entry, "entry a(b)") == "ENTRY a (b)"
    assert parse(Entry, "entry a(b,*)") == "ENTRY a (b, *)"
    assert (
        parse(Entry, 'entry a bind(c , name="a b")') == 'ENTRY a BIND (C, NAME = "a b")'
    )
    assert parse(Entry, "entry a result (b)") == "ENTRY a RESULT (b)"
    assert parse(Entry, "entry a bind(d) result (b)") == "ENTRY a RESULT (b) BIND (D)"
    assert parse(Entry, "entry a result (b) bind( c )") == "ENTRY a RESULT (b) BIND (C)"
    assert parse(Entry, "entry a(b,*) result (g)") == "ENTRY a (b, *) RESULT (g)"


def test_import():
    assert parse(Import, "import") == "IMPORT"
    assert parse(Import, "import a") == "IMPORT a"
    assert parse(Import, "import::a") == "IMPORT a"
    assert parse(Import, "import a , b") == "IMPORT a, b"


def test_forall():
    assert (
        parse(ForallStmt, "forall (i = 1:n(k,:) : 2) a(i) = i*i*b(i)")
        == "FORALL (i = 1 : n(k,:) : 2) a(i) = i*i*b(i)"
    )
    assert (
        parse(ForallStmt, "forall (i=1:n,j=2:3) a(i) = b(i,i)")
        == "FORALL (i = 1 : n, j = 2 : 3) a(i) = b(i,i)"
    )
    assert (
        parse(ForallStmt, "forall (i=1:n,j=2:3, 1+a(1,2)) a(i) = b(i,i)")
        == "FORALL (i = 1 : n, j = 2 : 3, 1+a(1,2)) a(i) = b(i,i)"
    )


def test_specificbinding():
    assert parse(SpecificBinding, "procedure a") == "PROCEDURE a"
    assert parse(SpecificBinding, "procedure :: a") == "PROCEDURE a"
    assert (
        parse(SpecificBinding, "procedure , NOPASS :: a") == "PROCEDURE , NOPASS :: a"
    )
    assert (
        parse(SpecificBinding, "procedure , public, pass(x ) :: a")
        == "PROCEDURE , PUBLIC, PASS (x) :: a"
    )
    assert parse(SpecificBinding, "procedure(n) a") == "PROCEDURE (n) a"
    assert (
        parse(SpecificBinding, "procedure(n), pass :: a") == "PROCEDURE (n) , PASS :: a"
    )
    assert parse(SpecificBinding, "procedure(n) :: a") == "PROCEDURE (n) a"
    assert parse(SpecificBinding, "procedure a= >b") == "PROCEDURE a => b"
    assert (
        parse(SpecificBinding, "procedure(n),pass :: a =>c")
        == "PROCEDURE (n) , PASS :: a => c"
    )


def test_genericbinding():
    assert parse(GenericBinding, "generic :: a=>b") == "GENERIC :: a => b"
    assert (
        parse(GenericBinding, "generic, public :: a=>b") == "GENERIC, PUBLIC :: a => b"
    )
    assert (
        parse(GenericBinding, "generic, public :: a(1,2)=>b ,c")
        == "GENERIC, PUBLIC :: a(1,2) => b, c"
    )


def test_finalbinding():
    assert parse(FinalBinding, "final a") == "FINAL a"
    assert parse(FinalBinding, "final::a") == "FINAL a"
    assert parse(FinalBinding, "final a , b") == "FINAL a, b"


def test_allocatable():
    assert parse(Allocatable, "allocatable a") == "ALLOCATABLE a"
    assert parse(Allocatable, "allocatable :: a") == "ALLOCATABLE a"
    assert parse(Allocatable, "allocatable a (1,2)") == "ALLOCATABLE a (1,2)"
    assert parse(Allocatable, "allocatable a (1,2) ,b") == "ALLOCATABLE a (1,2), b"


def test_asynchronous():
    assert parse(Asynchronous, "asynchronous a") == "ASYNCHRONOUS a"
    assert parse(Asynchronous, "asynchronous::a") == "ASYNCHRONOUS a"
    assert parse(Asynchronous, "asynchronous a , b") == "ASYNCHRONOUS a, b"


def test_bind():
    assert parse(Bind, "bind(c) a") == "BIND (C) a"
    assert parse(Bind, "bind(c) :: a") == "BIND (C) a"
    assert parse(Bind, "bind(c) a ,b") == "BIND (C) a, b"
    assert parse(Bind, "bind(c) /a/") == "BIND (C) / a /"
    assert parse(Bind, "bind(c) /a/ ,b") == "BIND (C) / a /, b"
    assert parse(Bind, 'bind(c,name="hey") a') == 'BIND (C, NAME = "hey") a'


def test_else():
    assert parse(Else, "else") == "ELSE"
    assert parse(ElseIf, "else if (a) then") == "ELSE IF (a) THEN"
    assert parse(ElseIf, "else if (a.eq.b(1,2)) then") == "ELSE IF (a.eq.b(1,2)) THEN"


def test_where():
    assert parse(WhereStmt, "where (1) a=1") == "WHERE ( 1 ) a = 1"
    assert parse(WhereStmt, "where (a(1,2)) a=1") == "WHERE ( a(1,2) ) a = 1"


def test_elsewhere():
    assert parse(ElseWhere, "else where") == "ELSE WHERE"
    assert parse(ElseWhere, "elsewhere (1)") == "ELSE WHERE ( 1 )"
    assert parse(ElseWhere, "elsewhere(a(1,2))") == "ELSE WHERE ( a(1,2) )"


def test_enumerator():
    assert parse(Enumerator, "enumerator a") == "ENUMERATOR a"
    assert parse(Enumerator, "enumerator:: a") == "ENUMERATOR a"
    assert parse(Enumerator, "enumerator a,b") == "ENUMERATOR a, b"
    assert parse(Enumerator, "enumerator a=1") == "ENUMERATOR a=1"
    assert parse(Enumerator, "enumerator a=1 , b=c(1,2)") == "ENUMERATOR a=1, b=c(1,2)"


def test_fortranname():
    assert parse(FortranName, "fortranname a") == "FORTRANNAME a"


def test_threadsafe():
    assert parse(Threadsafe, "threadsafe") == "THREADSAFE"


def test_depend():
    assert parse(Depend, "depend( a) b") == "DEPEND ( a ) b"
    assert parse(Depend, "depend( a) ::b") == "DEPEND ( a ) b"
    assert parse(Depend, "depend( a,c) b,e") == "DEPEND ( a, c ) b, e"


def test_check():
    assert parse(Check, "check(1) a") == "CHECK ( 1 ) a"
    assert parse(Check, "check(1) :: a") == "CHECK ( 1 ) a"
    assert parse(Check, "check(b(1,2)) a") == "CHECK ( b(1,2) ) a"
    assert parse(Check, "check(a>1) :: a") == "CHECK ( a>1 ) a"


def test_callstatement():
    assert (
        parse(CallStatement, "callstatement (*func)()", isstrict=1)
        == "CALLSTATEMENT (*func)()"
    )
    assert (
        parse(CallStatement, "callstatement i=1;(*func)()", isstrict=1)
        == "CALLSTATEMENT i=1;(*func)()"
    )


def test_callprotoargument():
    assert (
        parse(CallProtoArgument, "callprotoargument int(*), double")
        == "CALLPROTOARGUMENT int(*), double"
    )


def test_pause():
    assert parse(Pause, "pause") == "PAUSE"
    assert parse(Pause, "pause 1") == "PAUSE 1"
    assert parse(Pause, 'pause "hey"') == 'PAUSE "hey"'
    assert parse(Pause, 'pause "hey pa"') == 'PAUSE "hey pa"'


def test_byte():
    """Tests various declarations of byte variables."""
    assert parse(Byte, "byte") == "BYTE"
    assert parse(Byte, "byte a") == "BYTE a"
    assert parse(Byte, "byte, a") == "BYTE a"
    assert parse(Byte, "byte a ,b") == "BYTE a, b"
    assert parse(Byte, "byte :: a ,b") == "BYTE a, b"
    assert parse(Byte, "byte a(1,2)") == "BYTE a(1,2)"
    assert parse(Byte, "byte :: a(1,2),b") == "BYTE a(1,2), b"
    assert parse(Byte, "byte external :: a") == "BYTE, external :: a"
    assert parse(Byte, "byte, external :: a") == "BYTE, external :: a"
    assert (
        parse(Byte, "byte external , intent(in) :: a")
        == "BYTE, external, intent(in) :: a"
    )


def test_integer():
    """Tests various declarations of integer variables."""
    assert parse(Integer, "integer") == "INTEGER"
    assert parse(Integer, "integer*4") == "INTEGER*4"
    assert parse(Integer, "integer*4 a") == "INTEGER*4 a"
    assert parse(Integer, "integer*4, a") == "INTEGER*4 a"
    assert parse(Integer, "integer*4 a ,b") == "INTEGER*4 a, b"
    assert parse(Integer, "integer*4 :: a ,b") == "INTEGER*4 a, b"
    assert parse(Integer, "integer*4 a(1,2)") == "INTEGER*4 a(1,2)"
    assert parse(Integer, "integer*4 a(2*(1+2)-1)") == "INTEGER*4 a(2*(1+2)-1)"
    assert parse(Integer, "integer*4 :: a(1,2),b") == "INTEGER*4 a(1,2), b"
    assert parse(Integer, "integer*4 external :: a") == "INTEGER*4, external :: a"
    assert parse(Integer, "integer*4, external :: a") == "INTEGER*4, external :: a"
    assert (
        parse(Integer, "integer*4 external , intent(in) :: a")
        == "INTEGER*4, external, intent(in) :: a"
    )
    assert parse(Integer, "integer(kind=4)") == "INTEGER(KIND=4)"
    assert parse(Integer, "integer ( kind = 4)") == "INTEGER(KIND=4)"
    assert parse(Integer, "integer(kind=2+2)") == "INTEGER(KIND=2+2)"
    assert parse(Integer, "integer(kind=f(4,5))") == "INTEGER(KIND=f(4,5))"
    assert parse(Integer, "integer a = 5") == "INTEGER :: a = 5"


def test_const_array_decl():
    """Tests declarations that set an initial value to an array."""
    assert parse(Integer, "integer a(1) = (/1/)") == "INTEGER :: a(1) = (/1/)"
    assert (
        parse(Real, "real a(3) = (/1.1, 2.2, 3.3/)")
        == "REAL :: a(3) = (/1.1, 2.2, 3.3/)"
    )
    assert (
        parse(Logical, "logical :: a(3) = (/true, false, true/)")
        == "LOGICAL :: a(3) = (/true, false, true/)"
    )
    assert parse(Integer, "integer a(1) = [1]") == "INTEGER :: a(1) = [1]"
    assert parse(Integer, "integer a(3) = [1, 2, 3]") == "INTEGER :: a(3) = [1, 2, 3]"


def test_logical():
    """Tests various declarations of logical variables."""
    assert parse(Logical, "logical") == "LOGICAL"
    assert parse(Logical, "logical*4") == "LOGICAL*4"
    assert parse(Logical, "logical*4 a") == "LOGICAL*4 a"
    assert parse(Logical, "logical*4, a") == "LOGICAL*4 a"
    assert parse(Logical, "logical*4 a ,b") == "LOGICAL*4 a, b"
    assert parse(Logical, "logical*4 :: a ,b") == "LOGICAL*4 a, b"
    assert parse(Logical, "logical*4 a(1,2)") == "LOGICAL*4 a(1,2)"
    assert parse(Logical, "logical*4 :: a(1,2),b") == "LOGICAL*4 a(1,2), b"
    assert parse(Logical, "logical*4 external :: a") == "LOGICAL*4, external :: a"
    assert parse(Logical, "logical*4, external :: a") == "LOGICAL*4, external :: a"
    assert (
        parse(Logical, "logical*4 external , intent(in) :: a")
        == "LOGICAL*4, external, intent(in) :: a"
    )
    assert parse(Logical, "logical(kind=4)") == "LOGICAL(KIND=4)"
    assert parse(Logical, "logical ( kind = 4)") == "LOGICAL(KIND=4)"
    assert parse(Logical, "logical(kind=2+2)") == "LOGICAL(KIND=2+2)"
    assert parse(Logical, "logical(kind=f(4,5))") == "LOGICAL(KIND=f(4,5))"


def test_real():
    """Test various declarations of real variables."""

    assert parse(Real, "real") == "REAL"
    assert parse(Real, "real*4") == "REAL*4"
    assert parse(Real, "real*4 a") == "REAL*4 a"
    assert parse(Real, "real*4, a") == "REAL*4 a"
    assert parse(Real, "real*4 a ,b") == "REAL*4 a, b"
    assert parse(Real, "real*4 :: a ,b") == "REAL*4 a, b"
    assert parse(Real, "real*4 a(1,2)") == "REAL*4 a(1,2)"
    assert parse(Real, "real*4 :: a(1,2),b") == "REAL*4 a(1,2), b"
    assert parse(Real, "real*4 external :: a") == "REAL*4, external :: a"
    assert parse(Real, "real*4, external :: a") == "REAL*4, external :: a"
    assert (
        parse(Real, "real*4 external , intent(in) :: a")
        == "REAL*4, external, intent(in) :: a"
    )
    assert parse(Real, "real(kind=4)") == "REAL(KIND=4)"
    assert parse(Real, "real ( kind = 4)") == "REAL(KIND=4)"
    assert parse(Real, "real(kind=2+2)") == "REAL(KIND=2+2)"
    assert parse(Real, "real(kind=f(4,5))") == "REAL(KIND=f(4,5))"


def test_double_precision():
    """Test various declarations of double precision variables."""

    assert parse(DoublePrecision, "doubleprecision") == "DOUBLEPRECISION"
    assert parse(DoublePrecision, "double precision") == "DOUBLEPRECISION"
    assert parse(DoublePrecision, "doubleprecision a") == "DOUBLEPRECISION a"
    assert parse(DoublePrecision, "double precision, a") == "DOUBLEPRECISION a"
    assert parse(DoublePrecision, "doubleprecision a ,b") == "DOUBLEPRECISION a, b"
    assert parse(DoublePrecision, "double precision :: a ,b") == "DOUBLEPRECISION a, b"
    assert parse(DoublePrecision, "doubleprecision a(1,2)") == "DOUBLEPRECISION a(1,2)"
    assert (
        parse(DoublePrecision, "double precision :: a(1,2),b")
        == "DOUBLEPRECISION a(1,2), b"
    )
    assert (
        parse(DoublePrecision, "doubleprecision external :: a")
        == "DOUBLEPRECISION, external :: a"
    )
    assert (
        parse(DoublePrecision, "double precision, external :: a")
        == "DOUBLEPRECISION, external :: a"
    )
    assert (
        parse(DoublePrecision, "double precision external , intent(in) :: a")
        == "DOUBLEPRECISION, external, intent(in) :: a"
    )


def test_complex():
    """Test various declarations of complex variables."""

    assert parse(Complex, "complex") == "COMPLEX"
    assert parse(Complex, "complex") == "COMPLEX"
    assert parse(Complex, "complex*8 a") == "COMPLEX*8 a"
    assert parse(Complex, "complex*16, a") == "COMPLEX*16 a"
    assert parse(Complex, "complex a ,b") == "COMPLEX a, b"
    assert parse(Complex, "complex :: a ,b") == "COMPLEX a, b"
    assert parse(Complex, "complex a(1,2)") == "COMPLEX a(1,2)"
    assert parse(Complex, "complex :: a(1,2),b") == "COMPLEX a(1,2), b"
    assert parse(Complex, "complex external :: a") == "COMPLEX, external :: a"
    assert parse(Complex, "complex, external :: a") == "COMPLEX, external :: a"
    assert (
        parse(Complex, "complex external , intent(in) :: a")
        == "COMPLEX, external, intent(in) :: a"
    )
    assert parse(Complex, "complex(kind=8)") == "COMPLEX(KIND=8)"
    assert parse(Complex, "complex ( kind = 16)") == "COMPLEX(KIND=16)"
    assert parse(Complex, "complex(kind=4+4)") == "COMPLEX(KIND=4+4)"
    assert parse(Complex, "complex(kind=f(4,5))") == "COMPLEX(KIND=f(4,5))"


def test_double_complex():
    """Test various declarations of double complex variables."""

    assert parse(DoubleComplex, "double complex") == "DOUBLECOMPLEX"
    assert parse(DoubleComplex, "double complex") == "DOUBLECOMPLEX"
    assert parse(DoubleComplex, "double complex a") == "DOUBLECOMPLEX a"
    assert parse(DoubleComplex, "double complex, a") == "DOUBLECOMPLEX a"
    assert parse(DoubleComplex, "double complex a ,b") == "DOUBLECOMPLEX a, b"
    assert parse(DoubleComplex, "double complex :: a ,b") == "DOUBLECOMPLEX a, b"
    assert parse(DoubleComplex, "double complex a(1,2)") == "DOUBLECOMPLEX a(1,2)"
    assert (
        parse(DoubleComplex, "double complex :: a(1,2),b") == "DOUBLECOMPLEX a(1,2), b"
    )
    assert (
        parse(DoubleComplex, "double complex external :: a")
        == "DOUBLECOMPLEX, external :: a"
    )
    assert (
        parse(DoubleComplex, "double complex, external :: a")
        == "DOUBLECOMPLEX, external :: a"
    )
    assert (
        parse(DoubleComplex, "double complex external , intent(in) :: a")
        == "DOUBLECOMPLEX, external, intent(in) :: a"
    )
    assert parse(DoubleComplex, "double complex(kind=4)") == "DOUBLECOMPLEX(KIND=4)"
    assert parse(DoubleComplex, "double complex ( kind = 4)") == "DOUBLECOMPLEX(KIND=4)"
    assert parse(DoubleComplex, "double complex(kind=2+2)") == "DOUBLECOMPLEX(KIND=2+2)"
    assert (
        parse(DoubleComplex, "double complex(kind=f(4,5))")
        == "DOUBLECOMPLEX(KIND=f(4,5))"
    )


def test_character():
    assert parse(Character, "character") == "CHARACTER"
    assert parse(Character, "character*2") == "CHARACTER(LEN=2)"
    assert parse(Character, "character**") == "CHARACTER(LEN=*)"
    assert parse(Character, "character*(2)") == "CHARACTER(LEN=2)"
    assert parse(Character, "character*(len =2)") == "CHARACTER(LEN=2)"
    assert parse(Character, "character*(len =2)") == "CHARACTER(LEN=2)"
    assert parse(Character, "character*(len =:)") == "CHARACTER(LEN=:)"
    assert parse(Character, "character(len =2)") == "CHARACTER(LEN=2)"
    assert parse(Character, "character(2)") == "CHARACTER(LEN=2)"
    assert parse(Character, "character(kind=2)") == "CHARACTER(KIND=2)"
    assert parse(Character, "character(kind=2,len=3)") == "CHARACTER(LEN=3, KIND=2)"
    assert parse(Character, "character(lEN=3,kind=2)") == "CHARACTER(LEN=3, KIND=2)"
    assert (
        parse(Character, "character(len=3,kind=2)", isstrict=True)
        == "CHARACTER(LEN=3, KIND=2)"
    )
    assert (
        parse(Character, "chaRACTER(len=3,kind=fA(1,2))", isstrict=True)
        == "CHARACTER(LEN=3, KIND=fA(1,2))"
    )
    assert (
        parse(Character, "character(len=3,kind=fA(1,2))")
        == "CHARACTER(LEN=3, KIND=fa(1,2))"
    )


def test_implicit():
    assert parse(Implicit, "implicit none") == "IMPLICIT NONE"
    assert parse(Implicit, "implicit") == "IMPLICIT NONE"
    assert parse(Implicit, "implicit integer (i-m)") == "IMPLICIT INTEGER ( i-m )"
    assert (
        parse(Implicit, "implicit integer (i-m,p,q-r)")
        == "IMPLICIT INTEGER ( i-m, p, q-r )"
    )
    assert (
        parse(Implicit, "implicit integer (i-m), real (z)")
        == "IMPLICIT INTEGER ( i-m ), REAL ( z )"
    )


def test_type_bound_array_access():
    """Check that we can parse code that calls a type-bound procedure
    on the element of an array (of derived types)"""
    parsed_code = parse(Call, "call an_array(idx)%a_func(arg)")
    assert parsed_code == "CALL an_array(idx)%a_func(arg)"
    # Routine being called has no arguments - it is valid Fortran to
    # omit the parentheses entirely in this case
    parsed_code = parse(Call, "call an_array(idx)%a_func()")
    assert parsed_code == "CALL an_array(idx)%a_func"
    parsed_code = parse(Call, "call an_array(idx)%a_func")
    assert parsed_code == "CALL an_array(idx)%a_func"
    # Perversely put in a character string arg containing '('
    parsed_code = parse(Call, 'call an_array(idx)%a_func("(")')
    assert parsed_code == 'CALL an_array(idx)%a_func("(")'
    # Pass an array element as an argument
    parsed_code = parse(Call, "call an_array(idx)%a_func(b(3))")
    assert parsed_code == "CALL an_array(idx)%a_func(b(3))"


def test_invalid_type_bound_array_access():  # pylint: disable=invalid-name
    """Check that a call to a type-bound procedure with incorrect
    syntax is flagged as invalid"""
    with pytest.raises(ValueError) as excinfo:
        _ = parse(Call, "call an_array(idx)%a_func)")
    assert "with Call pattern failed" in str(excinfo.value)
    with pytest.raises(ValueError) as excinfo:
        _ = parse(Call, "call an_array(idx)%)")
    assert "with Call pattern failed" in str(excinfo.value)


def test_analyze_errors():
    """Tests that AnalyzeErrors are raised as expected. It also tests
    for various calling-sequence issues, e.g. parsing or analyzing twice,
    or calling analyze() without calling parse() first."""
    from fparser import api

    source_str = """none subroutine test()
      end
    """
    with pytest.raises(AnalyzeError) as error:
        _ = api.parse(source_str, isfree=True, isstrict=False)
    assert "no parse pattern found" in str(error.value)

    source_str = """subroutine test()
        incorrect :: c
      end
    """
    with pytest.raises(AnalyzeError) as error:
        _ = api.parse(source_str, isfree=False, isstrict=False)
    assert "no parse pattern found" in str(error.value)

    source_str = """subroutine test()
      end
    """
    from fparser.one.parsefortran import FortranParser

    reader = api.get_reader(source_str)
    parser = FortranParser(reader)

    # Handle analyze before parsing (does not raise an exception atm)
    parser.analyze()
    # Cover case that parsing is called twice
    parser.parse()
    parser.parse()

    # Check if analyse is called twice
    parser.analyze()
    parser.analyze()
