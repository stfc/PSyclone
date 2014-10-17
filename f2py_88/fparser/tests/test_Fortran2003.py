from fparser.Fortran2003 import *
from fparser.api import get_reader

from nose.tools import assert_equal

def assertRaises(exc, cls, s):
    try:
        cls(s)
        raise AssertionError('Expected %s but got nothing' % exc)
    except exc:
        pass


###############################################################################
############################### SECTION  2 ####################################
###############################################################################

def test_Program(): # R201

        cls = Program
        reader = get_reader('''\
      subroutine foo
      end subroutine foo
      subroutine bar
      end
      ''')
        a = cls(reader)
        assert isinstance(a, cls),`a`
        assert_equal(str(a), 'SUBROUTINE foo\nEND SUBROUTINE foo\nSUBROUTINE bar\nEND SUBROUTINE bar')

        reader = get_reader('''\
      subroutine foo (*)
      end subroutine foo
      ''')
        a = cls(reader)
        assert isinstance(a, cls),`a`
        assert_equal(str(a), 'SUBROUTINE foo(*)\nEND SUBROUTINE foo')

def test_Specification_Part(): # R204

    reader = get_reader('''\
    integer a''')
    cls = Specification_Part
    a = cls(reader)
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'INTEGER :: a')
    assert_equal(repr(a), "Specification_Part(Type_Declaration_Stmt(Intrinsic_Type_Spec('INTEGER', None), None, Entity_Decl(Name('a'), None, None, None)))")

    a = cls(get_reader('''
type a
end type a
type b
end type b
    '''))
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'TYPE :: a\nEND TYPE a\nTYPE :: b\nEND TYPE b')

###############################################################################
############################### SECTION  3 ####################################
###############################################################################

def test_Name(): # R304

        a = Name('a')
        assert isinstance(a,Name),`a`
        a = Name('a2')
        assert isinstance(a,Name),`a`
        a = Designator('a')
        assert isinstance(a,Name),`a`
        a = Constant('a')
        assert isinstance(a,Name),`a`
        a = Expr('a')
        assert isinstance(a,Name),`a`

def test_Literal_Constant(): # R305

    cls = Constant
    a = cls('.false.')
    assert isinstance(a, Logical_Literal_Constant), `a`
    assert str(a)=='.FALSE.'

def test_Literal_Constant(): # R306

    cls = Literal_Constant
    a = cls('.false.')
    assert isinstance(a, Logical_Literal_Constant), `a`
    assert str(a)=='.FALSE.'

###############################################################################
############################### SECTION  4 ####################################
###############################################################################

def test_Type_Param_Value(): # 402

        cls = Type_Param_Value
        a = cls('*')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'*')
        assert_equal(repr(a),"Type_Param_Value('*')")

        a = cls(':')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),':')

        a = cls('1+2')
        assert isinstance(a,Level_2_Expr),`a`
        assert_equal(str(a),'1 + 2')

def test_Intrinsic_Type_Spec(): # R403

        cls = Intrinsic_Type_Spec
        a = cls('INTEGER')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'INTEGER')
        assert_equal(repr(a), "Intrinsic_Type_Spec('INTEGER', None)")

        a = cls('Integer*2')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'INTEGER*2')

        a = cls('real*2')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'REAL*2')

        a = cls('logical*2')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'LOGICAL*2')

        a = cls('complex*2')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'COMPLEX*2')

        a = cls('character*2')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'CHARACTER*2')

        a = cls('double complex')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'DOUBLE COMPLEX')

        a = cls('double  precision')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'DOUBLE PRECISION')

def test_Kind_Selector(): # R404

        cls = Kind_Selector
        a = cls('(1)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'(KIND = 1)')
        assert_equal(repr(a),"Kind_Selector('(', Int_Literal_Constant('1', None), ')')")

        a = cls('(kind=1+2)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'(KIND = 1 + 2)')

        a = cls('* 1')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'*1')

def test_Signed_Int_Literal_Constant(): # R405

        cls = Signed_Int_Literal_Constant
        a = cls('1')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'1')
        assert_equal(repr(a),"%s('1', None)" % (cls.__name__))

        a = cls('+ 21_2')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'+21_2')
        assert_equal(repr(a),"%s('+21', '2')" % (cls.__name__))

        a = cls('-21_SHORT')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'-21_SHORT')

        a = cls('21_short')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'21_short')

        a = cls('+1976354279568241_8')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'+1976354279568241_8')

def test_Int_Literal_Constant(): # R406

        cls = Int_Literal_Constant
        a = cls('1')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'1')
        assert_equal(repr(a),"%s('1', None)" % (cls.__name__))

        a = cls('21_2')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'21_2')
        assert_equal(repr(a),"%s('21', '2')" % (cls.__name__))

        a = cls('21_SHORT')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'21_SHORT')

        a = cls('21_short')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'21_short')

        a = cls('1976354279568241_8')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'1976354279568241_8')

def test_Binary_Constant(): # R412

        cls = Boz_Literal_Constant
        bcls = Binary_Constant
        a = cls('B"01"')
        assert isinstance(a,bcls),`a`
        assert_equal(str(a),'B"01"')
        assert_equal(repr(a),"%s('B\"01\"')" % (bcls.__name__))

def test_Octal_Constant(): # R413

        cls = Boz_Literal_Constant
        ocls = Octal_Constant
        a = cls('O"017"')
        assert isinstance(a,ocls),`a`
        assert_equal(str(a),'O"017"')
        assert_equal(repr(a),"%s('O\"017\"')" % (ocls.__name__))

def test_Hex_Constant(): # R414

        cls = Boz_Literal_Constant
        zcls = Hex_Constant
        a = cls('Z"01A"')
        assert isinstance(a,zcls),`a`
        assert_equal(str(a),'Z"01A"')
        assert_equal(repr(a),"%s('Z\"01A\"')" % (zcls.__name__))

def test_Signed_Real_Literal_Constant(): # R416

        cls = Signed_Real_Literal_Constant
        a = cls('12.78')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'12.78')
        assert_equal(repr(a),"%s('12.78', None)" % (cls.__name__))

        a = cls('+12.78_8')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'+12.78_8')
        assert_equal(repr(a),"%s('+12.78', '8')" % (cls.__name__))

        a = cls('- 12.')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'-12.')

        a = cls('1.6E3')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'1.6E3')

        a = cls('+1.6E3_8')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'+1.6E3_8')

        a = cls('1.6D3')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'1.6D3')

        a = cls('-1.6E-3')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'-1.6E-3')
        a = cls('1.6E+3')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'1.6E+3')

        a = cls('3E4')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'3E4')

        a = cls('.123')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'.123')

        a = cls('+1.6E-3')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'+1.6E-3')

        a = cls('10.9E7_QUAD')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'10.9E7_QUAD')

        a = cls('-10.9e-17_quad')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'-10.9E-17_quad')

def test_Real_Literal_Constant(): # R417

        cls = Real_Literal_Constant
        a = cls('12.78')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'12.78')
        assert_equal(repr(a),"%s('12.78', None)" % (cls.__name__))

        a = cls('12.78_8')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'12.78_8')
        assert_equal(repr(a),"%s('12.78', '8')" % (cls.__name__))

        a = cls('12.')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'12.')

        a = cls('1.6E3')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'1.6E3')

        a = cls('1.6E3_8')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'1.6E3_8')

        a = cls('1.6D3')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'1.6D3')

        a = cls('1.6E-3')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'1.6E-3')
        a = cls('1.6E+3')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'1.6E+3')

        a = cls('3E4')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'3E4')

        a = cls('.123')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'.123')

        a = cls('1.6E-3')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'1.6E-3')

        a = cls('10.9E7_QUAD')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'10.9E7_QUAD')

        a = cls('10.9e-17_quad')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'10.9E-17_quad')

        a = cls('0.0D+0')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'0.0D+0')

def test_Char_Selector(): # R424

        cls = Char_Selector
        a = cls('(len=2, kind=8)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'(LEN = 2, KIND = 8)')
        assert_equal(repr(a),"Char_Selector(Int_Literal_Constant('2', None), Int_Literal_Constant('8', None))")


        a = cls('(2, kind=8)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'(LEN = 2, KIND = 8)')

        a = cls('(2, 8)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'(LEN = 2, KIND = 8)')

        a = cls('(kind=8)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'(KIND = 8)')

        a = cls('(kind=8,len=2)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'(LEN = 2, KIND = 8)')

def test_Complex_Literal_Constant(): # R421

        cls = Complex_Literal_Constant
        a = cls('(1.0, -1.0)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'(1.0, -1.0)')
        assert_equal(repr(a),"Complex_Literal_Constant(Signed_Real_Literal_Constant('1.0', None), Signed_Real_Literal_Constant('-1.0', None))")

        a = cls('(3,3.1E6)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'(3, 3.1E6)')

        a = cls('(4.0_4, 3.6E7_8)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'(4.0_4, 3.6E7_8)')

        a = cls('( 0., PI)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'(0., PI)')


def test_Type_Name(): # C424

        cls = Type_Name
        a = cls('a')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a')
        assert_equal(repr(a),"Type_Name('a')")

        assertRaises(NoMatchError,cls,'integer')
        assertRaises(NoMatchError,cls,'doubleprecision')

def test_Length_Selector(): # R425

        cls = Length_Selector
        a = cls('( len = *)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'(LEN = *)')
        assert_equal(repr(a),"Length_Selector('(', Type_Param_Value('*'), ')')")

        a = cls('*2,')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'*2')

def test_Char_Length(): # R426

        cls = Char_Length
        a = cls('(1)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'(1)')
        assert_equal(repr(a),"Char_Length('(', Int_Literal_Constant('1', None), ')')")

        a = cls('1')
        assert isinstance(a,Int_Literal_Constant),`a`
        assert_equal(str(a),'1')

        a = cls('(*)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'(*)')

        a = cls('(:)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'(:)')

def test_Char_Literal_Constant(): # R427

        cls = Char_Literal_Constant
        a = cls('NIH_"DO"')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'NIH_"DO"')
        assert_equal(repr(a),'Char_Literal_Constant(\'"DO"\', \'NIH\')')

        a = cls("'DO'")
        assert isinstance(a,cls),`a`
        assert_equal(str(a),"'DO'")
        assert_equal(repr(a),'Char_Literal_Constant("\'DO\'", None)')

        a = cls("'DON''T'")
        assert isinstance(a,cls),`a`
        assert_equal(str(a),"'DON''T'")

        a = cls('"DON\'T"')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'"DON\'T"')

        a = cls('""')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'""')

        a = cls("''")
        assert isinstance(a,cls),`a`
        assert_equal(str(a),"''")

        a = cls('"hey ha(ada)\t"')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'"hey ha(ada)\t"')

def test_Logical_Literal_Constant(): # R428

        cls = Logical_Literal_Constant
        a = cls('.TRUE.')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'.TRUE.')
        assert_equal(repr(a),"%s('.TRUE.', None)" % (cls.__name__))

        a = cls('.True.')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'.TRUE.')

        a = cls('.FALSE.')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'.FALSE.')
        
        a = cls('.false.')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'.FALSE.')

        a = cls('.TRUE._HA')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'.TRUE._HA')

def test_Derived_Type_Stmt(): # R430

        cls = Derived_Type_Stmt
        a = cls('type a')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'TYPE :: a')
        assert_equal(repr(a),"Derived_Type_Stmt(None, Type_Name('a'), None)")

        a = cls('type ::a(b,c)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'TYPE :: a(b, c)')

        a = cls('type, private, abstract::a(b,c)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'TYPE, PRIVATE, ABSTRACT :: a(b, c)')

def test_Type_Name(): # C423

        cls = Type_Name
        a = cls('a')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'a')
        assert_equal(repr(a),"Type_Name('a')")

def test_Type_Attr_Spec(): # R431

        cls = Type_Attr_Spec
        a = cls('abstract')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'ABSTRACT')
        assert_equal(repr(a),"Type_Attr_Spec('ABSTRACT', None)")

        a = cls('bind (c )')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'BIND(C)')

        a = cls('extends(a)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'EXTENDS(a)')

        a = cls('private')
        assert isinstance(a, Access_Spec),`a`
        assert_equal(str(a),'PRIVATE')


def test_End_Type_Stmt(): # R433

        cls = End_Type_Stmt
        a = cls('end type')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'END TYPE')
        assert_equal(repr(a),"End_Type_Stmt('TYPE', None)")

        a = cls('end type  a')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'END TYPE a')

def test_Sequence_Stmt(): # R434

        cls = Sequence_Stmt
        a = cls('sequence')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'SEQUENCE')
        assert_equal(repr(a),"Sequence_Stmt('SEQUENCE')")

def test_Type_Param_Def_Stmt(): # R435

        cls = Type_Param_Def_Stmt
        a = cls('integer ,kind :: a')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'INTEGER, KIND :: a')
        assert_equal(repr(a),"Type_Param_Def_Stmt(None, Type_Param_Attr_Spec('KIND'), Name('a'))")

        a = cls('integer*2 ,len :: a=3, b=2+c')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'INTEGER*2, LEN :: a = 3, b = 2 + c')

def test_Type_Param_Decl(): # R436

        cls = Type_Param_Decl
        a = cls('a=2')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'a = 2')
        assert_equal(repr(a),"Type_Param_Decl(Name('a'), '=', Int_Literal_Constant('2', None))")

        a = cls('a')
        assert isinstance(a, Name),`a`
        assert_equal(str(a),'a')

def test_Type_Param_Attr_Spec(): # R437

        cls = Type_Param_Attr_Spec
        a = cls('kind')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'KIND')
        assert_equal(repr(a),"Type_Param_Attr_Spec('KIND')")

        a = cls('len')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'LEN')

def test_Component_Attr_Spec(): # R441

        cls = Component_Attr_Spec
        a = cls('pointer')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'POINTER')
        assert_equal(repr(a),"Component_Attr_Spec('POINTER')")

        a = cls('allocatable')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'ALLOCATABLE')

        a = cls('dimension(a)')
        assert isinstance(a, Dimension_Component_Attr_Spec),`a`
        assert_equal(str(a),'DIMENSION(a)')

        a = cls('private')
        assert isinstance(a, Access_Spec),`a`
        assert_equal(str(a),'PRIVATE')

def test_Component_Decl(): # R442

        cls = Component_Decl
        a = cls('a(1)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'a(1)')
        assert_equal(repr(a),"Component_Decl(Name('a'), Explicit_Shape_Spec(None, Int_Literal_Constant('1', None)), None, None)")

        a = cls('a(1)*(3)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'a(1)*(3)')

        a = cls('a(1)*(3) = 2')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'a(1)*(3) = 2')

        a = cls('a(1) => NULL')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'a(1) => NULL')

def test_Proc_Component_Def_Stmt(): # R445
    cls = Proc_Component_Def_Stmt
    a = cls('procedure(), pointer :: a')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'PROCEDURE(), POINTER :: a')

    a = cls('procedure(real*8), pointer, pass(n) :: a, b')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'PROCEDURE(REAL*8), POINTER, PASS(n) :: a, b')

def test_Type_Bound_Procedure_Part(): # R448
    cls = Type_Bound_Procedure_Part
    a = cls(get_reader('''
contains
procedure, pass :: length => point_length
    '''))
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'CONTAINS\nPROCEDURE, PASS :: length => point_length')

def test_Proc_Binding_Stmt(): # R450
    cls = Proc_Binding_Stmt
    a = cls('procedure, pass :: length => point_length')
    assert isinstance(a, Specific_Binding),`a`
    assert_equal(str(a),'PROCEDURE, PASS :: length => point_length')

def test_Specific_Binding(): # R451
    cls = Specific_Binding
    a = cls('procedure, pass :: length => point_length')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'PROCEDURE, PASS :: length => point_length')

def test_Generic_Binding(): # R452
    cls = Generic_Binding
    a = cls('generic :: a => b')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'GENERIC :: a => b')

    a = cls('generic, private :: read(formatted) => b,c')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'GENERIC, PRIVATE :: READ(FORMATTED) => b, c')

def test_Final_Binding(): # R454

        cls = Final_Binding
        a = cls('final a, b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'FINAL :: a, b')
        assert_equal(repr(a),"Final_Binding('FINAL', Final_Subroutine_Name_List(',', (Name('a'), Name('b'))))")

        a = cls('final::a')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'FINAL :: a')

def test_Derived_Type_Spec(): # R455

        cls = Derived_Type_Spec
        a = cls('a(b)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a(b)')
        assert_equal(repr(a),"Derived_Type_Spec(Type_Name('a'), Name('b'))")

        a = cls('a(b,c,g=1)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a(b, c, g = 1)')

        a = cls('a')
        assert isinstance(a,Name),`a`
        assert_equal(str(a),'a')

        a = cls('a()')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a()')

def test_Type_Param_Spec(): # R456

        cls = Type_Param_Spec
        a = cls('a=1')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a = 1')
        assert_equal(repr(a),"Type_Param_Spec(Name('a'), Int_Literal_Constant('1', None))")

        a = cls('k=a')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'k = a')

        a = cls('k=:')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'k = :')

def test_Type_Param_Spec_List(): # R456-list

        cls = Type_Param_Spec_List

        a = cls('a,b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a, b')
        assert_equal(repr(a),"Type_Param_Spec_List(',', (Name('a'), Name('b')))")

        a = cls('a')
        assert isinstance(a,Name),`a`

        a = cls('k=a,c,g=1')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'k = a, c, g = 1')

def test_Structure_Constructor_2(): # R457.b

        cls = Structure_Constructor_2
        a = cls('k=a')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'k = a')
        assert_equal(repr(a),"Structure_Constructor_2(Name('k'), Name('a'))")

        a = cls('a')
        assert isinstance(a,Name),`a`
        assert_equal(str(a),'a')

def test_Structure_Constructor(): # R457

        cls = Structure_Constructor
        a = cls('t()')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'t()')
        assert_equal(repr(a),"Structure_Constructor(Type_Name('t'), None)")

        a = cls('t(s=1, a)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'t(s = 1, a)')

        a = cls('a=k')
        assert isinstance(a,Structure_Constructor_2),`a`
        assert_equal(str(a),'a = k')
        assert_equal(repr(a),"Structure_Constructor_2(Name('a'), Name('k'))")

        a = cls('a')
        assert isinstance(a,Name),`a`
        assert_equal(str(a),'a')

def test_Component_Spec(): # R458

        cls = Component_Spec
        a = cls('k=a')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'k = a')
        assert_equal(repr(a),"Component_Spec(Name('k'), Name('a'))")

        a = cls('a')
        assert isinstance(a,Name),`a`
        assert_equal(str(a),'a')

        a = cls('a % b')
        assert isinstance(a, Proc_Component_Ref),`a`
        assert_equal(str(a),'a % b')

        a = cls('s =a % b')
        assert isinstance(a, Component_Spec),`a`
        assert_equal(str(a),'s = a % b')

def test_Component_Spec_List(): # R458-list

        cls = Component_Spec_List
        a = cls('k=a, b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'k = a, b')
        assert_equal(repr(a),"Component_Spec_List(',', (Component_Spec(Name('k'), Name('a')), Name('b')))")

        a = cls('k=a, c')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'k = a, c')

def test_Enum_Def(): # R460
    cls = Enum_Def
    a = cls(get_reader('''
enum, bind(c)
enumerator :: red = 4, blue = 9
enumerator yellow
end enum
    '''))
    assert isinstance(a,cls),`a`
    assert_equal(str(a),'ENUM, BIND(C)\n  ENUMERATOR :: red = 4, blue = 9\n  ENUMERATOR :: yellow\nEND ENUM')

def test_Enum_Def_Stmt(): # R461
    cls = Enum_Def_Stmt
    a = cls('enum, bind(c)')
    assert isinstance(a,cls),`a`
    assert_equal(str(a),'ENUM, BIND(C)')

def test_Array_Constructor(): # R465

        cls = Array_Constructor
        a = cls('(/a/)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'(/a/)')
        assert_equal(repr(a),"Array_Constructor('(/', Name('a'), '/)')")

        a = cls('[a]')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'[a]')
        assert_equal(repr(a),"Array_Constructor('[', Name('a'), ']')")

        a = cls('[integer::a]')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'[INTEGER :: a]')

        a = cls('[integer::a,b]')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'[INTEGER :: a, b]')

def test_Ac_Spec(): # R466

        cls = Ac_Spec
        a = cls('integer ::')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'INTEGER ::')
        assert_equal(repr(a),"Ac_Spec(Intrinsic_Type_Spec('INTEGER', None), None)")

        a = cls('integer :: a,b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'INTEGER :: a, b')

        a = cls('a,b')
        assert isinstance(a,Ac_Value_List),`a`
        assert_equal(str(a),'a, b')

        a = cls('integer :: a, (a, b, n = 1, 5)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'INTEGER :: a, (a, b, n = 1, 5)')

def test_Ac_Value_List(): # R469-list

        cls = Ac_Value_List
        a = cls('a, b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a, b')
        assert_equal(repr(a),"Ac_Value_List(',', (Name('a'), Name('b')))")

        a = cls('a')
        assert isinstance(a,Name),`a`
        assert_equal(str(a),'a')

def test_Ac_Implied_Do(): # R470

        cls = Ac_Implied_Do
        a = cls('( a, b, n = 1, 5 )')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'(a, b, n = 1, 5)')
        assert_equal(repr(a),"Ac_Implied_Do(Ac_Value_List(',', (Name('a'), Name('b'))), Ac_Implied_Do_Control(Name('n'), [Int_Literal_Constant('1', None), Int_Literal_Constant('5', None)]))")

def test_Ac_Implied_Do_Control(): # R471

        cls = Ac_Implied_Do_Control
        a = cls('n = 3, 5')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'n = 3, 5')
        assert_equal(repr(a),"Ac_Implied_Do_Control(Name('n'), [Int_Literal_Constant('3', None), Int_Literal_Constant('5', None)])")

        a = cls('n = 3+1, 5, 1')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'n = 3 + 1, 5, 1')

###############################################################################
############################### SECTION  5 ####################################
###############################################################################

def test_Type_Declaration_Stmt(): # R501

        cls = Type_Declaration_Stmt
        a = cls('integer a')
        assert isinstance(a, cls),`a`
        assert_equal(str(a), 'INTEGER :: a')
        assert_equal(repr(a), "Type_Declaration_Stmt(Intrinsic_Type_Spec('INTEGER', None), None, Entity_Decl(Name('a'), None, None, None))")

        a = cls('integer ,dimension(2):: a*3')
        assert isinstance(a, cls),`a`
        assert_equal(str(a), 'INTEGER, DIMENSION(2) :: a*3')

        a = cls('real a')
        assert isinstance(a, cls),`a`
        assert_equal(str(a), 'REAL :: a')
        assert_equal(repr(a), "Type_Declaration_Stmt(Intrinsic_Type_Spec('REAL', None), None, Entity_Decl(Name('a'), None, None, None))")

        a = cls('REAL A( LDA, * ), B( LDB, * )')
        assert isinstance(a, cls),`a`
        assert_equal(str(a), 'REAL :: A(LDA, *), B(LDB, *)')

        a = cls('DOUBLE PRECISION   ALPHA, BETA')
        assert isinstance(a, cls),`a`
        assert_equal(str(a), 'DOUBLE PRECISION :: ALPHA, BETA')

        a = cls('logical,parameter:: T=.true.')
        assert isinstance(a, cls),`a`
        assert_equal(str(a), 'LOGICAL, PARAMETER :: T = .TRUE.')
        
        a = cls('character(n),private:: x(n)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a), 'CHARACTER(LEN = n), PRIVATE :: x(n)')

        a = cls('character(lenmax),private:: x(n)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a), 'CHARACTER(LEN = lenmax), PRIVATE :: x(n)')
        
def test_Declaration_Type_Spec(): # R502

        cls = Declaration_Type_Spec
        a = cls('Integer*2')
        assert isinstance(a, Intrinsic_Type_Spec),`a`
        assert_equal(str(a), 'INTEGER*2')

        a = cls('type(foo)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a), 'TYPE(foo)')
        assert_equal(repr(a), "Declaration_Type_Spec('TYPE', Type_Name('foo'))")

def test_Attr_Spec(): # R503

        cls = Attr_Spec
        a = cls('allocatable')
        assert isinstance(a, cls),`a`
        assert_equal(str(a), 'ALLOCATABLE')

        a = cls('dimension(a)')
        assert isinstance(a, Dimension_Attr_Spec),`a`
        assert_equal(str(a),'DIMENSION(a)')

def test_Dimension_Attr_Spec(): # R503.d

        cls = Dimension_Attr_Spec
        a = cls('dimension(a)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'DIMENSION(a)')
        assert_equal(repr(a),"Dimension_Attr_Spec('DIMENSION', Explicit_Shape_Spec(None, Name('a')))")

def test_Intent_Attr_Spec(): # R503.f

        cls = Intent_Attr_Spec
        a = cls('intent(in)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'INTENT(IN)')
        assert_equal(repr(a),"Intent_Attr_Spec('INTENT', Intent_Spec('IN'))")

def test_Entity_Decl(): # 504

    cls = Entity_Decl
    a = cls('a(1)')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'a(1)')
    assert_equal(repr(a),"Entity_Decl(Name('a'), Explicit_Shape_Spec(None, Int_Literal_Constant('1', None)), None, None)")
    
    a = cls('a(1)*(3)')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'a(1)*(3)')
    
    a = cls('a(1)*(3) = 2')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'a(1)*(3) = 2')

    a = cls('a = 2')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'a = 2')

    a = cls('a=2')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'a = 2')
    
    a = cls('a = "abc "')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'a = "abc "')
    
    a = cls('a = .true.')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'a = .TRUE.')

def test_Target_Entity_Decl():
    cls = Target_Entity_Decl
    a = cls('a(1)')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'a(1)')
    assert_equal(repr(a),"Target_Entity_Decl(Name('a'), Explicit_Shape_Spec(None, Int_Literal_Constant('1', None)), None, None)")

def test_Access_Spec(): # R508

        cls = Access_Spec
        a = cls('private')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'PRIVATE')
        assert_equal(repr(a),"Access_Spec('PRIVATE')")

        a = cls('public')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'PUBLIC')

def test_Language_Binding_Spec(): # R509

        cls = Language_Binding_Spec
        a = cls('bind(c)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'BIND(C)')
        assert_equal(repr(a),'Language_Binding_Spec(None)')

        a = cls('bind(c, name="hey")')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'BIND(C, NAME = "hey")')

def test_Explicit_Shape_Spec(): # R511

        cls = Explicit_Shape_Spec
        a = cls('a:b')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'a : b')
        assert_equal(repr(a),"Explicit_Shape_Spec(Name('a'), Name('b'))")

        a = cls('a')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'a')

def test_Upper_Bound(): # R513

        cls = Upper_Bound
        a = cls('a')
        assert isinstance(a, Name),`a`
        assert_equal(str(a),'a')

        assertRaises(NoMatchError,cls,'*')

def test_Assumed_Shape_Spec(): # R514

        cls = Assumed_Shape_Spec
        a = cls(':')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),':')
        assert_equal(repr(a),'Assumed_Shape_Spec(None, None)')

        a = cls('a :')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'a :')

def test_Deferred_Shape_Spec(): # R515

        cls = Deferred_Shape_Spec
        a = cls(':')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),':')
        assert_equal(repr(a),'Deferred_Shape_Spec(None, None)')


def test_Assumed_Size_Spec(): # R516

        cls = Assumed_Size_Spec
        a = cls('*')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'*')
        assert_equal(repr(a),'Assumed_Size_Spec(None, None)')

        a = cls('1:*')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'1 : *')

        a = cls('a,1:*')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'a, 1 : *')

        a = cls('a:b,1:*')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'a : b, 1 : *')

def test_Access_Stmt(): # R518

        cls = Access_Stmt
        a = cls('private')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'PRIVATE')
        assert_equal(repr(a),"Access_Stmt('PRIVATE', None)")

        a = cls('public a,b')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'PUBLIC :: a, b')

        a = cls('public ::a')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'PUBLIC :: a')

def test_Data_Stmt(): #R524
    cls = Data_Stmt
    a = cls('DATA YOURNAME % AGE, YOURNAME % NAME / 35, "FRED BROWN" /')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'DATA YOURNAME % AGE, YOURNAME % NAME / 35, "FRED BROWN" /')

    a = cls('DATA NAME / "JOHN DOE" / MILES / 10 * 0 /')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'DATA NAME / "JOHN DOE" /, MILES / 10 * 0 /')

    a = cls('DATA MYNAME / PERSON (21, \'JOHN SMITH\') /')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'DATA MYNAME / PERSON(21, \'JOHN SMITH\') /')

def test_Data_Stmt_Set(): #R525
    cls = Data_Stmt_Set
    a = cls('MILES / 10 * "2/3" /')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'MILES / 10 * "2/3" /')
        
def test_Data_Implied_Do(): # R527
    cls = Data_Implied_Do
    a = cls('((SKEW (K, J), J = 1, K), K = 1, 100)')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'((SKEW(K, J), J = 1, K), K = 1, 100)')

# R531-R534 are trivial

def test_Dimension_Stmt(): # R535

    cls = Dimension_Stmt
    a = cls('dimension :: a(5)')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'DIMENSION :: a(5)')
    assert_equal(repr(a),"Dimension_Stmt([(Name('a'), Explicit_Shape_Spec(None, Int_Literal_Constant('5', None)))])")

    a = cls('dimension a(n,m), b(:), c(2:n), d(*), e(n, 2:*)')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'DIMENSION :: a(n, m), b(:), c(2 : n), d(*), e(n, 2 : *)')

def test_Intent_Stmt(): # R536

    cls = Intent_Stmt
    a = cls('intent(in) :: a')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'INTENT(IN) :: a')
    assert_equal(repr(a),"Intent_Stmt(Intent_Spec('IN'), Name('a'))")

    a = cls('intent(out) a, b')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'INTENT(OUT) :: a, b')
    assert_equal(repr(a),"Intent_Stmt(Intent_Spec('OUT'), Dummy_Arg_Name_List(',', (Name('a'), Name('b'))))")

def test_Optional_Stmt(): # R537

    cls = Optional_Stmt
    a = cls('optional :: a')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'OPTIONAL :: a')
    assert_equal(repr(a),"Optional_Stmt('OPTIONAL', Name('a'))")

    a = cls('optional :: a, b, c')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'OPTIONAL :: a, b, c')
    assert_equal(repr(a),"Optional_Stmt('OPTIONAL', Dummy_Arg_Name_List(',', (Name('a'), Name('b'), Name('c'))))")

def test_Parameter_Stmt(): # R538

    cls = Parameter_Stmt
    a = cls('parameter(a=1)')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'PARAMETER(a = 1)')
    assert_equal(repr(a),"Parameter_Stmt('PARAMETER', Named_Constant_Def(Name('a'), Int_Literal_Constant('1', None)))")
    
    a = cls('parameter(a=1, b=a+2)')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'PARAMETER(a = 1, b = a + 2)')

    a = cls('PARAMETER        ( ONE = 1.0D+0, ZERO = 0.0D+0 )')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'PARAMETER(ONE = 1.0D+0, ZERO = 0.0D+0)')

def test_Named_Constant_Def(): # R539

    cls = Named_Constant_Def
    a = cls('a=1')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'a = 1')
    assert_equal(repr(a),"Named_Constant_Def(Name('a'), Int_Literal_Constant('1', None))")

def test_Pointer_Stmt(): # R540

    cls = Pointer_Stmt
    a = cls('pointer a(:), b')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'POINTER :: a(:), b')
    assert_equal(repr(a),"Pointer_Stmt('POINTER', Pointer_Decl_List(',', (Pointer_Decl(Name('a'), Deferred_Shape_Spec(None, None)), Name('b'))))")

def test_Pointer_Decl(): # R541

    cls = Pointer_Decl
    a = cls('a(:)')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'a(:)')
    assert_equal(repr(a),"Pointer_Decl(Name('a'), Deferred_Shape_Spec(None, None))")
    
    a = cls('a(:,:)')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'a(:, :)')

def test_Protected_Stmt(): # R542
    cls = Protected_Stmt
    a = cls('protected a,b')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'PROTECTED :: a, b')
    assert_equal(repr(a),"Protected_Stmt('PROTECTED', Entity_Name_List(',', (Name('a'), Name('b'))))")

    a = cls('protected ::a')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'PROTECTED :: a')
    assert_equal(repr(a),"Protected_Stmt('PROTECTED', Name('a'))")

def test_Save_Stmt(): # R543
    cls = Save_Stmt
    a = cls('save')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'SAVE')
    assert_equal(repr(a),"Save_Stmt('SAVE', None)")

    a = cls('save a, b')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'SAVE :: a, b')
    assert_equal(repr(a),"Save_Stmt('SAVE', Saved_Entity_List(',', (Name('a'), Name('b'))))")

    a = cls('save :: /a/ , b')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'SAVE :: /a/, b')
    assert_equal(repr(a),"Save_Stmt('SAVE', Saved_Entity_List(',', (Saved_Entity('/', Name('a'), '/'), Name('b'))))")

def test_Saved_Entity(): # R544
    cls = Saved_Entity
    a = cls('a')
    assert isinstance(a, Name),`a`
    assert_equal(str(a),'a')
    assert_equal(repr(a),"Name('a')")

    a = cls('/a/')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'/a/')
    assert_equal(repr(a),"Saved_Entity('/', Name('a'), '/')")

# R545 is trivial

def test_Target_Stmt(): # R546
    cls = Target_Stmt
    a = cls('target a, b(1000, 1000)')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'TARGET :: a, b(1000, 1000)')

    a = cls('target :: a, c')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'TARGET :: a, c')

def test_Value_Stmt(): # R547
    cls = Value_Stmt
    a = cls('value a')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'VALUE :: a')

    a = cls('value:: a, c')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'VALUE :: a, c')

def test_Volatile_Stmt(): # R548
    cls = Volatile_Stmt
    a = cls('volatile a')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'VOLATILE :: a')

    a = cls('volatile :: a, c')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'VOLATILE :: a, c')

def test_Implicit_Stmt(): # R549

        cls = Implicit_Stmt
        a = cls('implicitnone')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'IMPLICIT NONE')
        assert_equal(repr(a),"Implicit_Stmt('NONE')")

        a = cls('implicit real(a-d), double precision(r-t,x), type(a) (y-z)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'IMPLICIT REAL(A - D), DOUBLE PRECISION(R - T, X), TYPE(a)(Y - Z)')

def test_Implicit_Spec(): # R550

        cls = Implicit_Spec
        a = cls('integer (a-z)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'INTEGER(A - Z)')
        assert_equal(repr(a),"Implicit_Spec(Intrinsic_Type_Spec('INTEGER', None), Letter_Spec('A', 'Z'))")

        a = cls('double  complex (r,d-g)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'DOUBLE COMPLEX(R, D - G)')

def test_Letter_Spec(): # R551

        cls = Letter_Spec
        a = cls('a-z')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'A - Z')
        assert_equal(repr(a),"Letter_Spec('A', 'Z')")

        a = cls('d')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'D')

def test_Namelist_Stmt(): # R552
    cls = Namelist_Stmt
    a = cls('namelist / nlist / a')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'NAMELIST /nlist/ a')

    a = cls('namelist / nlist / a, /mlist/ b,c /klist/ d,e')
    assert_equal(str(a),'NAMELIST /nlist/ a, /mlist/ b, c, /klist/ d, e')
        
def test_Equivalence_Stmt(): # R554

        cls = Equivalence_Stmt
        a = cls('equivalence (a, b ,z)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'EQUIVALENCE(a, b, z)')
        assert_equal(repr(a),"Equivalence_Stmt('EQUIVALENCE', Equivalence_Set(Name('a'), Equivalence_Object_List(',', (Name('b'), Name('z')))))")

        a = cls('equivalence (a, b ,z),(b,l)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'EQUIVALENCE(a, b, z), (b, l)')

def test_Common_Stmt(): # R557

        cls = Common_Stmt
        a = cls('common a')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'COMMON // a')
        assert_equal(repr(a),"Common_Stmt([(None, Name('a'))])")

        a = cls('common // a,b')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'COMMON // a, b')

        a = cls('common /name/ a,b')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'COMMON /name/ a, b')

        a = cls('common /name/ a,b(4,5) // c, /ljuks/ g(2)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'COMMON /name/ a, b(4, 5) // c /ljuks/ g(2)')

def test_Common_Block_Object(): # R558

        cls = Common_Block_Object
        a = cls('a(2)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'a(2)')
        assert_equal(repr(a),"Common_Block_Object(Name('a'), Explicit_Shape_Spec(None, Int_Literal_Constant('2', None)))")

        a = cls('a')
        assert isinstance(a, Name),`a`
        assert_equal(str(a),'a')


###############################################################################
############################### SECTION  6 ####################################
###############################################################################

def test_Substring(): # R609

        cls = Substring
        a = cls('a(:)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'a(:)')
        assert_equal(repr(a),"Substring(Name('a'), Substring_Range(None, None))")

        a = cls('a(1:2)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a(1 : 2)')
        assert_equal(repr(a),"Substring(Name('a'), Substring_Range(Int_Literal_Constant('1', None), Int_Literal_Constant('2', None)))")


def test_Substring_Range(): # R611

        cls = Substring_Range
        a = cls(':')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),':')
        assert_equal(repr(a),"Substring_Range(None, None)")

        a = cls('a+1:')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'a + 1 :')

        a = cls('a+1: c/foo(g)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'a + 1 : c / foo(g)')

        a = cls('a:b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a : b')
        assert_equal(repr(a),"Substring_Range(Name('a'), Name('b'))")

        a = cls('a:')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a :')

        a = cls(':b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),': b')


def test_Data_Ref(): # R612

        cls = Data_Ref
        a = cls('a%b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a % b')
        assert_equal(repr(a),"Data_Ref('%', (Name('a'), Name('b')))")

        a = cls('a')
        assert isinstance(a,Name),`a`
        assert_equal(str(a),'a')

def test_Part_Ref(): # R613

        cls = Part_Ref
        a = cls('a')
        assert isinstance(a, Name),`a`
        assert_equal(str(a),'a')

def test_Type_Param_Inquiry(): # R615

        cls = Type_Param_Inquiry
        a = cls('a % b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a % b')
        assert_equal(repr(a),"Type_Param_Inquiry(Name('a'), '%', Name('b'))")


def test_Array_Section(): # R617

        cls = Array_Section
        a = cls('a(:)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a(:)')
        assert_equal(repr(a),"Array_Section(Name('a'), Substring_Range(None, None))")

        a = cls('a(2:)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a(2 :)')


def test_Section_Subscript(): # R619

        cls = Section_Subscript

        a = cls('1:2')
        assert isinstance(a, Subscript_Triplet),`a`
        assert_equal(str(a),'1 : 2')

        a = cls('zzz')
        assert isinstance(a, Name),`a`
        assert_equal(str(a),'zzz')

def test_Section_Subscript_List(): # R619-list

        cls = Section_Subscript_List
        a = cls('a,2')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a, 2')
        assert_equal(repr(a),"Section_Subscript_List(',', (Name('a'), Int_Literal_Constant('2', None)))")

        a = cls('::1')
        assert isinstance(a,Subscript_Triplet),`a`
        assert_equal(str(a),': : 1')

        a = cls('::1, 3')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),': : 1, 3')

def test_Subscript_Triplet(): # R620

        cls = Subscript_Triplet
        a = cls('a:b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a : b')
        assert_equal(repr(a),"Subscript_Triplet(Name('a'), Name('b'), None)")

        a = cls('a:b:1')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a : b : 1')

        a = cls(':')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),':')

        a = cls('::5')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),': : 5')

        a = cls(':5')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),': 5')

        a = cls('a+1 :')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a + 1 :')

def test_Allocate_Stmt(): # R623
    cls = Allocate_Stmt
    a = cls('allocate(a,b)')
    assert isinstance(a,cls),`a`
    assert_equal(str(a),'ALLOCATE(a, b)')

    a = cls('allocate(real::a)')
    assert_equal(str(a),'ALLOCATE(REAL::a)')

    a = cls('allocate(real(kind=8)::a, stat=b, source=c//d)')
    assert_equal(str(a),'ALLOCATE(REAL(KIND = 8)::a, STAT = b, SOURCE = c // d)')


def test_Alloc_Opt(): # R624

        cls = Alloc_Opt
        a = cls('stat=a')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'STAT = a')
        assert_equal(repr(a),"Alloc_Opt('STAT', Name('a'))")

def test_Nullify_Stmt(): # R633

        cls = Nullify_Stmt
        a = cls('nullify (a)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'NULLIFY(a)')
        assert_equal(repr(a),"Nullify_Stmt('NULLIFY', Name('a'))")

        a = cls('nullify (a,c)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'NULLIFY(a, c)')

def test_Deallocate_Stmt(): # R635
    cls = Deallocate_Stmt
    a = cls('deallocate (a)')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'DEALLOCATE(a)')

    a = cls('deallocate (a,stat=b)')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'DEALLOCATE(a, STAT = b)')
    a = cls('deallocate (a,c,stat=b,errmsg=d)')
    assert_equal(str(a),'DEALLOCATE(a, c, STAT = b, ERRMSG = d)')
        
###############################################################################
############################### SECTION  7 ####################################
###############################################################################

def test_Primary(): # R701

        cls = Primary
        a = cls('a')
        assert isinstance(a,Name),`a`
        assert_equal(str(a),'a')

        a = cls('(a)')
        assert isinstance(a,Parenthesis),`a`
        assert_equal(str(a),'(a)')

        a = cls('1')
        assert isinstance(a,Int_Literal_Constant),`a`
        assert_equal(str(a),'1')

        a = cls('1.')
        assert isinstance(a,Real_Literal_Constant),`a`
        assert_equal(str(a),'1.')

        a = cls('(1, n)')
        assert isinstance(a,Complex_Literal_Constant),`a`
        assert_equal(str(a),'(1, n)')

        a = cls('.true.')
        assert isinstance(a,Logical_Literal_Constant),`a`
        assert_equal(str(a),'.TRUE.')

        a = cls('"hey a()c"')
        assert isinstance(a,Char_Literal_Constant),`a`
        assert_equal(str(a),'"hey a()c"')

        a = cls('b"0101"')
        assert isinstance(a,Binary_Constant),`a`
        assert_equal(str(a),'B"0101"')

        a = cls('o"0107"')
        assert isinstance(a,Octal_Constant),`a`
        assert_equal(str(a),'O"0107"')

        a = cls('z"a107"')
        assert isinstance(a,Hex_Constant),`a`
        assert_equal(str(a),'Z"A107"')

        a = cls('a % b')
        assert isinstance(a,Data_Ref),`a`
        assert_equal(str(a),'a % b')

        a = cls('a(:)')
        assert isinstance(a,Array_Section),`a`
        assert_equal(str(a),'a(:)')

        a = cls('0.0E-1')
        assert isinstance(a,Real_Literal_Constant),`a`
        assert_equal(str(a),'0.0E-1')

def test_Parenthesis(): # R701.h

        cls = Parenthesis
        a  = cls('(a)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'(a)')
        assert_equal(repr(a),"Parenthesis('(', Name('a'), ')')")

        a  = cls('(a+1)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'(a + 1)')

        a  = cls('((a))')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'((a))')

        a  = cls('(a+(a+c))')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'(a + (a + c))')

def test_Level_1_Expr(): # R702

        cls = Level_1_Expr
        a = cls('.hey. a')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'.HEY. a')
        assert_equal(repr(a),"Level_1_Expr('.HEY.', Name('a'))")

        #assertRaises(NoMatchError,cls,'.not. a')

        a = cls('.false.')
        assert isinstance(a,Logical_Literal_Constant),`a`

def test_Mult_Operand(): # R704

        cls = Mult_Operand
        a = cls('a**b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a ** b')
        assert_equal(repr(a),"Mult_Operand(Name('a'), '**', Name('b'))")

        a = cls('a**2')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a ** 2')

        a = cls('(a+b)**2')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'(a + b) ** 2')

        a = cls('0.0E-1')
        assert isinstance(a,Real_Literal_Constant),`a`
        assert_equal(str(a),'0.0E-1')

def test_Add_Operand(): # R705

        cls = Add_Operand
        a = cls('a*b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a * b')
        assert_equal(repr(a),"Add_Operand(Name('a'), '*', Name('b'))")

        a = cls('a/b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a / b')

        a = cls('a**b')
        assert isinstance(a,Mult_Operand),`a`
        assert_equal(str(a),'a ** b')

        a = cls('0.0E-1')
        assert isinstance(a,Real_Literal_Constant),`a`
        assert_equal(str(a),'0.0E-1')

def test_Level_2_Expr(): # R706

        cls = Level_2_Expr
        a = cls('a+b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a + b')
        assert_equal(repr(a),"Level_2_Expr(Name('a'), '+', Name('b'))")

        a = cls('a-b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a - b')

        a = cls('a+b+c')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a + b + c')

        a = cls('+a')
        assert isinstance(a,Level_2_Unary_Expr),`a`
        assert_equal(str(a),'+ a')

        a = cls('+1')
        assert isinstance(a,Level_2_Unary_Expr),`a`
        assert_equal(str(a),'+ 1')

        a = cls('+a+b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'+ a + b')

        a = cls('0.0E-1')
        assert isinstance(a,Real_Literal_Constant),`a`
        assert_equal(str(a),'0.0E-1')


def test_Level_2_Unary_Expr():

        cls = Level_2_Unary_Expr
        a = cls('+a')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'+ a')
        assert_equal(repr(a),"Level_2_Unary_Expr('+', Name('a'))")

        a = cls('-a')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'- a')

        a = cls('+1')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'+ 1')

        a = cls('0.0E-1')
        assert isinstance(a,Real_Literal_Constant),`a`
        assert_equal(str(a),'0.0E-1')


def test_Level_3_Expr(): # R710

        cls = Level_3_Expr
        a = cls('a//b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a // b')
        assert_equal(repr(a),"Level_3_Expr(Name('a'), '//', Name('b'))")

        a = cls('"a"//"b"')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'"a" // "b"')

def test_Level_4_Expr(): # R712

        cls = Level_4_Expr
        a = cls('a.eq.b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a .EQ. b')
        assert_equal(repr(a),"Level_4_Expr(Name('a'), '.EQ.', Name('b'))")

        a = cls('a.ne.b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a .NE. b')

        a = cls('a.lt.b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a .LT. b')

        a = cls('a.gt.b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a .GT. b')

        a = cls('a.ge.b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a .GE. b')

        a = cls('a==b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a == b')

        a = cls('a/=b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a /= b')

        a = cls('a<b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a < b')

        a = cls('a<=b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a <= b')

        a = cls('a>=b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a >= b')

        a = cls('a>b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a > b')

def test_And_Operand(): # R714

        cls = And_Operand
        a = cls('.not.a')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'.NOT. a')
        assert_equal(repr(a),"And_Operand('.NOT.', Name('a'))")

def test_Or_Operand(): # R715

        cls = Or_Operand
        a = cls('a.and.b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a .AND. b')
        assert_equal(repr(a),"Or_Operand(Name('a'), '.AND.', Name('b'))")


def test_Equiv_Operand(): # R716

        cls = Equiv_Operand
        a = cls('a.or.b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a .OR. b')
        assert_equal(repr(a),"Equiv_Operand(Name('a'), '.OR.', Name('b'))")


def test_Level_5_Expr(): # R717

        cls = Level_5_Expr
        a = cls('a.eqv.b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a .EQV. b')
        assert_equal(repr(a),"Level_5_Expr(Name('a'), '.EQV.', Name('b'))")

        a = cls('a.neqv.b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a .NEQV. b')

        a = cls('a.eq.b')
        assert isinstance(a,Level_4_Expr),`a`
        assert_equal(str(a),'a .EQ. b')

def test_Expr(): # R722

        cls = Expr
        a = cls('a .op. b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a .OP. b')
        assert_equal(repr(a),"Expr(Name('a'), '.OP.', Name('b'))")

        a = cls('a')
        assert isinstance(a,Name),`a`
        assert_equal(str(a),'a')

        a = cls('3.e2')
        assert isinstance(a,Real_Literal_Constant),`a`

        a = cls('0.0E-1')
        assert isinstance(a,Real_Literal_Constant),`a`
        assert_equal(str(a),'0.0E-1')

        a = cls('123')
        assert isinstance(a,Int_Literal_Constant),`a`
        assert_equal(str(a),'123')

        a = cls('.false.')
        assert isinstance(a,Logical_Literal_Constant),`a`
        assert_equal(str(a),'.FALSE.')

        assertRaises(NoMatchError,Scalar_Int_Expr,'a,b')

def test_Logical_Expr(): # R724
    cls = Logical_Expr
    a = cls('(f0 .lt. f1) .and. abs(x1-x0) .gt. abs(x2) .or.  .not. root')
    assert isinstance(a,Equiv_Operand),`a`
    assert_equal(str(a),'(f0 .LT. f1) .AND. abs(x1 - x0) .GT. abs(x2) .OR. .NOT. root')
    
def test_Logical_Initialization_Expr(): # R733

    cls = Logical_Initialization_Expr
    a = cls('.false.')
    assert isinstance(a, Logical_Literal_Constant), `a`
    assert str(a)=='.FALSE.'

def test_Assignment_Stmt(): # R734

    cls = Assignment_Stmt
    a = cls('a = b')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'a = b')
    assert_equal(repr(a),"Assignment_Stmt(Name('a'), '=', Name('b'))")

    a = cls('a(3:4) = b+c')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'a(3 : 4) = b + c')

    a = cls('a%c = b+c')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'a % c = b + c')

    a = cls('a = .FALSE.')
    assert isinstance(a, cls),`a`
    assert_equal(repr(a),"Assignment_Stmt(Name('a'), '=', Logical_Literal_Constant('.FALSE.', None))")

    a = cls('a(n)(k:m) = 5')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'a(n)(k : m) = 5')

    a = cls('b = a + 1  d - 8')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'b = a + 1D-8')

    a = cls('b = a + 1  d - 8 + 1.1e+3')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'b = a + 1D-8 + 1.1E+3')

def test_Pointer_Assignment_Stmt(): # R735
    cls = Pointer_Assignment_Stmt
    a = cls('new_node % left => current_node')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'new_node % left => current_node')
    
    a = cls('simple_name => target_structure % substruct % component')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'simple_name => target_structure % substruct % component')

    for stmt in '''\
PTR => NULL()
ROW => MAT2D(N, :)
WINDOW => MAT2D(I - 1 : I + 1, J - 1 : J + 1)
POINTER_OBJECT => POINTER_FUNCTION(ARG_1, ARG_2)
EVERY_OTHER => VECTOR(1 : N : 2)
WINDOW2(0 :, 0 :) => MAT2D(ML : MU, NL : NU)
P => BESSEL
STRUCT % COMPONENT => BESSEL'''.split('\n'):
        a = cls(stmt)
        assert isinstance(a, cls),`a`
        assert_equal(str(a), stmt)
            
def test_Proc_Component_Ref(): # R741

        cls = Proc_Component_Ref
        a = cls('a % b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a % b')
        assert_equal(repr(a),"Proc_Component_Ref(Name('a'), '%', Name('b'))")

def test_Where_Stmt(): # R743

        cls = Where_Stmt
        a = cls('where (a) c=2')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'WHERE (a) c = 2')
        assert_equal(repr(a),"Where_Stmt(Name('a'), Assignment_Stmt(Name('c'), '=', Int_Literal_Constant('2', None)))")

def test_Where_Construct(): # R745
    cls = Where_Construct
    a = cls(get_reader('''
    where (pressure <= 1.0)
    pressure = pressure + inc_pressure
    temp = temp - 5.0
    elsewhere
    raining = .true.
    end where
'''))
    assert isinstance(a,cls),`a`
    assert_equal(str(a),'WHERE (pressure <= 1.0)\n  pressure = pressure + inc_pressure\n  temp = temp - 5.0\nELSEWHERE\n  raining = .TRUE.\nEND WHERE')

    a = cls(get_reader('''
    where (cond1)
    elsewhere (cond2)
    end where
'''))
    assert isinstance(a,cls),`a`
    assert_equal(str(a),'WHERE (cond1)\nELSEWHERE(cond2)\nEND WHERE')

    a = cls(get_reader('''
    n:where (cond1)
    elsewhere (cond2) n
    elsewhere n
    end where n
'''))
    assert isinstance(a,cls),`a`
    assert_equal(str(a),'n:WHERE (cond1)\nELSEWHERE(cond2) n\nELSEWHERE n\nEND WHERE n')


def test_Where_Construct_Stmt(): # R745

    cls = Where_Construct_Stmt
    a = cls('where (a)')
    assert isinstance(a,cls),`a`
    assert_equal(str(a),'WHERE (a)')
    assert_equal(repr(a),"Where_Construct_Stmt(Name('a'))")

def test_Forall_Construct(): # R752
    cls = Forall_Construct
    a = cls(get_reader('''
    forall (i = 1:10, j = 1:10, b(i, j) /= 0.0)
      a(i, j) = real (i + j - 2)
      b(i, j) = a(i, j) + b(i, j) * real (i * j)
    end forall
    '''))
    assert isinstance(a,cls),`a`
    assert_equal(str(a),'FORALL(i = 1 : 10, j = 1 : 10, b(i, j) /= 0.0)\n  a(i, j) = real(i + j - 2)\n  b(i, j) = a(i, j) + b(i, j) * real(i * j)\nEND FORALL')

    a = cls(get_reader('''
    n: forall (x = 1:5:2, j = 1:4)
      a(x, j) = j
    end forall n
    '''))
    assert isinstance(a,cls),`a`
    assert_equal(str(a),'n:FORALL(x = 1 : 5 : 2, j = 1 : 4)\n  a(x, j) = j\nEND FORALL n')
    
def test_Forall_Header(): # R754
    cls = Forall_Header
    a = cls('(n=1:2, a+1)')
    assert isinstance(a,cls),`a`
    assert_equal(str(a),'(n = 1 : 2, a + 1)')

    a = cls('(n=1:2, m=1:x-1:z(a))')
    assert isinstance(a,cls),`a`
    assert_equal(str(a),'(n = 1 : 2, m = 1 : x - 1 : z(a))')

def test_Forall_Triplet_Spec(): # R755

    cls = Forall_Triplet_Spec
    a = cls('n = 1: 2')
    assert isinstance(a,cls),`a`
    assert_equal(str(a),'n = 1 : 2')

    a = cls('n = f(x): 2-b:a+1')
    assert isinstance(a,cls),`a`
    assert_equal(str(a),'n = f(x) : 2 - b : a + 1')

###############################################################################
############################### SECTION  8 ####################################
###############################################################################

def test_If_Construct(): # R802
    cls = If_Construct
    a = cls(get_reader('''
if (expr) then
  a = 1
end if
    '''))
    assert isinstance(a,cls),`a`
    assert_equal(str(a),'IF (expr) THEN\n  a = 1\nEND IF')

    a = cls(get_reader('''
name: if (expr) then
  a = 1
end if name
    '''))

    assert_equal(str(a),'name:IF (expr) THEN\n  a = 1\nEND IF name')

    a = cls(get_reader('''
if (expr) then
  a = 1
  if (expr2) then
    a = 2
  endif
  a = 3
end if
    '''))
    assert isinstance(a,cls),`a`
    assert_equal(str(a),'IF (expr) THEN\n  a = 1\n  IF (expr2) THEN\n    a = 2\n  END IF\n  a = 3\nEND IF')

    a = cls(get_reader('''
if (expr) then
  a = 1
else if (expr2) then
  a = 2
end if
    '''))
    assert isinstance(a,cls),`a`
    assert_equal(str(a),'IF (expr) THEN\n  a = 1\nELSE IF (expr2) THEN\n  a = 2\nEND IF')

    a = cls(get_reader('''
if (expr) then
  a = 1
else
  a = 2
end if
    '''))
    assert_equal(str(a),'IF (expr) THEN\n  a = 1\nELSE\n  a = 2\nEND IF')

    a = cls(get_reader('''
if (expr) then
  a = 1
else if (expr2) then
  a = 2
else
  a = 3
end if
    '''))
    assert_equal(str(a),'IF (expr) THEN\n  a = 1\nELSE IF (expr2) THEN\n  a = 2\nELSE\n  a = 3\nEND IF')

    a = cls(get_reader('''
named: if (expr) then
  a = 1
else named
  a = 2
end if named
    '''))
    assert_equal(str(a),'named:IF (expr) THEN\n  a = 1\nELSE named\n  a = 2\nEND IF named')

    a = cls(get_reader('''
named: if (expr) then
  a = 1
  named2: if (expr2) then
    a = 2
  end if named2
end if named
'''))
    assert_equal(str(a),'named:IF (expr) THEN\n  a = 1\n  named2:IF (expr2) THEN\n    a = 2\n  END IF named2\nEND IF named')

    a = cls(get_reader('''
if (expr) then
  a = 1
else if (expr2) then
  a = 2
else if (expr3) then
  a = 3
end if
    '''))
    assert isinstance(a,cls),`a`
    assert_equal(str(a),'IF (expr) THEN\n  a = 1\nELSE IF (expr2) THEN\n  a = 2\nELSE IF (expr3) THEN\n  a = 3\nEND IF')

    a = cls(get_reader('''
        if (dxmx .gt. 0d0) then
          diff = 0
          do  80  k = 1, n
   80     diff = max(diff,abs(xnew(k)-xin(k)))
          if (diff .gt. dxmx) then
            betx = dxmx/diff

            call awrit3(' broyj:  max shift = %1;3g'//
     .        ' is larger than dxmx = %1;3g.  Scale by %1;3g',
     .        ' ',80,i1mach(2),diff,dxmx,dxmx/diff)

            do  82  k = 1, n
   82       xnew(k) = xin(k) + betx*(xnew(k)-xin(k))
          endif
        endif

    '''))
    
def test_if_nonblock_do():
    cls = If_Construct
    a = cls(get_reader('''
if (expr) then
   do  20  i = 1, 3
     a = 1
     do  20  j = 1, 3
       a = 2
       do  20  k = 1, 3
         a = 3
20 rotm(i,j) = r2(j,i)
endif
'''))    
    assert isinstance(a,cls),`a`
    assert len(a.content)==3,`a`
    a = a.content[1]
    assert isinstance(a, Action_Term_Do_Construct),`a`
    assert_equal(str(a),'DO 20 , i = 1, 3\n  a = 1\n  DO 20 , j = 1, 3\n    a = 2\n    DO 20 , k = 1, 3\n      a = 3\n20 rotm(i, j) = r2(j, i)')

    a = cls(get_reader('''
if (expr) then
    do  50  i = n, m, -1
  50 call foo(a)
endif'''))
    assert isinstance(a,cls),`a`
    assert len(a.content)==3,`a`
    a = a.content[1]
    assert isinstance(a, Action_Term_Do_Construct),`a`

def test_Case_Construct(): # R808
    cls = Case_Construct
    a = cls(get_reader('''
select case (n)
case (:-1)
  signum = -1
case (0)
  signum = 0
case (1:)
  signum = 1
end select
'''))
    assert isinstance(a,cls),`a`
    assert_equal(str(a),'SELECT CASE (n)\nCASE (: - 1)\n  signum = - 1\nCASE (0)\n  signum = 0\nCASE (1 :)\n  signum = 1\nEND SELECT')
    
def test_Case_Selector(): # R813
    cls = Case_Selector
    a = cls('default')
    assert isinstance(a,cls),`a`
    assert_equal(str(a),'DEFAULT')

    a = cls('(2)')
    assert isinstance(a,cls),`a`
    assert_equal(str(a),'(2)')

    a = cls('(2:3, c+2:, :-a)')
    assert isinstance(a,cls),`a`
    assert_equal(str(a),'(2 : 3, c + 2 :, : - a)')

def test_Associate_Construct(): # R816
    cls = Associate_Construct
    a = cls(get_reader('''
ASSOCIATE ( Z => EXP(-(X**2+Y**2)) * COS(THETA) )
PRINT *, A+Z, A-Z
END ASSOCIATE
    '''))
    assert isinstance(a,cls),`a`
    assert_equal(str(a),'ASSOCIATE(Z => EXP(- (X ** 2 + Y ** 2)) * COS(THETA))\n  PRINT *, A + Z, A - Z\nEND ASSOCIATE')

    a = cls(get_reader('''
name:ASSOCIATE ( XC => AX%B(I,J)%C )
XC%DV = XC%DV + PRODUCT(XC%EV(1:N))
END ASSOCIATE name
    '''))
    assert isinstance(a,cls),`a`
    assert_equal(str(a),'name:ASSOCIATE(XC => AX % B(I, J) % C)\n  XC % DV = XC % DV + PRODUCT(XC % EV(1 : N))\nEND ASSOCIATE name')

    a = cls(get_reader('''
ASSOCIATE ( W => RESULT(I,J)%W, ZX => AX%B(I,J)%D, ZY => AY%B(I,J)%D )
W = ZX*X + ZY*Y
END ASSOCIATE
    '''))
    assert_equal(str(a),'ASSOCIATE(W => RESULT(I, J) % W, ZX => AX % B(I, J) % D, ZY => AY % B(I, J) % D)\n  W = ZX * X + ZY * Y\nEND ASSOCIATE')

def test_Select_Type_Construct(): # R821
    cls = Select_Type_Construct
    a = cls(get_reader('''
n:SELECT TYPE ( A => P_OR_C )
CLASS IS ( POINT )
PRINT *, A%X, A%Y ! This block gets executed
TYPE IS ( POINT_3D )
PRINT *, A%X, A%Y, A%Z
END SELECT n
    '''))
    assert isinstance(a,cls),`a`
    assert_equal(str(a),'n:SELECT TYPE(A=>P_OR_C)\n  CLASS IS (POINT)\n  PRINT *, A % X, A % Y\n  TYPE IS (POINT_3D)\n  PRINT *, A % X, A % Y, A % Z\nEND SELECT n')

def test_Select_Type_Stmt(): # R822
    cls = Select_Type_Stmt
    a = cls('select type(a=>b)')
    assert isinstance(a,cls),`a`
    assert_equal(str(a),'SELECT TYPE(a=>b)')

    a = cls('select type(a)')
    assert isinstance(a,cls),`a`
    assert_equal(str(a),'SELECT TYPE(a)')

def test_Type_Guard_Stmt(): # R823
    cls = Type_Guard_Stmt
    a = cls('type is (real*8)')
    assert isinstance(a,cls),`a`
    assert_equal(str(a),'TYPE IS (REAL*8)')

    a = cls('class is (mytype) name')
    assert isinstance(a,cls),`a`
    assert_equal(str(a),'CLASS IS (mytype) name')

    a = cls('classdefault')
    assert isinstance(a,cls),`a`
    assert_equal(str(a),'CLASS DEFAULT')

def test_Block_Label_Do_Construct(): # R826_1
    cls = Block_Label_Do_Construct
    a = cls(get_reader('''
      do 12
        a = 1
 12   continue
    '''))
    assert isinstance(a,cls),`a`
    assert_equal(str(a),'DO 12\n  a = 1\n12 CONTINUE')

    a = cls(get_reader('''
      do 12
        do 13
          a = 1
 13   continue
 12   continue
    '''))
    assert_equal(str(a),'DO 12\n  DO 13\n    a = 1\n13 CONTINUE\n12 CONTINUE')
    assert len(a.content)==3,`len(a.content)`
    assert_equal(str(a.content[1]), 'DO 13\n  a = 1\n13 CONTINUE')

def test_Block_Nonlabel_Do_Construct(): # # R826_2
    cls = Block_Nonlabel_Do_Construct
    a = cls(get_reader('''
      do i=1,10
        a = 1
      end do
    '''))
    assert isinstance(a,cls),`a`
    assert_equal(str(a),'DO , i = 1, 10\n  a = 1\nEND DO')

    a = cls(get_reader('''
      foo:do i=1,10
        a = 1
      end do foo
    '''))
    assert isinstance(a,cls),`a`
    assert_equal(str(a),'foo:DO , i = 1, 10\n  a = 1\nEND DO foo')

    a = cls(get_reader('''
      do j=1,2
      foo:do i=1,10
        a = 1
      end do foo
      end do
    '''))
    assert isinstance(a,cls),`a`
    assert_equal(str(a),'DO , j = 1, 2\n  foo:DO , i = 1, 10\n    a = 1\n  END DO foo\nEND DO')

def test_Label_Do_Stmt(): # R828

    cls = Label_Do_Stmt
    a = cls('do 12')
    assert isinstance(a,cls),`a`
    assert_equal(str(a),'DO 12')
    assert_equal(repr(a),"Label_Do_Stmt(None, Label('12'), None)")

def test_Nonblock_Do_Construct(): # R835
    cls = Nonblock_Do_Construct
    a = cls(get_reader('''
      do  20  i = 1, 3
 20     rotm(i,j) = r2(j,i)
    '''))
    assert isinstance(a,Action_Term_Do_Construct),`a`
    assert_equal(str(a),'DO 20 , i = 1, 3\n20 rotm(i, j) = r2(j, i)')

    a = cls(get_reader('''
      do  20  i = 1, 3
      k = 3
      do  20  j = 1, 3
      l = 3
 20     rotm(i,j) = r2(j,i)
    '''))
    assert isinstance(a,Action_Term_Do_Construct),`a`
    assert_equal(str(a),'DO 20 , i = 1, 3\n  k = 3\n  DO 20 , j = 1, 3\n    l = 3\n20 rotm(i, j) = r2(j, i)')

    a = cls(get_reader('''
      do  20  i = 1, 3
 20     rotm(i,j) = r2(j,i)
    '''))
    assert isinstance(a,Action_Term_Do_Construct),`a`
    assert_equal(str(a),'DO 20 , i = 1, 3\n20 rotm(i, j) = r2(j, i)')

    a = cls(get_reader('''
    do  50  i = n, m, -1
  50 call foo(a)
    '''))
    assert isinstance(a,Action_Term_Do_Construct),`a`
    assert_equal(str(a),'DO 50 , i = n, m, - 1\n50 CALL foo(a)')
    
def test_Continue_Stmt(): # R848

    cls = Continue_Stmt
    a = cls('continue')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'CONTINUE')
    assert_equal(repr(a),"Continue_Stmt('CONTINUE')")

def test_Stop_Stmt(): # R849
    cls = Stop_Stmt
    a = cls('stop')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'STOP')

    a = cls('stop 123')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'STOP 123')

    a = cls('stop   \'hey you\'')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),"STOP 'hey you'")

###############################################################################
############################### SECTION  9 ####################################
###############################################################################

def test_Io_Unit(): # R901

        cls = Io_Unit
        a = cls('*')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'*')

        a = cls('a')
        assert isinstance(a, Name),`a`
        assert_equal(str(a),'a')

def test_Read_Stmt(): # R910
    cls = Read_Stmt
    a = cls('read(123)')
    assert isinstance(a, cls),`a`
    assert_equal(str(a), 'READ(UNIT = 123)')

    a = cls('read(123) a')
    assert_equal(str(a), 'READ(UNIT = 123) a')
    a = cls('read(123) a(  2)')
    assert_equal(str(a), 'READ(UNIT = 123) a(2)')

    a = cls('read*, a(  2), b')
    assert_equal(str(a), 'READ *, a(2), b')
    
def test_Write_Stmt(): # R911

        cls = Write_Stmt
        a = cls('write (123)"hey"')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'WRITE(UNIT = 123) "hey"')
        assert_equal(repr(a),'Write_Stmt(Io_Control_Spec_List(\',\', (Io_Control_Spec(\'UNIT\', Int_Literal_Constant(\'123\', None)),)), Char_Literal_Constant(\'"hey"\', None))')

def test_Print_Stmt(): # R912

        cls = Print_Stmt
        a = cls('print 123')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'PRINT 123')
        assert_equal(repr(a),"Print_Stmt(Label('123'), None)")

        a = cls('print *,"a=",a')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'PRINT *, "a=", a')

def test_Io_Control_Spec(): # R913

        cls = Io_Control_Spec
        a = cls('end=123')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'END = 123')
        assert_equal(repr(a),"Io_Control_Spec('END', Label('123'))")

def test_Io_Control_Spec_List(): # R913-list

        cls = Io_Control_Spec_List
        a = cls('end=123')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'END = 123')
        assert_equal(repr(a),"Io_Control_Spec_List(',', (Io_Control_Spec('END', Label('123')),))")

        a = cls('123')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'UNIT = 123')

        a = cls('123,*')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'UNIT = 123, FMT = *')

        a = cls('123,fmt=a')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'UNIT = 123, FMT = a')

        if 0:
            # see todo note in Io_Control_Spec_List
            a = cls('123,a')
            assert isinstance(a, cls),`a`
            assert_equal(str(a),'UNIT = 123, NML = a')

def test_Format(): # R914

    cls = Format
    a = cls('*')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'*')
    assert_equal(repr(a),"Format('*')")
    
    a = cls('a')
    assert isinstance(a, Name),`a`
    assert_equal(str(a),'a')
    
    a = cls('123')
    assert isinstance(a, Label),`a`
    assert_equal(str(a),'123')

def test_Io_Implied_Do(): # R917
    cls = Io_Implied_Do
    a = cls('(a, i=1,2)')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'(a, i = 1, 2)')

    a = cls('((i+j,j=3,4,1), i=1,2)')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'((i + j, j = 3, 4, 1), i = 1, 2)')

def test_Io_Implied_Do_Control(): # R919

    cls = Io_Implied_Do_Control
    a = cls('i=1,2')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'i = 1, 2')

    a = cls('i=f(2),2-1,a+2')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'i = f(2), 2 - 1, a + 2')

def test_Wait_Stmt(): # R921

    cls = Wait_Stmt
    a = cls('wait (123)')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'WAIT(UNIT = 123)')

def test_Wait_Spec(): # R922

        cls = Wait_Spec
        a = cls('123')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'UNIT = 123')
        assert_equal(repr(a),"Wait_Spec('UNIT', Int_Literal_Constant('123', None))")

        a = cls('err=1')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'ERR = 1')

def test_Backspace_Stmt(): # R923

    cls = Backspace_Stmt
    a = cls('backspace 1')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'BACKSPACE 1')

    a = cls('backspace  (unit=1,err=2)')
    assert_equal(str(a),'BACKSPACE(UNIT = 1, ERR = 2)')

def test_Endfile_Stmt(): # R924

    cls = Endfile_Stmt
    a = cls('endfile 1')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'ENDFILE 1')

    a = cls('endfile  (unit=1,err=2)')
    assert_equal(str(a),'ENDFILE(UNIT = 1, ERR = 2)')

def test_Rewind_Stmt(): # R925

    cls = Rewind_Stmt
    a = cls('rewind 1')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'REWIND 1')

    a = cls('rewind  (unit=1,err=2)')
    assert_equal(str(a),'REWIND(UNIT = 1, ERR = 2)')

def test_Position_Spec(): # R926

    cls = Position_Spec
    a = cls('1')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'UNIT = 1')
    a = cls('unit=1')
    assert_equal(str(a),'UNIT = 1')
    a = cls('err=2')
    assert_equal(str(a),'ERR = 2')
    a = cls('iomsg=a')
    assert_equal(str(a),'IOMSG = a')
    a = cls('iostat=a')
    assert_equal(str(a),'IOSTAT = a')

def test_Flush_Stmt(): # R927

    cls = Flush_Stmt
    a = cls('flush 1')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'FLUSH 1')

    a = cls('flush  (unit=1,err=2)')
    assert_equal(str(a),'FLUSH(UNIT = 1, ERR = 2)')

def test_Flush_Spec(): # R928

    cls = Flush_Spec
    a = cls('1')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'UNIT = 1')
    a = cls('unit=1')
    assert_equal(str(a),'UNIT = 1')
    a = cls('err=2')
    assert_equal(str(a),'ERR = 2')
    a = cls('iomsg=a')
    assert_equal(str(a),'IOMSG = a')
    a = cls('iostat=a')
    assert_equal(str(a),'IOSTAT = a')

def test_Inquire_Stmt(): # R929

    cls = Inquire_Stmt
    a = cls('inquire(1,file=a)')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'INQUIRE(UNIT = 1, FILE = a)')
    a = cls('inquire(iolength=n) a, b')
    assert_equal(str(a),'INQUIRE(IOLENGTH=n) a, b')

def test_Inquire_Spec(): # R930

    cls = Inquire_Spec
    a = cls('1')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'UNIT = 1')
    a = cls('file=fn')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'FILE = fn')

    a = cls('access=a')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'ACCESS = a')


    
###############################################################################
############################### SECTION 10 ####################################
###############################################################################

def test_Format_Stmt(): # R1001
    cls = Format_Stmt
    a = cls('format (3f9.4)')
    assert isinstance(a, cls),`type(a)`
    assert_equal(str(a),'FORMAT(3F9.4)')
    a = cls("format (' ',3f9.4)")
    assert isinstance(a, cls),`type(a)`
    assert_equal(str(a),"FORMAT(' ', 3F9.4)")

    a = cls('format(i6,f12.6,2x,f12.6)')
    assert isinstance(a, cls),`type(a)`
    assert_equal(str(a),'FORMAT(I6, F12.6, 2X, F12.6)')

    a = cls("format(' Enter smth',$)")
    assert isinstance(a, cls),`type(a)`
    assert_equal(str(a),"FORMAT(' Enter smth', $)")

    a = cls("format(/'a' /'b')")
    assert isinstance(a, cls),`type(a)`
    assert_equal(str(a),"FORMAT(/, 'a', /, 'b')")

    a = cls("format('a:':' b')")
    assert isinstance(a, cls),`type(a)`
    assert_equal(str(a),"FORMAT('a:', :, ' b')")

    return
    a = cls("format('text=','  '")
    assert_equal(str(a),'')
    
def test_Format_Specification(): # R1002
    cls = Format_Specification
    a = cls('(3f9.4, 2f8.1)')
    assert isinstance(a, cls),`type(a)`
    assert_equal(str(a),'(3F9.4, 2F8.1)')

    a = cls("(' ', 2f8.1)")
    assert isinstance(a, cls),`type(a)`
    assert_equal(str(a),"(' ', 2F8.1)")
    
def test_Format_Item(): # R1003
    cls = Format_Item
    a = cls('3f9.4')
    assert isinstance(a, cls),`type(a)`
    assert_equal(str(a),'3F9.4')

    a = cls("' '")
    assert isinstance(a, Char_Literal_Constant),`type(a)`
    assert_equal(str(a),"' '")

    a = cls('i4/')
    assert isinstance(a, Format_Item_C1002),`type(a)`
    assert_equal(str(a),'I4, /')

    a = cls('3f12.6/')
    assert_equal(str(a),'3F12.6, /')

    a = cls("/' '")
    assert_equal(str(a),"/, ' '")

    a = cls("' '/")
    assert_equal(str(a),"' ', /")

    a = cls("' '/' '")
    assert_equal(str(a),"' ', /, ' '")

def test_Format_Item_List():
    cls = Format_Item_List
    a = cls('3f9.4')
    assert isinstance(a, Format_Item),`type(a)`
    assert_equal(str(a),'3F9.4')

    a = cls('3f9.4, 2f8.1')
    assert isinstance(a, Format_Item_List),`type(a)`
    assert_equal(str(a),'3F9.4, 2F8.1')

    a = cls("' ', 2f8.1")
    assert isinstance(a, Format_Item_List),`type(a)`
    assert_equal(str(a),"' ', 2F8.1")

    a = cls("' ', ' '")
    assert_equal(str(a),"' ', ' '")
    

###############################################################################
############################### SECTION 11 ####################################
###############################################################################

def test_Main_Program(): # R1101
    cls = Main_Program
    a = cls(get_reader('''
program a
end
    '''))
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'PROGRAM a\nEND PROGRAM a')

    a = cls(get_reader('''
program a
  real b
  b = 1
  contains
  subroutine foo
  end
end
    '''))
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'PROGRAM a\n  REAL :: b\n  b = 1\n  CONTAINS\n  SUBROUTINE foo\n  END SUBROUTINE foo\nEND PROGRAM a')
    
    a = Main_Program0(get_reader('''
end
    '''))
    assert isinstance(a, Main_Program0),`a`
    assert_equal(str(a),'END PROGRAM')

    a = Main_Program0(get_reader('''
contains
  function foo()
  end
end
    '''))
    assert isinstance(a, Main_Program0),`a`
    assert_equal(str(a),'CONTAINS\nFUNCTION foo()\nEND FUNCTION\nEND PROGRAM')

def test_Module(): # R1104
    cls = Module
    a = cls(get_reader('''
module m
end
    '''))
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'MODULE m\nEND MODULE m')

    a = cls(get_reader('''
module m
type a
end type
type b
end type b
end
    '''))
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'MODULE m\n  TYPE :: a\n  END TYPE a\n  TYPE :: b\n  END TYPE b\nEND MODULE m')

def test_Module_Subprogram_Part(): # R1107
    cls = Module_Subprogram_Part
    a = cls(get_reader('''
contains
  subroutine foo(a)
  real a
  a = 1.0
  end
    '''))
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'CONTAINS\nSUBROUTINE foo(a)\n  REAL :: a\n  a = 1.0\nEND SUBROUTINE foo')
    
def test_Use_Stmt(): # R1109

    cls = Use_Stmt
    a = cls('use a')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'USE :: a')
    assert_equal(repr(a),"Use_Stmt(None, Name('a'), '', None)")
    
    a = cls('use :: a, c=>d')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'USE :: a, c => d')

    a = cls('use :: a, operator(.hey.)=>operator(.hoo.)')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'USE :: a, OPERATOR(.HEY.) => OPERATOR(.HOO.)')

    a = cls('use, intrinsic :: a, operator(.hey.)=>operator(.hoo.), c=>g')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'USE, INTRINSIC :: a, OPERATOR(.HEY.) => OPERATOR(.HOO.), c => g')

def test_Module_Nature(): # R1110

    cls = Module_Nature
    a = cls('intrinsic')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'INTRINSIC')
    assert_equal(repr(a),"Module_Nature('INTRINSIC')")
    
    a = cls('non_intrinsic')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'NON_INTRINSIC')

def test_Rename(): # R1111
    cls = Rename
    a = cls('a=>b')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'a => b')

    a = cls('operator(.foo.)=>operator(.bar.)')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'OPERATOR(.FOO.) => OPERATOR(.BAR.)')

def test_Block_Data(): # R1116
    cls = Block_Data
    a = cls(get_reader('''
block data a
real b
end block data
    '''))
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'BLOCK DATA a\n  REAL :: b\nEND BLOCK DATA a')

###############################################################################
############################### SECTION 12 ####################################
###############################################################################

def test_Interface_Block(): # R1201
    cls = Interface_Block
    a = cls(get_reader('''\
interface
end interface'''))
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'INTERFACE\nEND INTERFACE')

    a = cls(get_reader('''\
abstract interface
procedure a
module procedure b,c
end interface
'''))
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'ABSTRACT INTERFACE\n  MODULE PROCEDURE a\n  MODULE PROCEDURE b, c\nEND INTERFACE')

def test_Interface_Specification(): # R1202
    cls = Interface_Specification
    a = cls(get_reader('''
    function foo()
    end
    '''))
    assert isinstance(a, Function_Body),`a`
    assert_equal(str(a),'FUNCTION foo()\nEND FUNCTION')

def test_Interface_Stmt(): # R1203
    cls = Interface_Stmt
    a = cls('interface')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'INTERFACE')

    a = cls('interface assignment(=)')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'INTERFACE ASSIGNMENT(=)')

    a = cls('abstract interface')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'ABSTRACT INTERFACE')

def test_End_Interface_Stmt(): # R1204
    cls = End_Interface_Stmt
    a = cls('end interface')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'END INTERFACE')

    a = cls('end interface read(formatted)')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'END INTERFACE READ(FORMATTED)')

    a = cls('end interface assignment(=)')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'END INTERFACE ASSIGNMENT(=)')

def test_Interface_Body(): # R1205
    cls = Interface_Body
    a = cls(get_reader('''\
subroutine foo
end subroutine foo
'''))
    assert isinstance(a, Subroutine_Body),`a`
    assert_equal(str(a),'SUBROUTINE foo\nEND SUBROUTINE foo')

    a = cls(get_reader('''\
function foo(a) result(c)
  real a, c
end
'''))
    assert isinstance(a, Function_Body),`a`
    assert_equal(str(a),'FUNCTION foo(a) RESULT(c)\n  REAL :: a, c\nEND FUNCTION')

def test_Subroutine_Body():
    pass
def test_Function_Body():
    pass
    
def test_Procedure_Stmt(): # R1206
    cls = Procedure_Stmt
    a = cls('module procedure a')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'MODULE PROCEDURE a')

    a = cls('procedure a, b')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'MODULE PROCEDURE a, b')

def test_Generic_Spec(): # R1207
    cls = Generic_Spec
    a = cls('a')
    assert isinstance(a, Name),`a`
    assert_equal(str(a), 'a')
    a = cls('read(formatted)')
    assert isinstance(a, Dtio_Generic_Spec),`a`
    assert_equal(str(a), 'READ(FORMATTED)')

    a = cls('assignment ( = )')
    assert isinstance(a, cls),`a`
    assert_equal(str(a), 'ASSIGNMENT(=)')

    return # TODO
    a = cls('operator(.foo.)')
    assert isinstance(a, cls),`a`
    assert_equal(str(a), 'OPERATOR(.foo.)')
    

    
def test_Dtio_Generic_Spec(): # R1208
    cls = Dtio_Generic_Spec
    a = cls('read   ( formatted )')
    assert isinstance(a, cls),`a`
    assert_equal(str(a), 'READ(FORMATTED)')

    a = cls('write ( formatted )')
    assert_equal(str(a), 'WRITE(FORMATTED)')
    a = cls('read   ( unformatted )')
    assert_equal(str(a), 'READ(UNFORMATTED)')
    a = cls('write ( unformatted )')
    assert_equal(str(a), 'WRITE(UNFORMATTED)')
    

def test_Import_Stmt(): # R1209
    cls = Import_Stmt
    a = cls('import :: a, b')
    assert isinstance(a, cls),`a`
    assert_equal(str(a), 'IMPORT :: a, b')

    a = cls('import a')
    assert isinstance(a, cls),`a`
    assert_equal(str(a), 'IMPORT :: a')

def test_External_Stmt(): # R1210
    cls = External_Stmt
    a = cls('external :: a, b')
    assert isinstance(a, cls),`a`
    assert_equal(str(a), 'EXTERNAL :: a, b')

    a = cls('external a')
    assert isinstance(a, cls),`a`
    assert_equal(str(a), 'EXTERNAL :: a')

def test_Procedure_Declaration_Stmt(): # R1211
    cls = Procedure_Declaration_Stmt
    a = cls('procedure () a')
    assert isinstance(a, cls),`a`
    assert_equal(str(a), 'PROCEDURE() a')

    a = cls('procedure (n) a')
    assert_equal(str(a), 'PROCEDURE(n) a')

    a = cls('procedure (real*8) a')
    assert_equal(str(a), 'PROCEDURE(REAL*8) a')

    a = cls('procedure (real(kind=8)) a')
    assert_equal(str(a), 'PROCEDURE(REAL(KIND = 8)) a')

    a = cls('procedure (real*8) :: a')
    assert_equal(str(a), 'PROCEDURE(REAL*8) a')

    a = cls('procedure (real*8), intent(in), bind(c) :: a, b')
    assert_equal(str(a), 'PROCEDURE(REAL*8), INTENT(IN), BIND(C) :: a, b')


def test_Proc_Attr_Spec(): # R1213
    cls = Proc_Attr_Spec
    a = cls('intent(in)')
    assert isinstance(a, cls)
    assert_equal(str(a),'INTENT(IN)')

    a = cls('optional')
    assert isinstance(a, cls)
    assert_equal(str(a),'OPTIONAL')

    a = cls('save')
    assert isinstance(a, cls)
    assert_equal(str(a),'SAVE')

    a = cls('private')
    assert isinstance(a, Access_Spec),`type(a)`
    assert_equal(str(a),'PRIVATE')

    a = cls('bind(c)')
    assert isinstance(a, Language_Binding_Spec),`a`
    assert_equal(str(a),'BIND(C)')

def test_Proc_Decl(): # R1214

    cls = Proc_Decl
    a = cls('a => NULL')
    assert isinstance(a, cls)
    assert_equal(str(a),'a => NULL')

    a = cls('a')
    assert isinstance(a, Name),`type(a)`
    assert_equal(str(a),'a')

def test_Intrinsic_Stmt(): # R1216

    cls = Intrinsic_Stmt
    a = cls('intrinsic :: a, b')
    assert isinstance(a,cls),`a`
    assert_equal(str(a),'INTRINSIC :: a, b')
    a = cls('intrinsic a, b')
    assert_equal(str(a),'INTRINSIC :: a, b')

    a = cls('intrinsic a')
    assert_equal(str(a),'INTRINSIC :: a')

def test_Function_Reference(): # R1217

        cls = Function_Reference
        a = cls('f()')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'f()')
        assert_equal(repr(a),"Function_Reference(Name('f'), None)")

        a = cls('f(2,k=1,a)')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'f(2, k = 1, a)')

def test_Call_Stmt(): # R1218

    cls = Call_Stmt
    a = cls('call a')
    assert isinstance(a, cls)
    assert_equal(str(a), 'CALL a')

    a = cls('call a()')
    assert_equal(str(a), 'CALL a')

    a = cls('call a(b,c)')
    assert_equal(str(a), 'CALL a(b, c)')

def test_Procedure_Designator(): # R1219

        cls = Procedure_Designator
        a = cls('a%b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a % b')
        assert_equal(repr(a),"Procedure_Designator(Name('a'), '%', Name('b'))")

def test_Actual_Arg_Spec(): # R1220

        cls = Actual_Arg_Spec
        a = cls('k=a')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'k = a')
        assert_equal(repr(a),"Actual_Arg_Spec(Name('k'), Name('a'))")

        a = cls('a')
        assert isinstance(a,Name),`a`
        assert_equal(str(a),'a')

def test_Actual_Arg_Spec_List():

        cls = Actual_Arg_Spec_List
        a = cls('a,b')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'a, b')
        assert_equal(repr(a),"Actual_Arg_Spec_List(',', (Name('a'), Name('b')))")

        a = cls('a = k')
        assert isinstance(a,Actual_Arg_Spec),`a`
        assert_equal(str(a),'a = k')

        a = cls('a = k,b')
        assert isinstance(a,Actual_Arg_Spec_List),`a`
        assert_equal(str(a),'a = k, b')

        a = cls('a')
        assert isinstance(a,Name),`a`
        assert_equal(str(a),'a')

def test_Alt_Return_Spec(): # R1222

        cls = Alt_Return_Spec
        a = cls('* 123')
        assert isinstance(a,cls),`a`
        assert_equal(str(a),'*123')
        assert_equal(repr(a),"Alt_Return_Spec(Label('123'))")

def test_Function_Subprogram(): # R1223

    reader = get_reader('''\
    function foo()
    end function foo''')
    cls = Function_Subprogram
    a = cls(reader)
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'FUNCTION foo()\nEND FUNCTION foo')
    assert_equal(repr(a),"Function_Subprogram(Function_Stmt(None, Name('foo'), None, None), End_Function_Stmt('FUNCTION', Name('foo')))")

    reader = get_reader('''\
    pure real function foo(a) result(b) bind(c)
    integer a
    end function foo''')
    cls = Function_Subprogram
    a = cls(reader)
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'PURE REAL FUNCTION foo(a) RESULT(b) BIND(C)\n  INTEGER :: a\nEND FUNCTION foo')

def test_Function_Stmt(): # R1224
        cls = Function_Stmt
        a = cls('function foo()')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'FUNCTION foo()')
        assert_equal(repr(a),"Function_Stmt(None, Name('foo'), None, None)")

        a = cls('function foo(a,b)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'FUNCTION foo(a, b)')
        assert_equal(repr(a),"Function_Stmt(None, Name('foo'), Dummy_Arg_List(',', (Name('a'), Name('b'))), None)")    

        a = cls('function foo(a)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'FUNCTION foo(a)')

        a = cls('real function foo(a)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'REAL FUNCTION foo(a)')

        a = cls('real recursive function foo(a)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'REAL RECURSIVE FUNCTION foo(a)')

        a = cls('real function foo(a) bind(c)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'REAL FUNCTION foo(a) BIND(C)')

        a = cls('real function foo(a) result (b)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'REAL FUNCTION foo(a) RESULT(b)')

        a = cls('real function foo(a) bind(c) result(b)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'REAL FUNCTION foo(a) RESULT(b) BIND(C)')

def test_Dummy_Arg_Name(): # R1226
    cls = Dummy_Arg_Name
    a = cls('a')
    assert isinstance(a, Name),`a`
    assert_equal(str(a),'a')

def test_Prefix(): # R1227

        cls = Prefix
        a = cls('pure  recursive')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'PURE RECURSIVE')
        assert_equal(repr(a), "Prefix(' ', (Prefix_Spec('PURE'), Prefix_Spec('RECURSIVE')))")

        a = cls('integer * 2 pure')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'INTEGER*2 PURE')

def test_Prefix_Spec(): # R1228

        cls = Prefix_Spec
        a = cls('pure')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'PURE')
        assert_equal(repr(a),"Prefix_Spec('PURE')")

        a = cls('elemental')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'ELEMENTAL')

        a = cls('recursive')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'RECURSIVE')

        a = cls('integer * 2')
        assert isinstance(a, Intrinsic_Type_Spec),`a`
        assert_equal(str(a),'INTEGER*2')

def test_Suffix(): # R1229

    cls = Suffix
    
    a = cls('bind(c)')
    assert isinstance(a, Language_Binding_Spec),`a`
    assert_equal(str(a),'BIND(C)')
    assert_equal(repr(a),"Language_Binding_Spec(None)")

    a = cls('result(a)')
    assert isinstance(a, Suffix),`a`
    assert_equal(str(a),'RESULT(a)')

    a = cls('bind(c) result(a)')
    assert isinstance(a, Suffix),`a`
    assert_equal(str(a),'RESULT(a) BIND(C)')

    a = cls('result(a) bind(c)')
    assert isinstance(a, Suffix),`a`
    assert_equal(str(a),'RESULT(a) BIND(C)')

def test_End_Function_Stmt(): # R1230
    cls = End_Function_Stmt
    a = cls('end')
    assert isinstance(a, cls),`a`
    assert_equal(str(a), 'END FUNCTION')

    a = cls('endfunction')
    assert_equal(str(a), 'END FUNCTION')

    a = cls('endfunction foo')
    assert_equal(str(a), 'END FUNCTION foo')

def test_Subroutine_Subprogram(): # R1231

        reader = get_reader('''\
      subroutine foo
      end subroutine foo''')
        cls = Subroutine_Subprogram
        a = cls(reader)
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'SUBROUTINE foo\nEND SUBROUTINE foo')
        assert_equal(repr(a),"Subroutine_Subprogram(Subroutine_Stmt(None, Name('foo'), None, None), End_Subroutine_Stmt('SUBROUTINE', Name('foo')))")

        reader = get_reader('''\
      subroutine foo
        integer a
      end subroutine foo''')
        cls = Subroutine_Subprogram
        a = cls(reader)
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'SUBROUTINE foo\n  INTEGER :: a\nEND SUBROUTINE foo')

def test_Subroutine_Stmt(): # R1232

        cls = Subroutine_Stmt
        a = cls('subroutine foo')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'SUBROUTINE foo')
        assert_equal(repr(a),"Subroutine_Stmt(None, Name('foo'), None, None)")

        a = cls('pure subroutine foo')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'PURE SUBROUTINE foo')

        a = cls('pure subroutine foo(a,b)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'PURE SUBROUTINE foo(a, b)')

        a = cls('subroutine foo() bind(c)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'SUBROUTINE foo BIND(C)')

        a = cls('subroutine foo(a)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'SUBROUTINE foo(a)')

        a = cls('subroutine foo(a, b)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'SUBROUTINE foo(a, b)')

        a = cls('subroutine foo(a,*)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'SUBROUTINE foo(a, *)')

        a = cls('subroutine foo(*)')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'SUBROUTINE foo(*)')

def test_Dummy_Arg(): # R1233
    cls = Dummy_Arg
    a = cls('a')
    assert isinstance(a, Name),`a`
    assert_equal(str(a),'a')
    a = cls('*')
    assert isinstance(a, cls),`a`
    assert_equal(str(a),'*')
    
def test_End_Subroutine_Stmt(): # R1234

        cls = End_Subroutine_Stmt
        a = cls('end subroutine foo')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'END SUBROUTINE foo')
        assert_equal(repr(a),"End_Subroutine_Stmt('SUBROUTINE', Name('foo'))")

        a = cls('end')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'END SUBROUTINE')

        a = cls('endsubroutine')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'END SUBROUTINE')

def test_Entry_Stmt(): # R1235

    cls = Entry_Stmt
    a = cls('entry a')
    assert isinstance(a, cls),`a`
    assert_equal(str(a), 'ENTRY a()')

    a = cls('entry a()')
    assert_equal(str(a), 'ENTRY a()')

    a = cls('entry a(b, c)')
    assert_equal(str(a), 'ENTRY a(b, c)')

    a = cls('entry a(b, c) bind(c)')
    assert_equal(str(a), 'ENTRY a(b, c) BIND(C)')


def test_Return_Stmt(): # R1236

        cls = Return_Stmt
        a = cls('return')
        assert isinstance(a, cls),`a`
        assert_equal(str(a), 'RETURN')
        assert_equal(repr(a), 'Return_Stmt(None)')

def test_Contains(): # R1237

        cls = Contains_Stmt
        a = cls('Contains')
        assert isinstance(a, cls),`a`
        assert_equal(str(a),'CONTAINS')
        assert_equal(repr(a),"Contains_Stmt('CONTAINS')")

if 1:
    nof_needed_tests = 0
    nof_needed_match = 0
    total_needs = 0
    total_classes = 0
    for name in dir():
        obj = eval(name)
        if not isinstance(obj, ClassType): continue
        if not issubclass(obj, Base): continue
        clsname = obj.__name__
        if clsname.endswith('Base'): continue
        total_classes += 1
        subclass_names = obj.__dict__.get('subclass_names',None)
        use_names = obj.__dict__.get('use_names',None)
        if not use_names: continue
        match = obj.__dict__.get('match',None)
        try:
            test_cls = eval('test_%s' % (clsname))
        except NameError:
            test_cls = None
        total_needs += 1
        if match is None:
            if test_cls is None:
                print 'Needs tests:', clsname
                print 'Needs match implementation:', clsname
                nof_needed_tests += 1
                nof_needed_match += 1
            else:
                print 'Needs match implementation:', clsname
                nof_needed_match += 1
        else:
            if test_cls is None:
                print 'Needs tests:', clsname
                nof_needed_tests += 1
        continue
    print '-----'
    print 'Nof match implementation needs:',nof_needed_match,'out of',total_needs
    print 'Nof tests needs:',nof_needed_tests,'out of',total_needs
    print 'Total number of classes:',total_classes
    print '-----'
