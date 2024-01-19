# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2024, Science and Technology Facilities Council.
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
# Modified by A. R. Porter and R. W. Ford, STFC Daresbury Lab
# -----------------------------------------------------------------------------

'''Performs pytest tests on the psyclone.psyir.backend.c module'''

import pytest

from psyclone.psyir.backend.c import CWriter
from psyclone.psyir.backend.visitor import VisitorError
from psyclone.psyir.nodes import ArrayReference, Assignment, BinaryOperation, \
    CodeBlock, IfBlock, Literal, Node, Reference, Return, Schedule, \
    UnaryOperation, Loop, OMPTaskloopDirective, OMPMasterDirective, \
    OMPParallelDirective, IntrinsicCall
from psyclone.psyir.symbols import ArgumentInterface, ArrayType, \
    BOOLEAN_TYPE, CHARACTER_TYPE, DataSymbol, INTEGER_TYPE, REAL_TYPE


def test_cw_gen_declaration():
    '''Check the CWriter class gen_declaration method produces
    the expected declarations.

    '''
    cwriter = CWriter()

    # Basic entries
    symbol = DataSymbol("dummy1", INTEGER_TYPE)
    result = cwriter.gen_declaration(symbol)
    assert result == "int dummy1"

    symbol = DataSymbol("dummy1", CHARACTER_TYPE)
    result = cwriter.gen_declaration(symbol)
    assert result == "char dummy1"

    symbol = DataSymbol("dummy1", BOOLEAN_TYPE)
    result = cwriter.gen_declaration(symbol)
    assert result == "bool dummy1"

    # Array argument
    array_type = ArrayType(REAL_TYPE, [ArrayType.Extent.ATTRIBUTE,
                                       ArrayType.Extent.ATTRIBUTE,
                                       ArrayType.Extent.ATTRIBUTE])
    symbol = DataSymbol("dummy2", array_type,
                        interface=ArgumentInterface(
                            ArgumentInterface.Access.READ))
    result = cwriter.gen_declaration(symbol)
    assert result == "double * restrict dummy2"

    # Array with unknown access
    array_type = ArrayType(INTEGER_TYPE, [2, 4, 2])
    symbol = DataSymbol("dummy2", array_type,
                        interface=ArgumentInterface(
                            ArgumentInterface.Access.UNKNOWN))
    result = cwriter.gen_declaration(symbol)
    assert result == "int * restrict dummy2"

    # Check invalid datatype produces and error
    symbol._datatype = "invalid"
    with pytest.raises(NotImplementedError) as error:
        _ = cwriter.gen_declaration(symbol)
    assert "Could not generate C definition for variable 'dummy2', " \
        "type 'invalid' is not yet supported." in str(error.value)


def test_cw_gen_local_variable(monkeypatch):
    '''Check the CWriter class gen_local_variable method produces
    the expected declarations.

    '''
    cwriter = CWriter()

    monkeypatch.setattr(cwriter, "gen_declaration",
                        lambda x: "<declaration>")

    # Local variables are declared as single statements
    symbol = DataSymbol("dummy1", INTEGER_TYPE)
    result = cwriter.gen_local_variable(symbol)
    # Result should include the mocked gen_declaration and ';\n'
    assert result == "<declaration>;\n"


def test_cw_exception():
    '''Check the CWriter class instance raises an exception if an
    unsupported PSyIR node is found.

    '''
    # Define a Node which will be unsupported by the visitor
    class Unsupported(Node):
        '''A PSyIR node that will not be supported by the C visitor.'''
    # pylint: enable=abstract-method

    unsupported = Unsupported()

    cwriter = CWriter()
    with pytest.raises(VisitorError) as excinfo:
        _ = cwriter(unsupported)
    assert "Unsupported node 'Unsupported' found" in str(excinfo.value)


def test_cw_literal():
    '''Check the CWriter class literal method correctly prints
    out the C representation of a Literal.

    '''

    cwriter = CWriter()

    lit = Literal('1', INTEGER_TYPE)
    assert cwriter(lit) == '1'

    # Test that scientific notation is output correctly
    lit = Literal("3e5", REAL_TYPE, None)
    assert cwriter(lit) == '3e5'


def test_cw_assignment():
    '''Check the CWriter class assignment method generate the appropriate
    output.

    '''
    assignment = Assignment.create(Reference(DataSymbol('a', REAL_TYPE)),
                                   Reference(DataSymbol('b', REAL_TYPE)))
    # Generate C from the PSyIR schedule
    cwriter = CWriter()
    result = cwriter(assignment)
    assert result == "a = b;\n"


def test_cw_array():
    '''Check the CWriter class array method correctly prints
    out the C representation of an array.

    '''
    cwriter = CWriter()

    symbol = DataSymbol('a', REAL_TYPE)
    arr = ArrayReference(symbol)
    lit = Literal('0.0', REAL_TYPE)
    assignment = Assignment.create(arr, lit)

    # An array without any children (dimensions) should produce an error.
    with pytest.raises(VisitorError) as excinfo:
        result = cwriter(assignment)
    assert "Incomplete ArrayReference node (for symbol 'a') found: " \
           "must have one or more children." in str(excinfo.value)

    # Dimensions can be references, literals or operations
    arr.addchild(Reference(DataSymbol('b', INTEGER_TYPE)))
    arr.addchild(Literal('1', INTEGER_TYPE))
    uop = UnaryOperation.create(UnaryOperation.Operator.MINUS,
                                Literal('2', INTEGER_TYPE))
    arr.addchild(uop)

    result = cwriter(assignment)
    # Results is reversed and flatten (row-major 1D), so
    # a[b, 1, -2] becomes a[(-2) * aLEN2 * aLEN1 + 1 * aLEN1 + b]
    # dimensions are called <name>LEN<dimension> by convention
    assert result == "a[b + 1 * aLEN1 + (-2) * aLEN1 * aLEN2] = 0.0;\n"


def test_cw_ifblock():
    '''Check the CWriter class ifblock method correctly prints out the
    C representation.

    '''

    # Try with just an IfBlock node
    ifblock = IfBlock()
    cwriter = CWriter()
    with pytest.raises(VisitorError) as err:
        _ = cwriter(ifblock)
    assert ("IfBlock malformed or incomplete. It should have "
            "at least 2 children, but found 0." in str(err.value))

    # Add the if condition
    ifblock.addchild(Reference(DataSymbol('a', REAL_TYPE)))
    with pytest.raises(VisitorError) as err:
        _ = cwriter(ifblock)
    assert ("IfBlock malformed or incomplete. It should have "
            "at least 2 children, but found 1." in str(err.value))

    # Fill the if_body
    ifblock.addchild(Schedule(parent=ifblock))
    ifblock.if_body.addchild(Return(parent=ifblock.if_body))
    if_only = cwriter(ifblock)
    assert if_only == (
        "if (a) {\n"
        "  return;\n"
        "}\n")
    # Fill the else_body
    ifblock.addchild(Schedule(parent=ifblock))

    condition = Reference(DataSymbol('b', REAL_TYPE))
    then_content = [Return()]
    else_content = [Return()]
    ifblock2 = IfBlock.create(condition, then_content, else_content)
    ifblock.else_body.addchild(ifblock2)

    result = cwriter(ifblock)
    assert result == (
        "if (a) {\n"
        "  return;\n"
        "} else {\n"
        "  if (b) {\n"
        "    return;\n"
        "  } else {\n"
        "    return;\n"
        "  }\n"
        "}\n")


def test_cw_return():
    '''Check the CWriter class return method correctly prints out the
    C representation.

    '''
    cwriter = CWriter()
    result = cwriter(Return())
    assert "return;\n" in result


def test_cw_codeblock():
    '''Check the CWriter class codeblock method raises the expected
    error.
    '''

    cblock = CodeBlock([], "dummy")
    cwriter = CWriter()

    with pytest.raises(VisitorError) as error:
        _ = cwriter(cblock)
    assert "CodeBlocks can not be translated to C." in str(error.value)


def test_cw_unaryoperator():
    '''Check the CWriter class unary_operation method correctly prints out
    the C representation of any given UnaryOperation.

    '''
    cwriter = CWriter()

    # Test UnaryOperation without children.
    unary_operation = UnaryOperation(UnaryOperation.Operator.MINUS)
    with pytest.raises(VisitorError) as err:
        _ = cwriter(unary_operation)
    assert ("UnaryOperation malformed or incomplete. It should have "
            "exactly 1 child, but found 0." in str(err.value))

    # Add child
    ref1 = Literal("a", CHARACTER_TYPE, unary_operation)
    unary_operation.addchild(ref1)
    assert cwriter(unary_operation) == '(-a)'

    # Test all supported Operators
    test_list = ((UnaryOperation.Operator.PLUS, '(+a)'),
                 (UnaryOperation.Operator.MINUS, '(-a)'),
                 (UnaryOperation.Operator.NOT, '(!a)'))

    for operator, expected in test_list:
        unary_operation._operator = operator
        assert cwriter(unary_operation) in expected

    # Test that an unsupported operator raises an error
    class Unsupported():
        ''' Mock Unsupported object '''

    unary_operation._operator = Unsupported
    with pytest.raises(NotImplementedError) as err:
        _ = cwriter(unary_operation)
    assert "The C backend does not support the '" in str(err.value)
    assert "' operator." in str(err.value)


def test_cw_binaryoperator():
    '''Check the CWriter class binary_operation method correctly
    prints out the C representation of any given BinaryOperation.

    '''
    cwriter = CWriter()

    # Test UnaryOperation without children.
    binary_operation = BinaryOperation(BinaryOperation.Operator.ADD)
    with pytest.raises(VisitorError) as err:
        _ = cwriter(binary_operation)
    assert ("BinaryOperation malformed or incomplete. It should have "
            "exactly 2 children, but found 0." in str(err.value))

    # Test with children
    ref1 = Reference(DataSymbol("a", REAL_TYPE))
    ref2 = Reference(DataSymbol("b", REAL_TYPE))
    binary_operation = BinaryOperation.create(BinaryOperation.Operator.ADD,
                                              ref1, ref2)
    assert cwriter(binary_operation) == '(a + b)'

    # Test all supported Operators
    test_list = ((BinaryOperation.Operator.ADD, '(a + b)'),
                 (BinaryOperation.Operator.SUB, '(a - b)'),
                 (BinaryOperation.Operator.MUL, '(a * b)'),
                 (BinaryOperation.Operator.DIV, '(a / b)'),
                 (BinaryOperation.Operator.POW, 'pow(a, b)'),
                 (BinaryOperation.Operator.EQ, '(a == b)'),
                 (BinaryOperation.Operator.NE, '(a != b)'),
                 (BinaryOperation.Operator.GT, '(a > b)'),
                 (BinaryOperation.Operator.GE, '(a >= b)'),
                 (BinaryOperation.Operator.LT, '(a < b)'),
                 (BinaryOperation.Operator.LE, '(a <= b)'),
                 (BinaryOperation.Operator.OR, '(a || b)'),
                 (BinaryOperation.Operator.AND, '(a && b)'))

    for operator, expected in test_list:
        binary_operation._operator = operator
        assert cwriter(binary_operation) == expected

    # Test that an unsupported operator raises a error
    class Unsupported():
        '''Dummy class'''
        def __init__(self):
            pass
    binary_operation._operator = Unsupported
    with pytest.raises(VisitorError) as err:
        _ = cwriter(binary_operation)
    assert "The C backend does not support the '" in str(err.value)
    assert "' operator." in str(err.value)


def test_cw_intrinsiccall():
    '''Check the CWriter class intrinsiccall method correctly prints out
    the C representation of any given Intrinsic.

    '''
    cwriter = CWriter()

    # Test all supported Intrinsics with 1 argument
    test_list = ((IntrinsicCall.Intrinsic.SQRT, 'sqrt(a)'),
                 (IntrinsicCall.Intrinsic.COS, 'cos(a)'),
                 (IntrinsicCall.Intrinsic.SIN, 'sin(a)'),
                 (IntrinsicCall.Intrinsic.TAN, 'tan(a)'),
                 (IntrinsicCall.Intrinsic.ACOS, 'acos(a)'),
                 (IntrinsicCall.Intrinsic.ASIN, 'asin(a)'),
                 (IntrinsicCall.Intrinsic.ATAN, 'atan(a)'),
                 (IntrinsicCall.Intrinsic.ABS, 'abs(a)'),
                 (IntrinsicCall.Intrinsic.REAL, '(float)a'))
    ref1 = Reference(DataSymbol("a", REAL_TYPE))
    icall = IntrinsicCall.create(IntrinsicCall.Intrinsic.SQRT, [ref1])
    for intrinsic, expected in test_list:
        icall._intrinsic = intrinsic
        assert cwriter(icall) == expected

    # Check that operator-style formatting with a number of children different
    # than 2 produces an error
    with pytest.raises(VisitorError) as err:
        icall._intrinsic = IntrinsicCall.Intrinsic.MOD
        _ = cwriter(icall)
    assert ("The C Writer binary_operator formatter for IntrinsicCall only "
            "supports intrinsics with 2 children, but found '%' with '1' "
            "children." in str(err.value))

    # Test all supported Intrinsics with 2 arguments
    test_list = (
                 (IntrinsicCall.Intrinsic.MOD, '(a % b)'),
                 (IntrinsicCall.Intrinsic.SIGN, 'copysign(a, b)'),
    )
    ref1 = Reference(DataSymbol("a", REAL_TYPE))
    ref2 = Reference(DataSymbol("b", REAL_TYPE))
    icall = IntrinsicCall.create(IntrinsicCall.Intrinsic.MOD, [ref1, ref2])
    for intrinsic, expected in test_list:
        icall._intrinsic = intrinsic
        assert cwriter(icall) == expected

    # Check that casts with more than one children produce an error
    with pytest.raises(VisitorError) as err:
        icall._intrinsic = IntrinsicCall.Intrinsic.REAL
        _ = cwriter(icall)
    assert ("The C Writer IntrinsicCall cast-style formatter only supports "
            "intrinsics with 1 child, but found 'float' with '2' children."
            in str(err.value))


def test_cw_loop(fortran_reader):
    '''Tests writing out a Loop node in C. It parses Fortran code
    and outputs it as C. Note that this is atm a literal translation,
    the loops are not functionally identical to Fortran, see TODO #523.

    '''
    # Generate PSyIR from Fortran code.
    code = '''
        module test
        contains
        subroutine tmp()
          integer :: i, a
          integer, dimension(:) :: b
          do i = 1, 20, 2
            a = 2 * i
          enddo
        end subroutine tmp
        end module test'''
    container = fortran_reader.psyir_from_source(code).children[0]
    module = container.children[0]

    cwriter = CWriter()
    result = cwriter(module[0])
    correct = '''for(i=1; i<=20; i+=2)
{
  a = (2 * i);
}'''
    result = cwriter(module[0])
    assert correct in result


def test_cw_unsupported_intrinsiccall():
    ''' Check the CWriter class SIZE intrinsic raises the expected error since
    there is no C equivalent. '''
    cwriter = CWriter()
    arr = ArrayReference(DataSymbol('a', INTEGER_TYPE))
    lit = Literal('1', INTEGER_TYPE)
    size = IntrinsicCall.create(IntrinsicCall.Intrinsic.SIZE,
                                [arr, ("dim", lit)])
    lhs = Reference(DataSymbol('length', INTEGER_TYPE))
    assignment = Assignment.create(lhs, size)

    with pytest.raises(VisitorError) as excinfo:
        cwriter(assignment)
    assert ("The C backend does not support the 'SIZE' intrinsic."
            in str(excinfo.value))


def test_cw_structureref(fortran_reader):
    ''' Test the CWriter support for StructureReference. '''
    code = '''
        module test
        contains
        subroutine tmp()
          type :: my_type
            integer                   :: b
            integer, dimension(10,10) :: c
            integer, dimension(10)    :: d
          end type my_type
          type(my_type) :: a, b(5)
          integer :: i
          a%b = a%c(1,2) + b(3)%d(i)%e(i+1) + a%f%g(3)
        end subroutine tmp
        end module test'''
    container = fortran_reader.psyir_from_source(code).children[0]
    module = container.children[0]

    cwriter = CWriter()
    result = cwriter(module[0])
    correct = "a.b = ((a.c[1 + 2 * cLEN1] + b[3].d[i].e[(i + 1)]) + a.f.g[3])"
    assert correct in result

    module[0].children[0]._children = []
    with pytest.raises(VisitorError) as err:
        _ = cwriter(module[0])
    assert "A StructureReference must have a single child but the " \
           "reference to symbol 'a' has 0." in str(err.value)

    ref = module[0].children[0]
    ref._children = [Literal("1", INTEGER_TYPE)]
    with pytest.raises(VisitorError) as err:
        # We can't call cwriter(), it will complain about having a Literal
        # node which is invalid. So call _visit()
        _ = cwriter._visit(ref)
    assert "A StructureReference must have a single child which is a " \
           "sub-class of Member but the reference to symbol 'a' has a " \
           "child of type " in str(err.value)


def test_cw_arraystructureref(fortran_reader):
    ''' Test the CWriter support for ArrayStructureReference. '''
    code = '''
        module test
        contains
        subroutine tmp()
          type :: my_type
            integer                   :: b
            integer, dimension(10,10) :: c
            integer, dimension(10)    :: d
          end type my_type
          type(my_type) :: a, b(5)
          integer :: i
          b(5)%d(1) = 1
        end subroutine tmp
        end module test'''
    container = fortran_reader.psyir_from_source(code).children[0]
    module = container.children[0]

    cwriter = CWriter()
    result = cwriter(module[0])
    correct = "b[5].d[1] = 1"
    assert correct in result

    array_ref = module[0].children[0]
    array_ref._children = []
    with pytest.raises(VisitorError) as err:
        # We can't call cwriter(), it will complain about having a Literal
        # node which is invalid. So call _visit()
        _ = cwriter._visit(array_ref)
    assert "An ArrayOfStructuresReference must have at least two children " \
           "but found 0" in str(err.value)

    array_ref._children = [Literal("1", INTEGER_TYPE),
                           Literal("1", INTEGER_TYPE)]
    with pytest.raises(VisitorError) as err:
        # We can't call cwriter(), it will complain about having a Literal
        # node which is invalid. So call _visit()
        _ = cwriter._visit(array_ref)
    assert "An ArrayOfStructuresReference must have a Member as its first " \
           "child but found 'Literal'" in str(err.value)


def test_cw_directive_with_clause(fortran_reader):
    '''Test that a PSyIR directive with clauses is translated to
    the required C code.

    '''
    cwriter = CWriter()
    # Generate PSyIR from Fortran code.
    code = (
        "program test\n"
        "  integer, parameter :: n=20\n"
        "  integer :: i\n"
        "  real :: a(n)\n"
        "  do i=1,n\n"
        "    a(i) = 0.0\n"
        "  end do\n"
        "end program test")
    container = fortran_reader.psyir_from_source(code)
    schedule = container.children[0]
    loops = schedule.walk(Loop)
    loop = loops[0].detach()
    directive = OMPTaskloopDirective(children=[loop], num_tasks=32,
                                     nogroup=True)
    master = OMPMasterDirective(children=[directive])
    parallel = OMPParallelDirective.create(children=[master])
    schedule.addchild(parallel, 0)
    assert '''#pragma omp parallel default(shared), private(i)
{
  #pragma omp master
  {
    #pragma omp taskloop num_tasks(32), nogroup
    {
      for(i=1; i<=n; i+=1)
      {
        a[i] = 0.0;
      }
    }
  }
}
''' in cwriter(schedule.children[0])
