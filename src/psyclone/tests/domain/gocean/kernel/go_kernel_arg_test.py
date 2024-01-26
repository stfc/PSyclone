# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2024, Science and Technology Facilities Council.
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
# ----------------------------------------------------------------------------
# Authors A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified J. Henrichs, Bureau of Meteorology
# Modified R. W. Ford, STFC Daresbury Lab
# Modified I. Kavcic, Met Office

'''Tests of handling of arguments for GOcean kernels.
'''

import os

import pytest
from psyclone.configuration import Config
from psyclone.domain.gocean import GOSymbolTable
from psyclone.errors import InternalError, GenerationError
from psyclone.gocean1p0 import GOKernelArgument, GOKernelArguments
from psyclone.parse.algorithm import Arg, parse
from psyclone.parse.kernel import Descriptor
from psyclone.parse.utils import ParseError
from psyclone.psyir.nodes import (Node, StructureReference, Member,
                                  StructureMember, Reference, Literal)
from psyclone.psyir.symbols import (SymbolTable, UnresolvedType, DataSymbol,
                                    ScalarType, INTEGER_TYPE, REAL_TYPE,
                                    ArgumentInterface, DataTypeSymbol)
from psyclone.tests.utilities import get_base_path, get_invoke


API = "gocean1.0"


@pytest.fixture(scope="module", autouse=True)
def setup():
    '''Make sure that all tests here use gocean1.0 as API.'''
    Config.get().api = API
    yield
    Config._instance = None


def test_gokernelarguments_append():
    ''' Check the GOcean specialisation of KernelArguments append method'''

    # Parse a file to get an initialised GOKernelsArguments object
    psy, invoke = get_invoke("single_invoke.f90", API, idx=0)
    symtab = invoke.schedule.symbol_table
    kernelcall = invoke.schedule.coded_kernels()[0]
    argument_list = kernelcall.arguments
    assert isinstance(argument_list, GOKernelArguments)

    # Try append a non-string value
    with pytest.raises(TypeError) as err:
        argument_list.append(3, "space")
    assert "The name parameter given to GOKernelArguments.append method " \
           "should be a string, but found 'int' instead." in str(err.value)

    # Append well-constructed arguments
    var1 = symtab.new_symbol("var1", symbol_type=DataSymbol,
                             datatype=REAL_TYPE)
    var2 = symtab.new_symbol("var2", symbol_type=DataSymbol,
                             datatype=INTEGER_TYPE)
    argument_list.append(var1.name, "go_r_scalar")
    argument_list.append(var2.name, "go_i_scalar")

    assert isinstance(kernelcall.args[-1], GOKernelArgument)
    assert isinstance(kernelcall.args[-2], GOKernelArgument)
    assert kernelcall.args[-1].name == "var2"
    assert kernelcall.args[-2].name == "var1"

    # And the generated code looks as expected
    generated_code = str(psy.gen)
    assert "CALL compute_cu_code(i, j, cu_fld%data, p_fld%data, u_fld%data," \
           " var1, var2)" in generated_code


def test_gokernelargument_infer_datatype():
    ''' Check the GOcean specialisation of the infer_datatype works for each
    possible type of KernelArgument. '''

    # Parse an invoke with a scalar float and a field
    _, invoke = get_invoke("single_invoke_scalar_float_arg.f90", API, idx=0)
    kernelcall = invoke.schedule.coded_kernels()[0]
    argument_list = kernelcall.arguments

    # The first argument is a scalar Real
    datatype = argument_list.args[0].infer_datatype()
    assert isinstance(datatype, ScalarType)
    assert datatype.intrinsic == ScalarType.Intrinsic.REAL
    assert datatype.precision.name == "go_wp"

    # The second argument is a r2d_field (imported DataTypeSymbol)
    assert isinstance(argument_list.args[1].infer_datatype(), DataTypeSymbol)
    assert argument_list.args[1].infer_datatype().name == "r2d_field"

    _, invoke = get_invoke("single_invoke_scalar_int_arg.f90", API, idx=0)

    kernelcall = invoke.schedule.coded_kernels()[0]
    argument_list = kernelcall.arguments

    # The first argument is a scalar Integer
    assert argument_list.args[0].infer_datatype() == INTEGER_TYPE

    # The second argument is a r2d_field (imported DataTypeSymbol)
    assert isinstance(argument_list.args[1].infer_datatype(), DataTypeSymbol)
    assert argument_list.args[1].infer_datatype().name == "r2d_field"

    # Test an incompatible Kernel Argument
    argument_list.args[0]._arg._space = "incompatible"
    with pytest.raises(InternalError) as excinfo:
        _ = argument_list.args[0].infer_datatype()
    assert ("GOcean expects scalar arguments to be of 'go_r_scalar' or "
            "'go_i_scalar' type but found 'incompatible'."
            in str(excinfo.value))

    argument_list.args[0]._arg._argument_type = "incompatible"
    with pytest.raises(InternalError) as excinfo:
        _ = argument_list.args[0].infer_datatype()
    assert ("GOcean expects the Argument.argument_type() to be 'field' or "
            "'scalar' but found 'incompatible'." in str(excinfo.value))


def test_gokernelargument_intrinsic_type():
    ''' Check that the GOcean specialisation of the intrinsic_type returns the
    expected values. '''

    # Parse an invoke with a scalar float and a field
    _, invoke = get_invoke("single_invoke_scalar_float_arg.f90", API, idx=0)
    kernelcall = invoke.schedule.coded_kernels()[0]
    argument_list = kernelcall.arguments.args
    # First argument 'a_scalar' is a REAL
    assert argument_list[0].intrinsic_type == "real"
    # Second argument 'ssh_fld' is a derived type and doesn't have a single
    # intrinsic type, so it returns an empty string
    assert argument_list[1].intrinsic_type == ""
    # Change the first argument metadata type to integer, and check the
    # intrinsic_type value also changes
    argument_list[0]._arg._space = "go_i_scalar"
    assert argument_list[0].intrinsic_type == "integer"


def test_gokernelarguments_psyir_expressions():
    ''' Check the GOcean specialisation of psyir_expressions returns the
    expected list of PSyIR expressions for each argument'''

    # Parse an invoke with grid properties
    _, invoke = get_invoke("single_invoke_grid_props.f90", API, idx=0)
    kernelcall = invoke.schedule.coded_kernels()[0]
    argument_list = kernelcall.arguments.psyir_expressions()

    # It has 2 indices arguments plus the kernel arguments
    assert len(argument_list) == len(kernelcall.arguments.args) + 2

    # Second argument is a reference to the symbol tagged contiguous_kidx
    assert isinstance(argument_list[0], Reference)
    assert (argument_list[0].symbol is
            kernelcall.scope.symbol_table.lookup_with_tag("contiguous_kidx"))

    # Second argument is a reference to the symbol tagged noncontiguous_kidx
    assert isinstance(argument_list[1], Reference)
    assert (argument_list[1].symbol is
            kernelcall.scope.symbol_table.lookup_with_tag(
                "noncontiguous_kidx"))

    # Other arguments are also PSyIR expressions generated depending on the
    # argument type. In this case it has 5 more arguments, all of them are
    # structure references.
    for argument in argument_list[2:7]:
        assert isinstance(argument, StructureReference)


def test_gokernelargument_psyir_expression():
    ''' Check the GOcean specialisation of psyir_expression returns the
    expected expression for any GOKernelArgument and GOKernelGridArguments'''

    # Parse an invoke with grid properties
    _, invoke = get_invoke("single_invoke_grid_props.f90", API, idx=0)
    kernelcall = invoke.schedule.coded_kernels()[0]
    argument_list = kernelcall.arguments

    # The first argument is a field
    expr1 = argument_list.args[0].psyir_expression()
    assert isinstance(expr1, StructureReference)
    assert isinstance(expr1.member, Member)
    assert expr1.member.name == "data"

    # Third argument is a tmask grid property
    expr2 = argument_list.args[2].psyir_expression()
    assert isinstance(expr2, StructureReference)
    assert isinstance(expr2.member, StructureMember)
    assert isinstance(expr2.member.member, Member)
    assert expr2.member.name == "grid"
    assert expr2.member.member.name == "tmask"

    # Parse an invoke with a scalar int and a field
    _, invoke = get_invoke("single_invoke_scalar_int_arg.f90", API, idx=0)
    kernelcall = invoke.schedule.coded_kernels()[0]
    argument_list = kernelcall.arguments

    # The first argument is a scalar
    expr3 = argument_list.args[0].psyir_expression()
    assert isinstance(expr3, Reference)

    # Test an incompatible Kernel Argument
    argument_list.args[0]._arg._argument_type = "incompatible"
    with pytest.raises(InternalError) as excinfo:
        _ = argument_list.args[0].psyir_expression()
    assert ("GOcean expects the Argument.argument_type() to be 'field' or "
            "'scalar' but found 'incompatible'." in str(excinfo.value))


def test_gokernelargument_constant_psyir_expression():
    '''Test various constant arguments and their conversion to PSyIR.
    '''

    # Parse an invoke with a scalar int and a field
    _, invoke = get_invoke("single_invoke_scalar_float_arg.f90", API, idx=0)
    kernelcall = invoke.schedule.coded_kernels()[0]
    argument_list = kernelcall.arguments

    for (const, intr_type) in [("1", ScalarType.Intrinsic.INTEGER),
                               ("1.0", ScalarType.Intrinsic.REAL),
                               ("1.0e+0", ScalarType.Intrinsic.REAL),
                               ("1.0E-0", ScalarType.Intrinsic.REAL)]:
        argument_list.args[0]._name = const
        expr = argument_list.args[0].psyir_expression()
        assert isinstance(expr, Literal)
        assert expr.datatype.intrinsic == intr_type


def test_gokernelargument_type(monkeypatch):
    ''' Check the type property of the GOKernelArgument'''

    # Create a dummy node with the symbol_table property
    dummy_node = Node()
    dummy_node.symbol_table = SymbolTable()

    # Create a dummy GOKernelArgument
    descriptor = Descriptor(None, "go_r_scalar", 0)
    arg = Arg("variable", "arg", "arg")
    argument = GOKernelArgument(descriptor, arg, dummy_node)

    # If the descriptor does not have a type it defaults to 'scalar'
    assert argument.argument_type == "scalar"

    # Otherwise it returns the descriptor type
    # Mock the descriptor type method
    monkeypatch.setattr(argument._arg, "_argument_type", "descriptor_type")
    assert argument.argument_type == "descriptor_type"

    # Test that the expected exception is raised in method
    # _check_gocean_conformity within GOSymbolTable when one or both of
    # the first two kernel arguments are nor scalar integers.

    symbol_table = GOSymbolTable()
    i_var = DataSymbol("i", INTEGER_TYPE,
                       interface=ArgumentInterface(
                           ArgumentInterface.Access.READ))
    j_var = DataSymbol("j", INTEGER_TYPE,
                       interface=ArgumentInterface(
                           ArgumentInterface.Access.READ))
    symbol_table.specify_argument_list([i_var, j_var])
    # Set the datatype of the first datasymbol to have an invalid type
    # in order to raise the required exception.
    symbol_table._argument_list[0].datatype = UnresolvedType()
    with pytest.raises(GenerationError) as excinfo:
        symbol_table._check_gocean_conformity()
    assert ("GOcean API kernels first argument should be a scalar integer "
            "but got 'UnresolvedType'." in str(excinfo.value))


def test_gokernelargument_invalid_type():
    '''Tests that invalid kernel argument types are handled correctly.
    '''
    # Parse an existing kernel to create the required kernel_call
    # type.
    _, invoke_info = parse(os.path.join(get_base_path(API),
                                        "single_invoke_scalar_float_arg.f90"),
                           api=API)

    kernel_call = invoke_info.calls[0].kcalls[0]
    arg_descriptors = kernel_call.ktype.arg_descriptors

    # Now modify the argument type to be invalid.
    arg_descriptors[0]._argument_type = "INVALID"
    with pytest.raises(ParseError) as err:
        _ = GOKernelArguments(kernel_call, None)

    assert ("Invalid kernel argument type. Found 'INVALID' but must be one of "
            "['grid_property', 'scalar', 'field']" in str(err.value))


def test_gokernelarguments_scalar():
    '''Tests the GOKernelArguments.scalar property.
    '''
    _, invoke = get_invoke("single_invoke_scalar_float_arg.f90", API, idx=0)
    kern = invoke.schedule.coded_kernels()[0]
    assert kern.arguments.scalars == ['a_scalar']

    # Use 'set' to make sure the test is independent of order
    assert (set(kern.arguments.acc_args) ==
            set(['ssh_fld', 'ssh_fld%data', 'a_scalar', 'ssh_fld%grid',
                 'ssh_fld%grid%xstop', 'ssh_fld%grid%tmask']))
