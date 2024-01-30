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

'''Performs pytest tests on the psyclond.psyir.backend.opencl module'''

import pytest
from psyclone.psyir.backend.visitor import VisitorError
from psyclone.psyir.backend.opencl import OpenCLWriter
from psyclone.psyir.nodes import Return, KernelSchedule
from psyclone.psyir.symbols import DataSymbol, SymbolTable, \
    ArgumentInterface, UnresolvedInterface, ArrayType, REAL_TYPE, \
    INTEGER_TYPE


def test_oclw_initialization():
    '''Test that the OpenCLWriter-specific parameters are error checked'''

    # OCLWriter can be initialized with default values
    oclwriter = OpenCLWriter()
    assert oclwriter._kernels_local_size == 1

    # Pass a kernels_local_size parameter
    with pytest.raises(TypeError) as error:
        oclwriter = OpenCLWriter(kernels_local_size='invalid')
    assert "kernel_local_size should be an integer but found 'str'." \
        in str(error.value)

    with pytest.raises(ValueError) as error:
        oclwriter = OpenCLWriter(kernels_local_size=-4)
    assert "kernel_local_size should be a positive integer but found -4." \
        in str(error.value)

    oclwriter = OpenCLWriter(kernels_local_size=4)
    assert oclwriter._kernels_local_size == 4


def test_oclw_gen_id_variable():
    '''Check the OpenCLWriter class gen_id_variables method produces
    the expected declarations.

    '''
    oclwriter = OpenCLWriter()
    symbol = DataSymbol("id1", INTEGER_TYPE)
    result = oclwriter.gen_id_variable(symbol, 3)
    assert result == "int id1 = get_global_id(3);\n"

    array_type = ArrayType(INTEGER_TYPE, [ArrayType.Extent.ATTRIBUTE,
                                          ArrayType.Extent.ATTRIBUTE,
                                          ArrayType.Extent.ATTRIBUTE])
    symbol = DataSymbol("array", array_type)
    with pytest.raises(VisitorError) as excinfo:
        _ = oclwriter.gen_id_variable(symbol, 3)
    assert "OpenCL work-item identifiers must be scalar integer symbols " \
        "but found" in str(excinfo.value)


def test_oclw_gen_declaration():
    '''Check the OpenCLWriter class gen_declaration method produces
    the expected declarations.

    '''
    oclwriter = OpenCLWriter()

    # Basic entry - Scalar are passed by value and don't have additional
    # qualifiers.
    symbol = DataSymbol("dummy1", INTEGER_TYPE)
    result = oclwriter.gen_declaration(symbol)
    assert result == "int dummy1"

    # Array argument has a memory qualifier (only __global for now)
    array_type = ArrayType(INTEGER_TYPE, [ArrayType.Extent.ATTRIBUTE,
                                          ArrayType.Extent.ATTRIBUTE,
                                          ArrayType.Extent.ATTRIBUTE])
    symbol = DataSymbol("dummy2", array_type)
    result = oclwriter.gen_declaration(symbol)
    assert result == "__global int * restrict dummy2"

    # Array with unknown intent
    symbol = DataSymbol("dummy2", array_type,
                        interface=ArgumentInterface(
                            ArgumentInterface.Access.UNKNOWN))
    result = oclwriter.gen_declaration(symbol)
    assert result == "__global int * restrict dummy2"

    # Array with a lower bound other than 1
    array_type = ArrayType(INTEGER_TYPE, [2, ArrayType.ArrayBounds(2, 5)])
    symbol = DataSymbol("dummy3", array_type)
    with pytest.raises(VisitorError) as err:
        oclwriter.gen_declaration(symbol)
    assert ("a lower bound of 1 in each dimension. However, array 'dummy3' "
            "has a lower bound of '2' for dimension 1" in str(err.value))


def test_oclw_gen_array_length_variables():
    '''Check the OpenCLWriter class gen_array_length_variables method produces
    the expected declarations.

    '''
    oclwriter = OpenCLWriter()

    # A scalar should not return any LEN variables
    symbol1 = DataSymbol("dummy2LEN1", INTEGER_TYPE)
    result = oclwriter.gen_array_length_variables(symbol1)
    assert result == ""

    # Array with 1 dimension generates 1 length variable
    array_type = ArrayType(INTEGER_TYPE, [2])
    symbol2 = DataSymbol("dummy1", array_type)
    result = oclwriter.gen_array_length_variables(symbol2)
    assert result == "int dummy1LEN1 = get_global_size(0);\n"

    # Array with multiple dimension generates one variable per dimension
    array_type = ArrayType(INTEGER_TYPE, [ArrayType.Extent.ATTRIBUTE,
                                          ArrayType.Extent.ATTRIBUTE,
                                          ArrayType.Extent.ATTRIBUTE])
    symbol3 = DataSymbol("dummy2", array_type)
    result = oclwriter.gen_array_length_variables(symbol3)
    assert result == "int dummy2LEN1 = get_global_size(0);\n" \
        "int dummy2LEN2 = get_global_size(1);\n" \
        "int dummy2LEN3 = get_global_size(2);\n"

    # Create a symbol table
    symtab = SymbolTable()
    symtab.add(symbol1)
    symtab.add(symbol2)
    symtab.add(symbol3)

    # If there are no name clashes, generate array length variables.
    result = oclwriter.gen_array_length_variables(symbol2, symtab)
    assert result == "int dummy1LEN1 = get_global_size(0);\n"

    with pytest.raises(VisitorError) as excinfo:
        _ = oclwriter.gen_array_length_variables(symbol3, symtab)
    assert "Unable to declare the variable 'dummy2LEN1' to store the length " \
        "of 'dummy2' because the Symbol Table already contains a symbol with" \
        " the same name." in str(excinfo.value)


def test_oclw_kernelschedule():
    '''Check the OpenCLWriter class kernelschedule_node visitor produces
    the expected OpenCL code.

    '''

    # The kernelschedule OpenCL Backend relies on abstract methods that
    # need to be implemented by the APIs. A generic kernelschedule will
    # produce a NotImplementedError.
    oclwriter = OpenCLWriter()
    kschedule = KernelSchedule("kname")
    with pytest.raises(NotImplementedError) as error:
        _ = oclwriter(kschedule)
    assert "Abstract property. Which symbols are data arguments is " \
        "API-specific." in str(error.value)

    # Mock abstract properties. (pytest monkeypatch does not work
    # with properties, used sub-class instead)
    class MockSymbolTable(SymbolTable):
        ''' Mock needed abstract methods of the Symbol Table '''
        @property
        def iteration_indices(self):
            return self.argument_list[:2]

        @property
        def data_arguments(self):
            return self.argument_list[2:]
    kschedule.symbol_table.__class__ = MockSymbolTable

    # Create a sample symbol table and kernel schedule
    interface = ArgumentInterface(ArgumentInterface.Access.UNKNOWN)
    i = DataSymbol('i', INTEGER_TYPE, interface=interface)
    j = DataSymbol('j', INTEGER_TYPE, interface=interface)
    array_type = ArrayType(REAL_TYPE, [10, 10])
    data1 = DataSymbol('data1', array_type, interface=interface)
    data2 = DataSymbol('data2', array_type, interface=interface)
    kschedule.symbol_table.add(i)
    kschedule.symbol_table.add(j)
    kschedule.symbol_table.add(data1)
    kschedule.symbol_table.add(data2)
    kschedule.symbol_table.specify_argument_list([i, j, data1, data2])
    kschedule.addchild(Return(parent=kschedule))

    result = oclwriter(kschedule)
    assert result == "" \
        "__kernel void kname(\n" \
        "  __global double * restrict data1,\n" \
        "  __global double * restrict data2\n" \
        "  ){\n" \
        "  int data1LEN1 = get_global_size(0);\n" \
        "  int data1LEN2 = get_global_size(1);\n" \
        "  int data2LEN1 = get_global_size(0);\n" \
        "  int data2LEN2 = get_global_size(1);\n" \
        "  int i = get_global_id(0);\n" \
        "  int j = get_global_id(1);\n" \
        "  return;\n" \
        "}\n\n"

    # Set a local_size value different to 1 into the KernelSchedule
    oclwriter = OpenCLWriter(kernels_local_size=4)
    result = oclwriter(kschedule)

    assert result == "" \
        "__attribute__((reqd_work_group_size(4, 1, 1)))\n" \
        "__kernel void kname(\n" \
        "  __global double * restrict data1,\n" \
        "  __global double * restrict data2\n" \
        "  ){\n" \
        "  int data1LEN1 = get_global_size(0);\n" \
        "  int data1LEN2 = get_global_size(1);\n" \
        "  int data2LEN1 = get_global_size(0);\n" \
        "  int data2LEN2 = get_global_size(1);\n" \
        "  int i = get_global_id(0);\n" \
        "  int j = get_global_id(1);\n" \
        "  return;\n" \
        "}\n\n"

    # Add a symbol with an UnresolvedInterface and check that this raises the
    # expected error
    array_type = ArrayType(REAL_TYPE, [10, 10])
    kschedule.symbol_table.add(DataSymbol('broken', array_type,
                                          interface=UnresolvedInterface()))
    with pytest.raises(VisitorError) as err:
        _ = oclwriter(kschedule)
    assert ("symbol table contains unresolved data entries (i.e. that have no "
            "defined Interface) which are not used purely to define the "
            "precision of other symbols: 'broken'" in str(err.value))
