# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019, Science and Technology Facilities Council.
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
# -----------------------------------------------------------------------------

'''Performs pytest tests on the psyclond.psyir.backend.opencl module'''

import pytest
from psyclone.psyir.backend.base import VisitorError
from psyclone.psyir.backend.opencl import OpenCLWriter
from psyclone.psyGen import Symbol, SymbolTable, KernelSchedule, Return


def test_oclw_gen_id_variable():
    '''Check the OpenCLWriter class gen_id_variables method produces
    the expected declarations.

    '''
    oclwriter = OpenCLWriter()
    symbol = Symbol("id1", "integer")
    result = oclwriter.gen_id_variable(symbol, 3)
    assert result == "int id1 = get_global_id(3);\n"

    symbol = Symbol("array", "integer", shape=[2, None, 2])
    with pytest.raises(VisitorError) as excinfo:
        _ = oclwriter.gen_id_variable(symbol, 3)
    assert "OpenCL work-item identifiers must be scalar integer symbols " \
        "but found" in str(excinfo)


def test_oclw_gen_declaration():
    '''Check the OpenCLWriter class gen_declaration method produces
    the expected declarations.

    '''
    oclwriter = OpenCLWriter()

    # Basic entry - Scalar are passed by value and don't have additional
    # qualifiers.
    symbol = Symbol("dummy1", "integer")
    result = oclwriter.gen_declaration(symbol)
    assert result == "int dummy1"

    # Array argument has a memory qualifier (only __global for now)
    symbol = Symbol("dummy2", "integer", shape=[2, None, 2])
    result = oclwriter.gen_declaration(symbol)
    assert result == "__global int * restrict dummy2"

    # Array with unknown intent
    symbol = Symbol("dummy2", "integer", shape=[2, None, 2],
                    interface=Symbol.Argument(access=Symbol.Access.UNKNOWN))
    result = oclwriter.gen_declaration(symbol)
    assert result == "__global int * restrict dummy2"


def test_oclw_gen_array_length_variables():
    '''Check the OpenCLWriter class gen_array_length_variables method produces
    the expected declarations.

    '''
    oclwriter = OpenCLWriter()

    # An scalar should not return any LEN variables
    symbol1 = Symbol("dummy2LEN1", "integer")
    result = oclwriter.gen_array_length_variables(symbol1)
    assert result == ""

    # Array with 1 dimension generates 1 length variable
    symbol2 = Symbol("dummy1", "integer", shape=[2])
    result = oclwriter.gen_array_length_variables(symbol2)
    assert result == "int dummy1LEN1 = get_global_size(0);\n"

    # Array with multiple dimension generates one variable per dimension
    symbol3 = Symbol("dummy2", "integer", shape=[2, None, 2])
    result = oclwriter.gen_array_length_variables(symbol3)
    assert result == "int dummy2LEN1 = get_global_size(0);\n" \
        "int dummy2LEN2 = get_global_size(1);\n" \
        "int dummy2LEN3 = get_global_size(2);\n"

    # Create a symbol table
    symtab = SymbolTable()
    symtab.add(symbol1)
    symtab.add(symbol2)
    symtab.add(symbol3)

    # If there are no name clashed, generatre array length variables.
    result = oclwriter.gen_array_length_variables(symbol2, symtab)
    assert result == "int dummy1LEN1 = get_global_size(0);\n"

    with pytest.raises(VisitorError) as excinfo:
        _ = oclwriter.gen_array_length_variables(symbol3, symtab)
    assert "Unable to declare the variable 'dummy2LEN1' to store the length " \
        "of 'dummy2' because the Symbol Table already contains a symbol with" \
        " the same name." in str(excinfo)


def test_oclw_kernelschedule():
    '''Check the OpenCLWriter class kernelschedule_node visitor produces
    the expected C code.

    '''

    # The kernelschedule OpenCL Backend relies on abstrct methods that
    # need to be implemented by the APIs. A generic kernelschedule will
    # produce a NotImplementedError.
    oclwriter = OpenCLWriter()
    kschedule = KernelSchedule("kname")
    with pytest.raises(NotImplementedError) as excinfo:
        _ = oclwriter(kschedule)

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
    interface = Symbol.Argument(access=Symbol.Access.UNKNOWN)
    i = Symbol('i', 'integer', interface=interface)
    j = Symbol('j', 'integer', interface=interface)
    data1 = Symbol('data1', 'real', [10, 10], interface=interface)
    data2 = Symbol('data2', 'real', [10, 10], interface=interface)
    kschedule.symbol_table.add(i)
    kschedule.symbol_table.add(j)
    kschedule.symbol_table.add(data1)
    kschedule.symbol_table.add(data2)
    kschedule.symbol_table.specify_argument_list([i, j, data1, data2])
    kschedule.addchild(Return(parent=kschedule))

    result = oclwriter(kschedule)
    print(result)
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
        "}\n"
