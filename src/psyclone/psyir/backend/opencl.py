# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2020, Science and Technology Facilities Council
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
# Author S. Siso, STFC Daresbury Lab.
# Modified A. R. Porter and R. W. Ford, STFC Daresbury Lab.

'''OpenCL PSyIR backend. Extends the C PSyIR back-end to generate
OpenCL code from PSyIR nodes.

'''

from psyclone.psyir.backend.visitor import VisitorError
from psyclone.psyir.backend.c import CWriter
from psyclone.psyir.symbols import ScalarType


class OpenCLWriter(CWriter):
    '''Implements a PSyIR-to-OpenCL back-end for the PSyIR AST. This writer
    produces OpenCL code conforming to Version 1.2 of the specification
    (https://www.khronos.org/registry/OpenCL/specs/opencl-1.2.pdf).

    :param bool skip_nodes: if skip_nodes is False then an exception \
    is raised if a visitor method for a PSyIR node has not been \
    implemented, otherwise the visitor silently continues. This is an \
    optional argument which defaults to False.
    :param indent_string: specifies what to use for indentation. This \
    is an optional argument that defaults to two spaces.
    :type indent_string: str or NoneType
    :param int initial_indent_depth: specifies how much indentation to \
    start with. This is an optional argument that defaults to 0.
    :param int kernel_local_size: uses the given local_size when generating \
    OpenCL kernels.

    :raises TypeError: if kernel_local_size is not an integer.
    :raises ValueError: if kernel_local_size is not positive.

    '''
    def __init__(self, skip_nodes=False, indent_string="  ",
                 initial_indent_depth=0, kernels_local_size=1):

        super(OpenCLWriter, self).__init__(
            skip_nodes, indent_string, initial_indent_depth)

        if not isinstance(kernels_local_size, int):
            raise TypeError(
                "kernel_local_size should be an integer but found "
                "'{0}'.".format(type(kernels_local_size).__name__))

        if kernels_local_size < 1:
            raise ValueError(
                "kernel_local_size should be a positive integer but found "
                "{0}.".format(kernels_local_size))

        self._kernels_local_size = kernels_local_size

    def gen_id_variable(self, symbol, dimension_index):
        '''
        Generate the declaration and initialisation of a variable identifying
        an OpenCL work-item. IDs are initialised by the OpenCL function:
        `size_t get_global_id(uint dimindx)`

        :param symbol: The symbol instance.
        :type symbol: :py:class:`psyclone.psyir.symbols.DataSymbol`
        :param int dimension_index: Dimension which the given symbol will \
            iterate on.

        :returns: OpenCL declaration of an OpenCL id variable.
        :rtype: str

        :raises VisitorError: if symbol is not a scalar integer
        '''
        if (not isinstance(symbol.datatype, ScalarType) or
                symbol.datatype.intrinsic != ScalarType.Intrinsic.INTEGER):
            raise VisitorError(
                "OpenCL work-item identifiers must be scalar integer symbols "
                "but found {0}.".format(str(symbol)))

        code = ""
        code += self._nindent + "int " + symbol.name
        code += " = get_global_id(" + str(dimension_index) + ");\n"
        return code

    def gen_declaration(self, symbol):
        '''
        Generate the declaration of an OpenCL variable. This can be either
        a local variable or a routine argument, so no indention or punctuation
        is generated by this method.

        Memory buffers reside in specific levels of the memory hierarchy,
        and pointers are annotated with the region qualifiers __global,
        __local, __constant, and __private, reflecting this.
        At the moment all memory buffers (PSyIR arrays) are allocated at
        the global address space.

        :param symbol: The symbol instance.
        :type symbol: :py:class:`psyclone.psyir.symbols.DataSymbol`

        :returns: The OpenCL declaration of the given of the symbol.
        :rtype: str
        '''
        prefix = ""
        if symbol.shape:
            prefix += "__global "
        return prefix + super(OpenCLWriter, self).gen_declaration(symbol)

    def gen_array_length_variables(self, symbol, symtab=None):
        '''
        Generate length variable declaration and initialisation for each array
        dimension of the given symbol. By convention they are named:
        <name>LEN<DIM>, and initialised using the function:
        `size_t get_global_size(uint dimindx)`.


        :param symbol: The symbol instance.
        :type symbol: :py:class:`psyclone.psyir.symbols.DataSymbol`
        :param symtab: The symbol table from the given symbol context to \
            check for name clashes.
        :type symtab: :py:class:`psyclone.psyir.symbols.SymbolTable`

        :return: OpenCL declaration and initialisation of length variables.
        :rtype: str

        :raises VisitorError: if the array length variable name clashes \
            with another symbol name.
        '''

        code = ""
        dimensions = len(symbol.shape)
        for dim in range(1, dimensions + 1):
            code += self._nindent + "int "
            varname = symbol.name + "LEN" + str(dim)

            # Check there is no clash with other variables
            if symtab and varname in symtab:
                raise VisitorError(
                    "Unable to declare the variable '{0}' to store the "
                    "length of '{1}' because the Symbol Table already "
                    "contains a symbol with the same name."
                    "".format(varname, symbol.name))

            code += varname + " = get_global_size("
            code += str(dim - 1) + ");\n"
        return code

    def kernelschedule_node(self, node):
        '''This method is called when a KernelSchedule instance is found in
        the PSyIR tree.

        :param node: A KernelSchedule PSyIR node.
        :type node: :py:class:`psyclone.psyGen.KernelSchedule`

        :returns: The OpenCL code as a string.
        :rtype: str

        :raises VisitorError: if a non-precision symbol is found with a \
                              deferred interface.
        '''
        # OpenCL implementation assumptions:
        # - All array have the same size and it is given by the
        #   global_work_size argument to clEnqueueNDRangeKernel.
        # - Assumes no dependencies among kernels called concurrently.
        # - All real variables are 64-bit

        # TODO: At the moment, the method caller is responsible to ensure
        # these assumptions. KernelSchedule access to the kernel
        # meta-arguments could be used to check them and also improve the
        # generated code. (Issue #288)

        symtab = node.symbol_table
        data_args = symtab.data_arguments

        # Check that we know where everything in the symbol table
        # comes from.  TODO #592 ultimately precision symbols should
        # be included in this check too as we will need to be able to
        # map from them to the equivalent OpenCL type.
        unresolved_datasymbols = symtab.get_unresolved_datasymbols(
            ignore_precision=True)
        if unresolved_datasymbols:
            symbols_txt = ", ".join(
                ["'" + sym + "'" for sym in unresolved_datasymbols])
            raise VisitorError(
                "Cannot generate OpenCL because the symbol table contains "
                "unresolved data entries (i.e. that have no defined Interface)"
                " which are not used purely to define the precision of other "
                "symbols: {0}".format(symbols_txt))

        # Start OpenCL kernel definition
        code = self._nindent
        if self._kernels_local_size != 1:
            code += "__attribute__((reqd_work_group_size({0}, 1, 1)))\n" \
                    "".format(self._kernels_local_size)
        code += "__kernel void " + node.name + "(\n"
        self._depth += 1
        arguments = []

        # Declare kernel arguments
        for symbol in data_args:
            arguments.append(self._nindent + self.gen_declaration(symbol))
        code += ",\n".join(arguments) + "\n"
        code += self._nindent + "){\n"

        # Declare local variables.
        for symbol in symtab.local_datasymbols:
            code += self.gen_local_variable(symbol)

        # Declare array length
        for symbol in data_args:
            code += self.gen_array_length_variables(symbol, symtab)

        # Declare iteration indices
        for index, symbol in enumerate(symtab.iteration_indices):
            code += self.gen_id_variable(symbol, index)

        # Generate kernel body
        for child in node.children:
            code += self._visit(child)

        # Close kernel definition
        self._depth -= 1
        code += self._nindent + "}\n\n"

        return code
