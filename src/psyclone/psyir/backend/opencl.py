# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019, Science and Technology Facilities Council
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

'''OpenCL PSyIR backend. Extends the C PSyIR back-end to generate
OpenCL code from PSyIR nodes.

'''

from psyclone.psyir.backend.base import VisitorError
from psyclone.psyir.backend.C import CWriter

class OpenCLWriter(CWriter):
    '''Implements a PSyIR-to-OpenCLC back-end for the PSyIR AST.

    '''

    def gen_id_variables(self, symbol, id_number):
        '''
        Generate the declaration and initialization of OpenCL id variables.

        :param symbol: The symbol instance.
        :type symbol: :py:class:`psyclone.psyGen.Symbol`
        :param int id_number: Identifier for the variable.

        :returns: OpenCL declaration for a given OpenCL ID.
        :rtype: str
        '''
        code = ""
        code += self._nindent + "int " + symbol.name
        code += " = get_global_id(" + str(index) + ");\n"
        return code

    def gen_declaration(self, indent=0):
        '''
        Generate kernel arguments: in OpenCL we ignore the iteration
        indices which in GOcean are the first two arguments.

        :param indent: Depth of indent for the output string.
        :return: OpenCL argument list for the Symbol Table.
        :rtype: str
        '''
        prefix = ""
        # If argument is an array, it is allocated to the OpenCL global
        # address space.
        if symbol.shape:
            prefix += "__global "
        return prefix + super(OpenCLWriter, self).gen_declaration()


    def gen_array_length(self, symbol, symtab):
        '''
        Generate a <name>LEN<DIM> variable for each array dimension.

        :return: Length OpenCL declaration and initialisation for each dimension.
        :rtype: str
        :raises GenerationError: if the array length variable name clashes \
                with another symbol name.
        '''

        code = ""
        dimensions = len(symbol.shape)
        for dim in range(1, dimensions + 1):
            code += self.n_indent + "int "
            varname = symbol.name + "LEN" + str(dim)

            # Check there is no clash with other variables
            if varname in symtab:
                kname = ""
                if self._kernel:
                    kname = "'{0}'".format(self._kernel.name)
                raise GenerationError(
                    "Unable to declare the variable '{0}' to store the "
                    "length of '{1}' because the kernel {2} already "
                    "contains a symbol with the same name."
                    "".format(varname, symbol.name, kname))

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

        '''
        # OpenCL implementation assumptions:
        # - All array have the same size and it is given by the
        #   global_work_size argument to clEnqueueNDRangeKernel.
        # - Assumes no dependencies among kernels called concurrently.

        # TODO: At the moment, the method caller is responsible to ensure
        # these assumptions. KernelSchedule access to the kernel
        # meta-arguments could be used to check them and also improve the
        # generated code. (Issue #288)

        symtab = node.symbol_table
        data_args = symtab.data_arguments

        # Start OpenCL kernel definition
        code = self._nindent + "__kernel void " + node.name + "(\n"
        code += ",\n".join([self.gen_declarations(s) for s in data_args])
        code += "\n" 
        self._depth += 1
        code += self._nindent + "){\n"

        # Declare local variables.
        for symbol in symtab.local_symbols():
            code += self.gen_local_variable(symbol)

        # Declare array length
        for symbol in data_args:
            code += self.gen_array_length(symbol, symtab)

        # Declare iteration indices
        for index, symbol in enumerate(symtab.get_iteration_indices()):
            code += self.gen_id_variable(symbol, index)

        # Generate kernel body
        for child in node.children:
            code += self._visit(child) + "\n"

        # Close kernel definition
        self._depth -= 1
        code += self._nindent + "}\n"

        return code
