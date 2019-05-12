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
# Author R. W. Ford, STFC Daresbury Lab.

'''Fortran PSyIR backend. Generates Fortran code from PSyIR
nodes. Currently limited to PSyIR Kernel schedules as PSy-layer PSyIR
already has a gen() method to generate fortran.

'''

from psyclone.psyir.backend.base import PSyIRVisitor


def get_intent(symbol):
    '''Given a Symbol instance as input, determine the Fortran intent that
    the Symbol should have and return the value as a string.

    :param symbol: The symbol instance
    :type symbol: :py:class:`psyclone.psyGen.Symbol`

    :returns: the Fortran intent of the symbol instance in lower case, \
    or None if this is a local variable.

    :rtype: str or NoneType

    '''
    intent = None
    if symbol.is_input and symbol.is_output:
        intent = "inout"
    elif symbol.is_input:
        intent = "in"
    elif symbol.is_output:
        intent = "out"
    return intent


def get_dims(symbol):
    '''Given a Symbol instance as input, return a list of strings
    representing the symbols array dimensions.

    :param symbol: The symbol instance
    :type symbol: :py:class:`psyclone.psyGen.Symbol`
    :returns: the Fortran representation of the symbol's dimensions as \
    a list.
    :rtype: list of str

    :raises NotImplementedError: if an unknown dimension type is \
    found.

    '''
    from psyclone.psyGen import Symbol

    dims = []
    for index in symbol.shape:
        if isinstance(index, Symbol):
            # references another symbol
            dims.append(index.name)
        elif isinstance(index, int):
            # literal constant
            dims.append(str(index))
        else:
            raise NotImplementedError(
                "unsupported get_dims index '{0}'".format(str(index)))
    return dims


def get_kind(symbol):
    ''' From the symbol information, provide the expected Fortran kind value **************'''
    kind = None
    if symbol.datatype == "real":
        kind = "r_def"
    elif symbol.datatype == "integer":
        kind = "i_def"
    return kind


class FortranPSyIRVisitor(PSyIRVisitor):
    ''' xxx '''

    def kernelschedule_start(self, node):
        ''' xxx '''
        args = [symbol.name for symbol in node.symbol_table.argument_list]
        self._code += (
            "module {0}\n"
            "  use constants_mod, only : r_def, i_def\n"
            "  implicit none\n"
            "  contains\n"
            "  subroutine {1}({2})\n"
            "".format(node.name+"_mod", node.name, ",".join(args)))
        for symbol in node.symbol_table._symbols.values():
            intent = get_intent(symbol)
            dims = get_dims(symbol)
            kind = get_kind(symbol)
            self._code += "    {0}".format(symbol.datatype)
            if kind:
                self._code += "({0})".format(kind)
            if dims:
                self._code += ", dimension({0})".format(",".join(dims))
            if intent:
                self._code += ", intent({0})".format(intent)
            self._code += " :: {0}\n".format(symbol.name)

    def kernelschedule_end(self, node):
        ''' xxx '''
        self._code += (
            "\n  end subroutine {0}\n"
            "end module {1}\n".format(node.name, node.name+"_mod"))

    def codeblock_start(self, node):
        ''' xxx '''
        block_str = '    '
        block_str += '    '.join(str(node.ast).splitlines(True))
        self._code += str(block_str)

    def codeblock_end(self, node):
        ''' xxx '''
        pass
