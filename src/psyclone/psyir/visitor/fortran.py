''' Code to '''

from psyclone.psyir.visitor.base import PSyIRVisitor


def get_intent(symbol):
    ''' xxx '''
    intent = None
    if symbol.is_input and symbol.is_output:
        intent = "inout"
    elif symbol.is_input:
        intent = "in"
    elif symbol.is_output:
        intent = "out"
    return intent


def get_dims(symbol):
    ''' xxx '''
    dims = []
    from psyclone.psyGen import Symbol
    for index in symbol.shape:
        if isinstance(index, Symbol):
            # references another symbol
            dims.append(index.name)
        elif isinstance(index, int):
            # literal constant
            dims.append(str(index))
        else:
            raise NotImplementedError("unsupported get_dims index '{0}'".format(str(index)))
    return dims


def get_kind(symbol):
    ''' xxx '''
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
