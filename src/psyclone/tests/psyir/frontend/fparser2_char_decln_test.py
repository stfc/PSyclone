import pytest

from fparser.common.readfortran import FortranStringReader
from fparser.common.sourceinfo import FortranFormat
from fparser.two import Fortran2003

from psyclone.psyir.frontend.fparser2 import (
    Fparser2Reader)
from psyclone.psyir.nodes import Literal, Routine
from psyclone.psyir.symbols import INTEGER_TYPE, ScalarType


@pytest.mark.usefixtures("f2008_parser")
@pytest.mark.parametrize("len_expr,length,kind",
                         [("", "1", ScalarType.Precision.UNDEFINED),
                          ("(len=3)", "3", ScalarType.Precision.UNDEFINED),
                          ("(3)", "3", ScalarType.Precision.UNDEFINED),
                          ("*3", "3", ScalarType.Precision.UNDEFINED),
                          ("*(3)", "3", ScalarType.Precision.UNDEFINED),
                          ("(len=2*max_len)", "2 * max_len",
                           ScalarType.Precision.UNDEFINED),
                          ("*(2*max_len)", "2 * max_len",
                           ScalarType.Precision.UNDEFINED),
                          ("(len=:)", "COLON", ScalarType.Precision.UNDEFINED),
                          ("(:)", "COLON", ScalarType.Precision.UNDEFINED),
                          ("*(:)", "COLON", ScalarType.Precision.UNDEFINED),
                          ("(len=*)", "ASTERISK",
                           ScalarType.Precision.UNDEFINED),
                          ("(*)", "ASTERISK", ScalarType.Precision.UNDEFINED),
                          ("*(*)", "ASTERISK", ScalarType.Precision.UNDEFINED)])
def test_char_decln_length_handling(len_expr, length, kind):
    '''
    '''
    fake_parent = Routine.create("dummy_schedule")
    symtab = fake_parent.symbol_table
    processor = Fparser2Reader()

    # Test simple declarations
    reader = FortranStringReader(f"character{len_expr} :: l1")
    # Set reader to free format (otherwise this is a comment in fixed format)
    reader.set_format(FortranFormat(True, True))
    fparser2spec = Fortran2003.Specification_Part(reader).content[0]
    processor.process_declarations(fake_parent, [fparser2spec], [])
    l1_var = symtab.lookup("l1")
    assert l1_var.datatype.intrinsic == ScalarType.Intrinsic.CHARACTER
    assert l1_var.datatype.precision == kind
    assert l1_var.datatype.length.debug_string() == length
