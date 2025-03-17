from psyclone.psyir.symbols import SymbolTable, DataSymbol, ScalarType
from psyclone.psyir import nodes 
from psyclone.psyir.tools import EvaluateCondition

def test_evaluate_condition_bool(fortran_reader, fortran_writer):
    known_variables = {'x': True, 'y': False}
    eval = EvaluateCondition(known_variables)
    st = SymbolTable()
    x_sym = DataSymbol("x", ScalarType(ScalarType.Intrinsic.BOOLEAN,ScalarType.Precision.UNDEFINED))
    y_sym = DataSymbol("y", ScalarType(ScalarType.Intrinsic.BOOLEAN,ScalarType.Precision.UNDEFINED))
    st.add(x_sym)
    st.add(y_sym)
    node: nodes.Node = fortran_reader.psyir_from_expression("x .AND. y",st)
    assert node.debug_string() == fortran_writer(node)
    assert not eval.evaluate(node)
    assert not eval.evaluate_with_sympy(node)