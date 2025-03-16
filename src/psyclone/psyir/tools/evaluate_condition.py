from enum import Enum
from typing import Union

from psyclone.psyir import nodes
from psyclone.psyir.symbols import ScalarType, SymbolTable
from psyclone.psyir.transformations import TransformationError


class EvaluationError(TransformationError):
    pass


class BooleanValue(Enum):
    UNKNOWN = 0
    DYNAMIC = 1
    ## STATIC
    ALWAYS_TRUE = 2
    ALWAYS_FALSE = 3


class EvaluateCondition:

    def __init__(self, known_variables: dict[str, bool]) -> None:
        self._known_variables: dict[str, bool] = known_variables

    def get_integer_value_from_literal(self, psyir_node: nodes.Literal) -> int:
        if psyir_node.datatype.intrinsic == ScalarType.Intrinsic.INTEGER:
            return int(psyir_node.value)
        else:
            raise ValueError("Not an integer literal")

    def get_boolean_value_from_literal(self, psyir_node: nodes.Literal) -> bool:
        if psyir_node.datatype.intrinsic == ScalarType.Intrinsic.BOOLEAN:
            if psyir_node.value == "true":
                return True
            else:
                assert psyir_node.value == "false"
                return False
        else:
            raise ValueError("Not a boolean literal")

    def get_value_from_literal(self, psyir_node: nodes.Literal) -> Union[int, bool]:
        try:
            return self.get_boolean_value_from_literal(psyir_node)
        except ValueError as ve1:
            try:
                return self.get_integer_value_from_literal(psyir_node)
            except ValueError as ve2:
                raise ValueError(f"Not known Literal value: {ve2}") from ve1

    def _evaluate_literal(self, psyir_node: nodes.Literal, is_not: bool = False) -> BooleanValue:
        value: Union[int, bool] = self.get_value_from_literal(psyir_node)
        ## is_not XOR LiteralValue
        if is_not != value:
            return BooleanValue.ALWAYS_TRUE
        else:
            return BooleanValue.ALWAYS_FALSE

    def _evaluate_unary_operation(self, psyir_node: nodes.UnaryOperation, is_not: bool = False) -> BooleanValue:
        assert psyir_node._operator == nodes.UnaryOperation.Operator.NOT
        psyir_ref: nodes.Reference = psyir_node.children[0]
        return self.rec_evaluate(psyir_ref, is_not=(not is_not))

    def _not(self, boolean1: BooleanValue) -> BooleanValue:
        if boolean1 in (BooleanValue.DYNAMIC, BooleanValue.UNKNOWN):
            return boolean1
        elif boolean1 == BooleanValue.ALWAYS_TRUE:
            return BooleanValue.ALWAYS_FALSE
        else:
            assert boolean1 == BooleanValue.ALWAYS_FALSE
            return BooleanValue.ALWAYS_TRUE

    def _from_bool_to_boolean(self, expr: bool) -> BooleanValue:
        if expr:
            return BooleanValue.ALWAYS_TRUE
        else:
            return BooleanValue.ALWAYS_FALSE

    def _and(self, boolean1: BooleanValue, boolean2: BooleanValue) -> BooleanValue:
        if boolean1 in (BooleanValue.DYNAMIC, BooleanValue.UNKNOWN):
            return boolean1
        if boolean2 in (BooleanValue.DYNAMIC, BooleanValue.UNKNOWN):
            return boolean2
        if boolean1 == BooleanValue.ALWAYS_TRUE:
            return boolean2
        else:
            return boolean1

    def _or(self, boolean1: BooleanValue, boolean2: BooleanValue) -> BooleanValue:
        if boolean1 in (BooleanValue.DYNAMIC, BooleanValue.UNKNOWN):
            return boolean1
        if boolean2 in (BooleanValue.DYNAMIC, BooleanValue.UNKNOWN):
            return boolean2
        if boolean1 == BooleanValue.ALWAYS_TRUE:
            return boolean1
        else:
            return boolean2

    def _evaluate_equality(self, psyir_ref1: nodes.Reference, psyir_ref2: nodes.Reference) -> BooleanValue:
        name1 = psyir_ref1.name
        name2 = psyir_ref2.name
        boolvalue1 = self._known_variables.get(name1)
        intvalue1 = self._known_variables.get(name1)
        boolvalue2 = self._known_variables.get(name2)
        intvalue2 = self._known_variables.get(name2)
        if boolvalue1 is not None and boolvalue2 is not None:
            return self._from_bool_to_boolean(boolvalue1 == boolvalue2)
        elif intvalue1 is not None and intvalue2 is not None:
            return self._from_bool_to_boolean(intvalue1 == intvalue2)
        else:
            return BooleanValue.UNKNOWN

    def _evaluate_nonequality(self, psyir_ref1: nodes.Reference, psyir_ref2: nodes.Reference) -> BooleanValue:
        pass

    def _evaluate_lt(self, psyir_ref1: nodes.Reference, psyir_ref2: nodes.Reference) -> BooleanValue:
        pass

    def _evaluate_gt(self, psyir_ref1: nodes.Reference, psyir_ref2: nodes.Reference) -> BooleanValue:
        pass

    def _evaluate_binary_operation(self, psyir_node: nodes.BinaryOperation, is_not: bool = False) -> BooleanValue:
        boolean0: BooleanValue = self.rec_evaluate(psyir_node.children[0], is_not)
        boolean1: BooleanValue = self.rec_evaluate(psyir_node.children[1], is_not)
        if boolean0 == BooleanValue.DYNAMIC or boolean0 == BooleanValue.UNKNOWN:
            return boolean0
        if boolean1 == BooleanValue.DYNAMIC or boolean0 == BooleanValue.UNKNOWN:
            return boolean1
        if psyir_node.operator == nodes.BinaryOperation.Operator.AND:
            return self._not(self._and(boolean0, boolean1))
        elif psyir_node.operator == nodes.BinaryOperation.Operator.OR:
            return self._not(self._or(boolean0, boolean1))
        elif psyir_node.operator == nodes.BinaryOperation.Operator.EQ:
            assert len(psyir_node.children) == 2
            return self._evaluate_equality(psyir_node.children[0], psyir_node.children[1])
        elif psyir_node.operator == nodes.BinaryOperation.Operator.NE:
            assert len(psyir_node.children) == 2
            return self._evaluate_nonequality(psyir_node.children[0], psyir_node.children[1])
        elif psyir_node.operator == nodes.BinaryOperation.Operator.LT:
            assert len(psyir_node.children) == 2
            return self._evaluate_lt(psyir_node.children[0], psyir_node.children[1])
        elif psyir_node.operator == nodes.BinaryOperation.Operator.GT:
            assert len(psyir_node.children) == 2
            return self._evaluate_gt(psyir_node.children[0], psyir_node.children[1])
        else:
            raise NotImplementedError("Not supported.")

    def _evaluate_reference_as_known_bool_or_int(self, psyir_node: nodes.Reference) -> Union[bool, int]:
        var_name: str = psyir_node.name
        ## If it is found in NameList table, get its value
        if var_name in self._known_variables:
            return self._known_variables[var_name]
        else:
            raise EvaluationError("Not a known variable.")

    def _evaluate_single_reference(self, psyir_node: nodes.Reference, is_not: bool = False) -> Union[int, BooleanValue]:
        assert isinstance(psyir_node, nodes.Reference)
        try:
            value: Union[bool, int] = self._evaluate_reference_as_known_bool_or_int(psyir_node)
        except EvaluationError:
            return BooleanValue.DYNAMIC

        if type(value) is int:
            return value
        else:
            assert type(value) is bool
            ## Exclusive OR (XOR) between .NOT. (true or false) and the Reference
            if is_not != value:
                return BooleanValue.ALWAYS_TRUE
            else:
                return BooleanValue.ALWAYS_FALSE

    def rec_evaluate(self, psyir_node: nodes.Node, is_not: bool = False) -> Union[BooleanValue, int]:
        """Evaluate the boolean result of a psyir Reference.
        Either it is unknown, or dynamic (changes within code execution)
        or static: AlwaysTrue or AlwaysFalse

        :param psyir_node: _description_
        :type psyir_node: nodes.Node
        :return: _description_
        :rtype: bool
        """
        ## FIXME: try to use evaluate_with_sympy
        if isinstance(psyir_node, nodes.Literal):
            return self._evaluate_literal(psyir_node, is_not)
        elif isinstance(psyir_node, nodes.Reference):
            return self._evaluate_single_reference(psyir_node, is_not)
        elif isinstance(psyir_node, nodes.UnaryOperation):
            return self._evaluate_unary_operation(psyir_node, is_not)
        elif isinstance(psyir_node, nodes.BinaryOperation):
            return self._evaluate_binary_operation(psyir_node, is_not)
        else:
            raise TransformationError("Not implemented.")

    def evaluate(self, condition: nodes.Node) -> bool:
        """Walk over all references: if they are all known (Literal, or known variables)
        use sympy.
        Otherwise return Unknown.

        :param condition: _description_
        :raises EvaluationError: _description_
        :return: _description_
        """
        error_msg = ""
        try:
            boolean = self.rec_evaluate(condition)
        except ValueError as ve:
            error_msg = f"{ve}"
            boolean = BooleanValue.DYNAMIC
        if boolean == BooleanValue.ALWAYS_TRUE:
            return True
        elif boolean == BooleanValue.ALWAYS_FALSE:
            return False
        else:
            raise EvaluationError(f"Condition is {type(boolean)}: {error_msg}")

    def evaluate_with_sympy(self, condition: nodes.Node, sym_table: SymbolTable) -> bool:
        from psyclone.psyir.frontend.sympy_reader import SymPyReader
        from psyclone.psyir.backend.sympy_writer import SymPyWriter
        import sympy

        expr_sympy = SymPyWriter(condition)
        new_expr = sympy.simplify(expr_sympy)
        reader = SymPyReader(expr_sympy)

        psyir_expr: nodes.Node = reader.psyir_from_expression(new_expr, sym_table)
        print(psyir_expr.debug_string())
        return True
