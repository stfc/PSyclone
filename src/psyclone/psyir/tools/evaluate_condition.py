from psyclone.psyir import nodes

class EvaluateCondition:

    def __init__(self, known_variables) -> None:
        self._known_variables: dict[str, bool] = known_variables

    def _evaluate_literal(self, psyir_node: nodes.Literal, is_not: bool = False) -> BooleanValue:
        value: bool = Utils.get_boolean_value_from_literal(psyir_node)
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
            return BooleanValue.ALWAYS_TRUE

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
        boolvalue1 = self._known_boolean_table.get(name1)
        intvalue1 = self._known_int_table.get(name1)
        boolvalue2 = self._known_boolean_table.get(name2)
        intvalue2 = self._known_int_table.get(name2)
        if boolvalue1 is not None and boolvalue2 is not None:
            return self._from_bool_to_boolean(boolvalue1 == boolvalue2)
        elif intvalue1 is not None and intvalue2 is not None:
            return self._from_bool_to_boolean(intvalue1 == intvalue2)
        else:
            return BooleanValue.UNKNOWN

    def _evaluate_nonequality(self, psyir_ref1: nodes.Reference, psyir_ref2: nodes.Reference) -> BooleanValue:
        pass

    def _evaluate_binary_operation(self, psyir_node: nodes.BinaryOperation, is_not: bool = False) -> BooleanValue:
        boolean0: BooleanValue = self.rec_evaluate(psyir_node.children[0], is_not)
        boolean1: BooleanValue = self.rec_evaluate(psyir_node.children[1], is_not)
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
        else:
            raise Exception("Not supported.")

    def _evaluate_reference_as_known_bool_or_int(self, psyir_node: nodes.Reference) -> Union[bool, int]:
        var_name: str = psyir_node.name
        ## If it is found in NameList table, get its value
        if var_name in self._known_reference_bool:
            return self._known_reference_bool[var_name]
        ## if is parameter
        elif var_name in self._known_reference_int:
            return self._known_reference_int[var_name]
        else:
            raise TransformationError("Not in namelist not parameter table.")

    def _evaluate_single_reference(self, psyir_node: nodes.Reference, is_not: bool = False) -> Union[int, BooleanValue]:
        assert isinstance(psyir_node, nodes.Reference)
        try:
            value: Union[bool, int] = self._evaluate_reference_as_known_bool_or_int(psyir_node)
        except TransformationError as e:
            return BooleanValue.DYNAMIC

        if isinstance(value, int):
            return value
        else:
            assert isinstance(value, bool)
            ## Exclusive OR (XOR) between .NOT. (true or false) and the Reference
            if is_not != value:
                return BooleanValue.ALWAYS_TRUE
            else:
                return BooleanValue.ALWAYS_FALSE

    def rec_evaluate(self, psyir_node: nodes.Node, is_not: bool = False) -> BooleanValue:
        """Evaluate the boolean result of a psyir Reference.
        Either it is unknown, or dynamic (changes within code execution)
        or static: AlwaysTrue or AlwaysFalse

        :param psyir_node: _description_
        :type psyir_node: nodes.Node
        :return: _description_
        :rtype: bool
        """
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

    def evaluate(self, condition: Node) -> bool:
        boolean = self.rec_evaluate(condition)
        if boolean == BooleanValue.ALWAYS_TRUE:
            return True
        elif boolean == BooleanValue.ALWAYS_FALSE:
            return False
        else:
            raise TransformationError("Unknown or Dynamic condition.")

    def evaluate_with_sympy(self, condition: Node, sym_table: SymbolTable) -> bool:
        from psyclone.psyir.frontend.sympy_reader import SymPyReader
        from psyclone.psyir.backend.sympy_writer import SymPyWriter
        import sympy

        expr_sympy = SymPyWriter(condition)
        new_expr = sympy.simplify(expr_sympy)
        reader = SymPyReader(expr_sympy)

        psyir_expr: Node = reader.psyir_from_expression(new_expr, sym_table)
        print(psyir_expr.debug_string())
        return True
