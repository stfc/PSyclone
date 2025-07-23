# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2025, xDSL project
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

'''xDSL PSyIR backend. Generates xDSL PSyclone dialect from PSyIR nodes.

'''
from __future__ import annotations
import six
from fparser.two import Fortran2003
from fparser.two.utils import walk
from psyclone.psyir.backend.language_writer import LanguageWriter
from psyclone.errors import InternalError
from psyclone.psyir.backend.visitor import VisitorError
from psyclone.psyir.nodes import (Routine, CodeBlock, BinaryOperation,
                                  UnaryOperation, Schedule, DataNode,
                                  Range, Literal, IntrinsicCall)
from psyclone.psyir.symbols import (SymbolTable, ScalarType, RoutineSymbol,
                                    DataSymbol, DataTypeSymbol,
                                    ArgumentInterface, ArrayType, Symbol,
                                    NoType, UnsupportedType)
from psy.dialects import psy_ir
from xdsl.dialects.builtin import StringAttr, IntAttr
from xdsl.dialects.builtin import ModuleOp
from dataclasses import dataclass, field
from typing import Optional, Dict
from xdsl.ir import SSAValue

INTRINSIC_TYPE_TO_STRING = {
  ScalarType.Intrinsic.INTEGER: "integer",
  ScalarType.Intrinsic.REAL: "real",
  ScalarType.Intrinsic.BOOLEAN: "logical",
  ScalarType.Intrinsic.CHARACTER: "character"
}


@dataclass
class SSAValueCtx:
    """
    Context that relates identifiers from the AST to SSA values
    used in the flat representation.
    """
    dictionary: Dict[str, SSAValue] = field(default_factory=dict)
    parent_scope: Optional[SSAValueCtx] = None

    def __getitem__(self, identifier: str) -> Optional[SSAValue]:
        """
        Check if the given identifier is in the current scope,
        or a parent scope
        """
        ssa_value = self.dictionary.get(identifier, None)
        if ssa_value:
            return ssa_value
        elif self.parent_scope:
            return self.parent_scope[identifier]
        else:
            return None

    def __setitem__(self, identifier: str, ssa_value: SSAValue):
        """Relate the given identifier and SSA value in the current scope"""
        if identifier in self.dictionary:
            raise Exception()
        else:
            self.dictionary[identifier] = ssa_value


class xDSLWriter(LanguageWriter):
    '''Implements a PSyIR-to-xDSL back-end for the PSyIR.

    '''
    def __init__(self, skip_nodes=False, indent_string="  ",
                 initial_indent_depth=0, check_global_constraints=True):

        super(xDSLWriter, self).__init__(
            ("[", "]"), ".",
            skip_nodes, indent_string,
            initial_indent_depth, check_global_constraints
        )

        self.global_ctx = SSAValueCtx()
        self.ctx = self.global_ctx

    def apply_precision(self, precision, base_type):
        if isinstance(precision, ScalarType.Precision):
            if (
                base_type.type_name.data == "real" and
                precision == ScalarType.Precision.DOUBLE
            ):
                base_type.set_precision(IntAttr(8))
            elif (
                  base_type.type_name.data == "real" and
                  precision == ScalarType.Precision.SINGLE
            ):
                base_type.set_precision(IntAttr(4))
            elif (
                  base_type.type_name.data == "real" and
                  precision == ScalarType.Precision.UNDEFINED
            ):
                base_type.set_precision(psy_ir.EmptyAttr())
        elif isinstance(precision, int):
            if base_type.type_name.data not in ["real", "integer", "logical"]:
                print("Error - type not compatible with precision "
                      + base_type.name)
            if (
                base_type.type_name.data == 'real' and
                precision not in [4, 8, 16]
            ):
                print("Error - type 'real' supports precision"
                      + " of 4, 8, 16 only")
            if (
                base_type.type_name.data in ['integer', 'logical'] and
                precision not in [1, 2, 4, 8, 16]
            ):
                print("Error - type 'integer' supports precision"
                      + " of 1,2, 4, 8, 16 only")
            base_type.set_precision(IntAttr(precision))
        elif isinstance(precision, DataSymbol):
            if base_type.type_name.data not in ["real", "integer", "logical"]:
                print("Error - type not compatible with kind "
                      + base_type.type_name.data)
            base_type.set_kind(StringAttr(precision.name))

    def gen_type(self, datatype):
        if isinstance(datatype, ArrayType):
            array_shape = datatype.shape
            if array_shape:
                dims = self.gen_indices(array_shape)
            if isinstance(datatype.intrinsic, DataTypeSymbol):
                base_type = \
                  psy_ir.DerivedType.from_str(datatype.intrinsic.name)
            else:
                base_type = \
                  psy_ir.NamedType(
                    [StringAttr(
                      INTRINSIC_TYPE_TO_STRING[datatype.intrinsic]
                    ),
                      psy_ir.EmptyAttr(), psy_ir.EmptyAttr()]
                  )
                self.apply_precision(datatype.precision, base_type)
            return psy_ir.ArrayType.from_type_and_list(base_type, dims)
        elif isinstance(datatype, DataTypeSymbol):
            return psy_ir.DerivedType.from_str(datatype.name)
        elif isinstance(datatype, UnsupportedType):
            print("Error - not sure how to handle unsupported type")
        else:
            base_type = \
              psy_ir.NamedType(
                [StringAttr(
                  INTRINSIC_TYPE_TO_STRING[datatype.intrinsic]
                ),
                  psy_ir.EmptyAttr(), psy_ir.EmptyAttr()]
              )
            self.apply_precision(datatype.precision, base_type)
            return base_type

    def gen_intent(self, symbol):
        mapping = {
          ArgumentInterface.Access.UNKNOWN: "",
          ArgumentInterface.Access.READ: "in",
          ArgumentInterface.Access.WRITE: "out",
          ArgumentInterface.Access.READWRITE: "inout"
        }

        if symbol.is_argument:
            return mapping[symbol.interface.access]
        else:
            return ""  # non-Arguments do not have intent

    def gen_decls(self, symbol_table):
        var_defs = []

        for sym in symbol_table.argument_datasymbols:
            intent = self.gen_intent(sym)
            var_type = self.gen_type(sym.datatype)
            tkn = psy_ir.Token(
              [StringAttr(sym.name), var_type]
            )
            var_defs.append(
              psy_ir.VarDef.get(tkn, True, sym.is_constant, intent)
            )
            self.ctx[sym.name] = tkn

        for sym in symbol_table.automatic_datasymbols:
            try:
                var_type = self.gen_type(sym.datatype)
                tkn = psy_ir.Token([StringAttr(sym.name), var_type])
                self.ctx[sym.name] = tkn

                var_defs.append(psy_ir.VarDef.get(tkn, False, sym.is_constant))
            except (AttributeError, KeyError) as err:
                raise six.raise_from(NotImplementedError(
                  "Could not generate the definition for the variable '{0}', "
                  "type '{1}' is currently not supported."
                  "".format(sym.name, sym.datatype)), err
                )

        return var_defs

    def gen_use(self, symbol, symbol_table):
        # Construct the list of symbol names for the ONLY clause
        only_list = [
          StringAttr(dsym.name) for dsym in
          symbol_table.symbols_imported_from(symbol)
        ]

        return psy_ir.Import.get(symbol.name, only_list)

    def filecontainer_node(self, node):
        containers = []
        for child in node.children:
            containers.append(self._visit(child))

        return ModuleOp(containers)

    def container_node(self, node):
        if not all(
          isinstance(child, (Routine, CodeBlock)) for child in node.children
        ):
            raise VisitorError(
                f"The Fortran back-end requires all children of a Container "
                f"to be either CodeBlocks or sub-classes of Routine but found:"
                f" {[type(child).__name__ for child in node.children]}."
            )

        visibility = self.gen_access_stmt(node.symbol_table)
        public_routines, private_routines = (
          self.gen_routine_access_stmts(node.symbol_table)
        )

        imports = []
        for symbol in node.symbol_table.containersymbols:
            imports.append(self.gen_use(symbol, node.symbol_table))

        routines = []
        for child in node.children:
            routines.append(self._visit(child))

        return psy_ir.Container.get(node.name, visibility, public_routines,
                                    private_routines, imports, routines)

    def gen_access_stmt(self, symbol_table):
        # If no default visibility has been set then we use the Fortran
        # default of public.
        if symbol_table.default_visibility in [None, Symbol.Visibility.PUBLIC]:
            return self._nindent + "public"
        if symbol_table.default_visibility == Symbol.Visibility.PRIVATE:
            return self._nindent + "private"

        raise InternalError(
            f"Unrecognised visibility ('{symbol_table.default_visibility}') "
            f"found when attempting to generate access statement. Should be "
            f"either 'Symbol.Visibility.PUBLIC' or "
            f"'Symbol.Visibility.PRIVATE'\n"
        )

    def gen_routine_access_stmts(self, symbol_table):
        # Find the symbol that represents itself, this one will not need
        # an accessibility statement
        itself = self.symbol

        public_routines = []
        private_routines = []
        for symbol in symbol_table.symbols:
            if isinstance(symbol, RoutineSymbol):
                # Skip the symbol representing the routine where these
                # declarations belong
                if symbol is itself:
                    continue

                # It doesn't matter whether this symbol has a local or global
                # interface - its accessibility in *this* context is determined
                # by the local accessibility statements. e.g. if we are
                # dealing with the declarations in a given module which itself
                # uses a public symbol from some other module, the
                # accessibility of that symbol is determined by the
                # accessibility statements in the current module.
                if symbol.visibility == Symbol.Visibility.PUBLIC:
                    public_routines.append(StringAttr(symbol.name))
                elif symbol.visibility == Symbol.Visibility.PRIVATE:
                    private_routines.append(StringAttr(symbol.name))
                else:
                    raise InternalError(
                        f"Unrecognised visibility ('{symbol.visibility}') "
                        f"found for symbol '{symbol.name}'. Should be either "
                        f"'Symbol.Visibility.PUBLIC' or "
                        f"'Symbol.Visibility.PRIVATE'."
                    )
        return public_routines, private_routines

    def routine_node(self, node):
        whole_routine_scope = SymbolTable()
        imports = []

        for schedule in node.walk(Schedule):
            for symbol in schedule.symbol_table.symbols[:]:
                try:
                    whole_routine_scope.add(symbol)
                except KeyError:
                    new_name = \
                      whole_routine_scope.next_available_name(symbol.name)
                    while True:
                        # Ensure that the new name isn't
                        # already in the current symbol table.
                        local_name = \
                          schedule.symbol_table.next_available_name(new_name)
                        if local_name == new_name:
                            # new_name is availble in the current symbol table
                            # so we're done.
                            break
                        # new_name clashed with an entry in the current symbol
                        # table so try again.
                        new_name = (
                          whole_routine_scope.next_available_name(local_name)
                        )
                        schedule.symbol_table.rename_symbol(symbol, new_name)
                        whole_routine_scope.add(symbol)
                        imports.append(
                          self.gen_use(symbol, whole_routine_scope)
                        )

        for symbol in whole_routine_scope.containersymbols:
            imports.append(self.gen_use(symbol, whole_routine_scope))

        parent_ctx = self.ctx
        routine_scope_ctx = SSAValueCtx(dictionary={}, parent_scope=self.ctx)
        self.ctx = routine_scope_ctx

        declarations = self.gen_decls(whole_routine_scope)

        args = [
          self.ctx[symbol.name] for symbol in node.symbol_table.argument_list
        ]

        # Get the executable statements.
        exec_statements = []
        for child in node.children:
            child_visited = self._visit(child)
            if isinstance(child_visited, list):
                exec_statements.extend(child_visited)
            else:
                exec_statements.append(child_visited)

        self.ctx = parent_ctx
        if node.return_symbol:
            # Use routine_scope_ctx as the return variable will
            # be created in the routine so need to reference that
            return psy_ir.Routine.get(
              node.name, routine_scope_ctx[node.return_symbol.name],
              imports, args, declarations,
              exec_statements, node.is_program
            )
        else:
            return psy_ir.Routine.get(
              node.name, None,
              imports, args, declarations,
              exec_statements, node.is_program
            )

    def codeblock_node(self, node):
        # A code block can have multiple AST nodes which have not
        # been parsed, therefore process each of these and store
        # in a list that is returned at the end
        ops_to_return = []

        if node.structure == CodeBlock.Structure.STATEMENT:
            for ast_node in node.get_ast_nodes:
                name = \
                  str(walk(ast_node.children[0], Fortran2003.Type_Name)[0])
                args = []
                # TODO: Update to support arguments in calls
                # for arg in node.children:
                #  args.append(arg)
                ops_to_return.append(psy_ir.CallExpr.get(name, args=args))
        elif node.structure == CodeBlock.Structure.EXPRESSION:
            for ast_node in node.get_ast_nodes:
                name = \
                  str(walk(ast_node.children[0], Fortran2003.Type_Name)[0])
                args = []
                # TODO: Update to support arguments in calls
                # for arg in node.children:
                #    args.append(arg)
                ops_to_return.append(
                  psy_ir.CallExpr.get(
                    name, args,
                    psy_ir.NamedType([
                        StringAttr("integer"),
                        psy_ir.EmptyAttr(),
                        psy_ir.EmptyAttr()
                      ]
                    ),
                    intrinsic=True)
                )
        else:
            raise VisitorError(
                f"Unsupported CodeBlock Structure '{node.structure}' found."
            )
        return ops_to_return

    def checkIfStringIsType(self, string, typ):
        try:
            if (typ == int):
                int(string)
                return True
            elif (typ == float):
                float(string)
                return True
        except ValueError:
            return False
        raise VisitorError(f"Unknown type for string check '{str(type)}'")

    def nemokern_node(self, node):
        exec_statements = []
        schedules = node.get_callees()
        # IGNORE polymorphic routines.
        schedule = schedules[0]
        for child in schedule.children:
            exec_statements.append(self._visit(child))
        return exec_statements

    def assignment_node(self, node):
        visited_rhs = self._visit(node.rhs)
        if isinstance(visited_rhs, list):
            assert len(visited_rhs) == 1
            visited_rhs = visited_rhs[0]
        return psy_ir.Assign.get(self._visit(node.lhs), visited_rhs)

    def reference_node(self, node):
        return psy_ir.ExprName.get(
          node.symbol.name, self.ctx[node.symbol.name]
        )

    def structurereference_node(self, node):
        return psy_ir.StructureReference.get(
          self.ctx[node.symbol.name], self._visit(node.children[0])
        )

    def member_node(self, node):
        if not node.children:
            return psy_ir.StructureMember(
              [StringAttr(node.name), psy_ir.EmptyAttr()]
            )
        else:
            return psy_ir.StructureMember(
              [StringAttr(node.name), self._visit(node.children[0])]
            )

    def return_node(self, node):
        return psy_ir.Return()

    def arrayreference_node(self, node):
        if not node.children:
            raise VisitorError(
                "Incomplete ArrayReference node (for symbol '{0}') found: "
                "must have one or more children.".format(node.name)
            )

        args = self.gen_indices(node.children, node.name)

        return psy_ir.ArrayReference.get(self.ctx[node.name], args)

    def gen_indices(self, indices, var_name=None):
        dims = []
        for index in indices:
            if isinstance(index, (DataNode, Range)):
                # literal constant, symbol reference, or computed dimension
                expression = self._visit(index)
                dims.append(expression)
            elif (
              isinstance(index, ArrayType.Extent) and
              index == ArrayType.Extent.DEFERRED
            ):
                dims.append(psy_ir.DeferredAttr())
            elif (
              isinstance(index, ArrayType.Extent) and
              index == ArrayType.Extent.ATTRIBUTE
            ):
                dims.append(psy_ir.AssumedSizeAttr())
            elif isinstance(index, ArrayType.ArrayBounds):
                expression = self._visit(index.lower)
                if isinstance(expression, psy_ir.Literal):
                    dims.append(expression.value)
                else:
                    dims.append(expression)
                expression = self._visit(index.upper)
                if isinstance(expression, psy_ir.Literal):
                    dims.append(expression.value)
                else:
                    dims.append(expression)
            else:
                raise NotImplementedError(
                  f"unsupported gen_indices index '{index}'"
                )
        return dims

    def loop_node(self, node):
        start = self._visit(node.start_expr)
        stop = self._visit(node.stop_expr)
        step = self._visit(node.step_expr)

        body = []
        for child in node.loop_body:
            child_contents = self._visit(child)
            if isinstance(child_contents, list):
                body.extend(child_contents)
            else:
                body.append(child_contents)

        return psy_ir.Loop.get(
          self.ctx[node.variable.name], start, stop, step, body
        )

    def ifblock_node(self, node):
        condition = self._visit(node.children[0])

        if_body = []
        for child in node.if_body:
            if_body.append(self._visit(child))

        else_body = []
        # node.else_body is None if there is no else clause.
        if node.else_body:
            for child in node.else_body:
                else_body.append(self._visit(child))

        return psy_ir.IfBlock.get(condition, if_body, else_body)

    def binaryoperation_node(self, node):
        if len(node.children) != 2:
            raise VisitorError(
              "BinaryOperation malformed or incomplete. It "
              "should have exactly 2 children, but found {0}."
              "".format(len(node.children))
            )

        opmap = {
          BinaryOperation.Operator.ADD: "ADD",
          BinaryOperation.Operator.SUB: "SUB",
          BinaryOperation.Operator.MUL: "MUL",
          BinaryOperation.Operator.DIV: "DIV",
          BinaryOperation.Operator.REM: "REM",
          BinaryOperation.Operator.POW: "POW",
          BinaryOperation.Operator.EQ: "EQ",
          BinaryOperation.Operator.NE: "NE",
          BinaryOperation.Operator.GT: "GT",
          BinaryOperation.Operator.LT: "LT",
          BinaryOperation.Operator.GE: "GE",
          BinaryOperation.Operator.LE: "LE",
          BinaryOperation.Operator.AND: "AND",
          BinaryOperation.Operator.OR: "OR",
          BinaryOperation.Operator.EQV: "EQV",
          BinaryOperation.Operator.NEQV: "NEQV",
        }

        # For now ignore this, pow and copysign are functions so need
        # to be called expression rather than binary expression in dialect
        try:
            opstring = opmap[node.operator]
        except KeyError as err:
            raise six.raise_from(VisitorError(
                    "The xDSL backend does not support the '{0}' operator."
                    "".format(node.operator)), err
            )
        return psy_ir.BinaryOperation.get(
          opstring, self._visit(node.children[0]),
          self._visit(node.children[1])
        )

    def unaryoperation_node(self, node):
        if len(node.children) != 1:
            raise VisitorError(
              "UnaryOperation malformed or incomplete. It "
              "should have exactly 1 children, but found {0}."
              "".format(len(node.children))
            )

        opmap = {
          UnaryOperation.Operator.MINUS: "MINUS",
          UnaryOperation.Operator.PLUS: "PLUS",
          UnaryOperation.Operator.NOT: "NOT",
          }

        # For now ignore this, pow and copysign are functions so need
        # to be called expression rather than binary expression in dialect
        try:
            opstring = opmap[node.operator]
        except KeyError as err:
            raise six.raise_from(VisitorError(
                    "The xDSL backend does not support the '{0}' operator."
                    "".format(node.operator)), err
            )
        return psy_ir.UnaryOperation.get(
          opstring, self._visit(node.children[0])
        )

    def intrinsiccall_node(self, node):
        opmap = {
          # Following ordering in 'intrinsic_call.py'
          IntrinsicCall.Intrinsic.ALLOCATE: "ALLOCATE",
          IntrinsicCall.Intrinsic.DEALLOCATE: "DEALLOCATE",
          IntrinsicCall.Intrinsic.NULLIFY: "NULLIFY",
          IntrinsicCall.Intrinsic.ABS: "ABS",
          IntrinsicCall.Intrinsic.ACHAR: "ACHAR",
          IntrinsicCall.Intrinsic.ACOS: "ACOS",
          IntrinsicCall.Intrinsic.ACOSH: "ACOSH",
          IntrinsicCall.Intrinsic.ADJUSTL: "ADJUSTL",
          IntrinsicCall.Intrinsic.ADJUSTR: "ADJUSTR",
          IntrinsicCall.Intrinsic.AIMAG: "AIMAG",
          IntrinsicCall.Intrinsic.AINT: "AINT",
          IntrinsicCall.Intrinsic.ALL: "ALL",
          IntrinsicCall.Intrinsic.ALLOCATED: "ALLOCATED",
          IntrinsicCall.Intrinsic.ANINT: "ANINT",
          IntrinsicCall.Intrinsic.ANY: "ANY",
          IntrinsicCall.Intrinsic.ASIN: "ASIN",
          IntrinsicCall.Intrinsic.ASINH: "ASINH",
          IntrinsicCall.Intrinsic.ASSOCIATED: "ASSOCIATED",
          IntrinsicCall.Intrinsic.ATAN: "ATAN",
          IntrinsicCall.Intrinsic.ATAN2: "ATAN2",
          IntrinsicCall.Intrinsic.ATANH: "ATANH",
          IntrinsicCall.Intrinsic.ATOMIC_ADD: "ATOMIC_ADD",
          IntrinsicCall.Intrinsic.ATOMIC_AND: "ATOMIC_AND",
          IntrinsicCall.Intrinsic.ATOMIC_CAS: "ATOMIC_CAS",
          IntrinsicCall.Intrinsic.ATOMIC_DEFINE: "ATOMIC_DEFINE",
          IntrinsicCall.Intrinsic.ATOMIC_FETCH_ADD: "ATOMIC_FETCH_ADD",
          IntrinsicCall.Intrinsic.ATOMIC_FETCH_AND: "ATOMIC_FETCH_AND",
          IntrinsicCall.Intrinsic.ATOMIC_FETCH_OR: "ATOMIC_FETCH_OR",
          IntrinsicCall.Intrinsic.ATOMIC_FETCH_XOR: "ATOMIC_FETCH_XOR",
          IntrinsicCall.Intrinsic.ATOMIC_OR: "ATOMIC_OR",
          IntrinsicCall.Intrinsic.ATOMIC_REF: "ATOMIC_REF",
          IntrinsicCall.Intrinsic.ATOMIC_XOR: "ATOMIC_XOR",
          IntrinsicCall.Intrinsic.BESSEL_J0: "BESSEL_J0",
          IntrinsicCall.Intrinsic.BESSEL_J1: "BESSEL_J1",
          IntrinsicCall.Intrinsic.BESSEL_JN: "BESSEL_JN",
          IntrinsicCall.Intrinsic.BESSEL_Y0: "BESSEL_Y0",
          IntrinsicCall.Intrinsic.BESSEL_Y1: "BESSEL_Y1",
          IntrinsicCall.Intrinsic.BESSEL_YN: "BESSEL_YN",
          IntrinsicCall.Intrinsic.BGE: "BGE",
          IntrinsicCall.Intrinsic.BGT: "BGT",
          IntrinsicCall.Intrinsic.BIT_SIZE: "BIT_SIZE",
          IntrinsicCall.Intrinsic.BLE: "BLE",
          IntrinsicCall.Intrinsic.BLT: "BLT",
          IntrinsicCall.Intrinsic.BTEST: "BTEST",
          IntrinsicCall.Intrinsic.CEILING: "CEIL",
          IntrinsicCall.Intrinsic.CHAR: "CHAR",
          IntrinsicCall.Intrinsic.CMPLX: "CMPLX",
          IntrinsicCall.Intrinsic.CO_BROADCAST: "CO_BROADCAST",
          IntrinsicCall.Intrinsic.CO_MAX: "CO_MAX",
          IntrinsicCall.Intrinsic.CO_MIN: "CO_MIN",
          IntrinsicCall.Intrinsic.CO_REDUCE: "CO_REDUCE",
          IntrinsicCall.Intrinsic.CO_SUM: "CO_SUM",
          IntrinsicCall.Intrinsic.COMMAND_ARGUMENT_COUNT: (
            "COMMAND_ARGUMENT_COUNT"
          ),
          IntrinsicCall.Intrinsic.CONJG: "CONJG",
          IntrinsicCall.Intrinsic.COS: "COS",
          IntrinsicCall.Intrinsic.COSH: "COSH",
          IntrinsicCall.Intrinsic.COSHAPE: "COSHAPE",
          IntrinsicCall.Intrinsic.COUNT: "COUNT",
          IntrinsicCall.Intrinsic.CPU_TIME: "CPU_TIME",
          IntrinsicCall.Intrinsic.CSHIFT: "CSHIFT",
          IntrinsicCall.Intrinsic.DATE_AND_TIME: "DATE_AND_TIME",
          IntrinsicCall.Intrinsic.DBLE: "DBLE",
          IntrinsicCall.Intrinsic.DIGITS: "DIGITS",
          IntrinsicCall.Intrinsic.DIM: "DIM",
          IntrinsicCall.Intrinsic.DOT_PRODUCT: "DOT_PRODUCT",
          IntrinsicCall.Intrinsic.DPROD: "DPROD",
          IntrinsicCall.Intrinsic.DSHIFTL: "DSHIFTL",
          IntrinsicCall.Intrinsic.DSHIFTR: "DSHIFTR",
          IntrinsicCall.Intrinsic.EOSHIFT: "EOSHIFT",
          IntrinsicCall.Intrinsic.EPSILON: "EPSILON",
          IntrinsicCall.Intrinsic.ERF: "ERF",
          IntrinsicCall.Intrinsic.ERFC: "ERFC",
          IntrinsicCall.Intrinsic.ERFC_SCALED: "ERFC_SCALED",
          IntrinsicCall.Intrinsic.EVENT_QUERY: "EVENT_QUERY",
          IntrinsicCall.Intrinsic.EXECUTE_COMMAND_LINE: "EXECUTE_COMMAND_LINE",
          IntrinsicCall.Intrinsic.EXP: "EXP",
          IntrinsicCall.Intrinsic.EXPONENT: "EXPONENT",
          IntrinsicCall.Intrinsic.EXTENDS_TYPE_OF: "EXTENDS_TYPE_OF",
          IntrinsicCall.Intrinsic.FAILED_IMAGES: "FAILED_IMAGES",
          IntrinsicCall.Intrinsic.FINDLOC: "FINDLOC",
          IntrinsicCall.Intrinsic.FLOAT: "FLOAT",
          IntrinsicCall.Intrinsic.FLOOR: "FLOOR",
          IntrinsicCall.Intrinsic.FRACTION: "FRACTION",
          IntrinsicCall.Intrinsic.GAMMA: "GAMMA",
          IntrinsicCall.Intrinsic.GET_COMMAND: "GET_COMMAND",
          IntrinsicCall.Intrinsic.GET_COMMAND_ARGUMENT: "GET_COMMAND_ARGUMENT",
          IntrinsicCall.Intrinsic.GET_ENVIRONMENT_VARIABLE: (
            "GET_ENVIRONMENT_VARIABLE"
          ),
          IntrinsicCall.Intrinsic.GET_TEAM: "GET_TEAM",
          IntrinsicCall.Intrinsic.HUGE: "HUGE",
          IntrinsicCall.Intrinsic.HYPOT: "HYPOT",
          IntrinsicCall.Intrinsic.IACHAR: "IACHAR",
          IntrinsicCall.Intrinsic.IALL: "IALL",
          IntrinsicCall.Intrinsic.IAND: "IAND",
          IntrinsicCall.Intrinsic.IANY: "IANY",
          IntrinsicCall.Intrinsic.IBCLR: "IBCLR",
          IntrinsicCall.Intrinsic.IBITS: "IBITS",
          IntrinsicCall.Intrinsic.IBSET: "IBSET",
          IntrinsicCall.Intrinsic.ICHAR: "ICHAR",
          IntrinsicCall.Intrinsic.IEOR: "IEOR",
          IntrinsicCall.Intrinsic.IMAGE_INDEX: "IMAGE_INDEX",
          IntrinsicCall.Intrinsic.IMAGE_STATUS: "IMAGE_STATUS",
          IntrinsicCall.Intrinsic.INDEX: "INDEX",
          IntrinsicCall.Intrinsic.INT: "INT",
          IntrinsicCall.Intrinsic.IOR: "IOR",
          IntrinsicCall.Intrinsic.IPARITY: "IPARITY",
          IntrinsicCall.Intrinsic.ISHFT: "ISHFT",
          IntrinsicCall.Intrinsic.ISHFTC: "ISHFTC",
          IntrinsicCall.Intrinsic.IS_CONTIGUOUS: "IS_CONTIGUOUS",
          IntrinsicCall.Intrinsic.IS_IOSTAT_END: "IS_IOSTAT_END",
          IntrinsicCall.Intrinsic.IS_IOSTAT_EOR: "IS_IOSTAT_EOR",
          IntrinsicCall.Intrinsic.KIND: "KIND",
          IntrinsicCall.Intrinsic.LBOUND: "LBOUND",
          IntrinsicCall.Intrinsic.LCOBOUND: "LCOBOUND",
          IntrinsicCall.Intrinsic.LEADZ: "LEADZ",
          IntrinsicCall.Intrinsic.LEN: "LEN",
          IntrinsicCall.Intrinsic.LEN_TRIM: "LEN_TRIM",
          IntrinsicCall.Intrinsic.LGE: "LGE",
          IntrinsicCall.Intrinsic.LGT: "LGT",
          IntrinsicCall.Intrinsic.LLE: "LLE",
          IntrinsicCall.Intrinsic.LLT: "LLT",
          IntrinsicCall.Intrinsic.LOG: "LOG",
          IntrinsicCall.Intrinsic.LOG_GAMMA: "LOG_GAMMA",
          IntrinsicCall.Intrinsic.LOG10: "LOG10",
          IntrinsicCall.Intrinsic.LOGICAL: "LOGICAL",
          IntrinsicCall.Intrinsic.MASKL: "MASKL",
          IntrinsicCall.Intrinsic.MASKR: "MASKR",
          IntrinsicCall.Intrinsic.MATMUL: "MATMUL",
          IntrinsicCall.Intrinsic.MAX: "MAX",
          IntrinsicCall.Intrinsic.MAXEXPONENT: "MAXEXPONENT",
          IntrinsicCall.Intrinsic.MAXLOC: "MAXLOC",
          IntrinsicCall.Intrinsic.MAXVAL: "MAXVAL",
          IntrinsicCall.Intrinsic.MERGE: "MERGE",
          IntrinsicCall.Intrinsic.MERGE_BITS: "MERGE_BITS",
          IntrinsicCall.Intrinsic.MIN: "MIN",
          IntrinsicCall.Intrinsic.MINEXPONENT: "MINEXPONENT",
          IntrinsicCall.Intrinsic.MINLOC: "MINLOC",
          IntrinsicCall.Intrinsic.MINVAL: "MINVAL",
          IntrinsicCall.Intrinsic.MOD: "MOD",
          IntrinsicCall.Intrinsic.MODULO: "MODULO",
          IntrinsicCall.Intrinsic.MOVE_ALLOC: "MOVE_ALLOC",
          IntrinsicCall.Intrinsic.MVBITS: "MVBITS",
          IntrinsicCall.Intrinsic.NEAREST: "NEAREST",
          IntrinsicCall.Intrinsic.NEW_LINE: "NEW_LINE",
          IntrinsicCall.Intrinsic.NINT: "NINT",
          IntrinsicCall.Intrinsic.NORM2: "NORM2",
          IntrinsicCall.Intrinsic.NOT: "NOT",
          IntrinsicCall.Intrinsic.NULL: "NULL",
          IntrinsicCall.Intrinsic.NUM_IMAGES: "NUM_IMAGES",
          IntrinsicCall.Intrinsic.OUT_OF_RANGE: "OUT_OF_RANGE",
          IntrinsicCall.Intrinsic.PACK: "PACK",
          IntrinsicCall.Intrinsic.PARITY: "PARITY",
          IntrinsicCall.Intrinsic.POPCNT: "POPCNT",
          IntrinsicCall.Intrinsic.POPPAR: "POPPAR",
          IntrinsicCall.Intrinsic.PRECISION: "PRECISION",
          IntrinsicCall.Intrinsic.PRESENT: "PRESENT",
          IntrinsicCall.Intrinsic.PRODUCT: "PRODUCT",
          IntrinsicCall.Intrinsic.RADIX: "RADIX",
          IntrinsicCall.Intrinsic.RANDOM_INIT: "RANDOM_INIT",
          IntrinsicCall.Intrinsic.RANDOM_NUMBER: "RANDOM_NUMBER",
          IntrinsicCall.Intrinsic.RANDOM_SEED: "RANDOM_SEED",
          IntrinsicCall.Intrinsic.RANGE: "RANGE",
          IntrinsicCall.Intrinsic.RANK: "RANK",
          IntrinsicCall.Intrinsic.REAL: "REAL",
          IntrinsicCall.Intrinsic.REDUCE: "REDUCE",
          IntrinsicCall.Intrinsic.REPEAT: "REPEAT",
          IntrinsicCall.Intrinsic.RESHAPE: "RESHAPE",
          IntrinsicCall.Intrinsic.RRSPACING: "RRSPACING",
          IntrinsicCall.Intrinsic.SAME_TYPE_AS: "SAME_TYPE_AS",
          IntrinsicCall.Intrinsic.SCALE: "SCALE",
          IntrinsicCall.Intrinsic.SCAN: "SCAN",
          IntrinsicCall.Intrinsic.SELECTED_CHAR_KIND: "SELECTED_CHAR_KIND",
          IntrinsicCall.Intrinsic.SELECTED_INT_KIND: "SELECTED_INT_KIND",
          IntrinsicCall.Intrinsic.SELECTED_REAL_KIND: "SELECTED_REAL_KIND",
          IntrinsicCall.Intrinsic.SET_EXPONENT: "SET_EXPONENT",
          IntrinsicCall.Intrinsic.SHAPE: "SHAPE",
          IntrinsicCall.Intrinsic.SHIFTA: "SHIFTA",
          IntrinsicCall.Intrinsic.SHIFTL: "SHIFTL",
          IntrinsicCall.Intrinsic.SHIFTR: "SHIFTR",
          IntrinsicCall.Intrinsic.SIGN: "SIGN",
          IntrinsicCall.Intrinsic.SIN: "SIN",
          IntrinsicCall.Intrinsic.SINH: "SINH",
          IntrinsicCall.Intrinsic.SIZE: "SIZE",
          IntrinsicCall.Intrinsic.SPACING: "SPACING",
          IntrinsicCall.Intrinsic.SPREAD: "SPREAD",
          IntrinsicCall.Intrinsic.SQRT: "SQRT",
          IntrinsicCall.Intrinsic.STOPPED_IMAGES: "STOPPED_IMAGES",
          IntrinsicCall.Intrinsic.STORAGE_SIZE: "STORAGE_SIZE",
          IntrinsicCall.Intrinsic.SUM: "SUM",
          IntrinsicCall.Intrinsic.SYSTEM_CLOCK: "SYSTEM_CLOCK",
          IntrinsicCall.Intrinsic.TAN: "TAN",
          IntrinsicCall.Intrinsic.TANH: "TANH",
          IntrinsicCall.Intrinsic.TEAM_IMAGE: "TEAM_IMAGE",
          IntrinsicCall.Intrinsic.THIS_IMAGE: "THIS_IMAGE",
          IntrinsicCall.Intrinsic.TINY: "TINY",
          IntrinsicCall.Intrinsic.TRAILZ: "TRAILZ",
          IntrinsicCall.Intrinsic.TRANSFER: "TRANSFER",
          IntrinsicCall.Intrinsic.TRANSPOSE: "TRANSPOSE",
          IntrinsicCall.Intrinsic.TRIM: "TRIM",
          IntrinsicCall.Intrinsic.UBOUND: "UBOUND",
          IntrinsicCall.Intrinsic.UCOBOUND: "UCOBOUND",
          IntrinsicCall.Intrinsic.UNPACK: "UNPACK",
          IntrinsicCall.Intrinsic.VERIFY: "VERIFY",
        }

        arg_expr_list = []
        for arg in node.arguments:
            arg_expr_list.append(self._visit(arg))

        try:
            opstring = opmap[node.intrinsic]  # noqa: F841
        except KeyError as error:
            raise VisitorError(
                f"Unexpected intrinsic call '{node.intrinsic.name}'"
            ) from error

        if isinstance(node.routine.datatype, NoType):
            return psy_ir.CallExpr.get(
              node.routine.name, arg_expr_list, intrinsic=True
            )
        else:
            return psy_ir.CallExpr.get(
              node.routine.name, arg_expr_list,
              self.gen_type(node.routine.datatype), intrinsic=True
            )

    def literal_node(self, node):
        if (node.datatype.precision == ScalarType.Precision.SINGLE):
            width = 32
        elif (node.datatype.precision == ScalarType.Precision.DOUBLE):
            width = 64
        elif isinstance(node.datatype.precision, int):
            width = node.datatype.precision * 8
        else:
            width = 32
        if isinstance(node.datatype, ScalarType):
            if node.datatype.intrinsic == ScalarType.Intrinsic.INTEGER:
                return psy_ir.Literal.get(int(node.value), width)
            elif node.datatype.intrinsic == ScalarType.Intrinsic.REAL:
                return psy_ir.Literal.get(float(node.value), width)

        return psy_ir.Literal.get(node.value)

    def range_node(self, node):
        if node.parent and node.parent.is_lower_bound(
                node.parent.indices.index(node)):
            # The range starts for the first element in this
            # dimension. This is the default in Fortran so no need to
            # output anything.
            start = []
        else:
            start = [self._visit(node.start)]

        if node.parent and node.parent.is_upper_bound(
                node.parent.indices.index(node)):
            # The range ends with the last element in this
            # dimension. This is the default in Fortran so no need to
            # output anything.
            stop = []
        else:
            stop = [self._visit(node.stop)]

        if isinstance(node.step, Literal) and \
           node.step.datatype.intrinsic == ScalarType.Intrinsic.INTEGER and \
           node.step.value == "1":
            step = []
        else:
            step = [self._visit(node.step)]

        return psy_ir.Range.get(start, stop, step)

    def call_node(self, node):
        arg_expr_list = []
        for arg in node.arguments:
            arg_expr_list.append(self._visit(arg))

        if isinstance(node.routine.datatype, NoType):
            return psy_ir.CallExpr.get(
              node.routine.name, arg_expr_list, intrinsic=False
            )
        else:
            return psy_ir.CallExpr.get(
              node.routine.name, arg_expr_list,
              self.gen_type(node.routine.datatype),
              intrinsic=False
            )
