# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2025-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''

PSyIR fronted to ingest Fortran using the TreeSitter parse generator.

The structure of the expected fortran parse tree can be found in the
'rules' section of:
https://github.com/stadelmanma/tree-sitter-fortran/blob/master/grammar.js

To interpret the rules use:
https://tree-sitter.github.io/tree-sitter/creating-parsers/
2-the-grammar-dsl.html

'''

import codecs
from contextlib import contextmanager
from dataclasses import dataclass
from enum import Enum, auto
import logging
from typing import Callable, Iterable, Optional, TYPE_CHECKING, Union
from collections.abc import Generator, Container

from psyclone.errors import InternalError
from psyclone.psyir import nodes, symbols
from psyclone.psyir.nodes.codeblock import TreeSitterCodeBlock, CodeBlock

if TYPE_CHECKING:
    # Purposely inside typechecking because at runtime we want to lazily
    # import the parser only when it is actually used (import inside
    # generate_parse_tree_from_source)
    from tree_sitter import Node as TSNode


def log_decode_error_handler(err) -> tuple[str, int]:
    '''
    A custom error handler for use when reading files. Removes any
    characters that cause decoding errors and logs the error.

    :param err: the given error.

    :returns: 2-tuple containing replacement for bad chars (an empty string
        and the position from where encoding should continue.
    '''
    # Log the fact that this character will be removed from the input file
    logging.getLogger(__name__).warning(
        "Skipped bad character in input file, %s", str(err))
    return ("", err.end)


codecs.register_error("treesitter-encoding", log_decode_error_handler)


def to_str(node: 'TSNode') -> str:
    '''
    :param node: a given treesitter node.
    :returns: the string representing the node in utf8.
    '''
    return node.text.decode('utf8') if node.text else ""


def iter_child_of_type(
    tsnode: Optional['TSNode'], types: Union[str, Container[str]]
) -> Generator['TSNode']:
    ''' Provides a generator to iterate over the provided tsnode
    chidlren of the given type(s).

    :param tsnode: tree-sitter node whose children are searched.
    :param node_type: tree-sitter type to find.

    :yields: matching child, or ``None`` if no child matches.
    '''
    check_types = (types,) if isinstance(types, str) else types
    if tsnode:
        for child in tsnode.children:
            if child.type in check_types:
                yield child


def child_of_type(
    tsnode: Optional['TSNode'], node_type: Union[str, Container[str]]
) -> Optional['TSNode']:
    ''' Return the direct child having the supplied type(s). And validate
    that is the only child of the supplied type.

    :param tsnode: tree-sitter node whose children are searched.
    :param node_type: tree-sitter type(s) to find.

    :returns: matching child, or ``None`` if no child matches.

    :raises InternalError: if more than one node of that type exists.
    '''
    children = list(iter_child_of_type(tsnode, node_type))
    if len(children) == 0:
        return None
    elif len(children) > 1:
        raise InternalError("Expected only 1")
    return children[0]


@dataclass(frozen=True)
class _CommonDeclAttributes:
    ''' Properties shared by all entities of a fortran declaration (the lhs
    of ::).

    :param datatype: common PSyIR datatype or ``None`` if unsupported.
    :param intent: common PSyIR argument access.
    :param qualifiers: names of all declaration qualifiers.
    :param unsupported: qualifiers not represented directly in PSyIR.
    :param prefix: string preceding ``::`` (this is needed in case the
        entities end up as UnsupportedFortranType).
    '''

    datatype: Union[symbols.DataType, symbols.DataTypeSymbol, None]
    intent: symbols.ArgumentInterface.Access
    qualifiers: frozenset[str]
    unsupported: frozenset[str]
    prefix: str


class _NodeExpectation(Enum):
    '''Expected result of processing tree-sitter nodes.'''

    #: Expect a list (of zero, one or multiple) PSyIR nodes
    LIST = auto()
    #: Expect no result node (e.g. when processing a declaration)
    NONE = auto()
    #: Expect exactly one node
    ONE = auto()
    #: Expect exactly one node that is a DataNode
    EXPRESSION = auto()


class FortranTreeSitterReader():
    ''' Generate TreeSitter parse_trees and convert them to PSyIR nodes.

    The Reader works mostly by traversing the parse_tree with recursive
    calls to the _process_nodes dispatching method. This method calls
    the appropriate node handler for the given treesitter node.

    When Fortran scopes are found these are managed by the _using_scope
    context manager, this utility keeps a stack of nested scopes with a
    global reference to the top of the stack, this reference can be used
    everywhere to access the current scope.

    Generally, handlers can raise 3 types of errors:
    - NotImplementedError, if they find valid Fortran that is currently
    not supported. A parent node should catch it an convert it to a
    CodeBlock or an UnsupportedType declaration.
    - ValueError, if invalid Fortran is found.
    - InternalError, if an unexpected state is found that likely points
    to a bug in the Reader.

    Note that the implementation is incomplete, its main limitations are
    that:
    - the Reader parameters are ignored.
    - fparser is still not isolated, so the performance penalty of importing
    fparser is still paid when using treesitter.
    - the coverage of Fortran supported is more limited than in fparser.

    :param ignore_directives: Whether directives should be ignored or not
        (default True). Currently ignored.
    :param last_comments_as_codeblocks: Whether the last comments in the a
        given block (e.g. subroutine, do, if-then body, etc.) should be kept as
        CodeBlocks or lost (default False). Currently ignored.
    :param resolve_modules: Whether to resolve modules while parsing a file,
        for more precise control it also accepts a list of module names.
        Defaults to False. Currently ignored.
    :param ignore_comments: whether to let the parser ignore comments.
    :param free_form: whether to parse using Fortran free_form syntax.
    :param conditional_openmp: whether to parse conditional OpenMP statements.
    '''

    _UNARY_OPERATORS = {
        "+": nodes.UnaryOperation.Operator.PLUS,
        "-": nodes.UnaryOperation.Operator.MINUS,
        ".not.": nodes.UnaryOperation.Operator.NOT,
    }
    _BINARY_OPERATORS = {
        "+": nodes.BinaryOperation.Operator.ADD,
        "-": nodes.BinaryOperation.Operator.SUB,
        "*": nodes.BinaryOperation.Operator.MUL,
        "/": nodes.BinaryOperation.Operator.DIV,
        "**": nodes.BinaryOperation.Operator.POW,
        "==": nodes.BinaryOperation.Operator.EQ,
        ".eq.": nodes.BinaryOperation.Operator.EQ,
        "/=": nodes.BinaryOperation.Operator.NE,
        ".ne.": nodes.BinaryOperation.Operator.NE,
        "<": nodes.BinaryOperation.Operator.LT,
        ".lt.": nodes.BinaryOperation.Operator.LT,
        "<=": nodes.BinaryOperation.Operator.LE,
        ".le.": nodes.BinaryOperation.Operator.LE,
        ">": nodes.BinaryOperation.Operator.GT,
        ".gt.": nodes.BinaryOperation.Operator.GT,
        ">=": nodes.BinaryOperation.Operator.GE,
        ".ge.": nodes.BinaryOperation.Operator.GE,
        ".and.": nodes.BinaryOperation.Operator.AND,
        ".or.": nodes.BinaryOperation.Operator.OR,
        ".eqv.": nodes.BinaryOperation.Operator.EQV,
        ".neqv.": nodes.BinaryOperation.Operator.NEQV,
    }
    _INTENT_ACCESS = {
        "in": symbols.ArgumentInterface.Access.READ,
        "out": symbols.ArgumentInterface.Access.WRITE,
        "inout": symbols.ArgumentInterface.Access.READWRITE,
    }

    # Some tree-sitter node types share the same handler.
    _HANDLER_REDIRECTIONS = {
        "subroutine": "_procedure_handler",
        "function": "_procedure_handler",
        "program": "_procedure_handler",
        "unary_expression": "_operation",
        "logical_expression": "_operation",
        "relational_expression": "_operation",
        "math_expression": "_operation",
        "allocate_statement": "_memory_statement",
        "deallocate_statement": "_memory_statement",
        "nullify_statement": "_memory_statement",
    }

    # These arguments intentionally mirror the other Fortran reader API.
    # pylint: disable=too-many-arguments,too-many-positional-arguments
    def __init__(
        self,
        ignore_directives: bool = True,
        last_comments_as_codeblocks: bool = False,
        resolve_modules: bool = False,
        ignore_comments: bool = True,
        free_form: bool = True,
        conditional_openmp: bool = True,
    ):
        ''' Create a Fortran tree-sitter reader. '''
        # TODO #3038 Arguments are currently not used nor typechecked, but if
        # we decide this is the common reader interface, this can be done in a
        # super class instead of duplicate it here.
        self._ignore_directives = ignore_directives
        self._resolve_modules = resolve_modules
        self._last_comments_as_codeblocks = last_comments_as_codeblocks
        self._ignore_comments = ignore_comments
        self._free_form = free_form
        self._conditional_openmp = conditional_openmp
        # Keep a reference to the symbol table currently in scope, instead of
        # having it as argument everywhere. The initial one here is a
        # disposable instance (but prevents having to deal with the None type)
        self._current_scope: symbols.SymbolTable = symbols.SymbolTable()

    def generate_parse_tree_from_file(self, file_path) -> 'TSNode':
        '''
        Use the provided file to generate a treesitter parsetree.

        :param file_path: a given file.

        :returns: the treesitter parsetree of the given file.
        '''
        with open(
            file_path, encoding="utf-8", errors="treesitter-encoding"
        ) as fortran_file:
            source_code = fortran_file.read()
        return self.generate_parse_tree_from_source(source_code)

    def generate_parse_tree_from_source(
        self, source_code: str, partial_code: str = ""
    ) -> 'TSNode':
        ''' Use the provided source code to generate a treesitter parsetree.

        :param source_code: the given source code.
        :param partial_code: if the provided source_code is not a full unit
            this indicates the starting parsing point. It currently supports
            "expression" or "statement".

        :returns: the treesitter parsetree of the given source code.
        '''
        # pylint: disable=unused-argument
        # Purposely inlined to lazily load this modules only when needed
        # pylint: disable=import-outside-toplevel
        import tree_sitter_fortran
        from tree_sitter import Language, Parser

        def report_errors(node: 'TSNode'):
            ''' Recursively find and report errors.

            :param node: the given treesitter node

            :raises ValueError: if the given node has a parsing error.
            '''
            if node.type == 'ERROR':
                raise ValueError(
                    f"Syntax Error found at line {node.start_point[0] + 1}: "
                    f"{to_str(node)}")
            for child in node.children:
                report_errors(child)

        language = Language(tree_sitter_fortran.language())
        parser = Parser(language)
        parse_tree = parser.parse(bytes(source_code, "utf8"))
        report_errors(parse_tree.root_node)
        return parse_tree.root_node

    def generate_psyir(self, parse_tree: 'TSNode') -> nodes.Node:
        '''Translate the supplied treesitter node into PSyIR.

        :param parse_tree: the supplied treesitter parse tree.

        :returns: the equivalent PSyIR Node.
        '''
        # This is the public entry point, reset the scoping pointer
        self._current_scope = symbols.SymbolTable()
        return self._process_nodes(parse_tree, _NodeExpectation.ONE)

    @contextmanager
    def _using_scope(
        self, symtab: symbols.SymbolTable
    ) -> Generator[None]:
        ''' Make the given symtab the new parsing scope, but keep a reference
        to the previous scope in order to restore it when leaving this new
        scope (by a graceful exit or an exception).

        :param symtab: symbol table for the scope being translated.

        :yields: while ``symtab`` is the reader's current scope.
        '''
        previous_scope = self._current_scope
        self._current_scope = symtab
        try:
            yield
        finally:
            self._current_scope = previous_scope

    @contextmanager
    def _using_temporary_scope(
        self, parent: nodes.ScopingNode,
        scope: Optional[nodes.ScopingNode] = None
    ) -> Generator[None]:
        '''
        Like `_using_scope`, but soft-link the scope of the supplied parent
        or a disposable ScopingNode if none is provided. This is useful when
        the resulting PSyIR does not need the scope but lookup must still reach
        the parent. For example in the body of a derived type:

        .. code-block:: fortran

        module m
           integer, parameter :: size = 10
           type myt
              integer, dimension(size) :: array
           end type
        end module

        :param parent: real scope used for lexical lookup.
        :param scope: existing orphan scope to use, or ``None`` to create a
            disposable one.

        :yields: while the provided or a disposable scope is soft-linked
            to provide a temporary current scope.

        :raises ValueError: if the supplied scope already has a parent.
        '''
        if scope:
            if scope.parent is not None:
                raise InternalError("The supplied scope must be an orphan")
        else:
            scope = nodes.ScopingNode(symbol_table=symbols.SymbolTable())

        previous_scope = self._current_scope
        # This intentionally bypasses child validation.
        # pylint: disable=protected-access
        scope._parent = parent
        self._current_scope = scope.symbol_table
        try:
            yield
        finally:
            # Remove soft link
            scope._parent = None
            self._current_scope = previous_scope

    def _process_nodes(
        self,
        tsnodes: Union["TSNode", Iterable["TSNode"]],
        expect: _NodeExpectation,
    ) -> Optional[Union[list[nodes.Node], nodes.Node]]:
        '''
        This is the tsnodes handler dispatcher. Unsupported syntax is
        deliberately caught here rather than in individual handlers so that
        continuous unsupported nodes can be placed in a single CodeBlock.

        :param tsnodes: one tree-sitter node or an iterable of nodes.
        :param expect: expected number and kind of result nodes.

        :returns: PSyIR nodes produced from the supplied tree-sitter nodes.
        '''
        list_of_nodes = tsnodes if isinstance(tsnodes, Iterable) else [tsnodes]
        children = []
        for tsnode in list_of_nodes:
            try:
                handler = self._get_handler(tsnode)
                result = handler(tsnode)
                if result is not None:
                    children.append(result)
            except NotImplementedError as err:
                # TODO #3038: Aggregate contiguous CodeBlocks.
                structure = (CodeBlock.Structure.EXPRESSION
                             if expect is _NodeExpectation.EXPRESSION
                             else CodeBlock.Structure.STATEMENT)
                children.append(
                    self._create_codeblock(tsnode, str(err), structure))

        # Validate that the parsed nodes match the expectations of the caller
        if expect in (_NodeExpectation.ONE, _NodeExpectation.EXPRESSION):
            if len(children) != 1:
                raise InternalError(
                    f"Only one node was expected in this location but got:\n"
                    f"{[type(c).__name__ for c in children]}"
                )
            if expect is _NodeExpectation.EXPRESSION:
                if not isinstance(children[0], nodes.DataNode):
                    raise InternalError(
                        f"A DataNode was expected in this location but got: "
                        f"{type(children[0]).__name__}"
                    )
            return children[0]
        if expect is _NodeExpectation.NONE:
            if len(children) != 0:
                raise InternalError(
                    f"No node was expected in this location but got:\n"
                    f"{[type(c).__name__ for c in children]}"
                )
            return None
        if expect is not _NodeExpectation.LIST:
            raise InternalError(
                    f"Unsupported node expectation '{expect}'")
        return children

    @staticmethod
    def _create_codeblock(
        tsnode: 'TSNode', reason: str,
        structure: CodeBlock.Structure = CodeBlock.Structure.STATEMENT
    ) -> TreeSitterCodeBlock:
        '''Create a CodeBlock for unsupported valid Fortran.

        :param tsnode: tree-sitter node containing unsupported Fortran.
        :param reason: human-readable explanation of the limitation.
        :param structure: whether the unsupported code is a statement or an
            expression.

        :returns: CodeBlock retaining the original tree-sitter node.
        '''
        code_block = TreeSitterCodeBlock(tsnode, structure)
        code_block.append_preceding_comment(
            f"PSyclone CodeBlock (unsupported code) reason:\n"
            f"- {reason}"
        )
        return code_block

    def _get_handler(self, tsnode: 'TSNode') -> Callable:
        '''
        :param tsnode: a given treesitter node.

        :returns: the method that handles the given node type.

        :raises NotImplementedError: if the given node type does not have a
            handler for it.
        '''
        # Some nodes use a common handler
        redirection = self._HANDLER_REDIRECTIONS.get(tsnode.type)
        if redirection:
            return getattr(self, redirection)

        # Otherwise use the handler that matches its name
        handler = getattr(self, f"_{tsnode.type}_handler", None)
        if handler is not None:
            return handler

        # If at this point we still don't have a handler, it is unsupported
        raise NotImplementedError(
            f"Unsupported '{tsnode.type}' tree-sitter node.") from None

    def _translation_unit_handler(
        self, tsnode: 'TSNode'
    ) -> nodes.Node:
        ''' Handle treesitter 'translation_unit' node.

        :param tsnode: the treesitter node the process.

        :returns: the equivalent PSyIR Node.
        '''
        file_container = nodes.FileContainer("")
        with self._using_scope(file_container.symbol_table):
            file_container.children.extend(
                self._process_nodes(tsnode.children, _NodeExpectation.LIST)
            )
        return file_container

    def _module_handler(
        self, tsnode: 'TSNode'
    ) -> nodes.Node:
        ''' Handle a treesitter 'module' node.

        :param tsnode: the treesitter node the process.

        :returns: the equivalent PSyIR Node.

        :raises NotImplementedError: if the module has an unsupported child.
        :raises NotImplementedError: if the module permits implicit variables.
        '''
        statement = child_of_type(tsnode, "module_statement")
        name = child_of_type(statement, "name")
        container = nodes.Container(to_str(name) if name else "")

        with self._using_scope(container.symbol_table):
            visibility_map = self._process_access_statements(tsnode.children)

            # This nodes are already processed
            skip = {
                "module_statement", "end_module_statement",
                "implicit_statement", "internal_procedures",
                "public_statement", "private_statement"
            }
            # Specification statements normally only update the symbol table
            # and therefore return no Node. Keep any unsupported statements
            # as CodeBlocks so that valid Fortran is not lost (and, in
            # particular, does not violate an expectation of no result).
            container.children.extend(self._process_nodes(
                [child for child in tsnode.children
                 if child.type not in skip], _NodeExpectation.LIST))

            internal = child_of_type(tsnode, "internal_procedures")
            if internal:
                container.children.extend(
                    self._process_nodes(
                        [child for child in internal.children
                         if child.type != "contains_statement"],
                        _NodeExpectation.LIST))
            self._apply_visibility(visibility_map)
        return container

    def _procedure_handler(
        self, tsnode: 'TSNode'
    ) -> nodes.Routine:
        '''Handler shared by programs, subroutines and functions.

        :param tsnode: the procedure treesitter node.
        :returns: translated PSyIR Routine.
        '''
        routine_kind = tsnode.type
        signature = child_of_type(tsnode, f"{routine_kind}_statement")
        name_node = child_of_type(signature, "name")
        name = to_str(name_node) if name_node else routine_kind
        parameters = child_of_type(signature, "parameters")
        argument_names = tuple(
            to_str(child) for child in parameters.children
            if child.type == "identifier") if parameters else ()
        return_name, return_type = self._function_return_info(
            signature, name, routine_kind)

        # Insert arguments before declarations so specify_argument_list() can
        # retain source order. Declarations later complete these placeholders.
        routine_table = symbols.SymbolTable()
        for arg_name in argument_names:
            routine_table.add(symbols.DataSymbol(
                arg_name, symbols.UnresolvedType(),
                interface=symbols.ArgumentInterface()))
        if return_name and return_name not in routine_table:
            routine_table.add(symbols.DataSymbol(
                return_name, return_type or symbols.UnresolvedType()))

        rsymbol = self._create_routine_symbol(
            name, signature, return_type)
        routine = nodes.Routine(
            rsymbol, is_program=routine_kind == "program",
            symbol_table=routine_table)

        # A routine must be parsed inside a scope as it is also a declaration
        parent_symtab = self._current_scope
        parent = parent_symtab.node if parent_symtab else None
        if not isinstance(parent, nodes.ScopingNode):
            raise InternalError(
                "A Routine must be translated within a PSyIR scope")

        with self._using_temporary_scope(parent, routine):
            vis_map = self._process_access_statements(tsnode.children)
            # This nodes are already processed
            skip = {
                f"{routine_kind}_statement",
                f"end_{routine_kind}_statement",
                "implicit_statement", "public_statement",
                "private_statement"
            }
            routine.children.extend(self._process_nodes(
                [child for child in tsnode.children
                 if child.type not in skip], _NodeExpectation.LIST))

            args = [routine.symbol_table.lookup(name)
                    for name in argument_names]
            routine.symbol_table.specify_argument_list(args)
            if return_name:
                routine.return_symbol = routine.symbol_table.lookup(
                    return_name)
            self._apply_visibility(vis_map)

            # PSyIR cannot represent implicitly declared data. Leave
            # unresolved names alone only when a wildcard import or a
            # CodeBlock could contain their declaration, matching the
            # conditions under which the Fortran backend can preserve them.
            fallback_context = (
                routine.symbol_table.wildcard_imports() or
                routine.walk(nodes.CodeBlock))
            for symbol in routine.symbol_table.datasymbols:
                if not isinstance(symbol.datatype, symbols.UnresolvedType):
                    continue
                if (isinstance(symbol.interface,
                               symbols.UnresolvedInterface) and
                        fallback_context):
                    continue
                raise NotImplementedError(
                    f"Implicit declaration of '{symbol.name}' is not "
                    "supported")
        return routine

    def _function_return_info(
        self, signature: 'TSNode', routine_name: str, routine_kind: str
    ) -> tuple[Optional[str], Optional[symbols.DataType]]:
        '''Extract result name and datatype from a function statement.

        :param statement: node containing the function signature.
        :param routine_name: name of the program unit.
        :param routine_kind: one of ``program``, ``subroutine`` or
            ``function``.

        :returns: result-symbol name and datatype, both ``None`` for a
            non-function.
        '''
        if routine_kind != "function":
            return None, None

        result = child_of_type(signature, "function_result")
        result_name = child_of_type(result, "identifier")
        return_name = to_str(result_name) if result_name else routine_name
        type_node = next(
            (child for child in signature.children
             if child.type in ("intrinsic_type", "derived_type")), None)
        if not type_node:
            return return_name, None
        try:
            return return_name, self._datatype_from_type(type_node)
        except (NotImplementedError, KeyError, TypeError):
            return return_name, symbols.UnsupportedFortranType(
                to_str(signature).strip())

    def _create_routine_symbol(
        self, name: str, signature: 'TSNode', return_type
    ) -> symbols.RoutineSymbol:
        '''Create or complete the RoutineSymbol for a program unit.

        An interface block may create the symbol before its implementation is
        visited. Reusing it ensures interface members and the Routine node
        refer to the same object.

        :param name: routine name.
        :param signature: the signature node of the routine.
        :param return_type: translated function return type, if any.

        :returns: RoutineSymbol representing the program unit.
        '''
        qualifiers = {
            to_str(child).lower() for child in signature.children
            if child.type == "procedure_qualifier"}
        visibility = self._current_scope.default_visibility
        try:
            routine_symbol = self._current_scope.lookup(name)
        except KeyError:
            routine_symbol = None
        if isinstance(routine_symbol, symbols.RoutineSymbol):
            routine_symbol.datatype = (
                return_type or routine_symbol.datatype)
            routine_symbol.is_pure = "pure" in qualifiers
            routine_symbol.is_elemental = "elemental" in qualifiers
            routine_symbol.visibility = visibility
            return routine_symbol
        return symbols.RoutineSymbol(
            name, datatype=return_type or symbols.UnresolvedType(),
            is_pure="pure" in qualifiers,
            is_elemental="elemental" in qualifiers,
            visibility=visibility)

    def _number_literal_handler(
        self, tsnode: 'TSNode'
    ) -> nodes.Literal:
        '''Translate an integer or real literal.

        :param tsnode: number-literal tree-sitter node.

        :returns: PSyIR integer or real Literal.
        '''
        text = to_str(tsnode).lower()
        value, _, kind = text.partition("_")
        is_real = any(char in value for char in ".ed")
        # PSyIR stores all real exponents using ``e`` notation while the
        # Fortran ``d`` exponent also specifies double precision.
        has_double_exponent = "d" in value
        if has_double_exponent:
            value = value.replace("d", "e", 1)
        datatype = (
            symbols.ScalarType.real_double_type()
            if has_double_exponent else
            symbols.ScalarType.real_type()
            if is_real else symbols.ScalarType.integer_type())
        if kind:
            # A numeric Fortran KIND value is processor-specific and is not
            # equivalent to either a byte size or PSyIR relative precision.
            precision = (int(kind) if kind.isdigit()
                         else nodes.Reference(self._kind_symbol(kind)))
            datatype = symbols.ScalarType(
                datatype.intrinsic, precision)
        return nodes.Literal(value, datatype)

    def _string_literal_handler(
        self, tsnode: 'TSNode'
    ) -> nodes.Literal:
        '''Translate a character literal.

        :param tsnode: string-literal tree-sitter node.

        :returns: PSyIR character Literal.
        '''
        text = to_str(tsnode)
        quote_positions = [position for position in
                           (text.find("'"), text.find('"'))
                           if position >= 0]
        if not quote_positions:
            raise NotImplementedError(
                "A character literal has no quote delimiter")
        quote_position = min(quote_positions)
        quote = text[quote_position]
        if text[-1] != quote:
            raise NotImplementedError(
                "A character literal has mismatched quote delimiters")

        prefix = text[:quote_position]
        datatype = symbols.ScalarType.character_type()
        if prefix:
            if not prefix.endswith("_") or len(prefix) == 1:
                raise NotImplementedError(
                    "Unsupported character literal kind prefix")
            kind = prefix[:-1].lower()
            precision = (int(kind) if kind.isdigit() else
                         nodes.Reference(self._kind_symbol(kind)))
            datatype = symbols.ScalarType(
                symbols.ScalarType.Intrinsic.CHARACTER, precision)
        value = text[quote_position + 1:-1].replace(quote * 2, quote)
        return nodes.Literal(value, datatype)

    def _boolean_literal_handler(
        self, tsnode: 'TSNode'
    ) -> nodes.Literal:
        '''Translate a logical literal.

        :param tsnode: boolean-literal tree-sitter node.

        :returns: PSyIR boolean Literal.
        '''
        value = to_str(tsnode).lower().strip(".")
        return nodes.Literal(value, symbols.ScalarType.boolean_type())

    def _variable_declaration_handler(
        self, tsnode: 'TSNode'
    ) -> None:
        '''Translate every entity in a variable declaration.

        :param tsnode: variable-declaration tree-sitter node.
        '''
        # A declaration has properties shared by every entity (type and
        # attributes) followed by one or more entity-specific declarators.
        type_node = next((child for child in tsnode.children
                          if child.type in
                          ("intrinsic_type", "derived_type")), None)
        if not type_node:
            raise NotImplementedError(
                "A variable declaration has no supported type specification")

        qualifiers = [child for child in tsnode.children
                      if child.type == "type_qualifier"]
        qualifier_names = frozenset(
            child.children[0].type
            for child in qualifiers if child.children)
        supported_qualifiers = {
            "allocatable", "dimension", "intent", "parameter", "private",
            "public", "save"
        }
        # If we find anything else it will be UnsupportedType
        unsupported = qualifier_names.difference(supported_qualifiers)
        try:
            datatype = self._datatype_from_type(type_node)
        except (NotImplementedError, KeyError, TypeError):
            datatype = None

        dimension = next(
            (child_of_type(item, "argument_list") for item in qualifiers
             if item.children and item.children[0].type == "dimension"), None)
        is_allocatable = "allocatable" in qualifier_names
        if datatype and dimension:
            try:
                shape = self._shape_from_node(dimension, is_allocatable)
                datatype = symbols.ArrayType(datatype, shape)
            except (NotImplementedError, TypeError):
                datatype = None

        intent_node = next(
            (item for item in qualifiers
             if item.children and item.children[0].type == "intent"), None)
        intent = symbols.ArgumentInterface.Access.UNKNOWN
        if intent_node:
            intent = next(
                (self._INTENT_ACCESS[child.type]
                 for child in intent_node.children
                 if child.type in self._INTENT_ACCESS), intent)
        common_attr = _CommonDeclAttributes(
            datatype, intent, qualifier_names,
            frozenset(unsupported),
            to_str(tsnode).split("::", maxsplit=1)[0].strip())

        for declarator in tsnode.children:
            if declarator.type in (
                    "identifier", "sized_declarator", "init_declarator"):
                self._declare_entity(declarator, common_attr)

    def _declare_entity(
        self, declarator: 'TSNode', common_attr: _CommonDeclAttributes
    ):
        '''Translate one entity and add it to the current symbol table.

        :param declarator: identifier or entity-declarator tree-sitter node.
        :param common_attr: properties shared by the complete declaration.
        '''
        id_node = (declarator if declarator.type == "identifier"
                   else child_of_type(declarator, "identifier"))
        name = to_str(id_node)
        datatype, initial_value = self._declarator_datatype(
            declarator, common_attr)

        visibility = self._current_scope.default_visibility
        if "public" in common_attr.qualifiers:
            visibility = symbols.Symbol.Visibility.PUBLIC
        elif "private" in common_attr.qualifiers:
            visibility = symbols.Symbol.Visibility.PRIVATE

        kwargs = {"visibility": visibility}
        interface = self._declaration_interface(
            name, common_attr, initial_value is not None)
        if interface is not None:
            kwargs["interface"] = interface
        if initial_value is not None:
            kwargs["initial_value"] = initial_value
        if "parameter" in common_attr.qualifiers:
            kwargs["is_constant"] = True
        declared_symbol = symbols.DataSymbol(name, datatype, **kwargs)
        self._add_or_update_datasymbol(declared_symbol)

    def _declarator_datatype(
        self, declarator: 'TSNode', common_attr: _CommonDeclAttributes
    ):
        '''Translate one entity's datatype and initial value.

        Entity-specific syntax must be handled independently. In particular,
        an unsupported initializer on one entity must not make its siblings
        unsupported.

        :param declarator: entity-declarator tree-sitter node.
        :param common_attr: properties shared by the complete declaration.

        :returns: entity datatype and optional initial-value expression.
        '''
        is_unsupported = bool(common_attr.unsupported)
        initial_value = None
        if declarator.type == "init_declarator":
            expressions = [
                child for child in declarator.children
                if child.type not in ("identifier", "=")]
            if expressions:
                try:
                    initial_value = self._process_nodes(
                        expressions[-1], _NodeExpectation.EXPRESSION)
                except NotImplementedError:
                    is_unsupported = True

        # An entity-specific shape takes precedence over a shared DIMENSION
        # attribute. The latter has already been translated into an ArrayType,
        # from which the elemental type can be recovered.
        datatype = common_attr.datatype
        shape_node = child_of_type(declarator, "size")
        is_allocatable = "allocatable" in common_attr.qualifiers
        if datatype and shape_node:
            try:
                elemental_type = (
                    datatype.elemental_type
                    if isinstance(datatype, symbols.ArrayType) else datatype)
                if isinstance(elemental_type, symbols.DataType):
                    elemental_type = elemental_type.copy()
                shape = self._shape_from_node(
                    shape_node, is_allocatable)
                datatype = symbols.ArrayType(elemental_type, shape)
            except (NotImplementedError, TypeError):
                datatype = None
        elif is_allocatable and not isinstance(datatype, symbols.ArrayType):
            datatype = None
        elif isinstance(datatype, symbols.DataType):
            # A datatype can contain PSyIR expressions (e.g. array bounds or
            # character length), which must not be shared between symbols.
            datatype = datatype.copy()

        # UnsupportedFortranType is preferable to losing a declaration.
        # Keep only this entity in the saved text to avoid duplicating sibling
        # names when the Fortran backend emits the symbols.
        if datatype is None or is_unsupported:
            entity = to_str(declarator).strip()
            datatype = symbols.UnsupportedFortranType(
                f"{common_attr.prefix} :: {entity}")
        return datatype, initial_value

    def _declaration_interface(
        self, name: str, common_attr: _CommonDeclAttributes,
        has_initial_value: bool = False
    ):
        '''Return the PSyIR interface for one declared entity.

        Interfaces describe where a symbol is defined and how a dummy argument
        may be accessed. They are independent of its datatype.

        :param name: declared entity name.
        :param common_attr: properties shared by the complete declaration.
        :param has_initial_value: whether the entity has an initializer.

        :returns: symbol interface, or ``None`` for an automatic local.
        '''
        symtab = self._current_scope
        if name in symtab and symtab.lookup(name).is_argument:
            return symbols.ArgumentInterface(common_attr.intent)
        if (has_initial_value or
                {"save", "parameter"}.intersection(common_attr.qualifiers)):
            return symbols.StaticInterface()
        if isinstance(symtab.node, nodes.Container):
            return symbols.DefaultModuleInterface()
        return None

    def _add_or_update_datasymbol(
        self, declared_symbol: symbols.DataSymbol
    ):
        '''Add a declared symbol or complete a dummy-argument placeholder.

        Routine arguments are inserted before declarations are processed so
        their source order is known. Their later declaration therefore updates
        the placeholder, while ordinary local declarations create a new
        DataSymbol.

        :param declared_symbol: fully translated symbol for the entity.

        :raises ValueError: if the name is already used for an imported
            module.
        :raises NotImplementedError: if an existing symbol is not a
            DataSymbol.
        '''
        symtab = self._current_scope
        if declared_symbol.name in symtab:
            symbol = symtab.lookup(declared_symbol.name)
            if isinstance(symbol, symbols.ContainerSymbol):
                raise ValueError(
                    f"USE module '{symbol.name}' conflicts with another "
                    f"symbol")
            if not isinstance(symbol, symbols.DataSymbol):
                raise NotImplementedError(
                    f"'{declared_symbol.name}' is already declared as a "
                    f"non-data symbol")
            symbol.datatype = declared_symbol.datatype
            symbol.visibility = declared_symbol.visibility
            if not isinstance(
                    declared_symbol.interface, symbols.AutomaticInterface):
                symbol.interface = declared_symbol.interface
            if declared_symbol.initial_value is not None:
                # The initial value belongs to the temporary symbol created
                # for this declaration. Give the existing symbol its own
                # expression tree when completing a forward reference.
                symbol.initial_value = declared_symbol.initial_value.copy()
            if declared_symbol.is_constant:
                symbol.is_constant = True
            return
        symtab.add(declared_symbol)

    def _datatype_from_type(
        self, tsnode: 'TSNode'
    ):
        '''Return a PSyIR datatype for a tree-sitter type specification.

        :param tsnode: intrinsic- or derived-type tree-sitter node.

        :returns: translated PSyIR datatype.

        :raises NotImplementedError: if PSyIR cannot represent the type.
        '''
        symtab = self._current_scope
        if tsnode.type == "derived_type":
            keyword = tsnode.children[0].type
            name_node = child_of_type(tsnode, "type_name")
            name = to_str(name_node)
            if keyword == "class":
                raise NotImplementedError(
                    "Polymorphic CLASS declarations are not supported")
            try:
                return self._current_scope.lookup(name)
            except KeyError:
                datatype = symbols.DataTypeSymbol(
                    name, symbols.UnresolvedType())
                symtab.add(datatype)
                return datatype

        intrinsic = tsnode.children[0].type
        mapping = {
            "integer": symbols.ScalarType.Intrinsic.INTEGER,
            "real": symbols.ScalarType.Intrinsic.REAL,
            "logical": symbols.ScalarType.Intrinsic.BOOLEAN,
            "character": symbols.ScalarType.Intrinsic.CHARACTER,
        }
        if intrinsic not in mapping:
            if to_str(tsnode).lower().replace(" ", "_") == "double_precision":
                return symbols.ScalarType.real_double_type()
            raise NotImplementedError(
                f"Intrinsic type '{intrinsic}' has no PSyIR representation")
        precision = symbols.ScalarType.Precision.UNDEFINED
        length = None
        kind_node = child_of_type(tsnode, "kind")
        if kind_node:
            values = [child for child in kind_node.children
                      if child.type not in ("(", ")")]
            if values:
                value = values[0]
                if value.type == "keyword_argument":
                    key = to_str(value.children[0]).lower()
                    value = value.children[-1]
                    if key == "len":
                        length = self._process_nodes(
                            value, _NodeExpectation.EXPRESSION)
                    else:
                        precision = self._precision(value)
                elif intrinsic == "character":
                    length = self._process_nodes(
                        value, _NodeExpectation.EXPRESSION)
                else:
                    precision = self._precision(value)
        return symbols.ScalarType(mapping[intrinsic], precision, length)

    def _precision(self, tsnode: 'TSNode'):
        '''Translate a kind expression into a ScalarType precision value.

        :param tsnode: tree-sitter node containing the kind expression.

        :returns: precision enumeration, integer or PSyIR Reference.

        :raises NotImplementedError: if the kind expression is unsupported.
        '''
        expr = self._process_nodes(tsnode, _NodeExpectation.EXPRESSION)
        if isinstance(expr, nodes.Literal) and expr.value.isdigit():
            # Keep numeric KIND selectors as PSyIR expressions. An integer
            # precision would instead mean a byte size to PSyIR backends.
            return expr
        if isinstance(expr, nodes.Reference):
            # pylint: disable=unidiomatic-typecheck
            if type(expr.symbol) is symbols.Symbol:
                # All precisions must be integers
                expr.symbol.specialise(
                    symbols.DataSymbol,
                    datatype=symbols.ScalarType.integer_type())
            return expr
        raise NotImplementedError("kind expressions are not supported")

    def _kind_symbol(
        self, name: str
    ) -> symbols.DataSymbol:
        '''Look up or create a symbol used as a kind parameter.

        :param name: name of the kind parameter.

        :returns: DataSymbol representing the kind parameter.

        :raises NotImplementedError: if the name resolves to another symbol
            type.
        '''
        symtab = self._current_scope
        try:
            symbol = self._current_scope.lookup(name)
        except KeyError:
            symbol = symbols.DataSymbol(
                name, symbols.ScalarType.integer_type(),
                interface=symbols.UnresolvedInterface())
            symtab.add(symbol)
        return symbol

    @staticmethod
    def _split_extent(
        tsnode: 'TSNode'
    ) -> tuple[list['TSNode'], list['TSNode'], bool]:
        ''' Split the children of a ``lower:upper``-style construct.

        :param tsnode: tree-sitter node containing an optional colon.

        :returns: children before the colon, children after it, and whether a
            colon was present.
        '''
        colon = next((idx for idx, child in enumerate(tsnode.children)
                      if child.type == ":"), None)
        if colon is None:
            return list(tsnode.children), [], False
        return (list(tsnode.children[:colon]),
                list(tsnode.children[colon + 1:]), True)

    def _shape_from_node(
        self, tsnode: 'TSNode', is_allocatable: bool = False
    ) -> list:
        '''Translate a declaration size or argument list into an array
        shape.

        :param tsnode: size or argument-list tree-sitter node.
        :param is_allocatable: whether open bounds are deferred rather than
            assumed shape.

        :returns: PSyIR ArrayType shape entries.
        '''
        result = []
        for child in tsnode.children:
            if child.type in ("(", ")", ","):
                continue
            if child.type == "extent_specifier":
                before, after, has_colon = self._split_extent(child)
                if not has_colon:
                    result.append(self._process_nodes(
                        before[0], _NodeExpectation.EXPRESSION))
                    continue
                if not before and not after:
                    result.append(symbols.ArrayType.Extent.DEFERRED
                                  if is_allocatable else
                                  symbols.ArrayType.Extent.ATTRIBUTE)
                elif before and not after:
                    if is_allocatable:
                        # PSyIR cannot currently distinguish a deferred upper
                        # bound with an explicit lower bound from an assumed-
                        # shape bound, so preserve the declaration verbatim.
                        raise NotImplementedError(
                            "An allocatable bound with an explicit lower "
                            "bound is not supported")
                    result.append(
                        (self._process_nodes(
                            before[0], _NodeExpectation.EXPRESSION),
                         symbols.ArrayType.Extent.ATTRIBUTE))
                elif after and not before:
                    result.append(self._process_nodes(
                        after[0], _NodeExpectation.EXPRESSION))
                else:
                    result.append((self._process_nodes(
                        before[0], _NodeExpectation.EXPRESSION),
                        self._process_nodes(
                            after[0], _NodeExpectation.EXPRESSION)))
            else:
                result.append(self._process_nodes(
                    child, _NodeExpectation.EXPRESSION))
        return result

    def _process_access_statements(
        self, tsnodes: Iterable['TSNode']
    ) -> dict[str, symbols.Symbol.Visibility]:
        '''Record default and name-specific visibility for a scope.

        :param tsnodes: tree-sitter children of the current scope.

        :returns: explicit visibility indexed by normalised symbol name.
        '''
        symtab = self._current_scope
        visibility_map = {}
        for tsnode in tsnodes:
            if tsnode.type not in ("public_statement", "private_statement"):
                continue
            visibility = (symbols.Symbol.Visibility.PUBLIC
                          if tsnode.type == "public_statement" else
                          symbols.Symbol.Visibility.PRIVATE)
            names = [to_str(child) for child in tsnode.children
                     if child.type in ("identifier", "name", "type_name",
                                       "method_name")]
            if names:
                visibility_map.update({name.lower(): visibility
                                       for name in names})
            else:
                symtab.default_visibility = visibility
        return visibility_map

    def _apply_visibility(
        self, visibility_map: dict[str, symbols.Symbol.Visibility]
    ):
        '''Apply name-specific access rules after a scope is populated.

        Fortran access statements apply irrespective of source order. Delaying
        this step until all declarations and contained routines have been
        processed means the map remains local to its scope handler.

        :param visibility_map: explicit visibility indexed by symbol name.
        '''
        symtab = self._current_scope
        for name, visibility in visibility_map.items():
            # An unsupported specification may survive only as a CodeBlock and
            # therefore have no symbol to update.
            if name in symtab:
                symtab.lookup(
                    name, scope_limit=symtab.node).visibility = visibility

    def _use_statement_handler(
        self, tsnode: 'TSNode'
    ) -> None:
        '''Translate a USE statement into container and imported symbols.

        :param tsnode: use-statement tree-sitter node.

        :raises ValueError: if the module name conflicts with an
            existing non-container symbol.
        '''
        symtab = self._current_scope
        module_node = child_of_type(tsnode, "module_name")
        module_name = to_str(module_node)
        intrinsic = any(child.type == "intrinsic" for child in tsnode.children)
        included = child_of_type(tsnode, "included_items")
        wildcard = included is None
        try:
            container = symtab.lookup(module_name)
        except KeyError:
            container = symbols.ContainerSymbol(
                module_name, wildcard_import=wildcard,
                is_intrinsic=intrinsic,
                visibility=symtab.default_visibility)
            symtab.add(container)
        if not isinstance(container, symbols.ContainerSymbol):
            raise ValueError(
                f"USE module '{module_name}' conflicts with another symbol")
        # Multiple USE statements for the same module are cumulative. An
        # ONLY list must therefore not undo a wildcard import seen earlier.
        container.wildcard_import = container.wildcard_import or wildcard

        import_items = list(included.children) if included else []
        # A rename list without ONLY is represented directly beneath the USE
        # statement rather than inside ``included_items``.
        import_items.extend(
            child for child in tsnode.children
            if child.type in ("rename", "use_rename", "use_alias"))
        for child in import_items:
            if child.type == "identifier":
                local_name = to_str(child)
                self._add_imported_symbol(
                    local_name, local_name, container)
            elif child.type in ("rename", "use_rename", "use_alias"):
                names = [item for item in child.children
                         if item.type in
                         ("identifier", "name", "local_name")]
                if len(names) == 2:
                    self._add_imported_symbol(
                        to_str(names[0]), to_str(names[1]), container)

    def _add_imported_symbol(
        self, local_name: str, remote_name: str,
        container: symbols.ContainerSymbol
    ):
        '''Add one symbol imported from a container.

        :param local_name: name used for the symbol in this scope.
        :param remote_name: original name in the imported container.
        :param container: symbol representing the imported module.
        '''
        symtab = self._current_scope
        interface = symbols.ImportInterface(
            container, orig_name=(remote_name
                                  if remote_name != local_name else None))
        try:
            existing = symtab.lookup(local_name)
        except KeyError:
            symtab.add(symbols.Symbol(
                local_name, visibility=symtab.default_visibility,
                interface=interface))
        else:
            if existing is not container:
                existing.interface = interface

    def _derived_type_definition_handler(
        self, tsnode: 'TSNode'
    ) -> None:
        '''Translate a simple Fortran derived-type definition.

        :param tsnode: derived-type-definition tree-sitter node.

        :raises NotImplementedError: if the type name conflicts with an
            existing non-datatype symbol.
        '''
        symtab = self._current_scope
        statement = child_of_type(tsnode, "derived_type_statement")
        name_node = child_of_type(statement, "type_name")
        name = to_str(name_node)
        unsupported = any(child.type == "derived_type_procedures"
                          for child in tsnode.children)
        datatype = None
        if not unsupported:
            parent = symtab.node
            if not isinstance(parent, nodes.ScopingNode):
                raise InternalError(
                    "A derived type must be translated within a PSyIR scope")
            with self._using_temporary_scope(parent):
                visibility_map = self._process_access_statements(
                    tsnode.children)
                try:
                    for declaration in iter_child_of_type(
                            tsnode, "variable_declaration"):
                        self._variable_declaration_handler(declaration)
                    self._apply_visibility(visibility_map)
                    datatype = symbols.StructureType()
                    for component in self._current_scope.datasymbols:
                        datatype.add(
                            component.name, component.datatype,
                            component.visibility,
                            component.initial_value)
                except (NotImplementedError, TypeError, ValueError):
                    datatype = None
        if datatype is None:
            datatype = symbols.UnsupportedFortranType(to_str(tsnode).strip())

        visibility = symtab.default_visibility
        access = child_of_type(statement, "access_specifier")
        if access:
            visibility = (symbols.Symbol.Visibility.PRIVATE
                          if "private" in to_str(access).lower() else
                          symbols.Symbol.Visibility.PUBLIC)
        try:
            existing = symtab.lookup(name)
        except KeyError:
            symtab.add(symbols.DataTypeSymbol(
                name, datatype, visibility=visibility))
        else:
            if not isinstance(existing, symbols.DataTypeSymbol):
                raise NotImplementedError(
                    f"Derived type '{name}' conflicts with another symbol")
            existing.datatype = datatype
            existing.visibility = visibility

    def _interface_handler(
        self, tsnode: 'TSNode'
    ) -> None:
        '''Translate a named interface containing procedure declarations.

        :param tsnode: interface tree-sitter node.

        :raises NotImplementedError: if the interface form or a member is
            unsupported.
        '''
        symtab = self._current_scope
        statement = child_of_type(tsnode, "interface_statement")
        name_node = child_of_type(statement, "name")
        if not name_node:
            raise NotImplementedError(
                "Abstract and operator interfaces are not supported")
        name = to_str(name_node)
        routines = []
        for procedure in iter_child_of_type(tsnode, "procedure_statement"):
            from_container = "module" in [
                child.type for child in procedure.children[0].children]
            for method in iter_child_of_type(procedure, "method_name"):
                routine_name = to_str(method)
                try:
                    routine = symtab.lookup(routine_name)
                except KeyError:
                    routine = symbols.RoutineSymbol(routine_name)
                    symtab.add(routine)
                if not isinstance(routine, symbols.RoutineSymbol):
                    raise NotImplementedError(
                        f"Interface member '{routine_name}' is not a routine")
                routines.append((routine, from_container))
        if not routines:
            raise NotImplementedError(
                "Interfaces containing routine bodies are not supported")
        symtab.add(symbols.GenericInterfaceSymbol(
            name, routines, visibility=symtab.default_visibility))

    def _identifier_handler(
        self, tsnode: 'TSNode'
    ) -> nodes.Reference:
        '''Translate an identifier into a symbol reference.

        :param tsnode: identifier tree-sitter node.

        :returns: PSyIR Reference to the resolved symbol.
        '''
        symtab = self._current_scope
        name = to_str(tsnode).lower()
        try:
            symbol = self._current_scope.lookup(name)
        except KeyError:
            symbol = symbols.DataSymbol(
                name, symbols.UnresolvedType(),
                interface=symbols.UnresolvedInterface())
            symtab.add(symbol)
        return nodes.Reference(symbol)

    def _parenthesized_expression_handler(
        self, tsnode: 'TSNode'
    ):
        '''Discard parentheses while preserving the enclosed expression.

        :param tsnode: parenthesized-expression tree-sitter node.

        :returns: translated expression inside the parentheses.

        '''
        content = [child for child in tsnode.children
                   if child.type not in ("(", ")")]
        return self._process_nodes(
            content[0], _NodeExpectation.EXPRESSION)

    def _operation(
        self, tsnode: 'TSNode'
    ):
        '''Translate a unary or binary operation node.

        :param tsnode: operation tree-sitter node.

        :returns: PSyIR UnaryOperation or BinaryOperation.

        :raises NotImplementedError: if the operator or tree shape is
            unsupported.
        '''
        if len(tsnode.children) == 2:
            operator = to_str(tsnode.children[0]).lower()
            if operator not in self._UNARY_OPERATORS:
                raise NotImplementedError(
                    f"Unsupported unary operator '{operator}'")
            return nodes.UnaryOperation.create(
                self._UNARY_OPERATORS[operator],
                self._process_nodes(
                    tsnode.children[1], _NodeExpectation.EXPRESSION))
        if len(tsnode.children) == 3:
            operator = to_str(tsnode.children[1]).lower()
            if operator not in self._BINARY_OPERATORS:
                raise NotImplementedError(
                    f"Unsupported binary operator '{operator}'")
            return nodes.BinaryOperation.create(
                self._BINARY_OPERATORS[operator],
                self._process_nodes(
                    tsnode.children[0], _NodeExpectation.EXPRESSION),
                self._process_nodes(
                    tsnode.children[2], _NodeExpectation.EXPRESSION))
        raise NotImplementedError("Unexpected operation structure")

    def _call_expression_handler(
        self, tsnode: 'TSNode'
    ):
        '''Translate an array reference, intrinsic, or function call.

        Tree-sitter uses the same syntactic node for all three forms. The
        symbol datatype resolves the ambiguity: a declared array produces an
        ArrayReference, a recognised intrinsic produces an IntrinsicCall, and
        other names produce a Call.

        :param tsnode: call-expression tree-sitter node.

        :returns: PSyIR ArrayReference, IntrinsicCall, StructureReference or
            Call.

        :raises NotImplementedError: if the expression cannot be classified
            or its argument form is unsupported.
        '''
        symtab = self._current_scope
        name_node = tsnode.children[0]
        if name_node.type == "derived_type_member_expression":
            return self._structure_reference(
                name_node,
                trailing_arguments=child_of_type(tsnode, "argument_list"))
        name = to_str(name_node).lower()
        argument_list = child_of_type(tsnode, "argument_list")
        try:
            symbol = self._current_scope.lookup(name)
        except KeyError:
            symbol = None

        if isinstance(symbol, symbols.DataSymbol) and isinstance(
                symbol.datatype, symbols.ArrayType):
            indices = self._arguments(
                argument_list, array_symbol=symbol)
            if any(isinstance(arg, tuple) for arg in indices):
                raise NotImplementedError(
                    "Named subscripts are not supported")
            return nodes.ArrayReference.create(symbol, indices)

        arguments = self._arguments(argument_list)

        # Explicit imports initially create a bare Symbol. Once it is used as
        # a function, specialise it in the same way as for a CALL statement.
        # pylint: disable=unidiomatic-typecheck
        if type(symbol) is symbols.Symbol:
            symbol.specialise(symbols.RoutineSymbol)

        # Resolve declared routines (including generic interfaces) before
        # considering intrinsic names since Fortran permits an intrinsic to
        # be shadowed by a user procedure.
        if isinstance(symbol, symbols.RoutineSymbol):
            return nodes.Call.create(symbol, arguments)

        intrinsic = next(
            (item for item in nodes.IntrinsicCall.Intrinsic
             if item.name.lower() == name), None)
        if intrinsic:
            try:
                return nodes.IntrinsicCall.create(intrinsic, arguments)
            except (TypeError, ValueError):
                # Preserve parsed intrinsics even if the current PSyIR
                # signature validation is stricter than the grammar.
                raise NotImplementedError(
                    f"Unsupported argument form for intrinsic '{name}'"
                ) from None

        if symbol is not None and not isinstance(
                symbol, symbols.DataTypeSymbol):
            raise NotImplementedError(
                f"'{name}(...)' cannot be classified as an array or call")
        symbol = symbols.RoutineSymbol(name)
        if name not in symtab:
            symtab.add(symbol)
        return nodes.Call.create(symbol, arguments)

    def _arguments(
        self, tsnode: Optional['TSNode'],
        array_symbol: Optional[symbols.DataSymbol] = None
    ) -> list:
        '''Translate an argument or array-subscript list.

        :param tsnode: argument-list tree-sitter node, or ``None``.
        :param array_symbol: array being indexed when ranges are permitted.

        :returns: positional expressions and ``(name, expression)`` tuples.

        :raises NotImplementedError: if a range occurs outside an array
            reference.
        '''
        if tsnode is None:
            return []
        result = []
        dimension = 0
        for child in tsnode.children:
            if child.type in ("(", ")", ","):
                continue
            dimension += 1
            if child.type == "keyword_argument":
                key = to_str(child.children[0])
                result.append((key, self._process_nodes(
                    child.children[-1], _NodeExpectation.EXPRESSION)))
            elif child.type == "extent_specifier":
                if array_symbol is None:
                    raise NotImplementedError(
                        "Ranges are only supported in array references")
                result.append(self._range(
                    child, array_symbol, dimension))
            else:
                result.append(self._process_nodes(
                    child, _NodeExpectation.EXPRESSION))
        return result

    def _range(
        self, tsnode: 'TSNode', symbol: symbols.DataSymbol, dimension: int
    ) -> nodes.Range:
        '''Translate an array-section triplet.

        Omitted bounds are made explicit with LBOUND and UBOUND calls because
        a PSyIR Range always has start, stop and step children.

        :param tsnode: extent-specifier tree-sitter node.
        :param symbol: DataSymbol for the indexed array.
        :param dimension: one-based array dimension being indexed.

        :returns: PSyIR Range with explicit bounds.

        :raises NotImplementedError: if the range is malformed.
        '''
        before, after, has_colon = self._split_extent(tsnode)
        if not has_colon:
            raise NotImplementedError("Malformed array range")
        # Preserve the field separated by a second colon. In particular, an
        # absent upper bound in ``lower::step`` must not cause the step to be
        # interpreted as the stop expression.
        second_colon = next(
            (idx for idx, child in enumerate(after) if child.type == ":"),
            None)
        if second_colon is None:
            upper = after
            step_nodes = []
        else:
            upper = after[:second_colon]
            step_nodes = after[second_colon + 1:]
        dim = nodes.Literal(str(dimension),
                            symbols.ScalarType.integer_type())
        start = (self._process_nodes(
            before[0], _NodeExpectation.EXPRESSION) if before else
                 nodes.IntrinsicCall.create(
                     nodes.IntrinsicCall.Intrinsic.LBOUND,
                     [nodes.Reference(symbol), ("dim", dim.copy())]))
        stop = (self._process_nodes(
            upper[0], _NodeExpectation.EXPRESSION) if upper else
                nodes.IntrinsicCall.create(
                    nodes.IntrinsicCall.Intrinsic.UBOUND,
                    [nodes.Reference(symbol), ("dim", dim.copy())]))
        step = (self._process_nodes(
            step_nodes[0], _NodeExpectation.EXPRESSION)
                if step_nodes else None)
        return nodes.Range.create(start, stop, step)

    def _derived_type_member_expression_handler(
        self, tsnode: 'TSNode'
    ):
        '''Translate a structure-component reference.

        :param tsnode: derived-type-member-expression tree-sitter node.

        :returns: PSyIR structure reference.
        '''
        return self._structure_reference(tsnode)

    def _structure_reference(
        self, tsnode: 'TSNode',
        trailing_arguments: Optional['TSNode'] = None
    ):
        '''Translate nested and indexed structure-component references.

        :param tsnode: tree-sitter node describing the structure access.
        :param trailing_arguments: optional indices attached outside
            ``tsnode`` by a wrapping call-expression node.

        :returns: StructureReference or ArrayOfStructuresReference.

        :raises NotImplementedError: if the access or base symbol is
            unsupported.
        '''
        symtab = self._current_scope
        name, indices, members = self._decompose_structure(tsnode)
        if trailing_arguments:
            arguments = self._arguments(trailing_arguments)
            if not members or any(isinstance(arg, tuple)
                                  for arg in arguments):
                raise NotImplementedError(
                    "Unsupported structure member array access")
            members[-1] = (members[-1], arguments)
        try:
            symbol = self._current_scope.lookup(name)
        except KeyError:
            symbol = symbols.DataSymbol(
                name, symbols.UnresolvedType(),
                interface=symbols.UnresolvedInterface())
            symtab.add(symbol)
        if not isinstance(symbol, symbols.DataSymbol):
            raise NotImplementedError(
                "A structure base must be a data symbol")
        if indices:
            return nodes.ArrayOfStructuresReference.create(
                symbol, indices, members)
        return nodes.StructureReference.create(symbol, members)

    def _decompose_structure(
        self, tsnode: 'TSNode'
    ) -> tuple[str, list, list]:
        '''Return base name, base indices and member descriptors.

        Tree-sitter nests call-expression and member-expression nodes for
        accesses such as ``items(i)%vector(2)%x``. Flattening that syntax into
        the descriptor format expected by PSyIR keeps node construction out of
        this recursive routine.

        :param tsnode: identifier, call or member-expression tree-sitter node.

        :returns: base name, base indices and PSyIR member descriptors.

        :raises NotImplementedError: if the access shape is unsupported.
        '''
        if tsnode.type == "identifier":
            return to_str(tsnode).lower(), [], []
        if tsnode.type == "call_expression":
            base = tsnode.children[0]
            name, indices, members = self._decompose_structure(base)
            arguments = self._arguments(
                child_of_type(tsnode, "argument_list"))
            if any(isinstance(arg, tuple) for arg in arguments):
                raise NotImplementedError(
                    "Named arguments in structure accesses are not supported")
            if members:
                members[-1] = (members[-1], arguments)
            else:
                indices = arguments
            return name, indices, members
        if tsnode.type == "derived_type_member_expression":
            name, indices, members = self._decompose_structure(
                tsnode.children[0])
            member = child_of_type(tsnode, "type_member")
            if member is None:
                raise NotImplementedError(
                    "Malformed structure component access")
            members.append(to_str(member).lower())
            return name, indices, members
        raise NotImplementedError(
            f"Unsupported structure access base '{tsnode.type}'")

    def _array_literal_handler(
        self, tsnode: 'TSNode'
    ):
        '''Translate a simple array constructor.

        :param tsnode: array-literal tree-sitter node.

        :returns: PSyIR ArrayConstructor.

        :raises NotImplementedError: for an implied-DO constructor.
        '''
        if child_of_type(tsnode, "implied_do_loop_expression"):
            raise NotImplementedError(
                "Array constructors with implied-DO loops are not supported")
        elems = [self._process_nodes(child, _NodeExpectation.EXPRESSION)
                 for child in tsnode.children
                 if child.type not in ("[", "]", "(/", "/)", ",")]
        return nodes.ArrayConstructor.create(elems)

    def _comment_handler(
        self, tsnode: 'TSNode'
    ) -> None:
        '''Ignore comments.

        :param tsnode: comment tree-sitter node.

        '''
        del tsnode

    def _assignment_statement_handler(
        self, tsnode: 'TSNode'
    ) -> nodes.Assignment:
        '''Translate an intrinsic assignment.

        :param tsnode: assignment-statement tree-sitter node.

        :returns: PSyIR Assignment.

        :raises NotImplementedError: if the tree shape is unexpected.
        '''
        if len(tsnode.children) != 3:
            raise NotImplementedError("Unexpected assignment structure")
        return nodes.Assignment.create(
            self._process_nodes(
                tsnode.children[0], _NodeExpectation.EXPRESSION),
            self._process_nodes(
                tsnode.children[2], _NodeExpectation.EXPRESSION))

    def _pointer_association_statement_handler(
        self, tsnode: 'TSNode'
    ) -> nodes.Assignment:
        '''Translate a simple pointer assignment.

        :param tsnode: pointer-association-statement tree-sitter node.

        :returns: pointer-annotated PSyIR Assignment.

        :raises NotImplementedError: for bounds remapping.
        '''
        if len(tsnode.children) != 3:
            raise NotImplementedError(
                "Pointer assignment with bounds remapping is not supported")
        assignment = nodes.Assignment(is_pointer=True)
        assignment.children = [
            self._process_nodes(
                tsnode.children[0], _NodeExpectation.EXPRESSION),
            self._process_nodes(
                tsnode.children[2], _NodeExpectation.EXPRESSION)]
        return assignment

    def _subroutine_call_handler(
        self, tsnode: 'TSNode'
    ):
        '''Translate a CALL statement.

        :param tsnode: subroutine-call tree-sitter node.

        :returns: PSyIR Call.

        :raises NotImplementedError: if the called object is unsupported.
        '''
        name_node = next((child for child in tsnode.children
                          if child.type == "identifier"), None)
        if not name_node:
            raise NotImplementedError(
                "Calls through type-bound procedures are not supported")
        symtab = self._current_scope
        name = to_str(name_node).lower()
        try:
            symbol = self._current_scope.lookup(name)
        except KeyError:
            symbol = symbols.RoutineSymbol(name, datatype=symbols.NoType())
            symtab.add(symbol)
        # As above, only a bare forward-reference Symbol may be specialised.
        # pylint: disable=unidiomatic-typecheck
        if type(symbol) is symbols.Symbol:
            symbol.specialise(symbols.RoutineSymbol,
                              datatype=symbols.NoType())
        if not isinstance(symbol, symbols.RoutineSymbol):
            raise NotImplementedError(
                f"Called object '{name}' is not a routine")
        args = self._arguments(child_of_type(tsnode, "argument_list"))
        return nodes.Call.create(symbol, args)

    def _keyword_statement_handler(
        self, tsnode: 'TSNode'
    ):
        '''Translate a no-argument keyword statement.

        :param tsnode: keyword-statement tree-sitter node.

        :returns: PSyIR Return for a RETURN statement.

        :raises NotImplementedError: if the keyword has no PSyIR node.
        '''
        keyword = tsnode.children[0].type
        if keyword == "return":
            return nodes.Return()
        raise NotImplementedError(
            f"Fortran '{keyword.upper()}' has no PSyIR node")

    def _if_statement_handler(
        self, tsnode: 'TSNode'
    ) -> nodes.IfBlock:
        '''Translate block and single-line IF statements.

        :param tsnode: if-statement tree-sitter node.

        :returns: root PSyIR IfBlock.

        :raises NotImplementedError: if the statement has no condition.
        '''
        condition_node = child_of_type(tsnode, "parenthesized_expression")
        if not condition_node:
            raise NotImplementedError("IF statement has no condition")
        structural = {
            "if", "parenthesized_expression", "then",
            "end_if_statement", "else_clause", "elseif_clause"
        }
        body_nodes = [child for child in tsnode.children
                      if child.type not in structural]
        else_clause = child_of_type(tsnode, "else_clause")
        else_ifs = list(iter_child_of_type(tsnode, "elseif_clause"))
        annotations = []
        if not child_of_type(tsnode, "end_if_statement"):
            annotations.append("was_single_stmt")
        if_body = self._process_nodes(body_nodes, _NodeExpectation.LIST)
        else_body = None
        if else_clause:
            else_body = self._process_nodes(
                [child for child in else_clause.children
                 if child.type != "else"], _NodeExpectation.LIST)
        for else_if in reversed(else_ifs):
            else_body = [self._if_clause(else_if, else_body)]
        result = nodes.IfBlock.create(
            self._process_nodes(condition_node, _NodeExpectation.EXPRESSION),
            if_body, else_body)
        result.annotations.extend(annotations)
        return result

    def _if_clause(
        self, tsnode: 'TSNode',
        final_else: Optional[list[nodes.Node]] = None
    ) -> nodes.IfBlock:
        '''Translate an ELSE IF clause recursively.

        :param tsnode: elseif-clause tree-sitter node.
        :param final_else: PSyIR statements for a following ELSE clause.

        :returns: annotated PSyIR IfBlock.
        '''
        condition = child_of_type(tsnode, "parenthesized_expression")
        structural = {"else", "if", "parenthesized_expression", "then",
                      "else_clause", "elseif_clause"}
        body = self._process_nodes(
            [child for child in tsnode.children
             if child.type not in structural], _NodeExpectation.LIST)
        trailing = child_of_type(tsnode, "elseif_clause")
        otherwise = (
            [self._if_clause(trailing, final_else)] if trailing else
            self._process_nodes(
                [child for child in
                 (child_of_type(tsnode, "else_clause").children
                  if child_of_type(tsnode, "else_clause") else [])
                 if child.type != "else"], _NodeExpectation.LIST)
            or final_else)
        result = nodes.IfBlock.create(
            self._process_nodes(condition, _NodeExpectation.EXPRESSION),
            body, otherwise)
        result.annotations.append("was_elseif")
        return result

    def _do_loop_handler(
        self, tsnode: 'TSNode'
    ):
        '''Translate counted, conditional and unconditional DO loops.

        :param tsnode: do-loop tree-sitter node.

        :returns: PSyIR Loop for counted DO, otherwise WhileLoop.

        :raises NotImplementedError: if counted-loop control is unsupported.
        '''
        statement = child_of_type(tsnode, "do_statement")
        control = child_of_type(statement, "loop_control_expression")
        while_node = child_of_type(statement, "while_statement")
        body = self._process_nodes(
            [child for child in tsnode.children
             if child.type not in ("do_statement",
                                   "end_do_loop_statement")],
            _NodeExpectation.LIST)
        if control:
            parts = [child for child in control.children
                     if child.type not in ("=", ",")]
            if len(parts) not in (3, 4):
                raise NotImplementedError(
                    "Unsupported counted DO loop control")
            variable_ref = self._identifier_handler(parts[0])
            variable = variable_ref.symbol
            if not isinstance(variable, symbols.DataSymbol):
                raise NotImplementedError(
                    "A DO variable must be a data symbol")
            if not isinstance(variable.datatype, symbols.ScalarType):
                if isinstance(variable.datatype, symbols.UnresolvedType):
                    variable.datatype = symbols.ScalarType.integer_type()
                else:
                    raise NotImplementedError(
                        "A DO variable must be a scalar integer")
            step = (self._process_nodes(
                parts[3], _NodeExpectation.EXPRESSION) if len(parts) == 4
                    else nodes.Literal(
                        "1", symbols.ScalarType.integer_type()))
            return nodes.Loop.create(
                variable,
                self._process_nodes(parts[1], _NodeExpectation.EXPRESSION),
                self._process_nodes(parts[2], _NodeExpectation.EXPRESSION),
                step, body)
        if while_node:
            condition = child_of_type(
                while_node, "parenthesized_expression")
            return nodes.WhileLoop.create(
                self._process_nodes(condition, _NodeExpectation.EXPRESSION),
                body)
        result = nodes.WhileLoop.create(
            nodes.Literal("true", symbols.ScalarType.boolean_type()), body)
        result.annotations.append("was_unconditional")
        return result

    def _where_statement_handler(
        self, tsnode: 'TSNode'
    ) -> nodes.IfBlock:
        '''Translate block and single-statement WHERE constructs.

        :param tsnode: where-statement tree-sitter node.

        :returns: annotated PSyIR IfBlock.
        '''
        condition = child_of_type(tsnode, "parenthesized_expression")
        structural = {"where", "parenthesized_expression",
                      "elsewhere_clause", "end_where_statement"}
        body = self._process_nodes(
            [child for child in tsnode.children
             if child.type not in structural], _NodeExpectation.LIST)
        elsewhere_clauses = list(iter_child_of_type(
            tsnode, "elsewhere_clause"))
        other = None
        # Masked ELSEWHERE clauses have ELSE-IF semantics and are represented
        # by nested IfBlocks. Constructing the chain backwards makes the
        # following clause the else-body of the current masked clause.
        for elsewhere in reversed(elsewhere_clauses):
            mask = child_of_type(
                elsewhere, "parenthesized_expression")
            clause_body = self._process_nodes(
                [child for child in elsewhere.children
                 if child.type not in
                 ("elsewhere", "parenthesized_expression")],
                _NodeExpectation.LIST)
            if mask:
                nested = nodes.IfBlock.create(
                    self._process_nodes(
                        mask, _NodeExpectation.EXPRESSION),
                    clause_body, other)
                nested.annotations.append("was_where")
                other = [nested]
            else:
                if other is not None:
                    raise NotImplementedError(
                        "An unmasked ELSEWHERE must be the final clause")
                other = clause_body
        result = nodes.IfBlock.create(
            self._process_nodes(condition, _NodeExpectation.EXPRESSION),
            body, other)
        result.annotations.extend(
            ["was_where"] if child_of_type(
                tsnode, "end_where_statement") else
            ["was_where", "was_single_stmt"])
        return result

    def _select_case_statement_handler(
        self, tsnode: 'TSNode'
    ):
        '''Translate SELECT CASE into a nested annotated IF tree.

        Each non-default CASE becomes an IfBlock in the preceding block's
        else-body. CASE DEFAULT becomes the final else-body.

        :param tsnode: select-case-statement tree-sitter node.

        :returns: root annotated PSyIR IfBlock.

        :raises NotImplementedError: if no conditional CASE can be produced.
        '''
        selector_node = child_of_type(
            child_of_type(tsnode, "selector"), "identifier")
        if selector_node is None:
            selector = self._process_nodes(
                [child for child in
                 child_of_type(tsnode, "selector").children
                 if child.type not in ("(", ")")][0],
                _NodeExpectation.EXPRESSION)
        else:
            selector = self._process_nodes(
                selector_node, _NodeExpectation.EXPRESSION)
        cases = list(iter_child_of_type(tsnode, "case_statement"))
        default_body = None
        normal = []
        for case in cases:
            if child_of_type(case, "default"):
                default_body = self._process_nodes(
                    [child for child in case.children
                     if child.type not in ("case", "default")],
                    _NodeExpectation.LIST)
            else:
                values = child_of_type(case, "case_value_range_list")
                if values is None:
                    raise NotImplementedError(
                        "Malformed CASE value list")
                structural = {"case", "(", ")", "case_value_range_list"}
                body = self._process_nodes(
                    [child for child in case.children
                     if child.type not in structural], _NodeExpectation.LIST)
                normal.append((values, body))
        current = default_body
        for values, body in reversed(normal):
            condition = self._case_condition(selector, values)
            block = nodes.IfBlock.create(
                condition, body, current)
            block.annotations.append("was_case")
            current = [block]
        if normal and current and len(current) == 1:
            return current[0]
        raise NotImplementedError(
            "SELECT CASE with only a default clause has no PSyIR equivalent")

    def _case_condition(
        self, selector, values: 'TSNode'
    ):
        '''Build a condition for one CASE value/range list.

        :param selector: translated SELECT CASE selector expression.
        :param values: case-value-range-list tree-sitter node.

        :returns: PSyIR condition combining values with OR and range bounds
            with AND.
        '''
        conditions = []
        for child in values.children:
            if child.type == ",":
                continue
            if child.type == "extent_specifier":
                before, after, _ = self._split_extent(child)
                parts = []
                if before:
                    parts.append(nodes.BinaryOperation.create(
                        nodes.BinaryOperation.Operator.GE, selector.copy(),
                        self._process_nodes(
                            before[0], _NodeExpectation.EXPRESSION)))
                if after:
                    parts.append(nodes.BinaryOperation.create(
                        nodes.BinaryOperation.Operator.LE, selector.copy(),
                        self._process_nodes(
                            after[0], _NodeExpectation.EXPRESSION)))
                condition = parts[0] if len(parts) == 1 else (
                    nodes.BinaryOperation.create(
                        nodes.BinaryOperation.Operator.AND,
                        parts[0], parts[1]))
            else:
                condition = nodes.BinaryOperation.create(
                    nodes.BinaryOperation.Operator.EQ, selector.copy(),
                    self._process_nodes(
                        child, _NodeExpectation.EXPRESSION))
            conditions.append(condition)
        result = conditions[0]
        for condition in conditions[1:]:
            result = nodes.BinaryOperation.create(
                nodes.BinaryOperation.Operator.OR, result, condition)
        return result

    def _memory_statement(
        self, tsnode: 'TSNode'
    ):
        '''Translate operands of allocation-related statements.

        ALLOCATE shape specifications become ArrayReference indices whose
        Range children retain the requested lower and upper bounds.

        :param tsnode: allocation-related statement tree-sitter node.
        :returns: PSyIR IntrinsicCall.

        :raises NotImplementedError: if an object, bound or option is
            unsupported.
        '''
        intrinsic = {
            "allocate_statement": nodes.IntrinsicCall.Intrinsic.ALLOCATE,
            "deallocate_statement": nodes.IntrinsicCall.Intrinsic.DEALLOCATE,
            "nullify_statement": nodes.IntrinsicCall.Intrinsic.NULLIFY,
        }[tsnode.type]
        args = []
        for child in tsnode.children:
            if child.type in (
                    "allocate", "deallocate", "nullify", "(", ")", ","):
                continue
            if child.type == "keyword_argument":
                args.append((to_str(child.children[0]),
                             self._process_nodes(
                                 child.children[-1],
                                 _NodeExpectation.EXPRESSION)))
            elif child.type == "sized_allocation":
                args.append(self._allocation_reference(child))
            else:
                args.append(self._process_nodes(
                    child, _NodeExpectation.EXPRESSION))
        try:
            return nodes.IntrinsicCall.create(intrinsic, args)
        except (TypeError, ValueError):
            raise NotImplementedError(
                f"Unsupported operands for {intrinsic.name}") from None

    def _allocation_reference(
        self, tsnode: 'TSNode'
    ) -> nodes.ArrayReference:
        '''Translate an ALLOCATE object with explicit shape bounds.

        PSyIR represents the requested allocation shape as ArrayReference
        Range indices. This is an allocation request rather than an ordinary
        array access, but the representation permits the backend to reproduce
        the original bounds.

        :param tsnode: sized-allocation tree-sitter node.

        :returns: ArrayReference containing one Range per allocated dimension.

        :raises NotImplementedError: if the object is not a data symbol.
        '''
        ident = child_of_type(tsnode, "identifier")
        if ident is None:
            raise NotImplementedError(
                "Allocations of structure components are not supported")
        reference = self._identifier_handler(ident)
        if not isinstance(reference.symbol, symbols.DataSymbol):
            raise NotImplementedError(
                "An ALLOCATE object must be a data symbol")
        size = child_of_type(tsnode, "size")
        indices = [
            self._allocation_extent(extent)
            for extent in size.children
            if extent.type not in ("(", ")", ",")]
        return nodes.ArrayReference.create(reference.symbol, indices)

    def _allocation_extent(
        self, tsnode: 'TSNode'
    ) -> nodes.Range:
        '''Translate one requested allocation extent.

        Fortran's ``allocate(a(n))`` is equivalent to ``allocate(a(1:n))``;
        the implicit lower bound is therefore made explicit in PSyIR.

        :param tsnode: extent or extent-specifier tree-sitter node.

        :returns: Range containing explicit lower and upper bounds.

        :raises NotImplementedError: if the allocation bound is malformed.
        '''
        lower = nodes.Literal(
            "1", symbols.ScalarType.integer_type())
        if tsnode.type != "extent_specifier":
            return nodes.Range.create(
                lower, self._process_nodes(
                    tsnode, _NodeExpectation.EXPRESSION))

        before, after, has_colon = self._split_extent(tsnode)
        if not has_colon:
            raise NotImplementedError("Malformed allocation bound")
        if before:
            lower = self._process_nodes(
                before[0], _NodeExpectation.EXPRESSION)
        if not after:
            raise NotImplementedError(
                "Allocation upper bound is required")
        return nodes.Range.create(
            lower, self._process_nodes(
                after[0], _NodeExpectation.EXPRESSION))
