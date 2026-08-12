# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2025-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' PSyIR TreeSitter Fortran reader '''

import codecs
from contextlib import contextmanager
from dataclasses import dataclass
import logging
from typing import Callable, Iterable, Optional, TYPE_CHECKING, Union
from collections.abc import Generator, Container

from psyclone.psyir import nodes, symbols
from psyclone.psyir.nodes.codeblock import TreeSitterCodeBlock, CodeBlock

if TYPE_CHECKING:
    # Purposely inside typechecking because at runtime we want to lazily
    # import the parser (only if it is actually used)
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


def direct_child_of_type(
    tsnode: Optional['TSNode'], types: str | Container[str]
) -> Generator['TSNode']:
    '''Return the first direct child having the supplied type.

    :param tsnode: tree-sitter node whose children are searched.
    :param node_type: tree-sitter type to find.

    :returns: matching child, or ``None`` if no child matches.
    '''
    check_types = (types,) if isinstance(types, str) else types
    if tsnode:
        for child in tsnode.children:
            if child.type in check_types:
                yield child


def next_of_type(
    tsnode: Optional['TSNode'], node_type: str
) -> Optional['TSNode']:
    '''Return the first direct child having the supplied type.

    :param tsnode: tree-sitter node whose children are searched.
    :param node_type: tree-sitter type to find.

    :returns: matching child, or ``None`` if no child matches.
    '''
    return next(direct_child_of_type(tsnode, node_type), None)


@dataclass(frozen=True)
class _SharedDeclAttributes:
    ''' Properties shared by all entities of a fortran declaration (the lhs
    of ::)

    :param base_type: common PSyIR datatype, or ``None`` if unsupported.
    :param dimension: common DIMENSION argument list, if present.
    :param intent: common INTENT qualifier, if present.
    :param qualifiers: names of all declaration qualifiers.
    :param unsupported: qualifiers not represented directly in PSyIR.
    :param prefix: declaration text preceding ``::``.
    '''

    base_type: object
    dimension: Optional['TSNode']
    intent: Optional['TSNode']
    qualifiers: frozenset[str]
    unsupported: frozenset[str]
    prefix: str


class FortranTreeSitterReader():
    '''
    Processes the TreeSitter parse_tree and converts it to PSyIR nodes.
    Unsupported declarations retain their source in UnsupportedFortranType
    while unsupported executable statements become TreeSitterCodeBlocks.

    The structure of the expected fortran parse tree can be found in the
    'rules' section of:
    https://github.com/stadelmanma/tree-sitter-fortran/blob/master/grammar.js
    To interpret the rules use:
    https://tree-sitter.github.io/tree-sitter/creating-parsers/
    2-the-grammar-dsl.html

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

    # These nodes belong to a Fortran specification part and update a symbol
    # table rather than producing executable PSyIR children.
    _SPECIFICATION_TYPES = {
        "use_statement", "variable_declaration", "derived_type_definition",
        "interface"
    }

    # Punctuation and grammar-only nodes are listed explicitly at each scope
    # boundary. This makes it clear which tree-sitter children are consumed by
    # the scope handler and prevents them from becoming accidental CodeBlocks.
    _MODULE_NON_EXECUTABLE_TYPES = _SPECIFICATION_TYPES.union({
        "module_statement", "end_module_statement", "implicit_statement",
        "internal_procedures", "public_statement", "private_statement"
    })

    # Centralising these maps documents the supported Fortran spellings and
    # avoids recreating identical dictionaries for every parsed operation.
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

    # Some tree-sitter node types share a handler.
    _HANDLER_REDIRECTIONS = {
        "subroutine": "_routine_handler",
        "function": "_routine_handler",
        "program": "_routine_handler",
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
        '''Create a Fortran tree-sitter reader.

        :param ignore_directives: whether directives are ignored.
        :param last_comments_as_codeblocks: whether trailing comments in a
            block are retained as CodeBlocks.
        :param resolve_modules: whether imported modules are resolved.
        :param ignore_comments: whether comments are ignored.
        :param free_form: whether source is parsed as free-form Fortran.
        :param conditional_openmp: whether conditional OpenMP statements are
            parsed.
        '''
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
        return self._process_nodes(parse_tree)[0]

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
                raise ValueError("The supplied scope must be an orphan")
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
    ) -> list[nodes.Node]:
        '''
        This is the tsnodes handler dispatcher. Unsupported syntax is
        deliberately caught here rather than in individual handlers so that
        continuous unsupported nodes are placed in a single CodeBlock.

        :param tsnodes: one tree-sitter node or an iterable of nodes.

        :returns: PSyIR nodes produced from the supplied tree-sitter nodes.
        :rtype: list[:py:class:`psyclone.psyir.nodes.Node`]
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
                # TODO #3038: Add support for expression codeblocks and
                # aggregating contiguous codeblocks into a single one.
                children.append(self._create_codeblock(tsnode, str(err)))
        return children

    @staticmethod
    def _create_codeblock(
        tsnode: 'TSNode', reason: str
    ) -> TreeSitterCodeBlock:
        '''Create a statement CodeBlock for unsupported valid Fortran.

        :param tsnode: tree-sitter node containing unsupported Fortran.
        :param reason: human-readable explanation of the limitation.

        :returns: CodeBlock retaining the original tree-sitter node.
        '''
        code_block = TreeSitterCodeBlock(
            tsnode, CodeBlock.Structure.STATEMENT)
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
                self._process_nodes(tsnode.children)
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
        statement = next_of_type(tsnode, "module_statement")
        name = next_of_type(statement, "name")
        container = nodes.Container(to_str(name) if name else "")

        with self._using_scope(container.symbol_table):
            visibility_map = self._process_access_statements(tsnode.children)
            self._process_nodes(
                direct_child_of_type(tsnode, self._SPECIFICATION_TYPES))

            internal = next_of_type(tsnode, "internal_procedures")
            if internal:
                container.children.extend(
                    self._process_nodes(
                        [child for child in internal.children
                         if child.type != "contains_statement"]))

            container.children.extend(self._process_nodes(
                [child for child in tsnode.children
                 if child.type not in self._MODULE_NON_EXECUTABLE_TYPES]))
            self._apply_visibility(visibility_map)
        return container

    def _routine_handler(
        self, tsnode: 'TSNode'
    ) -> nodes.Routine:
        '''Create PSyIR shared by programs, subroutines and functions.

        :param tsnode: tree-sitter node for the complete program unit.
        :returns: translated PSyIR Routine.
        '''
        routine_kind = tsnode.type
        parent_symtab = self._current_scope
        if parent_symtab is None:
            raise RuntimeError(
                "A Routine must be translated within a current scope")
        statement = next_of_type(tsnode, f"{routine_kind}_statement")
        name_node = next_of_type(statement, "name")
        name = to_str(name_node) if name_node else routine_kind
        parameters = next_of_type(statement, "parameters")
        argument_names = tuple(
            to_str(child) for child in parameters.children
            if child.type == "identifier") if parameters else ()
        return_name, return_type = self._function_return_info(
            statement, name, routine_kind)

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
            name, statement, return_type)
        routine = nodes.Routine(
            rsymbol, is_program=routine_kind == "program",
            symbol_table=routine_table)

        parent = parent_symtab.node
        if not isinstance(parent, nodes.ScopingNode):
            raise RuntimeError(
                "A Routine must be translated within a PSyIR scope")
        with self._using_temporary_scope(parent, routine):
            visibility_map = self._process_access_statements(
                tsnode.children)
            self._process_nodes(
                child for child in tsnode.children
                if child.type in self._SPECIFICATION_TYPES)

            args = [routine.symbol_table.lookup(name)
                    for name in argument_names]
            routine.symbol_table.specify_argument_list(args)
            if return_name:
                routine.return_symbol = routine.symbol_table.lookup(
                    return_name)

            specification = {
                f"{routine_kind}_statement",
                f"end_{routine_kind}_statement",
                "implicit_statement", "public_statement",
                "private_statement"
            }
            specification.update(self._SPECIFICATION_TYPES)
            routine.children.extend(self._process_nodes(
                [child for child in tsnode.children
                 if child.type not in specification]))
            self._apply_visibility(visibility_map)
        return routine

    def _function_return_info(
        self, statement: 'TSNode', routine_name: str, routine_kind: str
    ) -> tuple[Optional[str], Optional[symbols.DataType]]:
        '''Extract result name and datatype from a function statement.

        :param statement: opening program-unit statement.
        :param routine_name: name of the program unit.
        :param routine_kind: one of ``program``, ``subroutine`` or
            ``function``.

        :returns: result-symbol name and datatype, both ``None`` for a
            non-function.
        '''
        if routine_kind != "function":
            return None, None

        result = next_of_type(statement, "function_result")
        result_name = next_of_type(result, "identifier")
        return_name = to_str(result_name) if result_name else routine_name
        type_node = next(
            (child for child in statement.children
             if child.type in ("intrinsic_type", "derived_type")), None)
        if not type_node:
            return return_name, None
        try:
            return return_name, self._datatype_from_type(type_node)
        except (NotImplementedError, KeyError, TypeError):
            return return_name, symbols.UnsupportedFortranType(
                to_str(statement).strip())

    def _create_routine_symbol(
        self, name: str, statement: 'TSNode', return_type
    ) -> symbols.RoutineSymbol:
        '''Create or complete the RoutineSymbol for a program unit.

        An interface block may create the symbol before its implementation is
        visited. Reusing it ensures interface members and the Routine node
        refer to the same object.

        :param name: routine name.
        :param statement: opening program-unit statement.
        :param return_type: translated function return type, if any.

        :returns: RoutineSymbol representing the program unit.
        '''
        parent_symtab = self._current_scope
        if parent_symtab is None:
            raise RuntimeError(
                "A RoutineSymbol must be created within a current scope")
        qualifiers = {
            to_str(child).lower() for child in statement.children
            if child.type == "procedure_qualifier"}
        visibility = parent_symtab.default_visibility
        try:
            routine_symbol = parent_symtab.lookup(name)
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
        datatype = (symbols.ScalarType.real_type()
                    if any(char in value for char in ".ed")
                    else symbols.ScalarType.integer_type())
        if kind:
            if kind == "4":
                precision = symbols.ScalarType.Precision.SINGLE
            elif kind == "8":
                precision = symbols.ScalarType.Precision.DOUBLE
            else:
                precision = (int(kind) if kind.isdigit()
                             else nodes.Reference(
                                 self._kind_symbol(kind)))
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
        return nodes.Literal(text[1:-1].replace(text[0] * 2, text[0]),
                             symbols.ScalarType.character_type())

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

        # qualifiers = direct_child_of_type(tsnode, "type_qualifier")
        qualifiers = [child for child in tsnode.children
                      if child.type == "type_qualifier"]
        qualifier_names = frozenset(
            child.children[0].type
            for child in qualifiers if child.children)
        unsupported = qualifier_names.intersection({
            "pointer", "target", "optional", "value", "volatile",
            "asynchronous", "contiguous"
        })
        try:
            base_type = self._datatype_from_type(type_node)
        except (NotImplementedError, KeyError, TypeError):
            base_type = None

        dimension = next(
            (next_of_type(item, "argument_list") for item in qualifiers
             if item.children and item.children[0].type == "dimension"), None)
        intent = next(
            (item for item in qualifiers
             if item.children and item.children[0].type == "intent"), None)
        common_attr = _SharedDeclAttributes(
            base_type, dimension, intent, qualifier_names,
            frozenset(unsupported),
            to_str(tsnode).split("::", maxsplit=1)[0].strip())

        for declarator in tsnode.children:
            if declarator.type in (
                    "identifier", "sized_declarator", "init_declarator"):
                self._declare_entity(declarator, common_attr)

    def _declare_entity(
        self, declarator: 'TSNode', common_attr: _SharedDeclAttributes
    ):
        '''Translate one entity and add it to the current symbol table.

        :param declarator: identifier or entity-declarator tree-sitter node.
        :param common_attr: properties shared by the complete declaration.
        '''
        id_node = (declarator if declarator.type == "identifier"
                   else next_of_type(declarator, "identifier"))
        name = to_str(id_node)
        datatype, initial_value = self._declarator_datatype(
            declarator, common_attr)

        visibility = self._current_scope.default_visibility
        if "public" in common_attr.qualifiers:
            visibility = symbols.Symbol.Visibility.PUBLIC
        elif "private" in common_attr.qualifiers:
            visibility = symbols.Symbol.Visibility.PRIVATE

        kwargs = {"visibility": visibility}
        interface = self._declaration_interface(name, common_attr)
        if interface is not None:
            kwargs["interface"] = interface
        if initial_value is not None:
            kwargs["initial_value"] = initial_value
        if "parameter" in common_attr.qualifiers:
            kwargs["is_constant"] = True
        declared_symbol = symbols.DataSymbol(name, datatype, **kwargs)
        self._add_or_update_datasymbol(declared_symbol)

    def _declarator_datatype(
        self, declarator: 'TSNode', common_attr: _SharedDeclAttributes
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
                    initial_value = self._expression(expressions[-1])
                except NotImplementedError:
                    is_unsupported = True

        # Array shape and initialisation belong to an individual entity, so
        # derive a fresh datatype from the common base type.
        datatype = common_attr.base_type
        shape_node = next_of_type(declarator, "size") or common_attr.dimension
        is_allocatable = "allocatable" in common_attr.qualifiers
        if datatype and shape_node:
            try:
                shape = self._shape_from_node(
                    shape_node, is_allocatable)
                datatype = symbols.ArrayType(datatype, shape)
            except (NotImplementedError, TypeError):
                datatype = None
        elif is_allocatable:
            datatype = None

        # UnsupportedFortranType is preferable to losing a declaration.
        # Keep only this entity in the saved text to avoid duplicating sibling
        # names when the Fortran backend emits the symbols.
        if datatype is None or is_unsupported:
            entity = to_str(declarator).strip()
            datatype = symbols.UnsupportedFortranType(
                f"{common_attr.prefix} :: {entity}")
        return datatype, initial_value

    def _declaration_interface(
        self, name: str, common_attr: _SharedDeclAttributes
    ):
        '''Return the PSyIR interface for one declared entity.

        Interfaces describe where a symbol is defined and how a dummy argument
        may be accessed. They are independent of its datatype.

        :param name: declared entity name.
        :param common_attr: properties shared by the complete declaration.

        :returns: symbol interface, or ``None`` for an automatic local.
        '''
        symtab = self._current_scope
        if name in symtab and symtab.lookup(name).is_argument:
            access = symbols.ArgumentInterface.Access.UNKNOWN
            if common_attr.intent:
                access = next(
                    (self._INTENT_ACCESS[child.type]
                     for child in common_attr.intent.children
                     if child.type in self._INTENT_ACCESS),
                    access)
            return symbols.ArgumentInterface(access)
        if {"save", "parameter"}.intersection(common_attr.qualifiers):
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

        :raises NotImplementedError: if an existing symbol is not a
            DataSymbol.
        '''
        symtab = self._current_scope
        if declared_symbol.name in symtab:
            symbol = symtab.lookup(declared_symbol.name)
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
                symbol.initial_value = declared_symbol.initial_value
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
            name_node = next_of_type(tsnode, "type_name")
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
        kind_node = next_of_type(tsnode, "kind")
        if kind_node:
            values = [child for child in kind_node.children
                      if child.type not in ("(", ")")]
            if values:
                value = values[0]
                if value.type == "keyword_argument":
                    key = to_str(value.children[0]).lower()
                    value = value.children[-1]
                    if key == "len":
                        length = self._expression(value)
                    else:
                        precision = self._precision(value)
                elif intrinsic == "character":
                    length = self._expression(value)
                else:
                    precision = self._precision(value)
        return symbols.ScalarType(mapping[intrinsic], precision, length)

    def _precision(self, tsnode: 'TSNode'):
        '''Translate a kind expression into a ScalarType precision value.

        :param tsnode: tree-sitter node containing the kind expression.

        :returns: precision enumeration, integer or PSyIR Reference.

        :raises NotImplementedError: if the kind expression is unsupported.
        '''
        expr = self._expression(tsnode)
        if isinstance(expr, nodes.Literal) and expr.value.isdigit():
            if expr.value == "4":
                return symbols.ScalarType.Precision.SINGLE
            if expr.value == "8":
                return symbols.ScalarType.Precision.DOUBLE
            return int(expr.value)
        if isinstance(expr, nodes.Reference):
            # A bare Symbol is a forward reference created before its role was
            # known. Exact type checking is intentional: specialised Symbol
            # subclasses must not be changed into a DataSymbol.
            # pylint: disable=unidiomatic-typecheck
            if type(expr.symbol) is symbols.Symbol:
                expr.symbol.specialise(
                    symbols.DataSymbol,
                    datatype=symbols.ScalarType.integer_type())
            return expr
        raise NotImplementedError("A kind must be a literal or named symbol")

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
        if not isinstance(symbol, symbols.DataSymbol):
            raise NotImplementedError(
                f"Kind parameter '{name}' is not a data symbol")
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
                    result.append(self._expression(before[0]))
                    continue
                if not before and not after:
                    result.append(symbols.ArrayType.Extent.DEFERRED
                                  if is_allocatable else
                                  symbols.ArrayType.Extent.ATTRIBUTE)
                elif before and not after:
                    result.append(
                        (self._expression(before[0]),
                         symbols.ArrayType.Extent.ATTRIBUTE))
                elif after and not before:
                    result.append(self._expression(after[0]))
                else:
                    result.append((self._expression(before[0]),
                                   self._expression(after[0])))
            else:
                result.append(self._expression(child))
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

        :raises NotImplementedError: if the module name conflicts with an
            existing non-container symbol.
        '''
        symtab = self._current_scope
        module_node = next_of_type(tsnode, "module_name")
        module_name = to_str(module_node)
        intrinsic = any(child.type == "intrinsic" for child in tsnode.children)
        included = next_of_type(tsnode, "included_items")
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
            raise NotImplementedError(
                f"USE module '{module_name}' conflicts with another symbol")
        container.wildcard_import = wildcard

        if included:
            for child in included.children:
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
                            to_str(names[0]), to_str(names[1]),
                            container)

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
        statement = next_of_type(tsnode, "derived_type_statement")
        name_node = next_of_type(statement, "type_name")
        name = to_str(name_node)
        unsupported = any(child.type == "derived_type_procedures"
                          for child in tsnode.children)
        datatype = None
        if not unsupported:
            parent = symtab.node
            if not isinstance(parent, nodes.ScopingNode):
                raise RuntimeError(
                    "A derived type must be translated within a PSyIR scope")
            with self._using_temporary_scope(parent):
                visibility_map = self._process_access_statements(
                    tsnode.children)
                try:
                    for declaration in direct_child_of_type(
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
        access = next_of_type(statement, "access_specifier")
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
        statement = next_of_type(tsnode, "interface_statement")
        name_node = next_of_type(statement, "name")
        if not name_node:
            raise NotImplementedError(
                "Abstract and operator interfaces are not supported")
        name = to_str(name_node)
        routines = []
        for procedure in direct_child_of_type(tsnode, "procedure_statement"):
            from_container = "module" in [
                child.type for child in procedure.children[0].children]
            for method in direct_child_of_type(procedure, "method_name"):
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

        :raises NotImplementedError: if the parse-tree shape is unexpected.
        '''
        content = [child for child in tsnode.children
                   if child.type not in ("(", ")")]
        if len(content) != 1:
            raise NotImplementedError(
                "Unexpected parenthesized expression structure")
        return self._expression(content[0])

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
                self._expression(tsnode.children[1]))
        if len(tsnode.children) == 3:
            operator = to_str(tsnode.children[1]).lower()
            if operator not in self._BINARY_OPERATORS:
                raise NotImplementedError(
                    f"Unsupported binary operator '{operator}'")
            return nodes.BinaryOperation.create(
                self._BINARY_OPERATORS[operator],
                self._expression(tsnode.children[0]),
                self._expression(tsnode.children[2]))
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
                trailing_arguments=next_of_type(tsnode, "argument_list"))
        name = to_str(name_node).lower()
        argument_list = next_of_type(tsnode, "argument_list")
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

        if not isinstance(symbol, symbols.RoutineSymbol):
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
                result.append((key, self._expression(child.children[-1])))
            elif child.type == "extent_specifier":
                if array_symbol is None:
                    raise NotImplementedError(
                        "Ranges are only supported in array references")
                result.append(self._range(
                    child, array_symbol, dimension))
            else:
                result.append(self._expression(child))
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
        # A section can have a second colon before its step.
        after = [child for child in after if child.type != ":"]
        dim = nodes.Literal(str(dimension),
                            symbols.ScalarType.integer_type())
        start = (self._expression(before[0]) if before else
                 nodes.IntrinsicCall.create(
                     nodes.IntrinsicCall.Intrinsic.LBOUND,
                     [nodes.Reference(symbol), ("dim", dim.copy())]))
        stop = (self._expression(after[0]) if after else
                nodes.IntrinsicCall.create(
                    nodes.IntrinsicCall.Intrinsic.UBOUND,
                    [nodes.Reference(symbol), ("dim", dim.copy())]))
        step = (self._expression(after[1])
                if len(after) > 1 else None)
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
                next_of_type(tsnode, "argument_list"))
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
            member = next_of_type(tsnode, "type_member")
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
        if next_of_type(tsnode, "implied_do_loop_expression"):
            raise NotImplementedError(
                "Array constructors with implied-DO loops are not supported")
        elems = [self._expression(child) for child in tsnode.children
                 if child.type not in ("[", "]", "(/", "/)", ",")]
        return nodes.ArrayConstructor.create(elems)

    def _expression(self, tsnode: 'TSNode'):
        '''Translate one expression, allowing failures to reach its
        statement.

        :param tsnode: expression tree-sitter node.

        :returns: translated PSyIR DataNode.
        '''
        return self._get_handler(tsnode)(tsnode)

    def _comment_handler(
        self, tsnode: 'TSNode'
    ) -> None:
        '''Ignore comments when requested.

        Comment attachment will be added when the reader options cease to be
        compatibility-only. Until then, comments must not turn otherwise
        supported source into CodeBlocks.

        :param tsnode: comment tree-sitter node.

        :raises NotImplementedError: if comment preservation was requested.
        '''
        del tsnode
        if self._ignore_comments:
            return None
        raise NotImplementedError("Comment preservation is not yet supported")

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
            self._expression(tsnode.children[0]),
            self._expression(tsnode.children[2]))

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
            self._expression(tsnode.children[0]),
            self._expression(tsnode.children[2])]
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
        args = self._arguments(next_of_type(tsnode, "argument_list"))
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
        condition_node = next_of_type(tsnode, "parenthesized_expression")
        if not condition_node:
            raise NotImplementedError("IF statement has no condition")
        structural = {
            "if", "parenthesized_expression", "then",
            "end_if_statement", "else_clause", "elseif_clause"
        }
        body_nodes = [child for child in tsnode.children
                      if child.type not in structural]
        else_clause = next_of_type(tsnode, "else_clause")
        else_ifs = list(direct_child_of_type(tsnode, "elseif_clause"))
        annotations = []
        if not next_of_type(tsnode, "end_if_statement"):
            annotations.append("was_single_stmt")
        if_body = self._process_nodes(body_nodes)
        else_body = None
        if else_clause:
            else_body = self._process_nodes(
                [child for child in else_clause.children
                 if child.type != "else"])
        for else_if in reversed(else_ifs):
            else_body = [self._if_clause(else_if, else_body)]
        result = nodes.IfBlock.create(
            self._expression(condition_node), if_body, else_body)
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
        condition = next_of_type(tsnode, "parenthesized_expression")
        structural = {"else", "if", "parenthesized_expression", "then",
                      "else_clause", "elseif_clause"}
        body = self._process_nodes(
            [child for child in tsnode.children
             if child.type not in structural])
        trailing = next_of_type(tsnode, "elseif_clause")
        otherwise = (
            [self._if_clause(trailing, final_else)] if trailing else
            self._process_nodes(
                [child for child in
                 (next_of_type(tsnode, "else_clause").children
                  if next_of_type(tsnode, "else_clause") else [])
                 if child.type != "else"]) or final_else)
        result = nodes.IfBlock.create(
            self._expression(condition), body, otherwise)
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
        statement = next_of_type(tsnode, "do_statement")
        control = next_of_type(statement, "loop_control_expression")
        while_node = next_of_type(statement, "while_statement")
        body = self._process_nodes(
            [child for child in tsnode.children
             if child.type not in ("do_statement",
                                   "end_do_loop_statement")])
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
            step = (self._expression(parts[3]) if len(parts) == 4
                    else nodes.Literal(
                        "1", symbols.ScalarType.integer_type()))
            return nodes.Loop.create(
                variable, self._expression(parts[1]),
                self._expression(parts[2]), step, body)
        if while_node:
            condition = next_of_type(
                while_node, "parenthesized_expression")
            return nodes.WhileLoop.create(
                self._expression(condition), body)
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
        condition = next_of_type(tsnode, "parenthesized_expression")
        structural = {"where", "parenthesized_expression",
                      "elsewhere_clause", "end_where_statement"}
        body = self._process_nodes(
            [child for child in tsnode.children
             if child.type not in structural])
        elsewhere = next_of_type(tsnode, "elsewhere_clause")
        other = (
            self._process_nodes(
                [child for child in elsewhere.children
                 if child.type != "elsewhere"])
            if elsewhere else None)
        result = nodes.IfBlock.create(
            self._expression(condition), body, other)
        result.annotations.extend(
            ["was_where"] if next_of_type(
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
        selector_node = next_of_type(
            next_of_type(tsnode, "selector"), "identifier")
        if selector_node is None:
            selector = self._expression(
                [child for child in
                 next_of_type(tsnode, "selector").children
                 if child.type not in ("(", ")")][0])
        else:
            selector = self._expression(selector_node)
        cases = list(direct_child_of_type(tsnode, "case_statement"))
        default_body = None
        normal = []
        for case in cases:
            if next_of_type(case, "default"):
                default_body = self._process_nodes(
                    [child for child in case.children
                     if child.type not in ("case", "default")])
            else:
                values = next_of_type(case, "case_value_range_list")
                if values is None:
                    raise NotImplementedError(
                        "Malformed CASE value list")
                structural = {"case", "(", ")", "case_value_range_list"}
                body = self._process_nodes(
                    [child for child in case.children
                     if child.type not in structural])
                normal.append((values, body))
        current = default_body
        for values, body in reversed(normal):
            condition = self._case_condition(selector, values)
            block = nodes.IfBlock.create(
                condition, body, current)
            block.annotations.append("was_case")
            current = [block]
        if current and len(current) == 1:
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
                        self._expression(before[0])))
                if after:
                    parts.append(nodes.BinaryOperation.create(
                        nodes.BinaryOperation.Operator.LE, selector.copy(),
                        self._expression(after[0])))
                condition = parts[0] if len(parts) == 1 else (
                    nodes.BinaryOperation.create(
                        nodes.BinaryOperation.Operator.AND,
                        parts[0], parts[1]))
            else:
                condition = nodes.BinaryOperation.create(
                    nodes.BinaryOperation.Operator.EQ, selector.copy(),
                    self._expression(child))
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
                             self._expression(child.children[-1])))
            elif child.type == "sized_allocation":
                args.append(self._allocation_reference(child))
            else:
                args.append(self._expression(child))
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
        ident = next_of_type(tsnode, "identifier")
        reference = self._identifier_handler(ident)
        if not isinstance(reference.symbol, symbols.DataSymbol):
            raise NotImplementedError(
                "An ALLOCATE object must be a data symbol")
        size = next_of_type(tsnode, "size")
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
                lower, self._expression(tsnode))

        before, after, has_colon = self._split_extent(tsnode)
        if not has_colon:
            raise NotImplementedError("Malformed allocation bound")
        if before:
            lower = self._expression(before[0])
        if not after:
            raise NotImplementedError(
                "Allocation upper bound is required")
        return nodes.Range.create(
            lower, self._expression(after[0]))
