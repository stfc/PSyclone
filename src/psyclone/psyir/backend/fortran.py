# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2024, Science and Technology Facilities Council.
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
# Authors R. W. Ford and S. Siso, STFC Daresbury Lab
# Modified J. Henrichs, Bureau of Meteorology
# Modified A. R. Porter, A. B. G. Chalk and N. Nobre, STFC Daresbury Lab

'''PSyIR Fortran backend. Implements a visitor that generates Fortran code
from a PSyIR tree. '''

# pylint: disable=too-many-lines
from psyclone.core import Signature
from psyclone.errors import GenerationError, InternalError
from psyclone.psyir.backend.language_writer import LanguageWriter
from psyclone.psyir.backend.visitor import VisitorError
from psyclone.psyir.frontend.fparser2 import (
    Fparser2Reader, TYPE_MAP_FROM_FORTRAN)
from psyclone.psyir.nodes import (
    BinaryOperation, Call, CodeBlock, DataNode, IntrinsicCall, Literal,
    Operation, Range, Routine, Schedule, UnaryOperation)
from psyclone.psyir.symbols import (
    ArgumentInterface, ArrayType, ContainerSymbol, DataSymbol, DataTypeSymbol,
    UnresolvedType, RoutineSymbol, ScalarType, Symbol, IntrinsicSymbol,
    SymbolTable, UnsupportedFortranType, UnsupportedType, UnresolvedInterface,
    StructureType)


# Mapping from PSyIR types to Fortran data types. Simply reverse the
# map from the frontend, removing the special case of "double
# precision", which is captured as a REAL intrinsic in the PSyIR.
TYPE_MAP_TO_FORTRAN = {}
for key, item in TYPE_MAP_FROM_FORTRAN.items():
    if key != "double precision":
        TYPE_MAP_TO_FORTRAN[item] = key


def gen_intent(symbol):
    '''Given a DataSymbol instance as input, determine the Fortran intent that
    the DataSymbol should have and return the value as a string.

    :param symbol: the symbol instance.
    :type symbol: :py:class:`psyclone.psyir.symbols.DataSymbol`

    :returns: the Fortran intent of the symbol instance in lower case, \
    or None if the access is unknown or if this is a local variable.
    :rtype: str or NoneType

    '''
    mapping = {ArgumentInterface.Access.UNKNOWN: None,
               ArgumentInterface.Access.READ: "in",
               ArgumentInterface.Access.WRITE: "out",
               ArgumentInterface.Access.READWRITE: "inout"}

    if symbol.is_argument:
        try:
            return mapping[symbol.interface.access]
        except KeyError as excinfo:
            raise VisitorError(
                    f"Unsupported access '{excinfo}' found.") from excinfo
    return None  # non-Arguments do not have intent


def gen_datatype(datatype, name):
    '''Given a DataType instance as input, return the Fortran datatype
    of the symbol including any specific precision properties.

    :param datatype: the DataType or DataTypeSymbol describing the type of \
                     the declaration.
    :type datatype: :py:class:`psyclone.psyir.symbols.DataType` or \
                    :py:class:`psyclone.psyir.symbols.DataTypeSymbol`
    :param str name: the name of the symbol being declared (only used for \
                     error messages).

    :returns: the Fortran representation of the symbol's datatype \
              including any precision properties.
    :rtype: str

    :raises NotImplementedError: if the symbol has an unsupported \
        datatype.
    :raises VisitorError: if the symbol specifies explicit precision \
        and this is not supported for the datatype.
    :raises VisitorError: if the size of the explicit precision is not \
        supported for the datatype.
    :raises VisitorError: if the size of the symbol is specified by \
        another variable and the datatype is not one that supports the \
        Fortran KIND option.
    :raises NotImplementedError: if the type of the precision object \
        is an unsupported type.

    '''
    if isinstance(datatype, DataTypeSymbol):
        # Symbol is of derived type
        return f"type({datatype.name})"

    if (isinstance(datatype, ArrayType) and
            isinstance(datatype.intrinsic, DataTypeSymbol)):
        # Symbol is an array of derived types
        return f"type({datatype.intrinsic.name})"

    try:
        fortrantype = TYPE_MAP_TO_FORTRAN[datatype.intrinsic]
    except KeyError as error:
        raise NotImplementedError(
            f"Unsupported datatype '{datatype.intrinsic}' for symbol '{name}' "
            f"found in gen_datatype().") from error

    precision = datatype.precision

    if isinstance(precision, int):
        if fortrantype not in ['real', 'integer', 'logical']:
            raise VisitorError(f"Explicit precision not supported for datatype"
                               f" '{fortrantype}' in symbol '{name}' in "
                               f"Fortran backend.")
        if fortrantype == 'real' and precision not in [4, 8, 16]:
            raise VisitorError(
                f"Datatype 'real' in symbol '{name}' supports fixed precision "
                f"of [4, 8, 16] but found '{precision}'.")
        if fortrantype in ['integer', 'logical'] and precision not in \
           [1, 2, 4, 8, 16]:
            raise VisitorError(
                f"Datatype '{fortrantype}' in symbol '{name}' supports fixed "
                f"precision of [1, 2, 4, 8, 16] but found '{precision}'.")
        # Precision has an an explicit size. Use the "type*size" Fortran
        # extension for simplicity. We could have used
        # type(kind=selected_int|real_kind(size)) or, for Fortran 2008,
        # ISO_FORTRAN_ENV; type(type64) :: MyType.
        return f"{fortrantype}*{precision}"

    if isinstance(precision, ScalarType.Precision):
        # The precision information is not absolute so is either
        # machine specific or is specified via the compiler. Fortran
        # only distinguishes relative precision for single and double
        # precision reals.
        if fortrantype.lower() == "real" and \
           precision == ScalarType.Precision.DOUBLE:
            return "double precision"
        # This logging warning can be added when issue #11 is
        # addressed.
        # import logging
        # logging.warning(
        #      "Fortran does not support relative precision for the '%s' "
        #      "datatype but '%s' was specified for variable '%s'.",
        #      datatype, str(symbol.precision), symbol.name)
        return fortrantype

    if isinstance(precision, DataSymbol):
        if fortrantype not in ["real", "integer", "logical"]:
            raise VisitorError(
                f"kind not supported for datatype '{fortrantype}' in symbol "
                f"'{name}' in Fortran backend.")
        # The precision information is provided by a parameter, so use KIND.
        return f"{fortrantype}(kind={precision.name})"

    raise VisitorError(
        f"Unsupported precision type '{type(precision).__name__}' found for "
        f"symbol '{name}' in Fortran backend.")


def precedence(fortran_operator):
    '''Determine the relative precedence of the supplied Fortran operator.
    Relative Operator precedence is taken from the Fortran 2008
    specification document and encoded as a list.

    :param str fortran_operator: the supplied Fortran operator.

    :returns: an integer indicating the relative precedence of the \
        supplied Fortran operator. The higher the value, the higher \
        the precedence.

    :raises KeyError: if the supplied operator is not in the \
        precedence list.

    '''
    # The index of the fortran_precedence list indicates relative
    # precedence. Strings within sub-lists have the same precedence
    # apart from the following two caveats. 1) unary + and - have
    # a higher precedence than binary + and -, e.g. -(a-b) !=
    # -a-b and 2) floating point operations are not actually
    # associative due to rounding errors, e.g. potentially (a * b) / c
    # != a * (b / c). Therefore, if a particular ordering is specified
    # then it should be respected. These issues are dealt with in the
    # binaryoperation handler.
    fortran_precedence = [
        ['.EQV.', '.NEQV.'],
        ['.OR.'],
        ['.AND.'],
        ['.NOT.'],
        ['.EQ.', '.NE.', '.LT.', '.LE.', '.GT.', '.GE.', '==', '/=', '<',
         '<=', '>', '>='],
        ['//'],
        ['+', '-'],
        ['*', '/'],
        ['**']]
    for oper_list in fortran_precedence:
        if fortran_operator in oper_list:
            return fortran_precedence.index(oper_list)
    raise KeyError()


def add_accessibility_to_unsupported_declaration(symbol):
    '''
    Utility that manipulates the UnsupportedFortranType declaration for the
    supplied Symbol so as to ensure that it has the correct accessibility
    specifier.
    (This is required because we capture an UnsupportedFortranType declaration
    as is and this may or may not include accessibility information.)

    :param symbol: the symbol for which the declaration is required.
    :type symbol: :py:class:`psyclone.psyir.symbols.Symbol`

    :returns: Fortran declaration of the supplied symbol with accessibility \
        information included (public/private).
    :rtype: str

    :raises TypeError: if the supplied argument is not a Symbol of \
        UnsupportedFortranType.
    :raises InternalError: if the declaration associated with the Symbol is \
        empty.
    :raises NotImplementedError: if the original declaration does not use \
        '::' to separate the entity name from its type.
    :raises InternalError: if the declaration stored for the supplied symbol \
        contains accessibility information which does not match the \
        visibility of the supplied symbol.

    '''
    if not isinstance(symbol, Symbol):
        raise TypeError(f"Expected a Symbol but got '{type(symbol).__name__}'")

    if not isinstance(symbol.datatype, UnsupportedFortranType):
        raise TypeError(f"Expected a Symbol of UnsupportedFortranType but "
                        f"symbol '{symbol.name}' has type '{symbol.datatype}'")

    if not symbol.datatype.declaration:
        raise InternalError(
            f"Symbol '{symbol.name}' is of UnsupportedFortranType but the "
            f"associated declaration text is empty.")

    # The original declaration text is obtained from fparser2 and will
    # already have had any line-continuation symbols removed.
    first_line = symbol.datatype.declaration.split("\n")[0]
    if "::" not in first_line:
        raise NotImplementedError(
            f"Cannot add accessibility information to an UnsupportedFortran"
            f"Type that does not have '::' in its original declaration: "
            f"'{symbol.datatype.declaration}'")

    parts = symbol.datatype.declaration.split("::")
    first_part = parts[0].lower()
    if symbol.visibility == Symbol.Visibility.PUBLIC:
        if "public" not in first_part:
            if "private" in first_part:
                raise InternalError(
                    f"Symbol '{symbol.name}' of UnsupportedFortranType has "
                    f"public visibility but its associated declaration "
                    f"specifies that it is private: "
                    f"'{symbol.datatype.declaration}'")
            first_part = first_part.rstrip() + ", public "
    else:
        if "private" not in first_part:
            if "public" in first_part:
                raise InternalError(
                    f"Symbol '{symbol.name}' of UnsupportedFortranType has "
                    f"private visibility but its associated declaration "
                    f"specifies that it is public: "
                    f"'{symbol.datatype.declaration}'")
            first_part = first_part.rstrip() + ", private "
    return "::".join([first_part]+parts[1:])


class FortranWriter(LanguageWriter):
    # pylint: disable=too-many-public-methods
    '''Implements a PSyIR-to-Fortran back end for PSyIR kernel code (not
    currently PSyIR algorithm code which has its own gen method for
    generating Fortran).

    :param bool skip_nodes: If skip_nodes is False then an exception \
        is raised if a visitor method for a PSyIR node has not been \
        implemented, otherwise the visitor silently continues. This is an \
        optional argument which defaults to False.
    :param str indent_string: Specifies what to use for indentation. This \
        is an optional argument that defaults to two spaces.
    :param int initial_indent_depth: Specifies how much indentation to \
        start with. This is an optional argument that defaults to 0.
    :param bool check_global_constraints: whether or not to validate all \
        global constraints when walking the tree. Defaults to True.

    '''
    _COMMENT_PREFIX = "! "

    def __init__(self, skip_nodes=False, indent_string="  ",
                 initial_indent_depth=0, check_global_constraints=True):
        # Construct the base class using () as array parenthesis, and
        # % as structure access symbol
        super().__init__(("(", ")"), "%", skip_nodes,
                         indent_string,
                         initial_indent_depth,
                         check_global_constraints)
        # Reverse the Fparser2Reader maps that are used to convert from
        # Fortran operator names to PSyIR operator names.
        self._operator_2_str = {}
        self._reverse_map(self._operator_2_str,
                          Fparser2Reader.unary_operators)
        self._reverse_map(self._operator_2_str,
                          Fparser2Reader.binary_operators)

        # Create and store a DependencyTools instance for use when ordering
        # parameter declarations. Have to import it here as DependencyTools
        # also uses this Fortran backend.
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.tools import DependencyTools
        self._dep_tools = DependencyTools()

    @staticmethod
    def _reverse_map(reverse_dict, op_map):
        '''
        Reverses the supplied fortran2psyir mapping to make a psyir2fortran
        mapping. Any key that does already exist in `reverse_dict`
        is not overwritten, only new keys are added.

        :param reverse_dict: the dictionary to which the new mapping of \
            operator to string is added.
        :type reverse_dict: dict from \
            :py:class:`psyclone.psyir.nodes.BinaryOperation`, \
            :py:class:`psyclone.psyir.nodes.NaryOperation` or \
            :py:class:`psyclone.psyir.nodes.UnaryOperation` to str

        :param op_map: mapping from string representation of operator to \
                       enumerated type.
        :type op_map: :py:class:`collections.OrderedDict`

        '''
        for operator in op_map:
            mapping_key = op_map[operator]
            mapping_value = operator
            # Only choose the first mapping value when there is more
            # than one.
            if mapping_key not in reverse_dict:
                reverse_dict[mapping_key] = mapping_value.upper()

    def get_operator(self, operator):
        '''Determine the Fortran operator that is equivalent to the provided
        PSyIR operator. This is achieved by reversing the Fparser2Reader
        maps that are used to convert from Fortran operator names to PSyIR
        operator names.

        :param operator: a PSyIR operator.
        :type operator: :py:class:`psyclone.psyir.nodes.Operation.Operator`

        :returns: the Fortran operator.
        :rtype: str

        :raises KeyError: if the supplied operator is not known.

        '''
        return self._operator_2_str[operator]

    def gen_indices(self, indices, var_name=None):
        '''Given a list of PSyIR nodes representing the dimensions of an
        array, return a list of strings representing those array dimensions.
        This is used both for array references and array declarations. Note
        that 'indices' can also be a shape in case of Fortran.

        :param indices: list of PSyIR nodes.
        :type indices: list of :py:class:`psyclone.psyir.symbols.Node`
        :param str var_name: name of the variable for which the dimensions \
            are created. Not used in the Fortran implementation.

        :returns: the Fortran representation of the dimensions.
        :rtype: list of str

        :raises NotImplementedError: if the format of the dimension is not \
            supported.

        '''
        dims = []
        for index in indices:
            if isinstance(index, (DataNode, Range)):
                # literal constant, symbol reference, or computed
                # dimension
                expression = self._visit(index)
                dims.append(expression)
            elif isinstance(index, ArrayType.ArrayBounds):
                # Lower and upper bounds of an array declaration specified
                # by literal constant, symbol reference, or computed dimension
                lower_expression = self._visit(index.lower)
                if isinstance(index.upper, ArrayType.Extent):
                    # We have an assumed-shape array (R514) where only the
                    # lower bound is specified.
                    upper_expression = ""
                else:
                    upper_expression = self._visit(index.upper)
                if lower_expression == "1":
                    # Lower bound of 1 is the default in Fortran
                    dims.append(upper_expression)
                else:
                    dims.append(lower_expression+":"+upper_expression)
            elif isinstance(index, ArrayType.Extent):
                # unknown extent
                dims.append(":")
            else:
                raise NotImplementedError(
                    f"unsupported gen_indices index '{index}'")
        return dims

    def gen_use(self, symbol, symbol_table):
        ''' Performs consistency checks and then creates and returns the
        Fortran use statement(s) for this ContainerSymbol as required for
        the supplied symbol table.

        :param symbol: the container symbol instance.
        :type symbol: :py:class:`psyclone.psyir.symbols.ContainerSymbol`
        :param symbol_table: the symbol table containing this container symbol.
        :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable`

        :returns: the Fortran use statement(s) as a string.
        :rtype: str

        :raises VisitorError: if the symbol argument is not a ContainerSymbol.
        :raises VisitorError: if the symbol_table argument is not a \
                            SymbolTable.
        :raises VisitorError: if the supplied symbol is not in the supplied \
                            SymbolTable.
        :raises VisitorError: if the supplied symbol has the same name as an \
                            entry in the SymbolTable but is a different object.
        '''
        if not isinstance(symbol, ContainerSymbol):
            raise VisitorError(
                f"gen_use() expects a ContainerSymbol as its first argument "
                f"but got '{type(symbol).__name__}'")
        if not isinstance(symbol_table, SymbolTable):
            raise VisitorError(
                f"gen_use() expects a SymbolTable as its second argument but "
                f"got '{type(symbol_table).__name__}'")
        if symbol.name not in symbol_table:
            raise VisitorError(
                f"gen_use() - the supplied symbol ('{symbol.name}') is not"
                f" in the supplied SymbolTable.")
        if symbol_table.lookup(symbol.name) is not symbol:
            raise VisitorError(
                f"gen_use() - the supplied symbol ('{symbol.name}') is not "
                f"the same object as the entry with that name in the supplied "
                f"SymbolTable.")

        # Construct the list of symbol names for the ONLY clause
        only_list = []
        for dsym in symbol_table.symbols_imported_from(symbol):
            if dsym.interface.orig_name:
                # This variable is renamed on import. Use Fortran's
                # 'new_name=>orig_name' syntax to reflect this.
                only_list.append(f"{dsym.name}=>{dsym.interface.orig_name}")
            else:
                # This variable is not renamed.
                only_list.append(dsym.name)

        # Finally construct the use statements for this Container (module)
        if not only_list and not symbol.wildcard_import:
            # We have a "use xxx, only:" - i.e. an empty only list
            return f"{self._nindent}use {symbol.name}, only :\n"
        if only_list and not symbol.wildcard_import:
            return f"{self._nindent}use {symbol.name}, only : " + \
                    ", ".join(sorted(only_list)) + "\n"

        return f"{self._nindent}use {symbol.name}\n"

    def gen_vardecl(self, symbol, include_visibility=False):
        '''Create and return the Fortran variable declaration for this Symbol
        or derived-type member.

        :param symbol: the symbol or member instance.
        :type symbol: :py:class:`psyclone.psyir.symbols.DataSymbol` or \
            :py:class:`psyclone.psyir.nodes.MemberReference`
        :param bool include_visibility: whether to include the visibility of \
            the symbol in the generated declaration (default False).

        :returns: the Fortran variable declaration as a string.
        :rtype: str

        :raises VisitorError: if the symbol is of UnresolvedType.
        :raises VisitorError: if the symbol is of UnsupportedType other than
            UnsupportedFortranType.
        :raises VisitorError: if the symbol is of known type but does not
            specify a variable declaration (it is not a local declaration or
            an argument declaration).
        :raises VisitorError: if the symbol is a runtime constant but does not
            have a StaticInterface.
        :raises InternalError: if the symbol is a ContainerSymbol or an import.
        :raises InternalError: if the symbol is a RoutineSymbol other than
            UnsupportedFortranType.
        :raises InternalError: if visibility is to be included but is not
            either PUBLIC or PRIVATE.

        '''
        # pylint: disable=too-many-branches
        if isinstance(symbol.datatype, UnresolvedType):
            raise VisitorError(f"Symbol '{symbol.name}' has a UnresolvedType "
                               f"and we can not generate a declaration for "
                               f"UnresolvedTypes.")
        if isinstance(symbol, ContainerSymbol) or \
                isinstance(symbol, Symbol) and symbol.is_import:
            raise InternalError(f"Symbol '{symbol.name}' is brought into scope"
                                f" from a Fortran USE statement and should be "
                                f"generated by 'gen_use' instead of "
                                f"'gen_vardecl'.")
        if isinstance(symbol, RoutineSymbol) and not \
                isinstance(symbol.datatype, UnsupportedFortranType):
            raise InternalError(f"Symbol '{symbol.name}' is a RoutineSymbol "
                                f"which is not imported nor an interface "
                                f"(UnsupportedFortranType). This is already "
                                f"implicitly declared by the routine itself "
                                f"and should not be provided to 'gen_vardecl'."
                                )

        # Whether we're dealing with an array declaration and, if so, the
        # shape of that array.
        if isinstance(symbol.datatype, ArrayType):
            array_shape = symbol.datatype.shape
        else:
            array_shape = []

        if isinstance(symbol.datatype, UnsupportedType):
            if isinstance(symbol.datatype, UnsupportedFortranType):

                if (include_visibility and
                        not isinstance(symbol, RoutineSymbol) and
                        not symbol.name.startswith("_PSYCLONE_INTERNAL")):
                    # We don't attempt to add accessibility to RoutineSymbols
                    # or to those created by PSyclone to handle named common
                    # blocks appearing in SAVE statements.
                    decln = add_accessibility_to_unsupported_declaration(
                                symbol)
                    return f"{self._nindent}{decln}\n"

                decln = symbol.datatype.declaration
                return f"{self._nindent}{decln}\n"
            # The Fortran backend only handles UnsupportedFortranType
            # declarations.
            raise VisitorError(
                f"{type(symbol).__name__} '{symbol.name}' is of "
                f"'{type(symbol.datatype).__name__}' type. This is not "
                f"supported by the Fortran backend.")

        datatype = gen_datatype(symbol.datatype, symbol.name)
        result = f"{self._nindent}{datatype}"

        if ArrayType.Extent.DEFERRED in array_shape:
            # A 'deferred' array extent means this is an allocatable array
            result += ", allocatable"

        # Specify Fortran attributes
        if array_shape:
            dims = self.gen_indices(array_shape)
            result += ", dimension(" + ",".join(dims) + ")"

        if isinstance(symbol, DataSymbol) and symbol.is_argument:
            intent = gen_intent(symbol)
            if intent:
                result += f", intent({intent})"

        if isinstance(symbol, DataSymbol) and symbol.is_constant:
            result += ", parameter"
        elif isinstance(symbol, DataSymbol) and symbol.is_static:
            # This condition is an elif because SAVE and PARAMETER are
            # incompatible, but we let PARAMETER take precedence because
            # a parameter is already behaving like a static value
            result += ", save"

        if include_visibility:
            if symbol.visibility == Symbol.Visibility.PRIVATE:
                result += ", private"
            elif symbol.visibility == Symbol.Visibility.PUBLIC:
                result += ", public"
            else:
                raise InternalError(
                    f"A Symbol must be either public or private but symbol "
                    f"'{symbol.name}' has visibility '{symbol.visibility}'")

        # Specify name
        result += f" :: {symbol.name}"

        # Specify initialisation expression
        if (isinstance(symbol, StructureType.ComponentType) and
                symbol.initial_value):
            result += " = " + self._visit(symbol.initial_value)
        elif isinstance(symbol, DataSymbol) and symbol.initial_value:
            if not symbol.is_static:
                raise VisitorError(
                    f"{type(symbol).__name__} '{symbol.name}' has an initial "
                    f"value ({self._visit(symbol.initial_value)}) and "
                    f"therefore (in Fortran) must have a StaticInterface. "
                    f"However it has an interface of '{symbol.interface}'.")
            result += " = " + self._visit(symbol.initial_value)

        return result + "\n"

    def gen_typedecl(self, symbol, include_visibility=True):
        '''
        Creates a derived-type declaration for the supplied DataTypeSymbol.

        :param symbol: the derived-type to declare.
        :type symbol: :py:class:`psyclone.psyir.symbols.DataTypeSymbol`
        :param bool include_visibility: whether or not to include visibility
            information in the declaration. (Default is True.)

        :returns: the Fortran declaration of the derived type.
        :rtype: str

        :raises VisitorError: if the supplied symbol is not a DataTypeSymbol.
        :raises VisitorError: if the datatype of the symbol is of
            UnsupportedType but is not of UnsupportedFortranType.
        :raises InternalError: if include_visibility is True and the
            visibility of the symbol is not of the correct type.
        :raises VisitorError: if the supplied symbol is of UnresolvedType.

        '''
        if not isinstance(symbol, DataTypeSymbol):
            raise VisitorError(
                f"gen_typedecl expects a DataTypeSymbol as argument but "
                f"got: '{type(symbol).__name__}'")

        if isinstance(symbol.datatype, UnsupportedType):
            if isinstance(symbol.datatype, UnsupportedFortranType):
                # This is a declaration of UnsupportedType. We have to ensure
                # that its visibility is correctly specified though.
                if include_visibility:
                    decln = add_accessibility_to_unsupported_declaration(
                                symbol)
                else:
                    decln = symbol.datatype.declaration
                return f"{self._nindent}{decln}\n"

            raise VisitorError(
                f"Fortran backend cannot generate code for symbol "
                f"'{symbol.name}' of type '{type(symbol.datatype).__name__}'")

        result = f"{self._nindent}type"

        if include_visibility:
            if symbol.visibility == Symbol.Visibility.PRIVATE:
                result += ", private"
            elif symbol.visibility == Symbol.Visibility.PUBLIC:
                result += ", public"
            else:
                raise InternalError(
                    f"A Symbol's visibility must be one of Symbol.Visibility."
                    f"PRIVATE/PUBLIC but '{symbol.name}' has visibility of "
                    f"type '{type(symbol.visibility).__name__}'")
        result += f" :: {symbol.name}\n"

        if isinstance(symbol.datatype, UnresolvedType):
            raise VisitorError(
                f"Local Symbol '{symbol.name}' is of UnresolvedType and "
                f"therefore no declaration can be created for it. Should it "
                f"have an ImportInterface?")

        self._depth += 1

        for member in symbol.datatype.components.values():
            # We can only specify the visibility of components within
            # a derived type if the declaration is within the specification
            # part of a module.
            result += self.gen_vardecl(member,
                                       include_visibility=include_visibility)
        self._depth -= 1

        result += f"{self._nindent}end type {symbol.name}\n"
        return result

    def gen_default_access_stmt(self, symbol_table):
        '''
        Generates the access statement for a module - either "private" or
        "public". Although the PSyIR captures the visibility of every Symbol
        explicitly, this information is required in order
        to ensure the correct visibility of symbols that have been imported
        into the current module from another one using a wildcard import
        (i.e. a `use` without an `only` clause) and also for those Symbols
        that are of UnsupportedFortranType (because their declaration may or
        may not include visibility information).

        :returns: text containing the access statement line.
        :rtype: str

        :raises InternalError: if the symbol table has an invalid default \
                               visibility.
        '''
        # If no default visibility has been set then we use the Fortran
        # default of public.
        if symbol_table.default_visibility in [None, Symbol.Visibility.PUBLIC]:
            return self._nindent + "public\n"
        if symbol_table.default_visibility == Symbol.Visibility.PRIVATE:
            return self._nindent + "private\n"

        raise InternalError(
            f"Unrecognised visibility ('{symbol_table.default_visibility}') "
            f"found when attempting to generate access statement. Should be "
            f"either 'Symbol.Visibility.PUBLIC' or "
            f"'Symbol.Visibility.PRIVATE'\n")

    def gen_access_stmts(self, symbol_table):
        '''
        Creates the accessibility statements (R518) for any routine or
        imported symbols in the supplied symbol table.

        :param symbol_table: the symbol table for which to generate \
                             accessibility statements.
        :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable`

        :returns: the accessibility statements for any routine or imported \
                  symbols.
        :rtype: str

        '''
        # Find the symbol that represents itself, this one will not need
        # an accessibility statement
        try:
            itself = symbol_table.lookup_with_tag('own_routine_symbol')
        except KeyError:
            itself = None

        public_symbols = []
        private_symbols = []
        for symbol in symbol_table.symbols:
            if (isinstance(symbol, RoutineSymbol) or
                    symbol.is_unresolved or symbol.is_import):

                # Skip the symbol representing the routine where these
                # declarations belong
                if isinstance(symbol, RoutineSymbol) and symbol is itself:
                    continue

                # It doesn't matter whether this symbol has a local or import
                # interface - its accessibility in *this* context is determined
                # by the local accessibility statements. e.g. if we are
                # dealing with the declarations in a given module which itself
                # uses a public symbol from some other module, the
                # accessibility of that symbol is determined by the
                # accessibility statements in the current module.
                if (symbol_table.default_visibility in
                        [None, Symbol.Visibility.PUBLIC]):
                    if symbol.visibility == Symbol.Visibility.PRIVATE:
                        # Default vis. is public but this symbol is private
                        private_symbols.append(symbol.name)
                else:
                    if symbol.visibility == Symbol.Visibility.PUBLIC:
                        # Default vis. is private but this symbol is public
                        public_symbols.append(symbol.name)

        result = "\n"
        if public_symbols:
            result += f"{self._nindent}public :: {', '.join(public_symbols)}\n"
        if private_symbols:
            result += (f"{self._nindent}private :: "
                       f"{', '.join(private_symbols)}\n")

        if len(result) > 1:
            return result
        return ""

    # pylint: disable=too-many-branches
    def _gen_parameter_decls(self, symbol_table, is_module_scope=False):
        ''' Create the declarations of all parameters present in the supplied
        symbol table. Declarations are ordered so as to satisfy any inter-
        dependencies between them.

        :param symbol_table: the SymbolTable instance.
        :type symbol: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :param bool is_module_scope: whether or not the declarations are in \
                                     a module scoping unit. Default is False.

        :returns: Fortran code declaring all parameters.
        :rtype: str

        :raises VisitorError: if there is no way of resolving \
                              interdependencies between parameter declarations.

        '''
        declarations = ""
        local_constants = []
        for sym in symbol_table.datasymbols:
            if sym.is_import or sym.is_unresolved:
                continue  # Skip, these don't need declarations
            if sym.is_constant:
                local_constants.append(sym)

        # There may be dependencies between these constants so setup a dict
        # listing the required inputs for each one.
        decln_inputs = {}
        # Avoid circular dependency
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.tools.read_write_info import ReadWriteInfo
        for symbol in local_constants:
            decln_inputs[symbol.name] = set()
            read_write_info = ReadWriteInfo()
            self._dep_tools.get_input_parameters(read_write_info,
                                                 symbol.initial_value)
            # The dependence analysis tools do not include symbols used to
            # define precision so check for those here.
            for lit in symbol.initial_value.walk(Literal):
                if isinstance(lit.datatype.precision, DataSymbol):
                    read_write_info.add_read(
                        Signature(lit.datatype.precision.name))
            # If the precision of the Symbol being declared is itself defined
            # by a Symbol then include that as an 'input'.
            if isinstance(symbol.datatype.precision, DataSymbol):
                read_write_info.add_read(
                    Signature(symbol.datatype.precision.name))
            # Remove any 'inputs' that are not local since these do not affect
            # the ordering of local declarations.
            for sig in read_write_info.signatures_read:
                if symbol_table.lookup(sig.var_name) in local_constants:
                    decln_inputs[symbol.name].add(sig)
        # We now iterate over the declarations, declaring those that have their
        # inputs satisfied. Creating a declaration for a given symbol removes
        # that symbol as a dependence from any outstanding declarations.
        declared = set()
        while local_constants:
            for symbol in local_constants[:]:
                inputs = decln_inputs[symbol.name]
                if inputs.issubset(declared):
                    # All inputs are satisfied so this declaration can be added
                    declared.add(Signature(symbol.name))
                    local_constants.remove(symbol)
                    declarations += self.gen_vardecl(
                        symbol, include_visibility=is_module_scope)
                    break
            else:
                # We looped through all of the variables remaining to be
                # declared and none had their dependencies satisfied.
                raise VisitorError(
                    f"Unable to satisfy dependencies for the declarations of "
                    f"{[sym.name for sym in local_constants]}")
        return declarations

    def gen_decls(self, symbol_table, is_module_scope=False):
        '''Create and return the Fortran declarations for the supplied
        SymbolTable.

        :param symbol_table: the SymbolTable instance.
        :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :param bool is_module_scope: whether or not the declarations are in
            a module scoping unit. Default is False.

        :returns: the Fortran declarations for the table.
        :rtype: str

        :raises VisitorError: if one of the symbols is a RoutineSymbol which
            does not have an ImportInterface or UnresolvedInterface (
            representing named and unqualified imports respectively) or
            ModuleDefaultInterface (representing routines declared in the
            same module) or is not a Fortran intrinsic.
        :raises VisitorError: if args_allowed is False and one or more
            argument declarations exist in symbol_table.
        :raises VisitorError: if there are any symbols (other than
            RoutineSymbols) in the supplied table that do not have an
            explicit declaration (UnresolvedInterface) and there are no
            wildcard imports or unknown interfaces.

        '''
        # pylint: disable=too-many-branches
        declarations = ""

        # Get all symbols local to this symbol table
        all_symbols = symbol_table.symbols

        # Before processing the declarations we remove:
        for sym in all_symbols[:]:
            # Everything that is a container or imported (because it should
            # already be done by the gen_use() method before)
            if isinstance(sym, ContainerSymbol):
                all_symbols.remove(sym)
                continue
            if sym.is_import:
                all_symbols.remove(sym)
                continue
            # All the IntrinsicSymbols and RoutineSymbols with an
            # UresolvedInterface (Fortran can have Calls which are
            # only resolved at link time)
            if isinstance(sym, IntrinsicSymbol) or (
                    isinstance(sym, RoutineSymbol) and
                    isinstance(sym.interface, UnresolvedInterface)):
                all_symbols.remove(sym)

        # If the symbol table contains any symbols with an
        # UnresolvedInterface interface (they are not explicitly
        # declared), we need to check that we have at least one
        # wildcard import which could be bringing them into this
        # scope, or an unknown interface which could be declaring
        # them.
        unresolved_symbols = []
        for sym in all_symbols[:]:
            if isinstance(sym.interface, UnresolvedInterface):
                unresolved_symbols.append(sym)
                all_symbols.remove(sym)
        try:
            internal_interface_symbol = symbol_table.lookup(
                "_psyclone_internal_interface")
        except KeyError:
            internal_interface_symbol = None
        if unresolved_symbols and not (
                symbol_table.has_wildcard_imports() or
                internal_interface_symbol):
            symbols_txt = ", ".join(
                ["'" + sym.name + "'" for sym in unresolved_symbols])
            raise VisitorError(
                f"The following symbols are not explicitly declared or "
                f"imported from a module and there are no wildcard "
                f"imports which could be bringing them into scope: "
                f"{symbols_txt}")

        # As a convention, we will declare the variables in the following
        # order:

        # 1: Routines (Interfaces)
        for sym in all_symbols[:]:
            if isinstance(sym, RoutineSymbol):
                # Interfaces to module procedures are captured by the frontend
                # as RoutineSymbols of UnsupportedFortranType. These must
                # therefore be declared.
                if isinstance(sym.datatype, UnsupportedType):
                    declarations += self.gen_vardecl(
                        sym, include_visibility=is_module_scope)
                elif sym.is_modulevar or sym.is_automatic:
                    pass
                else:
                    raise VisitorError(
                        f"Routine symbol '{sym.name}' has '{sym.interface}'. "
                        f"This is not supported by the Fortran back-end.")
                all_symbols.remove(sym)

        # 2: Constants.
        declarations += self._gen_parameter_decls(symbol_table,
                                                  is_module_scope)
        for sym in all_symbols[:]:
            if isinstance(sym, DataSymbol) and sym.is_constant:
                all_symbols.remove(sym)

        # 3: Argument variable declarations
        if symbol_table.argument_datasymbols and is_module_scope:
            raise VisitorError(
                f"Arguments are not allowed in this context but this symbol "
                f"table contains argument(s): "
                f"'{[sym.name for sym in symbol_table.argument_datasymbols]}'."
                )
        # We use symbol_table.argument_datasymbols because it has the
        # symbol order that we need
        for symbol in symbol_table.argument_datasymbols:
            declarations += self.gen_vardecl(
                symbol, include_visibility=is_module_scope)
            all_symbols.remove(symbol)

        # 4: Derived-type declarations. These must come before any declarations
        # of symbols of these types.
        for symbol in all_symbols[:]:
            if isinstance(symbol, DataTypeSymbol):
                declarations += self.gen_typedecl(
                    symbol, include_visibility=is_module_scope)
                all_symbols.remove(symbol)

        # 5: The rest of the symbols
        for symbol in all_symbols:
            declarations += self.gen_vardecl(
                symbol, include_visibility=is_module_scope)

        return declarations

    def filecontainer_node(self, node):
        '''This method is called when a FileContainer instance is found in
        the PSyIR tree.

        A file container node requires no explicit text in the Fortran
        back end.

        :param node: a Container PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.FileContainer`

        :returns: the Fortran code as a string.
        :rtype: str

        :raises VisitorError: if the attached symbol table contains \
            any data symbols.
        :raises VisitorError: if more than one child is a Routine Node \
            with is_program set to True.

        '''
        if node.symbol_table.symbols:
            raise VisitorError(
                f"In the Fortran backend, a file container should not have "
                f"any symbols associated with it, but found "
                f"{len(node.symbol_table.symbols)}.")

        program_nodes = len([child for child in node.children if
                             isinstance(child, Routine) and child.is_program])
        if program_nodes > 1:
            raise VisitorError(
                f"In the Fortran backend, a file container should contain at "
                f"most one routine node that is a program, but found "
                f"{program_nodes}.")

        result = ""
        for child in node.children:
            result += self._visit(child)
        return result

    def container_node(self, node):
        '''This method is called when a Container instance is found in
        the PSyIR tree.

        A container node is mapped to a module in the Fortran back end.

        :param node: a Container PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Container`

        :returns: the Fortran code as a string.
        :rtype: str

        :raises VisitorError: if the name attribute of the supplied \
            node is empty or None.
        :raises VisitorError: if any of the children of the supplied \
            Container node are not Routines or CodeBlocks.

        '''
        if not node.name:
            raise VisitorError("Expected Container node name to have a value.")

        # All children must be either Routines or CodeBlocks as modules within
        # modules are not supported.
        if not all(isinstance(child, (Routine, CodeBlock)) for
                   child in node.children):
            raise VisitorError(
                f"The Fortran back-end requires all children of a Container "
                f"to be either CodeBlocks or sub-classes of Routine but found:"
                f" {[type(child).__name__ for child in node.children]}.")

        result = f"{self._nindent}module {node.name}\n"

        self._depth += 1

        # Generate module imports
        imports = ""
        for symbol in node.symbol_table.containersymbols:
            imports += self.gen_use(symbol, node.symbol_table)

        # Declare the Container's data
        declarations = self.gen_decls(node.symbol_table, is_module_scope=True)

        # Generate the access statement (PRIVATE or PUBLIC)
        declarations += self.gen_default_access_stmt(node.symbol_table)

        # Accessibility statements for imported and routine symbols
        declarations += self.gen_access_stmts(node.symbol_table)

        # Get the subroutine statements.
        subroutines = ""
        for child in node.children:
            subroutines += self._visit(child)

        result += (
            f"{imports}"
            f"{self._nindent}implicit none\n"
            f"{declarations}\n"
            f"{self._nindent}contains\n"
            f"{subroutines}\n")

        self._depth -= 1
        result += f"{self._nindent}end module {node.name}\n"
        return result

    def routine_node(self, node):
        '''This method is called when a Routine node is found in
        the PSyIR tree.

        :param node: a Routine PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Routine`

        :returns: the Fortran code for this node.
        :rtype: str

        :raises VisitorError: if the name attribute of the supplied \
                              node is empty or None.

        '''
        if not node.name:
            raise VisitorError("Expected node name to have a value.")

        if node.is_program:
            result = f"{self._nindent}program {node.name}\n"
            routine_type = "program"
        else:
            args = [symbol.name for symbol in node.symbol_table.argument_list]
            suffix = ""
            if node.return_symbol:
                # This Routine has a return value and is therefore a Function
                routine_type = "function"
                if node.return_symbol.name.lower() != node.name.lower():
                    suffix = f" result({node.return_symbol.name})"
            else:
                routine_type = "subroutine"
            result = f"{self._nindent}{routine_type} {node.name}("
            result += ", ".join(args) + f"){suffix}\n"

        self._depth += 1

        if self._DISABLE_LOWERING:
            # If we are not lowering we don't have a deep_copied tree so it
            # should NOT make any modifications to the provided node or
            # symbol table.
            whole_routine_scope = node.symbol_table
        else:
            # The PSyIR has nested scopes but Fortran only supports declaring
            # variables at the routine level scope. For this reason, at this
            # point we have to unify all declarations and resolve possible name
            # clashes that appear when merging the scopes. Make sure we use
            # the same SymbolTable class used in the base class to get an
            # API-specific table here:
            whole_routine_scope = type(node.symbol_table)()

            for schedule in node.walk(Schedule):
                whole_routine_scope.merge(schedule.symbol_table)

            # Replace the symbol table
            node.symbol_table.detach()
            whole_routine_scope.attach(node)

        # Generate module imports
        imports = ""
        for symbol in whole_routine_scope.containersymbols:
            imports += self.gen_use(symbol, whole_routine_scope)

        # Generate declaration statements
        declarations = self.gen_decls(whole_routine_scope)

        # Get the executable statements.
        exec_statements = ""
        for child in node.children:
            exec_statements += self._visit(child)
        result += (
            f"{imports}"
            f"{declarations}\n"
            f"{exec_statements}\n")

        self._depth -= 1
        result += f"{self._nindent}end {routine_type} {node.name}\n"

        return result

    def assignment_node(self, node):
        '''This method is called when an Assignment instance is found in the
        PSyIR tree.

        :param node: an Assignment PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Assignment``

        :returns: the Fortran code as a string.
        :rtype: str

        '''
        lhs = self._visit(node.lhs)
        rhs = self._visit(node.rhs)
        result = f"{self._nindent}{lhs} = {rhs}\n"
        return result

    def binaryoperation_node(self, node):
        '''This method is called when a BinaryOperation instance is found in
        the PSyIR tree.

        :param node: a BinaryOperation PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.BinaryOperation`

        :returns: the Fortran code as a string.
        :rtype: str

        '''
        lhs = self._visit(node.children[0])
        rhs = self._visit(node.children[1])
        try:
            fort_oper = self.get_operator(node.operator)
            parent = node.parent
            if isinstance(parent, Operation):
                # We may need to enforce precedence
                parent_fort_oper = self.get_operator(parent.operator)
                if precedence(fort_oper) < precedence(parent_fort_oper):
                    # We need brackets to enforce precedence
                    return f"({lhs} {fort_oper} {rhs})"
                if precedence(fort_oper) == precedence(parent_fort_oper):
                    # We still may need to enforce precedence
                    if (isinstance(parent, UnaryOperation) or
                            (isinstance(parent, BinaryOperation) and
                             parent.children[1] == node)):
                        # We need brackets to enforce precedence
                        # as a) a unary operator is performed
                        # before a binary operator and b) floating
                        # point operations are not actually
                        # associative due to rounding errors.
                        return f"({lhs} {fort_oper} {rhs})"
            return f"{lhs} {fort_oper} {rhs}"
        except KeyError as error:
            raise VisitorError(
                f"Unexpected binary op '{node.operator}'.") from error

    def range_node(self, node):
        '''This method is called when a Range instance is found in the PSyIR
        tree.

        :param node: a Range PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Range`

        :returns: the Fortran code as a string.
        :rtype: str

        '''
        if node.parent and node.parent.is_lower_bound(
                node.parent.indices.index(node)):
            # The range starts for the first element in this
            # dimension. This is the default in Fortran so no need to
            # output anything.
            start = ""
        else:
            start = self._visit(node.start)

        if node.parent and node.parent.is_upper_bound(
                node.parent.indices.index(node)):
            # The range ends with the last element in this
            # dimension. This is the default in Fortran so no need to
            # output anything.
            stop = ""
        else:
            stop = self._visit(node.stop)
        result = f"{start}:{stop}"

        if isinstance(node.step, Literal) and \
           node.step.datatype.intrinsic == ScalarType.Intrinsic.INTEGER and \
           node.step.value == "1":
            # Step is 1. This is the default in Fortran so no need to
            # output any text.
            pass
        else:
            step = self._visit(node.step)
            result += f":{step}"
        return result

    def literal_node(self, node):
        '''This method is called when a Literal instance is found in the PSyIR
        tree.

        :param node: a Literal PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Literal`

        :returns: the Fortran code for the literal.
        :rtype: str

        '''
        # pylint: disable=too-many-branches
        precision = node.datatype.precision

        if node.datatype.intrinsic == ScalarType.Intrinsic.BOOLEAN:
            # Booleans need to be converted to Fortran format
            result = '.' + node.value + '.'
        elif node.datatype.intrinsic == ScalarType.Intrinsic.CHARACTER:
            # Need to take care with which quotation symbol to use since a
            # character string may include quotation marks, e.g. a format
            # specifier: "('hello',3A)". The outermost quotation marks are
            # not stored so we have to decide whether to use ' or ".
            if "'" not in node.value:
                # No single quotes in the string so use those
                quote_symbol = "'"
            else:
                # There are single quotes in the string so we use double
                # quotes (after verifying that there aren't both single *and*
                # double quotes in the string).
                if '"' in node.value:
                    raise NotImplementedError(
                        f"Character literals containing both single and double"
                        f" quotes are not supported but found >>{node.value}<<"
                        )
                quote_symbol = '"'
            result = f"{quote_symbol}{node.value}{quote_symbol}"
        elif (node.datatype.intrinsic == ScalarType.Intrinsic.REAL and
              precision == ScalarType.Precision.DOUBLE):
            # The PSyIR stores real scalar values using the standard 'e'
            # notation. If the scalar is in fact double precision then this
            # 'e' must be replaced by 'd' for Fortran.
            result = node.value.replace("e", "d", 1)
        else:
            result = node.value

        if isinstance(precision, DataSymbol):
            # A KIND variable has been specified
            if node.datatype.intrinsic == ScalarType.Intrinsic.CHARACTER:
                result = f"{precision.name}_{result}"
            else:
                result = f"{result}_{precision.name}"
        if isinstance(precision, int):
            # A KIND value has been specified
            if node.datatype.intrinsic == ScalarType.Intrinsic.CHARACTER:
                result = f"{precision}_{result}"
            else:
                result = f"{result}_{precision}"

        return result

    def ifblock_node(self, node):
        '''This method is called when an IfBlock instance is found in the
        PSyIR tree.

        :param node: an IfBlock PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.IfBlock`

        :returns: the Fortran code as a string.
        :rtype: str

        '''
        condition = self._visit(node.children[0])

        self._depth += 1
        if_body = ""
        for child in node.if_body:
            if_body += self._visit(child)
        else_body = ""
        # node.else_body is None if there is no else clause.
        if node.else_body:
            for child in node.else_body:
                else_body += self._visit(child)
        self._depth -= 1

        if else_body:
            result = (
                f"{self._nindent}if ({condition}) then\n"
                f"{if_body}"
                f"{self._nindent}else\n"
                f"{else_body}"
                f"{self._nindent}end if\n")
        else:
            result = (
                f"{self._nindent}if ({condition}) then\n"
                f"{if_body}"
                f"{self._nindent}end if\n")
        return result

    def whileloop_node(self, node):
        '''This method is called when a WhileLoop instance is found in the
        PSyIR tree.

        :param node: a WhileLoop PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.WhileLoop`

        :returns: the Fortran code.
        :rtype: str

        '''
        condition = self._visit(node.condition)

        self._depth += 1
        body = ""
        for child in node.loop_body:
            body += self._visit(child)
        self._depth -= 1

        result = (
            f"{self._nindent}do while ({condition})\n"
            f"{body}"
            f"{self._nindent}end do\n")
        return result

    def loop_node(self, node):
        '''This method is called when a Loop instance is found in the
        PSyIR tree.

        :param node: a Loop PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Loop`

        :returns: the loop node converted into a (language specific) string.
        :rtype: str

        '''
        start = self._visit(node.start_expr)
        stop = self._visit(node.stop_expr)
        step = self._visit(node.step_expr)

        self._depth += 1
        body = ""
        for child in node.loop_body:
            body += self._visit(child)
        self._depth -= 1

        # A generation error is raised if variable is not defined. This
        # happens in LFRic kernel that iterate over a domain.
        try:
            variable_name = node.variable.name
        except GenerationError:
            # If a kernel iterates over a domain - there is
            # no loop. But the loop node is maintained since it handles halo
            # exchanges. So just return the body in this case
            return body

        return (
            f"{self._nindent}do {variable_name} = {start}, {stop}, {step}\n"
            f"{body}"
            f"{self._nindent}enddo\n")

    def unaryoperation_node(self, node):
        '''This method is called when a UnaryOperation instance is found in
        the PSyIR tree.

        :param node: a UnaryOperation PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.UnaryOperation`

        :returns: the Fortran code as a string.
        :rtype: str

        :raises VisitorError: if an unexpected Unary op is encountered.

        '''
        content = self._visit(node.children[0])
        try:
            fort_oper = self.get_operator(node.operator)
            # If the parent node is a UnaryOperation or a BinaryOperation
            # such as '-' or '**' then we need parentheses. This ensures we
            # don't generate invalid Fortran such as 'a ** -b' or 'a - -b'.
            parent = node.parent
            if isinstance(parent, UnaryOperation):
                return f"({fort_oper}{content})"
            if isinstance(parent, BinaryOperation):
                if node is parent.children[1]:
                    return f"({fort_oper}{content})"
            return f"{fort_oper}{content}"

        except KeyError as error:
            raise VisitorError(
                f"Unexpected unary op '{node.operator}'.") from error

    def return_node(self, _):
        '''This method is called when a Return instance is found in
        the PSyIR tree.

        :param node: a Return PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Return`

        :returns: the Fortran code as a string.
        :rtype: str

        '''
        return f"{self._nindent}return\n"

    def codeblock_node(self, node):
        '''This method is called when a CodeBlock instance is found in the
        PSyIR tree. It returns the content of the CodeBlock as a
        Fortran string, indenting as appropriate.

        :param node: a CodeBlock PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.CodeBlock`

        :returns: the Fortran code as a string.
        :rtype: str

        '''
        result = ""
        if node.structure == CodeBlock.Structure.STATEMENT:
            # indent and newlines required
            for ast_node in node.get_ast_nodes:
                # Using tofortran() ensures we get any label associated
                # with this statement.
                result += f"{self._nindent}{ast_node.tofortran()}\n"
        elif node.structure == CodeBlock.Structure.EXPRESSION:
            for ast_node in node.get_ast_nodes:
                result += str(ast_node)
        else:
            raise VisitorError(
                f"Unsupported CodeBlock Structure '{node.structure}' found.")
        return result

    def operandclause_node(self, node):
        '''This method is called when a OperandClause is
        found in the PSyIR tree. It returns the clause and its children
        as a string.

        :param node: an OperandClause PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.OperandClause`

        :returns: the Fortran code for this node.
        :rtype: str

        '''
        if len(node.children) == 0:
            return ""

        result = node.clause_string

        result = result + "(" + node.operand + ": "

        child_list = []
        for child in node.children:
            child_list.append(self._visit(child))

        result = result + ",".join(child_list) + ")"

        return result

    def regiondirective_node(self, node):
        '''This method is called when a RegionDirective instance is found in
        the PSyIR tree. It returns the opening and closing directives, and
        the statements in between as a string.

        :param node: a RegionDirective PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.RegionDirective`

        :returns: the Fortran code for this node.
        :rtype: str

        '''
        result = f"{self._nindent}!${node.begin_string()}"

        clause_list = []
        for clause in node.clauses:
            val = self._visit(clause)
            # Some clauses return empty strings if they should not
            # generate any output (e.g. private clause with no children).
            if val != "":
                clause_list.append(val)
        # Add a space only if there are clauses
        if len(clause_list) > 0:
            result = result + " "
        result = result + ", ".join(clause_list)
        result = result + "\n"

        for child in node.dir_body:
            result = result + self._visit(child)

        end_string = node.end_string()
        if end_string:
            result = result + f"{self._nindent}!${end_string}\n"
        return result

    def standalonedirective_node(self, node):
        '''This method is called when a StandaloneDirective instance is found
        in the PSyIR tree. It returns the directive as a string.

        :param node: a StandaloneDirective PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.StandloneDirective`

        :returns: the Fortran code for this node.
        :rtype: str

        '''
        result = f"{self._nindent}!${node.begin_string()}"

        clause_list = []
        # Currently no standalone directives have clauses associated
        # so this code is left commented out. If a standalone directive
        # is added with clauses, this should be added in.
        # for clause in node.clauses:
        #     clause_list.append(self._visit(clause))
        # Add a space only if there are clauses
        # if len(clause_list) > 0:
        #     result = result + " "
        result = result + ", ".join(clause_list)
        result = result + "\n"

        return result

    def _gen_arguments(self, node):
        '''Utility function that check that all named args occur after all
        positional args. This is a Fortran restriction, not a PSyIR
        restriction. And if they are valid, it returns the whole list of
        arguments.

        :param node: the node to check.
        :type node: :py:class:`psyclone.psyir.nodes.Call`
        :returns: string representation of the complete list of arguments.
        :rtype: str

        raises TypeError: if the provided node is not a Call.
        raises VisitorError: if the all of the positional arguments are
            not before all of the named arguments.

        '''
        if not isinstance(node, Call):
            raise TypeError(
                f"The _gen_arguments utility function expects a "
                f"Call node, but found '{type(node).__name__}'.")

        found_named_arg = False
        for name in node.argument_names:
            if found_named_arg and not name:
                raise VisitorError(
                    f"Fortran expects all named arguments to occur after all "
                    f"positional arguments but this is not the case for "
                    f"{str(node)}")
            if name:
                found_named_arg = True

        # All arguments have been validated, proceed to generate them
        result_list = []
        for idx, child in enumerate(node.children):
            if node.argument_names[idx]:
                result_list.append(
                    f"{node.argument_names[idx]}={self._visit(child)}")
            else:
                result_list.append(self._visit(child))
        return ", ".join(result_list)

    def call_node(self, node):
        '''Translate the PSyIR call node to Fortran.

        :param node: a Call PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Call`

        :returns: the equivalent Fortran code.
        :rtype: str

        '''
        args = self._gen_arguments(node)
        if isinstance(node, IntrinsicCall) and node.routine.name in [
                "ALLOCATE", "DEALLOCATE"]:
            # An allocate/deallocate doesn't have 'call'.
            return f"{self._nindent}{node.routine.name}({args})\n"
        if not node.parent or isinstance(node.parent, Schedule):
            return f"{self._nindent}call {node.routine.name}({args})\n"

        # Otherwise it is inside-expression function call
        return f"{node.routine.name}({args})"

    def kernelfunctor_node(self, node):
        '''
        Translate the Kernel functor into Fortran.

        :param node: the PSyIR node to translate.
        :type node: :py:class:`psyclone.domain.common.algorithm.KernelFunctor`

        :returns: the equivalent Fortran code.
        :rtype: str

        '''
        result_list = []
        for child in node.children:
            result_list.append(self._visit(child))
        args = ", ".join(result_list)
        return f"{node.name}({args})"
