# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2021, Science and Technology Facilities Council
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
# Authors R. W. Ford and S. Siso, STFC Daresbury Lab.
# Modified J. Henrichs, Bureau of Meteorology
# Modified A. R. Porter, STFC Daresbury Lab.
# Modified A. B. G. Chalk, STFC Daresbury Lab.

'''Fortran PSyIR backend. Generates Fortran code from PSyIR
nodes. Currently limited to PSyIR Kernel and NemoInvoke schedules as
PSy-layer PSyIR already has a gen() method to generate Fortran.

'''

# pylint: disable=too-many-lines
from __future__ import absolute_import
import six

from fparser.two import Fortran2003

from psyclone.errors import InternalError
from psyclone.psyir.backend.language_writer import LanguageWriter
from psyclone.psyir.backend.visitor import VisitorError
from psyclone.psyir.frontend.fparser2 import Fparser2Reader, \
    TYPE_MAP_FROM_FORTRAN
from psyclone.psyir.nodes import BinaryOperation, CodeBlock, DataNode, \
    Literal, Operation, Range, Routine, Schedule, UnaryOperation
from psyclone.psyir.symbols import ArgumentInterface, ArrayType, \
    ContainerSymbol, DataSymbol, DataTypeSymbol, RoutineSymbol, ScalarType, \
    Symbol, SymbolTable, UnknownFortranType, UnknownType, UnresolvedInterface

# The list of Fortran instrinsic functions that we know about (and can
# therefore distinguish from array accesses). These are taken from
# fparser.
FORTRAN_INTRINSICS = Fortran2003.Intrinsic_Name.function_names

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
            raise six.raise_from(
                VisitorError("Unsupported access '{0}' found."
                             "".format(str(excinfo))), excinfo)
    else:
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
        return "type({0})".format(datatype.name)

    if (isinstance(datatype, ArrayType) and
            isinstance(datatype.intrinsic, DataTypeSymbol)):
        # Symbol is an array of derived types
        return "type({0})".format(datatype.intrinsic.name)

    try:
        fortrantype = TYPE_MAP_TO_FORTRAN[datatype.intrinsic]
    except KeyError as error:
        raise six.raise_from(NotImplementedError(
            "Unsupported datatype '{0}' for symbol '{1}' found in "
            "gen_datatype().".format(datatype.intrinsic, name)), error)

    precision = datatype.precision

    if isinstance(precision, int):
        if fortrantype not in ['real', 'integer', 'logical']:
            raise VisitorError("Explicit precision not supported for datatype "
                               "'{0}' in symbol '{1}' in Fortran backend."
                               "".format(fortrantype, name))
        if fortrantype == 'real' and precision not in [4, 8, 16]:
            raise VisitorError(
                "Datatype 'real' in symbol '{0}' supports fixed precision of "
                "[4, 8, 16] but found '{1}'.".format(name, precision))
        if fortrantype in ['integer', 'logical'] and precision not in \
           [1, 2, 4, 8, 16]:
            raise VisitorError(
                "Datatype '{0}' in symbol '{1}' supports fixed precision of "
                "[1, 2, 4, 8, 16] but found '{2}'."
                "".format(fortrantype, name, precision))
        # Precision has an an explicit size. Use the "type*size" Fortran
        # extension for simplicity. We could have used
        # type(kind=selected_int|real_kind(size)) or, for Fortran 2008,
        # ISO_FORTRAN_ENV; type(type64) :: MyType.
        return "{0}*{1}".format(fortrantype, precision)

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
                "kind not supported for datatype '{0}' in symbol '{1}' in "
                "Fortran backend.".format(fortrantype, name))
        # The precision information is provided by a parameter, so use KIND.
        return "{0}(kind={1})".format(fortrantype, precision.name)

    raise VisitorError(
        "Unsupported precision type '{0}' found for symbol '{1}' in Fortran "
        "backend.".format(type(precision).__name__, name))


def _reverse_map(op_map):
    '''
    Reverses the supplied fortran2psyir mapping to make a psyir2fortran
    mapping.

    :param op_map: mapping from string representation of operator to \
                   enumerated type.
    :type op_map: :py:class:`collections.OrderedDict`

    :returns: a mapping from PSyIR operation to the equivalent Fortran string.
    :rtype: dict with :py:class:`psyclone.psyir.nodes.Operation.Operator` \
            keys and str values.

    '''
    mapping = {}
    for operator in op_map:
        mapping_key = op_map[operator]
        mapping_value = operator
        # Only choose the first mapping value when there is more
        # than one.
        if mapping_key not in mapping:
            mapping[mapping_key] = mapping_value
    return mapping


def get_fortran_operator(operator):
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
    unary_mapping = _reverse_map(Fparser2Reader.unary_operators)
    if operator in unary_mapping:
        return unary_mapping[operator].upper()

    binary_mapping = _reverse_map(Fparser2Reader.binary_operators)
    if operator in binary_mapping:
        return binary_mapping[operator].upper()

    nary_mapping = _reverse_map(Fparser2Reader.nary_operators)
    if operator in nary_mapping:
        return nary_mapping[operator].upper()
    raise KeyError()


def is_fortran_intrinsic(fortran_operator):
    '''Determine whether the supplied Fortran operator is an intrinsic
    Fortran function or not.

    :param str fortran_operator: the supplied Fortran operator.

    :returns: true if the supplied Fortran operator is a Fortran \
        intrinsic and false otherwise.

    '''
    return fortran_operator in FORTRAN_INTRINSICS


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
    # precedence. Strings within sub-lists have the same precendence
    # apart from the following two caveats. 1) unary + and - have
    # a higher precedence than binary + and -, e.g. -(a-b) !=
    # -a-b and 2) floating point operations are not actually
    # associative due to rounding errors, e.g. potentially (a * b) / c
    # != a * (b / c). Therefore, if a particular ordering is specified
    # then it should be respected. These issues are dealt with in the
    # binaryoperation handler.
    fortran_precedence = [
        ['.EQV.', 'NEQV'],
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


def add_accessibility_to_unknown_declaration(symbol):
    '''
    Utility that manipulates the unknown Fortran declaration for the supplied
    Symbol so as to ensure that it has the correct accessibility specifier.
    (This is required because we capture an 'unknown' Fortran declaration as
    is and this may or may not include accessibility information.)

    :param symbol: the symbol for which the declaration is required.
    :type symbol: :py:class:`psyclone.psyir.symbols.Symbol`

    :returns: Fortran declaration of the supplied symbol with accessibility \
        information included (public/private).
    :rtype: str

    :raises TypeError: if the supplied argument is not a Symbol of \
        UnknownFortranType.
    :raises InternalError: if the declaration associated with the Symbol is \
        empty.
    :raises NotImplementedError: if the original declaration does not use \
        '::' to separate the entity name from its type.
    :raises InternalError: if the declaration stored for the supplied symbol \
        contains accessibility information which does not match the \
        visibility of the supplied symbol.

    '''
    if not isinstance(symbol, Symbol):
        raise TypeError("Expected a Symbol but got '{0}'".format(
            type(symbol).__name__))

    if not isinstance(symbol.datatype, UnknownFortranType):
        raise TypeError("Expected a Symbol of UnknownFortranType but symbol "
                        "'{0}' has type '{1}'".format(symbol.name,
                                                      symbol.datatype))

    if not symbol.datatype.declaration:
        raise InternalError(
            "Symbol '{0}' is of UnknownFortranType but the associated "
            "declaration text is empty.".format(symbol.name))

    # The original declaration text is obtained from fparser2 and will
    # already have had any line-continuation symbols removed.
    first_line = symbol.datatype.declaration.split("\n")[0]
    if "::" not in first_line:
        raise NotImplementedError(
            "Cannot add accessibility information to an UnknownFortranType "
            "that does not have '::' in its original declaration: '{0}'".
            format(symbol.datatype.declaration))

    parts = symbol.datatype.declaration.split("::")
    first_part = parts[0].lower()
    if symbol.visibility == Symbol.Visibility.PUBLIC:
        if "public" not in first_part:
            if "private" in first_part:
                raise InternalError(
                    "Symbol '{0}' of UnknownFortranType has public visibility "
                    "but its associated declaration specifies that it is "
                    "private: '{1}'".format(symbol.name,
                                            symbol.datatype.declaration))
            first_part = first_part.rstrip() + ", public "
    else:
        if "private" not in first_part:
            if "public" in first_part:
                raise InternalError(
                    "Symbol '{0}' of UnknownFortranType has private "
                    "visibility but its associated declaration specifies that "
                    "it is public: '{1}'".format(symbol.name,
                                                 symbol.datatype.declaration))
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
        super(FortranWriter, self).__init__(("(", ")"), "%", skip_nodes,
                                            indent_string,
                                            initial_indent_depth,
                                            check_global_constraints)

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
                    "unsupported gen_indices index '{0}'".format(str(index)))
        return dims

    def gen_use(self, symbol, symbol_table):
        ''' Performs consistency checks and then creates and returns the
        Fortran use statement(s) for this ContainerSymbol as required for
        the supplied symbol table. If this symbol has both a wildcard import
        and explicit imports then two use statements are generated. (This
        means that when generating Fortran from PSyIR created from Fortran
        code, we replicate the structure of the original.)

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
                "gen_use() expects a ContainerSymbol as its first argument "
                "but got '{0}'".format(type(symbol).__name__))
        if not isinstance(symbol_table, SymbolTable):
            raise VisitorError(
                "gen_use() expects a SymbolTable as its second argument but "
                "got '{0}'".format(type(symbol_table).__name__))
        if symbol.name not in symbol_table:
            raise VisitorError("gen_use() - the supplied symbol ('{0}') is not"
                               " in the supplied SymbolTable.".format(
                                   symbol.name))
        if symbol_table.lookup(symbol.name) is not symbol:
            raise VisitorError(
                "gen_use() - the supplied symbol ('{0}') is not the same "
                "object as the entry with that name in the supplied "
                "SymbolTable.".format(symbol.name))

        # Construct the list of symbol names for the ONLY clause
        only_list = [dsym.name for dsym in
                     symbol_table.symbols_imported_from(symbol)]

        # Finally construct the use statements for this Container (module)
        if not only_list and not symbol.wildcard_import:
            # We have a "use xxx, only:" - i.e. an empty only list
            return "{0}use {1}, only :\n".format(self._nindent, symbol.name)
        use_stmts = ""
        if only_list:
            use_stmts = "{0}use {1}, only : {2}\n".format(
                self._nindent, symbol.name, ", ".join(sorted(only_list)))
        # It's possible to have both explicit and wildcard imports from the
        # same Fortran module.
        if symbol.wildcard_import:
            use_stmts += "{0}use {1}\n".format(self._nindent, symbol.name)
        return use_stmts

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

        :raises VisitorError: if the symbol is of UnknownFortranType and \
            is not local.
        :raises VisitorError: if the symbol is of known type but does not \
            specify a variable declaration (it is not a local declaration or \
            an argument declaration).
        :raises VisitorError: if the symbol or member is an array with a \
            shape containing a mixture of DEFERRED and other extents.
        :raises InternalError: if visibility is to be included but is not \
            either PUBLIC or PRIVATE.

        '''
        # pylint: disable=too-many-branches
        # Whether we're dealing with a Symbol or a member of a derived type
        is_symbol = isinstance(symbol, (DataSymbol, RoutineSymbol))
        # Whether we're dealing with an array declaration and, if so, the
        # shape of that array.
        if isinstance(symbol.datatype, ArrayType):
            array_shape = symbol.datatype.shape
        else:
            array_shape = []

        if is_symbol:
            if isinstance(symbol.datatype, UnknownFortranType):
                if isinstance(symbol, RoutineSymbol) and not symbol.is_local:
                    raise VisitorError(
                        "{0} '{1}' is of UnknownFortranType but has"
                        " interface '{2}' instead of LocalInterface. This is "
                        "not supported by the Fortran back-end.".format(
                            type(symbol).__name__,
                            symbol.name, symbol.interface))
            elif not (symbol.is_local or symbol.is_argument):
                raise VisitorError(
                    "gen_vardecl requires the symbol '{0}' to have a Local or "
                    "an Argument interface but found a '{1}' interface."
                    "".format(symbol.name, type(symbol.interface).__name__))

        if isinstance(symbol.datatype, UnknownType):
            if isinstance(symbol.datatype, UnknownFortranType):
                if include_visibility and not isinstance(symbol,
                                                         RoutineSymbol):
                    decln = add_accessibility_to_unknown_declaration(symbol)
                else:
                    decln = symbol.datatype.declaration
                return "{0}{1}\n".format(self._nindent, decln)
            # The Fortran backend only handles unknown *Fortran* declarations.
            raise VisitorError(
                "{0} '{1}' is of '{2}' type. This is not supported by the "
                "Fortran backend.".format(type(symbol).__name__, symbol.name,
                                          type(symbol.datatype).__name__))

        datatype = gen_datatype(symbol.datatype, symbol.name)
        result = "{0}{1}".format(self._nindent, datatype)

        if ArrayType.Extent.DEFERRED in array_shape:
            if not all(dim == ArrayType.Extent.DEFERRED
                       for dim in array_shape):
                raise VisitorError(
                    "A Fortran declaration of an allocatable array must have"
                    " the extent of every dimension as 'DEFERRED' but "
                    "symbol '{0}' has shape: {1}.".format(
                        symbol.name, self.gen_indices(array_shape)))
            # A 'deferred' array extent means this is an allocatable array
            result += ", allocatable"
        if ArrayType.Extent.ATTRIBUTE in array_shape:
            if not all(dim == ArrayType.Extent.ATTRIBUTE
                       for dim in symbol.datatype.shape):
                # If we have an 'assumed-size' array then only the last
                # dimension is permitted to have an 'ATTRIBUTE' extent
                if (array_shape.count(ArrayType.Extent.ATTRIBUTE) != 1 or
                        array_shape[-1] != ArrayType.Extent.ATTRIBUTE):
                    raise VisitorError(
                        "An assumed-size Fortran array must only have its "
                        "last dimension unspecified (as 'ATTRIBUTE') but "
                        "symbol '{0}' has shape: {1}."
                        "".format(symbol.name, self.gen_indices(array_shape)))
        if array_shape:
            dims = self.gen_indices(array_shape)
            result += ", dimension({0})".format(",".join(dims))
        if is_symbol:
            # A member of a derived type cannot have the 'intent' or
            # 'parameter' attribute.
            intent = gen_intent(symbol)
            if intent:
                result += ", intent({0})".format(intent)
            if symbol.is_constant:
                result += ", parameter"

        if include_visibility:
            if symbol.visibility == Symbol.Visibility.PRIVATE:
                result += ", private"
            elif symbol.visibility == Symbol.Visibility.PUBLIC:
                result += ", public"
            else:
                raise InternalError(
                    "A Symbol must be either public or private but symbol "
                    "'{0}' has visibility '{1}'".format(symbol.name,
                                                        symbol.visibility))

        result += " :: {0}".format(symbol.name)
        if is_symbol and symbol.is_constant:
            result += " = {0}".format(self._visit(symbol.constant_value))
        result += "\n"
        return result

    def gen_typedecl(self, symbol, include_visibility=True):
        '''
        Creates a derived-type declaration for the supplied DataTypeSymbol.

        :param symbol: the derived-type to declare.
        :type symbol: :py:class:`psyclone.psyir.symbols.DataTypeSymbol`
        :param bool include_visibility: whether or not to include visibility \
            information in the declaration. (Default is True.)

        :returns: the Fortran declaration of the derived type.
        :rtype: str

        :raises VisitorError: if the supplied symbol is not a DataTypeSymbol.
        :raises VisitorError: if the datatype of the symbol is of UnknownType \
            but is not of UnknownFortranType.
        :raises InternalError: if include_visibility is True and the \
            visibility of the symbol is not of the correct type.

        '''
        if not isinstance(symbol, DataTypeSymbol):
            raise VisitorError(
                "gen_typedecl expects a DataTypeSymbol as argument but "
                "got: '{0}'".format(type(symbol).__name__))

        if isinstance(symbol.datatype, UnknownType):
            if isinstance(symbol.datatype, UnknownFortranType):
                # This is a declaration of unknown type. We have to ensure
                # that its visibility is correctly specified though.
                if include_visibility:
                    decln = add_accessibility_to_unknown_declaration(symbol)
                else:
                    decln = symbol.datatype.declaration
                return "{0}{1}\n".format(self._nindent, decln)

            raise VisitorError(
                "Fortran backend cannot generate code for symbol '{0}' of "
                "type '{1}'".format(symbol.name,
                                    type(symbol.datatype).__name__))

        result = "{0}type".format(self._nindent)

        if include_visibility:
            if symbol.visibility == Symbol.Visibility.PRIVATE:
                result += ", private"
            elif symbol.visibility == Symbol.Visibility.PUBLIC:
                result += ", public"
            else:
                raise InternalError(
                    "A Symbol's visibility must be one of Symbol.Visibility."
                    "PRIVATE/PUBLIC but '{0}' has visibility of type '{1}'".
                    format(symbol.name, type(symbol.visibility).__name__))
        result += " :: {0}\n".format(symbol.name)

        self._depth += 1
        for member in symbol.datatype.components.values():
            # We always want to specify the visibility of components within
            # a derived type.
            result += self.gen_vardecl(member, include_visibility=True)
        self._depth -= 1

        result += "{0}end type {1}\n".format(self._nindent, symbol.name)
        return result

    def gen_access_stmt(self, symbol_table):
        '''
        Generates the access statement for a module - either "private" or
        "public". Although the PSyIR captures the visibility of every Symbol
        explicitly, this information is required in order
        to ensure the correct visibility of symbols that have been imported
        into the current module from another one using a wildcard import
        (i.e. a `use` without an `only` clause) and also for those Symbols
        that are of UnknownFortranType (because their declaration may or may
        not include visibility information).

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
            "Unrecognised visibility ('{0}') found when attempting to generate"
            " access statement. Should be either 'Symbol.Visibility.PUBLIC' "
            "or 'Symbol.Visibility.PRIVATE'\n".format(
                str(symbol_table.default_visibility)))

    def gen_routine_access_stmts(self, symbol_table):
        '''
        Creates the accessibility statements (R518) for any routine symbols
        in the supplied symbol table.

        :param symbol_table: the symbol table for which to generate \
                             accessibility statements.
        :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable`

        :returns: the accessibility statements for any routine symbols.
        :rtype: str

        :raises InternalError: if a Routine symbol with an unrecognised \
                               visibility is encountered.
        '''

        # Find the symbol that represents itself, this one will not need
        # an accessibility statement
        try:
            itself = symbol_table.lookup_with_tag('own_routine_symbol')
        except KeyError:
            itself = None

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
                    public_routines.append(symbol.name)
                elif symbol.visibility == Symbol.Visibility.PRIVATE:
                    private_routines.append(symbol.name)
                else:
                    raise InternalError(
                        "Unrecognised visibility ('{0}') found for symbol "
                        "'{1}'. Should be either 'Symbol.Visibility.PUBLIC' "
                        "or 'Symbol.Visibility.PRIVATE'.".format(
                            str(symbol.visibility), symbol.name))
        result = "\n"
        if public_routines:
            result += "{0}public :: {1}\n".format(self._nindent,
                                                  ", ".join(public_routines))
        if private_routines:
            result += "{0}private :: {1}\n".format(self._nindent,
                                                   ", ".join(private_routines))
        if len(result) > 1:
            return result
        return ""

    def gen_decls(self, symbol_table, is_module_scope=False):
        '''Create and return the Fortran declarations for the supplied
        SymbolTable.

        :param symbol_table: the SymbolTable instance.
        :type symbol: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :param bool is_module_scope: whether or not the declarations are in \
                                     a module scoping unit. Default is False.

        :returns: the Fortran declarations as a string.
        :rtype: str

        :raises VisitorError: if one of the symbols is a RoutineSymbol \
            which does not have an ImportInterface or LocalInterface (and is \
            not a Fortran intrinsic) as this is not supported by this backend.
        :raises VisitorError: if args_allowed is False and one or more \
            argument declarations exist in symbol_table.
        :raises VisitorError: if there are any symbols in the supplied table \
            that do not have an explicit declaration and there are no \
            wildcard imports.

        '''
        # pylint: disable=too-many-branches
        declarations = ""
        # Keep a record of whether we've already checked for any wildcard
        # imports to save doing so repeatedly
        wildcard_imports_checked = False
        has_wildcard_import = False

        routine_symbols = [symbol for symbol in symbol_table.symbols
                           if isinstance(symbol, RoutineSymbol)]
        for sym in routine_symbols:
            if (isinstance(sym.interface, UnresolvedInterface) and
                    sym.name.upper() not in FORTRAN_INTRINSICS):
                if not wildcard_imports_checked:
                    has_wildcard_import = symbol_table.has_wildcard_imports()
                    wildcard_imports_checked = True
                if not has_wildcard_import:
                    raise VisitorError(
                        "Routine symbol '{0}' does not have an ImportInterface"
                        " or LocalInterface, is not a Fortran intrinsic and "
                        "there is no wildcard import which could bring it into"
                        " scope. This is not supported by the Fortran "
                        "back-end.".format(sym.name))
            if isinstance(sym.interface, ArgumentInterface):
                raise VisitorError(
                    "Routine symbol '{0}' is passed as an argument (has an "
                    "ArgumentInterface). This is not supported by the Fortran"
                    " back-end.".format(sym.name))
            # Interfaces to module procedures are captured by the frontend as
            # RoutineSymbols of UnknownFortranType. These must therefore be
            # declared.
            if isinstance(sym.datatype, UnknownType):
                declarations += self.gen_vardecl(
                    sym, include_visibility=is_module_scope)

        # Does the symbol table contain any symbols with a deferred
        # interface (i.e. we don't know how they are brought into scope)
        unresolved_datasymbols = symbol_table.get_unresolved_datasymbols()

        if unresolved_datasymbols:
            # We do have unresolved symbols. Is there at least one wildcard
            # import which could be bringing them into scope?
            if not wildcard_imports_checked:
                has_wildcard_import = symbol_table.has_wildcard_imports()
                wildcard_imports_checked = True
            if not has_wildcard_import:
                symbols_txt = ", ".join(
                    ["'" + sym + "'" for sym in unresolved_datasymbols])
                raise VisitorError(
                    "The following symbols are not explicitly declared or "
                    "imported from a module and there are no wildcard imports "
                    "which could be bringing them into scope: {0}".format(
                        symbols_txt))

        # Fortran requires use statements to be specified before
        # variable declarations. As a convention, this method also
        # declares any argument variables before local variables.

        # 1: Argument variable declarations
        if symbol_table.argument_datasymbols and is_module_scope:
            raise VisitorError(
                "Arguments are not allowed in this context but this symbol "
                "table contains argument(s): '{0}'."
                "".format([symbol.name for symbol in
                           symbol_table.argument_datasymbols]))
        for symbol in symbol_table.argument_datasymbols:
            declarations += self.gen_vardecl(
                symbol, include_visibility=is_module_scope)

        # 2: Local constants.
        local_constants = [sym for sym in symbol_table.local_datasymbols if
                           sym.is_constant]
        for symbol in local_constants:
            declarations += self.gen_vardecl(
                symbol, include_visibility=is_module_scope)

        # 3: Derived-type declarations. These must come before any declarations
        #    of symbols of these types.
        for symbol in symbol_table.local_datatypesymbols:
            declarations += self.gen_typedecl(
                symbol, include_visibility=is_module_scope)

        # 4: Local variable declarations.
        local_vars = [sym for sym in symbol_table.local_datasymbols if not
                      sym.is_constant]
        for symbol in local_vars:
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
                "In the Fortran backend, a file container should not have "
                "any symbols associated with it, but found {0}."
                "".format(len(node.symbol_table.symbols)))

        program_nodes = len([child for child in node.children if
                             isinstance(child, Routine) and child.is_program])
        if program_nodes > 1:
            raise VisitorError(
                "In the Fortran backend, a file container should contain at "
                "most one routine node that is a program, but found {0}."
                "".format(program_nodes))

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
                "The Fortran back-end requires all children of a Container "
                "to be either CodeBlocks or sub-classes of Routine but found: "
                "{0}.".format(
                    [type(child).__name__ for child in node.children]))

        result = "{0}module {1}\n".format(self._nindent, node.name)

        self._depth += 1

        # Generate module imports
        imports = ""
        for symbol in node.symbol_table.containersymbols:
            imports += self.gen_use(symbol, node.symbol_table)

        # Declare the Container's data
        declarations = self.gen_decls(node.symbol_table, is_module_scope=True)

        # Generate the access statement (PRIVATE or PUBLIC)
        declarations += self.gen_access_stmt(node.symbol_table)

        # Accessibility statements for routine symbols
        declarations += self.gen_routine_access_stmts(node.symbol_table)

        # Get the subroutine statements.
        subroutines = ""
        for child in node.children:
            subroutines += self._visit(child)

        result += (
            "{1}"
            "{0}implicit none\n"
            "{2}\n"
            "{0}contains\n"
            "{3}\n"
            "".format(self._nindent, imports, declarations, subroutines))

        self._depth -= 1
        result += "{0}end module {1}\n".format(self._nindent, node.name)
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
            result = ("{0}program {1}\n".format(self._nindent, node.name))
            routine_type = "program"
        else:
            args = [symbol.name for symbol in node.symbol_table.argument_list]
            suffix = ""
            if node.return_symbol:
                # This Routine has a return value and is therefore a Function
                routine_type = "function"
                if node.return_symbol.name.lower() != node.name.lower():
                    suffix = " result({0})".format(node.return_symbol.name)
            else:
                routine_type = "subroutine"
            result = "{0}{1} {2}({3}){4}\n".format(self._nindent, routine_type,
                                                   node.name, ", ".join(args),
                                                   suffix)

        self._depth += 1

        # The PSyIR has nested scopes but Fortran only supports declaring
        # variables at the routine level scope. For this reason, at this
        # point we have to unify all declarations and resolve possible name
        # clashes that appear when merging the scopes.
        whole_routine_scope = SymbolTable(node)

        for schedule in node.walk(Schedule):
            for symbol in schedule.symbol_table.symbols[:]:
                try:
                    whole_routine_scope.add(symbol)
                except KeyError:
                    new_name = whole_routine_scope.next_available_name(
                        symbol.name)
                    while True:
                        # Ensure that the new name isn't already in the current
                        # symbol table.
                        local_name = schedule.symbol_table.next_available_name(
                            new_name)
                        if local_name == new_name:
                            # new_name is availble in the current symbol table
                            # so we're done.
                            break
                        # new_name clashed with an entry in the current symbol
                        # table so try again.
                        new_name = whole_routine_scope.next_available_name(
                            local_name)
                    schedule.symbol_table.rename_symbol(symbol, new_name)
                    whole_routine_scope.add(symbol)

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
            "{0}"
            "{1}\n"
            "{2}\n"
            "".format(imports, declarations, exec_statements))

        self._depth -= 1
        result += (
            "{0}end {1} {2}\n"
            "".format(self._nindent, routine_type, node.name))

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
        result = "{0}{1} = {2}\n".format(self._nindent, lhs, rhs)
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
            fort_oper = get_fortran_operator(node.operator)
            if is_fortran_intrinsic(fort_oper):
                # This is a binary intrinsic function.
                return "{0}({1}, {2})".format(fort_oper, lhs, rhs)
            parent = node.parent
            if isinstance(parent, Operation):
                # We may need to enforce precedence
                parent_fort_oper = get_fortran_operator(parent.operator)
                if not is_fortran_intrinsic(parent_fort_oper):
                    # We still may need to enforce precedence
                    if precedence(fort_oper) < precedence(parent_fort_oper):
                        # We need brackets to enforce precedence
                        return "({0} {1} {2})".format(lhs, fort_oper, rhs)
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
                            return "({0} {1} {2})".format(lhs, fort_oper, rhs)
            return "{0} {1} {2}".format(lhs, fort_oper, rhs)
        except KeyError as error:
            raise six.raise_from(VisitorError("Unexpected binary op '{0}'."
                                              "".format(node.operator)),
                                 error)

    def naryoperation_node(self, node):
        '''This method is called when an NaryOperation instance is found in
        the PSyIR tree.

        :param node: an NaryOperation PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.NaryOperation`

        :returns: the Fortran code as a string.
        :rtype: str

        :raises VisitorError: if an unexpected N-ary operator is found.

        '''
        arg_list = []
        for child in node.children:
            arg_list.append(self._visit(child))
        try:
            fort_oper = get_fortran_operator(node.operator)
            return "{0}({1})".format(fort_oper, ", ".join(arg_list))
        except KeyError as error:
            raise six.raise_from(VisitorError("Unexpected N-ary op '{0}'".
                                              format(node.operator)),
                                 error)

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
        result = "{0}:{1}".format(start, stop)

        if isinstance(node.step, Literal) and \
           node.step.datatype.intrinsic == ScalarType.Intrinsic.INTEGER and \
           node.step.value == "1":
            # Step is 1. This is the default in Fortran so no need to
            # output any text.
            pass
        else:
            step = self._visit(node.step)
            result += ":{0}".format(step)
        return result

    # pylint: disable=no-self-use
    def literal_node(self, node):
        '''This method is called when a Literal instance is found in the PSyIR
        tree.

        :param node: a Literal PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Literal`

        :returns: the Fortran code for the literal.
        :rtype: str

        '''
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
                        "Character literals containing both single and double "
                        "quotes are not supported but found >>{0}<<".format(
                            node.value))
                quote_symbol = '"'
            result = quote_symbol + "{0}".format(node.value) + quote_symbol
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
                result = "{0}_{1}".format(precision.name, result)
            else:
                result = "{0}_{1}".format(result, precision.name)
        if isinstance(precision, int):
            # A KIND value has been specified
            if node.datatype.intrinsic == ScalarType.Intrinsic.CHARACTER:
                result = "{0}_{1}".format(precision, result)
            else:
                result = "{0}_{1}".format(result, precision)

        return result

    # pylint: enable=no-self-use
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
                "{0}if ({1}) then\n"
                "{2}"
                "{0}else\n"
                "{3}"
                "{0}end if\n"
                "".format(self._nindent, condition, if_body, else_body))
        else:
            result = (
                "{0}if ({1}) then\n"
                "{2}"
                "{0}end if\n"
                "".format(self._nindent, condition, if_body))
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
        variable_name = node.variable.name

        self._depth += 1
        body = ""
        for child in node.loop_body:
            body += self._visit(child)
        self._depth -= 1

        return "{0}do {1} = {2}, {3}, {4}\n"\
               "{5}"\
               "{0}enddo\n".format(self._nindent, variable_name,
                                   start, stop, step, body)

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
            fort_oper = get_fortran_operator(node.operator)
            if is_fortran_intrinsic(fort_oper):
                # This is a unary intrinsic function.
                return "{0}({1})".format(fort_oper, content)
            # It's not an intrinsic function so we need to consider the
            # parent node. If that is a UnaryOperation or a BinaryOperation
            # such as '-' or '**' then we need parentheses. This ensures we
            # don't generate invalid Fortran such as 'a ** -b' or 'a - -b'.
            parent = node.parent
            if isinstance(parent, UnaryOperation):
                parent_fort_oper = get_fortran_operator(parent.operator)
                if not is_fortran_intrinsic(parent_fort_oper):
                    return "({0}{1})".format(fort_oper, content)
            if isinstance(parent, BinaryOperation):
                parent_fort_oper = get_fortran_operator(parent.operator)
                if (not is_fortran_intrinsic(parent_fort_oper) and
                        node is parent.children[1]):
                    return "({0}{1})".format(fort_oper, content)
            return "{0}{1}".format(fort_oper, content)

        except KeyError as error:
            raise six.raise_from(VisitorError("Unexpected unary op '{0}'."
                                              .format(node.operator)), error)

    def return_node(self, _):
        '''This method is called when a Return instance is found in
        the PSyIR tree.

        :param node: a Return PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Return`

        :returns: the Fortran code as a string.
        :rtype: str

        '''
        return "{0}return\n".format(self._nindent)

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
                result += "{0}{1}\n".format(self._nindent,
                                            ast_node.tofortran())
        elif node.structure == CodeBlock.Structure.EXPRESSION:
            for ast_node in node.get_ast_nodes:
                result += str(ast_node)
        else:
            raise VisitorError(
                ("Unsupported CodeBlock Structure '{0}' found."
                 "".format(node.structure)))
        return result

    def nemokern_node(self, node):
        '''NEMO kernels are a group of nodes collected into a schedule
        so simply call the nodes in the schedule.

        :param node: a NemoKern PSyIR node.
        :type node: :py:class:`psyclone.nemo.NemoKern`

        :returns: the Fortran code as a string.
        :rtype: str

        '''
        result = ""
        schedule = node.get_kernel_schedule()
        for child in schedule.children:
            result += self._visit(child)
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
        result_list = ["{0}!${1}\n".format(self._nindent, node.begin_string())]
        for child in node.dir_body:
            result_list.append(self._visit(child))

        end_string = node.end_string()
        if end_string:
            result_list.append("{0}!${1}\n".format(self._nindent, end_string))
        return "".join(result_list)

    def standalonedirective_node(self, node):
        '''This method is called when a StandaloneDirective instance is found
        in the PSyIR tree. It returns the directive as a string.

        :param node: a StandaloneDirective PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.StandloneDirective`

        :returns: the Fortran code for this node.
        :rtype: str

        '''
        result_list = ["{0}!${1}\n".format(self._nindent, node.begin_string())]

        return "".join(result_list)

    def call_node(self, node):
        '''Translate the PSyIR call node to Fortran.

        :param node: a Call PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Call`

        :returns: the Fortran code as a string.
        :rtype: str

        '''

        result_list = []
        for child in node.children:
            result_list.append(self._visit(child))
        args = ", ".join(result_list)
        if not node.parent or isinstance(node.parent, Schedule):
            return "{0}call {1}({2})\n".format(self._nindent,
                                               node.routine.name,
                                               args)
        # Otherwise it is inside-expression function call
        return "{0}({1})".format(node.routine.name, args)
