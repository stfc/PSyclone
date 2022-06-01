# BSD 3-Clause License
#
# Copyright (c) 2017-2022, Science and Technology Facilities Council.
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
# Authors: R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
#          J. Henrichs, Bureau of Meteorology
#          I. Kavcic, Met Office
# -----------------------------------------------------------------------------

''' This module provides the fparser2 to PSyIR front-end, it follows a
    Visitor Pattern to traverse relevant fparser2 nodes and contains the logic
    to transform each node into the equivalent PSyIR representation.'''

from collections import OrderedDict
import six
from fparser.two import Fortran2003
from fparser.two.utils import walk, BlockBase, StmtBase
from psyclone.errors import InternalError, GenerationError
from psyclone.psyir.nodes import UnaryOperation, BinaryOperation, \
    NaryOperation, Schedule, CodeBlock, IfBlock, Reference, Literal, Loop, \
    Container, Assignment, Return, ArrayReference, Node, Range, \
    KernelSchedule, StructureReference, ArrayOfStructuresReference, \
    Call, Routine, Member, FileContainer, Directive, ArrayMember
from psyclone.psyir.nodes.array_mixin import ArrayMixin
from psyclone.psyir.nodes.array_of_structures_mixin import \
    ArrayOfStructuresMixin
from psyclone.psyir.symbols import SymbolError, DataSymbol, ContainerSymbol, \
    Symbol, ImportInterface, ArgumentInterface, UnresolvedInterface, \
    LocalInterface, ScalarType, ArrayType, DeferredType, UnknownType, \
    UnknownFortranType, StructureType, DataTypeSymbol, RoutineSymbol, \
    SymbolTable, NoType, INTEGER_TYPE

#: The list of Fortran instrinsic functions that we know about (and can
#: therefore distinguish from array accesses). These are taken from
#: fparser.
FORTRAN_INTRINSICS = Fortran2003.Intrinsic_Name.function_names

#: Mapping from Fortran data types to PSyIR types
TYPE_MAP_FROM_FORTRAN = {"integer": ScalarType.Intrinsic.INTEGER,
                         "character": ScalarType.Intrinsic.CHARACTER,
                         "logical": ScalarType.Intrinsic.BOOLEAN,
                         "real": ScalarType.Intrinsic.REAL,
                         "double precision": ScalarType.Intrinsic.REAL}

#: Mapping from Fortran access specifiers to PSyIR visibilities
VISIBILITY_MAP_FROM_FORTRAN = {"public": Symbol.Visibility.PUBLIC,
                               "private": Symbol.Visibility.PRIVATE}

#: Mapping from fparser2 Fortran Literal types to PSyIR types
CONSTANT_TYPE_MAP = {
    Fortran2003.Real_Literal_Constant: ScalarType.Intrinsic.REAL,
    Fortran2003.Logical_Literal_Constant: ScalarType.Intrinsic.BOOLEAN,
    Fortran2003.Char_Literal_Constant: ScalarType.Intrinsic.CHARACTER,
    Fortran2003.Int_Literal_Constant: ScalarType.Intrinsic.INTEGER}

#: Mapping from Fortran intent to PSyIR access type
INTENT_MAPPING = {"in": ArgumentInterface.Access.READ,
                  "out": ArgumentInterface.Access.WRITE,
                  "inout": ArgumentInterface.Access.READWRITE}


def _first_type_match(nodelist, typekind):
    '''Returns the first instance of the specified type in the given
    node list.

    :param list nodelist: list of fparser2 nodes.
    :param type typekind: the fparser2 Type we are searching for.

    :returns: the first instance of the specified type.
    :rtype: instance of typekind

    :raises ValueError: if the list does not contain an object of type \
        typekind.

    '''
    for node in nodelist:
        if isinstance(node, typekind):
            return node
    raise ValueError  # Type not found


def _find_or_create_imported_symbol(location, name, scope_limit=None,
                                    **kargs):
    '''Returns the symbol with the name 'name' from a symbol table
    associated with this node or one of its ancestors.  If a symbol is found
    and the `symbol_type` keyword argument is supplied then the type of the
    existing symbol is compared with the specified type. If it is not already
    an instance of this type, then the symbol is specialised (in place).

    If the symbol is not found and there are no ContainerSymbols with wildcard
    imports then an exception is raised. However, if there are one or more
    ContainerSymbols with wildcard imports (which could therefore be
    bringing the symbol into scope) then a new Symbol with the
    specified visibility but of unknown interface is created and
    inserted in the most local SymbolTable that has such an import.
    The scope_limit variable further limits the symbol table search so
    that the search through ancestor nodes stops when the scope_limit node
    is reached i.e. ancestors of the scope_limit node are not searched.

    :param location: PSyIR node from which to operate.
    :type location: :py:class:`psyclone.psyir.nodes.Node`
    :param str name: the name of the symbol.
    :param scope_limit: optional Node which limits the symbol \
        search space to the symbol tables of the nodes within the \
        given scope. If it is None (the default), the whole \
        scope (all symbol tables in ancestor nodes) is searched \
        otherwise ancestors of the scope_limit node are not \
        searched.
    :type scope_limit: :py:class:`psyclone.psyir.nodes.Node` or \
        `NoneType`

    :returns: the matching symbol.
    :rtype: :py:class:`psyclone.psyir.symbols.Symbol`

    :raises TypeError: if the supplied scope_limit is not a Node.
    :raises ValueError: if the supplied scope_limit node is not an \
        ancestor of the supplied node.
    :raises SymbolError: if no matching symbol is found and there are \
        no ContainerSymbols from which it might be brought into scope.

    '''
    if not isinstance(location, Node):
        raise TypeError(
            f"The location argument '{location}' provided to "
            f"_find_or_create_imported_symbol() is not of type `Node`.")

    if scope_limit is not None:
        # Validate the supplied scope_limit
        if not isinstance(scope_limit, Node):
            raise TypeError(
                f"The scope_limit argument '{scope_limit}' provided to "
                f"_find_or_create_imported_symbol() is not of type `Node`.")

        # Check that the scope_limit Node is an ancestor of this
        # Reference Node and raise an exception if not.
        mynode = location.parent
        while mynode is not None:
            if mynode is scope_limit:
                # The scope_limit node is an ancestor of the
                # supplied node.
                break
            mynode = mynode.parent
        else:
            # The scope_limit node is not an ancestor of the
            # supplied node so raise an exception.
            raise ValueError(
                f"The scope_limit node '{scope_limit}' provided to "
                f"_find_or_create_imported_symbol() is not an ancestor of this"
                f" node '{location}'.")

    # Keep a reference to the most local SymbolTable with a wildcard
    # import in case we need to create a Symbol.
    first_symbol_table = None
    test_node = location

    # Iterate over ancestor Nodes of this Node.
    while test_node:
        # For simplicity, test every Node for the existence of a
        # SymbolTable (rather than checking for the particular
        # Node types which we know to have SymbolTables).
        if hasattr(test_node, 'symbol_table'):
            # This Node does have a SymbolTable.
            symbol_table = test_node.symbol_table

            try:
                # If the name matches a Symbol in this SymbolTable then
                # return the Symbol (after specialising it, if necessary).
                sym = symbol_table.lookup(name, scope_limit=test_node)
                if "symbol_type" in kargs:
                    expected_type = kargs.pop("symbol_type")
                    if not isinstance(sym, expected_type):
                        # The caller specified a sub-class so we need to
                        # specialise the existing symbol.
                        sym.specialise(expected_type, **kargs)
                return sym
            except KeyError:
                # The supplied name does not match any Symbols in
                # this SymbolTable. Does this SymbolTable have any
                # wildcard imports?
                if first_symbol_table is None:
                    for csym in symbol_table.containersymbols:
                        if csym.wildcard_import:
                            first_symbol_table = symbol_table
                            break

        if test_node is scope_limit:
            # The ancestor scope/top-level Node has been reached and
            # nothing has matched.
            break

        # Move on to the next ancestor.
        test_node = test_node.parent

    if first_symbol_table:
        # No symbol found but there are one or more Containers from which
        # it may be being brought into scope. Therefore create a generic
        # Symbol with a deferred interface and add it to the most
        # local SymbolTable with a wildcard import.
        return first_symbol_table.new_symbol(
                name, interface=UnresolvedInterface(), **kargs)

    # All requested Nodes have been checked but there has been no
    # match and there are no wildcard imports so raise an exception.
    raise SymbolError(f"No Symbol found for name '{name}'.")


def _check_args(array, dim):
    '''Utility routine used by the _check_bound_is_full_extent and
    _check_array_range_literal functions to check common arguments.

    This routine is only in fparser2.py until #717 is complete as it
    is used to check that array syntax in a where statement is for the
    full extent of the dimension. Once #717 is complete this routine
    can be removed.

    :param array: the node to check.
    :type array: :py:class:`pysclone.psyir.node.array`
    :param int dim: the dimension index to use.

    :raises TypeError: if the supplied arguments are of the wrong type.
    :raises ValueError: if the value of the supplied dim argument is \
        less than 1 or greater than the number of dimensions in the \
        supplied array argument.

    '''
    if not isinstance(array, ArrayMixin):
        raise TypeError(
            f"method _check_args 'array' argument should be some sort of "
            f"array access (i.e. a sub-class of ArrayMixin) but found "
            f"'{type(array).__name__}'.")

    if not isinstance(dim, int):
        raise TypeError(
            f"method _check_args 'dim' argument should be an "
            f"int type but found '{type(dim).__name__}'.")
    if dim < 1:
        raise ValueError(
            f"method _check_args 'dim' argument should be at "
            f"least 1 but found {dim}.")
    if dim > len(array.children):
        raise ValueError(
            f"method _check_args 'dim' argument should be at most the number "
            f"of dimensions of the array ({len(array.children)}) but found "
            f"{dim}.")

    # The first element of the array (index 0) relates to the first
    # dimension (dim 1), so we need to reduce dim by 1.
    if not isinstance(array.indices[dim-1], Range):
        raise TypeError(
            f"method _check_args 'array' argument index '{dim-1}' should be a "
            f"Range type but found '{type(array.indices[dim-1]).__name__}'.")


def _is_bound_full_extent(array, dim, operator):
    '''A Fortran array section with a missing lower bound implies the
    access starts at the first element and a missing upper bound
    implies the access ends at the last element e.g. a(:,:)
    accesses all elements of array a and is equivalent to
    a(lbound(a,1):ubound(a,1),lbound(a,2):ubound(a,2)). The PSyIR
    does not support the shorthand notation, therefore the lbound
    and ubound operators are used in the PSyIR.

    This utility function checks that shorthand lower or upper
    bound Fortran code is captured as longhand lbound and/or
    ubound functions as expected in the PSyIR.

    This routine is only in fparser2.py until #717 is complete as it
    is used to check that array syntax in a where statement is for the
    full extent of the dimension. Once #717 is complete this routine
    can be moved into fparser2_test.py as it is used there in a
    different context.

    :param array: the node to check.
    :type array: :py:class:`pysclone.psyir.nodes.ArrayMixin`
    :param int dim: the dimension index to use.
    :param operator: the operator to check.
    :type operator: \
        :py:class:`psyclone.psyir.nodes.binaryoperation.Operator.LBOUND` \
        or :py:class:`psyclone.psyir.nodes.binaryoperation.Operator.UBOUND`

    :returns: True if the supplied array has the expected properties, \
        otherwise returns False.
    :rtype: bool

    :raises TypeError: if the supplied arguments are of the wrong type.

    '''
    _check_args(array, dim)

    if operator == BinaryOperation.Operator.LBOUND:
        index = 0
    elif operator == BinaryOperation.Operator.UBOUND:
        index = 1
    else:
        raise TypeError(
            f"'operator' argument  expected to be LBOUND or UBOUND but "
            f"found '{type(operator).__name__}'.")

    # The first element of the array (index 0) relates to the first
    # dimension (dim 1), so we need to reduce dim by 1.
    bound = array.indices[dim-1].children[index]

    if not isinstance(bound, BinaryOperation):
        return False

    reference = bound.children[0]
    literal = bound.children[1]

    if bound.operator != operator:
        return False

    if (not isinstance(literal, Literal) or
            literal.datatype.intrinsic != ScalarType.Intrinsic.INTEGER or
            literal.value != str(dim)):
        return False

    return isinstance(reference, Reference) and array.is_same_array(reference)


def _is_array_range_literal(array, dim, index, value):
    '''Utility function to check that the supplied array has an integer
    literal at dimension index "dim" and range index "index" with
    value "value".

    The step part of the range node has an integer literal with
    value 1 by default.

    This routine is only in fparser2.py until #717 is complete as it
    is used to check that array syntax in a where statement is for the
    full extent of the dimension. Once #717 is complete this routine
    can be moved into fparser2_test.py as it is used there in a
    different context.

    :param array: the node to check.
    :type array: :py:class:`pysclone.psyir.node.ArrayReference`
    :param int dim: the dimension index to check.
    :param int index: the index of the range to check (0 is the \
        lower bound, 1 is the upper bound and 2 is the step).
    :param int value: the expected value of the literal.

    :raises NotImplementedError: if the supplied argument does not \
        have the required properties.

    :returns: True if the supplied array has the expected properties, \
        otherwise returns False.
    :rtype: bool

    :raises TypeError: if the supplied arguments are of the wrong type.
    :raises ValueError: if the index argument has an incorrect value.

    '''
    _check_args(array, dim)

    if not isinstance(index, int):
        raise TypeError(
            f"method _check_array_range_literal 'index' argument should be an "
            f"int type but found '{type(index).__name__}'.")

    if index < 0 or index > 2:
        raise ValueError(
            f"method _check_array_range_literal 'index' argument should be "
            f"0, 1 or 2 but found {index}.")

    if not isinstance(value, int):
        raise TypeError(
            f"method _check_array_range_literal 'value' argument should be an "
            f"int type but found '{type(value).__name__}'.")

    # The first child of the array (index 0) relates to the first
    # dimension (dim 1), so we need to reduce dim by 1.
    literal = array.children[dim-1].children[index]

    if (isinstance(literal, Literal) and
            literal.datatype.intrinsic == ScalarType.Intrinsic.INTEGER and
            literal.value == str(value)):
        return True
    return False


def _is_range_full_extent(my_range):
    '''Utility function to check whether a Range object is equivalent to a
    ":" in Fortran array notation. The PSyIR representation of "a(:)"
    is "a(lbound(a,1):ubound(a,1):1). Therefore, for array a index 1,
    the lower bound is compared with "lbound(a,1)", the upper bound is
    compared with "ubound(a,1)" and the step is compared with 1.

    If everything is OK then this routine silently returns, otherwise
    an exception is raised by one of the functions
    (_check_bound_is_full_extent or _check_array_range_literal) called by this
    function.

    This routine is only in fparser2.py until #717 is complete as it
    is used to check that array syntax in a where statement is for the
    full extent of the dimension. Once #717 is complete this routine
    can be removed.

    :param my_range: the Range node to check.
    :type my_range: :py:class:`psyclone.psyir.node.Range`

    '''

    array = my_range.parent
    # The array index of this range is determined by its position in
    # the array list (+1 as the index starts from 0 but Fortran
    # dimensions start from 1).
    dim = array.children.index(my_range) + 1
    # Check lower bound
    is_lower = _is_bound_full_extent(
        array, dim, BinaryOperation.Operator.LBOUND)
    # Check upper bound
    is_upper = _is_bound_full_extent(
        array, dim, BinaryOperation.Operator.UBOUND)
    # Check step (index 2 is the step index for the range function)
    is_step = _is_array_range_literal(array, dim, 2, 1)
    return is_lower and is_upper and is_step


def _copy_full_base_reference(node):
    '''
    Given the supplied node, creates a new node with the same access
    apart from the final array access. Such a node is then suitable for use
    as an argument to either e.g. LBOUND or UBOUND.

    e.g. if `node` is an ArrayMember representing the inner access in
    'grid%data(:)' then this routine will return a PSyIR node for
    'grid%data'.

    :param node: the array access. In the case of a structure, this \
                 must be the inner-most part of the access.
    :type node: :py:class:`psyclone.psyir.nodes.Reference` or \
                :py:class:`psyclone.psyir.nodes.Member`

    :returns: the PSyIR for a suitable argument to either LBOUND or \
              UBOUND applied to the supplied `node`.
    :rtype: :py:class:`psyclone.psyir.nodes.Node`

    :raises InternalError: if the supplied node is not an instance of \
                           either Reference or Member.
    '''
    if isinstance(node, Reference):
        return Reference(node.symbol)

    if isinstance(node, Member):
        # We have to take care with derived types:
        # grid(1)%data(:...) becomes
        # grid(1)%data(lbound(grid(1)%data,1):...)
        # N.B. the argument to lbound becomes a Member access rather
        # than an ArrayMember access.
        parent_ref = node.ancestor(Reference, include_self=True)
        # We have to find the location of the supplied node in the
        # StructureReference.
        inner = parent_ref
        depth = 0
        while hasattr(inner, "member") and inner is not node:
            depth += 1
            inner = inner.member
        # Now we take a copy of the full reference and then modify it so
        # that the copy of 'node' is replaced by a Member().
        arg = parent_ref.copy()
        # We use the depth computed for the original reference in order
        # to find the copy of 'node'.
        inner = arg
        for step in range(depth-1):
            inner = inner.member
        # Change the innermost access to be a Member.
        inner.children[0] = Member(node.name, inner)
        return arg

    raise InternalError(
        f"The supplied node must be an instance of either Reference "
        f"or Member but got '{type(node).__name__}'.")


def _kind_find_or_create(name, symbol_table):
    '''
    Utility method that returns a Symbol representing the named KIND
    parameter. If the supplied Symbol Table (or one of its ancestors)
    does not contain an appropriate entry then one is created. If it does
    contain a matching entry then it must be either a Symbol or a
    DataSymbol.

    If it is a DataSymbol then it must have a datatype of
    'integer', 'deferred' or 'unknown'. If it is deferred then the fact
    that we now know that this Symbol represents a KIND parameter means we
    can change the datatype to be 'integer'.

    If the existing symbol is a generic Symbol then it is replaced with
    a new DataSymbol of type 'integer'.

    :param str name: the name of the variable holding the KIND value.
    :param symbol_table: the Symbol Table associated with the code being \
                         processed.
    :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable`

    :returns: the Symbol representing the KIND parameter.
    :rtype: :py:class:`psyclone.psyir.symbols.DataSymbol`

    :raises TypeError: if the symbol table already contains an entry for \
            `name` but it is not an instance of Symbol or DataSymbol.
    :raises TypeError: if the symbol table already contains a DataSymbol \
            for `name` and its datatype is not 'integer' or 'deferred'.

    '''
    lower_name = name.lower()

    try:
        kind_symbol = symbol_table.lookup(lower_name)
        # pylint: disable=unidiomatic-typecheck
        if type(kind_symbol) == Symbol:
            # There is an existing entry but it's only a generic Symbol
            # so we need to replace it with a DataSymbol of integer type.
            # Since the lookup() above looks through *all* ancestor symbol
            # tables, we have to find precisely which table the existing
            # Symbol is in.
            table = kind_symbol.find_symbol_table(symbol_table.node)
            new_symbol = DataSymbol(lower_name,
                                    default_integer_type(),
                                    visibility=kind_symbol.visibility,
                                    interface=kind_symbol.interface)
            table.swap(kind_symbol, new_symbol)
            kind_symbol = new_symbol
        elif isinstance(kind_symbol, DataSymbol):

            if not (isinstance(kind_symbol.datatype,
                               (UnknownType, DeferredType)) or
                    (isinstance(kind_symbol.datatype, ScalarType) and
                     kind_symbol.datatype.intrinsic ==
                     ScalarType.Intrinsic.INTEGER)):
                raise TypeError(
                    f"SymbolTable already contains a DataSymbol for variable "
                    f"'{lower_name}' used as a kind parameter but it is not a "
                    f"'deferred', 'unknown' or 'scalar integer' type.")
            # A KIND parameter must be of type integer so set it here if it
            # was previously 'deferred'. We don't know what precision this is
            # so set it to the default.
            if isinstance(kind_symbol.datatype, DeferredType):
                kind_symbol.datatype = default_integer_type()
        else:
            raise TypeError(
                f"A symbol representing a kind parameter must be an instance "
                f"of either a Symbol or a DataSymbol. However, found an entry "
                f"of type '{type(kind_symbol).__name__}' for variable "
                f"'{lower_name}'.")
    except KeyError:
        # The SymbolTable does not contain an entry for this kind parameter
        # so create one. We specify an UnresolvedInterface as we don't
        # currently know how this symbol is brought into scope.
        kind_symbol = DataSymbol(lower_name, default_integer_type(),
                                 visibility=symbol_table.default_visibility,
                                 interface=UnresolvedInterface())
        symbol_table.add(kind_symbol)
    return kind_symbol


def default_precision(_):
    '''Returns the default precision specified by the front end. This is
    currently always set to undefined irrespective of the datatype but
    could be read from a config file in the future. The unused
    argument provides the name of the datatype. This name will allow a
    future implementation of this method to choose different default
    precisions for different datatypes if required.

    There are alternative options for setting a default precision,
    such as:

    1) The back-end sets the default precision in a similar manner
    to this routine.
    2) A PSyIR transformation is used to set default precision.

    This routine is primarily here as a placeholder and could be
    replaced by an alternative solution, see issue #748.

    :returns: the default precision for the supplied datatype name.
    :rtype: :py:class:`psyclone.psyir.symbols.scalartype.Precision`

    '''
    return ScalarType.Precision.UNDEFINED


def default_integer_type():
    '''Returns the default integer datatype specified by the front end.

    :returns: the default integer datatype.
    :rtype: :py:class:`psyclone.psyir.symbols.ScalarType`

    '''
    return ScalarType(ScalarType.Intrinsic.INTEGER,
                      default_precision(ScalarType.Intrinsic.INTEGER))


def default_real_type():
    '''Returns the default real datatype specified by the front end.

    :returns: the default real datatype.
    :rtype: :py:class:`psyclone.psyir.symbols.ScalarType`

    '''
    return ScalarType(ScalarType.Intrinsic.REAL,
                      default_precision(ScalarType.Intrinsic.REAL))


def get_literal_precision(fparser2_node, psyir_literal_parent):
    '''Takes a Fortran2003 literal node as input and returns the
    appropriate PSyIR precision type for that node. Adds a deferred
    type DataSymbol in the SymbolTable if the precision is given by an
    undefined symbol.

    :param fparser2_node: the fparser2 literal node.
    :type fparser2_node: :py:class:`Fortran2003.Real_Literal_Constant` or \
        :py:class:`Fortran2003.Logical_Literal_Constant` or \
        :py:class:`Fortran2003.Char_Literal_Constant` or \
        :py:class:`Fortran2003.Int_Literal_Constant`
    :param psyir_literal_parent: the PSyIR node that will be the \
        parent of the PSyIR literal node that will be created from the \
        fparser2 node information.
    :type psyir_literal_parent: :py:class:`psyclone.psyir.nodes.Node`

    :returns: the PSyIR Precision of this literal value.
    :rtype: :py:class:`psyclone.psyir.symbols.DataSymbol`, int or \
        :py:class:`psyclone.psyir.symbols.ScalarType.Precision`

    :raises InternalError: if the arguments are of the wrong type.
    :raises InternalError: if there's no symbol table associated with \
                           `psyir_literal_parent` or one of its ancestors.

    '''
    if not isinstance(fparser2_node,
                      (Fortran2003.Real_Literal_Constant,
                       Fortran2003.Logical_Literal_Constant,
                       Fortran2003.Char_Literal_Constant,
                       Fortran2003.Int_Literal_Constant)):
        raise InternalError(
            f"Unsupported literal type '{type(fparser2_node).__name__}' found "
            f"in get_literal_precision.")
    if not isinstance(psyir_literal_parent, Node):
        raise InternalError(
            f"Expecting argument psyir_literal_parent to be a PSyIR Node but "
            f"found '{type(psyir_literal_parent).__name__}' in "
            f"get_literal_precision.")
    precision_name = fparser2_node.items[1]
    if not precision_name:
        # Precision may still be specified by the exponent in a real literal
        if isinstance(fparser2_node, Fortran2003.Real_Literal_Constant):
            precision_value = fparser2_node.items[0]
            if "d" in precision_value.lower():
                return ScalarType.Precision.DOUBLE
            if "e" in precision_value.lower():
                return ScalarType.Precision.SINGLE
        # Return the default precision
        try:
            data_name = CONSTANT_TYPE_MAP[type(fparser2_node)]
        except KeyError:
            raise NotImplementedError(
                f"Could not process {type(fparser2_node).__name__}. Only "
                f"'real', 'integer', 'logical' and 'character' intrinsic "
                f"types are supported.")
        return default_precision(data_name)
    try:
        # Precision is specified as an integer
        return int(precision_name)
    except ValueError:
        # Precision is not an integer so should be a kind symbol
        # PSyIR stores names as lower case.
        precision_name = precision_name.lower()
        # Find the closest symbol table
        try:
            symbol_table = psyir_literal_parent.scope.symbol_table
        except SymbolError as err:
            # No symbol table found. This should never happen in
            # normal usage but could occur if a test constructs a
            # PSyIR without a Schedule.
            raise InternalError(
                f"Failed to find a symbol table to which to add the kind "
                f"symbol '{precision_name}'.") from err
        return _kind_find_or_create(precision_name, symbol_table)


def _process_routine_symbols(module_ast, symbol_table, visibility_map):
    '''
    Examines the supplied fparser2 parse tree for a module and creates
    RoutineSymbols for every routine (function or subroutine) that it
    contains.

    :param module_ast: fparser2 parse tree for module.
    :type module_ast: :py:class:`fparser.two.Fortran2003.Program`
    :param symbol_table: the SymbolTable to which to add the symbols.
    :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable`
    :param visibility_map: dict of symbol names with explicit visibilities.
    :type visibility_map: dict with symbol names as keys and visibilities as \
                          values
    '''
    routines = walk(module_ast, (Fortran2003.Subroutine_Subprogram,
                                 Fortran2003.Function_Subprogram))
    # A subroutine has no type but a function does. However, we don't know what
    # it is at this stage so we give all functions a DeferredType.
    # TODO #1314 extend the frontend to ensure that the type of a Routine's
    # return_symbol matches the type of the associated RoutineSymbol.
    type_map = {Fortran2003.Subroutine_Subprogram: NoType(),
                Fortran2003.Function_Subprogram: DeferredType()}
    for routine in routines:
        name = str(routine.children[0].children[1]).lower()
        vis = visibility_map.get(name, symbol_table.default_visibility)
        # This routine is defined within this scoping unit and therefore has a
        # local interface.
        rsymbol = RoutineSymbol(name, type_map[type(routine)], visibility=vis,
                                interface=LocalInterface())
        symbol_table.add(rsymbol)


def _process_access_spec(attr):
    '''
    Converts from an fparser2 Access_Spec node to a PSyIR visibility.

    :param attr: the fparser2 AST node to process.
    :type attr: :py:class:`fparser.two.Fortran2003.Access_Spec`

    :return: the PSyIR visibility corresponding to the access spec.
    :rtype: :py:class:`psyclone.psyir.Symbol.Visibility`

    :raises InternalError: if an invalid access specification is found.

    '''
    try:
        return VISIBILITY_MAP_FROM_FORTRAN[attr.string.lower()]
    except KeyError as err:
        raise InternalError(f"Unexpected Access Spec attribute "
                            f"'{attr}'.") from err


def _create_struct_reference(parent, base_ref, base_symbol, members,
                             indices):
    '''
    Utility to create a StructureReference or ArrayOfStructuresReference. Any
    PSyIR nodes in the supplied lists of members and indices are copied
    when making the new node.

    :param parent: Parent node of the PSyIR node we are constructing.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`
    :param type base_ref: the type of Reference to create.
    :param base_symbol: the Symbol that the reference is to.
    :type base_symbol: :py:class:`psyclone.psyir.symbols.Symbol`
    :param members: the component(s) of the structure that are being accessed.\
        Any components that are array references must provide the name of the \
        array and a list of DataNodes describing which part of it is accessed.
    :type members: list of str or 2-tuples containing (str, \
        list of nodes describing array access)
    :param indices: a list of Nodes describing the array indices for \
        the base reference (if any).
    :type indices: list of :py:class:`psyclone.psyir.nodes.Node`

    :raises InternalError: if any element in the `members` list is not a \
        str or tuple or if `indices` are supplied for a StructureReference \
        or *not* supplied for an ArrayOfStructuresReference.
    :raises NotImplementedError: if `base_ref` is not a StructureReference or \
        an ArrayOfStructuresReference.

    '''
    # Ensure we create a copy of any References within the list of
    # members making up this structure access.
    new_members = []
    for member in members:
        if isinstance(member, six.string_types):
            new_members.append(member)
        elif isinstance(member, tuple):
            # Second member of the tuple is a list of index expressions
            new_members.append((member[0], [kid.copy() for kid in member[1]]))
        else:
            raise InternalError(
                f"List of members must contain only strings or tuples "
                f"but found entry of type '{type(member).__name__}'")
    if base_ref is StructureReference:
        if indices:
            raise InternalError(
                f"Creating a StructureReference but array indices have been "
                f"supplied ({indices}) which makes no sense.")
        return base_ref.create(base_symbol, new_members, parent=parent)
    if base_ref is ArrayOfStructuresReference:
        if not indices:
            raise InternalError(
                "Cannot create an ArrayOfStructuresReference without one or "
                "more index expressions but the 'indices' argument is empty.")
        return base_ref.create(base_symbol, [idx.copy() for idx in indices],
                               new_members, parent=parent)

    raise NotImplementedError(
        f"Cannot create structure reference for type '{base_ref}' - expected "
        f"either StructureReference or ArrayOfStructuresReference.")


def _get_arg_names(node_list):
    '''Utility function that given an fparser2 argument list returns two
    separate lists, one with the arguments themselves and another with
    the argument names.

    :param node_list: a list of fparser2 argument nodes which could \
        be positional or named.
    :type node_list: List[:py:class:`fparser.two.utils.Base`]

    :returns: a list of fparser2 arguments with any name \
        information and a separate list of named argument names.
    :rtype: Tuple[List[:py:class:`fparser.two.utils.Base`], \
         Union[List[str], None]]

    '''
    arg_names = []
    arg_nodes = []
    for node in node_list:
        if isinstance(node, Fortran2003.Actual_Arg_Spec):
            arg_names.append(node.children[0].string)
            arg_nodes.append(node.children[1])
        else:
            arg_names.append(None)
            arg_nodes.append(node)
    return arg_nodes, arg_names


class Fparser2Reader(object):
    '''
    Class to encapsulate the functionality for processing the fparser2 AST and
    convert the nodes to PSyIR.
    '''

    unary_operators = OrderedDict([
        ('+', UnaryOperation.Operator.PLUS),
        ('-', UnaryOperation.Operator.MINUS),
        ('.not.', UnaryOperation.Operator.NOT),
        ('abs', UnaryOperation.Operator.ABS),
        ('ceiling', UnaryOperation.Operator.CEIL),
        ('exp', UnaryOperation.Operator.EXP),
        ('log', UnaryOperation.Operator.LOG),
        ('log10', UnaryOperation.Operator.LOG10),
        ('sin', UnaryOperation.Operator.SIN),
        ('asin', UnaryOperation.Operator.ASIN),
        ('cos', UnaryOperation.Operator.COS),
        ('acos', UnaryOperation.Operator.ACOS),
        ('tan', UnaryOperation.Operator.TAN),
        ('atan', UnaryOperation.Operator.ATAN),
        ('sqrt', UnaryOperation.Operator.SQRT),
        ('sum', UnaryOperation.Operator.SUM),
        ('real', UnaryOperation.Operator.REAL),
        ('nint', UnaryOperation.Operator.NINT),
        ('int', UnaryOperation.Operator.INT)])

    binary_operators = OrderedDict([
        ('+', BinaryOperation.Operator.ADD),
        ('-', BinaryOperation.Operator.SUB),
        ('*', BinaryOperation.Operator.MUL),
        ('/', BinaryOperation.Operator.DIV),
        ('**', BinaryOperation.Operator.POW),
        ('==', BinaryOperation.Operator.EQ),
        ('.eq.', BinaryOperation.Operator.EQ),
        ('/=', BinaryOperation.Operator.NE),
        ('.ne.', BinaryOperation.Operator.NE),
        ('<=', BinaryOperation.Operator.LE),
        ('.le.', BinaryOperation.Operator.LE),
        ('<', BinaryOperation.Operator.LT),
        ('.lt.', BinaryOperation.Operator.LT),
        ('>=', BinaryOperation.Operator.GE),
        ('.ge.', BinaryOperation.Operator.GE),
        ('>', BinaryOperation.Operator.GT),
        ('.gt.', BinaryOperation.Operator.GT),
        ('.and.', BinaryOperation.Operator.AND),
        ('.or.', BinaryOperation.Operator.OR),
        ('dot_product', BinaryOperation.Operator.DOT_PRODUCT),
        ('int', BinaryOperation.Operator.INT),
        ('real', BinaryOperation.Operator.REAL),
        ('sign', BinaryOperation.Operator.SIGN),
        ('size', BinaryOperation.Operator.SIZE),
        ('sum', BinaryOperation.Operator.SUM),
        ('lbound', BinaryOperation.Operator.LBOUND),
        ('ubound', BinaryOperation.Operator.UBOUND),
        ('max', BinaryOperation.Operator.MAX),
        ('min', BinaryOperation.Operator.MIN),
        ('mod', BinaryOperation.Operator.REM),
        ('matmul', BinaryOperation.Operator.MATMUL),
        ('transfer', BinaryOperation.Operator.CAST)])

    nary_operators = OrderedDict([
        ('max', NaryOperation.Operator.MAX),
        ('min', NaryOperation.Operator.MIN),
        ('sum', NaryOperation.Operator.SUM)])

    def __init__(self):
        from fparser.two import utils
        # Map of fparser2 node types to handlers (which are class methods)
        self.handlers = {
            Fortran2003.Assignment_Stmt: self._assignment_handler,
            Fortran2003.Data_Ref: self._data_ref_handler,
            Fortran2003.Function_Subprogram: self._subroutine_handler,
            Fortran2003.Name: self._name_handler,
            Fortran2003.Parenthesis: self._parenthesis_handler,
            Fortran2003.Part_Ref: self._part_ref_handler,
            Fortran2003.Subscript_Triplet: self._subscript_triplet_handler,
            Fortran2003.If_Stmt: self._if_stmt_handler,
            utils.NumberBase: self._number_handler,
            Fortran2003.Int_Literal_Constant: self._number_handler,
            Fortran2003.Char_Literal_Constant: self._char_literal_handler,
            Fortran2003.Logical_Literal_Constant: self._bool_literal_handler,
            utils.BinaryOpBase: self._binary_op_handler,
            Fortran2003.End_Do_Stmt: self._ignore_handler,
            Fortran2003.End_Subroutine_Stmt: self._ignore_handler,
            Fortran2003.If_Construct: self._if_construct_handler,
            Fortran2003.Case_Construct: self._case_construct_handler,
            Fortran2003.Return_Stmt: self._return_handler,
            Fortran2003.UnaryOpBase: self._unary_op_handler,
            Fortran2003.Block_Nonlabel_Do_Construct:
                self._do_construct_handler,
            Fortran2003.Intrinsic_Function_Reference: self._intrinsic_handler,
            Fortran2003.Where_Construct: self._where_construct_handler,
            Fortran2003.Where_Stmt: self._where_construct_handler,
            Fortran2003.Call_Stmt: self._call_handler,
            Fortran2003.Subroutine_Subprogram: self._subroutine_handler,
            Fortran2003.Module: self._module_handler,
            Fortran2003.Main_Program: self._main_program_handler,
            Fortran2003.Program: self._program_handler,
        }

    @staticmethod
    def nodes_to_code_block(parent, fp2_nodes):
        '''Create a CodeBlock for the supplied list of fparser2 nodes and then
        wipe the list. A CodeBlock is a node in the PSyIR (Schedule)
        that represents a sequence of one or more Fortran statements
        and/or expressions which PSyclone does not attempt to handle.

        :param parent: Node in the PSyclone AST to which to add this code \
                       block.
        :type parent: :py:class:`psyclone.psyir.nodes.Node`
        :param fp2_nodes: list of fparser2 AST nodes constituting the \
                          code block.
        :type fp2_nodes: list of :py:class:`fparser.two.utils.Base`

        :returns: a CodeBlock instance.
        :rtype: :py:class:`psyclone.CodeBlock`

        '''
        if not fp2_nodes:
            return None

        # Determine whether this code block is a statement or an
        # expression. Statements always have a `Schedule` as parent
        # and expressions do not. The only unknown at this point are
        # directives whose structure are in discussion. Therefore, for
        # the moment, an exception is raised if a directive is found
        # as a parent.
        if isinstance(parent, (Schedule, Container)):
            structure = CodeBlock.Structure.STATEMENT
        elif isinstance(parent, Directive):
            raise InternalError(
                "Fparser2Reader:nodes_to_code_block: A CodeBlock with "
                "a Directive as parent is not yet supported.")
        else:
            structure = CodeBlock.Structure.EXPRESSION

        code_block = CodeBlock(fp2_nodes, structure, parent=parent)
        parent.addchild(code_block)
        del fp2_nodes[:]
        return code_block

    @staticmethod
    def _create_schedule(name):
        '''
        Create an empty KernelSchedule.

        :param str name: Name of the subroutine represented by the kernel.

        :returns: New KernelSchedule empty object.
        :rtype: py:class:`psyclone.psyir.nodes.KernelSchedule`

        '''
        return KernelSchedule(name)

    def generate_psyir(self, parse_tree):
        '''Translate the supplied fparser2 parse_tree into PSyIR.

        :param parse_tree: the supplied fparser2 parse tree.
        :type parse_tree: :py:class:`fparser.two.Fortran2003.Program`

        :returns: PSyIR representation of the supplied fparser2 parse_tree.
        :rtype: :py:class:`psyclone.psyir.nodes.Container` or \
            :py:class:`psyclone.psyir.nodes.Routine`

        :raises GenerationError: if the root of the supplied fparser2 \
            parse tree is not a Program.

        '''
        if not isinstance(parse_tree, Fortran2003.Program):
            raise GenerationError(
                f"The Fparser2Reader generate_psyir method expects the root "
                f"of the supplied fparser2 tree to be a Program, but found "
                f"'{type(parse_tree).__name__}'")

        node = Container("dummy")
        self.process_nodes(node, [parse_tree])
        result = node.children[0]
        return result.detach()

    def generate_container(self, module_ast):
        '''
        Create a Container from the supplied fparser2 module AST.

        :param module_ast: fparser2 AST of the full module.
        :type module_ast: :py:class:`fparser.two.Fortran2003.Program`

        :returns: PSyIR container representing the given module_ast or None \
                  if there's no module in the parse tree.
        :rtype: :py:class:`psyclone.psyir.nodes.Container`

        :raises GenerationError: unable to generate a Container from the \
                                 provided fpaser2 parse tree.
        '''
        # Assume just 1 or 0 Fortran module definitions in the file
        modules = walk(module_ast, Fortran2003.Module_Stmt)
        if len(modules) > 1:
            raise GenerationError(
                f"Could not process {module_ast}. Just one module definition "
                f"per file supported.")
        if not modules:
            return None

        module = modules[0].parent
        mod_name = str(modules[0].children[1])

        # Create a container to capture the module information
        new_container = Container(mod_name)

        # Search for any accessibility statements (e.g. "PUBLIC :: my_var") to
        # determine the default accessibility of symbols as well as identifying
        # those that are explicitly declared as public or private.
        (default_visibility, visibility_map) = self.process_access_statements(
            module)
        new_container.symbol_table.default_visibility = default_visibility

        # Create symbols for all routines defined within this module
        _process_routine_symbols(module_ast, new_container.symbol_table,
                                 visibility_map)

        # Parse the declarations if it has any
        for child in module.children:
            if isinstance(child, Fortran2003.Specification_Part):
                try:
                    self.process_declarations(new_container, child.children,
                                              [], visibility_map)
                except SymbolError as err:
                    raise SymbolError(
                        f"Error when generating Container for module "
                        f"'{mod_name}': {err.args[0]}") from err
                break

        return new_container

    def generate_schedule(self, name, module_ast, container=None):
        '''Create a Schedule from the supplied fparser2 AST.

        TODO #737. Currently this routine is also used to create a
        NemoInvokeSchedule from NEMO source code (hence the optional,
        'container' argument).  This routine needs re-naming and
        re-writing so that it *only* creates the PSyIR for a
        subroutine.

        :param str name: name of the subroutine represented by the kernel.
        :param module_ast: fparser2 AST of the full module where the kernel \
                           code is located.
        :type module_ast: :py:class:`fparser.two.Fortran2003.Program`
        :param container: the parent Container node associated with this \
                          Schedule (if any).
        :type container: :py:class:`psyclone.psyir.nodes.Container`

        :returns: PSyIR schedule representing the kernel.
        :rtype: :py:class:`psyclone.psyir.nodes.KernelSchedule`

        :raises GenerationError: unable to generate a kernel schedule from \
                                 the provided fpaser2 parse tree.

        '''
        new_schedule = self._create_schedule(name)

        routines = walk(module_ast, (Fortran2003.Subroutine_Subprogram,
                                     Fortran2003.Main_Program,
                                     Fortran2003.Function_Subprogram))
        for routine in routines:
            if isinstance(routine, Fortran2003.Function_Subprogram):
                # TODO fparser/#225 Function_Stmt does not have a get_name()
                # method. Once it does we can remove this branch.
                routine_name = str(routine.children[0].children[1])
            else:
                routine_name = str(routine.children[0].get_name())
            if routine_name == name:
                subroutine = routine
                break
        else:
            raise GenerationError(f"Unexpected kernel AST. Could not find "
                                  f"subroutine: {name}")

        # Check whether or not we need to create a Container for this schedule
        # TODO #737 this routine should just be creating a Subroutine, not
        # attempting to create a Container too. Perhaps it should be passed
        # a reference to the parent Container object.
        if not container:
            # Is the routine enclosed within a module?
            current = subroutine.parent
            while current:
                if isinstance(current, Fortran2003.Module):
                    # We have a parent module so create a Container
                    container = self.generate_container(current)
                    break
                current = current.parent
        if container:
            container.children.append(new_schedule)

        try:
            sub_spec = _first_type_match(subroutine.content,
                                         Fortran2003.Specification_Part)
            decl_list = sub_spec.content
            # TODO this if test can be removed once fparser/#211 is fixed
            # such that routine arguments are always contained in a
            # Dummy_Arg_List, even if there's only one of them.
            from fparser.two.Fortran2003 import Dummy_Arg_List
            if isinstance(subroutine, Fortran2003.Subroutine_Subprogram) and \
               isinstance(subroutine.children[0].children[2], Dummy_Arg_List):
                arg_list = subroutine.children[0].children[2].children
            else:
                # Routine has no arguments
                arg_list = []
        except ValueError:
            # Subroutine without declarations, continue with empty lists.
            decl_list = []
            arg_list = []
        finally:
            self.process_declarations(new_schedule, decl_list, arg_list)

        try:
            sub_exec = _first_type_match(subroutine.content,
                                         Fortran2003.Execution_Part)
        except ValueError:
            pass
        else:
            self.process_nodes(new_schedule, sub_exec.content)

        return new_schedule

    @staticmethod
    def _parse_dimensions(dimensions, symbol_table):
        '''
        Parse the fparser dimension attribute into a shape list. Each entry of
        this list is either None (if the extent is unknown) or a 2-tuple
        containing the lower and upper bound of that dimension. If any of the
        symbols encountered are instances of the generic Symbol class, they are
        specialised (in place) and become instances of DataSymbol with
        DeferredType.

        :param dimensions: fparser dimension attribute.
        :type dimensions: \
            :py:class:`fparser.two.Fortran2003.Dimension_Attr_Spec`
        :param symbol_table: symbol table of the declaration context.
        :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable`

        :returns: shape of the attribute in column-major order (leftmost \
            index is contiguous in memory). Each entry represents an array \
            dimension. If it is 'None' the extent of that dimension is \
            unknown, otherwise it holds a 2-tuple with the upper and lower \
            bounds of the dimension. If it is an empty list then the symbol \
            represents a scalar.
        :rtype: list of NoneType or 2-tuples of \
                :py:class:`psyclone.psyir.nodes.DataNode`

        :raises NotImplementedError: if anything other than scalar, integer \
            literals or symbols are encounted in the dimensions list.

        '''
        def _process_bound(bound_expr):
            '''Process the supplied fparser2 parse tree for the upper/lower
            bound of a dimension in an array declaration.

            :param bound_expr: fparser2 parse tree for lower/upper bound.
            :type bound_expr: :py:class:`fparser.two.utils.Base`

            :returns: PSyIR for the bound.
            :rtype: :py:class:`psyclone.psyir.nodes.DataNode`

            :raises NotImplementedError: if an unsupported form of array \
                                         bound is found.
            '''
            if isinstance(bound_expr, Fortran2003.Int_Literal_Constant):
                return Literal(bound_expr.items[0], INTEGER_TYPE)

            if isinstance(bound_expr, Fortran2003.Name):
                # Fortran does not regulate the order in which variables
                # may be declared so it's possible for the shape
                # specification of an array to reference variables that
                # come later in the list of declarations. The reference
                # may also be to a symbol present in a parent symbol table
                # (e.g. if the variable is declared in an outer, module
                # scope).
                dim_name = bound_expr.string.lower()
                try:
                    sym = symbol_table.lookup(dim_name)
                    # pylint: disable=unidiomatic-typecheck
                    if type(sym) == Symbol:
                        # An entry for this symbol exists but it's only a
                        # generic Symbol and we now know it must be a
                        # DataSymbol.
                        sym.specialise(DataSymbol, datatype=DeferredType())
                    elif isinstance(sym.datatype, (UnknownType,
                                                   DeferredType)):
                        # Allow symbols of Unknown/DeferredType.
                        pass
                    elif not (isinstance(sym.datatype, ScalarType) and
                              sym.datatype.intrinsic ==
                              ScalarType.Intrinsic.INTEGER):
                        # It's not of Unknown/DeferredType and it's not an
                        # integer scalar.
                        raise NotImplementedError()
                except KeyError:
                    # We haven't seen this symbol before so create a new
                    # one with a deferred interface (since we don't
                    # currently know where it is declared).
                    sym = DataSymbol(dim_name, default_integer_type(),
                                     interface=UnresolvedInterface())
                    symbol_table.add(sym)
                return Reference(sym)

            raise NotImplementedError()

        one = Literal("1", INTEGER_TYPE)
        shape = []
        # Traverse shape specs in Depth-first-search order
        for dim in walk(dimensions, (Fortran2003.Assumed_Shape_Spec,
                                     Fortran2003.Explicit_Shape_Spec,
                                     Fortran2003.Assumed_Size_Spec)):

            if isinstance(dim, Fortran2003.Assumed_Shape_Spec):
                shape.append(None)

            elif isinstance(dim, Fortran2003.Explicit_Shape_Spec):
                try:
                    upper = _process_bound(dim.items[1])
                    if dim.items[0]:
                        lower = _process_bound(dim.items[0])
                        shape.append((lower, upper))
                    else:
                        # Lower bound defaults to 1 in Fortran
                        shape.append((one.copy(), upper))
                except NotImplementedError as err:
                    raise NotImplementedError(
                        f"Could not process {dimensions}. Only scalar integer "
                        f"literals or symbols are supported for explicit-shape"
                        f" array declarations.") from err

            elif isinstance(dim, Fortran2003.Assumed_Size_Spec):
                raise NotImplementedError(
                    f"Could not process {dimensions}. Assumed-size arrays"
                    f" are not supported.")

            else:
                raise InternalError(
                    f"Reached end of loop body and array-shape specification "
                    f"{type(dim)} has not been handled.")

        return shape

    @staticmethod
    def process_access_statements(nodes):
        '''
        Search the supplied list of fparser2 nodes (which must represent a
        complete Specification Part) for any accessibility
        statements (e.g. "PUBLIC :: my_var") to determine the default
        visibility of symbols as well as identifying those that are
        explicitly declared as public or private.

        :param nodes: nodes in the fparser2 parse tree describing a \
                      Specification Part that will be searched.
        :type nodes: list of :py:class:`fparser.two.utils.Base`

        :returns: default visibility of symbols within the current scoping \
            unit and dict of symbol names with explicit visibilities.
        :rtype: 2-tuple of (:py:class:`psyclone.symbols.Symbol.Visibility`, \
                dict)

        :raises InternalError: if an accessibility attribute which is not \
            'public' or 'private' is encountered.
        :raises GenerationError: if the parse tree is found to contain more \
            than one bare accessibility statement (i.e. 'PUBLIC' or 'PRIVATE')
        :raises GenerationError: if a symbol is explicitly declared as being \
            both public and private.

        '''
        default_visibility = None
        # Sets holding the names of those symbols whose access is specified
        # explicitly via an access-stmt (e.g. "PUBLIC :: my_var")
        explicit_public = set()
        explicit_private = set()
        # R518 an access-stmt shall appear only in the specification-part
        # of a *module*.
        access_stmts = walk(nodes, Fortran2003.Access_Stmt)

        for stmt in access_stmts:

            if stmt.children[0].lower() == "public":
                public_stmt = True
            elif stmt.children[0].lower() == "private":
                public_stmt = False
            else:
                raise InternalError(
                    f"Failed to process '{stmt}'. Found an accessibility "
                    f"attribute of '{stmt.children[0]}' but expected either "
                    f"'public' or 'private'.")
            if not stmt.children[1]:
                if default_visibility:
                    # We've already seen an access statement without an
                    # access-id-list. This is therefore invalid Fortran (which
                    # fparser does not catch).
                    current_node = stmt.parent
                    while current_node:
                        if isinstance(current_node, Fortran2003.Module):
                            mod_name = str(
                                current_node.children[0].children[1])
                            raise GenerationError(
                                f"Module '{mod_name}' contains more than one "
                                f"access statement with an omitted "
                                f"access-id-list. This is invalid Fortran.")
                        current_node = current_node.parent
                    # Failed to find an enclosing Module. This is also invalid
                    # Fortran since an access statement is only permitted
                    # within a module.
                    raise GenerationError(
                        "Found multiple access statements with omitted access-"
                        "id-lists and no enclosing Module. Both of these "
                        "things are invalid Fortran.")
                if public_stmt:
                    default_visibility = Symbol.Visibility.PUBLIC
                else:
                    default_visibility = Symbol.Visibility.PRIVATE
            else:
                symbol_names = [child.string.lower() for child in
                                stmt.children[1].children]
                if public_stmt:
                    explicit_public.update(symbol_names)
                else:
                    explicit_private.update(symbol_names)
        # Sanity check the lists of symbols (because fparser2 does not
        # currently do much validation)
        invalid_symbols = explicit_public.intersection(explicit_private)
        if invalid_symbols:
            raise GenerationError(
                f"Symbols {list(invalid_symbols)} appear in access statements "
                f"with both PUBLIC and PRIVATE access-ids. This is invalid "
                f"Fortran.")

        # Symbols are public by default in Fortran
        if default_visibility is None:
            default_visibility = Symbol.Visibility.PUBLIC

        visibility_map = {}
        for name in explicit_public:
            visibility_map[name] = Symbol.Visibility.PUBLIC
        for name in explicit_private:
            visibility_map[name] = Symbol.Visibility.PRIVATE

        return (default_visibility, visibility_map)

    @staticmethod
    def _process_use_stmts(parent, nodes, visibility_map=None):
        '''
        Process all of the USE statements in the fparser2 parse tree
        supplied as a list of nodes. Imported symbols are added to
        the symbol table associated with the supplied parent node with
        appropriate interfaces.

        :param parent: PSyIR node in which to insert the symbols found.
        :type parent: :py:class:`psyclone.psyir.nodes.KernelSchedule`
        :param nodes: fparser2 AST nodes to search for use statements.
        :type nodes: list of :py:class:`fparser.two.utils.Base`
        :param visibility_map: mapping of symbol name to visibility (for \
            those symbols listed in an accessibility statement).
        :type visibility_map: dict with str keys and \
            :py:class:`psyclone.psyir.symbols.Symbol.Visibility` values

        :raises GenerationError: if the parse tree for a use statement has an \
            unrecognised structure.
        :raises SymbolError: if a symbol imported via a use statement is \
            already present in the symbol table.
        :raises NotImplementedError: if the form of use statement is not \
            supported.

        '''
        if visibility_map is None:
            visibility_map = {}

        for decl in walk(nodes, Fortran2003.Use_Stmt):

            # Check that the parse tree is what we expect
            if len(decl.items) != 5:
                # We can't just do str(decl) as that also checks that items
                # is of length 5
                text = ""
                for item in decl.items:
                    if item:
                        text += str(item)
                raise GenerationError(
                    f"Expected the parse tree for a USE statement to contain "
                    f"5 items but found {len(decl.items)} for '{text}'")

            mod_name = str(decl.items[2])
            mod_visibility = visibility_map.get(
                    mod_name,  parent.symbol_table.default_visibility)

            # Add the module symbol to the symbol table. Keep a record of
            # whether or not we've seen this module before for reporting
            # purposes in the code below.
            if mod_name not in parent.symbol_table:
                new_container = True
                container = ContainerSymbol(mod_name,
                                            visibility=mod_visibility)
                parent.symbol_table.add(container)
            else:
                new_container = False
                container = parent.symbol_table.lookup(mod_name)
                if not isinstance(container, ContainerSymbol):
                    raise SymbolError(
                        f"Found a USE of module '{mod_name}' but the symbol "
                        f"table already has a non-container entry with that "
                        f"name ({container}). This is invalid Fortran.")

            # Create a generic Symbol for each element in the ONLY clause.
            if isinstance(decl.items[4], Fortran2003.Only_List):
                if not new_container and not container.wildcard_import and \
                   not parent.symbol_table.symbols_imported_from(container):
                    # TODO #11 Log the fact that this explicit symbol import
                    # will replace a previous import with an empty only-list.
                    pass
                for name in decl.items[4].items:
                    sym_name = str(name).lower()
                    sym_visibility = visibility_map.get(
                        sym_name,  parent.symbol_table.default_visibility)
                    if sym_name not in parent.symbol_table:
                        # We're dealing with a symbol named in a use statement
                        # in the *current* scope therefore we do not check
                        # any ancestor symbol tables; we just create a
                        # new symbol. Since we don't yet know anything about
                        # the type of this symbol we create a generic Symbol.
                        parent.symbol_table.add(
                            Symbol(sym_name, visibility=sym_visibility,
                                   interface=ImportInterface(container)))
                    else:
                        # There's already a symbol with this name
                        existing_symbol = parent.symbol_table.lookup(
                            sym_name)
                        if not existing_symbol.is_import:
                            raise SymbolError(
                                f"Symbol '{sym_name}' is imported from module "
                                f"'{mod_name}' but is already present in the "
                                f"symbol table as either an argument or a "
                                f"local ({existing_symbol}).")
                        # TODO #11 Log the fact that we've already got an
                        # import of this symbol and that will take precendence.
            elif not decl.items[3]:
                # We have a USE statement without an ONLY clause.
                if not new_container and not container.wildcard_import and \
                   not parent.symbol_table.symbols_imported_from(container):
                    # TODO #11 Log the fact that this explicit symbol import
                    # will replace a previous import that had an empty
                    # only-list.
                    pass
                container.wildcard_import = True
            elif decl.items[3].lower().replace(" ", "") == ",only:":
                # This use has an 'only: ' but no associated list of
                # imported symbols. (It serves to keep a module in scope while
                # not actually importing anything from it.) We do not need to
                # set anything as the defaults (empty 'only' list and no
                # wildcard import) imply 'only:'.
                if not new_container and \
                       (container.wildcard_import or
                        parent.symbol_table.symbols_imported_from(container)):
                    # TODO #11 Log the fact that this import with an empty
                    # only-list is ignored because of existing 'use's of
                    # the module.
                    pass
            else:
                raise NotImplementedError(f"Found unsupported USE statement: "
                                          f"'{decl}'")

    def _process_type_spec(self, parent, type_spec):
        '''
        Processes the fparser2 parse tree of a type specification in order to
        extract the type and precision that are specified.

        :param parent: the parent of the current PSyIR node under construction.
        :type parent: :py:class:`psyclone.psyir.nodes.Node`
        :param type_spec: the fparser2 parse tree of the type specification.
        :type type_spec: \
            :py:class:`fparser.two.Fortran2003.Intrinsic_Type_Spec` or \
            :py:class:`fparser.two.Fortran2003.Declaration_Type_Spec`

        :returns: the type and precision specified by the type-spec.
        :rtype: 2-tuple of :py:class:`psyclone.psyir.symbols.ScalarType` or \
            :py:class:`psyclone.psyir.symbols.DataTypeSymbol` and \
            :py:class:`psyclone.psyir.symbols.DataSymbol.Precision` or \
            :py:class:`psyclone.psyir.symbols.DataSymbol` or int or NoneType

        :raises NotImplementedError: if an unsupported intrinsic type is found.
        :raises SymbolError: if a symbol already exists for the name of a \
            derived type but is not a DataTypeSymbol.
        :raises NotImplementedError: if the supplied type specification is \
            not for an intrinsic type or a derived type.

        '''
        base_type = None
        precision = None

        if isinstance(type_spec, Fortran2003.Intrinsic_Type_Spec):
            fort_type = str(type_spec.items[0]).lower()
            try:
                data_name = TYPE_MAP_FROM_FORTRAN[fort_type]
            except KeyError as err:
                raise NotImplementedError(
                    f"Could not process {type_spec}. Only 'real', 'double "
                    f"precision', 'integer', 'logical' and 'character' "
                    f"intrinsic types are supported.") from err
            if fort_type == "double precision":
                # Fortran double precision is equivalent to a REAL
                # intrinsic with precision DOUBLE in the PSyIR.
                precision = ScalarType.Precision.DOUBLE
            else:
                # Check for precision being specified.
                precision = self._process_precision(type_spec, parent)
            if not precision:
                precision = default_precision(data_name)
            # We don't support len or kind specifiers for character variables
            if fort_type == "character" and type_spec.children[1]:
                raise NotImplementedError(
                    f"Length or kind attributes not supported on a character "
                    f"variable: '{type_spec}'")
            base_type = ScalarType(data_name, precision)

        elif isinstance(type_spec, Fortran2003.Declaration_Type_Spec):
            # This is a variable of derived type
            if type_spec.children[0].lower() != "type":
                # We don't yet support declarations that use 'class'
                # TODO #1504 extend the PSyIR for this variable type.
                raise NotImplementedError(
                    f"Could not process {type_spec} - declarations "
                    f"other than 'type' are not yet supported.")
            type_name = str(walk(type_spec, Fortran2003.Type_Name)[0])
            # Do we already have a Symbol for this derived type?
            type_symbol = _find_or_create_imported_symbol(parent, type_name)
            # pylint: disable=unidiomatic-typecheck
            if type(type_symbol) == Symbol:
                # We do but we didn't know what kind of symbol it was. Create
                # a DataTypeSymbol to replace it.
                new_symbol = DataTypeSymbol(type_name, DeferredType(),
                                            interface=type_symbol.interface)
                table = type_symbol.find_symbol_table(parent)
                table.swap(type_symbol, new_symbol)
                type_symbol = new_symbol
            elif not isinstance(type_symbol, DataTypeSymbol):
                raise SymbolError(
                    f"Search for a DataTypeSymbol named '{type_name}' "
                    f"(required by specification '{type_spec}') found a "
                    f"'{type(type_symbol).__name__}' instead.")
            base_type = type_symbol

        else:
            # Not a supported type specification. This will result in a
            # CodeBlock or UnknownFortranType, depending on the context.
            raise NotImplementedError()

        return base_type, precision

    def _process_decln(self, parent, symbol_table, decl, visibility_map=None):
        '''
        Process the supplied fparser2 parse tree for a declaration. For each
        entity that is declared, a symbol is added to the supplied symbol
        table.

        :param parent: PSyIR node in which to insert the symbols found.
        :type parent: :py:class:`psyclone.psyir.nodes.KernelSchedule`
        :param symbol_table: the symbol table to which to add new symbols.
        :type symbol_table: py:class:`psyclone.psyir.symbols.SymbolTable`
        :param decl: fparser2 parse tree of declaration to process.
        :type decl: :py:class:`fparser.two.Fortran2003.Type_Declaration_Stmt`
        :param visibility_map: mapping of symbol name to visibility (for \
            those symbols listed in an accessibility statement).
        :type visibility_map: dict with str keys and \
            :py:class:`psyclone.psyir.symbols.Symbol.Visibility` values

        :raises NotImplementedError: if the save attribute is encountered on \
            a declaration that is not within a module.
        :raises NotImplementedError: if an unsupported attribute is found.
        :raises NotImplementedError: if an unsupported intent attribute is \
            found.
        :raises NotImplementedError: if an unsupported access-spec attribute \
            is found.
        :raises NotImplementedError: if the allocatable attribute is found on \
            a non-array declaration.
        :raises InternalError: if an array with defined extent has the \
            allocatable attribute.
        :raises NotImplementedError: if an initialisation expression is found \
            for a variable declaration.
        :raises NotImplementedError: if an unsupported initialisation \
            expression is found for a parameter declaration.
        :raises NotImplementedError: if a character-length specification is \
            found.
        :raises SymbolError: if a declaration is found for a symbol that is \
            already present in the symbol table with a defined interface.

        '''
        (type_spec, attr_specs, entities) = decl.items

        # Parse the type_spec
        base_type, _ = self._process_type_spec(parent, type_spec)

        # Parse declaration attributes:
        # 1) If no dimension attribute is provided, it defaults to scalar.
        attribute_shape = []
        # 2) If no intent attribute is provided, it is provisionally
        # marked as a local variable (when the argument list is parsed,
        # arguments with no explicit intent are updated appropriately).
        interface = LocalInterface()
        # 3) Record initialized constant values
        has_constant_value = False
        # 4) Whether the declaration has the allocatable attribute
        allocatable = False
        # 5) Access-specification - this var is only set if the declaration
        # has an explicit access-spec (e.g. INTEGER, PRIVATE :: xxx)
        decln_access_spec = None
        if attr_specs:
            for attr in attr_specs.items:
                if isinstance(attr, Fortran2003.Attr_Spec):
                    normalized_string = str(attr).lower().replace(' ', '')
                    if "save" in normalized_string:
                        # Variables declared with SAVE attribute inside a
                        # module, submodule or main program are implicitly
                        # SAVE'd (see Fortran specification 8.5.16.4) so it
                        # is valid to ignore the attribute in these
                        # situations.
                        if not (decl.parent and
                                isinstance(decl.parent.parent,
                                           (Fortran2003.Module,
                                            Fortran2003.Main_Program))):
                            raise NotImplementedError(
                                f"Could not process {decl.items}. The 'SAVE' "
                                f"attribute is not yet supported when it is"
                                f" not part of a module, submodule or main_"
                                f"program specification part.")

                    elif normalized_string == "parameter":
                        # Flag the existence of a constant value in the RHS
                        has_constant_value = True
                    elif normalized_string == "allocatable":
                        allocatable = True
                    else:
                        raise NotImplementedError(
                            f"Could not process {decl.items}. Unrecognised "
                            f"attribute '{attr}'.")
                elif isinstance(attr, Fortran2003.Intent_Attr_Spec):
                    (_, intent) = attr.items
                    normalized_string = \
                        intent.string.lower().replace(' ', '')
                    try:
                        interface = ArgumentInterface(
                            INTENT_MAPPING[normalized_string])
                    except KeyError as info:
                        message = (
                            f"Could not process {decl.items}. Unexpected "
                            f"intent attribute '{attr}'.")
                        raise InternalError(message) from info
                elif isinstance(attr,
                                (Fortran2003.Dimension_Attr_Spec,
                                 Fortran2003.Dimension_Component_Attr_Spec)):
                    attribute_shape = \
                        self._parse_dimensions(attr, symbol_table)
                elif isinstance(attr, Fortran2003.Access_Spec):
                    try:
                        decln_access_spec = _process_access_spec(attr)
                    except InternalError as err:
                        raise InternalError(
                            f"Could not process '{decl.items}': "
                            f"{err.value}") from err
                else:
                    raise NotImplementedError(
                        f"Could not process declaration '{decl}'. Unrecognised"
                        f" attribute type '{type(attr).__name__}'.")

        # Parse declarations RHS and declare new symbol into the
        # parent symbol table for each entity found.
        for entity in entities.items:
            (name, array_spec, char_len, initialisation) = entity.items
            ct_expr = None

            # If the entity has an array-spec shape, it has priority.
            # Otherwise use the declaration attribute shape.
            if array_spec is not None:
                entity_shape = \
                    self._parse_dimensions(array_spec, symbol_table)
            else:
                entity_shape = attribute_shape

            if allocatable and not entity_shape:
                # We have an allocatable attribute on something that we
                # don't recognise as an array - this is not supported.
                raise NotImplementedError(
                    f"Could not process {decl}. The 'allocatable' attribute is"
                    f" only supported on array declarations.")

            for idx, extent in enumerate(entity_shape):
                if extent is None:
                    if allocatable:
                        entity_shape[idx] = ArrayType.Extent.DEFERRED
                    else:
                        entity_shape[idx] = ArrayType.Extent.ATTRIBUTE
                elif not isinstance(extent, ArrayType.Extent) and \
                        allocatable:
                    # We have an allocatable array with a defined extent.
                    # This is invalid Fortran.
                    raise InternalError(
                        f"Invalid Fortran: '{decl}'. An array with defined "
                        f"extent cannot have the ALLOCATABLE attribute.")

            if initialisation:
                if has_constant_value:
                    # If it is a parameter parse its initialization into
                    # a dummy Assignment (but connected to the parent scope
                    # since symbols must be resolved)
                    dummynode = Assignment(parent=parent)
                    expr = initialisation.items[1]
                    self.process_nodes(parent=dummynode, nodes=[expr])
                    ct_expr = dummynode.children[0].detach()
                else:
                    raise NotImplementedError(
                        f"Could not process {decl.items}. Initialisations on "
                        f"the declaration statements are only supported for "
                        f"parameter declarations.")

            if char_len is not None:
                raise NotImplementedError(
                    f"Could not process {decl.items}. Character length "
                    f"specifications are not supported.")

            sym_name = str(name).lower()

            if decln_access_spec:
                visibility = decln_access_spec
            else:
                # There was no access-spec on the LHS of the decln
                if visibility_map is not None:
                    visibility = visibility_map.get(
                        sym_name, symbol_table.default_visibility)
                else:
                    visibility = symbol_table.default_visibility

            if entity_shape:
                # array
                datatype = ArrayType(base_type, entity_shape)
            else:
                # scalar
                datatype = base_type

            # Make sure the declared symbol exists in the SymbolTable
            tag = None
            try:
                sym = symbol_table.lookup(sym_name, scope_limit=parent)
                if sym is symbol_table.lookup_with_tag("own_routine_symbol"):
                    # In case it is its own function routine symbol, Fortran
                    # will declare it inside the function as a DataSymbol.
                    # Remove the RoutineSymbol in order to free the exact name
                    # for the DataSymbol.
                    symbol_table.remove(sym)
                    # And trigger the exception path but keeping the same tag
                    tag = "own_routine_symbol"
                    raise KeyError
                if not sym.is_unresolved:
                    raise SymbolError(
                        f"Symbol '{sym_name}' already present in SymbolTable "
                        f"with a defined interface ({sym.interface}).")
            except KeyError:
                try:
                    sym = DataSymbol(sym_name, datatype,
                                     visibility=visibility,
                                     constant_value=ct_expr)
                except ValueError:
                    # Error setting initial value have to be raised as
                    # NotImplementedError in order to create an UnknownType
                    # Therefore, the Error doesn't need raise_from or message
                    # pylint: disable=raise-missing-from
                    if tag:
                        raise InternalError(
                            f"The fparser2 frontend does not support "
                            f"declarations where the routine name is of "
                            f"UnknownType, but found this case in "
                            f"'{sym_name}'.")
                    raise NotImplementedError()

                symbol_table.add(sym, tag=tag)

            # The Symbol must have the interface given by the declaration. We
            # take a copy to ensure that it can be modified without side
            # effects.
            # TODO #1444 Can we ensure that an interface is only referenced
            # by a single symbol?
            sym.interface = interface.copy()

    def _process_derived_type_decln(self, parent, decl, visibility_map):
        '''
        Process the supplied fparser2 parse tree for a derived-type
        declaration. A DataTypeSymbol representing the derived-type is added
        to the symbol table associated with the parent node.

        :param parent: PSyIR node in which to insert the symbols found.
        :type parent: :py:class:`psyclone.psyGen.KernelSchedule`
        :param decl: fparser2 parse tree of declaration to process.
        :type decl: :py:class:`fparser.two.Fortran2003.Type_Declaration_Stmt`
        :param visibility_map: mapping of symbol name to visibility (for \
            those symbols listed in an accessibility statement).
        :type visibility_map: dict with str keys and \
            :py:class:`psyclone.psyir.symbols.Symbol.Visibility` values

        :raises SymbolError: if a Symbol already exists with the same name \
            as the derived type being defined and it is not a DataTypeSymbol \
            or is not of DeferredType.

        '''
        name = str(walk(decl.children[0], Fortran2003.Type_Name)[0]).lower()
        # Create a new StructureType for this derived type
        dtype = StructureType()

        # Look for any private-components-stmt (R447) within the type
        # decln. In the absence of this, the default visibility of type
        # components is public.
        private_stmts = walk(decl, Fortran2003.Private_Components_Stmt)
        if private_stmts:
            default_compt_visibility = Symbol.Visibility.PRIVATE
        else:
            default_compt_visibility = Symbol.Visibility.PUBLIC

        # The visibility of the symbol representing this derived type
        if name in visibility_map:
            dtype_symbol_vis = visibility_map[name]
        else:
            specs = walk(decl.children[0], Fortran2003.Access_Spec)
            if specs:
                dtype_symbol_vis = _process_access_spec(specs[0])
            else:
                dtype_symbol_vis = parent.symbol_table.default_visibility

        # We have to create the symbol for this type before processing its
        # components as they may refer to it (e.g. for a linked list).
        if name in parent.symbol_table:
            # An entry already exists for this type.
            # Check that it is a DataTypeSymbol
            tsymbol = parent.symbol_table.lookup(name)
            if not isinstance(tsymbol, DataTypeSymbol):
                raise SymbolError(
                    f"Error processing definition of derived type '{name}'. "
                    f"The symbol table already contains an entry with this "
                    f"name but it is a '{type(tsymbol).__name__}' when it "
                    f"should be a 'DataTypeSymbol' (for the derived-type "
                    f"definition '{decl}')")
            # Since we are processing the definition of this symbol, the only
            # permitted type for an existing symbol of this name is 'deferred'.
            if not isinstance(tsymbol.datatype, DeferredType):
                raise SymbolError(
                    f"Error processing definition of derived type '{name}'. "
                    f"The symbol table already contains a DataTypeSymbol with "
                    f"this name but it is of type "
                    f"'{type(tsymbol.datatype).__name__}' when it should be "
                    f"of 'DeferredType'")
        else:
            # We don't already have an entry for this type so create one
            tsymbol = DataTypeSymbol(name, dtype, visibility=dtype_symbol_vis)
            parent.symbol_table.add(tsymbol)

        # Populate this StructureType by processing the components of
        # the derived type
        try:
            # We don't yet support derived-type definitions with a CONTAINS
            # section.
            contains = walk(decl, Fortran2003.Contains_Stmt)
            if contains:
                raise NotImplementedError(
                    "Derived-type definition has a CONTAINS statement.")

            # Re-use the existing code for processing symbols
            local_table = SymbolTable(
                default_visibility=default_compt_visibility)
            for child in walk(decl, Fortran2003.Data_Component_Def_Stmt):
                self._process_decln(parent, local_table, child)
            # Convert from Symbols to type information
            for symbol in local_table.symbols:
                dtype.add(symbol.name, symbol.datatype, symbol.visibility)

            # Update its type with the definition we've found
            tsymbol.datatype = dtype

        except NotImplementedError:
            # Support for this declaration is not fully implemented so
            # set the datatype of the DataTypeSymbol to UnknownFortranType.
            tsymbol.datatype = UnknownFortranType(str(decl))

    def process_declarations(self, parent, nodes, arg_list,
                             visibility_map=None):
        '''
        Transform the variable declarations in the fparser2 parse tree into
        symbols in the symbol table of the PSyIR parent node. The default
        visibility of any new symbol is taken from the symbol table associated
        with the `parent` node if necessary. The `visibility_map` provides
        information on any explicit symbol visibilities that are specified
        for the declarations.

        :param parent: PSyIR node in which to insert the symbols found.
        :type parent: :py:class:`psyclone.psyir.nodes.KernelSchedule`
        :param nodes: fparser2 AST nodes containing declaration statements.
        :type nodes: list of :py:class:`fparser.two.utils.Base`
        :param arg_list: fparser2 AST node containing the argument list.
        :type arg_list: :py:class:`fparser.Fortran2003.Dummy_Arg_List`
        :param visibility_map: mapping of symbol names to explicit
                        visibilities.
        :type visibility_map: dict with str keys and values of type \
                        :py:class:`psyclone.psyir.symbols.Symbol.Visibility`

        :raises NotImplementedError: the provided declarations contain \
                                     attributes which are not supported yet.
        :raises GenerationError: if the parse tree for a USE statement does \
                                 not have the expected structure.
        :raises SymbolError: if a declaration is found for a Symbol that is \
                    already in the symbol table with a defined interface.
        :raises InternalError: if the provided declaration is an unexpected \
                               or invalid fparser or Fortran expression.

        '''
        if visibility_map is None:
            visibility_map = {}

        # Look at any USE statements
        self._process_use_stmts(parent, nodes, visibility_map)

        # Handle any derived-type declarations/definitions before we look
        # at general variable declarations in case any of the latter use
        # the former.
        for decl in walk(nodes, Fortran2003.Derived_Type_Def):
            self._process_derived_type_decln(parent, decl, visibility_map)

        # Now we've captured any derived-type definitions, proceed to look
        # at the variable declarations.
        for node in nodes:

            if isinstance(node, Fortran2003.Interface_Block):

                # We only support named interface blocks, and then only
                # partially. Fortran standard R1203 says that:
                #    interface-stmt = INTERFACE [ generic-spec ]
                # where generic-spec is either (R1207) a generic-name or one
                # of OPERATOR, ASSIGNMENT or dtio-spec.
                if (not isinstance(node.children[0],
                                   Fortran2003.Interface_Stmt) or
                    not isinstance(node.children[0].children[0],
                                   Fortran2003.Name)):
                    # An unsupported interface definition will result in
                    # the whole module containing this specification part
                    # being put into a CodeBlock.
                    raise NotImplementedError()
                name = node.children[0].children[0].string.lower()
                vis = visibility_map.get(
                    name, parent.symbol_table.default_visibility)
                # A named interface block corresponds to a RoutineSymbol.
                # (There will be calls to it although there will be no
                # corresponding implementation with that name.)
                # We store its definition using an UnknownFortranType so that
                # we can recreate it in the Fortran backend.
                parent.symbol_table.add(
                    RoutineSymbol(name, UnknownFortranType(str(node).lower()),
                                  visibility=vis))

            elif isinstance(node, Fortran2003.Type_Declaration_Stmt):
                try:
                    self._process_decln(parent, parent.symbol_table, node,
                                        visibility_map)
                except NotImplementedError:
                    # Found an unsupported variable declaration. Create a
                    # DataSymbol with UnknownType for each entity being
                    # declared. Currently this means that any symbols that come
                    # after an unsupported declaration will also have
                    # UnknownType. This is the subject of Issue #791.
                    specs = walk(node, Fortran2003.Access_Spec)
                    if specs:
                        decln_vis = _process_access_spec(specs[0])
                    else:
                        decln_vis = parent.symbol_table.default_visibility

                    orig_children = list(node.children[2].children[:])
                    for child in orig_children:
                        # Modify the fparser2 parse tree so that it only
                        # declares the current entity. `items` is a tuple and
                        # thus immutable so we create a new one.
                        node.children[2].items = (child,)
                        symbol_name = str(child.children[0]).lower()
                        vis = visibility_map.get(symbol_name, decln_vis)

                        # Check whether the symbol we're about to add
                        # corresponds to the routine we're currently inside. If
                        # it does then we remove the RoutineSymbol in order to
                        # free the exact name for the DataSymbol, but we keep
                        # the tag to reintroduce it to the new symbol.
                        tag = None
                        try:
                            routine_sym = parent.symbol_table.lookup_with_tag(
                                "own_routine_symbol")
                            if routine_sym.name.lower() == symbol_name:
                                parent.symbol_table.remove(routine_sym)
                                tag = "own_routine_symbol"  # Keep the tag
                        except KeyError:
                            pass

                        # If a declaration declares multiple entities, it's
                        # possible that some may have already been processed
                        # successfully and thus be in the symbol table.
                        try:
                            parent.symbol_table.add(
                                DataSymbol(symbol_name,
                                           UnknownFortranType(str(node)),
                                           visibility=vis),
                                tag=tag)
                        except KeyError as err:
                            if len(orig_children) == 1:
                                raise SymbolError(
                                    f"Error while processing unsupported "
                                    f"declaration ('{node}'). An entry for "
                                    f"symbol '{symbol_name}' is already in "
                                    f"the symbol table.") from err
                    # Restore the fparser2 parse tree
                    node.children[2].items = tuple(orig_children)

            elif isinstance(node, (Fortran2003.Access_Stmt,
                                   Fortran2003.Derived_Type_Def,
                                   Fortran2003.Stmt_Function_Stmt,
                                   Fortran2003.Use_Stmt,
                                   Fortran2003.Implicit_Part)):
                # These node types are handled separately with the exception
                # of Implicit_Part which we currently silently ignore
                # (TODO #1254).
                pass

            else:
                raise NotImplementedError(
                    f"Error processing declarations: fparser2 node of type "
                    f"'{type(node).__name__}' not supported")

        if visibility_map is not None:
            # Check for symbols named in an access statement but not explicitly
            # declared. These must then refer to symbols that have been brought
            # into scope by an unqualified use statement.
            for name, vis in visibility_map.items():
                if name not in parent.symbol_table:
                    try:
                        # If a suitable unqualified use statement is found then
                        # this call creates a Symbol and inserts it in the
                        # appropriate symbol table.
                        _find_or_create_imported_symbol(parent, name,
                                                        visibility=vis)
                    except SymbolError as err:
                        # Improve the error message with context-specific info
                        raise SymbolError(
                            f"'{name}' is listed in an accessibility "
                            f"statement as being '{vis}' but failed to find a "
                            f"declaration or possible import (use) of this "
                            f"symbol.") from err
        try:
            arg_symbols = []
            # Ensure each associated symbol has the correct interface info.
            for arg_name in [x.string.lower() for x in arg_list]:
                symbol = parent.symbol_table.lookup(arg_name)
                if symbol.is_local:
                    # We didn't previously know that this Symbol was an
                    # argument (as it had no 'intent' qualifier). Mark
                    # that it is an argument by specifying its interface.
                    # Athough a Fortran argument has intent(inout) by default,
                    # specifying this for an argument that is actually read
                    # only (and is declared as such in the caller) causes
                    # gfortran to complain. We therefore specify that the
                    # access is unknown at this stage.
                    symbol.interface = ArgumentInterface(
                        ArgumentInterface.Access.UNKNOWN)
                arg_symbols.append(symbol)
            # Now that we've updated the Symbols themselves, set the
            # argument list
            parent.symbol_table.specify_argument_list(arg_symbols)
        except KeyError:
            raise InternalError(f"The kernel argument list '{arg_list}' does "
                                f"not match the variable declarations for "
                                f"fparser nodes {nodes}.")

        # fparser2 does not always handle Statement Functions correctly, this
        # loop checks for Stmt_Functions that should be an array statement
        # and recovers them, otherwise it raises an error as currently
        # Statement Functions are not supported in PSyIR.
        for stmtfn in walk(nodes, Fortran2003.Stmt_Function_Stmt):
            (fn_name, arg_list, scalar_expr) = stmtfn.items
            try:
                symbol = parent.symbol_table.lookup(fn_name.string.lower())
                if symbol.is_array:
                    # This is an array assignment wrongly categorized as a
                    # statement_function by fparser2.
                    array_subscript = arg_list.items

                    assignment_rhs = scalar_expr

                    # Create assingment node
                    assignment = Assignment(parent=parent)
                    parent.addchild(assignment)

                    # Build lhs
                    lhs = ArrayReference(symbol, parent=assignment)
                    self.process_nodes(parent=lhs, nodes=array_subscript)
                    assignment.addchild(lhs)

                    # Build rhs
                    self.process_nodes(parent=assignment,
                                       nodes=[assignment_rhs])
                else:
                    raise InternalError(
                        f"Could not process '{stmtfn}'. Symbol "
                        f"'{symbol.name}' is in the SymbolTable but it is not "
                        f"an array as expected, so it can not be recovered as "
                        f"an array assignment.")
            except KeyError:
                raise NotImplementedError(
                    f"Could not process '{stmtfn}'. Statement Function "
                    f"declarations are not supported.")

    @staticmethod
    def _process_precision(type_spec, psyir_parent):
        '''Processes the fparser2 parse tree of the type specification of a
        variable declaration in order to extract precision
        information. Two formats for specifying precision are
        supported a) "*N" e.g. real*8 and b) "kind=" e.g. kind=i_def, or
        kind=KIND(x).

        :param type_spec: the fparser2 parse tree of the type specification.
        :type type_spec: \
            :py:class:`fparser.two.Fortran2003.Intrinsic_Type_Spec`
        :param psyir_parent: the parent PSyIR node where the new node \
            will be attached.
        :type psyir_parent: :py:class:`psyclone.psyir.nodes.Node`

        :returns: the precision associated with the type specification.
        :rtype: :py:class:`psyclone.psyir.symbols.DataSymbol.Precision` or \
            :py:class:`psyclone.psyir.symbols.DataSymbol` or int or NoneType

        :raises NotImplementedError: if a KIND intrinsic is found with an \
            argument other than a real or integer literal.
        :raises NotImplementedError: if we have `kind=xxx` but cannot find \
            a valid variable name.

        '''
        symbol_table = psyir_parent.scope.symbol_table

        if not isinstance(type_spec.items[1], Fortran2003.Kind_Selector):
            # No precision is specified
            return None

        kind_selector = type_spec.items[1]

        if (isinstance(kind_selector.children[0], str) and
                kind_selector.children[0] == "*"):
            # Precision is provided in the form *N
            precision = int(str(kind_selector.children[1]))
            return precision

        # Precision is supplied in the form "kind=..."
        intrinsics = walk(kind_selector.items,
                          Fortran2003.Intrinsic_Function_Reference)
        if intrinsics and isinstance(intrinsics[0].items[0],
                                     Fortran2003.Intrinsic_Name) and \
           str(intrinsics[0].items[0]).lower() == "kind":
            # We have kind=KIND(X) where X may be of any intrinsic type. It
            # may be a scalar or an array. items[1] is an
            # Actual_Arg_Spec_List with the first entry being the argument.
            kind_arg = intrinsics[0].items[1].items[0]

            # We currently only support integer and real literals as
            # arguments to KIND
            if isinstance(kind_arg, (Fortran2003.Int_Literal_Constant,
                                     Fortran2003.Real_Literal_Constant)):
                return get_literal_precision(kind_arg, psyir_parent)

            raise NotImplementedError(
                f"Only real and integer literals are supported as arguments "
                f"to the KIND intrinsic but found "
                f"'{type(kind_arg).__name__}' in: {kind_selector}")

        # We have kind=kind-param
        kind_names = walk(kind_selector.items, Fortran2003.Name)
        if not kind_names:
            raise NotImplementedError(
                f"Failed to find valid Name in Fortran Kind Selector: "
                f"{kind_selector}'")

        return _kind_find_or_create(str(kind_names[0]), symbol_table)

    def process_nodes(self, parent, nodes):
        '''
        Create the PSyIR of the supplied list of nodes in the
        fparser2 AST.

        :param parent: Parent node in the PSyIR we are constructing.
        :type parent: :py:class:`psyclone.psyir.nodes.Node`
        :param nodes: List of sibling nodes in fparser2 AST.
        :type nodes: list of :py:class:`fparser.two.utils.Base`

        '''
        code_block_nodes = []
        for child in nodes:

            try:
                psy_child = self._create_child(child, parent)
            except NotImplementedError:
                # If child type implementation not found, add them on the
                # ongoing code_block node list.
                code_block_nodes.append(child)
                if not isinstance(parent, Schedule):
                    # If we're not processing a statement then we create a
                    # separate CodeBlock for each node in the parse tree.
                    # (Otherwise it is hard to correctly reconstruct e.g.
                    # the arguments to a Call.)
                    self.nodes_to_code_block(parent, code_block_nodes)
            else:
                if psy_child:
                    self.nodes_to_code_block(parent, code_block_nodes)
                    parent.addchild(psy_child)
                # If psy_child is not initialised but it didn't produce a
                # NotImplementedError, it means it is safe to ignore it.

        # Complete any unfinished code-block
        self.nodes_to_code_block(parent, code_block_nodes)

    def _create_child(self, child, parent=None):
        '''
        Create a PSyIR node representing the supplied fparser 2 node.

        :param child: node in fparser2 AST.
        :type child: :py:class:`fparser.two.utils.Base`
        :param parent: Parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyir.nodes.Node`

        :returns: Returns the PSyIR representation of child, which can be a \
                  single node, a tree of nodes or None if the child can be \
                  ignored.
        :rtype: :py:class:`psyclone.psyir.nodes.Node` or NoneType

        :raises NotImplementedError: if the child node has a label or there \
            isn't a handler for the provided child type.

        '''
        # We don't support statements with labels.
        if isinstance(child, BlockBase):
            # An instance of BlockBase describes a block of code (no surprise
            # there), so we have to examine the first statement within it. We
            # must allow for the case where the block is empty though.
            if (child.content and child.content[0] and
                    child.content[0].item and child.content[0].item.label):
                raise NotImplementedError()
        elif isinstance(child, StmtBase):
            if child.item and child.item.label:
                raise NotImplementedError()

        handler = self.handlers.get(type(child))
        if handler is None:
            # If the handler is not found then check with the first
            # level parent class. This is done to simplify the
            # handlers map when multiple fparser2 types can be
            # processed with the same handler. (e.g. Subclasses of
            # BinaryOpBase: Mult_Operand, Add_Operand, Level_2_Expr,
            # ... can use the same handler.)
            generic_type = type(child).__bases__[0]
            handler = self.handlers.get(generic_type)
            if not handler:
                raise NotImplementedError()
        return handler(child, parent)

    def _ignore_handler(self, *_):
        '''
        This handler returns None indicating that the associated
        fparser2 node can be ignored.

        Note that this method contains ignored arguments to comform with
        the handler(node, parent) method interface.

        :returns: None
        :rtype: NoneType
        '''
        return None

    def _create_loop(self, parent, variable):
        '''
        Create a Loop instance. This is done outside _do_construct_handler
        because some APIs may want to instantiate a specialised Loop.

        :param parent: the parent of the node.
        :type parent: :py:class:`psyclone.psyir.nodes.Node`
        :param variable: the loop variable.
        :type variable: :py:class:`psyclone.psyir.symbols.DataSymbol`

        :return: a new Loop instance.
        :rtype: :py:class:`psyclone.psyir.nodes.Loop`

        '''
        return Loop(parent=parent, variable=variable)

    def _do_construct_handler(self, node, parent):
        '''
        Transforms a fparser2 Do Construct into its PSyIR representation.

        :param node: node in fparser2 tree.
        :type node: \
            :py:class:`fparser.two.Fortran2003.Block_Nonlabel_Do_Construct`
        :param parent: parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyir.nodes.Node`

        :returns: PSyIR representation of node
        :rtype: :py:class:`psyclone.psyir.nodes.Loop`

        :raises NotImplementedError: if the fparser2 tree has an unsupported \
            structure (e.g. DO WHILE or a DO with no loop control or a named \
            DO containing a reference to that name).
        '''
        nonlabel_do = walk(node.content, Fortran2003.Nonlabel_Do_Stmt)[0]
        if nonlabel_do.item is not None:
            # If the associated line has a name that is referenced inside the
            # loop then it isn't supported , e.g. `EXIT outer_loop`.
            if nonlabel_do.item.name:
                construct_name = nonlabel_do.item.name
                # Check that the construct-name is not referred to inside
                # the Loop (but exclude the END DO from this check).
                names = walk(node.content[:-1], Fortran2003.Name)
                if construct_name in [name.string for name in names]:
                    raise NotImplementedError()

        ctrl = walk(node.content, Fortran2003.Loop_Control)
        if not ctrl:
            # TODO #359 a DO with no loop control is put into a CodeBlock
            raise NotImplementedError()
        if ctrl[0].items[0]:
            # If this is a DO WHILE then the first element of items will not
            # be None. (See `fparser.two.Fortran2003.Loop_Control`.)
            # TODO #359 DO WHILE's are currently just put into CodeBlocks
            # rather than being properly described in the PSyIR.
            raise NotImplementedError()

        # Second element of items member of Loop Control is itself a tuple
        # containing:
        #   Loop variable, [start value expression, end value expression, step
        #   expression]
        # Loop variable will be an instance of Fortran2003.Name
        loop_var = str(ctrl[0].items[1][0])
        variable_name = str(loop_var)
        try:
            data_symbol = _find_or_create_imported_symbol(
                parent, variable_name, symbol_type=DataSymbol,
                datatype=DeferredType())
        except SymbolError:
            raise InternalError(
                f"Loop-variable name '{variable_name}' is not declared and "
                f"there are no unqualified use statements. This is currently "
                f"unsupported.")
        # The loop node is created with the _create_loop factory method as some
        # APIs require a specialised loop node type.
        loop = self._create_loop(parent, data_symbol)
        loop._ast = node

        # Get the loop limits. These are given in a list which is the second
        # element of a tuple which is itself the second element of the items
        # tuple:
        # (None, (Name('jk'), [Int_Literal_Constant('1', None), Name('jpk'),
        #                      Int_Literal_Constant('1', None)]), None)
        limits_list = ctrl[0].items[1][1]

        # Start expression child
        self.process_nodes(parent=loop, nodes=[limits_list[0]])

        # Stop expression child
        self.process_nodes(parent=loop, nodes=[limits_list[1]])

        # Step expression child
        if len(limits_list) == 3:
            self.process_nodes(parent=loop, nodes=[limits_list[2]])
        else:
            # Default loop increment is 1. Use the type of the start
            # or step nodes once #685 is complete. For the moment use
            # the default precision.
            default_step = Literal("1", default_integer_type())
            loop.addchild(default_step)

        # Create Loop body Schedule
        loop_body = Schedule(parent=loop)
        loop_body._ast = node
        loop.addchild(loop_body)
        # Process loop body (ignore 'do' and 'end do' statements with [1:-1])
        self.process_nodes(parent=loop_body, nodes=node.content[1:-1])

        return loop

    def _if_construct_handler(self, node, parent):
        '''
        Transforms an fparser2 If_Construct to the PSyIR representation.

        :param node: node in fparser2 tree.
        :type node: :py:class:`fparser.two.Fortran2003.If_Construct`
        :param parent: Parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyir.nodes.Node`
        :returns: PSyIR representation of node
        :rtype: :py:class:`psyclone.psyir.nodes.IfBlock`
        :raises InternalError: If the fparser2 tree has an unexpected \
            structure.
        '''

        # Check that the fparser2 parsetree has the expected structure
        if not isinstance(node.content[0], Fortran2003.If_Then_Stmt):
            raise InternalError(
                f"Failed to find opening if then statement in: {node}")
        if not isinstance(node.content[-1], Fortran2003.End_If_Stmt):
            raise InternalError(
                f"Failed to find closing end if statement in: {node}")

        # Search for all the conditional clauses in the If_Construct
        clause_indices = []
        for idx, child in enumerate(node.content):
            if isinstance(child, (Fortran2003.If_Then_Stmt,
                                  Fortran2003.Else_Stmt,
                                  Fortran2003.Else_If_Stmt,
                                  Fortran2003.End_If_Stmt)):
                clause_indices.append(idx)

        # Deal with each clause: "if", "else if" or "else".
        ifblock = None
        currentparent = parent
        num_clauses = len(clause_indices) - 1
        for idx in range(num_clauses):
            start_idx = clause_indices[idx]
            end_idx = clause_indices[idx+1]
            clause = node.content[start_idx]

            if isinstance(clause, (Fortran2003.If_Then_Stmt,
                                   Fortran2003.Else_If_Stmt)):
                # If it's an 'IF' clause just create an IfBlock, otherwise
                # it is an 'ELSE' clause and it needs an IfBlock annotated
                # with 'was_elseif' inside a Schedule.
                newifblock = None
                if isinstance(clause, Fortran2003.If_Then_Stmt):
                    ifblock = IfBlock(parent=currentparent)
                    ifblock.ast = node  # Keep pointer to fpaser2 AST
                    newifblock = ifblock
                else:
                    elsebody = Schedule(parent=currentparent)
                    currentparent.addchild(elsebody)
                    newifblock = IfBlock(parent=elsebody,
                                         annotations=['was_elseif'])
                    elsebody.addchild(newifblock)

                    # Keep pointer to fpaser2 AST
                    elsebody.ast = node.content[start_idx]
                    newifblock.ast = node.content[start_idx]

                # Create condition as first child
                self.process_nodes(parent=newifblock,
                                   nodes=[clause.items[0]])

                # Create if-body as second child
                ifbody = Schedule(parent=newifblock)
                ifbody.ast = node.content[start_idx + 1]
                ifbody.ast_end = node.content[end_idx - 1]
                newifblock.addchild(ifbody)
                self.process_nodes(parent=ifbody,
                                   nodes=node.content[start_idx + 1:end_idx])

                currentparent = newifblock

            elif isinstance(clause, Fortran2003.Else_Stmt):
                if not idx == num_clauses - 1:
                    raise InternalError(
                        f"Else clause should only be found next to last "
                        f"clause, but found {node.content}")
                elsebody = Schedule(parent=currentparent)
                currentparent.addchild(elsebody)
                elsebody.ast = node.content[start_idx]
                elsebody.ast_end = node.content[end_idx]
                self.process_nodes(parent=elsebody,
                                   nodes=node.content[start_idx + 1:end_idx])
            else:
                raise InternalError(
                    f"Only fparser2 If_Then_Stmt, Else_If_Stmt and Else_Stmt "
                    f"are expected, but found {clause}.")

        return ifblock

    def _if_stmt_handler(self, node, parent):
        '''
        Transforms an fparser2 If_Stmt to the PSyIR representation.

        :param node: node in fparser2 AST.
        :type node: :py:class:`fparser.two.Fortran2003.If_Stmt`
        :param parent: Parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyir.nodes.Node`
        :returns: PSyIR representation of node
        :rtype: :py:class:`psyclone.psyir.nodes.IfBlock`
        '''
        ifblock = IfBlock(parent=parent, annotations=['was_single_stmt'])
        ifblock.ast = node
        self.process_nodes(parent=ifblock, nodes=[node.items[0]])
        ifbody = Schedule(parent=ifblock)
        ifblock.addchild(ifbody)
        self.process_nodes(parent=ifbody, nodes=[node.items[1]])
        return ifblock

    def _case_construct_handler(self, node, parent):
        '''
        Transforms an fparser2 Case_Construct to the PSyIR representation.

        :param node: node in fparser2 tree.
        :type node: :py:class:`fparser.two.Fortran2003.Case_Construct`
        :param parent: Parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyir.nodes.Node`

        :returns: PSyIR representation of node
        :rtype: :py:class:`psyclone.psyir.nodes.IfBlock`

        :raises InternalError: If the fparser2 tree has an unexpected \
            structure.
        :raises NotImplementedError: If the fparser2 tree contains an \
            unsupported structure and should be placed in a CodeBlock.

        '''
        # Check that the fparser2 parsetree has the expected structure
        if not isinstance(node.content[0], Fortran2003.Select_Case_Stmt):
            raise InternalError(
                f"Failed to find opening case statement in: {node}")
        if not isinstance(node.content[-1], Fortran2003.End_Select_Stmt):
            raise InternalError(
                f"Failed to find closing case statement in: {node}")

        # Search for all the CASE clauses in the Case_Construct. We do this
        # because the fp2 parse tree has a flat structure at this point with
        # the clauses being siblings of the contents of the clauses. The
        # final index in this list will hold the position of the end-select
        # statement.
        clause_indices = []
        selector = None
        # The position of the 'case default' clause, if any
        default_clause_idx = None
        for idx, child in enumerate(node.content):
            if isinstance(child, Fortran2003.Select_Case_Stmt):
                selector = child.items[0]
            if isinstance(child, Fortran2003.Case_Stmt):
                if not isinstance(child.items[0], Fortran2003.Case_Selector):
                    raise InternalError(
                        f"Unexpected parse tree structure. Expected child of "
                        f"Case_Stmt to be a Case_Selector but got: "
                        f"'{type(child.items[0]).__name__}'")
                case_expression = child.items[0].items[0]
                if case_expression is None:
                    # This is a 'case default' clause - store its position.
                    # We do this separately as this clause is special and
                    # will be added as a final 'else'.
                    default_clause_idx = idx
                clause_indices.append(idx)
            if isinstance(child, Fortran2003.End_Select_Stmt):
                clause_indices.append(idx)

        # Deal with each Case_Stmt
        rootif = None
        currentparent = parent
        num_clauses = len(clause_indices) - 1
        for idx in range(num_clauses):
            # Skip the 'default' clause for now because we handle it last
            if clause_indices[idx] == default_clause_idx:
                continue
            start_idx = clause_indices[idx]
            end_idx = clause_indices[idx+1]
            clause = node.content[start_idx]
            case = clause.items[0]

            # If rootif is already initialised we chain the new case in the
            # last else branch, otherwise we start a new IfBlock
            if rootif:
                elsebody = Schedule(parent=currentparent)
                currentparent.addchild(elsebody)
                ifblock = IfBlock(annotations=['was_case'])
                elsebody.addchild(ifblock)
                elsebody.ast = node.content[start_idx + 1]
                elsebody.ast_end = node.content[end_idx - 1]
            else:
                ifblock = IfBlock(parent=currentparent,
                                  annotations=['was_case'])
                rootif = ifblock

            if idx == 0:
                # If this is the first IfBlock then have it point to
                # the original SELECT CASE in the parse tree
                ifblock.ast = node
            else:
                # Otherwise, this IfBlock represents a CASE clause in the
                # Fortran and so we point to the parse tree of the content
                # of the clause.
                ifblock.ast = node.content[start_idx + 1]
                ifblock.ast_end = node.content[end_idx - 1]

            # Process the logical expression
            self._process_case_value_list(selector, case.items[0].items,
                                          ifblock)

            # Add If_body
            ifbody = Schedule(parent=ifblock)
            self.process_nodes(parent=ifbody,
                               nodes=node.content[start_idx + 1:
                                                  end_idx])
            ifblock.addchild(ifbody)
            ifbody.ast = node.content[start_idx + 1]
            ifbody.ast_end = node.content[end_idx - 1]

            currentparent = ifblock

        if default_clause_idx:
            # Finally, add the content of the 'default' clause as a last
            # 'else' clause. If the 'default' clause was the only clause
            # then 'rootif' will still be None and we don't bother creating
            # an enclosing IfBlock at all.
            if rootif:
                elsebody = Schedule(parent=currentparent)
                currentparent.addchild(elsebody)
                currentparent = elsebody
            start_idx = default_clause_idx + 1
            # Find the next 'case' clause that occurs after 'case default'
            # (if any)
            end_idx = -1
            for idx in clause_indices:
                if idx > default_clause_idx:
                    end_idx = idx
                    break
            # Process the statements within the 'default' clause
            self.process_nodes(parent=currentparent,
                               nodes=node.content[start_idx:end_idx])
            if rootif:
                elsebody.ast = node.content[start_idx]
                elsebody.ast_end = node.content[end_idx - 1]
        return rootif

    def _process_case_value_list(self, selector, nodes, parent):
        '''
        Processes the supplied list of fparser2 nodes representing case-value
        expressions and constructs the equivalent PSyIR representation.
        e.g. for:

               SELECT CASE(my_flag)
               CASE(var1, var2:var3, :var5)
                 my_switch = .true.
               END SELECT

        the equivalent logical expression is:

        my_flag == var1 OR (myflag>=var2 AND myflag <= var3) OR my_flag <= var5

        and the corresponding structure of the PSyIR that we create is:

                    OR
                   /  \
                 EQ    OR
                      /  \
                   AND    LE
                  /  \
                GE    LE

        :param selector: the fparser2 parse tree representing the \
                      selector_expression in SELECT CASE(selector_expression).
        :type selector: sub-class of :py:class:`fparser.two.utils.Base`
        :param nodes: the nodes representing the label-list of the current \
                      CASE() clause.
        :type nodes: list of :py:class:`fparser.two.Fortran2003.Name` or \
                     :py:class:`fparser.two.Fortran2003.Case_Value_Range`
        :param parent: parent node in the PSyIR.
        :type parent: :py:class:`psyclone.psyir.nodes.Node`

        '''
        if len(nodes) == 1:
            # Only one item in list so process it
            self._process_case_value(selector, nodes[0], parent)
            return
        # More than one item in list. Create an OR node with the first item
        # on the list as one arg then recurse down to handle the remainder
        # of the list.
        orop = BinaryOperation(BinaryOperation.Operator.OR,
                               parent=parent)
        self._process_case_value(selector, nodes[0], orop)
        self._process_case_value_list(selector, nodes[1:], orop)
        parent.addchild(orop)

    def _process_case_value(self, selector, node, parent):
        '''
        Handles an individual condition inside a CASE statement. This can
        be a single scalar expression (e.g. CASE(1)) or a range specification
        (e.g. CASE(lim1:lim2)).

        :param selector: the node in the fparser2 parse tree representing the
                         'some_expr' of the SELECT CASE(some_expr).
        :type selector: sub-class of :py:class:`fparser.two.utils.Base`
        :param node: the node representing the case-value expression in the \
                     fparser2 parse tree.
        :type node: sub-class of :py:class:`fparser.two.utils.Base`
        :param parent: parent node in the PSyIR.
        :type parent: :py:class:`psyclone.psyir.nodes.Node`

        '''
        if isinstance(node, Fortran2003.Case_Value_Range):
            # The case value is a range (e.g. lim1:lim2)
            if node.items[0] and node.items[1]:
                # Have lower and upper limits so need a parent AND
                aop = BinaryOperation(BinaryOperation.Operator.AND,
                                      parent=parent)
                parent.addchild(aop)
                new_parent = aop
            else:
                # No need to create new parent node
                new_parent = parent

            if node.items[0]:
                # A lower limit is specified
                geop = BinaryOperation(BinaryOperation.Operator.GE,
                                       parent=new_parent)
                self.process_nodes(parent=geop, nodes=[selector])
                self.process_nodes(parent=geop, nodes=[node.items[0]])
                new_parent.addchild(geop)
            if node.items[1]:
                # An upper limit is specified
                leop = BinaryOperation(BinaryOperation.Operator.LE,
                                       parent=new_parent)
                self.process_nodes(parent=leop, nodes=[selector])
                self.process_nodes(parent=leop, nodes=[node.items[1]])
                new_parent.addchild(leop)
        else:
            # The case value is some scalar initialisation expression
            bop = BinaryOperation(BinaryOperation.Operator.EQ,
                                  parent=parent)
            parent.addchild(bop)
            self.process_nodes(parent=bop, nodes=[selector])
            self.process_nodes(parent=bop, nodes=[node])

    @staticmethod
    def _array_notation_rank(node):
        '''Check that the supplied candidate array reference uses supported
        array notation syntax and return the rank of the sub-section
        of the array that uses array notation. e.g. for a reference
        "a(:, 2, :)" the rank of the sub-section is 2.

        :param node: the reference to check.
        :type node: :py:class:`psyclone.psyir.nodes.ArrayReference` or \
            :py:class:`psyclone.psyir.nodes.ArrayMember` or \
            :py:class:`psyclone.psyir.nodes.StructureReference`

        :returns: rank of the sub-section of the array.
        :rtype: int

        :raises InternalError: if no ArrayMixin node with at least one \
                               Range in its indices is found.
        :raises InternalError: if two or more part references in a \
                               structure reference contain ranges.
        :raises NotImplementedError: if the supplied node is not of a \
                                     supported type.
        :raises NotImplementedError: if any ranges are encountered that are \
                                     not for the full extent of the dimension.
        '''
        if isinstance(node, (ArrayReference, ArrayMember)):
            array = node
        elif isinstance(node, StructureReference):
            array = None
            arrays = node.walk((ArrayMember, ArrayOfStructuresMixin))
            for part_ref in arrays:
                if any(isinstance(idx, Range) for idx in part_ref.indices):
                    if array:
                        # Cannot have two or more part references that contain
                        # ranges - this is not valid Fortran.
                        # pylint: disable=import-outside-toplevel
                        from psyclone.psyir.backend.fortran import (
                            FortranWriter)
                        lang_writer = FortranWriter()
                        raise InternalError(
                            f"Found a structure reference containing two or "
                            f"more part references that have ranges: "
                            f"'{lang_writer(node)}'. This is not valid within "
                            f"a WHERE in Fortran.")
                    array = part_ref
            if not array:
                raise InternalError(
                    f"No array access found in node '{node.name}'")
        else:
            # This will result in a CodeBlock.
            raise NotImplementedError(
                f"Expected either an ArrayReference, ArrayMember or a "
                f"StructureReference but got '{type(node).__name__}'")

        # Only array refs using basic colon syntax are currently
        # supported e.g. (a(:,:)).  Each colon is represented in the
        # PSyIR as a Range node with first argument being an lbound
        # binary operator, the second argument being a ubound operator
        # and the third argument being an integer Literal node with
        # value 1 i.e. a(:,:) is represented as
        # a(lbound(a,1):ubound(a,1):1,lbound(a,2):ubound(a,2):1) in
        # the PSyIR.
        num_colons = 0
        for idx_node in array.indices:
            if isinstance(idx_node, Range):
                # Found array syntax notation. Check that it is the
                # simple ":" format.
                if not _is_range_full_extent(idx_node):
                    raise NotImplementedError(
                        "Only array notation of the form my_array(:, :, ...) "
                        "is supported.")
                num_colons += 1
        return num_colons

    def _array_syntax_to_indexed(self, parent, loop_vars):
        '''
        Utility function that modifies each ArrayReference object in the
        supplied PSyIR fragment so that they are indexed using the supplied
        loop variables rather than having colon array notation.

        # TODO #1576 this functionality is very similar to that needed in
        # the NemoArrayRange2Loop transformation and should be rationalised.

        :param parent: root of PSyIR sub-tree to search for Array \
                       references to modify.
        :type parent:  :py:class:`psyclone.psyir.nodes.Node`
        :param loop_vars: the variable names for the array indices.
        :type loop_vars: list of str

        :raises NotImplementedError: if array sections of differing ranks are \
                                     found.
        '''
        assigns = parent.walk(Assignment)
        # Check that the LHS of any assignment uses array notation.
        # Note that this will prevent any WHERE blocks that contain scalar
        # assignments from being handled but is a necessary limitation until
        # #717 is done and we interrogate the type of each symbol.
        for assign in assigns:
            _ = self._array_notation_rank(assign.lhs)

        # TODO #717 if the supplied code accidentally omits array
        # notation for an array reference on the RHS then we will
        # identify it as a scalar and the code produced from the
        # PSyIR (using e.g. the Fortran backend) will not
        # compile. We need to implement robust identification of the
        # types of all symbols in the PSyIR fragment.
        arrays = parent.walk(ArrayMixin)
        first_rank = None
        for array in arrays:
            # Check that this is a supported array reference and that
            # all arrays are of the same rank
            rank = len([child for child in array.indices if
                        isinstance(child, Range)])
            if rank == 0:
                # This is an array reference without any ranges so we can
                # ignore it.
                continue

            if first_rank:
                if rank != first_rank:
                    raise NotImplementedError(
                        f"Found array sections of differing ranks within a "
                        f"WHERE construct: array section of {array.name} has "
                        f"rank {rank}")
            else:
                first_rank = rank

            # Replace the PSyIR Ranges with the loop variables
            range_idx = 0
            for idx, child in enumerate(array.indices):
                if isinstance(child, Range):
                    symbol = _find_or_create_imported_symbol(
                        array, loop_vars[range_idx],
                        symbol_type=DataSymbol, datatype=DeferredType())
                    array.children[idx] = Reference(symbol)
                    range_idx += 1

    def _where_construct_handler(self, node, parent):
        '''
        Construct the canonical PSyIR representation of a WHERE construct or
        statement. A construct has the form:

            WHERE(logical-mask)
              statements
            [ELSE WHERE(logical-mask)
              statements]
            [ELSE WHERE
              statements]
            END WHERE

        while a statement is just:

            WHERE(logical-mask) statement

        :param node: node in the fparser2 parse tree representing the WHERE.
        :type node: :py:class:`fparser.two.Fortran2003.Where_Construct` or \
                    :py:class:`fparser.two.Fortran2003.Where_Stmt`
        :param parent: parent node in the PSyIR.
        :type parent: :py:class:`psyclone.psyir.nodes.Node`

        :returns: the top-level Loop object in the created loop nest.
        :rtype: :py:class:`psyclone.psyir.nodes.Loop`

        :raises InternalError: if the parse tree does not have the expected \
                               structure.
        :raises NotImplementedError: if the logical mask of the WHERE does \
                                     not use array notation.
        '''
        if isinstance(node, Fortran2003.Where_Stmt):
            # We have a Where statement. Check that the parse tree has the
            # expected structure.
            if not len(node.items) == 2:
                raise InternalError(
                    f"Expected a Fortran2003.Where_Stmt to have exactly two "
                    f"entries in 'items' but found {len(node.items)}: "
                    f"{node.items}")
            if not isinstance(node.items[1], Fortran2003.Assignment_Stmt):
                raise InternalError(
                    f"Expected the second entry of a Fortran2003.Where_Stmt "
                    f"items tuple to be an Assignment_Stmt but found: "
                    f"{type(node.items[1]).__name__}")
            was_single_stmt = True
            annotations = ["was_where", "was_single_stmt"]
            logical_expr = [node.items[0]]
        else:
            # We have a Where construct. Check that the first and last
            # children are what we expect.
            if not isinstance(node.content[0],
                              Fortran2003.Where_Construct_Stmt):
                raise InternalError(f"Failed to find opening where construct "
                                    f"statement in: {node}")
            if not isinstance(node.content[-1], Fortran2003.End_Where_Stmt):
                raise InternalError(f"Failed to find closing end where "
                                    f"statement in: {node}")
            was_single_stmt = False
            annotations = ["was_where"]
            logical_expr = node.content[0].items

        # Examine the logical-array expression (the mask) in order to
        # determine the number of nested loops required. The Fortran
        # standard allows bare array notation here (e.g. `a < 0.0` where
        # `a` is an array) and thus we would need to examine our SymbolTable
        # to find out the rank of `a`. For the moment we limit support to
        # the NEMO style where the fact that `a` is an array is made
        # explicit using the colon notation, e.g. `a(:, :) < 0.0`.

        # For this initial processing of the logical-array expression we
        # use a temporary parent as we haven't yet constructed the PSyIR
        # for the loop nest and innermost IfBlock. Once we have a valid
        # parent for this logical expression we will repeat the processing.
        fake_parent = Assignment(parent=parent)
        self.process_nodes(fake_parent, logical_expr)
        arrays = fake_parent.walk(ArrayMixin)

        if not arrays:
            # If the PSyIR doesn't contain any Arrays then that must be
            # because the code doesn't use explicit array syntax. At least one
            # variable in the logical-array expression must be an array for
            # this to be a valid WHERE().
            # TODO #717. Look-up the shape of the array in the SymbolTable.
            raise NotImplementedError(
                f"Only WHERE constructs using explicit array notation (e.g. "
                f"my_array(:,:)) are supported but found '{logical_expr}'.")
        for array in arrays:
            if any(isinstance(idx, Range) for idx in array.indices):
                first_array = array
                break
        else:
            raise NotImplementedError(
                f"Only WHERE constructs using explicit array notation "
                f"including ranges (e.g. my_array(1,:) are supported but "
                f"found '{logical_expr}'")

        # All array sections in a Fortran WHERE must have the same rank so
        # just look at the first array.
        rank = self._array_notation_rank(first_array)
        # Create a list to hold the names of the loop variables as we'll
        # need them to index into the arrays.
        loop_vars = rank*[""]

        symbol_table = parent.scope.symbol_table
        integer_type = default_integer_type()

        # Now create a loop nest of depth `rank`
        new_parent = parent
        for idx in range(rank, 0, -1):

            data_symbol = symbol_table.new_symbol(
                f"widx{idx}", symbol_type=DataSymbol, datatype=integer_type)
            loop_vars[idx-1] = data_symbol.name

            loop = Loop(parent=new_parent, variable=data_symbol,
                        annotations=annotations)
            # Point to the original WHERE statement in the parse tree.
            loop.ast = node
            # Add loop lower bound
            loop.addchild(Literal("1", integer_type))
            # Add loop upper bound - we use the SIZE operator to query the
            # extent of the current array dimension
            size_node = BinaryOperation(BinaryOperation.Operator.SIZE,
                                        parent=loop)
            loop.addchild(size_node)

            # Create the first argument to the SIZE operator
            if isinstance(first_array, Member):
                # The array access is a member of some derived type
                parent_ref = first_array.ancestor(Reference)
                new_ref = parent_ref.copy()
                orig_member = parent_ref.member
                member = new_ref.member
                while orig_member is not first_array:
                    member = member.member
                    orig_member = orig_member.member
                member.parent.children[0] = Member(first_array.name,
                                                   parent=member.parent)
            else:
                # The array access is to a symbol of ArrayType
                symbol = _find_or_create_imported_symbol(
                    size_node, first_array.name, symbol_type=DataSymbol,
                    datatype=DeferredType())
                new_ref = Reference(symbol)
            size_node.addchild(new_ref)
            size_node.addchild(Literal(str(idx), integer_type,
                                       parent=size_node))
            # Add loop increment
            loop.addchild(Literal("1", integer_type))
            # Fourth child of a Loop must be a Schedule
            sched = Schedule(parent=loop)
            loop.addchild(sched)
            # Finally, add the Loop we've constructed to its parent (but
            # not into the existing PSyIR tree - that's done in
            # process_nodes()).
            if new_parent is not parent:
                new_parent.addchild(loop)
            else:
                # Keep a reference to the first loop as that's what this
                # handler returns
                root_loop = loop
            new_parent = sched
        # Now we have the loop nest, add an IF block to the innermost
        # schedule
        ifblock = IfBlock(parent=new_parent, annotations=annotations)
        ifblock.ast = node  # Point back to the original WHERE construct
        new_parent.addchild(ifblock)

        # We construct the conditional expression from the original
        # logical-array-expression of the WHERE. We process_nodes() a
        # second time here now that we have the correct parent node in the
        # PSyIR (and thus a SymbolTable) to refer to.
        self.process_nodes(ifblock, logical_expr)

        # Each array reference must now be indexed by the loop variables
        # of the loops we've just created.
        # N.B. we can't use `ifblock.condition` below because the IfBlock is
        # not yet fully constructed and therefore the consistency checks
        # inside that method fail.
        self._array_syntax_to_indexed(ifblock.children[0], loop_vars)

        # Now construct the body of the IF using the body of the WHERE
        sched = Schedule(parent=ifblock)
        ifblock.addchild(sched)

        if was_single_stmt:
            # We only had a single-statement WHERE
            self.process_nodes(sched, node.items[1:])
        else:
            # We have to allow for potentially multiple ELSE WHERE clauses
            clause_indices = []
            for idx, child in enumerate(node.content):
                if isinstance(child, (Fortran2003.Elsewhere_Stmt,
                                      Fortran2003.Masked_Elsewhere_Stmt,
                                      Fortran2003.End_Where_Stmt)):
                    clause_indices.append(idx)
            num_clauses = len(clause_indices) - 1

            if len(clause_indices) > 1:
                # We have at least one elsewhere clause.
                # Process the body of the where (up to the first elsewhere).
                self.process_nodes(sched, node.content[1:clause_indices[0]])
                current_parent = ifblock

                for idx in range(num_clauses):
                    start_idx = clause_indices[idx]
                    end_idx = clause_indices[idx+1]
                    clause = node.content[start_idx]

                    if isinstance(clause, Fortran2003.Masked_Elsewhere_Stmt):
                        elsebody = Schedule(parent=current_parent)
                        current_parent.addchild(elsebody)
                        newifblock = IfBlock(parent=elsebody,
                                             annotations=annotations)
                        elsebody.addchild(newifblock)

                        # Keep pointer to fpaser2 AST
                        elsebody.ast = node.content[start_idx]
                        newifblock.ast = node.content[start_idx]

                        # Create condition as first child
                        self.process_nodes(parent=newifblock,
                                           nodes=[clause.items[0]])

                        # Create if-body as second child
                        ifbody = Schedule(parent=newifblock)
                        ifbody.ast = node.content[start_idx + 1]
                        ifbody.ast_end = node.content[end_idx - 1]
                        newifblock.addchild(ifbody)
                        self.process_nodes(
                            parent=ifbody,
                            nodes=node.content[start_idx+1:end_idx])
                        current_parent = newifblock

                    elif isinstance(clause, Fortran2003.Elsewhere_Stmt):
                        if idx != num_clauses - 1:
                            raise InternalError(
                                f"Elsewhere_Stmt should only be found next to "
                                f"last clause, but found {node.content}")
                        elsebody = Schedule(parent=current_parent)
                        current_parent.addchild(elsebody)
                        elsebody.ast = node.content[start_idx]
                        elsebody.ast_end = node.content[end_idx]
                        self.process_nodes(
                            parent=elsebody,
                            nodes=node.content[start_idx + 1:end_idx])

                    else:
                        raise InternalError(
                            f"Expected either Fortran2003.Masked_Elsewhere"
                            f"_Stmt or Fortran2003.Elsewhere_Stmt but found "
                            f"'{type(clause).__name__}'")
            else:
                # No elsewhere clauses were found so put the whole body into
                # the single if block.
                self.process_nodes(sched, node.content[1:-1])

        # Convert all uses of array syntax to indexed accesses
        self._array_syntax_to_indexed(ifblock, loop_vars)
        # Return the top-level loop generated by this handler
        return root_loop

    def _return_handler(self, node, parent):
        '''
        Transforms an fparser2 Return_Stmt to the PSyIR representation.

        :param node: node in fparser2 parse tree.
        :type node: :py:class:`fparser.two.Fortran2003.Return_Stmt`
        :param parent: Parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyir.nodes.Node`

        :return: PSyIR representation of node.
        :rtype: :py:class:`psyclone.psyir.nodes.Return`

        '''
        rtn = Return(parent=parent)
        rtn.ast = node
        return rtn

    def _assignment_handler(self, node, parent):
        '''
        Transforms an fparser2 Assignment_Stmt to the PSyIR representation.

        :param node: node in fparser2 AST.
        :type node: :py:class:`fparser.two.Fortran2003.Assignment_Stmt`
        :param parent: Parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyir.nodes.Node`

        :returns: PSyIR representation of node.
        :rtype: :py:class:`psyclone.psyir.nodes.Assignment`
        '''
        assignment = Assignment(node, parent=parent)
        self.process_nodes(parent=assignment, nodes=[node.items[0]])
        self.process_nodes(parent=assignment, nodes=[node.items[2]])

        return assignment

    def _data_ref_handler(self, node, parent):
        '''
        Create the PSyIR for an fparser2 Data_Ref (representing an access
        to a derived type).

        :param node: node in fparser2 parse tree.
        :type node: :py:class:`fparser.two.Fortran2003.Data_Ref`
        :param parent: Parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyir.nodes.Node`

        :return: PSyIR representation of node
        :rtype: :py:class:`psyclone.psyir.nodes.StructureReference`

        :raises NotImplementedError: if the parse tree contains unsupported \
                                     elements.
        '''
        # If we encounter array ranges while processing this derived-type
        # access then we will need the symbol being referred to so we create
        # that first.
        if isinstance(node.children[0], Fortran2003.Name):
            # Base of reference is a scalar entity and must be a DataSymbol.
            base_sym = _find_or_create_imported_symbol(
                parent, node.children[0].string.lower(),
                symbol_type=DataSymbol, datatype=DeferredType())
            base_indices = []
            base_ref = StructureReference

        elif isinstance(node.children[0], Fortran2003.Part_Ref):
            # Base of reference is an array access. Lookup the corresponding
            # symbol.
            part_ref = node.children[0]
            base_sym = _find_or_create_imported_symbol(
                parent, part_ref.children[0].string.lower(),
                symbol_type=DataSymbol, datatype=DeferredType())
            # Processing the array-index expressions requires access to the
            # symbol table so create an ArrayReference node.
            sched = parent.ancestor(Schedule, include_self=True)
            aref = ArrayReference(parent=sched, symbol=base_sym)
            # The children of this node will represent the indices of the
            # ArrayOfStructuresReference.
            self.process_nodes(parent=aref,
                               nodes=part_ref.children[1].children)
            base_indices = aref.pop_all_children()
            base_ref = ArrayOfStructuresReference

        else:
            raise NotImplementedError(str(node))

        # Now construct the full list of 'members' making up the
        # derived-type reference. e.g. for "var%region(1)%start" this
        # will be [("region", [Literal("1")]), "start"].
        members = []
        for child in node.children[1:]:
            if isinstance(child, Fortran2003.Name):
                # Members of a structure do not refer to symbols
                members.append(child.string)
            elif isinstance(child, Fortran2003.Part_Ref):
                # In order to use process_nodes() we need a parent node
                # through which we can access the symbol table. This is
                # because array-index expressions must refer to symbols.
                sched = parent.ancestor(Schedule, include_self=True)
                # Since the index expressions may refer to the parent
                # reference we construct a full reference to the current
                # member of the derived type. We include a fake array index
                # to ensure that the innermost member is an ArrayMember
                # that can accept the real array-index expressions generated
                # by process_nodes().
                array_name = child.children[0].string
                new_ref = _create_struct_reference(
                    sched, base_ref, base_sym,
                    members + [(array_name, [Literal("1", INTEGER_TYPE)])],
                    base_indices)
                # 'Chase the pointer' all the way to the bottom of the
                # derived-type reference
                current_ref = new_ref
                while hasattr(current_ref, "member"):
                    current_ref = current_ref.member
                # Remove the fake array index
                current_ref.pop_all_children()
                # We can now process the child index expressions
                self.process_nodes(parent=current_ref,
                                   nodes=child.children[1].children)
                # The resulting children will become part of the structure
                # access expression
                children = current_ref.pop_all_children()
                members.append((array_name, children))
            else:
                # Found an unsupported entry in the parse tree. This will
                # result in a CodeBlock.
                raise NotImplementedError(str(node))

        # Now we have the list of members, use the `create()` method of the
        # appropriate Reference subclass.
        return _create_struct_reference(parent, base_ref, base_sym,
                                        members, base_indices)

    def _unary_op_handler(self, node, parent):
        '''
        Transforms an fparser2 UnaryOpBase or Intrinsic_Function_Reference
        to the PSyIR representation.

        :param node: node in fparser2 AST.
        :type node: :py:class:`fparser.two.utils.UnaryOpBase` or \
               :py:class:`fparser.two.Fortran2003.Intrinsic_Function_Reference`
        :param parent: Parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyir.nodes.Node`

        :return: PSyIR representation of node
        :rtype: :py:class:`psyclone.psyir.nodes.UnaryOperation`

        :raises NotImplementedError: if the supplied operator is not \
                                     supported by this handler.
        :raises InternalError: if the fparser parse tree does not have the \
                               expected structure.

        '''
        operator_str = str(node.items[0]).lower()
        try:
            operator = Fparser2Reader.unary_operators[operator_str]
        except KeyError:
            # Operator not supported, it will produce a CodeBlock instead
            raise NotImplementedError(operator_str)

        if isinstance(node.items[1], Fortran2003.Actual_Arg_Spec_List):
            if len(node.items[1].items) > 1:
                # We have more than one argument and therefore this is not a
                # unary operation!
                raise InternalError(
                    f"Operation '{node}' has more than one argument and is "
                    f"therefore not unary!")
            node_list = node.items[1].items
        else:
            node_list = [node.items[1]]
        unary_op = UnaryOperation(operator, parent=parent)

        # Store the names of any named args
        arg_nodes, arg_names = _get_arg_names(node_list)

        self.process_nodes(parent=unary_op, nodes=arg_nodes)

        # Detach the child and add it again with the argument
        # name
        child = unary_op.children[0]
        child.detach()
        unary_op.append_named_arg(arg_names[0], child)

        return unary_op

    def _binary_op_handler(self, node, parent):
        '''
        Transforms an fparser2 BinaryOp or Intrinsic_Function_Reference to
        the PSyIR representation.

        :param node: node in fparser2 AST.
        :type node: :py:class:`fparser.two.utils.BinaryOpBase` or \
               :py:class:`fparser.two.Fortran2003.Intrinsic_Function_Reference`
        :param parent: Parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyir.nodes.Node`

        :returns: PSyIR representation of node
        :rtype: :py:class:`psyclone.psyir.nodes.BinaryOperation`

        :raises NotImplementedError: if the supplied operator/intrinsic is \
                                     not supported by this handler.
        :raises InternalError: if the fparser parse tree does not have the \
                               expected structure.

        '''
        if isinstance(node, Fortran2003.Intrinsic_Function_Reference):
            operator_str = node.items[0].string.lower()
            # Arguments are held in an Actual_Arg_Spec_List
            if not isinstance(node.items[1], Fortran2003.Actual_Arg_Spec_List):
                raise InternalError(
                    f"Unexpected fparser parse tree for binary intrinsic "
                    f"operation '{node}'. Expected second child to be "
                    f"Actual_Arg_Spec_List but got '{type(node.items[1])}'.")
            arg_nodes = node.items[1].items
            if len(arg_nodes) != 2:
                raise InternalError(
                    f"Binary operator should have exactly two arguments but "
                    f"found {len(arg_nodes)} for '{node}'.")
        else:
            operator_str = node.items[1].lower()
            arg_nodes = [node.items[0], node.items[2]]

        try:
            operator = Fparser2Reader.binary_operators[operator_str]
        except KeyError:
            # Operator not supported, it will produce a CodeBlock instead
            raise NotImplementedError(operator_str)

        binary_op = BinaryOperation(operator, parent=parent)

        # Store the names of any named args
        new_arg_nodes, arg_names = _get_arg_names(arg_nodes)

        self.process_nodes(parent=binary_op, nodes=[new_arg_nodes[0]])
        self.process_nodes(parent=binary_op, nodes=[new_arg_nodes[1]])

        # Detach the children and add them again with the argument
        # names
        child_list = binary_op.children[:]
        for child in child_list:
            child.detach()
        for idx, child in enumerate(child_list):
            binary_op.append_named_arg(arg_names[idx], child)

        return binary_op

    def _nary_op_handler(self, node, parent):
        '''
        Transforms an fparser2 Intrinsic_Function_Reference with three or
        more arguments to the PSyIR representation.
        :param node: node in fparser2 Parse Tree.
        :type node: \
             :py:class:`fparser.two.Fortran2003.Intrinsic_Function_Reference`
        :param parent: Parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyir.nodes.Node`

        :returns: PSyIR representation of node.
        :rtype: :py:class:`psyclone.psyir.nodes.NaryOperation`

        :raises NotImplementedError: if the supplied Intrinsic is not \
                                     supported by this handler.
        :raises InternalError: if the fparser parse tree does not have the \
                               expected structure.

        '''
        operator_str = str(node.items[0]).lower()
        try:
            operator = Fparser2Reader.nary_operators[operator_str]
        except KeyError:
            # Intrinsic not supported, it will produce a CodeBlock instead
            raise NotImplementedError(operator_str)

        nary_op = NaryOperation(operator, parent=parent)

        if not isinstance(node.items[1], Fortran2003.Actual_Arg_Spec_List):
            raise InternalError(
                f"Expected second 'item' of N-ary intrinsic '{node}' in "
                f"fparser parse tree to be an Actual_Arg_Spec_List but found "
                f"'{type(node.items[1])}'.")
        if len(node.items[1].items) < 3:
            raise InternalError(
                f"An N-ary operation must have more than two arguments but "
                f"found {len(node.items[1].items)} for '{node}'.")

        # node.items[1] is a Fortran2003.Actual_Arg_Spec_List so we have
        # to process the `items` of that...

        # Store the names of any named args
        arg_nodes, arg_names = _get_arg_names(node.items[1].items)

        self.process_nodes(parent=nary_op, nodes=arg_nodes)

        # Detach the children and add them again with the argument
        # names
        child_list = nary_op.children[:]
        for child in child_list:
            child.detach()
        for idx, child in enumerate(child_list):
            nary_op.append_named_arg(arg_names[idx], child)

        return nary_op

    def _intrinsic_handler(self, node, parent):
        '''
        Transforms an fparser2 Intrinsic_Function_Reference to the PSyIR
        representation. Since Fortran Intrinsics can be unary, binary or
        nary this handler identifies the appropriate 'sub handler' by
        examining the number of arguments present.

        :param node: node in fparser2 Parse Tree.
        :type node: \
            :py:class:`fparser.two.Fortran2003.Intrinsic_Function_Reference`
        :param parent: Parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyir.nodes.Node`

        :returns: PSyIR representation of node
        :rtype: :py:class:`psyclone.psyir.nodes.UnaryOperation` or \
                :py:class:`psyclone.psyir.nodes.BinaryOperation` or \
                :py:class:`psyclone.psyir.nodes.NaryOperation`

        '''
        # First item is the name of the intrinsic
        name = node.items[0].string.upper()
        # Now work out how many arguments it has
        num_args = 0
        if len(node.items) > 1:
            num_args = len(node.items[1].items)

        # We don't handle any intrinsics that don't have arguments
        if num_args == 1:
            return self._unary_op_handler(node, parent)
        if num_args == 2:
            return self._binary_op_handler(node, parent)
        if num_args > 2:
            return self._nary_op_handler(node, parent)

        # Intrinsic is not handled - this will result in a CodeBlock
        raise NotImplementedError(name)

    def _name_handler(self, node, parent):
        '''
        Transforms an fparser2 Name to the PSyIR representation. If the parent
        is connected to a SymbolTable, it checks the reference has been
        previously declared.

        :param node: node in fparser2 AST.
        :type node: :py:class:`fparser.two.Fortran2003.Name`
        :param parent: Parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyir.nodes.Node`

        :returns: PSyIR representation of node
        :rtype: :py:class:`psyclone.psyir.nodes.Reference`

        '''
        symbol = _find_or_create_imported_symbol(parent, node.string)
        return Reference(symbol, parent)

    def _parenthesis_handler(self, node, parent):
        '''
        Transforms an fparser2 Parenthesis to the PSyIR representation.
        This means ignoring the parentheis and process the fparser2 children
        inside.

        :param node: node in fparser2 AST.
        :type node: :py:class:`fparser.two.Fortran2003.Parenthesis`
        :param parent: Parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyir.nodes.Node`
        :returns: PSyIR representation of node
        :rtype: :py:class:`psyclone.psyir.nodes.Node`
        '''
        # Use the items[1] content of the node as it contains the required
        # information (items[0] and items[2] just contain the left and right
        # brackets as strings so can be disregarded.
        return self._create_child(node.items[1], parent)

    def _part_ref_handler(self, node, parent):
        '''
        Transforms an fparser2 Part_Ref to the PSyIR representation. If the
        node is connected to a SymbolTable, it checks the reference has been
        previously declared.

        :param node: node in fparser2 AST.
        :type node: :py:class:`fparser.two.Fortran2003.Part_Ref`
        :param parent: Parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyir.nodes.Node`

        :raises NotImplementedError: If the fparser node represents \
            unsupported PSyIR features and should be placed in a CodeBlock.

        :returns: PSyIR representation of node
        :rtype: :py:class:`psyclone.psyir.nodes.ArrayReference`

        '''
        reference_name = node.items[0].string.lower()
        # We can't say for sure that the symbol we create here should be a
        # DataSymbol as fparser2 often identifies function calls as
        # part-references instead of function-references.
        symbol = _find_or_create_imported_symbol(parent, reference_name)

        array = ArrayReference(symbol, parent)
        self.process_nodes(parent=array, nodes=node.items[1].items)
        return array

    def _subscript_triplet_handler(self, node, parent):
        '''
        Transforms an fparser2 Subscript_Triplet to the PSyIR
        representation.

        :param node: node in fparser2 AST.
        :type node: :py:class:`fparser.two.Fortran2003.Subscript_Triplet`
        :param parent: parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyir.nodes.Node`

        :returns: PSyIR representation of node.
        :rtype: :py:class:`psyclone.psyir.nodes.Range`

        :raises InternalError: if the supplied parent node is not a sub-class \
                               of either Reference or Member.
        '''
        # The PSyIR stores array dimension information for the ArrayMixin
        # class in an ordered list. As we are processing the
        # dimensions in order, the number of children already added to
        # our parent indicates the current array dimension being processed
        # (with 0 being the first dimension, 1 being the second etc). Fortran
        # specifies the 1st dimension as being 1, the second dimension being
        # 2, etc.). We therefore add 1 to the number of children already added
        # to our parent to determine the Fortran dimension value. However, we
        # do have to take care in case the parent is a member of a structure
        # rather than a plain array reference.
        if isinstance(parent, (Reference, Member)):
            dimension = str(len([kid for kid in parent.children if
                                 not isinstance(kid, Member)]) + 1)
        else:
            raise InternalError(
                f"Expected parent PSyIR node to be either a Reference or a "
                f"Member but got '{type(parent).__name__}' when processing "
                f"'{node}'")

        integer_type = default_integer_type()
        my_range = Range(parent=parent)
        my_range.children = []

        if node.children[0]:
            self.process_nodes(parent=my_range, nodes=[node.children[0]])
        else:
            # There is no lower bound, it is implied. This is not
            # supported in the PSyIR so we create the equivalent code
            # by using the PSyIR lbound function:
            # a(:...) becomes a(lbound(a,1):...)
            lbound = BinaryOperation.create(
                BinaryOperation.Operator.LBOUND,
                _copy_full_base_reference(parent),
                Literal(dimension, integer_type))
            my_range.children.append(lbound)

        if node.children[1]:
            self.process_nodes(parent=my_range, nodes=[node.children[1]])
        else:
            # There is no upper bound, it is implied. This is not
            # supported in the PSyIR so we create the equivalent code
            # by using the PSyIR ubound function:
            # a(...:) becomes a(...:ubound(a,1))
            ubound = BinaryOperation.create(
                BinaryOperation.Operator.UBOUND,
                _copy_full_base_reference(parent),
                Literal(dimension, integer_type))
            my_range.children.append(ubound)

        if node.children[2]:
            self.process_nodes(parent=my_range, nodes=[node.children[2]])
        else:
            # There is no step, it is implied. This is not
            # supported in the PSyIR so we create the equivalent code
            # by using a PSyIR integer literal with the value 1
            # a(...:...:) becomes a(...:...:1)
            literal = Literal("1", integer_type)
            my_range.children.append(literal)
        return my_range

    def _number_handler(self, node, parent):
        '''
        Transforms an fparser2 NumberBase to the PSyIR representation.

        :param node: node in fparser2 parse tree.
        :type node: :py:class:`fparser.two.utils.NumberBase`
        :param parent: Parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyir.nodes.Node`

        :returns: PSyIR representation of node.
        :rtype: :py:class:`psyclone.psyir.nodes.Literal`

        :raises NotImplementedError: if the fparser2 node is not recognised.

        '''
        # pylint: disable=no-self-use
        if isinstance(node, Fortran2003.Int_Literal_Constant):
            integer_type = ScalarType(ScalarType.Intrinsic.INTEGER,
                                      get_literal_precision(node, parent))
            return Literal(str(node.items[0]), integer_type)
        if isinstance(node, Fortran2003.Real_Literal_Constant):
            real_type = ScalarType(ScalarType.Intrinsic.REAL,
                                   get_literal_precision(node, parent))
            # Make sure any exponent is lower case
            value = str(node.items[0]).lower()
            # Make all exponents use the letter "e". (Fortran also
            # allows "d").
            value = value.replace("d", "e")
            # If the value has a "." without a digit before it then
            # add a "0" as the PSyIR does not allow this
            # format. e.g. +.3 => +0.3
            if value[0] == "." or value[0:1] in ["+.", "-."]:
                value = value.replace(".", "0.")
            return Literal(value, real_type)
        # Unrecognised datatype - will result in a CodeBlock
        raise NotImplementedError()

    def _char_literal_handler(self, node, parent):
        '''
        Transforms an fparser2 character literal into a PSyIR literal.
        Currently does not support the use of a double '' or double "" to
        represent a single instance of one of those characters within a string
        delimited by the same character.

        :param node: node in fparser2 parse tree.
        :type node: :py:class:`fparser.two.Fortran2003.Char_Literal_Constant`
        :param parent: parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyir.nodes.Node`

        :returns: PSyIR representation of node.
        :rtype: :py:class:`psyclone.psyir.nodes.Literal`

        '''
        # pylint: disable=no-self-use
        character_type = ScalarType(ScalarType.Intrinsic.CHARACTER,
                                    get_literal_precision(node, parent))
        # fparser issue #295 - the value of the character string currently
        # contains the quotation symbols themselves. Once that's fixed this
        # check will need to be changed.
        char_value = str(node.items[0])
        if not ((char_value.startswith("'") and char_value.endswith("'")) or
                (char_value.startswith('"') and char_value.endswith('"'))):
            raise InternalError(
                f"Char literal handler expects a quoted value but got: "
                f">>{char_value}<<")
        # In Fortran "x""x" or 'x''x' represents a string containing x"x
        # or x'x, respectively. (See Note 4.12 in the Fortran 2003 standard.)
        # However, checking whether we have e.g. 'that''s a cat''s mat' is
        # difficult and so, for now, we don't support it.
        if len(char_value) > 2 and ("''" in char_value or '""' in char_value):
            raise NotImplementedError()
        # Strip the wrapping quotation chars before storing the value.
        return Literal(char_value[1:-1], character_type)

    def _bool_literal_handler(self, node, parent):
        '''
        Transforms an fparser2 logical literal into a PSyIR literal.

        :param node: node in fparser2 parse tree.
        :type node: \
            :py:class:`fparser.two.Fortran2003.Logical_Literal_Constant`
        :param parent: parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyir.nodes.Node`

        :returns: PSyIR representation of node.
        :rtype: :py:class:`psyclone.psyir.nodes.Literal`

        '''
        # pylint: disable=no-self-use
        boolean_type = ScalarType(ScalarType.Intrinsic.BOOLEAN,
                                  get_literal_precision(node, parent))
        value = str(node.items[0]).lower()
        if value == ".true.":
            return Literal("true", boolean_type)
        if value == ".false.":
            return Literal("false", boolean_type)
        raise GenerationError(
            f"Expected to find '.true.' or '.false.' as fparser2 logical "
            f"literal, but found '{value}' instead.")

    def _call_handler(self, node, parent):
        '''Transforms an fparser2 CALL statement into a PSyIR Call node.

        :param node: node in fparser2 parse tree.
        :type node: :py:class:`fparser.two.Fortran2003.Call_Stmt`
        :param parent: parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyir.nodes.Node`

        :returns: PSyIR representation of node.
        :rtype: :py:class:`psyclone.psyir.nodes.Call`

        :raises GenerationError: if the symbol associated with the \
            name of the call is an unsupported type.

        '''
        # pylint: disable="unidiomatic-typecheck"
        call_name = node.items[0].string
        symbol_table = parent.scope.symbol_table
        try:
            routine_symbol = symbol_table.lookup(call_name)
            # pylint: disable=unidiomatic-typecheck
            if type(routine_symbol) is Symbol:
                # Specialise routine_symbol from a Symbol to a
                # RoutineSymbol
                routine_symbol.specialise(RoutineSymbol)
            elif type(routine_symbol) is RoutineSymbol:
                # This symbol is already the expected type
                pass
            else:
                raise GenerationError(
                    f"Expecting the symbol '{call_name}', to be of type "
                    f"'Symbol' or 'RoutineSymbol', but found "
                    f"'{type(routine_symbol).__name__}'.")
        except KeyError:
            # A call must be to a subroutine which has no type in Fortran.
            routine_symbol = RoutineSymbol(
                call_name, interface=UnresolvedInterface())
            symbol_table.add(routine_symbol)

        call = Call(routine_symbol, parent=parent)

        arg_nodes = []
        arg_names = []
        if node.items[1]:
            # Store the names of any named args
            arg_nodes, arg_names = _get_arg_names(node.items[1].items)

        self.process_nodes(parent=call, nodes=arg_nodes)

        # Detach the children and add them again with the argument
        # names
        child_list = call.children[:]
        for child in child_list:
            child.detach()
        for idx, child in enumerate(child_list):
            call.append_named_arg(arg_names[idx], child)

        # Point to the original CALL statement in the parse tree.
        call.ast = node

        return call

    def _subroutine_handler(self, node, parent):
        '''Transforms an fparser2 Subroutine_Subprogram or Function_Subprogram
        statement into a PSyIR Routine node.

        :param node: node in fparser2 parse tree.
        :type node: :py:class:`fparser.two.Fortran2003.Subroutine_Subprogram` \
            or :py:class:`fparser.two.Fortran2003.Function_Subprogram`
        :param parent: parent node of the PSyIR node being constructed.
        :type parent: subclass of :py:class:`psyclone.psyir.nodes.Node`

        :returns: PSyIR representation of node.
        :rtype: :py:class:`psyclone.psyir.nodes.Routine`

        '''
        name = node.children[0].children[1].string
        routine = Routine(name, parent=parent)

        # Deal with any arguments
        try:
            sub_spec = _first_type_match(node.content,
                                         Fortran2003.Specification_Part)
            decl_list = sub_spec.content
            # TODO this if test can be removed once fparser/#211 is fixed
            # such that routine arguments are always contained in a
            # Dummy_Arg_List, even if there's only one of them.
            if isinstance(node, (Fortran2003.Subroutine_Subprogram,
                                 Fortran2003.Function_Subprogram)) and \
               isinstance(node.children[0].children[2],
                          Fortran2003.Dummy_Arg_List):
                arg_list = node.children[0].children[2].children
            else:
                # Routine has no arguments
                arg_list = []
        except ValueError:
            # Subroutine has no Specification_Part so has no
            # declarations. Continue with empty lists.
            decl_list = []
            arg_list = []
        finally:
            self.process_declarations(routine, decl_list, arg_list)

        # If this is a function then work out the return type
        if isinstance(node, Fortran2003.Function_Subprogram):
            # Check whether the function-stmt has a prefix specifying the
            # return type.
            prefix = node.children[0].children[0]
            if prefix:
                # If there is anything else in the prefix (PURE, ELEMENTAL or
                # RECURSIVE) then we will create a CodeBlock for this function.
                if len(prefix.children) > 1:
                    raise NotImplementedError()
                base_type, _ = self._process_type_spec(parent,
                                                       prefix.children[0])
            else:
                base_type = None

            # Check whether the function-stmt has a suffix containing
            # 'RETURNS'
            suffix = node.children[0].children[3]
            if suffix:
                # Although the suffix can, in principle, contain a proc-
                # language-binding-spec (e.g. BIND(C, "some_name")), this is
                # only valid in an interface block and we are dealing with a
                # function-subprogram here.
                return_name = suffix.children[0].string
            else:
                # Otherwise, the return value of the function is given by
                # a symbol of the same name.
                return_name = name

            # Ensure that we have an explicit declaration for the symbol
            # returned by the function.
            keep_tag = None
            if return_name in routine.symbol_table:
                symbol = routine.symbol_table.lookup(return_name)
                # If the symbol table still contains a RoutineSymbol
                # for the function name (rather than a DataSymbol)
                # then there is no explicit declaration within the
                # function of the variable used to hold the return
                # value.
                if isinstance(symbol, RoutineSymbol):
                    if not base_type:
                        # The type of the return value was not specified in the
                        # function prefix either therefore we have no explicit
                        # type information for it.
                        raise NotImplementedError()
                    # Remove the RoutineSymbol ready to replace it with a
                    # DataSymbol.
                    routine.symbol_table.remove(symbol)
                    keep_tag = "own_routine_symbol"

            if return_name not in routine.symbol_table:
                # There is no existing declaration for the symbol returned by
                # the function (because it is specified by the prefix and
                # suffix of the function declaration). We add one rather than
                # attempt to recreate the prefix. We have to set shadowing to
                # True as there is likely to be a RoutineSymbol for this
                # function in any enclosing Container.
                routine.symbol_table.new_symbol(return_name,
                                                tag=keep_tag,
                                                symbol_type=DataSymbol,
                                                datatype=base_type,
                                                shadowing=True)

            # Update the Routine object with the return symbol.
            routine.return_symbol = routine.symbol_table.lookup(return_name)

        try:
            sub_exec = _first_type_match(node.content,
                                         Fortran2003.Execution_Part)
        except ValueError:
            # Routines without any execution statements are still
            # valid.
            pass
        else:
            self.process_nodes(routine, sub_exec.content)

        return routine

    def _main_program_handler(self, node, parent):
        '''Transforms an fparser2 Main_Program statement into a PSyIR
        Routine node.

        :param node: node in fparser2 parse tree.
        :type node: :py:class:`fparser.two.Fortran2003.Main_Program`
        :param parent: parent node of the PSyIR node being constructed.
        :type parent: subclass of :py:class:`psyclone.psyir.nodes.Node`

        :returns: PSyIR representation of node.
        :rtype: :py:class:`psyclone.psyir.nodes.Routine`

        '''
        name = node.children[0].children[1].string
        routine = Routine(name, parent=parent, is_program=True)

        try:
            prog_spec = _first_type_match(node.content,
                                          Fortran2003.Specification_Part)
            decl_list = prog_spec.content
        except ValueError:
            # program has no Specification_Part so has no
            # declarations. Continue with empty list.
            decl_list = []
        finally:
            self.process_declarations(routine, decl_list, [])

        try:
            prog_exec = _first_type_match(node.content,
                                          Fortran2003.Execution_Part)
        except ValueError:
            # Routines without any execution statements are still
            # valid.
            pass
        else:
            self.process_nodes(routine, prog_exec.content)

        return routine

    def _module_handler(self, node, parent):
        '''Transforms an fparser2 Module statement into a PSyIR Container node.

        :param node: fparser2 representation of a module.
        :type node: :py:class:`fparser.two.Fortran2003.Module`
        :param parent: parent node of the PSyIR node being constructed.
        :type parent: subclass of :py:class:`psyclone.psyir.nodes.Node`

        :returns: PSyIR representation of module.
        :rtype: :py:class:`psyclone.psyir.nodes.Container`

        '''
        # Create a container to capture the module information
        mod_name = str(node.children[0].children[1])
        container = Container(mod_name, parent=parent)

        # Search for any accessibility statements (e.g. "PUBLIC :: my_var") to
        # determine the default accessibility of symbols as well as identifying
        # those that are explicitly declared as public or private.
        (default_visibility, visibility_map) = self.process_access_statements(
            node)
        container.symbol_table.default_visibility = default_visibility

        # Create symbols for all routines defined within this module
        _process_routine_symbols(node, container.symbol_table, visibility_map)

        # Parse the declarations if it has any
        try:
            spec_part = _first_type_match(
                node.children, Fortran2003.Specification_Part)
            self.process_declarations(container, spec_part.children,
                                      [], visibility_map)
        except ValueError:
            pass

        # Parse any module subprograms (subroutine or function)
        # skipping the contains node
        try:
            subprog_part = _first_type_match(
                node.children, Fortran2003.Module_Subprogram_Part)
            module_subprograms = \
                [subprogram for subprogram in subprog_part.children
                 if not isinstance(subprogram, Fortran2003.Contains_Stmt)]
            if module_subprograms:
                self.process_nodes(parent=container, nodes=module_subprograms)
        except ValueError:
            pass

        return container

    def _program_handler(self, node, parent):
        '''Processes an fparser2 Program statement. Program is the top level
        node of a complete fparser2 tree and may contain one or more
        program-units. This is captured with a FileContainer node.

        :param node: top level node in fparser2 parse tree.
        :type node: :py:class:`fparser.two.Fortran2003.Program`
        :param parent: parent node of the PSyIR node we are constructing.
        :type parent: :py:class:`psyclone.psyir.nodes.Node`

        :returns: PSyIR representation of the program.
        :rtype: subclass of :py:class:`psyclone.psyir.nodes.Node`

        '''
        # fparser2 does not keep the original filename (if there was
        # one) so this can't be provided as the name of the
        # FileContainer.
        file_container = FileContainer("None", parent=parent)
        if len(node.children) == 1 and node.children[0] is None:
            # We have an empty file
            return file_container
        self.process_nodes(file_container, node.children)
        return file_container


# For Sphinx AutoAPI documentation generation
__all__ = ["Fparser2Reader"]
