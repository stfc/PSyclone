# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2025, Science and Technology Facilities Council
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
# Author: J. Henrichs, Bureau of Meteorology
# Modified: S. Siso, STFC Daresbury Lab


'''PSyIR backend to create expressions that are handled by SymPy.
'''

import keyword
from typing import Iterable, Optional, Union

import sympy
from sympy.parsing.sympy_parser import parse_expr

from psyclone.core import (Signature, AccessSequence,
                           VariablesAccessMap)
from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.backend.visitor import VisitorError
from psyclone.psyir.frontend.sympy_reader import SymPyReader
from psyclone.psyir.nodes import (
    ArrayOfStructuresReference, ArrayReference, BinaryOperation, Call,
    DataNode, IntrinsicCall, Literal, Node,
    Range, Reference, StructureReference)
from psyclone.psyir.symbols import (
    ArrayType, DataSymbol, RoutineSymbol, ScalarType, Symbol,
    SymbolError, SymbolTable, UnresolvedType)


class SymPyWriter(FortranWriter):
    '''Implements a PSyIR-to-SymPy writer, which is used to create a
    representation of the PSyIR tree that can be understood by SymPy. Most
    Fortran expressions work as expected without modification. This class
    implements special handling for constants (which can have a precision
    attached, e.g. 2_4) and some intrinsic functions (e.g. ``MAX``, which SymPy
    expects to be ``Max``). Array accesses are converted into functions (while
    SymPy supports indexed expression, they cannot be used as expected when
    solving, SymPy does not solve component-wise - ``M[x]-M[1]`` would not
    result in ``x=1``, while it does for SymPy unknown functions).
    Array expressions are supported by the writer: it will convert any array
    expression like ``a(i:j:k)`` by using three arguments: ``a(i, j, k)``.
    Then simple array accesses like ``b(i,j)`` are converted to
    ``b(i,i,1,j,j,1)``. Similarly, if ``a`` is known to be an array, then the
    writer will use ``a(sympy_lower,sympy_upper,1)``. This makes sure all SymPy
    unknown functions that represent an array use the same number of
    arguments.

    The simple use case of converting a (list of) PSyIR expressions to SymPy
    expressions is as follows::

        symp_expr_list = SymPyWriter(exp1, exp2, ...)

    If additional functionality is required (access to the type map or
    to convert a potentially modified SymPy expression back to PSyIR), an
    instance of SymPy writer must be created::

        writer = SymPyWriter()
        symp_expr_list = writer([exp1, exp2, ...])

    It additionally supports accesses to structure types. A full description
    can be found in the manual:
    https://psyclone.readthedocs.io/en/latest/developer_guide/sympy.html#sympy

    '''
    # This option will disable the lowering of abstract nodes into language
    # level nodes, and as a consequence the backend does not need to deep-copy
    # the tree and is much faster to execute.
    # Be careful not to modify anything from the input tree when this option
    # is set to True as the modifications will persist after the Writer!
    _DISABLE_LOWERING = True

    # A list of all reserved Python keywords (Fortran variables that are the
    # same as a reserved name must be renamed, otherwise parsing will fail).
    # This class attribute will get initialised in __init__:
    _RESERVED_NAMES: set[str] = set()

    # A mapping of PSyIR's logical binary operations to the required
    # SymPy format:
    _BINARY_OP_MAPPING: dict[BinaryOperation.Operator, str] = \
        {BinaryOperation.Operator.AND: "And({lhs}, {rhs})",
         BinaryOperation.Operator.OR: "Or({lhs}, {rhs})",
         BinaryOperation.Operator.EQV: "Equivalent({lhs}, {rhs})",
         BinaryOperation.Operator.NEQV: "Xor({lhs}, {rhs})",
         BinaryOperation.Operator.EQ: "Eq({lhs}, {rhs})"
         }

    def __init__(self):
        super().__init__()

        # The symbol table is used to create unique names for structure
        # members that are being accessed (these need to be defined as
        # SymPy functions or symbols, which could clash with other
        # references in the expression).
        self._symbol_table = None

        # The writer will use special names in array expressions to indicate
        # the lower and upper bound (e.g. ``a(::)`` becomes
        # ``a(sympy_lower, sympy_upper, 1)`` while ``a`` becomes
        # ``a(sympy_no_bounds, sympy_no_bounds, 1)``). The symbol table will
        # be used to resolve a potential name clash with a user variable.
        self._lower_bound = "sympy_lower"
        self._upper_bound = "sympy_upper"
        self._no_bounds = "sympy_no_bounds"

        if not SymPyWriter._RESERVED_NAMES:
            # Get the list of all reserved Python words from the
            # keyword module:
            for reserved in keyword.kwlist:
                # Some Python keywords are capitalised (True, False, None).
                # Since all symbols in PSyclone are lowercase, these can
                # never clash when using SymPy. So only take keywords that
                # are in lowercase:
                if reserved.lower() == reserved:
                    SymPyWriter._RESERVED_NAMES.add(reserved)

        # This dictionary will be supplied when parsing a string by SymPy
        # and defines which symbols in the parsed expressions are scalars
        # (SymPy symbols) or arrays (SymPy functions).
        self._sympy_type_map = {}

        # The set of intrinsic Fortran operations that need a rename or
        # are case sensitive in SymPy:
        self._intrinsic = set()
        self._intrinsic_to_str = {}

        # Create the mapping of intrinsics to the name SymPy expects.
        for intr, intr_str in [(IntrinsicCall.Intrinsic.MAX, "Max"),
                               (IntrinsicCall.Intrinsic.MIN, "Min"),
                               (IntrinsicCall.Intrinsic.FLOOR, "floor"),
                               (IntrinsicCall.Intrinsic.TRANSPOSE,
                                "transpose"),
                               (IntrinsicCall.Intrinsic.MOD, "Mod"),
                               # exp is needed for a test case only, in
                               # general the maths functions can just be
                               # handled as unknown sympy functions.
                               (IntrinsicCall.Intrinsic.EXP, "exp"),
                               ]:
            self._intrinsic.add(intr_str)
            self._intrinsic_to_str[intr] = intr_str

    # -------------------------------------------------------------------------
    def __new__(cls, *expressions):
        '''This function allows the SymPy writer to be used in two
        different ways: if only the SymPy expression of the PSyIR expressions
        are required, it can be called as::

            sympy_expressions = SymPyWriter(exp1, exp2, ...)

        But if additional information is needed (e.g. the SymPy type map, or
        to convert a SymPy expression back to PSyIR), an instance of the
        SymPyWriter must be kept, e.g.::

            writer = SymPyWriter()
            sympy_expressions = writer([exp1, exp2, ...])
            writer.type_map

        :param expressions: a (potentially empty) tuple of PSyIR nodes
            to be converted to SymPy expressions.
        :type expressions: Tuple[:py:class:`psyclone.psyir.nodes.Node`]

        :returns: either an instance of SymPyWriter, if no parameter is
            specified, or a list of SymPy expressions.
        :rtype: Union[:py:class:`psyclone.psyir.backend.SymPyWriter`,
                      list[:py:class:`sympy.core.basic.Basic`]]

        '''
        if expressions:
            # If we have parameters, create an instance of the writer
            # and use it to convert the expressions:
            writer = SymPyWriter()
            return writer(expressions)

        # No parameter, just create an instance and return it:
        return super().__new__(cls)

    # -------------------------------------------------------------------------
    def __getitem__(self, _):
        '''This function is only here to trick pylint into thinking that
        the object returned from ``__new__`` is subscriptable, meaning that
        code like:
        ``out = SymPyWriter(exp1, exp2); out[1]`` does not trigger
        a pylint warning about unsubscriptable-object.
        '''
        raise NotImplementedError("__getitem__ for a SymPyWriter should "
                                  "never be called.")

    # -------------------------------------------------------------------------
    def _create_sympy_array_function(
            self,
            name: str,
            sig: Optional[Signature] = None,
            num_dims: Optional[list[int]] = None,
            is_call: Optional[bool] = False) -> sympy.Function:
        '''Creates a Function class with the given name to be used for SymPy
        parsing. This Function overwrites the conversion to string, and will
        replace the triplicated array indices back to the normal Fortran
        syntax. If the signature Sig and number of dimensions for each
        component of the signature are given, it will add this information
        to the object, so that the SymPyReader can recreate the proper
        access to a user-defined type.

        :param name: name of the function class to create.
        :param sig: the signature of the variable, which is required
            to convert user defined types back properly. Only defined for
            user-defined types.
        :param num_dims: the number of dimensions for each component of a
            user defined type.
        :param is_call: whether the Function represents an actual Call to
            a function. If so, it is permitted to have no arguments.

        :returns: a SymPy function, which has a special ``_sympystr`` function
            defined as attribute to print user-defined types..
        :rtype: :py:class:`sympy.Function`

        '''
        # Create a new sympy function instance, and overwrite how this
        # function is converted back into a string by defining the
        # ``_sympystr`` attribute, which points to a function that
        # controls how this object is converted into a string. Use the
        # ``print_fortran_array`` function from the SymPyReader for
        # this. Note that we cannot create a derived class based on
        # ``Function`` and define this function there: SymPy tests
        # internally if the type is a Function (not if it is an
        # instance), therefore, SymPy's behaviour would change if we
        # used a derived class:
        # https://docs.sympy.org/latest/modules/functions/index.html:
        # "It [Function class] also serves as a constructor for
        # undefined function classes."
        new_func = sympy.Function(name)
        # pylint: disable=protected-access
        new_func._sympystr = SymPyReader.print_fortran_array

        # Store the signature and the number of dimensions of each
        # component, so that SymPyReader.print_fortran_array can match
        # the indices back to the user defined types.
        new_func._sig = sig
        new_func._num_dims = num_dims
        # We also have to store whether or not this is a function call in
        # case it has no arguments.
        new_func._is_call = is_call
        # pylint: enable=protected-access
        return new_func

    @staticmethod
    def _ndims_for_struct_access(sig: Signature,
                                 sva: AccessSequence) -> list[int]:
        '''
        The same Signature can be accessed with different numbers of indices,
        e.g. a%b, a%b(1) and a(1)%b. This routine examines all accesses and
        returns a list of the maximum number of dimensions that is used for
        each component of the Signature.

        :param sig: the Signature of the structure access.
        :param sva: details on all accesses for the Signature.

        :returns: the number of dimensions of each component of the structure
                  access represented by the supplied Signature.
        '''
        # This list will hold lists of the number of indices on
        # each component, one list per access, e.g.:
        #  a%b => [0,0] and  a(2)%b => [1,0]
        num_dims_for_access = []
        for access in sva:
            indices = access.component_indices
            # Create the list of number of indices on each component for
            # this access.
            num_dims = []
            for i in range(len(sig)):
                num_dims.append(len(indices[i]))
            num_dims_for_access.append(num_dims)
        # For each component, find the maximum number of dimensions
        # seen in any access.
        max_dims = []
        for i in range(len(sig)):
            max_dims.append(max(dims[i] for dims in num_dims_for_access))
        return max_dims

    @staticmethod
    def _specialise_array_symbol(sym: Symbol, sva: AccessSequence):
        '''
        If we can be confident that the supplied Symbol should be of ArrayType
        due to the way it is accessed then we specialise it in place.

        :param sym: the Symbol to specialise.
        :param sva: information on the ways in which the Symbol is accessed.

        '''
        if all(acs.has_indices() for acs in sva):
            return
        if not sym or isinstance(sym, (DataSymbol, RoutineSymbol)):
            return
        # Find an access that has indices.
        for acs in sva:
            if not acs.has_indices():
                continue
            ndims = None
            for indices in acs.component_indices:
                if indices:
                    ndims = len(indices)
            if ndims is not None:
                sym.specialise(
                    DataSymbol,
                    datatype=ArrayType(UnresolvedType(),
                                       [ArrayType.Extent.DEFERRED]*ndims))
            return

    # -------------------------------------------------------------------------
    def _create_type_map(self, list_of_expressions: Iterable[Node],
                         identical_variables: Optional[dict[str, str]] = None,
                         all_variables_positive: Optional[bool] = None):
        '''This function creates a dictionary mapping each access in any
        of the expressions to either a SymPy Function (if the reference
        is an array reference) or a Symbol (if the reference is not an
        array reference). It defines a new SymPy function for each array,
        which has a special write method implemented that automatically
        converts array indices back by combining each three arguments into
        one expression (i. e. ``a(1,9,2)`` would become ``a(1:9:2)``).

        An access like ``a(i)%b(j)`` is mapped to a function named ``a_b``
        with arguments ``(i,i,1,j,j,1)`` (also handling name clashes in case
        that the user code already contains a symbol ``a_b``). The SymPy
        function created for this new symbol will store the original signature
        and the number of indices for each member (so in the example above
        that would be ``Signature("a%b")`` and ``(1,1)``. This information
        is sufficient to convert the SymPy symbol back to the correct Fortran
        representation.

        A new symbol table is created any time this function is called, so
        it is important to provide all expressions at once for the symbol
        table to avoid name clashes in any expression.

        This function also handles reserved names like 'lambda' which might
        occur in one of the Fortran expressions. These reserved names must be
        renamed (otherwise SymPy parsing, which uses ``eval`` internally,
        fails). A symbol table is used which is initially filled with all
        reserved names. Then for each reference accounted, a unique name is
        generated using this symbol table - so if the reference is a reserved
        name, a new name will be created (e.g. ``lambda`` might become
        ``lambda_1``). The SymPyWriter (e.g. in ``reference_node``) will
        use the renamed value when creating the string representation.

        The optional identical_variables dictionary can contain information
        about variables which are known to be the same. For example, if
        `identical_variables={'i': 'j'}`, then 'i+1' and 'j+1' will be
        considered equal.

        The optional `all_variables_positive` flag can be used to indicate that
        all variables are positive definite. This means that, e.g. 'i+j' will
        be considered greater than 'i'.

        :param list_of_expressions: the list of expressions from which all
            references are taken and added to a symbol table to avoid
            renaming any symbols (so that only member names will be renamed).
        :param identical_variables: which variable names are known to represent
            identical quantities.
        :param all_variables_positive: whether or not (the default) to assume
            that all variables are positive definite quantities.

        '''
        # Create a new symbol table, so previous symbols will not affect this
        # new conversion (i.e. this avoids name clashes with a previous
        # conversion). First, add all reserved names so that these names will
        # automatically be renamed. The symbol table is used later to also
        # create guaranteed unique names for lower and upper bounds.
        # pylint: disable=too-many-locals, too-many-branches
        self._symbol_table = SymbolTable()
        for reserved in SymPyWriter._RESERVED_NAMES:
            self._symbol_table.new_symbol(reserved)

        # Set-up whether we should assume all Symbols are positive.
        assumptions = {}
        if all_variables_positive:
            assumptions["positive"] = True

        # Find every symbol in each of the expressions, and declare this name
        # as either a SymPy Symbol (scalar reference), or a SymPy Function
        # (either an array or a function call).
        vam = VariablesAccessMap()
        for expr in list_of_expressions:
            vam.update(expr.reference_accesses())

        for sig in vam.all_signatures:
            sva: AccessSequence = vam[sig]

            flat_name = "_".join(name for name in sig)
            unique_sym = self._symbol_table.find_or_create_tag(
                str(sig), root_name=flat_name)

            try:
                # Depending on the situation, we won't always
                # have a scope, hence the try...except.
                orig_sym = sva[0].node.scope.symbol_table.lookup(sig.var_name)
            except SymbolError:
                # If we can't find it, use the symbol associated to the sva
                orig_sym = None
                if isinstance(sva[0].node, Reference):
                    orig_sym = sva[0].node.symbol

            is_fn_call = isinstance(orig_sym, RoutineSymbol)

            if (sva.has_indices() or
                    (orig_sym and (orig_sym.is_array or is_fn_call))):
                # A Fortran array or function call. Declare a new SymPy
                # function for it. This SymPy function will convert array
                # expressions back into the original Fortran code.
                if sig.is_structure:
                    max_dims = self._ndims_for_struct_access(sig, sva)
                    # We store the signature and the list of dimensions
                    # associated with each component as part of the Sympy fn.
                    self._sympy_type_map[unique_sym.name] = \
                        self._create_sympy_array_function(
                            unique_sym.name, sig, max_dims, is_call=is_fn_call)
                else:
                    # Not a structure access.
                    self._sympy_type_map[unique_sym.name] = \
                        self._create_sympy_array_function(sig.var_name,
                                                          is_call=is_fn_call)
                    # To avoid confusion in sympy_reader, we specialise any
                    # Symbol that we are now confident is an array.
                    self._specialise_array_symbol(orig_sym, sva)
            else:
                # A scalar access.
                if sig.is_structure:
                    self._sympy_type_map[unique_sym.name] = sympy.Symbol(
                        sig.to_language(), **assumptions)
                else:
                    self._sympy_type_map[unique_sym.name] = sympy.Symbol(
                        sig.var_name, **assumptions)

        if not identical_variables:
            identical_variables = {}
        # For all variables that are the same, set the symbols to be
        # identical. This means if e.g. identical_variables={'i': 'j'},
        # the expression i-j becomes j-j = 0
        for var1, var2 in identical_variables.items():
            if var1 in self._sympy_type_map and var2 in self._sympy_type_map:
                self._sympy_type_map[var1] = self._sympy_type_map[var2]

        # Now all symbols have been added to the symbol table, create
        # unique names for the lower- and upper-bounds using special tags:
        self._lower_bound = \
            self._symbol_table.new_symbol("sympy_lower",
                                          tag="sympy!lower_bound").name
        self._upper_bound = \
            self._symbol_table.new_symbol("sympy_upper",
                                          tag="sympy!upper_bound").name
        # This one is used when the array symbol is accessed (in the original
        # code) without any indexing at all.
        self._no_bounds = self._symbol_table.new_symbol(
            "sympy_no_bounds", tag="sympy!no_bounds").name

    # -------------------------------------------------------------------------
    @property
    def lower_bound(self) -> str:
        ''':returns: the name to be used for an unspecified lower bound.
        '''
        return self._lower_bound

    # -------------------------------------------------------------------------
    @property
    def upper_bound(self) -> str:
        ''':returns: the name to be used for an unspecified upper bound.
        '''
        return self._upper_bound

    # -------------------------------------------------------------------------
    @property
    def no_bounds(self) -> str:
        '''
        :returns: the name to be used when no bounds are present on an
                  array access.
        '''
        return self._no_bounds

    # -------------------------------------------------------------------------
    @property
    def type_map(self) -> dict[str, Union[sympy.core.symbol.Symbol,
                                          sympy.core.function.Function]]:
        ''':returns: the mapping of names to SymPy symbols or functions.

        '''
        return self._sympy_type_map

    # -------------------------------------------------------------------------
    def _to_str(
        self,
        list_of_expressions: Union[Node, Iterable[Node]],
        identical_variables: Optional[dict[str, str]] = None,
        all_variables_positive: Optional[bool] = False) -> Union[str,
                                                                 list[str]]:
        '''Converts PSyIR expressions to strings. It will replace Fortran-
        specific expressions with code that can be parsed by SymPy. The
        argument can either be a single element (in which case a single string
        is returned) or a list/tuple, in which case a list is returned.
        The optional identical_variables dictionary can contain information
        about variables which are known to be the same. For example, if
        `identical_variables={'i': 'j'}`, then 'i+1' and 'j+1' will be
        considered equal.

        :param identical_variables: which variable names are known to be
            identical
        :param list_of_expressions: the list of expressions which are to be
            converted into SymPy-parsable strings.
        :param all_variables_positive: whether or not (the
            default) to assume that all variables are positive definite
            quantities.

        :returns: the converted strings(s).

        '''
        is_list = True
        if isinstance(list_of_expressions, Node):
            is_list = False
            list_of_expressions = [list_of_expressions]

        # Create the type map in `self._sympy_type_map`, which is required
        # when converting these strings to SymPy expressions
        self._create_type_map(list_of_expressions,
                              identical_variables=identical_variables,
                              all_variables_positive=all_variables_positive)

        expression_str_list = []
        for expr in list_of_expressions:
            expression_str_list.append(super().__call__(expr))

        # If the argument was a single expression, only return a single
        # expression, otherwise return a list
        if not is_list:
            return expression_str_list[0]
        return expression_str_list

    # -------------------------------------------------------------------------
    def __call__(
        self,
        list_of_expressions: Union[Node, list[Node]],
        identical_variables: Optional[dict[str, str]] = None,
        all_variables_positive: Optional[bool] = False) \
            -> Union[sympy.core.basic.Basic,
                     list[sympy.core.basic.Basic]]:
        '''
        This function takes a list of PSyIR expressions, and converts
        them all into Sympy expressions using the SymPy parser.
        It takes care of all Fortran specific conversion required (e.g.
        constants with kind specification, ...), including the renaming of
        member accesses, as described in
        https://psyclone.readthedocs.io/en/latest/developer_guide/sympy.html
        The optional identical_variables dictionary can contain information
        about variables which are known to be the same. For example, if
        `identical_variables={'i': 'j'}`, then 'i+1' and 'j+1' will be
        considered equal.

        :param list_of_expressions: the list of expressions which are to be
            converted into SymPy-parsable strings.
        :param identical_variables: which variable names are known to be
            identical
        :param Optional[bool] all_variables_positive: whether or not (the
            default) to assume that all variables are positive definite
            quantities.

        :returns: a 2-tuple consisting of the the converted PSyIR
            expressions, followed by a dictionary mapping the symbol names
            to SymPy Symbols.

        :raises VisitorError: if an invalid SymPy expression is found.
        :raises TypeError: if the identical_variables parameter is not
            a dict, or does contain a key or value that is not a string.

        '''
        if identical_variables:
            if not isinstance(identical_variables, dict):
                raise TypeError(f"Expected identical_variables to be "
                                f"a dictionary, but got "
                                f"{type(identical_variables)}.")
            if any(not isinstance(key, str) or not isinstance(value, str)
                   for key, value in identical_variables.items()):
                raise TypeError("Dictionary identical_variables "
                                "contains a non-string key or value.")

        is_list = True
        if isinstance(list_of_expressions, Node):
            is_list = False
            list_of_expressions = [list_of_expressions]

        expression_str_list = self._to_str(
            list_of_expressions, identical_variables=identical_variables,
            all_variables_positive=all_variables_positive)

        result = []
        for expr in expression_str_list:
            try:
                result.append(parse_expr(expr, self.type_map))
            except SyntaxError as err:
                raise VisitorError(f"Invalid SymPy expression: '{expr}'.") \
                    from err

        if is_list:
            return result
        # We had no list initially, so only convert the one and only
        # list member
        return result[0]

    # -------------------------------------------------------------------------
    def arrayreference_node(self, node: ArrayReference) -> str:
        '''The implementation of the method handling a
        ArrayOfStructureReference is generic enough to also handle
        non-structure arrays. So just use it.

        :param node: a ArrayReference PSyIR node.

        :returns: the code as string.

        '''
        return self.arrayofstructuresreference_node(node)

    # -------------------------------------------------------------------------
    def structurereference_node(self, node: StructureReference) -> str:
        '''The implementation of the method handling a
        ArrayOfStructureReference is generic enough to also handle non-arrays.
        So just use it.

        :param node: a StructureReference PSyIR node.

        :returns: the code as string.

        '''
        return self.arrayofstructuresreference_node(node)

    # -------------------------------------------------------------------------
    def arrayofstructuresreference_node(
        self,
        node: Union[ArrayOfStructuresReference,
                    ArrayReference,
                    StructureReference]) -> str:
        '''
        This handles ArrayOfStructureReferences (and also simple
        StructureReferences).

        :param node: a StructureReference PSyIR node.

        :returns: text representation of the code.

        '''
        sig, indices = node.get_signature_and_indices()

        all_dims = []
        for i, _ in enumerate(sig):
            if indices[i]:
                for index in indices[i]:
                    all_dims.append(index)
            else:
                sym = self._symbol_table.lookup_with_tag("sympy!no_bounds")
                all_dims.append(Reference(sym))

        # Find the unique variable name created in _create_type_map()
        unique_name = self._symbol_table.lookup_with_tag(str(sig)).name

        if self.type_map[unique_name].is_Function:
            # If the SymPy type is a Function then this means the original
            # expression was either an array access or a function call.
            indices_str = self.gen_indices(all_dims)
            return f"{unique_name}({','.join(indices_str)})"

        # Not an array access. We use the unique name for the string.
        return unique_name

    # -------------------------------------------------------------------------
    def literal_node(self, node: Literal) -> str:
        '''This method is called when a Literal instance is found in the PSyIR
        tree. For SymPy we need to handle booleans (which are expected to
        be capitalised: True). Real values work by just ignoring any precision
        information (e.g. 2_4, 3.1_wp). Character constants are not supported
        and will raise an exception.

        :param node: a Literal PSyIR node.

        :returns: the SymPy representation for the literal.

        :raises TypeError: if a character constant is found, which
            is not supported with SymPy.

        '''
        if node.datatype.intrinsic == ScalarType.Intrinsic.BOOLEAN:
            # Booleans need to be converted to SymPy format
            return node.value.capitalize()

        if node.datatype.intrinsic == ScalarType.Intrinsic.CHARACTER:
            raise TypeError(f"SymPy cannot handle strings "
                            f"like '{node.value}'.")
        # All real (single, double precision) and integer work by just
        # using the node value. Single and double precision both use
        # 'e' as specification, which SymPy accepts, and precision
        # information can be ignored.
        return node.value

    def call_node(self, node: Call) -> str:
        '''
        Handler for Call nodes in the PSyIR tree. Ensures that we use the
        name created in the type map and encodes the arguments to the call
        in the same form as is used for array accesses (since both are mapped
        to a Sympy function).

        Note that a :class:`Call` in the PSyIR can represent a call to either a
        subroutine or a function. The latter can appear within expressions and
        either can have zero arguments.

        :param node: a PSyIR Call node.

        :returns: the SymPy representation for the Call.

        '''
        target_name = node.routine.symbol.name
        # Find the unique variable name created in _create_type_map()
        unique_name = self._symbol_table.lookup_with_tag(target_name).name
        # Encode the arguments to the call (if any) in the same way as we do
        # for indices on an array access.
        indices_str = self.gen_indices(node.arguments)
        return f"{unique_name}({','.join(indices_str)})"

    # -------------------------------------------------------------------------
    def intrinsiccall_node(self, node: IntrinsicCall) -> str:
        ''' This method is called when an IntrinsicCall instance is found in
        the PSyIR tree. The Sympy backend will use the exact sympy name for
        some math intrinsics (listed in _intrinsic_to_str) and will remove
        named arguments.

        :param node: an IntrinsicCall PSyIR node.

        :returns: the SymPy representation for the Intrinsic.

        '''
        # Sympy does not support argument names, remove them for now
        if any(node.argument_names):
            # TODO #2302: This is not totally right without canonical intrinsic
            # positions for arguments. One alternative is to refuse it with:
            # raise VisitorError(
            #     f"Named arguments are not supported by SymPy but found: "
            #     f"'{node.debug_string()}'.")
            # but this leaves sympy comparisons almost always giving false when
            # out of order arguments are rare, so instead we ignore it for now.

            # It makes a copy (of the parent because if matters to the call
            # visitor) because we don't want to delete the original arg names
            parent = node.parent.copy()
            node = parent.children[node.position]
            for idx in range(len(node.argument_names)):
                # pylint: disable=protected-access
                node._argument_names[idx] = (node._argument_names[idx][0],
                                             None)
        try:
            name = self._intrinsic_to_str[node.intrinsic]
            args = self._gen_arguments(node)
            return f"{self._nindent}{name}({args})"
        except KeyError:
            return super().call_node(node)

    # -------------------------------------------------------------------------
    def reference_node(self, node: Reference) -> str:
        '''This method is called when a Reference instance is found in the
        PSyIR tree. It handles the case that this normal reference might
        be an array expression, which in the SymPy writer needs to have
        indices added explicitly: it basically converts the array expression
        ``a`` to ``a(sympy_no_bounds, sympy_no_bounds, 1)``.

        :param node: a Reference PSyIR node.

        :returns: the text representation of this reference.

        '''
        # Support renaming a symbol (e.g. if it is a reserved Python name).
        # Look up with the name as tag, which will return the symbol with
        # a unique name (e.g. lambda --> lambda_1):
        try:
            name = self._symbol_table.lookup_with_tag(node.name).name
        except KeyError:
            # If the tag did not exist it means that this symbol has not
            # been re-named, and we can use it as is.
            name = node.name

        if not node.symbol.is_array:
            # This reference is not an array, just return the name
            return name

        # Now this must be an array expression without parentheses. For
        # consistency, we still treat it as a Sympy function call and therefore
        # add the triple array indices to represent `lower:upper:1` for each
        # dimension:
        shape = node.symbol.shape
        result = [f"{self.no_bounds},{self.no_bounds},1"]*len(shape)

        return (f"{name}{self.array_parenthesis[0]}"
                f"{','.join(result)}{self.array_parenthesis[1]}")

    # ------------------------------------------------------------------------
    def binaryoperation_node(self, node: BinaryOperation) -> str:
        '''This function converts logical binary operations into
        SymPy format. Non-logical binary operations have the same
        representation otherwise, so it calls the base class.

        :param node: a Reference PSyIR BinaryOperation.

        '''
        if node.operator in self._BINARY_OP_MAPPING:
            lhs = self._visit(node.children[0])
            rhs = self._visit(node.children[1])
            return self._BINARY_OP_MAPPING[node.operator].format(rhs=rhs,
                                                                 lhs=lhs)

        return super().binaryoperation_node(node)

    # ------------------------------------------------------------------------
    def gen_indices(self,
                    indices: Iterable[Node],
                    var_name: Optional[str] = None):
        '''Given a list of PSyIR nodes representing the dimensions of an
        array, return a list of strings representing those array dimensions.
        This is used both for array references and array declarations. Note
        that 'indices' can also be a shape in case of Fortran. The
        implementation here overwrites the one in the base class to convert
        each array index into three parameters to support array expressions.

        :param indices: list of PSyIR nodes.
        :param var_name: name of the variable for which the dimensions
            are created. Not used in this implementation.

        :returns: the Fortran representation of the dimensions.

        :raises NotImplementedError: if the format of the dimension is not
            supported.

        '''
        dims = []
        for index in indices:
            if isinstance(index, DataNode):
                # literal constant, symbol reference, or computed
                # dimension
                expression = self._visit(index)
                dims.extend([expression, expression, "1"])
            elif isinstance(index, Range):
                # literal constant, symbol reference, or computed
                # dimension
                expression = self._visit(index)
                dims.append(expression)
            elif isinstance(index, ArrayType.ArrayBounds):
                # Lower and upper bounds of an array declaration specified
                # by literal constant, symbol reference, or computed dimension
                lower_expression = self._visit(index.lower)
                upper_expression = self._visit(index.upper)
                dims.extend([lower_expression, upper_expression, "1"])
            elif isinstance(index, ArrayType.Extent):
                # unknown extent
                dims.extend([self.lower_bound, self.upper_bound, "1"])
            else:
                raise NotImplementedError(
                    f"unsupported gen_indices index '{index}'")
        return dims

    # -------------------------------------------------------------------------
    def range_node(self, node: Range) -> str:
        '''This method is called when a Range instance is found in the PSyIR
        tree. This implementation converts a range into three parameters
        for the corresponding SymPy function.

        :param node: a Range PSyIR node.

        :returns: the Fortran code as a string.

        '''
        if node.parent and node.parent.is_lower_bound(
                node.parent.index_of(node)):
            # The range starts for the first element in this
            # dimension, so use the generic name for lower bound:
            start = self.lower_bound
        else:
            start = self._visit(node.start)

        if node.parent and node.parent.is_upper_bound(
                node.parent.index_of(node)):
            # The range ends with the last element in this
            # dimension, so use the generic name for the upper bound:
            stop = self.upper_bound
        else:
            stop = self._visit(node.stop)
        result = f"{start},{stop}"

        step = self._visit(node.step)
        result += f",{step}"

        return result
