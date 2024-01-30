# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2024, Science and Technology Facilities Council
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

from sympy import Function, Symbol
from sympy.parsing.sympy_parser import parse_expr

from psyclone.psyir.backend.fortran import FortranWriter
from psyclone.psyir.backend.visitor import VisitorError
from psyclone.psyir.frontend.sympy_reader import SymPyReader
from psyclone.psyir.nodes import DataNode, Range, Reference, IntrinsicCall
from psyclone.psyir.symbols import (ArrayType, ScalarType, SymbolTable)


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
    https://psyclone-dev.readthedocs.io/en/latest/sympy.html#sympy

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
    _RESERVED_NAMES = set()

    def __init__(self):
        super().__init__()

        # The symbol table is used to create unique names for structure
        # members that are being accessed (these need to be defined as
        # SymPy functions or symbols, which could clash with other
        # references in the expression).
        self._symbol_table = None

        # The writer will use special names in array expressions to indicate
        # the lower and upper bound (e.g. ``a(::)`` becomes
        # ``a(sympy_lower, sympy_upper, 1)``). The symbol table will be used
        # to resolve a potential name clash with a user variable.
        self._lower_bound = "sympy_lower"
        self._upper_bound = "sympy_upper"

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
                      List[:py:class:`sympy.core.basic.Basic`]]

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
    def _create_sympy_array_function(self, name, sig=None, num_dims=None):
        '''Creates a Function class with the given name to be used for SymPy
        parsing. This Function overwrites the conversion to string, and will
        replace the triplicated array indices back to the normal Fortran
        syntax. If the signature Sig and number of dimensions for each
        component of the signature are given, it will add this information
        to the object, so that the SymPyReader can recreate the proper
        access to a user-defined type.

        :param str name: name of the function class to create.
        :param sig: the signature of the variable, which is required
            to convert user defined types back properly. Only defined for
            user-defined types.
        :type sig: Optional[:py:class:`psyclone.core.Signature`]
        :param num_dims: the number of dimensions for each component of a
            user defined type.
        :type num_dims: Optional[List[int]]

        :returns: a SymPy function, which has a special ``_sympystr`` function
            defined as attribute to print user-defined types..
        :rtype: :py:class:`sympy.Function`
        '''

        # Now a new Fortran array is used. Create a new function
        # instance, and overwrite how this function is converted back
        # into a string by defining the ``_sympystr`` attribute,
        # which points to a function that controls how this object
        # is converted into a string. Use the ``print_fortran_array``
        # function from the SymPyReader for this. Note that we cannot
        # create a derived class based on ``Function`` and define
        # this function there: SymPy tests internally if the type is a
        # Function (not if it is an instance), therefore, SymPy's
        # behaviour would change if we used a derived class:
        # https://docs.sympy.org/latest/modules/functions/index.html:
        # "It [Function class] also serves as a constructor for undefined
        # function classes."
        new_func = Function(name)
        # pylint: disable=protected-access
        new_func._sympystr = SymPyReader.print_fortran_array

        # Store the signature and the number of dimensions of each
        # component, so that SymPyReader.print_fortran_array can match
        # the indices back to the user defined types.
        new_func._sig = sig
        new_func._num_dims = num_dims
        # pylint: enable=protected-access
        return new_func

    # -------------------------------------------------------------------------
    def _create_type_map(self, list_of_expressions):
        '''This function creates a dictionary mapping each Reference in any
        of the expressions to either a SymPy Function (if the reference
        is an array reference) or a Symbol (if the reference is not an
        array reference). It defines a new SymPy function for each array,
        which has a special write method implemented that automatically
        converts array indices back by combining each three arguments into
        one expression (i. e. ``a(1,9,2)`` would become ``a(1:9:2)``).

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

        :param list_of_expressions: the list of expressions from which all
            references are taken and added to a symbol table to avoid
            renaming any symbols (so that only member names will be renamed).
        :type list_of_expressions: List[:py:class:`psyclone.psyir.nodes.Node`]

        '''
        # Create a new symbol table, so previous symbol will not affect this
        # new conversion (i.e. this avoids name clashes with a previous
        # conversion). First add all reserved names so that these names will
        # automatically be renamed. The symbol table is used later to also
        # create guaranteed unique names for lower and upper bounds.
        self._symbol_table = SymbolTable()
        for reserved in SymPyWriter._RESERVED_NAMES:
            self._symbol_table.new_symbol(reserved)

        # Find each reference in each of the expression, and declare this name
        # as either a SymPy Symbol (scalar reference), or a SymPy Function
        # (an array).
        for expr in list_of_expressions:
            for ref in expr.walk(Reference):
                name = ref.name
                # The reserved Python keywords do not have tags, so they
                # will not be found.
                if name in self._symbol_table.tags_dict:
                    continue

                # Any symbol from the list of expressions to be handled
                # will be created with a tag, so if the same symbol is
                # used more than once, the previous test will prevent
                # calling new_symbol again. If the name is a Python
                # reserved symbol, a new unique name will be created by
                # the symbol table.
                unique_sym = self._symbol_table.new_symbol(name, tag=name)
                # Test if an array or an array expression is used:
                if not ref.is_array:
                    self._sympy_type_map[unique_sym.name] = Symbol(name)
                    continue

                # A Fortran array is used which has not been seen before.
                # Declare a new SymPy function for it. This SymPy function
                # will convert array expressions back into the original
                # Fortran code.
                self._sympy_type_map[unique_sym.name] = \
                    self._create_sympy_array_function(name)

        # Now all symbols have been added to the symbol table, create
        # unique names for the lower- and upper-bounds using special tags:
        self._lower_bound = \
            self._symbol_table.new_symbol("sympy_lower",
                                          tag="sympy!lower_bound").name
        self._upper_bound = \
            self._symbol_table.new_symbol("sympy_upper",
                                          tag="sympy!upper_bound").name

    # -------------------------------------------------------------------------
    @property
    def lower_bound(self):
        ''':returns: the name to be used for an unspecified lower bound.
        :rtype: str

        '''
        return self._lower_bound

    # -------------------------------------------------------------------------
    @property
    def upper_bound(self):
        ''':returns: the name to be used for an unspecified upper bound.
        :rtype: str

        '''
        return self._upper_bound

    # -------------------------------------------------------------------------
    @property
    def type_map(self):
        ''':returns: the mapping of names to SymPy symbols or functions.
        :rtype: Dict[str, Union[:py:class:`sympy.core.symbol.Symbol`,
                                :py:class:`sympy.core.function.Function`]]

        '''
        return self._sympy_type_map

    # -------------------------------------------------------------------------
    def _to_str(self, list_of_expressions):
        '''Converts PSyIR expressions to strings. It will replace Fortran-
        specific expressions with code that can be parsed by SymPy. The
        argument can either be a single element (in which case a single string
        is returned) or a list/tuple, in which case a list is returned.

        :param list_of_expressions: the list of expressions which are to be
            converted into SymPy-parsable strings.
        :type list_of_expressions: Union[:py:class:`psyclone.psyir.nodes.Node`,
            List[:py:class:`psyclone.psyir.nodes.Node`]]

        :returns: the converted strings(s).
        :rtype: Union[str, List[str]]

        '''
        is_list = isinstance(list_of_expressions, (tuple, list))
        if not is_list:
            list_of_expressions = [list_of_expressions]

        # Create the type map in `self._sympy_type_map`, which is required
        # when converting these strings to SymPy expressions
        self._create_type_map(list_of_expressions)

        expression_str_list = []
        for expr in list_of_expressions:
            expression_str_list.append(super().__call__(expr))

        # If the argument was a single expression, only return a single
        # expression, otherwise return a list
        if not is_list:
            return expression_str_list[0]
        return expression_str_list

    # -------------------------------------------------------------------------
    def __call__(self, list_of_expressions):
        '''
        This function takes a list of PSyIR expressions, and converts
        them all into Sympy expressions using the SymPy parser.
        It takes care of all Fortran specific conversion required (e.g.
        constants with kind specification, ...), including the renaming of
        member accesses, as described in
        https://psyclone-dev.readthedocs.io/en/latest/sympy.html#sympy

        :param list_of_expressions: the list of expressions which are to be
            converted into SymPy-parsable strings.
        :type list_of_expressions: list of
            :py:class:`psyclone.psyir.nodes.Node`

        :returns: a 2-tuple consisting of the the converted PSyIR
            expressions, followed by a dictionary mapping the symbol names
            to SymPy Symbols.
        :rtype: Union[:py:class:`sympy.core.basic.Basic`,
                      List[:py:class:`sympy.core.basic.Basic`]]

        :raises VisitorError: if an invalid SymPy expression is found.

        '''
        is_list = isinstance(list_of_expressions, (tuple, list))
        if not is_list:
            list_of_expressions = [list_of_expressions]
        expression_str_list = self._to_str(list_of_expressions)

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
    def arrayreference_node(self, node):
        '''The implementation of the method handling a
        ArrayOfStructureReference is generic enough to also handle
        non-structure arrays. So just use it.

        :param node: a ArrayReference PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.ArrayReference`

        :returns: the code as string.
        :rtype: str

        '''
        return self.arrayofstructuresreference_node(node)

    # -------------------------------------------------------------------------
    def structurereference_node(self, node):
        '''The implementation of the method handling a
        ArrayOfStructureReference is generic enough to also handle non-arrays.
        So just use it.

        :param node: a StructureReference PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.StructureReference`

        :returns: the code as string.
        :rtype: str

        '''
        return self.arrayofstructuresreference_node(node)

    # -------------------------------------------------------------------------
    def arrayofstructuresreference_node(self, node):
        '''This handles ArrayOfStructureReferences (and also simple
        StructureReferences). An access like ``a(i)%b(j)`` is converted to
        the string ``a_b(i,i,1,j,j,1)`` (also handling name clashes in case
        that the user code already contains a symbol ``a_b``). The SymPy
        function created for this new symbol will store the original signature
        and the number of indices for each member (so in the example above
        that would be ``Signature("a%b")`` and ``(1,1)``. This information
        is sufficient to convert the SymPy symbol back to the correct Fortran
        representation

        :param node: a StructureReference PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.StructureReference`

        :returns: the code as string.
        :rtype: str

        '''
        sig, indices = node.get_signature_and_indices()

        out = []
        num_dims = []
        all_dims = []
        is_array = False
        for i, name in enumerate(sig):
            num_dims.append(len(indices[i]))
            for index in indices[i]:
                all_dims.append(index)
                is_array = True
            out.append(name)
        flat_name = "_".join(out)

        # Find (or create) a unique variable name:
        try:
            unique_name = self._symbol_table.lookup_with_tag(str(sig)).name
        except KeyError:
            unique_name = self._symbol_table.new_symbol(flat_name,
                                                        tag=str(sig)).name
        if is_array:
            indices_str = self.gen_indices(all_dims)
            # Create the corresponding SymPy function, which will store
            # the signature and num_dims, so that the correct Fortran
            # representation can be recreated later.
            self._sympy_type_map[unique_name] = \
                self._create_sympy_array_function(unique_name, sig, num_dims)
            return f"{unique_name}({','.join(indices_str)})"

        # Not an array access. We use the unique name  for the string,
        # but the required symbol is mapped to the original name, which means
        # if the SymPy expression is converted to a string (in order to be
        # parsed), it will use the original structure reference syntax:
        self._sympy_type_map[unique_name] = Symbol(sig.to_language())
        return unique_name

    # -------------------------------------------------------------------------
    def literal_node(self, node):
        '''This method is called when a Literal instance is found in the PSyIR
        tree. For SymPy we need to handle booleans (which are expected to
        be capitalised: True). Real values work by just ignoring any precision
        information (e.g. 2_4, 3.1_wp). Character constants are not supported
        and will raise an exception.

        :param node: a Literal PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Literal`

        :returns: the SymPy representation for the literal.
        :rtype: str

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

    def intrinsiccall_node(self, node):
        ''' This method is called when an IntrinsicCall instance is found in
        the PSyIR tree. The Sympy backend will use the exact sympy name for
        some math intrinsics (listed in _intrinsic_to_str) and will remove
        named arguments.

        :param node: an IntrinsicCall PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.IntrinsicCall`

        :returns: the SymPy representation for the Intrinsic.
        :rtype: str

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
    def reference_node(self, node):
        '''This method is called when a Reference instance is found in the
        PSyIR tree. It handles the case that this normal reference might
        be an array expression, which in the SymPy writer needs to have
        indices added explicitly: it basically converts the array expression
        ``a`` to ``a(sympy_lower, sympy_upper, 1)``.

        :param node: a Reference PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Reference`

        :returns: the text representation of this reference.
        :rtype: str

        '''
        # Support renaming a symbol (e.g. if it is a reserved Python name).
        # Look up with the name as tag, which will return the symbol with
        # a unique name (e.g. lambda --> lambda_1):
        name = self._symbol_table.lookup_with_tag(node.name).name
        if not node.is_array:
            # This reference is not an array, just return the name
            return name

        # Now this must be an array expression without parenthesis. Add
        # the triple-array indices to represent `lower:upper:1` for each
        # dimension:
        shape = node.symbol.shape
        result = [f"{self.lower_bound},"
                  f"{self.upper_bound},1"]*len(shape)

        return (f"{name}{self.array_parenthesis[0]}"
                f"{','.join(result)}{self.array_parenthesis[1]}")

    # ------------------------------------------------------------------------
    def gen_indices(self, indices, var_name=None):
        '''Given a list of PSyIR nodes representing the dimensions of an
        array, return a list of strings representing those array dimensions.
        This is used both for array references and array declarations. Note
        that 'indices' can also be a shape in case of Fortran. The
        implementation here overwrites the one in the base class to convert
        each array index into three parameters to support array expressions.

        :param indices: list of PSyIR nodes.
        :type indices: List[:py:class:`psyclone.psyir.symbols.Node`]
        :param str var_name: name of the variable for which the dimensions
            are created. Not used in this implementation.

        :returns: the Fortran representation of the dimensions.
        :rtype: List[str]

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
    def range_node(self, node):
        '''This method is called when a Range instance is found in the PSyIR
        tree. This implementation converts a range into three parameters
        for the corresponding SymPy function.

        :param node: a Range PSyIR node.
        :type node: :py:class:`psyclone.psyir.nodes.Range`

        :returns: the Fortran code as a string.
        :rtype: str

        '''
        if node.parent and node.parent.is_lower_bound(
                node.parent.indices.index(node)):
            # The range starts for the first element in this
            # dimension, so use the generic name for lower bound:
            start = self.lower_bound
        else:
            start = self._visit(node.start)

        if node.parent and node.parent.is_upper_bound(
                node.parent.indices.index(node)):
            # The range ends with the last element in this
            # dimension, so use the generic name for the upper bound:
            stop = self.upper_bound
        else:
            stop = self._visit(node.stop)
        result = f"{start},{stop}"

        step = self._visit(node.step)
        result += f",{step}"

        return result
