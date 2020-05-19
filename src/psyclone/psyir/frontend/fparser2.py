# BSD 3-Clause License
#
# Copyright (c) 2017-2020, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified I. Kavcic, Met Office
# -----------------------------------------------------------------------------

''' This module provides the fparser2 to PSyIR front-end, it follows a
    Visitor Pattern to traverse relevant fparser2 nodes and contains the logic
    to transform each node into the equivalent PSyIR representation.'''

from __future__ import absolute_import
from collections import OrderedDict
from fparser.two import Fortran2003
from fparser.two.utils import walk
from psyclone.psyir.nodes import UnaryOperation, BinaryOperation, \
    NaryOperation, Schedule, CodeBlock, IfBlock, Reference, Literal, Loop, \
    Container, Assignment, Return, Array, Node, Range
from psyclone.errors import InternalError, GenerationError
from psyclone.psyGen import Directive, KernelSchedule
from psyclone.psyir.symbols import SymbolError, DataSymbol, ContainerSymbol, \
    Symbol, GlobalInterface, ArgumentInterface, UnresolvedInterface, \
    LocalInterface, ScalarType, ArrayType, DeferredType

# The list of Fortran instrinsic functions that we know about (and can
# therefore distinguish from array accesses). These are taken from
# fparser.
FORTRAN_INTRINSICS = Fortran2003.Intrinsic_Name.function_names

# Mapping from Fortran data types to PSyIR types
TYPE_MAP_FROM_FORTRAN = {"integer": ScalarType.Intrinsic.INTEGER,
                         "character": ScalarType.Intrinsic.CHARACTER,
                         "logical": ScalarType.Intrinsic.BOOLEAN,
                         "real": ScalarType.Intrinsic.REAL}

# Mapping from fparser2 Fortran Literal types to PSyIR types
CONSTANT_TYPE_MAP = {
    Fortran2003.Real_Literal_Constant: ScalarType.Intrinsic.REAL,
    Fortran2003.Logical_Literal_Constant: ScalarType.Intrinsic.BOOLEAN,
    Fortran2003.Char_Literal_Constant: ScalarType.Intrinsic.CHARACTER,
    Fortran2003.Int_Literal_Constant: ScalarType.Intrinsic.INTEGER}


def _get_symbol_table(node):
    '''Find a symbol table associated with an ancestor of Node 'node' (or
    the node itself). If there is more than one symbol table, then the
    symbol table closest in ancestory to 'node' is returned. If no
    symbol table is found then None is returned.

    :param node: a PSyIR Node.
    :type node: :py:class:`psyclone.psyir.nodes.Node`

    :returns: a symbol table associated with node or one of its \
        ancestors or None if one is not found.
    :rtype: :py:class:`psyclone.psyir.symbols.SymbolTable` or NoneType

    :raises TypeError: if the node argument is not a Node.

    '''
    if not isinstance(node, Node):
        raise TypeError(
            "node argument to _get_symbol_table() should be of type Node, but "
            "found '{0}'.".format(type(node).__name__))
    current = node
    while current:
        if hasattr(current, "symbol_table"):
            return current.symbol_table
        current = current.parent
    return None


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
    if not isinstance(array, Array):
        raise TypeError(
            "method _check_args 'array' argument should be an "
            "Array type but found '{0}'.".format(type(array).__name__))

    if not isinstance(dim, int):
        raise TypeError(
            "method _check_args 'dim' argument should be an "
            "int type but found '{0}'.".format(type(dim).__name__))
    if dim < 1:
        raise ValueError(
            "method _check_args 'dim' argument should be at "
            "least 1 but found {0}.".format(dim))
    if dim > len(array.children):
        raise ValueError(
            "method _check_args 'dim' argument should be at "
            "most the number of dimensions of the array ({0}) but found "
            "{1}.".format(len(array.children), dim))

    # The first child of the array (index 0) relates to the first
    # dimension (dim 1), so we need to reduce dim by 1.
    if not isinstance(array.children[dim-1], Range):
        raise TypeError(
            "method _check_args 'array' argument index '{0}' "
            "should be a Range type but found '{1}'."
            "".format(dim-1, type(array.children[dim-1]).__name__))


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

    The supplied "array" argument is assumed to be an Array node and
    the contents of the specified dimension "dim" is assumed to be a
    Range node.

    This routine is only in fparser2.py until #717 is complete as it
    is used to check that array syntax in a where statement is for the
    full extent of the dimension. Once #717 is complete this routine
    can be moved into fparser2_test.py as it is used there in a
    different context.

    :param array: the node to check.
    :type array: :py:class:`pysclone.psyir.node.array`
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
            "'operator' argument  expected to be LBOUND or UBOUND but "
            "found '{0}'.".format(type(operator).__name__))

    # The first child of the array (index 0) relates to the first
    # dimension (dim 1), so we need to reduce dim by 1.
    bound = array.children[dim-1].children[index]

    if not isinstance(bound, BinaryOperation):
        return False

    reference = bound.children[0]
    literal = bound.children[1]

    # pylint: disable=too-many-boolean-expressions
    if (bound.operator == operator
            and isinstance(reference, Reference) and
            reference.symbol is array.symbol
            and isinstance(literal, Literal) and
            literal.datatype.intrinsic == ScalarType.Intrinsic.INTEGER
            and literal.value == str(dim)):
        return True
    # pylint: enable=too-many-boolean-expressions
    return False


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
    :type array: :py:class:`pysclone.psyir.node.Array`
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
            "method _check_array_range_literal 'index' argument should be an "
            "int type but found '{0}'.".format(type(index).__name__))

    if index < 0 or index > 2:
        raise ValueError(
            "method _check_array_range_literal 'index' argument should be "
            "0, 1 or 2 but found {0}.".format(index))

    if not isinstance(value, int):
        raise TypeError(
            "method _check_array_range_literal 'value' argument should be an "
            "int type but found '{0}'.".format(type(value).__name__))

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
            "Unsupported literal type '{0}' found in get_literal_precision."
            "".format(type(fparser2_node).__name__))
    if not isinstance(psyir_literal_parent, Node):
        raise InternalError(
            "Expecting argument psyir_literal_parent to be a PSyIR Node but "
            "found '{0}' in get_literal_precision."
            "".format(type(psyir_literal_parent).__name__))
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
                "Could not process {0}. Only 'real', 'integer', "
                "'logical' and 'character' intrinsic types are "
                "supported.".format(type(fparser2_node).__name__))
        return default_precision(data_name)
    try:
        # Precision is specified as an integer
        return int(precision_name)
    except ValueError:
        # Precision is not an integer so should be a kind symbol
        # PSyIR stores names as lower case.
        precision_name = precision_name.lower()
        # Find the closest symbol table
        symbol_table = _get_symbol_table(psyir_literal_parent)
        if not symbol_table:
            # No symbol table found. This should never happen in
            # normal usage but could occur if a test constructs a
            # PSyIR without a Schedule.
            raise InternalError(
                "Failed to find a symbol table to which to add the kind "
                "symbol '{0}'.".format(precision_name))
        # Lookup the precision symbol
        try:
            symbol = symbol_table.lookup(precision_name)
        except KeyError:
            # The symbol is not found so create a data
            # symbol with deferred type and add it to the
            # symbol table then return the symbol.
            symbol = DataSymbol(precision_name, DeferredType(),
                                interface=UnresolvedInterface())
            symbol_table.add(symbol)
        return symbol


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
        ('real', UnaryOperation.Operator.REAL),
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
        ('sign', BinaryOperation.Operator.SIGN),
        ('size', BinaryOperation.Operator.SIZE),
        ('sum', BinaryOperation.Operator.SUM),
        ('lbound', BinaryOperation.Operator.LBOUND),
        ('ubound', BinaryOperation.Operator.UBOUND),
        ('max', BinaryOperation.Operator.MAX),
        ('min', BinaryOperation.Operator.MIN),
        ('mod', BinaryOperation.Operator.REM),
        ('matmul', BinaryOperation.Operator.MATMUL)])

    nary_operators = OrderedDict([
        ('max', NaryOperation.Operator.MAX),
        ('min', NaryOperation.Operator.MIN),
        ('sum', NaryOperation.Operator.SUM)])

    def __init__(self):
        from fparser.two import utils
        # Map of fparser2 node types to handlers (which are class methods)
        self.handlers = {
            Fortran2003.Assignment_Stmt: self._assignment_handler,
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
        if isinstance(parent, Schedule):
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
    def get_inputs_outputs(nodes):
        '''
        Identify variables that are inputs and outputs to the section of
        Fortran code represented by the supplied list of nodes in the
        fparser2 parse tree. Loop variables are ignored.

        :param nodes: list of Nodes in the fparser2 AST to analyse.
        :type nodes: list of :py:class:`fparser.two.utils.Base`

        :return: 3-tuple of list of inputs, list of outputs, list of in-outs
        :rtype: (list of str, list of str, list of str)
        '''
        from fparser.two.Fortran2003 import Assignment_Stmt, Part_Ref, \
            Data_Ref, If_Then_Stmt, Array_Section
        readers = set()
        writers = set()
        readwrites = set()
        # A dictionary of all array accesses that we encounter - used to
        # sanity check the readers and writers we identify.
        all_array_refs = {}

        # Loop over a flat list of all the nodes in the supplied region
        for node in walk(nodes):

            if isinstance(node, Assignment_Stmt):
                # Found lhs = rhs
                structure_name_str = None

                lhs = node.items[0]
                rhs = node.items[2]
                # Do RHS first as we cull readers after writers but want to
                # keep a = a + ... as the RHS is computed before assigning
                # to the LHS
                for node2 in walk(rhs):
                    if isinstance(node2, Part_Ref):
                        name = node2.items[0].string
                        if name.upper() not in FORTRAN_INTRINSICS:
                            if name not in writers:
                                readers.add(name)
                    if isinstance(node2, Data_Ref):
                        # TODO we need a robust implementation - issue #309.
                        raise NotImplementedError(
                            "get_inputs_outputs: derived-type references on "
                            "the RHS of assignments are not yet supported.")
                # Now do LHS
                if isinstance(lhs, Data_Ref):
                    # This is a structure which contains an array access.
                    structure_name_str = lhs.items[0].string
                    writers.add(structure_name_str)
                    lhs = lhs.items[1]
                if isinstance(lhs, (Part_Ref, Array_Section)):
                    # This is an array reference
                    name_str = lhs.items[0].string
                    if structure_name_str:
                        # Array ref is part of a derived type
                        name_str = "{0}%{1}".format(structure_name_str,
                                                    name_str)
                        structure_name_str = None
                    writers.add(name_str)
            elif isinstance(node, If_Then_Stmt):
                # Check for array accesses in IF statements
                array_refs = walk(node, Part_Ref)
                for ref in array_refs:
                    name = ref.items[0].string
                    if name.upper() not in FORTRAN_INTRINSICS:
                        if name not in writers:
                            readers.add(name)
            elif isinstance(node, Part_Ref):
                # Keep a record of all array references to check that we
                # haven't missed anything. Once #309 is done we should be
                # able to get rid of this check.
                name = node.items[0].string
                if name.upper() not in FORTRAN_INTRINSICS and \
                   name not in all_array_refs:
                    all_array_refs[name] = node
            elif node:
                # TODO #309 handle array accesses in other contexts, e.g. as
                # loop bounds in DO statements.
                pass

        # Sanity check that we haven't missed anything. To be replaced when
        # #309 is done.
        accesses = list(readers) + list(writers)
        for name, node in all_array_refs.items():
            if name not in accesses:
                # A matching bare array access hasn't been found but it
                # might have been part of a derived-type access so check
                # for that.
                found = False
                for access in accesses:
                    if "%"+name in access:
                        found = True
                        break
                if not found:
                    raise InternalError(
                        "Array '{0}' present in source code ('{1}') but not "
                        "identified as being read or written.".
                        format(name, str(node)))
        # Now we check for any arrays that are both read and written
        readwrites = readers & writers
        # Remove them from the readers and writers sets
        readers = readers - readwrites
        writers = writers - readwrites
        # Convert sets to lists and sort so that we get consistent results
        # between Python versions (for testing)
        rlist = list(readers)
        rlist.sort()
        wlist = list(writers)
        wlist.sort()
        rwlist = list(readwrites)
        rwlist.sort()

        return (rlist, wlist, rwlist)

    @staticmethod
    def _create_schedule(name):
        '''
        Create an empty KernelSchedule.

        :param str name: Name of the subroutine represented by the kernel.

        :returns: New KernelSchedule empty object.
        :rtype: py:class:`psyclone.psyGen.KernelSchedule`

        '''
        return KernelSchedule(name)

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
                "Could not process {0}. Just one module definition per file "
                "supported.".format(str(module_ast)))
        if not modules:
            return None

        module = modules[0].parent
        mod_name = str(modules[0].children[1])

        # Create a container to capture the module information
        new_container = Container(mod_name)

        # Parse the declarations if it has any
        for child in module.children:
            if isinstance(child, Fortran2003.Specification_Part):
                self.process_declarations(new_container, child.children, [])
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
        :rtype: :py:class:`psyclone.psyGen.KernelSchedule`

        :raises GenerationError: unable to generate a kernel schedule from \
                                 the provided fpaser2 parse tree.

        '''
        def first_type_match(nodelist, typekind):
            '''
            Returns the first instance of the specified type in the given
            node list.

            :param list nodelist: List of fparser2 nodes.
            :param type typekind: The fparse2 Type we are searching for.
            '''
            for node in nodelist:
                if isinstance(node, typekind):
                    return node
            raise ValueError  # Type not found

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
            raise GenerationError("Unexpected kernel AST. Could not find "
                                  "subroutine: {0}".format(name))

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
            new_schedule.parent = container
            container.children.append(new_schedule)

        # Set pointer from schedule into fparser2 tree
        # TODO #435 remove this line once fparser2 tree not needed
        # pylint: disable=protected-access
        new_schedule._ast = subroutine
        # pylint: enable=protected-access

        try:
            sub_spec = first_type_match(subroutine.content,
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
            sub_exec = first_type_match(subroutine.content,
                                        Fortran2003.Execution_Part)
        except ValueError:
            pass
        else:
            self.process_nodes(new_schedule, sub_exec.content)

        return new_schedule

    @staticmethod
    def _parse_dimensions(dimensions, symbol_table):
        '''
        Parse the fparser dimension attribute into a shape list with
        the extent of each dimension.

        :param dimensions: fparser dimension attribute
        :type dimensions: \
            :py:class:`fparser.two.Fortran2003.Dimension_Attr_Spec`
        :param symbol_table: Symbol table of the declaration context.
        :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable`
        :returns: Shape of the attribute in column-major order (leftmost \
                  index is contiguous in memory). Each entry represents \
                  an array dimension. If it is 'None' the extent of that \
                  dimension is unknown, otherwise it holds an integer \
                  with the extent. If it is an empty list then the symbol \
                  represents a scalar.
        :rtype: list
        '''
        shape = []

        # Traverse shape specs in Depth-first-search order
        for dim in walk(dimensions, (Fortran2003.Assumed_Shape_Spec,
                                     Fortran2003.Explicit_Shape_Spec,
                                     Fortran2003.Assumed_Size_Spec)):

            if isinstance(dim, Fortran2003.Assumed_Shape_Spec):
                shape.append(None)

            elif isinstance(dim, Fortran2003.Explicit_Shape_Spec):
                def _unsupported_type_error(dimensions):
                    raise NotImplementedError(
                        "Could not process {0}. Only scalar integer literals"
                        " or symbols are supported for explicit shape array "
                        "declarations.".format(dimensions))
                if isinstance(dim.items[1],
                              Fortran2003.Int_Literal_Constant):
                    shape.append(int(dim.items[1].items[0]))
                elif isinstance(dim.items[1], Fortran2003.Name):
                    # Fortran does not regulate the order in which variables
                    # may be declared so it's possible for the shape
                    # specification of an array to reference variables that
                    # come later in the list of declarations. The reference
                    # may also be to a symbol present in a parent symbol table
                    # (e.g. if the variable is declared in an outer, module
                    # scope).
                    dim_name = dim.items[1].string.lower()
                    try:
                        sym = symbol_table.lookup(dim_name)
                        if (sym.datatype.intrinsic !=
                                ScalarType.Intrinsic.INTEGER or
                                isinstance(sym.datatype, ArrayType)):
                            _unsupported_type_error(dimensions)
                    except KeyError:
                        # We haven't seen this symbol before so create a new
                        # one with a deferred interface (since we don't
                        # currently know where it is declared).
                        sym = DataSymbol(dim_name, default_integer_type(),
                                         interface=UnresolvedInterface())
                        symbol_table.add(sym)
                    shape.append(sym)
                else:
                    _unsupported_type_error(dimensions)

            elif isinstance(dim, Fortran2003.Assumed_Size_Spec):
                raise NotImplementedError(
                    "Could not process {0}. Assumed-size arrays"
                    " are not supported.".format(dimensions))

            else:
                raise InternalError(
                    "Reached end of loop body and array-shape specification "
                    "{0} has not been handled.".format(type(dim)))

        return shape

    @staticmethod
    def _parse_access_statements(nodes):
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
            unit, list of public symbol names, list of private symbol names.
        :rtype: 3-tuple of (:py:class:`psyclone.symbols.Symbol.Visibility`, \
                list, list)

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
                    "Failed to process '{0}'. Found an accessibility "
                    "attribute of '{1}' but expected either 'public' or "
                    "'private'.".format(str(stmt), stmt.children[0]))
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
                                "Module '{0}' contains more than one access "
                                "statement with an omitted access-id-list. "
                                "This is invalid Fortran.".format(mod_name))
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
                if public_stmt:
                    explicit_public.update(
                        [child.string for child in stmt.children[1].children])
                else:
                    explicit_private.update(
                        [child.string for child in stmt.children[1].children])
        # Sanity check the lists of symbols (because fparser2 does not
        # currently do much validation)
        invalid_symbols = explicit_public.intersection(explicit_private)
        if invalid_symbols:
            raise GenerationError(
                "Symbols {0} appear in access statements with both PUBLIC "
                "and PRIVATE access-ids. This is invalid Fortran.".format(
                    list(invalid_symbols)))

        # Symbols are public by default in Fortran
        if default_visibility is None:
            default_visibility = Symbol.Visibility.PUBLIC

        return (default_visibility, list(explicit_public),
                list(explicit_private))

    def process_declarations(self, parent, nodes, arg_list):
        '''
        Transform the variable declarations in the fparser2 parse tree into
        symbols in the symbol table of the PSyIR parent node.

        :param parent: PSyIR node in which to insert the symbols found.
        :type parent: :py:class:`psyclone.psyGen.KernelSchedule`
        :param nodes: fparser2 AST nodes to search for declaration statements.
        :type nodes: list of :py:class:`fparser.two.utils.Base`
        :param arg_list: fparser2 AST node containing the argument list.
        :type arg_list: :py:class:`fparser.Fortran2003.Dummy_Arg_List`

        :raises NotImplementedError: the provided declarations contain \
                                     attributes which are not supported yet.
        :raises GenerationError: if the parse tree for a USE statement does \
                                 not have the expected structure.
        :raises SymbolError: if a declaration is found for a Symbol that is \
                    already in the symbol table with a defined interface.
        :raises InternalError: if the provided declaration is an unexpected \
                               or invalid fparser or Fortran expression.

        '''
        # Search for any accessibility statements (e.g. "PUBLIC :: my_var") to
        # determine the default accessibility of symbols as well as identifying
        # those that are explicitly declared as public or private.
        (default_visibility, explicit_public_symbols,
         explicit_private_symbols) = self._parse_access_statements(nodes)

        # Look at any USE statements
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
                    "Expected the parse tree for a USE statement to contain "
                    "5 items but found {0} for '{1}'".format(len(decl.items),
                                                             text))

            mod_name = str(decl.items[2])

            # Add the module symbol to the symbol table. Keep a record of
            # whether or not we've seen this module before for reporting
            # purposes in the code below.
            if mod_name not in parent.symbol_table:
                new_container = True
                container = ContainerSymbol(mod_name)
                parent.symbol_table.add(container)
            else:
                new_container = False
                container = parent.symbol_table.lookup(mod_name)
                if not isinstance(container, ContainerSymbol):
                    raise SymbolError(
                        "Found a USE of module '{0}' but the symbol table "
                        "already has a non-container entry with that name "
                        "({1}). This is invalid Fortran.".format(
                            mod_name, str(container)))

            # Create a 'deferred' symbol for each element in the ONLY clause.
            if isinstance(decl.items[4], Fortran2003.Only_List):
                if not new_container and not container.wildcard_import \
                   and not parent.symbol_table.imported_symbols(container):
                    # TODO #11 Log the fact that this explicit symbol import
                    # will replace a previous import with an empty only-list.
                    pass
                for name in decl.items[4].items:
                    # The DataSymbol adds itself to the list of symbols
                    # imported by the Container referenced in the
                    # GlobalInterface.
                    sym_name = str(name).lower()
                    if sym_name not in parent.symbol_table:
                        parent.symbol_table.add(
                            DataSymbol(sym_name,
                                       DeferredType(),
                                       interface=GlobalInterface(container)))
                    else:
                        # There's already a symbol with this name
                        existing_symbol = parent.symbol_table.lookup(sym_name)
                        if not existing_symbol.is_global:
                            raise SymbolError(
                                "Symbol '{0}' is imported from module '{1}' "
                                "but is already present in the symbol table as"
                                " either an argument or a local ({2}).".
                                format(sym_name, mod_name,
                                       str(existing_symbol)))
                        # TODO #11 Log the fact that we've already got an
                        # import of this symbol and that will take precendence.
            elif not decl.items[3]:
                # We have a USE statement without an ONLY clause.
                if (not new_container) and (not container.wildcard_import) \
                   and (not parent.symbol_table.imported_symbols(container)):
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
                        parent.symbol_table.imported_symbols(container)):
                    # TODO #11 Log the fact that this import with an empty
                    # only-list is ignored because of existing 'use's of
                    # the module.
                    pass
            else:
                raise NotImplementedError("Found unsupported USE statement: "
                                          "'{0}'".format(str(decl)))

        for decl in walk(nodes, Fortran2003.Type_Declaration_Stmt):
            (type_spec, attr_specs, entities) = decl.items

            # Parse type_spec, currently just 'real', 'integer', 'logical' and
            # 'character' intrinsic types are supported.
            if isinstance(type_spec, Fortran2003.Intrinsic_Type_Spec):
                fort_type = str(type_spec.items[0]).lower()
                try:
                    data_name = TYPE_MAP_FROM_FORTRAN[fort_type]
                except KeyError:
                    raise NotImplementedError(
                        "Could not process {0}. Only 'real', 'integer', "
                        "'logical' and 'character' intrinsic types are "
                        "supported.".format(str(decl.items)))
                # Check for a KIND specification
                precision = self._process_kind_selector(
                    type_spec, parent)

            # Parse declaration attributes:
            # 1) If no dimension attribute is provided, it defaults to scalar.
            attribute_shape = []
            # 2) If no intent attribute is provided, it is provisionally
            # marked as a local variable (when the argument list is parsed,
            # arguments with no explicit intent are updated appropriately).
            interface = LocalInterface()
            # 3) Record initialized constant values
            has_constant_value = False
            allocatable = False
            # 4) Access-specification - this var is only set if the declaration
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
                                    "Could not process {0}. The 'SAVE' "
                                    "attribute is not yet supported when it is"
                                    " not part of a module, submodule or main_"
                                    "program specification part.".
                                    format(decl.items))

                        elif normalized_string == "parameter":
                            # Flag the existence of a constant value in the RHS
                            has_constant_value = True
                        elif normalized_string == "allocatable":
                            allocatable = True
                        else:
                            raise NotImplementedError(
                                "Could not process {0}. Unrecognised "
                                "attribute '{1}'.".format(decl.items,
                                                          str(attr)))
                    elif isinstance(attr, Fortran2003.Intent_Attr_Spec):
                        (_, intent) = attr.items
                        normalized_string = \
                            intent.string.lower().replace(' ', '')
                        if normalized_string == "in":
                            interface = ArgumentInterface(
                                ArgumentInterface.Access.READ)
                        elif normalized_string == "out":
                            interface = ArgumentInterface(
                                ArgumentInterface.Access.WRITE)
                        elif normalized_string == "inout":
                            interface = ArgumentInterface(
                                ArgumentInterface.Access.READWRITE)
                        else:
                            raise InternalError(
                                "Could not process {0}. Unexpected intent "
                                "attribute '{1}'.".format(decl.items,
                                                          str(attr)))
                    elif isinstance(attr, Fortran2003.Dimension_Attr_Spec):
                        attribute_shape = \
                            self._parse_dimensions(attr, parent.symbol_table)
                    elif isinstance(attr, Fortran2003.Access_Spec):
                        if attr.string.lower() == 'public':
                            decln_access_spec = Symbol.Visibility.PUBLIC
                        elif attr.string.lower() == 'private':
                            decln_access_spec = Symbol.Visibility.PRIVATE
                        else:
                            raise InternalError(
                                "Could not process '{0}'. Unexpected Access "
                                "Spec attribute '{1}'.".format(decl.items,
                                                               str(attr)))
                    else:
                        raise NotImplementedError(
                            "Could not process declaration: '{0}'. "
                            "Unrecognised attribute type '{1}'.".format(
                                str(decl), str(type(attr).__name__)))

            if not precision:
                precision = default_precision(data_name)

            # Parse declarations RHS and declare new symbol into the
            # parent symbol table for each entity found.
            for entity in entities.items:
                (name, array_spec, char_len, initialisation) = entity.items
                ct_expr = None

                # If the entity has an array-spec shape, it has priority.
                # Otherwise use the declaration attribute shape.
                if array_spec is not None:
                    entity_shape = \
                        self._parse_dimensions(array_spec, parent.symbol_table)
                else:
                    entity_shape = attribute_shape

                if allocatable and not entity_shape:
                    # We have an allocatable attribute on something that we
                    # don't recognise as an array - this is not supported.
                    raise NotImplementedError(
                        "Could not process {0}. The 'allocatable' attribute is"
                        " only supported on array declarations.".format(
                            str(decl)))

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
                            "Invalid Fortran: '{0}'. An array with defined "
                            "extent cannot have the ALLOCATABLE attribute.".
                            format(str(decl)))

                if initialisation:
                    if has_constant_value:
                        # If it is a parameter parse its initialization into
                        # a dummy Assignment inside a Schedule which temporally
                        # hijacks the parent's node symbol table
                        tmp_sch = Schedule(symbol_table=parent.symbol_table)
                        dummynode = Assignment(parent=tmp_sch)
                        tmp_sch.addchild(dummynode)
                        expr = initialisation.items[1]
                        self.process_nodes(parent=dummynode, nodes=[expr])
                        ct_expr = dummynode.children[0]
                    else:
                        raise NotImplementedError(
                            "Could not process {0}. Initialisations on the"
                            " declaration statements are only supported for "
                            "parameter declarations.".format(decl.items))

                if char_len is not None:
                    raise NotImplementedError(
                        "Could not process {0}. Character length "
                        "specifications are not supported."
                        "".format(decl.items))

                sym_name = str(name).lower()

                if decln_access_spec is None:
                    # There was no access-spec on the LHS of the decln
                    if sym_name in explicit_private_symbols:
                        visibility = Symbol.Visibility.PRIVATE
                    elif sym_name in explicit_public_symbols:
                        visibility = Symbol.Visibility.PUBLIC
                    else:
                        visibility = default_visibility
                else:
                    visibility = decln_access_spec

                if entity_shape:
                    # array
                    datatype = ArrayType(ScalarType(data_name, precision),
                                         entity_shape)
                else:
                    # scalar
                    datatype = ScalarType(data_name, precision)

                if sym_name not in parent.symbol_table:
                    parent.symbol_table.add(DataSymbol(sym_name, datatype,
                                                       visibility=visibility,
                                                       constant_value=ct_expr,
                                                       interface=interface))
                else:
                    # The symbol table already contains an entry with this name
                    # so update its interface information.
                    sym = parent.symbol_table.lookup(sym_name)
                    if not sym.unresolved_interface:
                        raise SymbolError(
                            "Symbol '{0}' already present in SymbolTable with "
                            "a defined interface ({1}).".format(
                                sym_name, str(sym.interface)))
                    sym.interface = interface

        # Check for symbols named in an access statement but not explicitly
        # declared. These must then refer to symbols that have been brought
        # into scope by an unqualified use statement. As we have no idea
        # whether they represent data or a routine we use the Symbol base
        # class.
        for name in (list(explicit_public_symbols) +
                     list(explicit_private_symbols)):
            if name not in parent.symbol_table:
                if name in explicit_public_symbols:
                    vis = Symbol.Visibility.PUBLIC
                else:
                    vis = Symbol.Visibility.PRIVATE
                # TODO 736 Ideally we would use parent.find_or_create_symbol()
                # here since that checks that there is a possible source for
                # this previously-unseen symbol. However, we cannot yet do this
                # because we don't capture symbols for routine names so
                # that, e.g.:
                #   module my_mod
                #     public my_routine
                #   contains
                #     subroutine my_routine()
                # would cause us to raise an exception.
                parent.symbol_table.add(Symbol(name, visibility=vis))

        try:
            arg_symbols = []
            # Ensure each associated symbol has the correct interface info.
            for arg_name in [x.string.lower() for x in arg_list]:
                symbol = parent.symbol_table.lookup(arg_name)
                if symbol.is_local:
                    # We didn't previously know that this Symbol was an
                    # argument (as it had no 'intent' qualifier). Mark
                    # that it is an argument by specifying its interface.
                    # A Fortran argument has intent(inout) by default
                    symbol.interface = ArgumentInterface(
                        ArgumentInterface.Access.READWRITE)
                arg_symbols.append(symbol)
            # Now that we've updated the Symbols themselves, set the
            # argument list
            parent.symbol_table.specify_argument_list(arg_symbols)
        except KeyError:
            raise InternalError("The kernel argument "
                                "list '{0}' does not match the variable "
                                "declarations for fparser nodes {1}."
                                "".format(str(arg_list), nodes))

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
                    array_name = fn_name
                    array_subscript = arg_list.items

                    assignment_rhs = scalar_expr

                    # Create assingment node
                    assignment = Assignment(parent=parent)
                    parent.addchild(assignment)

                    # Build lhs
                    lhs = Array(symbol, parent=assignment)
                    self.process_nodes(parent=lhs, nodes=array_subscript)
                    assignment.addchild(lhs)

                    # Build rhs
                    self.process_nodes(parent=assignment,
                                       nodes=[assignment_rhs])
                else:
                    raise InternalError(
                        "Could not process '{0}'. Symbol '{1}' is in the"
                        " SymbolTable but it is not an array as expected, so"
                        " it can not be recovered as an array assignment."
                        "".format(str(stmtfn), symbol.name))
            except KeyError:
                raise NotImplementedError(
                    "Could not process '{0}'. Statement Function declarations "
                    "are not supported.".format(str(stmtfn)))

    @staticmethod
    def _process_kind_selector(type_spec, psyir_parent):
        '''Processes the fparser2 parse tree of the type specification of a
        variable declaration in order to extract KIND information. This
        information is used to determine the precision of the variable (as
        supplied to the DataSymbol constructor).

        :param type_spec: the fparser2 parse tree of the type specification.
        :type type_spec: :py:class:`fparser.two.Fortran2003.Intrinsic_Type_Spec
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
        symbol_table = _get_symbol_table(psyir_parent)

        if not isinstance(type_spec.items[1], Fortran2003.Kind_Selector):
            return None
        # The KIND() intrinsic itself is Fortran specific and has no direct
        # representation in the PSyIR. We therefore can't use
        # self.process_nodes() here.
        kind_selector = type_spec.items[1]
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
                "Only real and integer literals are supported "
                "as arguments to the KIND intrinsic but found '{0}' in: "
                "{1}".format(type(kind_arg).__name__, str(kind_selector)))

        # We have kind=kind-param
        kind_names = walk(kind_selector.items, Fortran2003.Name)
        if not kind_names:
            raise NotImplementedError(
                "Failed to find valid Name in Fortran Kind "
                "Selector: '{0}'".format(str(kind_selector)))
        return Fparser2Reader._kind_symbol_from_name(str(kind_names[0]),
                                                     symbol_table)

    @staticmethod
    def _kind_symbol_from_name(name, symbol_table):
        '''
        Utility method that returns a Symbol representing the named KIND
        parameter. If the supplied Symbol Table does not contain an appropriate
        entry then one is created. If it does contain a matching entry then
        its datatype must be 'integer' or 'deferred'. If the latter then the
        fact that we now know that this Symbol represents a KIND parameter
        means that we can change the datatype to be 'integer'.

        :param str name: the name of the variable holding the KIND value.
        :param symbol_table: the Symbol Table associated with the code being\
                             processed.
        :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable`

        :returns: the Symbol representing the KIND parameter.
        :rtype: :py:class:`psyclone.psyir.symbols.DataSymbol`

        :raises TypeError: if the Symbol Table already contains an entry for \
                       `name` and its datatype is not 'integer' or 'deferred'.
        '''
        lower_name = name.lower()
        try:
            kind_symbol = symbol_table.lookup(lower_name)
            if not (isinstance(kind_symbol.datatype, DeferredType) or
                    (isinstance(kind_symbol.datatype, ScalarType) and
                     kind_symbol.datatype.intrinsic ==
                     ScalarType.Intrinsic.INTEGER)):
                raise TypeError(
                    "SymbolTable already contains an entry for "
                    "variable '{0}' used as a kind parameter but it "
                    "is not a 'deferred' or 'scalar integer' type.".
                    format(lower_name))
            # A KIND parameter must be of type integer so set it here
            # (in case it was previously 'deferred'). We don't know
            # what precision this is so set it to the default.
            kind_symbol.datatype = default_integer_type()
        except KeyError:
            # The SymbolTable does not contain an entry for this kind parameter
            # so create one. We specify an UnresolvedInterface as we don't
            # currently know how this symbol is brought into scope.
            kind_symbol = DataSymbol(lower_name, default_integer_type(),
                                     interface=UnresolvedInterface())
            symbol_table.add(kind_symbol)
        return kind_symbol

    def process_nodes(self, parent, nodes):
        '''
        Create the PSyIR of the supplied list of nodes in the
        fparser2 AST. Currently also inserts parent information back
        into the fparser2 AST. This is a workaround until fparser2
        itself generates and stores this information.

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
        :raises NotImplementedError: There isn't a handler for the provided \
                child type.
        :returns: Returns the PSyIR representation of child, which can be a \
                  single node, a tree of nodes or None if the child can be \
                  ignored.
        :rtype: :py:class:`psyclone.psyir.nodes.Node` or NoneType
        '''
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

    def _create_loop(self, parent, variable_name):
        '''
        Create a Loop instance. This is done outside _do_construct_handler
        because some APIs may want to instantiate a specialised Loop.

        :param parent: the parent of the node.
        :type parent: :py:class:`psyclone.psyir.nodes.Node`
        :param str variable_name: name of the iteration variable.

        :return: a new Loop instance.
        :rtype: :py:class:`psyclone.psyir.nodes.Loop`

        '''
        return Loop(parent=parent, variable_name=variable_name)

    def _process_loopbody(self, loop_body, node):
        ''' Process the loop body. This is done outside _do_construct_handler
        because some APIs may want to perform specialised actions. By default
        continue processing the tree nodes inside the loop body.

        :param loop_body: Schedule representing the body of the loop.
        :type loop_body: :py:class:`psyclone.psyir.nodes.Schedule`
        :param node: fparser loop node being processed.
        :type node: \
            :py:class:`fparser.two.Fortran2003.Block_Nonlabel_Do_Construct`
        '''
        # Process loop body (ignore 'do' and 'end do' statements with [1:-1])
        self.process_nodes(parent=loop_body, nodes=node.content[1:-1])

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

        :raises InternalError: if the fparser2 tree has an unexpected \
            structure.
        '''
        ctrl = walk(node.content, Fortran2003.Loop_Control)
        if not ctrl:
            raise InternalError(
                "Unrecognised form of DO loop - failed to find Loop_Control "
                "element in the node '{0}'.".format(str(node)))
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
        loop = self._create_loop(parent, variable_name)
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
            default_step = Literal("1", default_integer_type(), parent=loop)
            loop.addchild(default_step)

        # Create Loop body Schedule
        loop_body = Schedule(parent=loop)
        loop_body._ast = node
        loop.addchild(loop_body)
        self._process_loopbody(loop_body, node)

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
                "Failed to find opening if then statement in: "
                "{0}".format(str(node)))
        if not isinstance(node.content[-1], Fortran2003.End_If_Stmt):
            raise InternalError(
                "Failed to find closing end if statement in: "
                "{0}".format(str(node)))

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
                ifbody = Schedule(parent=ifblock)
                ifbody.ast = node.content[start_idx + 1]
                ifbody.ast_end = node.content[end_idx - 1]
                newifblock.addchild(ifbody)
                self.process_nodes(parent=ifbody,
                                   nodes=node.content[start_idx + 1:end_idx])

                currentparent = newifblock

            elif isinstance(clause, Fortran2003.Else_Stmt):
                if not idx == num_clauses - 1:
                    raise InternalError(
                        "Else clause should only be found next to last "
                        "clause, but found {0}".format(node.content))
                elsebody = Schedule(parent=currentparent)
                currentparent.addchild(elsebody)
                elsebody.ast = node.content[start_idx]
                elsebody.ast_end = node.content[end_idx]
                self.process_nodes(parent=elsebody,
                                   nodes=node.content[start_idx + 1:end_idx])
            else:
                raise InternalError(
                    "Only fparser2 If_Then_Stmt, Else_If_Stmt and Else_Stmt "
                    "are expected, but found {0}.".format(clause))

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
                "Failed to find opening case statement in: "
                "{0}".format(str(node)))
        if not isinstance(node.content[-1], Fortran2003.End_Select_Stmt):
            raise InternalError(
                "Failed to find closing case statement in: "
                "{0}".format(str(node)))

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
                        "Unexpected parse tree structure. Expected child of "
                        "Case_Stmt to be a Case_Selector but got: '{0}'".
                        format(type(child.items[0]).__name__))
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

            ifblock = IfBlock(parent=currentparent,
                              annotations=['was_case'])
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

            if rootif:
                # If rootif is already initialised we chain the new
                # case in the last else branch.
                elsebody = Schedule(parent=currentparent)
                currentparent.addchild(elsebody)
                elsebody.addchild(ifblock)
                ifblock.parent = elsebody
                elsebody.ast = node.content[start_idx + 1]
                elsebody.ast_end = node.content[end_idx - 1]
            else:
                rootif = ifblock

            currentparent = ifblock

        if default_clause_idx:
            # Finally, add the content of the 'default' clause as a last
            # 'else' clause.
            elsebody = Schedule(parent=currentparent)
            start_idx = default_clause_idx
            # Find the next 'case' clause that occurs after 'case default'
            # (if any)
            end_idx = -1
            for idx in clause_indices:
                if idx > default_clause_idx:
                    end_idx = idx
                    break
            self.process_nodes(parent=elsebody,
                               nodes=node.content[start_idx + 1:
                                                  end_idx])
            currentparent.addchild(elsebody)
            elsebody.ast = node.content[start_idx + 1]
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
    def _array_notation_rank(array):
        '''Check that the supplied candidate array reference uses supported
        array notation syntax and return the rank of the sub-section
        of the array that uses array notation. e.g. for a reference
        "a(:, 2, :)" the rank of the sub-section is 2.

        :param array: the array reference to check.
        :type array: :py:class:`psyclone.psyir.nodes.Array`

        :returns: rank of the sub-section of the array.
        :rtype: int

        :raises NotImplementedError: if the array node does not have any \
                                     children.

        '''
        if not array.children:
            raise NotImplementedError("An Array reference in the PSyIR must "
                                      "have at least one child but '{0}' has "
                                      "none".format(array.name))
        # Only array refs using basic colon syntax are currently
        # supported e.g. (a(:,:)).  Each colon is represented in the
        # PSyIR as a Range node with first argument being an lbound
        # binary operator, the second argument being a ubound operator
        # and the third argument being an integer Literal node with
        # value 1 i.e. a(:,:) is represented as
        # a(lbound(a,1):ubound(a,1):1,lbound(a,2):ubound(a,2):1) in
        # the PSyIR.
        num_colons = 0
        for node in array.children:
            if isinstance(node, Range):
                # Found array syntax notation. Check that it is the
                # simple ":" format.
                if not _is_range_full_extent(node):
                    raise NotImplementedError(
                        "Only array notation of the form my_array(:, :, ...) "
                        "is supported.")
                num_colons += 1
        return num_colons

    def _array_syntax_to_indexed(self, parent, loop_vars):
        '''
        Utility function that modifies each Array object in the supplied PSyIR
        fragment so that they are indexed using the supplied loop variables
        rather than having colon array notation.

        :param parent: root of PSyIR sub-tree to search for Array \
                       references to modify.
        :type parent:  :py:class:`psyclone.psyir.nodes.Node`
        :param loop_vars: the variable names for the array indices.
        :type loop_vars: list of str

        :raises NotImplementedError: if array sections of differing ranks are \
                                     found.
        '''
        assigns = parent.walk(Assignment)
        # Check that the LHS of any assignment uses recognised array
        # notation.
        for assign in assigns:
            _ = self._array_notation_rank(assign.lhs)
        # TODO #717 if the supplied code accidentally omits array
        # notation for an array reference on the RHS then we will
        # identify it as a scalar and the code produced from the
        # PSyIR (using e.g. the Fortran backend) will not
        # compile. We need to implement robust identification of the
        # types of all symbols in the PSyIR fragment.
        arrays = parent.walk(Array)
        first_rank = None
        for array in arrays:
            # Check that this is a supported array reference and that
            # all arrays are of the same rank
            rank = len([child for child in array.children if
                        isinstance(child, Range)])
            if first_rank:
                if rank != first_rank:
                    raise NotImplementedError(
                        "Found array sections of differing ranks within a "
                        "WHERE construct: array section of {0} has rank {1}".
                        format(array.name, rank))
            else:
                first_rank = rank

            # Replace the PSyIR Ranges with the loop variables
            range_idx = 0
            for idx, child in enumerate(array.children):
                if isinstance(child, Range):
                    symbol = array.find_or_create_symbol(loop_vars[range_idx])
                    array.children[idx] = Reference(symbol, parent=array)
                    range_idx += 1

    def _where_construct_handler(self, node, parent):
        '''
        Construct the canonical PSyIR representation of a WHERE construct or
        statement. A construct has the form:

            WHERE(logical-mask)
              statements
            [ELSE WHERE(logical-mask)
              statements]
            [ELSE
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

        '''
        if isinstance(node, Fortran2003.Where_Stmt):
            # We have a Where statement. Check that the parse tree has the
            # expected structure.
            if not len(node.items) == 2:
                raise InternalError(
                    "Expected a Fortran2003.Where_Stmt to have exactly two "
                    "entries in 'items' but found {0}: {1}".format(
                        len(node.items), str(node.items)))
            if not isinstance(node.items[1], Fortran2003.Assignment_Stmt):
                raise InternalError(
                    "Expected the second entry of a Fortran2003.Where_Stmt "
                    "items tuple to be an Assignment_Stmt but found: {0}".
                    format(type(node.items[1]).__name__))
            was_single_stmt = True
            annotations = ["was_where", "was_single_stmt"]
            logical_expr = [node.items[0]]
        else:
            # We have a Where construct. Check that the first and last
            # children are what we expect.
            if not isinstance(node.content[0],
                              Fortran2003.Where_Construct_Stmt):
                raise InternalError("Failed to find opening where construct "
                                    "statement in: {0}".format(str(node)))
            if not isinstance(node.content[-1], Fortran2003.End_Where_Stmt):
                raise InternalError("Failed to find closing end where "
                                    "statement in: {0}".format(str(node)))
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
        arrays = fake_parent.walk(Array)
        if not arrays:
            # If the PSyIR doesn't contain any Arrays then that must be
            # because the code doesn't use explicit array syntax. At least one
            # variable in the logical-array expression must be an array for
            # this to be a valid WHERE().
            # TODO #717. Look-up the shape of the array in the SymbolTable.
            raise NotImplementedError("Only WHERE constructs using explicit "
                                      "array notation (e.g. my_array(:, :)) "
                                      "are supported.")
        # All array sections in a Fortran WHERE must have the same rank so
        # just look at the first array.
        rank = self._array_notation_rank(arrays[0])
        # Create a list to hold the names of the loop variables as we'll
        # need them to index into the arrays.
        loop_vars = rank*[""]

        # Since we don't have support for searching a hierarchy of symbol
        # tables (#630), for now we simply add new symbols to the highest-level
        # symbol table.
        symbol_table = parent.root.symbol_table

        # Create a set of all of the symbol names in the fparser2 parse
        # tree so that we can find any clashes. We go as far back up the tree
        # as we can before searching for all instances of Fortran2003.Name.
        # We can't just rely on looking in our symbol tables because there
        # may be CodeBlocks that access symbols that are e.g. imported from
        # modules without being explicitly named.
        name_list = walk(node.get_root(), Fortran2003.Name)
        for name_obj in name_list:
            name = str(name_obj)
            if name not in symbol_table:
                # TODO #630 some of these symbols will be put in the wrong
                # symbol table. We need support for creating a unique symbol
                # within a hierarchy of symbol tables. However, until we are
                # generating code from the PSyIR Fortran backend
                # (#435) this doesn't matter.
                symbol_table.add(Symbol(name))

        integer_type = default_integer_type()
        # Now create a loop nest of depth `rank`
        new_parent = parent
        for idx in range(rank, 0, -1):

            # TODO #630 this creation of a new symbol really needs to account
            # for all of the symbols in the hierarchy of symbol tables. Since
            # we don't yet have that functionality we just add everything to
            # the top-level symbol table.
            loop_vars[idx-1] = symbol_table.new_symbol_name(
                "widx{0}".format(idx))

            symbol_table.add(DataSymbol(loop_vars[idx-1], integer_type))

            loop = Loop(parent=new_parent, variable_name=loop_vars[idx-1],
                        annotations=annotations)
            # Point to the original WHERE statement in the parse tree.
            loop.ast = node
            # Add loop lower bound
            loop.addchild(Literal("1", integer_type, parent=loop))
            # Add loop upper bound - we use the SIZE operator to query the
            # extent of the current array dimension
            size_node = BinaryOperation(BinaryOperation.Operator.SIZE,
                                        parent=loop)
            loop.addchild(size_node)
            symbol = size_node.find_or_create_symbol(arrays[0].name)

            size_node.addchild(Reference(symbol, parent=size_node))
            size_node.addchild(Literal(str(idx), integer_type,
                                       parent=size_node))
            # Add loop increment
            loop.addchild(Literal("1", integer_type, parent=loop))
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
        new_parent.addchild(ifblock)
        ifblock.ast = node  # Point back to the original WHERE construct

        # We construct the conditional expression from the original
        # logical-array-expression of the WHERE. We process_nodes() a
        # second time here now that we have the correct parent node in the
        # PSyIR (and thus a SymbolTable) to refer to.
        self.process_nodes(ifblock, logical_expr)

        # Each array reference must now be indexed by the loop variables
        # of the loops we've just created.
        self._array_syntax_to_indexed(ifblock.children[0], loop_vars)

        # Now construct the body of the IF using the body of the WHERE
        sched = Schedule(parent=ifblock)
        ifblock.addchild(sched)

        if not was_single_stmt:
            # Do we have an ELSE WHERE?
            for idx, child in enumerate(node.content):
                if isinstance(child, Fortran2003.Elsewhere_Stmt):
                    self.process_nodes(sched, node.content[1:idx])
                    self._array_syntax_to_indexed(sched, loop_vars)
                    # Add an else clause to the IF block for the ELSEWHERE
                    # clause
                    sched = Schedule(parent=ifblock)
                    ifblock.addchild(sched)
                    self.process_nodes(sched, node.content[idx+1:-1])
                    break
            else:
                # No elsewhere clause was found
                self.process_nodes(sched, node.content[1:-1])
        else:
            # We only had a single-statement WHERE
            self.process_nodes(sched, node.items[1:])
        # Convert all uses of array syntax to indexed accesses
        self._array_syntax_to_indexed(sched, loop_vars)
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
                    "Operation '{0}' has more than one argument and is "
                    "therefore not unary!".format(str(node)))
            node_list = node.items[1].items
        else:
            node_list = [node.items[1]]
        unary_op = UnaryOperation(operator, parent=parent)
        self.process_nodes(parent=unary_op, nodes=node_list)

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
                    "Unexpected fparser parse tree for binary intrinsic "
                    "operation '{0}'. Expected second child to be "
                    "Actual_Arg_Spec_List but got '{1}'.".format(
                        str(node), type(node.items[1])))
            arg_nodes = node.items[1].items
            if len(arg_nodes) != 2:
                raise InternalError(
                    "Binary operator should have exactly two arguments but "
                    "found {0} for '{1}'.".format(len(arg_nodes), str(node)))
        else:
            operator_str = node.items[1].lower()
            arg_nodes = [node.items[0], node.items[2]]

        try:
            operator = Fparser2Reader.binary_operators[operator_str]
        except KeyError:
            # Operator not supported, it will produce a CodeBlock instead
            raise NotImplementedError(operator_str)

        binary_op = BinaryOperation(operator, parent=parent)
        self.process_nodes(parent=binary_op, nodes=[arg_nodes[0]])
        self.process_nodes(parent=binary_op, nodes=[arg_nodes[1]])

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
                "Expected second 'item' of N-ary intrinsic '{0}' in fparser "
                "parse tree to be an Actual_Arg_Spec_List but found '{1}'.".
                format(str(node), type(node.items[1])))
        if len(node.items[1].items) < 3:
            raise InternalError(
                "An N-ary operation must have more than two arguments but "
                "found {0} for '{1}'.".format(len(node.items[1].items),
                                              str(node)))

        # node.items[1] is a Fortran2003.Actual_Arg_Spec_List so we have
        # to process the `items` of that...
        self.process_nodes(parent=nary_op, nodes=list(node.items[1].items))
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
        symbol = parent.find_or_create_symbol(node.string)
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
        :rtype: :py:class:`psyclone.psyir.nodes.Array`

        '''
        reference_name = node.items[0].string.lower()
        symbol = parent.find_or_create_symbol(reference_name)

        array = Array(symbol, parent)
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

        '''
        # The PSyIR stores array dimension information for the Array
        # class in an ordered list. As we are processing the
        # dimensions in order, the number of children already added to
        # our parent indicates the current array dimension being
        # processed (with 0 being the first dimension, 1 being the
        # second etc). Fortran specifies the 1st dimension as being 1,
        # the second dimension being 2, etc.). We therefore add 1 to
        # the number of children added to out parent to determine the
        # Fortran dimension value.
        integer_type = default_integer_type()
        dimension = str(len(parent.children)+1)
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
                BinaryOperation.Operator.LBOUND, Reference(parent.symbol),
                Literal(dimension, integer_type))
            lbound.parent = my_range
            my_range.children.append(lbound)
        if node.children[1]:
            self.process_nodes(parent=my_range, nodes=[node.children[1]])
        else:
            # There is no upper bound, it is implied. This is not
            # supported in the PSyIR so we create the equivalent code
            # by using the PSyIR ubound function:
            # a(...:) becomes a(...:ubound(a,1))
            ubound = BinaryOperation.create(
                BinaryOperation.Operator.UBOUND, Reference(parent.symbol),
                Literal(dimension, integer_type))
            ubound.parent = my_range
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
            literal.parent = my_range
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
            return Literal(str(node.items[0]), integer_type, parent=parent)
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
            return Literal(value, real_type, parent=parent)
        # Unrecognised datatype - will result in a CodeBlock
        raise NotImplementedError()

    def _char_literal_handler(self, node, parent):
        '''
        Transforms an fparser2 character literal into a PSyIR literal.

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
        return Literal(str(node.items[0]), character_type, parent=parent)

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
            return Literal("true", boolean_type, parent=parent)
        if value == ".false.":
            return Literal("false", boolean_type, parent=parent)
        raise GenerationError(
            "Expected to find '.true.' or '.false.' as fparser2 logical "
            "literal, but found '{0}' instead.".format(value))
