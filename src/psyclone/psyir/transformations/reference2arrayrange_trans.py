# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2025, Science and Technology Facilities Council.
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
# Author: R. W. Ford, STFC Daresbury Lab
# Modified: S. Siso, STFC Daresbury Lab

'''Module providing a transformation from a reference to an Array (a = ...)
   to an ArrayReference with one or more array ranges (a(:) = ...). This can
   be useful to determine when we have array accesses (as it is not clear when
   there is a reference to an Array) and can allow further optimisations such
   as transforming to explicit loops.

'''
from psyclone.errors import LazyString
from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import (ArrayReference, Assignment, Call,
                                  IntrinsicCall, Literal, Range, Reference)
from psyclone.psyir.symbols import INTEGER_TYPE, ArrayType, Symbol
from psyclone.psyir.transformations.transformation_error import (
    TransformationError)
from psyclone.utils import transformation_documentation_wrapper


@transformation_documentation_wrapper
class Reference2ArrayRangeTrans(Transformation):
    '''Provides a transformation from PSyIR Array Notation (a reference to
    an Array) to a PSyIR Range. For example:

    >>> from psyclone.psyir.backend.fortran import FortranWriter
    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.nodes import Reference
    >>> from psyclone.psyir.transformations import TransformationError
    >>> CODE = ("program example\\n"
    ...         "real :: a(:)\\n"
    ...         "a = 0.0\\n"
    ...         "end program\\n")
    >>> trans = Reference2ArrayRangeTrans()
    >>> psyir = FortranReader().psyir_from_source(CODE)
    >>> for reference in psyir.walk(Reference):
    ...    try:
    ...        trans.apply(reference)
    ...    except TransformationError:
    ...        pass
    >>> print(FortranWriter()(psyir))
    program example
      real, dimension(:) :: a
    <BLANKLINE>
      a(:) = 0.0
    <BLANKLINE>
    end program example
    <BLANKLINE>

    This transformation does not currently support arrays within
    structures, see issue #1858.

    '''
    @staticmethod
    def _get_array_bound(symbol, index):
        '''A utility function that returns the appropriate loop bounds (lower,
        upper and step) for an array dimension.  If the array
        dimension is declared with known bounds (an integer or a
        symbol) then these bound values are used. If the size is
        unknown (a deferred or attribute type) then the LBOUND and
        UBOUND PSyIR nodes are used.

        :param symbol: the symbol that we are interested in.
        :type symbol: :py:class:`psyir.symbols.DataSymbol`
        :param int index: the (array) reference index that we are \
            interested in.

        :returns: the loop bounds for this array index.
        :rtype: Tuple(:py:class:`psyclone.psyir.nodes.Literal`, \
                      :py:class:`psyclone.psyir.nodes.Literal`, \
                      :py:class:`psyclone.psyir.nodes.Literal`) or \
                Tuple(:py:class:`psyclone.psyir.nodes.BinaryOperation`, \
                      :py:class:`psyclone.psyir.nodes.BinaryOperation`, \
                      :py:class:`psyclone.psyir.nodes.Literal`)

        '''
        # Look for explicit bounds in the array declaration.
        my_dim = symbol.shape[index]
        if isinstance(my_dim, ArrayType.ArrayBounds):
            lower_bound = my_dim.lower.copy()
            upper_bound = my_dim.upper.copy()
            step = Literal("1", INTEGER_TYPE)
            return (lower_bound, upper_bound, step)

        # No explicit array bound information could be found so use the
        # LBOUND and UBOUND intrinsics.
        lower_bound = IntrinsicCall.create(
            IntrinsicCall.Intrinsic.LBOUND,
            [Reference(symbol), ("dim", Literal(str(index+1), INTEGER_TYPE))])
        upper_bound = IntrinsicCall.create(
            IntrinsicCall.Intrinsic.UBOUND,
            [Reference(symbol), ("dim", Literal(str(index+1), INTEGER_TYPE))])
        step = Literal("1", INTEGER_TYPE)
        return (lower_bound, upper_bound, step)

    def validate(self, node, **kwargs):
        '''Check that the node is a Reference node and that the symbol it
        references is an array.

        :param node: a Reference node.
        :type node: :py:class:`psyclone.psyir.nodes.Reference`
        :param allow_call_arguments: by default, any references that may be
            arguments to non-elemental routines are not transformed. However,
            this transformation is sometimes used in other transformations
            where this restriction does not apply.

        :raises TransformationError: if the node is not a Reference
            node or the Reference node not does not reference an array
            symbol.
        :raises TransformationError: if the Reference node is (or may be)
            passed as an argument to a call that is not elemental and
            `allow_call_arguments` is False.

        '''
        self.validate_options(**kwargs)
        allow_call_arguments = self.get_option("allow_call_arguments",
                                               **kwargs)
        # TODO issue #1858. Add support for structures containing arrays.
        # pylint: disable=unidiomatic-typecheck
        if not type(node) is Reference:
            raise TransformationError(
                f"The supplied node should be a Reference but found "
                f"'{type(node).__name__}'.")
        if not node.symbol.is_array:
            raise TransformationError(
                f"The supplied node should be a Reference to a symbol "
                f"that is an array, but '{node.symbol.name}' is not.")
        if not allow_call_arguments and (isinstance(node.parent, Call) and
                                         not node.parent.is_elemental):
            raise TransformationError(LazyString(
                lambda: f"The supplied node is passed as an argument to a "
                f"Call to a non-elemental routine ("
                f"{node.parent.debug_string().strip()}) and should not be "
                f"transformed."))
        if (isinstance(node.parent, Reference) and (
                type(node.parent.symbol) is Symbol
                or not isinstance(node.parent.symbol.datatype, ArrayType))):
            raise TransformationError(LazyString(
                lambda: f"References to arrays that *may* be routine arguments"
                f" should not be transformed but found:\n "
                f"{node.parent.debug_string()} and {node.parent.symbol.name} "
                f"is not known to be of ArrayType (and therefore may be a "
                f"call)."))
        assignment = node.ancestor(Assignment)
        if assignment and assignment.is_pointer:
            raise TransformationError(
                f"'{type(self).__name__}' can not be applied to references"
                f" inside pointer assignments, but found '{node.name}' in"
                f" {assignment.debug_string()}")

    def apply(self, node, allow_call_arguments: bool = False, **kwargs):
        '''Apply the Reference2ArrayRangeTrans transformation to the specified
        node. The node must be a Reference to an array. The Reference
        is replaced by an ArrayReference with appropriate explicit
        range nodes (termed colon notation in Fortran).

        :param node: a Reference node.
        :type node: :py:class:`psyclone.psyir.nodes.Reference`
        :param allow_call_arguments: by default, any references that may be
            arguments to non-elemental routines are not transformed. However,
            this transformation is sometimes used in other transformations
            where this restriction does not apply.

        '''
        self.validate(node, allow_call_arguments=allow_call_arguments)

        symbol = node.symbol
        indices = []
        for idx, _ in enumerate(symbol.shape):
            lbound, ubound = symbol.get_bounds(idx)
            indices.append(Range.create(lbound, ubound,
                                        Literal("1", INTEGER_TYPE)))
        array_ref = ArrayReference.create(symbol, indices)
        node.replace_with(array_ref)
