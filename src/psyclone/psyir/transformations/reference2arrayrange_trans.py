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
from psyclone.psyir.nodes import (
    ArrayReference, Call, Reference, Member)
from psyclone.psyir.nodes.structure_accessor_mixin import (
    StructureAccessorMixin)
from psyclone.psyir.nodes.array_mixin import ArrayMixin
from psyclone.psyir.symbols import (
    DataSymbol, UnresolvedType, UnsupportedType, DataTypeSymbol,
    ArrayType, StructureType)
from psyclone.psyir.transformations.transformation_error import (
    TransformationError)
from psyclone.utils import transformation_documentation_wrapper


@transformation_documentation_wrapper
class Reference2ArrayRangeTrans(Transformation):
    '''
    Transformation to convert plain References of array symbols to
    ArrayReferences with full-extent ranges if it is semantically equivalent
    to do so (e.g. it won't convert call arguments because it would change the
    bounds values).

    Note that if the provided node does not need to be modified is provided (
    e.g. a Reference to a scalar or an ArrayReference to an array), the
    transformation will succeed. However, if we cannot guarantee the type of
    the symbol, or the validity of the transformations (e.g. it is in a call
    that we don't know if it is elemental or not), the transformation will
    fail.

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

    TODO #1858: This transformation does not currently support arrays within
    structures, which the validation will pass without an error.

    '''

    def validate(self, node, options=None, **kwargs):
        '''Check that the node is a Reference node and that we have all
        information necessary to decide if it can be expanded.

        :param node: a Reference node.
        :type node: :py:class:`psyclone.psyir.nodes.Reference`

        :raises TransformationError: if the node is not a Reference
            node or the Reference node not does not reference an array
            symbol.
        :raises TransformationError: if the Reference node is (or may be)
            passed as an argument to a call that is not elemental.
        :raises TransformationError: if provided a reference inside a
            pointer assignment.

        '''
        super().validate(node, **kwargs)
        self.validate_options(**kwargs)

        if node and node.parent and isinstance(node.parent, Call):
            if node is node.parent.routine:
                return
            if node.parent.is_elemental is None:
                raise TransformationError(LazyString(
                    lambda: f"The supplied node is passed as an argument to a "
                    f"Call that may or may not be elemental: "
                    f"'{node.parent.debug_string().strip()}'. Consider "
                    f"adding the function's filename to RESOLVE_IMPORTS."))
            if not node.parent.is_elemental:
                return

        if not isinstance(node, Reference):
            raise TransformationError(
                f"The supplied node should be a Reference but found "
                f"'{type(node).__name__}'.")

        if not isinstance(node.symbol, DataSymbol):
            raise TransformationError(
                f"The supplied node should be a Reference to a DataSymbol "
                f"but found '{node.symbol}'. Consider adding the declaration"
                f"'s filename to RESOLVE_IMPORTS.")

        cursor = node
        cursor_datatype = cursor.symbol.datatype
        while cursor:
            if isinstance(cursor_datatype, StructureType.ComponentType):
                # If it is a ComponentType, follow its declaration
                cursor_datatype = cursor_datatype.datatype

            if isinstance(cursor_datatype, DataTypeSymbol):
                # If it is a DataTypeSymbol, follow its declaration
                cursor_datatype = cursor_datatype.datatype

            # If we don't know if it is an array access (is not ArrayMixin)
            # or we recurse down (its a StructureAccessorMixin)s, we need to
            # know the exact type.
            if (
                not isinstance(cursor, ArrayMixin) or
                isinstance(cursor, StructureAccessorMixin)
            ):
                if isinstance(cursor_datatype, (UnresolvedType,
                                                UnsupportedType)):
                    # In case of a structure access, we can still guarantee
                    # it is fine without knowing the types if all the members
                    # accessors recursing down are all array accesses
                    while cursor:
                        if not isinstance(cursor, ArrayMixin):
                            break
                        if isinstance(cursor, StructureAccessorMixin):
                            cursor = cursor.member
                        else:
                            cursor = False
                    else:
                        # An 'else' in a while means that it has left
                        # without a break, in this case without finding
                        # a non-array, so the validation succeeds
                        return

                    raise TransformationError(
                        f"The supplied node should be a Reference to a symbol "
                        f"of known type, but '{node.debug_string()}' is "
                        f"'{cursor_datatype}'. Consider adding the declaration"
                        f"'s filename to RESOLVE_IMPORTS.")

            if isinstance(cursor, (StructureAccessorMixin, Member)):
                if not isinstance(cursor, ArrayMixin):
                    if isinstance(cursor_datatype, ArrayType):
                        # TODO #1858: This error can be removed when the apply
                        # has support for transforming StructureReferences to
                        # ArrayOfStuctureReferences and Members to ArrayMembers
                        raise TransformationError(
                            f"{self.name} does not support converting "
                            f"StructureReferences yet but in "
                            f"'{node.debug_string()}' '{cursor.name}'"
                            f" should be an array access."
                        )

                if isinstance(cursor, Member):
                    # Its a leaf member, finish recursion
                    break

                if isinstance(cursor_datatype, ArrayType):
                    cursor_datatype = cursor_datatype.intrinsic.datatype

                try:
                    cursor_datatype = cursor_datatype.components[
                        cursor.member.name
                    ]
                except (AttributeError, KeyError):
                    # pylint: disable=raise-missing-from
                    raise TransformationError(
                        f"{self.name} cannot validate {node.debug_string()} "
                        f"because it could not find {cursor.member.name}"
                        f"of {cursor_datatype}")

                cursor = cursor.member
            else:
                break

    def apply(self, node, options=None, **kwargs):
        '''Apply the Reference2ArrayRangeTrans transformation to the specified
        node. The node must be a Reference to an array. The Reference
        is replaced by an ArrayReference with appropriate explicit
        range nodes (termed colon notation in Fortran).

        :param node: a Reference node.
        :type node: :py:class:`psyclone.psyir.nodes.Reference`

        '''
        self.validate(node, **kwargs)

        # The following cases do not need expansions
        if node.parent and isinstance(node.parent, Call):
            if node is node.parent.routine:
                return
            if not node.parent.is_elemental:
                return

        # Recurse down the node converting References to ArrayReferences,
        # StructureReferences to ArrayOfStructuresReferences and Members to
        # ArrayMembers when the type is an ArrayType
        cursor = node
        symbol = cursor.symbol
        cursor_datatype = symbol.datatype
        while cursor:

            # If we know its an array but its not an array accessor, convert it
            if not isinstance(cursor, ArrayMixin):
                if isinstance(cursor_datatype, ArrayType):
                    array_ref = ArrayReference(symbol)
                    for idx, _ in enumerate(cursor_datatype.shape):
                        array_ref.addchild(array_ref.get_full_range(idx))
                    if node.parent:
                        node.replace_with(array_ref)
            break
