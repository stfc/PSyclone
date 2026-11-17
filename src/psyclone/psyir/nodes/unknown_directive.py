# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2025-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' This module contains the UnknownDirective node implementation.'''

from psyclone.psyir.nodes.directive import StandaloneDirective


class UnknownDirective(StandaloneDirective):
    '''
    Directive representing PSyclone-specific directives in the tree.

    :param directive_string: The content after the sentinel part of this
        directive (e.g. !$CONTENT).
    :param sentinel_infix_string: The content inside the sentinel part of this
        directive (e.g. !CONTENT$).
    :param kwargs: additional keyword arguments provided to the PSyIR node.

    :raises TypeError: if any of the provided strings are not a str.

    '''

    _children_valid_format = "<LeafNode>"

    def __init__(
        self,
        directive_string: str = "",
        sentinel_infix_string: str = "",
        **kwargs
    ):
        super().__init__(**kwargs)
        if not isinstance(directive_string, str):
            raise TypeError(
                f"'directive_string' must be a 'str' but found "
                f"'{type(directive_string).__name__}'")
        if not isinstance(sentinel_infix_string, str):
            raise TypeError(
                f"'sentinel_infix_string' must be a 'str' but found "
                f"'{type(sentinel_infix_string).__name__}'")
        self._directive_string = directive_string
        self._sentinel_infix_string = sentinel_infix_string

    @staticmethod
    def _validate_child(position, child):
        '''
        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        return False

    @property
    def directive_string(self) -> str:
        '''
        :returns: The content of this UnknownDirective node.
        '''
        return self._directive_string

    @property
    def sentinel_infix_string(self) -> str:
        '''
        :returns: The content inside the directive sentinel of this
            UnknownDirective node (e.g. in Fortran this is the string
            between the ! and $ such as in !DIR$ or !GCC$).
        '''
        return self._sentinel_infix_string
