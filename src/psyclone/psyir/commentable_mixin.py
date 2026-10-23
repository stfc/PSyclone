# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' This module contains the CommentableMixin implementation.'''


class CommentableMixin:
    '''
    Mixin that adds the Commentable trait into a PSyIR node. It provides
    two attributes that store preceding and inline comments and their
    respective property getters and setters.
    '''

    # By default classes with the Commentable trait will have empty strings
    # provided by the class attributes below. Once an instance is given a
    # comment using the property setter this class attributes will be shadowed
    # by an instance attribute storing the string for that specific instance.
    _preceding_comment = ""
    _inline_comment = ""

    @property
    def preceding_comment(self):
        '''
        :returns: comment preceding this statement.
        :rtype: str
        '''
        return self._preceding_comment

    @preceding_comment.setter
    def preceding_comment(self, comment):
        '''
        :param str comment: comment preceding this statement.

        :raises TypeError: if the comment is not a string.
        '''
        if not isinstance(comment, str):
            raise TypeError(f"The preceding_comment must be a string but"
                            f" found '{type(comment).__name__}'.")
        self._preceding_comment = comment

    def append_preceding_comment(self, comment):
        '''
        :param str comment: comment to append after an newline in this
            statement-preceding comment.

        :raises TypeError: if the comment is not a string.
        '''
        if not isinstance(comment, str):
            raise TypeError(f"The preceding_comment must be a string but"
                            f" found '{type(comment).__name__}'.")
        if self._preceding_comment:
            self._preceding_comment = f"{self._preceding_comment}\n{comment}"
        else:
            self._preceding_comment = comment

    @property
    def inline_comment(self):
        '''
        :returns: inline comment associated with this statement.
        :rtype: str
        '''
        return self._inline_comment

    @inline_comment.setter
    def inline_comment(self, comment):
        '''
        :param str comment: inline comment associated with this statement.

        :raises TypeError: if the comment is not a string.
        :raises ValueError: if the comment contains a newline character.
        '''
        if not isinstance(comment, str):
            raise TypeError(f"The inline_comment must be a string but"
                            f" found '{type(comment).__name__}'.")
        if '\n' in comment:
            raise ValueError(f"The inline_comment must be a single line but "
                             f"found a newline character in '{comment}'.")
        self._inline_comment = comment


# For automatic API documentation generation
__all__ = ["CommentableMixin"]
