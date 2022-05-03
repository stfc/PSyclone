# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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
# Author S. Siso and N. Nobre, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module contains the CommentableMixin implementation.'''

import six


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
        '''
        if not isinstance(comment, six.string_types):
            raise TypeError(f"The preceding_comment must be a string but"
                            f" found '{type(comment).__name__}'.")
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
        '''
        if not isinstance(comment, six.string_types):
            raise TypeError(f"The inline_comment must be a string but"
                            f" found '{type(comment).__name__}'.")
        self._inline_comment = comment


# For automatic API documentation generation
__all__ = ["CommentableMixin"]
