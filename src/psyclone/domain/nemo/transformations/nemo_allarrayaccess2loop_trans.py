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
# Author R. W. Ford, STFC Daresbury Lab

'''Module providing a transformation that transforms all constant
index accesses to an array (i.e. ones that do not contain loop
iterators) to single trip loops.

'''

from __future__ import absolute_import

from psyclone.domain.nemo.transformations import NemoArrayAccess2LoopTrans
from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import Assignment
from psyclone.psyir.transformations.transformation_error \
    import TransformationError


class NemoAllArrayAccess2LoopTrans(Transformation):
    '''Provides a transformation from a PSyIR Assignment containing
    constant index accesses to an array into single trip loops: For
    example:

    >>> from psyclone.domain.nemo.transformations import \\
    ...     NemoAllArrayAccess2LoopTrans
    >>> from psyclone.psyir.backend.fortran import FortranWriter
    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.nodes import Assignment
    >>> code = ("program example\\n"
    ...         "  real a(10,10), b(10,10)\\n"
    ...         "  integer :: n\\n"
    ...         "  a(1,n-1) = b(1,n-1)\\n"
    ...         "end program example\\n")
    >>> psyir = FortranReader().psyir_from_source(code)
    >>> assignment = psyir.walk(Assignment)[0]
    >>> NemoAllArrayAccess2LoopTrans().apply(assignment)
    >>> print(FortranWriter()(psyir))
    program example
      real, dimension(10,10) :: a
      real, dimension(10,10) :: b
      integer :: n
      integer :: ji
      integer :: jj
    <BLANKLINE>
      do ji = 1, 1, 1
        do jj = n - 1, n - 1, 1
          a(ji,jj) = b(ji,jj)
        enddo
      enddo
    <BLANKLINE>
    end program example
    <BLANKLINE>

    '''
    def apply(self, node, options=None):
        '''Apply the NemoAllArrayAccess2Loop transformation if the supplied
        node is an Assignment with an Array Reference on its
        left-hand-side. Each constant array index access (i.e. one not
        containing a loop iterator or a range) is then transformed
        into an iterator and the assignment placed within a single
        trip loop, subject to any constraints in the
        NemoArrayAccess2Loop transformation.

        If any of the NemoAllArrayAccess2Loop constraints are not
        satisfied for a loop index then this transformation does
        nothing for that index and silently moves to the next.

        :param node: an assignment.
        :type node: :py:class:`psyclone.psyir.nodes.Assignment`
        :param options: a dictionary with options for \
            transformations. No options are used in this \
            transformation. This is an optional argument that defaults \
            to None.
        :type options: dict of string:values or None

        '''
        self.validate(node)

        trans = NemoArrayAccess2LoopTrans()

        for index in node.lhs.children:
            try:
                trans.apply(index)
            except TransformationError:
                pass

    def validate(self, node, options=None):
        '''Perform any checks to ensure that it is valid to apply the
        NemoAllArrayAccess2LoopTrans transformation to the supplied
        PSyIR Node.

        :param node: the node that is being checked.
        :type node: :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for \
            transformations. No options are used in this \
            transformation. This is an optional argument that defaults \
            to None.
        :type options: dict of string:values or None

        '''
        # Not a PSyIR Assignment node
        if not isinstance(node, Assignment):
            raise TransformationError(
                "Error in NemoAllArrayAccess2LoopTrans transformation. The "
                "supplied node argument should be a PSyIR Assignment, "
                "but found '{0}'.".format(type(node).__name__))

    def __str__(self):
        return (
            "Convert the constant indices of a PSyIR array assignment into "
            "NemoLoops.")

    @property
    def name(self):
        '''
        :returns: the name of the transformation as a string.
        :rtype: str

        '''
        return type(self).__name__


# For automatic document generation
__all__ = [
    'NemoAllArrayAccess2LoopTrans']
