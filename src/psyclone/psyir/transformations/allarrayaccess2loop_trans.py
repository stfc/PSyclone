# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2024, Science and Technology Facilities Council.
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
# Authors: R. W. Ford, N. Nobre and S. Siso, STFC Daresbury Lab

'''Module providing a transformation that transforms all constant
index accesses to an array (i.e. ones that do not contain loop
iterators) to single-trip loops.

'''

from psyclone.psyir.transformations.arrayaccess2loop_trans import \
    ArrayAccess2LoopTrans
from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import Assignment
from psyclone.psyir.transformations.transformation_error \
    import TransformationError


class AllArrayAccess2LoopTrans(Transformation):
    '''Provides a transformation from a PSyIR Assignment containing
    constant index accesses to an array into single-trip loops. For
    example:

    >>> from psyclone.psyir.transformations import AllArrayAccess2LoopTrans
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
    >>> AllArrayAccess2LoopTrans().apply(assignment)
    >>> print(FortranWriter()(psyir))
    program example
      real, dimension(10,10) :: a
      real, dimension(10,10) :: b
      integer :: n
      integer :: idx
      integer :: idx_1
    <BLANKLINE>
      do idx = 1, 1, 1
        do idx_1 = n - 1, n - 1, 1
          a(idx,idx_1) = b(idx,idx_1)
        enddo
      enddo
    <BLANKLINE>
    end program example
    <BLANKLINE>

    '''
    def apply(self, node, options=None):
        '''Apply the AllArrayAccess2Loop transformation if the supplied
        node is an Assignment with an Array Reference on its
        left-hand-side. Each constant array index access (i.e. one not
        containing a loop iterator or a range) is then transformed into
        an iterator and the assignment placed within a single-trip loop,
        subject to any constraints in the ArrayAccess2Loop transformation.

        If any of the AllArrayAccess2Loop constraints are not
        satisfied for a loop index then this transformation does
        nothing for that index and silently moves to the next.

        :param node: an assignment.
        :type node: :py:class:`psyclone.psyir.nodes.Assignment`
        :param options: a dictionary with options for transformations.
            This is an optional argument that defaults to None.
        :type options: Optional[Dict[str, Any]]

        '''
        self.validate(node, options)

        trans = ArrayAccess2LoopTrans()

        for index in node.lhs.children:
            try:
                trans.apply(index)
            except TransformationError:
                pass

    def validate(self, node, options=None):
        '''Perform any checks to ensure that it is valid to apply the
        AllArrayAccess2LoopTrans transformation to the supplied
        PSyIR Node.

        :param node: the node that is being checked.
        :type node: :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
            This is an optional argument that defaults to None.
        :type options: Optional[Dict[str, Any]]

        '''
        # Not a PSyIR Assignment node
        if not isinstance(node, Assignment):
            raise TransformationError(
                f"Error in AllArrayAccess2LoopTrans transformation. The "
                f"supplied node argument should be a PSyIR Assignment, "
                f"but found '{type(node).__name__}'.")

    def __str__(self):
        return (
            "Convert the constant indices of a PSyIR array-element assignment "
            "into single-trip Loops.")

    @property
    def name(self):
        '''
        :returns: the name of the transformation as a string.
        :rtype: str

        '''
        return type(self).__name__


# For automatic document generation
__all__ = ['AllArrayAccess2LoopTrans']
