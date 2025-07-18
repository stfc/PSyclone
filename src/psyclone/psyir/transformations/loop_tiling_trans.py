# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2025, Science and Technology Facilities Council.
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
# Authors: S. Siso and N. Nobre, STFC Daresbury Lab
# Modified: M. Naylor, University of Cambridge, UK
# -----------------------------------------------------------------------------

'''This module provides LoopTilingTrans, which transforms a nested Loop
construct into a tiled implementation of the construct.'''

from psyclone.psyir.nodes import Loop, Schedule
from psyclone.psyir.transformations.chunk_loop_trans import ChunkLoopTrans
from psyclone.psyir.transformations.loop_swap_trans import LoopSwapTrans
from psyclone.psyir.transformations.loop_trans import LoopTrans
from psyclone.psyir.transformations.transformation_error import \
    TransformationError


class LoopTilingTrans(LoopTrans):
    '''
    Apply a loop tiling transformation to a loop. For example:

    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.nodes import Loop
    >>> from psyclone.psyir.transformations import LoopTilingTrans
    >>> psyir = FortranReader().psyir_from_source("""
    ... subroutine sub()
    ...     integer :: i, j, tmp(100)
    ...     do i=1, 100
    ...       do j=1, 100
    ...         tmp(i, j) = 2 * tmp(i, j)
    ...       enddo
    ...     enddo
    ... end subroutine sub""")
    >>> loop = psyir.walk(Loop)[0]
    >>> LoopTilingTrans().apply(loop, tiledims=[32, 32])

    will generate:

    .. code-block:: fortran

        subroutine sub()
            integer :: i
            integer :: j
            integer, dimension(100) :: tmp
            integer :: j_out_var
            integer :: i_out_var

            do i_out_var = 1, 100, 32
              do j_out_var = 1, 100, 32
                do i = i_out_var, MIN(i_out_var + (32 - 1), 100), 1
                  do j = j_out_var, MIN(j_out_var + (32 - 1), 100), 1
                    tmp(i, j) = 2 * tmp(i, j)
                  enddo
                enddo
              enddo
            enddo
        end subroutine sub

    '''
    def __str__(self):
        return "Tile the loop construct"

    def _sink_validate(self, loop: Loop, num_levels: int):
        '''
        Check that we can sink the outermost loop of a loop nest downwards by
        the given number of levels. This includes validating that the body of
        each loop in the loop nest (except the innermost one) is a single
        loop statement (this is checked via internal calls to LoopSwapTrans).

        :param loop: the Loop that we want to sink.
        :param num_levels: the number of levels to sink the loop by.

        :raises TransformationError: if it is not possible to sink the \
            loop by the requested number of levels.
        '''

        # Try to sink a loop by repeated swapping.
        # Do this on a copy of the loop as we are only validating here.
        loop_copy = loop.copy()
        # Make sure the copy has a parent, as required by LoopSwapTrans.
        Schedule().addchild(loop_copy)
        for _ in range(0, num_levels):
            LoopSwapTrans().apply(loop_copy)

    def _sink_apply(self, loop: Loop, num_levels: int):
        '''
        Sink the outermost loop of a loop nest downwards by the given \
        number of levels.

        :param loop: the Loop that we want to sink.
        :param num_levels: the number of levels to sink the loop by.

        :raises TransformationError: if it is not possible to sink the \
            loop by the requested number of levels.
        '''
        self._sink_validate(loop, num_levels)

        # Sink a loop by repeated swapping.
        for _ in range(0, num_levels):
            LoopSwapTrans().apply(loop)

    def validate(self, outer_loop: Loop, tiledims: list[int], **kwargs):
        '''
        Validates that the given Loop node can have a LoopTilingTrans applied.

        :param outer_loop: the loop to validate.
        :param tiledims: the dimensions of the tile.

        :raises TransformationError: if any of the tile dimensions are not \
                positive integers
        :raises TransformationError: if the transformation cannot be applied
        '''
        super().validate(outer_loop, **kwargs)
        self.validate_options(**kwargs)

        if (not (isinstance(tiledims, list) and
                 all([isinstance(n, int) and n > 0 for n in tiledims]))):
            raise TransformationError(
                f"The LoopTilingTrans tiledims argument must be a "
                f"list of positive integers but found '{tiledims}'.")
        numdims = len(tiledims)

        # Even though the loops that ultimately will be sunk are the ones
        # resulting from ChunkLoopTrans, sinking these has the same
        # validation constraints as sinking the original loops. The
        # following validations also guarantee that we have a nested loop
        # construct with numdims loops where each loop except the innermost
        # one contains exactly one child which is also a loop.
        sink_upto = numdims
        for loop in outer_loop.walk(Loop)[0:numdims]:
            for i in range(1, sink_upto):
                self._sink_validate(outer_loop, i)
            sink_upto = sink_upto - 1

        # Check that we can chunk each loop
        for (dim, loop) in zip(tiledims, outer_loop.walk(Loop)):
            ChunkLoopTrans().validate(loop, options={'chunksize': dim})

    def apply(self, outer_loop: Loop, tiledims: list[int], **kwargs):
        '''
        Converts the given Loop construct into a tiled version of the nested
        loops.

        :param outer_loop: the loop to transform.
        :param tiledims: the dimensions of the tile.

        :raises TransformationError: if any of the tile dimensions are not \
                positive integers
        :raises TransformationError: if the transformation cannot be applied
        '''
        self.validate(outer_loop, tiledims, **kwargs)

        numdims = len(tiledims)
        parent = outer_loop.parent
        position = outer_loop.position

        # Chunk the loops, from innermost to outermost
        for (dim, loop) in reversed(list(
                             zip(tiledims, outer_loop.walk(Loop)))):
            ChunkLoopTrans().apply(loop, options={'chunksize': dim})

        # Sink the new loops, from innermost to outermost
        loops = parent[position].walk(Loop)[1:2*numdims:2]
        for (i, loop) in enumerate(reversed(loops)):
            self._sink_apply(loop, i)
