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

'''This module provides the LoopTiling2DTrans, which transforms a 2D Loop
construct into a tiled implementation of the construct.'''

import warnings
from psyclone.psyir.transformations.loop_tiling_trans import LoopTilingTrans
from psyclone.psyir.transformations.loop_trans import LoopTrans
from psyclone.psyir.transformations.transformation_error import \
    TransformationError


class LoopTiling2DTrans(LoopTrans):
    '''
    Apply a 2D loop tiling transformation to a loop.  This is a special
    case of LoopTilingTrans for 2D square tiles. For example:

    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.nodes import Loop
    >>> from psyclone.psyir.transformations import LoopTiling2DTrans
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
    >>> LoopTiling2DTrans().apply(loop)

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
        return "Tile the loop construct using 2D blocks"

    def validate(self, node, options=None):
        '''
        Validates that the given Loop node can have a LoopTiling2DTrans
        applied.

        :param node: the loop to validate.
        :type node: :py:class:`psyclone.psyir.nodes.Loop`
        :param options: a dict with options for transformation.
        :type options: Optional[Dict[str, Any]]
        :param int options["tilesize"]: The size of the resulting tile, \
            currently square tiles are always used. If not specified, the \
            value 32 is used.

        :raises TransformationError: if an unsupported option has been \
            provided.
        :raises TransformationError: if the provided tilesize is not a \
            integer.
        '''
        if options is None:
            options = {}
        super(LoopTiling2DTrans, self).validate(node, options=options)

        # Validate options map
        # TODO #613: Hardcoding the valid_options does not allow for
        # subclassing this transformation and adding new options, this
        # should be fixed.
        valid_options = ['tilesize']
        for key, value in options.items():
            if key in valid_options:
                if key == "tilesize" and not isinstance(value, int):
                    raise TransformationError(
                        f"The LoopTiling2DTrans tilesize option must be a "
                        f"positive integer but found a "
                        f"'{type(value).__name__}'.")
                if key == "tilesize" and value <= 0:
                    raise TransformationError(
                        f"The LoopTiling2DTrans tilesize option must be a "
                        f"positive integer but found '{value}'.")
            else:
                raise TransformationError(
                    f"The LoopTiling2DTrans does not support the "
                    f"transformation option '{key}', the supported options "
                    f"are: {valid_options}.")

        tilesize = options.get("tilesize", 32)
        LoopTilingTrans().validate(node, tiledims=[tilesize, tilesize])

    def apply(self, node, options=None):
        '''
        Converts the given 2D Loop construct into a tiled version of the nested
        loops.

        :param node: the loop to transform.
        :type node: :py:class:`psyclone.psyir.nodes.Loop`
        :param options: a dict with options for transformations.
        :type options: Optional[Dict[str, Any]]
        :param int options["tilesize"]: The size of the resulting tile, \
                currently square tiles are always used. If not \
                specified, the value 32 is used.

        '''
        warnings.warn("LoopTiling2DTrans is deprecated. "
                      "Use LoopTilingTrans instead.",
                      DeprecationWarning, 2)
        self.validate(node, options)
        if options is None:
            options = {}
        tilesize = options.get("tilesize", 32)
        LoopTilingTrans().apply(node, tiledims=[tilesize, tilesize])
