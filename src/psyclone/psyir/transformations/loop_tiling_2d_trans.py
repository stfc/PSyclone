# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2021-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''This module provides the LoopTiling2DTrans, which transforms a 2D Loop
construct into a tiled implementation of the construct.'''

from typing import Any, Optional
import warnings
from psyclone.psyir.nodes import Loop
from psyclone.psyir.transformations.loop_tiling_trans import LoopTilingTrans
from psyclone.psyir.transformations.loop_trans import LoopTrans
from psyclone.psyir.transformations.transformation_error import \
    TransformationError
from psyclone.utils import transformation_documentation_wrapper


@transformation_documentation_wrapper
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

    def validate(self, node: Loop,
                 options: Optional[dict[str, Any]] = None,
                 **kwargs: Any) -> None:
        '''
        Validates that the given Loop node can have a LoopTiling2DTrans
        applied.

        :param node: the loop to validate.
        :param options: a dict with options for transformation.
        :param tilesize: The size of the resulting tile, \
            currently square tiles are always used. If not specified, the \
            value 32 is used.

        :raises TransformationError: if an unsupported option has been \
            provided.
        :raises TransformationError: if the provided tilesize is not a \
            integer.
        '''
        if options:
            # TODO #2668: Deprecate options dictionary.
            warnings.warn(self._deprecation_warning, DeprecationWarning, 2)
        else:
            self.validate_options(**kwargs)
            tilesize = self.get_option("tilesize", **kwargs)
        super().validate(node, options=options, **kwargs)

        # Validate options map
        # TODO #2668: Hardcoding the valid_options does not allow for
        # subclassing this transformation and adding new options, this
        # should be fixed.
        valid_options = ['tilesize']
        for key, value in (options or {}).items():
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

        if options:
            tilesize = options.get("tilesize", 32)
        LoopTilingTrans().validate(node, tiledims=[tilesize, tilesize])

    def apply(self, node: Loop, options: Optional[dict[str, Any]] = None,
              tilesize: int = 32, **kwargs: Any) -> None:
        '''
        Converts the given 2D Loop construct into a tiled version of the nested
        loops.

        :param node: the loop to transform.
        :param options: a dict with options for transformations.
        :param tilesize: The size of the resulting tile, \
                currently square tiles are always used. If not \
                specified, the value 32 is used.

        '''
        warnings.warn("LoopTiling2DTrans is deprecated. "
                      "Use LoopTilingTrans instead.",
                      DeprecationWarning, 2)
        self.validate(node, options, tilesize=tilesize, **kwargs)
        if options:
            tilesize = options.get("tilesize", 32)
        LoopTilingTrans().apply(node, tiledims=[tilesize, tilesize])
