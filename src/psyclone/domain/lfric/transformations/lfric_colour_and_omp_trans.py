# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''This module provides a meta-transformation that applies colouring and
then OpenMP parallelisation to the loops in an LFRic routine.'''

from typing import Union

from psyclone.domain.lfric.lfric_constants import LFRicConstants
from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import Routine, Loop, ProfileNode
from psyclone.psyir.transformations.omp_parallel_trans import OMPParallelTrans
from psyclone.psyir.transformations.transformation_error import (
    TransformationError)
from psyclone.transformations import LFRicColourTrans, LFRicOMPLoopTrans
from psyclone.utils import transformation_documentation_wrapper


@transformation_documentation_wrapper
class LFRicColourAndOMPTrans(Transformation):
    '''
    Colours the loops in an LFRic routine and then applies OpenMP
    parallelisation to them.
    '''
    _SUB_TRANSFORMATIONS = [LFRicColourTrans, OMPParallelTrans,
                            LFRicOMPLoopTrans]

    def validate(self, node: Routine,
                 reprod: Union[bool, None] = None, **kwargs):
        # pylint: disable=arguments-renamed
        '''
        Validates the input options of the LFRicColourAndOMPTrans.

        :param node: the Routine node to transform
        :param reprod: whether to use reproducible form of OpenMP reduction.
        If none, the default value from the configuration is used.
        '''
        self.validate_options(reprod=reprod, **kwargs)

        if not isinstance(node, Routine):
            raise TransformationError(
                f"Error in {self.name} transformation. The supplied "
                f"node should be a Routine but found '{type(node).__name__}'.")

        # Colouring must happen before any profiling calipers are added
        # otherwise the loops we need to transform are no longer children
        # of the Routine.
        if node.walk(ProfileNode):
            raise TransformationError(
                f"Error in {self.name} transformation. This transformation "
                f"must be applied BEFORE any profiling transformation but "
                f"the supplied Routine '{node.name}' already contains a "
                f"ProfileNode.")

    def _colour_loops(self, node: Routine, **colour_kwargs):
        '''
        Applies colouring to every loop over cell-columns that is on a
        continuous function space.

        :param node: the Routine whose loops are to be coloured.
        :param colour_kwargs: keyword arguments for the LFRicColourTrans
        '''
        const = LFRicConstants()
        ctrans = LFRicColourTrans()

        for child in node.children[:]:
            if (isinstance(child, Loop)
                    and child.iteration_space.endswith("cell_column")
                    and child.field_space.orig_name
                    not in const.VALID_DISCONTINUOUS_NAMES):
                ctrans.apply(child, **colour_kwargs)

    def apply(self, node: Routine, reprod: Union[bool, None] = None, **kwargs):
        # pylint: disable=arguments-renamed
        '''
        :param node: the Routine node to transform
        :param reprod: whether to use reproducible form of OpenMP reduction.
        If none, the default value from the configuration is used.
        '''
        local_kwargs, colour_kwargs, _, _ = self.split_kwargs(
            reprod=reprod, **kwargs)

        self.validate(node, **local_kwargs)

        self._colour_loops(node, **colour_kwargs)


# For Sphinx AutoAPI documentation generation
__all__ = ["LFRicColourAndOMPTrans"]
