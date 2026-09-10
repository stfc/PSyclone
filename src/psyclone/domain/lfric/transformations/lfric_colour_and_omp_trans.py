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
from psyclone.psyir.nodes import Directive, Loop, ProfileNode, Routine
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

    def _parallelise_loops(self, node: Routine,
                           reprod: Union[bool, None] = None, **par_kwargs):
        '''
        Applies OpenMP parallelisation to every loop that is not a loop
        over colours and it not already a directive.

        :param node: the Routine whose loops are to be parallelised.
        :param reprod: whether to use reproducible form of OpenMP reduction.
        If none, the default value from the configuration is used.
        :param par_kwargs: keyword arguments for the OMPParallelTrans.
        '''
        otrans = LFRicOMPLoopTrans()
        oregtrans = OMPParallelTrans()

        # TODO #2668: LFRicOMPLoopTrans has not yet been migrated to kwargs. It
        # it gives options inherrited from OMPLoopTrans but discards any
        # **kwargs it is given, so we need to build an options dict here.
        # Remove once it accepts kwargs directly.
        options = None if reprod is None else {"reprod": reprod}

        for loop in node.walk(Loop):
            if loop.loop_type in ["colours", "null"]:
                continue  # Skip loops over colours and null loops
            if loop.ancestor(Directive):
                continue  # Skip if an outer loop is already parallelised
            oregtrans.apply(loop, **par_kwargs)
            otrans.apply(loop, options=options)

    def apply(self, node: Routine, reprod: Union[bool, None] = None, **kwargs):
        # pylint: disable=arguments-renamed
        '''
        :param node: the Routine node to transform
        :param reprod: whether to use reproducible form of OpenMP reduction.
        If none, the default value from the configuration is used.
        '''
        local_kwargs, colour_kwargs, par_kwargs, _ = self.split_kwargs(
            reprod=reprod, **kwargs)

        self.validate(node, **local_kwargs)

        self._colour_loops(node, **colour_kwargs)
        self._parallelise_loops(node, reprod=reprod, **par_kwargs)


# For Sphinx AutoAPI documentation generation
__all__ = ["LFRicColourAndOMPTrans"]
