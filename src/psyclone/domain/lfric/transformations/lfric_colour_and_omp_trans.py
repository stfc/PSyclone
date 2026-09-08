# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''This module provides a meta-transofmation that applies colouring and
then OpenMP parallelisation to the loops in an LFRic routine.'''

from psyclone.psyGen import Transformation
from psyclone.psyir.nodes import Routine
from psyclone.psyir.transformations.omp_parallel_trans import OMPParallelTrans
from psyclone.transformations import LFRicColourTrans, LFRicOMPLoopTrans
from psyclone.utils import transformation_documentation_wrapper

from typing import Union


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
        '''
        Validates the input options of the LFRicColourAndOMPTrans.

        :param node: the Routine node to transform
        :param reprod: whether to use reproducible form of OpenMP reduction.
        If none, the default value from the configuration is used.
        '''
        raise NotImplementedError("TODO #3503")

    def apply(self, node: Routine, reprod: Union[bool, None] = None, **kwargs):
        '''
        :param node: the Routine node to transform
        :param reprod: whether to use reproducible form of OpenMP reduction.
        If none, the default value from the configuration is used.
        '''
        raise NotImplementedError("TODO #3503")


# For Sphinx AutoAPI documentation generation
__all__ = ["LFRicColourAndOMPTrans"]
