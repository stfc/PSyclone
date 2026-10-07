# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''This module contains the GOcean-specific loop-fusion transformation.
'''

from psyclone.psyir.transformations import LoopFuseTrans, TransformationError
from psyclone.gocean1p0 import GOLoop
from psyclone.utils import transformation_documentation_wrapper


@transformation_documentation_wrapper
class GOceanLoopFuseTrans(LoopFuseTrans):
    ''' GOcean API specialisation of the :py:class:`base class <LoopFuseTrans>`
    in order to fuse two GOcean loops after performing validity checks (e.g.
    that the loops are over the same grid-point type). For example:

    >>> from psyclone.tests.utilities import get_psylayer_schedule
    >>> filename = "eg1/shallow_alg.f90"
    >>> schedule = get_psylayer_schedule(filename, "gocean-examples")
    >>>
    >>> from psyclone.domain.gocean.transformations import GOceanLoopFuseTrans
    >>> ftrans = GOceanLoopFuseTrans()

    # Currently produces an error with  "Cannot fuse loops that are over "
    # "different grid-point types: go_cu and go_cv"
    # >>> ftrans.apply(schedule[0], schedule[1])

    '''
    def __str__(self):
        return ("Fuse two adjacent loops together with GOcean-specific "
                "validity checks")

    def validate(self, node1: GOLoop, node2: GOLoop, options=None, **kwargs):
        '''Checks if it is valid to apply the GOceanLoopFuseTrans
        transform. It ensures that the fused loops are over
        the same grid-point types, before calling the normal
        LoopFuseTrans validation function.

        :param node1: the first Node representing a GOLoop.
        :param node2: the second Node representing a GOLoop.
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

        :raises TransformationError: if the supplied loops are over \
                                     different grid-point types.

        :raises TransformationError: if invalid parameters are passed in.

        '''
        if not (isinstance(node1, GOLoop) and
                isinstance(node2, GOLoop)):
            raise TransformationError(f"Error in {self.name} transformation. "
                                      f"Both nodes must be of the same "
                                      f"GOLoop class.")

        if node1.field_space != node2.field_space:
            raise TransformationError(
                f"Error in {self.name} transformation. Cannot "
                f"fuse loops that are over different grid-point types: "
                f"{node1.field_space} and {node2.field_space}")

        super().validate(node1, node2, options=options, **kwargs)

    def apply(self, node1: GOLoop, node2: GOLoop,
              options=None, **kwargs):
        '''Applies the GoceanLoopFuseTrans to the provided nodes.
        :param node1: the first Node representing a GOLoop.
        :param node2: the second Node representing a GOLoop.
        '''
        # This function is used for documentation purposes.
        super().apply(node1, node2, options=options, **kwargs)


# For automatic documentation generation
__all__ = ["GOceanLoopFuseTrans"]
