# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' This module contains the ACCLoopTrans transformation.'''

from typing import Union

from psyclone.psyir.transformations.parallel_loop_trans import (
    ParallelLoopTrans)
from psyclone.psyir.nodes import (ACCLoopDirective, Loop, Node, PSyDataNode)
from psyclone.utils import transformation_documentation_wrapper


@transformation_documentation_wrapper
class ACCLoopTrans(ParallelLoopTrans):
    '''
    Adds an OpenACC loop directive to a loop. This directive must be within
    the scope of some OpenACC Parallel region (at code-generation time).

    For example:

    >>> from psyclone.tests.utilities import get_psylayer_schedule
    >>> filename = "nemolite2d_alg_mod.f90"
    >>> schedule = get_psylayer_schedule(filename, api="gocean")
    >>>
    >>> from psyclone.psyir.transformations import ACCLoopTrans
    >>> from psyclone.transformations import ACCParallelTrans
    >>>
    >>> ltrans = ACCLoopTrans()
    >>> rtrans = ACCParallelTrans()
    >>>
    >>> # Apply the OpenACC Loop transformation to *every* loop in the schedule
    >>> for child in schedule.children[:]:
    ...     ltrans.apply(child)
    >>>
    >>> # Enclose all of these loops within a single OpenACC parallel region
    >>> rtrans.apply(schedule)

    '''
    # The types of node that must be excluded from the section of PSyIR
    # being transformed.
    excluded_node_types = (PSyDataNode,)

    def __init__(self):
        # Whether to add the "independent" clause
        # to the loop directive.
        self._independent = True
        self._sequential = False
        self._gang = False
        self._vector = False
        super().__init__()

    def __str__(self):
        return "Adds an 'OpenACC loop' directive to a loop"

    def _directive(
        self, children: list[Node], collapse: Union[int, None] = None
    ) -> ACCLoopDirective:
        '''
        Creates the ACCLoopDirective needed by this sub-class of
        transformation.

        :param children: list of child nodes of the new directive Node.
        :param int collapse: number of nested loops to collapse or None if
                             no collapse attribute is required.
        '''
        directive = ACCLoopDirective(children=children,
                                     collapse=collapse,
                                     independent=self._independent,
                                     sequential=self._sequential,
                                     gang=self._gang,
                                     vector=self._vector)
        return directive

    def apply(self, node: Loop, options=None,
              independent: bool = True,
              sequential: bool = False,
              gang: bool = False, vector: bool = False,
              **kwargs) -> None:
        '''
        Apply the ACCLoop transformation to the specified node. This node
        must be a Loop since this transformation corresponds to
        inserting a directive immediately before a loop, e.g.:

        .. code-block:: fortran

          !$ACC LOOP
          do ...
             ...
          end do

        At code-generation time (when lowering is called),
        this node must be within (i.e. a child of) a PARALLEL region.

        :param node: the supplied node to which we will apply the
                     Loop transformation.
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]
        :param independent: whether to add the "independent"
                clause to the directive (not strictly necessary within
                PARALLEL regions).
        :param sequential: whether to add the "seq" clause to
                the directive.
        :param gang: whether to add the "gang" clause to the
                directive.
        :param vector: whether to add the "vector" clause to
                the directive.

        '''
        # Store sub-class specific options. These are used when
        # creating the directive (in the _directive() method).
        # TODO 2668: Deprecate options dict.
        if options:
            self._independent = options.get("independent", True)
            self._sequential = options.get("sequential", False)
            self._gang = options.get("gang", False)
            self._vector = options.get("vector", False)
        else:
            self.validate_options(independent=independent,
                                  sequential=sequential,
                                  gang=gang, vector=vector,
                                  **kwargs)
            self._independent = independent
            self._sequential = sequential
            self._gang = gang
            self._vector = vector

        # Call the apply() method of the base class
        super().apply(node, options, independent=independent,
                      sequential=sequential, gang=gang, vector=vector,
                      **kwargs)


# For Sphinx AutoAPI documentation generation
__all__ = ["ACCLoopTrans"]
