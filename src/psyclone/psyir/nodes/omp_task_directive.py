# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2022-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------
""" This module contains the implementation of the OpenMP Task Directive
node."""

from psyclone.errors import GenerationError
from psyclone.psyir.nodes.omp_clauses import (
    OMPPrivateClause,
    OMPFirstprivateClause,
    OMPDependClause,
    OMPSharedClause,
)
from psyclone.psyir.nodes.omp_directives import (
    OMPRegionDirective,
    OMPSingleDirective,
)
from psyclone.psyir.nodes.schedule import Schedule


class OMPTaskDirective(OMPRegionDirective):
    """
    Class representing an OpenMP TASK directive in the PSyIR after lowering.
    This node should not be created by any transformation, it is solely used
    to represent TASK directives after lowering a DynamicOMPTaskDirective.

    :param list children: list of Nodes that are children of this Node.
    :type children: List[:py:class:`psyclone.psyir.nodes.Node`]
    :param parent: the Node in the AST that has this directive as a child
    :type parent: :py:class:`psyclone.psyir.nodes.Node`
    :param clauses: optional list of clauses to be added to this Node.
    :type clauses: Optional[List[:py:class:`psyclone.psyir.nodes.Clause`]]
    """
    # TODO #2330: This node cannot handle if the tree beneath it changes after
    # the clause computation - this should not currently cause much issues but
    # we should improve it in the future.

    _children_valid_format = (
        "Schedule, OMPPrivateClause,"
        "OMPFirstprivateClause, OMPSharedClause"
        "OMPDependClause, OMPDependClause"
    )

    def __init__(self, children=None, parent=None, clauses=None):
        super().__init__(children=children, parent=parent)
        if clauses:
            for child in clauses:
                child.detach()
                self.addchild(child)

    @staticmethod
    def _validate_child(position, child):
        """
         Decides whether a given child and position are valid for this node.
         The rules are:
         1. Child 0 must always be a Schedule.
         2. Child 1 must always be an OMPPrivateClause
         3. Child 2 must always be an OMPFirstprivateClause
         4. Child 3 must always be an OMPSharedClause
         5. Child 4 and 5 must always be OMPDependClauses

        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        """
        if position == 0:
            return isinstance(child, Schedule)
        if position == 1:
            return isinstance(child, OMPPrivateClause)
        if position == 2:
            return isinstance(child, OMPFirstprivateClause)
        if position == 3:
            return isinstance(child, OMPSharedClause)
        if position in (4, 5):
            return isinstance(child, OMPDependClause)
        return False

    @property
    def input_depend_clause(self):
        """
        :returns: the OMPDependClause child of this node corresponding to \
                  input dependencies.
        :rtype: :py:class:`psyclones.psyir.nodes.OMPDependClause`
        """
        return self.children[4]

    @property
    def output_depend_clause(self):
        """
        :returns: the OMPDependClause child of this node corresponding to \
                  output dependencies.
        :rtype: :py:class:`psyclones.psyir.nodes.OMPDependClause`
        """
        return self.children[5]

    def begin_string(self):
        """Returns the beginning statement of this directive, i.e.
        "omp task ...". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the beginning statement for this directive.
        :rtype: str

        """
        # Generate the string containing the required clauses
        return "omp task"

    def end_string(self):
        """Returns the end (or closing) statement of this directive, i.e.
        "omp end task". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the end statement for this directive.
        :rtype: str

        """
        return "omp end task"

    def validate_global_constraints(self):
        """
        Perform validation checks that can only be done at code-generation
        time.

        :raises GenerationError: if this OMPTaskDirective is not \
                                 enclosed within an OpenMP serial region.
        """
        # It is only at the point of code generation that we can check for
        # correctness (given that we don't mandate the order that a user
        # can apply transformations to the code). A taskloop
        # directive, we must have an OMPSerialDirective as an
        # ancestor back up the tree.
        if not self.ancestor(OMPSingleDirective):
            raise GenerationError(
                "OMPTaskDirective must be inside an OMP Single region "
                "but could not find an ancestor node."
            )

        # If there is a nowait clause on the ancestor OMPSingleDirective,
        # it is possible that we could make other tasks we expect to be
        # dependent that wouldn't be, as dependencies are only counted
        # in OpenMP if spawned by the same thread.
        if self.ancestor(OMPSingleDirective).nowait:
            raise GenerationError(
                "OMPTaskDirective found inside an OMP Single region "
                "with nowait attached. This means we can't guarantee "
                "correctness with other potential Single regions so is "
                "forbidden with PSyclone.")
