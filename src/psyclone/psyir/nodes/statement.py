# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2020-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' This module contains the Statement abstract node implementation.'''

from psyclone.core import Signature
from psyclone.psyir.nodes.node import Node
from psyclone.psyir.commentable_mixin import CommentableMixin


class Statement(Node, CommentableMixin):
    '''
    Abstract node representing a general PSyIR Statement.
    '''

    def _get_next_accesses(self, sub_nodelist: list[Node]) -> list[Node]:
        '''
        :param sub_nodelist: The list of nodes to compute the next_accesses
            for.
        :returns: The combined next_accesses of the input nodes.
        '''
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.nodes.schedule import Schedule
        next_accesses = []
        for node in sub_nodelist:
            if isinstance(node, Schedule):
                var_accesses = self._get_next_accesses(node.children[:])
            else:
                var_accesses = node.next_accesses()
            self._merge_accesses(next_accesses, var_accesses)
        return next_accesses

    def next_accesses(self) -> list[Node]:
        '''
        :returns: the nodes containing the next accesses of the symbols
                  accessed within this node. It can be multiple nodes for
                  each symbol as it contains all nodes containing a following
                  access of any symbol accessed within this node. For each
                  symbol this includes all following accesses until a
                  guaranteed write is found.
        '''
        return self._get_next_accesses(self.children[:])

    def _get_prev_accesses(self, sub_nodelist: list[Node]) -> list[Node]:
        '''
        :param sub_nodelist: The list of nodes to compute the
            previous_accesses for.
        :returns: The combined previous_accesses of the input nodes.
        '''
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.nodes.schedule import Schedule
        prev_accesses = []
        for node in sub_nodelist:
            if isinstance(node, Schedule):
                var_accesses = self._get_prev_accesses(node.children[:])
            else:
                var_accesses = node.previous_accesses()
            self._merge_accesses(prev_accesses, var_accesses)
        return prev_accesses

    def previous_accesses(self) -> list[Node]:
        '''
        :returns: the nodes containing the previous accesses of the symbols
                  accessed within this node. It can be multiple nodes for
                  each symbol as it contains all nodes containing a previous
                  access of any symbol accessed within this node. For each
                  symbol this includes all previous accesses until a
                  guaranteed write is found.
        '''
        return self._get_prev_accesses(self.children[:])

    def _merge_accesses_dict(
        self, current_accesses: list[Node],
        access_dict: dict[Signature, list[Node]]
    ) -> None:
        '''
        Helper function to flatten and merge an access_dict from the
        DefinitionUseChains into the list of current_accesses for the
        next/previous_access functions.

        :param current_accesses: The list of currently computed dependent
            accesses for this node.
        :param access_dict: The access_dict to merge into
            current_accesses.
        '''
        # Flatten the access_dict into a list.
        results = []
        for sig in access_dict:
            for access in access_dict[sig]:
                if all(x is not access for x in results):
                    results.append(access)
        self._merge_accesses(current_accesses, results)

    def _merge_accesses(
        self, current_accesses: list[Node], new_accesses: list[Node]
    ) -> None:
        '''
        Helper function to merge access lists together for Statement
        subclass next/previous_accesses functions.
        Take all the accesses from new_accesses and adds them to the
        current_accesses list if they're not already present and are
        not contained in this nodes subtree.

        :param current_accesses: The list of currently computed dependent
            accesses for this node.
        :param new_accesses: The list of accesses to merge into
            current_accesses.
        '''
        for access in new_accesses:
            in_subtree = access.is_descendant_of(self)
            # If the access is not in the subtree of this node, and
            # is not already present in the current_accesses array
            # then add it to the array
            if not in_subtree and all(
                    [acc is not access for acc in current_accesses]):
                current_accesses.append(access)


# For automatic API documentation generation
__all__ = ["Statement"]
