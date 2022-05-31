# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2022, Science and Technology Facilities Council.
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
# Author J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

'''This module provides a class to manage indices in variable accesses.'''

from __future__ import print_function, absolute_import


from psyclone.errors import InternalError


class ComponentIndices():
    '''This class stores index information for variable accesses. It stores
    one index list for each component of a variable, e.g. for `a(i)%b(j)`
    it would store `[ [i], [j] ]`. Even for scalar accesses an empty list
    is stored, so `a` would have the component indices `[ [] ]`, and `a%b`
    would have `[ [], [] ]`. Each member of this list of lists is the PSyIR
    node describing the array expression used.

    As a shortcut, the `indices` parameter can be None or an empty list
    (which then creates the component indices as `[[]]`, i.e. indicating
    a scalar access), a list `l` (which will then create the component
    indices as `[l]`, i.e. a single component variable, which uses
    all the indices in the list `l` as array indices).

    :param indices: the indices from which to create this object.
    :type indices: None, [], a list or a list of lists of \
        :py:class:`psyclone.psyir.nodes.Node`

    :raises InternalError: if the indices parameter is not None, a list \
        or a list of lists.
    :raises InternalError: if the indices parameter is a list, and some \
        but not all members are a list.

    '''
    def __init__(self, indices=None):
        if indices is None or indices == []:
            self._component_indices = [[]]
        elif isinstance(indices, list):
            if all(isinstance(indx, list) for indx in indices):
                # All elements of the indices are a list - so we
                # got a list of lists:
                self._component_indices = indices
            elif all(not isinstance(indx, list) for indx in indices):
                self._component_indices = [indices]
            else:
                raise InternalError(f"ComponentIndices: Invalid "
                                    f"list parameter '{indices}' - some "
                                    f"elements but not all are lists")
        else:
            raise InternalError(f"Index object in ComponentIndices "
                                f"constructor must be None, a list or "
                                f"list of lists, got '{indices}'")

    # ------------------------------------------------------------------------
    def __str__(self):
        '''Returns a string representating the indices.'''
        return str(self._component_indices)

    # ------------------------------------------------------------------------
    def iterate(self):
        '''Allows iterating over all component indices. It returns a tuple
        with two elements, the first one indicating the component, the second
        the dimension for which the index is. The return tuple can be used
        in a dictionary access (see `__getitem__`) of this object.

        :returns: a tuple of the component index and index.
        :rtype: tuple(int, int)

        '''
        for comp_ind, component in enumerate(self._component_indices):
            for indx in range(len(component)):
                yield (comp_ind, indx)

    # ------------------------------------------------------------------------
    def __getitem__(self, indx):
        '''Allows to use this class as a dictionary. If `indx` is an integer,
        the list of indices for the specified component is returned. If `indx`
        is a tuple (as returned from `iterate`), it will return the PSyIR of
        the index for the specified component at the specified dimension.

        :returns: either the list of indices for a component, or the index \
            PSyIR node for the specified tuple.
        :rtype: list of :py:class:`psyclone.psyir.nodes.Node`, or \
            :py:class:`psyclone.psyir.nodes.Node`

        :raises IndexError: if a tuple is given and one of the indices is \
            outside of the valid range.

        '''
        if isinstance(indx, tuple):
            if indx[0] < 0 or indx[0] >= len(self._component_indices):
                raise IndexError(f"First index ({indx[0]}) of {indx} is out "
                                 f"of range.")
            if indx[1] < 0 or indx[1] >= len(self._component_indices[indx[0]]):
                raise IndexError(f"Second index ({indx[1]}) of {indx} is out "
                                 f"of range.")
            return self._component_indices[indx[0]][indx[1]]
        return self._component_indices[indx]

    # ------------------------------------------------------------------------
    def __len__(self):
        ''':returns: the number of components in this class.
        :rtype: int '''
        return len(self._component_indices)

    # ------------------------------------------------------------------------
    @property
    def indices_lists(self):
        ''':returns: the component indices list of lists.
        :rtype: list of list of :py:class:`psyclone.psyir.nodes.Node`
        '''
        return self._component_indices

    # ------------------------------------------------------------------------
    def is_array(self):
        '''Test whether there is an index used in any component. E.g. an access
        like `a(i)%b` with indices `[ [i], [] ]` would still be considered an
        array.

        :returns: whether any of the variable components uses an index, i.e.\
            the variable is an array.
        :rtype: bool
        '''
        return any(grp for grp in self._component_indices)

    # ------------------------------------------------------------------------
    def get_subscripts_of(self, set_of_vars):
        '''This function returns a flat list of which variable from the
        given set of variables is used in each subscript. For example, the
        access `a(i+i2)%b(j*j+k,k)%c(l,5)` would have the component_indices
        `[[i+i2], [j*j+k,k], [l,5]]`. If the set of variables is
        `(i,j,k)`, then `get_subscripts_of` would return
        `[{i},{j,k},{k},{l},{}]`.

        :param set_of_vars: set with name of all variables.
        :type set_of_vars: Set[str]

        :return: a list of sets with all variables used in the corresponding \
            array subscripts as strings.
        :rtype: List[Set[str]]

        '''
        # Circular import
        # pylint: disable=import-outside-toplevel
        from psyclone.core import VariablesAccessInfo

        indices = []
        for i in self.iterate():
            indx = self[i]
            index_vars = VariablesAccessInfo(indx)
            unique_vars = set(str(sig) for sig in index_vars.keys())
            unique_vars = unique_vars.intersection(set_of_vars)
            indices.append(unique_vars)
        return indices


# ---------- Documentation utils -------------------------------------------- #
# The list of module members that we wish AutoAPI to generate
# documentation for. (See https://psyclone-ref.readthedocs.io)
__all__ = ["ComponentIndices"]
