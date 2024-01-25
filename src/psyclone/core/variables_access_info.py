# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2024, Science and Technology Facilities Council.
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
# Modified by: S. Siso, STFC Daresbury Laboratory
#              A. R. Porter, STFC Daresbury Laboratory
#              A. B. G. Chalk, STFC Daresbury Laboratory
# -----------------------------------------------------------------------------

'''This module provides management of variable access information.'''


from psyclone.core.component_indices import ComponentIndices
from psyclone.core.signature import Signature
from psyclone.core.single_variable_access_info import SingleVariableAccessInfo
from psyclone.errors import InternalError


class VariablesAccessInfo(dict):
    '''This class stores all `SingleVariableAccessInfo` instances for all
    variables in the corresponding code section. It maintains 'location'
    information, which is an integer number that is increased for each new
    statement. It can be used to easily determine if one access is before
    another.

    :param nodes: optional, a single PSyIR node or list of nodes from
        which to initialise this object.
    :type nodes: Optional[:py:class:`psyclone.psyir.nodes.Node` |
        List[:py:class:`psyclone.psyir.nodes.Node`]]
    :param options: a dictionary with options to influence which variable
        accesses are to be collected.
    :type options: Dict[str, Any]
    :param Any options["COLLECT-ARRAY-SHAPE-READS"]: if this option is set
        to a True value, arrays used as first parameter to the PSyIR query
        operators lbound, ubound, or size will be reported as 'read'.
        Otherwise, these accesses will be ignored.
    :param Any options["USE-ORIGINAL-NAMES"]: if this option is set to a
        True value, an imported symbol that is renamed (``use mod, a=>b``)
        will be reported using the original name (``b`` in the example).
        Otherwise these symbols will be reported using the renamed name
        (``a``).

    :raises InternalError: if the optional options parameter is not a
        dictionary.
    :raises InternalError: if the nodes parameter either is a list and
        contains an element that is not a
        :py:class:`psyclone.psyir.nodes.Node`, of if nodes is not a list and
        is not of type :py:class:`psyclone.psyir.nodes.Node`

    '''
    # List of valid options and their default values. Note that only the
    # options method checks this, since it is convenient to pass in options
    # from the DependencyTools that might contain options for these tools.
    # COLLECT-ARRAY-SHAPE-READS: controls if access to the shape of an array
    #     (e.g. ``ubound(a)`` are reported as read or not at all. Defaults
    #     to True.
    # USE-ORIGINAL-NAMES: if set this will report the original names of any
    #     symbol that is being renamed (``use mod, renamed_a=>a``). Defaults
    #     to False.
    _DEFAULT_OPTIONS = {"COLLECT-ARRAY-SHAPE-READS": False,
                        "USE-ORIGINAL-NAMES": False}

    def __init__(self, nodes=None, options=None):
        # This dictionary stores the mapping of signatures to the
        # corresponding SingleVariableAccessInfo instance.
        dict.__init__(self)

        self._options = VariablesAccessInfo._DEFAULT_OPTIONS.copy()
        if options:
            if not isinstance(options, dict):
                raise InternalError(f"The options argument for "
                                    f"VariablesAccessInfo must be a "
                                    f"dictionary or None, but got "
                                    f"'{type(options).__name__}'.")
            self._options.update(options)

        # Stores the current location information
        self._location = 0
        if nodes:
            # Import here to avoid circular dependency
            # pylint: disable=import-outside-toplevel
            from psyclone.psyir.nodes import Node
            if isinstance(nodes, list):
                for node in nodes:
                    if not isinstance(node, Node):
                        raise InternalError(f"Error in VariablesAccessInfo. "
                                            f"One element in the node list is "
                                            f"not a Node, but of type "
                                            f"{type(node)}")

                    node.reference_accesses(self)
            elif isinstance(nodes, Node):
                nodes.reference_accesses(self)
            else:
                arg_type = str(type(nodes))
                raise InternalError(f"Error in VariablesAccessInfo. "
                                    f"Argument must be a single Node in a "
                                    f"schedule or a list of Nodes in a "
                                    f"schedule but have been passed an "
                                    f"object of type: {arg_type}")

    def __str__(self):
        '''Gives a shortened visual representation of all variables
        and their access mode. The output is one of: READ, WRITE, READ+WRITE,
        or READWRITE for each variable accessed.
        READ+WRITE is used if the statement (or set of statements)
        contain individual read and write accesses, e.g. 'a=a+1'. In this
        case two accesses to `a` will be recorded, but the summary displayed
        using this function will be 'READ+WRITE'. Same applies if this object
        stores variable access information about more than one statement, e.g.
        'a=b; b=1'. There would be two different accesses to 'b' with two
        different locations, but the string representation would show this as
        READ+WRITE. If a variable is is passed to a kernel for which no
        individual variable information is available, and the metadata for
        this kernel indicates a READWRITE access, this is marked as READWRITE
        in the string output.'''

        all_signatures = self.all_signatures
        output_list = []
        for signature in all_signatures:
            mode = ""
            if self.has_read_write(signature):
                mode = "READWRITE"
            else:
                if self.is_read(signature):
                    if self.is_written(signature):
                        mode = "READ+WRITE"
                    else:
                        mode = "READ"
                elif self.is_written(signature):
                    mode = "WRITE"
            output_list.append(f"{signature}: {mode}")
        return ", ".join(output_list)

    def options(self, key=None):
        '''Returns the value of the options for a specified key,
        or None if the key is not specified in the options. If no
        key is specified, the whole option dictionary is returned.

        :param key: the option to query, or None if all options should
                    be returned.
        :type key: Optional[str]

        :returns: the value of the option associated with the provided key
                  or the whole option dictionary if it is not supplied.
        :rtype: Union[None, Any, dict]

        :raises InternalError: if an invalid key is specified.

        '''
        if key:
            if key not in VariablesAccessInfo._DEFAULT_OPTIONS:
                valids = list(VariablesAccessInfo._DEFAULT_OPTIONS.keys())
                # This makes sure the message always contains the valid
                # keys in the same order, important for testing.
                valids.sort()
                raise InternalError(f"Option key '{key}' is invalid, it "
                                    f"must be one of {valids}.")
            return self._options.get(key, None)
        return self._options

    @property
    def location(self):
        '''Returns the current location of this instance, which is
        the location at which the next accesses will be stored.
        See the Developers' Guide for more information.

        :returns: the current location of this object.
        :rtype: int'''
        return self._location

    def next_location(self):
        '''Increases the location number.'''
        self._location = self._location + 1

    def add_access(self, signature, access_type, node, component_indices=None):
        '''Adds access information for the variable with the given signature.
        If the `component_indices` parameter is not an instance of
        `ComponentIndices`, it is used to construct an instance. Therefore it
        can be None, a list or a list of lists of PSyIR nodes. In the case of
        a list of lists, this will be used unmodified to construct the
        ComponentIndices structures. If it is a simple list, it is assumed
        that it contains the indices used in accessing the last component
        of the signature. For example, for `a%b` with
        `component_indices=[i,j]`, it will create `[[], [i,j]` as component
        indices, indicating that no index is used in the first component `a`.
        If the access is supposed to be for `a(i)%b(j)`, then the
        `component_indices` argument must be specified as a list of lists,
        i.e. `[[i], [j]]`.

        :param signature: the signature of the variable.
        :type signature: :py:class:`psyclone.core.Signature`
        :param access_type: the type of access (READ, WRITE, ...)
        :type access_type: :py:class:`psyclone.core.access_type.AccessType`
        :param node: Node in PSyIR in which the access happens.
        :type node: :py:class:`psyclone.psyir.nodes.Node` instance
        :param component_indices: index information for the access.
        :type component_indices: \
            :py:class:`psyclone.core.component_indices.ComponentIndices`, or \
            any other type that can be used to construct a ComponentIndices \
            instance (None, List[:py:class:`psyclone.psyir.nodes.Node`] \
             or List[List[:py:class:`psyclone.psyir.nodes.Node`]])

        '''
        if not isinstance(signature, Signature):
            raise InternalError(f"Got '{signature}' of type "
                                f"'{type(signature).__name__}' but expected "
                                f"it to be of type psyclone.core.Signature.")

        # To make it easier for the user, we allow to implicitly create the
        # component indices instance here:
        if not isinstance(component_indices, ComponentIndices):
            # Handle some convenient cases:
            # 1. Add the right number of [] if component_indices is None:
            if component_indices is None:
                component_indices = [[]] * len(signature)
            elif isinstance(component_indices, list):
                # 2. If the argument is a simple list (not a list of lists),
                # assume that the indices are for the last component, and
                # add enough [] to give the right number of entries in the
                # list that is used to create the ComponentIndices instance:
                is_list_of_lists = all(isinstance(indx, list)
                                       for indx in component_indices)
                if not is_list_of_lists:
                    component_indices = [[]] * (len(signature)-1) \
                                      + [component_indices]

            component_indices = ComponentIndices(component_indices)

        if len(signature) != len(component_indices):
            raise InternalError(f"Cannot add '{component_indices}' with "
                                f"length {len(component_indices)} as "
                                f"indices for '{signature}' which "
                                f"requires {len(signature)} elements.")

        if signature in self:
            self[signature].add_access_with_location(access_type,
                                                     self._location, node,
                                                     component_indices)
        else:
            var_info = SingleVariableAccessInfo(signature)
            var_info.add_access_with_location(access_type, self._location,
                                              node, component_indices)
            self[signature] = var_info

    @property
    def all_signatures(self):
        ''':returns: all signatures contained in this instance, sorted (in \
                     order to make test results reproducible).
        :rtype: List[:py:class:`psyclone.core.signature`]
        '''
        list_of_vars = list(self.keys())
        list_of_vars.sort()
        return list_of_vars

    def merge(self, other_access_info):
        '''Merges data from a VariablesAccessInfo instance to the
        information in this instance.

        :param other_access_info: the other VariablesAccessInfo instance.
        :type other_access_info: \
            :py:class:`psyclone.core.VariablesAccessInfo`
        '''

        # For each variable add all accesses. After merging the new data,
        # we need to increase the location so that all further added data
        # will have a location number that is larger.
        max_new_location = 0
        for signature in other_access_info.all_signatures:
            var_info = other_access_info[signature]
            for access_info in var_info.all_accesses:
                # Keep track of how much we need to update the next location
                # in this object:
                if access_info.location > max_new_location:
                    max_new_location = access_info.location
                new_location = access_info.location + self._location
                if signature in self:
                    var_info = self[signature]
                else:
                    var_info = SingleVariableAccessInfo(signature)
                    self[signature] = var_info

                var_info.add_access_with_location(access_info.access_type,
                                                  new_location,
                                                  access_info.node,
                                                  access_info.
                                                  component_indices)
        # Increase the current location of this instance by the amount of
        # locations just merged in
        self._location = self._location + max_new_location

    def is_written(self, signature):
        '''Checks if the specified variable signature is at least
        written once.

        :param signature: signature of the variable.
        :type signature: :py:class:`psyclone.core.Signature`

        :returns: True if the specified variable is written (at least \
            once).
        :rtype: bool

        :raises: KeyError if the signature name cannot be found.

        '''
        var_access_info = self[signature]
        return var_access_info.is_written()

    def is_read(self, signature):
        '''Checks if the specified variable signature is at least read once.

        :param signature: signature of the variable
        :type signature: :py:class:`psyclone.core.Signature`

        :returns: True if the specified variable name is read (at least \
            once).
        :rtype: bool

        :raises: KeyError if the signature cannot be found.'''

        var_access_info = self[signature]
        return var_access_info.is_read()

    def has_read_write(self, signature):
        '''Checks if the specified variable signature has at least one
        READWRITE access (which is typically only used in a function call).

        :param signature: signature of the variable
        :type signature: :py:class:`psyclone.core.Signature`

        :returns: True if the specified variable name has (at least one) \
            READWRITE access.
        :rtype: bool

        :raises: KeyError if the signature cannot be found.'''

        var_access_info = self[signature]
        return var_access_info.has_read_write()


# ---------- Documentation utils -------------------------------------------- #
# The list of module members that we wish AutoAPI to generate
# documentation for. (See https://psyclone-ref.readthedocs.io)
__all__ = ["VariablesAccessInfo"]
