# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2025, Science and Technology Facilities Council.
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


from typing import Dict, List, Tuple

from psyclone.core.access_type import AccessType
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
    # USE-ORIGINAL-NAMES: if set this will report the original names of any
    #     symbol that is being renamed (``use mod, renamed_a=>a``). Defaults
    #     to False.

    _DEFAULT_OPTIONS = {"USE-ORIGINAL-NAMES": False,
                        "FLATTEN": False}

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
                else:
                    # The data associated with this signature is not accessed.
                    mode = "NO_DATA_ACCESS"
            all_accesses = self[signature]
            cond = any(acc.conditional for acc in all_accesses)
            output_list.append(f"{'%' if cond else ''}{signature}: {mode}")
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

    def add_access(self, signature, access_type, node, component_indices=None,
                   conditional=False):
        # pylint: disable=too-many-arguments
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
                                                     component_indices,
                                                     conditional=conditional)
        else:
            var_info = SingleVariableAccessInfo(signature)
            var_info.add_access_with_location(access_type, self._location,
                                              node, component_indices,
                                              conditional=conditional)
            self[signature] = var_info

    @property
    def all_signatures(self):
        ''':returns: all signatures contained in this instance, sorted (in \
                     order to make test results reproducible).
        :rtype: List[:py:class:`psyclone.core.Signature`]
        '''
        list_of_vars = list(self.keys())
        list_of_vars.sort()
        return list_of_vars

    @property
    def all_data_accesses(self) -> List[Signature]:
        '''
        :returns: all Signatures in this instance that have a data access (i.e.
                  the data associated with them is read or written).
        '''
        result = []
        for sig in self.all_signatures:
            if self[sig].has_data_access():
                result.append(sig)
        return result

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
                                                  component_indices,
                                                  access_info.conditional)
        # Increase the current location of this instance by the amount of
        # locations just merged in
        self._location = self._location + max_new_location

    def is_called(self, signature: Signature) -> bool:
        '''
        :param signature: signature of the variable.

        :returns: True if the specified variable is called at least once.
        '''
        return self[signature].is_called()

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

    def is_read(self, signature) -> bool:
        '''Checks if the specified variable signature is at least read once.

        :param signature: signature of the variable
        :type signature: :py:class:`psyclone.core.Signature`

        :returns: True if the specified variable name is read (at least
            once).

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

    def set_conditional_accesses(self, node, if_branch, else_branch):
        '''This function adds the accesses from `if_branch` and `else_branch`
        as accesses as part of `node`, and if required marks these accesses
        as conditional.

        marking them as conditional if the accesses are already conditional,
        or only happen in one of the two branches. While this function is
        at the moment only used for if-statements, it can also be used for
        e.g. loops by providing None as `else_branch` object.

        :param if_branch: the first branch.
        :type if_branch: :py:class:`psyclone.psyir.nodes.Node`
        :param else_branch: the second branch, which can be None.
        :type else_branch: :py:class:`psyclone.psyir.nodes.Node`

        '''
        var_if = VariablesAccessInfo(if_branch, self.options())
        # Create an empty access info object in case that we do not have
        # a second branch.
        if else_branch:
            var_else = VariablesAccessInfo(else_branch, self.options())
        else:
            var_else = VariablesAccessInfo()

        # Get the list of all signatures in the if and else branch:
        all_sigs = set(var_if.keys())
        all_sigs.update(set(var_else.keys()))

        for sig in all_sigs:
            print("@@", sig)
            if sig not in var_if or sig not in var_else:
                # Access using this signature is only in one of the branches.
                # Add all these accesses as conditional accesses to the
                # specified node (exactly once in case of multiple accesses)
                done = []
                var_access = var_if[sig] if sig in var_if else var_else[sig]
                for access in var_access.all_accesses:
                    if any(access.component_indices.equal(i) for i in done):
                        # Duplicate, ignore
                        continue
                    self.add_access(sig, access.access_type, node,
                                    access.component_indices,
                                    conditional=True)
                    done.append(access.component_indices)
                continue

            # As a first step, split all the accesses into equivalence
            # classes (e.g. a(i) and a(i+1) are different, but a(1+i)
            # and a(i+2-1) are in the same class, since the indices are
            # mathematically equivalent)
            # Each equivalent class stores two lists as a pair: the
            # first one with the accesses from the if branch, the second with
            # the accesses from the else branch.

            equiv = self._equivalence_classes(var_if[sig].all_accesses,
                                              var_else[sig].all_accesses)

            print("===============================")
            # Now handle each equivalent set of component indices:
            for comp_index in equiv:
                if_accesses, else_accesses = equiv[comp_index]
                if not if_accesses or not else_accesses:
                    # Only accesses in one section, therefore conditional:
                    var_access = if_accesses if if_accesses else else_accesses

                    for access in var_access:
                        access.conditional = True
                        self.add_access(sig, access.access_type, node,
                                        access.component_indices,
                                        conditional=True)

                    continue

                # Now we have accesses to the same indices in both branches.
                # We still need to distinguish between read and write accesses.
                # This can result in incorrect/unexpected results in some rare
                # cases:
                # if ()
                #    call kernel(a(i))     ! Assume a(i) is READWRITE
                # else
                #    b = a(i)
                # endif
                # Now the read access to a(i) is unconditional, but the write
                # access to a(i) as part of the readwrite is conditional. But
                # since there is only one accesses for the readwrite, we can't
                # mark it as both conditional and unconditional
                conditional_in_if = True

                for mode in [AccessType.READ, AccessType.WRITE]:
                    for access in if_accesses:
                        # Ignore read or write accesses depending on mode
                        if mode is AccessType.READ and not access.is_read:
                            continue
                        if mode is AccessType.WRITE and not access.is_written:
                            continue
                        if not access.conditional:
                            conditional_in_if = False
                            break

                    overall_conditional = conditional_in_if
                    # If there is no conditional access in the if branch, there
                    # might still be one in the else branch, making the whole
                    # access conditional:
                    if not conditional_in_if:
                        # Assume that there is a conditional access in the else
                        # branch, unless we find an unconditional one
                        overall_conditional = True
                        for access in else_accesses:
                            # Ignore read or write accesses depending on mode
                            if mode is AccessType.READ and not access.is_read:
                                continue
                            if mode is AccessType.WRITE and \
                                    not access.is_written:
                                continue
                            if not access.conditional:
                                # We have an unconditional access, so know now
                                # that the access is unconditional:
                                overall_conditional = False
                                break

                    # If the access to this equivalence class is conditional,
                    # mark all accesses as conditional:
                    for access in if_accesses + else_accesses:
                        # Ignore read or write accesses depending on mode
                        if mode is AccessType.READ and not access.is_read:
                            continue
                        if mode is AccessType.WRITE and \
                                not access.is_written:
                            continue
                        access.conditional = overall_conditional
                        print("conditional" if overall_conditional
                              else "unconditional",
                              mode,
                              sig.to_language(component_indices=comp_index))
                print("-----------------------------")
        self.merge(var_if)
        self.merge(var_else)

    @staticmethod
    def _equivalence_classes(if_accesses, else_accesses) -> \
            Dict[ComponentIndices, tuple(List[SingleVariableAccessInfo],
                                         List[SingleVariableAccessInfo])]:
        '''This function is called when the specified signature is accessed
        in both the if and else block. In case of array variables, we need to
        distinguish between different indices, e.g. a(i) might be
        written to unconditionally, but a(i+1) might be written
        conditionally. Additionally, we should support mathematically
        equivalent statements (e.g. a(i+1), and a(1+i)).

        This function creates a dictionary. The keys are ComponentIndices
        The values for a given key is a Pair of lists, the first one containing
        all accesses to the same index in the if-branch, the second one
        containing all accesses to the same index in the else-branch.

        It returns dict[ComponentIndices, Pair(List[access], List[access])]
        '''
        equiv = {}
        for access in if_accesses:
            for comp_access in equiv:
                if access.component_indices.equal(comp_access):
                    equiv[comp_access][0].append(access)
                    break
            else:
                # New component index equivalence class
                equiv[access.component_indices] = ([access], [])
        # While we know that the signature is used in both branches, the
        # accesses for a given equivalence class of indices could still
        # be in only in one of them (e.g.
        # if () then a(i)=1 else a(i+1)=2 endif). The access to `i+1`
        # happens only in the else branch:
        for access in else_accesses:
            for comp_access in equiv:
                if access.component_indices.equal(comp_access):
                    equiv[comp_access][1].append(access)
                    break
            else:
                # New component index equivalence class:
                equiv[access.component_indices] = ([], [access])

        return equiv

    def _add_accesses_and_conditional_to_node(self):
        pass


# ---------- Documentation utils -------------------------------------------- #
# The list of module members that we wish AutoAPI to generate
# documentation for. (See https://psyclone-ref.readthedocs.io)
__all__ = ["VariablesAccessInfo"]
