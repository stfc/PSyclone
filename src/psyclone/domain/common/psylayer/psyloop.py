# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter, S. Siso  A. B. G. Chalk and N. Nobre,
#           STFC Daresbury Lab
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' This module contains the PSyLoop node implementation.'''

from psyclone.core import AccessType
from psyclone.psyir.nodes import Routine, Loop


class PSyLoop(Loop):
    # pylint: disable=too-many-instance-attributes
    '''Node representing a psylayer loop within the PSyIR. It extends the PSyIR
    loop construct with information about the domain-specific iteration space
    that the loop is traversing and utility methods to interact with other
    psylayer nodes.

    :param List[str] valid_loop_types: a list of loop types that are specific \
        to a particular API.
    :param kwargs: additional keyword arguments provided to the PSyIR node.
    :type kwargs: unwrapped dict.

    '''
    # Textual description of the node.
    _text_name = "Loop"
    _colour = "red"

    def __init__(self, valid_loop_types=None, **kwargs):
        super().__init__(**kwargs)

        if valid_loop_types is None:
            self._valid_loop_types = []
        else:
            self._valid_loop_types = valid_loop_types
        self._loop_type = None        # inner, outer, colour, colours, ...
        self._field = None
        self._field_name = None       # name of the field
        self._field_space = None      # v0, v1, ...,     cu, cv, ...
        self._iteration_space = None  # cells, ...,      cu, cv, ...
        self._kern = None             # Kernel associated with this loop
        # TODO 1731: _kern expects one kernel. What happens when we doo loop
        # fusion?

        # TODO 1731: replace iterates_over with iteration_space
        self._iterates_over = "unknown"

    def __eq__(self, other):
        '''
        Checks whether two nodes are equal. Two PSyLoop nodes are equal
        if they have equal loop_type, field, field_name, field_space
        iteraction_space and kernel.

        :param object other: the object to check equality to.

        :returns: whether other is equal to self.
        :rtype: bool
        '''
        is_eq = super().__eq__(other)
        is_eq = is_eq and self.loop_type == other.loop_type
        is_eq = is_eq and self.field == other.field
        is_eq = is_eq and self.field_name == other.field_name
        is_eq = is_eq and self.field_space == other.field_space
        is_eq = is_eq and self.iteration_space == other.iteration_space
        is_eq = is_eq and self.kernel == other.kernel
        # pylint: disable=protected-access
        is_eq = is_eq and self._iterates_over == other._iterates_over

        return is_eq

    @property
    def dag_name(self):
        '''
        :returns: the dag name to use for this loop.
        :rtype: str

        '''
        if self.loop_type:
            _, position = self._find_position(self.ancestor(Routine))
            name = f"loop_[{self.loop_type}]_{position}"
        else:
            name = super().dag_name
        return name

    @property
    def valid_loop_types(self):
        '''
        :returns: the (domain-specific) loop types allowed by this instance.
        :rtype: list of str
        '''
        return self._valid_loop_types

    @property
    def loop_type(self):
        '''
        :returns: the (domain-specific) type of this loop.
        :rtype: str
        '''
        return self._loop_type

    @loop_type.setter
    def loop_type(self, value):
        '''
        Set the type of this Loop.

        :param str value: the type of this loop.

        :raises TypeError: if the specified value is not a recognised \
            loop type.
        '''
        if value not in self._valid_loop_types:
            raise TypeError(
                f"Error, loop_type value ({value}) is invalid. Must be one of "
                f"{self._valid_loop_types}.")
        self._loop_type = value

    def node_str(self, colour=True):
        '''
        Returns the name of this node with (optional) control codes
        to generate coloured output in a terminal that supports it.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        :rtype: str

        '''
        return (f"{self.coloured_name(colour)}[type='{self._loop_type}', "
                f"field_space='{self._field_space}', "
                f"it_space='{self.iteration_space}']")

    # TODO 1731: The properties below should be dynamically computed instead
    # of storing an attribute that can become inconsistent with the kernel or
    # loop bounds.

    @property
    def field_space(self):
        '''
        :returns: the field_space associated this loop.
        :rtype: str
        '''
        return self._field_space

    @field_space.setter
    def field_space(self, my_field_space):
        ''' Set a new field_space for this loop.

        :param my_field_space: the new field_space value.
        :rtype my_field_space: str
        '''
        self._field_space = my_field_space

    @property
    def field_name(self):
        '''
        :returns: the field name associated to this loop.
        :rtype: str
        '''
        return self._field_name

    @property
    def field(self):
        '''
        :returns: the field associated to this loop.
        :rtype: :py:class:`psyclone.psyGen.Argument`
        '''
        return self._field

    @field_name.setter
    def field_name(self, my_field_name):
        ''' Set a new field_name for the field associated to this loop.

        :param str my_field_name: the new field name.
        '''
        self._field_name = my_field_name

    @property
    def iteration_space(self):
        '''
        :returns: the iteration_space of this loop.
        :rtype: str
        '''
        return self._iteration_space

    @iteration_space.setter
    def iteration_space(self, it_space):
        ''' Set a new iteration space for this loop.

        :param str it_space: the new iteration_space fore this loop.
        '''
        self._iteration_space = it_space

    @property
    def kernel(self):
        '''
        :returns: the kernel object associated with this PSyLoop (if any).
        :rtype: Optional[:py:class:`psyclone.psyGen.Kern`]
        '''
        return self._kern

    @kernel.setter
    def kernel(self, kern):
        '''
        Setter for kernel object associated with this PSyLoop.

        :param kern: a kernel object.
        :type kern: :py:class:`psyclone.psyGen.Kern`
        '''
        self._kern = kern

    def __str__(self):
        # Give Loop sub-classes a specialised name
        name = self.__class__.__name__
        result = name + "["
        result += "variable:'" + self.variable.name
        if self.loop_type:
            result += "', loop_type:'" + self._loop_type
        result += "']\n"
        for entity in self._children:
            result += str(entity) + "\n"
        result += "End " + name
        return result

    def has_inc_arg(self):
        '''
        :returns: True if any of the Kernels called within this loop have an \
                argument with INC access, False otherwise.
        :rtype: bool
        '''
        for kern_call in self.coded_kernels():
            for arg in kern_call.arguments.args:
                if arg.access == AccessType.INC:
                    return True
        return False

    def unique_modified_args(self, arg_type):
        '''Return all unique arguments of the given type from kernels inside
        this loop that are modified.

        :param str arg_type: the type of kernel argument (e.g. field, \
                             operator) to search for.
        :returns: all unique arguments of the given type from kernels inside \
            this loop that are modified.
        :rtype: List[:py:class:`psyclone.psyGen.DynKernelArgument`]
        '''
        arg_names = []
        args = []
        for call in self.kernels():
            for arg in call.arguments.args:
                if arg.argument_type.lower() == arg_type:
                    if arg.access != AccessType.READ:
                        if arg.name not in arg_names:
                            arg_names.append(arg.name)
                            args.append(arg)
        return args

    def unique_fields_with_halo_reads(self):
        '''
        :returns: fields in this loop that require at least some of their \
            halo to be clean to work correctly.
        :rtype: List[:py:class:`psyclone.psyGen.Argument`]
        '''

        unique_fields = []
        unique_field_names = []

        for call in self.kernels():
            for arg in call.arguments.args:
                if self._halo_read_access(arg):
                    if arg.name not in unique_field_names:
                        unique_field_names.append(arg.name)
                        unique_fields.append(arg)
        return unique_fields

    def args_filter(self, arg_types=None, arg_accesses=None, unique=False):
        '''
        :returns: all arguments of type arg_types and arg_accesses. If these \
            are not set then return all arguments. If unique is set to \
            True then only return uniquely named arguments.
        :rtype: List[:py:class:`psyclone.psyGen.Argument`]
        '''
        # Avoid circular import
        # pylint: disable=import-outside-toplevel
        from psyclone.psyGen import args_filter
        all_args = []
        all_arg_names = []
        for call in self.kernels():
            call_args = args_filter(call.arguments.args, arg_types,
                                    arg_accesses)
            if unique:
                for arg in call_args:
                    if arg.name not in all_arg_names:
                        all_args.append(arg)
                        all_arg_names.append(arg.name)
            else:
                all_args.extend(call_args)
        return all_args

    def gen_mark_halos_clean_dirty(self, parent):
        '''
        Generates the necessary code to mark halo regions as clean or dirty
        following execution of this loop. This default implementation does
        nothing.

        TODO #1648 - this method should be removed when the corresponding
        one in DynLoop is removed.

        :param parent: the node in the f2pygen AST to which to add content.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        '''

    def _halo_read_access(self, arg):
        '''Determines whether the supplied argument has (or might have) its
        halo data read within this loop. Returns True if it does, or if
        it might and False if it definitely does not.

        :param arg: an argument contained within this loop.
        :type arg: :py:class:`psyclone.psyGen.KernelArgument`

        :return: True if the argument reads, or might read from the \
                 halo and False otherwise.
        :rtype: bool

        :raises NotImplementedError: This is an abstract method.

        '''
        raise NotImplementedError("This method needs to be implemented by the "
                                  "APIs that support distributed memory.")
