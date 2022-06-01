# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2022, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
#         I. Kavcic, Met Office
#         J. Henrichs, Bureau of Meteorology
# Modified A. B. G. Chalk, STFC Daresbury Lab
# -----------------------------------------------------------------------------

''' This module contains the Loop node implementation.'''

from psyclone.psyir.nodes.datanode import DataNode
from psyclone.psyir.nodes.statement import Statement
from psyclone.psyir.nodes.routine import Routine
from psyclone.psyir.nodes import Schedule, Literal
from psyclone.psyir.symbols import ScalarType, DataSymbol
from psyclone.core import AccessType, Signature
from psyclone.errors import InternalError, GenerationError
from psyclone.f2pygen import DoGen, DeclGen


class Loop(Statement):
    # pylint: disable=too-many-instance-attributes
    '''Node representing a loop within the PSyIR. It has 4 mandatory children:
    the first one represents the loop lower bound, the second one represents
    the loop upper bound, the third one represents the step value and the
    fourth one is always a PSyIR Schedule node containing the statements inside
    the loop body.

    (Note: currently this loop only represents the equivalent to Fortran do
    loops. This means the loop is bounded by start/stop/step expressions
    evaluated before the loop starts.)

    :param parent: parent of this node in the PSyIR.
    :type parent: sub-class of :py:class:`psyclone.psyir.nodes.Node`
    :param variable: optional reference to the loop iterator \
        variable. Defaults to None.
    :type variable: :py:class:`psyclone.psyir.symbols.DataSymbol` or \
        `NoneType`
    :param valid_loop_types: a list of loop types that are specific \
        to a particular API.
    :type valid_loop_types: list of str
    :param annotations: One or more labels that provide additional information\
          about the node (primarily relating to the input code that it was \
          created from).
    :type annotations: list of str

    :raises InternalError: if the 'was_single_stmt' annotation is supplied \
                           without the 'was_where' annotation.

    '''
    valid_annotations = ('was_where', 'was_single_stmt', 'chunked')
    # Textual description of the node.
    _children_valid_format = "DataNode, DataNode, DataNode, Schedule"
    _text_name = "Loop"
    _colour = "red"

    def __init__(self, parent=None, variable=None, valid_loop_types=None,
                 annotations=None):
        super(Loop, self).__init__(self, parent=parent,
                                   annotations=annotations)
        # Although the base class checks on the annotations individually, we
        # need to do further checks here
        if annotations:
            if 'was_single_stmt' in annotations and \
               'was_where' not in annotations:
                raise InternalError(
                    f"A Loop with the 'was_single_stmt' annotation "
                    f"must also have the 'was_where' annotation but"
                    f" got: {annotations}")

        # we need to determine whether this is a built-in or kernel
        # call so our schedule can do the right thing.

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

        # TODO replace iterates_over with iteration_space
        self._iterates_over = "unknown"

        if variable:
            # The variable might not be provided when the loop is
            # first created so only check if it is.
            self._check_variable(variable)
        self._variable = variable

    def __eq__(self, other):
        '''
        Checks whether two nodes are equal. Two Loop nodes are equal
        if they have equal loop_type, field, field_name, field_space
        iteraction_space, kernel and variable.

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

        is_eq = is_eq and self.variable == other.variable

        return is_eq

    @staticmethod
    def _check_variable(variable):
        '''The loop variable should be a scalar integer. Check that this is
        the case and raise an exception if not.

        :param variable: the loop iterator.
        :type variable: :py:class:`psyclone.psyir.symbols.DataSymbol`

        :raises GenerationError: if the supplied variable is not a \
            scalar integer.

        '''
        if not isinstance(variable, DataSymbol):
            raise GenerationError(
                f"variable property in Loop class should be a DataSymbol but "
                f"found '{type(variable).__name__}'.")
        if not isinstance(variable.datatype, ScalarType):
            raise GenerationError(
                f"variable property in Loop class should be a ScalarType but "
                f"found '{type(variable.datatype).__name__}'.")
        if variable.datatype.intrinsic != ScalarType.Intrinsic.INTEGER:
            raise GenerationError(
                f"variable property in Loop class should be a scalar integer "
                f"but found '{variable.datatype.intrinsic.name}'.")

    @staticmethod
    def _validate_child(position, child):
        '''
        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        return (position in (0, 1, 2) and isinstance(child, DataNode)) or (
            position == 3 and isinstance(child, Schedule))

    @staticmethod
    def create(variable, start, stop, step, children):
        '''Create a Loop instance given valid instances of a variable,
        start, stop and step nodes, and a list of child nodes for the
        loop body.

        :param variable: the PSyIR node containing the variable \
            of the loop iterator.
        :type variable: :py:class:`psyclone.psyir.symbols.DataSymbol`
        :param start: the PSyIR node determining the value for the \
            start of the loop.
        :type start: :py:class:`psyclone.psyir.nodes.Node`
        :param end: the PSyIR node determining the value for the end \
            of the loop.
        :type end: :py:class:`psyclone.psyir.nodes.Node`
        :param step: the PSyIR node determining the value for the loop \
            step.
        :type step: :py:class:`psyclone.psyir.nodes.Node`
        :param children: a list of PSyIR nodes contained in the \
            loop.
        :type children: list of :py:class:`psyclone.psyir.nodes.Node`

        :returns: a Loop instance.
        :rtype: :py:class:`psyclone.psyir.nodes.Loop`

        :raises GenerationError: if the arguments to the create method \
            are not of the expected type.

        '''
        Loop._check_variable(variable)

        if not isinstance(children, list):
            raise GenerationError(
                f"children argument in create method of Loop class "
                f"should be a list but found '{type(children).__name__}'.")

        loop = Loop(variable=variable)
        schedule = Schedule(parent=loop, children=children)
        loop.children = [start, stop, step, schedule]
        return loop

    def _check_completeness(self):
        ''' Check that the Loop has 4 children and the 4th is a Schedule.

        :raises InternalError: If the loop does not have 4 children or the
            4th one is not a Schedule
        '''
        # We cannot just do str(self) in this routine we can end up being
        # called as a result of str(self) higher up the call stack
        # (because loop bounds are evaluated dynamically).
        if len(self.children) < 4:
            raise InternalError(
                f"Loop is incomplete. It should have exactly 4 "
                f"children, but found loop with "
                f"'{', '.join([str(child) for child in self.children])}'.")

    @property
    def start_expr(self):
        '''
        :returns: the PSyIR Node representing the Loop start expression.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        '''
        self._check_completeness()
        return self._children[0]

    @start_expr.setter
    def start_expr(self, expr):
        ''' Setter for Loop start_expr attribute.

        :param expr: New PSyIR start expression.
        :type expr: :py:class:`psyclone.psyir.nodes.Node`

        '''
        self._check_completeness()
        self._children[0] = expr

    @property
    def stop_expr(self):
        '''
        :returns: the PSyIR Node representing the Loop stop expression.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        '''
        self._check_completeness()
        return self._children[1]

    @stop_expr.setter
    def stop_expr(self, expr):
        ''' Setter for Loop stop_expr attribute.

        :param expr: New PSyIR stop expression.
        :type expr: :py:class:`psyclone.psyir.nodes.Node`

        '''
        self._check_completeness()
        self._children[1] = expr

    @property
    def step_expr(self):
        '''
        :returns: the PSyIR Node representing the Loop step expression.
        :rtype: :py:class:`psyclone.psyir.nodes.Node`

        '''
        self._check_completeness()
        return self._children[2]

    @step_expr.setter
    def step_expr(self, expr):
        ''' Setter for Loop step_expr attribute.

        :param expr: New PSyIR step expression.
        :type expr: :py:class:`psyclone.psyir.nodes.Node`

        '''
        self._check_completeness()
        self._children[2] = expr

    @property
    def loop_body(self):
        '''
        :returns: the PSyIR Schedule with the loop body statements.
        :rtype: :py:class:`psyclone.psyir.nodes.Schedule`

        '''
        self._check_completeness()
        return self._children[3]

    @property
    def dag_name(self):
        ''' Return the name to use in a dag for this node

        :returns: Return the dag name for this loop
        :rtype: string

        '''
        _, position = self._find_position(self.ancestor(Routine))

        if self.loop_type:
            name = f"loop_[{self.loop_type}]_{position}"
        else:
            name = "loop_" + str(position)
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
        :raises GenerationError: if the specified value is not a recognised \
                                 loop type.
        '''
        if value not in self._valid_loop_types:
            raise GenerationError(
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

    @property
    def field_space(self):
        return self._field_space

    @field_space.setter
    def field_space(self, my_field_space):
        self._field_space = my_field_space

    @property
    def field_name(self):
        return self._field_name

    @property
    def field(self):
        return self._field

    @field_name.setter
    def field_name(self, my_field_name):
        self._field_name = my_field_name

    @property
    def iteration_space(self):
        return self._iteration_space

    @iteration_space.setter
    def iteration_space(self, it_space):
        self._iteration_space = it_space

    @property
    def kernel(self):
        '''
        :returns: the kernel object associated with this Loop (if any).
        :rtype: :py:class:`psyclone.psyGen.Kern`
        '''
        return self._kern

    @kernel.setter
    def kernel(self, kern):
        '''
        Setter for kernel object associated with this loop.

        :param kern: a kernel object.
        :type kern: :py:class:`psyclone.psyGen.Kern`
        '''
        self._kern = kern

    @property
    def variable(self):
        '''
        :returns: a reference to the control variable for this loop.
        :rtype: :py:class:`psyclone.psyir.symbols.DataSymbol`
        '''
        self._check_variable(self._variable)
        return self._variable

    @variable.setter
    def variable(self, var):
        '''
        Setter for the variable associated with this loop.

        :param var: the control variable reference.
        :type var: :py:class:`psyclone.psyir.symbols.DataSymbol`

        '''
        self._check_variable(var)
        self._variable = var

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

    def reference_accesses(self, var_accesses):
        '''Get all variable access information. It combines the data from
        the loop bounds (start, stop and step), as well as the loop body.
        The loop variable is marked as 'READ+WRITE' and references in start,
        stop and step are marked as 'READ'.

        :param var_accesses: VariablesAccessInfo instance that stores the \
            information about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`
        '''

        # It is important to first add the WRITE access, since this way
        # the dependency analysis for declaring openmp private variables
        # will automatically declare the loop variables to be private
        # (write access before read)
        var_accesses.add_access(Signature(self.variable.name),
                                AccessType.WRITE, self)
        var_accesses.add_access(Signature(self.variable.name),
                                AccessType.READ, self)

        # Accesses of the start/stop/step expressions
        self.start_expr.reference_accesses(var_accesses)
        self.stop_expr.reference_accesses(var_accesses)
        self.step_expr.reference_accesses(var_accesses)
        var_accesses.next_location()

        for child in self.loop_body.children:
            child.reference_accesses(var_accesses)
            var_accesses.next_location()

    def has_inc_arg(self):
        ''' Returns True if any of the Kernels called within this
        loop have an argument with INC access. Returns False otherwise '''
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
        :rtype: list of :py:class:`psyclone.psyGen.DynKernelArgument`
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
        ''' Returns all fields in this loop that require at least some
        of their halo to be clean to work correctly.

        :returns: fields in this loop that require at least some of their \
            halo to be clean to work correctly.
        :rtype: list of :py:class:`psyclone.psyGen.Argument`
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
        '''Return all arguments of type arg_types and arg_accesses. If these
        are not set then return all arguments. If unique is set to
        True then only return uniquely named arguments'''
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

    def gen_code(self, parent):
        '''
        Generate the Fortran Loop and any associated code.

        :param parent: the node in the f2pygen AST to which to add content.
        :type parent: :py:class:`psyclone.f2pygen.SubroutineGen`

        '''
        # Avoid circular dependency
        # pylint: disable=import-outside-toplevel
        from psyclone.psyGen import zero_reduction_variables

        def is_unit_literal(expr):
            ''' Check if the given expression is equal to the literal '1'.

            :param expr: a PSyIR expression.
            :type expr: :py:class:`psyclone.psyir.nodes.Node`

            :returns: True if it is equal to the literal '1', false otherwise.
            '''
            return isinstance(expr, Literal) and expr.value == '1'

        if not self.is_openmp_parallel():
            calls = self.reductions()
            zero_reduction_variables(calls, parent)

        # Avoid circular dependency
        # pylint: disable=import-outside-toplevel
        from psyclone.psyir.backend.fortran import FortranWriter
        # start/stop/step_expr are generated with the FortranWriter
        # backend, the rest of the loop with f2pygen.
        fwriter = FortranWriter()
        if is_unit_literal(self.step_expr):
            step_str = None
        else:
            step_str = fwriter(self.step_expr)

        do_stmt = DoGen(parent, self.variable.name,
                        fwriter(self.start_expr),
                        fwriter(self.stop_expr),
                        step_str)
        # need to add do loop before children as children may want to add
        # info outside of do loop
        parent.add(do_stmt)
        for child in self.loop_body:
            child.gen_code(do_stmt)
        my_decl = DeclGen(parent, datatype="integer",
                          entity_decls=[self.variable.name])
        parent.add(my_decl)

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
