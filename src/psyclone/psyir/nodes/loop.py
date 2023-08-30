# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2023, Science and Technology Facilities Council.
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

    (Note: Loop only represents the equivalent to Fortran counted do loops.
    This means the loop is bounded by start/stop/step expressions evaluated
    before the loop starts. See WhileLoop for while loops, including the
    Fortran do while and do loop with no condition.)

    :param variable: optional reference to the loop iterator \
        variable. Defaults to None.
    :type variable: Optional[:py:class:`psyclone.psyir.symbols.DataSymbol`]
    :param annotations: One or more labels that provide additional information\
        about the node (primarily relating to the input code that it was \
        created from).
    :type annotations: Optional[List[str]]
    :param kwargs: additional keyword arguments provided to the PSyIR node.
    :type kwargs: unwrapped dict.

    :raises InternalError: if the 'was_single_stmt' annotation is supplied \
                           without the 'was_where' annotation.

    '''
    valid_annotations = ('was_where', 'was_single_stmt', 'chunked')
    # Textual description of the node.
    _children_valid_format = "DataNode, DataNode, DataNode, Schedule"
    _text_name = "Loop"
    _colour = "red"

    def __init__(self, variable=None, annotations=None, **kwargs):
        # Although the base class checks on the annotations individually, we
        # need to do further checks here
        if annotations:
            if 'was_single_stmt' in annotations and \
               'was_where' not in annotations:
                raise InternalError(
                    f"A Loop with the 'was_single_stmt' annotation "
                    f"must also have the 'was_where' annotation but"
                    f" got: {annotations}")

        super().__init__(self, annotations=annotations, **kwargs)
        # Call the variable setter for error checking
        self._variable = None
        if variable is not None:
            self.variable = variable

    def __eq__(self, other):
        '''
        Checks whether two nodes are equal. Two Loop nodes are equal
        if they have the same iteration variable and their children are
        equal.

        :param object other: the object to check equality to.

        :returns: whether other is equal to self.
        :rtype: bool
        '''
        is_eq = super().__eq__(other)
        # Similar to Reference equality, it is enough to compare the name
        # since if the same-named symbols represent the same is already
        # done in their respective scope symbol_table equality check.
        is_eq = is_eq and self.variable.name == other.variable.name

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

    @classmethod
    def create(cls, variable, start, stop, step, children):
        # pylint: disable=too-many-arguments
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
        cls._check_variable(variable)

        if not isinstance(children, list):
            raise GenerationError(
                f"children argument in create method of Loop class "
                f"should be a list but found '{type(children).__name__}'.")

        loop = cls(variable=variable)
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

        return "loop_" + str(position)

    def node_str(self, colour=True):
        '''
        Returns the name of this node with (optional) control codes
        to generate coloured output in a terminal that supports it.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        :rtype: str

        '''
        return f"{self.coloured_name(colour)}[variable='{self.variable.name}']"

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
            :py:class:`psyclone.core.VariablesAccessInfo`
        '''

        # Only add the loop variable and start/stop/step values if this is
        # not an LFRic domain loop. We need to access the variable directly
        # to avoid a crash in the getter if the loop variable is not defined.
        if self._variable:
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

    # -------------------------------------------------------------------------
    def _is_loop_suitable_for_parallel(self, only_nested_loops=True):
        '''Simple first test to see if a loop should even be considered to
        be parallelised. The default implementation tests whether the loop
        has a certain type (e.g. 'latitude'), and optional tests for
        nested loops (to avoid parallelising single loops which will likely
        result in a slowdown due to thread synchronisation costs). This
        function is used by can_loop_be_parallelised() and can of course be
        overwritten by the user to implement different suitability criteria.

        :param loop: the loop to test.
        :type loop: :py:class:`psyclone.psyir.nodes.Loop`
        :param bool only_nested_loops: true (default) if only nested loops\
                                        should be considered.

        :returns: true if the loop fulfills the requirements.
        :rtype: bool
        '''
        if only_nested_loops:
            all_loops = self.walk(Loop)
            if len(all_loops) == 1:
                self._add_message("Not a nested loop.",
                                  DTCode.INFO_NOT_NESTED_LOOP)
                return False

        if self._loop_types_to_parallelise:
            if self.loop_type not in self._loop_types_to_parallelise:
                self._add_message(f"Loop has wrong loop type '"
                                  f"{self.loop_type}'.",
                                  DTCode.INFO_WRONG_LOOP_TYPE)
                return False
        return True

    def _array_access_parallelisable(self, loop_variables, var_info):
        '''Tries to determine if the access pattern for an array
        given in `var_info` allows parallelisation along the first variable
        in `loop_variables`. The other elements of `loop_variables` specify
        the loop variables of any inner loops. This implementation follows:

        "Optimizing compilers for modern architectures -
        a dependence-based approach
        Ken Kennedy and John R. Allen. 2001.
        Morgan Kaufmann Publishers Inc., San Francisco, CA, USA."

        Chapter 3.6 "Dependency Testing - Putting it all Together"
        But many of the more advanced tests (e.g. multi-variable ones)
        are not (yet) implemented, since it appears unlikely that they
        will occur in 'real' code. But the outline of this testing follows
        the full structure  When we use the same function for other tests
        (loop fusion etc), the subroutines here can be generalised to use
        the dependency direction (i.e. "<", "=", ">") and instead of
        checking for distance 0 then check for direction being only "="").

        De-facto all that is happening atm is to try to find one subscript
        that is guaranteed to be independent, which is all that is needed
        for parallelisation.

        :param loop_variables: the list of all loop variables in the code to \
            be parallelised. The first one must be the loop to be \
            parallelised (a possible outer loop does not matter, the value of \
            the loop variable is a constant within the loop to be parallelised.
        :type loop_variables: List[str]
        :param var_info: access information for this variable.
        :type var_info: \
            :py:class:`psyclone.core.SingleVariableAccessInfo`

        :return: whether the variable can be used in parallel.
        :rtype: bool

        '''
        # pylint: disable=too-many-locals
        # If a variable is read-only, it can be parallelised
        if var_info.is_read_only():
            return True

        all_write_accesses = var_info.all_write_accesses

        for write_access in all_write_accesses:
            # We need to compare each write access with any other access,
            # including itself (to detect write-write race conditions:
            # a((i-2)**2) = b(i): i=1 and i=3 would write to a(1))
            for other_access in var_info:
                if not self._is_loop_carried_dependency(loop_variables,
                                                        write_access,
                                                        other_access):
                    # There is a dependency. Try to give precise error
                    # messages:
                    # We need to use default parameters, since otherwise
                    # the value of a variable might be different when
                    # the message is actually evaluated.
                    # Some pylint version complain here (because of the
                    # above). The code is correct, so disable this
                    # message:
                    # pylint: disable=cell-var-from-loop
                    if write_access is other_access:
                        # The write access has a dependency on itself, e.g.
                        # a(3) = ...    or a((i-2)**2) = ...
                        # Both would result in a write-write conflict
                        node = write_access.node
                        self._add_message(LazyString(
                            lambda node=write_access.node:
                                (f"The write access to '"
                                 f"{node.debug_string()}' causes "
                                 f"a write-write race condition.")),
                            DTCode.ERROR_WRITE_WRITE_RACE,
                            [LazyString(lambda node=node:
                                        f"{node.debug_string()}")])
                    else:
                        self._add_message(LazyString(
                            lambda wnode=write_access.node,
                            onode=other_access.node:
                                (f"The write access to "
                                 f"'{wnode.debug_string()}' "
                                 f"and to '{onode.debug_string()}"
                                 f"' are dependent and cannot be "
                                 f"parallelised.")),
                            DTCode.ERROR_DEPENDENCY,
                            [LazyString(lambda wnode=write_access.node:
                                        f"{wnode.debug_string()}"
                                        ),
                             LazyString(lambda onode=other_access.node:
                                        f"{onode.debug_string()}")])

                    return False
        return True

    # -------------------------------------------------------------------------
    def _is_scalar_parallelisable(self, var_info):
        '''Checks if the accesses to the given scalar variable can be
        parallelised, i.e. it is not a reduction.

        :param var_info: the access information for the variable to test.
        :type var_info: :py:class:`psyclone.core.var_info.VariableInfo`
        :return: True if the scalar variable is not a reduction, i.e. it \
            can be parallelised.
        :rtype: bool
        '''

        # Read only scalar variables can be parallelised
        if var_info.is_read_only():
            return True

        all_accesses = var_info.all_accesses
        if len(all_accesses) == 1:
            # The variable is used only once. Either it is a read-only
            # variable, or it is supposed to store the result from the loop to
            # be used outside of the loop (or it is bad code). Read-only access
            # has already been tested above, so it must be a write access here,
            # which prohibits parallelisation.
            # We could potentially use lastprivate here?
            self._add_message(f"Scalar variable '{var_info.var_name}' is "
                              f"only written once.",
                              DTCode.WARN_SCALAR_WRITTEN_ONCE,
                              [f"{var_info.var_name}"])
            return False

        # Now we have at least two accesses. If the first access is a WRITE,
        # then the variable is not used in a reduction. This relies on sorting
        # the accesses by location. Note that an argument to a kernel can have
        # a 'READWRITE' access because, in that case, all we know is what the
        # kernel metadata tells us. However, we do know that such an access is
        # *not* a reduction because that would have 'SUM' access.
        if all_accesses[0].access_type in (AccessType.WRITE,
                                           AccessType.READWRITE):
            return True

        # Otherwise there is a read first, which would indicate that this loop
        # is a reduction, which is not supported atm.
        self._add_message(f"Variable '{var_info.var_name}' is read first, "
                          f"which indicates a reduction.",
                          DTCode.WARN_SCALAR_REDUCTION,
                          [var_info.var_name])
        return False

    def can_be_parallelised(self,
                            only_nested_loops=True,
                            test_all_variables=False,
                            signatures_to_ignore=None):
        # pylint: disable=too-many-branches,too-many-locals
        '''This function analyses a loop in the PsyIR to see if
        it can be safely parallelised over the specified variable.

        :param bool only_nested_loops: if True, a loop must have an inner\
                                       loop in order to be considered\
                                       parallelisable (default: True).
        :param bool test_all_variables: if True, it will test if all variable\
                                        accesses can be parallelised,\
                                        otherwise it will stop after the first\
                                        variable is found that can not be\
                                        parallelised.
        :param signatures_to_ignore: list of signatures for which to skip \
                                     the access checks.
        :type signatures_to_ignore: list of :py:class:`psyclone.core.Signature`

        :returns: True if the loop can be parallelised.
        :rtype: bool

        '''
        #self._clear_messages()

        # Check if the loop type should be parallelised, e.g. to avoid
        # parallelising inner loops which might not have enough work. This
        # is supposed to be a fast first check to avoid collecting variable
        # accesses in some unsuitable loops.
        if not self._is_loop_suitable_for_parallel(only_nested_loops):
            # Appropriate messages will have been added already, so just exit
            return False

        var_accesses = VariablesAccessInfo(self)
        if not signatures_to_ignore:
            signatures_to_ignore = []

        # Collect all variables used as loop variable:
        loop_vars = [loop.variable.name for loop in self.walk(Loop)]

        result = True
        # Now check all variables used in the loop
        for signature in var_accesses.all_signatures:
            # This string contains derived type information, e.g.
            # "a%b"
            var_string = str(signature)
            # Ignore all loop variables - they look like reductions because of
            # the write-read access in the loop:
            if var_string in loop_vars:
                continue
            if signature in signatures_to_ignore:
                continue

            # This returns the first component of the signature,
            # i.e. in case of "a%b" it will only return "a"
            var_name = signature.var_name
            var_info = var_accesses[signature]
            symbol_table = self.scope.symbol_table
            symbol = symbol_table.lookup(var_name)
            # TODO #1270 - the is_array_access function might be moved
            is_array = symbol.is_array_access(access_info=var_info)
            if is_array:
                # Handle arrays
                par_able = self._array_access_parallelisable(loop_vars,
                                                             var_info)
            else:
                # Handle scalar variable
                par_able = self._is_scalar_parallelisable(var_info)
            if not par_able:
                if not test_all_variables:
                    return False
                # The user might have requested to continue in order to get
                # all messages for all variables preventing parallelisation,
                # not just the first one
                result = False

        return result

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
