# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2025, Science and Technology Facilities Council.
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

from psyclone.core import VariablesAccessMap
from psyclone.psyir.nodes.datanode import DataNode
from psyclone.psyir.nodes.statement import Statement
from psyclone.psyir.nodes.routine import Routine
from psyclone.psyir.nodes import Schedule
from psyclone.psyir.symbols import DataSymbol, ScalarType, Symbol
from psyclone.core import AccessType, Signature
from psyclone.errors import InternalError, GenerationError


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

    # Set of rules that give a loop a certain loop_type by inspecting
    # its variable name
    _loop_type_inference_rules = {}

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
        # Hold the set of symbols that will be private/local to the iteration
        # if this loop is run concurrently. Alternatively this could be
        # implemented by moving the symbols to the loop_body symbol table.
        self._explicitly_private_symbols = set()

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

    @property
    def explicitly_private_symbols(self):
        '''
        :returns: the set of symbols inside the loop which are private to each
            iteration of the loop if it is executed concurrently.
        :rtype: Set[:py:class:`psyclone.psyir.symbols.DataSymbol`]
        '''
        return self._explicitly_private_symbols

    @property
    def loop_type(self):
        '''
        :returns: the type of this loop, if set.
        :rtype: Optional[str]
        '''
        if not self._variable:
            return None
        return self._loop_type_inference_rules.get(self.variable.name, None)

    @classmethod
    def set_loop_type_inference_rules(cls, rules):
        '''
        Specify the rules that define a loop type by inspecting its variable,
        name. This affects all instances of the Loop class. For example:

        .. code-block::

            rules = {
                "lon": {"variable": "ji"},
                "lat": {"variable": "jj"}
            }

        :param rules: new set of rules for inferring loop_types.
        :type rules: dict[str, dict[str, str]]
        '''
        if rules is None:
            cls._loop_type_inference_rules = {}
            return

        # Check that the provided rules have the right format
        if not isinstance(rules, dict):
            raise TypeError(f"The rules argument must be of type 'dict' but "
                            f"found '{type(rules)}'.")
        for key, rule in rules.items():
            if not isinstance(key, str):
                raise TypeError(f"The rules keys must be of type 'str' but "
                                f"found '{type(key)}'.")
            if not isinstance(rule, dict):
                raise TypeError(f"The rules values must be of type 'dict' but "
                                f"found '{type(rule)}'.")
            for rkey, value in rule.items():
                if not isinstance(rkey, str) or not isinstance(value, str):
                    raise TypeError(
                        f"All the values of the rule definition must be "
                        f"of type 'str' but found '{rule}'.")
                if rkey != "variable":
                    raise TypeError(f"Currently only the 'variable' rule key"
                                    f" is accepted, but found: '{rkey}'.")
            if "variable" not in rule:
                raise TypeError(f"A rule must at least have a 'variable' field"
                                f" to specify the loop variable name that "
                                f"defines this loop_type, but the rule for "
                                f"'{key}' does not have it.")

        # Convert the rules to a dictionary with variable as a key
        inference_rules = {}
        for key, rule in rules.items():
            inference_rules[rule["variable"]] = key
        cls._loop_type_inference_rules = inference_rules

    @staticmethod
    def _check_variable(variable):
        '''The loop variable should be a scalar integer. Check that this is
        the case and raise an exception if not.

        :param variable: the loop iterator.
        :type variable: :py:class:`psyclone.psyir.symbols.DataSymbol`

        :raises GenerationError: if the supplied variable is not a \
            scalar integer.

        '''
        try:
            variable_name = f"'{variable.name}'"
        except AttributeError:
            variable_name = "property"
        if not isinstance(variable, DataSymbol):
            raise GenerationError(
                f"variable {variable_name} in Loop class should be a "
                f"DataSymbol but found '{type(variable).__name__}'.")
        if not isinstance(variable.datatype, ScalarType):
            raise GenerationError(
                f"variable {variable_name} in Loop class should be a "
                f"ScalarType but found '{type(variable.datatype).__name__}'.")
        if variable.datatype.intrinsic != ScalarType.Intrinsic.INTEGER:
            raise GenerationError(
                f"variable {variable_name} in Loop class should be a "
                f"scalar integer but found "
                f"'{variable.datatype.intrinsic.name}'.")

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

        :raises InternalError: if this Loop has no ancestor Routine.

        '''
        routine = self.ancestor(Routine)
        if not routine:
            raise InternalError(f"Cannot generate DAG name for loop node "
                                f"'{self}' because it is not contained within "
                                f"a Routine.")
        _, position = self._find_position(routine)

        return "loop_" + str(position)

    def node_str(self, colour=True):
        '''
        Returns the name of this node with (optional) control codes
        to generate coloured output in a terminal that supports it.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        :rtype: str

        '''
        result = f"{self.coloured_name(colour)}["
        result += f"variable='{self.variable.name}'"
        if self.loop_type:
            result += f", loop_type='{self.loop_type}'"
        return result + "]"

    @property
    def variable(self):
        '''
        :returns: the control variable for this loop.
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

    def replace_symbols_using(self, table_or_symbol):
        '''
        Replace the Symbol referred to by this object's `variable` and
        `explicit_local_symbols` properties with those in the supplied
        SymbolTable (or just the supplied Symbol instance) if they
        have matching names. If there is no match for a given Symbol then it
        is left unchanged.

        :param table_or_symbol: the symbol table from which to get replacement
            symbols or a single, replacement Symbol.
        :type table_or_symbol: :py:class:`psyclone.psyir.symbols.SymbolTable` |
            :py:class:`psyclone.psyir.symbols.Symbol`

        '''
        if self._variable:
            if isinstance(table_or_symbol, Symbol):
                if table_or_symbol.name.lower() == self._variable.name.lower():
                    self._variable = table_or_symbol
            else:
                try:
                    new_sym = table_or_symbol.lookup(self._variable.name)
                    self.variable = new_sym
                except KeyError:
                    pass

        for symbol in list(self._explicitly_private_symbols):
            if isinstance(table_or_symbol, Symbol):
                if table_or_symbol.name.lower() == symbol.name.lower():
                    self._explicitly_private_symbols.remove(symbol)
                    self._explicitly_private_symbols.add(table_or_symbol)
            else:
                try:
                    new_sym = table_or_symbol.lookup(symbol.name)
                    self._explicitly_private_symbols.remove(symbol)
                    self._explicitly_private_symbols.add(new_sym)
                except KeyError:
                    pass
        super().replace_symbols_using(table_or_symbol)

    def __str__(self):
        # Give Loop sub-classes a specialised name
        name = self.__class__.__name__
        result = name + "["
        if self._variable:
            result += f"variable:'{self.variable.name}'"
        else:
            result += "variable:None"
        if self.loop_type:
            result += f", loop_type:'{self.loop_type}'"
        if self._explicitly_private_symbols:
            sym_names = sorted([symbol.name for symbol in
                                self._explicitly_private_symbols])
            result += f", explicit_private_symbols:{sym_names}"
        result += "]\n"
        for entity in self._children:
            result += str(entity) + "\n"
        result += "End " + name
        return result

    def get_all_accessed_symbols(self) -> set[Symbol]:
        '''
        :returns: a set of all the symbols accessed inside this Loop.
        '''
        symbols = super().get_all_accessed_symbols()
        if self.variable:
            # TODO #3124: This is needed because the loop variable reference
            # is not part of the tree
            symbols.add(self.variable)
        return symbols

    def reference_accesses(self) -> VariablesAccessMap:
        '''
        :returns: a map of all the symbol accessed inside this node, the
            keys are Signatures (unique identifiers to a symbol and its
            structure acccessors) and the values are AccessSequence
            (a sequence of AccessTypes).

        '''
        var_accesses = VariablesAccessMap()

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
            var_accesses.update(self.start_expr.reference_accesses())
            var_accesses.update(self.stop_expr.reference_accesses())
            var_accesses.update(self.step_expr.reference_accesses())

        for child in self.loop_body.children:
            var_accesses.update(child.reference_accesses())
        return var_accesses

    def independent_iterations(self,
                               test_all_variables=False,
                               signatures_to_ignore=None,
                               dep_tools=None):
        '''This function analyses a loop in the PSyIR to see whether
        its iterations are independent.

        :param bool test_all_variables: if True, it will test if all variable
            accesses are independent, otherwise it will stop after the first
            variable access is found that isn't.
        :param signatures_to_ignore: list of signatures for which to skip
            the access checks.
        :type signatures_to_ignore: Optional[
            List[:py:class:`psyclone.core.Signature`]]
        :param dep_tools: an optional instance of DependencyTools so that the
            caller can access any diagnostic messages detailing why the loop
            iterations are not independent.
        :type dep_tools: Optional[
            :py:class:`psyclone.psyir.tools.DependencyTools`]

        :returns: True if the loop iterations are independent, False otherwise.
        :rtype: bool

        '''
        if not dep_tools:
            # pylint: disable=import-outside-toplevel
            from psyclone.psyir.tools import DependencyTools
            dtools = DependencyTools()
        else:
            dtools = dep_tools
        return dtools.can_loop_be_parallelised(
            self, test_all_variables=test_all_variables,
            signatures_to_ignore=signatures_to_ignore)

    def enters_scope(self, scope, visited_nodes=None) -> bool:
        '''
        This is a Reference method, but sometimes it will reach this point
        because self.reference_accesses returns a Loop as the Node associated
        with the loop variable.
        In this case we can always return False as we know that this variable
        gets the iteration value.

        #TODO #3124: Alternatively move the loop variable to a child Reference.

        :param scope: the given scope that we evaluate.
        :param visited_nodes: a set of nodes already visited, this is necessary
            because the dependency chains may contain cycles. Defaults to an
            empty set.
        :returns: whether the symbol lifetime starts before the given scope.
        '''
        # pylint: disable=unused-argument
        return False
