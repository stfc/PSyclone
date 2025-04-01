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
#         A. B. G. Chalk STFC Daresbury Lab
#         J. Henrichs, Bureau of Meteorology
# Modified I. Kavcic, Met Office

''' This module provides the ParallelLoopTrans transformation.'''

import abc
from collections.abc import Iterable

from psyclone import psyGen
from psyclone.core import Signature
from psyclone.domain.common.psylayer import PSyLoop
from psyclone.psyir import nodes
from psyclone.psyir.nodes import Loop, Reference, Call, Routine
from psyclone.psyir.symbols import AutomaticInterface
from psyclone.psyir.tools import DependencyTools, DTCode
from psyclone.psyir.transformations.loop_trans import LoopTrans
from psyclone.psyir.transformations.transformation_error import \
    TransformationError


class ParallelLoopTrans(LoopTrans, metaclass=abc.ABCMeta):
    '''
    Adds an abstract directive (it needs to be specified by sub-classing this
    transformation) to a loop indicating that it should be parallelised. It
    performs some data dependency checks to guarantee that the loop can be
    parallelised without changing the semantics of it.

    '''
    # The types of node that must be excluded from the section of PSyIR
    # being transformed.
    excluded_node_types = (nodes.Return, psyGen.HaloExchange, nodes.CodeBlock)

    @abc.abstractmethod
    def _directive(self, children, collapse=None):
        '''
        Returns the directive object to insert into the Schedule.
        Must be implemented by sub-class.

        :param children: list of nodes that will be children of this Directive.
        :type children: list of :py:class:`psyclone.psyir.nodes.Node`
        :param int collapse: the number of tightly-nested loops to which
            this directive applies or None.

        :returns: the new Directive node.
        :rtype: sub-class of :py:class:`psyclone.psyir.nodes.Directive`.
        '''

    @staticmethod
    def _attempt_privatisation(node, symbol_name, dry_run=False):
        ''' Check and (if dry_run is False) perform symbol privatisation
        for the given symbol_name in the given node.

        :param node: the loop that will be parallelised.
        :type node: :py:class:`psyclone.psyir.nodes.Loop`
        :param str symbol_name: the symbol that we want to privatise.
        :param bool dry_run: whether to perform the actual privatisation.

        :returns: whether the symbol_name can be privatised.
        :rtype: bool
        '''
        try:
            sym = node.scope.symbol_table.lookup(symbol_name)
        except KeyError:
            # Structures are reported with the full expression:
            # "mystruct%myfield" by the DA var_name, we purposely avoid
            # privatising these
            return False

        # If it's not a local symbol, we cannot safely analyse its lifetime
        if not isinstance(sym.interface, AutomaticInterface):
            return False

        # Check that the symbol is not referenced after this loop (before
        # the loop is fine because we can use OpenMP/OpenACC first-private or
        # Fortran do concurrent local_init())
        if any(ref.symbol is sym
               for ref in node.following(include_children=False)
               if isinstance(ref, Reference)):
            return False

        if not dry_run:
            node.explicitly_private_symbols.add(sym)

        return True

    def validate(self, node, options=None):
        '''
        Perform validation checks before applying the transformation

        :param node: the node we are checking.
        :type node: :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]
        :param bool|int options["collapse"]: if it's a bool and is False
            (default), it won't collapse. If it's a bool and is True, it will
            collapse as much as possible. If it's an integer, it will attempt
            to collapse until the specified number of loops (if they exist and
            are safe to collapse). The options 'ignore_dependencies_for'
            and 'force' also affect the collapse applicability.
        :param bool options["force"]: whether to force parallelisation of the
            target loop (i.e. ignore any dependence analysis).
        :param list[str] options["ignore_dependencies_for"]: whether to ignore
            some symbol names from the dependence analysis checks.
        :param bool options["sequential"]: whether this is a sequential loop.
        :param bool options["verbose"]: whether to report the reasons the
            validate and collapse steps have failed.

        :raises TypeError: if 'collapse' is not an int or a bool.
        :raises TypeError: if 'ignore_dependencies_for' is not a list of str.
        :raises TransformationError: if the given loop iterates over a
            colours (LFRic domain) iteration space.
        :raises TransformationError: if the given loops calls a procedure that
            is not guaranteed to be pure (and therefore could have dependencies
            beyond the specified by the arguments intent)
        :raises TransformationError: if the given loop is inside a pure routine
            as these do not allow parallel constructs.
        :raises TransformationError: if there is a data dependency that
            prevents the parallelisation of the loop and the provided
            options don't disregard them.

        '''
        # Check that the supplied node is a Loop and does not contain any
        # unsupported nodes.
        super().validate(node, options=options)

        if not options:
            options = {}
        verbose = options.get("verbose", False)
        collapse = options.get("collapse", False)
        force = options.get("force", False)
        ignore_dependencies_for = options.get("ignore_dependencies_for", [])
        sequential = options.get("sequential", False)
        privatise_arrays = options.get("privatise_arrays", False)

        # Check we are not a sequential loop
        if (not sequential and isinstance(node, PSyLoop) and
                node.loop_type == 'colours'):
            raise TransformationError(f"Error in {self.name} transformation. "
                                      f"The target loop is over colours and "
                                      f"must be computed serially.")

        # If 'collapse' is specified, check that it is an int and that the
        # loop nest has at least that number of loops in it
        if collapse:
            if not isinstance(collapse, (int, bool)):
                raise TypeError(
                    f"The 'collapse' argument must be an integer or a bool but"
                    f" got an object of type {type(collapse)}")

        if not isinstance(privatise_arrays, bool):
            raise TypeError(
                f"The 'privatise_arrays' option must be a bool "
                f"but got an object of type {type(privatise_arrays).__name__}")

        routine = node.ancestor(Routine)
        if routine is not None and routine.parent is not None:
            rsym = routine.symbol
            if rsym.is_pure or rsym.is_elemental:
                raise TransformationError(
                    f"Loops inside a pure (or elemental) routine cannot be"
                    f" parallelised, but attempted to parallelise loop "
                    f"inside '{routine.name}'"
                )

        # If it's sequential or we 'force' the transformation, the validations
        # below this point are skipped
        if sequential or force:
            return

        # Check that all calls inside the loop are pure, and therefore all its
        # dependencies given by the intent of its arguments
        not_pure = [call.routine.name for call in node.walk(Call)
                    if not call.is_pure]
        if not_pure:
            message = (
                f"Loop cannot be parallelised because it cannot "
                f"guarantee that the following calls are pure: "
                f"{sorted(set(not_pure))}")
            if verbose:
                node.append_preceding_comment(message)
            raise TransformationError(message)

        if ignore_dependencies_for:
            if (not isinstance(ignore_dependencies_for, Iterable) or
                    isinstance(ignore_dependencies_for, str) or not all(
                    isinstance(v, str) for v in ignore_dependencies_for)):
                raise TypeError(
                    f"The 'ignore_dependencies_for' option must be an Iterable"
                    f" object containing str representing the "
                    f"symbols to ignore, but got '{ignore_dependencies_for}'.")

        dep_tools = DependencyTools()

        signatures = [Signature(name) for name in ignore_dependencies_for]

        if not node.independent_iterations(
                       dep_tools=dep_tools,
                       test_all_variables=True,
                       signatures_to_ignore=signatures):
            # The DependencyTools also returns False for things that are
            # not an issue, so we ignore specific messages.
            errors = []
            for message in dep_tools.get_all_messages():
                if message.code == DTCode.WARN_SCALAR_WRITTEN_ONCE:
                    continue
                if (privatise_arrays and
                        message.code == DTCode.ERROR_WRITE_WRITE_RACE):
                    for var_name in message.var_names:
                        if not self._attempt_privatisation(node, var_name,
                                                           dry_run=True):
                            errors.append(
                                f"The write-write dependency in '{var_name}'"
                                f" cannot be solved by array privatisation "
                                f"because it is not a plain local array or "
                                f"it is used after the loop.")
                    continue
                errors.append(str(message))

            if errors:
                error_lines = "\n".join(errors)
                messages = (f"Loop cannot be parallelised because:\n"
                            f"{error_lines}\n"
                            f"Consider using the \"ignore_dependencies_for\""
                            f" transformation option if this is a false "
                            f"dependency\nConsider using the \"array_"
                            f"privatisation\" transformation option if "
                            f"this is a write-write dependency")
                if verbose:
                    # This message can get quite long, we will skip it if an
                    # ancestor loop already has the exact same message
                    cursor = node.ancestor(Loop)
                    while cursor:
                        if messages in cursor.preceding_comment:
                            break
                        cursor = cursor.ancestor(Loop)
                    if not cursor:
                        node.append_preceding_comment(f"PSyclone: {messages}")
                raise TransformationError(messages)

    def apply(self, node, options=None):
        '''
        Apply the Loop transformation to the specified node in a
        Schedule. This node must be a Loop since this transformation
        corresponds to wrapping the generated code with directives,
        e.g. for OpenMP:

        .. code-block:: fortran

          !$OMP DO
          do ...
             ...
          end do
          !$OMP END DO

        At code-generation time (when gen_code()` is called), this node must be
        within (i.e. a child of) a PARALLEL region.

        :param node: the supplied node to which we will apply the
                loop parallelisation transformation.
        :type node: :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]
        :param bool|int options["collapse"]: if it's a bool and is False
            (default), it won't collapse. If it's a bool and is True, it will
            collapse as much as possible. If it's an integer, it will attempt
            to collapse until the specified number of loops (if they exist and
            are safe to collapse them). The options 'ignore_dependencies_for'
            and 'force' also affect the collapse applicabilty analysis.
        :param bool options["force"]: whether to force parallelisation of the
            target loop (i.e. ignore any dependence analysis).
        :param list[str] options["ignore_dependencies_for"]: whether to ignore
            some symbol names from the dependence analysis checks.
        :param bool options["sequential"]: whether this is a sequential loop.
        :param bool options["verbose"]: whether to report the reasons the
            validate and collapse steps have failed.

        '''
        if not options:
            options = {}
        self.validate(node, options=options)

        verbose = options.get("verbose", False)
        collapse = options.get("collapse", False)
        ignore_dep_analysis = options.get("force", False)
        list_of_names = options.get("ignore_dependencies_for", [])
        privatise_arrays = options.get("privatise_arrays", False)
        list_of_signatures = [Signature(name) for name in list_of_names]
        dtools = DependencyTools()

        # keep a reference to the node's original parent and its index as these
        # are required and will change when we change the node's location
        node_parent = node.parent
        node_position = node.position

        # If 'privatise_arrays' is specified, make the write-write symbols
        # private (we know this succeeds because the validate did a dry_run)
        if privatise_arrays and not node.independent_iterations(
                                   dep_tools=dtools,
                                   test_all_variables=True,
                                   signatures_to_ignore=list_of_signatures):
            for message in dtools.get_all_messages():
                if message.code == DTCode.ERROR_WRITE_WRITE_RACE:
                    for var_name in message.var_names:
                        self._attempt_privatisation(node, var_name)

        # If 'collapse' is specified, check that it is an int and that the
        # loop nest has at least that number of loops in it
        if collapse:
            # Count the number of perfectly nested loops that can be collapsed
            num_collapsable_loops = 0
            next_loop = node
            previous_iteration_variables = []
            while isinstance(next_loop, Loop):
                previous_iteration_variables.append(next_loop.variable)
                num_collapsable_loops += 1
                if not isinstance(collapse, bool):
                    if num_collapsable_loops >= collapse:
                        break

                # If it has more than one child, the next loop will not be
                # perfectly nested, so stop searching. If there is no child,
                # we have an empty loop and we also stop here.
                if len(next_loop.loop_body.children) != 1:
                    if (next_loop.loop_body.children and
                            isinstance(next_loop.loop_body[0], Loop)):
                        next_loop.loop_body[0].append_preceding_comment(
                            "Loop cannot be collapsed because it has siblings")
                    break

                next_loop = next_loop.loop_body[0]
                if not isinstance(next_loop, Loop):
                    break

                # If it is a loop dependent on a previous iteration variable
                # (e.g. a triangular iteration space), it can not be collapsed
                dependent_on_previous_variable = False
                for bound in (next_loop.start_expr, next_loop.stop_expr,
                              next_loop.step_expr):
                    for ref in bound.walk(Reference):
                        if ref.symbol in previous_iteration_variables:
                            dependent_on_previous_variable = ref.symbol
                            break
                if dependent_on_previous_variable:
                    if verbose:
                        next_loop.append_preceding_comment(
                            f"Loop cannot be collapsed because one of the "
                            f"bounds depends on the previous iteration variab"
                            f"le '{dependent_on_previous_variable.name}'")
                    break

                # Check that the next loop has no loop-carried dependencies
                if not ignore_dep_analysis:
                    if not next_loop.independent_iterations(
                               dep_tools=dtools,
                               signatures_to_ignore=list_of_signatures):
                        if verbose:
                            msgs = dtools.get_all_messages()
                            next_loop.preceding_comment = (
                                "\n".join([str(m) for m in msgs]) +
                                " Consider using the \"ignore_dependencies_"
                                "for\" transformation option if this is a "
                                "false dependency.")
                        break
        else:
            num_collapsable_loops = None

        # Add our orphan loop directive setting its parent to the node's
        # parent and its children to the node. This calls down to the sub-class
        # to get the type of directive we require.
        directive = self._directive([node.detach()], num_collapsable_loops)

        # Add the loop directive as a child of the node's parent
        node_parent.addchild(directive, index=node_position)
