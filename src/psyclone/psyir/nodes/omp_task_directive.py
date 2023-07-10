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
# Authors A. B. G. Chalk, STFC Daresbury Lab
# -----------------------------------------------------------------------------
""" This module contains the implementation of the OpenMP Task Directive
node."""

from __future__ import absolute_import
import itertools
import math

from psyclone.errors import GenerationError
from psyclone.psyir.nodes import (
    Reference,
    Assignment,
    IfBlock,
    ArrayReference,
    ArrayOfStructuresReference,
    StructureReference,
    Call,
    ArrayMember,
)
from psyclone.psyir.nodes.array_mixin import ArrayMixin
from psyclone.psyir.nodes.array_of_structures_member import (
    ArrayOfStructuresMember,
)
from psyclone.psyir.nodes.operation import BinaryOperation
from psyclone.psyir.nodes.loop import Loop
from psyclone.psyir.nodes.literal import Literal
from psyclone.psyir.nodes.ranges import Range
from psyclone.psyir.nodes.member import Member
from psyclone.psyir.nodes.omp_clauses import (
    OMPPrivateClause,
    OMPFirstprivateClause,
    OMPDependClause,
    OMPSharedClause,
)
from psyclone.psyir.nodes.omp_directives import (
    OMPRegionDirective,
    OMPSingleDirective,
    OMPParallelDirective,
)
from psyclone.psyir.nodes.schedule import Schedule
from psyclone.psyir.symbols import INTEGER_TYPE, DataSymbol


class OMPTaskDirective(OMPRegionDirective):
    """
    Class representing an OpenMP TASK directive in the PSyIR after lowering.
    This node should not be created by any transformation, and it solely used
    to represent TASK directives after lowering a DynamicOMPTaskDirective.

    :param list children: list of Nodes that are children of this Node.
    :param parent: the Node in the AST that has this directive as a child
    :type parent: :py:class:`psyclone.psyir.nodes.Node`
    :param bool lowering: If this node is being lowered from another node.
    """

    _children_valid_format = (
        "Schedule, OMPPrivateClause,"
        "OMPFirstprivateClause, OMPSharedClause"
        "OMPDependClause, OMPDependClause"
    )

    def __init__(self, children=None, parent=None, lowering=False):
        # If we're lowering this node from a DynamicOMPTaskDirective then
        # we need remove the children from that node before adding
        # the children from this node.
        if lowering:
            sched_childs = children[0].pop_all_children()
            super().__init__(
                children=sched_childs, parent=parent
            )
            for child in children[1:]:
                self.addchild(child)
        else:
            super().__init__(children=children, parent=parent)

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
        # pylint: disable=no-self-use
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


class DynamicOMPTaskDirective(OMPTaskDirective):
    """
    Class representing an OpenMP TASK directive in the PSyIR.

    :param list children: list of Nodes that are children of this Node.
    :param parent: the Node in the AST that has this directive as a child
    :type parent: :py:class:`psyclone.psyir.nodes.Node`
    """

    _children_valid_format = (
        "Schedule, OMPPrivateClause,"
        "OMPFirstprivateClause, OMPSharedClause"
        "OMPDependClause, OMPDependClause"
    )

    def __init__(self, children=None, parent=None):
        super().__init__(
            children=children, parent=parent
        )
        # We don't know if we have a parent OMPParallelClause at initialisation
        # so we can only create dummy clauses for now.
        self.children.append(OMPPrivateClause())
        self.children.append(OMPFirstprivateClause())
        self.children.append(OMPSharedClause())
        self.children.append(
            OMPDependClause(depend_type=OMPDependClause.DependClauseTypes.IN)
        )
        self.children.append(
            OMPDependClause(depend_type=OMPDependClause.DependClauseTypes.OUT)
        )
        # We store the symbol names for the parent loops so we can work out the
        # "chunked" loop variables.
        self._parent_loop_vars = []
        self._parent_loops = []
        self._proxy_loop_vars = {}
        self._parent_parallel = None
        self._parallel_private = None

        # We need to do extra steps when inside a Kern to correctly identify
        # symbols.
        self._in_kern = False

    def _find_parent_loop_vars(self):
        """
        Finds the loop variable of each parent loop inside the same
        OMPParallelDirective and stores them in the _parent_loop_vars member.
        Also stores the parent OMPParallelDirective in _parent_parallel.
        """
        anc = self.ancestor((OMPParallelDirective, Loop))
        while isinstance(anc, Loop):
            # Store the loop variable of each parent loop
            var = anc.variable
            self._parent_loop_vars.append(var)
            self._parent_loops.append(anc)
            # Recurse up the tree
            anc = anc.ancestor((OMPParallelDirective, Loop))

        # Store the parent parallel directive node
        self._parent_parallel = anc
        # pylint: disable=protected-access
        self._parallel_private, firstprivate, _ = \
            anc.infer_sharing_attributes()
        self._parallel_private = self._parallel_private.union(firstprivate)

    def _is_reference_private(self, ref):
        """
        Determines whether the provided reference is private or shared in the
        enclosing parallel region.

        :param ref: The Reference object to be determined if it is private \
                    or shared.
        :type ref: :py:class:`psyclone.psyir.nodes.Reference`

        :returns: True if ref is private, else False.
        :rtype: bool
        """
        for parent_sym in self._parallel_private:
            if (
                ref.symbol.name == parent_sym.name
                and ref.symbol.datatype.intrinsic
                == parent_sym.datatype.intrinsic
                and ref.symbol.datatype.precision
                == parent_sym.datatype.precision
            ):
                return True
        return False

    def _evaluate_readonly_baseref(
        self, ref, private_list, firstprivate_list, in_list
    ):
        """
        Evaluates any read-only References to variables inside the OpenMP task
        region and adds a copy of the Reference to the appropriate data-sharing
        list used to construct the clauses for this task region.

        The basic rules for this are:
        1. If the Reference is private in the parallel region containing this
        task, the Reference will be added to the list of firstprivate
        References unless it has already been added to either the list of
        private or firstprivate References for this task.
        2. If the Reference is shared, then the Reference will be added to the
        input list of References unless it is already present in that list.

        :param ref: The reference to be evaluated.
        :type ref: :py:class:`psyclone.psyir.nodes.Reference`
        :param private_list: The list of private References for this task.
        :type private_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param firstprivate_list: The list of firstprivate References for this
                                  task.
        :type firstprivate_list: List of
                                 :py:class:`psyclone.psyir.nodes.Reference`
        :param in_list: The list of input References for this task.
        :type in_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        """
        symbol = ref.symbol
        is_private = symbol in self._parallel_private
        if is_private:
            # If the reference is private in the parent parallel,
            # then it is added to the firstprivate clause for this
            # task if it has not yet been written to (i.e. is not
            # yet in the private clause list).
            if ref not in private_list and ref not in firstprivate_list:
                firstprivate_list.append(ref.copy())
        else:
            # Otherwise it was a shared variable. Its not an
            # array so we just add the name to the in_list
            # if not already there. If its already in out_list
            # we still add it as this is the same as an inout
            # dependency
            if ref not in in_list:
                in_list.append(ref.copy())

    def _handle_index_binop(
        self, node, index_list, firstprivate_list, private_list
    ):
        """
        Evaluates a binary operation index used to access an array
        within this OpenMP task.

        For each index, the code checks that the index matches the expected
        format, which is [Reference] [ADD/SUB] [Literal] (or the opposite
        ordering). PSyclone does not currently support other binary operation
        indexing inside an OpenMP task.

        Once this is confirmed, PSyclone builds the appropriate list of
        References to correctly express the dependencies of this array access,
        and appends them to the `index_list` input argument. This can depend on
        the structure of the Loop inside the task, and any parent Loops.

        The Reference inside the binary operation must be a private or
        firstprivate variable inside the task region, else PSyclone does not
        support using it as an array index.

        :param node: The BinaryOperation to be evaluated.
        :type node: :py:class:`psyclone.psyir.nodes.BinaryOperation`
        :param index_list: A list of Nodes used to handle the dependencies
                           for this array access. This may be reused over
                           multiple calls to this function to avoid duplicating
                           Nodes.
        :type index_list: List of :py:class:`psyclone.psyir.nodes.Node`
        :param firstprivate_list: The list of firstprivate References used in
                                  this task region.
        :type firstprivate_list: List of
                                 :py:class:`psyclone.psyir.nodes.Reference`
        :param private_list: The list of private References used in
                             this task region.
        :type private_list: List of :py:class:`psyclone.psyir.nodes.Reference`

        :raises GenerationError: if this BinaryOperation is not an addition or
                                 subtraction.
        :raises GenerationError: if this BinaryOperation does not contain both
                                 a Reference and a Literal.
        :raises GenerationError: if this BinaryOperation contains a Reference
                                 to a shared variable.
        """

        # Binary Operation operator check, must be ADD or SUB.
        if (
            node.operator is not BinaryOperation.Operator.ADD
            and node.operator is not BinaryOperation.Operator.SUB
        ):
            raise GenerationError(
                f"Binary Operator of type {node.operator} used "
                "as in index inside an "
                "OMPTaskDirective which is not "
                "supported"
            )
        # We have ADD or SUB BinaryOperation
        # It must be either Ref OP Lit or Lit OP Ref
        if not (
            (
                isinstance(node.children[0], Reference)
                and isinstance(node.children[1], Literal)
            )
            or (
                isinstance(node.children[0], Literal)
                and isinstance(node.children[1], Reference)
            )
        ):
            raise GenerationError(
                "Children of BinaryOperation are of "
                f"types {type(node.children[0]).__name__} and "
                f"{type(node.children[1]).__name__}, expected one "
                "Reference and one Literal when"
                " used as an index inside an "
                "OMPTaskDirective."
            )

        # Have Reference +/- Literal, analyse
        # and create clause appropriately.

        # index_private stores whether the index is private.
        index_private = False
        # is_proxy stores whether the index is a proxy loop variable.
        is_proxy = False
        # ref stores the Reference child of the BinaryOperation.
        ref = None
        # literal stores the Literal child of the BinaryOperation.
        literal = None
        # ref_index stores which child the reference is from the BinOp
        ref_index = None
        if isinstance(node.children[0], Reference):
            index_symbol = node.children[0].symbol
            index_private = self._is_reference_private(node.children[0])
            is_proxy = index_symbol in self._proxy_loop_vars
            ref = node.children[0]
            ref_index = 0
            literal = node.children[1]
        if isinstance(node.children[1], Reference):
            index_symbol = node.children[1].symbol
            index_private = self._is_reference_private(node.children[1])
            is_proxy = index_symbol in self._proxy_loop_vars
            ref = node.children[1]
            ref_index = 1
            literal = node.children[0]

        # We have some array access which is of the format:
        # array( Reference +/- Literal).
        # If the Reference is to a proxy (is_proxy is True) then we replace
        # the Reference with the proxy variable instead. This is the most
        # important case.
        # If the task contains Loops, and the Reference is to one of the
        # Loop variables, then we create a Range object for : for that
        # dimension. All other situations are treated as a constant.

        # Find the child loops that are not proxy loop variables.
        child_loop_vars = []
        for child_loop in self.walk(Loop):
            if child_loop.variable not in self._proxy_loop_vars:
                child_loop_vars.append(child_loop.variable)

        # Handle the proxy_loop case
        if is_proxy:
            # Treat it as though we came across the parent loop variable.
            parent_loop = self._proxy_loop_vars[index_symbol]["parent_loop"]

            # Create a Reference to the real variable
            real_ref = self._proxy_loop_vars[index_symbol][
                "parent_node"
            ].copy()
            for temp_ref in self._proxy_loop_vars[index_symbol]["parent_node"]:
                real_ref = temp_ref.copy()
                # We have a Literal step value, and a Literal in
                # the Binary Operation. These Literals must both be
                # Integer types, so we will convert them to integers
                # and do some divison.
                step_val = int(parent_loop.step_expr.value)
                literal_val = int(literal.value)
                divisor = math.ceil(literal_val / step_val)
                modulo = literal_val % step_val
                # If the divisor is > 1, then we need to do
                # divisor*step_val
                # We also need to add divisor-1*step_val to cover the case
                # where e.g. array(i+1) is inside a larger loop, as we
                # need dependencies to array(i) and array(i+step), unless
                # modulo == 0
                step = None
                step2 = None
                if divisor > 1:
                    step = BinaryOperation.create(
                        BinaryOperation.Operator.MUL,
                        Literal(f"{divisor}", INTEGER_TYPE),
                        Literal(f"{step_val}", INTEGER_TYPE),
                    )
                    if divisor > 2:
                        step2 = BinaryOperation.create(
                            BinaryOperation.Operator.MUL,
                            Literal(f"{divisor-1}", INTEGER_TYPE),
                            Literal(f"{step_val}", INTEGER_TYPE),
                        )
                    else:
                        step2 = Literal(f"{step_val}", INTEGER_TYPE)
                else:
                    step = Literal(f"{step_val}", INTEGER_TYPE)

                # Create a Binary Operation of the correct format.
                binop = None
                binop2 = None
                if ref_index == 0:
                    # We have Ref OP Literal
                    binop = BinaryOperation.create(
                        node.operator, real_ref.copy(), step
                    )
                    # If modulo is non-zero then we need a second binop
                    # dependency to handle the module, so we have two
                    # dependency clauses, one which handles each of the
                    # step-sized array chunks that overlaps with this access.
                    if modulo != 0:
                        if step2 is not None:
                            binop2 = BinaryOperation.create(
                                node.operator, real_ref.copy(), step2
                            )
                        else:
                            binop2 = real_ref.copy()
                else:
                    # We have Literal OP Ref
                    binop = BinaryOperation.create(
                        node.operator, step, real_ref.copy()
                    )
                    # If modulo is non-zero then we need a second binop
                    # dependency to handle the module, so we have two
                    # dependency clauses, one which handles each of the
                    # step-sized array chunks that overlaps with this access.
                    if modulo != 0:
                        if step2 is not None:
                            binop2 = BinaryOperation.create(
                                node.operator, step2, real_ref.copy()
                            )
                        else:
                            binop2 = real_ref.copy()
                # Add this to the list of indexes
                if binop2 is not None:
                    index_list.append([binop, binop2])
                else:
                    index_list.append(binop)

        # Proxy use case handled.
        # If the variable is private:
        elif index_private:
            # If the variable is in our private list
            if ref in private_list:
                # If its a child loop variable
                if ref.symbol in child_loop_vars:
                    # Return a full range (:)
                    dim = len(index_list)
                    one = Literal(str(dim + 1), INTEGER_TYPE)
                    # Find the arrayref
                    array_access_member = ref.ancestor(ArrayMember)
                    # If there is an ArrayMember ancestor of this index, then
                    # this is a StructureReference. For that case we need
                    # to find the StructureReference parent, and recreate
                    # the full a%b%c(...) style structure.
                    if array_access_member is not None:
                        start = ref.ancestor(StructureReference)
                        sub_ref = StructureReference(start.symbol)
                        sub_ref.addchild(start.member.copy())
                        array_member = sub_ref.walk(Member)[-1]
                        num_child = len(array_member.children)
                        array_member.pop_all_children()
                        for _ in range(num_child):
                            array_member.addchild(one.copy())
                    else:
                        arrayref = ref.parent.parent
                        sub_ref = Reference(arrayref.symbol)
                    # Create a full range object for this symbol and
                    # children
                    lbound = BinaryOperation.create(
                        BinaryOperation.Operator.LBOUND,
                        sub_ref.copy(),
                        one.copy(),
                    )
                    ubound = BinaryOperation.create(
                        BinaryOperation.Operator.UBOUND,
                        sub_ref.copy(),
                        one.copy(),
                    )
                    full_range = Range.create(lbound, ubound)
                    index_list.append(full_range)
                else:
                    # We have a private constant, written to inside
                    # our region, so we can't do anything better than
                    # a full range I think (since we don't know what
                    # the value is/how it changes.
                    # Return a full range (:)
                    dim = len(index_list)
                    one = Literal(str(dim + 1), INTEGER_TYPE)
                    arrayref = ref.parent.parent
                    lbound = BinaryOperation.create(
                        BinaryOperation.Operator.LBOUND,
                        Reference(arrayref.symbol),
                        one.copy(),
                    )
                    ubound = BinaryOperation.create(
                        BinaryOperation.Operator.UBOUND,
                        Reference(arrayref.symbol),
                        one.copy(),
                    )
                    full_range = Range.create(lbound, ubound)
                    index_list.append(full_range)
            else:
                if ref not in firstprivate_list:
                    firstprivate_list.append(ref.copy())
                if ref.symbol in self._parent_loop_vars:
                    # Non-proxy access to a parent loop variable.
                    # In this case we have to do similar to when accessing a
                    # proxy loop variable.

                    # Find index of parent loop var
                    ind = self._parent_loop_vars.index(ref.symbol)
                    parent_loop = self._parent_loops[ind]
                    # We have a Literal step value, and a Literal in
                    # the Binary Operation. These Literals must both be
                    # Integer types, so we will convert them to integers
                    # and do some divison.
                    step_val = int(parent_loop.step_expr.value)
                    literal_val = int(literal.value)
                    divisor = math.ceil(literal_val / step_val)
                    modulo = literal_val % step_val
                    # If the divisor is > 1, then we need to do
                    # divisor*step_val
                    # We also need to add divisor-1*step_val to cover the case
                    # where e.g. array(i+1) is inside a larger loop, as we
                    # need dependencies to array(i) and array(i+step), unless
                    # modulo == 0
                    step = None
                    step2 = None
                    if divisor > 1:
                        step = BinaryOperation.create(
                            BinaryOperation.Operator.MUL,
                            Literal(f"{divisor}", INTEGER_TYPE),
                            Literal(f"{step_val}", INTEGER_TYPE),
                        )
                        if divisor > 2:
                            step2 = BinaryOperation.create(
                                BinaryOperation.Operator.MUL,
                                Literal(f"{divisor-1}", INTEGER_TYPE),
                                Literal(f"{step_val}", INTEGER_TYPE),
                            )
                        else:
                            step2 = Literal(f"{step_val}", INTEGER_TYPE)
                    else:
                        step = Literal(f"{step_val}", INTEGER_TYPE)

                    # Create a Binary Operation of the correct format.
                    binop = None
                    binop2 = None
                    if ref_index == 0:
                        # We have Ref OP Literal
                        binop = BinaryOperation.create(
                            node.operator, ref.copy(), step
                        )
                        if modulo != 0:
                            if step2 is not None:
                                binop2 = BinaryOperation.create(
                                    node.operator, ref.copy(), step2
                                )
                            else:
                                binop2 = ref.copy()
                    else:
                        # We have Literal OP Ref
                        binop = BinaryOperation.create(
                            node.operator, step, ref.copy()
                        )
                        if modulo != 0:
                            if step2 is not None:
                                binop2 = BinaryOperation.create(
                                    node.operator, step2, ref.copy()
                                )
                            else:
                                binop2 = ref.copy()
                    # Add this to the list of indexes
                    if binop2 is not None:
                        index_list.append([binop, binop2])
                    else:
                        index_list.append(binop)
                else:
                    # It can't be a child loop variable (these have to be
                    # private). Just has to be a firstprivate constant, which
                    # we can just use the reference to for now.
                    index_list.append(node.copy())
        else:
            # Have a shared variable, which we're not currently supporting
            raise GenerationError(
                "Shared variable access used "
                "as an index inside an "
                "OMPTaskDirective which is not "
                "supported."
            )

    def _evaluate_readonly_arrayref(
        self, ref, private_list, firstprivate_list, shared_list, in_list
    ):
        """
        Evaluates a read-only access to an Array inside the task region, and
        computes any data-sharing clauses and dependency clauses based upon the
        access.

        This is done by evaluating each of the array indices, and determining
        whether they are:
        1. A Literal index, in which case we need a dependency to that
           specific section of the array.
        2. A Reference index, in which case we need a dependency to the section
           of the array represented by that Reference.
        3. A Binary Operation, in which case the code calls
          `_handle_index_binop` to evaluate any additional dependencies.

        Once these have been computed, any new dependencies are added into the
        in_list, and the array reference itself will be added to the
        shared_list if not already present.

        :param node: The Reference to be evaluated.
        :type node: :py:class:`psyclone.psyir.nodes.Reference`
        :param private_list: The list of private References used in this task
                             region.
        :type private_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param firstprivate_list: The list of firstprivate References used in
                                  this task region.
        :type firstprivate_list: List of
                                 :py:class:`psyclone.psyir.nodes.Reference`
        :param shared_list: The list of shared References for this task.
        :type shared_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param in_list: The list of input References for this task.
        :type in_list: List of :py:class:`psyclone.psyir.nodes.Reference`

        :raises GenerationError: If an array index is a shared variable.
        :raises GenerationError: If an array index is not a Reference, Literal
                                 or BinaryOperation.
        """
        # Index list stores the set of indices to use with this ArrayMixin
        # for the depend clause.
        index_list = []

        # Arrays are always shared variables in the parent parallel region.

        # The reference is shared. Since it is an array,
        # we need to check the following restrictions:
        # 1. No ArrayReference or ArrayOfStructuresReference
        # or StructureReference appear in the indexing.
        # 2. Each index is a firstprivate variable, or a
        # private parent variable that has not yet been
        # declared (in which case we declare it as
        # firstprivate). Alternatively each index is
        # a BinaryOperation whose children are a
        # Reference to a firstprivate variable and a
        # Literal, with operator of ADD or SUB
        for dim, index in enumerate(ref.indices):
            # pylint: disable=unidiomatic-typecheck
            if type(index) is Reference:
                # Check whether the Reference is private
                index_private = self._is_reference_private(index)
                # Check whether the reference is to a child loop variable.
                child_loop_vars = []
                for child_loop in self.walk(Loop):
                    if child_loop.variable not in self._proxy_loop_vars:
                        child_loop_vars.append(child_loop.variable)

                if index_private:
                    if (
                        index not in private_list
                        and index not in firstprivate_list
                    ):
                        firstprivate_list.append(index.copy())
                    # Special case 1. If index belongs to a child loop
                    # that is NOT a proxy for a parent loop, then we
                    # can only do as well as guessing the entire range
                    # of the loop is used.
                    if index.symbol in child_loop_vars:
                        # Append a full Range (i.e., :)
                        one = Literal(str(dim + 1), INTEGER_TYPE)
                        lbound = BinaryOperation.create(
                            BinaryOperation.Operator.LBOUND,
                            Reference(ref.symbol),
                            one.copy(),
                        )
                        ubound = BinaryOperation.create(
                            BinaryOperation.Operator.UBOUND,
                            Reference(ref.symbol),
                            one.copy(),
                        )
                        full_range = Range.create(lbound, ubound)
                        index_list.append(full_range)
                    elif index.symbol in self._proxy_loop_vars:
                        # Special case 2. the index is a proxy for a parent
                        # loop's variable. In this case, we add a reference to
                        # the parent loop's value. We create a list of all
                        # possible variants, as we might have multiple values
                        # set for a value, e.g. for a boundary condition if
                        # statement.
                        if len(index_list) <= dim:
                            index_list.append([])
                        for temp_ref in self._proxy_loop_vars[index.symbol][
                            "parent_node"
                        ]:
                            parent_ref = temp_ref.copy()
                            if isinstance(parent_ref, BinaryOperation):
                                quick_list = []
                                self._handle_index_binop(
                                    parent_ref,
                                    quick_list,
                                    firstprivate_list,
                                    private_list,
                                )
                                for element in quick_list:
                                    if isinstance(element, list):
                                        index_list[dim].extend(element)
                                    else:
                                        index_list[dim].append(element)
                            else:
                                index_list[dim].append(parent_ref)
                    else:
                        # Final case is just a generic Reference, in which case
                        # just copy the Reference
                        index_list.append(index.copy())
                else:
                    raise GenerationError(
                        "Shared variable access used "
                        "as an index inside an "
                        "OMPTaskDirective which is not "
                        f"supported. Variable name is {index}"
                    )
            elif isinstance(index, BinaryOperation):
                # Binary Operation check
                # A single binary operation, e.g. a(i+1) can require
                # multiple clauses to correctly handle.
                self._handle_index_binop(
                    index, index_list, firstprivate_list, private_list
                )
            elif isinstance(index, Literal):
                # Just place literal directly into the dependency clause.
                index_list.append(index.copy())
            else:
                # Not allowed type appears
                raise GenerationError(
                    f"{type(index).__name__} object is not allowed to "
                    "appear in an Array Index "
                    "expression inside an "
                    "OMPTaskDirective."
                )
        # So we have a list of (lists of) indices
        # [ [index1, index4], index2, index3] so convert these
        # to an ArrayReference again.
        # To create all combinations, we use itertools.product
        # We have to create a new list which only contains lists.
        new_index_list = []
        for element in index_list:
            if isinstance(element, list):
                new_index_list.append(element)
            else:
                new_index_list.append([element])
        combinations = itertools.product(*new_index_list)
        for temp_list in combinations:
            # We need to make copies of the members as each
            # member can only be the child of one ArrayRef
            final_list = []
            for element in temp_list:
                final_list.append(element.copy())
            dclause = ArrayReference.create(ref.symbol, list(final_list))
            # Add dclause into the in_list if required
            if dclause not in in_list:
                in_list.append(dclause)
        # Add to shared_list (for explicity)
        sclause = Reference(ref.symbol)
        if sclause not in shared_list:
            shared_list.append(sclause)

    def _evaluate_structure_with_array_reference_read(
        self,
        ref,
        array_access_member,
        private_list,
        firstprivate_list,
        shared_list,
        in_list,
    ):
        """
        Evaluates a read-only access to an Array inside the task region, and
        computes any data-sharing clauses and dependency clauses based upon the
        access.

        This is done by evaluating each of the array indices, and determining
        whether they are:
        1. A Literal index, in which case we need a dependency to that
           specific section of the array.
        2. A Reference index, in which case we need a dependency to the section
           of the array represented by that Reference.
        3. A Binary Operation, in which case the code calls
          `_handle_index_binop` to evaluate any additional dependencies.

        Once these have been computed, any new dependencies are added into the
        in_list, and the array reference itself will be added to the
        shared_list if not already present.

        :param node: The Reference to be evaluated.
        :type node: :py:class:`psyclone.psyir.nodes.Reference`
        :param array_access_member: The ArrayMixin member child of the
                                    node.
        :type array_access_member: \
                :py:class:psyclone.psyir.nodes.array_mixin.ArrayMixin`
        :param private_list: The list of private References used in this task
                             region.
        :type private_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param firstprivate_list: The list of firstprivate References used in
                                  this task region.
        :type firstprivate_list: List of
                                 :py:class:`psyclone.psyir.nodes.Reference`
        :param shared_list: The list of shared References for this task.
        :type shared_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param in_list: The list of input References for this task.
        :type in_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        """

        # Index list stores the set of indices to use with this ArrayMixin
        # for the depend clause.
        index_list = []

        # Find the list of members we need to include in the final reference.
        new_member = ref.member.copy()
        sref_base = StructureReference(ref.symbol)
        sref_base.addchild(new_member)
        self._evaluate_structure_with_array_reference_indexlist(
            ref,
            sref_base,
            array_access_member,
            private_list,
            firstprivate_list,
            shared_list,
            index_list,
        )

        # So we have a list of (lists of) indices
        # [ [index1, index4], index2, index3] so convert these
        # to an ArrayReference again.
        # To create all combinations, we use itertools.product
        # We have to create a new list which only contains lists.
        new_index_list = []
        for element in index_list:
            if isinstance(element, list):
                new_index_list.append(element)
            else:
                new_index_list.append([element])
        combinations = itertools.product(*new_index_list)
        for temp_list in combinations:
            # We need to make copies of the members as each
            # member can only be the child of one ArrayRef
            final_list = []
            for element in temp_list:
                final_list.append(element.copy())
            final_member = ArrayMember.create(
                array_access_member.name, list(final_list)
            )
            sref_copy = sref_base.copy()
            # Copy copies the members
            members = sref_copy.walk(Member)
            members[-1].replace_with(final_member)
            # Add dclause into the in_list if required
            if sref_copy not in in_list:
                in_list.append(sref_copy)
        # Add to shared_list (for explicity)
        sclause = Reference(ref.symbol)
        if sclause not in shared_list:
            shared_list.append(sclause)

    def _evaluate_readonly_reference(
        self, ref, private_list, firstprivate_list, shared_list, in_list
    ):
        """
        Evaluates any Reference used in a read context. This is done by
        calling the appropriate helper functions for ArrayReferences,
        StructureReferences or other References as appropriate.

        :param node: The Reference to be evaluated.
        :type node: :py:class:`psyclone.psyir.nodes.Reference`
        :param private_list: The list of private References used in this task
                             region.
        :type private_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param firstprivate_list: The list of firstprivate References used in
                                  this task region.
        :type firstprivate_list: List of
                                 :py:class:`psyclone.psyir.nodes.Reference`
        :param shared_list: The list of shared References for this task.
        :type shared_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param in_list: The list of input References for this task.
        :type in_list: List of :py:class:`psyclone.psyir.nodes.Reference`

        :raises GenerationError: If a StructureReference containing multiple \
                                 ArrayMember or ArrayOfStructuresMember as \
                                 children is found.
        """
        if isinstance(ref, (ArrayReference, ArrayOfStructuresReference)):
            # Resolve ArrayReference or ArrayOfStructuresReference
            self._evaluate_readonly_arrayref(
                ref, private_list, firstprivate_list, shared_list, in_list
            )
        elif isinstance(ref, StructureReference):
            # If the StructureReference contains an ArrayMixin then
            # we need to treat it differently, like an arrayref, however
            # the code to handle an arrayref is not compatible with more
            # than one ArrayMixin child
            array_children = ref.walk((ArrayOfStructuresMember, ArrayMember))
            if len(array_children) > 0:
                if len(array_children) > 1:
                    raise GenerationError(
                        "Doesn't support a "
                        "StructureReference child with multiple array "
                        "accessing members."
                    )
                self._evaluate_structure_with_array_reference_read(
                    ref,
                    array_children[0],
                    private_list,
                    firstprivate_list,
                    shared_list,
                    in_list,
                )
            else:
                # This is treated the same as a Reference, except we have to
                # create a Reference to the symbol to handle.
                base_ref = Reference(ref.symbol)
                self._evaluate_readonly_baseref(
                    base_ref, private_list, firstprivate_list, in_list
                )
        elif isinstance(ref, Reference):
            self._evaluate_readonly_baseref(
                ref, private_list, firstprivate_list, in_list
            )

    def _evaluate_structure_with_array_reference_indexlist(
        self,
        ref,
        sref_base,
        array_access_member,
        private_list,
        firstprivate_list,
        shared_list,
        index_list,
    ):
        """
        Evaluates an access to an Array inside the task region, and
        generates the index_list used by the calling function - 
        either _evaluate_structure_with_array_reference_{read/write}.


        This is done by evaluating each of the array indices, and determining
        whether they are:
        1. A Literal index, in which case we need a dependency to that
           specific section of the array.
        2. A Reference index, in which case we need a dependency to the section
           of the array represented by that Reference.
        3. A Binary Operation, in which case the code calls
          `_handle_index_binop` to evaluate any additional dependencies.

        Each of these results are added to the index_list, used in the callee.

        :param ref: The Reference to be evaluated.
        :type ref: :py:class:`psyclone.psyir.nodes.Reference`
        :param sref_base: A copy of ref containing the members included
                          in the final reference.
        :type sref_base: :py:class:`psyclone.psyir.nodes.StructureReference`
        :param array_access_member: The ArrayMixin member child of the
                                    node.
        :type array_access_member: \
                :py:class:psyclone.psyir.nodes.array_mixin.ArrayMixin`
        :param private_list: The list of private References used in this task
                             region.
        :type private_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param firstprivate_list: The list of firstprivate References used in
                                  this task region.
        :type firstprivate_list: List of
                                 :py:class:`psyclone.psyir.nodes.Reference`
        :param shared_list: The list of shared References for this task.
        :type shared_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param index_list: The list of output References for this task.
        :type index_list: List of :py:class:`psyclone.psyir.nodes.Reference`

        :raises GenerationError: If an array index is a shared variable.
        :raises GenerationError: If an array index is not a Reference, Literal
                                 or BinaryOperation.
        """
        for dim, index in enumerate(array_access_member.indices):
            # pylint: disable=unidiomatic-typecheck
            if type(index) is Reference:
                # Check whether the Reference is private
                index_private = self._is_reference_private(index)
                # Check whether the reference is to a child loop variable.
                child_loop_vars = []
                for child_loop in self.walk(Loop):
                    if child_loop.variable not in self._proxy_loop_vars:
                        child_loop_vars.append(child_loop.variable)

                if index_private:
                    if (
                        index not in private_list
                        and index not in firstprivate_list
                    ):
                        firstprivate_list.append(index.copy())
                    # Special case 1. If index belongs to a child loop
                    # that is NOT a proxy for a parent loop, then we
                    # can only do as well as guessing the entire range
                    # of the loop is used.
                    if index.symbol in child_loop_vars:
                        # Append a full Range (i.e., :)
                        one = Literal(str(dim + 1), INTEGER_TYPE)
                        members = sref_base.walk(Member)
                        new_member = members[0].copy()
                        final_member = new_member.walk(Member)[-1]
                        num_child = len(final_member.children)
                        final_member.pop_all_children()
                        for _ in range(num_child):
                            final_member.addchild(one.copy())

                        # Need a copy of the members for ubound as well
                        new_member2 = new_member.copy()

                        # Similar to StructureReference._create but we already
                        # have member objects.
                        lbound_sref = StructureReference(sref_base.symbol)
                        lbound_sref.addchild(new_member)
                        lbound = BinaryOperation.create(
                            BinaryOperation.Operator.LBOUND,
                            lbound_sref,
                            one.copy(),
                        )
                        # Similar to StructureReference._create but we already
                        # have member objects.
                        ubound_sref = StructureReference(sref_base.symbol)
                        ubound_sref.addchild(new_member2)
                        ubound = BinaryOperation.create(
                            BinaryOperation.Operator.UBOUND,
                            ubound_sref,
                            one.copy(),
                        )
                        full_range = Range.create(lbound, ubound)
                        index_list.append(full_range)
                    elif index.symbol in self._proxy_loop_vars:
                        # Special case 2. the index is a proxy for a parent
                        # loop's variable. In this case, we add a reference to
                        # the parent loop's value. We create a list of all
                        # possible variants, as we might have multiple values
                        # set for a value, e.g. for a boundary condition if
                        # statement.
                        if len(index_list) <= dim:
                            index_list.append([])
                        for temp_ref in self._proxy_loop_vars[index.symbol][
                            "parent_node"
                        ]:
                            parent_ref = temp_ref.copy()
                            if isinstance(parent_ref, BinaryOperation):
                                quick_list = []
                                self._handle_index_binop(
                                    parent_ref,
                                    quick_list,
                                    firstprivate_list,
                                    private_list,
                                )
                                for element in quick_list:
                                    if isinstance(element, list):
                                        index_list[dim].extend(element)
                                    else:
                                        index_list[dim].append(element)
                            else:
                                index_list[dim].append(parent_ref)
                    else:
                        # Final case is just a generic Reference, in which case
                        # just copy the Reference
                        index_list.append(index.copy())
                else:
                    raise GenerationError(
                        "Shared variable access used "
                        "as an index inside an "
                        "OMPTaskDirective which is not "
                        f"supported. Variable name is {index}"
                    )
            elif isinstance(index, BinaryOperation):
                # Binary Operation check
                # A single binary operation, e.g. a(i+1) can require
                # multiple clauses to correctly handle.
                self._handle_index_binop(
                    index, index_list, firstprivate_list, private_list
                )
            elif isinstance(index, Literal):
                # Just place literal directly into the dependency clause.
                index_list.append(index.copy())
            else:
                # Not allowed type appears
                raise GenerationError(
                    f"{type(index).__name__} object is not allowed to "
                    "appear in an Array Index "
                    "expression inside an "
                    "OMPTaskDirective."
                )

    def _evaluate_structure_with_array_reference_write(
        self,
        ref,
        array_access_member,
        private_list,
        firstprivate_list,
        shared_list,
        out_list,
    ):
        """
        Evaluates a write access to an Array inside the task region, and
        computes any data-sharing clauses and dependency clauses based upon the
        access.

        This is done by evaluating each of the array indices, and determining
        whether they are:
        1. A Literal index, in which case we need a dependency to that
           specific section of the array.
        2. A Reference index, in which case we need a dependency to the section
           of the array represented by that Reference.
        3. A Binary Operation, in which case the code calls
          `_handle_index_binop` to evaluate any additional dependencies.

        Once these have been computed, any new dependencies are added into the
        out_list, and the array reference itself will be added to the
        shared_list if not already present.

        :param ref: The Reference to be evaluated.
        :type ref: :py:class:`psyclone.psyir.nodes.Reference`
        :param array_access_member: The ArrayMixin member child of the
                                    node.
        :type array_access_member: \
                :py:class:psyclone.psyir.nodes.array_mixin.ArrayMixin`
        :param private_list: The list of private References used in this task
                             region.
        :type private_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param firstprivate_list: The list of firstprivate References used in
                                  this task region.
        :type firstprivate_list: List of
                                 :py:class:`psyclone.psyir.nodes.Reference`
        :param shared_list: The list of shared References for this task.
        :type shared_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param out_list: The list of output References for this task.
        :type out_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        """
        # We write to this arrayref, so its shared and depend out on
        # the array.

        # Find the list of members we need to include in the final reference.
        new_member = ref.member.copy()
        sref_base = StructureReference(ref.symbol)
        sref_base.addchild(new_member)

        # Arrays are always shared at the moment, so we ignore the possibility
        # of it being private now.

        # Index list stores the set of indices to use with this ArrayMixin
        # for the depend clause.
        index_list = []
        self._evaluate_structure_with_array_reference_indexlist(
            ref,
            sref_base,
            array_access_member,
            private_list,
            firstprivate_list,
            shared_list,
            index_list,
        )

        # So we have a list of (lists of) indices
        # [ [index1, index4], index2, index3] so convert these
        # to an ArrayReference again.
        # To create all combinations, we use itertools.product
        # We have to create a new list which only contains lists.
        new_index_list = []
        for element in index_list:
            if isinstance(element, list):
                new_index_list.append(element)
            else:
                new_index_list.append([element])
        combinations = itertools.product(*new_index_list)
        for temp_list in combinations:
            # We need to make copies of the members as each
            # member can only be the child of one ArrayRef
            final_list = []
            for element in temp_list:
                final_list.append(element.copy())
            final_member = ArrayMember.create(
                array_access_member.name, list(final_list)
            )
            sref_copy = sref_base.copy()
            if len(sref_copy.children) > 0:
                sref_children = sref_copy
                while isinstance(sref_children.children[0], Member):
                    sref_children = sref_children.children[0]
                sref_children.parent.children[0] = final_member
            # Add dclause into the out_list if required
            if sref_copy not in out_list:
                out_list.append(sref_copy)
        # Add to shared_list (for explicity)
        sclause = Reference(ref.symbol)
        if sclause not in shared_list:
            shared_list.append(sclause)

    def _evaluate_write_arrayref(
        self, ref, private_list, firstprivate_list, shared_list, out_list
    ):
        """
        Evaluates a write access to an Array inside the task region, and
        computes any data-sharing clauses and dependency clauses based upon the
        access.

        This is done by evaluating each of the array indices, and determining
        whether they are:
        1. A Literal index, in which case we need a dependency to that
           specific section of the array.
        2. A Reference index, in which case we need a dependency to the section
           of the array represented by that Reference.
        3. A Binary Operation, in which case the code calls
          `_handle_index_binop` to evaluate any additional dependencies.

        Once these have been computed, any new dependencies are added into the
        out_list, and the array reference itself will be added to the
        shared_list if not already present.

        :param ref: The Reference to be evaluated.
        :type ref: :py:class:`psyclone.psyir.nodes.Reference`
        :param private_list: The list of private References used in this task
                             region.
        :type private_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param firstprivate_list: The list of firstprivate References used in
                                  this task region.
        :type firstprivate_list: List of
                                 :py:class:`psyclone.psyir.nodes.Reference`
        :param shared_list: The list of shared References for this task.
        :type shared_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param out_list: The list of output References for this task.
        :type out_list: List of :py:class:`psyclone.psyir.nodes.Reference`

        :raises GenerationError: If an array index is a shared variable.
        """
        # We write to this arrayref, so its shared and depend out on
        # the array.

        # Arrays are always shared at the moment, so we ignore the possibility
        # of it being private now.

        # Index list stores the set of indices to use with this ArrayMixin
        # for the depend clause.
        index_list = []
        # Work out the indices needed.
        for dim, index in enumerate(ref.indices):
            if isinstance(index, Literal):
                # Literals are just a value, just use the value.
                index_list.append(index.copy())
            elif isinstance(index, Reference):
                index_private = self._is_reference_private(index)
                # Check whether the reference is to a child loop variable.
                child_loop_vars = []
                for child_loop in self.walk(Loop):
                    if child_loop.variable not in self._proxy_loop_vars:
                        child_loop_vars.append(child_loop.variable)

                if index_private:
                    if (
                        index not in private_list
                        and index not in firstprivate_list
                    ):
                        firstprivate_list.append(index.copy())
                    # Special case 1. If index belongs to a child loop
                    # that is NOT a proxy for a parent loop, then we
                    # can only do as well as guessing the entire range
                    # of the loop is used.
                    if index.symbol in child_loop_vars:
                        # Return a :
                        one = Literal(str(dim + 1), INTEGER_TYPE)
                        lbound = BinaryOperation.create(
                            BinaryOperation.Operator.LBOUND,
                            Reference(ref.symbol),
                            one.copy(),
                        )
                        ubound = BinaryOperation.create(
                            BinaryOperation.Operator.UBOUND,
                            Reference(ref.symbol),
                            one.copy(),
                        )
                        full_range = Range.create(lbound, ubound)
                        index_list.append(full_range)
                    elif index.symbol in self._proxy_loop_vars:
                        # Special case 2. the index is a proxy for a parent
                        # loop's variable. In this case, we add a reference to
                        # the parent loop's value. We create a list of all
                        # possible variants, as we might have multiple values
                        # set for a value, e.g. for a boundary condition if
                        # statement.
                        if len(index_list) <= dim:
                            index_list.append([])
                        for temp_ref in self._proxy_loop_vars[index.symbol][
                            "parent_node"
                        ]:
                            parent_ref = temp_ref.copy()
                            if isinstance(parent_ref, BinaryOperation):
                                quick_list = []
                                self._handle_index_binop(
                                    parent_ref,
                                    quick_list,
                                    firstprivate_list,
                                    private_list,
                                )
                                for element in quick_list:
                                    if isinstance(element, list):
                                        index_list[dim].extend(element)
                                    else:
                                        index_list[dim].append(element)
                            else:
                                index_list[dim].append(parent_ref)
                    else:
                        # Final case is just a generic Reference, in which case
                        # just copy the Reference
                        index_list.append(index.copy())
                else:
                    raise GenerationError(
                        "Shared variable access used "
                        "as an index inside an "
                        "OMPTaskDirective which is not "
                        f"supported. Variable name is {index}"
                    )
            elif isinstance(index, BinaryOperation):
                self._handle_index_binop(
                    index, index_list, firstprivate_list, private_list
                )

        # So we have a list of (lists of) indices
        # [ [index1, index4], index2, index3] so convert these
        # to an ArrayReference again.
        # To create all combinations, we use itertools.product
        # We have to create a new list which only contains lists.
        new_index_list = []
        for element in index_list:
            if isinstance(element, list):
                new_index_list.append(element)
            else:
                new_index_list.append([element])
        combinations = itertools.product(*new_index_list)
        for temp_list in combinations:
            # We need to make copies of the members as each
            # member can only be the child of one ArrayRef
            final_list = []
            for element in temp_list:
                final_list.append(element.copy())
            dclause = ArrayReference.create(ref.symbol, list(final_list))
            # Add dclause into the out_list if required
            if dclause not in out_list:
                out_list.append(dclause)
        # Add to shared_list (for explicity)
        sclause = Reference(ref.symbol)
        if sclause not in shared_list:
            shared_list.append(sclause)

    def _evaluate_write_baseref(
        self, ref, private_list, shared_list, out_list
    ):
        """
        Evaluates a write to a non-ArrayReference reference. If the variable
        is declared private in the parent parallel region, then the variable
        is added to the private clause for this task.

        If the variable is not private (therefore is shared), it is added to
        the shared and output dependence lists for this task region.

        :param ref: The Reference to be evaluated.
        :type ref: :py:class:`psyclone.psyir.nodes.Reference`
        :param private_list: The list of private References used in this task
                             region.
        :type private_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param shared_list: The list of shared References for this task.
        :type shared_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param out_list: The list of output References for this task.
        :type out_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        """
        # Check if its a private variable
        is_private = self._is_reference_private(ref)
        # If its private should add it to private list if not already present
        if is_private and ref not in private_list:
            private_list.append(ref.copy())
        # Otherwise its a shared variable
        if not is_private:
            if ref not in shared_list:
                shared_list.append(ref.copy())
            if ref not in out_list:
                out_list.append(ref.copy())

    def _evaluate_write_reference(
        self, ref, private_list, firstprivate_list, shared_list, out_list
    ):
        """
        Evaluates a write to any Reference in the task region. This is done by
        calling the appropriate subfunction depending on the type of the
        Reference.

        :param ref: The Reference to be evaluated.
        :type ref: :py:class:`psyclone.psyir.nodes.Reference`
        :param private_list: The list of private References used in this task
                             region.
        :type private_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param firstprivate_list: The list of firstprivate References used in
                                  this task region.
        :type firstprivate_list: List of
                                 :py:class:`psyclone.psyir.nodes.Reference`
        :param shared_list: The list of shared References for this task.
        :type shared_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param out_list: The list of output References for this task.
        :type out_list: List of :py:class:`psyclone.psyir.nodes.Reference`

        :raises GenerationError: If a StructureReference containing multiple \
                                 ArrayMember or ArrayOfStructuresMember as \
                                 children is found.
        """
        if isinstance(ref, (ArrayReference, ArrayOfStructuresReference)):
            self._evaluate_write_arrayref(
                ref, private_list, firstprivate_list, shared_list, out_list
            )
        elif isinstance(ref, StructureReference):
            # If the StructureReference contains an ArrayMixin then
            # we need to treat it differently, like an arrayref, however
            # the code to handle an arrayref is not compatible with more
            # than one ArrayMixin child
            array_children = ref.walk((ArrayOfStructuresMember, ArrayMember))
            if len(array_children) > 0:
                if len(array_children) > 1:
                    raise GenerationError(
                        "Doesn't support a "
                        "StructureReference child with multiple array "
                        "accessing members."
                    )
                self._evaluate_structure_with_array_reference_write(
                    ref,
                    array_children[0],
                    private_list,
                    firstprivate_list,
                    shared_list,
                    out_list,
                )
            else:
                # This is treated the same as a Reference, but we create a
                # Reference to the symbol to handle.
                base_ref = Reference(ref.symbol)
                self._evaluate_write_baseref(
                    base_ref, private_list, shared_list, out_list
                )
        elif isinstance(ref, Reference):
            self._evaluate_write_baseref(
                ref, private_list, shared_list, out_list
            )

    def _evaluate_assignment(
        self,
        node,
        private_list,
        firstprivate_list,
        shared_list,
        in_list,
        out_list,
    ):
        """
        Evaluates an Assignment node within this task region. This is done
        by calling the appropriate subfunction on each reference on the
        LHS and RHS of the Assignment.

        :param ref: The Assignment to be evaluated.
        :type ref: :py:class:`psyclone.psyir.nodes.Assignment`
        :param private_list: The list of private References used in this task
                             region.
        :type private_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param firstprivate_list: The list of firstprivate References used in
                                  this task region.
        :type firstprivate_list: List of
                                 :py:class:`psyclone.psyir.nodes.Reference`
        :param shared_list: The list of shared References for this task.
        :type shared_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param in_list: The list of input References for this task.
        :type in_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param out_list: The list of output References for this task.
        :type out_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        """
        lhs = node.children[0]
        rhs = node.children[1]
        # Evaluate LHS
        self._evaluate_write_reference(
            lhs, private_list, firstprivate_list, shared_list, out_list
        )

        # Evaluate RHS
        references = rhs.walk(Reference)

        # If RHS involves a parent loop variable, then our lhs node is a proxy
        # loop variable.
        # This handles the case where we have a parent loop, e.g.
        # do i = x, y, 32
        # and we have code inside the task region which does
        # my_temp_var = i+1
        # array(my_temp_var) = ...
        # In this case, we need to have this variable as an extra copy of
        # the proxy parent loop variable. Additionally, there can be multiple
        # value set for this variable, so we need to store all possible ones
        # (as they could occur inside an if/else statement and both values)
        # be visible for each set.
        added = False
        for ref in references:
            if isinstance(ref.parent, ArrayMixin):
                continue
            for index, parent_var in enumerate(self._parent_loop_vars):
                if ref.symbol == parent_var:
                    if lhs.symbol in self._proxy_loop_vars:
                        if (
                            rhs
                            not in self._proxy_loop_vars[lhs.symbol][
                                "parent_node"
                            ]
                        ):
                            self._proxy_loop_vars[lhs.symbol][
                                "parent_node"
                            ].append(rhs.copy())
                    else:
                        subdict = {}
                        subdict["parent_var"] = parent_var
                        subdict["parent_node"] = [rhs.copy()]
                        subdict["loop"] = node
                        subdict["parent_loop"] = self._parent_loops[index]

                        self._proxy_loop_vars[lhs.symbol] = subdict
                    added = True
            if added:
                break

        # If RHS involves a proxy loop variable, then our lhs node is also a
        # proxy to that same variable
        # This handles the case where we have a parent loop, e.g.
        # do i = x, y, 32
        # and we have code inside the task region which does
        # for j = i, i + 32, 1
        # my_temp_var = j+1
        # array(my_temp_var) = ...
        # In this case, we need to have this variable as an extra copy of
        # the proxy parent loop variable. Additionally, there can be multiple
        # value set for this variable, so we need to store all possible ones
        # (as they could occur inside an if/else statement and both values)
        # be visible for each set.
        added = False
        key_list = list(self._proxy_loop_vars.keys())
        for ref in references:
            if isinstance(ref.parent, ArrayMixin):
                continue
            for index, proxy_var in enumerate(key_list):
                if ref.symbol == proxy_var:
                    if lhs.symbol in self._proxy_loop_vars:
                        if (
                            rhs
                            not in self._proxy_loop_vars[lhs.symbol][
                                "parent_node"
                            ]
                        ):
                            self._proxy_loop_vars[lhs.symbol][
                                "parent_node"
                            ].append(rhs.copy())
                    else:
                        subdict = {}
                        subdict["parent_var"] = \
                            self._proxy_loop_vars[proxy_var]["parent_var"]
                        subdict["parent_node"] = [rhs.copy()]
                        subdict["loop"] = node
                        subdict["parent_loop"] = self._parent_loops[index]
                        self._parent_loops.append(self._parent_loops[index])
                        self._proxy_loop_vars[lhs.symbol] = subdict
                    added = True
            if added:
                break

        for ref in references:
            self._evaluate_readonly_reference(
                ref, private_list, firstprivate_list, shared_list, in_list
            )

    def _evaluate_loop(
        self,
        node,
        private_list,
        firstprivate_list,
        shared_list,
        in_list,
        out_list,
    ):
        """
        Evaluates a Loop node within this task Region. This is done in several
        steps:
        1. Check the loop variable, start/stop/step values, and ensure that
           they are valid. The loop variable must not be shared and the start,
           stop, and step variables are not ArrayReferences. It also detects if
           the loop variable is a "proxy", i.e. it represents a parent loop's
           variable as a chunked loop variable. Any variables that are not yet
           private, firstprivate or shared will be declared as firstprivate (as
           they are read before being accessed elsewhere).
        2. Loop through each of the nodes in the Loop's Schedule child, and
           evaluate them through the _evaluate_node call.

        :param node: The Loop to be evaluated.
        :type node: :py:class:`psyclone.psyir.nodes.Loop`
        :param private_list: The list of private References used in this task
                             region.
        :type private_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param firstprivate_list: The list of firstprivate References used in
                                  this task region.
        :type firstprivate_list: List of
                                 :py:class:`psyclone.psyir.nodes.Reference`
        :param shared_list: The list of shared References for this task.
        :type shared_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param in_list: The list of input References for this task.
        :type in_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param out_list: The list of output References for this task.
        :type out_list: List of :py:class:`psyclone.psyir.nodes.Reference`

        :raises GenerationError: If the loop variable is a shared variable.
        :raises GenerationError: If the loop start, stop or step expression
                                 contains an ArrayReference.
        """
        # Look at loop bounds etc first.
        # Find our loop initialisation, variable and bounds
        loop_var = node.variable
        start_val = node.start_expr
        stop_val = node.stop_expr
        step_val = node.step_expr

        to_remove = None

        # Check if we have a loop of type do ii = i where i is a parent loop
        # variable.
        start_val_refs = start_val.walk(Reference)
        if len(start_val_refs) == 1 and isinstance(
            start_val_refs[0], Reference
        ):
            # Loop through the parent loop variables
            for index, parent_var in enumerate(self._parent_loop_vars):
                # If its a parent loop variable, we need to make it a proxy
                # variable for now.
                if start_val_refs[0].symbol == parent_var:
                    to_remove = loop_var
                    # Store the loop and parent_var
                    subdict = {}
                    subdict["parent_var"] = parent_var
                    subdict["parent_node"] = [Reference(parent_var)]
                    subdict["loop"] = node
                    subdict["parent_loop"] = self._parent_loops[index]

                    self._proxy_loop_vars[to_remove] = subdict
                    break

        # Loop variable is private unless already set as firstprivate.
        # Throw exception if shared
        loop_var_ref = Reference(loop_var)
        if loop_var not in self._parallel_private:
            raise GenerationError(
                "Found shared loop variable which is "
                "not allowed in OpenMP Task directive. "
                f"Variable name is {loop_var_ref.name}"
            )
        if loop_var_ref not in firstprivate_list:
            if loop_var_ref not in private_list:
                private_list.append(loop_var_ref)

        # If we have a proxy variable, the parent loop variable has to be
        # firstprivate
        if to_remove is not None:
            parent_var_ref = Reference(
                self._proxy_loop_vars[to_remove]["parent_var"]
            )
            if parent_var_ref not in firstprivate_list:
                firstprivate_list.append(parent_var_ref.copy())

        # For all non-array accesses we make them firstprivate unless they
        # are already declared as something else
        for ref in start_val_refs:
            if isinstance(ref, (ArrayReference, ArrayOfStructuresReference)):
                raise GenerationError(
                    f"{type(ref).__name__} not supported in "
                    "the start variable of a Loop in a "
                    "OMPTaskDirective node."
                )
            # If we have a StructureReference, then we need to only add the
            # base symbol to the lists
            if isinstance(ref, StructureReference):
                ref_copy = Reference(ref.symbol)
                # start_val can't be written to in Fortran so if its a
                # structure we should make it shared
                # Only the base Structure is allowed to be in a depend clause.
                if ref_copy not in shared_list:
                    shared_list.append(ref_copy.copy())
                if ref_copy not in in_list:
                    in_list.append(ref_copy.copy())
                ref = ref_copy
            if (
                ref not in firstprivate_list
                and ref not in private_list
                and ref not in shared_list
            ):
                firstprivate_list.append(ref.copy())

        stop_val_refs = stop_val.walk(Reference)
        for ref in stop_val_refs:
            if isinstance(ref, (ArrayReference, ArrayOfStructuresReference)):
                raise GenerationError(
                    f"{type(ref).__name__} not supported in "
                    "the stop variable of a Loop in a "
                    "OMPTaskDirective node."
                )
            # If we have a StructureReference, then we need to only add the
            # base symbol to the lists
            if isinstance(ref, StructureReference):
                ref_copy = Reference(ref.symbol)
                # stop_val can't be written to in Fortran so if its a structure
                # we should make it shared
                # Only the base Structure is allowed to be in a depend clause.
                if ref_copy not in shared_list:
                    shared_list.append(ref_copy.copy())
                if ref_copy not in in_list:
                    in_list.append(ref_copy.copy())
                ref = ref_copy
            if (
                ref not in firstprivate_list
                and ref not in private_list
                and ref not in shared_list
            ):
                firstprivate_list.append(ref.copy())

        step_val_refs = step_val.walk(Reference)
        for ref in step_val_refs:
            if isinstance(ref, (ArrayReference, ArrayOfStructuresReference)):
                raise GenerationError(
                    f"{type(ref).__name__} not supported in "
                    "the step variable of a Loop in a "
                    "OMPTaskDirective node."
                )
            # If we have a StructureReference, then we need to only add the
            # base symbol to the lists
            if isinstance(ref, StructureReference):
                ref_copy = Reference(ref.symbol)
                # stop_val can't be written to in Fortran so if its a structure
                # we should make it shared
                # Only the base Structure is allowed to be in a depend clause.
                if ref_copy not in shared_list:
                    shared_list.append(ref_copy.copy())
                if ref_copy not in in_list:
                    in_list.append(ref_copy.copy())
                ref = ref_copy
            if (
                ref not in firstprivate_list
                and ref not in private_list
                and ref not in shared_list
            ):
                firstprivate_list.append(ref.copy())

        # Finished handling the loop bounds now

        # Recurse to the children
        for child_node in node.children[3].children:
            self._evaluate_node(
                child_node,
                private_list,
                firstprivate_list,
                shared_list,
                in_list,
                out_list,
            )

        # Remove any stuff added to proxy_loop_vars etc. if needed
        if to_remove is not None:
            self._proxy_loop_vars.pop(to_remove)

    def _evaluate_ifblock(
        self,
        node,
        private_list,
        firstprivate_list,
        shared_list,
        in_list,
        out_list,
    ):
        """
        Evaluates an ifblock inside a task region. This is done by calling
        _evaluate_readonly_reference on each Reference inside the if condition,
        and by calling _evaluate_node on each Node inside the if_body and
        else_body.

        :param node: The IfBlock to be evaluated.
        :type node: :py:class:`psyclone.psyir.nodes.IfBlock`
        :param private_list: The list of private References used in this task
                             region.
        :type private_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param firstprivate_list: The list of firstprivate References used in
                                  this task region.
        :type firstprivate_list: List of
                                 :py:class:`psyclone.psyir.nodes.Reference`
        :param shared_list: The list of shared References for this task.
        :type shared_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param in_list: The list of input References for this task.
        :type in_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param out_list: The list of output References for this task.
        :type out_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        """
        for ref in node.condition.walk(Reference):
            self._evaluate_readonly_reference(
                ref, private_list, firstprivate_list, shared_list, in_list
            )

        # Recurse to the children
        # If block
        for child_node in node.if_body.children:
            self._evaluate_node(
                child_node,
                private_list,
                firstprivate_list,
                shared_list,
                in_list,
                out_list,
            )
        # Else block if present
        if node.else_body is not None:
            for child_node in node.else_body.children:
                self._evaluate_node(
                    child_node,
                    private_list,
                    firstprivate_list,
                    shared_list,
                    in_list,
                    out_list,
                )

    def _evaluate_node(
        self,
        node,
        private_list,
        firstprivate_list,
        shared_list,
        in_list,
        out_list,
    ):
        """
        Evaluates a generic Node inside the task region. Calls the appropriate
        call depending on whether the node is an Assignment, Loop or IfBlock.

        :param node: The Node to be evaluated.
        :type node: :py:class:`psyclone.psyir.nodes.Node`
        :param private_list: The list of private References used in this task
                             region.
        :type private_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param firstprivate_list: The list of firstprivate References used in
                                  this task region.
        :type firstprivate_list: List of
                                 :py:class:`psyclone.psyir.nodes.Reference`
        :param shared_list: The list of shared References for this task.
        :type shared_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param in_list: The list of input References for this task.
        :type in_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        :param out_list: The list of output References for this task.
        :type out_list: List of :py:class:`psyclone.psyir.nodes.Reference`
        """
        # For the node, check if it is Loop, Assignment or IfBlock
        if isinstance(node, Assignment):
            # Resolve assignment
            self._evaluate_assignment(
                node,
                private_list,
                firstprivate_list,
                shared_list,
                in_list,
                out_list,
            )
        elif isinstance(node, Loop):
            # Resolve loop
            self._evaluate_loop(
                node,
                private_list,
                firstprivate_list,
                shared_list,
                in_list,
                out_list,
            )
        elif isinstance(node, IfBlock):
            # Resolve IfBlock
            self._evaluate_ifblock(
                node,
                private_list,
                firstprivate_list,
                shared_list,
                in_list,
                out_list,
            )

        # All other node types are ignored as they shouldn't affect
        # dependency computation, as these are the only nodes that
        # have read or write accesses that can get to this function
        # as Calls are prohibited in validation.

    def _compute_clauses(self):
        """
        Computes the clauses for this OMPTaskDirective.

        The OMPTaskDirective must have exactly 1 child, which must be a Loop.
        Upon confirming this, the function calls _evaluate_node to compute all
        data-sharing attributes and dependencies.
        The clauses are then built up from those, and returned.

        :raises GenerationError: If the OMPTaskDirective has multiple children.
        :raises GenerationError: If the OMPTaskDirective's child is not a Loop.

        :returns: The clauses computed for this OMPTaskDirective.
        :rtype: List of [OMPPrivateClause, OMPFirstprivateClause,
                         OMPSharedClause, OMPDependClause, OMPDependClause]
        """

        # These lists will store PSyclone nodes which are to be added to the
        # clauses for this OMPTaskDirective.
        private_list = []
        firstprivate_list = []
        shared_list = []
        in_list = []
        out_list = []

        # Reset this in case we already computed clauses before but are
        # recomputing them (usually due to a code change or multiple outputs).
        self._proxy_loop_vars = {}

        # Find all the parent loop variables
        self._find_parent_loop_vars()

        # Find the child loop node, and check our schedule contains a single
        # loop for now.
        if len(self.children[0].children) != 1:
            raise GenerationError(
                "OMPTaskDirective must have exactly one Loop"
                f" child. Found "
                f"{len(self.children[0].children)} "
                "children."
            )
        if not isinstance(self.children[0].children[0], Loop):
            raise GenerationError(
                "OMPTaskDirective must have exactly one Loop"
                " child. Found "
                f"{type(self.children[0].children[0])}"
            )
        self._evaluate_node(
            self.children[0].children[0],
            private_list,
            firstprivate_list,
            shared_list,
            in_list,
            out_list,
        )

        # Make the clauses to return.
        # We skip references to constants as we don't need them.
        # Constants will never be private.
        private_clause = OMPPrivateClause()
        for ref in private_list:
            private_clause.addchild(ref)
        firstprivate_clause = OMPFirstprivateClause()
        for ref in firstprivate_list:
            firstprivate_clause.addchild(ref)
        shared_clause = OMPSharedClause()
        for ref in shared_list:
            shared_clause.addchild(ref)

        in_clause = OMPDependClause(
            depend_type=OMPDependClause.DependClauseTypes.IN
        )
        # For input references we need to ignore references to constant
        # symbols. This means we need to try to get external symbols as well
        for ref in in_list:
            if isinstance(ref.symbol, DataSymbol) and ref.symbol.is_constant:
                continue
            if (ref.symbol.is_import and
                    ref.symbol.get_external_symbol().is_constant):
                continue
            in_clause.addchild(ref)
        out_clause = OMPDependClause(
            depend_type=OMPDependClause.DependClauseTypes.OUT
        )
        for ref in out_list:
            out_clause.addchild(ref)

        return (
            private_clause,
            firstprivate_clause,
            shared_clause,
            in_clause,
            out_clause,
        )

    def lower_to_language_level(self):
        """
        Lowers the structure of the PSyIR tree inside the Directive
        to generate the Clauses that are required for this Directive.
        """
        # pylint: disable=import-outside-toplevel
        from psyclone.psyGen import Kern

        # If we find a Kern or Call child then we abort.
        # Note that if the transformation is used it will have already
        # attempted to do this inlining.
        if len(self.walk((Kern, Call))) > 0:
            raise GenerationError(
                "Attempted to lower to OMPTaskDirective "
                "node, but the node contains a Call or Kern "
                "which must be inlined first."
            )

        # Create the clauses
        (
            private_clause,
            firstprivate_clause,
            shared_clause,
            in_clause,
            out_clause,
        ) = self._compute_clauses()

        if len(self.children) < 2 or private_clause != self.children[1]:
            self.children[1] = private_clause
        if len(self.children) < 3 or firstprivate_clause != self.children[2]:
            self.children[2] = firstprivate_clause
        if len(self.children) < 4 or shared_clause != self.children[3]:
            self.children[3] = shared_clause
        if len(self.children) < 5 or in_clause != self.children[4]:
            self.children[4] = in_clause
        if len(self.children) < 6 or out_clause != self.children[5]:
            self.children[5] = out_clause
        super().lower_to_language_level()

        # Replace this node with an OMPTaskDirective
        childs = self.pop_all_children()
        replacement = OMPTaskDirective(children=childs, lowering=True)
        self.replace_with(replacement)
