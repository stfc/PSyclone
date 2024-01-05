# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council.
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
""" This module contains the implementation of the Dynamic OpenMP Task
Directive node, which is used pre-lowering to represent Task Directives."""

import itertools
import math
from collections import namedtuple

from psyclone.errors import GenerationError, InternalError
from psyclone.psyir.nodes import (
    ArrayMember,
    ArrayOfStructuresReference,
    ArrayReference,
    Assignment,
    Call,
    IfBlock,
    IntrinsicCall,
    Reference,
    StructureReference,
)
from psyclone.psyir.nodes.array_mixin import ArrayMixin
from psyclone.psyir.nodes.array_of_structures_member import (
    ArrayOfStructuresMember,
)
from psyclone.psyir.nodes.operation import BinaryOperation
from psyclone.psyir.nodes.loop import Loop
from psyclone.psyir.nodes.literal import Literal
from psyclone.psyir.nodes.member import Member
from psyclone.psyir.nodes.omp_clauses import (
    OMPPrivateClause,
    OMPFirstprivateClause,
    OMPDependClause,
    OMPSharedClause,
)
from psyclone.psyir.nodes.omp_directives import (
    OMPParallelDirective,
)
from psyclone.psyir.nodes.omp_task_directive import (
    OMPTaskDirective
)
from psyclone.psyir.symbols import INTEGER_TYPE, DataSymbol


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

    # The named tuple object to handle the creation of ProxyVar
    # elements. These contain the parent_var (The variable this
    # is a proxy to), and the node and loop containing the parent
    # variable. It also contains the loop with the proxy variable as
    # its loop variable.
    _proxy_vars = namedtuple(
            'ProxyVars', ['parent_var', 'parent_node',
                          'loop', 'parent_loop']
    )

    # The named tuple object containing all the clause lists that
    # are passed throughout the functions. Each of the members
    # of this tuple are list of References that will be added to
    # the appropriate clause when the clauses are created.
    _clause_lists = namedtuple(
            'ClauseLists', ['private_list', 'firstprivate_list',
                            'shared_list', 'in_list', 'out_list']
    )

    # List of Fortran Intrinsics allowed to appear inside a
    # task directive.
    _allowed_intrinsics = [
        IntrinsicCall.Intrinsic.ABS,
        IntrinsicCall.Intrinsic.ACOS,
        IntrinsicCall.Intrinsic.ACOSH,
        IntrinsicCall.Intrinsic.ASIN,
        IntrinsicCall.Intrinsic.ASINH,
        IntrinsicCall.Intrinsic.ATAN,
        IntrinsicCall.Intrinsic.ATAN2,
        IntrinsicCall.Intrinsic.ATANH,
        IntrinsicCall.Intrinsic.CEILING,
        IntrinsicCall.Intrinsic.COS,
        IntrinsicCall.Intrinsic.COSH,
        IntrinsicCall.Intrinsic.ERF,
        IntrinsicCall.Intrinsic.EXP,
        IntrinsicCall.Intrinsic.FLOOR,
        IntrinsicCall.Intrinsic.INT,
        IntrinsicCall.Intrinsic.LBOUND,
        IntrinsicCall.Intrinsic.LEN,
        IntrinsicCall.Intrinsic.LOG,
        IntrinsicCall.Intrinsic.LOG10,
        IntrinsicCall.Intrinsic.MAX,
        IntrinsicCall.Intrinsic.MIN,
        IntrinsicCall.Intrinsic.MODULO,
        IntrinsicCall.Intrinsic.REAL,
        IntrinsicCall.Intrinsic.SIGN,
        IntrinsicCall.Intrinsic.SIN,
        IntrinsicCall.Intrinsic.SINH,
        IntrinsicCall.Intrinsic.SIZE,
        IntrinsicCall.Intrinsic.SQRT,
        IntrinsicCall.Intrinsic.TAN,
        IntrinsicCall.Intrinsic.TANH,
        IntrinsicCall.Intrinsic.UBOUND,
    ]

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
        self._child_loop_vars = []
        self._parent_parallel = None
        self._parallel_private = None
        self._parallel_firstprivate = None

        # We need to do extra steps when inside a Kern to correctly identify
        # symbols.
        self._in_kern = False

    def _array_for_clause_combination_helper(self, ref, temp_list,
                                             base_member=None):
        '''
        Helper function for creating an ArrayReference or StructureReference
        containing an ArrayMember to place into the
        relevant in or out dependency list when computing index combinations.

        :param ref: The reference whose symbol the created ArrayReference is
                    to reference.
        :type ref: :py:class:`psyclone.psyir.nodes.Reference`
        :param temp_list: The current list of indices to add to the created
                          ArrayReference
        :type temp_list: List[:py:class:`psyclone.psyir.nodes.Node`]
        :param base_member: Optional argument containing the array member
                            child of ref to duplicate with
                            the indices specified by temp_list
        :type base_member: Optional[
                           :py:class:`psyclone.psyir.nodes.ArrayMember`]

        :returns: an ArrayReference to the provided symbol and with the
                  provided indices, or a StructureReference containing the
                  new ArrayMember if base_member is provided.
        :rtype: :py:class:`psyclone.psyir.nodes.ArrayReference` or
                :py:class:`psyclone.psyir.nodes.StructureReference`

        '''
        final_list = [element.copy() for element in temp_list]
        if base_member:
            final_member = ArrayMember.create(base_member.name, final_list)
            sref_copy = ref.copy()
            # Copying the StructureReference includes the Members
            members = sref_copy.walk(Member)
            # Replace the last one with the new ArrayMember
            members[-1].replace_with(final_member)
            return sref_copy

        dclause = ArrayReference.create(ref.symbol, final_list)
        return dclause

    def _add_dependencies_from_index_list(self, index_list, dependency_list,
                                          reference, array_access_member=None):
        '''
        Computes all of the dependency combinations for an array access and
        adds them to the provided dependency_list.
        If array_access_member is None, then ArrayReferences will be added,
        otherwise StructureReferences containing ArrayMembers will be added.

        :param index_list: A list of (lists of) indices to turn into
                           dependencies.
        :type index_list: List[Union[List[
                               :py:class:`psyclone.psyir.nodes.Reference]],
                               :py:class:`psyclone.psyir.nodes.Reference]
        :param dependency_list: The dependency list to add the newly created
                                dependencies to.
        :type dependency_list: List[:py:class`psyclone.psyir.nodes.Reference]
        :param reference: The reference containing the array access to create
                          new dependencies to.
        :type reference: Union[:py:class:`psyclone.psyir.nodes.ArrayReference`,
                         :py:class:`psyclone.psyir.nodes.StructureReference`]
        :param array_access_member: optional argument provided if reference
                                    is a StructureReference to the ArrayMember
                                    containing the dependencies.
        '''
        # So we have a list of (lists of) indices
        # [ [index1, index4], index2, index3] so convert these
        # to an ArrayReference again.
        # To create all combinations, we use itertools.product
        # We have to create a new list which only contains lists.
        new_index_list = [element if isinstance(element, list) else [element]
                          for element in index_list]

        combinations = itertools.product(*new_index_list)
        for temp_list in combinations:
            new_ref = self._array_for_clause_combination_helper(
                    reference, temp_list, array_access_member
            )

            if new_ref not in dependency_list:
                dependency_list.append(new_ref)

    def _find_parent_loop_vars(self):
        """
        Finds the loop variable of each parent loop inside the same
        OMPParallelDirective and stores them in the _parent_loop_vars member.
        Also stores the parent OMPParallelDirective in _parent_parallel.

        :raises GenerationError: if no ancestor OMPParallelDirective is
                                 found.
        """
        anc = self.ancestor((OMPParallelDirective, Loop))
        while isinstance(anc, Loop):
            # Store the loop variable of each parent loop
            var = anc.variable
            self._parent_loop_vars.append(var)
            self._parent_loops.append(anc)
            # Recurse up the tree
            anc = anc.ancestor((OMPParallelDirective, Loop))

        if not isinstance(anc, OMPParallelDirective):
            raise GenerationError("Failed to find an ancestor "
                                  "OMPParallelDirective which is required "
                                  "to compute dependencies of a "
                                  "(Dynamic)OMPTaskDirective.")

        # Store the parent parallel directive node
        self._parent_parallel = anc
        self._parallel_private, self._parallel_firstprivate, _ = \
            anc.infer_sharing_attributes()
        self._parallel_private = self._parallel_private.union(
                self._parallel_firstprivate)

    def _handle_proxy_loop_index(self, index_list, dim, index,
                                 clause_lists):
        '''
        Handles the special case where an index is a proxy loop variable
        to a parent of the node. In this case, we add a reference to the
        parent's loop value to the index_list, and create a list of all
        possible variants, as we might have multiple values set for the
        reference, e.g. for a boundary condition if statement.

        :param index_list: The list to contain the new indices.
        :type index_list: List[:py:class:`psyclone.psyir.nodes.Reference`]
        :param int dim: The dimension of the index in the containing
                        ArrayReference
        :param index: The index that is a proxy to a parent loop variable.
        :type index: :py:class:`psyclone.psyir.nodes.Reference`
        :param clause_lists: The namedtuple containing the lists storing the
                             clauses.
        :type clause_lists: namedtuple(
                            private_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            firstprivate_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            shared_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            in_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            out_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ])

        '''
        # Ensure we have the correct number of entries in index_list
        while len(index_list) <= dim:
            index_list.append([])
        # For each reference our index proxies add the appropriate
        # set of indices to the index_list
        for temp_ref in self._proxy_loop_vars[index.symbol].\
                parent_node:
            parent_ref = temp_ref.copy()
            if isinstance(parent_ref, BinaryOperation):
                quick_list = []
                self._handle_index_binop(
                    parent_ref,
                    quick_list,
                    clause_lists
                )
                for element in quick_list:
                    if isinstance(element, list):
                        index_list[dim].extend(element)
                    else:
                        index_list[dim].append(element)
            else:
                # parent_ref is a Reference, so we just append
                # the index.
                index_list[dim].append(parent_ref)

    def _is_reference_private(self, ref):
        """
        Determines whether the provided reference is private or shared in the
        enclosing parallel region.

        :param ref: The Reference object to be determined if it is private
                    or shared.
        :type ref: :py:class:`psyclone.psyir.nodes.Reference`

        :returns: True if ref is private, else False.
        :rtype: bool
        """
        for parent_sym in self._parallel_private:
            if ref.symbol.name == parent_sym.name:
                return True
        return False

    def _evaluate_readonly_baseref(
        self, ref, clause_lists
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
        :param clause_lists: The namedtuple containing the lists storing the
                             clauses.
        :type clause_lists: namedtuple(
                            private_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            firstprivate_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            shared_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            in_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            out_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ])
        """
        symbol = ref.symbol
        is_private = symbol in self._parallel_private
        if is_private:
            # If the reference is private in the parent parallel,
            # then it is added to the firstprivate clause for this
            # task if it has not yet been written to (i.e. is not
            # yet in the private clause list).
            if (ref not in clause_lists.private_list and
                    ref not in clause_lists.firstprivate_list):
                clause_lists.firstprivate_list.append(ref.copy())
        else:
            # Otherwise it was a shared variable. Its not an
            # array so we just add the name to the in_list
            # if not already there. If its already in out_list
            # we still add it as this is the same as an inout
            # dependency
            if ref not in clause_lists.in_list:
                clause_lists.in_list.append(ref.copy())

    def _create_binops_from_step_and_divisors(self, node, ref,
                                              step_val, divisor,
                                              modulo, ref_index):
        """
        Takes a node, step_val, divisor, modulo and ref_index from
        _handle_index_binop and computes the BinaryOperation(s)
        required for them to be handled in a depend clause.

        :param node: the BinaryOperation being evaluated.
        :type node: :py:class:`psyclone.psyir.nodes.BinaryOperation`
        :param ref: the Reference representing the loop variable inside node
        :type ref: :py:class:`psyclone.psyir.nodes.Reference`
        :param int step_val: the step of the loop containing the node in
                         _handle_index_binop.
        :param int divisor: the ceiling result of literal_val / step, where
                            literal_val is the int value of the Literal child
                            of the node in _handle_index_binop.
        :param int modulo: the result of literal_val % step in
                           _handle_index_binop.
        :param int ref_index: The index of the Reference child of node from
                              _handle_index_binop.

        :returns: the BinaryOperation(s) required to be added to the
                  index_list in _handle_index_binop to satisfy the
                  dependencies.
        :rtype: Tuple[:py:class`psyclone.psyir.nodes.BinaryOperation`,
                      Union[:py:class`psyclone.psyir.nodes.BinaryOperation`,
                            NoneType]]

        """
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
            # Setup vars to do ref OP step when we then
            # create the BinaryOperations to represent this
            # access
            first_arg = ref.copy()
            alt_first_arg = first_arg.copy()
            second_arg = step.copy()
            alt_second_arg = step2.copy() if step2 else None
        else:
            # We have Literal OP Ref
            # Setup vars to do step OP ref when we then
            # create the BinaryOperations to represent this
            # access
            first_arg = step.copy()
            alt_first_arg = step2.copy() if step2 else None
            second_arg = ref.copy()
            alt_second_arg = second_arg.copy()
        # Create the BinaryOperations for this access according to the
        # vars we set up.
        binop = BinaryOperation.create(
            node.operator, first_arg, second_arg
        )
        # If modulo is non-zero then we need a second binop
        # dependency to handle the modulus, so we have two
        # dependency clauses, one which handles each of the
        # step-sized array chunks that overlaps with this access.
        if modulo != 0:
            if step2 is not None:
                binop2 = BinaryOperation.create(
                    node.operator, alt_first_arg, alt_second_arg
                )
            else:
                binop2 = ref.copy()

        return binop, binop2

    def _handle_index_binop(
        self, node, index_list, clause_lists
    ):
        """
        Evaluates an expression consisting a binary operation which is used
        to index into an array within this OpenMP task.

        For each expression, the code checks that the expression matches the
        expected format, which is [Reference] [ADD/SUB] [Literal] (or the
        opposite ordering). PSyclone does not currently support other binary
        operation indexing inside an OpenMP task.

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
        :type index_list: List[:py:class:`psyclone.psyir.nodes.Node`]
        :param clause_lists: The namedtuple containing the lists storing the
                             clauses.
        :type clause_lists: namedtuple(
                            private_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            firstprivate_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            shared_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            in_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            out_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ])

        :raises GenerationError: if this BinaryOperation is not an addition or
                                 subtraction.
        :raises GenerationError: if this BinaryOperation does not contain both
                                 a Reference and a Literal.
        :raises GenerationError: if this BinaryOperation contains a Reference
                                 to a shared variable.
        """

        # Binary Operation operator check, must be ADD or SUB.
        if (
            node.operator not in [BinaryOperation.Operator.ADD,
                                  BinaryOperation.Operator.SUB]
        ):
            raise GenerationError(
                f"Binary Operator of type {node.operator} used "
                f"as an array index '{node.debug_string()}' inside an "
                f"OMPTaskDirective which is not "
                f"supported"
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
                f"Children of BinaryOperation are of "
                f"types '{type(node.children[0]).__name__}' and "
                f"'{type(node.children[1]).__name__}', expected one "
                f"Reference and one Literal when"
                f" used as an array index inside an "
                f"OMPTaskDirective. The containing ArrayReference is "
                f"'{node.parent.debug_string()}'."
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
            ref_index = 0
        else:
            ref_index = 1

        index_symbol = node.children[ref_index].symbol
        index_private = self._is_reference_private(node.children[ref_index])
        is_proxy = index_symbol in self._proxy_loop_vars
        ref = node.children[ref_index]
        # 1-ref_index inverts 1 or 0 value so we can find the literal as well
        literal = node.children[1-ref_index]

        # We have some array access which is of the format:
        # array( Reference +/- Literal).
        # If the Reference is to a proxy (is_proxy is True) then we replace
        # the Reference with the proxy variable instead. This is the most
        # important case.
        # If the task contains Loops, and the Reference is to one of the
        # Loop variables, then we create a Range object for : for that
        # dimension, i.e. we default to assuming that the whole array
        # range is used as to be conservative.
        # All other situations are treated as a constant.

        # Find the child loops that are not proxy loop variables.
        child_loop_vars = self._child_loop_vars

        # Handle the proxy_loop case
        if is_proxy:
            # Treat it as though we came across the parent loop variable.
            parent_loop = self._proxy_loop_vars[index_symbol].parent_loop

            # Create a Reference to the real variable
            real_ref = self._proxy_loop_vars[index_symbol].parent_node.copy()
            for temp_ref in self._proxy_loop_vars[index_symbol].parent_node:
                real_ref = temp_ref.copy()
                # We have a Literal step value, and a Literal in
                # the Binary Operation. These Literals must both be
                # Integer types, so we will convert them to integers
                # and do some divison.
                step_val = int(parent_loop.step_expr.value)
                literal_val = int(literal.value)
                divisor = math.ceil(literal_val / step_val)
                modulo = literal_val % step_val
                binop, binop2 = self._create_binops_from_step_and_divisors(
                        node, real_ref, step_val, divisor, modulo, ref_index
                )
                # Add this to the list of indexes
                if binop2 is not None:
                    index_list.append([binop, binop2])
                else:
                    index_list.append(binop)
        # Proxy loop cases handled - end of "if is_proxy" statement.

        # If the variable is private:
        elif index_private:
            # If the variable is in our private list
            if ref in clause_lists.private_list:
                # If its a child loop variable
                if ref.symbol in child_loop_vars:
                    # Return a full range (:)
                    dim = len(index_list)
                    # Find the arrayref
                    array_access_member = ref.ancestor(ArrayMember)
                    if array_access_member is not None:
                        full_range = array_access_member.get_full_range(dim)
                    else:
                        arrayref = ref.parent.parent
                        full_range = arrayref.get_full_range(dim)
                    index_list.append(full_range)
                else:
                    # We have a private constant, written to inside
                    # our region, so we can't do anything better than
                    # a full range I think (since we don't know what
                    # the value is/how it changes.
                    # Return a full range (:)
                    dim = len(index_list)
                    arrayref = ref.parent.parent
                    full_range = arrayref.get_full_range(dim)
                    index_list.append(full_range)
            else:
                if ref not in clause_lists.firstprivate_list:
                    clause_lists.firstprivate_list.append(ref.copy())
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
                    binop, binop2 = self._create_binops_from_step_and_divisors(
                            node, ref, step_val, divisor,
                            modulo, ref_index
                    )
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
                f"Shared variable '{ref.debug_string()}' used "
                f"as an array index inside an "
                f"OMPTaskDirective which is not "
                f"supported. The full access is '{node.debug_string()}'."
            )

    def _evaluate_readonly_arrayref(
        self, ref, clause_lists
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
        :param clause_lists: The namedtuple containing the lists storing the
                             clauses.
        :type clause_lists: namedtuple(
                            private_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            firstprivate_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            shared_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            in_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            out_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ])

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
                child_loop_vars = self._child_loop_vars

                if index_private:
                    if (
                        index not in clause_lists.private_list
                        and index not in clause_lists.firstprivate_list
                    ):
                        clause_lists.firstprivate_list.append(index.copy())
                    # Special case 1. If index belongs to a child loop
                    # that is NOT a proxy for a parent loop, then we
                    # can only do as well as guessing the entire range
                    # of the loop is used.
                    if index.symbol in child_loop_vars:
                        # Append a full Range (i.e., :)
                        full_range = ref.get_full_range(dim)
                        index_list.append(full_range)
                    elif index.symbol in self._proxy_loop_vars:
                        # Special case 2. the index is a proxy for a parent
                        # loop's variable. In this case, we add a reference to
                        # the parent loop's value. We create a list of all
                        # possible variants, as we might have multiple values
                        # set for a value, e.g. for a boundary condition if
                        # statement.
                        self._handle_proxy_loop_index(index_list, dim, index,
                                                      clause_lists)
                    else:
                        # Final case is just a generic Reference, in which case
                        # just copy the Reference
                        index_list.append(index.copy())
                else:
                    raise GenerationError(
                        f"Shared variable access used "
                        f"as an array index inside an "
                        f"OMPTaskDirective which is not "
                        f"supported. Variable name is '{index.symbol.name}'. "
                        f"The full access is '{ref.debug_string()}'."
                    )
            elif isinstance(index, BinaryOperation):
                # Binary Operation check
                # A single binary operation, e.g. a(i+1) can require
                # multiple clauses to correctly handle.
                self._handle_index_binop(
                    index, index_list, clause_lists
                )
            elif isinstance(index, Literal):
                # Just place literal directly into the dependency clause.
                index_list.append(index.copy())
            else:
                # Not allowed type appears
                raise GenerationError(
                    f"'{type(index).__name__}' object is not allowed to "
                    f"appear in an array index "
                    f"expression inside an "
                    f"OMPTaskDirective. The index was "
                    f"'{index.debug_string()}'."
                )

        # Add all combinations of dependencies from the computed index_list
        # into in_list
        self._add_dependencies_from_index_list(index_list,
                                               clause_lists.in_list, ref)

        # Add to shared_list
        sclause = Reference(ref.symbol)
        if sclause not in clause_lists.shared_list:
            clause_lists.shared_list.append(sclause)

    def _evaluate_structure_with_array_reference_read(
        self,
        ref,
        array_access_member,
        clause_lists,
    ):
        """
        Evaluates a read-only access to an array within a structure inside the
        task region, and computes any data-sharing clauses and dependency
        clauses based upon the access.

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
        :type array_access_member:
                :py:class:psyclone.psyir.nodes.array_mixin.ArrayMixin`
        :param clause_lists: The namedtuple containing the lists storing the
                             clauses.
        :type clause_lists: namedtuple(
                            private_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            firstprivate_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            shared_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            in_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            out_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ])
        """

        # Index list stores the set of indices to use with this ArrayMixin
        # for the depend clause.
        index_list = []

        # Find the list of members we need to include in the final reference.
        new_member = ref.member.copy()
        sref_base = StructureReference(ref.symbol)
        sref_base.addchild(new_member)
        self._evaluate_structure_with_array_reference_indexlist(
            sref_base,
            array_access_member,
            clause_lists,
            index_list
        )

        # Add all combinations of dependencies from the computed index_list
        # into in_list
        self._add_dependencies_from_index_list(
                index_list, clause_lists.in_list, sref_base,
                array_access_member=array_access_member
        )
        # Add to shared_list
        sclause = Reference(ref.symbol)
        if sclause not in clause_lists.shared_list:
            clause_lists.shared_list.append(sclause)

    def _evaluate_readonly_reference(
        self, ref, clause_lists
    ):
        """
        Evaluates any Reference used in a read context. This is done by
        calling the appropriate helper functions for ArrayReferences,
        StructureReferences or other References as appropriate.

        :param node: The Reference to be evaluated.
        :type node: :py:class:`psyclone.psyir.nodes.Reference`
        :param clause_lists: The namedtuple containing the lists storing the
                             clauses.
        :type clause_lists: namedtuple(
                            private_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            firstprivate_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            shared_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            in_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            out_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ])

        :raises GenerationError: If a StructureReference containing multiple
                                 ArrayMember or ArrayOfStructuresMember as
                                 children is found.
        :raises GenerationError: If an ArrayOfStructuresReference containing
                                 an ArrayMember of ArrayOfStructuresMember as
                                 a child is found.
        """
        if isinstance(ref, (ArrayReference, ArrayOfStructuresReference)):
            # If ref is an ArrayOfStructuresReference and contains an
            # ArrayMember then we can't handle this case.
            if isinstance(ref, ArrayOfStructuresReference):
                array_children = ref.walk((ArrayOfStructuresMember,
                                           ArrayMember))
                if array_children:
                    raise GenerationError(
                        f"PSyclone doesn't support an OMPTaskDirective "
                        f"containing an ArrayOfStructuresReference with "
                        f"an array accessing member. Found "
                        f"'{ref.debug_string()}'."
                    )

            # Resolve ArrayReference or ArrayOfStructuresReference
            self._evaluate_readonly_arrayref(
                ref, clause_lists
            )
        elif isinstance(ref, StructureReference):
            # If the StructureReference contains an ArrayMixin then
            # we need to treat it differently, like an arrayref, however
            # the code to handle an arrayref is not compatible with more
            # than one ArrayMixin child.
            array_children = ref.walk((ArrayOfStructuresMember, ArrayMember))
            if array_children:
                if len(array_children) > 1:
                    raise GenerationError(
                        f"PSyclone doesn't support an OMPTaskDirective "
                        f"containing a StructureReference with multiple array"
                        f" accessing members. Found '{ref.debug_string()}'."
                    )
                self._evaluate_structure_with_array_reference_read(
                    ref,
                    array_children[0],
                    clause_lists
                )
            else:
                # We have a StructureReference with no array children, so it
                # should be treated the same as a Reference, except we have to
                # create a Reference to the symbol as according to OpenMP
                # standard only accesses to the base Structure or arrays
                # within a structure are valid in clauses.
                base_ref = Reference(ref.symbol)
                self._evaluate_readonly_baseref(
                    base_ref, clause_lists
                )
        elif isinstance(ref, Reference):
            self._evaluate_readonly_baseref(
                ref, clause_lists
            )

    def _evaluate_structure_with_array_reference_indexlist(
        self,
        sref_base,
        array_access_member,
        clause_lists,
        index_list
    ):
        """
        Evaluates an access to an array with a structure inside the task
        region, and generates the index_list used by the calling function -
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

        :param sref_base: A copy of ref containing the members included
                          in the final reference.
        :type sref_base: :py:class:`psyclone.psyir.nodes.StructureReference`
        :param array_access_member: The ArrayMixin member child of the
                                    node.
        :type array_access_member:
                :py:class:psyclone.psyir.nodes.array_mixin.ArrayMixin`
        :param clause_lists: The namedtuple containing the lists storing the
                             clauses.
        :type clause_lists: namedtuple(
                            private_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            firstprivate_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            shared_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            in_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            out_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ])
        :param index_list: The output References for this task.
        :type index_list: List[:py:class:`psyclone.psyir.nodes.Reference`]

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
                child_loop_vars = self._child_loop_vars

                if index_private:
                    if (
                        index not in clause_lists.private_list
                        and index not in clause_lists.firstprivate_list
                    ):
                        clause_lists.firstprivate_list.append(index.copy())
                    # Special case 1. If index belongs to a child loop
                    # that is NOT a proxy for a parent loop, then we
                    # can only do as well as guessing the entire range
                    # of the loop is used.
                    if index.symbol in child_loop_vars:
                        # Append a full Range (i.e., :)
                        full_range = sref_base.walk(ArrayMember)[0].\
                                get_full_range(dim)
                        index_list.append(full_range)
                    elif index.symbol in self._proxy_loop_vars:
                        # Special case 2. the index is a proxy for a parent
                        # loop's variable. In this case, we add a reference to
                        # the parent loop's value. We create a list of all
                        # possible variants, as we might have multiple values
                        # set for a value, e.g. for a boundary condition if
                        # statement.
                        self._handle_proxy_loop_index(index_list, dim, index,
                                                      clause_lists)
                    else:
                        # Final case is just a generic Reference, in which case
                        # just copy the Reference
                        index_list.append(index.copy())
                else:
                    raise GenerationError(
                        f"Shared variable access used "
                        f"as an array index inside an "
                        f"OMPTaskDirective which is not "
                        f"supported. Variable name is '{index.symbol.name}'. "
                        f"The full access is '{sref_base.debug_string()}'."
                    )
            elif isinstance(index, BinaryOperation):
                # Binary Operation check
                # A single binary operation, e.g. a(i+1) can require
                # multiple clauses to correctly handle.
                self._handle_index_binop(
                    index, index_list, clause_lists
                )
            elif isinstance(index, Literal):
                # Just place literal directly into the dependency clause.
                index_list.append(index.copy())
            else:
                # Not allowed type appears
                raise GenerationError(
                    f"'{type(index).__name__}' object is not allowed to "
                    f"appear in an array index "
                    f"expression inside an "
                    f"OMPTaskDirective."
                )

    def _evaluate_structure_with_array_reference_write(
        self,
        ref,
        array_access_member,
        clause_lists
    ):
        """
        Evaluates a write access to an array within a structure inside the
        task region, and computes any data-sharing clauses and dependency
        clauses based upon the access.

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
        :type array_access_member:
                :py:class:psyclone.psyir.nodes.array_mixin.ArrayMixin`
        :param clause_lists: The namedtuple containing the lists storing the
                             clauses.
        :type clause_lists: namedtuple(
                            private_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            firstprivate_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            shared_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            in_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            out_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ])
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
            sref_base,
            array_access_member,
            clause_lists,
            index_list,
        )

        # Add all combinations of dependencies from the computed index_list
        # into out_list
        self._add_dependencies_from_index_list(
                index_list, clause_lists.out_list, sref_base,
                array_access_member=array_access_member
        )
        # Add to shared_list
        sclause = Reference(ref.symbol)
        if sclause not in clause_lists.shared_list:
            clause_lists.shared_list.append(sclause)

    def _evaluate_write_arrayref(
        self, ref, clause_lists
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
        :param clause_lists: The namedtuple containing the lists storing the
                             clauses.
        :type clause_lists: namedtuple(
                            private_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            firstprivate_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            shared_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            in_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            out_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ])

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
                child_loop_vars = self._child_loop_vars
                if index_private:
                    if (
                        index not in clause_lists.private_list
                        and index not in clause_lists.firstprivate_list
                    ):
                        clause_lists.firstprivate_list.append(index.copy())
                    # Special case 1. If index belongs to a child loop
                    # that is NOT a proxy for a parent loop, then we
                    # can only do as well as guessing the entire range
                    # of the loop is used.
                    if index.symbol in child_loop_vars:
                        # Return a Full Range (i.e. :)
                        full_range = ref.walk(ArrayMixin)[0].get_full_range(
                                dim
                        )
                        index_list.append(full_range)
                    elif index.symbol in self._proxy_loop_vars:
                        # Special case 2. the index is a proxy for a parent
                        # loop's variable. In this case, we add a reference to
                        # the parent loop's value. We create a list of all
                        # possible variants, as we might have multiple values
                        # set for a value, e.g. for a boundary condition if
                        # statement.
                        self._handle_proxy_loop_index(index_list, dim, index,
                                                      clause_lists)
                    else:
                        # Final case is just a generic Reference, in which case
                        # just copy the Reference if its firstprivate.
                        if index in clause_lists.firstprivate_list:
                            index_list.append(index.copy())
                        else:
                            # If its general private, then we don't know what
                            # the value of this is at the time we evaluate the
                            # depend clause, so we can only generate a full
                            # range (:)
                            full_range = ref.walk(ArrayMixin)[0].\
                                    get_full_range(dim)
                            index_list.append(full_range)
                else:
                    raise GenerationError(
                        f"Shared variable access used "
                        f"as an array index inside an "
                        f"OMPTaskDirective which is not "
                        f"supported. Variable name is '{index.symbol.name}'. "
                        f"The full access is '{ref.debug_string()}'."
                    )
            elif isinstance(index, BinaryOperation):
                self._handle_index_binop(
                    index, index_list, clause_lists
                )

        # Add all combinations of dependencies from the computed index_list
        # into out_list
        self._add_dependencies_from_index_list(
                index_list, clause_lists.out_list, ref
        )
        # Add to shared_list
        sclause = Reference(ref.symbol)
        if sclause not in clause_lists.shared_list:
            clause_lists.shared_list.append(sclause)

    def _evaluate_write_baseref(
        self, ref, clause_lists
    ):
        """
        Evaluates a write to a non-ArrayReference reference. If the variable
        is declared private in the parent parallel region, then the variable
        is added to the private clause for this task.

        If the variable is not private (therefore is shared), it is added to
        the shared and output dependence lists for this task region.

        :param ref: The Reference to be evaluated.
        :type ref: :py:class:`psyclone.psyir.nodes.Reference`
        :param clause_lists: The namedtuple containing the lists storing the
                             clauses.
        :type clause_lists: namedtuple(
                            private_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            firstprivate_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            shared_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            in_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            out_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ])
        """
        # Check if its a private variable
        is_private = self._is_reference_private(ref)
        # If its private should add it to private list if not already present
        if is_private and ref not in clause_lists.private_list:
            clause_lists.private_list.append(ref.copy())
        # Otherwise its a shared variable
        if not is_private:
            if ref not in clause_lists.shared_list:
                clause_lists.shared_list.append(ref.copy())
            if ref not in clause_lists.out_list:
                clause_lists.out_list.append(ref.copy())

    def _evaluate_write_reference(
        self, ref, clause_lists
    ):
        """
        Evaluates a write to any Reference in the task region. This is done by
        calling the appropriate subfunction depending on the type of the
        Reference.

        :param ref: The Reference to be evaluated.
        :type ref: :py:class:`psyclone.psyir.nodes.Reference`
        :param clause_lists: The namedtuple containing the lists storing the
                             clauses.
        :type clause_lists: namedtuple(
                            private_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            firstprivate_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            shared_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            in_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            out_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ])

        :raises GenerationError: If a StructureReference containing multiple
                                 ArrayMember or ArrayOfStructuresMember as
                                 children is found.
        :raises GenerationError: If an ArrayOfStructuresReference containing
                                 an ArrayMember of ArrayOfStructuresMember as
                                 a child if found.
        """
        if isinstance(ref, (ArrayReference, ArrayOfStructuresReference)):
            # If ref is an ArrayOfStructuresReference and contains an
            # ArrayMember then we can't handle this case.
            if isinstance(ref, ArrayOfStructuresReference):
                array_children = ref.walk((ArrayOfStructuresMember,
                                           ArrayMember))
                if array_children:
                    raise GenerationError(
                        f"PSyclone doesn't support an OMPTaskDirective "
                        f"containing an ArrayOfStructuresReference with "
                        f"an array accessing member. Found "
                        f"'{ref.debug_string()}'."
                    )

            # Resolve ArrayReference or ArrayOfStructuresReference
            self._evaluate_write_arrayref(
                ref, clause_lists
            )
        elif isinstance(ref, StructureReference):
            # If the StructureReference contains an ArrayMixin then
            # we need to treat it differently, like an arrayref, however
            # the code to handle an arrayref is not compatible with more
            # than one ArrayMixin child
            array_children = ref.walk((ArrayOfStructuresMember, ArrayMember))
            if array_children:
                if len(array_children) > 1:
                    raise GenerationError(
                        f"PSyclone doesn't support an OMPTaskDirective "
                        f"containing a StructureReference with multiple array"
                        f" accessing members. Found '{ref.debug_string()}'."
                    )
                self._evaluate_structure_with_array_reference_write(
                    ref,
                    array_children[0],
                    clause_lists
                )
            else:
                # This is treated the same as a Reference, but we create a
                # Reference to the symbol to handle.
                base_ref = Reference(ref.symbol)
                self._evaluate_write_baseref(
                    base_ref, clause_lists
                )
        elif isinstance(ref, Reference):
            self._evaluate_write_baseref(
                ref, clause_lists
            )
        else:
            raise InternalError(f"PSyclone can't handle an OMPTaskDirective "
                                f"containing an assignment with a LHS that "
                                f"is not a Reference. Found "
                                f"'{ref.debug_string()}'.")

    def _evaluate_assignment(
        self,
        node,
        clause_lists
    ):
        """
        Evaluates an Assignment node within this task region. This is done
        by calling the appropriate subfunction on each reference on the
        LHS and RHS of the Assignment.

        :param ref: The Assignment to be evaluated.
        :type ref: :py:class:`psyclone.psyir.nodes.Assignment`
        :param clause_lists: The namedtuple containing the lists storing the
                             clauses.
        :type clause_lists: namedtuple(
                            private_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            firstprivate_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            shared_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            in_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            out_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ])
        """
        lhs = node.children[0]
        rhs = node.children[1]
        # Evaluate LHS
        self._evaluate_write_reference(
            lhs, clause_lists
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
                if ref.symbol != parent_var:
                    continue
                if lhs.symbol in self._proxy_loop_vars:
                    if (
                        rhs
                        not in self._proxy_loop_vars[lhs.symbol].parent_node
                    ):
                        self._proxy_loop_vars[lhs.symbol].parent_node.append(
                                rhs.copy()
                        )
                else:
                    subdict = self._proxy_vars(
                            parent_var, [rhs.copy()], node,
                            self._parent_loops[index]
                    )

                    self._proxy_loop_vars[lhs.symbol] = subdict
                added = True
            # If we find any proxy loop variable on the RHS then we stop.
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
                            not in self._proxy_loop_vars[lhs.symbol].
                            parent_node
                        ):
                            self._proxy_loop_vars[lhs.symbol].parent_node.\
                                    append(rhs.copy())
                    else:
                        subdict = self._proxy_vars(
                                self._proxy_loop_vars[proxy_var].parent_var,
                                [rhs.copy()], node, self._parent_loops[index]
                        )
                        self._parent_loops.append(self._parent_loops[index])
                        self._proxy_loop_vars[lhs.symbol] = subdict
                    added = True
            # If we find any proxy loop variable on the RHS then we stop.
            if added:
                break

        # Use the intrinsic walk function to evaluate any references inside
        # Intrinsics
        for intrinsic in node.walk(IntrinsicCall):
            self._evaluate_intrinsic(intrinsic, clause_lists)

        for ref in references:
            # If the ref has an IntrinsicCall ancestor, it will already
            # have been evaluated
            if ref.ancestor(IntrinsicCall):
                continue
            self._evaluate_readonly_reference(
                ref, clause_lists
            )

    def _evaluate_loop(
        self,
        node,
        clause_lists
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
        :param clause_lists: The namedtuple containing the lists storing the
                             clauses.
        :type clause_lists: namedtuple(
                            private_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            firstprivate_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            shared_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            in_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            out_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ])

        :raises GenerationError: If the loop variable is a shared variable.
        :raises GenerationError: If the loop start, stop or step expression
                                 contains an ArrayReference.
        :raises GenerationError: If the loop step expression contains an
                                 IntrinsicCall.
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
                    subdict = self._proxy_vars(
                            parent_var, [Reference(parent_var)], node,
                            self._parent_loops[index]
                    )
                    self._proxy_loop_vars[to_remove] = subdict
                    break

        remove_child_var = None
        if to_remove is None:
            # If this loop is not a proxy_loop, then it is a child_loop
            remove_child_var = loop_var
            self._child_loop_vars.append(loop_var)

        # Loop variable is private unless already set as firstprivate.
        # Throw exception if shared
        loop_var_ref = Reference(loop_var)
        if loop_var not in self._parallel_private:
            raise GenerationError(
                "Found shared loop variable which is "
                "not allowed in OpenMP Task directive. "
                f"Variable name is {loop_var_ref.name}"
            )
        if loop_var_ref not in clause_lists.firstprivate_list:
            if loop_var_ref not in clause_lists.private_list:
                clause_lists.private_list.append(loop_var_ref)

        # If we have a proxy variable, the parent loop variable has to be
        # firstprivate
        if to_remove is not None:
            parent_var_ref = Reference(
                self._proxy_loop_vars[to_remove].parent_var
            )
            if parent_var_ref not in clause_lists.firstprivate_list:
                clause_lists.firstprivate_list.append(parent_var_ref.copy())

        # For all non-array accesses we make them firstprivate unless they
        # are already declared as something else
        for ref in start_val_refs:
            if isinstance(ref, (ArrayReference, ArrayOfStructuresReference)):
                raise GenerationError(
                    f"'{type(ref).__name__}' not supported in "
                    f"the start variable of a Loop in a "
                    f"OMPTaskDirective node. The start expression is "
                    f"'{node.start_expr.debug_string()}'."
                )
            # Ignore references inside inquiry IntrinsicCalls (e.g. BOUNDs)
            icall = ref.ancestor(IntrinsicCall)
            if icall and icall.intrinsic.is_inquiry:
                continue
            # If we have a StructureReference, then we need to only add the
            # base symbol to the lists
            if isinstance(ref, StructureReference):
                ref_copy = Reference(ref.symbol)
                # start_val can't be written to in Fortran so if its a
                # structure we should make it shared
                # Only the base Structure is allowed to be in a depend clause
                # in OpenMP, see OpenMP section 2.1
                if ref_copy not in clause_lists.shared_list:
                    clause_lists.shared_list.append(ref_copy.copy())
                if ref_copy not in clause_lists.in_list:
                    clause_lists.in_list.append(ref_copy.copy())
                ref = ref_copy
            if (
                ref not in clause_lists.firstprivate_list
                and ref not in clause_lists.private_list
                and ref not in clause_lists.shared_list
            ):
                clause_lists.firstprivate_list.append(ref.copy())

        stop_val_refs = stop_val.walk(Reference)
        for ref in stop_val_refs:
            if isinstance(ref, (ArrayReference, ArrayOfStructuresReference)):
                raise GenerationError(
                    f"'{type(ref).__name__}' not supported in "
                    f"the stop variable of a Loop in a "
                    f"OMPTaskDirective node. The stop expression is "
                    f"'{node.stop_expr.debug_string()}'."
                )
            # Ignore references inside inquiry IntrinsicCalls (e.g. BOUNDs)
            icall = ref.ancestor(IntrinsicCall)
            if icall and icall.intrinsic.is_inquiry:
                continue
            # If we have a StructureReference, then we need to only add the
            # base symbol to the lists
            if isinstance(ref, StructureReference):
                ref_copy = Reference(ref.symbol)
                # stop_val can't be written to in Fortran so if its a structure
                # we should make it shared
                # Only the base Structure is allowed to be in a depend clause.
                if ref_copy not in clause_lists.shared_list:
                    clause_lists.shared_list.append(ref_copy.copy())
                if ref_copy not in clause_lists.in_list:
                    clause_lists.in_list.append(ref_copy.copy())
                ref = ref_copy
            if (
                ref not in clause_lists.firstprivate_list
                and ref not in clause_lists.private_list
                and ref not in clause_lists.shared_list
            ):
                clause_lists.firstprivate_list.append(ref.copy())

        step_val_refs = step_val.walk(Reference)
        for ref in step_val_refs:
            if isinstance(ref, (ArrayReference, ArrayOfStructuresReference)):
                raise GenerationError(
                    f"'{type(ref).__name__}' not supported in "
                    f"the step variable of a Loop in a "
                    f"OMPTaskDirective node. The step expression is "
                    f"'{node.step_expr.debug_string()}'."
                )
            # We disallow intrinsic calls inside the step value, this is
            # beyond the scope of the current implementation
            if ref.ancestor(IntrinsicCall):
                raise GenerationError(
                    f"IntrinsicCall not supported in the step variable "
                    f"of a Loop in an OMPTaskDirective node. The step "
                    f"expression is '{node.step_expr.debug_string()}'."
                )
            # If we have a StructureReference, then we need to only add the
            # base symbol to the lists
            if isinstance(ref, StructureReference):
                ref_copy = Reference(ref.symbol)
                # stop_val can't be written to in Fortran so if its a structure
                # we should make it shared
                # Only the base Structure is allowed to be in a depend clause.
                if ref_copy not in clause_lists.shared_list:
                    clause_lists.shared_list.append(ref_copy.copy())
                if ref_copy not in clause_lists.in_list:
                    clause_lists.in_list.append(ref_copy.copy())
                ref = ref_copy
            if (
                ref not in clause_lists.firstprivate_list
                and ref not in clause_lists.private_list
                and ref not in clause_lists.shared_list
            ):
                clause_lists.firstprivate_list.append(ref.copy())

        # Finished handling the loop bounds now

        # Recurse to the children
        for child_node in node.children[3].children:
            self._evaluate_node(
                child_node,
                clause_lists
            )

        # Remove any stuff added to proxy_loop_vars if needed
        if to_remove is not None:
            self._proxy_loop_vars.pop(to_remove)
        # Remove from child_loop_vars if needed
        if remove_child_var is not None:
            self._child_loop_vars.remove(remove_child_var)

    def _evaluate_ifblock(
        self,
        node,
        clause_lists
    ):
        """
        Evaluates an ifblock inside a task region. This is done by calling
        _evaluate_readonly_reference on each Reference inside the if condition,
        and by calling _evaluate_node on each Node inside the if_body and
        else_body.

        :param node: The IfBlock to be evaluated.
        :type node: :py:class:`psyclone.psyir.nodes.IfBlock`
        :param clause_lists: The namedtuple containing the lists storing the
                             clauses.
        :type clause_lists: namedtuple(
                            private_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            firstprivate_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            shared_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            in_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            out_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ])
        """
        for ref in node.condition.walk(Reference):
            self._evaluate_readonly_reference(
                ref, clause_lists
            )

        # Recurse to the children
        # If block
        for child_node in node.if_body.children:
            self._evaluate_node(
                child_node,
                clause_lists
            )
        # Else block if present
        if node.else_body is not None:
            for child_node in node.else_body.children:
                self._evaluate_node(
                    child_node,
                    clause_lists
                )

    def _evaluate_intrinsic(
        self,
        node,
        clause_lists
    ):
        """
        Evaluates an IntrinsicCall node inside the task region.
        The function immediately stops if the intrinsic has
        `is_inquiry` set to True, as no data dependency occurs
        on these intrinsic.
        All other allowed intrinsics are read-only, so the relevant
        helper functions are called on each argument.

        :param node: The IntrinsicCall to be evaluated.
        :type node: :py:class:`psyclone.psyir.nodes.IntrinsicCall`
        :param clause_lists: The namedtuple containing the lists storing the
                             clauses.
        :type clause_lists: namedtuple(
                            private_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            firstprivate_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            shared_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            in_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            out_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ])
        """
        # If the intrinsic is an inquiry intrinsic we don't need to
        # do anything.
        if node.intrinsic.is_inquiry:
            return

        # Otherwise, we loop through the references in the IntrinsicCall
        # and compute any new dependencies.
        for ref in node.walk(Reference):
            self._evaluate_readonly_reference(
                ref, clause_lists
            )

    def _evaluate_node(
        self,
        node,
        clause_lists
    ):
        """
        Evaluates a generic Node inside the task region. Calls the appropriate
        handler depending on whether the node is an Assignment, Loop or
        IfBlock.

        :param node: The Node to be evaluated.
        :type node: :py:class:`psyclone.psyir.nodes.Node`
        :param clause_lists: The namedtuple containing the lists storing the
                             clauses.
        :type clause_lists: namedtuple(
                            private_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            firstprivate_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            shared_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            in_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ],
                            out_list=List[
                                :py:class:`psyclone.psyir.nodes.Reference`
                            ])
        """
        # For the node, check if it is Loop, Assignment or IfBlock
        if isinstance(node, Assignment):
            # Resolve assignment
            self._evaluate_assignment(
                node,
                clause_lists
            )
        elif isinstance(node, Loop):
            # Resolve loop
            self._evaluate_loop(
                node,
                clause_lists
            )
        elif isinstance(node, IfBlock):
            # Resolve IfBlock
            self._evaluate_ifblock(
                node,
                clause_lists
            )

        # All other node types are ignored as they shouldn't affect
        # dependency computation, as these are the only nodes that
        # have read or write accesses that can get to this function
        # as Calls are prohibited in validation, and the allowed
        # IntrinsicCalls will all be children of other nodes.

    def _compute_clauses(self):
        """
        Computes the clauses for this OMPTaskDirective.

        The OMPTaskDirective must have exactly 1 child, which must be a Loop.
        Upon confirming this, the function calls _evaluate_node to compute all
        data-sharing attributes and dependencies.
        The clauses are then built up from those, and returned.

        :returns: The clauses computed for this OMPTaskDirective.
        :rtype: List[:py:class:`psyclone.psyir.nodes.OMPPrivateClause`,
                     :py:class:`psyclone.psyir.nodes.OMPFirstprivateClause`,
                     :py:class:`psyclone.psyir.nodes.OMPSharedClause`,
                     :py:class:`psyclone.psyir.nodes.OMPDependClause`,
                     :py:class:`psyclone.psyir.nodes.OMPDependClause`]

        :raises GenerationError: If the OMPTaskDirective has multiple children.
        :raises GenerationError: If the OMPTaskDirective's child is not a Loop.

        """

        # These lists will store PSyclone nodes which are to be added to the
        # clauses for this OMPTaskDirective.
        private_list = []
        firstprivate_list = []
        shared_list = []
        in_list = []
        out_list = []
        clause_lists = self._clause_lists(private_list, firstprivate_list,
                                          shared_list, in_list, out_list)

        # Reset this in case we already computed clauses before but are
        # recomputing them (usually due to a code change or multiple outputs).
        self._proxy_loop_vars = {}
        self._child_loop_vars = []

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
                f"'{type(self.children[0].children[0])}'"
            )
        self._evaluate_node(
            self.children[0].children[0],
            clause_lists
        )

        # Make the clauses to return.
        # We skip references to constants as we don't need them.
        # Constants will never be private.
        private_clause = OMPPrivateClause()
        firstprivate_clause = OMPFirstprivateClause()
        for ref in private_list:
            # If the symbol is in the parallel_firstprivate set then
            # we trust the parent parallel region and make it firstprivate
            if ref.symbol in self._parallel_firstprivate:
                firstprivate_list.append(ref)
            else:
                private_clause.addchild(ref)
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
        if self.walk(Kern):
            raise GenerationError(
                "Attempted to lower to OMPTaskDirective "
                "node, but the node contains a Kern "
                "which must be inlined first."
            )
        # We allow a subset of IntrinsicCall nodes
        for child in self.walk(Call):
            if not isinstance(child, IntrinsicCall):
                raise GenerationError(
                    "Attempted to lower to OMPTaskDirective "
                    "node, but the node contains a Call "
                    "which must be inlined first."
                )
            # Otherwise we have an IntrinsicCall
            if child.intrinsic not in self._allowed_intrinsics:
                raise GenerationError(
                    f"Attempted to lower to OMPTaskDirective "
                    f"node, but the node contains a "
                    f"'{child.debug_string()}' intrinsic call, which "
                    f"is not supported."
                )

        # Create the clauses
        (
            private_clause,
            firstprivate_clause,
            shared_clause,
            in_clause,
            out_clause,
        ) = self._compute_clauses()

        # Replace the children with the new children
        old_children = self.pop_all_children()
        self.addchild(old_children[0])
        self.addchild(private_clause)
        self.addchild(firstprivate_clause)
        self.addchild(shared_clause)
        self.addchild(in_clause)
        self.addchild(out_clause)
        super().lower_to_language_level()

        # Replace this node with an OMPTaskDirective
        childs = self.dir_body.pop_all_children()
        clauses = self.clauses[:]
        self.pop_all_children()
        replacement = OMPTaskDirective(children=childs, clauses=clauses)
        self.replace_with(replacement)
