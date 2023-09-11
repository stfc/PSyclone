# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2023, Science and Technology Facilities Council.
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
#         A. B. G. Chalk, STFC Daresbury Lab
#         I. Kavcic,    Met Office
#         C.M. Maynard, Met Office / University of Reading
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' This module contains the implementation of the various OpenMP Directive
nodes.'''


import abc

from psyclone.configuration import Config
from psyclone.core import AccessType, VariablesAccessInfo
from psyclone.errors import GenerationError
from psyclone.f2pygen import (AssignGen, UseGen, DeclGen, DirectiveGen,
                              CommentGen)
from psyclone.psyir.nodes.directive import StandaloneDirective, \
    RegionDirective
from psyclone.psyir.nodes.loop import Loop
from psyclone.psyir.nodes.if_block import IfBlock
from psyclone.psyir.nodes.while_loop import WhileLoop
from psyclone.psyir.nodes.literal import Literal
from psyclone.psyir.nodes.omp_clauses import OMPGrainsizeClause, \
    OMPNowaitClause, OMPNogroupClause, OMPNumTasksClause, OMPPrivateClause, \
    OMPDefaultClause, OMPReductionClause, OMPScheduleClause, \
    OMPFirstprivateClause
from psyclone.psyir.nodes.reference import Reference
from psyclone.psyir.nodes.routine import Routine
from psyclone.psyir.nodes.schedule import Schedule
from psyclone.psyir.symbols import INTEGER_TYPE

# OMP_OPERATOR_MAPPING is used to determine the operator to use in the
# reduction clause of an OpenMP directive.
OMP_OPERATOR_MAPPING = {AccessType.SUM: "+"}


class OMPDirective(metaclass=abc.ABCMeta):
    '''
    Base mixin class for all OpenMP-related directives.

    This class is useful to provide a unique common ancestor to all the
    OpenMP directives, for instance when traversing the tree with
    `node.walk(OMPDirective)`

    Note that classes inheriting from it must place the OMPDirective in
    front of the other Directive node sub-class, so that the Python
    MRO gives preference to this class's attributes.
    '''
    _PREFIX = "OMP"


class OMPRegionDirective(OMPDirective, RegionDirective, metaclass=abc.ABCMeta):
    '''
    Base class for all OpenMP region-related directives.

    '''
    def _get_reductions_list(self, reduction_type):
        '''
        Returns the names of all scalars within this region that require a
        reduction of type 'reduction_type'. Returned names will be unique.

        :param reduction_type: the reduction type (e.g. AccessType.SUM) to \
                               search for.
        :type reduction_type: :py:class:`psyclone.core.access_type.AccessType`

        :returns: names of scalar arguments with reduction access.
        :rtype: list of str

        '''
        result = []
        const = Config.get().api_conf().get_constants()
        for call in self.kernels():
            if not call.arguments:
                continue
            for arg in call.arguments.args:
                if arg.argument_type in const.VALID_SCALAR_NAMES:
                    if arg.descriptor.access == reduction_type:
                        if arg.name not in result:
                            result.append(arg.name)
        return result


class OMPStandaloneDirective(OMPDirective, StandaloneDirective,
                             metaclass=abc.ABCMeta):
    ''' Base class for all OpenMP-related standalone directives. '''


class OMPDeclareTargetDirective(OMPStandaloneDirective):
    '''
    Class representing an OpenMP Declare Target directive in the PSyIR.

    '''
    def gen_code(self, parent):
        '''Generate the fortran OMP Declare Target Directive and any
        associated code.

        :param parent: the parent Node in the Schedule to which to add our \
                       content.
        :type parent: sub-class of :py:class:`psyclone.f2pygen.BaseGen`
        '''
        # Check the constraints are correct
        self.validate_global_constraints()

        # Generate the code for this Directive
        parent.add(DirectiveGen(parent, "omp", "begin", "declare", "target"))

    def begin_string(self):
        '''Returns the beginning statement of this directive, i.e.
        "omp routine". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the opening statement of this directive.
        :rtype: str

        '''
        return "omp declare target"

    def validate_global_constraints(self):
        '''
        Perform validation checks that can only be done at code-generation
        time.

        :raises GenerationError: if this directive is not the first statement \
            in a routine.

        '''
        if self.parent and (not isinstance(self.parent, Routine) or
                            self.parent.children[0] is not self):
            raise GenerationError(
                f"A OMPDeclareTargetDirective must be the first child (index "
                f"0) of a Routine but found one as child {self.position} of a "
                f"{type(self.parent).__name__}.")

        super().validate_global_constraints()


class OMPTaskwaitDirective(OMPStandaloneDirective):
    '''
    Class representing an OpenMP TASKWAIT directive in the PSyIR.

    '''
    def validate_global_constraints(self):
        '''
        Perform validation checks that can only be done at code-generation
        time.

        :raises GenerationError: if this OMPTaskwait is not enclosed \
                            within some OpenMP parallel region.

        '''
        # It is only at the point of code generation that we can check for
        # correctness (given that we don't mandate the order that a user
        # can apply transformations to the code). As a Parallel Child
        # directive, we must have an OMPParallelDirective as an ancestor
        # somewhere back up the tree.
        if not self.ancestor(OMPParallelDirective,
                             excluding=OMPParallelDoDirective):
            raise GenerationError(
                "OMPTaskwaitDirective must be inside an OMP parallel region "
                "but could not find an ancestor OMPParallelDirective node")

        super().validate_global_constraints()

    def gen_code(self, parent):
        '''Generate the fortran OMP Taskwait Directive and any associated
        code

        :param parent: the parent Node in the Schedule to which to add our \
                       content.
        :type parent: sub-class of :py:class:`psyclone.f2pygen.BaseGen`
        '''
        # Check the constraints are correct
        self.validate_global_constraints()

        # Generate the code for this Directive
        parent.add(DirectiveGen(parent, "omp", "begin", "taskwait", ""))
        # No children or end code for this node

    def begin_string(self):
        '''Returns the beginning statement of this directive, i.e.
        "omp taskwait". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the opening statement of this directive.
        :rtype: str

        '''
        return "omp taskwait"


class OMPSerialDirective(OMPRegionDirective, metaclass=abc.ABCMeta):
    '''
    Abstract class representing OpenMP serial regions, e.g.
    OpenMP SINGLE or OpenMP Master.

    '''

    def validate_global_constraints(self):
        '''
        Perform validation checks that can only be done at code-generation
        time.

        :raises GenerationError: if this OMPSerial is not enclosed \
                                 within some OpenMP parallel region.
        :raises GenerationError: if this OMPSerial is enclosed within \
                                 any OMPSerialDirective subclass region.

        '''
        # It is only at the point of code generation that we can check for
        # correctness (given that we don't mandate the order that a user
        # can apply transformations to the code). As a Parallel Child
        # directive, we must have an OMPParallelDirective as an ancestor
        # somewhere back up the tree.
        # Also check the single region is not enclosed within another OpenMP
        # single region.
        # It could in principle be allowed for that parent to be a ParallelDo
        # directive, however I can't think of a use case that would be done
        # best in a parallel code by that pattern
        if not self.ancestor(OMPParallelDirective,
                             excluding=OMPParallelDoDirective):
            raise GenerationError(
                f"{self._text_name} must be inside an OMP parallel region but "
                f"could not find an ancestor OMPParallelDirective node")

        if self.ancestor(OMPSerialDirective):
            raise GenerationError(
                f"{self._text_name} must not be inside another OpenMP "
                f"serial region")

        super().validate_global_constraints()


class OMPSingleDirective(OMPSerialDirective):
    '''
    Class representing an OpenMP SINGLE directive in the PSyIR.

    :param bool nowait: Argument describing whether this single should have \
        a nowait clause applied. Default value is False.
    :param kwargs: additional keyword arguments provided to the PSyIR node.
    :type kwargs: unwrapped dict.

    '''
    _children_valid_format = "Schedule, [OMPNowaitClause]"
    # Textual description of the node
    _text_name = "OMPSingleDirective"

    def __init__(self, nowait=False, **kwargs):

        self._nowait = nowait
        # Call the init method of the base class once we've stored
        # the nowait requirement
        super().__init__(**kwargs)
        if self._nowait:
            self.children.append(OMPNowaitClause())

    @staticmethod
    def _validate_child(position, child):
        '''
         Decides whether a given child and position are valid for this node.
         The rules are:
         1. Child 0 must always be a Schedule.
         2. Child 1 can only be a OMPNowaitClause.

        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        if position == 0:
            return isinstance(child, Schedule)
        if position == 1:
            return isinstance(child, OMPNowaitClause)
        return False

    @property
    def nowait(self):
        '''
        :returns: whether the nowait clause is specified for this directive.
        :rtype: bool

        '''
        return self._nowait

    def gen_code(self, parent):
        '''Generate the fortran OMP Single Directive and any associated
        code

        :param parent: the parent Node in the Schedule to which to add our \
                       content.
        :type parent: sub-class of :py:class:`psyclone.f2pygen.BaseGen`
        '''
        # Check the constraints are correct
        self.validate_global_constraints()

        # Capture the nowait section of the string if required
        nowait_string = ""
        if self._nowait:
            nowait_string = "nowait"

        parent.add(DirectiveGen(parent, "omp", "begin", "single",
                                nowait_string))

        # Generate the code for all of this node's children
        for child in self.dir_body:
            child.gen_code(parent)

        # Generate the end code for this node
        parent.add(DirectiveGen(parent, "omp", "end", "single", ""))

    def begin_string(self):
        '''Returns the beginning statement of this directive, i.e.
        "omp single". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the opening statement of this directive.
        :rtype: str

        '''
        return "omp single"

    def end_string(self):
        '''Returns the end (or closing) statement of this directive, i.e.
        "omp end single". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the end statement for this directive.
        :rtype: str

        '''
        return "omp end single"


class OMPMasterDirective(OMPSerialDirective):
    '''
    Class representing an OpenMP MASTER directive in the PSyclone AST.

    '''

    # Textual description of the node
    _text_name = "OMPMasterDirective"

    def gen_code(self, parent):
        '''Generate the Fortran OMP Master Directive and any associated
        code

        :param parent: the parent Node in the Schedule to which to add our \
                       content.
        :type parent: sub-class of :py:class:`psyclone.f2pygen.BaseGen`
        '''

        # Check the constraints are correct
        self.validate_global_constraints()

        parent.add(DirectiveGen(parent, "omp", "begin", "master", ""))

        # Generate the code for all of this node's children
        for child in self.children:
            child.gen_code(parent)

        # Generate the end code for this node
        parent.add(DirectiveGen(parent, "omp", "end", "master", ""))

    def begin_string(self):
        '''Returns the beginning statement of this directive, i.e.
        "omp master". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the opening statement of this directive.
        :rtype: str

        '''
        return "omp master"

    def end_string(self):
        '''Returns the end (or closing) statement of this directive, i.e.
        "omp end master". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the end statement for this directive.
        :rtype: str

        '''
        return "omp end master"


class OMPParallelDirective(OMPRegionDirective):
    ''' Class representing an OpenMP Parallel directive.
    '''

    _children_valid_format = ("Schedule, OMPDefaultClause, OMPPrivateClause, "
                              "OMPFirstprivate, [OMPReductionClause]*")

    @staticmethod
    def create(children=None):
        '''
        Create an OMPParallelDirective.

        :param children: The child nodes of the new directive.
        :type children: List of :py:class:`psyclone.psyir.nodes.Node`

        :returns: A new OMPParallelDirective.
        :rtype: :py:class:`psyclone.psyir.nodes.OMPParallelDirective`
        '''

        instance = OMPParallelDirective(children=children)

        # An OMPParallelDirective must have 4 children.
        # Child 0 is a Schedule, created in the constructor.
        # The create function adds the other three mandatory children:
        # OMPDefaultClause, OMPPrivateClause and OMPFirstprivateClause
        instance.addchild(OMPDefaultClause(clause_type=OMPDefaultClause.
                                           DefaultClauseTypes.SHARED))
        instance.addchild(OMPPrivateClause())
        instance.addchild(OMPFirstprivateClause())

        return instance

    @staticmethod
    def _validate_child(position, child):
        '''
        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        if position == 0 and isinstance(child, Schedule):
            return True
        if position == 1 and isinstance(child, OMPDefaultClause):
            return True
        if position == 2 and isinstance(child, OMPPrivateClause):
            return True
        if position == 3 and isinstance(child, OMPFirstprivateClause):
            return True
        if position >= 4 and isinstance(child, OMPReductionClause):
            return True
        return False

    @property
    def default_clause(self):
        '''
        :returns: The OMPDefaultClause associated with this Directive.
        :rtype: :py:class:`psyclone.psyir.nodes.OMPDefaultClause`
        '''
        return self.children[1]

    @property
    def private_clause(self):
        '''
        :returns: The current OMPPrivateClause associated with this Directive.
        :rtype: :py:class:`psyclone.psyir.nodes.OMPPrivateClause`
        '''
        return self.children[2]

    def gen_code(self, parent):
        '''Generate the fortran OMP Parallel Directive and any associated
        code.

        :param parent: the node in the generated AST to which to add content.
        :type parent: :py:class:`psyclone.f2pygen.BaseGen`

        :raises GenerationError: if the OpenMP directive needs some
            synchronisation mechanism to create valid code. These are not
            implemented yet.

        '''
        # pylint: disable=import-outside-toplevel
        from psyclone.psyGen import zero_reduction_variables

        # We're not doing nested parallelism so make sure that this
        # omp parallel region is not already within some parallel region
        self.validate_global_constraints()

        # Check that this OpenMP PARALLEL directive encloses other
        # OpenMP directives. Although it is valid OpenMP if it doesn't,
        # this almost certainly indicates a user error.
        self._encloses_omp_directive()

        # Generate the private and firstprivate clauses
        private, fprivate, need_sync = self._infer_sharing_attributes()
        private_clause = OMPPrivateClause.create(
                            sorted(private, key=lambda x: x.name))
        fprivate_clause = OMPFirstprivateClause.create(
                            sorted(fprivate, key=lambda x: x.name))
        if need_sync:
            raise GenerationError(
                f"OMPParallelDirective.gen_code() does not support symbols "
                f"that need synchronisation, but found: "
                f"{[x.name for x in need_sync]}")

        reprod_red_call_list = self.reductions(reprod=True)
        if reprod_red_call_list:
            # we will use a private thread index variable
            thread_idx = self.scope.symbol_table.\
                lookup_with_tag("omp_thread_index")
            private_clause.addchild(Reference(thread_idx))
            thread_idx = thread_idx.name
            # declare the variable
            parent.add(DeclGen(parent, datatype="integer",
                               entity_decls=[thread_idx]))

        calls = self.reductions()

        # first check whether we have more than one reduction with the same
        # name in this Schedule. If so, raise an error as this is not
        # supported for a parallel region.
        names = []
        for call in calls:
            name = call.reduction_arg.name
            if name in names:
                raise GenerationError(
                    f"Reduction variables can only be used once in an invoke. "
                    f"'{name}' is used multiple times, please use a different "
                    f"reduction variable")
            names.append(name)

        zero_reduction_variables(calls, parent)

        # pylint: disable=protected-access
        clauses_str = self.default_clause._clause_string
        # pylint: enable=protected-access

        private_list = [child.symbol.name for child in private_clause.children]
        if private_list:
            clauses_str += ", private(" + ",".join(private_list) + ")"
        fp_list = [child.symbol.name for child in fprivate_clause.children]
        if fp_list:
            clauses_str += ", firstprivate(" + ",".join(fp_list) + ")"
        parent.add(DirectiveGen(parent, "omp", "begin", "parallel",
                                f"{clauses_str}"))

        if reprod_red_call_list:
            # add in a local thread index
            parent.add(UseGen(parent, name="omp_lib", only=True,
                              funcnames=["omp_get_thread_num"]))
            parent.add(AssignGen(parent, lhs=thread_idx,
                                 rhs="omp_get_thread_num()+1"))

        first_type = type(self.dir_body[0])
        for child in self.dir_body.children:
            if first_type != type(child):
                raise NotImplementedError("Cannot correctly generate code"
                                          " for an OpenMP parallel region"
                                          " containing children of "
                                          "different types")
            child.gen_code(parent)

        parent.add(DirectiveGen(parent, "omp", "end", "parallel", ""))

        if reprod_red_call_list:
            parent.add(CommentGen(parent, ""))
            parent.add(CommentGen(parent, " sum the partial results "
                                  "sequentially"))
            parent.add(CommentGen(parent, ""))
            for call in reprod_red_call_list:
                call.reduction_sum_loop(parent)

        self.gen_post_region_code(parent)

    def lower_to_language_level(self):
        '''
        In-place construction of clauses as PSyIR constructs.
        At the higher level these clauses rely on dynamic variable dependence
        logic to decide what is private and what is shared, so we use this
        lowering step to find out which References are private, and place them
        explicitly in the lower-level tree to be processed by the backend
        visitor.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        :raises GenerationError: if the OpenMP directive needs some
            synchronisation mechanism to create valid code. These are not
            implemented yet.
        '''
        # Keep the first two children and compute the rest using the current
        # state of the node/tree (lowering it first in case new symbols are
        # created)
        self._children = self._children[:2]
        for child in self.children:
            child.lower_to_language_level()

        # Create data sharing clauses (order alphabetically to make generation
        # reproducible)
        private, fprivate, need_sync = self._infer_sharing_attributes()
        private_clause = OMPPrivateClause.create(
                            sorted(private, key=lambda x: x.name))
        fprivate_clause = OMPFirstprivateClause.create(
                            sorted(fprivate, key=lambda x: x.name))
        if need_sync:
            raise GenerationError(
                f"Lowering {type(self).__name__} does not support symbols that"
                f" need synchronisation, but found: "
                f"{[x.name for x in need_sync]}")

        self.addchild(private_clause)
        self.addchild(fprivate_clause)
        return self

    def begin_string(self):
        '''Returns the beginning statement of this directive, i.e.
        "omp parallel". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the opening statement of this directive.
        :rtype: str

        '''
        result = "omp parallel"
        # TODO #514: not yet working with NEMO, so commented out for now
        # if not self._reprod:
        #     result += self._reduction_string()

        return result

    def end_string(self):
        '''Returns the end (or closing) statement of this directive, i.e.
        "omp end parallel". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the end statement for this directive.
        :rtype: str

        '''
        return "omp end parallel"

    def _infer_sharing_attributes(self):
        '''
        The PSyIR does not specify if each symbol inside an OpenMP region is
        private, firstprivate, shared or shared but needs synchronisation,
        the attributes are infered looking at the usage of each symbol inside
        the parallel region.

        This method analyses the directive body and automatically classifies
        each symbol using the following rules:
        - All arrays are shared.
        - Scalars that are accessed only once are shared.
        - Scalars that are read-only or written outside a loop are shared.
        - Scalars written in multiple iterations of a loop are private, unless:
          * there is a write-after-read dependency in a loop iteration,
          in this case they are shared but need synchronisation;
          * they are read before in the same parallel region (but not inside
          the same loop iteration), in this case they are firstprivate.
          * they are only conditionally written in some iterations;
          in this case they are firstprivate.

        This method returns the sets of private, firstprivate, and shared but
        needing synchronisation symbols, all symbols not in these sets are
        assumed shared. How to synchronise the symbols in the third set is
        up to the caller of this method.

        :returns: three set of symbols that classify each of the symbols in \
            the directive body as PRIVATE, FIRSTPRIVATE or SHARED NEEDING \
            SYNCHRONISATION.
        :rtype: Tuple[Set(:py:class:`psyclone.psyir.symbols.symbol), \
                      Set(:py:class:`psyclone.psyir.symbols.symbol), \
                      Set(:py:class:`psyclone.psyir.symbols.symbol)]

        :raises GenerationError: if the DefaultClauseType associated with \
                                 this OMPParallelDirective is not shared.

        '''
        if (self.default_clause.clause_type !=
                OMPDefaultClause.DefaultClauseTypes.SHARED):
            raise GenerationError("OMPParallelClause cannot correctly generate"
                                  " the private clause when its default "
                                  "data sharing attribute in its default "
                                  "clause is not 'shared'.")

        # TODO #598: Improve the handling of scalar variables, there are
        # remaining issues when we have accesses after the parallel region
        # of variables that we currently declare as private. This should be
        # lastprivate.
        # e.g:
        # !$omp parallel do <- will set private(ji, my_index)
        # do ji = 1, jpk
        #   my_index = ji+1
        #   array(my_index) = 2
        # enddo
        # #end do
        # call func(my_index) <- my_index has not been updated

        private = set()
        fprivate = set()
        need_sync = set()

        # Determine variables that must be private, firstprivate or need_sync
        var_accesses = VariablesAccessInfo()
        self.reference_accesses(var_accesses)
        for signature in var_accesses.all_signatures:
            accesses = var_accesses[signature].all_accesses
            # Ignore variables that have indices, we only look at scalars
            if accesses[0].is_array():
                continue

            # If a variable is only accessed once, it is either an error
            # or a shared variable - anyway it is not private
            if len(accesses) == 1:
                continue

            # TODO #598: If we only have writes, it must be need_sync:
            # do ji = 1, jpk
            #   if ji=3:
            #      found = .true.
            # Or lastprivate in order to maintain the serial semantics
            # do ji = 1, jpk
            #   found = ji

            # We consider private variables as being the ones that are written
            # in every iteration of a loop.
            # If one such scalar is potentially read before it is written, it
            # will be considered firstprivate.
            has_been_read = False
            last_read_position = 0
            for access in accesses:
                if access.access_type == AccessType.READ:
                    has_been_read = True
                    last_read_position = access.node.abs_position

                if access.access_type == AccessType.WRITE:

                    # Check if the write access is outside a loop. In this case
                    # it will be marked as shared. This is done because it is
                    # likely to be re-used later. e.g:
                    # !$omp parallel
                    # jpk = 100
                    # !omp do
                    # do ji = 1, jpk
                    loop_ancestor = access.node.ancestor(
                        (Loop, WhileLoop),
                        limit=self,
                        include_self=True)
                    if not loop_ancestor:
                        # If we find it at least once outside a loop we keep it
                        # as shared
                        break

                    # Otherwise, the assignment to this variable is inside a
                    # loop (and it will be repeated for each iteration), so
                    # we declare it as private or need_synch
                    name = signature.var_name
                    # TODO #2094: var_name only captures the top-level
                    # component in the derived type accessor. If the attributes
                    # only apply to a sub-component, this won't be captured
                    # appropriately.
                    symbol = access.node.scope.symbol_table.lookup(name)

                    # If it has been read before we have to check if ...
                    if has_been_read:
                        loop_pos = loop_ancestor.loop_body.abs_position
                        if last_read_position < loop_pos:
                            # .. it was before the loop, so it is fprivate
                            fprivate.add(symbol)
                        else:
                            # or inside the loop, in which case it needs sync
                            need_sync.add(symbol)
                        break

                    # If the write is not guaranteed, we make it firstprivate
                    # so that in the case that the write doesn't happen we keep
                    # the original value
                    conditional_write = access.node.ancestor(
                        IfBlock,
                        limit=loop_ancestor,
                        include_self=True)
                    if conditional_write:
                        fprivate.add(symbol)
                        break

                    # Already found the first write and decided if it is
                    # shared, private or firstprivate. We can stop looking.
                    private.add(symbol)
                    break

        return private, fprivate, need_sync

    def validate_global_constraints(self):
        '''
        Perform validation checks that can only be done at code-generation
        time.

        :raises GenerationError: if this OMPDoDirective is not enclosed \
                            within some OpenMP parallel region.
        '''
        if self.ancestor(OMPParallelDirective) is not None:
            raise GenerationError("Cannot nest OpenMP parallel regions.")
        self._encloses_omp_directive()

    def _encloses_omp_directive(self):
        ''' Check that this Parallel region contains other OpenMP
            directives. While it doesn't have to (in order to be valid
            OpenMP), it is likely that an absence of directives
            is an error on the part of the user. '''
        # We need to recurse down through all our children and check
        # whether any of them are an OMPRegionDirective.
        node_list = self.walk(OMPRegionDirective)
        if not node_list:
            # TODO raise a warning here so that the user can decide
            # whether or not this is OK.
            pass
            # raise GenerationError("OpenMP parallel region does not enclose "
            #                       "any OpenMP directives. This is probably "
            #                       "not what you want.")


class OMPTaskloopDirective(OMPRegionDirective):
    '''
    Class representing an OpenMP TASKLOOP directive in the PSyIR.

    :param grainsize: The grainsize value used to specify the grainsize \
                      clause on this OpenMP directive. If this is None \
                      the grainsize clause is not applied. Default \
                      value is None.
    :type grainsize: int or None.
    :param num_tasks: The num_tasks value used to specify the num_tasks \
                      clause on this OpenMP directive. If this is None \
                      the num_tasks clause is not applied. Default value \
                      is None.
    :type num_tasks: int or None.
    :param nogroup: Whether the nogroup clause should be used for this node. \
                    Default value is False
    :type nogroup: bool
    :param kwargs: additional keyword arguments provided to the PSyIR node.
    :type kwargs: unwrapped dict.

    :raises GenerationError: if this OMPTaskloopDirective has both \
                             a grainsize and num_tasks value \
                             specified.
    '''
    # This specification respects the mutual exclusion of OMPGransizeClause
    # and OMPNumTasksClause, but adds an additional ordering requirement.
    # Other specifications to soften the ordering requirement are possible,
    # but need additional checks in the global constraints instead.
    _children_valid_format = ("Schedule, [OMPGrainsizeClause | "
                              "OMPNumTasksClause], [OMPNogroupClause]")

    def __init__(self, grainsize=None, num_tasks=None, nogroup=False,
                 **kwargs):
        # These remain primarily for the gen_code interface
        self._grainsize = grainsize
        self._num_tasks = num_tasks
        self._nogroup = nogroup
        if self._grainsize is not None and self._num_tasks is not None:
            raise GenerationError(
                "OMPTaskloopDirective must not have both grainsize and "
                "numtasks clauses specified.")
        super().__init__(**kwargs)
        if self._grainsize is not None:
            child = [Literal(f"{grainsize}", INTEGER_TYPE)]
            self._children.append(OMPGrainsizeClause(children=child))
        if self._num_tasks is not None:
            child = [Literal(f"{num_tasks}", INTEGER_TYPE)]
            self._children.append(OMPNumTasksClause(children=child))
        if self._nogroup:
            self._children.append(OMPNogroupClause())

    @staticmethod
    def _validate_child(position, child):
        '''
         Decides whether a given child and position are valid for this node.
         The rules are:
         1. Child 0 must always be a Schedule.
         2. Child 1 may be either a OMPGrainsizeClause or OMPNumTasksClause, \
            or if neither of those clauses are present, it may be a \
            OMPNogroupClause.
         3. Child 2 must always be a OMPNogroupClause, and can only exist if \
            child 1 is a OMPGrainsizeClause or OMPNumTasksClause.

        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        if position == 0:
            return isinstance(child, Schedule)
        if position == 1:
            return isinstance(child, (OMPGrainsizeClause, OMPNumTasksClause,
                                      OMPNogroupClause))
        if position == 2:
            return isinstance(child, OMPNogroupClause)
        return False

    @property
    def nogroup(self):
        '''
        :returns: the nogroup clause status of this node.
        :rtype: bool
        '''
        return self._nogroup

    def validate_global_constraints(self):
        '''
        Perform validation checks that can only be done at code-generation
        time.

        :raises GenerationError: if this OMPTaskloopDirective is not \
                                 enclosed within an OpenMP serial region.
        :raises GenerationError: if this OMPTaskloopDirective has two
                                 Nogroup clauses as children.
        '''
        # It is only at the point of code generation that we can check for
        # correctness (given that we don't mandate the order that a user
        # can apply transformations to the code). A taskloop directive, we must
        # have an OMPSerialDirective as an ancestor back up the tree.
        if not self.ancestor(OMPSerialDirective):
            raise GenerationError(
                "OMPTaskloopDirective must be inside an OMP Serial region "
                "but could not find an ancestor node")

        # Check children are well formed.
        # _validate_child will ensure position 0 and 1 are valid.
        if len(self._children) == 3 and isinstance(self._children[1],
                                                   OMPNogroupClause):
            raise GenerationError(
                "OMPTaskloopDirective has two Nogroup clauses as children "
                "which is not allowed.")

        super().validate_global_constraints()

    def gen_code(self, parent):
        '''
        Generate the f2pygen AST entries in the Schedule for this OpenMP
        taskloop directive.

        :param parent: the parent Node in the Schedule to which to add our \
                       content.
        :type parent: sub-class of :py:class:`psyclone.f2pygen.BaseGen`
        :raises GenerationError: if this "!$omp taskloop" is not enclosed \
                                 within an OMP Parallel region and an OMP \
                                 Serial region.

        '''
        self.validate_global_constraints()

        extra_clauses = ""
        # Find the specified clauses
        clause_list = []
        if self._grainsize is not None:
            clause_list.append(f"grainsize({self._grainsize})")
        if self._num_tasks is not None:
            clause_list.append(f"num_tasks({self._num_tasks})")
        if self._nogroup:
            clause_list.append("nogroup")
        # Generate the string containing the required clauses
        extra_clauses = ", ".join(clause_list)

        parent.add(DirectiveGen(parent, "omp", "begin", "taskloop",
                                extra_clauses))

        self.dir_body.gen_code(parent)

        # make sure the directive occurs straight after the loop body
        position = parent.previous_loop()
        parent.add(DirectiveGen(parent, "omp", "end", "taskloop", ""),
                   position=["after", position])

    def begin_string(self):
        '''Returns the beginning statement of this directive, i.e.
        "omp taskloop ...". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the beginning statement for this directive.
        :rtype: str

        '''
        return "omp taskloop"

    def end_string(self):
        '''Returns the end (or closing) statement of this directive, i.e.
        "omp end taskloop". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the end statement for this directive.
        :rtype: str

        '''
        return "omp end taskloop"


class OMPDoDirective(OMPRegionDirective):
    '''
    Class representing an OpenMP DO directive in the PSyIR.

    :param str omp_schedule: the OpenMP schedule to use (defaults to
        "none" which means it is implementation dependent).
    :param Optional[int] collapse: optional number of nested loops to \
        collapse into a single iteration space to parallelise. Defaults to \
        None.
    :param Optional[bool] reprod: whether or not to generate code for \
        run-reproducible OpenMP reductions (if not specified the value is \
        provided by the PSyclone Config file).
    :param kwargs: additional keyword arguments provided to the PSyIR node.
    :type kwargs: unwrapped dict.

    '''
    _directive_string = "do"

    def __init__(self, omp_schedule="none", collapse=None, reprod=None,
                 **kwargs):

        super().__init__(**kwargs)
        if reprod is None:
            self._reprod = Config.get().reproducible_reductions
        else:
            self._reprod = reprod

        self._omp_schedule = omp_schedule
        self._collapse = None
        self.collapse = collapse  # Use setter with error checking

    def __eq__(self, other):
        '''
        Checks whether two nodes are equal. Two OMPDoDirective nodes are equal
        if they have the same schedule, the same reproducible reduction option
        (and the inherited equality is True).

        :param object other: the object to check equality to.

        :returns: whether other is equal to self.
        :rtype: bool
        '''
        is_eq = super().__eq__(other)
        is_eq = is_eq and self.omp_schedule == other.omp_schedule
        is_eq = is_eq and self.reprod == other.reprod
        is_eq = is_eq and self.collapse == other.collapse

        return is_eq

    @property
    def collapse(self):
        '''
        :returns: the value of the collapse clause.
        :rtype: int or NoneType
        '''
        return self._collapse

    @collapse.setter
    def collapse(self, value):
        '''
        TODO #1648: Note that gen_code ignores the collapse clause but the
        generated code is still valid. Since gen_code is going to be removed
        and it is only used for LFRic (which does not support GPU offloading
        that gets improved with the collapse clause) it will not be supported.

        :param value: optional number of nested loop to collapse into a \
            single iteration space to parallelise. Defaults to None.
        :type value: int or NoneType.

        :raises TypeError: if the collapse value given is not an integer \
            or NoneType.
        :raises ValueError: if the collapse integer given is not positive.

        '''
        if value is not None and not isinstance(value, int):
            raise TypeError(
                f"The {type(self).__name__} collapse clause must be a positive"
                f" integer or None, but value '{value}' has been given.")

        if value is not None and value <= 0:
            raise ValueError(
                f"The {type(self).__name__} collapse clause must be a positive"
                f" integer or None, but value '{value}' has been given.")

        self._collapse = value

    def node_str(self, colour=True):
        '''
        Returns the name of this node with (optional) control codes
        to generate coloured output in a terminal that supports it.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        :rtype: str
        '''
        parts = []
        if self.omp_schedule != "none":
            parts.append(f"omp_schedule={self.omp_schedule}")
        if self.reductions():
            parts.append(f"reprod={self._reprod}")
        if self._collapse and self._collapse > 1:
            parts.append(f"collapse={self._collapse}")
        return f"{self.coloured_name(colour)}[{','.join(parts)}]"

    def _reduction_string(self):
        '''
        :returns: the OMP reduction information.
        :rtype: str
        '''
        for reduction_type in AccessType.get_valid_reduction_modes():
            reductions = self._get_reductions_list(reduction_type)
            parts = []
            for reduction in reductions:
                parts.append(f"reduction("
                             f"{OMP_OPERATOR_MAPPING[reduction_type]}:"
                             f"{reduction})")
        return ", ".join(parts)

    @property
    def omp_schedule(self):
        '''
        :returns: the omp_schedule for this object.
        :rtype: str
        '''
        return self._omp_schedule

    @omp_schedule.setter
    def omp_schedule(self, value):
        '''
        :param str value: the omp_schedule for this object.

        :raises TypeError: if the provided omp_schedule is not a valid \
            schedule string.
        '''
        if not isinstance(value, str):
            raise TypeError(
                f"{type(self).__name__} omp_schedule should be a str "
                f"but found '{type(value).__name__}'.")
        if (value.split(',')[0].lower() not in
                OMPScheduleClause.VALID_OMP_SCHEDULES):
            raise TypeError(
                f"{type(self).__name__} omp_schedule should be one of "
                f"{OMPScheduleClause.VALID_OMP_SCHEDULES} but found "
                f"'{value}'.")
        self._omp_schedule = value

    @property
    def reprod(self):
        '''
        :returns: whether reprod has been set for this object or not.
        '''
        return self._reprod

    @reprod.setter
    def reprod(self, value):
        '''
        :param bool value: enable or disable reproducible loop parallelism.
        '''
        self._reprod = value

    def validate_global_constraints(self):
        '''
        Perform validation checks that can only be done at code-generation
        time.

        :raises GenerationError: if this OMPDoDirective is not enclosed \
                            within some OpenMP parallel region.
        '''
        # It is only at the point of code generation that we can check for
        # correctness (given that we don't mandate the order that a user
        # can apply transformations to the code). As a loop
        # directive, we must have an OMPParallelDirective as an ancestor
        # somewhere back up the tree.
        if not self.ancestor(OMPParallelDirective,
                             excluding=OMPParallelDoDirective):
            raise GenerationError(
                "OMPDoDirective must be inside an OMP parallel region but "
                "could not find an ancestor OMPParallelDirective node")

        self._validate_single_loop()
        self._validate_collapse_value()

        super().validate_global_constraints()

    def _validate_collapse_value(self):
        '''
        Checks that if there is a collapse clause, there must be as many
        immediately nested loops as the collapse value.

        :raises GenerationError: if this OMPLoopDirective has a collapse \
            clause but it doesn't have the expected number of nested Loops.
        '''
        if self._collapse:
            cursor = self.dir_body.children[0]
            for depth in range(self._collapse):
                if (len(cursor.parent.children) != 1 or
                        not isinstance(cursor, Loop)):
                    raise GenerationError(
                        f"{type(self).__name__} must have as many immediately "
                        f"nested loops as the collapse clause specifies but "
                        f"'{self}' has a collapse={self._collapse} and the "
                        f"nested body at depth {depth} cannot be "
                        f"collapsed.")
                cursor = cursor.loop_body.children[0]

    def _validate_single_loop(self):
        '''
        Checks that this directive is only applied to a single Loop node.

        :raises GenerationError: if this directive has more than one child.
        :raises GenerationError: if the child of this directive is not a Loop.

        '''
        if len(self.dir_body.children) != 1:
            raise GenerationError(
                f"An {type(self).__name__} can only be applied to a single "
                f"loop but this Node has {len(self.dir_body.children)} "
                f"children: {self.dir_body.children}")

        if not isinstance(self.dir_body[0], Loop):
            raise GenerationError(
                f"An {type(self).__name__} can only be applied to a loop but "
                f"this Node has a child of type "
                f"'{type(self.dir_body[0]).__name__}'")

    def gen_code(self, parent):
        '''
        Generate the f2pygen AST entries in the Schedule for this OpenMP do
        directive.

        TODO #1648: Note that gen_code ignores the collapse clause but the
        generated code is still valid. Since gen_code is going to be removed
        and it is only used for LFRic (which does not support GPU offloading
        that gets improved with the collapse clause) it will not be supported.

        :param parent: the parent Node in the Schedule to which to add our \
                       content.
        :type parent: sub-class of :py:class:`psyclone.f2pygen.BaseGen`
        :raises GenerationError: if this "!$omp do" is not enclosed within \
                                 an OMP Parallel region.

        '''
        self.validate_global_constraints()

        parts = []

        if self.omp_schedule != "none":
            parts.append(f"schedule({self.omp_schedule})")

        if not self._reprod:
            red_str = self._reduction_string()
            if red_str:
                parts.append(red_str)

        # As we're a loop we don't specify the scope
        # of any variables so we don't have to generate the
        # list of private variables
        options = ", ".join(parts)
        parent.add(DirectiveGen(parent, "omp", "begin", "do", options))

        for child in self.children:
            child.gen_code(parent)

        # make sure the directive occurs straight after the loop body
        position = parent.previous_loop()
        parent.add(DirectiveGen(parent, "omp", "end", "do", ""),
                   position=["after", position])

    def begin_string(self):
        '''Returns the beginning statement of this directive, i.e.
        "omp do ...". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the beginning statement for this directive.
        :rtype: str

        '''
        string = f"omp {self._directive_string}"
        if self.omp_schedule != "none":
            string += f" schedule({self.omp_schedule})"
        if self._collapse:
            string += f" collapse({self._collapse})"
        return string

    def end_string(self):
        '''Returns the end (or closing) statement of this directive, i.e.
        "omp end do". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the end statement for this directive.
        :rtype: str

        '''
        return f"omp end {self._directive_string}"


class OMPParallelDoDirective(OMPParallelDirective, OMPDoDirective):
    ''' Class for the !$OMP PARALLEL DO directive. This inherits from
        both OMPParallelDirective (because it creates a new OpenMP
        thread-parallel region) and OMPDoDirective (because it
        causes a loop to be parallelised).

        :param kwargs: additional keyword arguments provided to the PSyIR node.
        :type kwargs: unwrapped dict.
    '''

    _children_valid_format = ("Schedule, OMPDefaultClause, OMPPrivateClause, "
                              "OMPFirstprivateClause, OMPScheduleClause, "
                              "[OMPReductionClause]*")
    _directive_string = "parallel do"

    def __init__(self, **kwargs):
        OMPDoDirective.__init__(self, **kwargs)
        self.addchild(OMPDefaultClause(
            clause_type=OMPDefaultClause.DefaultClauseTypes.SHARED))

    @staticmethod
    def _validate_child(position, child):
        '''
        :param int position: the position to be validated.
        :param child: a child to be validated.
        :type child: :py:class:`psyclone.psyir.nodes.Node`

        :return: whether the given child and position are valid for this node.
        :rtype: bool

        '''
        if position == 0 and isinstance(child, Schedule):
            return True
        if position == 1 and isinstance(child, OMPDefaultClause):
            return True
        if position == 2 and isinstance(child, OMPPrivateClause):
            return True
        if position == 3 and isinstance(child, OMPFirstprivateClause):
            return True
        if position == 4 and isinstance(child, OMPScheduleClause):
            return True
        if position >= 5 and isinstance(child, OMPReductionClause):
            return True
        return False

    def gen_code(self, parent):
        '''
        Generate the f2pygen AST entries in the Schedule for this OpenMP
        directive.

        TODO #1648: Note that gen_code ignores the collapse clause but the
        generated code is still valid. Since gen_code is going to be removed
        and it is only used for LFRic (which does not support GPU offloading
        that gets improved with the collapse clause) it will not be supported.

        :param parent: the parent Node in the Schedule to which to add our \
                       content.
        :type parent: sub-class of :py:class:`psyclone.f2pygen.BaseGen`

        '''
        # We're not doing nested parallelism so make sure that this
        # omp parallel do is not already within some parallel region
        # pylint: disable=import-outside-toplevel
        from psyclone.psyGen import zero_reduction_variables
        self.validate_global_constraints()

        calls = self.reductions()
        zero_reduction_variables(calls, parent)

        # Set default() private() and firstprivate() clauses
        # pylint: disable=protected-access
        default_str = self.children[1]._clause_string
        # pylint: enable=protected-access
        private, fprivate, need_sync = self._infer_sharing_attributes()
        private_clause = OMPPrivateClause.create(
                            sorted(private, key=lambda x: x.name))
        fprivate_clause = OMPFirstprivateClause.create(
                            sorted(fprivate, key=lambda x: x.name))
        if need_sync:
            raise GenerationError(
                f"OMPParallelDoDirective.gen_code() does not support symbols "
                f"that need synchronisation, but found: "
                f"{[x.name for x in need_sync]}")

        private_str = ""
        fprivate_str = ""
        private_list = [child.symbol.name for child in private_clause.children]
        if private_list:
            private_str = "private(" + ",".join(private_list) + ")"
        fp_list = [child.symbol.name for child in fprivate_clause.children]
        if fp_list:
            fprivate_str = "firstprivate(" + ",".join(fp_list) + ")"

        # Set schedule clause
        if self._omp_schedule != "none":
            schedule_str = f"schedule({self._omp_schedule})"
        else:
            schedule_str = ""

        # Add directive to the f2pygen tree
        parent.add(
            DirectiveGen(
                parent, "omp", "begin", "parallel do", ", ".join(
                    text for text in [default_str, private_str, fprivate_str,
                                      schedule_str, self._reduction_string()]
                    if text)))

        for child in self.dir_body:
            child.gen_code(parent)

        # make sure the directive occurs straight after the loop body
        position = parent.previous_loop()
        parent.add(DirectiveGen(parent, *self.end_string().split()),
                   position=["after", position])

        self.gen_post_region_code(parent)

    def lower_to_language_level(self):
        '''
        In-place construction of clauses as PSyIR constructs.
        The clauses here may need to be updated if code has changed, or be
        added if not yet present.

        :returns: the lowered version of this node.
        :rtype: :py:class:`psyclone.psyir.node.Node`

        '''
        # Calling the super() explicitly to avoid confusion
        # with the multiple-inheritance
        OMPParallelDirective.lower_to_language_level(self)
        self.addchild(OMPScheduleClause(self._omp_schedule))
        return self

    def begin_string(self):
        '''Returns the beginning statement of this directive, i.e.
        "omp parallel do ...". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the beginning statement for this directive.
        :rtype: str

        '''
        string = f"omp {self._directive_string}"
        if self._collapse:
            string += f" collapse({self._collapse})"
        string += self._reduction_string()
        return string

    def end_string(self):
        '''Returns the end (or closing) statement of this directive, i.e.
        "omp end parallel do". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the end statement for this directive.
        :rtype: str

        '''
        return f"omp end {self._directive_string}"

    def validate_global_constraints(self):
        '''
        Perform validation checks that can only be done at code-generation
        time.

        '''
        OMPParallelDirective.validate_global_constraints(self)

        self._validate_single_loop()
        self._validate_collapse_value()


class OMPTeamsDistributeParallelDoDirective(OMPParallelDoDirective):
    ''' Class representing the OMP teams distribute parallel do directive. '''
    _directive_string = "teams distribute parallel do"


class OMPTargetDirective(OMPRegionDirective):
    ''' Class for the !$OMP TARGET directive that offloads the code contained
    in its region into an accelerator device. '''

    def begin_string(self):
        '''Returns the beginning statement of this directive, i.e.
        "omp target". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the opening statement of this directive.
        :rtype: str

        '''
        return "omp target"

    def end_string(self):
        '''Returns the end (or closing) statement of this directive, i.e.
        "omp end target". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the end statement for this directive.
        :rtype: str

        '''
        return "omp end target"


class OMPLoopDirective(OMPRegionDirective):
    ''' Class for the !$OMP LOOP directive that specifies that the iterations
    of the associated loops may execute concurrently.

    :param Optional[int] collapse: optional number of nested loops to \
        collapse into a single iteration space to parallelise. Defaults \
        to None.
    :param kwargs: additional keyword arguments provided to the PSyIR node.
    :type kwargs: unwrapped dict.
    '''

    def __init__(self, collapse=None, **kwargs):
        super().__init__(**kwargs)
        self._collapse = None
        self.collapse = collapse  # Use setter with error checking

    def __eq__(self, other):
        '''
        Checks whether two nodes are equal. Two OMPLoopDirective nodes are
        equal if they have the same collapse status and the inherited
        equality is true.

        :param object other: the object to check equality to.

        :returns: whether other is equal to self.
        :rtype: bool
        '''
        is_eq = super().__eq__(other)
        is_eq = is_eq and self.collapse == other.collapse

        return is_eq

    @property
    def collapse(self):
        '''
        :returns: the value of the collapse clause.
        :rtype: int or NoneType
        '''
        return self._collapse

    @collapse.setter
    def collapse(self, value):
        '''
        TODO #1648: Note that gen_code ignores the collapse clause but the
        generated code is still valid. Since gen_code is going to be removed
        and it is only used for LFRic (which does not support GPU offloading
        that gets improved with the collapse clause) it will not be supported.

        :param value: optional number of nested loop to collapse into a \
            single iteration space to parallelise. Defaults to None.
        :type value: int or NoneType.

        :raises TypeError: if the collapse value given is not an integer \
            or NoneType.
        :raises ValueError: if the collapse integer given is not positive.

        '''
        if value is not None and not isinstance(value, int):
            raise TypeError(
                f"The OMPLoopDirective collapse clause must be a positive "
                f"integer or None, but value '{value}' has been given.")

        if value is not None and value <= 0:
            raise ValueError(
                f"The OMPLoopDirective collapse clause must be a positive "
                f"integer or None, but value '{value}' has been given.")

        self._collapse = value

    def node_str(self, colour=True):
        ''' Returns the name of this node with (optional) control codes
        to generate coloured output in a terminal that supports it.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        :rtype: str
        '''
        text = self.coloured_name(colour)
        if self._collapse:
            text += f"[collapse={self._collapse}]"
        else:
            text += "[]"
        return text

    def begin_string(self):
        ''' Returns the beginning statement of this directive, i.e. "omp loop".
        The visitor is responsible for adding the correct directive beginning
        (e.g. "!$").

        :returns: the opening statement of this directive.
        :rtype: str

        '''
        string = "omp loop"
        if self._collapse:
            string += f" collapse({self._collapse})"
        return string

    def end_string(self):
        '''Returns the end (or closing) statement of this directive, i.e.
        "omp end loop". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the end statement for this directive.
        :rtype: str

        '''
        return "omp end loop"

    def validate_global_constraints(self):
        ''' Perform validation of those global constraints that can only be
        done at code-generation time.

        :raises GenerationError: if this OMPLoopDirective has more than one \
            child in its associated schedule.
        :raises GenerationError: if the schedule associated with this \
            OMPLoopDirective does not contain a Loop.
        :raises GenerationError: this directive must be inside a omp target \
            or parallel region.
        :raises GenerationError: if this OMPLoopDirective has a collapse \
            clause but it doesn't have the expected number of nested Loops.

        '''
        if len(self.dir_body.children) != 1:
            raise GenerationError(
                f"OMPLoopDirective must have exactly one child in its "
                f"associated schedule but found {self.dir_body.children}.")

        if not isinstance(self.dir_body.children[0], Loop):
            raise GenerationError(
                f"OMPLoopDirective must have a Loop as child of its associated"
                f" schedule but found '{self.dir_body.children[0]}'.")

        if not self.ancestor((OMPTargetDirective, OMPParallelDirective)):
            # Also omp teams or omp threads regions but these are not supported
            # in the PSyIR
            raise GenerationError(
                f"OMPLoopDirective must be inside a OMPTargetDirective or a "
                f"OMPParallelDirective, but '{self}' is not.")

        # If there is a collapse clause, there must be as many immediately
        # nested loops as the collapse value
        if self._collapse:
            cursor = self.dir_body.children[0]
            for depth in range(self._collapse):
                if not isinstance(cursor, Loop):
                    raise GenerationError(
                        f"OMPLoopDirective must have as many immediately "
                        f"nested loops as the collapse clause specifies but "
                        f"'{self}' has a collapse={self._collapse} and the "
                        f"nested statement at depth {depth} is a "
                        f"{type(cursor).__name__} rather than a Loop.")
                cursor = cursor.loop_body.children[0]

        super().validate_global_constraints()


# For automatic API documentation generation
__all__ = ["OMPRegionDirective", "OMPParallelDirective", "OMPSingleDirective",
           "OMPMasterDirective", "OMPDoDirective", "OMPParallelDoDirective",
           "OMPSerialDirective", "OMPTaskloopDirective", "OMPTargetDirective",
           "OMPTaskwaitDirective", "OMPDirective", "OMPStandaloneDirective",
           "OMPLoopDirective", "OMPDeclareTargetDirective"]
