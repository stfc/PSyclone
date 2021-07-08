# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
#         I. Kavcic,    Met Office
#         C.M. Maynard, Met Office / University of Reading
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' This module contains the OpenMP Directives node implementation.'''

from __future__ import absolute_import
from psyclone.f2pygen import (AssignGen, UseGen, DeclGen, DirectiveGen,
                              CommentGen)
from psyclone.psyir.nodes.directive import Directive
from psyclone.psyir.nodes.routine import Routine
from psyclone.psyir.nodes.loop import Loop
from psyclone.errors import GenerationError, InternalError
from psyclone.configuration import Config
from psyclone.core import AccessType, VariablesAccessInfo

# OMP_OPERATOR_MAPPING is used to determine the operator to use in the
# reduction clause of an OpenMP directive.
OMP_OPERATOR_MAPPING = {AccessType.SUM: "+"}


class OMPDirective(Directive):
    '''
    Base class for all OpenMP-related directives

    '''
    _PREFIX = "OMP"

    @property
    def dag_name(self):
        '''
        :returns: the name to use in a dag for this node
        :rtype: str
        '''
        _, position = self._find_position(self.ancestor(Routine))
        return "OMP_directive_" + str(position)

    def node_str(self, colour=True):
        '''
        Returns the name of this node with (optional) control codes
        to generate coloured output in a terminal that supports it.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        :rtype: str
        '''
        return self.coloured_name(colour) + "[OMP]"

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
            for arg in call.arguments.args:
                if arg.argument_type in const.VALID_SCALAR_NAMES:
                    if arg.descriptor.access == reduction_type:
                        if arg.name not in result:
                            result.append(arg.name)
        return result


class OMPParallelDirective(OMPDirective):

    @property
    def dag_name(self):
        '''
        :returns: the name to use in the DAG for this node.
        :rtype: str
        '''
        _, position = self._find_position(self.ancestor(Routine))
        return "OMP_parallel_" + str(position)

    def node_str(self, colour=True):
        '''
        Returns the name of this node with (optional) control codes
        to generate coloured output in a terminal that supports it.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        :rtype: str
        '''
        return self.coloured_name(colour) + "[OMP parallel]"

    def gen_code(self, parent):
        '''Generate the fortran OMP Parallel Directive and any associated
        code'''
        from psyclone.psyGen import zero_reduction_variables

        private_list = self._get_private_list()

        reprod_red_call_list = self.reductions(reprod=True)
        if reprod_red_call_list:
            # we will use a private thread index variable
            thread_idx = self.scope.symbol_table.\
                lookup_with_tag("omp_thread_index").name
            private_list.append(thread_idx)
            # declare the variable
            parent.add(DeclGen(parent, datatype="integer",
                               entity_decls=[thread_idx]))
        private_str = ",".join(private_list)

        # We're not doing nested parallelism so make sure that this
        # omp parallel region is not already within some parallel region
        self.validate_global_constraints()

        # Check that this OpenMP PARALLEL directive encloses other
        # OpenMP directives. Although it is valid OpenMP if it doesn't,
        # this almost certainly indicates a user error.
        self._encloses_omp_directive()

        calls = self.reductions()

        # first check whether we have more than one reduction with the same
        # name in this Schedule. If so, raise an error as this is not
        # supported for a parallel region.
        names = []
        for call in calls:
            name = call.reduction_arg.name
            if name in names:
                raise GenerationError(
                    "Reduction variables can only be used once in an invoke. "
                    "'{0}' is used multiple times, please use a different "
                    "reduction variable".format(name))
            else:
                names.append(name)

        zero_reduction_variables(calls, parent)

        parent.add(DirectiveGen(parent, "omp", "begin", "parallel",
                                "default(shared), private({0})".
                                format(private_str)))

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
        private_list = self._get_private_list()
        private_str = ",".join(private_list)

        if private_str:
            result = "{0} private({1})".format(result, private_str)
        return result

    def end_string(self):
        '''Returns the end (or closing) statement of this directive, i.e.
        "omp end parallel". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the end statement for this directive.
        :rtype: str

        '''
        # pylint: disable=no-self-use
        return "omp end parallel"

    def _get_private_list(self):
        '''
        Returns the variable names used for any loops within a directive
        and any variables that have been declared private by a Kernel
        within the directive.

        :returns: list of variables to declare as thread private.
        :rtype: list of str

        :raises InternalError: if a Kernel has local variable(s) but they \
                               aren't named.
        '''
        from psyclone.psyGen import InvokeSchedule
        result = set()
        # get variable names from all calls that are a child of this node
        for call in self.kernels():
            for variable_name in call.local_vars():
                if variable_name == "":
                    raise InternalError(
                        "call '{0}' has a local variable but its "
                        "name is not set.".format(call.name))
                result.add(variable_name.lower())

        # Now determine scalar variables that must be private:
        var_accesses = VariablesAccessInfo()
        self.reference_accesses(var_accesses)
        for signature in var_accesses.all_signatures:
            accesses = var_accesses[signature].all_accesses
            # Ignore variables that have indices, we only look at scalar
            if accesses[0].is_array():
                continue

            # If a variable is only accessed once, it is either an error
            # or a shared variable - anyway it is not private
            if len(accesses) == 1:
                continue

            # We have at least two accesses. If the first one is a write,
            # assume the variable should be private:
            if accesses[0].access_type == AccessType.WRITE:
                # Check if the write access is inside the parallel loop. If
                # the write is outside of a loop, it is an assignment to
                # a shared variable. Example where jpk is likely used
                # outside of the parallel section later, so it must be
                # declared as shared in order to have its value in other loops:
                # !$omp parallel
                # jpk = 100
                # !omp do
                # do ji = 1, jpk

                # TODO #598: improve the handling of scalar variables.

                # Go up the tree till we either find the InvokeSchedule,
                # which is at the top, or a Loop statement (or no parent,
                # which means we have reached the end of a called kernel).
                parent = accesses[0].node.ancestor((Loop, InvokeSchedule),
                                                   include_self=True)

                if parent and isinstance(parent, Loop):
                    # The assignment to the variable is inside a loop, so
                    # declare it to be private
                    result.add(str(signature).lower())

        # Convert the set into a list and sort it, so that we get
        # reproducible results
        list_result = list(result)
        list_result.sort()
        return list_result

    def validate_global_constraints(self):
        '''
        Perform validation checks that can only be done at code-generation
        time.

        :raises GenerationError: if this OMPDoDirective is not enclosed \
                            within some OpenMP parallel region.
        '''
        if self.ancestor(OMPParallelDirective) is not None:
            raise GenerationError("Cannot nest OpenMP parallel regions.")

    def _encloses_omp_directive(self):
        ''' Check that this Parallel region contains other OpenMP
            directives. While it doesn't have to (in order to be valid
            OpenMP), it is likely that an absence of directives
            is an error on the part of the user. '''
        # We need to recurse down through all our children and check
        # whether any of them are an OMPDirective.
        node_list = self.walk(OMPDirective)
        if not node_list:
            # TODO raise a warning here so that the user can decide
            # whether or not this is OK.
            pass
            # raise GenerationError("OpenMP parallel region does not enclose "
            #                       "any OpenMP directives. This is probably "
            #                       "not what you want.")

    def update(self):
        '''
        Updates the fparser2 AST by inserting nodes for this OpenMP
        parallel region.

        '''
        # TODO #435: Remove this function once this is fixed
        self._add_region(
            start_text="parallel default(shared), private({0})".format(
                ",".join(self._get_private_list())),
            end_text="end parallel")


class OMPDoDirective(OMPDirective):
    '''
    Class representing an OpenMP DO directive in the PSyclone AST.

    :param list children: list of Nodes that are children of this Node.
    :param parent: the Node in the AST that has this directive as a child.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`
    :param str omp_schedule: the OpenMP schedule to use.
    :param bool reprod: whether or not to generate code for run-reproducible \
                        OpenMP reductions.

    '''
    def __init__(self, children=None, parent=None, omp_schedule="static",
                 reprod=None):

        if children is None:
            children = []

        if reprod is None:
            self._reprod = Config.get().reproducible_reductions
        else:
            self._reprod = reprod

        self._omp_schedule = omp_schedule

        # Call the init method of the base class once we've stored
        # the OpenMP schedule
        super(OMPDoDirective, self).__init__(children=children,
                                             parent=parent)

    @property
    def dag_name(self):
        '''
        :returns: the name to use in the DAG for this node.
        :rtype: str
        '''
        _, position = self._find_position(self.ancestor(Routine))
        return "OMP_do_" + str(position)

    def node_str(self, colour=True):
        '''
        Returns the name of this node with (optional) control codes
        to generate coloured output in a terminal that supports it.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        :rtype: str
        '''
        if self.reductions():
            reprod = "[reprod={0}]".format(self._reprod)
        else:
            reprod = ""
        return "{0}[OMP do]{1}".format(self.coloured_name(colour), reprod)

    def _reduction_string(self):
        ''' Return the OMP reduction information as a string '''
        reduction_str = ""
        for reduction_type in AccessType.get_valid_reduction_modes():
            reductions = self._get_reductions_list(reduction_type)
            for reduction in reductions:
                reduction_str += ", reduction({0}:{1})".format(
                    OMP_OPERATOR_MAPPING[reduction_type], reduction)
        return reduction_str

    @property
    def reprod(self):
        ''' returns whether reprod has been set for this object or not '''
        return self._reprod

    def validate_global_constraints(self):
        '''
        Perform validation checks that can only be done at code-generation
        time.

        :raises GenerationError: if this OMPDoDirective is not enclosed \
                            within some OpenMP parallel region.
        '''
        # It is only at the point of code generation that we can check for
        # correctness (given that we don't mandate the order that a user
        # can apply transformations to the code). As an orphaned loop
        # directive, we must have an OMPParallelDirective as an ancestor
        # somewhere back up the tree.
        if not self.ancestor(OMPParallelDirective,
                             excluding=OMPParallelDoDirective):
            raise GenerationError(
                "OMPDoDirective must be inside an OMP parallel region but "
                "could not find an ancestor OMPParallelDirective node")

        super(OMPDoDirective, self).validate_global_constraints()

    def gen_code(self, parent):
        '''
        Generate the f2pygen AST entries in the Schedule for this OpenMP do
        directive.

        :param parent: the parent Node in the Schedule to which to add our \
                       content.
        :type parent: sub-class of :py:class:`psyclone.f2pygen.BaseGen`
        :raises GenerationError: if this "!$omp do" is not enclosed within \
                                 an OMP Parallel region.

        '''
        self.validate_global_constraints()

        if self._reprod:
            local_reduction_string = ""
        else:
            local_reduction_string = self._reduction_string()

        # As we're an orphaned loop we don't specify the scope
        # of any variables so we don't have to generate the
        # list of private variables
        options = "schedule({0})".format(self._omp_schedule) + \
                  local_reduction_string
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
        return "omp do schedule({0})".format(self._omp_schedule)

    def end_string(self):
        '''Returns the end (or closing) statement of this directive, i.e.
        "omp end do". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the end statement for this directive.
        :rtype: str

        '''
        # pylint: disable=no-self-use
        return "omp end do"

    def update(self):
        '''
        Updates the fparser2 AST by inserting nodes for this OpenMP do.

        :raises GenerationError: if the existing AST doesn't have the \
                                 correct structure to permit the insertion \
                                 of the OpenMP parallel do.
        '''
        self.validate_global_constraints()

        # Since this is an OpenMP do, it can only be applied
        # to a single loop.
        if len(self.dir_body.children) != 1:
            raise GenerationError(
                "An OpenMP DO can only be applied to a single loop "
                "but this Node has {0} children: {1}".
                format(len(self.dir_body.children), self.dir_body.children))

        self._add_region(start_text="do schedule({0})".format(
            self._omp_schedule), end_text="end do")


class OMPParallelDoDirective(OMPParallelDirective, OMPDoDirective):
    ''' Class for the !$OMP PARALLEL DO directive. This inherits from
        both OMPParallelDirective (because it creates a new OpenMP
        thread-parallel region) and OMPDoDirective (because it
        causes a loop to be parallelised). '''

    def __init__(self, children=[], parent=None, omp_schedule="static"):
        OMPDoDirective.__init__(self,
                                children=children,
                                parent=parent,
                                omp_schedule=omp_schedule)

    @property
    def dag_name(self):
        '''
        :returns: the name to use in the DAG for this node.
        :rtype: str
        '''
        _, position = self._find_position(self.ancestor(Routine))
        return "OMP_parallel_do_" + str(position)

    def node_str(self, colour=True):
        '''
        Returns the name of this node with (optional) control codes
        to generate coloured output in a terminal that supports it.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        :rtype: str
        '''
        return self.coloured_name(colour) + "[OMP parallel do]"

    def gen_code(self, parent):

        # We're not doing nested parallelism so make sure that this
        # omp parallel do is not already within some parallel region
        from psyclone.psyGen import zero_reduction_variables
        self.validate_global_constraints()

        calls = self.reductions()
        zero_reduction_variables(calls, parent)
        private_str = ",".join(self._get_private_list())
        parent.add(DirectiveGen(parent, "omp", "begin", "parallel do",
                                "default(shared), private({0}), "
                                "schedule({1})".
                                format(private_str, self._omp_schedule) +
                                self._reduction_string()))
        for child in self.children:
            child.gen_code(parent)

        # make sure the directive occurs straight after the loop body
        position = parent.previous_loop()
        parent.add(DirectiveGen(parent, *self.end_string().split()),
                   position=["after", position])

    def begin_string(self):
        '''Returns the beginning statement of this directive, i.e.
        "omp do ...". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the beginning statement for this directive.
        :rtype: str

        '''
        private_str = ",".join(self._get_private_list())
        return ("omp parallel do default(shared), private({0}), "
                "schedule({1})".format(private_str, self._omp_schedule) +
                self._reduction_string())

    def end_string(self):
        '''
        :returns: the closing statement for this directive.
        :rtype: str
        '''
        # pylint: disable=no-self-use
        return "omp end parallel do"

    def update(self):
        '''
        Updates the fparser2 AST by inserting nodes for this OpenMP
        parallel do.

        :raises GenerationError: if the existing AST doesn't have the \
                                 correct structure to permit the insertion \
                                 of the OpenMP parallel do.
        '''
        # Since this is an OpenMP (parallel) do, it can only be applied
        # to a single loop.
        if len(self.dir_body.children) != 1:
            raise GenerationError(
                "An OpenMP PARALLEL DO can only be applied to a single loop "
                "but this Node has {0} children: {1}".
                format(len(self.dir_body.children), self.dir_body.children))

        self._add_region(
            start_text="parallel do default(shared), private({0}), "
            "schedule({1})".format(",".join(self._get_private_list()),
                                   self._omp_schedule),
            end_text="end parallel do")
