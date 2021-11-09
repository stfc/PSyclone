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
#         A. B. G. Chalk, STFC Daresbury Lab
#         I. Kavcic,    Met Office
#         C.M. Maynard, Met Office / University of Reading
#         J. Henrichs, Bureau of Meteorology
# -----------------------------------------------------------------------------

''' This module contains the implementation of the various OpenMP Directive
nodes.'''


from __future__ import absolute_import
import abc
import six

from psyclone.configuration import Config
from psyclone.core import AccessType, VariablesAccessInfo
from psyclone.errors import GenerationError, InternalError
from psyclone.f2pygen import (AssignGen, UseGen, DeclGen, DirectiveGen,
                              CommentGen)
from psyclone.psyir.nodes.directive import StandaloneDirective, \
    RegionDirective
from psyclone.psyir.nodes.loop import Loop

# OMP_OPERATOR_MAPPING is used to determine the operator to use in the
# reduction clause of an OpenMP directive.
OMP_OPERATOR_MAPPING = {AccessType.SUM: "+"}


@six.add_metaclass(abc.ABCMeta)
class OMPDirective():
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


@six.add_metaclass(abc.ABCMeta)
class OMPRegionDirective(OMPDirective, RegionDirective):
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


@six.add_metaclass(abc.ABCMeta)
class OMPStandaloneDirective(OMPDirective, StandaloneDirective):
    '''
    Base class for all OpenMP-related standalone directives

    '''


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

        super(OMPTaskwaitDirective, self).validate_global_constraints()

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
        # pylint: disable=no-self-use
        return "omp taskwait"


@six.add_metaclass(abc.ABCMeta)
class OMPSerialDirective(OMPRegionDirective):
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
                "{} must be inside an OMP parallel region but "
                "could not find an ancestor OMPParallelDirective node".format(
                    self._text_name))

        if self.ancestor(OMPSerialDirective):
            raise GenerationError(
                    "{} must not be inside another OpenMP "
                    "serial region".format(self._text_name))

        super(OMPSerialDirective, self).validate_global_constraints()


class OMPSingleDirective(OMPSerialDirective):
    '''
    Class representing an OpenMP SINGLE directive in the PSyIR.

    :param list children: List of Nodes that are children of this Node.
    :param parent: The Node in the AST that has this directive as a child.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`
    :param bool nowait: Argument describing whether this single should have \
        a nowait clause applied. Default value is False.

    '''
    # Textual description of the node
    _text_name = "OMPSingleDirective"

    def __init__(self, children=None, parent=None, nowait=False):

        self._nowait = nowait
        # Call the init method of the base class once we've stored
        # the nowait requirement
        super(OMPSingleDirective, self).__init__(children=children,
                                                 parent=parent)

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
        for child in self.children:
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
        result = "omp single"

        if self._nowait:
            result = result + " nowait"
        return result

    def end_string(self):
        '''Returns the end (or closing) statement of this directive, i.e.
        "omp end single". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the end statement for this directive.
        :rtype: str

        '''
        # pylint: disable=no-self-use
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
        # pylint: disable=no-self-use
        return "omp master"

    def end_string(self):
        '''Returns the end (or closing) statement of this directive, i.e.
        "omp end master". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the end statement for this directive.
        :rtype: str

        '''
        # pylint: disable=no-self-use
        return "omp end master"


class OMPParallelDirective(OMPRegionDirective):
    ''' Class representing an OpenMP Parallel directive. '''

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
        result = "omp parallel default(shared)"
        # TODO #514: not yet working with NEMO, so commented out for now
        # if not self._reprod:
        #     result += self._reduction_string()
        private_list = self._get_private_list()
        private_str = ",".join(private_list)

        if private_str:
            result = "{0}, private({1})".format(result, private_str)
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

    :param list children: list of Nodes that are children of this Node.
    :param parent: the Node in the AST that has this directive as a child.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`
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

    :raises GenerationError: if this OMPTaskloopDirective has both \
                             a grainsize and num_tasks value \
                             specified.
    '''
    # pylint: disable=too-many-arguments
    def __init__(self, children=None, parent=None, grainsize=None,
                 num_tasks=None, nogroup=False):
        self._grainsize = grainsize
        self._num_tasks = num_tasks
        self._nogroup = nogroup
        if self._grainsize is not None and self._num_tasks is not None:
            raise GenerationError(
                "OMPTaskloopDirective must not have both grainsize and "
                "numtasks clauses specified.")
        super(OMPTaskloopDirective, self).__init__(children=children,
                                                   parent=parent)

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
        '''
        # It is only at the point of code generation that we can check for
        # correctness (given that we don't mandate the order that a user
        # can apply transformations to the code). A taskloop
        # directive, we must have an OMPSerialDirective as an
        # ancestor back up the tree.
        if not self.ancestor(OMPSerialDirective):
            raise GenerationError(
                "OMPTaskloopDirective must be inside an OMP Serial region "
                "but could not find an ancestor node")

        super(OMPTaskloopDirective, self).validate_global_constraints()

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
            clause_list.append("grainsize({0})".format(self._grainsize))
        if self._num_tasks is not None:
            clause_list.append("num_tasks({0})".format(self._num_tasks))
        if self._nogroup:
            clause_list.append("nogroup")

        # Generate the string containing the required clauses
        extra_clauses = ", ".join(clause_list)

        parent.add(DirectiveGen(parent, "omp", "begin", "taskloop",
                                extra_clauses))

        for child in self.children:
            child.gen_code(parent)

        # make sure the directive occurs straight after the loop body
        position = parent.previous_loop()
        parent.add(DirectiveGen(parent, "omp", "end", "taskloop", ""),
                   position=["after", position])

    def begin_string(self):
        '''Returns the beginning statement of this directive, i.e.
        "omp do ...". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the beginning statement for this directive.
        :rtype: str

        '''
        extra_clauses = ""
        # Find the specified clauses
        clause_list = []
        if self._grainsize is not None:
            clause_list.append(" grainsize({0})".format(self._grainsize))
        if self._num_tasks is not None:
            clause_list.append(" num_tasks({0})".format(self._num_tasks))
        if self._nogroup:
            clause_list.append(" nogroup")

        # Generate the string containing the required clauses
        extra_clauses = ",".join(clause_list)

        return "omp taskloop" + extra_clauses

    def end_string(self):
        '''Returns the end (or closing) statement of this directive, i.e.
        "omp end do". The visitor is responsible for adding the
        correct directive beginning (e.g. "!$").

        :returns: the end statement for this directive.
        :rtype: str

        '''
        # pylint: disable=no-self-use
        return "omp end taskloop"


class OMPDoDirective(OMPRegionDirective):
    '''
    Class representing an OpenMP DO directive in the PSyIR.

    :param list children: list of Nodes that are children of this Node.
    :param parent: the Node in the AST that has this directive as a child.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`
    :param str omp_schedule: the OpenMP schedule to use.
    :param bool reprod: whether or not to generate code for run-reproducible \
                        OpenMP reductions.

    '''
    def __init__(self, children=None, parent=None, omp_schedule="static",
                 reprod=None):

        if reprod is None:
            self._reprod = Config.get().reproducible_reductions
        else:
            self._reprod = reprod

        self._omp_schedule = omp_schedule

        # Call the init method of the base class once we've stored
        # the OpenMP schedule
        super(OMPDoDirective, self).__init__(children=children,
                                             parent=parent)

    def node_str(self, colour=True):
        '''
        Returns the name of this node with (optional) control codes
        to generate coloured output in a terminal that supports it.

        :param bool colour: whether or not to include colour control codes.

        :returns: description of this node, possibly coloured.
        :rtype: str
        '''
        if self.reductions():
            reprod = "reprod={0}".format(self._reprod)
        else:
            reprod = ""
        return "{0}[{1}]".format(self.coloured_name(colour), reprod)

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
        # can apply transformations to the code). As a loop
        # directive, we must have an OMPParallelDirective as an ancestor
        # somewhere back up the tree.
        if not self.ancestor(OMPParallelDirective,
                             excluding=OMPParallelDoDirective):
            raise GenerationError(
                "OMPDoDirective must be inside an OMP parallel region but "
                "could not find an ancestor OMPParallelDirective node")

        self._validate_single_loop()

        super(OMPDoDirective, self).validate_global_constraints()

    def _validate_single_loop(self):
        '''
        Checks that this directive is only applied to a single Loop node.

        :raises GenerationError: if this directive has more than one child.
        :raises GenerationError: if the child of this directive is not a Loop.

        '''
        if len(self.dir_body.children) != 1:
            raise GenerationError(
                "An {0} can only be applied to a single loop "
                "but this Node has {1} children: {2}".
                format(type(self).__name__, len(self.dir_body.children),
                       self.dir_body.children))

        if not isinstance(self.dir_body[0], Loop):
            raise GenerationError(
                "An {0} can only be applied to a loop "
                "but this Node has a child of type '{1}'".format(
                    type(self).__name__, type(self.dir_body[0]).__name__))

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

        # As we're a loop we don't specify the scope
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

    def validate_global_constraints(self):
        '''
        Perform validation checks that can only be done at code-generation
        time.

        '''
        super(OMPParallelDoDirective, self).validate_global_constraints()

        self._validate_single_loop()


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
        # pylint: disable=no-self-use
        return "omp target"


class OMPLoopDirective(OMPRegionDirective):
    ''' Class for the !$OMP LOOP directive that specifies that the iterations
    of the associated loops may execute concurrently.

    :param int collapse: optional number of nested loops to collapse into a \
        single iteration space to parallelise. Defaults to None.
    '''

    def __init__(self, collapse=None, **kwargs):
        super(OMPLoopDirective, self).__init__(**kwargs)
        self._collapse = None
        self.collapse = collapse  # Use setter with error checking

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
        :param value: optional number of nested loop to collapse into a \
            single iteration space to parallelise. Defaults to None.
        :type value: int or NoneType.

        :raises TypeError: if the collapse value given is not an integer \
            or NoneType.
        :raises ValueError: if the collapse integer given is not positive.

        '''
        if value is not None and not isinstance(value, int):
            raise TypeError(
                "The OMPLoopDirective collapse clause must be a positive "
                "integer or None, but value '{0}' has been given."
                "".format(value))

        if value is not None and value <= 0:
            raise ValueError(
                "The OMPLoopDirective collapse clause must be a positive "
                "integer or None, but value '{0}' has been given."
                "".format(value))

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
            text += "[collapse={0}]".format(str(self._collapse))
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
            string += " collapse({0})".format(str(self._collapse))
        return string

    def validate_global_constraints(self):
        ''' Perform validation of those global constraints that can only be
        done at code-generation time.

        :raises GenerationError: if this OMPLoopDirective is not enclosed \
            within some OMPParallelDirective region.
        :raises GenerationError: if this OMPLoopDirective has more than one \
            child in its associated schedule.
        :raises GenerationError: if the schedule associated with this \
            OMPLoopDirective does not contain a Loop.
        :raises GenerationError: if this OMPLoopDirective has a collapse \
            clause but it doesn't have the expected number of nested Loops.

        '''
        if not self.ancestor(OMPParallelDirective):
            raise GenerationError(
                "OMPLoopDirective must have an OMPParallelDirective as an "
                "ancestor.")

        if len(self.dir_body.children) != 1:
            raise GenerationError(
                "OMPLoopDirective must have exactly one child in its "
                "associated schedule but found {0}.".format(
                    self.dir_body.children))

        if not isinstance(self.dir_body.children[0], Loop):
            raise GenerationError(
                "OMPLoopDirective must have a Loop as child of its associated "
                "schedule but found '{0}'.".format(self.dir_body.children[0]))

        # If there is a collapse clause, there must be as many immediately
        # nested loops as the collapse value
        if self._collapse:
            cursor = self.dir_body.children[0]
            for depth in range(self._collapse):
                if not isinstance(cursor, Loop):
                    raise GenerationError(
                        "OMPLoopDirective must have as many immediately nested"
                        " loops as the collapse clause specifies but '{0}' "
                        "has a collpase={1} and the nested statement at depth "
                        "{2} is a {3} rather than a Loop."
                        "".format(self, self._collapse, depth,
                                  type(cursor).__name__))
                cursor = cursor.loop_body.children[0]

        super(OMPLoopDirective, self).validate_global_constraints()


# For automatic API documentation generation
__all__ = ["OMPRegionDirective", "OMPParallelDirective", "OMPSingleDirective",
           "OMPMasterDirective", "OMPDoDirective", "OMPParallelDoDirective",
           "OMPSerialDirective", "OMPTaskloopDirective", "OMPTargetDirective",
           "OMPTaskwaitDirective", "OMPDirective", "OMPStandaloneDirective",
           "OMPLoopDirective"]
