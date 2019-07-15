# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2019, Science and Technology Facilities Council.
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
#        J. Henrichs, Bureau of Meteorology
# Modified I. Kavcic, Met Office

''' This module provides the various transformations that can be applied to
    PSyIR nodes. There are both general and API-specific transformation
    classes in this module where the latter typically apply API-specific
    checks before calling the base class for the actual transformation. '''

from __future__ import absolute_import, print_function
import abc
import six
from psyclone.psyGen import Transformation, InternalError, Schedule
from psyclone.configuration import Config
from psyclone.undoredo import Memento
from psyclone.dynamo0p3 import VALID_ANY_SPACE_NAMES

VALID_OMP_SCHEDULES = ["runtime", "static", "dynamic", "guided", "auto"]


class TransformationError(Exception):
    ''' Provides a PSyclone-specific error class for errors found during
        code transformation operations. '''

    def __init__(self, value):
        Exception.__init__(self, value)
        self.value = "Transformation Error: "+value

    def __str__(self):
        return repr(self.value)


@six.add_metaclass(abc.ABCMeta)
class RegionTrans(Transformation):
    '''
    This abstract class is a base class for all transformations that act
    on a list of nodes. It gives access to a _validate function that
    makes sure that the nodes in the list are in the same order as in
    the original AST, no node is duplicated, and that all nodes have
    the same parent. We also check that all nodes to be enclosed are
    valid for this transformation - this requires that the sub-class
    populate the `valid_node_types` tuple.

    '''
    # The types of Node that we support within this region. Must be
    # populated by sub-class.
    valid_node_types = ()

    # Avoid pylint warning about abstract functions (apply, name) not
    # overwritten:
    # pylint: disable=abstract-method,arguments-differ

    def _validate(self, node_list):
        '''Test if the nodes in node_list are in the original order.

        :param list node_list: List of PSyIR nodes.
        :raises TransformationError: If the nodes in the list are not \
                in the original order in which they are in the AST, \
                a node is duplicated or the nodes have different parents.
        :raises TransformationError: if any of the nodes to be enclosed in \
                the region are of an unsupported type.
        :raises TransformationError: if the condition part of an IfBlock \
                                     is erroneously included in the region.

        '''
        from psyclone.psyGen import IfBlock, Literal, Reference
        node_parent = node_list[0].parent
        prev_position = -1
        for child in node_list:
            if child.parent is not node_parent:
                raise TransformationError(
                    "Error in {0} transformation: supplied nodes "
                    "are not children of the same parent."
                    .format(self.name))
            if prev_position >= 0 and prev_position+1 != child.position:
                raise TransformationError(
                    "Children are not consecutive children of one parent: "
                    "child '{0}' has position {1}, but previous child had "
                    "position {2}."
                    .format(str(child), child.position, prev_position))
            prev_position = child.position

        # Check that the proposed region contains only supported node types
        for child in node_list:
            flat_list = [item for item in child.walk([child], object)
                         if not isinstance(item, Schedule)]
            for item in flat_list:
                if not isinstance(item, self.valid_node_types):
                    # TODO: Quickfix added, look what is the best solution
                    # for this, are basic PSyIR node going to appear.
                    if isinstance(item, (Literal, Reference)):
                        continue
                    raise TransformationError(
                        "Nodes of type '{0}' cannot be enclosed by a {1} "
                        "transformation".format(type(item), self.name))

        # Sanity check that we've not been passed the condition part of
        # an If statement (which is child 0)
        if isinstance(node_parent, IfBlock):
            if node_parent.children[0] in node_list:
                raise TransformationError(
                    "Cannot apply transformation to the conditional expression"
                    " (first child) of an If/Case statement. Error in "
                    "transformation script.")

            # Check that we've not been supplied with both the if and
            # else clauses of an IfBlock as we can't put them both in
            # a region without their parent.
            if len(node_list) > 1:
                raise TransformationError(
                    "Cannot enclose both the if- and else- clauses of an "
                    "IfBlock by a {0} transformation. Apply the "
                    "transformation to the IfBlock node instead.".
                    format(self.name))


# =============================================================================
def check_intergrid(node):
    '''
    Utility function to check that the supplied node does not have
    an intergrid kernel as a child.

    This is used to ensure that we reject any attempt to apply
    transformations to loops containing inter-grid kernels. (This restriction
    will be lifted in Issue #134 and this routine can then be removed.)

    # TODO remove this routine once #134 is complete.

    :param node: The PSyIR node to check.
    :type node: :py:class:`psyGen.Node`

    :raises TransformationError: if the supplied node has an inter-grid
                                 kernel as a child
    '''
    if not node.children:
        return
    from psyclone.dynamo0p3 import DynKern
    child_kernels = node.walk(node.children, DynKern)
    for kern in child_kernels:
        if kern.is_intergrid:
            raise TransformationError(
                "Transformations cannot currently be applied to nodes which "
                "have inter-grid kernels as children and {0} is such a "
                "kernel.".format(kern.name))


class LoopFuseTrans(Transformation):
    ''' Provides a loop-fuse transformation.
        For example:

        >>> from psyclone.parse.algorithm import parse
        >>> from psyclone.psyGen import PSyFactory
        >>> ast,invokeInfo=parse("dynamo.F90")
        >>> psy=PSyFactory("dynamo0.1").create(invokeInfo)
        >>> schedule=psy.invokes.get('invoke_v3_kernel_type').schedule
        >>> schedule.view()
        >>>
        >>> from psyclone.transformations import LoopFuseTrans
        >>> trans=LoopFuseTrans()
        >>> new_schedule,memento=trans.apply(schedule.children[0],
                                             schedule.children[1])
        >>> new_schedule.view()
    '''

    def __str__(self):
        return "Fuse two adjacent loops together"

    @property
    def name(self):
        ''' Returns the name of this transformation as a string.'''
        return "LoopFuse"

    def _validate(self, node1, node2):
        '''Perform various checks to ensure that it is valid to apply the
        LoopFuseTrans transformation to the supplied nodes

        :param node1: the first node we are checking
        :type node1: :py:class:`psyclone.psyGen.Node`
        :param node2: the second node we are checking
        :type node2: :py:class:`psyclone.psyGen.Node`
        :raises TransformationError: if one or both of the nodes is/are not a
        :py:class:`psyclone.psyGen.Loop`
        :raises TransformationError: if the nodes do not have the same parent
        :raises TransformationError: if the nodes are not next to each
        other in the tree
        :raises TransformationError: if the
        :py:class:`psyclone.psyGen.Loop`s do not have the same
        iteration space
        '''

        # Check that the supplied Node is a Loop
        from psyclone.psyGen import Loop
        if not isinstance(node1, Loop) or not isinstance(node2, Loop):
            raise TransformationError("Error in LoopFuse transformation. "
                                      "At least one of the nodes is not "
                                      "a loop")

            # If they are loops, they must be fully-formed.
            if len(node1.children) != 4:
                raise TransformationError("")

            if len(node2.children) != 4:
                raise TransformationError("")

        # check loop1 and loop2 have the same parent
        if not node1.sameParent(node2):
            raise TransformationError("Error in LoopFuse transformation. "
                                      "loops do not have the same parent")
        # check node1 and node2 are next to each other
        if abs(node1.position-node2.position) != 1:
            raise TransformationError("Error in LoopFuse transformation. "
                                      "nodes are not siblings who are "
                                      "next to each other")
        # Check iteration space is the same
        # TODO: Do I have to check the start/stop/step_expr now???
        if node1.iteration_space != node2.iteration_space:
            raise TransformationError("Error in LoopFuse transformation. "
                                      "Loops do not have the same "
                                      "iteration space")

    def apply(self, node1, node2):
        '''Fuse the loops represented by :py:obj:`node1` and
        :py:obj:`node2`.'''

        self._validate(node1, node2)

        schedule = node1.root

        # create a memento of the schedule and the proposed transformation
        keep = Memento(schedule, self, [node1, node2])

        # add loop contents of node2 to node1
        node1.loop_body._children.extend(node2.loop_body)

        # change the parent of the loop contents of node2 to node1
        for child in node2.loop_body:
            child.parent = node1.children[3]

        # remove node2
        node2.parent.children.remove(node2)

        return schedule, keep


class GOceanLoopFuseTrans(LoopFuseTrans):
    ''' Performs error checking (that the loops are over the same grid-point
        type) before calling the :py:meth:`LoopFuseTrans.apply` method of the
        :py:class:`base class <LoopFuseTrans>` in order to fuse two
        GOcean loops. '''

    def __str__(self):
        return ("Fuse two adjacent loops together with GOcean-specific "
                "validity checks")

    @property
    def name(self):
        ''' Returns the name of this transformation as a string.'''
        return "GOceanLoopFuse"

    def apply(self, node1, node2):
        '''Fuse the two GOcean loops represented by :py:obj:`node1` and
        :py:obj:`node2`.

        :param node1: A node representing a GOLoop.
        :type node1: :py:class:`psyclone.gocean1p0.GOLoop`
        :param node2: A node representing a GOLoop.
        :type node2: :py:class:`psyclone.gocean1p0.GOLoop`
        :raises TransformationError: if the supplied node2 can not be fused,
            e.g. not all nodes are loops, don't have the same parent, are not
            next to each other or have different iteration spaces.
        '''

        LoopFuseTrans._validate(self, node1, node2)

        try:
            if node1.field_space != node2.field_space:
                raise TransformationError(
                    "Error in GOceanLoopFuse transformation. Cannot "
                    "fuse loops that are over different grid-point types: "
                    "{0} {1}".format(node1.field_space,
                                     node2.field_space))
            return LoopFuseTrans.apply(self, node1, node2)
        except TransformationError as err:
            raise err
        except Exception as err:
            raise TransformationError("Unexpected exception: {0}".
                                      format(err))


class DynamoLoopFuseTrans(LoopFuseTrans):
    '''Performs error checking before calling the
        :py:meth:`~LoopFuseTrans.apply` method of the
        :py:class:`base class <LoopFuseTrans>` in order
        to fuse two Dynamo loops.

    '''

    def __str__(self):
        return ("Fuse two adjacent loops together with Dynamo-specific "
                "validity checks")

    @property
    def name(self):
        ''' Returns the name of this transformation as a string.'''
        return "DynamoLoopFuse"

    def apply(self, node1, node2, same_space=False):
        '''
        Fuse the two Dynamo loops represented by :py:obj:`node1` and
        :py:obj:`node2`. The optional same_space flag asserts that an
        unknown iteration space (i.e. any_space) matches the other
        iteration space. This is set at the users own risk.

        :param node1: First Loop to fuse.
        :type node1: :py:class:`psyclone.dynamo0p3.DynLoop`
        :param node2: Second Loop to fuse.
        :type node2: :py:class:`psyclone.dynamo0p3.DynLoop`
        :returns: two-tuple of modified Schedule and Memento
        :rtype: :py:class:`psyclone.psyGen.Schedule`, \
                :py:class:`psyclone.undoredo.Memento`
        :raises TransformationError: if either of the supplied loops contains \
                                     an inter-grid kernel.
        '''
        LoopFuseTrans._validate(self, node1, node2)

        # Check that we don't have an inter-grid kernel
        check_intergrid(node1)
        check_intergrid(node2)

        from psyclone.dynamo0p3 import VALID_FUNCTION_SPACES
        try:
            if node1.field_space.orig_name in VALID_FUNCTION_SPACES and \
               node2.field_space.orig_name in VALID_FUNCTION_SPACES:
                if node1.field_space.orig_name != node2.field_space.orig_name:
                    if same_space:
                        info = (
                            " Note, the same_space flag was set, but "
                            "does not apply because neither field is "
                            "ANY_SPACE.")
                    else:
                        info = ""
                    raise TransformationError(
                        "Error in DynamoLoopFuse transformation. "
                        "Cannot fuse loops that are over different spaces: "
                        "{0} {1}.{2}".format(node1.field_space.orig_name,
                                             node2.field_space.orig_name,
                                             info))
            else:  # one or more of the function spaces is any_space
                if not same_space:
                    raise TransformationError(
                        "DynamoLoopFuseTrans. One or more of the iteration "
                        "spaces is unknown ('any_space') so loop fusion might "
                        "be invalid. If you know the spaces are the same then "
                        "please set the 'same_space' optional argument to "
                        "True.")
            if node1.upper_bound_name != node2.upper_bound_name:
                raise TransformationError(
                    "Error in DynamoLoopFuse transformation. The upper bound "
                    "names are not the same. Found '{0}' and '{1}'".
                    format(node1.upper_bound_name, node2.upper_bound_name))
            if node1.upper_bound_halo_depth != node2.upper_bound_halo_depth:
                raise TransformationError(
                    "Error in DynamoLoopFuse transformation. The halo-depth "
                    "indices are not the same. Found '{0}' and '{1}'".
                    format(node1.upper_bound_halo_depth,
                           node2.upper_bound_halo_depth))
            from psyclone.psyGen import MAPPING_SCALARS
            from psyclone.core.access_type import AccessType
            arg_types = MAPPING_SCALARS.values()
            all_reductions = AccessType.get_valid_reduction_modes()
            node1_red_args = node1.args_filter(arg_types=arg_types,
                                               arg_accesses=all_reductions)
            node2_red_args = node2.args_filter(arg_types=arg_types,
                                               arg_accesses=all_reductions)

            if node1_red_args and node2_red_args:
                raise TransformationError(
                    "Error in DynamoLoopFuse transformation. "
                    "Cannot fuse loops when each loop already "
                    "contains a reduction")

            if node1_red_args:
                for reduction_arg in node1_red_args:
                    other_args = node2.args_filter()
                    for arg in other_args:
                        if reduction_arg.name == arg.name:
                            raise TransformationError(
                                "Error in DynamoLoopFuse transformation. "
                                "Cannot fuse loops as the first loop "
                                "has a reduction and the second loop "
                                "reads the result of the reduction")

            return LoopFuseTrans.apply(self, node1, node2)
        except TransformationError as err:
            raise err
        except Exception as err:
            raise TransformationError("Unexpected exception: {0}".
                                      format(err))


@six.add_metaclass(abc.ABCMeta)
class ParallelLoopTrans(Transformation):

    '''
    Adds an orphaned directive to a loop indicating that it should be
    parallelised. i.e. the directive must be inside the scope of some
    other Parallel REGION. This condition is tested at
    code-generation time.

    '''
    @abc.abstractmethod
    def __str__(self):
        return  # pragma: no cover

    @abc.abstractproperty
    def name(self):
        '''
        :returns: the name of this transformation as a string.
        :rtype: str
        '''

    @abc.abstractmethod
    def _directive(self, parent, children, collapse=None):
        '''
        Returns the directive object to insert into the Schedule.
        Must be implemented by sub-class.

        :param parent: the parent of this Directive in the Schedule.
        :type parent: :py:class:`psyclone.psyGen.Node`.
        :param children: list of nodes that will be children of this Directive.
        :type children: list of :py:class:`psyclone.psyGen.Node`.
        :param int collapse: the number of tightly-nested loops to which \
                             this directive applies or None.
        :returns: the new Directive node.
        :rtype: sub-class of :py:class:`psyclone.psyGen.Directive`.
        '''

    def _validate(self, node, collapse=None):
        '''
        Perform validation checks before applying the transformation

        :param node: the node we are checking.
        :type node: :py:class:`psyclone.psyGen.Node`.
        :param int collapse: number of nested loops to collapse or None.
        :raises TransformationError: if the node is not a \
        :py:class:`psyclone.psyGen.Loop`
        :raises TransformationError: if the \
        :py:class:`psyclone.psyGen.Loop` loop iterates over colours

        '''
        # Check that the supplied node is a Loop
        from psyclone.psyGen import Loop
        if not isinstance(node, Loop):
            raise TransformationError(
                "Cannot apply a parallel-loop directive to something that is "
                "not a loop")
        # Check we are not a sequential loop
        # TODO add a list of loop types that are sequential
        if node.loop_type == 'colours':
            raise TransformationError("Error in "+self.name+" transformation. "
                                      "The target loop is over colours and "
                                      "must be computed serially.")
        # If 'collapse' is specified, check that it is an int and that the
        # loop nest has at least that number of loops in it
        if collapse:
            if not isinstance(collapse, int):
                raise TransformationError(
                    "The 'collapse' argument must be an integer but got an "
                    "object of type {0}".format(type(collapse)))
            if collapse < 2:
                raise TransformationError(
                    "It only makes sense to collapse 2 or more loops "
                    "but got a value of {0}".format(collapse))
            # Count the number of loops in the loop nest
            loop_count = 0
            cnode = node
            while isinstance(cnode, Loop):
                loop_count += 1
                # Loops must be tightly nested (no intervening statements)
                cnode = cnode.loop_body[0]
            if collapse > loop_count:
                raise TransformationError(
                    "Cannot apply COLLAPSE({0}) clause to a loop nest "
                    "containing only {1} loops".format(collapse, loop_count))

    def apply(self, node, collapse=None):
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

        :param node: the supplied node to which we will apply the \
                     Loop transformation.
        :type node: :py:class:`psyclone.psyGen.Node`.
        :param int collapse: number of loops to collapse into single \
                             iteration space or None.
        :returns: (:py:class:`psyclone.psyGen.Schedule`, \
                   :py:class:`psyclone.undoredo.Memento`)

        '''
        self._validate(node, collapse)

        schedule = node.root

        # create a memento of the schedule and the proposed
        # transformation
        keep = Memento(schedule, self, [node, collapse])

        # keep a reference to the node's original parent and its index as these
        # are required and will change when we change the node's location
        node_parent = node.parent
        node_position = node.position

        # Add our orphan loop directive setting its parent to the node's
        # parent and its children to the node. This calls down to the sub-class
        # to get the type of directive we require.
        directive = self._directive(node_parent, [node], collapse)

        # Add the loop directive as a child of the node's parent
        node_parent.addchild(directive, index=node_position)

        # Change the node's parent to be the loop directive.
        node.parent = directive

        # Remove the reference to the loop from the original parent.
        node_parent.children.remove(node)

        return schedule, keep


class OMPLoopTrans(ParallelLoopTrans):

    '''
    Adds an orphaned OpenMP directive to a loop. i.e. the directive
    must be inside the scope of some other OMP Parallel
    REGION. This condition is tested at code-generation time. The
    optional 'reprod' argument in the apply method decides whether
    standard OpenMP reduction support is to be used (which is not
    reproducible) or whether a manual reproducible reproduction is
    to be used.

    :param str omp_schedule: The OpenMP schedule to use.

    For example:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.parse.utils import ParseError
    >>> from psyclone.psyGen import PSyFactory, GenerationError
    >>> api = "gocean1.0"
    >>> filename = "nemolite2d_alg.f90"
    >>> ast, invokeInfo = parse(filename, api=api, invoke_name="invoke")
    >>> psy = PSyFactory(api).create(invokeInfo)
    >>> print psy.invokes.names
    >>>
    >>> from psyclone.psyGen import TransInfo
    >>> t = TransInfo()
    >>> ltrans = t.get_trans_name('OMPLoopTrans')
    >>> rtrans = t.get_trans_name('OMPParallelTrans')
    >>>
    >>> schedule = psy.invokes.get('invoke_0').schedule
    >>> schedule.view()
    >>> new_schedule = schedule
    >>>
    # Apply the OpenMP Loop transformation to *every* loop
    # in the schedule
    >>> for child in schedule.children:
    >>>     newschedule, memento = ltrans.apply(child, reprod=True)
    >>>     schedule = newschedule
    >>>
    # Enclose all of these loops within a single OpenMP
    # PARALLEL region
    >>> rtrans.omp_schedule("dynamic,1")
    >>> newschedule, memento = rtrans.apply(schedule.children)
    >>>
    >>>

    '''
    def __init__(self, omp_schedule="static"):
        # Whether or not to generate code for (run-to-run on n threads)
        # reproducible OpenMP reductions. This setting can be overridden
        # via the `reprod` argument to the apply() method.
        self._reprod = Config.get().reproducible_reductions

        self._omp_schedule = ""
        # Although we create the _omp_schedule attribute above (so that
        # pylint doesn't complain), we actually set its value using
        # the setter method in order to make use of the latter's error
        # checking.
        self.omp_schedule = omp_schedule

        super(OMPLoopTrans, self).__init__()

    def __str__(self):
        return "Adds an 'OpenMP DO' directive to a loop"

    @property
    def name(self):
        ''' Returns the name of this transformation as a string.'''
        return "OMPLoopTrans"

    @property
    def omp_schedule(self):
        ''' Returns the OpenMP schedule that will be specified by
            this transformation. The default schedule is 'static'.'''
        return self._omp_schedule

    @omp_schedule.setter
    def omp_schedule(self, value):
        ''' Sets the OpenMP schedule that will be specified by
            this transformation. Checks that the string supplied in
            :py:obj:`value` is a recognised OpenMP schedule. '''

        # Some schedules have an optional chunk size following a ','
        value_parts = value.split(',')
        if value_parts[0].lower() not in VALID_OMP_SCHEDULES:
            raise TransformationError("Valid OpenMP schedules are {0} "
                                      "but got {1}".
                                      format(VALID_OMP_SCHEDULES,
                                             value_parts[0]))
        if len(value_parts) > 1:
            if value_parts[0] == "auto":
                raise TransformationError("Cannot specify a chunk size "
                                          "when using an OpenMP schedule"
                                          " of 'auto'")
            elif value_parts[1].strip() == "":
                raise TransformationError("Supplied OpenMP schedule '{0}'"
                                          " has missing chunk-size.".
                                          format(value))

        self._omp_schedule = value

    def _directive(self, parent, children, collapse=None):
        '''
        Creates the type of directive needed for this sub-class of
        transformation.

        :param parent: The Node that will be the parent of the created \
                       directive Node.
        :param children: List of Nodes that will be the children of \
                         the created directive.
        :type children: list of :py:class:`psyclone.psyGen.Node`
        :param int collapse: currently un-used but required to keep \
                             interface the same as in base class.
        :returns: the new node representing the directive in the AST
        :rtype: :py:class:`psyclone.psyGen.OMPDoDirective`
        :raises NotImplementedError: if a collapse argument is supplied
        '''
        if collapse:
            raise NotImplementedError(
                "The COLLAPSE clause is not yet supported for '!$omp do' "
                "directives.")

        from psyclone.psyGen import OMPDoDirective
        _directive = OMPDoDirective(parent=parent,
                                    children=children,
                                    omp_schedule=self.omp_schedule,
                                    reprod=self._reprod)
        return _directive

    def apply(self, node, reprod=None):
        '''Apply the OMPLoopTrans transformation to the specified node in a
        Schedule. This node must be a Loop since this transformation
        corresponds to wrapping the generated code with directives like so:

        .. code-block:: fortran

          !$OMP DO
          do ...
             ...
          end do
          !$OMP END DO

        At code-generation time (when
        :py:meth:`OMPLoopDirective.gen_code` is called), this node must be
        within (i.e. a child of) an OpenMP PARALLEL region.

        The optional reprod argument will cause a reproducible
        reduction to be generated if it is set to True, otherwise the default
        value (as read from the psyclone.cfg file) will be used. Note,
        reproducible in this case means obtaining the same results
        with the same number of OpenMP threads, not for different
        numbers of OpenMP threads.

        :param node: the supplied node to which we will apply the \
        OMPLoopTrans transformation
        :type node: :py:class:`psyclone.psyGen.Node`
        :param reprod: optional argument to determine whether to \
        generate reproducible OpenMP reductions (True) or not \
        (False). The default value is None which will cause PSyclone \
        to look up a default value
        :type reprod: Boolean or None
        :returns: (:py:class:`psyclone.psyGen.Schedule`, \
        :py:class:`psyclone.undoredo.Memento`)

        '''
        if reprod:
            self._reprod = reprod

        return super(OMPLoopTrans, self).apply(node)


class ACCLoopTrans(ParallelLoopTrans):
    '''
    Adds an OpenACC loop directive to a loop. This directive must be within
    the scope of some OpenACC Parallel region (at code-generation time).

    For example:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.parse.utils import ParseError
    >>> from psyclone.psyGen import PSyFactory, GenerationError
    >>> api = "gocean1.0"
    >>> filename = "nemolite2d_alg.f90"
    >>> ast, invokeInfo = parse(filename, api=api, invoke_name="invoke")
    >>> psy = PSyFactory(api).create(invokeInfo)
    >>>
    >>> from psyclone.psyGen import TransInfo
    >>> t = TransInfo()
    >>> ltrans = t.get_trans_name('ACCLoopTrans')
    >>> rtrans = t.get_trans_name('ACCParallelTrans')
    >>>
    >>> schedule = psy.invokes.get('invoke_0').schedule
    >>> schedule.view()
    >>> new_schedule = schedule
    >>>
    # Apply the OpenACC Loop transformation to *every* loop
    # in the schedule
    >>> for child in schedule.children:
    >>>     newschedule, memento = ltrans.apply(child, reprod=True)
    >>>     schedule = newschedule
    >>>
    # Enclose all of these loops within a single OpenACC
    # PARALLEL region
    >>> rtrans.omp_schedule("dynamic,1")
    >>> newschedule, memento = rtrans.apply(schedule.children)
    >>>

    '''
    def __init__(self):
        # Whether to add the "independent" clause
        # to the loop directive.
        self._independent = True
        self._sequential = False
        super(ACCLoopTrans, self).__init__()

    def __str__(self):
        return "Adds an 'OpenACC loop' directive to a loop"

    @property
    def name(self):
        ''' Returns the name of this transformation as a string.'''
        return "ACCLoopTrans"

    def _directive(self, parent, children, collapse=None):
        '''
        Creates the ACCLoopDirective needed by this sub-class of
        transformation.

        :param parent: the parent Node of the new directive Node.
        :type parent: :py:class:`psyclone.psyGen.Node`.
        :param children: list of child nodes of the new directive Node.
        :type children: list of :py:class:`psyclone.psyGen.Node`.
        :param int collapse: number of nested loops to collapse or None if \
                             no collapse attribute is required.
        '''
        from psyclone.psyGen import ACCLoopDirective
        directive = ACCLoopDirective(parent=parent,
                                     children=children,
                                     collapse=collapse,
                                     independent=self._independent,
                                     sequential=self._sequential)
        return directive

    def _validate(self, node, collapse=None):
        '''
        Does OpenACC-specific validation checks before calling the
        _validate method of the base class.

        :param node: the proposed target of the !$acc loop directive.
        :type node: :py:class:`psyclone.psyGen.Node`.
        :param int collapse: number of loops to collapse or None.
        :raises NotImplementedError: if an API other than GOcean 1.0 is \
                                     being used.
        '''
        from psyclone.gocean1p0 import GOInvokeSchedule
        from psyclone.nemo import NemoInvokeSchedule
        sched = node.root
        if not isinstance(sched, (GOInvokeSchedule, NemoInvokeSchedule)):
            raise NotImplementedError(
                "OpenACC loop transformations are currently only supported "
                "for the gocean 1.0 and nemo APIs")
        super(ACCLoopTrans, self)._validate(node, collapse)

    def apply(self, node, collapse=None, independent=True, sequential=False):
        '''
        Apply the ACCLoop transformation to the specified node in a
        GOInvokeSchedule. This node must be a Loop since this transformation
        corresponds to inserting a directive immediately before a loop, e.g.:

        .. code-block:: fortran

          !$ACC LOOP
          do ...
             ...
          end do

        At code-generation time (when
        :py:meth:`psyclone.psyGen.ACCLoopDirective.gen_code` is called), this
        node must be within (i.e. a child of) a PARALLEL region.

        :param node: the supplied node to which we will apply the \
                     Loop transformation.
        :type node: :py:class:`psyclone.psyGen.Loop`.
        :param int collapse: number of loops to collapse into single \
                             iteration space or None.
        :param bool independent: whether to add the "independent" clause to \
                                 the directive (not strictly necessary within \
                                 PARALLEL regions).
        :returns: (:py:class:`psyclone.psyGen.GOInvokeSchedule`, \
                  :py:class:`psyclone.undoredo.Memento`)
        '''
        # Store sub-class specific options. These are used when
        # creating the directive (in the _directive() method).
        self._independent = independent
        self._sequential = sequential
        # Call the apply() method of the base class
        return super(ACCLoopTrans, self).apply(node, collapse)


class OMPParallelLoopTrans(OMPLoopTrans):

    ''' Adds an OpenMP PARALLEL DO directive to a loop.

        For example:

        >>> from psyclone.parse.algorithm import parse
        >>> from psyclone.psyGen import PSyFactory
        >>> ast, invokeInfo = parse("dynamo.F90")
        >>> psy = PSyFactory("dynamo0.1").create(invokeInfo)
        >>> schedule = psy.invokes.get('invoke_v3_kernel_type').schedule
        >>> schedule.view()
        >>>
        >>> from psyclone.transformations import OMPParallelLoopTrans
        >>> trans = OMPParallelLoopTrans()
        >>> new_schedule, memento = trans.apply(schedule.children[0])
        >>> new_schedule.view()

    '''

    @property
    def name(self):
        ''' Returns the name of this transformation as a string.'''
        return "OMPParallelLoopTrans"

    def __str__(self):
        return "Add an 'OpenMP PARALLEL DO' directive with no validity checks"

    def _validate(self, node):
        ''' validity checks for input arguments '''
        # Check that the supplied Node is a Loop
        from psyclone.psyGen import Loop
        if not isinstance(node, Loop):
            raise TransformationError("Error in {0} transformation. The "
                                      "node is not a loop.".format(self.name))

        # Check we are not a sequential loop
        if node.loop_type == 'colours':
            raise TransformationError("Error in "+self.name+" transformation. "
                                      "The requested loop is over colours and "
                                      "must be computed serially.")

    def apply(self, node):
        ''' Apply an OMPParallelLoop Transformation to the supplied node
        (which must be a Loop). In the generated code this corresponds to
        wrapping the Loop with directives:

        .. code-block:: fortran

          !$OMP PARALLEL DO ...
          do ...
            ...
          end do
          !$OMP END PARALLEL DO

        :param node: the node (loop) to which to apply the transformation.
        :type node: :py:class:`psyclone.f2pygen.DoGen`
        :returns: Two-tuple of transformed schedule and a record of the \
                  transformation.
        :rtype: (:py:class:`psyclone.psyGen.Schedule, \
                 :py:class:`psyclone.undoredo.Memento`)
        '''
        self._validate(node)

        schedule = node.root
        # create a memento of the schedule and the proposed transformation
        keep = Memento(schedule, self, [node])

        # keep a reference to the node's original parent and its index as these
        # are required and will change when we change the node's location
        node_parent = node.parent
        node_position = node.position

        # add our OpenMP loop directive setting its parent to the node's
        # parent and its children to the node
        from psyclone.psyGen import OMPParallelDoDirective
        directive = OMPParallelDoDirective(parent=node_parent,
                                           children=[node],
                                           omp_schedule=self.omp_schedule)

        # add the OpenMP loop directive as a child of the node's parent
        node_parent.addchild(directive, index=node_position)

        # change the node's parent to be the loop directive
        node.parent = directive

        # remove the original loop
        node_parent.children.remove(node)

        return schedule, keep


class DynamoOMPParallelLoopTrans(OMPParallelLoopTrans):

    ''' Dynamo-specific OpenMP loop transformation. Adds Dynamo specific
        validity checks. Actual transformation is done by the
        :py:class:`base class <OMPParallelLoopTrans>`.

    '''

    @property
    def name(self):
        ''' Returns the name of this transformation as a string.'''
        return "DynamoOMPParallelLoopTrans"

    def __str__(self):
        return "Add an OpenMP Parallel Do directive to a Dynamo loop"

    def apply(self, node):

        '''Perform Dynamo specific loop validity checks then call the
        :py:meth:`~OMPParallelLoopTrans.apply` method of the
        :py:class:`base class <OMPParallelLoopTrans>`.

        :param node: the Node in the Schedule to check
        :type node: :py:class:`psyclone.psyGen.Node`

        :raise TransformationError: if the associated loop requires \
        colouring.

        '''
        OMPParallelLoopTrans._validate(self, node)

        # If the loop is not already coloured then check whether or not
        # it should be. If the field space is discontinuous then we don't
        # need to worry about colouring.
        from psyclone.dynamo0p3 import DISCONTINUOUS_FUNCTION_SPACES
        if node.field_space.orig_name not in DISCONTINUOUS_FUNCTION_SPACES:
            if node.loop_type is not 'colour' and node.has_inc_arg():
                raise TransformationError(
                    "Error in {0} transformation. The kernel has an "
                    "argument with INC access. Colouring is required.".
                    format(self.name))

        return OMPParallelLoopTrans.apply(self, node)


class GOceanOMPParallelLoopTrans(OMPParallelLoopTrans):

    '''GOcean specific OpenMP Do loop transformation. Adds GOcean
       specific validity checks (that supplied Loop is an inner or outer
       loop). Actual transformation is done by
       :py:class:`base class <OMPParallelLoopTrans>`.

       :param omp_schedule: The omp schedule to be created. Must be one of
           'runtime', 'static', 'dynamic', 'guided' or 'auto'.
       '''

    @property
    def name(self):
        ''' Returns the name of this transformation as a string.'''
        return "GOceanOMPParallelLoopTrans"

    def __str__(self):
        return "Add an OpenMP Parallel Do directive to a GOcean loop"

    def apply(self, node):
        ''' Perform GOcean-specific loop validity checks then call
        :py:meth:`OMPParallelLoopTrans.apply`.

        :param node: A Loop node from an AST.
        :type node: :py:class:`psyclone.psyGen.Loop`
        :raises TransformationError: if the supplied node is not an inner or
            outer loop.

        '''

        OMPParallelLoopTrans._validate(self, node)

        # Check we are either an inner or outer loop
        if node.loop_type not in ["inner", "outer"]:
            raise TransformationError(
                "Error in "+self.name+" transformation.  The requested loop"
                " is not of type inner or outer.")

        return OMPParallelLoopTrans.apply(self, node)


class Dynamo0p3OMPLoopTrans(OMPLoopTrans):

    ''' Dynamo 0.3 specific orphan OpenMP loop transformation. Adds
    Dynamo-specific validity checks. Actual transformation is done by
    :py:class:`base class <OMPLoopTrans>`.

    '''

    @property
    def name(self):
        ''' Returns the name of this transformation as a string.'''
        return "Dynamo0p3OMPLoopTrans"

    def __str__(self):
        return "Add an OpenMP DO directive to a Dynamo 0.3 loop"

    def apply(self, node, reprod=None):
        '''Perform Dynamo 0.3 specific loop validity checks then call
        :py:meth:`OMPLoopTrans.apply`.

        :param node: the Node in the Schedule to check
        :type node: :py:class:`psyclone.psyGen.Node`
        :param reprod: if reproducible reductions should be used.
        :type reprod: bool or None (default, which indicates to use the \
              default from the config file).

        :raise TransformationError: if an OMP loop transform would create \
                incorrect code.
        '''

        if reprod is None:
            reprod = Config.get().reproducible_reductions

        OMPLoopTrans._validate(self, node)

        # If the loop is not already coloured then check whether or not
        # it should be
        if node.loop_type is not 'colour' and node.has_inc_arg():
            raise TransformationError(
                "Error in {0} transformation. The kernel has an argument"
                " with INC access. Colouring is required.".
                format(self.name))

        return OMPLoopTrans.apply(self, node, reprod=reprod)


class GOceanOMPLoopTrans(OMPLoopTrans):

    ''' GOcean-specific orphan OpenMP loop transformation. Adds GOcean
        specific validity checks (that the node is either an inner or outer
        Loop). Actual transformation is done by
        :py:class:`base class <OMPLoopTrans>`.

        :param omp_schedule: The omp schedule to be created. Must be one of
            'runtime', 'static', 'dynamic', 'guided' or 'auto'.

        '''

    @property
    def name(self):
        ''' Returns the name of this transformation as a string.'''
        return "GOceanOMPLoopTrans"

    def __str__(self):
        return "Add an OpenMP DO directive to a GOcean loop"

    def apply(self, node):
        '''Perform GOcean specific loop validity checks then call
        :py:meth:`OMPLoopTrans.apply`.

        :param node: The loop to parallelise using OMP Do.
        :type node: :py:class:`psyclone.psyGen.Loop`.

        '''
        # check node is a loop. Although this is not GOcean specific
        # it is required for the subsequent checks to function
        # correctly.
        from psyclone.psyGen import Loop
        if not isinstance(node, Loop):
            raise TransformationError("Error in "+self.name+" transformation."
                                      " The node is not a loop.")
        # Check we are either an inner or outer loop
        if node.loop_type not in ["inner", "outer"]:
            raise TransformationError("Error in "+self.name+" transformation."
                                      " The requested loop is not of type "
                                      "inner or outer.")

        return OMPLoopTrans.apply(self, node)


class ColourTrans(Transformation):
    '''
    Apply a colouring transformation to a loop (in order to permit a
    subsequent parallelisation over colours). For example:

    >>> invoke = ...
    >>> schedule = invoke.schedule
    >>>
    >>> ctrans = ColourTrans()
    >>>
    >>> # Colour all of the loops
    >>> for child in schedule.children:
    >>>     cschedule, _ = ctrans.apply(child)
    >>>
    >>> csched.view()

    '''

    def __str__(self):
        return "Split a loop into colours"

    @property
    def name(self):
        ''' Returns the name of this transformation as a string.'''
        return "LoopColourTrans"

    def apply(self, node):
        '''
        Converts the Loop represented by :py:obj:`node` into a
        nested loop where the outer loop is over colours and the inner
        loop is over cells of that colour.

        :param node: The loop to transform.
        :type node: :py:class:`psyclone.psyGen.Loop`

        :returns: Tuple of modified schedule and record of transformation
        :rtype: (:py:class:`psyclone.psyGen.Schedule, \
                 :py:class:`psyclone.undoredo.Memento`)
        '''
        schedule = node.root

        # create a memento of the schedule and the proposed transformation
        keep = Memento(schedule, self, [node])

        node_parent = node.parent
        node_position = node.position

        # create a colours loop. This loops over colours and must be run
        # sequentially
        colours_loop = node.__class__(parent=node_parent, loop_type="colours")
        colours_loop.field_space = node.field_space
        colours_loop.iteration_space = node.iteration_space
        colours_loop.set_lower_bound("start")
        colours_loop.set_upper_bound("ncolours")
        # Add this loop as a child of the original node's parent
        node_parent.addchild(colours_loop, index=node_position)

        # create a colour loop. This loops over a particular colour and
        # can be run in parallel
        colour_loop = node.__class__(parent=colours_loop.children[3],
                                     loop_type="colour")
        colour_loop.field_space = node.field_space
        colour_loop.field_name = node.field_name
        colour_loop.iteration_space = node.iteration_space
        colour_loop.set_lower_bound("start")
        colour_loop.kernel = node.kernel

        if Config.get().distributed_memory:
            index = node.upper_bound_halo_depth
            colour_loop.set_upper_bound("colour_halo", index)
        else:  # no distributed memory
            colour_loop.set_upper_bound("ncolour")
        # Add this loop as a child of our loop over colours
        colours_loop.loop_body.addchild(colour_loop)

        # add contents of node to colour loop
        colour_loop.loop_body._children.extend(node.loop_body)

        # change the parent of the node's contents to the colour loop
        for child in node.loop_body:
            child.parent = colour_loop.children[3]

        # remove original loop
        node_parent.children.remove(node)

        return schedule, keep


class KernelModuleInlineTrans(Transformation):
    '''Switches on, or switches off, the inlining of a Kernel subroutine
    into the PSy layer module. For example:

    >>> invoke = ...
    >>> schedule = invoke.schedule
    >>>
    >>> inline_trans = KernelModuleInlineTrans()
    >>>
    >>> ischedule, _ = inline_trans.apply(schedule.children[0].loop_body[0])
    >>> ischedule.view()

    .. warning ::
        For this transformation to work correctly, the Kernel subroutine
        must only use data that is passed in by argument, declared locally
        or included via use association within the subroutine. Two
        examples where in-lining will not work correctly are:

        #. A variable is declared within the module that ``contains`` the
           Kernel subroutine and is then accessed within that Kernel;
        #. A variable is included via use association at the module level
           and accessed within the Kernel subroutine.

        *There are currently no checks that these rules are being followed
        when in-lining so the onus is on the user to ensure correctness.*
    '''

    def __str__(self):
        return ("Inline (or cancel inline of) a kernel subroutine into the "
                "PSy module")

    @property
    def name(self):
        ''' Returns the name of this transformation as a string.'''
        return "KernelModuleInline"

    def apply(self, node, inline=True):
        '''Checks that the node is of the correct type (a Kernel) then marks
        the Kernel to be inlined, or not, depending on the value of
        the inline argument. If the inline argument is not passed the
        Kernel is marked to be inlined.'''

        self.validate(node, inline)

        schedule = node.root

        # create a memento of the schedule and the proposed transformation
        keep = Memento(schedule, self, [node])

        # set kernel's inline status
        if node.module_inline == inline:
            # issue a warning here when we implement logging
            # print "Warning, Kernel inline is already set to "+str(inline)
            pass
        else:
            node.module_inline = inline

        return schedule, keep

    def validate(self, node, inline):
        '''
        Check that the supplied kernel is eligible to be module inlined.

        :param node: the node in the PSyIR that is to be module inlined.
        :type node: sub-class of :py:class:`psyclone.psyGen.Node`
        :param bool inline: whether or not the kernel is to be inlined.

        :raises TransformationError: if the supplied node is not a kernel.
        :raises TransformationError: if the supplied kernel has itself been \
                                     transformed (Issue #229).
        '''
        # check node is a kernel
        from psyclone.psyGen import Kern
        if not isinstance(node, Kern):
            raise TransformationError(
                "Error in KernelModuleInline transformation. The node is not "
                "a Kernel")

        if inline and node._fp2_ast:
            raise TransformationError("Cannot inline kernel {0} because it "
                                      "has previously been transformed.")


class Dynamo0p3ColourTrans(ColourTrans):

    '''Split a Dynamo 0.3 loop over cells into colours so that it can be
    parallelised. For example:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.psyGen import PSyFactory
    >>> import transformations
    >>> import os
    >>> import pytest
    >>>
    >>> TEST_API = "dynamo0.3"
    >>> _,info=parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
    >>>              "tests", "test_files", "dynamo0p3",
    >>>              "4.6_multikernel_invokes.f90"),
    >>>              api=TEST_API)
    >>> psy = PSyFactory(TEST_API).create(info)
    >>> invoke = psy.invokes.get('invoke_0')
    >>> schedule = invoke.schedule
    >>>
    >>> ctrans = Dynamo0p3ColourTrans()
    >>> otrans = DynamoOMPParallelLoopTrans()
    >>>
    >>> # Colour all of the loops
    >>> for child in schedule.children:
    >>>     cschedule, _ = ctrans.apply(child)
    >>>
    >>> # Then apply OpenMP to each of the colour loops
    >>> schedule = cschedule
    >>> for child in schedule.children:
    >>>     newsched, _ = otrans.apply(child.children[0])
    >>>
    >>> newsched.view()

    Colouring in the Dynamo 0.3 API is subject to the following rules:

    * Only kernels with an iteration space of CELLS and which modify a
      continuous field require colouring. Any other type of loop will
      cause this transformation to raise an exception.
    * A kernel may have at most one field with 'INC' access
    * A separate colour map will be required for each field that is coloured
      (if an invoke contains >1 kernel call)

    '''

    def __str__(self):
        return "Split a Dynamo 0.3 loop over cells into colours"

    @property
    def name(self):
        ''' Returns the name of this transformation as a string.'''
        return "Dynamo0p3LoopColourTrans"

    def apply(self, node):
        '''Performs Dynamo0.3-specific error checking and then uses the parent
        class to convert the Loop represented by :py:obj:`node` into a
        nested loop where the outer loop is over colours and the inner
        loop is over cells of that colour.

        :param node: the loop to transform.
        :type node: :py:class:`psyclone.dynamo0p3.DynLoop`

        :returns: 2-tuple of new schedule and memento of transform
        :rtype: (:py:class:`psyclone.dynamo0p3.DynInvokeSchedule`, \
                 :py:class:`psyclone.undoredo.Memento`)

        '''
        # check node is a loop
        from psyclone.psyGen import Loop
        if not isinstance(node, Loop):
            raise TransformationError("Error in DynamoColour transformation. "
                                      "The supplied node is not a loop")
        # Check we need colouring
        from psyclone.dynamo0p3 import DISCONTINUOUS_FUNCTION_SPACES
        if node.field_space.orig_name in DISCONTINUOUS_FUNCTION_SPACES:
            raise TransformationError(
                "Error in DynamoColour transformation. Loops iterating over "
                "a discontinuous function space are not currently supported.")

        # Colouring is only necessary (and permitted) if the loop is
        # over cells. Since this is the default it is represented by
        # an empty string.
        if node.loop_type != "":
            raise TransformationError(
                "Error in DynamoColour transformation. Only loops over cells "
                "may be coloured but this loop is over {0}".
                format(node.loop_type))

        # Check whether we have a field that has INC access
        if not node.has_inc_arg():
            # TODO generate a warning here as we don't need to colour
            # a loop that does not update a field with INC access
            pass

        # Check that we're not attempting to colour a loop that is
        # already within an OpenMP region (because the loop over
        # colours *must* be sequential)
        from psyclone.psyGen import OMPDirective
        if node.ancestor(OMPDirective):
            raise TransformationError("Cannot have a loop over colours "
                                      "within an OpenMP parallel region.")

        schedule, keep = ColourTrans.apply(self, node)

        return schedule, keep


@six.add_metaclass(abc.ABCMeta)
class ParallelRegionTrans(RegionTrans):
    '''
    Base class for transformations that create a parallel region.

    '''
    def __init__(self):
        # Holds the class instance for the type of parallel region
        # to generate
        self._pdirective = None
        super(ParallelRegionTrans, self).__init__()

    @abc.abstractmethod
    def __str__(self):
        pass  # pragma: no cover

    @abc.abstractproperty
    def name(self):
        ''' Returns the name of this transformation as a string.'''

    def _validate(self, node_list):
        '''
        Check that the supplied list of Nodes are eligible to be
        put inside a parallel region.

        :param list node_list: List of nodes to put into a parallel region
        :raises TransformationError: if the nodes cannot be put into a \
                                     parallel region.
        '''

        # temporary dynamo0.3-specific test for haloexchange calls
        # existing within a parallel region. As we are going to
        # support this in the future, see #526, it does not warrant
        # making a separate dynamo-specific class.
        from psyclone.psyGen import HaloExchange, InvokeSchedule
        for node in node_list:
            if isinstance(node, HaloExchange):
                raise TransformationError(
                    "A halo exchange within a parallel region is not "
                    "supported")

        if isinstance(node_list[0], InvokeSchedule):
            raise TransformationError(
                "A {0} transformation cannot be applied to an InvokeSchedule "
                "but only to one or more nodes from within an InvokeSchedule.".
                format(self.name))

        node_parent = node_list[0].parent

        for child in node_list:
            if child.parent is not node_parent:
                raise TransformationError(
                    "Error in {0} transformation: supplied nodes are not "
                    "children of the same parent.".format(self.name))
        super(ParallelRegionTrans, self)._validate(node_list)

    def apply(self, nodes):
        '''
        Apply this transformation to a subset of the nodes within a
        schedule - i.e. enclose the specified Loops in the
        schedule within a single parallel region.

        :param nodes: a single Node or a list of Nodes.
        :type nodes: (list of) :py:class:`psyclone.psyGen.Node`.
        :raises TransformationError: if the nodes argument is not of the \
                                     correct type.
        '''

        # Check whether we've been passed a list of nodes or just a
        # single node. If the latter then we create ourselves a
        # list containing just that node.
        from psyclone.psyGen import Node
        if isinstance(nodes, list) and isinstance(nodes[0], Node):
            node_list = nodes
        elif isinstance(nodes, Node):
            node_list = [nodes]
        else:
            arg_type = str(type(nodes))
            raise TransformationError("Error in {0} transformation. "
                                      "Argument must be a single Node in a "
                                      "schedule or a list of Nodes in a "
                                      "schedule but have been passed an "
                                      "object of type: {1}".
                                      format(self.name, arg_type))
        self._validate(node_list)

        # Keep a reference to the parent of the nodes that are to be
        # enclosed within a parallel region. Also keep the index of
        # the first child to be enclosed as that will become the
        # position of the new !$omp parallel directive.
        node_parent = node_list[0].parent
        node_position = node_list[0].position

        # create a memento of the schedule and the proposed
        # transformation
        schedule = node_list[0].root

        keep = Memento(schedule, self)

        # Create the parallel directive as a child of the
        # parent of the nodes being enclosed and with those nodes
        # as its children.
        # We slice the nodes list in order to get a new list object
        # (although the actual items in the list are still those in the
        # original). If we don't do this then we get an infinite
        # recursion in the new schedule.
        directive = self._pdirective(parent=node_parent,
                                     children=node_list[:])

        # Change all of the affected children so that they have
        # the region directive as their parent. Use a slice
        # of the list of nodes so that we're looping over a local
        # copy of the list. Otherwise things get confused when
        # we remove children from the list.
        for child in node_list[:]:
            # Remove child from the parent's list of children
            node_parent.children.remove(child)
            child.parent = directive

        # Add the region directive as a child of the parent
        # of the nodes being enclosed and at the original location
        # of the first of these nodes
        node_parent.addchild(directive,
                             index=node_position)

        return schedule, keep


class OMPParallelTrans(ParallelRegionTrans):
    '''
    Create an OpenMP PARALLEL region by inserting directives. For
    example:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.parse.utils import ParseError
    >>> from psyclone.psyGen import PSyFactory, GenerationError
    >>> api = "gocean1.0"
    >>> filename = "nemolite2d_alg.f90"
    >>> ast, invokeInfo = parse(filename, api=api, invoke_name="invoke")
    >>> psy = PSyFactory(api).create(invokeInfo)
    >>>
    >>> from psyclone.psyGen import TransInfo
    >>> t = TransInfo()
    >>> ltrans = t.get_trans_name('GOceanOMPLoopTrans')
    >>> rtrans = t.get_trans_name('OMPParallelTrans')
    >>>
    >>> schedule = psy.invokes.get('invoke_0').schedule
    >>> schedule.view()
    >>> new_schedule = schedule
    >>>
    >>> # Apply the OpenMP Loop transformation to *every* loop
    >>> # in the schedule
    >>> for child in schedule.children:
    >>>     newschedule, memento = ltrans.apply(child)
    >>>     schedule = newschedule
    >>>
    >>> # Enclose all of these loops within a single OpenMP
    >>> # PARALLEL region
    >>> newschedule, _ = rtrans.apply(schedule.children)
    >>> newschedule.view()

    '''
    from psyclone import psyGen
    # The types of node that this transformation can enclose
    valid_node_types = (psyGen.Loop, psyGen.Kern, psyGen.BuiltIn,
                        psyGen.OMPDirective, psyGen.GlobalSum)

    def __init__(self):
        super(OMPParallelTrans, self).__init__()
        from psyclone.psyGen import OMPParallelDirective
        # Set the type of directive that the base class will use
        self._pdirective = OMPParallelDirective

    def __str__(self):
        return "Insert an OpenMP Parallel region"

    @property
    def name(self):
        '''
        :returns: The name of this transformation as a string.
        :rtype: str
        '''
        return "OMPParallelTrans"

    def _validate(self, node_list):
        '''
        Perform OpenMP-specific validation checks.

        :param node_list: List of Nodes to put within parallel region.
        :type node_list: list of :py:class:`psyclone.psyGen.Node`.
        :raises TransformationError: if the target Nodes are already within \
                                     some OMP parallel region.
        '''
        from psyclone.psyGen import OMPDirective

        if node_list[0].ancestor(OMPDirective):
            raise TransformationError("Error in OMPParallel transformation:" +
                                      " cannot create an OpenMP PARALLEL " +
                                      "region within another OpenMP region.")

        # Now call the general validation checks
        super(OMPParallelTrans, self)._validate(node_list)


class ACCParallelTrans(ParallelRegionTrans):
    '''
    Create an OpenACC parallel region by inserting directives. This parallel
    region *must* come after an enter-data directive (see `ACCEnterDataTrans`)
    or within a data region (see `ACCDataTrans`). For example:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.psyGen import PSyFactory
    >>> api = "gocean1.0"
    >>> filename = "nemolite2d_alg.f90"
    >>> ast, invokeInfo = parse(filename, api=api, invoke_name="invoke")
    >>> psy = PSyFactory(api).create(invokeInfo)
    >>>
    >>> from psyclone.psyGen import TransInfo
    >>> t = TransInfo()
    >>> ptrans = t.get_trans_name('ACCParallelTrans')
    >>> dtrans = t.get_trans_name('ACCDataTrans')
    >>>
    >>> schedule = psy.invokes.get('invoke_0').schedule
    >>> schedule.view()
    >>>
    >>> # Enclose everything within a single OpenACC PARALLEL region
    >>> newschedule, _ = ptrans.apply(schedule.children)
    >>> # Add an enter-data directive
    >>> newschedule, _ = dtrans.apply(newschedule)
    >>> newschedule.view()
    '''
    from psyclone import gocean1p0, nemo, psyGen
    valid_node_types = (gocean1p0.GOLoop, gocean1p0.GOKern,
                        nemo.NemoLoop, nemo.NemoKern, psyGen.IfBlock,
                        psyGen.ACCLoopDirective, psyGen.Assignment,
                        psyGen.Reference, psyGen.Literal,
                        psyGen.BinaryOperation)

    def __init__(self):
        super(ACCParallelTrans, self).__init__()
        from psyclone.psyGen import ACCParallelDirective
        # Set the type of directive that the base class will use
        self._pdirective = ACCParallelDirective

    def __str__(self):
        return "Insert an OpenACC Parallel region"

    @property
    def name(self):
        '''
        :returns: The name of this transformation as a string.
        :rtype: str
        '''
        return "ACCParallelTrans"

    def _validate(self, node_list):
        '''
        OpenACC-specific validation checks that the supplied list
        of nodes can be enclosed in a parallel region.

        :param node_list: proposed list of nodes to put inside region.
        :type node_list: list of :py:class:`psyclone.psyGen.Node`.
        :raises NotImplementedError: if an API other than GOcean 1.0 is \
                                     being used.
        '''
        from psyclone.gocean1p0 import GOInvokeSchedule
        from psyclone.nemo import NemoInvokeSchedule
        sched = node_list[0].root
        if not isinstance(sched, (GOInvokeSchedule, NemoInvokeSchedule)):
            raise NotImplementedError(
                "OpenACC parallel regions are currently only "
                "supported for the gocean 1.0 and nemo APIs")
        super(ACCParallelTrans, self)._validate(node_list)


class GOConstLoopBoundsTrans(Transformation):
    ''' Switch on (or off) the use of constant loop bounds within
    a GOInvokeSchedule. In the absence of constant loop bounds, PSyclone will
    generate loops where the bounds are obtained by de-referencing a field
    object, e.g.:
    ::

      DO j = my_field%grid%internal%ystart, my_field%grid%internal%ystop

    Some compilers are able to produce more efficient code if they are
    provided with information on the relative trip-counts of the loops
    within an Invoke. With constant loop bounds switched on, PSyclone
    generates code like:
    ::

      ny = my_field%grid%subdomain%internal%ystop
      ...
      DO j = 1, ny-1

    In practice, the application of the constant loop bounds looks
    something like, e.g.:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.psyGen import PSyFactory
    >>> import os
    >>> TEST_API = "gocean1.0"
    >>> _, info = parse(os.path.join("tests", "test_files", "gocean1p0",
    >>>                              "single_invoke.f90"),
    >>>                 api=TEST_API)
    >>> psy = PSyFactory(TEST_API).create(info)
    >>> invoke = psy.invokes.get('invoke_0_compute_cu')
    >>> schedule = invoke.schedule
    >>>
    >>> from psyclone.transformations import GOConstLoopBoundsTrans
    >>> clbtrans = GOConstLoopBoundsTrans()
    >>>
    >>> newsched, _ = clbtrans.apply(schedule)
    >>> # or, to turn off const. looop bounds:
    >>> # newsched, _ = clbtrans.apply(schedule, const_bounds=False)
    >>>
    >>> newsched.view()

    '''

    def __str__(self):
        return "Use constant loop bounds for all loops in a GOInvokeSchedule"

    @property
    def name(self):
        ''' Return the name of the Transformation as a string.'''
        return "GOConstLoopBoundsTrans"

    def apply(self, node, const_bounds=True):
        '''Switches constant loop bounds on or off for all loops in a
        GOInvokeSchedule. Default is 'on'.

        :param node: The GOInvokeSchedule of which all loops will get the
            constant loop bounds switched on or off.
        :type node: :py:class:`psyclone.gocean1p0.GOInvokeSchedule`
        :param const_bounds: If the constant loop should be used (True)
            or not (False). Default is True.
        :type const_bounds: bool
        '''

        # Check node is a Schedule
        from psyclone.gocean1p0 import GOInvokeSchedule
        if not isinstance(node, GOInvokeSchedule):
            raise TransformationError("Error in GOConstLoopBoundsTrans: "
                                      "node is not a GOInvokeSchedule")

        keep = Memento(node, self)

        node.const_loop_bounds = const_bounds

        return node, keep


class MoveTrans(Transformation):
    '''Provides a transformation to move a node in the tree. For
    example:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.psyGen import PSyFactory
    >>> ast,invokeInfo=parse("dynamo.F90")
    >>> psy=PSyFactory("dynamo0.3").create(invokeInfo)
    >>> schedule=psy.invokes.get('invoke_v3_kernel_type').schedule
    >>> schedule.view()
    >>>
    >>> from psyclone.transformations import MoveTrans
    >>> trans=MoveTrans()
    >>> new_schedule, memento = trans.apply(schedule.children[0],
                                            schedule.children[2],
                                            position="after")
    >>> new_schedule.view()

    Nodes may only be moved to a new location with the same parent
    and must not break any dependencies otherwise an exception is
    raised.'''

    def __str__(self):
        return "Move a node to a different location"

    @property
    def name(self):
        ''' Returns the name of this transformation as a string.'''
        return "Move"

    # pylint: disable=no-self-use
    def _validate(self, node, location, position):
        ''' validity checks for input arguments '''

        # Check that the first argument is a Node
        from psyclone.psyGen import Node
        if not isinstance(node, Node):
            raise TransformationError(
                "In the Move transformation apply method the first argument "
                "is not a Node")

        # Check new location conforms to any data dependencies
        # This also checks the location and position arguments
        if not node.is_valid_location(location, position=position):
            raise TransformationError(
                "In the Move transformation apply method, data dependencies "
                "forbid the move to the new location")
        # pylint: enable=no-self-use

    def apply(self, node, location, position="before"):
        '''Move the node represented by :py:obj:`node` before location
        :py:obj:`location` (which is also a node) by default and after
        if the optional `position` argument is set to 'after'. An
        exception is raised if the move is invalid.'''
        # pylint:disable=arguments-differ

        self._validate(node, location, position)

        schedule = node.root

        # Create a memento of the schedule and the proposed transformation
        keep = Memento(schedule, self, [node, location])

        parent = node.parent

        my_node = parent.children.pop(node.position)

        location_index = location.position
        if position == "before":
            schedule.children.insert(location_index, my_node)
        else:
            schedule.children.insert(location_index+1, my_node)

        return schedule, keep


class Dynamo0p3RedundantComputationTrans(Transformation):
    '''This transformation allows the user to modify a loop's bounds so
    that redundant computation will be performed. Redundant
    computation can result in halo exchanges being modified, new halo
    exchanges being added or existing halo exchanges being removed.

    * This transformation should be performed before any
      parallelisation transformations (e.g. for OpenMP) to the loop in
      question and will raise an exception if this is not the case.

    * This transformation can not be applied to a loop containing a
      reduction and will again raise an exception if this is the case.

    * This transformation can only be used to add redundant
      computation to a loop, not to remove it.

    * This transformation allows a loop that is already performing
      redundant computation to be modified, but only if the depth is
      increased.

    '''

    def __str__(self):
        return "Change iteration space to perform redundant computation"

    @property
    def name(self):
        ''' Returns the name of this transformation as a string.'''
        return "RedundantComputation"

    def _validate(self, node, depth):
        '''Perform various checks to ensure that it is valid to apply the
        RedundantComputation transformation to the supplied node

        :param node: the supplied node on which we are performing
        validity checks
        :type node: :py:class:`psyclone.psyGen.Node`
        :param depth: the depth of the stencil if the value is
        provided and None if not
        :type depth: int or None
        :raises GenerationError: if the node is not a
        :py:class:`psyclone.psyGen.Loop`
        :raises GenerationError: if the parent of the loop is a
        :py:class:`psyclone.psyGen.Directive`
        :raises GenerationError: if the parent of the loop is not a
        :py:class:`psyclone.psyGen.Loop` or a
        :py:class:`psyclone.psyGen.DynInvokeSchedule`
        :raises GenerationError: if the parent of the loop is a
        :py:class:`psyclone.psyGen.Loop` but the original loop does
        not iterate over 'colour'
        :raises GenerationError: if the parent of the loop is a
        :py:class:`psyclone.psyGen.Loop` but the parent does not
        iterate over 'colours'
        :raises GenerationError: if the parent of the loop is a
        :py:class:`psyclone.psyGen.Loop` but the parent's parent is
        not a :py:class:`psyclone.psyGen.DynInvokeSchedule`
        :raises GenerationError: if this transformation is applied
        when distributed memory is not switched on
        :raises GenerationError: if the loop does not iterate over
        cells, dofs or colour
        :raises GenerationError: if the transformation is setting the
        loop to the maximum halo depth but the loop already computes
        to the maximum halo depth
        :raises GenerationError: if the transformation is setting the
        loop to the maximum halo depth but the loop contains a stencil
        access (as this would result in the field being accessed
        beyond the halo depth)
        :raises GenerationError: if the supplied depth value is not an
        integer
        :raises GenerationError: if the supplied depth value is less
        than 1
        :raises GenerationError: if the supplied depth value is not
        greater than 1 when a continuous loop is modified as this is
        the minimum valid value
        :raises GenerationError: if the supplied depth value is not
        greater than the existing depth value, as we should not need
        to undo existing transformations
        :raises GenerationError: if a depth value has been supplied
        but the loop has already been set to the maximum halo depth

        '''
        # check node is a loop
        from psyclone.psyGen import Loop, Directive
        from psyclone.dynamo0p3 import DynInvokeSchedule
        if not isinstance(node, Loop):
            raise TransformationError(
                "In the Dynamo0p3RedundantComputation transformation apply "
                "method the first argument is not a Loop")
        # check loop's parent is the schedule, or its parent is a
        # colours loop and perform other colour(s) loop checks,
        # otherwise halo exchange placement might fail. The only
        # current example where the placement would fail is when
        # directives have already been added. This could be fixed but
        # it actually makes sense to require redundant computation
        # transformations to be applied before adding directives so it
        # is not particularly important.
        if not isinstance(node.parent, (DynInvokeSchedule, Loop)):
            if isinstance(node.parent, Directive):
                raise TransformationError(
                    "In the Dynamo0p3RedundantComputation transformation "
                    "apply method the parent of the supplied loop is a "
                    "directive of type {0}. Redundant computation must be "
                    "applied before directives are "
                    "added.".format(type(node.parent)))
            else:
                raise TransformationError(
                    "In the Dynamo0p3RedundantComputation transformation "
                    "apply method the parent of the supplied loop must be "
                    "the DynInvokeSchedule, or a Loop, but found {0}".
                    format(type(node.parent)))
        if isinstance(node.parent, Loop):
            if node.loop_type != "colour":
                raise TransformationError(
                    "In the Dynamo0p3RedundantComputation transformation "
                    "apply method, if the parent of the supplied Loop is "
                    "also a Loop then the supplied Loop must iterate over "
                    "'colour', but found '{0}'".format(node.loop_type))
            if node.parent.loop_type != "colours":
                raise TransformationError(
                    "In the Dynamo0p3RedundantComputation transformation "
                    "apply method, if the parent of the supplied Loop is "
                    "also a Loop then the parent must iterate over "
                    "'colours', but found '{0}'".format(node.parent.loop_type))
            if not isinstance(node.parent.parent, DynInvokeSchedule):
                raise TransformationError(
                    "In the Dynamo0p3RedundantComputation transformation "
                    "apply method, if the parent of the supplied Loop is "
                    "also a Loop then the parent's parent must be the "
                    "DynInvokeSchedule, but found {0}"
                    .format(type(node.parent)))
        if not Config.get().distributed_memory:
            raise TransformationError(
                "In the Dynamo0p3RedundantComputation transformation apply "
                "method distributed memory must be switched on")

        # loop must iterate over cells, dofs or colour. Note, an
        # empty loop_type iterates over cells
        if node.loop_type not in ["", "dofs", "colour"]:
            raise TransformationError(
                "In the Dynamo0p3RedundantComputation transformation apply "
                "method the loop must iterate over cells, dofs or cells of "
                "a given colour, but found '{0}'".format(node.loop_type))

        from psyclone.dynamo0p3 import HALO_ACCESS_LOOP_BOUNDS

        # We don't currently support the application of transformations to
        # loops containing inter-grid kernels
        check_intergrid(node)

        if depth is None:
            if node.upper_bound_name in HALO_ACCESS_LOOP_BOUNDS:
                if not node.upper_bound_halo_depth:
                    raise TransformationError(
                        "In the Dynamo0p3RedundantComputation transformation "
                        "apply method the loop is already set to the maximum "
                        "halo depth so this transformation does nothing")
                for call in node.kernels():
                    for arg in call.arguments.args:
                        if arg.stencil:
                            raise TransformationError(
                                "In the Dynamo0p3RedundantComputation "
                                "transformation apply method the loop "
                                "contains field '{0}' with a stencil "
                                "access in kernel '{1}', so it is invalid "
                                "to set redundant computation to maximum "
                                "depth".format(arg.name, call.name))
        else:
            if not isinstance(depth, int):
                raise TransformationError(
                    "In the Dynamo0p3RedundantComputation transformation "
                    "apply method the supplied depth should be an integer but "
                    "found type '{0}'".format(type(depth)))
            if depth < 1:
                raise TransformationError(
                    "In the Dynamo0p3RedundantComputation transformation "
                    "apply method the supplied depth is less than 1")

            if node.upper_bound_name in HALO_ACCESS_LOOP_BOUNDS:
                if node.upper_bound_halo_depth:
                    if node.upper_bound_halo_depth >= depth:
                        raise TransformationError(
                            "In the Dynamo0p3RedundantComputation "
                            "transformation apply method the supplied depth "
                            "({0}) must be greater than the existing halo "
                            "depth ({1})".format(depth,
                                                 node.upper_bound_halo_depth))
                else:
                    raise TransformationError(
                        "In the Dynamo0p3RedundantComputation transformation "
                        "apply method the loop is already set to the maximum "
                        "halo depth so can't be set to a fixed value")

    def apply(self, loop, depth=None):  # pylint:disable=arguments-differ
        '''Apply the redundant computation tranformation to the loop
        :py:obj:`loop`. This transformation can be applied to loops iterating
        over 'cells or 'dofs'. if :py:obj:`depth` is set to a value then the
        value will be the depth of the field's halo over which redundant
        computation will be performed. If :py:obj:`depth` is not set to a
        value then redundant computation will be performed to the full depth
        of the field's halo.

        :param loop: the loop that we are transforming
        :type loop: :py:class:`psyclone.psyGen.DynLoop`
        :param depth: the depth of the stencil. Defaults to None if a
                      depth is not provided.
        :type depth: int or None

        '''
        self._validate(loop, depth)

        schedule = loop.root

        # create a memento of the schedule and the proposed
        # transformation
        keep = Memento(schedule, self, [loop, depth])

        if loop.loop_type == "":
            # Loop is over cells
            loop.set_upper_bound("cell_halo", depth)
        elif loop.loop_type == "colour":
            # Loop is over cells of a single colour
            loop.set_upper_bound("colour_halo", depth)
        elif loop.loop_type == "dofs":
            loop.set_upper_bound("dof_halo", depth)
        else:
            raise TransformationError(
                "Unsupported loop_type '{0}' found in Dynamo0p3Redundant"
                "ComputationTrans.apply()".format(loop.loop_type))
        # Add/remove halo exchanges as required due to the redundant
        # computation
        loop.update_halo_exchanges()

        return schedule, keep


class GOLoopSwapTrans(Transformation):
    ''' Provides a loop-swap transformation, e.g.:
    ::

      DO j=1, m
         DO i=1, n

    becomes:
    ::

      DO i=1, n
         DO j=1, m

    This transform is used as follows:

     >>> from psyclone.parse.algorithm import parse
     >>> from psyclone.psyGen import PSyFactory
     >>> ast, invokeInfo = parse("shallow_alg.f90")
     >>> psy = PSyFactory("gocean1.0").create(invokeInfo)
     >>> schedule = psy.invokes.get('invoke_0').schedule
     >>> schedule.view()
     >>>
     >>> from psyclone.transformations import GOLoopSwapTrans
     >>> swap = GOLoopSwapTrans()
     >>> new_schedule, memento = swap.apply(schedule.children[0])
     >>> new_schedule.view()
    '''

    def __str__(self):
        return "Exchange the order of two nested loops: inner becomes " + \
               "outer and vice versa"

    @property
    def name(self):
        '''Returns the name of this transformation as a string.'''
        return "GOLoopSwap"

    def _validate(self, node_outer):  # pylint: disable=no-self-use
        '''Checks if the given nodes contains a valid Fortran structure
           to allow swapping loops. This means the node must represent
           a loop, and it must have exactly one child that is also a loop.

           :param node_outer: A node from an AST.
           :type node_outer: py:class:`psyclone.psyGen.Node`
           :raises TransformationError: if the supplied node does not
                                        allow a loop swap to be done.
         '''

        from psyclone.psyGen import Loop
        if not isinstance(node_outer, Loop):
            raise TransformationError("Error in GOLoopSwap transformation. "
                                      "Given node '{0}' is not a loop."
                                      .format(node_outer))

        from psyclone.gocean1p0 import GOLoop
        if not isinstance(node_outer, GOLoop):
            raise TransformationError("Error in GOLoopSwap transformation. "
                                      "Given node '{0}' is not a GOLoop, but "
                                      "an instance of '{1}."
                                      .format(node_outer, type(node_outer)))

        if not node_outer.loop_body:
            raise TransformationError("Error in GOLoopSwap transformation. "
                                      "Supplied node '{0}' must be the outer "
                                      "loop of a loop nest and must have one "
                                      "inner loop, but this node does not "
                                      "have any statements inside."
                                      .format(node_outer))

        node_inner = node_outer.loop_body[0]
        # Check that the supplied Node is a Loop
        if not isinstance(node_inner, Loop):
            raise TransformationError("Error in GOLoopSwap transformation. "
                                      "Supplied node '{0}' must be the outer "
                                      "loop of a loop nest but the first "
                                      "inner statement is not a loop, got "
                                      "'{1}'."
                                      .format(node_outer, node_inner))

        if len(node_outer.loop_body) > 1:
            raise TransformationError("Error in GOLoopSwap transformation. "
                                      "Supplied node '{0}' must be the outer "
                                      "loop of a loop nest and must have "
                                      "exactly one inner loop, but this node "
                                      "has {1} inner statements, the first "
                                      "two being '{2}' and '{3}'"
                                      .format(node_outer,
                                              len(node_outer.children),
                                              node_outer.loop_body[0],
                                              node_outer.loop_body[1]))

    def apply(self, outer):  # pylint: disable=arguments-differ
        '''The argument :py:obj:`outer` must be a loop which has exactly
        one inner loop. This transform then swaps the outer and inner loop.

        :param outer: The node representing the outer loop.
        :type outer: :py:class:`psyclone.psyGen.Loop`
        :returns: A tuple consisting of the new schedule, and a Memento.
        :raises TransformationError: if the supplied node does not
                                        allow a loop swap to be done.'''
        self._validate(outer)

        schedule = outer.root
        inner = outer.loop_body[0]
        parent = outer.parent

        # create a memento of the schedule and the proposed transformation
        keep = Memento(schedule, self, [inner, outer])

        exit(0)
        # Remove outer from parent:
        index = parent.children.index(outer)
        del parent.children[index]
        outer.parent = None

        # Move inner to parent:
        inner.parent = parent
        parent.children.insert(index, inner)
        outer.children.remove(inner)

        # Move inner's children to outer:
        for child in inner.children:
            inner.children.remove(child)
            outer.children.append(child)
            child.parent = outer

        # Move outer under inner:
        inner.children.append(outer)
        outer.parent = inner

        return schedule, keep


class OCLTrans(Transformation):
    '''
    Switches on/off the generation of an OpenCL PSy layer for a given
    InvokeSchedule. Additionally, it will generate OpenCL kernels for
    each of the kernels referenced by the Invoke. For example:

    >>> invoke = ...
    >>> schedule = invoke.schedule
    >>>
    >>> ocl_trans = OCLTrans()
    >>> new_sched, _ = ocl_trans.apply(schedule)

    '''
    @property
    def name(self):
        '''
        :returns: the name of this transformation.
        :rtype: str
        '''
        return "OCLTrans"

    def apply(self, sched, opencl=True):
        '''
        Apply the OpenCL transformation to the supplied GOInvokeSchedule. This
        causes PSyclone to generate an OpenCL version of the corresponding
        PSy-layer routine. The generated code makes use of the FortCL
        library (https://github.com/stfc/FortCL) in order to manage the
        OpenCL device directly from Fortran.

        :param sched: InvokeSchedule to transform.
        :type sched: :py:class:`psyclone.psyGen.GOInvokeSchedule`
        :param bool opencl: whether or not to enable OpenCL generation.

        '''
        if opencl:
            self._validate(sched)
        # Create a memento of the schedule and the proposed transformation
        keep = Memento(sched, self, [sched, opencl])
        # All we have to do here is set the flag in the Schedule. When this
        # flag is True PSyclone produces OpenCL at code-generation time.
        sched.opencl = opencl
        return sched, keep

    def _validate(self, sched):
        '''
        Checks that the supplied Schedule is valid and that an OpenCL
        version of it can be generated.

        :param sched: Schedule to check.
        :type sched: :py:class:`psyclone.psyGen.Schedule`
        :raises TransformationError: if the Schedule is not for the GOcean1.0 \
                                     API.
        :raises NotImplementedError: if any of the kernels have arguments \
                                     passed by value.
        '''
        from psyclone.psyGen import Schedule, args_filter
        from psyclone.gocean1p0 import GOInvokeSchedule
        if isinstance(sched, Schedule):
            if not isinstance(sched, GOInvokeSchedule):
                raise TransformationError(
                    "OpenCL generation is currently only supported for the "
                    "GOcean API but got a Schedule of type: '{0}'".
                    format(type(sched)))
        else:
            raise TransformationError(
                "Error in OCLTrans: the supplied node must be a (sub-class "
                "of) Schedule but got {0}".format(type(sched)))
        # Now we need to check the arguments of all the kernels
        args = args_filter(sched.args, arg_types=["scalar"], is_literal=True)
        for arg in args:
            if arg.is_literal:
                raise NotImplementedError(
                    "Cannot generate OpenCL for Invokes that contain "
                    "kernels with arguments passed by value")


class ProfileRegionTrans(RegionTrans):
    ''' Create a profile region around a list of statements. For
    example:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.parse.utils import ParseError
    >>> from psyclone.psyGen import PSyFactory, GenerationError
    >>> api = "gocean1.0"
    >>> filename = "nemolite2d_alg.f90"
    >>> ast, invokeInfo = parse(filename, api=api, invoke_name="invoke")
    >>> psy = PSyFactory(api).create(invokeInfo)
    >>>
    >>> from psyclone.psyGen import TransInfo
    >>> t = TransInfo()
    >>> p_trans = t.get_trans_name('ProfileRegionTrans')
    >>>
    >>> schedule = psy.invokes.get('invoke_0').schedule
    >>> schedule.view()
    >>>
    >>> # Enclose all children within a single profile region
    >>> newschedule, _ = p_trans.apply(schedule.children)
    >>> newschedule.view()

    '''
    from psyclone import psyGen, profiler
    valid_node_types = (psyGen.Loop, psyGen.Kern, psyGen.BuiltIn,
                        psyGen.HaloExchange, psyGen.Directive,
                        psyGen.GlobalSum, profiler.ProfileNode)

    def __str__(self):
        return "Insert a profile start and end call."

    @property
    def name(self):
        ''' Returns the name of this transformation as a string '''
        return "ProfileRegionTrans"

    def apply(self, nodes):
        # pylint: disable=arguments-differ
        '''Apply this transformation to a subset of the nodes within a
        schedule - i.e. enclose the specified Nodes in the
        schedule within a single profiler region.

        :param nodes: Can be a single node or a list of nodes.
        :type nodes: :py:obj:`psyclone.psygen.Node` or list of\
        :py:obj:`psyclone.psygen.Node`.
        '''

        # Check whether we've been passed a list of nodes or just a
        # single node. If the latter then we create ourselves a
        # list containing just that node.
        from psyclone.psyGen import Node, OMPDoDirective, ACCLoopDirective
        if isinstance(nodes, list) and isinstance(nodes[0], Node):
            node_list = nodes
        elif isinstance(nodes, Node):
            node_list = [nodes]
        else:
            arg_type = str(type(nodes))
            raise TransformationError("Error in {1}. "
                                      "Argument must be a single Node in a "
                                      "schedule or a list of Nodes in a "
                                      "schedule but have been passed an "
                                      "object of type: {0}".
                                      format(arg_type, str(self)))

        # Keep a reference to the parent of the nodes that are to be
        # enclosed within a profile region. Also keep the index of
        # the first child to be enclosed as that will become the
        # position of the new Profile node
        node_parent = node_list[0].parent
        if isinstance(node_parent, (OMPDoDirective, ACCLoopDirective)):
            raise TransformationError("A ProfileNode cannot be inserted "
                                      "between an OpenMP/ACC directive and "
                                      "the loop(s) to which it applies!")
        node_position = node_list[0].position

        super(ProfileRegionTrans, self)._validate(node_list)

        # create a memento of the schedule and the proposed
        # transformation
        schedule = node_list[0].root

        keep = Memento(schedule, self)

        from psyclone.profiler import ProfileNode
        profile_node = ProfileNode(parent=node_parent, children=node_list[:])

        # Change all of the affected children so that they have
        # the ProfileNode as their parent. Use a slice
        # of the list of nodes so that we're looping over a local
        # copy of the list. Otherwise things get confused when
        # we remove children from the list.
        for child in node_list[:]:
            # Remove child from the parent's list of children
            node_parent.children.remove(child)
            child.parent = profile_node

        # Add the Profile node as a child of the parent
        # of the nodes being enclosed and at the original location
        # of the first of these nodes
        node_parent.addchild(profile_node,
                             index=node_position)

        return schedule, keep


class Dynamo0p3AsyncHaloExchangeTrans(Transformation):
    '''Splits a synchronous halo exchange into a halo exchange start and
    halo exchange end. For example:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.psyGen import PSyFactory
    >>> api = "dynamo0.3"
    >>> ast, invokeInfo = parse("file.f90", api=api)
    >>> psy=PSyFactory(api).create(invokeInfo)
    >>> schedule = psy.invokes.get('invoke_0').schedule
    >>> schedule.view()
    >>>
    >>> from psyclone.transformations import Dynamo0p3AsyncHaloExchangeTrans
    >>> trans = Dynamo0p3AsyncHaloExchangeTrans()
    >>> new_schedule, memento = trans.apply(schedule.children[0])
    >>> new_schedule.view()

    '''

    def __str__(self):
        return "Changes a synchronous halo exchange into an asynchronous one."

    @property
    def name(self):
        '''
        :returns: the name of this transformation as a string.
        :rtype: str
        '''
        return "Dynamo0p3AsyncHaloExchangeTrans"

    def apply(self, node):
        '''Transforms a synchronous halo exchange, represented by a
        HaloExchange node, into an asynchronous halo exchange,
        represented by HaloExchangeStart and HaloExchangeEnd nodes.

        :param node: A synchronous haloexchange node
        :type node: :py:obj:`psyclone.psygen.HaloExchange`
        :returns: Tuple of the modified schedule and a record of the \
                  transformation.
        :rtype: (:py:class:`psyclone.psyGen.Schedule`, \
                :py:class:`psyclone.undoredo.Memento`)

        '''
        self._validate(node)

        schedule = node.root

        # create a memento of the schedule and the proposed transformation
        keep = Memento(schedule, self, [node])

        from psyclone.dynamo0p3 import DynHaloExchangeStart, DynHaloExchangeEnd
        # add asynchronous start and end halo exchanges and initialise
        # them using information from the existing synchronous halo
        # exchange
        node.parent.addchild(
            DynHaloExchangeStart(
                node.field, check_dirty=node._check_dirty,
                vector_index=node.vector_index, parent=node.parent),
            index=node.position)
        node.parent.addchild(
            DynHaloExchangeEnd(
                node.field, check_dirty=node._check_dirty,
                vector_index=node.vector_index, parent=node.parent),
            index=node.position)

        # remove the existing synchronous halo exchange
        node.parent.children.remove(node)

        return schedule, keep

    def _validate(self, node):
        '''Internal method to check whether the node is valid for this
        transformation.

        :param node: A synchronous Halo Exchange node
        :type node: :py:obj:`psyclone.psygen.HaloExchange`
        :raises TransformationError: if the node argument is not a
                         HaloExchange (or subclass thereof)

        '''
        from psyclone.psyGen import HaloExchange
        from psyclone.dynamo0p3 import DynHaloExchangeStart, DynHaloExchangeEnd

        if not isinstance(node, HaloExchange) or \
           isinstance(node, (DynHaloExchangeStart, DynHaloExchangeEnd)):
            raise TransformationError(
                "Error in Dynamo0p3AsyncHaloExchange transformation. Supplied "
                "node must be a synchronous halo exchange but found '{0}'."
                .format(type(node)))


class Dynamo0p3KernelConstTrans(Transformation):
    '''Modifies a kernel so that the number of dofs, number of layers and
    number of quadrature points are fixed in the kernel rather than
    being passed in by argument.

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.psyGen import PSyFactory
    >>> api = "dynamo0.3"
    >>> ast, invokeInfo = parse("file.f90", api=api)
    >>> psy=PSyFactory(api).create(invokeInfo)
    >>> schedule = psy.invokes.get('invoke_0').schedule
    >>> schedule.view()
    >>>
    >>> from psyclone.transformations import Dynamo0p3KernelConstTrans
    >>> trans = Dynamo0p3KernelConstTrans()
    >>> for kernel in schedule.coded_kernels():
    >>>     new_schedule, _ = trans.apply(kernel)
    >>>     kernel_schedule = kernel.get_kernel_schedule()
    >>>     kernel_schedule.symbol_table.view()

    '''

    # ndofs for different function spaces on a quadrilateral element
    # for different orders. Formulas kindly provided by Tom
    # Melvin. See the Qr table at http://femtable.org/background.html,
    # for computed values of w0, w1, w2 and w3 up to order 7.
    space_to_dofs = {"w3":     (lambda n: (n+1)**3),
                     "w2":     (lambda n: 3*(n+2)*(n+1)**2),
                     "w1":     (lambda n: 3*(n+2)**2*(n+1)),
                     "w0":     (lambda n: (n+2)**3),
                     "wtheta": (lambda n: (n+2)*(n+1)**2),
                     "w2h":    (lambda n: 2*(n+2)*(n+1)**2),
                     "w2v":    (lambda n: (n+2)*(n+1)**2)}

    def __str__(self):
        return ("Makes the number of degrees of freedom, the number of "
                "quadrature points and the number of layers constant in "
                "a Kernel.")

    @property
    def name(self):
        '''
        :returns: the name of this transformation as a string.
        :rtype: str
        '''
        return "Dynamo0p3KernelConstTrans"

    def apply(self, node, cellshape="quadrilateral", element_order=None,
              number_of_layers=None, quadrature=False):
        '''Transforms a kernel so that the values for the number of degrees of
        freedom (if a valid value for the element_order arg is
        provided), the number of quadrature points (if the quadrature
        arg is set to True) and the number of layers (if a valid value
        for the number_of_layers arg is provided) are constant in a
        kernel rather than being passed in by argument.

        The "cellshape", "element_order" and "number_of_layers"
        arguments are provided to mirror the namelist values that are
        input into an LFRic model when it is run.

        Quadrature support is currently limited to XYoZ in ths
        transformation. In the case of XYoZ the number of quadrature
        points (for horizontal and vertical) are set to the
        element_order + 3 in the LFRic infrastructure so their value
        is derived.

        :param node: A kernel node
        :type node: :py:obj:`psyclone.psygen.DynKern`
        :type str cellshape: the shape of the cells. This is provided \
        as it helps determine the number of dofs a field has for a \
        particular function space. Currently only "quadrilateral" is \
        supported which is also the default value.
        :type int element_order: the order of the cell. In \
        combination with cellshape, this determines the number of \
        dofs a field has for a particular function space. If it is set \
        to None (the default) then the dofs values are not set as \
        constants in the kernel, otherwise they are.
        :type int number_of_layers: the number of vertical layers in \
        the LFRic model mesh used for this particular run. If this is \
        set to None (the default) then the nlayers value is not set as \
        a constant in the kernel, otherwise it is.
        :type bool quadrature: whether the number of quadrature \
        points values are set as constants in the kernel (True) or not \
        (False). The default is False.

        :returns: Tuple of the modified schedule and a record of the \
                  transformation.
        :rtype: (:py:class:`psyclone.psyGen.Schedule`, \
                :py:class:`psyclone.undoredo.Memento`)

        '''

        def make_constant(symbol_table, arg_position, value,
                          function_space=None):
            '''Utility function that modifies the argument at position
            'arg_position' into a compile-time constant with value
            'value'.

            :param symbol_table: The symbol table for the kernel \
                         holding the argument that is going to be modified.
            :type symbol_table: :py:class:`psyclone.psyGen.SymbolTable`
            :param int arg_position: The argument's position in the \
                                     argument list.
            :param value: The constant value that this argument is \
                   going to be given. Its type depends on the type of the \
                   argument.
            :type value: int, str or bool.
            :type str function_space: the name of the function space \
                        if there is a function space associated with this \
                        argument. Defaults to None.

            '''
            from psyclone.psyGen import Symbol
            arg_index = arg_position - 1
            try:
                symbol = symbol_table.argument_list[arg_index]
            except IndexError:
                raise TransformationError(
                    "The argument index '{0}' is greater than the number of "
                    "arguments '{1}'.".format(arg_index,
                                              len(symbol_table.argument_list)))
            # Perform some basic checks on the argument to make sure
            # it is the expected type
            if symbol.datatype != "integer" or \
               symbol.shape or symbol.is_constant:
                raise TransformationError(
                    "Expected entry to be a scalar integer argument "
                    "but found '{0}'.".format(symbol))

            # Create a new symbol with a known constant value then swap
            # it with the argument. The argument then becomes xxx_dummy
            # and is unused within the kernel body.
            # TODO: Temporarily use unsafe name change until the name
            # space manager is introduced into the SymbolTable (Issue
            # #321).
            orig_name = symbol.name
            local_symbol = Symbol(orig_name+"_dummy", "integer",
                                  constant_value=value)
            symbol_table.add(local_symbol)
            symbol_table.swap_symbol_properties(symbol, local_symbol)

            if function_space:
                print("    Modified {0}, arg position {1}, function space "
                      "{2}, value {3}.".format(orig_name, arg_position,
                                               function_space, value))
            else:
                print("    Modified {0}, arg position {1}, value {2}."
                      "".format(orig_name, arg_position, value))

        self._validate(node, cellshape, element_order, number_of_layers,
                       quadrature)

        schedule = node.root
        kernel = node

        # create a memento of the schedule and the proposed transformation
        keep = Memento(schedule, self, [kernel])

        from psyclone.dynamo0p3 import KernCallArgList
        arg_list_info = KernCallArgList(kernel)
        arg_list_info.generate()
        try:
            kernel_schedule = kernel.get_kernel_schedule()
        except NotImplementedError as excinfo:
            raise TransformationError(
                "Failed to parse kernel '{0}'. Error reported was '{1}'."
                "".format(kernel.name, str(excinfo)))

        symbol_table = kernel_schedule.symbol_table
        if number_of_layers:
            make_constant(symbol_table, arg_list_info.nlayers_positions[0],
                          number_of_layers)

        if quadrature and arg_list_info.nqp_positions:
            if kernel.eval_shape.lower() == "gh_quadrature_xyoz":
                make_constant(symbol_table,
                              arg_list_info.nqp_positions[0]["horizontal"],
                              element_order+3)
                make_constant(symbol_table,
                              arg_list_info.nqp_positions[0]["vertical"],
                              element_order+3)
            else:
                raise TransformationError(
                    "Error in Dynamo0p3KernelConstTrans transformation. "
                    "Support is currently limited to xyoz quadrature but "
                    "found '{0}'.".format(kernel.eval_shape))

        if element_order is not None:
            # Modify the symbol table for degrees of freedom here.
            for info in arg_list_info.ndf_positions:
                if info.function_space.lower() in (VALID_ANY_SPACE_NAMES +
                                                   ["any_w2"]):
                    # skip any_space_* and any_w2
                    print(
                        "    Skipped dofs, arg position {0}, function space "
                        "{1}".format(info.position, info.function_space))
                else:
                    try:
                        ndofs = Dynamo0p3KernelConstTrans. \
                                space_to_dofs[
                                    info.function_space](element_order)
                    except KeyError:
                        raise InternalError(
                            "Error in Dynamo0p3KernelConstTrans "
                            "transformation. Unsupported function space "
                            "'{0}' found. Expecting one of {1}."
                            "".format(info.function_space,
                                      Dynamo0p3KernelConstTrans.
                                      space_to_dofs.keys()))
                    make_constant(symbol_table, info.position, ndofs,
                                  function_space=info.function_space)

        return schedule, keep

    def _validate(self, node, cellshape, element_order, number_of_layers,
                  quadrature):
        '''Internal method to check whether the input arguments are valid for
        this transformation.

        :param node: A dynamo 0.3 kernel node
        :type node: :py:obj:`psyclone.psygen.DynKern`
        :type str cellshape: the shape of the elements/cells.
        :type int element_order: the order of the elements/cells.
        :type int number_of_layers: the number of layers to use.
        :type bool quadrature: whether quadrature dimension sizes \
        should or shouldn't be set as constants in a kernel.
        :raises TransformationError: if the node argument is not a \
        dynamo 0.3 kernel, the cellshape argument is not set to \
        "quadrilateral", the element_order argument is not a 0 or a \
        positive integer, the number of layers argument is not a \
        positive integer, the quadrature argument is not a boolean, \
        neither element order nor number of layers arguments are set \
        (as the transformation would then do nothing), or the \
        quadrature argument is True but the element order is not \
        provided (as the former needs the latter).

        '''
        from psyclone.dynamo0p3 import DynKern
        if not isinstance(node, DynKern):
            raise TransformationError(
                "Error in Dynamo0p3KernelConstTrans transformation. Supplied "
                "node must be a dynamo kernel but found '{0}'."
                .format(type(node)))

        if cellshape.lower() != "quadrilateral":
            # Only quadrilaterals are currently supported
            raise TransformationError(
                "Error in Dynamo0p3KernelConstTrans transformation. Supplied "
                "cellshape must be set to 'quadrilateral' but found '{0}'."
                .format(cellshape))

        if element_order is not None and \
           (not isinstance(element_order, int) or element_order < 0):
            # element order must be 0 or a positive integer
            raise TransformationError(
                "Error in Dynamo0p3KernelConstTrans transformation. The "
                "element_order argument must be >= 0 but found '{0}'."
                .format(element_order))

        if number_of_layers is not None and \
           (not isinstance(number_of_layers, int) or number_of_layers < 1):
            # number of layers must be a positive integer
            raise TransformationError(
                "Error in Dynamo0p3KernelConstTrans transformation. The "
                "number_of_layers argument must be > 0 but found '{0}'."
                .format(number_of_layers))

        if quadrature not in [False, True]:
            # quadrature must be a boolean value
            raise TransformationError(
                "Error in Dynamo0p3KernelConstTrans transformation. The "
                "quadrature argument must be boolean but found '{0}'."
                .format(quadrature))

        if element_order is None and not number_of_layers:
            # As a minimum, element order or number of layers must have values.
            raise TransformationError(
                "Error in Dynamo0p3KernelConstTrans transformation. At least "
                "one of element_order or number_of_layers must be set "
                "otherwise this transformation does nothing.")

        if quadrature and element_order is None:
            # if quadrature then element order
            raise TransformationError(
                "Error in Dynamo0p3KernelConstTrans transformation. If "
                "quadrature is set then element_order must also be set (as "
                "the values of the former are derived from the latter.")


class ACCEnterDataTrans(Transformation):
    '''
    Adds an OpenACC "enter data" directive to a Schedule.
    For example:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.psyGen import PSyFactory
    >>> api = "gocean1.0"
    >>> filename = "nemolite2d_alg.f90"
    >>> ast, invokeInfo = parse(filename, api=api, invoke_name="invoke")
    >>> psy = PSyFactory(api).create(invokeInfo)
    >>>
    >>> from psyclone.psyGen import TransInfo
    >>> t = TransInfo()
    >>> dtrans = t.get_trans_name('ACCEnterDataTrans')
    >>>
    >>> schedule = psy.invokes.get('invoke_0').schedule
    >>> schedule.view()
    >>>
    >>> # Add an enter-data directive
    >>> newschedule, _ = dtrans.apply(schedule)
    >>> newschedule.view()

    '''
    def __str__(self):
        return "Adds an OpenACC 'enter data' directive"

    @property
    def name(self):
        '''
        :returns: the name of this transformation.
        :rtype: str
        '''
        return "ACCEnterDataTrans"

    def apply(self, sched):
        '''Adds an OpenACC "enter data" directive to the invoke associated
        with the supplied Schedule. Any fields accessed by OpenACC kernels
        within this schedule will be added to this data region in
        order to ensure they remain on the target device.

        :param sched: Schedule to which to add an "enter data" directive.
        :type sched: sub-class of :py:class:`psyclone.psyGen.Schedule`.
        :returns: Tuple of the modified schedule and a record of the \
                  transformation.
        :rtype: (:py:class:`psyclone.psyGen.Schedule`, \
                :py:class:`psyclone.undoredo.Memento`)
        '''
        from psyclone.gocean1p0 import GOInvokeSchedule

        # Ensure that the proposed transformation is valid
        self._validate(sched)

        if isinstance(sched, GOInvokeSchedule):
            from psyclone.gocean1p0 import GOACCEnterDataDirective as \
                AccEnterDataDir
        else:
            # Should not get here provided that _validate() has done its job
            raise InternalError(
                "ACCEnterDataTrans._validate() has not rejected an "
                "(unsupported) schedule of type {0}".format(type(sched)))

        # Create a memento of the schedule and the proposed
        # transformation.
        keep = Memento(sched, self, [sched])

        # Add the directive
        data_dir = AccEnterDataDir(parent=sched, children=[])
        sched.addchild(data_dir, index=0)

        return sched, keep

    def _validate(self, sched):
        '''
        Check that we can safely apply the OpenACC enter-data transformation
        to the supplied Schedule.

        :param sched: Schedule to which to add an "enter data" directive.
        :type sched: sub-class of :py:class:`psyclone.psyGen.Schedule`.

        :raises NotImplementedError: for any API other than GOcean 1.0 or NEMO.
        :raises TransformationError: if passed something that is not a \
                         (subclass of) :py:class:`psyclone.psyGen.Schedule`.
        '''
        from psyclone.psyGen import Schedule, Directive, \
            ACCDataDirective, ACCEnterDataDirective
        from psyclone.gocean1p0 import GOInvokeSchedule

        super(ACCEnterDataTrans, self)._validate(sched)

        if not isinstance(sched, Schedule):
            raise TransformationError("Cannot apply an OpenACC enter-data "
                                      "directive to something that is "
                                      "not a Schedule")

        if not isinstance(sched, GOInvokeSchedule):
            raise NotImplementedError(
                "ACCEnterDataTrans: ACCEnterDataDirective not implemented for "
                "a schedule of type {0}".format(type(sched)))

        # Check that we don't already have a data region of any sort
        directives = sched.walk(sched.children, Directive)
        data_directives = [True if isinstance(ddir, (ACCDataDirective,
                                                     ACCEnterDataDirective))
                           else False for ddir in directives]
        if True in data_directives:
            raise TransformationError("Schedule already has an OpenACC data "
                                      "region - cannot add an enter data.")


class ACCRoutineTrans(Transformation):
    '''
    Transform a kernel subroutine by adding a "!$acc routine" directive
    (causing it to be compiled for the OpenACC accelerator device).
    For example:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.psyGen import PSyFactory
    >>> api = "gocean1.0"
    >>> filename = "nemolite2d_alg.f90"
    >>> ast, invokeInfo = parse(filename, api=api)
    >>> psy = PSyFactory(api).create(invokeInfo)
    >>>
    >>> from psyclone.transformations import ACCRoutineTrans
    >>> rtrans = ACCRoutineTrans()
    >>>
    >>> schedule = psy.invokes.get('invoke_0').schedule
    >>> schedule.view()
    >>> kern = schedule.children[0].children[0].children[0]
    >>> # Transform the kernel
    >>> newkern, _ = rtrans.apply(kern)
    '''
    @property
    def name(self):
        '''
        :returns: the name of this transformation class.
        :rtype: str
        '''
        return "ACCRoutineTrans"

    def apply(self, kern):
        '''
        Modifies the AST of the supplied kernel so that it contains an
        '!$acc routine' OpenACC directive.

        :param kern: The kernel object to transform.
        :type kern: :py:class:`psyclone.psyGen.Kern`
        :returns: (transformed kernel, memento of transformation)
        :rtype: 2-tuple of (:py:class:`psyclone.psyGen.Kern`, \
                :py:class:`psyclone.undoredo.Memento`).
        :raises TransformationError: if we fail to find the subroutine \
                                     corresponding to the kernel object.
        '''
        # pylint: disable=too-many-locals

        from fparser.two.Fortran2003 import Subroutine_Subprogram, \
            Subroutine_Stmt, Specification_Part, Type_Declaration_Stmt, \
            Implicit_Part, Comment
        from fparser.two.utils import walk_ast
        from fparser.common.readfortran import FortranStringReader

        # Check that we can safely apply this transformation
        self.validate(kern)

        # Get the fparser2 AST of the kernel
        ast = kern.ast
        # Keep a record of this transformation
        keep = Memento(kern, self)
        # Find the kernel subroutine
        kern_sub = None
        subroutines = walk_ast(ast.content, [Subroutine_Subprogram])
        for sub in subroutines:
            for child in sub.content:
                if isinstance(child, Subroutine_Stmt) and \
                   str(child.items[1]) == kern.name:
                    kern_sub = sub
                    break
            if kern_sub:
                break
        if not kern_sub:
            raise TransformationError(
                "Failed to find subroutine source for kernel {0}".
                format(kern.name))
        # Find the last declaration statement in the subroutine
        spec = walk_ast(kern_sub.content, [Specification_Part])[0]
        posn = -1
        for idx, node in enumerate(spec.content):
            if not isinstance(node, (Implicit_Part, Type_Declaration_Stmt)):
                posn = idx
                break
        # Create the directive and insert it
        cmt = Comment(FortranStringReader("!$acc routine",
                                          ignore_comments=False))
        if posn == -1:
            spec.content.append(cmt)
        else:
            spec.content.insert(posn, cmt)
        # Flag that the kernel has been modified
        kern.modified = True
        # Return the now modified kernel
        return kern, keep

    def validate(self, kern):
        '''
        Perform checks that the supplied kernel can be transformed.

        :param kern: the kernel which is the target of the transformation.
        :type kern: :py:class:`psyclone.psyGen.Kern`

        :raises TransformationError: if the target kernel is a built-in.

        '''
        from psyclone.psyGen import BuiltIn
        if isinstance(kern, BuiltIn):
            raise TransformationError(
                "Applying ACCRoutineTrans to a built-in kernel is not yet "
                "supported and kernel '{0}' is of type '{1}'".
                format(kern.name, type(kern)))

        if kern.module_inline:
            raise TransformationError("Cannot transform kernel {0} because "
                                      "it will be module-inlined.".
                                      format(kern.name))


class ACCKernelsTrans(RegionTrans):
    '''
    Enclose a sub-set of nodes from a Schedule within an OpenACC kernels
    region (i.e. within "!$acc kernels" ... "!$acc end kernels" directives).
    Currently only supported for the NEMO API.

    For example:

    >>> from psyclone.parse import parse
    >>> from psyclone.psyGen import PSyFactory
    >>> api = "NEMO"
    >>> filename = "tra_adv.F90"
    >>> ast, invokeInfo = parse(filename, api=api)
    >>> psy = PSyFactory(api).create(invokeInfo)
    >>>
    >>> from psyclone.transformations import ACCKernelsTrans
    >>> ktrans = ACCKernelsTrans()
    >>>
    >>> schedule = psy.invokes.get('invoke_0').schedule
    >>> schedule.view()
    >>> kernels = schedule.children[0].children[0].children[0:-1]
    >>> # Transform the kernel
    >>> new_sched, _ = ktrans.apply(kernels)

    '''
    from psyclone import nemo, psyGen
    valid_node_types = (nemo.NemoLoop, nemo.NemoKern, psyGen.IfBlock,
                        psyGen.Operation, psyGen.Literal,
                        psyGen.Assignment, psyGen.Reference)

    @property
    def name(self):
        '''
        :returns: the name of this transformation class.
        :rtype: str
        '''
        return "ACCKernelsTrans"

    def apply(self, node_list, default_present=False):
        '''
        Enclose the supplied list of PSyIR nodes within an OpenACC
        Kernels region.

        :param node_list: The list of nodes in the PSyIR to enclose.
        :type node_list: list of :py:class:`psyclone.psyGen.Node`
        :param bool default_present: whether or not the kernels region \
           should have the 'default present' attribute (indicating that data \
           is already on the accelerator). When using managed memory this \
           option should be False.
        :returns: (transformed schedule, memento of transformation)
        :rtype: 2-tuple of (:py:class:`psyclone.psyGen.Schedule`,
                            :py:class:`psyclone.undoredo.Memento`).

        '''
        self._validate(node_list)

        # Keep a record of this transformation
        keep = Memento(node_list[:], self)

        parent = node_list[0].parent
        schedule = node_list[0].root

        # Create the directive and insert it. Take a copy of the list
        # as it may just be a reference to the parent.children list
        # that we are about to modify.
        from psyclone.psyGen import ACCKernelsDirective
        directive = ACCKernelsDirective(parent=parent,
                                        children=node_list[:],
                                        default_present=default_present)
        start_index = parent.children.index(node_list[0])

        for child in directive.children:
            parent.children.remove(child)
            child.parent = directive

        parent.children.insert(start_index, directive)

        # Return the now modified kernel
        return schedule, keep

    def _validate(self, node_list):
        '''
        Check that we can safely enclose the supplied list of nodes within
        OpenACC kernels ... end kernels directives.
        '''
        from psyclone.nemo import NemoInvokeSchedule
        from psyclone.psyGen import Loop
        # Check that the API is valid
        sched = node_list[0].root
        if not isinstance(sched, NemoInvokeSchedule):
            raise NotImplementedError("OpenACC kernels regions are currently "
                                      "only supported for the nemo API")
        super(ACCKernelsTrans, self)._validate(node_list)

        # Check that we have at least one loop within the proposed region
        found = False
        for node in node_list:
            loops = node.walk(node.children, Loop)
            if loops or isinstance(node, Loop):
                found = True
                break
        if not found:
            raise TransformationError("A kernels transformation must enclose "
                                      "at least one loop but none were found.")

        # TODO #315 Check that the SymbolTable associated with the
        # KernelSchedule does not have any symbols with `deferred` type (as
        # that indicates that we haven't yet worked out what they are). We
        # can't do that yet as we can't create the PSyIR for our test kernels.
        # That's the subject of #256.


class ACCDataTrans(RegionTrans):
    '''
    Add an OpenACC data region around a list of nodes in the PSyIR.
    COPYIN, COPYOUT and COPY clauses are added as required.

    For example:

    >>> from psyclone.parse import parse
    >>> from psyclone.psyGen import PSyFactory
    >>> api = "NEMO"
    >>> filename = "tra_adv.F90"
    >>> ast, invokeInfo = parse(filename, api=api)
    >>> psy = PSyFactory(api).create(invokeInfo)
    >>>
    >>> from psyclone.transformations import ACCDataTrans
    >>> dtrans = ACCDataTrans()
    >>>
    >>> schedule = psy.invokes.get('invoke_0').schedule
    >>> schedule.view()
    >>> kernels = schedule.children[0].children[0].children[0:-1]
    >>> # Enclose the kernels
    >>> new_sched, _ = dtrans.apply(kernels)

    '''
    from psyclone import psyGen
    valid_node_types = (psyGen.Loop, psyGen.Kern, psyGen.BuiltIn,
                        psyGen.Directive, psyGen.IfBlock, psyGen.Literal,
                        psyGen.Assignment, psyGen.Reference,
                        psyGen.BinaryOperation)

    @property
    def name(self):
        '''
        :returns: the name of this transformation.
        :rtype: str

        '''
        return "ACCDataTrans"

    def apply(self, node_list):
        '''
        Put the supplied list of nodes within an OpenACC data region.

        :param node_list: The list of PSyIR nodes to enclose in the data \
                          region.
        :type node_list: list of :py:class:`psyclone.psyGen.Node`
        :returns: (transformed schedule, memento of transformation)
        :rtype: 2-tuple of (:py:class:`psyclone.psyGen.Schedule`, \
                :py:class:`psyclone.undoredo.Memento`).

        '''
        self._validate(node_list)

        # Keep a record of this transformation
        keep = Memento(node_list[:], self)

        parent = node_list[0].parent
        schedule = node_list[0].root

        # Create the directive and insert it. Take a copy of the list
        # as it may just be a reference to the parent.children list
        # that we are about to modify.
        from psyclone.psyGen import ACCDataDirective
        directive = ACCDataDirective(parent=parent, children=node_list[:])
        start_index = parent.children.index(node_list[0])

        for child in directive.children:
            parent.children.remove(child)
            child.parent = directive

        parent.children.insert(start_index, directive)

        # Return the now modified kernel
        return schedule, keep

    def _validate(self, node_list):
        '''
        Check that we can safely add a data region around the supplied list
        of nodes.

        :param node_list: the proposed list of nodes to enclose in a data \
                          region.
        :type node_list: list of subclasses of :py:class:`psyclone.psyGen.Node`

        :raises TransformationError: if the Schedule to which the nodes \
                                belong already has an 'enter data' directive.
        :raises TransformationError: if any of the nodes are themselves \
                                     data directives.
        '''
        from psyclone.psyGen import ACCEnterDataDirective
        super(ACCDataTrans, self)._validate(node_list)

        # Check that the Schedule to which the nodes belong does not already
        # have an 'enter data' directive.
        schedule = node_list[0].root
        acc_dirs = schedule.walk(schedule.children, ACCEnterDataDirective)
        if acc_dirs:
            raise TransformationError(
                "Cannot add an OpenACC data region to a schedule that "
                "already contains an 'enter data' directive.")


class NemoExplicitLoopTrans(Transformation):
    '''
    Transforms the outermost array slice in an implicit loop in a
    NEMOInvokeSchedule into an explicit loop. For example, if
    "implicit_loop.f90" contained:

    .. code-block:: fortran

        my_array(:, :, :) = 1.0

    then doing:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.psyGen import PSyFactory
    >>> api = "nemo"
    >>> filename = "implicit_loop.f90"
    >>> ast, invokeInfo = parse(filename, api=api)
    >>> psy = PSyFactory(api).create(invokeInfo)
    >>>
    >>> from psyclone.transformations import NemoExplicitLoopTrans
    >>> rtrans = NemoExplicitLoopTrans()
    >>>
    >>> schedule = psy.invokes.get('invoke_0').schedule
    >>> loop = schedule.children[0]
    >>> newloop, _ = rtrans.apply(loop)

    will create a new NemoLoop object for an explicit loop over levels
    (the outermost slice) that then contains an implicit loop:

    .. code-block:: fortran

        DO jk = 1, jpk
          my_array(:, :, jk) = 1.0
        END DO

    Subsequently applying `rtrans` to `newloop` will create:

    .. code-block:: fortran

        DO jk = 1, jpk
          DO jj = 1, jpj
            my_array(:, jj, jk) = 1.0
          END DO
        END DO

    '''
    @property
    def name(self):
        '''
        :returns: the name of this transformation class.
        :rtype: str
        '''
        return "NemoExplicitLoopTrans"

    def apply(self, loop):
        '''
        Transform the outermost array slice in the supplied implicit loop
        into an explicit loop.

        :param loop: the NemoImplicitLoop to transform.
        :type loop: :py:class:`psyclone.nemo.NemoImplicitLoop`
        :returns: a new PSyIR loop object and a memento of the transformation.
        :rtype: (:py:class:`psyclone.nemo.NemoLoop`, \
                 :py:class:`psyclone.undoredo.Memento`)

        :raises NotImplementedError: if the array slice has explicit bounds.
        :raises TransformationError: if an array slice is not in dimensions \
                                     1-3 of the array.
        '''
        from fparser.two import Fortran2003
        from fparser.two.utils import walk_ast
        from fparser.common.readfortran import FortranStringReader
        from psyclone import nemo

        self.validate(loop)

        # Keep a record of this transformation
        keep = Memento(loop, self)

        # Find all uses of array syntax in the statement
        subsections = walk_ast(loop.ast.items,
                               [Fortran2003.Section_Subscript_List])
        # Create a list identifying which dimensions contain a range
        sliced_dimensions = []
        # A Section_Subscript_List is a tuple with each item the
        # array-index expressions for the corresponding dimension of the array.
        for idx, item in enumerate(subsections[0].items):
            if isinstance(item, Fortran2003.Subscript_Triplet):
                # A Subscript_Triplet has a 3-tuple containing the expressions
                # for the start, end and increment of the slice. If any of
                # these are not None then we have an explicit range of some
                # sort and we do not yet support that.
                # TODO #278 allow for implicit loops with specified bounds
                # (e.g. 2:jpjm1)
                if [part for part in item.items if part]:
                    raise NotImplementedError(
                        "Support for implicit loops with specified bounds is "
                        "not yet implemented: '{0}'".format(str(loop.ast)))
                # If an array index is a Subscript_Triplet then it is a range
                # and thus we need to create an explicit loop for this
                # dimension.
                outermost_dim = idx
                # Store the fact that this array index is a range.
                sliced_dimensions.append(idx)

        if outermost_dim < 0 or outermost_dim > 2:
            raise TransformationError(
                "Array section in unsupported dimension ({0}) for code "
                "'{1}'".format(outermost_dim+1, str(loop.ast)))

        # TODO (fparser/#102) since the fparser2 AST does not have parent
        # information (and no other way of getting to the root node), it is
        # currently not possible to cleanly insert a declaration in the correct
        # location.
        # For the moment, we can work around the fparser2 AST limitation
        # by using the fact that we *can* get hold of the PSyclone Invoke
        # object and that contains a reference to the root of the fparser2
        # AST...

        # Get a reference to the Invoke to which this loop belongs
        invoke = loop.root.invoke
        nsm = invoke._name_space_manager
        config = Config.get().api_conf("nemo")
        index_order = config.get_index_order()
        loop_type_data = config.get_loop_type_data()

        loop_type = loop_type_data[index_order[outermost_dim]]
        base_name = loop_type["var"]
        loop_var = nsm.create_name(root_name=base_name, context="PSyVars",
                                   label=base_name)
        loop_start = loop_type["start"]
        loop_stop = loop_type["stop"]
        loop_step = "1"
        name = Fortran2003.Name(FortranStringReader(loop_var))
        # TODO #255 we need some sort of type/declarations table to check that
        # we don't already have a declaration for a variable of this name.
        # For the moment we keep a list of variables we have created in
        # Invoke._loop_vars.
        if loop._variable_name not in invoke._loop_vars:
            invoke._loop_vars.append(loop_var)

            prog_unit = loop.root.invoke._ast
            spec_list = walk_ast(prog_unit.content,
                                 [Fortran2003.Specification_Part])
            if not spec_list:
                # Routine has no specification part so create one and add it
                # in to the AST
                spec = Fortran2003.Specification_Part(
                    FortranStringReader(
                        "integer :: {0}".format(loop_var)))
                spec._parent = prog_unit
                for idx, child in enumerate(prog_unit.content):
                    if isinstance(child, Fortran2003.Execution_Part):
                        prog_unit.content.insert(idx, spec)
                        break
            else:
                spec = spec_list[0]
                decln = Fortran2003.Type_Declaration_Stmt(
                    FortranStringReader(
                        "integer :: {0}".format(loop_var)))
                spec.content.append(decln)

        # Modify the line containing the implicit do by replacing every
        # occurrence of the outermost ':' with the new loop variable name.
        for subsec in subsections:
            # A tuple is immutable so work with a list
            indices = list(subsec.items)
            if outermost_dim >= len(indices):
                raise InternalError(
                    "Expecting a colon for index {0} but array only has {1} "
                    "dimensions: {2}".format(outermost_dim+1, len(indices),
                                             str(loop.ast)))
            if not isinstance(indices[outermost_dim],
                              Fortran2003.Subscript_Triplet):
                raise TransformationError(
                    "Currently implicit loops are restricted to cases where "
                    "all array range specifications occur in the same "
                    "dimension(s) of each array in an assignment.")
            # Replace the colon with our new variable name
            indices[outermost_dim] = name
            # Replace the original tuple with a new one
            subsec.items = tuple(indices)

        # Create the fparser AST for an explicit loop
        text = ("do {0}={1},{2},{3}\n"
                "  replace = me\n"
                "end do\n".format(loop_var, loop_start, loop_stop,
                                  loop_step))
        new_loop = Fortran2003.Block_Nonlabel_Do_Construct(
            FortranStringReader(text))

        # Insert it in the fparser2 AST at the location of the implicit
        # loop
        parent_index = loop.ast._parent.content.index(loop.ast)
        loop.ast._parent.content.insert(parent_index, new_loop)
        # Replace the content of the loop with the (modified) implicit
        # loop
        new_loop.content[1] = loop.ast
        # Remove the implicit loop from its original parent in the AST
        loop.ast._parent.content.remove(loop.ast)

        # Now we must update the PSyIR to reflect the new AST
        # First we update the parent of the loop we have transformed
        psyir_parent = loop.parent
        psyir_parent.children.remove(loop)
        # Next, we simply process the transformed fparser2 AST to generate
        # the new PSyIR of it
        astprocessor = nemo.NemoFparser2ASTProcessor()
        astprocessor.process_nodes(psyir_parent, [new_loop], loop.ast._parent)
        # Delete the old PSyIR node that we have transformed
        del loop
        loop = None
        # Return the new NemoLoop object that we have created
        return psyir_parent.children[0], keep

    def validate(self, loop):
        '''
        Check that the supplied loop is a valid target for this transformation.

        :param loop: the loop node to validate.
        :type loop: :py:class:`psyclone.nemo.NemoImplicitLoop`

        :raises TransformationError: if the supplied loop is not a \
                                     NemoImplicitLoop.
        '''
        from psyclone.nemo import NemoImplicitLoop
        if not isinstance(loop, NemoImplicitLoop):
            raise TransformationError(
                "Cannot apply NemoExplicitLoopTrans to something that is "
                "not a NemoImplicitLoop (got {0})".format(type(loop)))


class ExtractRegionTrans(RegionTrans):
    ''' Provides a transformation to extract code represented by a \
    subset of the Nodes in the PSyIR of a Schedule into a stand-alone \
    program. Examples are given in descriptions of children classes \
    DynamoExtractRegionTrans and GOceanExtractRegionTrans.

    After applying the transformation the Nodes marked for extraction are \
    children of the ExtractNode. \
    Nodes to extract can be individual constructs within an Invoke (e.g. \
    Loops containing a Kernel or BuiltIn call) or entire Invokes. This \
    functionality does not support distributed memory.
    '''
    from psyclone import psyGen
    # The types of node that this transformation can enclose
    valid_node_types = (psyGen.Loop, psyGen.Kern, psyGen.BuiltIn,
                        psyGen.Directive)

    def __str__(self):
        return ("Create a sub-tree of the PSyIR that has ExtractNode "
                "at its root.")

    @property
    def name(self):
        ''' Returns the name of this transformation as a string.'''
        return "ExtractRegionTrans"

    def _validate(self, node_list):
        ''' Perform validation checks before applying the transformation

        :param node_list: the list of Node(s) we are checking.
        :type node_list: list of :py:class:`psyclone.psyGen.Node`.
        :raises TransformationError: if distributed memory is configured.
        :raises TransformationError: if transformation is applied to a \
                                     Kernel or a BuiltIn call without its \
                                     parent Loop.
        :raises TransformationError: if transformation is applied to a Loop \
                                     without its parent Directive when \
                                     optimisations are applied.
        :raises TransformationError: if transformation is applied to an \
                                     orphaned Directive without its parent \
                                     Directive.
        '''

        # First check constraints on Nodes in the node_list common to
        # all RegionTrans transformations.
        super(ExtractRegionTrans, self)._validate(node_list)

        # Now check ExtractRegionTrans specific constraints.

        # Extracting distributed memory code is not supported due to
        # generation of infrastructure calls to set halos dirty or clean.
        # This constraint covers the presence of HaloExchange and
        # GlobalSum classses as they are only generated when distributed
        # memory is enabled.
        if Config.get().distributed_memory:
            raise TransformationError(
                "Error in {0}: Distributed memory is not supported."
                .format(str(self.name)))

        # Check constraints not covered by valid_node_types for
        # individual Nodes in node_list.
        from psyclone.psyGen import Loop, Kern, BuiltIn, Directive, \
            OMPParallelDirective, ACCParallelDirective

        for node in node_list:

            # Check that ExtractNode is not inserted between a Kernel or
            # a BuiltIn call and its parent Loop.
            if isinstance(node, (Kern, BuiltIn)) and \
               isinstance(node.parent.parent, Loop):
                raise TransformationError(
                    "Error in {0}: Extraction of a Kernel or a Built-in "
                    "call without its parent Loop is not allowed."
                    .format(str(self.name)))

            # Check that ExtractNode is not inserted between a Loop and its
            # parent Directive when optimisations are applied, as this may
            # result in including the end of Directive for extraction but
            # not the beginning.
            if isinstance(node, Loop) and isinstance(node.parent, Directive):
                raise TransformationError(
                    "Error in {0}: Extraction of a Loop without its parent "
                    "Directive is not allowed.".format(str(self.name)))

            # Check that ExtractNode is not inserted within a thread
            # parallel region when optimisations are applied. For instance,
            # this may be between an orphaned Directive (e.g. OMPDoDirective,
            # ACCLoopDirective) and its ancestor Directive (e.g. ACC or OMP
            # Parallel Directive) or within an OMPParallelDoDirective.
            if node.ancestor(OMPParallelDirective) or \
                    node.ancestor(ACCParallelDirective):
                raise TransformationError(
                    "Error in {0}: Extraction of Nodes enclosed within "
                    "a thread parallel region is not allowed."
                    .format(str(self.name)))

    def apply(self, nodes):
        # pylint: disable=arguments-differ
        ''' Apply this transformation to a subset of the Nodes within
        a Schedule - i.e. enclose the specified Nodes in the Schedule
        within a single Extract region.

        :param nodes: a single Node or a list of Nodes.
        :type nodes: (list of) :py:class:`psyclone.psyGen.Node`.
        :returns: tuple of the modified Schedule and a record of the \
                  transformation.
        :rtype: (:py:class:`psyclone.psyGen.Schedule`, \
                 :py:class:`psyclone.undoredo.Memento`).
        :raises TransformationError: if the `nodes` argument is not of \
                                     the correct type.
        '''

        # Check whether we've been passed a list of Nodes or just a
        # single Node. If the latter then we create ourselves a list
        # containing just that Node.
        from psyclone.psyGen import Node
        if isinstance(nodes, list) and isinstance(nodes[0], Node):
            node_list = nodes
        elif isinstance(nodes, Node):
            node_list = [nodes]
        else:
            arg_type = str(type(nodes))
            raise TransformationError("Error in {0}: "
                                      "Argument must be a single Node in a "
                                      "Schedule or a list of Nodes in a "
                                      "Schedule but have been passed an "
                                      "object of type: {1}".
                                      format(str(self.name), arg_type))

        # Validate transformation
        self._validate(node_list)

        # Keep a reference to the parent of the Nodes that are to be
        # enclosed within an Extract region. Also keep the index of
        # the first child to be enclosed as that will be the position
        # of the ExtractNode.
        node_parent = node_list[0].parent
        node_position = node_list[0].position

        # Create a Memento of the Schedule and the proposed
        # transformation
        schedule = node_list[0].root

        keep = Memento(schedule, self)

        from psyclone.extractor import ExtractNode
        extract_node = ExtractNode(parent=node_parent, children=node_list[:])

        # Change all of the affected children so that they have the
        # ExtractNode as their parent. Use a slice of the list of Nodes
        # so that we're looping over a local copy of the list. Otherwise
        # things get confused when we remove children from the list.
        for child in node_list[:]:
            # Remove child from the parent's list of children
            node_parent.children.remove(child)
            child.parent = extract_node

        # Add the ExtractNode as a child of the parent of the Nodes being
        # enclosed at the original location of the first of these Nodes
        node_parent.addchild(extract_node,
                             index=node_position)

        return schedule, keep


class DynamoExtractRegionTrans(ExtractRegionTrans):
    ''' Dynamo0.3 API application of ExtractRegionTrans transformation \
    to extract code into a stand-alone program. For example:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.psyGen import PSyFactory
    >>>
    >>> API = "dynamo0.3"
    >>> FILENAME = "solver_alg.x90"
    >>> ast, invokeInfo = parse(FILENAME, api=API)
    >>> psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    >>> schedule = psy.invokes.get('invoke_0').schedule
    >>>
    >>> from psyclone.transformations import DynamoExtractRegionTrans
    >>> etrans =  DynamoExtractRegionTrans()
    >>>
    >>> # Apply DynamoExtractRegionTrans transformation to selected Nodes
    >>> newsched, _ = etrans.apply(schedule.children[0:3])
    >>> newsched.view()
    '''

    @property
    def name(self):
        ''' Returns the name of this transformation as a string.'''
        return "DynamoExtractRegionTrans"

    def _validate(self, node_list):
        ''' Perform Dynamo0.3 API specific validation checks before applying
        the transformation.

        :param node_list: the list of Node(s) we are checking.
        :type node_list: list of :py:class:`psyclone.psyGen.Node`.

        :raises TransformationError: if transformation is applied to a Loop \
                                     over cells in a colour without its \
                                     parent Loop over colours.
        '''

        # First check constraints on Nodes in the node_list inherited from
        # the parent classes (ExtractRegionTrans and RegionTrans)
        super(DynamoExtractRegionTrans, self)._validate(node_list)

        # Check DynamoExtractRegionTrans specific constraints
        from psyclone.dynamo0p3 import DynLoop
        for node in node_list:

            # Check that ExtractNode is not inserted between a Loop
            # over colours and a Loop over cells in a colour when
            # colouring is applied.
            ancestor = node.ancestor(DynLoop)
            if ancestor and ancestor.loop_type == 'colours':
                raise TransformationError(
                    "Error in {0} for Dynamo0.3 API: Extraction of a Loop "
                    "over cells in a colour without its ancestor Loop over "
                    "colours is not allowed.".format(str(self.name)))


class GOceanExtractRegionTrans(ExtractRegionTrans):
    ''' GOcean1.0 API application of ExtractRegionTrans transformation \
    to extract code into a stand-alone program. For example:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.psyGen import PSyFactory
    >>>
    >>> API = "gocean1.0"
    >>> FILENAME = "shallow_alg.f90"
    >>> ast, invokeInfo = parse(FILENAME, api=API)
    >>> psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    >>> schedule = psy.invokes.get('invoke_0').schedule
    >>>
    >>> from psyclone.transformations import GOceanExtractRegionTrans
    >>> etrans = GOceanExtractRegionTrans()
    >>>
    >>> # Apply GOceanExtractRegionTrans transformation to selected Nodes
    >>> newsched, _ = etrans.apply(schedule.children[0])
    >>> newsched.view()
    '''

    @property
    def name(self):
        ''' Returns the name of this transformation as a string.'''
        return "GOceanExtractRegionTrans"

    def _validate(self, node_list):
        ''' Perform GOcean1.0 API specific validation checks before applying
        the transformation.

        :param node_list: the list of Node(s) we are checking.
        :type node_list: list of :py:class:`psyclone.psyGen.Node`.

        :raises TransformationError: if transformation is applied to an \
                                     inner Loop without its parent outer \
                                     Loop.
        '''

        # First check constraints on Nodes in the node_list inherited from
        # the parent classes (ExtractRegionTrans and RegionTrans)
        super(GOceanExtractRegionTrans, self)._validate(node_list)

        # Check GOceanExtractRegionTrans specific constraints
        from psyclone.gocean1p0 import GOLoop
        for node in node_list:

            # Check that ExtractNode is not inserted between an inner
            # and an outer Loop.
            ancestor = node.ancestor(GOLoop)
            if ancestor and ancestor.loop_type == 'outer':
                raise TransformationError(
                    "Error in {0} for GOcean1.0 API: Extraction of an "
                    "inner Loop without its ancestor outer Loop is not "
                    "allowed.".format(str(self.name)))
