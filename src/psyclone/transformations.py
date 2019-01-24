# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2018, Science and Technology Facilities Council
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
# Authors R. W. Ford and A. R. Porter, STFC Daresbury Lab
#        J. Henrichs, Bureau of Meteorology
# Modified I. Kavcic, Met Office

''' This module provides the various transformations that can
    be applied to the schedule associated with an invoke(). There
    are both general and API-specific transformation classes in
    this module where the latter typically apply API-specific
    checks before calling the base class for the actual
    transformation. '''

from __future__ import absolute_import, print_function
import abc
import six
from psyclone.psyGen import Transformation
from psyclone.configuration import Config

VALID_OMP_SCHEDULES = ["runtime", "static", "dynamic", "guided", "auto"]


class TransformationError(Exception):
    ''' Provides a PSyclone-specific error class for errors found during
        code transformation operations. '''

    def __init__(self, value):
        Exception.__init__(self, value)
        self.value = "Transformation Error: "+value

    def __str__(self):
        return repr(self.value)


# =============================================================================
class RegionTrans(Transformation):
    '''This class is a base class for all transforms that act on list of
    nodes. It gives access to a _validate function that makes sure that
    the nodes in the list are in the same order as in the original AST,
    no node is duplicated, and that all nodes have the same parent.
    '''

    # Avoid pylint warning about abstract functions (apply, name) not
    # overwritten:
    # pylint: disable=abstract-method,arguments-differ

    def _validate(self, node_list):
        '''Test if the nodes in node_list are in the original order.

        :param list node_list: List of nodes.
        :raises TransformationError: If the nodes in the list are not\
                in the original order in which they are in the AST,\
                a node is duplicated or the nodes have different parents.
        '''

        node_parent = node_list[0].parent
        prev_position = -1
        for child in node_list:
            if child.parent is not node_parent:
                raise TransformationError(
                    "Error in {0} transformation: supplied nodes "
                    "are not children of the same Schedule/parent."
                    .format(self.name))
            if prev_position >= 0 and prev_position+1 != child.position:
                raise TransformationError(
                    "Children are not consecutive children of one parent: "
                    "child '{0}' has position {1}, but previous child had "
                    "position {2}."
                    .format(str(child), child.position, prev_position))
            prev_position = child.position


# =============================================================================
def check_intergrid(node):
    '''
    Utility function to check that the supplied node does not have
    an intergrid kernel as a child.

    This is used to ensure that we reject any attempt to apply
    transformations to loops containing inter-grid kernels. (This restriction
    will be lifted in Issue #134 and this routine can then be removed.)

    # TODO remove this routine once #134 is complete.

    :param node: the Node in the Schedule to check
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

        >>> from psyclone.parse import parse
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
        from psyclone.undoredo import Memento
        keep = Memento(schedule, self, [node1, node2])

        # add loop contents of node2 to node1
        node1.children.extend(node2.children)

        # change the parent of the loop contents of node2 to node1
        for child in node2.children:
            child.parent = node1

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
        :returns: two-tuple of modified schedule and Memento
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
            from psyclone.psyGen import MAPPING_SCALARS, MAPPING_REDUCTIONS
            arg_types = MAPPING_SCALARS.values()
            arg_accesses = MAPPING_REDUCTIONS.values()
            node1_red_args = node1.args_filter(arg_types=arg_types,
                                               arg_accesses=arg_accesses)
            node2_red_args = node2.args_filter(arg_types=arg_types,
                                               arg_accesses=arg_accesses)

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
                cnode = cnode.children[0]
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
        :return: (:py:class:`psyclone.psyGen.Schedule`, \
                  :py:class:`psyclone.undoredo.Memento`)

        '''
        self._validate(node, collapse)

        schedule = node.root

        # create a memento of the schedule and the proposed
        # transformation
        from psyclone.undoredo import Memento
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

    >>> from psyclone.parse import parse, ParseError
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
        :return: (:py:class:`psyclone.psyGen.Schedule`, \
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

    >>> from psyclone.parse import parse, ParseError
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
                                     independent=self._independent)
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
        from psyclone.gocean1p0 import GOSchedule
        sched = node.root
        if not isinstance(sched, GOSchedule):
            raise NotImplementedError(
                "OpenACC loop transformations are currently only supported "
                "for the gocean 1.0 API")
        super(ACCLoopTrans, self)._validate(node, collapse)

    def apply(self, node, collapse=None, independent=True):
        '''
        Apply the ACCLoop transformation to the specified node in a
        Schedule. This node must be a Loop since this transformation
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
        :return: (:py:class:`psyclone.psyGen.Schedule`, \
                  :py:class:`psyclone.undoredo.Memento`)

        '''
        # Store sub-class specific options. These are used when
        # creating the directive (in the _directive() method).
        self._independent = independent
        # Call the apply() method of the base class
        return super(ACCLoopTrans, self).apply(node, collapse)


class OMPParallelLoopTrans(OMPLoopTrans):

    ''' Adds an OpenMP PARALLEL DO directive to a loop.

        For example:

        >>> from psyclone.parse import parse
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
        from psyclone.undoredo import Memento
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
        from psyclone.undoredo import Memento
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
        colour_loop = node.__class__(parent=colours_loop, loop_type="colour")
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
        colours_loop.addchild(colour_loop)

        # add contents of node to colour loop
        colour_loop.children.extend(node.children)

        # change the parent of the node's contents to the colour loop
        for child in node.children:
            child.parent = colour_loop

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
    >>> ischedule, _ = inline_trans.apply(schedule.children[0].children[0])
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
        from psyclone.undoredo import Memento
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

    >>> from psyclone.parse import parse
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
        :rtype: (:py:class:`psyclone.dynamo0p3.DynSchedule`, \
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
        from psyclone.psyGen import HaloExchange, Schedule
        for node in node_list:
            if isinstance(node, HaloExchange):
                raise TransformationError(
                    "A halo exchange within a parallel region is not "
                    "supported")

        if isinstance(node_list[0], Schedule):
            raise TransformationError(
                "A {0} transformation cannot be applied to a Schedule but "
                "only to one or more nodes from within a Schedule.".
                format(self.name))

        node_parent = node_list[0].parent

        for child in node_list:
            if child.parent is not node_parent:
                raise TransformationError(
                    "Error in {0} transformation: supplied nodes are not "
                    "children of the same Schedule/parent.".format(self.name))
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

        from psyclone.undoredo import Memento
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

    >>> from psyclone.parse import parse, ParseError
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
    region *must* come after an enter-data directive (see `ACCDataTrans`). For
    example:

    >>> from psyclone.parse import parse
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
        from psyclone.gocean1p0 import GOSchedule
        sched = node_list[0].root
        if not isinstance(sched, GOSchedule):
            raise NotImplementedError(
                "OpenACC parallel regions are currently only "
                "supported for the gocean 1.0 API")
        super(ACCParallelTrans, self)._validate(node_list)


class GOConstLoopBoundsTrans(Transformation):
    ''' Switch on (or off) the use of constant loop bounds within
    a GOSchedule. In the absence of constant loop bounds, PSyclone will
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

    >>> from psyclone.parse import parse
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
        return "Use constant loop bounds for all loops in a GOSchedule"

    @property
    def name(self):
        ''' Return the name of the Transformation as a string.'''
        return "GOConstLoopBoundsTrans"

    def apply(self, node, const_bounds=True):
        '''Switches constant loop bounds on or off for all loops in the
        schedule :py:obj:`node`. Default is 'on'.

        :param node: The schedule of which all loops will get the constant
            loop bounds switched on or off.
        :type node: :py:class:`psyclone.gocean1p0.GOSchedule`
        :param const_bounds: If the constant loop should be used (True)
            or not (False). Default is True.
        :type const_bounds: bool
        '''

        # Check node is a Schedule
        from psyclone.gocean1p0 import GOSchedule
        if not isinstance(node, GOSchedule):
            raise TransformationError("Error in GOConstLoopBoundsTrans: "
                                      "node is not a GOSchedule")

        from psyclone.undoredo import Memento
        keep = Memento(node, self)

        node.const_loop_bounds = const_bounds

        return node, keep


class MoveTrans(Transformation):
    '''Provides a transformation to move a node in the tree. For
    example:

    >>> from psyclone.parse import parse
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

        # create a memento of the schedule and the proposed transformation
        from .undoredo import Memento
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
        :py:class:`psyclone.psyGen.Schedule`
        :raises GenerationError: if the parent of the loop is a
        :py:class:`psyclone.psyGen.Loop` but the original loop does
        not iterate over 'colour'
        :raises GenerationError: if the parent of the loop is a
        :py:class:`psyclone.psyGen.Loop` but the parent does not
        iterate over 'colours'
        :raises GenerationError: if the parent of the loop is a
        :py:class:`psyclone.psyGen.Loop` but the parent's parent is
        not a :py:class:`psyclone.psyGen.Schedule`
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
        from psyclone.psyGen import Loop
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
        from psyclone.psyGen import Schedule
        if not (isinstance(node.parent, Schedule) or
                (isinstance(node.parent, Loop))):
            from psyclone.psyGen import Directive
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
                    "the Schedule, or a Loop, but found {0}".
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
            if not isinstance(node.parent.parent, Schedule):
                raise TransformationError(
                    "In the Dynamo0p3RedundantComputation transformation "
                    "apply method, if the parent of the supplied Loop is "
                    "also a Loop then the parent's parent must be the "
                    "Schedule, but found {0}".format(type(node.parent)))
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
                for call in node.calls():
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
        from psyclone.undoredo import Memento
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

     >>> from parse import parse
     >>> from psyGen import PSyFactory
     >>> ast,invokeInfo=parse("shallow_alg.f90")
     >>> psy=PSyFactory("gocean1.0").create(invokeInfo)
     >>> schedule=psy.invokes.get('invoke_0').schedule
     >>> schedule.view()
     >>>
     >>> from transformations import GOLoopSwapTrans
     >>> swap=GOLoopSwapTrans()
     >>> new_schedule,memento=swap.apply(schedule.children[0])
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

        if len(node_outer.children) == 0:
            raise TransformationError("Error in GOLoopSwap transformation. "
                                      "Supplied node '{0}' must be the outer "
                                      "loop of a loop nest and must have one "
                                      "inner loop, but this node does not "
                                      "have any statements inside."
                                      .format(node_outer))

        node_inner = node_outer.children[0]
        # Check that the supplied Node is a Loop
        if not isinstance(node_inner, Loop):
            raise TransformationError("Error in GOLoopSwap transformation. "
                                      "Supplied node '{0}' must be the outer "
                                      "loop of a loop nest but the first "
                                      "inner statement is not a loop, got "
                                      "'{1}'."
                                      .format(node_outer, node_inner))

        if len(node_outer.children) > 1:
            raise TransformationError("Error in GOLoopSwap transformation. "
                                      "Supplied node '{0}' must be the outer "
                                      "loop of a loop nest and must have "
                                      "exactly one inner loop, but this node "
                                      "has {1} inner statements, the first "
                                      "two being '{2}' and '{3}'"
                                      .format(node_outer,
                                              len(node_outer.children),
                                              node_outer.children[0],
                                              node_outer.children[1]))

    def apply(self, outer):  # pylint: disable=arguments-differ
        '''The argument :py:obj:`outer` must be a loop which has exactly
        one inner loop. This transform then swaps the outer and inner loop.

        :param outer: The node representing the outer loop.
        :type outer: :py:class:`psyclone.psyGen.Loop`
        :return: A tuple consistent of the new schedule, and a Memento.
        :raises TransformationError: if the supplied node does not
                                        allow a loop swap to be done.'''
        self._validate(outer)

        schedule = outer.root
        inner = outer.children[0]
        parent = outer.parent

        # create a memento of the schedule and the proposed transformation
        from psyclone.undoredo import Memento
        keep = Memento(schedule, self, [inner, outer])

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


class ProfileRegionTrans(RegionTrans):

    ''' Create a profile region around a list of statements. For
    example:

    >>> from psyclone.parse import parse, ParseError
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

        from psyclone.undoredo import Memento
        keep = Memento(schedule, self)

        from psyclone.profiler import ProfileNode
        profile_node = ProfileNode(parent=node_parent, children=node_list[:])

        # Change all of the affected children so that they have
        # the ProfileNode astheir parent. Use a slice
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

    >>> from psyclone.parse import parse
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
        from psyclone.undoredo import Memento
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


class ACCDataTrans(Transformation):
    '''
    Adds an OpenACC "enter data" directive to a Schedule.
    For example:

    >>> from psyclone.parse import parse
    >>> from psyclone.psyGen import PSyFactory
    >>> api = "gocean1.0"
    >>> filename = "nemolite2d_alg.f90"
    >>> ast, invokeInfo = parse(filename, api=api, invoke_name="invoke")
    >>> psy = PSyFactory(api).create(invokeInfo)
    >>>
    >>> from psyclone.psyGen import TransInfo
    >>> t = TransInfo()
    >>> dtrans = t.get_trans_name('ACCDataTrans')
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
        return "ACCDataTrans"

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
        :raises NotImplementedError: for any API other than GOcean 1.0.
        :raises TransformationError: if passed something that is not a \
                         (subclass of) :py:class:`psyclone.psyGen.Schedule`.
        '''
        # Check that the supplied node is a Schedule
        from psyclone.psyGen import Schedule
        from psyclone.gocean1p0 import GOSchedule

        if isinstance(sched, GOSchedule):
            from psyclone.gocean1p0 import GOACCDataDirective as AccDataDir
        elif isinstance(sched, Schedule):
            raise NotImplementedError(
                "ACCDataTrans: ACCDataDirective not implemented for a "
                "schedule of type {0}".format(type(sched)))
        else:
            raise TransformationError("Cannot apply an OpenACC data "
                                      "directive to something that is "
                                      "not a Schedule")
        schedule = sched
        # Check that we don't already have a data region
        data_dir = schedule.walk(schedule.children, AccDataDir)
        if len(data_dir) != 0:
            raise TransformationError("Schedule already has an OpenACC "
                                      "data region - cannot add another.")
        # create a memento of the schedule and the proposed
        # transformation
        from psyclone.undoredo import Memento
        keep = Memento(schedule, self, [schedule])

        # Add the directive
        data_dir = AccDataDir(parent=schedule, children=[])
        schedule.addchild(data_dir, index=0)

        return schedule, keep


class ACCRoutineTrans(Transformation):
    '''
    Transform a kernel subroutine by adding a "!$acc routine" directive
    (causing it to be compiled for the OpenACC accelerator device).
    For example:

    >>> from psyclone.parse import parse
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
        :type kern: :py:class:`psyclone.psyGen.Call`
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
        from psyclone.undoredo import Memento
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
        :type kern: :py:class:`psyclone.psyGen.Call`

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
