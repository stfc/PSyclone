# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2020, Science and Technology Facilities Council.
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
from psyclone.psyGen import Transformation, Kern, InvokeSchedule
from psyclone.errors import InternalError
from psyclone.psyir.nodes import Schedule
from psyclone.configuration import Config
from psyclone.undoredo import Memento
from psyclone.dynamo0p3 import VALID_ANY_SPACE_NAMES, \
    VALID_ANY_DISCONTINUOUS_SPACE_NAMES
from psyclone.psyir.transformations import RegionTrans, TransformationError
from psyclone.psyir.symbols import SymbolError, ScalarType, DeferredType, \
    INTEGER_TYPE, DataSymbol

VALID_OMP_SCHEDULES = ["runtime", "static", "dynamic", "guided", "auto"]


def check_intergrid(node):
    '''
    Utility function to check that the supplied node does not have
    an intergrid kernel amongst its descendants.

    This is used ensure any attempt to apply loop-fusion and redundant-
    computation transformations to loops containing inter-grid kernels is
    rejected (since support for those is not yet implemented).

    :param node: the PSyIR node to check.
    :type node: :py:class:`psyir.nodes.Node`

    :raises TransformationError: if the supplied node has an inter-grid \
                                 kernel as a descendant.

    '''
    if not node.children:
        return
    from psyclone.dynamo0p3 import DynKern
    child_kernels = node.walk(DynKern)
    for kern in child_kernels:
        if kern.is_intergrid:
            raise TransformationError(
                "This Transformation cannot currently be applied to nodes "
                "which have inter-grid kernels as descendents and {0} is "
                "such a kernel.".format(kern.name))


class KernelTrans(Transformation):
    '''
    Base class for all Kernel transformations.

    '''
    @staticmethod
    def validate(kern, options=None):
        '''
        Checks that the supplied node is a Kernel and that it is possible to
        construct the PSyIR of its contents.

        :param kern: the kernel which is the target of the transformation.
        :type kern: :py:class:`psyclone.psyGen.Kern` or sub-class
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :raises TransformationError: if the target node is not a sub-class of \
                                     psyGen.Kern.
        :raises TransformationError: if the subroutine containing the \
                                     implementation of the kernel cannot be \
                                     found in the fparser2 Parse Tree.
        :raises TransformationError: if the PSyIR cannot be constructed \
                                     because there are symbols of unknown type.

        '''
        from psyclone.errors import GenerationError

        if not isinstance(kern, Kern):
            raise TransformationError(
                "Target of a kernel transformation must be a sub-class of "
                "psyGen.Kern but got '{0}'".format(type(kern).__name__))

        # Check that the PSyIR and associated Symbol table of the Kernel is OK.
        # If this kernel contains symbols that are not captured in the PSyIR
        # SymbolTable then this raises an exception.
        try:
            kernel_schedule = kern.get_kernel_schedule()
        except GenerationError:
            raise TransformationError(
                "Failed to find subroutine source for kernel {0}".
                format(kern.name))
        except SymbolError as err:
            raise TransformationError(
                "Kernel '{0}' contains accesses to data that are not captured "
                "in the PSyIR Symbol Table(s) ({1}). Cannot transform such a "
                "kernel.".format(kern.name, str(err.args[0])))
        # Check that all kernel symbols are declared in the kernel
        # symbol table(s). At this point they may be declared in a
        # container containing this kernel which is not supported.
        from psyclone.psyir.nodes import Reference
        from psyclone.psyGen import KernelSchedule
        for var in kernel_schedule.walk(Reference):
            try:
                _ = var.find_or_create_symbol(
                    var.name, scope_limit=var.ancestor(KernelSchedule))
            except SymbolError:
                raise TransformationError(
                    "Kernel '{0}' contains accesses to data (variable '{1}') "
                    "that are not captured in the PSyIR Symbol Table(s) "
                    "within KernelSchedule scope. Cannot transform such a "
                    "kernel.".format(kern.name, var.name))


class LoopFuseTrans(Transformation):
    ''' Provides a generic loop-fuse transformation to two Nodes in the
    PSyIR of a Schedule after performing validity checks for the supplied
    Nodes. Examples are given in the descriptions of any children classes.
    '''

    def __str__(self):
        return "Fuse two adjacent loops together"

    @property
    def name(self):
        ''' Returns the name of this transformation as a string.'''
        return "LoopFuse"

    def validate(self, node1, node2, options=None):
        # pylint: disable=arguments-differ
        ''' Performs various checks to ensure that it is valid to apply
        the LoopFuseTrans transformation to the supplied Nodes.

        :param node1: the first Node that is being checked.
        :type node1: :py:class:`psyclone.psyir.nodes.Node`
        :param node2: the second Node that is being checked.
        :type node2: :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :raises TransformationError: if one or both of the Nodes is/are not \
                                     a :py:class:`psyclone.psyir.nodes.Loop`.
        :raises TransformationError: if one or both Nodes are not fully-formed.
        :raises TransformationError: if the Nodes do not have the same parent.
        :raises TransformationError: if the Nodes are not next to each \
                                     other in the tree.
        :raises TransformationError: if the two Loops do not have the same \
                                     iteration space.
        '''

        # Check that the supplied Node is a Loop
        from psyclone.psyir.nodes import Loop
        if not isinstance(node1, Loop) or not isinstance(node2, Loop):
            raise TransformationError("Error in {0} transformation. "
                                      "At least one of the nodes is not "
                                      "a loop.".format(self.name))

        # If they are loops, they must be fully-formed.
        if len(node1.children) != 4:
            raise TransformationError(
                "Error in {0} transformation. The first loop "
                "does not have 4 children.".format(self.name))

        if len(node2.children) != 4:
            raise TransformationError(
                "Error in {0} transformation. The second loop "
                "does not have 4 children.".format(self.name))

        # Check loop1 and loop2 have the same parent
        if not node1.sameParent(node2):
            raise TransformationError(
                "Error in {0} transformation. Loops do not have "
                "the same parent.".format(self.name))

        # Check node1 and node2 are next to each other
        if abs(node1.position-node2.position) != 1:
            raise TransformationError(
                "Error in {0} transformation. Nodes are not siblings "
                "who are next to each other.".format(self.name))
        # Check that the iteration space is the same
        if node1.iteration_space != node2.iteration_space:
            raise TransformationError(
                "Error in {0} transformation. Loops do not have the "
                "same iteration space.".format(self.name))

    def apply(self, node1, node2, options=None):
        # pylint: disable=arguments-differ
        ''' Fuses two loops represented by `psyclone.psyir.nodes.Node` objects
        after performing validity checks.

        :param node1: the first Node that is being checked.
        :type node1: :py:class:`psyclone.psyir.nodes.Node`
        :param node2: the second Node that is being checked.
        :type node2: :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :returns: two-tuple of the modified Schedule and a record of \
                  the transformation.
        :rtype: (:py:class:`psyclone.psyir.nodes.Schedule`, \
                 :py:class:`psyclone.undoredo.Memento`).
        '''

        # Validity checks for the supplied nodes
        self.validate(node1, node2, options)

        schedule = node1.root

        # Create a memento of the schedule and the proposed transformation
        keep = Memento(schedule, self, [node1, node2])

        # Add loop contents of node2 to node1
        node1.loop_body.children.extend(node2.loop_body)

        # Change the parent of the loop contents of node2 to node1
        for child in node2.loop_body:
            child.parent = node1.loop_body

        # Remove node2
        node2.parent.children.remove(node2)

        return schedule, keep


class GOceanLoopFuseTrans(LoopFuseTrans):
    ''' GOcean API specialisation of the :py:class:`base class <LoopFuseTrans>`
    in order to fuse two GOcean loops after performing validity checks (e.g.
    that the loops are over the same grid-point type). For example:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.psyGen import PSyFactory
    >>> ast, invokeInfo = parse("shallow_alg.f90")
    >>> psy = PSyFactory("gocean1.0").create(invokeInfo)
    >>> schedule = psy.invokes.get('invoke_0').schedule
    >>> schedule.view()
    >>>
    >>> from psyclone.transformations import GOceanLoopFuseTrans
    >>> ftrans = GOceanLoopFuseTrans()
    >>> new_schedule, memento = ftrans.apply(schedule[0], schedule[1])
    >>> new_schedule.view()
    '''

    def __str__(self):
        return ("Fuse two adjacent loops together with GOcean-specific "
                "validity checks")

    @property
    def name(self):
        ''' Returns the name of this transformation as a string.'''
        return "GOceanLoopFuse"

    def validate(self, node1, node2, options=None):
        '''Checks if it is valid to apply the GOceanLoopFuseTrans
        transform. It ensures that the fused loops are over
        the same grid-point types, before calling the normal
        LoopFuseTrans validation function.

        :param node1: the first Node representing a GOLoop.
        :type node1: :py:class:`psyclone.gocean1p0.GOLoop`
        :param node2: the second Node representing a GOLoop.
        :type node2: :py:class:`psyclone.gocean1p0.GOLoop`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :raises TransformationError: if the supplied loops are over \
                                     different grid-point types.

        :raises TransformationError: if invalid parameters are passed in.

        '''
        import psyclone.gocean1p0
        import psyclone.gocean0p1
        # Either both nodes are gocean1.0 loop nodes, or both
        # nodes are gocean0.1 loop nodes, otherwise raise an exception:
        if not ((isinstance(node1, psyclone.gocean0p1.GOLoop) and
                 isinstance(node2, psyclone.gocean0p1.GOLoop)) or
                (isinstance(node1, psyclone.gocean1p0.GOLoop) and
                 isinstance(node2, psyclone.gocean1p0.GOLoop))):
            raise TransformationError("Error in {0} transformation. "
                                      "Both nodes must be of the same "
                                      "GOLoop class.".format(self.name))

        super(GOceanLoopFuseTrans, self).validate(node1, node2, options)

        if node1.field_space != node2.field_space:
            raise TransformationError(
                "Error in {0} transformation. Cannot "
                "fuse loops that are over different grid-point types: "
                "{1} {2}".format(self.name, node1.field_space,
                                 node2.field_space))

    def apply(self, node1, node2, options=None):
        ''' Fuses two `psyclone.gocean1p0.GOLoop` loops after performing
        validity checks by calling :py:meth:`LoopFuseTrans.apply` method
        of the base class.

        :param node1: the first Node representing a GOLoop.
        :type node1: :py:class:`psyclone.gocean1p0.GOLoop`
        :param node2: the second Node representing a GOLoop.
        :type node2: :py:class:`psyclone.gocean1p0.GOLoop`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :returns: two-tuple of the modified Schedule and a record of \
                  the transformation.
        :rtype: (:py:class:`psyclone.psyir.nodes.Schedule`, \
                 :py:class:`psyclone.undoredo.Memento`)

        :raises TransformationError: if the supplied loops are over \
                                     different grid-point types.
        :raises TransformationError: if there is an unexpected exception.
        '''

        # Validate first
        self.validate(node1, node2, options)

        # Now check for GOcean-specific constraints before applying
        # the transformation
        try:
            return LoopFuseTrans.apply(self, node1, node2, options)
        except Exception as err:
            raise TransformationError(
                "Error in {0} transformation. Unexpected exception: {1}".
                format(self.name, err))


class DynamoLoopFuseTrans(LoopFuseTrans):
    ''' Dynamo0.3 API specialisation of the
    :py:class:`base class <LoopFuseTrans>` in order to fuse two Dynamo
    loops after performing validity checks. For example:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.psyGen import PSyFactory
    >>>
    >>> API = "dynamo0.3"
    >>> FILENAME = "alg.x90"
    >>> ast, invokeInfo = parse(FILENAME, api=API)
    >>> psy = PSyFactory(API, distributed_memory=False).create(invoke_info)
    >>> schedule = psy.invokes.get('invoke_0').schedule
    >>>
    >>> from psyclone.transformations import DynamoLoopFuseTrans
    >>> ftrans =  DynamoLoopFuseTrans()
    >>>
    >>> new_schedule, memento = ftrans.apply(schedule[0], schedule[1])
    >>> new_schedule.view()

    The optional argument `same_space` can be set as

    >>> ftrans.same_space = True

    after the instance of the transformation is created.
    '''

    def __init__(self, same_space=False):
        # Creates the 'same_space' attribute. Its value is set in via
        # the setter method below.
        # TODO: Remove when the suport for multiple options in
        # Transformations is introduced (issue #478)
        self._same_space = same_space

    def __str__(self):
        return ("Fuse two adjacent loops together with Dynamo-specific "
                "validity checks")

    @property
    def name(self):
        ''' Returns the name of this transformation as a string.'''
        return "DynamoLoopFuse"

    # TODO: Remove the 'same_space' property and the setter below and
    # reformulate the relevant tests and documentation when the suport for
    # multiple options in Transformations is introduced (issue #478)
    @property
    def same_space(self):
        ''' Returns the `same_space` flag that is specified when applying
        this transformation. The default value is `False`.
        This optional flag, set to `True`, asserts that an unknown iteration
        space (i.e. `ANY_SPACE`) matches the other iteration space. This is
        set at the user's own risk. If both iteration spaces are discontinuous
        the loops can be fused without having to use the `same_space` flag.'''
        return self._same_space

    @same_space.setter
    def same_space(self, value):
        ''' Sets value of the `same_space` flag and checks that the
        supplied value is Boolean or None.

        :param value: optional argument to determine whether two unknown \
                      function spaces are the same. The default value is \
                      False (also when no value is provided).
        :type value: Boolean or None

        :raises TransformationError: if the provided value is not Boolean \
                                     or None.
        '''

        if not value:
            self._same_space = False
        elif isinstance(value, bool):
            self._same_space = value
        else:
            raise TransformationError(
                "Error in {0} transformation: The value of the 'same_space' "
                "flag must be either Boolean or None type, but the type of "
                "flag provided was '{1}'.".
                format(self.name, type(value).__name__))

    def validate(self, node1, node2, options=None):
        ''' Performs various checks to ensure that it is valid to apply
        the DynamoLoopFuseTrans transformation to the supplied loops.

        :param node1: the first Loop to fuse.
        :type node1: :py:class:`psyclone.dynamo0p3.DynLoop`
        :param node2: the second Loop to fuse.
        :type node2: :py:class:`psyclone.dynamo0p3.DynLoop`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :raises TransformationError: if either of the supplied loops contains \
                                     an inter-grid kernel.
        :raises TransformationError: if one or both function spaces have \
                                     invalid names.
        :raises TransformationError: if the `same_space` flag was set, but \
                                     does not apply because neither field \
                                     is on `ANY_SPACE` or the spaces are not \
                                     the same.
        :raises TransformationError: if one or more of the iteration spaces \
                                     is unknown (`ANY_SPACE`) and the \
                                     `same_space` flag is not set to `True`.
        :raises TransformationError: if the loops are over different spaces \
                                     that are not both discontinuous and \
                                     the loops both iterate over cells.
        :raises TransformationError: if the loops' upper bound names are \
                                     not the same.
        :raises TransformationError: if the halo-depth indices of two loops \
                                     are not the same.
        :raises TransformationError: if each loop already contains a reduction.
        :raises TransformationError: if the first loop has a reduction and \
                                     the second loop reads the result of \
                                     the reduction.
        '''
        # pylint: disable=too-many-locals,too-many-branches
        # Call the parent class validation first
        super(DynamoLoopFuseTrans, self).validate(node1, node2, options)

        # Now test for Dynamo-specific constraints

        from psyclone.dynamo0p3 import VALID_FUNCTION_SPACE_NAMES, \
            VALID_DISCONTINUOUS_FUNCTION_SPACE_NAMES

        # 1) Check that we don't have an inter-grid kernel
        check_intergrid(node1)
        check_intergrid(node2)

        # 2) Check function space names
        node1_fs_name = node1.field_space.orig_name
        node2_fs_name = node2.field_space.orig_name
        # 2.1) Check that both function spaces are valid
        if not (node1_fs_name in VALID_FUNCTION_SPACE_NAMES and
                node2_fs_name in VALID_FUNCTION_SPACE_NAMES):
            raise TransformationError(
                "Error in {0} transformation: One or both function "
                "spaces '{1}' and '{2}' have invalid names.".
                format(self.name, node1_fs_name, node2_fs_name))
        # Check whether any of the spaces is ANY_SPACE. Loop fusion over
        # ANY_SPACE is allowed only when the 'same_space' flag is set
        node_on_any_space = node1_fs_name in VALID_ANY_SPACE_NAMES or \
            node2_fs_name in VALID_ANY_SPACE_NAMES
        # 2.2) If 'same_space' is true check that both function spaces are
        # the same or that at least one of the nodes is on ANY_SPACE. The
        # former case is convenient when loop fusion is applied generically.
        if self.same_space:
            if node1_fs_name == node2_fs_name:
                pass
            elif not node_on_any_space:
                raise TransformationError(
                    "Error in {0} transformation: The 'same_space' "
                    "flag was set, but does not apply because "
                    "neither field is on 'ANY_SPACE'.".format(self))
        # 2.3) If 'same_space' is not True then make further checks
        else:
            # 2.3.1) Check whether one or more of the function spaces
            # is ANY_SPACE without the 'same_space' flag
            if node_on_any_space:
                raise TransformationError(
                    "Error in {0} transformation: One or more of the "
                    "iteration spaces is unknown ('ANY_SPACE') so loop "
                    "fusion might be invalid. If you know the spaces "
                    "are the same then please set the 'same_space' "
                    "optional argument to 'True'.".format(self.name))
            # 2.3.2) Check whether specific function spaces are the
            # same. If they are not, the loop fusion is still possible
            # but only when both function spaces are discontinuous
            # (w3, w2v, wtheta or any_discontinuous_space) and the upper
            # loop bounds are the same (checked further below).
            if node1_fs_name != node2_fs_name:
                if not (node1_fs_name in
                        VALID_DISCONTINUOUS_FUNCTION_SPACE_NAMES and
                        node2_fs_name in
                        VALID_DISCONTINUOUS_FUNCTION_SPACE_NAMES):
                    raise TransformationError(
                        "Error in {0} transformation: Cannot fuse loops "
                        "that are over different spaces '{1}' and '{2}' "
                        "unless they are both discontinuous.".
                        format(self.name, node1_fs_name,
                               node2_fs_name))

        # 3) Check upper loop bounds
        if node1.upper_bound_name != node2.upper_bound_name:
            raise TransformationError(
                "Error in {0} transformation: The upper bound names "
                "are not the same. Found '{1}' and '{2}'.".
                format(self.name, node1.upper_bound_name,
                       node2.upper_bound_name))

        # 4) Check halo depths
        if node1.upper_bound_halo_depth != node2.upper_bound_halo_depth:
            raise TransformationError(
                "Error in {0} transformation: The halo-depth indices "
                "are not the same. Found '{1}' and '{2}'.".
                format(self.name, node1.upper_bound_halo_depth,
                       node2.upper_bound_halo_depth))

        # 5) Check for reductions
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
                "Error in {0} transformation: Cannot fuse loops "
                "when each loop already contains a reduction.".
                format(self.name))
        if node1_red_args:
            for reduction_arg in node1_red_args:
                other_args = node2.args_filter()
                for arg in other_args:
                    if reduction_arg.name == arg.name:
                        raise TransformationError(
                            "Error in {0} transformation: Cannot fuse "
                            "loops as the first loop has a reduction "
                            "and the second loop reads the result of "
                            "the reduction.".format(self.name))

    def apply(self, node1, node2, options=None):
        ''' Fuses two `psyclone.dynamo0p3.DynLoop` loops after performing
        validity checks by calling :py:meth:`LoopFuseTrans.apply` method
        of the base class.

        :param node1: the first Loop to fuse.
        :type node1: :py:class:`psyclone.dynamo0p3.DynLoop`
        :param node2: the second Loop to fuse.
        :type node2: :py:class:`psyclone.dynamo0p3.DynLoop`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :returns: two-tuple of the modified Schedule and a record of \
                  the transformation.
        :rtype: (:py:class:`psyclone.psyir.nodes.Schedule`, \
                 :py:class:`psyclone.undoredo.Memento`)
        '''

        # Validity checks for the supplied nodes
        self.validate(node1, node2, options)

        # Apply fuse method from the parent class
        return super(DynamoLoopFuseTrans, self).apply(node1, node2, options)


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
        :type parent: :py:class:`psyclone.psyir.nodes.Node`
        :param children: list of nodes that will be children of this Directive.
        :type children: list of :py:class:`psyclone.psyir.nodes.Node`
        :param int collapse: the number of tightly-nested loops to which \
                             this directive applies or None.

        :returns: the new Directive node.
        :rtype: sub-class of :py:class:`psyclone.psyGen.Directive`.
        '''

    def validate(self, node, options=None):
        '''
        Perform validation checks before applying the transformation

        :param node: the node we are checking.
        :type node: :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.\
                        This transform supports "collapse", which is the\
                        number of nested loops to collapse.
        :type options: dictionary of string:values or None
        :param int options["collapse"]: number of nested loops to collapse \
                                        or None.

        :raises TransformationError: if the node is not a \
                :py:class:`psyclone.psyir.nodes.Loop`.
        :raises TransformationError: if the \
                :py:class:`psyclone.psyir.nodes.Loop` loop iterates over \
                colours.

        '''
        # Check that the supplied node is a Loop
        from psyclone.psyir.nodes import Loop
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
        if not options:
            options = {}
        collapse = options.get("collapse", None)

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

        :param node: the supplied node to which we will apply the \
                     Loop transformation.
        :type node: :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations. \
        :type options: dictionary of string:values or None
        :param int options["collapse"]: the number of loops to collapse into \
                single iteration space or None.

        :returns: (:py:class:`psyclone.psyir.nodes.Schedule`, \
                   :py:class:`psyclone.undoredo.Memento`)

        '''
        if not options:
            options = {}
        self.validate(node, options)

        schedule = node.root

        collapse = options.get("collapse", None)

        # create a memento of the schedule and the proposed
        # transformation
        keep = Memento(schedule, self, [node, options])

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

        # Change the node's parent to be the loop directive's Schedule.
        node.parent = directive.dir_body

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

    :param str omp_schedule: the OpenMP schedule to use.

    For example:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.parse.utils import ParseError
    >>> from psyclone.psyGen import PSyFactory
    >>> from psyclone.errors import GenerationError
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
            if value_parts[1].strip() == "":
                raise TransformationError("Supplied OpenMP schedule '{0}'"
                                          " has missing chunk-size.".
                                          format(value))

        self._omp_schedule = value

    def _directive(self, parent, children, collapse=None):
        '''
        Creates the type of directive needed for this sub-class of
        transformation.

        :param parent: the Node that will be the parent of the created \
                       directive Node.
        :param children: list of Nodes that will be the children of \
                         the created directive.
        :type children: list of :py:class:`psyclone.psyir.nodes.Node`
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

    def apply(self, node, options=None):
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

        If the keyword "reprod" is specified in the options, it will cause a
        reproducible reduction to be generated if it is set to True, otherwise
        the default value (as read from the psyclone.cfg file) will be used.
        Note, reproducible in this case means obtaining the same results
        with the same number of OpenMP threads, not for different
        numbers of OpenMP threads.

        :param node: the supplied node to which we will apply the \
                     OMPLoopTrans transformation
        :type node: :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations\
                        and validation.
        :type options: dictionary of string:values or None
        :param bool options["reprod"]:
                indicating whether reproducible reductions should be used. \
                By default the value from the config file will be used.

        :returns: (:py:class:`psyclone.psyir.nodes.Schedule`, \
        :py:class:`psyclone.undoredo.Memento`)

        '''
        from psyclone.nemo import NemoInvokeSchedule

        if not options:
            options = {}
        self._reprod = options.get("reprod",
                                   Config.get().reproducible_reductions)

        # Add variable names for OMP functions into the InvokeSchedule (root)
        # symboltable if they don't already exist
        if not isinstance(node.root, NemoInvokeSchedule):
            symtab = node.root.symbol_table
            try:
                symtab.lookup_with_tag("omp_thread_index")
            except KeyError:
                thread_idx = symtab.new_symbol_name("th_idx")
                symtab.add(DataSymbol(thread_idx, INTEGER_TYPE),
                           tag="omp_thread_index")
            try:
                symtab.lookup_with_tag("omp_num_threads")
            except KeyError:
                nthread = symtab.new_symbol_name("nthreads")
                symtab.add(DataSymbol(nthread, INTEGER_TYPE),
                           tag="omp_num_threads")

        return super(OMPLoopTrans, self).apply(node, options)


class ACCLoopTrans(ParallelLoopTrans):
    '''
    Adds an OpenACC loop directive to a loop. This directive must be within
    the scope of some OpenACC Parallel region (at code-generation time).

    For example:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.parse.utils import ParseError
    >>> from psyclone.psyGen import PSyFactory
    >>> from psyclone.errors import GenerationError
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
        :type parent: :py:class:`psyclone.psyir.nodes.Node`
        :param children: list of child nodes of the new directive Node.
        :type children: list of :py:class:`psyclone.psyir.nodes.Node`
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

    def apply(self, node, options=None):
        '''
        Apply the ACCLoop transformation to the specified node. This node
        must be a Loop since this transformation corresponds to
        inserting a directive immediately before a loop, e.g.:

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
        :type node: :py:class:`psyclone.psyir.nodes.Loop`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None
        :param int options["collapse"]: number of nested loops to collapse.
        :param bool options["independent"]: whether to add the "independent" \
                clause to the directive (not strictly necessary within \
                PARALLEL regions).

        :returns: 2-tuple of new schedule and memento of transform
        :rtype: (:py:class:`psyclone.dynamo0p3.DynInvokeSchedule`, \
                 :py:class:`psyclone.undoredo.Memento`)

        '''
        # Store sub-class specific options. These are used when
        # creating the directive (in the _directive() method).
        if not options:
            options = {}
        self._independent = options.get("independent", True)
        self._sequential = options.get("sequential", False)

        # Call the apply() method of the base class
        return super(ACCLoopTrans, self).apply(node, options)


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

    def validate(self, node, options=None):
        '''Validity checks for input arguments.

        :param node: the PSyIR node to validate.
        :type node: :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :raises TransformationError: if the nodes is not a Loop.
        :raises TransformationError: if the nodes is over colours.

         '''
        # Check that the supplied Node is a Loop
        from psyclone.psyir.nodes import Loop
        if not isinstance(node, Loop):
            raise TransformationError("Error in {0} transformation. The "
                                      "node is not a loop.".format(self.name))

        # Check we are not a sequential loop
        if node.loop_type == 'colours':
            raise TransformationError("Error in "+self.name+" transformation. "
                                      "The requested loop is over colours and "
                                      "must be computed serially.")
        super(OMPParallelLoopTrans, self).validate(node, options)

    def apply(self, node, options=None):
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
        :param options: a dictionary with options for transformations\
                        and validation.
        :type options: dictionary of string:values or None

        :returns: two-tuple of transformed schedule and a record of the \
                  transformation.
        :rtype: (:py:class:`psyclone.psyir.nodes.Schedule, \
                 :py:class:`psyclone.undoredo.Memento`)
        '''
        self.validate(node, options)

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

        # change the node's parent to be the Schedule of the loop directive
        node.parent = directive.dir_body

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

    def apply(self, node, options=None):

        '''Perform Dynamo specific loop validity checks then call the
        :py:meth:`~OMPParallelLoopTrans.apply` method of the
        :py:class:`base class <OMPParallelLoopTrans>`.

        :param node: the Node in the Schedule to check
        :type node: :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :raises TransformationError: if the associated loop requires \
                colouring.

        :returns: 2-tuple of new schedule and memento of transform.
        :rtype: (:py:class:`psyclone.dynamo0p3.DynInvokeSchedule`, \
                 :py:class:`psyclone.undoredo.Memento`)

        '''
        self.validate(node, options)

        # If the loop is not already coloured then check whether or not
        # it should be. If the field space is discontinuous (including
        # any_discontinuous_space) then we don't need to worry about
        # colouring.
        from psyclone.dynamo0p3 import VALID_DISCONTINUOUS_FUNCTION_SPACE_NAMES
        if node.field_space.orig_name not in \
           VALID_DISCONTINUOUS_FUNCTION_SPACE_NAMES:
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

       :param omp_schedule: the omp schedule to be created. Must be one of
           'runtime', 'static', 'dynamic', 'guided' or 'auto'.
       '''

    @property
    def name(self):
        ''' Returns the name of this transformation as a string.'''
        return "GOceanOMPParallelLoopTrans"

    def __str__(self):
        return "Add an OpenMP Parallel Do directive to a GOcean loop"

    def apply(self, node, options=None):
        ''' Perform GOcean-specific loop validity checks then call
        :py:meth:`OMPParallelLoopTrans.apply`.

        :param node: a Loop node from an AST.
        :type node: :py:class:`psyclone.psyir.nodes.Loop`
        :param options: a dictionary with options for transformations\
                        and validation.
        :type options: dictionary of string:values or None

        :raises TransformationError: if the supplied node is not an inner or\
            outer loop.

        :returns: 2-tuple of new schedule and memento of transform
        :rtype: (:py:class:`psyclone.dynamo0p3.GOInvokeSchedule`, \
                 :py:class:`psyclone.undoredo.Memento`)

        '''
        self.validate(node, options)

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

    def apply(self, node, options=None):
        '''Perform Dynamo 0.3 specific loop validity checks then call
        :py:meth:`OMPLoopTrans.apply`.

        :param node: the Node in the Schedule to check
        :type node: :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations \
                        and validation.
        :type options: dictionary of string:values or None
        :param bool options["reprod"]:
                indicating whether reproducible reductions should be used. \
                By default the value from the config file will be used.

        :raise TransformationError: if an OMP loop transform would create \
                incorrect code.
        '''
        if not options:
            options = {}

        # Since this function potentially modifies the user's option
        # dictionary, create a copy:
        options = options.copy()
        # Make sure the default is set:
        options["reprod"] = options.get("reprod",
                                        Config.get().reproducible_reductions)

        self.validate(node, options)

        # If the loop is not already coloured then check whether or not
        # it should be
        if node.loop_type is not 'colour' and node.has_inc_arg():
            raise TransformationError(
                "Error in {0} transformation. The kernel has an argument"
                " with INC access. Colouring is required.".
                format(self.name))

        return OMPLoopTrans.apply(self, node, options)


class GOceanOMPLoopTrans(OMPLoopTrans):

    ''' GOcean-specific orphan OpenMP loop transformation. Adds GOcean
        specific validity checks (that the node is either an inner or outer
        Loop). Actual transformation is done by
        :py:class:`base class <OMPLoopTrans>`.

        :param omp_schedule: the omp schedule to be created. Must be one of
            'runtime', 'static', 'dynamic', 'guided' or 'auto'.

        '''

    @property
    def name(self):
        ''' Returns the name of this transformation as a string.'''
        return "GOceanOMPLoopTrans"

    def __str__(self):
        return "Add an OpenMP DO directive to a GOcean loop"

    def apply(self, node, options=None):
        '''Perform GOcean specific loop validity checks then call
        :py:meth:`OMPLoopTrans.apply`.

        :param node: the loop to parallelise using OMP Do.
        :type node: :py:class:`psyclone.psyir.nodes.Loop`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        '''
        # check node is a loop. Although this is not GOcean specific
        # it is required for the subsequent checks to function
        # correctly.
        from psyclone.psyir.nodes import Loop
        if not isinstance(node, Loop):
            raise TransformationError("Error in "+self.name+" transformation."
                                      " The node is not a loop.")
        # Check we are either an inner or outer loop
        if node.loop_type not in ["inner", "outer"]:
            raise TransformationError("Error in "+self.name+" transformation."
                                      " The requested loop is not of type "
                                      "inner or outer.")

        return OMPLoopTrans.apply(self, node, options)


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

    def apply(self, node, options=None):
        '''
        Converts the Loop represented by :py:obj:`node` into a
        nested loop where the outer loop is over colours and the inner
        loop is over cells of that colour.

        :param node: the loop to transform.
        :type node: :py:class:`psyclone.psyir.nodes.Loop`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :returns: Tuple of modified schedule and record of transformation
        :rtype: (:py:class:`psyclone.psyir.nodes.Schedule, \
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
        colour_loop = node.__class__(parent=colours_loop.loop_body,
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
        colour_loop.loop_body.children.extend(node.loop_body)

        # change the parent of the node's contents to the colour loop
        for child in node.loop_body:
            child.parent = colour_loop.loop_body

        # remove original loop
        node_parent.children.remove(node)

        return schedule, keep


class KernelModuleInlineTrans(KernelTrans):
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
        examples where in-lining will not work are:

        #. A variable is declared within the module that ``contains`` the
           Kernel subroutine and is then accessed within that Kernel;
        #. A variable is included via use association at the module level
           and accessed within the Kernel subroutine.

        The transformation will reject attempts to in-line such kernels.
    '''

    def __str__(self):
        return ("Inline (or cancel inline of) a kernel subroutine into the "
                "PSy module")

    @property
    def name(self):
        ''' Returns the name of this transformation as a string.'''
        return "KernelModuleInline"

    def apply(self, node, options=None):
        '''Checks that the node is of the correct type (a Kernel) then marks
        the Kernel to be inlined, or not, depending on the value of
        the inline option. If the inline option is not passed the
        Kernel is marked to be inlined.

        :param node: the loop to transform.
        :type node: :py:class:`psyclone.psyir.nodes.Loop`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None
        :param bool options["inline"]: whether the kernel should be module\
                inlined or not.

        :returns: 2-tuple of new schedule and memento of transform.
        :rtype: (:py:class:`psyclone.dynamo0p3.DynInvokeSchedule`, \
                 :py:class:`psyclone.undoredo.Memento`)

        '''
        self.validate(node, options)

        schedule = node.root

        # create a memento of the schedule and the proposed transformation
        keep = Memento(schedule, self, [node])

        if not options:
            options = {}
        inline = options.get("inline", True)

        # set kernel's inline status
        if node.module_inline == inline:
            # issue a warning here when we implement logging
            # print "Warning, Kernel inline is already set to "+str(inline)
            pass
        else:
            node.module_inline = inline

        return schedule, keep

    def validate(self, node, options=None):
        '''
        Check that the supplied kernel is eligible to be module inlined.

        :param node: the node in the PSyIR that is to be module inlined.
        :type node: sub-class of :py:class:`psyclone.psyir.nodes.Node`
        :param bool inline: whether or not the kernel is to be inlined.
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None
        :param bool options["inline"]: whether the kernel should be module \
                                       inlined or not.

        :raises TransformationError: if the supplied kernel has itself been \
                                     transformed (Issue #229).
        '''
        super(KernelModuleInlineTrans, self).validate(node, options)

        if not options:
            options = {}
        inline = options.get("inline", True)

        if inline and node.modified:
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

    def apply(self, node, options=None):
        '''Performs Dynamo0.3-specific error checking and then uses the parent
        class to convert the Loop represented by :py:obj:`node` into a
        nested loop where the outer loop is over colours and the inner
        loop is over cells of that colour.

        :param node: the loop to transform.
        :type node: :py:class:`psyclone.dynamo0p3.DynLoop`
        :param options: a dictionary with options for transformations.\
        :type options: dictionary of string:values or None

        :returns: 2-tuple of new schedule and memento of transform.
        :rtype: (:py:class:`psyclone.dynamo0p3.DynInvokeSchedule`, \
                 :py:class:`psyclone.undoredo.Memento`)

        '''
        # check node is a loop
        from psyclone.psyir.nodes import Loop
        if not isinstance(node, Loop):
            raise TransformationError("Error in DynamoColour transformation. "
                                      "The supplied node is not a loop")
        # Check we need colouring
        from psyclone.dynamo0p3 import VALID_DISCONTINUOUS_FUNCTION_SPACE_NAMES
        if node.field_space.orig_name in \
           VALID_DISCONTINUOUS_FUNCTION_SPACE_NAMES:
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

    def validate(self, node_list, options=None):
        '''
        Check that the supplied list of Nodes are eligible to be
        put inside a parallel region.

        :param list node_list: list of nodes to put into a parallel region
        :param options: a dictionary with options for transformations.\
        :type options: dictionary of string:values or None
        :param bool options["node-type-check"]: this flag controls whether \
            or not the type of the nodes enclosed in the region should be \
            tested to avoid using unsupported nodes inside a region.

        :raises TransformationError: if the nodes cannot be put into a \
                                     parallel region.
        '''

        # Haloexchange calls existing within a parallel region are not
        # supported.
        from psyclone.psyGen import HaloExchange
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
        super(ParallelRegionTrans, self).validate(node_list, options)

    def apply(self, nodes, options=None):
        '''
        Apply this transformation to a subset of the nodes within a
        schedule - i.e. enclose the specified Loops in the
        schedule within a single parallel region.

        :param nodes: a single Node or a list of Nodes.
        :type nodes: (list of) :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None
        :param bool options["node-type-check"]: this flag controls if the \
                type of the nodes enclosed in the region should be tested \
                to avoid using unsupported nodes inside a region.

        :returns: 2-tuple of new schedule and memento of transform.
        :rtype: (:py:class:`psyclone.dynamo0p3.DynInvokeSchedule`, \
                 :py:class:`psyclone.undoredo.Memento`)

        '''

        # Check whether we've been passed a list of nodes or just a
        # single node. If the latter then we create ourselves a
        # list containing just that node.
        node_list = self.get_node_list(nodes)
        self.validate(node_list, options)

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
        # the region directive's Schedule as their parent. Note
        # that node_list is a copy, so we can remove children
        # from the tree without affecting the content of
        # node_list
        for child in node_list:
            # Remove child from the parent's list of children
            node_parent.children.remove(child)
            child.parent = directive.dir_body

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
    >>> from psyclone.psyGen import PSyFactory
    >>> from psyclone.errors import GenerationError
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
    from psyclone.psyir import nodes
    # The types of node that this transformation can enclose
    valid_node_types = (nodes.Loop, psyGen.Kern, psyGen.BuiltIn,
                        psyGen.OMPDirective, psyGen.GlobalSum,
                        nodes.Literal, nodes.Reference)

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
        :returns: the name of this transformation as a string.
        :rtype: str
        '''
        return "OMPParallelTrans"

    def validate(self, node_list, options=None):
        '''
        Perform OpenMP-specific validation checks.

        :param node_list: list of Nodes to put within parallel region.
        :type node_list: list of :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None
        :param bool options["node-type-check"]: this flag controls if the \
                type of the nodes enclosed in the region should be tested \
                to avoid using unsupported nodes inside a region.

        :raises TransformationError: if the target Nodes are already within \
                                     some OMP parallel region.
        '''
        from psyclone.psyGen import OMPDirective

        if node_list[0].ancestor(OMPDirective):
            raise TransformationError("Error in OMPParallel transformation:" +
                                      " cannot create an OpenMP PARALLEL " +
                                      "region within another OpenMP region.")

        # Now call the general validation checks
        super(OMPParallelTrans, self).validate(node_list, options)


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
    from psyclone import psyGen
    from psyclone.psyir import nodes
    valid_node_types = (
        nodes.Loop, psyGen.Kern, nodes.IfBlock,
        psyGen.ACCLoopDirective, nodes.Assignment, nodes.Reference,
        nodes.Literal, nodes.BinaryOperation)

    def __init__(self):
        from psyclone.psyGen import ACCParallelDirective
        super(ACCParallelTrans, self).__init__()
        # Set the type of directive that the base class will use
        self._pdirective = ACCParallelDirective

    def __str__(self):
        return "Insert an OpenACC Parallel region"

    @property
    def name(self):
        '''
        :returns: the name of this transformation as a string.
        :rtype: str
        '''
        return "ACCParallelTrans"


class GOConstLoopBoundsTrans(Transformation):
    ''' Switch on (or off) the use of constant loop bounds within
    a GOInvokeSchedule. In the absence of constant loop bounds, PSyclone will
    generate loops where the bounds are obtained by de-referencing a field
    object, e.g.:

    .. code-block:: fortran

      DO j = my_field%grid%internal%ystart, my_field%grid%internal%ystop

    Some compilers are able to produce more efficient code if they are
    provided with information on the relative trip-counts of the loops
    within an Invoke. With constant loop bounds switched on, PSyclone
    generates code like:

    .. code-block:: fortran

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

    def apply(self, node, options=None):
        '''Switches constant loop bounds on or off for all loops in a
        GOInvokeSchedule. Default is 'on'.

        :param node: the GOInvokeSchedule of which all loops will get the
            constant loop bounds switched on or off.
        :type node: :py:class:`psyclone.gocean1p0.GOInvokeSchedule`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :param bool options["const_bounds"]: whether the constant loop should\
            be used (True) or not (False). Default is True.

        :returns: 2-tuple of new schedule and memento of transform.
        :rtype: (:py:class:`psyclone.dynamo0p3.DynInvokeSchedule`, \
                 :py:class:`psyclone.undoredo.Memento`)

        '''

        # Check node is a Schedule
        from psyclone.gocean1p0 import GOInvokeSchedule
        if not isinstance(node, GOInvokeSchedule):
            raise TransformationError("Error in GOConstLoopBoundsTrans: "
                                      "node is not a GOInvokeSchedule")

        keep = Memento(node, self)

        if not options:
            options = {}

        node.const_loop_bounds = options.get("const_bounds", True)

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

    def validate(self, node, location, options=None):
        # pylint: disable=no-self-use
        ''' validity checks for input arguments.

        :param node: the node to be moved.
        :type node: :py:class:`psyclone.psyir.nodes.Node`
        :param location: node before or after which the given node\
            should be moved.
        :type location: :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None
        :param str options["position"]: either 'before' or 'after'.

        :raises TransformationError: if the given node is not an instance \
            of :py:class:`psyclone.psyir.nodes.Node`
        :raises TransformationError: if the location is not valid.
        '''

        # Check that the first argument is a Node
        from psyclone.psyir.nodes import Node
        if not isinstance(node, Node):
            raise TransformationError(
                "In the Move transformation apply method the first argument "
                "is not a Node")

        # Check new location conforms to any data dependencies
        # This also checks the location and position arguments
        if not options:
            options = {}
        position = options.get("position", "before")
        if not node.is_valid_location(location, position=position):
            raise TransformationError(
                "In the Move transformation apply method, data dependencies "
                "forbid the move to the new location")

    def apply(self, node, location, options=None):
        '''Move the node represented by :py:obj:`node` before location
        :py:obj:`location` (which is also a node) by default and after
        if the optional `position` argument is set to 'after'.

        :param node: the node to be moved.
        :type node: :py:class:`psyclone.psyir.nodes.Node`
        :param location: node before or after which the given node\
            should be moved.
        :type location: :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None
        :param str options["position"]: either 'before' or 'after'.

        :raises TransformationError: if the given node is not an instance \
            of :py:class:`psyclone.psyir.nodes.Node`
        :raises TransformationError: if the location is not valid.

        :returns: 2-tuple of new schedule and memento of transform.
        :rtype: (:py:class:`psyclone.dynamo0p3.DynInvokeSchedule`, \
                 :py:class:`psyclone.undoredo.Memento`)

        '''
        # pylint:disable=arguments-differ

        self.validate(node, location, options)

        schedule = node.root

        if not options:
            options = {}
        position = options.get("position", "before")

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

    def validate(self, node, options=None):
        '''Perform various checks to ensure that it is valid to apply the
        RedundantComputation transformation to the supplied node

        :param node: the supplied node on which we are performing\
                     validity checks
        :type node: :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None
        :param int options["depth"]: the depth of the stencil if the value \
                     is provided and None if not.

        :raises TransformationError: if the node is not a\
            :py:class:`psyclone.psyir.nodes.Loop`.
        :raises TransformationError: if the parent of the loop is a\
            :py:class:`psyclone.psyGen.Directive`.
        :raises TransformationError: if the parent of the loop is not a\
            :py:class:`psyclone.psyir.nodes.Loop` or a\
            :py:class:`psyclone.psyGen.DynInvokeSchedule`.
        :raises TransformationError: if the parent of the loop is a\
            :py:class:`psyclone.psyir.nodes.Loop` but the original loop does\
            not iterate over 'colour'.
        :raises TransformationError: if the parent of the loop is a\
            :py:class:`psyclone.psyir.nodes.Loop` but the parent does not
            iterate over 'colours'.
        :raises TransformationError: if the parent of the loop is a\
            :py:class:`psyclone.psyir.nodes.Loop` but the parent's parent is\
            not a :py:class:`psyclone.psyGen.DynInvokeSchedule`.
        :raises TransformationError: if this transformation is applied\
            when distributed memory is not switched on.
        :raises TransformationError: if the loop does not iterate over\
            cells, dofs or colour.
        :raises TransformationError: if the transformation is setting the\
            loop to the maximum halo depth but the loop already computes\
            to the maximum halo depth.
        :raises TransformationError: if the transformation is setting the\
            loop to the maximum halo depth but the loop contains a stencil\
            access (as this would result in the field being accessed\
            beyond the halo depth).
        :raises TransformationError: if the supplied depth value is not an\
            integer.
        :raises TransformationError: if the supplied depth value is less\
            than 1.
        :raises TransformationError: if the supplied depth value is not\
            greater than 1 when a continuous loop is modified as this is\
            the minimum valid value.
        :raises TransformationError: if the supplied depth value is not\
            greater than the existing depth value, as we should not need\
            to undo existing transformations.
        :raises TransformationError: if a depth value has been supplied\
            but the loop has already been set to the maximum halo depth.

        '''
        # check node is a loop
        from psyclone.psyGen import Directive
        from psyclone.psyir.nodes import Loop
        from psyclone.dynamo0p3 import DynInvokeSchedule
        if not isinstance(node, Loop):
            raise TransformationError(
                "In the Dynamo0p3RedundantComputation transformation apply "
                "method the first argument is not a Loop")
        # Check loop's parent is the InvokeSchedule, or that it is nested
        # in a colours loop and perform other colour(s) loop checks,
        # otherwise halo exchange placement might fail. The only
        # current example where the placement would fail is when
        # directives have already been added. This could be fixed but
        # it actually makes sense to require redundant computation
        # transformations to be applied before adding directives so it
        # is not particularly important.
        dir_node = node.ancestor(Directive)
        if dir_node:
            raise TransformationError(
                "In the Dynamo0p3RedundantComputation transformation apply "
                "method the supplied loop is sits beneath a directive of "
                "type {0}. Redundant computation must be applied before "
                "directives are added.".format(type(dir_node)))
        if not (isinstance(node.parent, DynInvokeSchedule) or
                isinstance(node.parent.parent, Loop)):
            raise TransformationError(
                "In the Dynamo0p3RedundantComputation transformation "
                "apply method the parent of the supplied loop must be "
                "the DynInvokeSchedule, or a Loop, but found {0}".
                format(type(node.parent)))
        if isinstance(node.parent.parent, Loop):
            if node.loop_type != "colour":
                raise TransformationError(
                    "In the Dynamo0p3RedundantComputation transformation "
                    "apply method, if the parent of the supplied Loop is "
                    "also a Loop then the supplied Loop must iterate over "
                    "'colour', but found '{0}'".format(node.loop_type))
            if node.parent.parent.loop_type != "colours":
                raise TransformationError(
                    "In the Dynamo0p3RedundantComputation transformation "
                    "apply method, if the parent of the supplied Loop is "
                    "also a Loop then the parent must iterate over "
                    "'colours', but found '{0}'"
                    "".format(node.parent.parent.loop_type))
            if not isinstance(node.parent.parent.parent, DynInvokeSchedule):
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

        if not options:
            options = {}
        depth = options.get("depth")
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

    def apply(self, loop, options=None):
        # pylint:disable=arguments-differ
        '''Apply the redundant computation transformation to the loop
        :py:obj:`loop`. This transformation can be applied to loops iterating
        over 'cells or 'dofs'. if :py:obj:`depth` is set to a value then the
        value will be the depth of the field's halo over which redundant
        computation will be performed. If :py:obj:`depth` is not set to a
        value then redundant computation will be performed to the full depth
        of the field's halo.

        :param loop: the loop that we are transforming.
        :type loop: :py:class:`psyclone.psyGen.DynLoop`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None
        :param int options["depth"]: the depth of the stencil. Defaults \
                to None.

        :returns: 2-tuple of new schedule and memento of transform.
        :rtype: (:py:class:`psyclone.dynamo0p3.DynInvokeSchedule`, \
                 :py:class:`psyclone.undoredo.Memento`)

        '''
        self.validate(loop, options)
        if not options:
            options = {}
        depth = options.get("depth")

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

    .. code-block:: fortran

        DO j=1, m
            DO i=1, n

    becomes:

    .. code-block:: fortran

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

    def validate(self, node_outer, options=None):
        # pylint: disable=no-self-use
        '''Checks if the given node contains a valid Fortran structure
        to allow swapping loops. This means the node must represent
        a loop, and it must have exactly one child that is also a loop.

        :param node_outer: a Loop node from an AST.
        :type node_outer: py:class:`psyclone.psyir.nodes.Loop`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :raises TransformationError: if the supplied node does not\
                                     allow a loop swap to be done.
         '''

        from psyclone.psyir.nodes import Loop
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

        if not node_outer.loop_body or not node_outer.loop_body.children:
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

        if len(node_outer.loop_body.children) > 1:
            raise TransformationError(
                "Error in GOLoopSwap transformation. Supplied node '{0}' must"
                " be the outer loop of a loop nest and must have exactly one "
                "inner loop, but this node has {1} inner statements, the "
                "first two being '{2}' and '{3}'"
                "".format(node_outer, len(node_outer.loop_body.children),
                          node_outer.loop_body[0], node_outer.loop_body[1]))

    def apply(self, outer, options=None):
        # pylint: disable=arguments-differ
        '''The argument :py:obj:`outer` must be a loop which has exactly
        one inner loop. This transform then swaps the outer and inner loop.

        :param outer: the node representing the outer loop.
        :type outer: :py:class:`psyclone.psyir.nodes.Loop`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :raises TransformationError: if the supplied node does not \
                                     allow a loop swap to be done.

        :returns: 2-tuple of new schedule and memento of transform.
        :rtype: (:py:class:`psyclone.dynamo0p3.DynInvokeSchedule`, \
                 :py:class:`psyclone.undoredo.Memento`)

        '''
        self.validate(outer, options)

        schedule = outer.root
        inner = outer.loop_body[0]
        parent = outer.parent

        # create a memento of the schedule and the proposed transformation
        keep = Memento(schedule, self, [inner, outer])

        # Remove outer from parent
        index = parent.children.index(outer)
        del parent.children[index]
        outer.parent = None

        # Move inner to parent
        inner.parent = parent
        parent.children.insert(index, inner)
        outer.loop_body.children.remove(inner)

        # Move inner's schedule to outer
        outer.children[3] = inner.loop_body
        for child in outer.loop_body:
            child.parent = outer.loop_body

        # Move outer under inner (create new Schedule to remove old entries)
        inner.children[3] = Schedule()
        inner.loop_body.parent = inner
        inner.loop_body.children.append(outer)
        outer.parent = inner.loop_body

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

    def apply(self, sched, options=None):
        '''
        Apply the OpenCL transformation to the supplied GOInvokeSchedule. This
        causes PSyclone to generate an OpenCL version of the corresponding
        PSy-layer routine. The generated code makes use of the FortCL
        library (https://github.com/stfc/FortCL) in order to manage the
        OpenCL device directly from Fortran.

        :param sched: the InvokeSchedule to transform.
        :type sched: :py:class:`psyclone.psyGen.GOInvokeSchedule`
        :param options: set of option to tune the OpenCL generation.
        :type options: dictionary of string:values or None
        :param bool options["opencl"]: whether or not to enable OpenCL \
                                       generation.

        :returns: 2-tuple of new schedule and memento of transform.
        :rtype: (:py:class:`psyclone.dynamo0p3.DynInvokeSchedule`, \
                 :py:class:`psyclone.undoredo.Memento`)
        '''
        if not options:
            options = {}
        opencl = options.get("opencl", True)

        if opencl:
            self.validate(sched, options)

        # Create a memento of the schedule and the proposed transformation
        keep = Memento(sched, self, [sched, opencl])
        # All we have to do here is set the flag in the Schedule. When this
        # flag is True PSyclone produces OpenCL at code-generation time.
        sched.opencl = opencl

        try:
            # Store the provided OpenCL options in the InvokeSchedule.
            sched.set_opencl_options(options)

        # The raised exceptions are converted to 'TransformationError's.
        except (TypeError, AttributeError) as error:
            raise TransformationError(str(error))

        return sched, keep

    def validate(self, sched, options=None):
        '''
        Checks that the supplied InvokeSchedule is valid and that an OpenCL
        version of it can be generated.

        :param sched: the Schedule to check.
        :type sched: :py:class:`psyclone.psyGen.InvokeSchedule`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :raises TransformationError: if the InvokeSchedule is not for the \
                                     GOcean1.0 API.
        :raises NotImplementedError: if any of the kernels have arguments \
                                     passed by value.
        '''
        from psyclone.psyGen import args_filter
        from psyclone.gocean1p0 import GOInvokeSchedule

        if isinstance(sched, InvokeSchedule):
            if not isinstance(sched, GOInvokeSchedule):
                raise TransformationError(
                    "OpenCL generation is currently only supported for the "
                    "GOcean API but got an InvokeSchedule of type: '{0}'".
                    format(type(sched)))
        else:
            raise TransformationError(
                "Error in OCLTrans: the supplied node must be a (sub-class "
                "of) InvokeSchedule but got {0}".format(type(sched)))

        # Now we need to check the arguments of all the kernels
        args = args_filter(sched.args, arg_types=["scalar"], is_literal=True)
        for arg in args:
            if arg.is_literal:
                raise NotImplementedError(
                    "Cannot generate OpenCL for Invokes that contain "
                    "kernels with arguments passed by value")

        # Check that we can construct the PSyIR and SymbolTable of each of
        # the kernels in this Schedule. Also check that none of them access
        # any form of global data (that is not a routine argument).
        for kern in sched.kernels():
            KernelTrans.validate(kern)
            ksched = kern.get_kernel_schedule()
            global_variables = ksched.symbol_table.global_datasymbols
            if global_variables:
                raise TransformationError(
                    "The Symbol Table for kernel '{0}' contains the following "
                    "symbols with 'global' scope: {1}. An OpenCL kernel cannot"
                    " call other kernels and all of the data it accesses must "
                    "be passed by argument. Use the KernelGlobalsToArguments "
                    "transformation to convert such symbols to kernel "
                    "arguments first.".
                    format(kern.name, [sym.name for sym in global_variables]))


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

    def apply(self, node, options=None):
        '''Transforms a synchronous halo exchange, represented by a
        HaloExchange node, into an asynchronous halo exchange,
        represented by HaloExchangeStart and HaloExchangeEnd nodes.

        :param node: a synchronous haloexchange node.
        :type node: :py:obj:`psyclone.psygen.HaloExchange`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :returns: tuple of the modified schedule and a record of the \
                  transformation.
        :rtype: (:py:class:`psyclone.psyir.nodes.Schedule`, \
                :py:class:`psyclone.undoredo.Memento`)

        '''
        self.validate(node, options)

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

    def validate(self, node, options):
        '''Internal method to check whether the node is valid for this
        transformation.

        :param node: a synchronous Halo Exchange node
        :type node: :py:obj:`psyclone.psygen.HaloExchange`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

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
    >>>     new_schedule, _ = trans.apply(kernel, number_of_layers=150)
    >>>     kernel_schedule = kernel.get_kernel_schedule()
    >>>     kernel_schedule.symbol_table.view()

    '''

    # ndofs per 3D cell for different function spaces on a quadrilateral
    # element for different orders. Formulas kindly provided by Tom Melvin and
    # Thomas Gibson. See the Qr table at http://femtable.org/background.html,
    # for computed values of w0, w1, w2 and w3 up to order 7.
    # Note: w2*trace spaces have dofs only on cell faces and no volume dofs.
    # As there is currently no dedicated structure for face dofs in kernel
    # constants, w2*trace dofs are included here. w2*trace ndofs formulas
    # require the number of reference element faces in the horizontal (4)
    # for w2htrace space, in the vertical (2) for w2vtrace space and all (6)
    # for w2trace space.

    space_to_dofs = {"w3":       (lambda n: (n+1)**3),
                     "w2":       (lambda n: 3*(n+2)*(n+1)**2),
                     "w1":       (lambda n: 3*(n+2)**2*(n+1)),
                     "w0":       (lambda n: (n+2)**3),
                     "wtheta":   (lambda n: (n+2)*(n+1)**2),
                     "w2h":      (lambda n: 2*(n+2)*(n+1)**2),
                     "w2v":      (lambda n: (n+2)*(n+1)**2),
                     "w2broken": (lambda n: 3*(n+1)**2*(n+2)),
                     "wchi":     (lambda n: (n+1)**3),
                     "w2trace":  (lambda n: 6*(n+1)**2),
                     "w2htrace": (lambda n: 4*(n+1)**2),
                     "w2vtrace": (lambda n: 2*(n+1)**2)}

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

    def apply(self, node, options=None):
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

        :param node: a kernel node.
        :type node: :py:obj:`psyclone.psygen.DynKern`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None
        :param str options["cellshape"]: the shape of the cells. This is\
            provided as it helps determine the number of dofs a field has\
            for a particular function space. Currently only "quadrilateral"\
            is supported which is also the default value.
        :param int options["element_order"]: the order of the cell. In \
            combination with cellshape, this determines the number of \
            dofs a field has for a particular function space. If it is set \
            to None (the default) then the dofs values are not set as \
            constants in the kernel, otherwise they are.
        :param int options["number_of_layers"]: the number of vertical \
            layers in the LFRic model mesh used for this particular run. If \
            this is set to None (the default) then the nlayers value is not \
            set as a constant in the kernel, otherwise it is.
        :param bool options["quadrature"]: whether the number of quadrature \
            points values are set as constants in the kernel (True) or not \
            (False). The default is False.

        :returns: tuple of the modified schedule and a record of the \
                  transformation.
        :rtype: (:py:class:`psyclone.psyir.nodes.Schedule`, \
                :py:class:`psyclone.undoredo.Memento`)

        '''
        # --------------------------------------------------------------------
        def make_constant(symbol_table, arg_position, value,
                          function_space=None):
            '''Utility function that modifies the argument at position
            'arg_position' into a compile-time constant with value
            'value'.

            :param symbol_table: the symbol table for the kernel \
                         holding the argument that is going to be modified.
            :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable`
            :param int arg_position: the argument's position in the \
                                     argument list.
            :param value: the constant value that this argument is \
                    going to be given. Its type depends on the type of the \
                    argument.
            :type value: int, str or bool
            :type str function_space: the name of the function space \
                    if there is a function space associated with this \
                    argument. Defaults to None.

            '''
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
            if not isinstance(symbol.datatype, ScalarType):
                raise TransformationError(
                    "Expected entry to be a scalar argument but found "
                    "'{0}'.".format(type(symbol.datatype).__name__))
            if symbol.datatype.intrinsic != ScalarType.Intrinsic.INTEGER:
                raise TransformationError(
                    "Expected entry to be a scalar integer argument "
                    "but found '{0}'.".format(symbol.datatype))
            if symbol.is_constant:
                raise TransformationError(
                    "Expected entry to be a scalar integer argument "
                    "but found a constant.")

            # Create a new symbol with a known constant value then swap
            # it with the argument. The argument then becomes xxx_dummy
            # and is unused within the kernel body.
            # TODO: Temporarily use unsafe name change until the name
            # space manager is introduced into the SymbolTable (Issue
            # #321).
            orig_name = symbol.name
            local_symbol = DataSymbol(orig_name+"_dummy", INTEGER_TYPE,
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
        # --------------------------------------------------------------------

        self.validate(node, options)

        if not options:
            options = {}
        number_of_layers = options.get("number_of_layers", None)
        quadrature = options.get("quadrature", False)
        element_order = options.get("element_order", None)
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
            # TODO #705 - support the transformation of kernels requiring
            # other quadrature types (face/edge, multiple).
            if kernel.eval_shapes == ["gh_quadrature_xyoz"]:
                make_constant(symbol_table,
                              arg_list_info.nqp_positions[0]["horizontal"],
                              element_order+3)
                make_constant(symbol_table,
                              arg_list_info.nqp_positions[0]["vertical"],
                              element_order+3)
            else:
                raise TransformationError(
                    "Error in Dynamo0p3KernelConstTrans transformation. "
                    "Support is currently limited to 'xyoz' quadrature but "
                    "found {0}.".format(kernel.eval_shapes))

        if element_order is not None:
            # Modify the symbol table for degrees of freedom here.
            for info in arg_list_info.ndf_positions:
                if (info.function_space.lower() in
                        (VALID_ANY_SPACE_NAMES +
                         VALID_ANY_DISCONTINUOUS_SPACE_NAMES + ["any_w2"])):
                    # skip any_space_*, any_discontinuous_space_* and any_w2
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

        # Flag that the kernel has been modified
        kernel.modified = True

        return schedule, keep

    def validate(self, node, options=None):
        '''This method checks whether the input arguments are valid for
        this transformation.

        :param node: a dynamo 0.3 kernel node.
        :type node: :py:obj:`psyclone.psygen.DynKern`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None
        :param str options["cellshape"]: the shape of the elements/cells.
        :param int options["element_order"]: the order of the elements/cells.
        :param int options["number_of_layers"]: the number of layers to use.
        :param bool options["quadrature"]: whether quadrature dimension sizes \
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

        if not options:
            options = {}
        cellshape = options.get("cellshape", "quadrilateral")
        element_order = options.get("element_order", None)
        number_of_layers = options.get("number_of_layers", None)
        quadrature = options.get("quadrature", False)
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

    def apply(self, sched, options=None):
        '''Adds an OpenACC "enter data" directive to the invoke associated
        with the supplied Schedule. Any fields accessed by OpenACC kernels
        within this schedule will be added to this data region in
        order to ensure they remain on the target device.

        :param sched: schedule to which to add an "enter data" directive.
        :type sched: sub-class of :py:class:`psyclone.psyir.nodes.Schedule`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :returns: tuple of the modified schedule and a record of the \
                  transformation.
        :rtype: (:py:class:`psyclone.psyir.nodes.Schedule`, \
                :py:class:`psyclone.undoredo.Memento`)
        '''
        from psyclone.gocean1p0 import GOInvokeSchedule
        from psyclone.dynamo0p3 import DynInvokeSchedule

        # Ensure that the proposed transformation is valid
        self.validate(sched, options)

        if isinstance(sched, GOInvokeSchedule):
            from psyclone.gocean1p0 import GOACCEnterDataDirective as \
                AccEnterDataDir
        elif isinstance(sched, DynInvokeSchedule):
            from psyclone.dynamo0p3 import DynACCEnterDataDirective as \
                AccEnterDataDir
        else:
            # Should not get here provided that validate() has done its job
            raise InternalError(
                "ACCEnterDataTrans.validate() has not rejected an "
                "(unsupported) schedule of type {0}".format(type(sched)))

        # Create a memento of the schedule and the proposed
        # transformation.
        keep = Memento(sched, self, [sched])

        # Add the directive
        data_dir = AccEnterDataDir(parent=sched, children=[])
        sched.addchild(data_dir, index=0)

        return sched, keep

    def validate(self, sched, options=None):
        # pylint: disable=arguments-differ
        '''
        Check that we can safely apply the OpenACC enter-data transformation
        to the supplied Schedule.

        :param sched: Schedule to which to add an "enter data" directive.
        :type sched: sub-class of :py:class:`psyclone.psyir.nodes.Schedule`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :raises NotImplementedError: for any API other than GOcean 1.0 or NEMO.
        :raises TransformationError: if passed something that is not a \
            (subclass of) :py:class:`psyclone.psyir.nodes.Schedule`.
        '''
        from psyclone.psyGen import Directive, \
            ACCDataDirective, ACCEnterDataDirective
        from psyclone.gocean1p0 import GOInvokeSchedule
        from psyclone.dynamo0p3 import DynInvokeSchedule

        super(ACCEnterDataTrans, self).validate(sched, options)

        if not isinstance(sched, Schedule):
            raise TransformationError("Cannot apply an OpenACC enter-data "
                                      "directive to something that is "
                                      "not a Schedule")

        if not isinstance(sched, (GOInvokeSchedule, DynInvokeSchedule)):
            raise NotImplementedError(
                "ACCEnterDataTrans: ACCEnterDataDirective not implemented for "
                "a schedule of type {0}".format(type(sched)))

        # Check that we don't already have a data region of any sort
        directives = sched.walk(Directive)
        if any(isinstance(ddir, (ACCDataDirective,
                                 ACCEnterDataDirective))
               for ddir in directives):
            raise TransformationError("Schedule already has an OpenACC data "
                                      "region - cannot add an enter data.")


class ACCRoutineTrans(KernelTrans):
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

    def apply(self, kern, options=None):
        '''
        Modifies the AST of the supplied kernel so that it contains an
        '!$acc routine' OpenACC directive.

        :param kern: the kernel object to transform.
        :type kern: :py:class:`psyclone.psyGen.Kern`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :raises TransformationError: if we fail to find the subroutine \
                                     corresponding to the kernel object.

        :returns: (transformed kernel, memento of transformation)
        :rtype: 2-tuple of (:py:class:`psyclone.psyGen.Kern`, \
                :py:class:`psyclone.undoredo.Memento`).

        '''
        # pylint: disable=too-many-locals

        from fparser.two.Fortran2003 import Subroutine_Subprogram, \
            Subroutine_Stmt, Specification_Part, Type_Declaration_Stmt, \
            Implicit_Part, Comment
        from fparser.two.utils import walk
        from fparser.common.readfortran import FortranStringReader

        # Check that we can safely apply this transformation
        self.validate(kern, options)

        # Get the fparser2 AST of the kernel
        ast = kern.ast
        # Keep a record of this transformation
        keep = Memento(kern, self)
        # Find the kernel subroutine in the fparser2 parse tree
        kern_sub = None
        subroutines = walk(ast.content, Subroutine_Subprogram)
        for sub in subroutines:
            for child in sub.content:
                if isinstance(child, Subroutine_Stmt) and \
                   str(child.items[1]) == kern.name:
                    kern_sub = sub
                    break
            if kern_sub:
                break
        # Find the last declaration statement in the subroutine
        spec = walk(kern_sub.content, Specification_Part)[0]
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

    def validate(self, kern, options=None):
        '''
        Perform checks that the supplied kernel can be transformed.

        :param kern: the kernel which is the target of the transformation.
        :type kern: :py:class:`psyclone.psyGen.Kern`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :raises TransformationError: if the target kernel is a built-in.
        :raises TransformationError: if any of the symbols in the kernel are \
                            accessed via a module use statement.
        :raises TransformationError: if the target kernel has already been \
                            transformed (since all other transformations work \
                            on the PSyIR but this one still uses the fparser2 \
                            parse tree (#490).
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
        if kern.modified:
            raise TransformationError(
                "Cannot transform kernel '{0}' because it has previously been "
                "transformed and this transformation works on the fparser2 "
                "parse tree rather than the PSyIR (#490).".format(kern.name))

        # Perform general validation checks. In particular this checks that
        # the PSyIR of the kernel body can be constructed.
        super(ACCRoutineTrans, self).validate(kern, options)

        # Check that the kernel does not access any data or routines via a
        # module 'use' statement
        sched = kern.get_kernel_schedule()
        global_variables = sched.symbol_table.global_datasymbols
        if global_variables:
            raise TransformationError(
                "The Symbol Table for kernel '{0}' contains the following "
                "symbol(s) with global scope: {1}. If these symbols represent"
                " data then they must first be converted to kernel arguments "
                "using the KernelGlobalsToArguments transformation. If the "
                "symbols represent external routines then PSyclone cannot "
                "currently transform this kernel for execution on an OpenACC "
                "device (issue #342).".
                format(kern.name, [sym.name for sym in global_variables]))
        # Prevent unwanted side effects by removing the kernel schedule that
        # we have just constructed. This is necessary while
        # psyGen.Kern.rename_and_write still supports kernels that have been
        # transformed by manipulation of the fparser2 Parse Tree (as opposed
        # to the PSyIR).
        # TODO #490 remove the following line.
        kern._kern_schedule = None


class ACCKernelsTrans(RegionTrans):
    '''
    Enclose a sub-set of nodes from a Schedule within an OpenACC kernels
    region (i.e. within "!$acc kernels" ... "!$acc end kernels" directives).

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
    from psyclone import nemo, psyGen, dynamo0p3
    from psyclone.psyir import nodes
    valid_node_types = (nodes.Loop, nemo.NemoKern, nodes.IfBlock,
                        nodes.Operation, nodes.Literal,
                        nodes.Assignment, nodes.Reference,
                        dynamo0p3.DynLoop, dynamo0p3.DynKern, psyGen.BuiltIn)

    @property
    def name(self):
        '''
        :returns: the name of this transformation class.
        :rtype: str
        '''
        return "ACCKernelsTrans"

    def apply(self, node_list, options=None):
        '''
        Enclose the supplied list of PSyIR nodes within an OpenACC
        Kernels region.

        :param node_list: the list of nodes in the PSyIR to enclose.
        :type node_list: list of :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None
        :param bool options["default_present"]: whether or not the kernels \
            region should have the 'default present' attribute (indicating \
            that data is already on the accelerator). When using managed \
            memory this option should be False.

        :returns: (transformed schedule, memento of transformation)
        :rtype: 2-tuple of (:py:class:`psyclone.psyir.nodes.Schedule`,
                            :py:class:`psyclone.undoredo.Memento`).

        '''
        self.validate(node_list, options)

        # Keep a record of this transformation
        keep = Memento(node_list[:], self)

        parent = node_list[0].parent
        schedule = node_list[0].root

        if not options:
            options = {}
        default_present = options.get("default_present", False)

        # Create the directive and insert it. Take a copy of the list
        # as it may just be a reference to the parent.children list
        # that we are about to modify.
        from psyclone.psyGen import ACCKernelsDirective
        directive = ACCKernelsDirective(parent=parent,
                                        children=node_list[:],
                                        default_present=default_present)
        start_index = parent.children.index(node_list[0])

        for child in directive.dir_body.children:
            parent.children.remove(child)

        parent.children.insert(start_index, directive)

        # Return the now modified kernel
        return schedule, keep

    def validate(self, node_list, options):
        '''
        Check that we can safely enclose the supplied list of nodes within
        OpenACC kernels ... end kernels directives.

        :param node_list: the proposed list of PSyIR nodes to enclose in the \
                          kernels region.
        :type node_list: list of :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :raises NotImplementedError: if the supplied Nodes do not belong to \
                                     a NemoInvokeSchedule.
        :raises TransformationError: if there are no Loops within the \
                                     proposed region.

        '''
        from psyclone.nemo import NemoInvokeSchedule
        from psyclone.dynamo0p3 import DynInvokeSchedule
        from psyclone.psyir.nodes import Loop
        # Check that the front-end is valid
        sched = node_list[0].ancestor((NemoInvokeSchedule, DynInvokeSchedule))
        if not sched:
            raise NotImplementedError(
                "OpenACC kernels regions are currently only supported for the "
                "nemo and dynamo0.3 front-ends")
        super(ACCKernelsTrans, self).validate(node_list, options)

        # Check that we have at least one loop within the proposed region
        for node in node_list:
            if node.walk(Loop):
                break
        else:
            # Branch executed if loop does not exit with a break
            raise TransformationError("A kernels transformation must enclose "
                                      "at least one loop but none were found.")


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
    from psyclone.psyir import nodes
    valid_node_types = (nodes.Loop, psyGen.Kern, psyGen.BuiltIn,
                        psyGen.Directive, nodes.IfBlock, nodes.Literal,
                        nodes.Assignment, nodes.Reference,
                        nodes.Operation)

    @property
    def name(self):
        '''
        :returns: the name of this transformation.
        :rtype: str

        '''
        return "ACCDataTrans"

    def apply(self, node_list, options=None):
        '''
        Put the supplied list of nodes within an OpenACC data region.

        :param node_list: the list of PSyIR nodes to enclose in the data \
                          region.
        :type node_list: list of :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :returns: (transformed schedule, memento of transformation)
        :rtype: 2-tuple of (:py:class:`psyclone.psyir.nodes.Schedule`, \
                :py:class:`psyclone.undoredo.Memento`).

        '''
        self.validate(node_list, options)

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

        for child in directive.dir_body[:]:
            parent.children.remove(child)
            child.parent = directive.dir_body

        parent.children.insert(start_index, directive)

        # Return the now modified kernel
        return schedule, keep

    def validate(self, node_list, options):
        '''
        Check that we can safely add a data region around the supplied list
        of nodes.

        :param node_list: the proposed list of nodes to enclose in a data \
                          region.
        :type node_list: list of subclasses of \
                         :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :raises TransformationError: if the Schedule to which the nodes \
                                belong already has an 'enter data' directive.
        :raises TransformationError: if any of the nodes are themselves \
                                     data directives.
        '''
        from psyclone.psyGen import ACCEnterDataDirective
        super(ACCDataTrans, self).validate(node_list, options)

        # Check that the Schedule to which the nodes belong does not already
        # have an 'enter data' directive.
        schedule = node_list[0].root
        acc_dirs = schedule.walk(ACCEnterDataDirective)
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

    def apply(self, loop, options=None):
        '''
        Transform the outermost array slice in the supplied implicit loop
        into an explicit loop.

        :param loop: the NemoImplicitLoop to transform.
        :type loop: :py:class:`psyclone.nemo.NemoImplicitLoop`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :raises NotImplementedError: if the array slice has explicit bounds.
        :raises TransformationError: if an array slice is not in dimensions \
                                     1-3 of the array.

        :returns: a new PSyIR loop object and a memento of the transformation.
        :rtype: (:py:class:`psyclone.nemo.NemoLoop`, \
                 :py:class:`psyclone.undoredo.Memento`)

        '''
        from fparser.two import Fortran2003
        from fparser.two.utils import walk
        from fparser.common.readfortran import FortranStringReader
        from psyclone import nemo

        self.validate(loop, options)

        # Keep a record of this transformation
        keep = Memento(loop, self)

        # Find all uses of array syntax in the statement
        subsections = walk(loop.ast.items, Fortran2003.Section_Subscript_List)
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

        # Get a reference to the highest-level symbol table
        symbol_table = loop.root.symbol_table
        config = Config.get().api_conf("nemo")
        index_order = config.get_index_order()
        loop_type_data = config.get_loop_type_data()

        loop_type = loop_type_data[index_order[outermost_dim]]
        base_name = loop_type["var"]
        loop_var = symbol_table.new_symbol_name(base_name)
        symbol_table.add(DataSymbol(loop_var, INTEGER_TYPE))
        loop_start = loop_type["start"]
        loop_stop = loop_type["stop"]
        loop_step = "1"

        # TODO remove this as part of #435 (since we will no longer have to
        # insert declarations into the fparser2 parse tree).
        prog_unit = loop.ast.get_root()
        spec_list = walk(prog_unit.content, Fortran2003.Specification_Part)
        if not spec_list:
            # Routine has no specification part so create one and add it
            # in to the parse tree
            from psyclone.psyGen import object_index
            exe_part = walk(prog_unit.content,
                            Fortran2003.Execution_Part)[0]
            idx = object_index(exe_part.parent.content, exe_part)

            spec = Fortran2003.Specification_Part(
                FortranStringReader("integer :: {0}".format(loop_var)))
            spec.parent = exe_part.parent
            exe_part.parent.content.insert(idx, spec)
        else:
            from psyclone.psyir.frontend.fparser2 import Fparser2Reader
            reader = Fparser2Reader()
            fake_parent = Schedule()
            spec = spec_list[0]
            reader.process_declarations(fake_parent, [spec], [])
            if loop_var not in fake_parent.symbol_table:
                decln = Fortran2003.Type_Declaration_Stmt(
                    FortranStringReader("integer :: {0}".format(loop_var)))
                decln.parent = spec
                spec.content.append(decln)

        name = Fortran2003.Name(FortranStringReader(loop_var))

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
        parent_index = loop.ast.parent.content.index(loop.ast)
        loop.ast.parent.content.insert(parent_index, new_loop)
        # Ensure it has the correct parent information
        new_loop.parent = loop.ast.parent
        # Replace the content of the loop with the (modified) implicit
        # loop
        new_loop.content[1] = loop.ast
        # Remove the implicit loop from its original parent in the AST
        loop.ast.parent.content.remove(loop.ast)
        # Now set its parent to be the new loop instead
        new_loop.content[1].parent = new_loop

        # Now we must update the PSyIR to reflect the new AST
        # First we update the parent of the loop we have transformed
        psyir_parent = loop.parent
        psyir_parent.children.remove(loop)
        # Next, we simply process the transformed fparser2 AST to generate
        # the new PSyIR of it
        astprocessor = nemo.NemoFparser2Reader()
        astprocessor.process_nodes(psyir_parent, [new_loop])
        # Delete the old PSyIR node that we have transformed
        del loop
        # Return the new NemoLoop object that we have created
        return psyir_parent.children[0], keep

    def validate(self, loop, options=None):
        '''
        Check that the supplied loop is a valid target for this transformation.

        :param loop: the loop node to validate.
        :type loop: :py:class:`psyclone.nemo.NemoImplicitLoop`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :raises TransformationError: if the supplied loop is not a \
                                     NemoImplicitLoop.
        '''
        from psyclone.nemo import NemoImplicitLoop
        if not isinstance(loop, NemoImplicitLoop):
            raise TransformationError(
                "Cannot apply NemoExplicitLoopTrans to something that is "
                "not a NemoImplicitLoop (got {0})".format(type(loop)))


class KernelGlobalsToArguments(Transformation):
    '''
    Transformation that removes any accesses of global data from the supplied
    kernel and places them in the caller. The values/references are then passed
    by argument into the kernel.
    '''
    @property
    def name(self):
        '''
        :returns: the name of this transformation.
        :rtype: str
        '''
        return "KernelGlobalsToArguments"

    def __str__(self):
        return ("Convert the global variables used inside the kernel "
                "into arguments and modify the InvokeSchedule to pass them"
                " in the kernel call.")

    def validate(self, node, options=None):
        '''
        Check that the supplied node is a valid target for this transformation.

        :param node: the PSyIR node to validate.
        :type node: :py:class:`psyclone.psyGen.CodedKern`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :raises TransformationError: if the supplied node is not a CodedKern.
        :raises TransformationError: if this transformation is not applied to \
            a Gocean API Invoke.
        :raises TransformationError: if the supplied kernel contains wildcard \
            imports of symbols from one or more containers (e.g. a USE without\
            an ONLY clause in Fortran).
        '''
        from psyclone.psyGen import CodedKern
        from psyclone.gocean1p0 import GOInvokeSchedule

        if not isinstance(node, CodedKern):
            raise TransformationError(
                "The {0} transformation can only be applied to CodedKern "
                "nodes but found '{1}' instead.".
                format(self.name, type(node).__name__))

        if not isinstance(node.root, GOInvokeSchedule):
            raise TransformationError(
                "The {0} transformation is currently only supported for the "
                "GOcean API but got an InvokeSchedule of type: '{1}'".
                format(self.name, type(node.root).__name__))

        # Check that there are no unqualified imports or undeclared symbols
        try:
            kernel = node.get_kernel_schedule()
        except SymbolError as err:
            raise TransformationError(
                "Kernel '{0}' contains undeclared symbol: {1}".format(
                    node.name, str(err.value)))

        symtab = kernel.symbol_table
        for container in symtab.containersymbols:
            if container.wildcard_import:
                raise TransformationError(
                    "Kernel '{0}' has a wildcard import of symbols from "
                    "container '{1}'. This is not supported.".format(
                        node.name, container.name))

        # TODO #649. Check for variables accessed by the kernel but declared
        # in an outer scope.

    def apply(self, node, options=None):
        '''
        Convert the global variables used inside the kernel into arguments and
        modify the InvokeSchedule to pass the same global variables to the
        kernel call.

        This apply() method does not return anything, as agreed in #595.
        However, this change has yet to be applied to the other Transformation
        classes.

        :param node: a kernel call.
        :type node: :py:class:`psyclone.psyGen.CodedKern`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        '''
        from psyclone.psyir.symbols import ArgumentInterface

        self.validate(node)

        kernel = node.get_kernel_schedule()
        symtab = kernel.symbol_table
        invoke_symtab = node.root.symbol_table

        # Transform each global variable into an argument.
        # TODO #11: When support for logging is added, we could warn the user
        # if no globals are found in the kernel.
        for globalvar in kernel.symbol_table.global_datasymbols[:]:

            # Resolve the data type information if it is not available
            if isinstance(globalvar.datatype, DeferredType):
                globalvar.resolve_deferred()

            # Copy the global into the InvokeSchedule SymbolTable
            invoke_symtab.copy_external_global(
                globalvar, tag="AlgArgs_" + globalvar.name)

            # Keep a reference to the original container so that we can
            # update it after the interface has been updated.
            container = globalvar.interface.container_symbol

            # Convert the symbol to an argument and add it to the argument list
            current_arg_list = symtab.argument_list
            if globalvar.is_constant:
                # Global constants lose the constant value but are read-only
                # TODO: When #633 and #11 are implemented, warn the user that
                # they should transform the constants to literal values first.
                globalvar.constant_value = None
                globalvar.interface = ArgumentInterface(
                    ArgumentInterface.Access.READ)
            else:
                globalvar.interface = ArgumentInterface(
                    ArgumentInterface.Access.READWRITE)
            current_arg_list.append(globalvar)
            symtab.specify_argument_list(current_arg_list)

            # Convert PSyIR DataTypes to Gocean VALID_SCALAR_TYPES
            # TODO #678: Ideally this strings should be provided by the GOcean
            # API configuration.
            go_space = ""
            if globalvar.datatype.intrinsic == ScalarType.Intrinsic.REAL:
                go_space = "go_r_scalar"
            elif globalvar.datatype.intrinsic == ScalarType.Intrinsic.INTEGER:
                go_space = "go_i_scalar"
            else:
                raise TypeError(
                    "The global variable '{0}' could not be promoted to an "
                    "argument because the GOcean infrastructure does not have"
                    " any scalar type equivalent to the PSyIR {1} type.".
                    format(globalvar.name, globalvar.datatype))

            # Add the global variable in the call argument list
            node.arguments.append(globalvar.name, go_space)

            # Check whether we still need the Container symbol from which
            # this global was originally accessed
            if not kernel.symbol_table.imported_symbols(container) and \
               not container.wildcard_import:
                kernel.symbol_table.remove(container)
        # TODO #663 - uncomment line below and fix tests.
        # node.modified = True
