# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017, Science and Technology Facilities Council
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2016.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
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
# Authors R. Ford and A. R. Porter, STFC Daresbury Lab

''' This module provides the various transformations that can
    be applied to the schedule associated with an invoke(). There
    are both general and API-specific transformation classes in
    this module where the latter typically apply API-specific
    checks before calling the base class for the actual
    transformation. '''

from psyGen import Transformation

VALID_OMP_SCHEDULES = ["runtime", "static", "dynamic", "guided", "auto"]


class TransformationError(Exception):
    ''' Provides a PSyclone-specific error class for errors found during
        code transformation operations. '''

    def __init__(self, value):
        Exception.__init__(self, value)
        self.value = "Transformation Error: "+value

    def __str__(self):
        return repr(self.value)


class LoopFuseTrans(Transformation):
    ''' Provides a loop-fuse transformation.
        For example:

        >>> from parse import parse
        >>> from psyGen import PSyFactory
        >>> ast,invokeInfo=parse("dynamo.F90")
        >>> psy=PSyFactory("dynamo0.1").create(invokeInfo)
        >>> schedule=psy.invokes.get('invoke_v3_kernel_type').schedule
        >>> schedule.view()
        >>>
        >>> from transformations import LoopFuseTrans
        >>> trans=LoopFuseTrans()
        >>> new_schedule,memento=trans.apply(schedule.children[0],
                                             schedule.children[1])
        >>> new_schedule.view()
    '''

    def __str__(self):
        return "Fuse two adjacent loops together"

    @property
    def name(self):
        ''' Returns the name of this transformation as a string '''
        return "LoopFuse"

    def _validate(self, node1, node2):
        ''' validity checks for input arguments '''
        # Check that the supplied Node is a Loop
        from psyGen import Loop
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
        if not node1.iteration_space == node2.iteration_space:
            raise TransformationError("Error in LoopFuse transformation. "
                                      "Loops do not have the same "
                                      "iteration space")

    def apply(self, node1, node2):
        ''' Fuse the loops represented by :py:obj:`node1` and
        :py:obj:`node2` '''

        self._validate(node1, node2)

        schedule = node1.root

        # create a memento of the schedule and the proposed transformation
        from undoredo import Memento
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
        ''' Returns the name of this transformation as a string '''
        return "GOceanLoopFuse"

    def apply(self, node1, node2):
        ''' Fuse the two GOcean loops represented by :py:obj:`node1` and
        :py:obj:`node2` '''

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
        ''' Returns the name of this transformation as a string '''
        return "DynamoLoopFuse"

    def apply(self, node1, node2, same_space=False):
        '''Fuse the two Dynamo loops represented by :py:obj:`node1` and
        :py:obj:`node2`. The optional same_space flag asserts that an
        unknown iteration space (i.e. any_space) matches the other
        iteration space. This is set at the users own risk. '''

        LoopFuseTrans._validate(self, node1, node2)

        from dynamo0p3 import VALID_FUNCTION_SPACES
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
            if node1._upper_bound_name != node2._upper_bound_name:
                raise TransformationError(
                    "Error in DynamoLoopFuse transformation. The upper bound "
                    "names are not the same. Found '{0}' and '{1}'".
                    format(node1._upper_bound_name, node2._upper_bound_name))
            if node1._upper_bound_index != node2._upper_bound_index:
                raise TransformationError(
                    "Error in DynamoLoopFuse transformation. The upper bound "
                    "indices are not the same. Found '{0}' and '{1}'".
                    format(node1._upper_bound_index, node2._upper_bound_index))
            from psyGen import MAPPING_SCALARS, MAPPING_REDUCTIONS
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


class OMPLoopTrans(Transformation):

    '''Adds an orphaned OpenMP directive to a loop. i.e. the directive
        must be inside the scope of some other OMP Parallel
        REGION. This condition is tested at code-generation time. The
        optional 'reprod' argument in the apply method decides whether
        standard OpenMP reduction support is to be used (which is not
        reproducible) or whether a manual reproducible reproduction is
        to be used.

        For example:

        >>> from parse import parse,ParseError
        >>> from psyGen import PSyFactory,GenerationError
        >>> api="gocean1.0"
        >>> filename="nemolite2d_alg.f90"
        >>> ast,invokeInfo=parse(filename,api=api,invoke_name="invoke")
        >>> psy=PSyFactory(api).create(invokeInfo)
        >>> print psy.invokes.names
        >>>
        >>> from psyGen import TransInfo
        >>> t=TransInfo()
        >>> ltrans = t.get_trans_name('OMPLoopTrans')
        >>> rtrans = t.get_trans_name('OMPParallelTrans')
        >>>
        >>> schedule=psy.invokes.get('invoke_0').schedule
        >>> schedule.view()
        >>> new_schedule=schedule
        >>>
        # Apply the OpenMP Loop transformation to *every* loop
        # in the schedule
        >>> for child in schedule.children:
        >>>     newschedule,memento=ltrans.apply(child, reprod=True)
        >>>     schedule = newschedule
        >>>
        # Enclose all of these loops within a single OpenMP
        # PARALLEL region
        >>> rtrans.omp_schedule("dynamic,1")
        >>> newschedule,memento = rtrans.apply(schedule.children)
        >>>
        >>>

    '''
    def __str__(self):
        return "Adds an 'OpenMP DO' directive to a loop"

    @property
    def name(self):
        ''' Returns the name of this transformation as a string '''
        return "OMPLoopTrans"

    @property
    def omp_schedule(self):
        ''' Returns the OpenMP schedule that will be specified by
            this transformation. The default schedule is 'static' '''
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

    def __init__(self, omp_schedule="static"):
        self._omp_schedule = ""
        # Although we create the _omp_schedule attribute above (so that
        # pylint doesn't complain), we actually set its value using
        # the setter method in order to make use of the latter's error
        # checking.
        self.omp_schedule = omp_schedule
        Transformation.__init__(self)

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
        :py:meth:`OMPLoopTrans.gen_code` is called), this node must be
        within (i.e. a child of) an OpenMP PARALLEL region.

        The optional reprod argument will cause a reproducible
        reduction to be generated if it is set to True, otherwise the
        default, non-reproducible OpenMP reduction will used. Note,
        reproducible in this case means obtaining the same results
        with the same number of OpenMP threads, not for different
        numbers of OpenMP threads.

        '''

        if reprod is None:
            import config
            reprod = config.REPRODUCIBLE_REDUCTIONS

        # Check that the supplied node is a Loop
        from psyGen import Loop
        if not isinstance(node, Loop):
            raise TransformationError("Cannot apply an OpenMP Loop "
                                      "directive to something that is "
                                      "not a loop")
        # Check we are not a sequential loop
        if node.loop_type == 'colours':
            raise TransformationError("Error in "+self.name+" transformation. "
                                      "The target loop is over colours and "
                                      "must be computed serially.")

        schedule = node.root

        # create a memento of the schedule and the proposed
        # transformation
        from undoredo import Memento
        keep = Memento(schedule, self, [node])

        # keep a reference to the node's original parent and its index as these
        # are required and will change when we change the node's location
        node_parent = node.parent
        node_position = node.position

        # add our orphan OpenMP loop directive setting its parent to
        # the node's parent and its children to the node
        from psyGen import OMPDoDirective
        directive = OMPDoDirective(parent=node_parent,
                                   children=[node],
                                   omp_schedule=self.omp_schedule,
                                   reprod=reprod)

        # add the OpenMP loop directive as a child of the node's parent
        node_parent.addchild(directive, index=node_position)

        # change the node's parent to be the loop directive
        node.parent = directive

        # remove the original loop
        node_parent.children.remove(node)

        return schedule, keep


class OMPParallelLoopTrans(OMPLoopTrans):

    ''' Adds an OpenMP PARALLEL DO directive to a loop.

        For example:

        >>> from parse import parse
        >>> from psyGen import PSyFactory
        >>> ast,invokeInfo=parse("dynamo.F90")
        >>> psy=PSyFactory("dynamo0.1").create(invokeInfo)
        >>> schedule=psy.invokes.get('invoke_v3_kernel_type').schedule
        >>> schedule.view()
        >>>
        >>> from transformations import OMPParallelLoopTrans
        >>> trans=OMPParallelLoopTrans()
        >>> new_schedule,memento=trans.apply(schedule.children[0])
        >>> new_schedule.view()

    '''

    @property
    def name(self):
        ''' Returns the name of this transformation as a string '''
        return "OMPParallelLoopTrans"

    def __str__(self):
        return "Add an 'OpenMP PARALLEL DO' directive with no validity checks"

    def _validate(self, node):
        ''' validity checks for input arguments '''
        # Check that the supplied Node is a Loop
        from psyGen import Loop
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

        '''

        self._validate(node)

        schedule = node.root
        # create a memento of the schedule and the proposed transformation
        from undoredo import Memento
        keep = Memento(schedule, self, [node])

        # keep a reference to the node's original parent and its index as these
        # are required and will change when we change the node's location
        node_parent = node.parent
        node_position = node.position

        # add our OpenMP loop directive setting its parent to the node's
        # parent and its children to the node
        from psyGen import OMPParallelDoDirective
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
        ''' Returns the name of this transformation as a string '''
        return "DynamoOMPParallelLoopTrans"

    def __str__(self):
        return "Add an OpenMP Parallel Do directive to a Dynamo loop"

    def apply(self, node):

        ''' Perform Dynamo specific loop validity checks then call the
        :py:meth:`~OMPParallelLoopTrans.apply` method of the
        :py:class:`base class <OMPParallelLoopTrans>`. '''
        OMPParallelLoopTrans._validate(self, node)

        # If the loop is not already coloured then check whether or not
        # it should be. If the field space is W3 then we don't need
        # to worry about colouring.
        if node.field_space.orig_name != "w3":
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
       :py:class:`base class <OMPParallelLoopTrans>`.  '''

    @property
    def name(self):
        ''' Returns the name of this transformation as a string '''
        return "GOceanOMPParallelLoopTrans"

    def __str__(self):
        return "Add an OpenMP Parallel Do directive to a GOcean loop"

    def apply(self, node):

        ''' Perform GOcean-specific loop validity checks then call
        :py:meth:`OMPParallelLoopTrans.apply`.

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
        ''' Returns the name of this transformation as a string '''
        return "Dynamo0p3OMPLoopTrans"

    def __str__(self):
        return "Add an OpenMP DO directive to a Dynamo 0.3 loop"

    def apply(self, node, reprod=None):
        '''Perform Dynamo 0.3 specific loop validity checks then call
        :py:meth:`OMPLoopTrans.apply`.

        '''

        if reprod is None:
            import config
            reprod = config.REPRODUCIBLE_REDUCTIONS

        # check node is a loop
        from psyGen import Loop
        if not isinstance(node, Loop):
            raise TransformationError("Error in "+self.name+" transformation."
                                      " The node is not a loop.")
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
        :py:class:`base class <OMPLoopTrans>`. '''

    @property
    def name(self):
        ''' Returns the name of this transformation as a string '''
        return "GOceanOMPLoopTrans"

    def __str__(self):
        return "Add an OpenMP DO directive to a GOcean loop"

    def apply(self, node):

        '''Perform GOcean specific loop validity checks then call
            `:py:meth:`OMPLoopTrans.apply`.

        '''
        # check node is a loop. Although this is not GOcean specific
        # it is required for the subsequent checks to function
        # correctly.
        from psyGen import Loop
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

    ''' Apply a colouring transformation to a loop (in order to permit a
        subsequent OpenMP parallelisation over colours). For example:

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
        ''' Returns the name of this transformation as a string '''
        return "LoopColourTrans"

    def apply(self, node):
        '''Converts the Loop represented by :py:obj:`node` into a
        nested loop where the outer loop is over colours and the inner
        loop is over cells of that colour.
        '''
        schedule = node.root

        # create a memento of the schedule and the proposed transformation
        from undoredo import Memento
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
        colour_loop.iteration_space = node.iteration_space
        colour_loop.set_lower_bound("start")
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
        ''' Returns the name of this transformation as a string '''
        return "KernelModuleInline"

    def apply(self, node, inline=True):
        '''Checks that the node is of the correct type (a Kernel) then marks
        the Kernel to be inlined, or not, depending on the value of
        the inline argument. If the inline argument is not passed the
        Kernel is marked to be inlined.'''
        # check node is a kernel
        from psyGen import Kern
        if not isinstance(node, Kern):
            raise TransformationError(
                "Error in KernelModuleInline transformation. The node is not "
                "a Kernel")

        schedule = node.root

        # create a memento of the schedule and the proposed transformation
        from undoredo import Memento
        keep = Memento(schedule, self, [node])

        # set kernel's inline status
        if node.module_inline == inline:
            # issue a warning here when we implement logging
            # print "Warning, Kernel inline is already set to "+str(inline)
            pass
        else:
            node.module_inline = inline

        return schedule, keep


class Dynamo0p3ColourTrans(ColourTrans):

    ''' Split a Dynamo 0.3 loop over cells into colours so that it can be
    parallelised. For example:

    >>> from parse import parse
    >>> from psyGen import PSyFactory
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

    * Only kernels with an iteration space of CELLS require colouring. Any
      other loop type will be rejected by this transformation.
    * Any kernel which has a field with 'INC' access must be coloured UNLESS
      that field is on w3 (or another discontinuous space)
    * A kernel may have at most one field with 'INC' access
    * Attempting to colour a kernel that updates a field on w3 (with INC
      access) should result in PSyclone issuing a warning
    * Attempting to colour any kernel that doesn't have a field with INC
      access should also result in PSyclone issuing a warning.
    * A separate colour map will be required for each field that is coloured
      (if an invoke contains >1 kernel call)

    '''

    def __str__(self):
        return "Split a Dynamo 0.3 loop over cells into colours"

    @property
    def name(self):
        ''' Returns the name of this transformation as a string '''
        return "Dynamo0p3LoopColourTrans"

    def apply(self, node):
        '''Performs Dynamo0.3-specific error checking and then uses the parent
        class to convert the Loop represented by :py:obj:`node` into a
        nested loop where the outer loop is over colours and the inner
        loop is over cells of that colour.

        '''
        # check node is a loop
        from psyGen import Loop
        if not isinstance(node, Loop):
            raise TransformationError("Error in DynamoColour transformation. "
                                      "The supplied node is not a loop")
        # Check we need colouring
        if node.field_space.orig_name == "w3":
            pass
            # TODO generate a warning here as we don't need to colour
            # a loop that updates a field on W3.

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
        from psyGen import OMPDirective
        if node.ancestor(OMPDirective):
            raise TransformationError("Cannot have a loop over colours "
                                      "within an OpenMP parallel region.")

        schedule, keep = ColourTrans.apply(self, node)

        return schedule, keep


class OMPParallelTrans(Transformation):

    ''' Create an OpenMP PARALLEL region by inserting directives. For
    example:

    >>> from parse import parse, ParseError
    >>> from psyGen import PSyFactory, GenerationError
    >>> api="gocean1.0"
    >>> filename="nemolite2d_alg.f90"
    >>> ast,invokeInfo=parse(filename,api=api,invoke_name="invoke")
    >>> psy=PSyFactory(api).create(invokeInfo)
    >>>
    >>> from psyGen import TransInfo
    >>> t=TransInfo()
    >>> ltrans = t.get_trans_name('GOceanOMPLoopTrans')
    >>> rtrans = t.get_trans_name('OMPParallelTrans')
    >>>
    >>> schedule=psy.invokes.get('invoke_0').schedule
    >>> schedule.view()
    >>> new_schedule=schedule
    >>>
    >>> # Apply the OpenMP Loop transformation to *every* loop
    >>> # in the schedule
    >>> for child in schedule.children:
    >>>     newschedule,memento=ltrans.apply(child)
    >>>     schedule = newschedule
    >>>
    >>> # Enclose all of these loops within a single OpenMP
    >>> # PARALLEL region
    >>> newschedule, _ = rtrans.apply(schedule.children)
    >>> newschedule.view()

    '''

    def __str__(self):
        return "Insert an OpenMP Parallel region"

    @property
    def name(self):
        ''' Returns the name of this transformation as a string '''
        return "OMPParallelTrans"

    def apply(self, nodes):
        '''Apply this transformation to a subset of the nodes within a
            schedule - i.e. enclose the specified Loops in the
            schedule within a single OpenMP region. :py:obj:`nodes`
            can be a single Node or a list of Nodes.

        '''
        from psyGen import OMPDirective, OMPParallelDirective

        # Check whether we've been passed a list of nodes or just a
        # single node. If the latter then we create ourselves a
        # list containing just that node.
        from psyGen import Node
        if isinstance(nodes, list) and isinstance(nodes[0], Node):
            node_list = nodes
        elif isinstance(nodes, Node):
            node_list = [nodes]
        else:
            arg_type = str(type(nodes))
            raise TransformationError("Error in OMPParallel transformation. "
                                      "Argument must be a single Node in a "
                                      "schedule or a list of Nodes in a "
                                      "schedule but have been passed an "
                                      "object of type: {0}".
                                      format(arg_type))

        # temporary dynamo0.3-specific test for haloexchange calls
        # existing within a parallel region. As we are going to
        # support this in the future, see #526, it does not warrant
        # making a separate dynamo-specific class.
        from psyGen import HaloExchange
        for node in node_list:
            if isinstance(node, HaloExchange):
                raise TransformationError(
                    "A halo exchange within a parallel region is not "
                    "supported")

        # Keep a reference to the parent of the nodes that are to be
        # enclosed within a parallel region. Also keep the index of
        # the first child to be enclosed as that will become the
        # position of the new !$omp parallel directive.
        node_parent = node_list[0].parent
        node_position = node_list[0].position

        if node_list[0].ancestor(OMPDirective):
            raise TransformationError("Error in OMPParallel transformation:" +
                                      " cannot create an OpenMP PARALLEL " +
                                      "region within another OpenMP region.")
        for child in node_list:
            if child.parent is not node_parent:
                raise TransformationError(
                    "Error in OMPParallel transformation: supplied nodes "
                    "are not children of the same Schedule/parent.")

        # create a memento of the schedule and the proposed
        # transformation
        schedule = node_list[0].root

        from undoredo import Memento
        keep = Memento(schedule, self)

        # Create the OpenMP parallel directive as a child of the
        # parent of the nodes being enclosed and with those nodes
        # as its children.
        # We slice the nodes list in order to get a new list object
        # (although the actual items in the list are still those in the
        # original). If we don't do this then we get an infinite
        # recursion in the new schedule.
        directive = OMPParallelDirective(parent=node_parent,
                                         children=node_list[:])

        # Change all of the affected children so that they have
        # the OpenMP directive as their parent. Use a slice
        # of the list of nodes so that we're looping over a local
        # copy of the list. Otherwise things get confused when
        # we remove children from the list.
        for child in node_list[:]:
            # Remove child from the parent's list of children
            node_parent.children.remove(child)
            child.parent = directive

        # Add the OpenMP region directive as a child of the parent
        # of the nodes being enclosed and at the original location
        # of the first of these nodes
        node_parent.addchild(directive,
                             index=node_position)

        return schedule, keep


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

      ny = my_field%grid%simulation_domain%ystop
      ...
      DO j = 1, ny-1

    In practice, the application of the constant loop bounds looks
    something like, e.g.:

    >>> from parse import parse
    >>> from psyGen import PSyFactory
    >>> import os
    >>> TEST_API = "gocean1.0"
    >>> _,info = parse(os.path.join("tests", "test_files", "gocean1p0",
    >>>                             "single_invoke.f90"),
    >>>                api=TEST_API)
    >>> psy = PSyFactory(TEST_API).create(info)
    >>> invoke = psy.invokes.get('invoke_0_compute_cu')
    >>> schedule = invoke.schedule
    >>>
    >>> from transformations import GOConstLoopBoundsTrans
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
        ''' Return the name of the Transformation as a string '''
        return "GOConstLoopBoundsTrans"

    def apply(self, node, const_bounds=True):

        # Check node is a Schedule
        from gocean1p0 import GOSchedule
        if not isinstance(node, GOSchedule):
            raise TransformationError("Error in GOConstLoopBoundsTrans: "
                                      "node is not a GOSchedule")

        from undoredo import Memento
        keep = Memento(node, self)

        node.const_loop_bounds = const_bounds

        return node, keep


class MoveTrans(Transformation):
    '''Provides a transformation to move a node in the tree. For
    example:

    >>> from parse import parse
    >>> from psyGen import PSyFactory
    >>> ast,invokeInfo=parse("dynamo.F90")
    >>> psy=PSyFactory("dynamo0.3").create(invokeInfo)
    >>> schedule=psy.invokes.get('invoke_v3_kernel_type').schedule
    >>> schedule.view()
    >>>
    >>> from transformations import MoveTrans
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
        ''' Returns the name of this transformation as a string '''
        return "Move"

    def _validate(self, node, location, position):
        ''' validity checks for input arguments '''

        # Check that the first argument is a Node
        from psyGen import Node
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

    def apply(self, node, location, position="before"):
        '''Move the node represented by :py:obj:`node` before location
        :py:obj:`location` (which is also a node) by default and after
        if the optional `position` argument is set to 'after'. An
        exception is raised if the move is invalid '''

        self._validate(node, location, position)

        schedule = node.root

        # create a memento of the schedule and the proposed transformation
        from undoredo import Memento
        keep = Memento(schedule, self, [node, location])

        parent = node.parent

        my_node = parent.children.pop(node.position)

        location_index = location.position
        if position == "before":
            schedule.children.insert(location_index, my_node)
        else:
            schedule.children.insert(location_index+1, my_node)

        return schedule, keep


class DynamoRedundantComputationTrans(Transformation):
    '''This transformation allows the user to modify a loop's bounds so
    that redundant computation will be performed. Redundant computation
    can result in halo exchanges being modified, new halo exchange being
    added or existing halo exchanges being removed.'''
    def __str__(self):
        return "Change iteration space to perform redundant computation"

    @property
    def name(self):
        ''' Returns the name of this transformation as a string '''
        return "RedundantComputation"

    def _validate(self, node, depth):
        '''Perform various checks to ensure that it is valid to apply the
        RedundantComputation transformation to the supplied node'''

        # check node is a loop
        from psyGen import Loop
        if not isinstance(node, Loop):
            raise TransformationError(
                "In the DynamoRedundantComputation transformation apply "
                "method the first argument is not a Loop")

        # check loop's parent is the schedule otherwise halo exchange
        # placement fails. The only current example when this would be
        # the case is when directives have been added.
        from psyGen import Schedule
        if not isinstance(node.parent, Schedule):
            raise TransformationError(
                "In the DynamoRedundantComputation transformation apply "
                "method the parent must be the schedule, but found "
                "{0}".format(type(node.parent)))

        import config
        if not config.DISTRIBUTED_MEMORY:
            raise TransformationError(
                "In the DynamoRedundantComputation transformation apply "
                "method distributed memory must be switched on")

        # loop must iterate over cells or dofs. This currently
        # precludes loops over colours. Note, an empty loop_type
        # iterates over cells
        if node.loop_type not in ["", "dofs"]:
                raise TransformationError(
                    "In the DynamoRedundantComputation transformation apply "
                    "method the loop must iterate over cells or dofs, but "
                    "found '{0}'".format(node.loop_type))

        if depth is None:
            if node.upper_bound_name in ["cell_halo", "dof_halo"]:
                if not node.upper_bound_index:
                    raise TransformationError(
                        "In the DynamoRedundantComputation transformation "
                        "apply method the loop is already set to the maximum "
                        "halo depth so this transformation does nothing")
                for call in node.calls():
                    for arg in call.arguments.args:
                        if arg.stencil:
                            raise TransformationError(
                                "In the DynamoRedundantComputation "
                                "transformation apply method the loop "
                                "contains field '{0}' with a stencil "
                                "access in kernel '{1}', so it is invalid "
                                "to set redundant computation to maximum "
                                "depth".format(arg.name, call.name))
        else:
            if not isinstance(depth, int):
                raise TransformationError(
                    "In the DynamoRedundantComputation transformation apply "
                    "method the supplied depth should be an integer")
            if depth < 1:
                raise TransformationError(
                    "In the DynamoRedundantComputation transformation apply "
                    "method the supplied depth is less than 1")

            if not node.field.discontinuous and depth == 1 and \
               node.iteration_space == "cells":
                raise TransformationError(
                    "In the DynamoRedundantComputation transformation apply "
                    "method the supplied depth must be greater than 1 as this "
                    "loop  modifies a continuous field")

            if node.upper_bound_name in ["cell_halo", "dof_halo"]:
                if node.upper_bound_index:
                    if node.upper_bound_index >= depth:
                        raise TransformationError(
                            "In the DynamoRedundantComputation transformation "
                            "apply method the supplied depth ({0}) must be "
                            "greater than the existing halo depth ({1})".
                            format(depth, node.upper_bound_index))
                else:
                    raise TransformationError(
                        "In the DynamoRedundantComputation transformation "
                        "apply method the loop is already set to the maximum "
                        "halo depth so can't be set to a fixed value")

    def apply(self, loop, depth=None):
        '''Apply the redundant computation tranformation to the loop
        :py:obj:`loop`. This transformation can be applied to loops iterating
        over 'cells or 'dofs'. if :py:obj:`depth` is set to a value then the
        value will be the depth of the field's halo over which redundant
        computation will be performed. If :py:obj:`depth` is not set to a
        value then redundant computation will be performed to the full depth
        of the field's halo.'''

        self._validate(loop, depth)

        schedule = loop.root

        # create a memento of the schedule and the proposed
        # transformation
        from undoredo import Memento
        keep = Memento(schedule, self, [loop, depth])

        if loop.loop_type == "":  # iteration space is cells
            loop.set_upper_bound("cell_halo", depth)
        else:  # iteration space is dofs
            loop.set_upper_bound("dof_halo", depth)

        # Add/remove halo exchanges as required due to the redundant
        # computation
        loop.update_halo_exchanges()

        return schedule, keep
