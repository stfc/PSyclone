# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2024, Science and Technology Facilities Council.
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
# Modified I. Kavcic, J. G. Wallwork, O. Brunt and L. Turner, Met Office

''' This module provides the various transformations that can be applied to
    PSyIR nodes. There are both general and API-specific transformation
    classes in this module where the latter typically apply API-specific
    checks before calling the base class for the actual transformation. '''

# pylint: disable=too-many-lines

import abc

from psyclone import psyGen
from psyclone.configuration import Config
from psyclone.core import Signature, VariablesAccessInfo
from psyclone.domain.lfric import (KernCallArgList, LFRicConstants, LFRicKern,
                                   LFRicLoop)
from psyclone.dynamo0p3 import (LFRicHaloExchangeEnd, LFRicHaloExchangeStart,
                                DynInvokeSchedule)
from psyclone.errors import InternalError
from psyclone.gocean1p0 import GOInvokeSchedule
from psyclone.nemo import NemoInvokeSchedule
from psyclone.psyGen import (Transformation, CodedKern, Kern, InvokeSchedule,
                             BuiltIn)
from psyclone.psyir.nodes import (
    ACCDataDirective, ACCDirective,
    ACCEnterDataDirective, ACCKernelsDirective, ACCLoopDirective,
    ACCParallelDirective, ACCRoutineDirective, Assignment, Call, CodeBlock,
    Directive, IntrinsicCall, Loop, Node, OMPDeclareTargetDirective,
    OMPDirective, OMPMasterDirective,
    OMPParallelDirective, OMPParallelDoDirective, OMPSerialDirective,
    OMPSingleDirective, OMPTaskloopDirective, PSyDataNode, Reference,
    Return, Routine, Schedule)
from psyclone.psyir.nodes.array_mixin import ArrayMixin
from psyclone.psyir.nodes.structure_member import StructureMember
from psyclone.psyir.nodes.structure_reference import StructureReference
from psyclone.psyir.symbols import (
    ArgumentInterface, DataSymbol, UnresolvedType, INTEGER_TYPE, ScalarType,
    Symbol, SymbolError)
from psyclone.psyir.transformations.loop_trans import LoopTrans
from psyclone.psyir.transformations.omp_loop_trans import OMPLoopTrans
from psyclone.psyir.transformations.parallel_loop_trans import \
    ParallelLoopTrans
from psyclone.psyir.transformations.region_trans import RegionTrans
from psyclone.psyir.transformations.transformation_error import \
    TransformationError


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
    child_kernels = node.walk(LFRicKern)
    for kern in child_kernels:
        if kern.is_intergrid:
            raise TransformationError(
                f"This Transformation cannot currently be applied to nodes "
                f"which have inter-grid kernels as descendents and {kern.name}"
                f" is such a kernel.")


class OMPTaskloopTrans(ParallelLoopTrans):
    '''
    Adds an OpenMP taskloop directive to a loop. Only one of grainsize or
    num_tasks must be specified.

    TODO: #1364 Taskloops do not yet support reduction clauses.

    :param grainsize: the grainsize to use in for this transformation.
    :type grainsize: int or None
    :param num_tasks: the num_tasks to use for this transformation.
    :type num_tasks: int or None
    :param bool nogroup: whether or not to use a nogroup clause for this
                         transformation. Default is False.

    For example:

    >>> from pysclone.parse.algorithm import parse
    >>> from psyclone.psyGen import PSyFactory
    >>> api = "gocean1.0"
    >>> ast, invokeInfo = parse(GOCEAN_SOURCE_FILE, api=api)
    >>> psy = PSyFactory(api).create(invokeInfo)
    >>>
    >>> from psyclone.transformations import OMPParallelTrans, OMPSingleTrans
    >>> from psyclone.transformations import OMPTaskloopTrans
    >>> from psyclone.psyir.transformations import OMPTaskwaitTrans
    >>> singletrans = OMPSingleTrans()
    >>> paralleltrans = OMPParallelTrans()
    >>> tasklooptrans = OMPTaskloopTrans()
    >>> taskwaittrans = OMPTaskwaitTrans()
    >>>
    >>> schedule = psy.invokes.get('invoke_0').schedule
    >>> # Uncomment the following line to see a text view of the schedule
    >>> # print(schedule.view())
    >>>
    >>> # Apply the OpenMP Taskloop transformation to *every* loop
    >>> # in the schedule.
    >>> # This ignores loop dependencies. These can be handled
    >>> # by the OMPTaskwaitTrans
    >>> for child in schedule.children:
    >>>     tasklooptrans.apply(child)
    >>> # Enclose all of these loops within a single OpenMP
    >>> # SINGLE region
    >>> singletrans.apply(schedule.children)
    >>> # Enclose all of these loops within a single OpenMP
    >>> # PARALLEL region
    >>> paralleltrans.apply(schedule.children)
    >>> # Ensure loop dependencies are satisfied
    >>> taskwaittrans.apply(schedule.children)
    >>> # Uncomment the following line to see a text view of the schedule
    >>> # print(schedule.view())

    '''
    def __init__(self, grainsize=None, num_tasks=None, nogroup=False):
        self._grainsize = None
        self._num_tasks = None
        self.omp_grainsize = grainsize
        self.omp_num_tasks = num_tasks
        self.omp_nogroup = nogroup
        super().__init__()

    def __str__(self):
        return "Adds an 'OpenMP TASKLOOP' directive to a loop"

    @property
    def omp_nogroup(self):
        '''
        Returns whether the nogroup clause should be specified for
        this transformation. By default the nogroup clause is applied.

        :returns: whether the nogroup clause should be specified by
                  this transformation.
        :rtype: bool
        '''
        return self._nogroup

    @omp_nogroup.setter
    def omp_nogroup(self, nogroup):
        '''
        Sets whether the nogroup clause should be specified for this
        transformation.

        :param bool nogroup: value to set whether the nogroup clause should be
                             used for this transformation.

        raises TypeError: if the nogroup parameter is not a bool.
        '''
        if not isinstance(nogroup, bool):
            raise TypeError(f"Expected nogroup to be a bool "
                            f"but got a {type(nogroup).__name__}")
        self._nogroup = nogroup

    @property
    def omp_grainsize(self):
        '''
        Returns the grainsize that will be specified by
        this transformation. By default the grainsize
        clause is not applied, so grainsize is None.

        :returns: The grainsize specified by this transformation.
        :rtype: int or None
        '''
        return self._grainsize

    @omp_grainsize.setter
    def omp_grainsize(self, value):
        '''
        Sets the grainsize that will be specified by
        this transformation. Checks the grainsize is
        a positive integer value or None.

        :param value: integer value to use in the grainsize clause.
        :type value: int or None

        :raises TransformationError: if value is not an int and is not None.
        :raises TransformationError: if value is negative.
        :raises TransformationError: if grainsize and num_tasks are \
                                     both specified.
        '''
        if (not isinstance(value, int)) and (value is not None):
            raise TransformationError(f"grainsize must be an integer or None, "
                                      f"got {type(value).__name__}")

        if (value is not None) and (value <= 0):
            raise TransformationError(f"grainsize must be a positive "
                                      f"integer, got {value}")

        if value is not None and self.omp_num_tasks is not None:
            raise TransformationError(
                "The grainsize and num_tasks clauses would both "
                "be specified for this Taskloop transformation")
        self._grainsize = value

    @property
    def omp_num_tasks(self):
        '''
        Returns the num_tasks that will be specified
        by this transformation. By default the num_tasks
        clause is not applied so num_tasks is None.

        :returns: The grainsize specified by this transformation.
        :rtype: int or None
        '''
        return self._num_tasks

    @omp_num_tasks.setter
    def omp_num_tasks(self, value):
        '''
        Sets the num_tasks that will be specified by
        this transformation. Checks that num_tasks is
        a positive integer value or None.

        :param value: integer value to use in the num_tasks clause.
        :type value: int or None

        :raises TransformationError: if value is not an int and is not None.
        :raises TransformationError: if value is negative.
        :raises TransformationError: if grainsize and num_tasks are \
                                     both specified.

        '''
        if (not isinstance(value, int)) and (value is not None):
            raise TransformationError(f"num_tasks must be an integer or None,"
                                      f" got {type(value).__name__}")

        if (value is not None) and (value <= 0):
            raise TransformationError(f"num_tasks must be a positive "
                                      f"integer, got {value}")

        if value is not None and self.omp_grainsize is not None:
            raise TransformationError(
                "The grainsize and num_tasks clauses would both "
                "be specified for this Taskloop transformation")
        self._num_tasks = value

    def _directive(self, children, collapse=None):
        '''
        Creates the type of directive needed for this sub-class of
        transformation.

        :param children: list of Nodes that will be the children of \
                         the created directive.
        :type children: list of :py:class:`psyclone.psyir.nodes.Node`
        :param int collapse: currently un-used but required to keep \
                             interface the same as in base class.
        :returns: the new node representing the directive in the AST.
        :rtype: :py:class:`psyclone.psyir.nodes.OMPTaskloopDirective`

        :raises NotImplementedError: if a collapse argument is supplied
        '''
        # TODO 1370: OpenMP loop functions don't support collapse
        if collapse:
            raise NotImplementedError(
                "The COLLAPSE clause is not yet supported for "
                "'!$omp taskloop' directives.")
        _directive = OMPTaskloopDirective(children=children,
                                          grainsize=self.omp_grainsize,
                                          num_tasks=self.omp_num_tasks,
                                          nogroup=self.omp_nogroup)
        return _directive

    def apply(self, node, options=None):
        '''Apply the OMPTaskloopTrans transformation to the specified node in
        a Schedule. This node must be a Loop since this transformation
        corresponds to wrapping the generated code with directives like so:

        .. code-block:: fortran

          !$OMP TASKLOOP
          do ...
             ...
          end do
          !$OMP END TASKLOOP

        At code-generation time (when
        :py:meth:`OMPTaskloopDirective.gen_code` is called), this node must be
        within (i.e. a child of) an OpenMP SERIAL region.

        If the keyword "nogroup" is specified in the options, it will cause a
        nogroup clause be generated if it is set to True. This will override
        the value supplied to the constructor, but will only apply to the
        apply call to which the value is supplied.

        :param node: the supplied node to which we will apply the \
                     OMPTaskloopTrans transformation
        :type node: :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations\
                        and validation.
        :type options: Optional[Dict[str, Any]]
        :param bool options["nogroup"]:
                indicating whether a nogroup clause should be applied to
                this taskloop.

        '''
        if not options:
            options = {}
        current_nogroup = self.omp_nogroup
        # If nogroup is specified it overrides that supplied to the
        # constructor of the Transformation, but will be reset at the
        # end of this function
        self.omp_nogroup = options.get("nogroup", current_nogroup)

        try:
            super().apply(node, options)
        finally:
            # Reset the nogroup value to the original value
            self.omp_nogroup = current_nogroup


class OMPDeclareTargetTrans(Transformation):
    '''
    Adds an OpenMP declare target directive to the specified routine.

    For example:

    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.nodes import Loop
    >>> from psyclone.transformations import OMPDeclareTargetTrans
    >>>
    >>> tree = FortranReader().psyir_from_source("""
    ...     subroutine my_subroutine(A)
    ...         integer, dimension(10, 10), intent(inout) :: A
    ...         integer :: i
    ...         integer :: j
    ...         do i = 1, 10
    ...             do j = 1, 10
    ...                 A(i, j) = 0
    ...             end do
    ...         end do
    ...     end subroutine
    ...     """
    >>> omptargettrans = OMPDeclareTargetTrans()
    >>> omptargettrans.apply(tree.walk(Routine)[0])

    will generate:

    .. code-block:: fortran

        subroutine my_subroutine(A)
            integer, dimension(10, 10), intent(inout) :: A
            integer :: i
            integer :: j
            !$omp declare target
            do i = 1, 10
                do j = 1, 10
                    A(i, j) = 0
                end do
            end do
        end subroutine

    '''
    def apply(self, node, options=None):
        ''' Insert an OMPDeclareTargetDirective inside the provided routine.

        :param node: the PSyIR routine to insert the directive into.
        :type node: :py:class:`psyclone.psyir.nodes.Routine`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

        '''
        self.validate(node, options)
        for child in node.children:
            if isinstance(child, OMPDeclareTargetDirective):
                return  # The routine is already marked with OMPDeclareTarget
        node.children.insert(0, OMPDeclareTargetDirective())

    def validate(self, node, options=None):
        ''' Check that an OMPDeclareTargetDirective can be inserted.

        :param node: the PSyIR node to validate.
        :type node: :py:class:`psyclone.psyir.nodes.Routine`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

        :raises TransformationError: if the node is not a Routine

        '''
        super().validate(node, options=options)
        # Check that the supplied Node is a Routine
        if not isinstance(node, Routine):
            raise TransformationError(
                f"The OMPDeclareTargetTrans must be applied to a Routine, "
                f"but found: '{type(node).__name__}'.")

        # Check that the kernel does not access any data or routines via a
        # module 'use' statement or that are not captured by the SymbolTable
        for candidate in node.walk((Reference, CodeBlock)):
            if isinstance(candidate, CodeBlock):
                names = candidate.get_symbol_names()
            else:
                names = [candidate.name]
            for name in names:
                try:
                    candidate.scope.symbol_table.lookup(name, scope_limit=node)
                except KeyError as err:
                    raise TransformationError(
                        f"Kernel '{node.name}' contains accesses to data "
                        f"(variable '{name}') that are not present in the "
                        f"Symbol Table(s) within the scope of this routine. "
                        f"Cannot transform such a kernel.") from err

        imported_variables = node.symbol_table.imported_symbols
        if imported_variables:
            raise TransformationError(
                f"The Symbol Table for kernel '{node.name}' contains the "
                f"following symbol(s) with imported interface: "
                f"{[sym.name for sym in imported_variables]}. If these "
                f"symbols represent data then they must first be converted"
                f" to kernel arguments using the KernelImportsToArguments "
                f"transformation. If the symbols represent external "
                f"routines then PSyclone cannot currently transform this "
                f"kernel for execution on an OpenMP target.")


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
    >>> ast, invokeInfo = parse(GOCEAN_SOURCE_FILE, api=api)
    >>> psy = PSyFactory(api).create(invokeInfo)
    >>>
    >>> from psyclone.psyGen import TransInfo
    >>> t = TransInfo()
    >>> ltrans = t.get_trans_name('ACCLoopTrans')
    >>> rtrans = t.get_trans_name('ACCParallelTrans')
    >>>
    >>> schedule = psy.invokes.get('invoke_0').schedule
    >>> # Uncomment the following line to see a text view of the schedule
    >>> # print(schedule.view())
    >>>
    >>> # Apply the OpenACC Loop transformation to *every* loop in the schedule
    >>> for child in schedule.children[:]:
    ...     ltrans.apply(child)
    >>>
    >>> # Enclose all of these loops within a single OpenACC parallel region
    >>> rtrans.apply(schedule)
    >>>

    '''
    # The types of node that must be excluded from the section of PSyIR
    # being transformed.
    excluded_node_types = (PSyDataNode,)

    def __init__(self):
        # Whether to add the "independent" clause
        # to the loop directive.
        self._independent = True
        self._sequential = False
        self._gang = False
        self._vector = False
        super().__init__()

    def __str__(self):
        return "Adds an 'OpenACC loop' directive to a loop"

    def _directive(self, children, collapse=None):
        '''
        Creates the ACCLoopDirective needed by this sub-class of
        transformation.

        :param children: list of child nodes of the new directive Node.
        :type children: list of :py:class:`psyclone.psyir.nodes.Node`
        :param int collapse: number of nested loops to collapse or None if
                             no collapse attribute is required.
        '''
        directive = ACCLoopDirective(children=children,
                                     collapse=collapse,
                                     independent=self._independent,
                                     sequential=self._sequential,
                                     gang=self._gang,
                                     vector=self._vector)
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
        :py:meth:`psyclone.psyir.nodes.ACCLoopDirective.gen_code` is called),
        this node must be within (i.e. a child of) a PARALLEL region.

        :param node: the supplied node to which we will apply the
                     Loop transformation.
        :type node: :py:class:`psyclone.psyir.nodes.Loop`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]
        :param int options["collapse"]: number of nested loops to collapse.
        :param bool options["independent"]: whether to add the "independent"
                clause to the directive (not strictly necessary within
                PARALLEL regions).
        :param bool options["sequential"]: whether to add the "seq" clause to
                the directive.
        :param bool options["gang"]: whether to add the "gang" clause to the
                directive.
        :param bool options["vector"]: whether to add the "vector" clause to
                the directive.

        '''
        # Store sub-class specific options. These are used when
        # creating the directive (in the _directive() method).
        if not options:
            options = {}
        self._independent = options.get("independent", True)
        self._sequential = options.get("sequential", False)
        self._gang = options.get("gang", False)
        self._vector = options.get("vector", False)

        # Call the apply() method of the base class
        super().apply(node, options)


class OMPParallelLoopTrans(OMPLoopTrans):

    ''' Adds an OpenMP PARALLEL DO directive to a loop.

        For example:

        >>> from psyclone.parse.algorithm import parse
        >>> from psyclone.psyGen import PSyFactory
        >>> ast, invokeInfo = parse("dynamo.F90")
        >>> psy = PSyFactory("dynamo0.3").create(invokeInfo)
        >>> schedule = psy.invokes.get('invoke_v3_kernel_type').schedule
        >>> # Uncomment the following line to see a text view of the schedule
        >>> # print(schedule.view())
        >>>
        >>> from psyclone.transformations import OMPParallelLoopTrans
        >>> trans = OMPParallelLoopTrans()
        >>> trans.apply(schedule.children[0])
        >>> # Uncomment the following line to see a text view of the schedule
        >>> # print(schedule.view())

    '''
    def __str__(self):
        return "Add an 'OpenMP PARALLEL DO' directive"

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
        :type options: Optional[Dict[str, Any]]
        '''
        self.validate(node, options=options)

        # keep a reference to the node's original parent and its index as these
        # are required and will change when we change the node's location
        node_parent = node.parent
        node_position = node.position

        # add our OpenMP loop directive setting its parent to the node's
        # parent and its children to the node
        directive = OMPParallelDoDirective(children=[node.detach()],
                                           omp_schedule=self.omp_schedule)

        # add the OpenMP loop directive as a child of the node's parent
        node_parent.addchild(directive, index=node_position)


class DynamoOMPParallelLoopTrans(OMPParallelLoopTrans):

    ''' Dynamo-specific OpenMP loop transformation. Adds Dynamo specific
        validity checks. Actual transformation is done by the
        :py:class:`base class <OMPParallelLoopTrans>`.

        :param str omp_directive: choose which OpenMP loop directive to use.
            Defaults to "do".
        :param str omp_schedule: the OpenMP schedule to use. Must be one of
            'runtime', 'static', 'dynamic', 'guided' or 'auto'. Defaults to
            'static'.

    '''
    def __init__(self, omp_directive="do", omp_schedule="static"):
        super().__init__(omp_directive=omp_directive,
                         omp_schedule=omp_schedule)

    def __str__(self):
        return "Add an OpenMP Parallel Do directive to a Dynamo loop"

    def validate(self, node, options=None):
        '''
        Perform LFRic-specific loop validity checks then call the `validate`
        method of the base class.

        :param node: the Node in the Schedule to check
        :type node: :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

        :raises TransformationError: if the supplied Node is not a LFRicLoop.
        :raises TransformationError: if the associated loop requires
            colouring.
        '''
        if not isinstance(node, LFRicLoop):
            raise TransformationError(
                f"Error in {self.name} transformation. The supplied node "
                f"must be a LFRicLoop but got '{type(node).__name__}'")

        # If the loop is not already coloured then check whether or not
        # it should be. If the field space is discontinuous (including
        # any_discontinuous_space) then we don't need to worry about
        # colouring.
        const = LFRicConstants()
        if node.field_space.orig_name not in const.VALID_DISCONTINUOUS_NAMES:
            if node.loop_type != 'colour' and node.has_inc_arg():
                raise TransformationError(
                    f"Error in {self.name} transformation. The kernel has an "
                    f"argument with INC access. Colouring is required.")
        # As this is a domain-specific loop, we don't perform general
        # dependence analysis because it is too conservative and doesn't
        # account for the special steps taken for such a loop at code-
        # generation time (e.g. the way we ensure variables are given the
        # correct sharing attributes).
        local_options = options.copy() if options else {}
        local_options["force"] = True
        super().validate(node, options=local_options)


class GOceanOMPParallelLoopTrans(OMPParallelLoopTrans):

    '''GOcean specific OpenMP Do loop transformation. Adds GOcean
       specific validity checks (that supplied Loop is an inner or outer
       loop). Actual transformation is done by
       :py:class:`base class <OMPParallelLoopTrans>`.

        :param str omp_directive: choose which OpenMP loop directive to use. \
            Defaults to "do".
        :param str omp_schedule: the OpenMP schedule to use. Must be one of \
            'runtime', 'static', 'dynamic', 'guided' or 'auto'. Defaults to \
            'static'.

    '''
    def __init__(self, omp_directive="do", omp_schedule="static"):
        super().__init__(omp_directive=omp_directive,
                         omp_schedule=omp_schedule)

    def __str__(self):
        return "Add an OpenMP Parallel Do directive to a GOcean loop"

    def apply(self, node, options=None):
        ''' Perform GOcean-specific loop validity checks then call
        :py:meth:`OMPParallelLoopTrans.apply`.

        :param node: a Loop node from an AST.
        :type node: :py:class:`psyclone.psyir.nodes.Loop`
        :param options: a dictionary with options for transformations\
                        and validation.
        :type options: Optional[Dict[str, Any]]

        :raises TransformationError: if the supplied node is not an inner or\
            outer loop.

        '''
        self.validate(node, options=options)

        # Check we are either an inner or outer loop
        if node.loop_type not in ["inner", "outer"]:
            raise TransformationError(
                "Error in "+self.name+" transformation.  The requested loop"
                " is not of type inner or outer.")

        OMPParallelLoopTrans.apply(self, node)


class Dynamo0p3OMPLoopTrans(OMPLoopTrans):

    ''' LFRic (Dynamo 0.3) specific orphan OpenMP loop transformation. Adds
    Dynamo-specific validity checks.

    :param str omp_schedule: the OpenMP schedule to use. Must be one of \
        'runtime', 'static', 'dynamic', 'guided' or 'auto'. Defaults to \
        'static'.

    '''
    def __init__(self, omp_schedule="static"):
        super().__init__(omp_directive="do", omp_schedule=omp_schedule)

    def __str__(self):
        return "Add an OpenMP DO directive to a Dynamo 0.3 loop"

    def validate(self, node, options=None):
        ''' Perform LFRic (Dynamo 0.3) specific loop validity checks for the
        OMPLoopTrans.

        :param node: the Node in the Schedule to check
        :type node: :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations \
                        and validation.
        :type options: Optional[Dict[str, Any]]
        :param bool options["reprod"]: \
            indicating whether reproducible reductions should be used. \
            By default the value from the config file will be used.

        :raises TransformationError: if an OMP loop transform would create \
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

        # This transformation allows to parallelise loops with potential
        # dependencies because we use cell colouring to guarantee that
        # neighbours are not updated at the same time.
        options["force"] = True
        super().validate(node, options=options)

        # If the loop is not already coloured then check whether or not
        # it should be
        if node.loop_type != 'colour' and node.has_inc_arg():
            raise TransformationError(
                f"Error in {self.name} transformation. The kernel has an "
                f"argument with INC access. Colouring is required.")

    def apply(self, node, options=None):
        ''' Apply LFRic (Dynamo 0.3) specific OMPLoopTrans.

        :param node: the Node in the Schedule to check.
        :type node: :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations \
                        and validation.
        :type options: Optional[Dict[str, Any]]
        :param bool options["reprod"]: \
                indicating whether reproducible reductions should be used. \
                By default the value from the config file will be used.

        '''
        if not options:
            options = {}

        # Since this function potentially modifies the user's option
        # dictionary, create a copy:
        options = options.copy()
        # Make sure the default is set:
        options["reprod"] = options.get("reprod",
                                        Config.get().reproducible_reductions)

        # This transformation allows to parallelise loops with potential
        # dependencies because we use cell colouring to guarantee that
        # neighbours are not updated at the same time.
        options["force"] = True

        super().apply(node, options)


class GOceanOMPLoopTrans(OMPLoopTrans):

    ''' GOcean-specific orphan OpenMP loop transformation. Adds GOcean
        specific validity checks (that the node is either an inner or outer
        Loop).

        :param str omp_directive: choose which OpenMP loop directive to use. \
            Defaults to "do".
        :param str omp_schedule: the OpenMP schedule to use. Must be one of \
            'runtime', 'static', 'dynamic', 'guided' or 'auto'. Defaults to \
            'static'.

        '''
    def __init__(self, omp_directive="do", omp_schedule="static"):
        super().__init__(omp_directive=omp_directive,
                         omp_schedule=omp_schedule)

    def __str__(self):
        return "Add the selected OpenMP loop directive to a GOcean loop"

    def validate(self, node, options=None):
        '''
        Checks that the supplied node is a valid target for parallelisation
        using OMP directives.

        :param node: the candidate loop for parallelising using OMP Do.
        :type node: :py:class:`psyclone.psyir.nodes.Loop`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

        :raises TransformationError: if the loop_type of the supplied Loop is \
                                     not "inner" or "outer".

        '''
        super().validate(node, options=options)

        # Check we are either an inner or outer loop
        if node.loop_type not in ["inner", "outer"]:
            raise TransformationError("Error in "+self.name+" transformation."
                                      " The requested loop is not of type "
                                      "inner or outer.")


class ColourTrans(LoopTrans):
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
    >>>     ctrans.apply(child)
    >>>
    >>> # Uncomment the following line to see a text view of the schedule
    >>> # print(schedule.view())

    '''
    def __str__(self):
        return "Split a loop into colours"

    def apply(self, node, options=None):
        '''
        Converts the Loop represented by :py:obj:`node` into a
        nested loop where the outer loop is over colours and the inner
        loop is over cells of that colour.

        :param node: the loop to transform.
        :type node: :py:class:`psyclone.psyir.nodes.Loop`
        :param options: options for the transformation.
        :type options: Optional[Dict[str, Any]]

        '''
        self.validate(node, options=options)

        colours_loop = self._create_colours_loop(node)

        # Add this loop as a child of the original node's parent
        node.parent.addchild(colours_loop, index=node.position)

        # Add contents of node to colour loop.
        colours_loop.loop_body[0].loop_body.children.extend(
            node.loop_body.pop_all_children())

        # remove original loop
        node.detach()

    def _create_colours_loop(self, node):
        '''
        Creates a nested loop (colours, and cells of a given colour) to
        replace the supplied loop over cells.

        :param node: the loop for which to create a coloured version.
        :type node: :py:class:`psyclone.psyir.nodes.Loop`

        :returns: doubly-nested loop over colours and cells of a given colour.
        :rtype: :py:class:`psyclone.psyir.nodes.Loop`

        :raises NotImplementedError: this method must be overridden in an \
                                     API-specific sub-class.
        '''
        raise InternalError("_create_colours_loop() must be overridden in an "
                            "API-specific sub-class.")


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
    >>>     ctrans.apply(child)
    >>>
    >>> # Then apply OpenMP to each of the colour loops
    >>> for child in schedule.children:
    >>>     otrans.apply(child.children[0])
    >>>
    >>> # Uncomment the following line to see a text view of the schedule
    >>> # print(schedule.view())

    Colouring in the LFRic (Dynamo 0.3) API is subject to the following rules:

    * Only kernels which operate on 'CELL_COLUMN's and which increment a
      field on a continuous function space require colouring. Kernels that
      update a field on a discontinuous function space will cause this
      transformation to raise an exception. Kernels that only write to a field
      on a continuous function space also do not require colouring but are
      permitted.
    * A kernel may have at most one field with 'GH_INC' access.
    * A separate colour map will be required for each field that is coloured
      (if an invoke contains >1 kernel call).

    '''
    def __str__(self):
        return "Split a Dynamo 0.3 loop over cells into colours"

    def apply(self, node, options=None):
        '''Performs Dynamo0.3-specific error checking and then uses the parent
        class to convert the Loop represented by :py:obj:`node` into a
        nested loop where the outer loop is over colours and the inner
        loop is over cells of that colour.

        :param node: the loop to transform.
        :type node: :py:class:`psyclone.domain.lfric.LFRicLoop`
        :param options: a dictionary with options for transformations.\
        :type options: Optional[Dict[str, Any]]

        '''
        # check node is a loop
        super().validate(node, options=options)

        # Check we need colouring
        const = LFRicConstants()
        if node.field_space.orig_name in \
           const.VALID_DISCONTINUOUS_NAMES:
            raise TransformationError(
                "Error in DynamoColour transformation. Loops iterating over "
                "a discontinuous function space are not currently supported.")

        # Colouring is only necessary (and permitted) if the loop is
        # over cells. Since this is the default it is represented by
        # an empty string.
        if node.loop_type != "":
            raise TransformationError(
                f"Error in DynamoColour transformation. Only loops over cells "
                f"may be coloured but this loop is over {node.loop_type}")

        # Check whether we have a field that has INC access
        if not node.has_inc_arg():
            # TODO generate a warning here as we don't need to colour
            # a loop that does not update a field with INC access
            pass

        # Check that we're not attempting to colour a loop that is
        # already within an OpenMP region (because the loop over
        # colours *must* be sequential)
        if node.ancestor(OMPDirective):
            raise TransformationError("Cannot have a loop over colours "
                                      "within an OpenMP parallel region.")

        super().apply(node, options=options)

    def _create_colours_loop(self, node):
        '''
        Creates a nested loop (colours, and cells of a given colour) which
        can be used to replace the supplied loop over cells.

        :param node: the loop for which to create a coloured version.
        :type node: :py:class:`psyclone.psyir.nodes.Loop`

        :returns: doubly-nested loop over colours and cells of a given colour.
        :rtype: :py:class:`psyclone.psyir.nodes.Loop`

        '''
        # Create a colours loop. This loops over colours and must be run
        # sequentially.
        colours_loop = node.__class__(parent=node.parent, loop_type="colours")
        colours_loop.field_space = node.field_space
        colours_loop.iteration_space = node.iteration_space
        colours_loop.set_lower_bound("start")
        colours_loop.set_upper_bound("ncolours")

        # Create a colour loop. This loops over cells of a particular colour
        # and can be run in parallel.
        colour_loop = node.__class__(parent=colours_loop.loop_body,
                                     loop_type="colour")
        colour_loop.field_space = node.field_space
        colour_loop.field_name = node.field_name
        colour_loop.iteration_space = node.iteration_space
        colour_loop.set_lower_bound("start")
        colour_loop.kernel = node.kernel

        if node.upper_bound_name in LFRicConstants().HALO_ACCESS_LOOP_BOUNDS:
            # If the original loop went into the halo then this coloured loop
            # must also go into the halo.
            index = node.upper_bound_halo_depth
            colour_loop.set_upper_bound("colour_halo", index)
        else:
            # No halo access.
            colour_loop.set_upper_bound("ncolour")

        # Add this loop as a child of our loop over colours
        colours_loop.loop_body.addchild(colour_loop)

        return colours_loop


class ParallelRegionTrans(RegionTrans, metaclass=abc.ABCMeta):
    '''
    Base class for transformations that create a parallel region.

    '''
    # The types of node that must be excluded from the section of PSyIR
    # being transformed.
    excluded_node_types = (CodeBlock, Return, psyGen.HaloExchange)

    def __init__(self):
        # Holds the class instance or create call for the type of
        # parallel region to generate
        self._directive_factory = None
        super().__init__()

    @abc.abstractmethod
    def __str__(self):
        pass  # pragma: no cover

    def validate(self, node_list, options=None):
        # pylint: disable=arguments-renamed
        '''
        Check that the supplied list of Nodes are eligible to be
        put inside a parallel region.

        :param list node_list: list of nodes to put into a parallel region
        :param options: a dictionary with options for transformations.\
        :type options: Optional[Dict[str, Any]]
        :param bool options["node-type-check"]: this flag controls whether \
            or not the type of the nodes enclosed in the region should be \
            tested to avoid using unsupported nodes inside a region.

        :raises TransformationError: if the supplied node is an \
            InvokeSchedule rather than being within an InvokeSchedule.
        :raises TransformationError: if the supplied nodes are not all \
            children of the same parent (siblings).

        '''
        if isinstance(node_list[0], InvokeSchedule):
            raise TransformationError(
                f"A {self.name} transformation cannot be applied to an "
                f"InvokeSchedule but only to one or more nodes from within an "
                f"InvokeSchedule.")

        node_parent = node_list[0].parent

        for child in node_list:
            if child.parent is not node_parent:
                raise TransformationError(
                    f"Error in {self.name} transformation: supplied nodes are "
                    f"not children of the same parent.")
        super().validate(node_list, options)

    def apply(self, target_nodes, options=None):
        # pylint: disable=arguments-renamed
        '''
        Apply this transformation to a subset of the nodes within a
        schedule - i.e. enclose the specified Loops in the
        schedule within a single parallel region.

        :param target_nodes: a single Node or a list of Nodes.
        :type target_nodes: (list of) :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]
        :param bool options["node-type-check"]: this flag controls if the \
                type of the nodes enclosed in the region should be tested \
                to avoid using unsupported nodes inside a region.

        '''

        # Check whether we've been passed a list of nodes or just a
        # single node. If the latter then we create ourselves a
        # list containing just that node.
        node_list = self.get_node_list(target_nodes)
        self.validate(node_list, options)

        # Keep a reference to the parent of the nodes that are to be
        # enclosed within a parallel region. Also keep the index of
        # the first child to be enclosed as that will become the
        # position of the new !$omp parallel directive.
        node_parent = node_list[0].parent
        node_position = node_list[0].position

        # Create the parallel directive as a child of the
        # parent of the nodes being enclosed and with those nodes
        # as its children.
        # pylint: disable=not-callable
        directive = self._directive_factory(
            children=[node.detach() for node in node_list])

        # Add the region directive as a child of the parent
        # of the nodes being enclosed and at the original location
        # of the first of these nodes
        node_parent.addchild(directive, index=node_position)


class OMPSingleTrans(ParallelRegionTrans):
    '''
    Create an OpenMP SINGLE region by inserting directives. The most
    likely use case for this transformation is to wrap around task-based
    transformations. The parent region for this should usually also be
    a OMPParallelTrans.

    :param bool nowait: whether to apply a nowait clause to this \
                       transformation. The default value is False

    For example:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.psyGen import PSyFactory
    >>> api = "gocean1.0"
    >>> ast, invokeInfo = parse(GOCEAN_SOURCE_FILE, api=api)
    >>> psy = PSyFactory(api).create(invokeInfo)
    >>>
    >>> from psyclone.transformations import OMPParallelTrans, OMPSingleTrans
    >>> singletrans = OMPSingleTrans()
    >>> paralleltrans = OMPParallelTrans()
    >>>
    >>> schedule = psy.invokes.get('invoke_0').schedule
    >>> # Uncomment the following line to see a text view of the schedule
    >>> # print(schedule.view())
    >>>
    >>> # Enclose all of these loops within a single OpenMP
    >>> # SINGLE region
    >>> singletrans.apply(schedule.children)
    >>> # Enclose all of these loops within a single OpenMP
    >>> # PARALLEL region
    >>> paralleltrans.apply(schedule.children)
    >>> # Uncomment the following line to see a text view of the schedule
    >>> # print(schedule.view())

    '''
    # The types of node that this transformation cannot enclose
    excluded_node_types = (CodeBlock, Return, ACCDirective,
                           psyGen.HaloExchange, OMPSerialDirective,
                           OMPParallelDirective)

    def __init__(self, nowait=False):
        super().__init__()
        # Set the type of directive that the base class will use
        self._directive_factory = self._directive
        # Store whether this single directive has a barrier or not
        self._omp_nowait = nowait

    def __str__(self):
        return "Insert an OpenMP Single region"

    @property
    def omp_nowait(self):
        ''' :returns: whether or not this Single region uses a nowait \
                      clause to remove the end barrier.
            :rtype: bool
        '''
        return self._omp_nowait

    @property
    def name(self):
        '''
        :returns: the name of this transformation.
        :rtype: str
        '''
        return "OMPSingleTrans"

    @omp_nowait.setter
    def omp_nowait(self, value):
        ''' Sets the nowait property that will be specified by
            this transformation. Checks that the value supplied in
            :py:obj:`value` is a bool

            :param bool value: whether this Single clause should have a \
                               nowait applied.

            :raises TypeError: if the value parameter is not a bool.

        '''
        if not isinstance(value, bool):
            raise TypeError(f"Expected nowait to be a bool "
                            f"but got a {type(value).__name__}")
        self._omp_nowait = value

    def _directive(self, children):
        '''
        Creates the type of directive needed for this sub-class of
        transformation.

        :param children: list of Nodes that will be the children of \
                         the created directive.
        :type children: list of :py:class:`psyclone.psyir.nodes.Node`

        :returns: The directive created for the OpenMP Single Directive
        :rtype: :py:class:`psyclone.psyGen.OMPSingleDirective`

        '''
        _directive = OMPSingleDirective(children=children,
                                        nowait=self.omp_nowait)
        return _directive

    def apply(self, node_list, options=None):
        # pylint: disable=arguments-renamed
        '''Apply the OMPSingleTrans transformation to the specified node in a
        Schedule.

        At code-generation time this node must be within (i.e. a child of)
        an OpenMP PARALLEL region. Code generation happens when
        :py:meth:`OMPLoopDirective.gen_code` is called, or when the PSyIR
        tree is given to a backend.

        If the keyword "nowait" is specified in the options, it will cause a
        nowait clause to be added if it is set to True, otherwise no clause
        will be added.

        :param node_list: the supplied node or node list to which we will \
                          apply the OMPSingleTrans transformation
        :type node_list: (a list of) :py:class:`psyclone.psyir.nodes.Node`
        :param options: a list with options for transformations \
                        and validation.
        :type options: Optional[Dict[str, Any]]
        :param bool options["nowait"]:
                indicating whether or not to use a nowait clause on this \
                single region.

        '''
        if not options:
            options = {}
        if options.get("nowait") is not None:
            self.omp_nowait = options.get("nowait")

        super().apply(node_list, options)


class OMPMasterTrans(ParallelRegionTrans):
    '''
    Create an OpenMP MASTER region by inserting directives. The most
    likely use case for this transformation is to wrap around task-based
    transformations. Note that adding this directive requires a parent
    OpenMP parallel region (which can be inserted by OMPParallelTrans),
    otherwise it will produce an error in generation-time.

    For example:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.psyGen import PSyFactory
    >>> api = "gocean1.0"
    >>> ast, invokeInfo = parse(GOCEAN_SOURCE_FILE, api=api)
    >>> psy = PSyFactory(api).create(invokeInfo)
    >>>
    >>> from psyclone.transformations import OMPParallelTrans, OMPMasterTrans
    >>> mastertrans = OMPMasterTrans()
    >>> paralleltrans = OMPParallelTrans()
    >>>
    >>> schedule = psy.invokes.get('invoke_0').schedule
    >>> # Uncomment the following line to see a text view of the schedule
    >>> # print(schedule.view())
    >>>
    >>> # Enclose all of these loops within a single OpenMP
    >>> # MASTER region
    >>> mastertrans.apply(schedule.children)
    >>> # Enclose all of these loops within a single OpenMP
    >>> # PARALLEL region
    >>> paralleltrans.apply(schedule.children)
    >>> # Uncomment the following line to see a text view of the schedule
    >>> # print(schedule.view())

    '''
    # The types of node that this transformation cannot enclose
    excluded_node_types = (CodeBlock, Return, ACCDirective,
                           psyGen.HaloExchange, OMPSerialDirective,
                           OMPParallelDirective)

    def __init__(self):
        super().__init__()
        # Set the type of directive that the base class will use
        self._directive_factory = OMPMasterDirective

    def __str__(self):
        return "Insert an OpenMP Master region"

    @property
    def name(self):
        '''
        :returns: the name of this transformation as a string.
        :rtype: str
        '''
        return "OMPMasterTrans"


class OMPParallelTrans(ParallelRegionTrans):
    '''
    Create an OpenMP PARALLEL region by inserting directives. For
    example:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.parse.utils import ParseError
    >>> from psyclone.psyGen import PSyFactory
    >>> from psyclone.errors import GenerationError
    >>> api = "gocean1.0"
    >>> ast, invokeInfo = parse(GOCEAN_SOURCE_FILE, api=api)
    >>> psy = PSyFactory(api).create(invokeInfo)
    >>>
    >>> from psyclone.psyGen import TransInfo
    >>> t = TransInfo()
    >>> ltrans = t.get_trans_name('GOceanOMPLoopTrans')
    >>> rtrans = t.get_trans_name('OMPParallelTrans')
    >>>
    >>> schedule = psy.invokes.get('invoke_0').schedule
    >>> # Uncomment the following line to see a text view of the schedule
    >>> # print(schedule.view())
    >>>
    >>> # Apply the OpenMP Loop transformation to *every* loop
    >>> # in the schedule
    >>> for child in schedule.children:
    >>>     ltrans.apply(child)
    >>>
    >>> # Enclose all of these loops within a single OpenMP
    >>> # PARALLEL region
    >>> rtrans.apply(schedule.children)
    >>> # Uncomment the following line to see a text view of the schedule
    >>> # print(schedule.view())

    '''
    # The types of node that this transformation cannot enclose
    excluded_node_types = (CodeBlock, Return, ACCDirective,
                           psyGen.HaloExchange)

    def __init__(self):
        super().__init__()
        # Set the type of directive that the base class will use
        self._directive_factory = OMPParallelDirective.create

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
        :type options: Optional[Dict[str, Any]]
        :param bool options["node-type-check"]: this flag controls if the \
                type of the nodes enclosed in the region should be tested \
                to avoid using unsupported nodes inside a region.

        :raises TransformationError: if the target Nodes are already within \
                                     some OMP parallel region.
        '''
        if node_list[0].ancestor(OMPDirective):
            raise TransformationError("Error in OMPParallel transformation:" +
                                      " cannot create an OpenMP PARALLEL " +
                                      "region within another OpenMP region.")

        # Now call the general validation checks
        super().validate(node_list, options)


class ACCParallelTrans(ParallelRegionTrans):
    '''
    Create an OpenACC parallel region by inserting an 'acc parallel'
    directive.

    >>> from psyclone.psyGen import TransInfo
    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.backend.fortran import FortranWriter
    >>> from psyclone.psyir.nodes import Loop
    >>> psyir = FortranReader().psyir_from_source("""
    ... program do_loop
    ...     real, dimension(10) :: A
    ...     integer i
    ...     do i = 1, 10
    ...       A(i) = i
    ...     end do
    ... end program do_loop
    ... """)
    >>> ptrans = TransInfo().get_trans_name('ACCParallelTrans')
    >>>
    >>> # Enclose the loop within a OpenACC PARALLEL region
    >>> ptrans.apply(psyir.walk(Loop))
    >>> print(FortranWriter()(psyir))
    program do_loop
      real, dimension(10) :: a
      integer :: i
    <BLANKLINE>
      !$acc parallel default(present)
      do i = 1, 10, 1
        a(i) = i
      enddo
      !$acc end parallel
    <BLANKLINE>
    end program do_loop
    <BLANKLINE>

    '''
    excluded_node_types = (CodeBlock, Return, PSyDataNode,
                           ACCDataDirective, ACCEnterDataDirective,
                           psyGen.HaloExchange)

    def __init__(self, default_present=True):
        super().__init__()
        if not isinstance(default_present, bool):
            raise TypeError(
                f"The provided 'default_present' argument must be a "
                f"boolean, but found '{default_present}'."
            )
        self._default_present = default_present

    def __str__(self):
        return "Insert an OpenACC Parallel region"

    def validate(self, node_list, options=None):
        '''
        Validate this transformation.

        :param node_list: a single Node or a list of Nodes.
        :type node_list: :py:class:`psyclone.psyir.nodes.Node` |
            List[:py:class:`psyclone.psyir.nodes.Node`]
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]
        :param bool options["node-type-check"]: this flag controls if the
            type of the nodes enclosed in the region should be tested to
            avoid using unsupported nodes inside a region.
        :param bool options["default_present"]: this flag controls if the
            inserted directive should include the default_present clause.

        '''
        super().validate(node_list, options)
        if options is not None and "default_present" in options:
            if not isinstance(options["default_present"], bool):
                raise TransformationError(
                    f"The provided 'default_present' option must be a "
                    f"boolean, but found '{options['default_present']}'."
                )

    def apply(self, target_nodes, options=None):
        '''
        Encapsulate given nodes with the ACCParallelDirective.

        :param target_nodes: a single Node or a list of Nodes.
        :type target_nodes: :py:class:`psyclone.psyir.nodes.Node` |
            List[:py:class:`psyclone.psyir.nodes.Node`]
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]
        :param bool options["node-type-check"]: this flag controls if the
            type of the nodes enclosed in the region should be tested to
            avoid using unsupported nodes inside a region.
        :param bool options["default_present"]: this flag controls if the
            inserted directive should include the default_present clause.

        '''
        if not options:
            options = {}
        # Check whether we've been passed a list of nodes or just a
        # single node. If the latter then we create ourselves a
        # list containing just that node.
        node_list = self.get_node_list(target_nodes)
        self.validate(node_list, options)

        # Keep a reference to the parent of the nodes that are to be
        # enclosed within a parallel region. Also keep the index of
        # the first child to be enclosed as that will become the
        # position of the new !$omp parallel directive.
        node_parent = node_list[0].parent
        node_position = node_list[0].position

        # Create the parallel directive
        directive = ACCParallelDirective(
            children=[node.detach() for node in node_list])
        directive.default_present = options.get("default_present",
                                                self._default_present)

        # Add the region directive as a child of the parent
        # of the nodes being enclosed and at the original location
        # of the first of these nodes
        node_parent.addchild(directive, index=node_position)


class MoveTrans(Transformation):
    '''Provides a transformation to move a node in the tree. For
    example:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.psyGen import PSyFactory
    >>> ast,invokeInfo=parse("dynamo.F90")
    >>> psy=PSyFactory("dynamo0.3").create(invokeInfo)
    >>> schedule=psy.invokes.get('invoke_v3_kernel_type').schedule
    >>> # Uncomment the following line to see a text view of the schedule
    >>> # print(schedule.view())
    >>>
    >>> from psyclone.transformations import MoveTrans
    >>> trans=MoveTrans()
    >>> trans.apply(schedule.children[0], schedule.children[2],
    ...             options = {"position":"after")
    >>> # Uncomment the following line to see a text view of the schedule
    >>> # print(schedule.view())

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
        # pylint: disable=arguments-differ
        ''' validity checks for input arguments.

        :param node: the node to be moved.
        :type node: :py:class:`psyclone.psyir.nodes.Node`
        :param location: node before or after which the given node\
            should be moved.
        :type location: :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]
        :param str options["position"]: either 'before' or 'after'.

        :raises TransformationError: if the given node is not an instance \
            of :py:class:`psyclone.psyir.nodes.Node`
        :raises TransformationError: if the location is not valid.
        '''

        # Check that the first argument is a Node
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
        :type options: Optional[Dict[str, Any]]
        :param str options["position"]: either 'before' or 'after'.

        :raises TransformationError: if the given node is not an instance \
            of :py:class:`psyclone.psyir.nodes.Node`
        :raises TransformationError: if the location is not valid.

        '''
        # pylint:disable=arguments-differ

        self.validate(node, location, options)

        if not options:
            options = {}
        position = options.get("position", "before")

        parent = node.parent

        my_node = parent.children.pop(node.position)

        location_index = location.position
        if position == "before":
            location.parent.children.insert(location_index, my_node)
        else:
            location.parent.children.insert(location_index+1, my_node)


class Dynamo0p3RedundantComputationTrans(LoopTrans):
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

    def validate(self, node, options=None):
        '''Perform various checks to ensure that it is valid to apply the
        RedundantComputation transformation to the supplied node

        :param node: the supplied node on which we are performing\
                     validity checks
        :type node: :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]
        :param int options["depth"]: the depth of the stencil if the value \
                     is provided and None if not.

        :raises TransformationError: if the parent of the loop is a\
            :py:class:`psyclone.psyir.nodes.Directive`.
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
        # pylint: disable=too-many-branches
        # check node is a loop
        super().validate(node, options=options)

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
                f"In the Dynamo0p3RedundantComputation transformation apply "
                f"method the supplied loop is sits beneath a directive of "
                f"type {type(dir_node)}. Redundant computation must be applied"
                f" before directives are added.")
        if not (isinstance(node.parent, DynInvokeSchedule) or
                isinstance(node.parent.parent, Loop)):
            raise TransformationError(
                f"In the Dynamo0p3RedundantComputation transformation "
                f"apply method the parent of the supplied loop must be the "
                f"DynInvokeSchedule, or a Loop, but found {type(node.parent)}")
        if isinstance(node.parent.parent, Loop):
            if node.loop_type != "colour":
                raise TransformationError(
                    f"In the Dynamo0p3RedundantComputation transformation "
                    f"apply method, if the parent of the supplied Loop is "
                    f"also a Loop then the supplied Loop must iterate over "
                    f"'colour', but found '{node.loop_type}'")
            if node.parent.parent.loop_type != "colours":
                raise TransformationError(
                    f"In the Dynamo0p3RedundantComputation transformation "
                    f"apply method, if the parent of the supplied Loop is "
                    f"also a Loop then the parent must iterate over "
                    f"'colours', but found '{node.parent.parent.loop_type}'")
            if not isinstance(node.parent.parent.parent, DynInvokeSchedule):
                raise TransformationError(
                    f"In the Dynamo0p3RedundantComputation transformation "
                    f"apply method, if the parent of the supplied Loop is "
                    f"also a Loop then the parent's parent must be the "
                    f"DynInvokeSchedule, but found {type(node.parent)}")
        if not Config.get().distributed_memory:
            raise TransformationError(
                "In the Dynamo0p3RedundantComputation transformation apply "
                "method distributed memory must be switched on")

        # loop must iterate over cell-column, dof or colour. Note, an
        # empty loop_type iterates over cell-columns.
        if node.loop_type not in ["", "dof", "colour"]:
            raise TransformationError(
                f"In the Dynamo0p3RedundantComputation transformation apply "
                f"method the loop type must be one of '' (cell-columns), 'dof'"
                f" or 'colour', but found '{node.loop_type}'")

        # We don't currently support the application of transformations to
        # loops containing inter-grid kernels
        check_intergrid(node)
        const = LFRicConstants()

        if not options:
            options = {}
        depth = options.get("depth")
        if depth is None:
            if node.upper_bound_name in const.HALO_ACCESS_LOOP_BOUNDS:
                if not node.upper_bound_halo_depth:
                    raise TransformationError(
                        "In the Dynamo0p3RedundantComputation transformation "
                        "apply method the loop is already set to the maximum "
                        "halo depth so this transformation does nothing")
                for call in node.kernels():
                    for arg in call.arguments.args:
                        if arg.stencil:
                            raise TransformationError(
                                f"In the Dynamo0p3RedundantComputation "
                                f"transformation apply method the loop "
                                f"contains field '{arg.name}' with a stencil "
                                f"access in kernel '{call.name}', so it is "
                                f"invalid to set redundant computation to "
                                f"maximum depth")
        else:
            if not isinstance(depth, int):
                raise TransformationError(
                    f"In the Dynamo0p3RedundantComputation transformation "
                    f"apply method the supplied depth should be an integer but"
                    f" found type '{type(depth)}'")
            if depth < 1:
                raise TransformationError(
                    "In the Dynamo0p3RedundantComputation transformation "
                    "apply method the supplied depth is less than 1")

            if node.upper_bound_name in const.HALO_ACCESS_LOOP_BOUNDS:
                if node.upper_bound_halo_depth:
                    if node.upper_bound_halo_depth >= depth:
                        raise TransformationError(
                            f"In the Dynamo0p3RedundantComputation "
                            f"transformation apply method the supplied depth "
                            f"({depth}) must be greater than the existing halo"
                            f" depth ({node.upper_bound_halo_depth})")
                else:
                    raise TransformationError(
                        "In the Dynamo0p3RedundantComputation transformation "
                        "apply method the loop is already set to the maximum "
                        "halo depth so can't be set to a fixed value")

    def apply(self, loop, options=None):
        # pylint:disable=arguments-renamed
        '''Apply the redundant computation transformation to the loop
        :py:obj:`loop`. This transformation can be applied to loops iterating
        over 'cells or 'dofs'. if :py:obj:`depth` is set to a value then the
        value will be the depth of the field's halo over which redundant
        computation will be performed. If :py:obj:`depth` is not set to a
        value then redundant computation will be performed to the full depth
        of the field's halo.

        :param loop: the loop that we are transforming.
        :type loop: :py:class:`psyclone.psyGen.LFRicLoop`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]
        :param int options["depth"]: the depth of the stencil. Defaults \
                to None.

        '''
        self.validate(loop, options=options)
        if not options:
            options = {}
        depth = options.get("depth")

        if loop.loop_type == "":
            # Loop is over cells
            loop.set_upper_bound("cell_halo", depth)
        elif loop.loop_type == "colour":
            # Loop is over cells of a single colour
            loop.set_upper_bound("colour_halo", depth)
        elif loop.loop_type == "dof":
            loop.set_upper_bound("dof_halo", depth)
        else:
            raise TransformationError(
                f"Unsupported loop_type '{loop.loop_type}' found in "
                f"Dynamo0p3Redundant ComputationTrans.apply()")
        # Add/remove halo exchanges as required due to the redundant
        # computation
        loop.update_halo_exchanges()


class Dynamo0p3AsyncHaloExchangeTrans(Transformation):
    '''Splits a synchronous halo exchange into a halo exchange start and
    halo exchange end. For example:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.psyGen import PSyFactory
    >>> api = "dynamo0.3"
    >>> ast, invokeInfo = parse("file.f90", api=api)
    >>> psy=PSyFactory(api).create(invokeInfo)
    >>> schedule = psy.invokes.get('invoke_0').schedule
    >>> # Uncomment the following line to see a text view of the schedule
    >>> # print(schedule.view())
    >>>
    >>> from psyclone.transformations import Dynamo0p3AsyncHaloExchangeTrans
    >>> trans = Dynamo0p3AsyncHaloExchangeTrans()
    >>> trans.apply(schedule.children[0])
    >>> # Uncomment the following line to see a text view of the schedule
    >>> # print(schedule.view())

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
        :type options: Optional[Dict[str, Any]]

        '''
        self.validate(node, options)

        # add asynchronous start and end halo exchanges and initialise
        # them using information from the existing synchronous halo
        # exchange
        # pylint: disable=protected-access
        node.parent.addchild(
            LFRicHaloExchangeStart(
                node.field, check_dirty=node._check_dirty,
                vector_index=node.vector_index, parent=node.parent),
            index=node.position)
        node.parent.addchild(
            LFRicHaloExchangeEnd(
                node.field, check_dirty=node._check_dirty,
                vector_index=node.vector_index, parent=node.parent),
            index=node.position)

        # remove the existing synchronous halo exchange
        node.detach()

    def validate(self, node, options):
        # pylint: disable=signature-differs
        '''Internal method to check whether the node is valid for this
        transformation.

        :param node: a synchronous Halo Exchange node
        :type node: :py:obj:`psyclone.psygen.HaloExchange`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

        :raises TransformationError: if the node argument is not a
                         HaloExchange (or subclass thereof)

        '''
        if not isinstance(node, psyGen.HaloExchange) or \
           isinstance(node, (LFRicHaloExchangeStart, LFRicHaloExchangeEnd)):
            raise TransformationError(
                f"Error in Dynamo0p3AsyncHaloExchange transformation. Supplied"
                f" node must be a synchronous halo exchange but found "
                f"'{type(node)}'.")


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
    >>> # Uncomment the following line to see a text view of the schedule
    >>> # print(schedule.view())
    >>>
    >>> from psyclone.transformations import Dynamo0p3KernelConstTrans
    >>> trans = Dynamo0p3KernelConstTrans()
    >>> for kernel in schedule.coded_kernels():
    >>>     trans.apply(kernel, number_of_layers=150)
    >>>     kernel_schedule = kernel.get_kernel_schedule()
    >>>     # Uncomment the following line to see a text view of the
    >>>     # symbol table
    >>>     # print(kernel_schedule.symbol_table.view())

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
        # pylint: disable=too-many-statements, too-many-locals
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
        :type node: :py:obj:`psyclone.domain.lfric.LFRicKern`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]
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

        '''
        # --------------------------------------------------------------------
        def make_constant(symbol_table, arg_position, value,
                          function_space=None):
            '''Utility function that modifies the argument at position
            'arg_position' into a compile-time constant with value
            'value'.

            :param symbol_table: the symbol table for the kernel holding
                the argument that is going to be modified.
            :type symbol_table: :py:class:`psyclone.psyir.symbols.SymbolTable`
            :param int arg_position: the argument's position in the
                argument list.
            :param value: the constant value that this argument is going to
                be given. Its type depends on the type of the argument.
            :type value: int, str or bool
            :type str function_space: the name of the function space if there
                is a function space associated with this argument. Defaults
                to None.

            '''
            arg_index = arg_position - 1
            try:
                symbol = symbol_table.argument_list[arg_index]
            except IndexError as err:
                raise TransformationError(
                    f"The argument index '{arg_index}' is greater than the "
                    f"number of arguments "
                    f"'{len(symbol_table.argument_list)}'.") from err
            # Perform some basic checks on the argument to make sure
            # it is the expected type
            if not isinstance(symbol.datatype, ScalarType):
                raise TransformationError(
                    f"Expected entry to be a scalar argument but found "
                    f"'{type(symbol.datatype).__name__}'.")
            if symbol.datatype.intrinsic != ScalarType.Intrinsic.INTEGER:
                raise TransformationError(
                    f"Expected entry to be a scalar integer argument "
                    f"but found '{symbol.datatype}'.")
            if symbol.is_constant:
                raise TransformationError(
                    "Expected entry to be a scalar integer argument "
                    "but found a constant.")

            # Create a new symbol with a known constant value then swap
            # it with the argument. The argument then becomes xxx_dummy
            # and is unused within the kernel body.
            orig_name = symbol.name
            new_name = symbol_table.next_available_name(f"{orig_name}_dummy")
            local_symbol = DataSymbol(new_name, INTEGER_TYPE,
                                      is_constant=True, initial_value=value)
            symbol_table.add(local_symbol)
            symbol_table.swap_symbol_properties(symbol, local_symbol)

            if function_space:
                print(f"    Modified {orig_name}, arg position {arg_position},"
                      f" function space {function_space}, value {value}.")
            else:
                print(f"    Modified {orig_name}, arg position {arg_position},"
                      f" value {value}.")
        # --------------------------------------------------------------------

        self.validate(node, options)

        if not options:
            options = {}
        number_of_layers = options.get("number_of_layers", None)
        quadrature = options.get("quadrature", False)
        element_order = options.get("element_order", None)
        kernel = node

        arg_list_info = KernCallArgList(kernel)
        arg_list_info.generate()
        try:
            kernel_schedule = kernel.get_kernel_schedule()
        except NotImplementedError as excinfo:
            raise TransformationError(
                f"Failed to parse kernel '{kernel.name}'. Error reported was "
                f"'{excinfo}'.") from excinfo

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
                    f"Error in Dynamo0p3KernelConstTrans transformation. "
                    f"Support is currently limited to 'xyoz' quadrature but "
                    f"found {kernel.eval_shapes}.")

        const = LFRicConstants()
        if element_order is not None:
            # Modify the symbol table for degrees of freedom here.
            for info in arg_list_info.ndf_positions:
                if (info.function_space.lower() in
                        (const.VALID_ANY_SPACE_NAMES +
                         const.VALID_ANY_DISCONTINUOUS_SPACE_NAMES +
                         ["any_w2"])):
                    # skip any_space_*, any_discontinuous_space_* and any_w2
                    print(f"    Skipped dofs, arg position {info.position}, "
                          f"function space {info.function_space}")
                else:
                    try:
                        ndofs = Dynamo0p3KernelConstTrans. \
                                space_to_dofs[
                                    info.function_space](element_order)
                    except KeyError as err:
                        raise InternalError(
                            f"Error in Dynamo0p3KernelConstTrans "
                            f"transformation. Unsupported function space "
                            f"'{info.function_space}' found. Expecting one of "
                            f"""{Dynamo0p3KernelConstTrans.
                                 space_to_dofs.keys()}.""") from err
                    make_constant(symbol_table, info.position, ndofs,
                                  function_space=info.function_space)

        # Flag that the kernel has been modified
        kernel.modified = True

    def validate(self, node, options=None):
        '''This method checks whether the input arguments are valid for
        this transformation.

        :param node: a dynamo 0.3 kernel node.
        :type node: :py:obj:`psyclone.domain.lfric.LFRicKern`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]
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
        if not isinstance(node, LFRicKern):
            raise TransformationError(
                f"Error in Dynamo0p3KernelConstTrans transformation. Supplied "
                f"node must be a dynamo kernel but found '{type(node)}'.")

        if not options:
            options = {}
        cellshape = options.get("cellshape", "quadrilateral")
        element_order = options.get("element_order", None)
        number_of_layers = options.get("number_of_layers", None)
        quadrature = options.get("quadrature", False)
        if cellshape.lower() != "quadrilateral":
            # Only quadrilaterals are currently supported
            raise TransformationError(
                f"Error in Dynamo0p3KernelConstTrans transformation. Supplied "
                f"cellshape must be set to 'quadrilateral' but found "
                f"'{cellshape}'.")

        if element_order is not None and \
           (not isinstance(element_order, int) or element_order < 0):
            # element order must be 0 or a positive integer
            raise TransformationError(
                f"Error in Dynamo0p3KernelConstTrans transformation. The "
                f"element_order argument must be >= 0 but found "
                f"'{element_order}'.")

        if number_of_layers is not None and \
           (not isinstance(number_of_layers, int) or number_of_layers < 1):
            # number of layers must be a positive integer
            raise TransformationError(
                f"Error in Dynamo0p3KernelConstTrans transformation. The "
                f"number_of_layers argument must be > 0 but found "
                f"'{number_of_layers}'.")

        if quadrature not in [False, True]:
            # quadrature must be a boolean value
            raise TransformationError(
                f"Error in Dynamo0p3KernelConstTrans transformation. The "
                f"quadrature argument must be boolean but found "
                f"'{quadrature}'.")

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
    >>> ast, invokeInfo = parse(GOCEAN_SOURCE_FILE, api=api)
    >>> psy = PSyFactory(api).create(invokeInfo)
    >>>
    >>> from psyclone.transformations import \
        ACCEnterDataTrans, ACCLoopTrans, ACCParallelTrans
    >>> dtrans = ACCEnterDataTrans()
    >>> ltrans = ACCLoopTrans()
    >>> ptrans = ACCParallelTrans()
    >>>
    >>> schedule = psy.invokes.get('invoke_0').schedule
    >>> # Uncomment the following line to see a text view of the schedule
    >>> # print(schedule.view())
    >>>
    >>> # Apply the OpenACC Loop transformation to *every* loop in the schedule
    >>> for child in schedule.children[:]:
    ...     ltrans.apply(child)
    >>>
    >>> # Enclose all of these loops within a single OpenACC parallel region
    >>> ptrans.apply(schedule)
    >>>
    >>> # Add an enter data directive
    >>> dtrans.apply(schedule)
    >>>
    >>> # Uncomment the following line to see a text view of the schedule
    >>> # print(schedule.view())

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
        # pylint: disable=arguments-renamed
        '''Adds an OpenACC "enter data" directive to the invoke associated
        with the supplied Schedule. Any fields accessed by OpenACC kernels
        within this schedule will be added to this data region in
        order to ensure they remain on the target device.

        :param sched: schedule to which to add an "enter data" directive.
        :type sched: sub-class of :py:class:`psyclone.psyir.nodes.Schedule`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

        '''
        # Ensure that the proposed transformation is valid
        self.validate(sched, options)

        # pylint: disable=import-outside-toplevel
        if isinstance(sched, DynInvokeSchedule):
            from psyclone.dynamo0p3 import DynACCEnterDataDirective as \
                AccEnterDataDir
        elif isinstance(sched, GOInvokeSchedule):
            from psyclone.gocean1p0 import GOACCEnterDataDirective as \
                AccEnterDataDir
        elif isinstance(sched, NemoInvokeSchedule):
            from psyclone.nemo import NemoACCEnterDataDirective as \
                AccEnterDataDir
        else:
            # Should not get here provided that validate() has done its job
            raise InternalError(
                f"ACCEnterDataTrans.validate() has not rejected an "
                f"(unsupported) schedule of type {type(sched)}")

        # Find the position of the first child statement of the current
        # schedule which contains an OpenACC compute construct.
        posn = 0
        directive_cls = (ACCParallelDirective, ACCKernelsDirective)
        directive = sched.walk(directive_cls, stop_type=directive_cls)
        if directive:
            current = directive[0]
            while current not in sched.children:
                current = current.parent
            posn = sched.children.index(current)

        # Add the directive at the position determined above, i.e. just before
        # the first statement containing an OpenACC compute construct.
        data_dir = AccEnterDataDir(parent=sched, children=[])
        sched.addchild(data_dir, index=posn)

    def validate(self, sched, options=None):
        # pylint: disable=arguments-differ, arguments-renamed
        '''
        Check that we can safely apply the OpenACC enter-data transformation
        to the supplied Schedule.

        :param sched: Schedule to which to add an "enter data" directive.
        :type sched: sub-class of :py:class:`psyclone.psyir.nodes.Schedule`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

        :raises TransformationError: if passed something that is not a \
            (subclass of) :py:class:`psyclone.psyir.nodes.Schedule`.

        '''
        super().validate(sched, options)

        if not isinstance(sched, Schedule):
            raise TransformationError("Cannot apply an OpenACC enter data "
                                      "directive to something that is not a "
                                      "Schedule")

        # Check that we don't already have a data region of any sort
        directive_cls = (ACCDataDirective, ACCEnterDataDirective)
        if sched.walk(directive_cls, stop_type=directive_cls):
            raise TransformationError("Schedule already has an OpenACC data "
                                      "region - cannot add an enter data.")


class ACCRoutineTrans(Transformation):
    '''
    Transform a kernel or routine by adding a "!$acc routine" directive
    (causing it to be compiled for the OpenACC accelerator device).
    For example:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.psyGen import PSyFactory
    >>> api = "gocean1.0"
    >>> ast, invokeInfo = parse(GOCEAN_SOURCE_FILE, api=api)
    >>> psy = PSyFactory(api).create(invokeInfo)
    >>>
    >>> from psyclone.transformations import ACCRoutineTrans
    >>> rtrans = ACCRoutineTrans()
    >>>
    >>> schedule = psy.invokes.get('invoke_0').schedule
    >>> # Uncomment the following line to see a text view of the schedule
    >>> # print(schedule.view())
    >>> kern = schedule.children[0].children[0].children[0]
    >>> # Transform the kernel
    >>> rtrans.apply(kern)

    '''
    @property
    def name(self):
        '''
        :returns: the name of this transformation class.
        :rtype: str
        '''
        return "ACCRoutineTrans"

    def apply(self, node, options=None):
        '''
        Add the '!$acc routine' OpenACC directive into the code of the
        supplied Kernel (in a PSyKAl API such as GOcean or LFRic) or directly
        in the supplied Routine.

        :param node: the kernel call or routine implementation to transform.
        :type node: :py:class:`psyclone.psyGen.Kern` or \
                    :py:class:`psyclone.psyir.nodes.Routine`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

        '''
        # Check that we can safely apply this transformation
        self.validate(node, options)

        if isinstance(node, Kern):
            # Flag that the kernel has been modified
            node.modified = True

            # Get the schedule representing the kernel subroutine
            routine = node.get_kernel_schedule()
        else:
            routine = node

        # Insert the directive to the routine if it doesn't already exist
        for child in routine.children:
            if isinstance(child, ACCRoutineDirective):
                return  # The routine is already marked with ACCRoutine
        routine.children.insert(0, ACCRoutineDirective())

    def validate(self, node, options=None):
        '''
        Perform checks that the supplied kernel or routine can be transformed.

        :param node: the kernel which is the target of the transformation.
        :type node: :py:class:`psyclone.psyGen.Kern` |
                    :py:class:`psyclone.psyir.nodes.Routine`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

        :raises TransformationError: if the node is not a kernel or a routine.
        :raises TransformationError: if the target is a built-in kernel.
        :raises TransformationError: if it is a kernel but without an
                                     associated PSyIR.
        :raises TransformationError: if any of the symbols in the kernel are
                                     accessed via a module use statement.
        :raises TransformationError: if the kernel contains any calls to other
                                     routines.
        '''
        super().validate(node, options)

        force = options.get("force", False) if options else False

        if not isinstance(node, Kern) and not isinstance(node, Routine):
            raise TransformationError(
                f"The ACCRoutineTrans must be applied to a sub-class of "
                f"Kern or Routine but got '{type(node).__name__}'.")

        # If it is a kernel call it must have an accessible implementation
        if isinstance(node, BuiltIn):
            raise TransformationError(
                f"Applying ACCRoutineTrans to a built-in kernel is not yet"
                f" supported and kernel '{node.name}' is of type "
                f"'{type(node).__name__}'")

        if isinstance(node, Kern):
            # Get the PSyIR routine from the associated kernel. If there is an
            # exception (this could mean that there is no associated tree
            # or that the frontend failed to convert it into PSyIR) reraise it
            # as a TransformationError
            try:
                kernel_schedule = node.get_kernel_schedule()
            except Exception as error:
                raise TransformationError(
                    f"Failed to create PSyIR for kernel '{node.name}'. "
                    f"Cannot transform such a kernel.") from error
            k_or_r = "Kernel"
        else:
            # Supplied node is a PSyIR Routine which *is* a Schedule.
            kernel_schedule = node
            k_or_r = "routine"

        # Check that the routine does not access any data that is imported via
        # a 'use' statement.
        # TODO #2271 - this implementation will not catch symbols from literal
        # precisions or intialisation expressions.
        refs = kernel_schedule.walk(Reference)
        for ref in refs:
            if ref.symbol.is_import:
                # resolve_type does nothing if the Symbol type is known.
                try:
                    ref.symbol.resolve_type()
                except SymbolError:
                    # TODO #11 - log that we failed to resolve this Symbol.
                    pass
                if (isinstance(ref.symbol, DataSymbol) and
                        ref.symbol.is_constant):
                    # An import of a compile-time constant is fine.
                    continue
                raise TransformationError(
                    f"{k_or_r} '{node.name}' accesses the symbol "
                    f"'{ref.symbol}' which is imported. If this symbol "
                    f"represents data then it must first be converted to a "
                    f"{k_or_r} argument using the KernelImportsToArguments "
                    f"transformation.")

        # We forbid CodeBlocks because we can't be certain that what they
        # contain can be executed on a GPU. However, we do permit the user
        # to override this check.
        cblocks = kernel_schedule.walk(CodeBlock)
        if not force:
            if cblocks:
                cblock_txt = ("\n  " + "\n  ".join(str(node) for node in
                                                   cblocks[0].get_ast_nodes)
                              + "\n")
                option_txt = "options={'force': True}"
                raise TransformationError(
                    f"Cannot safely add 'ACC routine' to {k_or_r} "
                    f"'{node.name}' because its PSyIR contains one or more "
                    f"CodeBlocks:{cblock_txt}You may use '{option_txt}' to "
                    f"override this check.")
        else:
            # Check any accesses within CodeBlocks.
            # TODO #2271 - this will be handled as part of the checking to be
            # implemented using the dependence analysis.
            for cblock in cblocks:
                names = cblock.get_symbol_names()
                for name in names:
                    sym = kernel_schedule.symbol_table.lookup(name)
                    if sym.is_import:
                        raise TransformationError(
                            f"{k_or_r} '{node.name}' accesses the symbol "
                            f"'{sym.name}' within a CodeBlock and this symbol "
                            f"is imported. 'ACC routine' cannot be added to "
                            f"such a {k_or_r}.")

        calls = kernel_schedule.walk(Call)
        for call in calls:
            if not isinstance(call, IntrinsicCall):
                call_str = call.debug_string().rstrip("\n")
                raise TransformationError(
                    f"{k_or_r} '{node.name}' calls another routine "
                    f"'{call_str}' and therefore cannot have "
                    f"'ACC routine' added to it (TODO #342).")


class ACCKernelsTrans(RegionTrans):
    '''
    Enclose a sub-set of nodes from a Schedule within an OpenACC kernels
    region (i.e. within "!$acc kernels" ... "!$acc end kernels" directives).

    For example:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.psyGen import PSyFactory
    >>> api = "nemo"
    >>> ast, invokeInfo = parse(NEMO_SOURCE_FILE, api=api)
    >>> psy = PSyFactory(api).create(invokeInfo)
    >>>
    >>> from psyclone.transformations import ACCKernelsTrans
    >>> ktrans = ACCKernelsTrans()
    >>>
    >>> schedule = psy.invokes.get('tra_adv').schedule
    >>> # Uncomment the following line to see a text view of the schedule
    >>> # print(schedule.view())
    >>> kernels = schedule.children[9]
    >>> # Transform the kernel
    >>> ktrans.apply(kernels)

    '''
    excluded_node_types = (CodeBlock, Return, PSyDataNode,
                           psyGen.HaloExchange)

    @property
    def name(self):
        '''
        :returns: the name of this transformation class.
        :rtype: str
        '''
        return "ACCKernelsTrans"

    def apply(self, node, options=None):
        '''
        Enclose the supplied list of PSyIR nodes within an OpenACC
        Kernels region.

        :param node: a node or list of nodes in the PSyIR to enclose.
        :type node: (a list of) :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]
        :param bool options["default_present"]: whether or not the kernels \
            region should have the 'default present' attribute (indicating \
            that data is already on the accelerator). When using managed \
            memory this option should be False.

        '''
        # Ensure we are always working with a list of nodes, even if only
        # one was supplied via the `node` argument.
        node_list = self.get_node_list(node)

        self.validate(node_list, options)

        parent = node_list[0].parent
        start_index = node_list[0].position

        if not options:
            options = {}
        default_present = options.get("default_present", False)

        # Create a directive containing the nodes in node_list and insert it.
        directive = ACCKernelsDirective(
            parent=parent, children=[node.detach() for node in node_list],
            default_present=default_present)

        parent.children.insert(start_index, directive)

    def validate(self, nodes, options):
        # pylint: disable=signature-differs
        '''
        Check that we can safely enclose the supplied node or list of nodes
        within OpenACC kernels ... end kernels directives.

        :param nodes: the proposed PSyIR node or nodes to enclose in the \
                      kernels region.
        :type nodes: (list of) :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

        :raises NotImplementedError: if the supplied Nodes belong to \
                                     a GOInvokeSchedule.
        :raises TransformationError: if there are no Loops within the \
                                     proposed region.

        '''
        # Ensure we are always working with a list of nodes, even if only
        # one was supplied via the `nodes` argument.
        node_list = self.get_node_list(nodes)

        # Check that the front-end is valid
        sched = node_list[0].ancestor((NemoInvokeSchedule, DynInvokeSchedule))
        if not sched:
            raise NotImplementedError(
                "OpenACC kernels regions are currently only supported for the "
                "nemo and dynamo0.3 front-ends")
        super().validate(node_list, options)

        # Check that we have at least one loop or array range within
        # the proposed region
        for node in node_list:
            if (any(assign for assign in node.walk(Assignment)
                    if assign.is_array_assignment) or node.walk(Loop)):
                break
        else:
            # Branch executed if loop does not exit with a break
            raise TransformationError(
                "A kernels transformation must enclose at least one loop or "
                "array range but none were found.")


class ACCDataTrans(RegionTrans):
    '''
    Add an OpenACC data region around a list of nodes in the PSyIR.
    COPYIN, COPYOUT and COPY clauses are added as required.

    For example:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.psyGen import PSyFactory
    >>> api = "nemo"
    >>> ast, invokeInfo = parse(NEMO_SOURCE_FILE, api=api)
    >>> psy = PSyFactory(api).create(invokeInfo)
    >>>
    >>> from psyclone.transformations import ACCKernelsTrans, ACCDataTrans
    >>> ktrans = ACCKernelsTrans()
    >>> dtrans = ACCDataTrans()
    >>>
    >>> schedule = psy.invokes.get('tra_adv').schedule
    >>> # Uncomment the following line to see a text view of the schedule
    >>> # print(schedule.view())
    >>>
    >>> # Add a kernels construct for execution on the device
    >>> kernels = schedule.children[9]
    >>> ktrans.apply(kernels)
    >>>
    >>> # Enclose the kernels in a data construct
    >>> kernels = schedule.children[9]
    >>> dtrans.apply(kernels)

    '''
    excluded_node_types = (CodeBlock, Return, PSyDataNode)

    @property
    def name(self):
        '''
        :returns: the name of this transformation.
        :rtype: str

        '''
        return "ACCDataTrans"

    def apply(self, node, options=None):
        '''
        Put the supplied node or list of nodes within an OpenACC data region.

        :param node: the PSyIR node(s) to enclose in the data region.
        :type node: (list of) :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

        '''
        # Ensure we are always working with a list of nodes, even if only
        # one was supplied via the `node` argument.
        node_list = self.get_node_list(node)

        self.validate(node_list, options)

        parent = node_list[0].parent
        start_index = node_list[0].position

        # Create a directive containing the nodes in node_list and insert it.
        directive = ACCDataDirective(
                parent=parent, children=[node.detach() for node in node_list])

        parent.children.insert(start_index, directive)

    def validate(self, nodes, options):
        # pylint: disable=signature-differs
        '''
        Check that we can safely add a data region around the supplied list
        of nodes.

        :param nodes: the proposed node(s) to enclose in a data region.
        :type nodes: List[:py:class:`psyclone.psyir.nodes.Node`] |
            :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

        :raises TransformationError: if the Schedule to which the nodes
            belong already has an 'enter data' directive.
        :raises TransformationError: if any of the nodes are themselves
            data directives.
        :raises TransformationError: if an array of structures needs to be
            deep copied (this is not currently supported).

        '''
        # Ensure we are always working with a list of nodes, even if only
        # one was supplied via the `nodes` argument.
        node_list = self.get_node_list(nodes)

        super().validate(node_list, options)

        # Check that the Schedule to which the nodes belong does not already
        # have an 'enter data' directive.
        schedule = node_list[0].root
        acc_dirs = schedule.walk(ACCEnterDataDirective)
        if acc_dirs:
            raise TransformationError(
                "Cannot add an OpenACC data region to a schedule that "
                "already contains an 'enter data' directive.")
        # Check that we don't have any accesses to arrays of derived types
        # that we can't yet deep copy.
        for node in node_list:
            for sref in node.walk(StructureReference):

                # Find the loop variables for all Loops that contain this
                # access and are themselves within the data region.
                loop_vars = []
                cursor = sref.ancestor(Loop, limit=node)
                while cursor:
                    loop_vars.append(Signature(cursor.variable.name))
                    cursor = cursor.ancestor(Loop)

                # Now check whether any of these loop variables appear within
                # the structure reference.
                # Loop over each component of the structure reference that is
                # an array access.
                array_accesses = sref.walk(ArrayMixin)
                for access in array_accesses:
                    if not isinstance(access, StructureMember):
                        continue
                    var_accesses = VariablesAccessInfo(access.indices)
                    for var in loop_vars:
                        if var not in var_accesses.all_signatures:
                            continue
                        # For an access such as my_struct(ii)%my_array(ji)
                        # then if we're inside a loop over it we would actually
                        # need a loop to do the deep copy:
                        #   do ii = 1, N
                        #   !$acc data copyin(my_struct(ii)%my_array)
                        #   end do
                        raise TransformationError(
                            f"Data region contains a structure access "
                            f"'{sref.debug_string()}' where component "
                            f"'{access.name}' is an array and is iterated over"
                            f" (variable '{var}'). Deep copying of data for "
                            f"structures is only supported where the deepest "
                            f"component is the one being iterated over.")


class KernelImportsToArguments(Transformation):
    '''
    Transformation that removes any accesses of imported data from the supplied
    kernel and places them in the caller. The values/references are then passed
    by argument into the kernel.
    '''
    @property
    def name(self):
        '''
        :returns: the name of this transformation.
        :rtype: str
        '''
        return "KernelImportsToArguments"

    def __str__(self):
        return ("Convert the imported variables used inside the kernel "
                "into arguments and modify the InvokeSchedule to pass them"
                " in the kernel call.")

    def validate(self, node, options=None):
        '''
        Check that the supplied node is a valid target for this transformation.

        :param node: the PSyIR node to validate.
        :type node: :py:class:`psyclone.psyGen.CodedKern`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

        :raises TransformationError: if the supplied node is not a CodedKern.
        :raises TransformationError: if this transformation is not applied to \
            a Gocean API Invoke.
        :raises TransformationError: if the supplied kernel contains wildcard \
            imports of symbols from one or more containers (e.g. a USE without\
            an ONLY clause in Fortran).
        '''
        if not isinstance(node, CodedKern):
            raise TransformationError(
                f"The {self.name} transformation can only be applied to "
                f"CodedKern nodes but found '{type(node).__name__}' instead.")

        invoke_schedule = node.ancestor(InvokeSchedule)
        if not isinstance(invoke_schedule, GOInvokeSchedule):
            raise TransformationError(
                f"The {self.name} transformation is currently only supported "
                f"for the GOcean API but got an InvokeSchedule of type: "
                f"'{type(invoke_schedule).__name__}'")

        # Check that there are no unqualified imports or undeclared symbols
        try:
            kernel = node.get_kernel_schedule()
        except SymbolError as err:
            raise TransformationError(
                f"Kernel '{node.name}' contains undeclared symbol: "
                f"{err.value}") from err

        symtab = kernel.symbol_table
        for container in symtab.containersymbols:
            if container.wildcard_import:
                raise TransformationError(
                    f"Kernel '{node.name}' has a wildcard import of symbols "
                    f"from container '{container.name}'. This is not "
                    f"supported.")

        # TODO #649. Check for variables accessed by the kernel but declared
        # in an outer scope.

    def apply(self, node, options=None):
        '''
        Convert the imported variables used inside the kernel into arguments
        and modify the InvokeSchedule to pass the same imported variables to
        the kernel call.

        :param node: a kernel call.
        :type node: :py:class:`psyclone.psyGen.CodedKern`
        :param options: a dictionary with options for transformations.
        :type options: Optional[Dict[str, Any]]

        '''

        self.validate(node, options)

        kernel = node.get_kernel_schedule()
        symtab = kernel.symbol_table
        invoke_symtab = node.ancestor(InvokeSchedule).symbol_table
        count_imported_vars_removed = 0

        # Transform each imported variable into an argument.
        # TODO #11: When support for logging is added, we could warn the user
        # if no imports are found in the kernel.
        for imported_var in kernel.symbol_table.imported_symbols[:]:
            count_imported_vars_removed += 1

            # Resolve the data type information if it is not available
            # pylint: disable=unidiomatic-typecheck
            if (type(imported_var) is Symbol or
                    isinstance(imported_var.datatype, UnresolvedType)):
                updated_sym = imported_var.resolve_type()
                # If we have a new symbol then we must update the symbol table
                if updated_sym is not imported_var:
                    kernel.symbol_table.swap(imported_var, updated_sym)
            # pylint: enable=unidiomatic-typecheck

            # Copy the imported symbol into the InvokeSchedule SymbolTable
            invoke_symtab.copy_external_import(
                updated_sym, tag="AlgArgs_" + updated_sym.name)

            # Keep a reference to the original container so that we can
            # update it after the interface has been updated.
            container = updated_sym.interface.container_symbol

            # Convert the symbol to an argument and add it to the argument list
            current_arg_list = symtab.argument_list
            # An argument does not have an initial value.
            was_constant = updated_sym.is_constant
            updated_sym.is_constant = False
            updated_sym.initial_value = None
            if was_constant:
                # Imported constants lose the constant value but are read-only
                # TODO: When #633 and #11 are implemented, warn the user that
                # they should transform the constants to literal values first.
                updated_sym.interface = ArgumentInterface(
                    ArgumentInterface.Access.READ)
            else:
                updated_sym.interface = ArgumentInterface(
                    ArgumentInterface.Access.READWRITE)
            current_arg_list.append(updated_sym)
            symtab.specify_argument_list(current_arg_list)

            # Convert PSyIR DataTypes to Gocean VALID_SCALAR_TYPES
            # TODO #678: Ideally this strings should be provided by the GOcean
            # API configuration.
            go_space = ""
            if updated_sym.datatype.intrinsic == ScalarType.Intrinsic.REAL:
                go_space = "go_r_scalar"
            elif (updated_sym.datatype.intrinsic ==
                  ScalarType.Intrinsic.INTEGER):
                go_space = "go_i_scalar"
            else:
                raise TypeError(
                    f"The imported variable '{updated_sym.name}' could not be "
                    f"promoted to an argument because the GOcean "
                    f"infrastructure does not have any scalar type equivalent "
                    f"to the PSyIR {updated_sym.datatype} type.")

            # Add the imported variable in the call argument list
            node.arguments.append(updated_sym.name, go_space)

            # Check whether we still need the Container symbol from which
            # this import was originally accessed
            if not kernel.symbol_table.symbols_imported_from(container) and \
               not container.wildcard_import:
                kernel.symbol_table.remove(container)

        if count_imported_vars_removed > 0:
            node.modified = True


# For Sphinx AutoAPI documentation generation
__all__ = [
   "ACCEnterDataTrans",
   "ACCDataTrans",
   "ACCKernelsTrans",
   "ACCLoopTrans",
   "ACCParallelTrans",
   "ACCRoutineTrans",
   "ColourTrans",
   "Dynamo0p3AsyncHaloExchangeTrans",
   "Dynamo0p3ColourTrans",
   "Dynamo0p3KernelConstTrans",
   "Dynamo0p3OMPLoopTrans",
   "Dynamo0p3RedundantComputationTrans",
   "DynamoOMPParallelLoopTrans",
   "GOceanOMPLoopTrans",
   "GOceanOMPParallelLoopTrans",
   "KernelImportsToArguments",
   "MoveTrans",
   "OMPLoopTrans",
   "OMPMasterTrans",
   "OMPParallelLoopTrans",
   "OMPParallelTrans",
   "OMPSingleTrans",
   "ParallelRegionTrans",
]
