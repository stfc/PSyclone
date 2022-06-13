# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2022, Science and Technology Facilities Council.
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
# Modified I. Kavcic, Met Office

''' This module provides the various transformations that can be applied to
    PSyIR nodes. There are both general and API-specific transformation
    classes in this module where the latter typically apply API-specific
    checks before calling the base class for the actual transformation. '''

# pylint: disable=too-many-lines

from __future__ import absolute_import, print_function

import abc


from psyclone import psyGen
from psyclone.configuration import Config
from psyclone.domain.lfric import KernCallArgList, LFRicConstants
from psyclone.dynamo0p3 import DynHaloExchangeEnd, DynHaloExchangeStart, \
    DynInvokeSchedule, DynKern
from psyclone.errors import InternalError, GenerationError
from psyclone.gocean1p0 import GOInvokeSchedule
from psyclone.nemo import NemoInvokeSchedule
from psyclone.psyGen import Transformation, CodedKern, Kern, InvokeSchedule, \
    BuiltIn
from psyclone.psyir.nodes import ACCDataDirective, ACCDirective, \
    ACCEnterDataDirective, ACCKernelsDirective, ACCLoopDirective, \
    ACCParallelDirective, ACCRoutineDirective, Assignment, CodeBlock, \
    Directive, KernelSchedule, Loop, Node, OMPDeclareTargetDirective, \
    OMPDirective, OMPDoDirective, OMPLoopDirective, OMPMasterDirective, \
    OMPParallelDirective, OMPParallelDoDirective, OMPSerialDirective, \
    OMPSingleDirective, OMPTargetDirective, OMPTaskloopDirective, \
    PSyDataNode, Reference, Return, Routine, Schedule
from psyclone.psyir.symbols import ArgumentInterface, DataSymbol, \
    DeferredType, INTEGER_TYPE, ScalarType, Symbol, SymbolError
from psyclone.psyir.tools import DTCode, DependencyTools
from psyclone.psyir.transformations import RegionTrans, LoopTrans, \
    TransformationError


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
    child_kernels = node.walk(DynKern)
    for kern in child_kernels:
        if kern.is_intergrid:
            raise TransformationError(
                f"This Transformation cannot currently be applied to nodes "
                f"which have inter-grid kernels as descendents and {kern.name}"
                f" is such a kernel.")


class KernelTrans(Transformation):
    # pylint: disable=abstract-method
    '''
    Base class for all Kernel transformations.

    '''
    @staticmethod
    def validate(kern, options=None):
        # pylint: disable=arguments-renamed
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

        if not isinstance(kern, Kern):
            raise TransformationError(
                f"Target of a kernel transformation must be a sub-class of "
                f"psyGen.Kern but got '{type(kern).__name__}'")

        # Check that the PSyIR and associated Symbol table of the Kernel is OK.
        # If this kernel contains symbols that are not captured in the PSyIR
        # SymbolTable then this raises an exception.
        try:
            kernel_schedule = kern.get_kernel_schedule()
        except GenerationError as error:
            raise TransformationError(
                f"Failed to create PSyIR for kernel '{kern.name}'. "
                f"Cannot transform such a kernel.") from error
        except SymbolError as err:
            raise TransformationError(
                f"Kernel '{kern.name}' contains accesses to data that are not "
                f"present in the Symbol Table(s). Cannot "
                f"transform such a kernel.") from err
        # Check that all kernel symbols are declared in the kernel
        # symbol table(s). At this point they may be declared in a
        # container containing this kernel which is not supported.
        for var in kernel_schedule.walk(Reference):
            try:
                var.scope.symbol_table.lookup(
                    var.name, scope_limit=var.ancestor(KernelSchedule))
            except KeyError as err:
                raise TransformationError(
                    f"Kernel '{kern.name}' contains accesses to data (variable"
                    f" '{var.name}') that are not present in the Symbol Table"
                    f"(s) within KernelSchedule scope. Cannot transform such a"
                    f" kernel.") from err


class ParallelLoopTrans(LoopTrans, metaclass=abc.ABCMeta):
    '''
    Adds an abstract directive (it needs to be specified by sub-classing this
    transformation) to a loop indicating that it should be parallelised. It
    performs some data dependency checks to guarantee that the loop can be
    parallelised without changing the semantics of it.

    '''
    # The types of node that must be excluded from the section of PSyIR
    # being transformed.
    excluded_node_types = (Return, psyGen.HaloExchange, CodeBlock)

    @abc.abstractmethod
    def __str__(self):
        # pylint: disable=invalid-str-returned
        return  # pragma: no cover

    @abc.abstractmethod
    def _directive(self, children, collapse=None):
        '''
        Returns the directive object to insert into the Schedule.
        Must be implemented by sub-class.

        :param children: list of nodes that will be children of this Directive.
        :type children: list of :py:class:`psyclone.psyir.nodes.Node`
        :param int collapse: the number of tightly-nested loops to which \
                             this directive applies or None.

        :returns: the new Directive node.
        :rtype: sub-class of :py:class:`psyclone.psyir.nodes.Directive`.
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

        :raises TransformationError: if the \
                :py:class:`psyclone.psyir.nodes.Loop` loop iterates over \
                colours.
        :raises TransformationError: if 'collapse' is supplied with an \
                invalid number of loops.
        :raises TransformationError: if there is a data dependency that \
                prevents the parallelisation of the loop.

        '''
        # Check that the supplied node is a Loop and does not contain any
        # unsupported nodes.
        super().validate(node, options=options)

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
                    f"The 'collapse' argument must be an integer but got an "
                    f"object of type {type(collapse)}")
            if collapse < 2:
                raise TransformationError(
                    f"It only makes sense to collapse 2 or more loops "
                    f"but got a value of {collapse}")
            # Count the number of loops in the loop nest
            loop_count = 0
            cnode = node
            while isinstance(cnode, Loop):
                loop_count += 1
                # Loops must be tightly nested (no intervening statements)
                cnode = cnode.loop_body[0]
            if collapse > loop_count:
                raise TransformationError(
                    f"Cannot apply COLLAPSE({collapse}) clause to a loop nest "
                    f"containing only {loop_count} loops")

        # Check that there are no loop-carried dependencies
        dep_tools = DependencyTools()

        try:
            if not dep_tools.can_loop_be_parallelised(node,
                                                      only_nested_loops=False):

                # The DependencyTools also returns False for things that are
                # not an issue, so we ignore specific messages.
                for message in dep_tools.get_all_messages():
                    if message.code == DTCode.WARN_SCALAR_WRITTEN_ONCE:
                        continue
                    all_msg_str = [str(message) for message in
                                   dep_tools.get_all_messages()]
                    messages = "\n".join(all_msg_str)
                    raise TransformationError(
                        f"Dependency analysis failed with the following "
                        f"messages:\n{messages}")
        except (KeyError, InternalError):
            # LFRic still has symbols that don't exist in the symbol_table
            # until the gen_code() step, so the dependency analysis raises
            # KeyErrors in some cases. We ignore this for now.
            pass

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

        '''
        if not options:
            options = {}
        self.validate(node, options=options)

        collapse = options.get("collapse", None)

        # keep a reference to the node's original parent and its index as these
        # are required and will change when we change the node's location
        node_parent = node.parent
        node_position = node.position

        # Add our orphan loop directive setting its parent to the node's
        # parent and its children to the node. This calls down to the sub-class
        # to get the type of directive we require.
        directive = self._directive([node.detach()], collapse)

        # Add the loop directive as a child of the node's parent
        node_parent.addchild(directive, index=node_position)


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
    >>> ast, invokeInfo = parse(SOURCE_FILE, api=api)
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
        :type options: dict of str:values or None
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


class OMPTargetTrans(RegionTrans):
    '''
    Adds an OpenMP target directive to a region of code.

    For example:

    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.nodes import Loop
    >>> from psyclone.transformations import OMPTargetTrans
    >>>
    >>> tree = FortranReader().psyir_from_source("""
    ...     subroutine my_subroutine()
    ...         integer, dimension(10, 10) :: A
    ...         integer :: i
    ...         integer :: j
    ...         do i = 1, 10
    ...             do j = 1, 10
    ...                 A(i, j) = 0
    ...             end do
    ...         end do
    ...     end subroutine
    ...     """
    >>> omptargettrans = OMPTargetTrans()
    >>> omptargettrans.apply(tree.walk(Loop))

    will generate:

    .. code-block:: fortran

        subroutine my_subroutine()
            integer, dimension(10, 10) :: A
            integer :: i
            integer :: j
            !$omp target
            do i = 1, 10
                do j = 1, 10
                    A(i, j) = 0
                end do
            end do
            !$omp end target
        end subroutine

    '''
    def apply(self, node, options=None):
        ''' Insert an OMPTargetDirective before the provided node or list
        of nodes.

        :param node: the PSyIR node or nodes to enclose in the OpenMP \
                      target region.
        :type node: (list of) :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: dict of str:values or None

        '''
        # Check whether we've been passed a list of nodes or just a
        # single node. If the latter then we create ourselves a
        # list containing just that node.
        node_list = self.get_node_list(node)
        self.validate(node_list, options)

        # Create a directive containing the nodes in node_list and insert it.
        parent = node_list[0].parent
        start_index = node_list[0].position
        directive = OMPTargetDirective(
            parent=parent, children=[node.detach() for node in node_list])

        parent.children.insert(start_index, directive)


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
        :type options: dict of str:values or None

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
        :type options: dict of str:values or None

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


class OMPLoopTrans(ParallelLoopTrans):
    '''
    Adds an OpenMP directive to a loop. This can be the loop worksharing
    OpenMP Do/For directive to distribute the iterations of the enclosed
    loop or a descriptive OpenMP loop directive to let the compiler decide
    the best implementation. The OpenMP schedule used for the worksharing
    directive can also be specified, but this will be ignored in case of the
    descriptive OpenMP loop. The configuration-defined 'reprod' parameter
    also specifies whether a manual reproducible reproduction is to be used.

    :param str omp_schedule: the OpenMP schedule to use. Defaults to 'static'.
    :param bool omp_worksharing: whether to generate OpenMP loop worksharing \
        directives (e.g. omp do/for) or an OpenMP loop directive. Defaults to \
        True.

    For example:

    >>> from psyclone.psyir.frontend.fortran import FortranReader
    >>> from psyclone.psyir.nodes import Routine
    >>> from psyclone.transformations import OMPLoopTrans, OMPParallelTrans
    >>>
    >>> tree = FortranReader().psyir_from_source("""
    ...     subroutine my_subroutine()
    ...         integer, dimension(10, 10) :: A
    ...         integer :: i
    ...         integer :: j
    ...         do i = 1, 10
    ...             do j = 1, 10
    ...                 A(i, j) = 0
    ...             end do
    ...         end do
    ...         do i = 1, 10
    ...             do j = 1, 10
    ...                 A(i, j) = 0
    ...             end do
    ...         end do
    ...     end subroutine
    ...     """
    >>> routine.walk(Routine)
    >>> ompparalleltrans = OMPParallelTrans()  # Necessary in loop worksharing
    >>> omplooptrans1 = OMPLoopTrans(omp_schedule="auto")
    >>> omplooptrans2 = OMPLoopTrans(omp_worksharing=False)
    >>> omplooptrans1.apply(routine.children[0])
    >>> ompparalleltrans.apply(routine.children[0])
    >>> omplooptrans2.apply(routine.children[1])

    will generate:

    .. code-block:: fortran

        subroutine my_subroutine()
            integer, dimension(10, 10) :: A
            integer :: i
            integer :: j
            !$omp parallel
            !$omp do schedule(auto)
            do i = 1, 10
                do j = 1, 10
                    A(i, j) = 0
                end do
            end do
            !$omp end do
            !$omp end parallel
            !$omp loop
            do i = 1, 10
                do j = 1, 10
                    A(i, j) = 0
                end do
            end do
            !$omp end loop
        end subroutine

    '''
    def __init__(self, omp_schedule="static", omp_worksharing=True):
        # Whether or not to generate code for (run-to-run on n threads)
        # reproducible OpenMP reductions. This setting can be overridden
        # via the `reprod` argument to the apply() method.
        self._reprod = Config.get().reproducible_reductions

        # Declare the attributes but use the property setter for proper
        # error checking
        self._omp_worksharing = None
        self.omp_worksharing = omp_worksharing

        self._omp_schedule = ""
        self.omp_schedule = omp_schedule

        super().__init__()

    def __str__(self):
        return "Adds an 'OpenMP DO' directive to a loop"

    @property
    def omp_worksharing(self):
        '''
        :returns: the value of the omp_worksharing attribute.
        :rtype: bool
        '''
        return self._omp_worksharing

    @omp_worksharing.setter
    def omp_worksharing(self, value):
        '''
        :param bool value: new value of the omp_worksharing attribute.

        :raises TypeError: if the provided value is not a boolean.
        '''
        if not isinstance(value, bool):
            raise TypeError(
                f"The OMPLoopTrans.omp_worksharing property must be a boolean"
                f" but found a '{type(value).__name__}'.")
        self._omp_worksharing = value

    @property
    def omp_schedule(self):
        '''
        :returns: the OpenMP schedule that will be specified by \
            this transformation. The default schedule is 'static'.
        :rtype: str

        '''
        return self._omp_schedule

    @omp_schedule.setter
    def omp_schedule(self, value):
        '''
        :param str value: Sets the OpenMP schedule value that will be \
            specified by this transformation.

        :raises TypeError: if the provided value is not a string.
        :raises ValueError: if the provided string is not a valid OpenMP \
            schedule format.
        '''

        if not isinstance(value, str):
            raise TypeError(
                f"The OMPLoopTrans.omp_schedule property must be a 'str'"
                f" but found a '{type(value).__name__}'.")

        # Some schedules have an optional chunk size following a ','
        value_parts = value.split(',')
        if value_parts[0].lower() not in VALID_OMP_SCHEDULES:
            raise ValueError(f"Valid OpenMP schedules are "
                             f"{VALID_OMP_SCHEDULES} but got "
                             f"'{value_parts[0]}'.")

        if len(value_parts) > 1:
            if value_parts[0] == "auto":
                raise ValueError("Cannot specify a chunk size when using an "
                                 "OpenMP schedule of 'auto'.")
            try:
                int(value_parts[1].strip())
            except ValueError as err:
                raise ValueError(f"Supplied OpenMP schedule '{value}' has an "
                                 f"invalid chunk-size.") from err

        self._omp_schedule = value

    def _directive(self, children, collapse=None):
        '''
        Creates the type of directive needed for this sub-class of
        transformation.

        :param children: list of Nodes that will be the children of \
                         the created directive.
        :type children: list of :py:class:`psyclone.psyir.nodes.Node`
        :param int collapse: number of nested loops to collapse or None if \
                             no collapse attribute is required.

        :returns: the new node representing the directive in the AST
        :rtype: :py:class:`psyclone.psyir.nodes.OMPDoDirective` or \
                :py:class:`psyclone.psyir.nodes.OMPLoopDirective`

        '''
        if self._omp_worksharing:
            # TODO 1370: OpenMP Do Directive don't support collapse yet.
            _directive = OMPDoDirective(children=children,
                                        omp_schedule=self.omp_schedule,
                                        reprod=self._reprod)
        else:
            _directive = OMPLoopDirective(children=children,
                                          collapse=collapse)

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

        '''
        if not options:
            options = {}
        self._reprod = options.get("reprod",
                                   Config.get().reproducible_reductions)

        # Add variable names for OMP functions into the InvokeSchedule
        # (a Routine) symboltable if they don't already exist
        root = node.ancestor(Routine)

        symtab = root.symbol_table
        try:
            symtab.lookup_with_tag("omp_thread_index")
        except KeyError:
            symtab.new_symbol(
                "th_idx", tag="omp_thread_index",
                symbol_type=DataSymbol, datatype=INTEGER_TYPE)
        try:
            symtab.lookup_with_tag("omp_num_threads")
        except KeyError:
            symtab.new_symbol(
                "nthreads", tag="omp_num_threads",
                symbol_type=DataSymbol, datatype=INTEGER_TYPE)

        super().apply(node, options)


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
    >>> ast, invokeInfo = parse(SOURCE_FILE, api=api)
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
    >>> new_schedule = schedule
    >>>
    # Apply the OpenACC Loop transformation to *every* loop
    # in the schedule
    >>> for child in schedule.children:
    >>>     ltrans.apply(child, reprod=True)
    >>>     schedule = newschedule
    >>>
    # Enclose all of these loops within a single OpenACC
    # PARALLEL region
    >>> rtrans.omp_schedule("dynamic,1")
    >>> rtrans.apply(schedule.children)
    >>>

    '''
    # The types of node that must be excluded from the section of PSyIR
    # being transformed.
    excluded_node_types = (PSyDataNode)

    def __init__(self):
        # Whether to add the "independent" clause
        # to the loop directive.
        self._independent = True
        self._sequential = False
        super().__init__()

    def __str__(self):
        return "Adds an 'OpenACC loop' directive to a loop"

    def _directive(self, children, collapse=None):
        '''
        Creates the ACCLoopDirective needed by this sub-class of
        transformation.

        :param children: list of child nodes of the new directive Node.
        :type children: list of :py:class:`psyclone.psyir.nodes.Node`
        :param int collapse: number of nested loops to collapse or None if \
                             no collapse attribute is required.
        '''
        directive = ACCLoopDirective(children=children,
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
        :py:meth:`psyclone.psyir.nodes.ACCLoopDirective.gen_code` is called),
        this node must be within (i.e. a child of) a PARALLEL region.

        :param node: the supplied node to which we will apply the \
                     Loop transformation.
        :type node: :py:class:`psyclone.psyir.nodes.Loop`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None
        :param int options["collapse"]: number of nested loops to collapse.
        :param bool options["independent"]: whether to add the "independent" \
                clause to the directive (not strictly necessary within \
                PARALLEL regions).

        '''
        # Store sub-class specific options. These are used when
        # creating the directive (in the _directive() method).
        if not options:
            options = {}
        self._independent = options.get("independent", True)
        self._sequential = options.get("sequential", False)

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
        return "Add an 'OpenMP PARALLEL DO' directive with no validity checks"

    def validate(self, node, options=None):
        '''Validity checks for input arguments.

        :param node: the PSyIR node to validate.
        :type node: :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :raises TransformationError: if the node is a loop over colours.

        '''
        # Check that the supplied Node is a Loop
        super().validate(node, options=options)

        # Check we are not a sequential loop
        if node.loop_type == 'colours':
            raise TransformationError("Error in "+self.name+" transformation. "
                                      "The requested loop is over colours and "
                                      "must be computed serially.")

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

    '''
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

        '''
        self.validate(node, options=options)

        # If the loop is not already coloured then check whether or not
        # it should be. If the field space is discontinuous (including
        # any_discontinuous_space) then we don't need to worry about
        # colouring.
        const = LFRicConstants()
        if node.field_space.orig_name not in \
           const.VALID_DISCONTINUOUS_NAMES:
            if node.loop_type != 'colour' and node.has_inc_arg():
                raise TransformationError(
                    f"Error in {self.name} transformation. The kernel has an "
                    f"argument with INC access. Colouring is required.")

        OMPParallelLoopTrans.apply(self, node)


class GOceanOMPParallelLoopTrans(OMPParallelLoopTrans):

    '''GOcean specific OpenMP Do loop transformation. Adds GOcean
       specific validity checks (that supplied Loop is an inner or outer
       loop). Actual transformation is done by
       :py:class:`base class <OMPParallelLoopTrans>`.

       :param omp_schedule: the omp schedule to be created. Must be one of \
           'runtime', 'static', 'dynamic', 'guided' or 'auto'.

    '''
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

        '''
        self.validate(node, options=options)

        # Check we are either an inner or outer loop
        if node.loop_type not in ["inner", "outer"]:
            raise TransformationError(
                "Error in "+self.name+" transformation.  The requested loop"
                " is not of type inner or outer.")

        OMPParallelLoopTrans.apply(self, node)


class Dynamo0p3OMPLoopTrans(OMPLoopTrans):

    ''' Dynamo 0.3 specific orphan OpenMP loop transformation. Adds
    Dynamo-specific validity checks. Actual transformation is done by
    :py:class:`base class <OMPLoopTrans>`.

    '''
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

        self.validate(node, options=options)

        # If the loop is not already coloured then check whether or not
        # it should be
        if node.loop_type != 'colour' and node.has_inc_arg():
            raise TransformationError(
                f"Error in {self.name} transformation. The kernel has an "
                f"argument with INC access. Colouring is required.")

        OMPLoopTrans.apply(self, node, options)


class GOceanOMPLoopTrans(OMPLoopTrans):

    ''' GOcean-specific orphan OpenMP loop transformation. Adds GOcean
        specific validity checks (that the node is either an inner or outer
        Loop). Actual transformation is done by
        :py:class:`base class <OMPLoopTrans>`.

        :param omp_schedule: the omp schedule to be created. Must be one of
            'runtime', 'static', 'dynamic', 'guided' or 'auto'.

        '''
    def __str__(self):
        return "Add an OpenMP DO directive to a GOcean loop"

    def validate(self, node, options=None):
        '''
        Checks that the supplied node is a valid target for parallelisation
        using OMP Do.

        :param node: the candidate loop for parallelising using OMP Do.
        :type node: :py:class:`psyclone.psyir.nodes.Loop`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

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
        :type options: Optional[Dict[str,str]]

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
        # pylint: disable=no-self-use
        raise InternalError("_create_colours_loop() must be overridden in an "
                            "API-specific sub-class.")


class KernelModuleInlineTrans(KernelTrans):
    '''Switches on, or switches off, the inlining of a Kernel subroutine
    into the PSy layer module. For example:

    >>> invoke = ...
    >>> schedule = invoke.schedule
    >>>
    >>> inline_trans = KernelModuleInlineTrans()
    >>>
    >>> inline_trans.apply(schedule.children[0].loop_body[0])
    >>> # Uncomment the following line to see a text view of the schedule
    >>> # print(schedule.view())

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

        '''
        self.validate(node, options)

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
        :type node: :py:class:`psyclone.dynamo0p3.DynLoop`
        :param options: a dictionary with options for transformations.\
        :type options: dictionary of string:values or None

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
        # Holds the class instance for the type of parallel region
        # to generate
        self._pdirective = None
        super().__init__()

    @abc.abstractmethod
    def __str__(self):
        pass  # pragma: no cover

    @property
    @abc.abstractmethod
    def name(self):
        ''' Returns the name of this transformation as a string.'''

    def validate(self, node_list, options=None):
        # pylint: disable=arguments-renamed
        '''
        Check that the supplied list of Nodes are eligible to be
        put inside a parallel region.

        :param list node_list: list of nodes to put into a parallel region
        :param options: a dictionary with options for transformations.\
        :type options: dictionary of string:values or None
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
        :type options: dictionary of string:values or None
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
        directive = self._pdirective(
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
    >>> ast, invokeInfo = parse(SOURCE_FILE, api=api)
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
        self._pdirective = self._directive
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
        :type options: a dict of string:values or None
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
    >>> ast, invokeInfo = parse(SOURCE_FILE, api=api)
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
        self._pdirective = OMPMasterDirective

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
    >>> ast, invokeInfo = parse(SOURCE_FILE, api=api)
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
        if node_list[0].ancestor(OMPDirective):
            raise TransformationError("Error in OMPParallel transformation:" +
                                      " cannot create an OpenMP PARALLEL " +
                                      "region within another OpenMP region.")

        # Now call the general validation checks
        super().validate(node_list, options)


class ACCParallelTrans(ParallelRegionTrans):
    '''
    Create an OpenACC parallel region by inserting directives. This parallel
    region *must* come after an enter-data directive (see `ACCEnterDataTrans`)
    or within a data region (see `ACCDataTrans`). For example:

    >>> from psyclone.parse.algorithm import parse
    >>> from psyclone.psyGen import PSyFactory
    >>> api = "gocean1.0"
    >>> ast, invokeInfo = parse(SOURCE_FILE, api=api)
    >>> psy = PSyFactory(api).create(invokeInfo)
    >>>
    >>> from psyclone.psyGen import TransInfo
    >>> t = TransInfo()
    >>> ptrans = t.get_trans_name('ACCParallelTrans')
    >>> dtrans = t.get_trans_name('ACCDataTrans')
    >>>
    >>> schedule = psy.invokes.get('invoke_0').schedule
    >>> # Uncomment the following line to see a text view of the schedule
    >>> # print(schedule.view())
    >>>
    >>> # Enclose everything within a single OpenACC PARALLEL region
    >>> ptrans.apply(schedule.children)
    >>> # Add an enter-data directive
    >>> dtrans.apply(schedule)
    >>> # Uncomment the following line to see a text view of the schedule
    >>> # print(schedule.view())

    '''
    excluded_node_types = (CodeBlock, Return, PSyDataNode,
                           ACCDataDirective, ACCEnterDataDirective)

    def __init__(self):
        super().__init__()
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
        # pylint: disable=no-self-use, arguments-differ
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
        :type options: dictionary of string:values or None
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
        :type loop: :py:class:`psyclone.psyGen.DynLoop`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None
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
        :type options: dictionary of string:values or None

        '''
        self.validate(node, options)

        # add asynchronous start and end halo exchanges and initialise
        # them using information from the existing synchronous halo
        # exchange
        # pylint: disable=protected-access
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
        node.detach()

    def validate(self, node, options):
        # pylint: disable=signature-differs
        '''Internal method to check whether the node is valid for this
        transformation.

        :param node: a synchronous Halo Exchange node
        :type node: :py:obj:`psyclone.psygen.HaloExchange`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :raises TransformationError: if the node argument is not a
                         HaloExchange (or subclass thereof)

        '''
        if not isinstance(node, psyGen.HaloExchange) or \
           isinstance(node, (DynHaloExchangeStart, DynHaloExchangeEnd)):
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
            # TODO: Temporarily use unsafe name change until the name
            # space manager is introduced into the SymbolTable (Issue
            # #321).
            orig_name = symbol.name
            local_symbol = DataSymbol(orig_name+"_dummy", INTEGER_TYPE,
                                      constant_value=value)
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
        if not isinstance(node, DynKern):
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
    >>> ast, invokeInfo = parse(SOURCE_FILE, api=api)
    >>> psy = PSyFactory(api).create(invokeInfo)
    >>>
    >>> from psyclone.psyGen import TransInfo
    >>> t = TransInfo()
    >>> dtrans = t.get_trans_name('ACCEnterDataTrans')
    >>>
    >>> schedule = psy.invokes.get('invoke_0').schedule
    >>> # Uncomment the following line to see a text view of the schedule
    >>> # print(schedule.view())
    >>>
    >>> # Add an enter-data directive
    >>> dtrans.apply(schedule)
    >>> # Uncomment the following line to see a text view of the schedule
    >>> # print(schedule.view())

    ...

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
        :type options: dictionary of string:values or None

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
        else:
            # Should not get here provided that validate() has done its job
            raise InternalError(
                f"ACCEnterDataTrans.validate() has not rejected an "
                f"(unsupported) schedule of type {type(sched)}")

        # Add the directive
        data_dir = AccEnterDataDir(parent=sched, children=[])
        sched.addchild(data_dir, index=0)

    def validate(self, sched, options=None):
        # pylint: disable=arguments-differ, arguments-renamed
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
        super().validate(sched, options)

        if not isinstance(sched, Schedule):
            raise TransformationError("Cannot apply an OpenACC enter-data "
                                      "directive to something that is "
                                      "not a Schedule")

        if not isinstance(sched, (GOInvokeSchedule, DynInvokeSchedule)):
            raise NotImplementedError(
                f"ACCEnterDataTrans: ACCEnterDataDirective not implemented for"
                f" a schedule of type {type(sched)}")

        # Check that we don't already have a data region of any sort
        directives = sched.walk(Directive)
        if any(isinstance(ddir, (ACCDataDirective, ACCEnterDataDirective))
               for ddir in directives):
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
    >>> ast, invokeInfo = parse(SOURCE_FILE, api=api)
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
        :type options: dictionary of string:values or None

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
        :type node: :py:class:`psyclone.psyGen.Kern` or \
                    :py:class:`psyclone.psyir.nodes.Routine`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :raises TransformationError: if the node is not a kernel or a routine.
        :raises TransformationError: if the target is a built-in kernel.
        :raises TransformationError: if it is a kernel but without an \
                                     associated PSyIR.
        :raises TransformationError: if any of the symbols in the kernel are \
                                     accessed via a module use statement.
        '''
        super().validate(node, options)

        if not isinstance(node, Kern) and not isinstance(node, Routine):
            raise TransformationError(
                f"The ACCRoutineTrans must be applied to a sub-class of "
                f"Kern or Routine but got '{type(node).__name__}'.")

        # If it is a kernel call it must have an accessible implementation
        if isinstance(node, Kern):
            if isinstance(node, BuiltIn):
                raise TransformationError(
                    f"Applying ACCRoutineTrans to a built-in kernel is not yet"
                    f" supported and kernel '{node.name}' is of type "
                    f"'{type(node).__name__}'")

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

            # Check that the kernel does not access any data or routines via a
            # module 'use' statement
            imported_variables = kernel_schedule.symbol_table.imported_symbols
            if imported_variables:
                raise TransformationError(
                    f"The Symbol Table for kernel '{node.name}' contains the "
                    f"following symbol(s) with imported interface: "
                    f"{[sym.name for sym in imported_variables]}. If these "
                    f"symbols represent data then they must first be converted"
                    f" to kernel arguments using the KernelImportsToArguments "
                    f"transformation. If the symbols represent external "
                    f"routines then PSyclone cannot currently transform this "
                    f"kernel for execution on an OpenACC device (issue #342).")


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
    >>> # Uncomment the following line to see a text view of the schedule
    >>> # print(schedule.view())
    >>> kernels = schedule.children[0].children[0].children[0:-1]
    >>> # Transform the kernel
    >>> ktrans.apply(kernels)

    '''
    excluded_node_types = (CodeBlock, Return, PSyDataNode)

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
        :type options: dictionary of string:values or None
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
        :type options: dictionary of string:values or None

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
                    if assign.is_array_range) or node.walk(Loop)):
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
    >>> # Uncomment the following line to see a text view of the schedule
    >>> # print(schedule.view())
    >>> kernels = schedule.children[0].children[0].children[0:-1]
    >>> # Enclose the kernels
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
        :type options: dictionary of string:values or None

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
        :type nodes: (list of) subclasses of \
                     :py:class:`psyclone.psyir.nodes.Node`
        :param options: a dictionary with options for transformations.
        :type options: dictionary of string:values or None

        :raises TransformationError: if the Schedule to which the nodes \
                                belong already has an 'enter data' directive.
        :raises TransformationError: if any of the nodes are themselves \
                                     data directives.
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
        :type options: dictionary of string:values or None

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
        :type options: dictionary of string:values or None

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
            if (type(imported_var) == Symbol or
                    isinstance(imported_var.datatype, DeferredType)):
                updated_sym = imported_var.resolve_deferred()
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
            if updated_sym.is_constant:
                # Imported constants lose the constant value but are read-only
                # TODO: When #633 and #11 are implemented, warn the user that
                # they should transform the constants to literal values first.
                updated_sym.constant_value = None
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
__all__ = ["KernelTrans",
           "ParallelLoopTrans",
           "OMPLoopTrans",
           "ACCLoopTrans",
           "OMPParallelLoopTrans",
           "DynamoOMPParallelLoopTrans",
           "GOceanOMPParallelLoopTrans",
           "Dynamo0p3OMPLoopTrans",
           "GOceanOMPLoopTrans",
           "ColourTrans",
           "KernelModuleInlineTrans",
           "Dynamo0p3ColourTrans",
           "ParallelRegionTrans",
           "OMPSingleTrans",
           "OMPMasterTrans",
           "OMPParallelTrans",
           "ACCParallelTrans",
           "MoveTrans",
           "Dynamo0p3RedundantComputationTrans",
           "Dynamo0p3AsyncHaloExchangeTrans",
           "Dynamo0p3KernelConstTrans",
           "ACCEnterDataTrans",
           "ACCRoutineTrans",
           "ACCKernelsTrans",
           "ACCDataTrans",
           "KernelImportsToArguments"]
