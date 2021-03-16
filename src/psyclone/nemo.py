# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2021, Science and Technology Facilities Council.
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
# ----------------------------------------------------------------------------
# Authors: R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified work Copyright (c) 2019 by J. Henrichs, Bureau of Meteorology

'''This module implements the PSyclone NEMO API by specialising
   the required base classes for both code generation (PSy, Invokes,
   Invoke, InvokeSchedule, Loop, CodedKern, Arguments and KernelArgument)
   and parsing (Fparser2Reader).

'''

from __future__ import print_function, absolute_import
from fparser.two.utils import walk
from fparser.two import Fortran2003
from psyclone.configuration import Config
from psyclone.psyGen import PSy, Invokes, Invoke, InvokeSchedule, \
    InlinedKern
from psyclone.errors import InternalError
from psyclone.psyir.nodes import Loop, Schedule, Routine
from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.errors import GenerationError


class NemoFparser2Reader(Fparser2Reader):
    '''
    Specialisation of Fparser2Reader for the Nemo API.
    '''
    @staticmethod
    def _create_schedule(name):
        '''
        Create an empty InvokeSchedule. The un-named argument would be 'name'
        but this isn't used in the NEMO API.

        :returns: New InvokeSchedule empty object.
        :rtype: py:class:`psyclone.nemo.NemoInvokeSchedule`
        '''
        return NemoInvokeSchedule(name)

    def _create_loop(self, parent, variable):
        '''
        Specialised method to create a NemoLoop instead of a
        generic Loop.

        :param parent: the parent of the node.
        :type parent: :py:class:`psyclone.psyir.nodes.Node`
        :param variable: the loop variable.
        :type variable: :py:class:`psyclone.psyir.symbols.DataSymbol`

        :return: a new NemoLoop instance.
        :rtype: :py:class:`psyclone.nemo.NemoLoop`

        '''
        loop = NemoLoop(parent=parent, variable=variable)

        loop_type_mapping = Config.get().api_conf("nemo")\
            .get_loop_type_mapping()

        # Identify the type of loop
        if variable.name in loop_type_mapping:
            loop.loop_type = loop_type_mapping[variable.name]
        else:
            loop.loop_type = "unknown"

        return loop

    def _process_loopbody(self, loop_body, node):
        '''
        Specialized method to process Nemo loop bodies. If the schedule
        matches with a NemoKern, it will add a NemoKern instead of statements
        in the loop_body.

        :param loop_body: schedule representing the body of the loop.
        :type loop_body: :py:class:`psyclone.psyir.nodes.Schedule`
        :param node: fparser loop node being processed.
        :type node: \
            :py:class:`fparser.two.Fortran2003.Block_Nonlabel_Do_Construct`
        '''
        # We create a fake node because we need to parse the children
        # before we decide what to do with them.
        fakeparent = Schedule(parent=loop_body)
        self.process_nodes(parent=fakeparent, nodes=node.content[1:-1])

        if NemoKern.match(fakeparent):
            # Create a new kernel object and make it the only
            # child of this Loop node. The PSyIR of the loop body becomes
            # the schedule of this kernel.
            nemokern = NemoKern(fakeparent.children, node, parent=loop_body)
            loop_body.children.append(nemokern)
        else:
            # Otherwise just connect the new children into the tree.
            loop_body.children.extend(fakeparent.children)
            for child in fakeparent.children:
                child.parent = loop_body


def raise_psyir(psyir):
    '''
    Takes generic PSyIR and replaces recognised structures with
    NEMO-specific PSyIR (in-place). Note that this may mean replacing
    the top-level node itself and therefore this routine returns the
    root of the modified tree.

    :param psyir: the root node of the PSyIR tree to process.

    :returns: root of the modified PSyIR tree.
    :rtype: :py:class:`psyclone.psyir.nodes.Node`

    '''
    # Deal with Routines first. Have to take care of the case where the
    # supplied top-level node is itself a Routine and must therefore be
    # replaced.
    root = psyir

    for routine in psyir.walk(Routine):
        new_node = NemoInvokeSchedule.create(routine.name,
                                             routine.symbol_table,
                                             routine.children,
                                             is_program=routine.is_program)
        if routine is not psyir:
            routine.replace_with(new_node)
        else:
            # We need to replace the top node in the (possibly sub-) PSyIR
            # tree that we've been passsed.
            if psyir.parent:
                psyir.replace_with(new_node)
            root = new_node

    # Reverse the result of the walk() so that we process loops depth-first.
    # This permits the correct identification of NemoKern's.
    for loop in reversed(root.walk(Loop)):
        # Convert a generic loop into a NEMO Loop by creating a new
        # NemoLoop object and inserting it into the PSyIR.
        new_loop = NemoLoop.create(loop.variable, loop.children[0],
                                   loop.children[1],
                                   loop.children[2], loop.loop_body.children)
        loop.replace_with(new_loop)
        if NemoKern.match(new_loop.loop_body):
            nemokern = NemoKern(new_loop.loop_body[:], None,
                                parent=new_loop.loop_body)
            # TODO use new detach() method on original children of loop body
            new_loop.loop_body.children = []
            new_loop.loop_body.addchild(nemokern)

    return root


class NemoInvoke(Invoke):
    '''
    Represents a NEMO 'Invoke' which, since NEMO is existing code, means
    an existing program unit, e.g. a subroutine.

    :param sched: PSyIR node representing the program unit.
    :type sched: :py:class:`psyclone.psyir.nodes.Routine`
    :param str name: the name of this Invoke (program unit).
    :param invokes: the Invokes object that holds this Invoke.
    :type invokes: :py:class:`psyclone.psyGen.NemoInvokes`

    '''
    def __init__(self, sched, name, invokes):
        # pylint: disable=super-init-not-called
        self._invokes = invokes
        self._schedule = sched
        self._name = name
        self._schedule.invoke = self

    def update(self):
        '''
        Updates the fparser2 parse tree associated with this schedule to
        make it reflect any transformations that have been applied to
        the PSyclone PSyIR.
        '''
        if not self._schedule:
            return
        self._schedule.update()


class NemoInvokes(Invokes):
    '''
    Class capturing information on all 'Invokes' (program units) within
    a single NEMO source file. Contains a reference to the PSyIR Container
    node for the encapsulating Fortran module.

    :param ast: the fparser2 AST for the whole Fortran source file.
    :type ast: :py:class:`fparser.two.Fortran2003.Main_Program`
    '''
    def __init__(self, ast, psy):
        # pylint: disable=super-init-not-called
        self._psy = psy
        self.invoke_map = {}
        self.invoke_list = []
        # Keep a pointer to the whole fparser2 AST
        self._ast = ast

        processor = Fparser2Reader()
        psyir = processor.generate_psyir(ast)
        self._container = raise_psyir(psyir)
        routines = self._container.walk(Routine)

        # Create an Invoke for each routine we've found
        for subroutine in routines:

            my_invoke = NemoInvoke(subroutine, subroutine.name, self)
            self.invoke_map[subroutine.name] = my_invoke
            self.invoke_list.append(my_invoke)

    def update(self):
        ''' Walk down the tree and update the underlying fparser2 AST
        to reflect any transformations. '''
        for invoke in self.invoke_list:
            invoke.update()

    @property
    def container(self):
        # TODO move this to base class?
        return self._container


class NemoPSy(PSy):
    '''
    The NEMO-specific PSy class. This creates a NEMO-specific
    invokes object (which controls all the required invocation calls).
    Also overrides the PSy gen() method so that we update and then
    return the fparser2 AST for the (transformed) PSy layer.

    :param ast: the fparser2 AST for this PSy layer (i.e. NEMO routine)
    :type ast: :py:class:`fparser.two.Fortran2003.Main_Program` or \
               :py:class:`fparser.two.Fortran2003.Subroutine_Subprogram` or \
               :py:class:`fparser.two.Fortran2003.Function_Subprogram`.
    :raises InternalError: if no Fortran2003.Name nodes are found in the \
                           supplied AST.
    '''
    def __init__(self, ast):
        # pylint: disable=super-init-not-called
        names = walk(ast.content, Fortran2003.Name)
        # The name of the program unit will be the first in the list
        if not names:
            raise InternalError("Found no names in supplied Fortran - should "
                                "be impossible!")
        self._name = str(names[0]) + "_psy"
        # TODO this is now done in the NemoInvokes class
        self._container = None
        # TODO #435 remove this reference to the parse tree
        self._ast = ast
        self._invokes = NemoInvokes(ast, self)

    def inline(self, _):
        '''
        :raises NotImplementedError: since kernels in NEMO are, in general,
                                     already in-lined.
        '''
        # Override base-class method because we don't yet support it
        raise NotImplementedError("The NemoPSy.inline method has not yet "
                                  "been implemented!")

    @property
    def gen(self):
        '''
        Generate the Fortran for the NEMO code represented by this
        NemoPSy object.

        :returns: the Fortran code.
        :rtype: str

        '''
        from psyclone.psyir.backend.fortran import FortranWriter
        fwriter = FortranWriter()
        #import pdb; pdb.set_trace()
        return fwriter(self._invokes.container)


class NemoInvokeSchedule(InvokeSchedule):
    '''
    The NEMO-specific InvokeSchedule sub-class. This is the top-level node in
    PSyclone's IR of a NEMO program unit (program, subroutine etc).

    :param invoke: the Invoke to which this NemoInvokeSchedule belongs.
    :type invoke: :py:class:`psyclone.nemo.NemoInvoke`

    '''
    _text_name = "NemoInvokeSchedule"

    def __init__(self, name, invoke=None):
        # TODO #1010: The name placeholder should be changed with the
        # expected InvokeShcedule name to use the PSyIR backend.
        super(NemoInvokeSchedule, self).__init__(name, None, None)

        self._invoke = invoke
        # Whether or not we've already checked the associated Fortran for
        # potential name-clashes when inserting PSyData code.
        # TODO this can be removed once #435 is done and we're no longer
        # manipulating the fparser2 parse tree.
        self._name_clashes_checked = False

    @property
    def psy_data_name_clashes_checked(self):
        '''Getter for whether or not the underlying fparser2 AST has been
        checked for clashes with the symbols required by PSyData.
        TODO remove once #435 is complete.

        :returns: whether or not we've already checked the underlying \
                  fparser2 parse tree for symbol clashes with code we will \
                  insert for PSyData.
        :rtype: bool

        '''
        return self._name_clashes_checked

    @psy_data_name_clashes_checked.setter
    def psy_data_name_clashes_checked(self, value):
        ''' Setter for whether or not we've already checked the underlying
        fparser2 parse tree for symbol clashes with code we will insert for
        PSyData.
        TODO remove once #435 is complete.

        :param bool value: whether or not the check has been performed.

        '''
        self._name_clashes_checked = value

    def coded_kernels(self):
        '''
        Returns a list of all of the user-supplied kernels (as opposed to
        builtins) that are beneath this node in the PSyIR. In the NEMO API
        this means all instances of InlinedKern.

        :returns: all user-supplied kernel calls below this node.
        :rtype: list of :py:class:`psyclone.psyGen.CodedKern`
        '''
        return self.walk(InlinedKern)


class NemoKern(InlinedKern):
    ''' Stores information about NEMO kernels as extracted from the
    NEMO code. As an inlined kernel it contains a Schedule as first
    child.

    :param psyir_nodes: the list of PSyIR nodes that represent the body \
                        of this kernel.
    :type psyir_nodes: list of :py:class:`psyclone.psyir.nodes.Node`
    :param parse_tree: reference to the innermost loop in the fparser2 parse \
                       tree that encloses this kernel.
    :type parse_tree: \
              :py:class:`fparser.two.Fortran2003.Block_Nonlabel_Do_Construct`
    :param parent: the parent of this Kernel node in the PSyIR or None (if \
                   this kernel is being created in isolation).
    :type parent: :py:class:`psyclone.nemo.NemoLoop` or NoneType.

    '''
    # pylint: disable=too-many-instance-attributes
    def __init__(self, psyir_nodes, parse_tree, parent=None):
        super(NemoKern, self).__init__(psyir_nodes)
        self._name = ""
        self._parent = parent
        # The corresponding set of nodes in the fparser2 parse tree
        self._ast = parse_tree

        # Name and colour-code to use for displaying this node
        self._reduction = False

    @staticmethod
    def match(node):
        '''Whether or not the PSyIR sub-tree pointed to by node represents a
        kernel. A kernel is defined as a section of code that sits
        within a recognised loop structure and does not itself contain
        loops, array assignments, or 'CodeBlocks' (code not
        represented in the PSyIR such as subroutine calls or IO
        operations).

        :param node: node in the PSyIR to check.
        :type node: :py:class:`psyclone.psyir.nodes.Schedule`
        :returns: true if this node conforms to the rules for a kernel.
        :rtype: bool

        '''
        from psyclone.psyir.nodes import CodeBlock, Assignment
        # This function is called with node being a Schedule.
        if not isinstance(node, Schedule):
            raise InternalError("Expected 'Schedule' in 'match', got '{0}'.".
                                format(type(node)))

        # Check for array assignment, codeblock and loop
        nodes = [assign for assign in node.walk(Assignment)
                 if assign.is_array_range]
        nodes += node.walk((CodeBlock, NemoLoop))

        # A kernel cannot contain loops, array assignments or other
        # unrecognised code (including IO operations and routine
        # calls) or loops. So if there is any node in the result of
        # the walk, this node can not be a kernel.
        return len(nodes) == 0

    def get_kernel_schedule(self):
        '''
        Returns a PSyIR Schedule representing the kernel code. The
        kernel_schedule is created in the constructor and always exists.

        :returns: the kernel schedule representing the inlined kernel code.
        :rtype: :py:class:`psyclone.psyir.nodes.KernelSchedule`
        '''
        return self.children[0]

    def local_vars(self):
        '''
        :returns: list of the variable (names) that are local to this loop \
                  (and must therefore be e.g. threadprivate if doing OpenMP)
        :rtype: list of str
        '''
        return []

    def reference_accesses(self, var_accesses):
        '''Get all variable access information. It calls the corresponding
        kernel schedule function.

        :param var_accesses: VariablesAccessInfo that stores the information\
            about variable accesses.
        :type var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`
        '''
        self.children[0].reference_accesses(var_accesses)

    @property
    def ast(self):
        '''
        Override the default ast method as, for the NEMO API, we don't need
        to take any special action to get hold of the parse tree for the
        kernel.

        :returns: a reference to that part of the fparser2 parse tree that \
                  describes this kernel.
        :rtype: sub-class of :py:class:`fparser.two.utils.Base`
        '''
        return self._ast

    def gen_code(self, parent):
        '''This method must not be called for NEMO, since the actual
        kernels are inlined.

        :param parent: The parent of this kernel call in the f2pygen AST.
        :type parent: :py:calls:`psyclone.f2pygen.LoopGen`

        :raises InternalError: if this function is called.
        '''
        raise InternalError("NEMO kernels are assumed to be in-lined by "
                            "default therefore the gen_code method should not "
                            "have been called.")


class NemoLoop(Loop):
    '''
    Class representing a Loop in NEMO.

    :param parent: parent of this NemoLoop in the PSyclone AST.
    :type parent: :py:class:`psyclone.psyir.nodes.Node`
    :param str variable_name: optional name of the loop iterator \
        variable. Defaults to an empty string.
    '''
    def __init__(self, parent=None, variable=None):
        # The order in which names are returned in
        # get_valid_loop_types depends on the Python implementation
        # (as it pulls the values from a dictionary). To make it clear
        # that there is no implied ordering here we store
        # valid_loop_types as a set, rather than a list.
        valid_loop_types = set(
            Config.get().api_conf("nemo").get_valid_loop_types())
        Loop.__init__(self, parent=parent,
                      variable=variable,
                      valid_loop_types=valid_loop_types)

    @staticmethod
    def create(variable, start, stop, step, children):
        '''Create a NemoLoop instance given valid instances of a variable,
        start, stop and step nodes, and a list of child nodes for the
        loop body.

        :param variable: the PSyIR node containing the variable \
            of the loop iterator.
        :type variable: :py:class:`psyclone.psyir.symbols.DataSymbol`
        :param start: the PSyIR node determining the value for the \
            start of the loop.
        :type start: :py:class:`psyclone.psyir.nodes.Node`
        :param end: the PSyIR node determining the value for the end \
            of the loop.
        :type end: :py:class:`psyclone.psyir.nodes.Node`
        :param step: the PSyIR node determining the value for the loop \
            step.
        :type step: :py:class:`psyclone.psyir.nodes.Node`
        :param children: a list of PSyIR nodes contained in the \
            loop.
        :type children: list of :py:class:`psyclone.psyir.nodes.Node`

        :returns: a NemoLoop instance.
        :rtype: :py:class:`psyclone.nemo.NemoLoop`

        :raises GenerationError: if the arguments to the create method \
            are not of the expected type.

        '''
        Loop._check_variable(variable)

        if not isinstance(children, list):
            raise GenerationError(
                "children argument in create method of NemoLoop class "
                "should be a list but found '{0}'."
                "".format(type(children).__name__))

        # Create the loop
        loop = NemoLoop(variable=variable)
        schedule = Schedule(parent=loop, children=children)
        loop.children = [start, stop, step, schedule]
        for child in children:
            child.parent = schedule
        start.parent = loop
        stop.parent = loop
        step.parent = loop

        # Indicate the type of loop
        loop_type_mapping = Config.get().api_conf("nemo") \
                                        .get_loop_type_mapping()
        loop.loop_type = loop_type_mapping.get(variable.name, "unknown")
        return loop

    @property
    def kernel(self):
        '''
        :returns: the kernel object if one is associated with this loop, \
                  None otherwise.
        :rtype: :py:class:`psyclone.nemo.NemoKern` or None

        :raises NotImplementedError: if the loop contains >1 kernel.
        '''
        kernels = self.walk(NemoKern)
        if kernels:
            # TODO cope with case where loop contains >1 kernel (e.g.
            # following loop fusion)
            if len(kernels) > 1:
                raise NotImplementedError(
                    "Kernel getter method does not yet support a loop "
                    "containing more than one kernel but this loop contains "
                    "{0}".format(len(kernels)))
            return kernels[0]
        return None
