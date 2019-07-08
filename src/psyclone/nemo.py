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
# ----------------------------------------------------------------------------
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab

'''This module implements the PSyclone NEMO API by specialising
   the required base classes for both code generation (PSy, Invokes,
   Invoke, InvokeSchedule, Loop, CodedKern, Arguments and KernelArgument)
   and parsing (Descriptor and KernelType).

'''

from __future__ import print_function, absolute_import
import copy
from fparser.two.utils import walk_ast, get_child
from fparser.two import Fortran2003
from psyclone.configuration import Config
from psyclone.core.access_type import AccessType
from psyclone.psyGen import PSy, Invokes, Invoke, InvokeSchedule, Node, \
    Loop, CodedKern, InternalError, NameSpaceFactory, \
    Fparser2ASTProcessor, SCHEDULE_COLOUR_MAP as _BASE_CMAP

# The base colour map doesn't have CodeBlock as that is currently
# a NEMO-API-specific entity.
NEMO_SCHEDULE_COLOUR_MAP = copy.deepcopy(_BASE_CMAP)
NEMO_SCHEDULE_COLOUR_MAP["CodeBlock"] = "red"


class NemoFparser2ASTProcessor(Fparser2ASTProcessor):
    '''
    Specialisation of Fparser2ASTProcessor for the Nemo API. It is used
    as a Mixin in the Nemo API.
    '''
    def _create_child(self, child, parent=None):
        '''
        Adds Nemo API specific processors for certain fparser2 types
        before calling the parent _create_child.

        :param child: node in fparser2 AST.
        :type child:  :py:class:`fparser.two.utils.Base`
        :param parent: Parent node in the PSyclone IR we are constructing.
        :type parent: :py:class:`psyclone.psyGen.Node`
        :return: Returns the PSyIR representation of child or None if \
                 there isn't one.
        :rtype: :py:class:`psyclone.psyGen.Node` or NoneType
        '''
        if NemoLoop.match(child):
            return NemoLoop(child, parent=parent)
        if isinstance(child, Fortran2003.Nonlabel_Do_Stmt):
            # The fparser2 parse tree representing a Do loop has a
            # Block_Nonlabel_Do_Construct which then has a Nonlabel_Do_Stmt
            # as its child. Since we handle the former (by creating a NemoLoop)
            # in the previous if-clause, we don't need to do anything with
            # the Nonlabel_Do_Stmt and so return None.
            return None
        if NemoImplicitLoop.match(child):
            return NemoImplicitLoop(child, parent=parent)
        return super(NemoFparser2ASTProcessor,
                     self)._create_child(child, parent=parent)


class NemoInvoke(Invoke):
    '''
    Represents a NEMO 'Invoke' which, since NEMO is existing code, means
    an existing program unit, e.g. a subroutine.

    :param ast: node in fparser2 AST representing the program unit.
    :type ast: :py:class:`fparser.two.Fortran2003.Main_Program` or \
               :py:class:`fparser.two.Fortran2003.Module`
    :param str name: the name of this Invoke (program unit).
    '''
    def __init__(self, ast, name):
        # pylint: disable=super-init-not-called
        self._schedule = None
        self._name = name
        # Store the whole fparser2 AST
        self._ast = ast
        # A temporary workaround for the fact that we don't yet have a
        # symbol table to store information on the variable declarations.
        # TODO (#255) remove this workaround.
        self._loop_vars = []
        self._name_space_manager = NameSpaceFactory().create()
        from fparser.two.Fortran2003 import Execution_Part, Specification_Part

        # Find the section of the tree containing the execution part
        # of the code
        exe_part = get_child(ast, Execution_Part)
        if not exe_part:
            # This subroutine has no execution part so we skip it
            # TODO log this event
            return

        # Store the root of this routine's specification in the AST
        self._spec_part = get_child(ast, Specification_Part)

        # We now walk through the AST produced by fparser2 and construct a
        # new AST using objects from the nemo module.
        self._schedule = NemoInvokeSchedule(self, exe_part)

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
    a single NEMO source file.

    :param ast: The fparser2 AST for the whole Fortran source file
    :type ast: :py:class:`fparser.two.Fortran2003.Main_Program`
    '''
    def __init__(self, ast):
        # pylint: disable=super-init-not-called, too-many-locals
        from fparser.two.Fortran2003 import Main_Program, Program_Stmt, \
            Subroutine_Subprogram, Function_Subprogram, Function_Stmt, \
            Subroutine_Stmt, Name

        self.invoke_map = {}
        self.invoke_list = []
        # Keep a pointer to the whole fparser2 AST
        self._ast = ast

        # Find all the subroutines contained in the file
        routines = walk_ast(ast.content, [Subroutine_Subprogram,
                                          Function_Subprogram])
        # Add the main program as a routine to analyse - take care
        # here as the Fortran source file might not contain a
        # main program (might just be a subroutine in a module)
        main_prog = get_child(ast, Main_Program)
        if main_prog:
            routines.append(main_prog)

        # Analyse each routine we've found
        for subroutine in routines:
            # Get the name of this (sub)routine
            substmt = walk_ast(subroutine.content,
                               [Subroutine_Stmt, Function_Stmt, Program_Stmt])
            if isinstance(substmt[0], Function_Stmt):
                for item in substmt[0].items:
                    if isinstance(item, Name):
                        sub_name = str(item)
                        break
            else:
                sub_name = str(substmt[0].get_name())

            my_invoke = NemoInvoke(subroutine, name=sub_name)
            self.invoke_map[sub_name] = my_invoke
            self.invoke_list.append(my_invoke)

    def update(self):
        ''' Walk down the tree and update the underlying fparser2 AST
        to reflect any transformations. '''
        for invoke in self.invoke_list:
            invoke.update()


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
        names = walk_ast(ast.content, [Fortran2003.Name])
        # The name of the program unit will be the first in the list
        if not names:
            raise InternalError("Found no names in supplied Fortran - should "
                                "be impossible!")
        self._name = str(names[0]) + "_psy"

        self._invokes = NemoInvokes(ast)
        self._ast = ast

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
        Generate the (updated) fparser2 AST for the NEMO code represented
        by this NemoPSy object.

        :returns: the fparser2 AST for the Fortran code.
        :rtype: :py:class:`fparser.two.Fortran2003.Main_Program` or \
                :py:class:`fparser.two.Fortran2003.Subroutine_Subprogram` or \
                :py:class:`fparser.two.Fortran2003.Function_Subprogram`.
        '''
        # Walk down our Schedule and update the underlying fparser2 AST
        # to account for any transformations
        self.invokes.update()

        # Return the fparser2 AST
        return self._ast


class NemoInvokeSchedule(InvokeSchedule, NemoFparser2ASTProcessor):
    '''
    The NEMO-specific InvokeSchedule sub-class. This is the top-level node in
    PSyclone's IR of a NEMO program unit (program, subroutine etc).

    :param invoke: The Invoke to which this NemoInvokeSchedule belongs.
    :type invoke: :py:class:`psyclone.nemo.NemoInvoke`
    :param ast: the fparser2 AST of the NEMO code for which to generate \
                a NemoInvokeSchedule.
    :type ast: :py:class:`fparser.two.Fortran2003.Main_Program` or \
               :py:class:`fparser.two.Fortran2003.Subroutine_Subprogram` or \
               :py:class:`fparser.two.Fortran2003.Function_Subprogram`.

    '''
    def __init__(self, invoke, ast):
        # pylint: disable=super-init-not-called, non-parent-init-called
        Node.__init__(self)
        NemoFparser2ASTProcessor.__init__(self)

        self._invoke = invoke
        self._ast = ast
        self.process_nodes(self, ast.content, ast)

    def view(self, indent=0):
        '''
        Print a representation of this NemoInvokeSchedule to stdout.

        :param int indent: level to which to indent output.
        '''
        print(self.indent(indent) + self.coloured_text + "[]")
        for entity in self._children:
            entity.view(indent=indent + 1)

    def __str__(self):
        ''' Returns the string representation of this NemoInvokeSchedule. '''
        result = "NemoInvokeSchedule():\n"
        for entity in self._children:
            result += str(entity)+"\n"
        result += "End Schedule"
        return result


class NemoKern(CodedKern):
    ''' Stores information about NEMO kernels as extracted from the
    NEMO code. Kernels are leaves in the PSyIR. I.e. they have
    no self._children but they do have a KernelSchedule.

    :param psyir_nodes: the list of PSyIR nodes that represent the body \
                        of this kernel.
    :type psyir_nodes: list of :py:class:`psyclone.psyGen.Node`
    :param parse_tree: Reference to the innermost loop in the fparser2 parse \
                       tree that encloses this kernel.
    :type parse_tree: \
              :py:class:`fparser.two.Fortran2003.Block_Nonlabel_Do_Construct`
    :param parent: the parent of this Kernel node in the PSyIR or None (if \
                   this kernel is being created in isolation).
    :type parent: :py:class:`psyclone.nemo.NemoLoop` or NoneType.

    '''
    def __init__(self, psyir_nodes, parse_tree, parent=None):
        # pylint: disable=super-init-not-called
        from psyclone.psyGen import KernelSchedule
        self._name = ""
        self._parent = parent
        # The corresponding set of nodes in the fparser2 parse tree
        self._ast = parse_tree
        # Create a kernel schedule
        self._kern_schedule = KernelSchedule(self._name)
        # Attach the PSyIR sub-tree to it
        self._kern_schedule.children = psyir_nodes[:]
        # Update the parent info for each node we've moved
        for node in self._kern_schedule.children:
            node.parent = self._kern_schedule
        # A Kernel is a leaf in the PSyIR that then has its own KernelSchedule.
        # We therefore don't have any children.
        self._children = []

    @staticmethod
    def match(node):
        '''
        Whether or not the PSyIR sub-tree pointed to by node represents a
        kernel. A kernel is defined as a section of code that sits
        within a recognised loop structure and does not itself contain
        loops or 'CodeBlocks' (code not represented in the PSyIR such as
        subroutine calls or IO operations).

        :param node: Node in the PSyIR to check.
        :type node: :py:class:`psyclone.psyGen.Node`
        :returns: True if this node conforms to the rules for a kernel.
        :rtype: bool
        '''
        from psyclone.psyGen import CodeBlock
        # This function is called with node being a NemoLoop. This means
        # that 'node' is always part of the result of walk. So if there
        # is a loop or CodeBlock inside, walk will return more than one
        # element (the first being the node)
        assert isinstance(node, NemoLoop)
        nodes = node.walk((CodeBlock, NemoLoop))

        # A kernel cannot contain loops or other unrecognised code (including
        # IO operations and routine calls) or loops. So if there is more than
        # one node in the result, this node can not be a kernel.
        return len(nodes) == 1

    def local_vars(self):
        '''
        :returns: list of the variable (names) that are local to this loop \
                  (and must therefore be e.g. threadprivate if doing OpenMP)
        :rtype: list of str
        '''
        return []

    def view(self, indent=0):
        '''
        Print representation of this node to stdout.
        :param int indent: level to which to indent output.
        '''
        print(self.indent(indent) + self.coloured_text + "[]")

    def reference_accesses(self, var_accesses):
        '''Get all variable access information. It calls the corresponding
        kernel schedule function.
        :param var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`
        '''
        self._kern_schedule.reference_accesses(var_accesses)

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


class NemoLoop(Loop, NemoFparser2ASTProcessor):
    '''
    Class representing a Loop in NEMO.

    :param ast: node in the fparser2 AST representing the loop.
    :type ast: :py:class:`fparser.two.Block_Nonlabel_Do_Construct`
    :param parent: parent of this NemoLoop in the PSyclone AST.
    :type parent: :py:class:`psyclone.psyGen.Node`
    '''
    def __init__(self, ast, parent=None):
        from fparser.two.Fortran2003 import Loop_Control
        valid_loop_types = Config.get().api_conf("nemo").get_valid_loop_types()
        Loop.__init__(self, parent=parent,
                      valid_loop_types=valid_loop_types)
        NemoFparser2ASTProcessor.__init__(self)
        # Keep a ptr to the corresponding node in the parse tree
        self._ast = ast

        # Get the loop variable
        ctrl = walk_ast(ast.content, [Loop_Control])
        # If this is a DO WHILE then the first element of items will
        # not be None. The `match` method should have already rejected
        # such loops so we should never get to here.
        if ctrl[0].items[0]:
            raise InternalError("NemoLoop constructor should not have been "
                                "called for a DO WHILE")

        # Second element of items member of Loop Control is itself a tuple
        # containing:
        #   Loop variable, [start value expression, end value expression, step
        #   expression]
        # Loop variable will be an instance of Fortran2003.Name
        loop_var = str(ctrl[0].items[1][0])
        self._variable_name = str(loop_var)

        # Identify the type of loop
        loop_type_mapping = Config.get().api_conf("nemo")\
            .get_loop_type_mapping()
        if self._variable_name in loop_type_mapping:
            self.loop_type = loop_type_mapping[self._variable_name]
        else:
            self.loop_type = "unknown"

        # Get the loop limits. These are given in a list which is the second
        # element of a tuple which is itself the second element of the items
        # tuple:
        # (None, (Name('jk'), [Int_Literal_Constant('1', None), Name('jpk'),
        #                      Int_Literal_Constant('1', None)]), None)
        limits_list = ctrl[0].items[1][1]
        self._start = str(limits_list[0])
        self._stop = str(limits_list[1])
        if len(limits_list) == 3:
            self._step = str(limits_list[2])
        else:
            # Default loop increment is 1
            self._step = "1"

        # First process the rest of the parse tree below this point
        self.process_nodes(self, self._ast.content, self._ast)
        # Now check the PSyIR of this loop body to see whether it is
        # a valid kernel
        if NemoKern.match(self):
            # It is, so we create a new kernel object and make it the only
            # child of this Loop node. The PSyIR of the loop body becomes
            # the schedule of this kernel.
            self.children = [NemoKern(self.children, self._ast, parent=self)]

    @staticmethod
    def match(node):
        '''
        Tests the supplied node to see whether it is a recognised form of
        NEMO loop.

        :param node: the node in the fparser2 parse tree to test for a match.
        :type node: :py:class:`fparser.two.utils.Base`

        :returns: True if the node represents a recognised form of loop, \
                  False otherwise.
        :rtype: bool

        :raises InternalError: if the parse tree represents a loop but no \
                               Loop_Control element is present.

        '''
        if not isinstance(node, Fortran2003.Block_Nonlabel_Do_Construct):
            return False
        ctrl = walk_ast(node.content, my_types=[Fortran2003.Loop_Control])
        if not ctrl:
            raise InternalError("Unrecognised form of DO loop - failed to "
                                "find Loop_Control element in parse tree.")
        if ctrl[0].items[0]:
            # If this is a DO WHILE then the first element of items will not
            # be None. (See `fparser.two.Fortran2003.Loop_Control`.)
            # TODO #359 DO WHILE's are currently just put into CodeBlocks
            # rather than being properly described in the PSyIR.
            return False
        return True

    def __str__(self):
        result = ("NemoLoop[" + self._loop_type + "]: " + self._variable_name +
                  "=" + ",".join([self._start, self._stop, self._step]) + "\n")
        for entity in self._children:
            result += str(entity) + "\n"
        result += "EndLoop"
        return result

    def reference_accesses(self, var_accesses):
        '''Get all variable access information. The loop variable is
        set as READ and WRITE. Then the loop body's access is added.
        TODO: The start, stop and step values are only strings, so we
        can't get access information. It might then also be possible to
        just fall back to Loop.reference_accesses (which then should work).
        :param var_accesses: \
            :py:class:`psyclone.core.access_info.VariablesAccessInfo`
        '''

        var_accesses.add_access(self._variable_name, AccessType.WRITE, self)
        var_accesses.add_access(self._variable_name, AccessType.READ, self)
        var_accesses.next_location()
        # TODO: atm start, stop and step are just strings, so we can't
        # get any variable information
        # self._start.reference_accesses(var_accesses)
        # self._stop.reference_accesses(var_accesses)
        # self._step.reference_accesses(var_accesses)
        # We also can't call the base class, since it will accesses
        # start stop etc :(
        # super(NemoLoop, self).reference_accesses(var_accesses)
        for child in self.children:
            child.reference_accesses(var_accesses)
            var_accesses.next_location()

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


class NemoImplicitLoop(NemoLoop):
    '''
    Class representing an implicit loop in NEMO (i.e. using Fortran array
    syntax).

    :param ast: the part of the fparser2 AST representing the loop.
    :type ast: :py:class:`fparser.two.Fortran2003.Assignment_Stmt`
    :param parent: the parent of this Loop in the PSyIRe.
    :type parent: :py:class:`psyclone.psyGen.Node`

    '''
    def __init__(self, ast, parent=None):
        # pylint: disable=super-init-not-called, non-parent-init-called
        valid_loop_types = Config.get().api_conf("nemo").get_valid_loop_types()
        Loop.__init__(self, parent=parent,
                      valid_loop_types=valid_loop_types)
        # Keep a ptr to the corresponding node in the AST
        self._ast = ast

    def __str__(self):
        # Display the LHS of the assignment in the str representation
        return "NemoImplicitLoop[{0}]\n".format(self._ast.items[0])

    @staticmethod
    def match(node):
        '''
        Checks whether the supplied node in the fparser2 AST represents
        an implicit loop (using Fortran array syntax).

        :param node: node in the fparser2 AST to check
        :type node: :py:class:`fparser.two.Fortran2003.Assignment_Stmt`
        :returns: True if the node does represent an implicit loop.
        :rtype: bool

        '''
        # pylint: disable=too-many-return-statements
        if not isinstance(node, Fortran2003.Assignment_Stmt):
            return False
        # We are expecting something like:
        #    array(:,:,jk) = some_expression
        # but we have to beware of cases like the following:
        #   array(1,:,:) = a_func(array2(:,:,:), mask(:,:))
        # where a_func is an array-valued function and `array2(:,:,:)`
        # could just be `array2`.
        # We check the left-hand side...
        lhs = node.items[0]
        if not isinstance(lhs, Fortran2003.Part_Ref):
            # LHS is not an array reference
            return False
        colons = walk_ast(lhs.items, [Fortran2003.Subscript_Triplet])
        if not colons:
            # LHS does not use array syntax
            return False
        # Now check the right-hand side...
        rhs = node.items[2]
        try:
            if not walk_ast(rhs.items, [Fortran2003.Subscript_Triplet]):
                # We don't have any array syntax on the RHS
                return True
        except AttributeError:
            # The RHS doesn't have the `items` attribute (it may be just
            # a Name for instance).
            return True
        # Check that we haven't got array syntax used within the index
        # expression to another array. Array references are represented by
        # Part_Ref nodes in the fparser2 AST. This would be easier to do
        # if the fparser2 AST carried parent information with each node.
        # As it is we have to walk down the tree rather than come back up
        # from each colon.
        # Find all array references
        array_refs = []
        if isinstance(rhs, Fortran2003.Part_Ref):
            # Since walk_ast is slightly clunky we have to manually allow
            # for the top-level "rhs" node being an array reference
            array_refs.append(rhs)
        array_refs += walk_ast(rhs.items, [Fortran2003.Part_Ref])
        for ref in array_refs:
            nested_refs = walk_ast(ref.items, [Fortran2003.Part_Ref])
            # Do any of these nested array references use array syntax?
            for nested_ref in nested_refs:
                colons = walk_ast(nested_ref.items,
                                  [Fortran2003.Subscript_Triplet])
                if colons:
                    return False
        return True
