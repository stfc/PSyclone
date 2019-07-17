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
from psyclone.configuration import Config
from psyclone.psyGen import PSy, Invokes, Invoke, InvokeSchedule, Node, \
    Loop, CodedKern, InternalError, NameSpaceFactory, Schedule, \
    Fparser2ASTProcessor
from fparser.two.utils import walk_ast, get_child
from fparser.two import Fortran2003


class NemoFparser2ASTProcessor(Fparser2ASTProcessor):
    '''
    Specialisation of Fparser2ASTProcessor for the Nemo API.
    '''
    def __init__(self):
        super(NemoFparser2ASTProcessor, self).__init__()

    def _create_loop(self, parent, variable_name):
        '''
        Specialized method to create a NemoLoop instead of a
        generic Loop.

        :param parent: The parent of the node.
        :type parent: :py:class:`psyclone.psyGen.Node`
        :param str variable_name: Name of the iteration variable

        :return: A new NemoLoop instance
        :rtype: :py:class:`psyclone.nemo.NemoLoop`
        '''
        loop = NemoLoop(parent=parent, variable_name=variable_name,
                        preinit=False)

        loop_type_mapping = Config.get().api_conf("nemo")\
            .get_loop_type_mapping()

        # Identify the type of loop
        if variable_name in loop_type_mapping:
            loop.loop_type = loop_type_mapping[variable_name]
        else:
            loop.loop_type = "unknown"

        return loop

    def _process_loopbody(self, loop_body, node):
        '''
        Specialized method to process Nemo loop bodies. If the schedule
        matches with a NemoKern, it will add a NemoKern instead of statements
        in the loop_body.

        :param loop_body: Schedule representing the body of the loop.
        :type loop_body: :py:class:`psyclone.psyGen.Schedule`
        :param node: fparser loop node being processed.
        :type node: \
            :py:class:`fparser.two.Fortran2003.Block_Nonlabel_Do_Construct`
        '''
        # We create a fake node because we need to parse the children
        # before we decide what to do with them.
        fakeparent = Schedule()
        self.process_nodes(parent=fakeparent, nodes=node.content[1:-1],
                           nodes_parent=node)

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

    def _create_child(self, child, parent=None):
        '''
        Before executing the generic _create_child, it checks if the parsed
        AST is a NemoImplicitLoop because this is not handled yet by the
        generic fparser2ASTProcessor.

        :param child: node in fparser2 AST.
        :type child:  :py:class:`fparser.two.utils.Base`
        :param parent: Parent node in the PSyclone IR we are constructing.
        :type parent: :py:class:`psyclone.psyGen.Node`

        :return: Returns the PSyIR representation of child or None if \
                 there isn't one.
        :rtype: :py:class:`psyclone.psyGen.Node` or NoneType
        '''
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
        if node.walk(node.children, (CodeBlock, NemoLoop)):
            # A kernel cannot contain unrecognised code (including IO
            # operations and routine calls) or loops.
            return False
        return True

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


class NemoLoop(Loop):
    '''
    Class representing a Loop in NEMO.

    :param parent: parent of this NemoLoop in the PSyclone AST.
    :type parent: :py:class:`psyclone.psyGen.Node`
    '''
    def __init__(self, parent=None, variable_name='', preinit=False):
        valid_loop_types = Config.get().api_conf("nemo").get_valid_loop_types()
        Loop.__init__(self, parent=parent,
                      variable_name=variable_name,
                      preinit=preinit,
                      valid_loop_types=valid_loop_types)

    @property
    def kernel(self):
        '''
        :returns: the kernel object if one is associated with this loop, \
                  None otherwise.
        :rtype: :py:class:`psyclone.nemo.NemoKern` or None

        :raises NotImplementedError: if the loop contains >1 kernel.
        '''
        kernels = self.walk(self.children, NemoKern)
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
