# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2018, Science and Technology Facilities Council.
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
# Authors R. W. Ford and A. R. Porter, STFC Daresbury Lab

'''This module implements the PSyclone NEMO API by specialising
    the required base classes for both code generation (PSy, Invokes,
    Invoke, Schedule, Loop, Kern, Arguments and KernelArgument)
    and parsing (Descriptor and KernelType).

'''

from __future__ import print_function, absolute_import
import copy
from psyclone.psyGen import PSy, Invokes, Invoke, Schedule, Node, \
    Loop, Kern, GenerationError, InternalError, colored, IfBlock, IfClause, \
    NameSpaceFactory, Fparser2ASTProcessor, SCHEDULE_COLOUR_MAP as _BASE_CMAP
from fparser.two.utils import walk_ast, get_child
from fparser.two import Fortran2003

# The base colour map doesn't have CodeBlock as that is currently
# a NEMO-API-specific entity.
NEMO_SCHEDULE_COLOUR_MAP = copy.deepcopy(_BASE_CMAP)
NEMO_SCHEDULE_COLOUR_MAP["CodeBlock"] = "red"

# The valid types of loop and associated loop variable and bounds
VALID_LOOP_TYPES = {"lon": {"var": "ji", "start": "1", "stop": "jpi"},
                    "lat": {"var": "jj", "start": "1", "stop": "jpj"},
                    "levels": {"var": "jk", "start": "1", "stop": "jpk"},
                    # TODO what is the upper bound of tracer loops?
                    "tracers": {"var": "jt", "start": "1", "stop": ""},
                    "unknown": {"var": "", "start": "1", "stop": ""}}

# Mapping from loop variable to loop type. This is how we identify each
# explicit do loop we encounter.
NEMO_LOOP_TYPE_MAPPING = {"ji": "lon", "jj": "lat", "jk": "levels",
                          "jt": "tracers", "jn": "tracers"}

# Mapping from loop type to array index. NEMO uses an "i, j, k" data
# layout.
NEMO_INDEX_ORDERING = ["lon", "lat", "levels", "tracers"]


class NemoFparser2ASTProcessor(Fparser2ASTProcessor):
    '''
    Specialisation of Fparser2ASTProcessor for the Nemo API. It is used
    as a Mixin in the Nemo API.
    '''
    def __init__(self):
        super(NemoFparser2ASTProcessor, self).__init__()

    def _create_child(self, child, parent=None):
        '''
        Adds Nemo API specific processors for certain fparser2 types
        before calling the parent _create_child.

        :param child: node in fparser2 AST.
        :type child:  :py:class:`fparser.two.utils.Base`
        :param parent: Parent node in the PSyclone IR we are constructing.
        :type parent: :py:class:`psyclone.psyGen.Node`
        :return: Returns the PSyIRe representation of child.
        :rtype: :py:class:`psyclone.psyGen.Node`
        '''
        if isinstance(child, Fortran2003.Block_Nonlabel_Do_Construct):
            return NemoLoop(child, parent=parent)
        elif isinstance(child, Fortran2003.Nonlabel_Do_Stmt):
            pass
        elif NemoImplicitLoop.match(child):
            return NemoImplicitLoop(child, parent=parent)
        elif NemoIfBlock.match(child):
            return NemoIfBlock(child, parent=parent)
        else:
            return super(NemoFparser2ASTProcessor, self)._create_child(child)


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
        self._schedule = NemoSchedule(self, exe_part)

    def update(self):
        '''Updates the fparser2 AST associated with this Schedule to make it
        reflect any transformations that have been applied to the
        PSyclone AST.

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


class NemoSchedule(Schedule, NemoFparser2ASTProcessor):
    '''
    The NEMO-specific schedule class. This is the top-level node in
    PSyclone's IR of a NEMO program unit (program, subroutine etc).

    :param invoke: The Invoke to which this Schedule belongs.
    :type invoke: :py:class:`psyclone.nemo.NemoInvoke`
    :param ast: the fparser2 AST of the NEMO code for which to generate \
                a Schedule.
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
        Print a representation of this NemoSchedule to stdout.

        :param int indent: level to which to indent output.
        '''
        print(self.indent(indent) + self.coloured_text + "[]")
        for entity in self._children:
            entity.view(indent=indent + 1)

    def __str__(self):
        ''' Returns the string representation of this NemoSchedule. '''
        result = "NemoSchedule():\n"
        for entity in self._children:
            result += str(entity)+"\n"
        result += "End Schedule"
        return result


class NemoKern(Kern):
    ''' Stores information about NEMO kernels as extracted from the
    NEMO code. Kernels are leaves in the Schedule AST (i.e. they have
    no children).

    :param loop: Reference to the loop (in the fparser2 AST) containing \
                 this kernel
    :type loop: :py:class:`fparser.two.Fortran2003.Block_Nonlabel_Do_Construct`
    :param parent: the parent of this Kernel node in the PSyclone AST
    type parent: :py:class:`psyclone.nemo.NemoLoop`
    '''
    def __init__(self, loop=None, parent=None):
        ''' Create an empty NemoKern object. The object is given state via
        a subsequent call to the load method if loop is None. '''
        # Create those member variables required for testing and to keep
        # pylint happy
        self._children = []
        self._name = ""
        # The Loop object created by fparser2 which holds the AST for the
        # section of code associated with this kernel
        self._loop = None
        # List of the loop variables, one for each loop
        self._loop_vars = []
        # A list of 2-tuples, one for each loop
        self._loop_ranges = []
        # List of variable names that must be thread-private
        self._private_vars = None
        # List of variable names that must be first-private because they
        # are scalars with a first access of read
        self._first_private_vars = None
        # Whether or not this kernel performs a reduction
        self._reduction = False
        # List of variables that are shared between threads
        self._shared_vars = None
        # Type of kernel (2D, 3D..)
        self._kernel_type = ""
        self._body = []
        # Will point to the corresponding set of nodes in the fparser2 AST
        self._ast = []
        if loop:
            self.load(loop)

    @staticmethod
    def match(node):
        '''
        Whether or not the AST fragment pointed to by node represents a
        kernel. A kernel is defined as a section of code that sits
        within a recognised loop structure and does not itself contain
        loops or IO operations.

        :param node: Node in fparser2 AST to check.
        :type node: :py:class:`fparser.two.Fortran2003.Base`
        :returns: True if this node conforms to the rules for a kernel.
        :rtype: bool
        '''
        from fparser.two.Fortran2003 import Subscript_Triplet,  \
            Block_Nonlabel_Do_Construct, Write_Stmt, Read_Stmt
        child_loops = walk_ast(node.content,
                               [Block_Nonlabel_Do_Construct, Write_Stmt,
                                Read_Stmt])
        if child_loops:
            # A kernel cannot contain other loops or reads or writes
            return False

        # Currently a kernel cannot contain implicit loops.
        # TODO we may have to differentiate between implicit loops over
        # grid points and any other implicit loop. Possibly using the
        # scope of the array being accessed?
        impl_loops = walk_ast(node.content, [Subscript_Triplet])
        if impl_loops:
            return False

        return True

    @property
    def ktype(self):
        '''
        :returns: what type of kernel this is.
        :rtype: str
        '''
        return self._kernel_type

    def load(self, loop):
        ''' Populate the state of this NemoKern object.

        :param loop: node in the fparser2 AST representing a loop (explicit \
                     or implicit).
        :type loop: :py:class:`fparser.two.Fortran2003.Assignment_Stmt` or \
                :py:class:`fparser.two.Fortran2003.Block_Nonlabel_Do_Construct`

        :raises InternalError: if the supplied loop node is not recognised.
        '''
        from fparser.two.Fortran2003 import Block_Nonlabel_Do_Construct, \
            Assignment_Stmt

        if isinstance(loop, Block_Nonlabel_Do_Construct):
            self._load_from_loop(loop)
        elif isinstance(loop, Assignment_Stmt):
            self._load_from_implicit_loop(loop)
        else:
            raise InternalError(
                "Expecting either Block_Nonlabel_Do_Construct or "
                "Assignment_Stmt but got {0}".format(str(type(loop))))

    def _load_from_loop(self, loop):
        '''
        Populate the state of this NemoKern object from an fparser2
        AST for an explicit loop.

        :param loop: Node in the fparser2 AST representing an implicit loop.
        :type loop: :py:class:`fparser.two.Fortran2003.Assignment_Stmt`

        :raises InternalError: if first child of supplied loop node is not a \
                           :py:class:`fparser.two.Fortran2003.Nonlabel_Do_Stmt`
        '''
        from fparser.two.Fortran2003 import Nonlabel_Do_Stmt, End_Do_Stmt

        # Keep a pointer to the original loop in the AST
        self._loop = loop

        if not isinstance(loop.content[0], Nonlabel_Do_Stmt):
            raise InternalError("Expecting Nonlabel_Do_Stmt as first child "
                                "of Block_Nonlabel_Do_Construct but "
                                "got {0}".format(type(loop.content[0])))
        self._body = []
        for content in loop.content[1:]:
            if isinstance(content, End_Do_Stmt):
                break
            self._body.append(content)

        # Kernel is "explicit" since we have a coded loop nest rather than
        # array notation
        self._kernel_type = "Explicit"

        # TODO decide how to provide this functionality. Do we use
        # Habakkuk or something else?
        #  Analyse the loop body to identify private and shared variables
        #  for use when parallelising with OpenMP.
        # from habakkuk.make_dag import dag_of_code_block
        #  Create a DAG of the kernel code block using Habakkuk
        # kernel_dag = dag_of_code_block(loop, "nemo_kernel")
        # inputs = kernel_dag.input_nodes()
        # outputs = kernel_dag.output_nodes()
        # print "Kernel has {0} outputs: ".format(len(outputs)) + \
        #     ",".join([node.variable.orig_name for node in outputs])
        self._shared_vars = set()
        self._first_private_vars = set()
        self._private_vars = set()
        #  If there are scalar variables that are inputs to the DAG (other than
        #  the loop counters) then they must be declared first-private in an
        #  OpenMP loop directive.
        # for node in inputs:
        #     if not node.node_type:
        #         if node.name not in NEMO_LOOP_TYPE_MAPPING:
        #             self._first_private_vars.add(node.name)
        # for key, node in kernel_dag._nodes.iteritems():
        #     if node.node_type == "array_ref":
        #         self._shared_vars.add(node.variable.orig_name)
        #     elif not node.node_type:
        #         self._private_vars.add(node.variable.orig_name)
        # self._private_vars -= self._first_private_vars
        # print "OpenMP shared vars: " + ",".join(self._shared_vars)
        # print "OpenMP private vars: " + ",".join(self._private_vars)
        # print "OpenMP first-private vars: " + \
        #     ",".join(self._first_private_vars)

    def _load_from_implicit_loop(self, loop):
        '''
        Populate the state of this NemoKern object from an fparser2
        AST for an implicit loop (Fortran array syntax).

        :param loop: Node in the fparser2 AST representing an implicit loop.
        :type loop: :py:class:`fparser.two.Fortran2003.Assignment_Stmt`
        '''
        # TODO implement this method!
        self._kernel_type = "Implicit"
        self._loop = loop

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
        print(self.indent(indent) + self.coloured_text + "[" +
              self.ktype + "]")


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
        Loop.__init__(self, parent=parent,
                      valid_loop_types=VALID_LOOP_TYPES)
        NemoFparser2ASTProcessor.__init__(self)
        # Keep a ptr to the corresponding node in the AST
        self._ast = ast

        # Get the loop variable
        ctrl = walk_ast(ast.content, [Loop_Control])
        # Second element of items member of Loop Control is itself a tuple
        # containing:
        #   Loop variable, [start value expression, end value expression, step
        #   expression]
        # Loop variable will be an instance of Fortran2003.Name
        loop_var = str(ctrl[0].items[1][0])
        self._variable_name = str(loop_var)

        # Identify the type of loop
        if self._variable_name in NEMO_LOOP_TYPE_MAPPING:
            self.loop_type = NEMO_LOOP_TYPE_MAPPING[self._variable_name]
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

        # Is this loop body a kernel?
        if NemoKern.match(self._ast):
            self.addchild(NemoKern(self._ast, parent=self))
            return
        # It's not - walk on down the AST...
        self.process_nodes(self, self._ast.content, self._ast)

    def __str__(self):
        result = ("NemoLoop[" + self._loop_type + "]: " + self._variable_name +
                  "=" + ",".join([self._start, self._stop, self._step]) + "\n")
        for entity in self._children:
            result += str(entity) + "\n"
        result += "EndLoop"
        return result

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

    :raises NotImplementedError: if the array slice has explicit bounds.
    :raises GenerationError: if an array slice is not in dimensions 1-3 of \
                             the array.
    '''
    def __init__(self, ast, parent=None):
        from fparser.common.readfortran import FortranStringReader
        Loop.__init__(self, parent=parent,
                      valid_loop_types=VALID_LOOP_TYPES)
        # Keep a ptr to the corresponding node in the AST
        self._ast = ast

        # Get a reference to the name-space manager
        name_space_manager = NameSpaceFactory().create()

        # Find all uses of array syntax in the statement
        subsections = walk_ast(ast.items, [Fortran2003.Section_Subscript_List])
        # A Section_Subscript_List is a tuple with each item the
        # array-index expressions for the corresponding dimension of the array.
        for idx, item in enumerate(subsections[0].items):
            if isinstance(item, Fortran2003.Subscript_Triplet):
                # A Subscript_Triplet has a 3-tuple containing the expressions
                # for the start, end and increment of the slice. If any of
                # these are not None then we have an explicit range of some
                # sort and we do not yet support that.
                # TODO allow for implicit loops with specified bounds
                # (e.g. 2:jpjm1)
                if [part for part in item.items if part]:
                    raise NotImplementedError(
                        "Support for implicit loops with specified bounds is "
                        "not yet implemented: '{0}'".format(str(ast)))
                # If an array index is a Subscript_Triplet then it is a range
                # and thus we need to create an explicit loop for this
                # dimension.
                outermost_dim = idx

        if outermost_dim < 0 or outermost_dim > 2:
            raise GenerationError(
                "Array section in unsupported dimension ({0}) for code "
                "'{1}'".format(outermost_dim+1, str(ast)))

        self._step = 1
        # TODO ensure no name clash is possible with variables that
        # already exist in the NEMO source.
        self.loop_type = NEMO_INDEX_ORDERING[outermost_dim]
        self._start = VALID_LOOP_TYPES[self.loop_type]["start"]
        self._stop = VALID_LOOP_TYPES[self.loop_type]["stop"]
        var_name = "psy_" + VALID_LOOP_TYPES[self.loop_type]["var"]
        self._variable_name = name_space_manager.create_name(
            root_name=var_name, context="PSyVars", label=var_name)

        # TODO Since the fparser2 AST does not have parent
        # information (and no other way of getting to the root node), it is
        # currently not possible to insert a declaration in the correct
        # location.
        # For the moment, we can work around the fparser2 AST limitation
        # by using the fact that we *can* get hold of the PSyclone Invoke
        # object and that contains a reference to the root of the fparser2
        # AST...
        name = Fortran2003.Name(FortranStringReader(self._variable_name))
        prog_unit = self.root.invoke._ast
        spec = walk_ast(prog_unit.content, [Fortran2003.Specification_Part])
        if not spec:
            names = walk_ast(prog_unit.content, [Fortran2003.Name])
            # TODO create a Specification_Part
            raise InternalError("No specification part found for routine {0}!".
                                format(names[0]))
        # TODO check that this variable has not already been declared
        # Requires that we capture all variable declarations in the routine
        # in some sort of management class.
        decln = Fortran2003.Type_Declaration_Stmt(
            FortranStringReader("integer :: {0}".format(self._variable_name)))
        spec[0].content.append(decln)

        for subsec in subsections:
            # A tuple is immutable so work with a list
            indices = list(subsec.items)
            # Replace the colon with our new variable name
            indices[outermost_dim] = name
            # Replace the original tuple with a new one
            subsec.items = tuple(indices)

        # Create an explicit loop
        text = ("do {0}=1,{1},{2}\n"
                "  replace = me\n"
                "end do\n".format(self._variable_name, self._stop, self._step))
        loop = Fortran2003.Block_Nonlabel_Do_Construct(
            FortranStringReader(text))
        parent_index = ast._parent.content.index(ast)
        # Insert it in the fparser2 AST at the location of the implicit
        # loop
        ast._parent.content.insert(parent_index, loop)
        # Replace the content of the loop with the (modified) implicit
        # loop
        loop.content[1] = ast
        # Remove the implicit loop from its original parent in the AST
        ast._parent.content.remove(ast)
        # Update the parent of the AST
        ast._parent = loop
        # Update our own pointer into the AST to now point to the explicit
        # loop we've just created
        self._ast = loop

        if NemoImplicitLoop.match(ast):
            # We still have an implicit loop so recurse
            self.addchild(NemoImplicitLoop(ast, parent=self))
        else:
            # We must be left with a kernel
            self.addchild(NemoKern(ast, parent=self))

    @staticmethod
    def match(node):
        '''
        Checks whether the supplied node in the fparser2 AST represents
        an implicit loop (using Fortran array syntax).
        :param node: node in the fparser2 AST to check
        :type node: :py:class:`fparser.two.Fortran2003.Assignment_Stmt`
        :returns: True if the node does represet an implicit loop.
        :rtype: bool
        '''
        if not isinstance(node, Fortran2003.Assignment_Stmt):
            return False
        # We are expecting something like:
        #    array(:,:,jk) = some_expression
        # so we check the left-hand side...
        lhs = node.items[0]
        if isinstance(lhs, Fortran2003.Part_Ref):
            colons = walk_ast(lhs.items, [Fortran2003.Subscript_Triplet])
            if colons:
                return True
        return False


class NemoIfBlock(IfBlock, NemoFparser2ASTProcessor):
    '''
    Represents an if-block within a NEMO Schedule.
    Within the fparser2 AST, an if-block is represented as:
      If_Then_Stmt
      statement(s)
      Else_Stmt
      further statement(s)
      End_If_Stmt
    i.e. the statements contained inside the if-block are siblings
    of the control statements, not children of them.

    :param ast: reference to fparser2 AST representing if block.
    :type ast: :py:class:`fparser.two.Fortran2003.If_Construct`
    :param parent: parent node of this if block in the PSyIRe.
    :type parent: :py:class:`psyclone.psyGen.Node`

    :raises InternalError: if the fparser2 AST does not have the expected \
                           structure.
    '''
    def __init__(self, ast, parent=None):
        super(NemoIfBlock, self).__init__(parent=parent)
        NemoFparser2ASTProcessor.__init__(self)
        # Keep a ptr to the corresponding node in the AST
        self._ast = ast
        # Check that the fparser2 AST has the expected structure
        if not isinstance(ast.content[0], Fortran2003.If_Then_Stmt):
            raise InternalError("Failed to find opening if then statement: "
                                "{0}".format(str(ast)))
        if not isinstance(ast.content[-1], Fortran2003.End_If_Stmt):
            raise InternalError("Failed to find closing end if statement: "
                                "{0}".format(str(ast)))
        clause_indices = []
        for idx, child in enumerate(ast.content):
            child._parent = self._ast  # Retrofit parent info
            if isinstance(child, (Fortran2003.If_Then_Stmt,
                                  Fortran2003.Else_Stmt,
                                  Fortran2003.Else_If_Stmt,
                                  Fortran2003.End_If_Stmt)):
                clause_indices.append(idx)
        # Create the body of the main If
        end_idx = clause_indices[1]
        self._condition = str(ast.content[0].items[0])
        self.process_nodes(parent=self,
                           nodes=ast.content[1:end_idx],
                           nodes_parent=ast)
        # Now deal with any other clauses (i.e. "else if" or "else")
        # An If block has one fewer clauses than it has control statements
        # (c.f. panels and posts):
        num_clauses = len(clause_indices) - 1
        for idx in range(1, num_clauses):
            start_idx = clause_indices[idx]
            # No need to subtract 1 here as Python's slice notation means
            # that the end_idx'th element is excluded
            end_idx = clause_indices[idx+1]
            ast.content[start_idx]._parent = ast  # Retrofit parent info
            self.addchild(NemoIfClause(ast.content[start_idx:end_idx],
                                       parent=self))

    def gen_code(self):
        '''
        Override abstract method of base class.
        :raises InternalError: because is not relevant to this API.
        '''
        # If we get here it's an error as the NEMO API does not generate
        # code (we manipulate existing code instead).
        raise InternalError("this method should not have been called!")

    @staticmethod
    def match(node):
        '''
        Checks whether the supplied fparser2 AST represents an if-block
        that must be represented in the Schedule AST. If-blocks that do
        not contain kernels are just treated as code blocks.

        :param node: the node in the fparser2 AST representing an if-block
        :type node: :py:class:`fparser.two.Fortran2003.If_Construct`
        :returns: True if this if-block must be represented in the PSyIRe
        :rtype: bool

        '''
        if not isinstance(node, Fortran2003.If_Construct):
            return False

        # We only care about if-blocks if they contain something significant
        # i.e. a recognised type of loop (whether implicit or explicit).
        loops = walk_ast(node.content,
                         [Fortran2003.Subscript_Triplet,
                          Fortran2003.Block_Nonlabel_Do_Construct])
        if loops:
            return True
        return False


class NemoIfClause(IfClause, NemoFparser2ASTProcessor):
    '''
    Represents a sub-clause of an if-block (else-if or else).

    :param list ast_nodes: List of nodes making up the clause in the fparser2 \
                           AST. First node is the else/else-if statement \
                           itself.
    :param parent: Parent of this clause in the PSyIRe (must be an IfBlock).
    :type parent: :py:class:`psyclone.nemo.NemoIfBlock`

    :raises InternalError: if fparser2 AST doesn't have the expected structure.
    '''
    def __init__(self, ast_nodes, parent=None):
        super(NemoIfClause, self).__init__(parent=parent)
        NemoFparser2ASTProcessor.__init__(self)
        # Keep a ptr to the corresponding node in the AST
        self._ast = ast_nodes[0]
        # Store what type of clause we are
        if isinstance(ast_nodes[0], Fortran2003.Else_Stmt):
            self._clause_type = "Else"
        elif isinstance(ast_nodes[0], Fortran2003.Else_If_Stmt):
            self._clause_type = "Else If"
        else:
            raise InternalError(
                "Unrecognised member of if block: '{0}'. Expected one of "
                "Else_Stmt or Else_If_Stmt.".format(type(ast_nodes[0])))
        # Continue on down the AST
        self.process_nodes(parent=self,
                           nodes=ast_nodes[1:],
                           nodes_parent=self._ast._parent)

    def gen_code(self):
        '''
        Override abstract method of base class.
        :raises InternalError: because is not relevant to this API.
        '''
        # If we get here it's an error as the NEMO API does not generate
        # code (we manipulate existing code instead).
        raise InternalError("This method should not have been called!")
