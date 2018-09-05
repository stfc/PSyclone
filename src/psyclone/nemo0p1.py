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

'''This module implements the PSyclone NEMO 0.1 API by specialising
    the required base classes for both code generation (PSy, Invokes,
    Invoke, Schedule, Loop, Kern, Arguments and KernelArgument)
    and parsing (Descriptor and KernelType).

'''

from __future__ import print_function, absolute_import
import copy
from psyclone.parse import ParseError
from psyclone.psyGen import PSy, Invokes, Invoke, Schedule, Node, \
    Loop, Kern, GenerationError, InternalError, colored, IfBlock, IfClause, \
    NameSpaceFactory, SCHEDULE_COLOUR_MAP as _BASE_CMAP
from fparser.two.Fortran2003 import walk_ast
from fparser.two import Fortran2003

# The base colour map doesn't have CodeBlock as that is currently
# a NEMO-API-specific entity.
NEMO_SCHEDULE_COLOUR_MAP = copy.deepcopy(_BASE_CMAP)
NEMO_SCHEDULE_COLOUR_MAP["CodeBlock"] = "red"

# The two scalar types we support
VALID_SCALAR_TYPES = ["i_scalar", "r_scalar"]

# The sets of grid points that a kernel may operate on
VALID_ITERATES_OVER = ["all_pts", "internal_pts", "external_pts"]

# Valid values for the type of access a kernel argument may have
VALID_ARG_ACCESSES = ["read", "write", "readwrite"]

# The loop index variables we expect NEMO to use
NEMO_LOOP_VARS = ["ji", "jj", "jk"]

# The valid types of loop.
VALID_LOOP_TYPES = ["lon", "lat", "levels", "tracers"]

# Mapping from loop variable to loop type
NEMO_LOOP_TYPE_MAPPING = {"ji": "lon", "jj": "lat", "jk": "levels",
                          "jt": "tracers", "jn": "tracers"}


class ASTProcessor(object):
    '''
    Mixin class to provide functionality for processing the fparser2 AST.
    '''
    @staticmethod
    def add_code_block(parent, statements):
        '''
        Create a NemoCodeBlock for the supplied list of statements
        and then wipe the list of statements.

        :param parent: Node in the PSyclone AST to which to add this code \
                       block.
        :type parent: :py:class:`psyclone.psyGen.Node`
        :param list statements: List of fparser2 AST nodes consituting the \
                                code block.
        :rtype: :py:class:`psyclone.nemo0p1.NemoCodeBlock`
        '''
        if not statements:
            return None

        code_block = NemoCodeBlock(statements,
                                   parent=parent)
        parent.addchild(code_block)
        del statements[:]
        return code_block

    # TODO remove nodes_parent argument once fparser2 AST contains
    # parent information (fparser/#102).
    def process_nodes(self, parent, nodes, nodes_parent):
        '''
        Create the PSyclone AST representation of the code represented
        by the supplied list of nodes in the fparser2 AST.
        '''
        code_block_nodes = []
        for child in nodes:
            child._parent = nodes_parent  # Retro-fit parent info
            if isinstance(child,
                          Fortran2003.Block_Nonlabel_Do_Construct):
                # The start of a loop is taken as the end of any
                # existing code block so we create that now
                self.add_code_block(parent, code_block_nodes)
                parent.addchild(NemoLoop(child, parent=parent))
            elif NemoImplicitLoop.match(child):
                # An implicit loop marks the end of any current
                # code block
                self.add_code_block(parent, code_block_nodes)
                parent.addchild(NemoImplicitLoop(child, parent=parent))
            elif NemoIfBlock.match(child):
                self.add_code_block(parent, code_block_nodes)
                parent.addchild(NemoIfBlock(child, parent=parent))
            elif not isinstance(child, (Fortran2003.End_Do_Stmt,
                                        Fortran2003.Nonlabel_Do_Stmt)):
                # Don't include the do or end-do in a code block
                code_block_nodes.append(child)

        # Complete any unfinished code-block
        self.add_code_block(parent, code_block_nodes)


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
        self._psy_unique_vars = ["a_variable"]
        # Store the whole fparser2 AST
        self._ast = ast
        self._name_space_manager = NameSpaceFactory().create()

        from habakkuk.parse2003 import get_child, ParseError as perror
        from fparser.two.Fortran2003 import Execution_Part, Specification_Part

        # Find the section of the tree containing the execution part
        # of the code
        try:
            exe_part = get_child(ast, Execution_Part)
        except perror:
            # This subroutine has no execution part so we skip it
            # TODO log this event
            return

        # Store the root of this routine's specification in the AST
        self._spec_part = get_child(ast, Specification_Part)

        # We now walk through the AST produced by fparser2 and construct a
        # new AST using objects from the nemo0p1 module.
        self._schedule = NemoSchedule(self, exe_part)

    def gen(self):
        return str(self._ast)

    @property
    def psy_unique_var_names(self):
        return self._psy_unique_vars

    def update(self):
        '''Updates the fparser2 AST associated with this Schedule to make it
        reflect any transformations that have been applied to the
        PSyclone AST.

        '''
        if not self._schedule:
            return
        self._schedule.update()


class NemoInvokes(Invokes):

    def __init__(self, ast):
        from habakkuk.parse2003 import get_child, ParseError as perror
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
        try:
            main_prog = get_child(ast, Main_Program)
            routines.append(main_prog)
        except perror:
            pass

        # Analyse each routine we've found
        for subroutine in routines:
            # Get the name of this (sub)routine
            substmt = walk_ast(subroutine.content,
                               [Subroutine_Stmt, Function_Stmt,
                                Program_Stmt])
            if isinstance(substmt[0], Function_Stmt):
                for item in substmt[0].items:
                    if isinstance(item, Name):
                        sub_name = str(item)
            else:
                sub_name = str(substmt[0].get_name())

            my_invoke = NemoInvoke(subroutine, name=sub_name)
            self.invoke_map[sub_name] = my_invoke
            self.invoke_list.append(my_invoke)

    def update(self):
        ''' TBD '''
        for invoke in self.invoke_list:
            invoke.update()


class NemoPSy(PSy):
    ''' The NEMO 0.1-specific PSy class. This creates a NEMO-specific
        invokes object (which controls all the required invocation calls).
        Also overrides the PSy gen method so that we generate GOcean-
        specific PSy module code. '''

    def __init__(self, ast):

        self._name = "NEMO-PSY"  # TODO use a meaningful name
        self._invokes = NemoInvokes(ast)
        self._ast = ast

    def inline(self, module):
        # Override base-class method because we don't yet support it
        pass

    @property
    def gen(self):
        '''
        Generate the (updated) fparser2 AST for the NEMO code represented
        by this NemoPSy object.

        :rtype: :py:class:`fparser.two.Fortran2003.Main_Program` or \
                :py:class:`fparser.two.Fortran2003.Subroutine_Subprogram` or \
                :py:class:`fparser.two.Fortran2003.Function_Subprogram`.
        '''
        # Walk down our Schedule and update the underlying fparser2 AST
        # to account for any transformations
        self.invokes.update()

        # Return the fparser2 AST
        return self._ast


class NemoSchedule(Schedule, ASTProcessor):
    '''
    The GOcean specific schedule class. We call the base class
    constructor and pass it factories to create GO-specific calls to both
    user-supplied kernels and built-ins.

    :param invoke:
    :param ast: the fparser2 AST of the NEMO code for which to generate \
                a Schedule.
    :type ast: :py:class:`fparser.two.Fortran2003.Main_Program` or \
               :py:class:`fparser.two.Fortran2003.Subroutine_Subprogram` or \
               :py:class:`fparser.two.Fortran2003.Function_Subprogram`.
    '''

    def __init__(self, invoke, ast=None):
        Node.__init__(self)

        self._invoke = invoke

        if not ast:
            # This Schedule will be populated by a subsequent call
            # to load()
            # TODO - implement load()
            return

        self._ast = ast

        # List of nodes we will use to create 'codeBlocks' that we don't
        # attempt to understand
        code_block_nodes = []

        for child in ast.content:
            # The fparser2 AST does not hold parent information so add
            # that now.
            child._parent = ast
            if isinstance(child, Fortran2003.Block_Nonlabel_Do_Construct):
                # The start of a loop is taken as the end of any exising
                # code block so we create that now
                self.add_code_block(self, code_block_nodes)
                self.addchild(NemoLoop(child, parent=self))
            elif NemoImplicitLoop.match(child):
                # Similarly, an implicit loop ends any existing code block
                self.add_code_block(self, code_block_nodes)
                self.addchild(NemoImplicitLoop(child, parent=self))
            elif NemoIfBlock.match(child):
                self.add_code_block(self, code_block_nodes)
                self.addchild(NemoIfBlock(child, parent=self))
            else:
                code_block_nodes.append(child)

        # Finish any open code block
        self.add_code_block(self, code_block_nodes)

        return


    def view(self, indent=0):
        ''' Print a representation of this NemoSchedule '''
        print(self.indent(indent) + self.coloured_text + "[]")
        for entity in self._children:
            entity.view(indent=indent + 1)

    def __str__(self):
        ''' Returns the string representation of this NemoSchedule '''
        result = "NemoSchedule():\n"
        for entity in self._children:
            result += str(entity)+"\n"
        result += "End Schedule"
        return result

    @property
    def iloop_stop(self):
        '''Returns the variable name to use for the upper bound of inner
        loops if we're generating loops with constant bounds. Raises
        an error if constant bounds are not being used.

        '''
        if self._const_loop_bounds:
            return "istop"
        else:
            raise GenerationError(
                "Refusing to supply name of inner loop upper bound "
                "because constant loop bounds are not being used.")

    @property
    def jloop_stop(self):
        '''Returns the variable name to use for the upper bound of outer
        loops if we're generating loops with constant bounds. Raises
        an error if constant bounds are not being used.

        '''
        if self._const_loop_bounds:
            return "jstop"
        else:
            raise GenerationError(
                "Refusing to supply name of outer loop upper bound "
                "because constant loop bounds are not being used.")

    @property
    def const_loop_bounds(self):
        ''' Returns True if constant loop bounds are enabled for this
        schedule. Returns False otherwise. '''
        return self._const_loop_bounds

    @const_loop_bounds.setter
    def const_loop_bounds(self, obj):
        ''' Set whether the Schedule will use constant loop bounds or
        will look them up from the field object for every loop '''
        self._const_loop_bounds = obj


class NemoCodeBlock(Node):
    ''' Node representing some generic Fortran code that PSyclone
    does not attempt to manipulate '''

    def __init__(self, statements, parent=None):
        Node.__init__(self, parent=parent)
        # Store a list of the parser objects holding the code
        # associated with this block
        self._statements = statements[:]

    @property
    def coloured_text(self):
        '''
        Return the name of this node type with control codes for
        terminal colouring
        :return: Name of node + control chars for colour
        :rtype: string
        '''
        return colored("NemoCodeBlock", NEMO_SCHEDULE_COLOUR_MAP["CodeBlock"])

    def view(self, indent=0):
        ''' Print a representation of this node in the schedule '''
        print(self.indent(indent) + self.coloured_text + "[" +
              str(type(self._statements[0])) + "]")
        for entity in self._children:
            entity.view(indent=indent + 1)

    def __str__(self):
        return "CodeBlock[{0} statements]".format(len(self._statements))


class NemoKern(Kern):
    ''' Stores information about NEMO kernels as extracted from the
    NEMO code. '''
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
            self.load(loop, parent)

    @staticmethod
    def match(node):
        '''
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
    def type(self):
        ''' Returns what type of kernel this is '''
        return self._kernel_type

    def load(self, loop, parent=None):
        ''' Populate the state of this NemoKern object.

        :param node:
        :param parent:
        '''
        from fparser.two.Fortran2003 import Block_Nonlabel_Do_Construct, \
            Assignment_Stmt

        if isinstance(loop, Block_Nonlabel_Do_Construct):
            self._load_from_loop(loop, parent=parent)
        elif isinstance(loop, Assignment_Stmt):
            self._load_from_implicit_loop(loop, parent=parent)
        else:
            raise ParseError(
                "Internal error: expecting either "
                "Block_Nonlabel_Do_Construct or Assignment_Stmt but got "
                "{0}".format(str(type(loop))))

    def _load_from_loop(self, loop, parent=None):
        ''' Populate the state of this NemoKern object from an fparser2
        AST for a loop nest '''
        from fparser.two.Fortran2003 import Nonlabel_Do_Stmt, End_Do_Stmt

        # Keep a pointer to the original loop in the AST
        self._loop = loop

        if not isinstance(loop.content[0], Nonlabel_Do_Stmt):
            raise ParseError("Internal error, expecting Nonlabel_Do_Stmt as "
                             "first child of Block_Nonlabel_Do_Construct but "
                             "got {0}".format(type(loop.content[0])))
        self._body = []
        for content in loop.content[1:]:
            if isinstance(content, End_Do_Stmt):
                break
            self._body.append(content)

        # I could get this by walking back up the tree and counting how
        # many NemoLoops I have as ancestors before I come across
        # something that is not a loop...
        #if len(self._loop_vars) == 2:
        #    self._kernel_type = "2D"
        #else:
        #    self._kernel_type = "3D"

        # TODO bring habakkuk up-to-date with changes to fparser and then
        # uncomment this code.

        # Analyse the loop body to identify private and shared variables
        #from habakkuk.make_dag import dag_of_code_block
        # Create a DAG of the kernel code block using Habakkuk
        #kernel_dag = dag_of_code_block(loop, "nemo_kernel")
        #inputs = kernel_dag.input_nodes()
        #outputs = kernel_dag.output_nodes()
        #print "Kernel has {0} outputs: ".format(len(outputs)) + \
        #    ",".join([node.variable.orig_name for node in outputs])
        self._shared_vars = set()
        self._first_private_vars = set()
        self._private_vars = set()
        # If there are scalar variables that are inputs to the DAG (other than
        # the loop counters) then they must be declared first-private.
        #for node in inputs:
        #    if not node.node_type:
        #        if node.name not in NEMO_LOOP_VARS:
        #            self._first_private_vars.add(node.name)
        #for key, node in kernel_dag._nodes.iteritems():
        #    if node.node_type == "array_ref":
        #        self._shared_vars.add(node.variable.orig_name)
        #    elif not node.node_type:
        #        self._private_vars.add(node.variable.orig_name)
        #self._private_vars -= self._first_private_vars
        #print "OpenMP shared vars: " + ",".join(self._shared_vars)
        #print "OpenMP private vars: " + ",".join(self._private_vars)
        #print "OpenMP first-private vars: " + \
        #    ",".join(self._first_private_vars)
        return

    def _load_from_implicit_loop(self, loop, parent=None):
        ''' Populate the state of this NemoKern object from an fparser2
        AST for an implicit loop (Fortran array syntax) '''
        # TODO implement this method!
        self._kernel_type = "Implicit"
        return

    @property
    def loop(self):
        ''' Returns the Fortran2003 loop object associated with this kernel '''
        return self._loop

    def tofortran(self, tab='', isfix=False):
        ''' Returns a string containing the Fortran representation of this
        kernel '''
        fort_lines = []
        tablen = len(tab)
        for idx, loop_var in enumerate(self._loop_vars):
            fort_lines.append(tablen*" "+"DO {0} = {1}, {2}".
                              format(loop_var,
                                     self._loop_ranges[idx][0],
                                     self._loop_ranges[idx][1]))
            tablen += 2
        for item in self._body:
            fort_lines.append(item.tofortran(tab=tablen*" ", isfix=isfix))
        for loop_var in self._loop_vars:
            tablen -= 2
            fort_lines.append(tablen*" "+"END DO")
        return "\n".join(fort_lines)

    def local_vars(self):
        '''Return a list of the variable (names) that are local to this loop
        (and must therefore be e.g. threadprivate if doing OpenMP)

        '''
        return []

    def view(self, indent=0):
        ''' Print representation of this node to stdout '''
        print(self.indent(indent) + self.coloured_text + "[" +
              self._kernel_type + "]")
        for entity in self._children:
            entity.view(indent=indent + 1)


class NemoLoop(Loop, ASTProcessor):
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
                      valid_loop_types=VALID_LOOP_TYPES+["unknown"])
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
        ''' Returns the kernel object if one is associated with this loop,
        None otherwise. '''
        kernels = self.walk(self.children, NemoKern)
        if kernels:
            # TODO cope with case where loop contains >1 kernel (e.g.
            # following loop fusion)
            return kernels[0]
        return None


class NemoImplicitLoop(NemoLoop):
    '''
    Class representing an implicit loop in NEMO (i.e. using Fortran array
    syntax).

    :param ast: the part of the fparser2 AST representing the loop
    :type ast: :py:class:`fparser.two.Fortran2003.Assignment_Stmt`
    :param parent: the parent of this Loop in the PSyclone AST
    '''
    def __init__(self, ast, parent=None):
        from fparser.common.readfortran import FortranStringReader
        Loop.__init__(self, parent=parent,
                      valid_loop_types=VALID_LOOP_TYPES+["unknown"])
        # Keep a ptr to the corresponding node in the AST
        self._ast = ast

        # Get a reference to the name-space manager
        name_space_manager = NameSpaceFactory().create()

        # Find all uses of array syntax in the statement
        subsections = Fortran2003.walk_ast(
            ast.items, [Fortran2003.Section_Subscript_List])
        # A Section_Subscript_List is a tuple with each item the
        # array-index expressions for the corresponding dimension of the array.
        for idx, item in enumerate(subsections[0].items):
            if isinstance(item, Fortran2003.Subscript_Triplet):
                # If an array index is a Subscript_Triplet then it is a range
                # and thus we need an explicit loop for this dimension.
                outermost_dim = idx

        # TODO allow for implicit loops with specified bounds (e.g. 2:jpjm1)
        self._start = 1
        self._step = 1
        if outermost_dim == 0:
            # TODO ensure no name clash is possible with variables that
            # already exist in the NEMO source.
            self._variable_name = name_space_manager.create_name(
                root_name="psy_ji", context="PSyVars", label="psy_ji")
            self.loop_type = "lon"
            self._stop = "jpi"
        elif outermost_dim == 1:
            self._variable_name = name_space_manager.create_name(
                root_name="psy_jj", context="PSyVars", label="psy_jj")
            self.loop_type = "lat"
            self._stop = "jpj"
        elif outermost_dim == 2:
            self._variable_name = name_space_manager.create_name(
                root_name="psy_jk", context="PSyVars", label="psy_jk")
            self.loop_type = "levels"
            self._stop = "jpk"
        else:
            raise GenerationError("Array section in unsupported dimension "
                                  "({0}) for code {1}".format(outermost_dim,
                                                              str(ast)))
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
            raise InternalError("No specifcation part found for routine {0}!".
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
        text = '''\
do {0}=1,{1},{2}
  replace = me
end do
'''.format(self._variable_name, self._stop, self._step)
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


class NemoIfBlock(IfBlock, ASTProcessor):
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
    '''
    def __init__(self, ast, parent=None):
        super(NemoIfBlock, self).__init__(parent=parent)
        # Keep a ptr to the corresponding node in the AST
        self._ast = ast
        # TODO convert these into proper exceptions
        assert isinstance(ast.content[0], Fortran2003.If_Then_Stmt)
        assert isinstance(ast.content[-1], Fortran2003.End_If_Stmt)
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

    @staticmethod
    def match(node):
        '''
        Checks whether the supplied fparser2 AST represents an if-block
        that must be represented in the Schedule AST. If-blocks that do
        not contain kernels are just treated as code blocks.
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


class NemoIfClause(IfClause, ASTProcessor):
    '''
    Represents a sub-clause of an if-block (else-if or else).

    :param list ast_nodes: List of nodes making up the clause. First node \
                           is the else/else-if statement itself.
    :param parent: Parent of this clause in the AST (must be an IfBlock).
    :type parent: :py:class:`psyclone.nemo0p1.NemoIfBlock`
    '''
    def __init__(self, ast_nodes, parent=None):
        super(NemoIfClause, self).__init__(parent=parent)
        # Keep a ptr to the corresponding node in the AST
        self._ast = ast_nodes[0]
        # Store what type of clause we are
        if isinstance(ast_nodes[0], Fortran2003.Else_Stmt):
            self._clause_type = "Else"
        elif isinstance(ast_nodes[0], Fortran2003.Else_If_Stmt):
            self._clause_type = "Else If"
        else:
            raise InternalError("Unrecognised member of if block: {0}".
                                format(type(ast_nodes[0])))
        # Continue on down the AST
        self.process_nodes(parent=self,
                           nodes=ast_nodes[1:],
                           nodes_parent=self._ast._parent)
