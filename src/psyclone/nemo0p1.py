# ----------------------------------------------------------------------------
# (c) Copyright Science and Technology Facilities Council, 2017
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
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
# Authors R. Ford and A. R. Porter, STFC Daresbury Lab

'''This module implements the PSyclone NEMO 0.1 API by specialising
    the required base classes for both code generation (PSy, Invokes,
    Invoke, Schedule, Loop, Kern, Arguments and KernelArgument)
    and parsing (Descriptor and KernelType).

'''

from __future__ import print_function
from psyclone.parse import Descriptor, KernelType, ParseError
from psyclone.psyGen import PSy, Invokes, Invoke, Schedule, Node, \
    Loop, Kern, Arguments, KernelArgument, GenerationError, colored, \
    SCHEDULE_COLOUR_MAP as _BASE_CMAP
from fparser.two.Fortran2003 import walk_ast

# The base colour map doesn't have CodeBlock as that is currently
# a NEMO-API-specific entity.
import copy
NEMO_SCHEDULE_COLOUR_MAP = copy.deepcopy(_BASE_CMAP)
NEMO_SCHEDULE_COLOUR_MAP["CodeBlock"] = "red"

# The different grid-point types that a field can live on
VALID_FIELD_GRID_TYPES = ["cu", "cv", "ct", "cf", "every"]

# The two scalar types we support
VALID_SCALAR_TYPES = ["i_scalar", "r_scalar"]

# Index-offset schemes (for the Arakawa C-grid)
VALID_OFFSET_NAMES = ["offset_se", "offset_sw",
                      "offset_ne", "offset_nw", "offset_any"]

# The offset schemes for which we can currently generate constant
# loop bounds in the PSy layer
SUPPORTED_OFFSETS = ["offset_ne", "offset_sw", "offset_any"]

# The sets of grid points that a kernel may operate on
VALID_ITERATES_OVER = ["all_pts", "internal_pts", "external_pts"]

# Valid values for the type of access a kernel argument may have
VALID_ARG_ACCESSES = ["read", "write", "readwrite"]

# The list of valid stencil properties. We currently only support
# pointwise. This property could probably be removed from the
# GOcean API altogether.
VALID_STENCILS = ["pointwise"]

# A dictionary giving the mapping from meta-data names for
# properties of the grid to their names in the Fortran grid_type.
GRID_PROPERTY_DICT = {"grid_area_t": "area_t",
                      "grid_area_u": "area_u",
                      "grid_area_v": "area_v",
                      "grid_mask_t": "tmask",
                      "grid_dx_t": "dx_t",
                      "grid_dx_u": "dx_u",
                      "grid_dx_v": "dx_v",
                      "grid_dy_t": "dy_t",
                      "grid_dy_u": "dy_u",
                      "grid_dy_v": "dy_v",
                      "grid_lat_u": "gphiu",
                      "grid_lat_v": "gphiv",
                      "grid_dx_const": "dx",
                      "grid_dy_const": "dy"}

# The loop index variables we expect NEMO to use
NEMO_LOOP_VARS = ["ji", "jj", "jk"]

# The valid types of loop.
VALID_LOOP_TYPES = ["lon", "lat", "levels", "tracers"]

# Mapping from loop variable to loop type
NEMO_LOOP_TYPE_MAPPING = {"ji": "lon", "jj": "lat", "jk": "levels",
                          "jt": "tracers", "jn": "tracers"}


def renamed_translate_ast(node, parent, indent=0, debug=False):
    '''' Walk down the tree produced by the f2003 parser where children
    are listed under 'content'.  Replace any loop nests that we've
    identified as kernels with the corresponding Kernel object. '''
    import fparser
    from fparser.two import Fortran2003
    from habakkuk.parse2003 import walk_ast
    cblock_list = []
    # If a node is an instance of Fortran2003.BlockBase then it has children
    # listed in .content. Otherwise, it has children listed under
    # .items. If a node has neither then it has no children.
    if hasattr(parent, "content"):
        children = parent.content
    elif hasattr(parent, "items"):
        children = parent.items
    else:
        return

    code_block_statements = []

    for idx, child in enumerate(children[:]):
        child._parent = parent # ARPDBG - retro-fix parent
        if debug:
            print(indent*"  " + "child type = ", type(child))
            
        if type(child) in [Fortran2003.Block_Nonlabel_Do_Construct]:

            # The start of a loop is taken as the end of any exising
            # code block so we create that now
            _add_code_block(node, code_block_statements)

            ctrl = walk_ast(child.content, [Fortran2003.Loop_Control])
            # items member of Loop Control contains:
            #   Loop variable, start value expression, end value expression
            # Loop variable will be an instance of Fortran2003.Name
            loop_var = str(ctrl[0].items[0])

            nested_loops = walk_ast(child.content,
                                   [Fortran2003.Block_Nonlabel_Do_Construct])
            is_kern = True
            io_statements = []
            if not nested_loops:
                # A Kernel must be a loop nest
                is_kern = False
            else:
                # Check the content of the innermost loop
                io_statements = walk_ast(nested_loops[-1].content,
                                         [Fortran2003.Write_Stmt,
                                          Fortran2003.Read_Stmt])
                if io_statements:
                    # A kernel cannot contain IO statements
                    is_kern = False

            if not nested_loops and not io_statements:
                # Does this loop itself contain an implicit loop?
                # TODO implement this!
                pass
                #walk_ast(, [Fortran2003.Section_Subscript_List])

            # TODO check for perfect nesting (i.e. no statements between
            # the nested DO's or END DO's)
            if is_kern and ((loop_var == "jk" and len(nested_loops) == 2) or
                            (loop_var == "jj" and len(nested_loops) == 1)):
                depth = 0
                ploop = NemoLoop(parent=node, loop_ast=child, #ctrl[depth],
                                 loop_var=loop_var, contains_kern=True)
                node.addchild(ploop)
                for loop in nested_loops:
                    depth += 1
                    loop_var = str(ctrl[depth].items[0])
                    nloop = NemoLoop(parent=ploop,
                                     loop_ast=loop, #ctrl[depth],
                                     loop_var=loop_var)
                    ploop.addchild(nloop)
                    ploop = nloop

                kern = NemoKern()
                kern.load(child, parent=ploop)
                ploop.addchild(kern)
            else:
                loop = NemoLoop(parent=node, loop_ast=child,
                                loop_var=loop_var)
                node.addchild(loop)
                translate_ast(loop, child, indent+1, debug)
        elif isinstance(child, Fortran2003.BlockBase):
            code_block_statements.append(child)
            loops = walk_ast(child.content,
                             [Fortran2003.Block_Nonlabel_Do_Construct])
            if loops:
                # It contains one or more loops so we walk further down
                # the tree
                block = _add_code_block(node, code_block_statements)
                if block:
                    translate_ast(block, child, indent+1, debug)
                else:
                    translate_ast(node, child, indent+1, debug)
        else:
            # Check whether this is an implicit loop
            if is_implicit_loop(child):
                # An implicit loop marks the end of any current
                # code block
                _add_code_block(node, code_block_statements)

                # Create a kernel for this implicit loop
                kern = NemoKern()
                kern.load(child, parent=node)
                node.addchild(kern)
            else:
                # Add this node in the AST to our list for the current
                # code block (unless it is loop-related in which case we
                # ignore it)
                if type(child) not in [fparser.two.Fortran2003.Nonlabel_Do_Stmt,
                                       fparser.two.Fortran2003.End_Do_Stmt]:
                    code_block_statements.append(child)

    # Finish any open code block
    _add_code_block(node, code_block_statements)

    return


# TODO this routine belongs with the higher-level wrapper around
# fparser2
def is_implicit_loop(node):
    '''
    Checks whether the supplied node uses Fortran array syntax and is
    thus an implicit loop

    :param node: the node in the fparser2 AST to check for array syntax
    :type node: :py:class:`fparser.Fortran2003.Assignment_Stmt`
    :return: True if the statement contains an implicit loop, False otherwise
    '''
    from fparser.two import Fortran2003
    from habakkuk.parse2003 import walk_ast

    if not isinstance(node, Fortran2003.Assignment_Stmt):
        return False

    for child in node.items:
        if isinstance(child, Fortran2003.Part_Ref):
            colons = walk_ast(child.items, [Fortran2003.Subscript_Triplet])
            if colons:
                return True
    return False


def get_routine_type(ast):
    '''
    Identify the type for Fortran routine the ast represents

    :param ast: the fparser2 AST representing the Fortran code
    :type ast: `fparser.Fortran2003.Program_Unit`
    :return: the type of Fortran routine (program, subroutine, function)
    :rtype: string
    '''
    from fparser.two import Fortran2003
    from habakkuk.parse2003 import walk_ast
    for child in ast.content:
        if isinstance(child, Fortran2003.Program_Stmt):
            return "program"
        #elif isinstance(child, fparser.Fortran2003.)
    raise ParseError("Unrecognised Fortran unit: {0}", str(ast))


def _add_code_block(parent, statements):
    ''' Create a NemoCodeBlock for the supplied list of statements
    and then wipe the list of statements '''
    from nemo0p1 import NemoCodeBlock

    if not statements:
        return None
    
    code_block = NemoCodeBlock(statements,
                               parent=parent)
    parent.addchild(code_block)
    del statements[:]
    return code_block


class NemoInvoke(Invoke):

    def __init__(self, ast, name):
        '''
        :param ast: The fparser2 AST for the Fortran code to process
        :type ast: :py:class:`fparser.Fortran2003.Main_Program` or Module
        :param str name: The name of the program unit
        '''
        self._schedule = None
        self._name = name
        self._psy_unique_vars = ["a_variable"]
        # Store the whole fparser2 AST
        self._ast = ast

        from habakkuk.parse2003 import walk_ast, Loop, get_child, ParseError
        from fparser.two.Fortran2003 import Main_Program, Program_Stmt, \
            Subroutine_Subprogram, Function_Subprogram, Function_Stmt, \
            Subroutine_Stmt, Block_Nonlabel_Do_Construct, Execution_Part, \
            Name, Specification_Part

        # Find the section of the tree containing the execution part
        # of the code
        try:
            exe_part = get_child(ast, Execution_Part)
        except ParseError:
            # This subroutine has no execution part so we skip it
            # TODO log this event
            return

        # Identify whether we have a Program, a Subroutine or a Function
        self._routine_type = get_routine_type(ast)

        # Store the root of this routine's specification in the AST
        self._spec_part = get_child(ast, Specification_Part)

        # We now walk through the AST produced by fparser2 and construct a
        # new AST using objects from the nemo0p1 module.
        self._schedule = NemoSchedule(self, exe_part)
        #translate_ast(self._schedule, exe_part, debug=True)

    def gen(self):
        return str(self._ast)

    def gen_code(self, parent):
        '''
        Generates the f2pygen AST for this invoke
        :param parent: Parent of this node in the AST we are creating
        :type parent: :py:class:`psyclone.f2pygen.ModuleGen`
        '''
        from psyclone.f2pygen2 import SubroutineGen, DeclGen, TypeDeclGen, \
            CommentGen, AssignGen, ProgramGen

        if not self._schedule:
            return
        
        # create the parent routine
        if self._routine_type == "subroutine":
            top_node = SubroutineGen(parent, name=self.name,
                                     args=self.psy_unique_var_names)
        elif self._routine_type == "program":
            top_node = ProgramGen(parent, self.name, implicitnone=False)

        parent.add(top_node)
        self.schedule.gen_code(top_node)

    @property
    def psy_unique_var_names(self):
        return self._psy_unique_vars

    def update(self):
        ''' TBD '''
        if not self._schedule:
            return
        self._schedule.update()


class NemoInvokes(Invokes):

    def __init__(self, ast):
        from habakkuk.parse2003 import walk_ast, get_child, ParseError
        from fparser.two.Fortran2003 import Main_Program, Program_Stmt, \
            Subroutine_Subprogram, Function_Subprogram, Function_Stmt, \
            Subroutine_Stmt
        
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
        except ParseError:
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

    def gen_code(self):
        return self._ast

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
        Update the PSy code for the NEMO api v.0.1.

        :rtype: ast

        '''
        # Walk down our Schedule and update the underlying fparser2 AST
        # to account for any transformations
        self.invokes.update()
        
        # Return the fparser2 AST
        return self._ast


class NemoSchedule(Schedule):
    ''' The GOcean specific schedule class. We call the base class
    constructor and pass it factories to create GO-specific calls to both
    user-supplied kernels and built-ins. '''

    def __init__(self, invoke, ast=None):
        '''
        '''
        from fparser.two import Fortran2003

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
            if isinstance(child, Fortran2003.Block_Nonlabel_Do_Construct):
                # The start of a loop is taken as the end of any exising
                # code block so we create that now
                _add_code_block(self, code_block_nodes)
                self.addchild(NemoLoop(child, parent=self))
            else:
                code_block_nodes.append(child)

        # Finish any open code block
        _add_code_block(self, code_block_nodes)

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

    def gen_code(self, parent):
        ''' Generates the Fortran for this Schedule '''
        for entity in self._children:
            entity.gen_code(parent)

    def update(self):
        ''' TBD '''
        for child in self._children:
            child.update()


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

    def gen_code(self, parent):
        ''' Convert this code block back to Fortran '''
        for statement in self._statements:
            # TODO each statement is an item from the fparser2 AST but
            # parent.add expects an f2pygen object.
            parent.add(statement)
        for entity in self._children:
            entity.gen_code(parent)

    def update(self):
        ''' TBD '''
        return


class NemoKern(Kern):
    ''' Stores information about NEMO kernels as extracted from the
    NEMO code. '''
    def __init__(self):
        ''' Create an empty NemoKern object. The object is given state via
        the load method '''
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

    @staticmethod
    def is_kernel(node):
        '''
        :param node: Node in fparser2 AST to check
        :type node: xml.minidom.XXXX
        :returns: True if this node conforms to the rules for a kernel
        :rtype: bool
        '''
        from fparser.two.Fortran2003 import walk_ast, \
            Block_Nonlabel_Do_Construct, Write_Stmt, Read_Stmt
        child_loops = walk_ast(node.content,
                               [Block_Nonlabel_Do_Construct, Write_Stmt,
                                Read_Stmt])
        if child_loops:
            # A kernel cannot contain other loops or reads or writes
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
        from habakkuk.parse2003 import walk_ast
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
        from habakkuk.parse2003 import walk_ast
        from fparser.two.Fortran2003 import Loop_Control, \
            Nonlabel_Do_Stmt, End_Do_Stmt

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
        from fparser.two.Fortran2003 import Section_Subscript_List
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

    def gen_code(self, parent):
        '''
        Create the node(s) in the f2pygen AST that will generate the code
        for this object

        :param parent: parent node in the f2pygen AST
        :type parent: :py:class:`psyclone.f2pygen.DoGen`
        '''
        return
        from psyclone.f2pygen2 import AssignGen
        for item in self._body:
            parent.add(AssignGen(item))

    def update(self):
        ''' TBD '''
        return


class NemoLoop(Loop):
    '''
    Class representing a Loop in NEMO.
    '''
    def __init__(self, ast, parent=None):
        '''
        :param xnode: the node in the XML dom representing the Loop
        :type xnode: :py:class:`xml.dom.minidom.xxxxx`
        :param parent: the parent of this Loop in the PSyclone AST
        '''
        from fparser.two.Fortran2003 import Loop_Control, \
            Block_Nonlabel_Do_Construct, Nonlabel_Do_Stmt, End_Do_Stmt
        from psyclone.psyGen import Loop
        Loop.__init__(self, parent=parent,
                      valid_loop_types=VALID_LOOP_TYPES+["unknown"])
        # Keep a ptr to the corresponding node in the AST
        self._ast = ast

        # Get the loop variable
        ctrl = walk_ast(ast.content, [Loop_Control])
        # Second element of items member of Loop Control is itself a tuple
        # containing:
        #   Loop variable, start value expression, end value expression, step
        #   expression
        # Loop variable will be an instance of Fortran2003.Name
        loop_var = str(ctrl[0].items[1][0])
        self._variable_name = str(loop_var)

        # Identify the type of loop
        if self._variable_name in NEMO_LOOP_TYPE_MAPPING:
            self.loop_type = NEMO_LOOP_TYPE_MAPPING[self._variable_name]
        else:
            self.loop_type = "unknown"

        # Get the loop limits. These are the members of a list which is
        # the second element of the items tuple.
        self._start = str(ctrl[0].items[1][0])
        self._stop = str(ctrl[0].items[1][1])

        # TODO check that the third element of items really does contain
        # the loop increment
        if ctrl[0].items[2]:
            self._step = str(ctrl[0].items[2])
        else:
            self._step = ""

        # List of nodes we will use to create 'code blocks' that we don't
        # attempt to understand
        code_block_nodes = []

        # Is this loop body a kernel?
        if NemoKern.is_kernel(self._ast):
            kern = NemoKern()
            kern.load(self._ast, parent=self)
            self.addchild(kern)
            return

        for child in self._ast.content:
            if isinstance(child, Block_Nonlabel_Do_Construct):
                # The start of a loop is taken as the end of any exising
                # code block so we create that now
                _add_code_block(self, code_block_nodes)
                self.addchild(NemoLoop(child, parent=self))
            elif isinstance(child, (End_Do_Stmt, Nonlabel_Do_Stmt)):
                # Don't include the do or end-do in a code block
                pass
            else:
                code_block_nodes.append(child)

        # Finish any open code block
        _add_code_block(self, code_block_nodes)

    @property
    def kernel(self):
        ''' Returns the kernel object if one is associated with this loop,
        None otherwise. '''
        kernels = self.walk(self.children, NemoKern)
        if kernels:
            # TODO cope with case where loop contains >1 kernel (e.g.
            # following loop fusion)
            return kernels[0]
        else:
            return None

    def update(self):
        ''' TBD '''
        for child in self._children:
            child.update()
