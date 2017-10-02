# ----------------------------------------------------------------------------
# (c) Copyright Science and Technology Facilities Council, 2017
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
# ----------------------------------------------------------------------------
# Authors R. Ford and A. R. Porter, STFC Daresbury Lab

'''This module implements the PSyclone NEMO 0.1 API by specialising
    the required base classes for both code generation (PSy, Invokes,
    Invoke, Schedule, Loop, Kern, Arguments and KernelArgument)
    and parsing (Descriptor and KernelType).

'''

from psyclone.parse import Descriptor, KernelType, ParseError
from psyclone.psyGen import PSy, Invokes, Invoke, Schedule, Node, \
    Loop, Kern, Arguments, KernelArgument, GenerationError, colored, \
    SCHEDULE_COLOUR_MAP as _BASE_CMAP

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


def translate_ast(node, parent, indent=0, debug=False):
    '''' Walk down the tree produced by the f2003 parser where children
    are listed under 'content'.  Replace any loop nests that we've
    identified as kernels with the corresponding Kernel object. '''
    import fparser
    from fparser import Fortran2003
    from habakkuk.parse2003 import walk_ast
    cblock_list = []
    # Depending on their level in the tree produced by fparser2003,
    # some nodes have children listed in .content and some have them
    # listed under .items. If a node has neither then it has no
    # children.
    if hasattr(parent, "content"):
        children = parent.content
    elif hasattr(parent, "items"):
        children = parent.items
    else:
        return

    code_block_statements = []

    for idx, child in enumerate(children[:]):
        if debug:
            print indent*"  " + "child type = ", type(child)
            
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
                ploop = NEMOLoop(parent=node, loop_ast=ctrl[depth],
                                 loop_var=loop_var, contains_kern=True)
                node.addchild(ploop)
                for loop in nested_loops:
                    depth += 1
                    loop_var = str(ctrl[depth].items[0])
                    nloop = NEMOLoop(parent=ploop, loop_ast=ctrl[depth],
                                     loop_var=loop_var)
                    ploop.addchild(nloop)
                    ploop = nloop

                kern = NEMOKern()
                kern.load(child, parent=ploop)
                ploop.addchild(kern)
            else:
                loop = NEMOLoop(parent=node, loop_ast=ctrl[0],
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
                kern = NEMOKern()
                kern.load(child, parent=node)
                node.addchild(kern)
            else:
                # Add this node in the AST to our list for the current
                # code block (unless it is loop-related in which case we
                # ignore it)
                if type(child) not in [fparser.Fortran2003.Nonlabel_Do_Stmt,
                                       fparser.Fortran2003.End_Do_Stmt]:
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
    from fparser import Fortran2003
    from habakkuk.parse2003 import walk_ast

    if not isinstance(node, Fortran2003.Assignment_Stmt):
        return False

    for child in node.items:
        if isinstance(child, Fortran2003.Part_Ref):
            colons = walk_ast(child.items, [Fortran2003.Subscript_Triplet])
            if colons:
                return True
    return False


def _add_code_block(parent, statements):
    ''' Create a NEMOCodeBlock for the supplied list of statements
    and then wipe the list of statements '''
    from nemo0p1 import NEMOCodeBlock

    if not statements:
        return None
    
    code_block = NEMOCodeBlock(statements,
                               parent=parent)
    parent.addchild(code_block)
    statements = []
    return code_block


class NEMOInvoke(Invoke):

    def __init__(self, ast, name):
        self._schedule = None
        self._name = name
        self._psy_unique_vars = ["a_variable"]

        from habakkuk.parse2003 import walk_ast, Loop, get_child, ParseError
        from fparser.Fortran2003 import Main_Program, Program_Stmt, \
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

        # Store the root of this routine's specification in the AST
        self._spec_part = get_child(ast, Specification_Part)

        # We now walk through the AST produced by fparser2 and construct a
        # new AST using objects from the nemo0p1 module.
        self._schedule = NEMOSchedule()
        translate_ast(self._schedule, exe_part, debug=True)

    def gen_code(self, parent):
        '''
        Generates the f2pygen AST for this invoke
        :param parent: Parent of this node in the AST we are creating
        :type parent: :py:class:`psyclone.f2pygen.ModuleGen`
        '''
        from psyclone.f2pygen import SubroutineGen, DeclGen, TypeDeclGen, \
            CommentGen, AssignGen

        if not self._schedule:
            return
        
        # create the subroutine
        invoke_sub = SubroutineGen(parent, name=self.name,
                                   args=self.psy_unique_var_names)
        parent.add(invoke_sub)
        self.schedule.gen_code(invoke_sub)

    @property
    def psy_unique_var_names(self):
        return self._psy_unique_vars


class NEMOInvokes(Invokes):

    def __init__(self, ast):
        from habakkuk.parse2003 import walk_ast, get_child, ParseError
        from fparser.Fortran2003 import Main_Program, Program_Stmt, \
            Subroutine_Subprogram, Function_Subprogram, Function_Stmt, \
            Subroutine_Stmt
        
        self.invoke_map = {}
        self.invoke_list = []

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

            my_invoke = NEMOInvoke(subroutine, name=sub_name)
            self.invoke_map[sub_name] = my_invoke
            self.invoke_list.append(my_invoke)


class NEMOPSy(PSy):
    ''' The NEMO 0.1-specific PSy class. This creates a NEMO-specific
        invokes object (which controls all the required invocation calls).
        Also overrides the PSy gen method so that we generate GOcean-
        specific PSy module code. '''
    
    def __init__(self, ast):

        self._name = "NEMO-PSY"  # TODO use a meaningful name
        self._invokes = NEMOInvokes(ast)
        
    def inline(self, module):
        # Override base-class method because we don't yet support it
        pass

    @property
    def gen(self):
        '''
        Generate PSy code for the GOcean api v.1.0.

        :rtype: ast

        '''
        from psyclone.f2pygen import ModuleGen, UseGen

        # create an empty PSy layer module
        psy_module = ModuleGen(self.name)
        # include the kind_params module
        psy_module.add(UseGen(psy_module, name="kind_params_mod"))
        # include the field_mod module
        psy_module.add(UseGen(psy_module, name="field_mod"))
        # add in the subroutines for each invocation
        self.invokes.gen_code(psy_module)
        # inline kernels where requested
        self.inline(psy_module)
        return psy_module.root


class NEMOSchedule(Schedule):
    ''' The GOcean specific schedule class. We call the base class
    constructor and pass it factories to create GO-specific calls to both
    user-supplied kernels and built-ins. '''

    def __init__(self):
        Node.__init__(self)
        #Schedule.__init__(self, NEMOKernCallFactory, GOBuiltInCallFactory,
        #                  alg_calls)

        # Configuration of this Schedule - we default to having
        # constant loop bounds. If we end up having a long list
        # of configuration member variables here we may want
        # to create a a new ScheduleConfig object to manage them.
        self._const_loop_bounds = True

    def view(self, indent=0):
        ''' Print a representation of this NEMOSchedule '''
        print self.indent(indent) + self.coloured_text + "[]"
        for entity in self._children:
            entity.view(indent=indent + 1)

    def __str__(self):
        ''' Returns the string representation of this NEMOSchedule '''
        result = "NEMOSchedule(Constant loop bounds=" + \
                 str(self._const_loop_bounds) + "):\n"
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


class NEMOLoop(Loop):
    ''' The NEMO-specific Loop class. This passes the GOcean specific
        single loop information to the base class so it creates the one we
        require. Adds a GOcean specific setBounds method which tells the loop
        what to iterate over. Need to harmonise with the topology_name method
        in the Dynamo api. '''
    def __init__(self, parent=None, topology_name="", loop_type="",
                 loop_ast=None, loop_var=None, contains_kern=False):
        Loop.__init__(self, parent=parent,
                      valid_loop_types=VALID_LOOP_TYPES)

        # Whether or not this Loop is associated with a kernel
        self._contains_kern = contains_kern
        
        # TODO identify correct loop type
        if loop_var in NEMO_LOOP_TYPE_MAPPING:
            self.loop_type = NEMO_LOOP_TYPE_MAPPING[loop_var]
        else:
            self.loop_type = "unknown"

        # We set the loop variable name in the constructor so that it is
        # available when we're determining which vars should be OpenMP
        # PRIVATE (which is done *before* code generation is performed)
        if self.loop_type == "lon":
            self._variable_name = "ji"
        elif self.loop_type == "lat":
            self._variable_name = "jj"
        elif self.loop_type == "levels":
            self._variable_name = "jk"
        elif self.loop_type == "tracers":
            self._variable_name = "jt"
        elif self.loop_type == "unknown":
            # TODO work out whether we care about variable name
            # for NEMO loops
            self._variable_name = "index"
        else:
            raise GenerationError(
                "Invalid loop type of '{0}'. Expected one of {1}".
                format(self._loop_type, VALID_LOOP_TYPES))
        self._lower_bound = str(loop_ast.items[1][0])
        self._upper_bound = str(loop_ast.items[1][1])

    def view(self, indent=0):
        ''' Print a representation of this Loop to stdout '''
        print self.indent(indent) + self.coloured_text + \
            "[type='{0}',field_space='{1}',it_space='{2}']".\
            format(self._loop_type, self._field_space, self.iteration_space)
        for entity in self._children:
            entity.view(indent=indent + 1)

    def __str__(self):
        ''' Returns a string describing this Loop object '''
        step = self._step
        if not step:
            step = "1"

        result = ("NEMOLoop[" + self._id + "]: " + self._variable_name +
                  "=" + self._lower_bound +
                  "," + self._upper_bound + "," + step + "\n")
        for entity in self._children:
            result += str(entity)+"\n"
        result += "EndLoop"
        return result

    @property
    def contains_kern(self):
        ''' Returns True if this loop is associated with a kernel,
        False otherwise. '''
        return self._contains_kern

    @property
    def kernel(self):
        ''' Returns the kernel object if one is associated with this loop,
        None otherwise. '''
        kernels = self.walk(self.children, NEMOKern)
        if kernels:
            # TODO cope with case where loop contains >1 kernel (e.g.
            # following loop fusion)
            return kernels[0]
        else:
            return None
        
    def gen_code(self, parent):
        ''' Generate the Fortran source for this loop '''
        # Our schedule holds the names to use for the loop bounds.
        # Climb up the tree looking for our enclosing Schedule
        schedule = self.ancestor(NEMOSchedule)
        if schedule is None or not isinstance(schedule, NEMOSchedule):
            raise GenerationError("Internal error: cannot find parent"
                                  " NEMOSchedule for this Do loop")

        # Generate the upper and lower loop bounds
        self._start = self._lower_bound
        self._stop = self._upper_bound
        Loop.gen_code(self, parent)


class NEMOCodeBlock(Node):
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
        return colored("NEMOCodeBlock", NEMO_SCHEDULE_COLOUR_MAP["CodeBlock"])

    def view(self, indent=0):
        ''' Print a representation of this node in the schedule '''
        print self.indent(indent) + self.coloured_text + "[" + \
            str(type(self._statements[0])) + "]"
        for entity in self._children:
            entity.view(indent=indent + 1)

    def __str__(self):
        return "CodeBlock[{0} statements]".format(len(self._statements))

    def gen_code(self, parent):
        ''' Convert this code block back to Fortran '''
        for statement in self._statements:
            # TODO each statement is an item from the fparser2 AST but
            # parent.add expects an f2pygen object.
            #parent.add(statement)
            pass
        for entity in self._children:
            entity.gen_code(parent)


class NEMOKern(Kern):
    ''' Stores information about NEMO kernels as extracted from the
    NEMO code. '''
    def __init__(self):
        ''' Create an empty NEMOKern object. The object is given state via
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

    @property
    def type(self):
        ''' Returns what type of kernel this is '''
        return self._kernel_type

    def load(self, loop, parent=None):
        ''' Populate the state of this NEMOKern object '''
        from habakkuk.parse2003 import walk_ast
        from fparser.Fortran2003 import Block_Nonlabel_Do_Construct, \
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
        ''' Populate the state of this NEMOKern object from an fparser2
        AST for a loop nest '''
        from habakkuk.parse2003 import walk_ast
        from fparser.Fortran2003 import Loop_Control, \
            Block_Nonlabel_Do_Construct, Nonlabel_Do_Stmt, End_Do_Stmt

        # Keep a pointer to the original loop in the AST
        self._loop = loop

        ctrls = walk_ast(loop.content, [Loop_Control], debug=False)
        # items member of Loop Control contains a 2-tuple:
        #   (Loop variable, [start value expression, end value expression])
        # i.e. the second element of the tuple is itself a list containing
        # the loop limits.
        # Loop variable will be an instance of Fortran2003.Name
        for ctrl in ctrls:
            self._loop_vars.append(str(ctrl.items[0]))
            self._loop_ranges.append( (str(ctrl.items[1][0]),
                                       str(ctrl.items[1][1])) )

        # Now we find the content of this nested loop
        nested_loops = walk_ast(loop.content, [Block_Nonlabel_Do_Construct])
        inner_loop = nested_loops[-1]
        if not isinstance(inner_loop.content[0], Nonlabel_Do_Stmt):
            raise ParseError("Internal error, expecting Nonlabel_Do_Stmt as "
                             "first child of Block_Nonlabel_Do_Construct but "
                             "got {0}".format(type(inner_loop.content[0])))
        self._body = []
        for content in inner_loop.content[1:]:
            if isinstance(content, End_Do_Stmt):
                break
            self._body.append(content)

        if len(self._loop_vars) == 2:
            self._kernel_type = "2D"
        else:
            self._kernel_type = "3D"

        # Analyse the loop body to identify private and shared variables
        from habakkuk.make_dag import dag_of_code_block
        # Create a DAG of the kernel code block using Habakkuk
        kernel_dag = dag_of_code_block(inner_loop, "nemo_kernel")
        inputs = kernel_dag.input_nodes()
        outputs = kernel_dag.output_nodes()
        print "Kernel has {0} outputs: ".format(len(outputs)) + \
            ",".join([node.variable.orig_name for node in outputs])
        self._shared_vars = set()
        self._first_private_vars = set()
        self._private_vars = set()
        # If there are scalar variables that are inputs to the DAG (other than
        # the loop counters) then they must be declared first-private.
        for node in inputs:
            if not node.node_type:
                if node.name not in NEMO_LOOP_VARS:
                    self._first_private_vars.add(node.name)
        for key, node in kernel_dag._nodes.iteritems():
            if node.node_type == "array_ref":
                self._shared_vars.add(node.variable.orig_name)
            elif not node.node_type:
                self._private_vars.add(node.variable.orig_name)
        self._private_vars -= self._first_private_vars
        print "OpenMP shared vars: " + ",".join(self._shared_vars)
        print "OpenMP private vars: " + ",".join(self._private_vars)
        print "OpenMP first-private vars: " + \
            ",".join(self._first_private_vars)
        return
    
    def _load_from_implicit_loop(self, loop, parent=None):
        ''' Populate the state of this NEMOKern object from an fparser2
        AST for an implicit loop (Fortran array syntax) '''
        from fparser.Fortran2003 import Section_Subscript_List
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
        print (self.indent(indent) + self.coloured_text + "[" +
               self._kernel_type + "]")
        for entity in self._children:
            entity.view(indent=indent + 1)

    def gen_code(self, parent):
        print self.tofortran()
        #print str(self.loop)
