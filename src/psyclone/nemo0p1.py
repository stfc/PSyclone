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
    Loop, Kern, Arguments, KernelArgument, GenerationError

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
    from nemo0p1 import NEMOLoop, NEMOKern, NEMO_LOOP_TYPE_MAPPING
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
            if is_kern and (loop_var == "jk" and len(nested_loops) == 2):
                kern = NEMOKern()
                kern.load(child, parent=node)
                node.addchild(kern)
                # We don't want to create kernels for any of the loops
                # nested within this loop so we don't carry on any
                # further down the tree

            elif is_kern and (loop_var == "jj" and len(nested_loops) == 1):
                kern = NEMOKern()
                kern.load(child, parent=node)
                node.addchild(kern)
                # We don't want to create kernels for any of the loops
                # nested within this loop so we don't carry on any
                # further down the tree
            
            else:
                # TODO identify correct loop type
                if loop_var in NEMO_LOOP_TYPE_MAPPING:
                    ltype = NEMO_LOOP_TYPE_MAPPING[loop_var]
                else:
                    ltype = "unknown"
                loop = NEMOLoop(parent=node, loop_type=ltype)
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
            arr_sections = []
            if isinstance(child, Fortran2003.Assignment_Stmt):
                arr_sections = walk_ast(child.items,
                                        [Fortran2003.Section_Subscript_List],
                                        debug=True)
                if arr_sections:
                    # An implicit loop marks the end of any current
                    # code block
                    _add_code_block(node, code_block_statements)
                        
                    # Create a kernel for this implicit loop
                    kern = NEMOKern()
                    kern.load(child, parent=node)
                    node.addchild(kern)

            # Add this node in the AST to our list for the current
            # code block (unless it is loop-related in which case we
            # ignore it)
            if (not arr_sections) and \
               type(child) not in [fparser.Fortran2003.Nonlabel_Do_Stmt,
                                   fparser.Fortran2003.End_Do_Stmt]:
                code_block_statements.append(child)

    # Finish any open code block
    _add_code_block(node, code_block_statements)

    return


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
        self._name = name

        from habakkuk.parse2003 import walk_ast, Loop, get_child, ParseError
        from fparser.Fortran2003 import Main_Program, Program_Stmt, \
            Subroutine_Subprogram, Function_Subprogram, Function_Stmt, \
            Subroutine_Stmt, Block_Nonlabel_Do_Construct, Execution_Part, \
            Name

        # Find the section of the tree containing the execution part
        # of the code
        try:
            exe_part = get_child(ast, Execution_Part)
        except ParseError:
            # This subroutine has no execution part so we skip it
            # TODO log this event
            return

        # Make a list of all Do loops in the routine
        loops = walk_ast(exe_part.content,
                         [Block_Nonlabel_Do_Construct])

        if not loops:
            print "Routine {0} contains no DO loops - skipping".\
                format(name)
            return

        # Since this subroutine contains loops we now walk through
        # the AST produced by fparser2 and construct a new AST
        # using objects from the nemo0p1 module.
        self._schedule = NEMOSchedule()
        translate_ast(self._schedule, exe_part, debug=True)
        self._schedule.view()



class NEMOInvokes(Invokes):

    def __init__(self, ast):
        from habakkuk.parse2003 import walk_ast, get_child, ParseError
        from fparser.Fortran2003 import Main_Program, Program_Stmt, \
            Subroutine_Subprogram, Function_Subprogram, Function_Stmt, \
            Subroutine_Stmt
        self._invoke_list = []
        
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

            self._invoke_list.append(NEMOInvoke(subroutine,
                                                name=sub_name))


class NEMOPSy(PSy):
    ''' The NEMO 0.1-specific PSy class. This creates a NEMO-specific
        invokes object (which controls all the required invocation calls).
        Also overrides the PSy gen method so that we generate GOcean-
        specific PSy module code. '''
    
    def __init__(self, ast):

        self._invokes = NEMOInvokes(ast)
        

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


class GOInvokes(Invokes):
    ''' The GOcean specific invokes class. This passes the GOcean specific
        invoke class to the base class so it creates the one we require. '''
    def __init__(self, alg_calls):
        if False:
            self._0_to_n = GOInvoke(None, None)  # for pyreverse
        Invokes.__init__(self, alg_calls, GOInvoke)

        index_offsets = []
        # Loop over all of the kernels in all of the invoke() calls
        # and check that they work on compatible grid-index offsets.
        # Strictly speaking this check should be done in the parsing
        # code since it is a check on the correctness of the meta-data.
        # However, that would require a fundamental change to the parsing
        # code since it requires information  on all of the invokes and
        # kernels in an application. Therefore it is much simpler to
        # do it here where we have easy access to that information.
        for invoke in self.invoke_list:
            for kern_call in invoke.schedule.kern_calls():
                # We only care if the index offset is not offset_any (since
                # that is compatible with any other offset)
                if kern_call.index_offset != "offset_any":
                    # Loop over the offsets we've seen so far
                    for offset in index_offsets:
                        if offset != kern_call.index_offset:
                            raise GenerationError(
                                "Meta-data error in kernel {0}: "
                                "INDEX_OFFSET of '{1}' does not match that "
                                "({2}) of other kernels. This is not "
                                "supported.".format(kern_call.name,
                                                    kern_call.index_offset,
                                                    offset))
                    # Append the index-offset of this kernel to the list of
                    # those seen so far
                    index_offsets.append(kern_call.index_offset)


class GOInvoke(Invoke):
    ''' The GOcean specific invoke class. This passes the GOcean specific
        schedule class to the base class so it creates the one we require.
        A set of GOcean infrastructure reserved names are also passed to
        ensure that there are no name clashes. Also overrides the gen_code
        method so that we generate GOcean specific invocation code and
        provides three methods which separate arguments that are arrays from
        arguments that are {integer, real} scalars. '''
    def __init__(self, alg_invocation, idx):
        if False:
            self._schedule = GOSchedule(None)  # for pyreverse
        Invoke.__init__(self, alg_invocation, idx, GOSchedule)

    @property
    def unique_args_arrays(self):
        ''' find unique arguments that are arrays (defined as those that are
            field objects as opposed to scalars or properties of the grid). '''
        result = []
        for call in self._schedule.calls():
            for arg in call.arguments.args:
                if arg.type == 'field' and arg.name not in result:
                    result.append(arg.name)
        return result

    @property
    def unique_args_rscalars(self):
        ''' find unique arguments that are scalars of type real (defined
            as those that are r_scalar 'space'. '''
        result = []
        for call in self._schedule.calls():
            for arg in call.arguments.args:
                if arg.type == 'scalar' and \
                   arg.space.lower() == "r_scalar" and arg.name not in result:
                    result.append(arg.name)
        return result

    @property
    def unique_args_iscalars(self):
        ''' find unique arguments that are scalars of type integer (defined
            as those that are i_scalar 'space'). '''
        result = []
        for call in self._schedule.calls():
            for arg in call.arguments.args:
                if arg.type == 'scalar' and \
                   arg.space.lower() == "i_scalar" and arg.name not in result:
                    result.append(arg.name)
        return result

    def gen_code(self, parent):
        ''' Generates GOcean specific invocation code (the subroutine called
            by the associated invoke call in the algorithm layer). This
            consists of the PSy invocation subroutine and the declaration of
            its arguments.'''
        from psyclone.f2pygen import SubroutineGen, DeclGen, TypeDeclGen, \
            CommentGen, AssignGen
        # create the subroutine
        invoke_sub = SubroutineGen(parent, name=self.name,
                                   args=self.psy_unique_var_names)
        parent.add(invoke_sub)

        # add declarations for the variables holding the upper bounds
        # of loops in i and j
        if self.schedule.const_loop_bounds:
            invoke_sub.add(DeclGen(invoke_sub, datatype="INTEGER",
                                   entity_decls=[self.schedule.iloop_stop,
                                                 self.schedule.jloop_stop]))

        # Generate the code body of this subroutine
        self.schedule.gen_code(invoke_sub)

        # add the subroutine argument declarations for fields
        if len(self.unique_args_arrays) > 0:
            my_decl_arrays = TypeDeclGen(invoke_sub, datatype="r2d_field",
                                         intent="inout",
                                         entity_decls=self.unique_args_arrays)
            invoke_sub.add(my_decl_arrays)

        # add the subroutine argument declarations for real scalars
        if len(self.unique_args_rscalars) > 0:
            my_decl_rscalars = DeclGen(invoke_sub, datatype="REAL",
                                       intent="inout", kind="wp",
                                       entity_decls=self.unique_args_rscalars)
            invoke_sub.add(my_decl_rscalars)
        # add the subroutine argument declarations for integer scalars
        if len(self.unique_args_iscalars) > 0:
            my_decl_iscalars = DeclGen(invoke_sub, datatype="INTEGER",
                                       intent="inout",
                                       entity_decls=self.unique_args_iscalars)
            invoke_sub.add(my_decl_iscalars)

        if self._schedule.const_loop_bounds and \
           len(self.unique_args_arrays) > 0:

            # Look-up the loop bounds using the first field object in the
            # list
            sim_domain = self.unique_args_arrays[0] +\
                "%grid%simulation_domain%"
            position = invoke_sub.last_declaration()

            invoke_sub.add(CommentGen(invoke_sub, ""),
                           position=["after", position])
            invoke_sub.add(AssignGen(invoke_sub, lhs=self.schedule.jloop_stop,
                                     rhs=sim_domain+"ystop"),
                           position=["after", position])
            invoke_sub.add(AssignGen(invoke_sub, lhs=self.schedule.iloop_stop,
                                     rhs=sim_domain+"xstop"),
                           position=["after", position])
            invoke_sub.add(CommentGen(invoke_sub, " Look-up loop bounds"),
                           position=["after", position])
            invoke_sub.add(CommentGen(invoke_sub, ""),
                           position=["after", position])


class NEMOSchedule(Schedule):
    ''' The GOcean specific schedule class. We call the base class
    constructor and pass it factories to create GO-specific calls to both
    user-supplied kernels and built-ins. '''

    def __init__(self):
        Node.__init__(self)
        #Schedule.__init__(self, GOKernCallFactory, GOBuiltInCallFactory,
        #                  alg_calls)

        # Configuration of this Schedule - we default to having
        # constant loop bounds. If we end up having a long list
        # of configuration member variables here we may want
        # to create a a new ScheduleConfig object to manage them.
        self._const_loop_bounds = True

    def view(self, indent=0):
        ''' Print a representation of this NEMOSchedule '''
        from numpy.distutils.misc_util import red_text
        print self.indent(indent) + red_text("NEMOSchedule[]")
        for entity in self._children:
            entity.view(indent=indent + 1)

    def __str__(self):
        ''' Returns the string representation of this GOSchedule '''
        result = "GOSchedule(Constant loop bounds=" + \
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


class NEMOLoop(Loop):
    ''' The NEMO-specific Loop class. This passes the GOcean specific
        single loop information to the base class so it creates the one we
        require. Adds a GOcean specific setBounds method which tells the loop
        what to iterate over. Need to harmonise with the topology_name method
        in the Dynamo api. '''
    def __init__(self, parent=None,
                 topology_name="", loop_type=""):
        Loop.__init__(self, parent=parent,
                      valid_loop_types=VALID_LOOP_TYPES)
        self.loop_type = loop_type

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

        # Create a dictionary to simplify the business of looking-up
        # loop bounds
        self._bounds_lookup = {}
        for grid_offset in SUPPORTED_OFFSETS:
            self._bounds_lookup[grid_offset] = {}
            for gridpt_type in VALID_FIELD_GRID_TYPES:
                self._bounds_lookup[grid_offset][gridpt_type] = {}
                for itspace in VALID_ITERATES_OVER:
                    self._bounds_lookup[grid_offset][gridpt_type][itspace] = {}

        # Loop bounds for a mesh with NE offset
        self._bounds_lookup['offset_ne']['ct']['all_pts'] = \
            {'inner': {'start': "1", 'stop': "+1"},
             'outer': {'start': "1", 'stop': "+1"}}
        self._bounds_lookup['offset_ne']['ct']['internal_pts'] = \
            {'inner': {'start': "2", 'stop': ""},
             'outer': {'start': "2", 'stop': ""}}
        self._bounds_lookup['offset_ne']['cu']['all_pts'] = \
            {'inner': {'start': "1", 'stop': ""},
             'outer': {'start': "1", 'stop': "+1"}}
        self._bounds_lookup['offset_ne']['cu']['internal_pts'] = \
            {'inner': {'start': "2", 'stop': "-1"},
             'outer': {'start': "2", 'stop': ""}}
        self._bounds_lookup['offset_ne']['cv']['all_pts'] = \
            {'inner': {'start': "1", 'stop': "+1"},
             'outer': {'start': "1", 'stop': ""}}
        self._bounds_lookup['offset_ne']['cv']['internal_pts'] = \
            {'inner': {'start': "2", 'stop': ""},
             'outer': {'start': "2", 'stop': "-1"}}
        self._bounds_lookup['offset_ne']['cf']['all_pts'] = \
            {'inner': {'start': "1", 'stop': ""},
             'outer': {'start': "1", 'stop': ""}}
        self._bounds_lookup['offset_ne']['cf']['internal_pts'] = \
            {'inner': {'start': "1", 'stop': "-1"},
             'outer': {'start': "1", 'stop': "-1"}}
        # Loop bounds for a mesh with SE offset
        self._bounds_lookup['offset_sw']['ct']['all_pts'] = \
            {'inner': {'start': "1", 'stop': "+1"},
             'outer': {'start': "1", 'stop': "+1"}}
        self._bounds_lookup['offset_sw']['ct']['internal_pts'] = \
            {'inner': {'start': "2", 'stop': ""},
             'outer': {'start': "2", 'stop': ""}}
        self._bounds_lookup['offset_sw']['cu']['all_pts'] = \
            {'inner': {'start': "1", 'stop': "+1"},
             'outer': {'start': "1", 'stop': "+1"}}
        self._bounds_lookup['offset_sw']['cu']['internal_pts'] = \
            {'inner': {'start': "2", 'stop': "+1"},
             'outer': {'start': "2", 'stop': ""}}
        self._bounds_lookup['offset_sw']['cv']['all_pts'] = \
            {'inner': {'start': "1", 'stop': "+1"},
             'outer': {'start': "1", 'stop': "+1"}}
        self._bounds_lookup['offset_sw']['cv']['internal_pts'] = \
            {'inner': {'start': "2", 'stop': ""},
             'outer': {'start': "2", 'stop': "+1"}}
        self._bounds_lookup['offset_sw']['cf']['all_pts'] = \
            {'inner': {'start': "1", 'stop': "+1"},
             'outer': {'start': "1", 'stop': "+1"}}
        self._bounds_lookup['offset_sw']['cf']['internal_pts'] = \
            {'inner': {'start': "2", 'stop': "+1"},
             'outer': {'start': "2", 'stop': "+1"}}
        # For offset 'any'
        for gridpt_type in VALID_FIELD_GRID_TYPES:
            for itspace in VALID_ITERATES_OVER:
                self._bounds_lookup['offset_any'][gridpt_type][itspace] = \
                    {'inner': {'start': "1", 'stop': ""},
                     'outer': {'start': "1", 'stop': ""}}
        # For 'every' grid-point type
        for offset in SUPPORTED_OFFSETS:
            for itspace in VALID_ITERATES_OVER:
                self._bounds_lookup[offset]['every'][itspace] = \
                    {'inner': {'start': "1", 'stop': "+1"},
                     'outer': {'start': "1", 'stop': "+1"}}

    def view(self, indent=0):
        ''' Print a representation of this Loop to stdout '''
        from numpy.distutils.misc_util import blue_text
        print self.indent(indent) +\
            blue_text("Loop[type='{0}',field_space='{1}',it_space='{2}']".\
            format(self._loop_type, self._field_space, self.iteration_space))
        for entity in self._children:
            entity.view(indent=indent + 1)

    def _upper_bound(self):
        ''' Returns the upper bound of this loop as a string '''
        schedule = self.ancestor(GOSchedule)
        if schedule.const_loop_bounds:
            index_offset = ""
            # Look for a child kernel in order to get the index offset.
            # Since this is the __str__ method we have no guarantee
            # what state we expect our object to be in so we allow
            # for the case where we don't have any child kernels.
            go_kernels = self.walk(self.children, GOKern)
            if go_kernels:
                index_offset = go_kernels[0].index_offset

            if self._loop_type == "inner":
                stop = schedule.iloop_stop
            else:
                stop = schedule.jloop_stop

            if index_offset:
                stop += (self._bounds_lookup[index_offset][self.field_space]
                         [self._iteration_space][self._loop_type]["stop"])
            else:
                stop = "not yet set"
        else:
            if self.field_space == "every":
                # Bounds are independent of the grid-offset convention in use

                # We look-up the upper bounds by enquiring about the SIZE of
                # the array itself
                if self._loop_type == "inner":
                    stop = "SIZE("+self.field_name+"%data, 1)"
                elif self._loop_type == "outer":
                    stop = "SIZE("+self.field_name+"%data, 2)"

            else:
                # loop bounds are pulled from the field object which
                # is more straightforward for us but provides the
                # Fortran compiler with less information.
                stop = self.field_name

                if self._iteration_space.lower() == "internal_pts":
                    stop += "%internal"
                elif self._iteration_space.lower() == "all_pts":
                    stop += "%whole"
                else:
                    raise GenerationError("Unrecognised iteration space, {0}. "
                                          "Cannot generate loop bounds.".
                                          format(self._iteration_space))
                if self._loop_type == "inner":
                    stop += "%xstop"
                elif self._loop_type == "outer":
                    stop += "%ystop"
        return stop

    def _lower_bound(self):
        ''' Returns a string containing the expression for the lower
        bound of the loop '''
        schedule = self.ancestor(GOSchedule)
        if schedule.const_loop_bounds:
            index_offset = ""
            # Look for a child kernel in order to get the index offset.
            # Since this is the __str__ method we have no guarantee
            # what state we expect our object to be in so we allow
            # for the case where we don't have any child kernels.
            go_kernels = self.walk(self.children, GOKern)
            if go_kernels:
                index_offset = go_kernels[0].index_offset

            if index_offset:
                start = (self._bounds_lookup[index_offset][self.field_space]
                         [self._iteration_space][self._loop_type]["start"])
            else:
                start = "not yet set"
        else:
            if self.field_space == "every":
                # Bounds are independent of the grid-offset convention in use
                start = "1"
            else:
                # loop bounds are pulled from the field object which
                # is more straightforward for us but provides the
                # Fortran compiler with less information.
                start = self.field_name
                if self._iteration_space.lower() == "internal_pts":
                    start += "%internal"
                elif self._iteration_space.lower() == "all_pts":
                    start += "%whole"
                else:
                    raise GenerationError("Unrecognised iteration space, {0}. "
                                          "Cannot generate loop bounds.".
                                          format(self._iteration_space))
                if self._loop_type == "inner":
                    start += "%xstart"
                elif self._loop_type == "outer":
                    start += "%ystart"
        return start

    def __str__(self):
        ''' Returns a string describing this Loop object '''
        step = self._step
        if not step:
            step = "1"

        result = ("Loop[" + self._id + "]: " + self._variable_name +
                  "=" + self._id + " lower=" + self._lower_bound() +
                  "," + self._upper_bound() + "," + step + "\n")
        for entity in self._children:
            result += str(entity)+"\n"
        result += "EndLoop"
        return result

    def gen_code(self, parent):
        ''' Generate the Fortran source for this loop '''
        # Our schedule holds the names to use for the loop bounds.
        # Climb up the tree looking for our enclosing Schedule
        schedule = self.ancestor(GOSchedule)
        if schedule is None or not isinstance(schedule, GOSchedule):
            raise GenerationError("Internal error: cannot find parent"
                                  " GOSchedule for this Do loop")

        # Walk down the tree looking for a kernel so that we can
        # look-up what index-offset convention we are to use
        go_kernels = self.walk(self.children, GOKern)
        if len(go_kernels) == 0:
            raise GenerationError("Internal error: cannot find the "
                                  "GOcean Kernel enclosed by this loop")
        index_offset = go_kernels[0].index_offset
        if schedule.const_loop_bounds and \
           index_offset not in SUPPORTED_OFFSETS:
            raise GenerationError("Constant bounds generation"
                                  " not implemented for a grid offset "
                                  "of {0}. Supported offsets are {1}".
                                  format(index_offset,
                                         SUPPORTED_OFFSETS))
        # Check that all kernels enclosed by this loop expect the same
        # grid offset
        for kernel in go_kernels:
            if kernel.index_offset != index_offset:
                raise GenerationError("All Kernels must expect the same "
                                      "grid offset but kernel {0} has offset "
                                      "{1} which does not match {2}".
                                      format(kernel.name,
                                             kernel.index_offset,
                                             index_offset))
        # Generate the upper and lower loop bounds
        self._start = self._lower_bound()
        self._stop = self._upper_bound()
        Loop.gen_code(self, parent)


class NEMOCodeBlock(Node):
    ''' Node representing some generic Fortran code that PSyclone
    does not attempt to manipulate '''

    def __init__(self, statements, parent=None):
        Node.__init__(self, parent=parent)
        # Store a list of the parser objects holding the code
        # associated with this block
        self._statements = statements[:]

    def view(self, indent=0):
        ''' Print a representation of this node in the schedule '''
        print self.indent(indent) + "CodeBlock[" + \
            str(type(self._statements[0])) + "]"
        for entity in self._children:
            entity.view(indent=indent + 1)

    
class GOKernCallFactory(object):
    ''' A GOcean-specific kernel-call factory. A standard kernel call in
    GOcean consists of a doubly-nested loop (over i and j) and a call to
    the user-supplied kernel routine. '''
    @staticmethod
    def create(call, parent=None):
        ''' Create a new instance of a call to a GO kernel. Includes the
        looping structure as well as the call to the kernel itself. '''
        outer_loop = GOLoop(parent=parent,
                            loop_type="outer")
        inner_loop = GOLoop(parent=outer_loop,
                            loop_type="inner")
        outer_loop.addchild(inner_loop)
        gocall = GOKern()
        gocall.load(call, parent=inner_loop)
        inner_loop.addchild(gocall)
        # determine inner and outer loops space information from the
        # child kernel call. This is only picked up automatically (by
        # the inner loop) if the kernel call is passed into the inner
        # loop.
        inner_loop.iteration_space = gocall.iterates_over
        outer_loop.iteration_space = inner_loop.iteration_space
        inner_loop.field_space = gocall.\
            arguments.iteration_space_arg().function_space
        outer_loop.field_space = inner_loop.field_space
        inner_loop.field_name = gocall.\
            arguments.iteration_space_arg().name
        outer_loop.field_name = inner_loop.field_name
        return outer_loop


class NEMOKern(Node):
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
        # List of variables that are shared between threads
        self._shared_vars = None
        # Type of kernel (2D, 3D..)
        self._kernel_type = ""

    def load(self, loop, parent=None):
        ''' Populate the state of this NEMOKern object '''
        from habakkuk.parse2003 import walk_ast
        from fparser.Fortran2003 import Block_Nonlabel_Do_Construct, \
            Assignment_Stmt
        
        if isinstance(loop, Block_Nonlabel_Do_Construct):
            self._load_from_loop(loop, parent)
        elif isinstance(loop, Assignment_Stmt):
            self._load_from_implicit_loop(loop, parent)
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
        from numpy.distutils.misc_util import yellow_text
        print self.indent(indent) + yellow_text("NEMOKern[" + self._kernel_type + "]")
        for entity in self._children:
            entity.view(indent=indent + 1)


class GOKernelArguments(Arguments):
    '''Provides information about GOcean kernel-call arguments
        collectively, as specified by the kernel argument
        metadata. This class ensures that initialisation is performed
        correctly. It also overrides the iteration_space_arg method to
        supply a GOcean-specific dictionary for the mapping of
        argument-access types.

    '''
    def __init__(self, call, parent_call):
        if False:
            self._0_to_n = GOKernelArgument(None, None, None)  # for pyreverse
        Arguments.__init__(self, parent_call)

        self._args = []
        # Loop over the kernel arguments obtained from the meta data
        for (idx, arg) in enumerate(call.ktype.arg_descriptors):
            # arg is a GO1p0Descriptor object
            if arg.type == "grid_property":
                # This is an argument supplied by the psy layer
                self._args.append(GOKernelGridArgument(arg))
            elif arg.type == "scalar" or arg.type == "field":
                # This is a kernel argument supplied by the Algorithm layer
                self._args.append(GOKernelArgument(arg, call.args[idx],
                                                   parent_call))
            else:
                raise ParseError("Invalid kernel argument type. Found '{0}' "
                                 "but must be one of {1}".
                                 format(arg.type, ["grid_property", "scalar",
                                                   "field"]))
        self._dofs = []

    @property
    def dofs(self):
        ''' Currently required for invoke base class although this makes no
            sense for GOcean. Need to refactor the invoke class and pull out
            dofs into the gunghoproto api '''
        return self._dofs

    def iteration_space_arg(self, mapping=None):
        if mapping:
            my_mapping = mapping
        else:
            # We provide an empty mapping for inc as it is not supported
            # in the GOcean 1.0 API. However, the entry has to be there
            # in the dictionary as a field that has read access causes
            # the code (that checks that a kernel has at least one argument
            # that is written to) to attempt to lookup "inc".
            my_mapping = {"write": "write", "read": "read",
                          "readwrite": "readwrite", "inc": ""}
        arg = Arguments.iteration_space_arg(self, my_mapping)
        return arg


class GOKernelArgument(KernelArgument):
    ''' Provides information about individual GOcean kernel call arguments
        as specified by the kernel argument metadata. '''
    def __init__(self, arg, arg_info, call):

        self._arg = arg
        KernelArgument.__init__(self, arg, arg_info, call)

    @property
    def type(self):
        ''' Return the type of this kernel argument - whether it is a field,
            a scalar or a grid_property (to be supplied by the PSy layer) '''
        return self._arg.type

    @property
    def function_space(self):
        ''' Returns the expected finite difference space for this
            argument as specified by the kernel argument metadata.'''
        return self._arg.function_space


class GOKernelGridArgument(object):
    ''' Describes arguments that supply grid properties to a kernel.
        These arguments are provided by the PSy layer rather than in
        the Algorithm layer. '''

    def __init__(self, arg):

        if arg.grid_prop in GRID_PROPERTY_DICT:
            self._name = GRID_PROPERTY_DICT[arg.grid_prop]
        else:
            raise GenerationError("Unrecognised grid property specified. "
                                  "Expected one of {0} but found '{1}'".
                                  format(str(GRID_PROPERTY_DICT.keys()),
                                         arg.grid_prop))

        # This object always represents an argument that is a grid_property
        self._type = "grid_property"

    @property
    def name(self):
        ''' Returns the Fortran name of the grid property. This name is
            used in the generated code like so: <fld>%grid%name '''
        return self._name

    @property
    def type(self):
        ''' The type of this argument. We have this for compatibility with
            GOKernelArgument objects since, for this class, it will always be
            "grid_property". '''
        return self._type

    @property
    def text(self):
        ''' The raw text used to pass data from the algorithm layer
            for this argument. Grid properties are not passed from the
            algorithm layer so None is returned.'''
        return None


class GO1p0Descriptor(Descriptor):
    '''Description of a GOcean 1.0 kernel argument, as obtained by
        parsing the kernel meta-data

    '''

    def __init__(self, kernel_name, kernel_arg):

        nargs = len(kernel_arg.args)

        if nargs == 3:
            # This kernel argument is supplied by the Algorithm layer
            # and is either a field or a scalar

            access = kernel_arg.args[0].name
            funcspace = kernel_arg.args[1].name
            stencil = kernel_arg.args[2].name

            # Valid values for the grid-point type that a kernel argument
            # may have. (We use the funcspace argument for this as it is
            # similar to the space in Finite-Element world.)
            valid_func_spaces = VALID_FIELD_GRID_TYPES + VALID_SCALAR_TYPES

            self._grid_prop = ""
            if funcspace.lower() in VALID_FIELD_GRID_TYPES:
                self._type = "field"
            elif funcspace.lower() in VALID_SCALAR_TYPES:
                self._type = "scalar"
            else:
                raise ParseError("Meta-data error in kernel {0}: argument "
                                 "grid-point type is '{1}' but must be one "
                                 "of {2} ".format(kernel_name, funcspace,
                                                  valid_func_spaces))

            if stencil.lower() not in VALID_STENCILS:
                raise ParseError("Meta-data error in kernel {0}: 3rd "
                                 "descriptor (stencil) of field argument "
                                 "is '{1}' but must be one of {2}".
                                 format(kernel_name, stencil, VALID_STENCILS))

        elif nargs == 2:
            # This kernel argument is a property of the grid
            access = kernel_arg.args[0].name
            grid_var = kernel_arg.args[1].name
            funcspace = ""
            stencil = ""

            self._grid_prop = grid_var
            self._type = "grid_property"

            if grid_var.lower() not in GRID_PROPERTY_DICT:
                raise ParseError(
                    "Meta-data error in kernel {0}: un-recognised grid "
                    "property '{1}' requested. Must be one of {2}".
                    format(kernel_name,
                           grid_var,
                           str(GRID_PROPERTY_DICT.keys())))
        else:
            raise ParseError(
                "Meta-data error in kernel {0}: 'arg' type expects 2 or 3 "
                "arguments but found '{1}' in '{2}'".
                format(kernel_name,
                       str(len(kernel_arg.args)),
                       kernel_arg.args))

        if access.lower() not in VALID_ARG_ACCESSES:
            raise ParseError("Meta-data error in kernel {0}: argument "
                             "access  is given as '{1}' but must be "
                             "one of {2}".
                             format(kernel_name, access, VALID_ARG_ACCESSES))

        # Finally we can call the __init__ method of our base class
        Descriptor.__init__(self, access, funcspace, stencil)

    def __str__(self):
        return repr(self)

    @property
    def grid_prop(self):
        ''' The name of the grid-property that this argument is to supply
            to the kernel '''
        return self._grid_prop

    @property
    def type(self):
        ''' The type of this argument - whether it is a scalar, a field or
            a grid-property. The latter are special because they must be
            supplied by the PSy layer. '''
        return self._type


class GOKernelType1p0(KernelType):
    ''' Description of a kernel including the grid index-offset it
        expects and the region of the grid that it expects to
        operate upon '''

    def __str__(self):
        return ('GOcean 1.0 kernel ' + self._name + ', index-offset = ' +
                self._index_offset + ', iterates-over = ' +
                self._iterates_over)

    def __init__(self, ast, name=None):
        # Initialise the base class
        KernelType.__init__(self, ast, name=name)

        # What grid offset scheme this kernel expects
        self._index_offset = self._ktype.get_variable('index_offset').init

        if self._index_offset is None:
            raise ParseError("Meta-data error in kernel {0}: an INDEX_OFFSET "
                             "must be specified and must be one of {1}".
                             format(name, VALID_OFFSET_NAMES))

        if self._index_offset.lower() not in VALID_OFFSET_NAMES:
            raise ParseError("Meta-data error in kernel {0}: INDEX_OFFSET "
                             "has value '{1}' but must be one of {2}".
                             format(name,
                                    self._index_offset,
                                    VALID_OFFSET_NAMES))

        # Check that the meta-data for this kernel is valid
        if self._iterates_over is None:
            raise ParseError("Meta-data error in kernel {0}: ITERATES_OVER "
                             "is missing. (Valid values are: {1})".
                             format(name, VALID_ITERATES_OVER))

        if self._iterates_over.lower() not in VALID_ITERATES_OVER:
            raise ParseError("Meta-data error in kernel {0}: ITERATES_OVER "
                             "has value '{1}' but must be one of {2}".
                             format(name,
                                    self._iterates_over.lower(),
                                    VALID_ITERATES_OVER))

        # The list of kernel arguments
        self._arg_descriptors = []
        have_grid_prop = False
        for init in self._inits:
            if init.name != 'arg':
                raise ParseError("Each meta_arg value must be of type " +
                                 "'arg' for the gocean1.0 api, but " +
                                 "found '{0}'".format(init.name))
            # Pass in the name of this kernel for the purposes
            # of error reporting
            new_arg = GO1p0Descriptor(name, init)
            # Keep track of whether this kernel requires any
            # grid properties
            have_grid_prop = (have_grid_prop or
                              (new_arg.type == "grid_property"))
            self._arg_descriptors.append(new_arg)

        # If this kernel expects a grid property then check that it
        # has at least one field object as an argument (which we
        # can use to access the grid)
        if have_grid_prop:
            have_fld = False
            for arg in self.arg_descriptors:
                if arg.type == "field":
                    have_fld = True
                    break
            if not have_fld:
                raise ParseError(
                    "Kernel {0} requires a property of the grid but does "
                    "not have any field objects as arguments.".format(name))

    # Override nargs from the base class so that it returns the no.
    # of args specified in the algorithm layer (and thus excludes those
    # that must be added in the PSy layer). This is done to simplify the
    # check on the no. of arguments supplied in any invoke of the kernel.
    @property
    def nargs(self):
        ''' Count and return the number of arguments that this kernel
            expects the Algorithm layer to provide '''
        count = 0
        for arg in self.arg_descriptors:
            if arg.type != "grid_property":
                count += 1
        return count

    @property
    def index_offset(self):
        ''' Return the grid index-offset that this kernel expects '''
        return self._index_offset
