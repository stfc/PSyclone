#-------------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
#-------------------------------------------------------------------------------
# Authors R. Ford and A. R. Porter, STFC Daresbury Lab
# Funded by the GOcean project

''' This module implements the emerging PSyclone GOcean API by specialising
    the required base classes (PSy, Invokes, Invoke, Schedule, Loop, Kern,
    Inf, Arguments and KernelArgument). '''

from psyGen import PSy, Invokes, Invoke, Schedule, Loop, Kern, Arguments, \
                   KernelArgument, GenerationError, Inf, Node

class GOPSy(PSy):
    ''' The GOcean 1.0 specific PSy class. This creates a GOcean specific
        invokes object (which controls all the required invocation calls).
        Also overrides the PSy gen method so that we generate GOcean-
        specific PSy module code. '''
    def __init__(self, invoke_info):
        PSy.__init__(self, invoke_info)
        self._invokes = GOInvokes(invoke_info.calls)
    @property
    def gen(self):
        '''
        Generate PSy code for the GOcean api v.1.0.

        :rtype: ast

        '''
        from f2pygen import ModuleGen, UseGen

        # create an empty PSy layer module
        psy_module = ModuleGen(self.name)
        # include the kind_params module
        psy_module.add(UseGen(psy_module, name = "kind_params_mod"))
        # include the field_mod module
        psy_module.add(UseGen(psy_module, name = "field_mod"))
        # add in the subroutines for each invocation
        self.invokes.gen_code(psy_module)
        return psy_module.root

class GOInvokes(Invokes):
    ''' The GOcean specific invokes class. This passes the GOcean specific
        invoke class to the base class so it creates the one we require. '''
    def __init__(self, alg_calls):
        if False:
            self._0_to_n = GOInvoke(None, None) # for pyreverse
        Invokes.__init__(self, alg_calls, GOInvoke)

class GOInvoke(Invoke):
    ''' The GOcean specific invoke class. This passes the GOcean specific
        schedule class to the base class so it creates the one we require.
        A set of GOcean infrastructure reserved names are also passed to
        ensure that there are no name clashes. Also overrides the gen_code
        method so that we generate GOcean specific invocation code and
        provides to methods which separate arguments that are arrays from
        arguments that are scalars. '''
    def __init__(self, alg_invocation, idx):
        if False:
            self._schedule = GOSchedule(None) # for pyreverse
        Invoke.__init__(self, alg_invocation, idx, GOSchedule,
                        reserved_names = ["cf", "ct", "cu", "cv"])

    @property
    def unique_args_arrays(self):
        ''' find unique arguments that are arrays (defined as those that are
            field objects as opposed to scalars or properties of the grid). '''
        result = []
        for call in self._schedule.calls():
            for arg in call.arguments.args:
                if arg.type == 'field' and not arg.name in result:
                    result.append(arg.name)
        return result

    @property
    def unique_args_rscalars(self):
        ''' find unique arguments that are scalars of type real (defined 
            as those that are r_scalar 'space'. '''
        result = []
        for call in self._schedule.calls():
            for arg in call.arguments.args:
                if arg.type == 'scalar' and arg.space.lower()=="r_scalar" and \
                   not arg.name in result:
                    result.append(arg.name)
        return result


    @property
    def unique_args_iscalars(self):
        ''' find unique arguments that are scalars of type integer (defined 
            as those that are i_scalar 'space'). '''
        result = []
        for call in self._schedule.calls():
            for arg in call.arguments.args:
                if arg.type == 'scalar' and arg.space.lower()=="i_scalar" and \
                   not arg.name in result:
                    result.append(arg.name)
        return result

    def gen_code(self, parent):
        ''' Generates GOcean specific invocation code (the subroutine called
            by the associated invoke call in the algorithm layer). This
            consists of the PSy invocation subroutine and the declaration of
            its arguments.'''
        from f2pygen import SubroutineGen, DeclGen, TypeDeclGen
        # create the subroutine
        invoke_sub = SubroutineGen(parent, name = self.name,
                                   args = self.unique_args)
        parent.add(invoke_sub)
        self.schedule.gen_code(invoke_sub)
        # add the subroutine argument declarations for arrays
        if len(self.unique_args_arrays) > 0:
            my_decl_arrays = TypeDeclGen(invoke_sub, datatype = "r2d_field",
                                     intent = "inout",
                                     entity_decls = self.unique_args_arrays)
            invoke_sub.add(my_decl_arrays)
        # add the subroutine argument declarations for real scalars
        if len(self.unique_args_rscalars) > 0:
            my_decl_rscalars = DeclGen(invoke_sub, datatype = "REAL", 
                                     intent = "inout", kind = "wp", 
                                     entity_decls = self.unique_args_rscalars) 
            invoke_sub.add(my_decl_rscalars)
        # add the subroutine argument declarations for integer scalars
        if len(self.unique_args_iscalars) > 0:
            my_decl_iscalars = DeclGen(invoke_sub, datatype = "INTEGER", 
                                     intent = "inout", 
                                     entity_decls = self.unique_args_iscalars) 
            invoke_sub.add(my_decl_iscalars)

class GOSchedule(Schedule):

    ''' The GOcean specific schedule class. The PSyclone schedule class assumes
        that a call has one parent loop. Therefore we override the _init_ method
        and add in our two loops. '''

    def __init__(self, alg_calls):
        sequence = []
        from parse import InfCall
        for call in alg_calls:
            if isinstance(call, InfCall):
                sequence.append(GOInf.create(call, parent = self))
            else:
                outer_loop = GOLoop(call = None, parent = self)
                sequence.append(outer_loop)
                outer_loop.loop_type = "outer"
                inner_loop = GOLoop(call = None, parent = outer_loop)
                inner_loop.loop_type = "inner"
                outer_loop.addchild(inner_loop)
                call = GOKern(call, parent = inner_loop)
                inner_loop.addchild(call)
                # determine inner and outer loops space information from the
                # child kernel call. This is only picked up automatically (by
                # the inner loop) if the kernel call is passed into the inner
                # loop.
                inner_loop.iteration_space = call.iterates_over
                outer_loop.iteration_space = inner_loop.iteration_space
                inner_loop.field_space = call.arguments.iteration_space_arg().function_space
                outer_loop.field_space = inner_loop.field_space
                inner_loop.field_name = call.arguments.iteration_space_arg().name
                outer_loop.field_name = inner_loop.field_name
        Node.__init__(self, children = sequence)

class GOLoop(Loop):
    ''' The GOcean specific Loop class. This passes the GOcean specific
        single loop information to the base class so it creates the one we
        require. Adds a GOcean specific setBounds method which tells the loop
        what to iterate over. Need to harmonise with the topology_name method
        in the Dynamo api. '''
    def __init__(self, call = None, parent = None, variable_name = "",
                 topology_name = ""):
        Loop.__init__(self, GOInf, GOKern, call = call, parent = parent,
                      valid_loop_types = ["inner", "outer"])

    def gen_code(self,parent):

        if self._loop_type == "inner":
            self._variable_name = "i"
        elif self._loop_type == "outer":
            self._variable_name = "j"

        if self.field_space=="every":
            from f2pygen import DeclGen, AssignGen
            dim_var = DeclGen(parent, datatype = "INTEGER",
                           entity_decls = [self._variable_name])
            parent.add(dim_var)

            # loop bounds
            self._start = "1"
            if self._loop_type == "inner":
                self._stop = "idim1"
                index = "1"
            elif self._loop_type == "outer":
                self._stop = "idim2"
                index = "2"
            new_parent, position = parent.start_parent_loop()
            dim_size = AssignGen(new_parent, lhs = self._stop,
                                 rhs = "SIZE("+self.field_name+"%data, "
                                 +index+")")
            new_parent.add(dim_size, position = ["before", position])

            dims = DeclGen(parent, datatype = "INTEGER",
                           entity_decls = [self._stop])
            parent.add(dims)

        else: # one of our spaces so use values provided by the infrastructure
            
            # loop bounds are pulled from the field object
            self._start= self.field_name
            self._stop = self.field_name

            if self._iteration_space.lower() == "internal_pts":
                self._start += "%internal"
                self._stop  += "%internal"
            elif self._iteration_space.lower() == "all_pts":
                self._start += "%whole"
                self._stop  += "%whole"
            else:
                raise GenerationError("Unrecognised iteration space, {0}. Cannot generate loop bounds.".format(self._iteration_space))

            if self._loop_type == "inner":
                self._start += "%xstart"
                self._stop  += "%xstop"
            elif self._loop_type == "outer":
                self._start += "%ystart"
                self._stop  += "%ystop"

        Loop.gen_code(self, parent)
        

class GOInf(Inf):
    ''' A GOcean specific infrastructure call factory. No infrastructure
        calls are supported in GOcean at the moment so we just call the base
        class (which currently recognises the set() infrastructure call). '''
    @staticmethod
    def create(call, parent = None):
        ''' Creates a specific infrastructure call. Currently just calls
            the base class method. '''
        return(Inf.create(call, parent))
        
class GOKern(Kern):
    ''' Stores information about GOcean Kernels as specified by the Kernel
        metadata. Uses this information to generate appropriate PSy layer
        code for the Kernel instance. Specialises the gen_code method to
        create the appropriate GOcean specific kernel call. '''
    def __init__(self, call, parent = None):
        if False:
            self._arguments = GOKernelArguments(None, None) # for pyreverse
        Kern.__init__(self, GOKernelArguments, call, parent)

        # The different access types a field may have, ordered such that
        # the earlier in the list, the better it is for the purposes
        # of getting at a (read-only) grid property. e.g. some compilers
        # see a quantity that is written when a read-only array is pulled
        # from an object that has 'write' access. This then has 
        # implications for the optimisations that the compiler will perform.
        self._access_types = ["read","readwrite","write"]

    def local_vars(self):
        return []

    def _find_grid_access(self):
        ''' Determine the best kernel argument from which to get 
            properties of the grid. For this, an argument must be a field
           (i.e. not a scalar) and must be supplied by the algorithm layer. 
           If possible it should also be a field that is read-only
           as otherwise compilers can get confused about data dependencies
           and refuse to SIMD vectorise. '''
        for access in self._access_types:
            for arg in self._arguments.args:
                if arg.type == "field" and arg.access.lower() == access:
                    return arg
        # We failed to find any kernel argument which could be used
        # to access the grid properties. This will only be a problem
        # if the kernel requires a grid-property argument.
        return None

    def gen_code(self, parent):
        ''' Generates GOcean v1.0 specific psy code for a call to the dynamo
            kernel instance. '''
        from f2pygen import CallGen, UseGen

        # Before we do anything else, go through the arguments and 
        # determine the best one from which to obtain the grid properties.
        grid_arg = self._find_grid_access()

        # A GOcean 1.0 kernel always requires the [i,j] indices of the
        # grid-point that is to be updated
        arguments = ["i", "j"]
        for arg in self._arguments.args:

            if isinstance(arg, GOKernelArgument):
                if arg.type == "scalar":
                    # Scalar arguments require no de-referencing
                    arguments.append(arg.name)
                elif arg.type == "field":
                    # Field objects are Fortran derived-types
                    arguments.append(arg.name + "%data")

            elif isinstance(arg, GOKernelGridArgument):
                # Argument is a property of the grid which we can access via
                # the grid member of any field object.
                # We use the most suitable field as chosen above.
                if grid_arg is None:
                    raise GenerationError("Error: kernel {0} requires "
                                          "grid property {1} but does not "
                                          "have any arguments that are "
                                          "fields".format(self._name, arg.name))
                else:
                    arguments.append(grid_arg.name+"%grid%"+arg.name)

        parent.add(CallGen(parent, self._name, arguments))
        parent.add(UseGen(parent, name = self._module_name, only = True,
                          funcnames = [self._name]))

class GOKernelArguments(Arguments):
    ''' Provides information about GOcean kernel call arguments collectively,
        as specified by the kernel argument metadata. This class ensures that
        initialisation is performed correctly. It also adds three '''
    def __init__(self, call, parent_call):
        if False:
            self._0_to_n = GOKernelArgument(None, None, None) # for pyreverse
        Arguments.__init__(self, parent_call)

        self._args = []
        # Loop over the kernel arguments obtained from the meta data
        for (idx, arg) in enumerate (call.ktype.arg_descriptors):
            # arg is a GO1p0Descriptor object
            if arg.type == "grid_property":
                # This is an argument supplied by the psy layer
                self._args.append(GOKernelGridArgument(arg))
            else:
                # This is a kernel argument supplied by the Algorithm layer
                self._args.append(GOKernelArgument(arg, call.args[idx],
                                                   parent_call))
        self._dofs = []
    @property
    def dofs(self):
        ''' Currently required for invoke base class although this makes no
            sense for GOcean. Need to refactor the invoke class and pull out
            dofs into the gunghoproto api '''
        return self._dofs

    def iteration_space_arg(self, mapping={}):
        if mapping != {}:
            my_mapping = mapping
        else:
            my_mapping = {"write":"write", "read":"read","readwrite":"readwrite", 
                          "inc":"inc"}
        arg = Arguments.iteration_space_arg(self,my_mapping)
        return arg

class GOKernelArgument(KernelArgument):
    ''' Provides information about individual GOcean kernel call arguments
        as specified by the kernel argument metadata. '''
    def __init__(self, arg, arg_info, call):

        self._arg = arg
        KernelArgument.__init__(self, arg, arg_info, call)
        self._grid_prop = arg.grid_prop

    @property
    def type(self):
        ''' Return the type of this kernel argument - whether it is a field, 
            a scalar or a grid_property (to be supplied by the PSy layer) '''
        return self._arg._type

    @property
    def grid_prop(self):
        return self._grid_prop

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

        # A dictionary giving the mapping from meta-data names for 
        # properties of the grid to their names in the Fortran grid_type.
        self._grid_properties = {"grid_area_t":"area_t",
                                 "grid_area_u":"area_u",
                                 "grid_area_v":"area_v",
                                 "grid_mask_t":"tmask",
                                 "grid_dx_t":"dx_t",
                                 "grid_dx_u":"dx_u",
                                 "grid_dx_v":"dx_v",
                                 "grid_dy_t":"dy_t",
                                 "grid_dy_u":"dy_u",
                                 "grid_dy_v":"dy_v",
                                 "grid_lat_u":"gphiu",
                                 "grid_lat_v":"gphiv",
                                 "grid_dx_const":"dx",
                                 "grid_dy_const":"dy"}

        if self._grid_properties.has_key(arg.grid_prop):
            self._name = self._grid_properties[arg.grid_prop]
        else:
            raise GenerationError("Unrecognised grid property specified. Expected one of {0} but found {1}".format(str(self._grid_properties.keys()), 
                                  arg.grid_prop))

        # This object always represents an argument that is a grid_property
        self._type = "grid_property"

    @property
    def name(self):
        return self._name

    @property
    def type(self):
        return self._type

