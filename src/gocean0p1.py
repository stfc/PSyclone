''' This module implements the emerging PSyclone GOcean API by specialising
    the required base classes (PSy, Invokes, Invoke, Schedule, Loop, Kern,
    Inf, Arguments and KernelArgument). '''

from psyGen import PSy, Invokes, Invoke, Schedule, Loop, Kern, Arguments, \
                   KernelArgument, Inf

class GOPSy(PSy):
    ''' The GOcean specific PSy class. This creates a GOcean specific
        invokes object (which controls all the required invocation calls).
        Also overrides the PSy gen method so that we generate GOceaen
        specific PSy module code. '''
    def __init__(self, invoke_info):
        PSy.__init__(self, invoke_info)
        self._invokes = GOInvokes(invoke_info.calls)
    @property
    def gen(self):
        '''
        Generate PSy code for the GOcean api.

        :rtype: ast

        '''
        from f2pygen import ModuleGen, UseGen

        # create an empty PSy layer module
        psy_module = ModuleGen(self.name)
        # include the kind_params module
        kp_use = UseGen(psy_module, name = "kind_params_mod")
        psy_module.add(kp_use)
        # include the field_mod module in case we have any r-space variables
        fm_use = UseGen(psy_module, name = "field_mod",
                        only=["scalar_field_type"])
        psy_module.add(fm_use)
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
            not rspace). GOcean needs to kow this as we are dealing with
            arrays directly so need to declare them correctly. '''
        result=[]
        for call in self._schedule.calls():
            for arg in call.arguments.args:
                if not arg.is_literal and not arg.space.lower()=="r" and \
                   not arg.name in result:
                    result.append(arg.name)
        return result

    @property
    def unique_args_scalars(self):
        ''' find unique arguments that are scalars (defined as those that are
            rspace). GOcean needs to kow this as we are dealing with arrays
            directly so need to declare them correctly. '''
        result=[]
        for call in self._schedule.calls():
            for arg in call.arguments.args:
                if not arg.is_literal and arg.space.lower()=="r" and \
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
        self.schedule.gen_code(invoke_sub)
        parent.add(invoke_sub)
        # add the subroutine argument declarations for arrays
        if len(self.unique_args_arrays) > 0:
            my_decl_arrays = DeclGen(invoke_sub, datatype = "REAL",
                                     intent = "inout", kind = "wp",
                                     entity_decls = self.unique_args_arrays,
                                     dimension = ":,:")
            invoke_sub.add(my_decl_arrays)
        # add the subroutine argument declarations for scalars
        if len(self.unique_args_scalars) > 0:
            my_decl_scalars = TypeDeclGen(invoke_sub,
                                  datatype = "scalar_field_type",
                                  entity_decls = self.unique_args_scalars,
                                  intent = "inout")
            invoke_sub.add(my_decl_scalars)

class GOSchedule(Schedule):
    ''' The GOcean specific schedule class. This passes the GOcean specific
        loop and infrastructure classes to the base class so it creates the
        ones we require.'''
    def __init__(self, arg):
        Schedule.__init__(self, GODoubleLoop, GOInf, arg)
from psyGen import Node
class GODoubleLoop(Node):
    ''' A GOcean specific double loop class that supports the lat/lon loops
        required for direct addressing. Currently not sure whether this is
        a good solution or not. The alternative is to just have two GOLoops. '''
    def __init__(self, call = None, parent = None, variable_name = "",
                 topology_name = ""):
        self._outer_loop = GOLoop(call = None, parent = self,
                                  variable_name = "i")
        self._outer_loop.set_bounds(start="1",end="idim1")
        Node.__init__(self,children=[self._outer_loop],parent=parent)
        self._inner_loop = GOLoop(call = None, parent = self._outer_loop,
                                  variable_name = "j")
        self._inner_loop.set_bounds(start="1",end="idim2")
        self._outer_loop.addchild(self._inner_loop)
        self._call = GOKern(call, parent = self._inner_loop)
        self._inner_loop.addchild(self._call)
    def gen_code(self, parent):
        ''' Creates the required loop structure including variable names. If
            the iteration space is for all elements (every) then the bounds
            are the size of the array, otherwise an infrastructure look-up
            is used based on the grid position that the loops iterates
            over. '''
        from f2pygen import DeclGen, AssignGen, UseGen
        arg_space = self._call.arguments.iteration_space_type()
        if arg_space == "every":
            # access all elements so use the size of the input data
            dim1_name = "idim1"
            dim2_name = "idim2"
            dims = DeclGen(parent, datatype = "INTEGER",
                           entity_decls = [dim1_name, dim2_name])
            parent.add(dims)
            # choose iteration space owner as the field name.
            field_name = self._call.arguments.iteration_space_owner_name()
            dim1 = AssignGen(parent, lhs = dim1_name,
                             rhs = "SIZE("+field_name+", 1)")
            parent.add(dim1)
            self._outer_loop.set_bounds("1", dim1_name)
            dim2 = AssignGen(parent, lhs = dim2_name,
                             rhs = "SIZE("+field_name+", 2)")
            parent.add(dim2)
            self._inner_loop.set_bounds("1", dim2_name)
        else: # one of our spaces so use values provided by the infrastructure
            use = UseGen(parent, "topology_mod", only = [arg_space])
            parent.add(use)
            self._outer_loop.set_bounds(arg_space+"%istart",
                                        arg_space+"%istop")
            self._inner_loop.set_bounds(arg_space+"%jstart",
                                        arg_space+"%jstop")
        self._outer_loop.gen_code(parent)

class GOLoop(Loop):
    ''' The GOcean specific Loop class. This passes the GOcean specific
        single loop information to the base class so it creates the one we
        require. Adds a GOcean specific setBounds method which tells the loop
        what to iterate over. Need to harmonise with the topology_name method
        in the Dynamo api. '''
    def __init__(self, call = None, parent = None, variable_name = "",
                 topology_name = "topology"):
        Loop.__init__(self, GOInf, GOKern, call, parent, variable_name,
                      topology_name)
        self._start = None
        self._stop = None
        self._step = None
    def set_bounds(self, start, end, step = ""):
        ''' The loop does not know what to iterate over. This method allows an
            external object to provide the start, end, and step of the loop.
        '''
        self._start = start
        self._stop = end
        self._step = step

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
    def gen_code(self, parent):
        ''' Generates GOcean specific psy code for a call to the dynamo
            kernel instance. '''
        from f2pygen import CallGen, UseGen
        arguments = ["i", "j"]
        for arg in self._arguments.args:
            if arg.space.lower() == "r":
                arguments.append(arg.name + "%data")
            else:
                arguments.append(arg.name)
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
        for (idx, arg) in enumerate (call.ktype.arg_descriptors):
            self._args.append(GOKernelArgument(arg, call.args[idx],
                                               parent_call))
        self._dofs = []
    @property
    def dofs(self):
        ''' Currently required for invoke base class although this makes no
            sense for GOcean. Need to refactor the invoke class and pull out
            dofs into the gunghoproto api '''
        return self._dofs
    def iteration_space_type(self):
        ''' Returns the type of an argument that determines the kernels
            iteration space. Uses a mapping from the GOcean terminology to
            the one used by the base class. '''
        mapping = {"read":"read", "write":"write", "readwrite":"readwrite"}
        return Arguments.it_space_type(self, mapping)
    def iteration_space_owner_name(self):
        ''' Returns the name of an argument that determines the kernels
            iteration space. Uses a mapping from the GOcean terminology to
            the one used by the base class. '''
        mapping = {"read":"read", "write":"write", "readwrite":"readwrite"}
        return Arguments.it_space_owner_name(self, mapping)

class GOKernelArgument(KernelArgument):
    ''' Provides information about individual GOcean kernel call arguments
        as specified by the kernel argument metadata. Only passes information
        onto the base class. '''
    def __init__(self, arg, arg_info, call):
        KernelArgument.__init__(self, arg, arg_info, call)
