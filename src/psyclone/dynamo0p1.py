#-------------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
#-------------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab

''' This module implements the PSyclone Dynamo 0.1 API by specialising the
    required base classes (PSy, Invokes, Invoke, Schedule, Loop, Kern,
    Arguments and Argument). '''

from psyGen import PSy, Invokes, Invoke, Schedule, Loop, Kern, Arguments, \
                   Argument, GenerationError

class DynamoPSy(PSy):
    ''' The Dynamo specific PSy class. This creates a Dynamo specific
        invokes object (which controls all the required invocation calls).
        Also overrides the PSy gen method so that we generate dynamo
        specific PSy module code. '''
    def __init__(self, invoke_info):
        PSy.__init__(self, invoke_info)
        self._invokes = DynamoInvokes(invoke_info.calls)
    @property
    def gen(self):
        '''
        Generate PSy code for the Dynamo0.1 api.

        :rtype: ast

        '''
        from f2pygen import ModuleGen, UseGen

        # create an empty PSy layer module
        psy_module = ModuleGen(self.name)
        # include the lfric module
        lfric_use = UseGen(psy_module, name = "lfric")
        psy_module.add(lfric_use)
        # add all invoke specific information
        self.invokes.gen_code(psy_module)
        # inline kernel subroutines if requested
        self.inline(psy_module)
        return psy_module.root

class DynamoInvokes(Invokes):
    ''' The Dynamo specific invokes class. This passes the Dynamo specific
        invoke class to the base class so it creates the one we require. '''
    def __init__(self, alg_calls):
        if False:
            self._0_to_n = DynInvoke(None, None) # for pyreverse
        Invokes.__init__(self, alg_calls, DynInvoke)

class DynInvoke(Invoke):
    ''' The Dynamo specific invoke class. This passes the Dynamo specific
        schedule class to the base class so it creates the one we require.
        Also overrides the gen_code method so that we generate dynamo
        specific invocation code. '''
    def __init__(self, alg_invocation, idx):
        if False:
            self._schedule = DynSchedule(None) # for pyreverse
        Invoke.__init__(self, alg_invocation, idx, DynSchedule)

    def gen_code(self, parent):
        ''' Generates Dynamo specific invocation code (the subroutine called
            by the associated invoke call in the algorithm layer). This
            consists of the PSy invocation subroutine and the declaration of
            its arguments.'''
        from f2pygen import SubroutineGen, TypeDeclGen
        # create the subroutine
        invoke_sub = SubroutineGen(parent, name = self.name,
                                   args = self.psy_unique_var_names)
        self.schedule.gen_code(invoke_sub)
        parent.add(invoke_sub)
        # add the subroutine argument declarations
        my_typedecl = TypeDeclGen(invoke_sub, datatype = "field_type",
                                  entity_decls = self.psy_unique_var_names,
                                  intent = "inout")
        invoke_sub.add(my_typedecl)


class DynSchedule(Schedule):
    ''' The Dynamo specific schedule class. This passes the Dynamo specific
        loop and infrastructure classes to the base class so it creates the
        ones we require. '''
    def __init__(self, arg):
        Schedule.__init__(self, DynKernCallFactory, DynBuiltInCallFactory, arg)


class DynLoop(Loop):
    ''' The Dynamo specific Loop class. This passes the Dynamo specific
        loop information to the base class so it creates the one we require.
        Creates Dynamo specific loop bounds when the code is being generated.
    '''
    def __init__(self, parent=None, loop_type=""):
        Loop.__init__(self, parent=parent,
                      valid_loop_types=["", "colours", "colour"])
        self.loop_type = loop_type

        # Work out the variable name from  the loop type
        if self._loop_type == "colours":
            self._variable_name = "colour"
        elif self._loop_type == "colour":
            self._variable_name = "cell"
        else:
            self._variable_name = "cell"

    def load(self, kern):
        ''' Load the state of this Loop using the supplied Kernel
        object. This method is provided so that we can individually
        construct Loop objects for a given kernel call. '''
        self._field = kern.arguments.iteration_space_arg()
        self._field_name = self._field.name
        self._field_space = self._field.function_space

    def gen_code(self,parent):
        ''' Work out the appropriate loop bounds and then call the base
            class to generate the code '''
        self._start = "1"
        if self._loop_type == "colours":
            self._stop = "ncolour"
        elif self._loop_type == "colour":
            self._stop = "ncp_ncolour(colour)"
        else:
            self._stop = self.field_name+"%get_ncell()"
        Loop.gen_code(self,parent)


class DynBuiltInCallFactory(object):
    ''' A Dynamo 0.1 specific Built-In call factory. No built-in
        calls are supported in Dynamo 0.1 so we do nothing. '''
    @staticmethod
    def create(call, parent=None):
        ''' Creates a specific built-in call. Currently does
        nothing '''
        return None


class DynKernCallFactory(object):
    ''' A Dynamo 0.1 specific kernel call factory. '''
    @staticmethod
    def create(call, parent=None):
        
        # Loop over cells
        cloop = DynLoop(parent=parent)

        # The kernel itself
        kern = DynKern()
        kern.load(call, cloop)

        # Add the kernel as a child of the loop
        cloop.addchild(kern)
        
        # Set-up the loop now we have the kernel object
        cloop.load(kern)

        # Return the outermost loop
        return cloop


class DynKern(Kern):
    ''' Stores information about Dynamo Kernels as specified by the Kernel
        metadata. Uses this information to generate appropriate PSy layer
        code for the Kernel instance. '''
    def __init__(self):
        if False:
            self._arguments = DynKernelArguments(None, None) # for pyreverse

    def load(self, call, parent=None):
        Kern.__init__(self, DynKernelArguments, call, parent)

    def local_vars(self):
        return ["cell","map"]

    def gen_code(self, parent):
        ''' Generates dynamo version 0.1 specific psy code for a call to
            the dynamo kernel instance. '''
        from f2pygen import CallGen, DeclGen, AssignGen, UseGen

        # TODO: we simply choose the first field as the lookup for the moment
        field_name = self.arguments.args[0].name

        # add a dofmap lookup using first field.
        # TODO: This needs to be generalised to work for multiple dofmaps
        parent.add(CallGen(parent, field_name+"%vspace%get_cell_dofmap",
                           ["cell", "map"]))
        parent.add(DeclGen(parent, datatype = "integer",
                           entity_decls = ["cell"]))
        parent.add(DeclGen(parent, datatype = "integer", pointer = True,
                           entity_decls = ["map(:)"]))

        # create the argument list on the fly so we can also create
        # appropriate variables and lookups
        arglist = []
        arglist.append("nlayers")
        arglist.append("ndf")
        arglist.append("map")

        found_gauss_quad = False
        gauss_quad_arg = None
        for arg in self._arguments.args:
            if arg.requires_basis:
                basis_name = arg.function_space+"_basis_"+arg.name
                arglist.append(basis_name)
                new_parent, position = parent.start_parent_loop() 
                new_parent.add(CallGen(new_parent,
                                       field_name+"%vspace%get_basis",
                                       [basis_name]),
                               position = ["before",
                                           position])
                parent.add(DeclGen(parent, datatype = "real", kind = "dp",
                                   pointer = True,
                                   entity_decls = [basis_name+"(:,:,:,:,:)"]))
            if arg.requires_diff_basis:
                raise GenerationError("differential basis has not yet "
                                      "been coded")
            if arg.requires_gauss_quad:
                if found_gauss_quad:
                    raise GenerationError("found more than one gaussian "
                                          "quadrature in this kernel")
                found_gauss_quad = True
                gauss_quad_arg = arg
            dataref = "%data"
            arglist.append(arg.name+dataref)

        if found_gauss_quad:
            gq_name = "gaussian_quadrature"
            arglist.append(gauss_quad_arg.name+"%"+gq_name)

        # generate the kernel call and associated use statement
        parent.add(CallGen(parent, self._name, arglist))
        if not self.module_inline:
            parent.add(UseGen(parent, name=self._module_name,
                              only=True, funcnames=[self._name]))

        # declare and initialise the number of layers and the number
        # of degrees of freedom. Needs to be generalised.
        parent.add(DeclGen(parent, datatype = "integer",
                           entity_decls = ["nlayers", "ndf"]))
        new_parent, position = parent.start_parent_loop() 
        new_parent.add(AssignGen(new_parent, lhs = "nlayers",
                                    rhs = field_name+"%get_nlayers()"),
                          position = ["before", position])
        new_parent.add(AssignGen(new_parent, lhs = "ndf",
                                 rhs = field_name+"%vspace%get_ndf()"),
                       position = ["before", position])

class DynKernelArguments(Arguments):
    ''' Provides information about Dynamo kernel call arguments collectively,
        as specified by the kernel argument metadata. This class currently
        adds no additional functionality to its base class other than
        ensuring that initialisation is performed correctly. '''
    def __init__(self, call, parent_call):
        if False:
            self._0_to_n = DynKernelArgument(None, None, None) # for pyreverse
        Arguments.__init__(self, parent_call)
        self._args = []
        for (idx, arg) in enumerate (call.ktype.arg_descriptors):
            self._args.append(DynKernelArgument(arg, call.args[idx],
                                                parent_call))
        self._dofs = []

    def iteration_space_arg(self, mapping={}):
        if mapping != {}:
            my_mapping = mapping
        else:
            my_mapping = {"write":"gh_write", "read":"gh_read","readwrite":"gh_rw", "inc":"gh_inc"}
        arg = Arguments.iteration_space_arg(self,my_mapping)
        return arg

    @property
    def dofs(self):
        ''' Currently required for invoke base class although this makes no
            sense for dynamo. Need to refactor the invoke class and pull out
            dofs into the gunghoproto api '''
        return self._dofs

class DynKernelArgument(Argument):
    ''' Provides information about individual Dynamo kernel call arguments
        as specified by the kernel argument metadata. '''
    def __init__(self, arg, arg_info, call):
        self._arg = arg
        Argument.__init__(self, call, arg_info, arg.access)
    @property
    def function_space(self):
        ''' Returns the expected finite element function space for this
            argument as specified by the kernel argument metadata.'''
        return self._arg.function_space
    @property
    def requires_basis(self):
        ''' Returns true if the metadata for this argument specifies that
            its basis function values should be passed into the routine. '''
        if self._arg.basis.lower() == ".true.":
            return True
        if self._arg.basis.lower() == ".false.":
            return False
        raise GenerationError("error: basis is not set to .true. or .false.")
    @property
    def requires_diff_basis(self):
        ''' Returns true if the metadata for this argument specifies that
            its differential basis function values should be passed into
            the routine. '''
        if self._arg.diff_basis.lower() == ".true.":
            return True
        if self._arg.diff_basis.lower() == ".false.":
            return False
        raise GenerationError("error: diff_basis is not set to .true. "
                              "or .false.")
    @property
    def requires_gauss_quad(self):
        ''' Returns true if the metadata for this argument specifies that
            its gausian quadrature values should be passed into the
            routine. '''
        if self._arg.gauss_quad.lower() == ".true.":
            return True
        if self._arg.gauss_quad.lower() == ".false.":
            return False
        raise GenerationError("error: gaussian quadrature is not set to "
                              ".true. or .false.")
