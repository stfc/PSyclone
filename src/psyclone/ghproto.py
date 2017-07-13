#-------------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
#-------------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab

from psyGen import PSy,Invokes,Invoke,Schedule,Loop,Kern,Arguments,Argument,Inf

class GHProtoPSy(PSy):
    def __init__(self,invoke_info):
        PSy.__init__(self,invoke_info)
        self._invokes=GHProtoInvokes(invoke_info.calls)

    @property
    def gen(self):
        '''
        Generate PSy code for the GungHoProto api.

        :rtype: ast

        '''
        from f2pygen import ModuleGen,UseGen

        # create an empty PSy layer module
        psy_module=ModuleGen(self.name)

        # include the lfric module
        lfric_use=UseGen(psy_module,name="lfric",only="['field_type']")
        psy_module.add(lfric_use)

        # add in the subroutines for each invocation
        self.invokes.gen_code(psy_module)
        return psy_module.root

class GHProtoInvokes(Invokes):
    def __init__(self,alg_calls):
        Invokes.__init__(self,alg_calls,GHInvoke)

class GHInvoke(Invoke):
    def __init__(self,alg_invocation,idx):
        Invoke.__init__(self,alg_invocation,idx,GHSchedule)

class GHSchedule(Schedule):
    def __init__(self,arg):
        Schedule.__init__(self,GHLoop,GHInf,arg)

class GHLoop(Loop):
    def __init__(self,call=None,parent=None,variable_name="column",topology_name="topology"):
        Loop.__init__(self,GHInf,GHKern,call,parent,variable_name,topology_name)
        self._start="1"
        self._stop=topology_name+"%entity_counts({0})".format(self.children[0].iterates_over)
        self._step=""
        self._id="TBD"

class GHInf(Loop):
    @staticmethod
    def create(call,parent=None):
        return(Inf.create(call,parent))

class GHKern(Kern):
    def __init__(self,call,parent=None):
        Kern.__init__(self,GHKernelArguments,call,parent)

class GHKernelArguments(Arguments):
    ''' functionality for arguments associated with a gunghoproto api kernel call '''
    def __init__(self,call,parentCall):
        Arguments.__init__(self,parentCall)
        # kernels have metadata describing the expected arguments
        self._0toN=GHKernelArgument(None,None,None) # only here for pyreverse!
        for (idx,arg) in enumerate (call.ktype.arg_descriptors):
            self._args.append(GHKernelArgument(arg,call.args[idx],parentCall))
        self._arglist=[]
        self._dofs={}
        if call.ktype.iterates_over=="dofs":
            self._arglist.append("nLayers*ndofs")
        elif call.ktype.iterates_over=="cells":
            self._arglist.append("nLayers")
            for arg in self._args:
                if arg.ptype is not None:
                    dofmap=arg.ptype+"dofmap(:,column)"
                    if dofmap not in self._arglist:
                        self._arglist.append(dofmap)
                    if not arg.ptype in self._dofs:
                        self._dofs[arg.ptype]=[arg]
                    else:
                        self._dofs[arg.ptype].append(arg)
        else:
            print "Error, unsupported function space"
            raise Exception
        for arg in self._args:
            dataref="%data"
            if str(arg.element)=="r":
                dataref+="(1)" # scalar in kernel
            self._arglist.append(arg.name+dataref)
    @property
    def dofs(self):
        return self._dofs
    @property
    def arglist(self):
        ''' return a comma separated string with the required arguments.
            Will need indexing info at some point '''
        return self._arglist

class GHKernelArgument(Argument):
    ''' kernel routine argument for gunghoproto api'''
    def __init__(self,arg,argInfo,call):
        if arg==None and argInfo==None and call==None:return
        Argument.__init__(self,call,argInfo,arg.stencil)
        mapping={"dg * dg" : "p0", "cg1 * cg1" : "p1", "r" : None }
        self._stencil=arg.access
        self._fs_dimension=arg.function_space.dimension
        self._fs_element=arg.element
        try:
            self._ptype=mapping[str(self._fs_element)]
        except:
            raise GenerationError("Kernel metadata mapping '{0}' is not supported for variable '{2}'. Supported values are '{1}'".format(str(self._fs_element),str(mapping),self._name))
    @property
    def ptype(self):
        return self._ptype
    @property
    def element(self):
        return self._fs_element
    @property
    def stencil(self):
        return self._stencil
