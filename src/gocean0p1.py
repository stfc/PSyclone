from psyGen import PSy,Invokes,Invoke,Schedule,Loop,Kern,Arguments,Argument

class GOPSy(PSy):
    def __init__(self,invoke_info):
        PSy.__init__(self,invoke_info)
        self._invokes=GOInvokes(invoke_info.calls)
    @property
    def gen(self):
        '''
        Generate PSy code for the GOcean api.

        :rtype: ast

        '''
        from f2pygen import ModuleGen,UseGen

        # create an empty PSy layer module
        psy_module=ModuleGen(self.name)
        # include the kind_params module
        kp_use=UseGen(psy_module,name="kind_params")
        psy_module.add(kp_use)
        # add in the subroutines for each invocation
        self.invokes.genCode(psy_module)
        return psy_module.root

class GOInvokes(Invokes):
    def __init__(self,alg_calls):
        Invokes.__init__(self,alg_calls,GOInvoke)

class GOInvoke(Invoke):
    def __init__(self,alg_invocation,idx):
        Invoke.__init__(self,alg_invocation,idx,GOSchedule)

    def genCode(self,parent):
        from f2pygen import SubroutineGen
        # create the subroutine
        invoke_sub=SubroutineGen(parent,name=self.name,args=self.unique_args)
        self.schedule.genCode(invoke_sub)
        parent.add(invoke_sub)

class GOSchedule(Schedule):
    def __init__(self,arg):
        Schedule.__init__(self,GOLoop,GOInf,arg)

class GOLoop(Loop):
    def __init__(self,call=None,parent=None,variable_name="column",topology_name="topology"):
        Loop.__init__(self,GOInf,GOKern,call,parent,variable_name,topology_name)

class GOInf(Loop):
    @staticmethod
    def create(call,parent=None):
        return(Inf.create(call,parent))
        
class GOKern(Kern):
    def __init__(self,call,parent=None):
        Kern.__init__(self,GOKernelArguments,call,parent)

class GOKernelArguments(Arguments):
    def __init__(self,call,parentCall):
        self._arglist=""
        self._dofs={}
    @property
    def dofs(self):
        return self._dofs
    @property
    def arglist(self):
        return self._arglist


