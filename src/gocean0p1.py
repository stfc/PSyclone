from psyGen import PSy,Invokes,Invoke,Schedule,Loop,Kern,Arguments,Argument,Node

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
        from f2pygen import SubroutineGen,DeclGen
        # create the subroutine
        invoke_sub=SubroutineGen(parent,name=self.name,args=self.unique_args)
        self.schedule.genCode(invoke_sub)
        parent.add(invoke_sub)
        # add the subroutine argument declarations
        my_decl=DeclGen(invoke_sub,datatype="REAL",intent="inout",kind="wp",entity_decls=self.unique_args,dimension=":,:")
        invoke_sub.add(my_decl)

class GOSchedule(Schedule):
    def __init__(self,arg):
        Schedule.__init__(self,GODoubleLoop,GOInf,arg)

class GODoubleLoop(object):
    def __init__(self,call=None,parent=None,variable_name="",topology_name=""):
        self._outerLoop=GOLoop(call=None,parent=parent,variable_name="i",end="idim1")
        self._innerLoop=GOLoop(call=None,parent=self._outerLoop,variable_name="j",end="idim2")
        self._outerLoop.addchild(self._innerLoop)
        self._call=GOKern(call,parent=self._innerLoop)
        self._innerLoop.addchild(self._call)
    def genCode(self,parent):
        from f2pygen import DeclGen,AssignGen
        dims=DeclGen(parent,datatype="INTEGER",entity_decls=["idim1","idim2"])
        parent.add(dims)
        fieldName=self._call.arguments.args[0].name # arbitrarily choose 1st argument
        dim1=AssignGen(parent,lhs="idim1",rhs="SIZE("+fieldName+", 1)")
        parent.add(dim1)
        dim2=AssignGen(parent,lhs="idim2",rhs="SIZE("+fieldName+", 2)")
        parent.add(dim2)
        self._outerLoop.genCode(parent)

class GOLoop(Loop):
    def __init__(self,call=None,parent=None,variable_name="",topology_name="topology",start="1",end="n"):
        Loop.__init__(self,GOInf,GOKern,call,parent,variable_name,topology_name)
        self._start=start
        self._stop=end
        self._step=""

class GOInf(Loop):
    @staticmethod
    def create(call,parent=None):
        return(Inf.create(call,parent))
        
class GOKern(Kern):
    def __init__(self,call,parent=None):
        Kern.__init__(self,GOKernelArguments,call,parent)
    def genCode(self,parent):
        from f2pygen import CallGen,UseGen
        arguments=["i","j"]
        for arg in self._arguments.args:
            arguments.append(arg.name)
        parent.add(CallGen(parent,self._name,arguments))
        parent.add(UseGen(parent,name=self._module_name,only=True,funcnames=[self._name]))

class GOKernelArguments(Arguments):
    def __init__(self,call,parentCall):
        self._args=[]
        for (idx,arg) in enumerate (call.ktype.arg_descriptors):
            self._args.append(GOKernelArgument(arg,call.args[idx],parentCall))
        self._dofs={}
    @property
    def dofs(self):
        return self._dofs

class GOKernelArgument(Argument):
    def __init__(self,arg,argInfo,call):
        if arg==None and argInfo==None and call==None:return
        Argument.__init__(self,call,argInfo,arg.access)

