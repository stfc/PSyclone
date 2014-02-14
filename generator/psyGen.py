# Copyright 2013 STFC, all rights reserved
from f2pygen import ModuleGen,SubroutineGen,SelectionGen,AssignGen,CallGen,DoGen,UseGen,DeclGen,TypeDeclGen

class GenerationError(Exception):
    def __init__(self, value):
        self.value = "Generation Error: "+value
    def __str__(self):
        return repr(self.value)

class Argument(object):
    def __init__(self,arg,name,call):
        mapping={"dg * dg" : "p0", "cg1 * cg1" : "p1", "r" : None }
        self._name=name
        self._stencil=arg.access
        self._access=arg.stencil
        self._fs_dimension=arg.function_space.dimension
        self._fs_element=arg.element
        self._call=call
        try:
            self._ptype=mapping[str(self._fs_element)]
        except:
            raise GenerationError("Kernel metadata mapping '{0}' is not supported for variable '{2}'. Supported values are '{1}'".format(str(self._fs_element),str(mapping),name))
    @property
    def ptype(self):
        return self._ptype

    @property
    def name(self):
        return self._name

    @property
    def element(self):
        return self._fs_element

    @property
    def call(self):
        return self._call

    @property
    def access(self):
        return self._access

    @property
    def stencil(self):
        return self._stencil

class InfArguments(object):
    def __init__(self,call,my_call):
        self._arglist=call.args
        self._dofs={}
    @property
    def arglist(self):
        return self._arglist
    @property
    def dofs(self):
        #print "InfArguments returning dofs"
        return self._dofs
    
        
class Arguments(object):
    def __init__(self,call,my_call):
        nargs=call.ktype.nargs
        arglist=call.args
        self._args=[]
        for (idx,arg) in enumerate (call.ktype.arg_descriptors):
            self._args.append(Argument(arg,call.args[idx],my_call))
        self._arglist=[]
        self._dofs={}
        if my_call.iterates_over=="dofs":
            self._arglist.append("nLayers*ndofs")
        elif my_call.iterates_over=="cells":
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
            print "Error, unsupported space"
            exit(1)
        for arg in self._args:
            dataref="%data"
            if str(arg.element)=="r":
                dataref+="(1)" # scalar in kernel
            self._arglist.append(arg.name+dataref)
            
    @property
    def args(self):
        return self._args

    @property
    def arglist(self):
        return self._arglist

    @property
    def dofs(self):
        return self._dofs
        
class InfCall(object):
    def __init__(self,call):
        self._arguments=InfArguments(call,self)
        self._iterates_over="TBD"
        self._name=call.func_name

    @property
    def module_name(self):
        return self._name
    @property
    def func_name(self):
        return self._name
    @property
    def arguments(self):
        return self._arguments
    def gen(self,parent):
        return CallGen(parent,self._name,self._arguments.arglist)
    @property
    def iterates_over(self):
        return self._iterates_over

class Call(object):
    def __init__(self,call):
        nargs=call.ktype.nargs
        self._iterates_over=call.ktype.iterates_over
        name=call.ktype.name
        self._procedure=call.ktype.procedure
        try:
            self._arguments=Arguments(call,self)
        except:
            print "Error: in call '{0}'".format(call.ktype.name)
            raise

    @property
    def arguments(self):
        return self._arguments
    def gen(self,parent):
        return CallGen(parent,self._procedure.name,self._arguments.arglist)
    @property
    def iterates_over(self):
        return self._iterates_over

class Calls:
    def __init__(self,kcalls):
        self._kern_call={}
        for call in kcalls:
            import parse
            if isinstance(call,parse.InfCall):
                #print "InfCall"
                self._kern_call[call]=InfCall(call)
            else: # assumption that this is a KernelCall object
                self._kern_call[call]=Call(call)
            #for arg in kern_call[call].arguments.args:
            #    print arg.name, arg.ptype
            #print kern_call[call].arguments.dofs

        # dofs calculations should not be here surely?
        self._dofs={}
        for call in self._kern_call.itervalues():
            #print type(call.arguments.dofs)
            for dof in call.arguments.dofs:
                if not dof in self._dofs:
                    self._dofs[dof]=call.arguments.dofs[dof]
                else:
                    self._dofs[dof].append(call.arguments.dofs[dof])

    def getCall(self,call):
        return self._kern_call[call]

    @property
    def dofs(self):
        return self._dofs

def psyGen(alg_calls,psyName="psy",infName="inf",kernName="kern"):

    # create an empty PSy layer module
    psy_module=ModuleGen(psyName)

    # include the lfric module
    lfric_use=UseGen(psy_module,name="lfric")
    psy_module.add(lfric_use)

    # iterate over invocations
    for psy_subroutine in alg_calls.values():

        # create kernel call information
        calls=Calls(psy_subroutine.kcalls)

        # add a subroutine for each invocation (these are called by the algorithm layer)
        if  psy_subroutine.name is not None:
          sub_name=psy_subroutine.name
        else:
          sub_name="invoke_"+str(alg_calls.values().index(psy_subroutine))
        invoke_sub=SubroutineGen(psy_module,name=sub_name,args=psy_subroutine.unique_args)
        psy_module.add(invoke_sub)

        # declare field-type, column topology and function-space types
        column_topology_name="topology"
        my_typedecl=TypeDeclGen(invoke_sub,datatype="field_type",entity_decls=psy_subroutine.unique_args,intent="inout")
        invoke_sub.add(my_typedecl)
        my_typedecl=TypeDeclGen(invoke_sub,datatype="ColumnTopology",entity_decls=[column_topology_name],pointer=True)
        invoke_sub.add(my_typedecl)

        # declare any basic types required
        my_decl=DeclGen(invoke_sub,datatype="integer",entity_decls=["nlayers"])
        invoke_sub.add(my_decl)

        # for each dofmap required in our subroutine
        for (idx,dof) in enumerate(calls.dofs):
            # choose the *first argument in the list* for this mapping (we could choose any in the list as they all have the same dofmap)
            arg=calls.dofs[dof][0]
            # declare a type select clause which is used to map from a base class to FunctionSpace_type
            type_select=SelectionGen(invoke_sub,expr=arg.name+"_space=>"+arg.name+"%function_space",typeselect=True)
            invoke_sub.add(type_select)

            my_typedecl=TypeDeclGen(invoke_sub,datatype="FunctionSpace_type",entity_decls=[arg.name+"_space"],pointer=True)
            invoke_sub.add(my_typedecl)

            content=[]
            if idx==0:
                # use the first model to provide nlayers
                # *** assumption that all fields operate over the same number of layers
                assign1=AssignGen(type_select,lhs="topology",rhs=arg.name+"_space%topology",pointer=True)
                assign2=AssignGen(type_select,lhs="nlayers",rhs="topology%layer_count()")
                content.append(assign1)
                content.append(assign2)
            iterates_over=arg.call.iterates_over
            #if iterates_over!="cells":
            #    print type(iterates_over)
            #    print iterates_over
            stencil=arg.stencil
            assign3=AssignGen(type_select,lhs=dof+"dofmap",rhs=arg.name+"_space%dof_map("+iterates_over+", "+stencil+")",pointer=True)
            content.append(assign3)
            type_select.addcase(["FunctionSpace_type"],content=content)
            # declare our dofmap
            my_decl=DeclGen(invoke_sub,datatype="integer",entity_decls=[dof+"dofmap(:,:)"],pointer=True)
            invoke_sub.add(my_decl)

        # declare our do-loop variable once for all loops
        iterator_name="column"
        my_decl=DeclGen(invoke_sub,datatype="integer",entity_decls=[iterator_name])
        invoke_sub.add(my_decl)

        # iterate over specified kernels
        for call in psy_subroutine.kcalls:

            import parse
            if isinstance(call,parse.InfCall): # InfCall
                # include required infrastructure
                my_use=UseGen(psy_module,name=call.module_name,only=True,funcnames=[call.func_name])
            else: # assumption that this is a KernelCall object
                # include required kernels
                my_use=UseGen(psy_module,name=call.module_name,only=True,funcnames=[call.ktype.procedure.name])
            #print "HELLO",type(my_use.root)
            psy_module.add(my_use)

            # add a do-loop and declare variable
            do=DoGen(invoke_sub,iterator_name,"1",column_topology_name+"%entity_counts({0})".format(iterates_over))
            invoke_sub.add(do)

            #add kernel call to the do loop
            do.add(calls.getCall(call).gen(do))

    return psy_module.root
