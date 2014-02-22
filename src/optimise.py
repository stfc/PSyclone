# Copyright 2013 STFC, all rights reserved
class GenerationError(Exception):
    def __init__(self, value):
        self.value = "Generation Error: "+value
    def __str__(self):
        return repr(self.value)

class PSy(object):
    '''
    Manage and generate PSy code for a single algorithm file. Takes the invocation information output from the function :func:`parse.parse` as it's input and stores this is a way suitable for optimisation and code generation.

    :param FileInfo invoke_info: An object containing the required invocation information for code optimisation and generation. Produced by the function :func:`parse.parse`.

    For example:

    >>> from parse import parse
    >>> ast,info=parse("argspec.F90")
    >>> from optimise import PSy
    >>> psy=PSy(info)
    >>> print(psy.gen)

    '''
    def __init__(self,invoke_info):
        self._name=invoke_info.name
        ''' populate the object using the parser information '''
        self._invokes=Invokes(invoke_info.calls)
    def __str__(self):
        return "PSy"
    @property
    def invokes(self):
        return self._invokes
    @property
    def name(self):
        return "psy_"+self._name
    @property
    def gen(self):
        '''
        Generate PSy code.

        :rtype: ast

        '''
        from f2pygen import ModuleGen,UseGen

        # create an empty PSy layer module
        psy_module=ModuleGen(self.name)

        # include the lfric module
        lfric_use=UseGen(psy_module,name="lfric",only="['field_type']")
        psy_module.add(lfric_use)

        # add in the subroutines for each invocation
        self.invokes.genCode(psy_module)
        return psy_module.root

class Invokes(object):
    ''' Manage the invoke calls '''
    def __init__(self,alg_calls):
        self.invokeMap={}
        self.invokeList=[]
        for idx,alg_invocation in enumerate(alg_calls.values()):
            # create invoke before adding it to the map as the name supplied
            # by alg_invocation may be None (i.e. not supplied)
            myInvoke=Invoke(alg_invocation,idx)
            self.invokeMap[myInvoke.name]=myInvoke
            self.invokeList.append(myInvoke)
    def __str__(self):
        return "Invokes object containing "+str(self.names)
    @property
    def names(self):
        return self.invokeMap.keys()
    def get(self,invokeName):
        # add a try here for keyerror
        return self.invokeMap[invokeName]
    def genCode(self,parent):
        for invoke in self.invokeList:
            #print "invokes calling invoke"
            invoke.genCode(parent)

class Invoke(object):
    def __str__(self):
        return self._name+"("+self.unique_args+")"
    def __init__(self,alg_invocation,idx):

        # now create the schedule functionality
        self._schedule=Schedule(alg_invocation.kcalls)

        # create a name for the call if one does not already exist
        if alg_invocation.name is not None:
            self._name = alg_invocation.name
        elif len(alg_invocation.kcalls) and alg_invocation.kcalls[0].type=="KernelCall":
            # use the name of the kernel call
            self._name = "invoke_"+alg_invocation.kcalls[0].ktype.name
        else:
            # use the position of the invoke
            self._name = "invoke_"+str(idx)

        # work out the argument list. It needs to be unique and should
        # not contain any literals.
        self._unique_args=[]
        for call in alg_invocation.kcalls:
            for arg in call.args:
                if not arg.isLiteral(): # skip literals
                    if arg.value not in self._unique_args:
                        self._unique_args.append(arg.value)

        # work out the unique dofs required in this subroutine
        self._dofs={}
        for kernCall in self._schedule.kernCalls():
            dofs=kernCall.arguments.dofs
            for dof in dofs:
                if not self._dofs.has_key(dof):
                    # only keep the first occurence for the moment.
                    # we will need to change this logic at some point
                    # as we need to cope with writes determining the
                    # dofs that are used.
                    self._dofs[dof]=[kernCall,dofs[dof][0]]
    @property
    def name(self):
        return self._name
    @property
    def unique_args(self):
        return self._unique_args
    @property
    def schedule(self):
        return self._schedule

    def genCode(self,parent):
        from f2pygen import SubroutineGen,TypeDeclGen,DeclGen,SelectionGen,AssignGen
        # create the subroutine
        invoke_sub=SubroutineGen(parent,name=self.name,args=self.unique_args)
        # add the subroutine argument declarations
        my_typedecl=TypeDeclGen(invoke_sub,datatype="field_type",entity_decls=self.unique_args,intent="inout")
        invoke_sub.add(my_typedecl)
        # declare field-type, column topology and function-space types
        column_topology_name="topology"
        my_typedecl=TypeDeclGen(invoke_sub,datatype="ColumnTopology",entity_decls=[column_topology_name],pointer=True)
        invoke_sub.add(my_typedecl)
        # declare any basic types required
        my_decl=DeclGen(invoke_sub,datatype="integer",entity_decls=["nlayers"])
        invoke_sub.add(my_decl)

        for (idx,dof) in enumerate(self._dofs):
            call=self._dofs[dof][0]
            arg=self._dofs[dof][1]
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
            iterates_over=call.iterates_over
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

        # create the subroutine kernel call content
        self.schedule.genCode(invoke_sub)
        parent.add(invoke_sub)

class Schedule(object):
    def __init__(self,alg_calls):
        from parse import InfCall,KernelCall
        self._sequence=[]
        for call in alg_calls:
            if isinstance(call,InfCall):
                self._sequence.append(Inf(call))
            else:
                self._sequence.append(Loop(call))
    def __str__(self):
        return "Schedule ..."
    def kernCalls(self):
        ''' return all kernel calls in this schedule '''
        #print "looking for kernCalls"
        return self.walk(self._sequence,Kern)
    def infCalls(self):
        ''' return all infrastructure calls in this schedule '''
        return self.walk(self._sequence,Inf)
    def loops(self):
        ''' return all loops currently in this schedule '''
        return self.walk(self._sequence,Loop)

    def walk(self,children,mytype):
        ''' recurse through tree and return objects of mytype '''
        localList=[]
        for child in children:
            if isinstance(child,mytype):
                localList.append(child)
            if isinstance(child,Loop):
                localList+=self.walk(child.children,mytype)
        return localList

    def genCode(self,parent):
        for entity in self._sequence:
            entity.genCode(parent)

# this is the wrong name for this as a loop is not a statement.
# So what should it be called?
class Statement(object):
    def __init__(self):
        pass

class Loop(Statement):
    def __init__(self,call,variable_name="column",topology_name="topology"):
        self._children=[]
        from parse import InfCall,KernelCall
        if isinstance(call,InfCall):
            self._children.append(Inf(call))
        elif isinstance(call,KernelCall):
            self._children.append(Kern(call))
        else:
            raise Exception
        self._start="1"
        self._stop=topology_name+"%entity_counts({0})".format(self.children[0].iterates_over)
        self._step=None
        self._variable_name=variable_name
        self._id="TBD"
    def __str__(self):
        return "Loop: id="+self._id+" lower="+self._start+" upper="+self._stop+"step="+self._step+" var_name="+self._variable_name
    @property
    def children(self):
        return self._children
    def genCode(self,parent):
        if self._start=="1" and self._stop=="1": # no need for a loop
            self.call.genCode(parent)
        else:
            from f2pygen import DoGen,DeclGen
            do=DoGen(parent,self._variable_name,self._start,self._stop)
            for child in self.children:
                child.genCode(do)
            parent.add(do)
            my_decl=DeclGen(parent,datatype="integer",entity_decls=[self._variable_name])
            parent.add(my_decl)

class Call(Statement):
    def __init__(self):
        pass
    def genCode(self):
        pass
    def iterates_over(self):
        pass

class Inf(Call):
    def __init__(self,call):
        self._supportedCalls=["set"]
        self._module_name=call.module_name
        self._name=call.func_name
        self._arglist=call.args
    def genCode(self,parent):
        if self._name=="set":
            from f2pygen import AssignGen
            field_name=self._arglist[0].value
            var_name=field_name+"%data"
            value=self._arglist[1].value
            assign2=AssignGen(parent,lhs=var_name,rhs=value)
            parent.add(assign2)
        else:
            raise GenerationError("Unknown infrastructure call. Supported calls are {0} but found {1}".format(str(self._supportedCalls),self._name))
        return

#        mapping={"dg * dg" : "p0", "cg1 * cg1" : "p1", "r" : None }
#        self._dofs={}
#        for kcall in alg_invocation.kcalls:
#            #print "kernel interates_over",kcall.ktype.iterates_over
#            #print "kernel name",kcall.ktype.name
#            #print "kernel nargs",kcall.ktype.nargs
#            ##print kcall.ktype.procedure # the ast
#            for descriptor in kcall.ktype.arg_descriptors:
#                pass
#                #print "descriptor access",descriptor.access
#                #print "descriptor element",descriptor.element
#                #print "descriptor function_space dimension",descriptor.function_space.dimension
#                #print "descriptor function_space element",descriptor.function_space.element
#                #print "descriptor stencil",descriptor.stencil
#
#            for arg in kcall.args:
#                #print dir(kcall)
#                print "arg: The name as a string ",arg

class Kern(Call):
    def __init__(self,call):
        self._module_name=call.module_name
        self._name=call.ktype.procedure.name
        self._iterates_over=call.ktype.iterates_over
        self._arguments=KernelArguments(call)
    @property
    def arguments(self):
        return self._arguments
    @property
    def name(self):
        return self._name
    @property
    def iterates_over(self):
        return self._iterates_over
    def genCode(self,parent):
        from f2pygen import CallGen,UseGen
        parent.add(CallGen(parent,self._name,self._arguments.arglist))
        parent.add(UseGen(parent,name=self._module_name,only=True,funcnames=[self._name]))

class Arguments(object):
    ''' arguments abstract base class '''
    def __init__(self):
        pass
    def arglist(self):
        pass

class KernelArguments(Arguments):
    ''' functionality for arguments associated with a kernel call '''
    def __init__(self,call):
        # kernels have metadata describing the expected arguments
        self._args=[]
        for (idx,arg) in enumerate (call.ktype.arg_descriptors):
            self._args.append(KernelArgument(arg,call.args[idx]))
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

class InfArguments(Arguments):
    ''' functionality for arguments associated with an infrastructure call '''
    def __init__(self):
        pass
    @property
    def arglist(self):
        ''' return a comma separated string with the required arguments.
            Will need indexing info at some point '''
        self._arglist="iarg1,iarg2,iarg3"
        return self._arglist

class Argument(object):
    ''' argument abstract base class '''
    def __init__(self):
        pass

class KernelArgument(Argument):
    ''' kernel argument functionality '''
    def __init__(self,arg,argInfo):
        mapping={"dg * dg" : "p0", "cg1 * cg1" : "p1", "r" : None }
        self._form=argInfo.form
        self._name=argInfo.value
        self._stencil=arg.access
        self._access=arg.stencil
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
    def name(self):
        return self._name
    @property
    def element(self):
        return self._fs_element
    #@property
    #def call(self):
    #    return self._call
    @property
    def access(self):
        return self._access
    @property
    def stencil(self):
        return self._stencil


#class Transform(object):
#    def __init__(self):
#        pass
#class OpenMPParallel(Transform):
#    def __init__(self):
#        pass
#unddo
#redo

class transformations(object):
    '''
    This class provides information about, and access, to the available transformations in this implementation of PSyclone. New transformations will be picked up automatically as long as they subclass the abstract Transformation class.

    For example:

    >>> from optimise import transformations
    >>> t=transformations()
    >>> print t.list()
    There is 1 transformation available:
      1: testTrans, A test transformation
    >>> t.getTransNum(1)
    >>> t.getTransName("testTrans")

'''

    def __init__(self,module=None,baseclass=None):
        ''' if module and/or baseclass are provided then use these else use the default module "optimise" (this module) and the default base_class "transformation"'''
        import optimise
        if module is None:
            module=optimise
        if baseclass is None:
            baseclass=optimise.transformation
        # find our transformations.
        self._classes=self._find_subclasses(module,baseclass)

        # create our transformations
        self._objects=[]
        self._objmap={}
        for myclass in self._classes:
            myobject=myclass()
            self._objects.append(myobject)
            self._objmap[myobject.name]=myobject

    @property
    def list(self):
        ''' return a string with a human readable list of the available transformations '''
        import os
        if len(self._objects)==1:
            s="There is 1 transformation available:"
        else:
            s="There are {0} transformations available:".format(len(self._objects))
        s+=os.linesep
        for idx,myobject in enumerate(self._objects):
            s+="  "+str(idx+1)+": "+myobject.name+": "+str(myobject)+os.linesep
        return s

    @property
    def numTrans(self):
        ''' return the number of transformations available '''
        return len(self._objects)

    def getTransNum(self,number):
        ''' return the transformation with this number (use list() first to see available transformations) '''
        if number<1 or number>len(self._objects):
            raise GenerationError("Invalid transformation number supplied")
        return self._objects[number-1]

    def getTransName(self,name):
        ''' return the transformation with this name (use list() first to see available transformations) '''
        try:
            return self._objmap[name]
        except KeyError:
            raise GenerationError("Invalid transformation name supplied")

    def _find_subclasses(self,module,baseclass):
        ''' return a list of classes defined within the specified module that are a subclass of the specified baseclass. '''
        import inspect
        return [
            cls
                for name, cls in inspect.getmembers(module)
                    if inspect.isclass(cls) and issubclass(cls, baseclass) and not cls is baseclass
            ]

import abc
class transformation(object):
    ''' abstract baseclass for a transformation. Use of abc means it can not be instantiated. '''
    __metaclass__ = abc.ABCMeta
    @abc.abstractmethod
    def name(self):
        return

class testTrans(transformation):
    ''' A placeholder test transformation '''
    def __init__(self):
        pass
    def __str__(self):
        return "A test transformation"
    @property
    def name(self):
        return "testTrans"
