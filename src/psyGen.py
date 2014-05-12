# Copyright 2013 STFC, all rights reserved
import abc

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
    >>> from psyGen import PSy
    >>> psy=PSy(info)
    >>> print(psy.gen)

    '''
    def __init__(self,invoke_info,api=""):

        if api=="":
            from config import DEFAULTAPI
            api=DEFAULTAPI
        else:
            from config import SUPPORTEDAPIS
            if api not in SUPPORTEDAPIS:
                raise GenerationError("PSy: Unsupported API '{0}' specified. Supported types are {1}.".format(api, SUPPORTEDAPIS))
        if api in ["dynamo0.1","gocean"]:
            raise GenerationError("PSy code generation does not yet support API {0}, sorry".format(api))

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
        self._0toN=Invoke(None,None) # only here for pyreverse!
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

class Dependencies(object):
    def __init__(self,thisArg):
        self._arg=thisArg
        self._precedes=[]
        self._follows=[]
    def set(self):
        if self.isLiteral:
            pass
        else:
            for following_call in self._call.followingCalls:
                for argument in following_call.arguments._args:
                    if argument.name==self._name:
                        self._addFollows(argument)
            for preceding_call in self._call.precedingCalls:
                for argument in preceding_call.arguments._args:
                    if argument.name==self._name:
                        self._addPrecedes(argument)

    def addPrecedes(self,obj):
        self._precedes.append(obj)
    def addFollows(self,obj):
        self._follows.append(obj)
    def getPrecedes():
        return self._precedes
    def getFollows():
        return self._follows

class DependenciesOLD(object):
    from collections import defaultdict
    def __init_(self):
        self._precedes=defaultdict(list)
        self._follows=defaultdict(list)
    def addPrecedes(self,obj1,obj2):
        # obj1 precedes obj2
        self._precedes[obj1].append(obj2)
        # obj2 follows obj1
        self._follows[obj2].append(obj1)
    def addFollows(self,obj1,obj2):
        self.add_precedes(obj2,obj1)
    def getPrecedes(obj1):
        return self._precedes[obj1]
    def getFollows(obj1):
        return self._follows[obj1]

class Invoke(object):

    def __str__(self):
        return self._name+"("+self.unique_args+")"
    def __init__(self,alg_invocation,idx):

        if alg_invocation==None and idx==None: return

        # create the schedule
        self._schedule=Schedule(alg_invocation.kcalls)

        # Set up the ordering constraints between the calls in the schedule
        # Obviously the schedule must be created first
        for call in self._schedule.calls():
            call.setConstraints()

        # create a name for the call if one does not already exist
        if alg_invocation.name is not None:
            self._name = alg_invocation.name
        elif len(alg_invocation.kcalls)==1 and alg_invocation.kcalls[0].type=="kernelCall":
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

    def gen(self):
        from f2pygen import ModuleGen
        module=ModuleGen("container")
        self.genCode(module)
        return module.root

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

class Node(object):
    ''' baseclass for a node in a schedule '''

    def __init__(self,children=[],parent=None):
        self._children=children
        self._parent=parent

    def __str__(self):
        raise NotImplementedError("Please implement me")

    @property
    def children(self):
        return self._children

    @property
    def parent(self):
        return self._parent

    @property
    def position(self):
        if self.parent is None: return 0
        return self.parent.children.index(self)

        current=self.root
        position=0

    @property
    def absPosition(self):
        ''' Find my position in the schedule. Needs to be computed dynamically as my position may change. '''

        if self.root==self: return 0
        found,position=self._findPosition(self.root.children,0)
        if not found: raise Exception("Error in search for my position in the tree")
        return position

    def _findPosition(self,children,position):
        ''' Recurse through the tree depth first returning position if found.'''
        for child in children:
            position+=1
            if child==self:
                return True,position
            if isinstance(child,Loop):
                found,position=self._findPosition(child.children,position)
                if found: return True,position
        return False,position

    @property
    def root(self):
        node=self
        while node.parent is not None:
            node=node.parent
        return node

    def sameRoot(self,node2):
        if self.root==node2.root: return True
        return False

    def sameParent(self,node2):
        if self.parent is None or node2.parent is None: return False
        if self.parent==node2.parent: return True
        return False

    def walk(self,children,mytype):
        ''' recurse through tree and return objects of mytype '''
        localList=[]
        for child in children:
            if isinstance(child,mytype):
                localList.append(child)
            if isinstance(child,Loop):
                localList+=self.walk(child.children,mytype)
        return localList

    def calls(self):
        ''' return all calls in this schedule '''
        return self.walk(self.root.children,Call)

    @property
    def followingCalls(self):
        ''' return all calls after me in the schedule '''
        allcalls=self.calls()
        position=allcalls.index(self)
        return allcalls[position+1:]

    @property
    def precedingCalls(self):
        ''' return all calls before me in the schedule '''
        allcalls=self.calls()
        position=allcalls.index(self)
        return allcalls[:position-1]

    def kernCalls(self):
        ''' return all kernel calls in this schedule '''
        #print "looking for kernCalls"
        return self.walk(self._children,Kern)

    def infCalls(self):
        ''' return all infrastructure calls in this schedule '''
        return self.walk(self._children,Inf)

    def loops(self):
        ''' return all loops currently in this schedule '''
        return self.walk(self._children,Loop)

    def genCode(self):
        raise NotImplementedError("Please implement me")

class Schedule(Node):

    ''' Stores schedule information for an invocation call. Schedules can be optimised using transformations.
        
        >>> from parse import parse
        >>> ast,info=parse("algorithm.f90")
        >>> from psyGen import PSy
        >>> psy=PSy(info)
        >>> invokes=psy.invokes
        >>> invokes.names
        >>> invoke=invokes.get("name")
        >>> schedule=invoke.schedule
        >>> print schedule

    '''

    def tkinter_delete(self):
        for entity in self._children:
            entity.tkinter_delete()

    def tkinter_display(self,canvas,x,y):
        yOffset=0
        for entity in self._children:
            entity.tkinter_display(canvas,x,y+yOffset)
            yOffset=yOffset+entity.height
        
    def __init__(self,alg_calls=[]):
            
        # we need to separate calls into loops (an iteration space really) and calls
        # so that we can perform optimisations separately on the two entities.
        sequence=[]
        from parse import InfCall
        for call in alg_calls:
            if isinstance(call,InfCall):
                sequence.append(Inf.create(call,parent=self))
            else:
                sequence.append(Loop(call,parent=self))
        #for call in alg_calls:
        #    sequence.append(Loop(call,parent=self))
        Node.__init__(self,children=sequence)

    def __str__(self):
        result=""
        for entity in self._children:
            result+=str(entity)
        return result

    def genCode(self,parent):
        for entity in self._children:
            entity.genCode(parent)

class Loop(Node):

    @property
    def height(self):
        callsHeight=0
        for child in self.children:
            callsHeight+=child.height
        return self._height+callsHeight

    def tkinter_delete(self):
        if self._shape is not None:
            assert self._canvas is not None, "Error"
            self._canvas.delete(self._shape)
        if self._text is not None:
            assert self._canvas is not None, "Error"
            self._canvas.delete(self._text)
        for child in self.children:
            child.tkinter_delete()

    def tkinter_display(self,canvas,x,y):
        self.tkinter_delete()
        self._canvas=canvas
        from Tkinter import ROUND
        name="Loop"
        minCallWidth=100
        maxCallsWidth=minCallWidth
        callsHeight=0
        for child in self.children:
            callsHeight+=child.height
            maxCallsWidth=max(maxCallsWidth,child.width)

        self._shape = canvas.create_polygon(x,y, \
                                                x+self._width+maxCallsWidth,y, \
                                                x+self._width+maxCallsWidth,y+self._height, \
                                                x+self._width,y+self._height, \
                                                x+self._width,y+self._height+callsHeight, \
                                                x,y+self._height+callsHeight, \
                                                outline="red",fill="green",width=2,activeoutline="blue",joinstyle=ROUND)
        self._text = canvas.create_text(x+(self._width+maxCallsWidth)/2, y+self._height/2, text=name)

        callHeight=0
        for child in self.children:
            child.tkinter_display(canvas,x+self._width,y+self._height+callHeight)
            callHeight=callHeight+child.height

    def __init__(self,call=None,parent=None,variable_name="column",topology_name="topology"):

        children=[]
        # we need to determine whether this is an infrastructure or kernel call
        # so our schedule can do the right thing.
        if call is not None:
            from parse import InfCall,KernelCall
            if isinstance(call,InfCall):
                children.append(Inf.create(call,parent=self))
            elif isinstance(call,KernelCall):
                children.append(Kern(call,parent=self))
            else:
                raise Exception
        Node.__init__(self,children=children,parent=parent)

        self._start="1"
        self._stop=topology_name+"%entity_counts({0})".format(self.children[0].iterates_over)
        self._step=""
        self._variable_name=variable_name
        self._id="TBD"

        # visual properties
        self._width=30
        self._height=30
        self._shape=None
        self._text=None
        self._canvas=None

    def __str__(self):
        return "Loop["+self._id+"]: "+self._variable_name+"="+self._id+" lower="+self._start+","+self._stop+","+self._step

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

class Call(Node):

    @property
    def width(self):
        return self._width

    @property
    def height(self):
        return self._height

    def tkinter_delete(self):
        if self._shape is not None:
            assert self._canvas is not None, "Error"
            self._canvas.delete(self._shape)
        if self._text is not None:
            assert self._canvas is not None, "Error"
            self._canvas.delete(self._text)

    def tkinter_display(self,canvas,x,y):
        self.tkinter_delete()
        self._canvas=canvas
        self._x=x
        self._y=y
        self._shape=self._canvas.create_rectangle(self._x,self._y,self._x+self._width,self._y+self._height,outline="red",fill="yellow",activeoutline="blue",width=2)
        self._text=self._canvas.create_text(self._x+self._width/2, self._y+self._height/2, text=self._name)

    def __init__(self,parent,call,name,arguments):
        Node.__init__(self,children=[],parent=parent)
        self._module_name=call.module_name
        self._arguments=arguments
        self._name=name

        # visual properties
        self._width=250
        self._height=30
        self._shape=None
        self._text=None
        self._canvas=None

    def setConstraints(self):
        # first set up the dependencies of my arguments
        self.arguments.setDependencies
        # TODO: set up constraints between calls
        
    @property
    def arguments(self):
        return self._arguments
    @property
    def name(self):
        return self._name
    def __str__(self):
        raise NotImplementedError("Call.__str__ should be implemented")
    def iterates_over(self):
        raise NotImplementedError("Call.iterates_over should be implemented")
    def genCode(self):
        raise NotImplementedError("Call.genCode should be implemented")

class Inf(object):
    ''' infrastructure call factory, Used to create a call specific class '''
    @staticmethod
    def create(call,parent=None):
        supportedCalls=["set"]
        if call.func_name not in supportedCalls:
            raise GenerationError("Unknown infrastructure call. Supported calls are {0} but found {1}".format(str(supportedCalls),call.func_name))
        if call.func_name=="set": return SetInfCall(call,parent)

class SetInfCall(Call):
    ''' the set infrastructure call '''
    def __init__(self,call,parent=None):
        assert call.func_name=="set", "Error"
        access=["write",None]
        Call.__init__(self,parent,call,call.func_name,InfArguments(call,self,access))
        #self._arguments=InfArguments(call,self,access)
    def __str__(self):
        return "set inf call"
    def genCode(self,parent):
        from f2pygen import AssignGen
        field_name=self._arguments.arglist[0]
        var_name=field_name+"%data"
        value=self._arguments.arglist[1]
        assign2=AssignGen(parent,lhs=var_name,rhs=value)
        parent.add(assign2)
        return

class Kern(Call):
    def __init__(self,call,parent=None):
        Call.__init__(self,parent,call,call.ktype.procedure.name,KernelArguments(call,self))
        self._iterates_over=call.ktype.iterates_over
        #self._arguments=KernelArguments(call,self)
    def __str__(self):
        return "kern call: "+self._name
    @property
    def iterates_over(self):
        return self._iterates_over
    def genCode(self,parent):
        from f2pygen import CallGen,UseGen
        parent.add(CallGen(parent,self._name,self._arguments.arglist))
        parent.add(UseGen(parent,name=self._module_name,only=True,funcnames=[self._name]))

class Arguments(object):
    ''' arguments abstract base class '''
    def __init__(self,parentCall):
        self._parentCall=parentCall
        self._args=[]
    @property
    def args(self):
        return self._args
    @property
    def arglist(self):
        raise NotImplementedError("Arguments:arglist() should be implemented by subclass")
    @property
    def argNames(self):
        raise NotImplementedError("Arguments:argNames() should be implemented by subclass")
    def setDependencies(self):
        for argument in self._args:
            argument.setDependencies()
        # TODO create a summary of dependencies

class KernelArguments(Arguments):
    ''' functionality for arguments associated with a kernel call '''
    def __init__(self,call,parentCall):
        Arguments.__init__(self,parentCall)
        # kernels have metadata describing the expected arguments
        self._0toN=KernelArgument(None,None,None) # only here for pyreverse!
        for (idx,arg) in enumerate (call.ktype.arg_descriptors):
            self._args.append(KernelArgument(arg,call.args[idx],parentCall))
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
    ''' arguments associated with an infrastructure call '''
    def __init__(self,callInfo,parentCall,access):
        Arguments.__init__(self,parentCall)
        self._0toN=InfArgument(None,None,None) # only here for pyreverse!
        for idx,arg in enumerate(callInfo.args):
            self._args.append(InfArgument(arg,parentCall,access[idx]))
    @property
    def args(self):
        return self._args
    @property
    def arglist(self):
        myarglist=[]
        for arg in self._args:
            myarglist.append(arg.name)
        return myarglist

class Argument(object):
    ''' argument base class '''
    def __init__(self,call,argInfo,access):
        self._dependencies=Dependencies(self)
        self._call=call
        self._name=argInfo.value
        self._form=argInfo.form
        self._isLiteral=argInfo.isLiteral()
        self._access=access
    def __str__(self):
        return self._name
    @property
    def name(self):
        return self._name
    @property
    def form(self):
        return self._form
    @property
    def isLiteral(self):
        return self._isLiteral
    @property
    def access(self):
        return self._access
    def setDependencies(self):
        writers=["WRITE","INC","SUM"]
        readers=["READ","INC"]
        for following_call in self._call.followingCalls:
            for argument in following_call.arguments.args:
                if argument.name==self._name:
                    if self.access in writers and argument.access in readers:
                        self._trueDependence.append(argument)
                    if self.access in readers and argument.access in writers:
                        self._antiDependence.append(argument)
    def hasTrueDependence():
        if len(self._trueDependence)>0: return True
        return False
    def hasAntiDependence():
        if len(self._antiDependence)>0: return True
        return False
    def hasDependence():
        if hasAntiDependence or hasTrueDependence: return True
        return False
    def trueDependencies():
        return self._trueDependence
    def antiDependencies():
        return self._antiDependence
    def dependencies():
        return trueDependencies+antiDependencies

class InfArgument(Argument):
    ''' infrastructure call argument '''
    def __init__(self,argInfo,call,access):
        if access==None and argInfo==None and call==None:return
        Argument.__init__(self,call,argInfo,access)
    
class KernelArgument(Argument):
    ''' kernel routine argument '''
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


class transformations(object):
    '''
    This class provides information about, and access, to the available transformations in this implementation of PSyclone. New transformations will be picked up automatically as long as they subclass the abstract Transformation class.

    For example:

    >>> from psyGen import transformations
    >>> t=transformations()
    >>> print t.list
    There is 1 transformation available:
      1: SwapTrans, A test transformation
    >>> t.getTransNum(1)
    >>> t.getTransName("SwapTrans")

'''

    def __init__(self,module=None,baseclass=None):
        ''' if module and/or baseclass are provided then use these else use the default module "transformations" and the default base_class "transformation"'''

        self._0toN=DummyTransformation() # only here for pyreverse!

        if module is None:
            # default to the transformation module
            import transformations
            module=transformations
        if baseclass is None:
            import psyGen
            baseclass=psyGen.transformation
        # find our transformations
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

class transformation(object):
    ''' abstract baseclass for a transformation. Uses the abc module so it can not be instantiated.
    '''
    __metaclass__ = abc.ABCMeta
    @abc.abstractproperty
    def name(self):
        return
    @abc.abstractmethod
    def apply(self):
        return schedule,memento

class DummyTransformation(transformation):
    def name(self):
        return
    def apply(self):
        return None,None
