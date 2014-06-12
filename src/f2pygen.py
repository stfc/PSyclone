# Copyright 2013 STFC, all rights reserved

from fparser.readfortran import FortranStringReader

class BaseGen(object):
    def __init__(self,parent,root):
        self._parent=parent
        self._root=root
        self._children=[]
    @property
    def parent(self):
        return self._parent
    @property
    def children(self):
        return self._children
    @property
    def root(self):
        return self._root
    def add(self,new_object,position=["append"]):
        if position[0]=="auto":
            raise Exception('Error: BaseGen:add: auto option must be implemented by the sub class!')
        options=["append","first","after","before","insert"]
        if position[0] not in options:
            raise GenerationError("Error: BaseGen:add: supported positions are {0} but found {1}".format(str(options),position[0]))
        if position[0]=="append":
            self.root.content.append(new_object.root)
        elif position[0]=="first":
            self.root.content.insert(0,new_object.root)
        elif position[0]=="insert":
            index=position[1]
            self.root.content.insert(index,new_object.root)
        elif position[0]=="after":
            self.root.content.insert(self.root.content.index(position[1].root)+1,new_object.root)
        elif position[0]=="before":
            try:
                self.root.content.insert(self.root.content.index(position[1].root),new_object.root)
            except ValueError:
                print "inserting:"
                print str(new_object),str(new_object.root)
                print "parent is: "
                print str(self.root)
                print "looking for: "
                print str(position[1].root)
                exit(1)
        else:
            raise Exception("Error: BaseGen:add: internal error, should not get to here")
        self.children.append(new_object)
            
class TestAdd:
    ''' pyunit tests for adding code '''
    def test_add_before(self):
        ''' add the new code before a particular object '''
        module=ModuleGen(name="testmodule")
        subroutine=SubroutineGen(module,name="testsubroutine")
        module.add(subroutine)
        loop=DoGen(subroutine,"it","1","10")
        subroutine.add(loop)
        call=CallGen(subroutine,"testcall")
        subroutine.add(call,position=["before",loop])
        lines=str(module.root).splitlines()
        # the call should be inserted before the loop
        assert "SUBROUTINE testsubroutine" in lines[3]
        assert "CALL testcall" in lines[4]
        assert "DO it=1,10" in lines[5]
        
class TestModuleGen:
    ''' pyunit tests for the ModuleGen class '''
    def test_vanilla(self):
        module=ModuleGen()
        lines=str(module.root).splitlines()
        assert "MODULE" in lines[0]
        assert "IMPLICIT NONE" in lines[1]
        assert "CONTAINS" in lines[2]
        assert "END MODULE" in lines[3]
    def test_module_name(self):
        name="test"
        module=ModuleGen(name=name)
        assert "MODULE "+name in str(module.root)
    def test_no_contains(self):
        module=ModuleGen(name="test",contains=False)
        assert "CONTAINS" not in str(module.root)
    def test_no_implicit_none(implicitnone=False):
        module=ModuleGen(name="test",implicitnone=False)
        assert "IMPLICIT NONE" not in str(module.root)


class ModuleGen(BaseGen):
    def __init__(self,name="",contains=True,implicitnone=True):
        from fparser import api

        code='''\
module vanilla
'''
        if implicitnone:
            code+='''\
implicit none
'''
        if contains:
            code+='''\
contains
'''
        code+='''\
end module vanilla
'''
        tree=api.parse(code,ignore_comments=False)
        module=tree.content[0]
        module.name=name
        endmod=module.content[len(module.content)-1]
        endmod.name=name
        BaseGen.__init__(self,None,module)

    def add(self,content):
        if not content.parent==self:
            #print "****",content.root, "***",self._module
            # this is an error as a module can not have a parent
            # in the current version of f2pygen
            #print "**",type(content)
            raise Exception("The requested parent is "+str(type(content.parent))+" but it should be "+str(type(self.root)),str(type(self)))
        if not content.root.parent==self.root:
            content.root.parent=self.root
        import fparser
        if isinstance(content.root,fparser.statements.Use):
            index=0
        else:
            index=len(self.root.content)-1 # append
        self.root.content.insert(index,content.root)
        self._children.append(content)

def createmodule(name,contains=True,implicitnone=True):
    from fparser import api

    code='''\
module vanilla
'''
    if implicitnone:
        code+='''\
implicit none
'''
    if contains:
        code+='''\
contains
'''
    code+='''\
end module vanilla
'''
    tree=api.parse(code,ignore_comments=False)
    module=tree.content[0]
    module.name=name
    endmod=module.content[len(module.content)-1]
    endmod.name=name
    return module

def getLine(myString):
    reader=FortranStringReader(myString)
    reader.set_mode(True, False) # free form, strict
    myline=reader.next()
    return myline

def addexternal(names,parent):
    if names==[] : return None
    from fparser import api
    myline=getLine("external :: vanilla")
    from fparser.statements import External
    external=External(parent,myline)
    external.items=names
    idx=0
    parent.content.insert(idx,external)
    return external

class SubroutineGen(BaseGen):

    def __init__(self,parent,name="",args=[],index=None):
        from fparser import api
        reader=FortranStringReader("subroutine vanilla(vanilla_arg)\nend subroutine")
        reader.set_mode(True, True) # free form, strict
        subline=reader.next()
        endsubline=reader.next()

        from fparser.block_statements import Subroutine,EndSubroutine
        sub=Subroutine(parent.root,subline)
        sub.name=name
        sub.args=args
        endsub=EndSubroutine(sub,endsubline)
        sub.content.append(endsub)

        BaseGen.__init__(self,parent,sub)

    def add(self,content,position=["auto"]):
        # content may have been passed on so make me the parent
        if content.root.parent!=self.root:
            content.root.parent=self.root
        from fparser.typedecl_statements import TypeDeclarationStatement
        import fparser
        if position[0]!="auto": # position[0] is not 'auto' so the baseclass can deal with it
            BaseGen.add(self,content,position)
        else: # position[0]=="auto" so insert in a context sensitive way

            if isinstance(content,DeclGen) or isinstance(content,TypeDeclGen):
                # have I already been declared?
                for child in self._children:
                    if isinstance(child,DeclGen) or isinstance(child,TypeDeclGen):
                        for var_name in content.root.entity_decls[:]: # take a copy of the list as we are deleting elements of the original
                            for child_name in child.root.entity_decls:
                                if var_name.lower()==child_name.lower():
                                    content.root.entity_decls.remove(var_name)
                                    if len(content.root.entity_decls)==0:
                                        return # return as all variables in this declaration already exists
                # skip over any use statements
                index=0
                while isinstance(self.root.content[index],fparser.statements.Use):
                    index+=1
                # skip over any declarations which have an intent
                try:
                    intent=True
                    while intent:
                        intent=False
                        for attr in self.root.content[index].attrspec:
                            if attr.find("intent")==0:
                                intent=True
                                index+=1
                                break
                except AttributeError:
                    pass
                except Exception:
                    raise
            elif isinstance(content.root,fparser.statements.Use):
                # have I already been declared?
                for child in self._children:
                    if isinstance(child,UseGen):
                        if child.root.name == content.root.name:
                            # found an existing use with the same name
                            if not child.root.isonly and not content.root.isonly:
                                # both are generic use statements so skip this declaration
                                return
                            if child.root.isonly and not content.root.isonly:
                                # new use is generic and existing use is specific so we can safely add
                                pass
                            if not child.root.isonly and content.root.isonly:
                                # existing use is generic and new use is specific so we can skip this declaration
                                return
                            if child.root.isonly and content.root.isonly:
                                # see if the same names are specified.
                                for new_name in content.root.items[:]: # take a copy of the list as we are deleting elements of the original
                                    for existing_name in child.root.items:
                                        if existing_name.lower()==new_name.lower():
                                            content.root.items.remove(new_name)
                                            if len(content.root.items)==0:
                                                return
                index=0
            else:
                index=len(self.root.content)-1 
            self.root.content.insert(index,content.root)
            self._children.append(content)

def addsub(name,args,parent,index=None):
    from fparser import api

    reader=FortranStringReader("subroutine vanilla(vanilla_arg)\nend subroutine")
    reader.set_mode(True, True) # free form, strict
    subline=reader.next()
    endsubline=reader.next()

    from fparser.block_statements import Subroutine,EndSubroutine
    sub=Subroutine(parent,subline)
    sub.name=name
    sub.args=args
    if index is None: index=len(parent.content)-1 # append
    parent.content.insert(index,sub)

    endsub=EndSubroutine(sub,endsubline)
    sub.content.append(endsub)

    return sub

class CallGen(BaseGen):
    def __init__(self,parent,name="",args=[]):
        from fparser import api

        reader=FortranStringReader("call vanilla(vanilla_arg)")
        reader.set_mode(True, True) # free form, strict
        myline=reader.next()

        from fparser.block_statements import Call
        self._call=Call(parent.root,myline)
        self._call.designator=name
        self._call.items=args

        BaseGen.__init__(self,parent,self._call)

def addcall(name,args,parent,index=None):
    from fparser import api

    #TODO check parent is a valid class
    #<class 'fparser.block_statements.Subroutine'>

    reader=FortranStringReader("call vanilla(vanilla_arg)")
    reader.set_mode(True, True) # free form, strict
    myline=reader.next()

    from fparser.block_statements import Call
    call=Call(parent,myline)
    call.designator=name
    call.items=args

    if index is None: index=len(parent.content)-1 # append
    parent.content.insert(index,call)
    return call

class UseGen(BaseGen):
    def __init__(self,parent,name="",only=False,funcnames=[]):
        from fparser import api
        reader=FortranStringReader("use kern,only : func1_kern=>func1")
        reader.set_mode(True, True) # free form, strict
        myline=reader.next()
        import fparser
        root=parent.root
        from fparser.block_statements import Use
        use=Use(root,myline)
        use.name=name
        use.isonly=only
        if funcnames==[]: use.isonly=False
        use.items=funcnames
        BaseGen.__init__(self,parent,use)

def adduse(name,parent,only=False,funcnames=[]):

    from fparser import api
    reader=FortranStringReader("use kern,only : func1_kern=>func1")
    reader.set_mode(True, True) # free form, strict
    myline=reader.next()

    from fparser.block_statements import Use
    # find an appropriate place to add in our use statement
    import fparser
    while not (isinstance(parent,fparser.block_statements.Program) or isinstance(parent,fparser.block_statements.Module) or isinstance(parent,fparser.block_statements.Subroutine)):
        parent=parent.parent
    use=Use(parent,myline)
    use.name=name
    use.isonly=only
    if funcnames==[]: use.isonly=False
    use.items=funcnames

    parent.content.insert(0,use)
    return use

class DeclGen(BaseGen):
    def __init__(self,parent,datatype="",entity_decls=[],intent="",pointer=False,kind="",dimension=""):

        if datatype.lower()=="integer":
            from fparser.typedecl_statements import Integer
            reader=FortranStringReader("integer :: vanilla")
            reader.set_mode(True, False) # free form, strict
            myline=reader.next()
            self._decl=Integer(parent.root,myline)
        elif datatype.lower()=="real":
            from fparser.typedecl_statements import Real
            reader=FortranStringReader("real :: vanilla")
            reader.set_mode(True, False) # free form, strict
            myline=reader.next()
            self._decl=Real(parent.root,myline)
        else:
            raise RuntimeError("f2pygen:DeclGen:init: Only integer and real are currently supported and you specified '"+datatype+"'")
        self._decl.entity_decls=entity_decls
        my_attrspec=[]
        if intent != "":
            my_attrspec.append("intent({0})".format(intent))
        if pointer is not False:
            my_attrspec.append("pointer")
        self._decl.attrspec=my_attrspec
        if dimension != "":
             my_attrspec.append("dimension({0})".format(dimension))
        if kind is not "":
            self._decl.selector=('',kind)
        BaseGen.__init__(self,parent,self._decl)

def adddecl(datatype,entity_decls,parent,intent="",pointer=False):

    reader=FortranStringReader("integer :: vanilla")
    reader.set_mode(True, False) # free form, strict
    myline=reader.next()

    if datatype=="integer":
        from fparser.typedecl_statements import Integer
        decl=Integer(parent,myline)
    else:
        raise ParseError("Only integer is currently supported and you specified {0}".format(datatype))
    decl.entity_decls=entity_decls
    my_attrspec=[]
    if intent != "":
        my_attrspec.append("intent({0})".format(intent))
    if pointer is not False:
        my_attrspec.append("pointer")
    decl.attrspec=my_attrspec
    parent.content.insert(0,decl)
    return decl

class TypeDeclGen(BaseGen):
    def __init__(self,parent,datatype="",entity_decls=[],intent="",pointer=False,attrspec=[]):
        my_attrspec=[ spec for spec in attrspec ]
        if intent != "":
            my_attrspec.append("intent({0})".format(intent))
        if pointer is not False:
            my_attrspec.append("pointer")
        self._names=entity_decls

        reader=FortranStringReader("type(vanillatype) :: vanilla")
        reader.set_mode(True, False) # free form, strict
        myline=reader.next()

        from fparser.typedecl_statements import Type
        self._typedecl=Type(parent.root,myline)
        self._typedecl.selector=('',datatype)
        self._typedecl.attrspec=my_attrspec
        self._typedecl.entity_decls=entity_decls
        BaseGen.__init__(self,parent,self._typedecl)

    @property
    def names(self):
        return self._names
    @property
    def root(self):
        return self._typedecl

def addtypedecl(name,entity_decls,parent,index=None,attrspec=[],intent="",pointer=False):
    from fparser import api

    #TODO check parent is a valid class
    my_attrspec=[ spec for spec in attrspec ]
    if intent != "":
        my_attrspec.append("intent({0})".format(intent))
    if pointer is not False:
        my_attrspec.append("pointer")

    reader=FortranStringReader("type(vanillatype) :: vanilla")
    reader.set_mode(True, False) # free form, strict
    myline=reader.next()

    from fparser.typedecl_statements import Type
    typedecl=Type(parent,myline)
    typedecl.selector=('',name)
    typedecl.attrspec=my_attrspec
    typedecl.entity_decls=entity_decls

    # insert after any existing type declarations
    #for statement in parent.content:
    #    print type(statement)
    #exit(0)
    idx=0
    while isinstance(parent.content[idx],Type):
        idx+=1
    parent.content.insert(idx,typedecl)
    return typedecl

from fparser.block_statements import Select
class TypeSelect(Select):
    def tostr(self):
        return 'SELECT TYPE ( %s )' % (self.expr)

from fparser.statements import Case
class TypeCase(Case):
    def tofortran(self, isfix=None):
        tab = self.get_indent_tab(isfix=isfix)
        s = 'TYPE IS'
        if self.items:
            l = []
            for item in self.items:
                l.append((' : '.join(item)).strip())
            s += ' ( %s )' % (', '.join(l))
        else:
            s = 'CLASS DEFAULT'
        if self.name:
            s += ' ' + self.name
        return tab+s

class SelectionGen(BaseGen):
    """ STUFF """

    def __init__(self,parent,expr="UNSET",typeselect=False):
        """ INIT STUFF """
        from fparser.block_statements import Select,Case,EndSelect
        self._typeselect=typeselect
        reader=FortranStringReader("SELECT CASE (x)\nCASE (1)\nCASE DEFAULT\nEND SELECT")
        reader.set_mode(True, True) # free form, strict
        selectLine=reader.next()
        self._caseLine=reader.next()
        self._caseDefaultLine=reader.next()
        endSelectLine=reader.next()
        if self._typeselect:
            select=TypeSelect(parent.root,selectLine)
        else:
            select=Select(parent.root,selectLine)
        endselect=EndSelect(select,endSelectLine)
        select.expr=expr
        select.content.append(endselect)
        BaseGen.__init__(self,parent,select)

    def addcase(self,casenames,content=[]):
        if self._typeselect:
            case=TypeCase(self.root,self._caseLine)
        else:
            case=Case(self.root,self._caseLine)
        case.items=[casenames]
        self.root.content.insert(0,case)
        idx=0
        for stmt in content:
            idx+=1
            self.root.content.insert(idx,stmt.root)

    def adddefault(self):
        if self._typeselect:
            caseDefault=TypeCase(self.root,self._caseDefaultLine)
        else:
            caseDefault=Case(self.root,self._caseDefaultLine)
        self.root.content.insert(len(self.root.content)-1,caseDefault)

class DoGen(BaseGen):
    def __init__(self,parent,variable_name,start,end,step=None):
        reader=FortranStringReader("do i=1,n\nend do")
        reader.set_mode(True, True) # free form, strict
        doline=reader.next()
        enddoline=reader.next()
        from fparser.block_statements import Do,EndDo
        do=Do(parent.root,doline)
        do.loopcontrol=variable_name+"="+start+","+end
        if step is not None:
            do.loopcontrol=do.loopcontrol+","+step
        enddo=EndDo(do,enddoline)
        do.content.append(enddo)

        BaseGen.__init__(self,parent,do)

    def add(self,content,position=["auto"]):
        #print "The requested content is "+str(type(content))+\
        #      ", the contents parent is "+str(type(content.parent.root))+\
        #      " and I am "+str(type(self.root))
        if position[0]=="auto" or position[0]=="append":
            if position[0]=="auto" and ( isinstance(content,UseGen) \
                                      or isinstance(content,DeclGen) \
                                      or isinstance(content,TypeDeclGen) ):
                # a use and declarations can not appear in a do loop so pass on to parent
                self.parent.add(content)
            else:
                # append at the end of the loop. This is not a simple append as
                # the last element in the loop is the "end do" so we insert at the
                # penultimate location
                BaseGen.add(self,content,position=["insert",len(self.root.content)-1])
        else:
            BaseGen.add(self,content,position=position)
        
def adddo(variable_name,start,end,parent,step=None):

    reader=FortranStringReader("do i=1,n\nend do")
    reader.set_mode(True, True) # free form, strict
    doline=reader.next()
    enddoline=reader.next()

    from fparser.block_statements import Do,EndDo
    do=Do(parent,doline)
    do.loopcontrol=variable_name+"="+start+","+end
    if step is not None:
        do.loopcontrol=do.loopcontrol+","+step

    from fparser.block_statements import EndSubroutine
    idx=len(parent.content)
    if isinstance(parent.content[idx-1],EndSubroutine): idx-=1
    parent.content.insert(idx,do)
    enddo=EndDo(do,enddoline)
    do.content.append(enddo)
    return do

class AssignGen(BaseGen):

    def __init__(self,parent,lhs="",rhs="",pointer=False):
        if pointer:
            reader=FortranStringReader("lhs=>rhs")
        else:
            reader=FortranStringReader("lhs=rhs")
        reader.set_mode(True, True) # free form, strict
        myline=reader.next()
        if pointer:
            from fparser.statements import PointerAssignment
            self._assign=PointerAssignment(parent.root,myline)
        else:
            from fparser.statements import Assignment
            self._assign=Assignment(parent.root,myline)
        self._assign.expr=rhs
        self._assign.variable=lhs
        BaseGen.__init__(self,parent,self._assign)

def addassign(lhs,rhs,parent,pointer=False):
    if pointer:
        reader=FortranStringReader("lhs=>rhs")
    else:
         reader=FortranStringReader("lhs=rhs")
    reader.set_mode(True, True) # free form, strict
    myline=reader.next()
    if pointer:
        from fparser.statements import PointerAssignment
        assign=PointerAssignment(parent,myline)
    else:
        from fparser.statements import Assignment
        assign=Assignment(parent,myline)    
    assign.expr=rhs
    assign.variable=lhs
    idx=len(parent.content)
    from fparser.block_statements import EndSubroutine
    if isinstance(parent.content[idx-1],EndSubroutine): idx-=1
    parent.content.insert(idx,assign)
    return assign

def getuse(parent,usename="",onlyname=""):
  import fparser
  for stmt in parent.content:
    if isinstance(stmt,fparser.statements.Use):
      if usename=="":
        if onlyname=="" : return None
        else:
          for name in stmt.items:
            if name==onlyname: return stmt
      elif stmt.name==usename:
        if onlyname=="" : return stmt
        else:
          for name in stmt.items:
            if name==onlyname: return stmt
  if parent.parent==parent or \
     isinstance(parent.parent,fparser.block_statements.BeginSource):
    return None
  return getuse(parent.parent,usename,onlyname)
