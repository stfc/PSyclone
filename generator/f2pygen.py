# Copyright 2013 STFC, all rights reserved

from fparser.readfortran import FortranStringReader

class BaseGen(object):
    def __init__(self):
        self._parent=None
        self._children=[]
    @property
    def parent(self):
        return self._parent
    @property
    def children(self):
        return self._children
    def add(self,content):
        raise NotImplementedError('I need to be implemented!')
    @property
    def root(self):
        raise NotImplementedError('I need to be implemented!')

class ModuleGen(BaseGen):
    def __init__(self,name="",contains=True,implicitnone=True):
        super(ModuleGen, self).__init__()
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
        self._module=tree.content[0]
        self._module.name=name
        endmod=self._module.content[len(self._module.content)-1]
        endmod.name=name

    @property
    def root(self):
        return self._module

    def add(self,content):
        if not content.parent==self:
            #print "****",content.root, "***",self._module
            # this is an error as a module can not have a parent
            # in the current version of f2pygen
            #print "**",type(content)
            raise Exception("The requested parent is "+str(type(content.parent))+" but it should be "+str(type(self._module)),str(type(self)))
            
        import fparser
        if isinstance(content.root,fparser.statements.Use):
            index=0
        else:
            index=len(self._module.content)-1 # append
        self._module.content.insert(index,content.root)
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
        super(SubroutineGen, self).__init__()
        self._parent=parent
        from fparser import api
        reader=FortranStringReader("subroutine vanilla(vanilla_arg)\nend subroutine")
        reader.set_mode(True, True) # free form, strict
        subline=reader.next()
        endsubline=reader.next()

        from fparser.block_statements import Subroutine,EndSubroutine
        self._sub=Subroutine(parent.root,subline)
        self._sub.name=name
        self._sub.args=args

        endsub=EndSubroutine(self._sub,endsubline)
        self._sub.content.append(endsub)

    @property
    def root(self):
        return self._sub

    def add(self,content):
        from fparser.typedecl_statements import TypeDeclarationStatement
        import fparser
        if isinstance(content,DeclGen) or isinstance(content,TypeDeclGen):
            # have I already been declared?
            for child in self._children:
                if isinstance(child,DeclGen) or isinstance(child,TypeDeclGen):
                    for var_name in content.names:
                        if var_name in child.names:
                            if len(content.names)==1:
                                #print "Skipping as",var_name,"already exists"
                                return # as this variable already exists
                            else:
                                # remove it
                                #print "Removing",var_name
                                content.names.remove(var_name)
                
            # skip over any use statements
            index=0
            while isinstance(self._sub.content[index],fparser.statements.Use):
                index+=1
            # skip over any declarations which have an intent
            try:
                intent=True
                while intent:
                    intent=False
                    for attr in self._sub.content[index].attrspec:
                        if attr.find("intent")==0:
                            intent=True
                            index+=1
                            break
            except AttributeError:
                pass
            except Exception:
                raise
        elif isinstance(content.root,fparser.statements.Use):
            index=0
        else:
            index=len(self._sub.content)-1 
        self._sub.content.insert(index,content.root)
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
        super(CallGen, self).__init__()
        self._parent=parent
        from fparser import api

        reader=FortranStringReader("call vanilla(vanilla_arg)")
        reader.set_mode(True, True) # free form, strict
        myline=reader.next()

        from fparser.block_statements import Call
        self._call=Call(parent.root,myline)
        self._call.designator=name
        self._call.items=args

        self._parent=parent.root

    @property
    def root(self):
        return self._call

    @property
    def parent(self):
        return self._parent

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
        super(UseGen, self).__init__()
        self._parent=parent
        from fparser import api
        reader=FortranStringReader("use kern,only : func1_kern=>func1")
        reader.set_mode(True, True) # free form, strict
        myline=reader.next()
        # find an appropriate place to put the use statement
        import fparser
        root=parent.root
        #print "UseGen original parent",type(root)
        while not (isinstance(root,fparser.block_statements.Subroutine) or \
                  isinstance(root,fparser.block_statements.Module) or \
                  isinstance(root,fparser.block_statements.Program)):
            root=root.parent
        #print "UseGen new parent",type(root)

        from fparser.block_statements import Use
        self._use=Use(root,myline)
        self._use.name=name
        self._use.isonly=only
        if funcnames==[]: self._use.isonly=False
        self._use.items=funcnames

    @property
    def parent(self):
        return self._parent
    @property
    def root(self):
        return self._use


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
    def __init__(self,parent,datatype="",entity_decls=[],intent="",pointer=False):
        super(DeclGen, self).__init__()
        self._parent=parent
        reader=FortranStringReader("integer :: vanilla")
        reader.set_mode(True, False) # free form, strict
        myline=reader.next()
        self._names=entity_decls

        if datatype=="integer":
            from fparser.typedecl_statements import Integer
            self._decl=Integer(parent.root,myline)
        else:
            raise ParseError("Only integer is currently supported and you specified {0}".format(datatype))
        self._decl.entity_decls=entity_decls
        my_attrspec=[]
        if intent != "":
            my_attrspec.append("intent({0})".format(intent))
        if pointer is not False:
            my_attrspec.append("pointer")
        self._decl.attrspec=my_attrspec

    @property
    def names(self):
        return self._names
    @property
    def root(self):
        return self._decl

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
        super(TypeDeclGen, self).__init__()
        self._parent=parent
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
        super(SelectionGen, self).__init__()
        self._parent=parent
        """ INIT STUFF """
        from fparser.block_statements import Select,Case,EndSelect
        #self._parent=parent
        self._typeselect=typeselect
        reader=FortranStringReader("SELECT CASE (x)\nCASE (1)\nCASE DEFAULT\nEND SELECT")
        reader.set_mode(True, True) # free form, strict
        selectLine=reader.next()
        self._caseLine=reader.next()
        self._caseDefaultLine=reader.next()
        endSelectLine=reader.next()
        if self._typeselect:
            self._select=TypeSelect(parent.root,selectLine)
        else:
            self._select=Select(parent.root,selectLine)
        endselect=EndSelect(self._select,endSelectLine)
        self._select.expr=expr
        self._select.content.append(endselect)

    @property
    def root(self):
        return self._select

    def addcase(self,casenames,content=[]):
        if self._typeselect:
            case=TypeCase(self._select,self._caseLine)
        else:
            case=Case(self._select,self._caseLine)
        case.items=[casenames]
        self._select.content.insert(0,case)
        idx=0
        for stmt in content:
            idx+=1
            self._select.content.insert(idx,stmt.root)

    def adddefault(self):
        if self._typeselect:
            caseDefault=TypeCase(self._select,self._caseDefaultLine)
        else:
            caseDefault=Case(self._select,self._caseDefaultLine)
        self._select.content.insert(len(self._select.content)-1,caseDefault)

class DoGen(BaseGen):
    def __init__(self,parent,variable_name,start,end,step=None):
        super(DoGen, self).__init__()
        reader=FortranStringReader("do i=1,n\nend do")
        reader.set_mode(True, True) # free form, strict
        doline=reader.next()
        enddoline=reader.next()
        self._parent=parent
        from fparser.block_statements import Do,EndDo
        self._do=Do(parent.root,doline)
        self._do.loopcontrol=variable_name+"="+start+","+end
        if step is not None:
            self._do.loopcontrol=self._do.loopcontrol+","+step
        enddo=EndDo(self._do,enddoline)
        self._do.content.append(enddo)

    @property
    def root(self):
        return self._do

    def add(self,content):
        #print "The requested parent is "+str(type(content.parent))+" and I am "+str(type(self._do))
        if not content.parent==self._do:
            #print "DoGen.add passing onto parent"
            self.parent.add(content)
        else:
            index=len(self._do.content)-1
            self._do.content.insert(index,content.root)
            self._children.append(content)

        
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
        super(AssignGen, self).__init__()
        self._parent=parent
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

    @property
    def root(self):
        return self._assign

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
