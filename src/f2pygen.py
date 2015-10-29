#-------------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
#-------------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab

''' Fortran code-generation library. This wraps the f2py fortran parser to
    provide routines which can be used to generate fortran code. This library
    includes pytest tests. '''

''' This section subclasses the f2py comment class so that we can reason about directives '''

from fparser.statements import Comment

class OMPDirective(Comment):
    ''' Subclass f2py comment for OpenMP directives so we can 
        reason about them when walking the tree '''
    def __init__(self,root,line,position,dir_type):
        self._types = ["parallel do", "parallel", "do"]
        self._positions = ["begin", "end"]
        if not dir_type in self._types:
            raise RuntimeError("Error, unrecognised directive type '{0}'. "
                               "Should be one of {1}".\
                               format(dir_type, self._types))
        if not position in self._positions:
            raise RuntimeError("Error, unrecognised position '{0}'. "
                               "Should be one of {1}".\
                               format(position, self._positions))
        self._my_type = dir_type
        self._position = position
        Comment.__init__(self,root,line)
    @property
    def type(self):
        return self._my_type
    @property
    def position(self):
        return self._position

''' This section provides new classes which provide a relatively high level interface to creating code and adding code to an existing ast '''

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

        '''Adds a new object to the tree. The actual position is determined by
        the position argument. Note, there are two trees, the first is
        the f2pygen object tree, the other is the f2py generated code
        tree. These are similar but different. At the moment we
        specify where to add things in terms of the f2pygen tree
        (which is a higher level api) but we also insert into the f2py
        tree at exactly the same location which needs to be sorted out
        at some point.

        '''

        if position[0]=="auto":
            raise Exception("Error: BaseGen:add: auto option must be "
                            "implemented by the sub class!")
        options=["append","first","after","before","insert","before_index","after_index"]
        if position[0] not in options:
            raise Exception("Error: BaseGen:add: supported positions are "
                            "{0} but found {1}".\
                            format(str(options), position[0]))
        if position[0]=="append":
            self.root.content.append(new_object.root)
        elif position[0]=="first":
            self.root.content.insert(0,new_object.root)
        elif position[0]=="insert":
            index=position[1]
            self.root.content.insert(index,new_object.root)
        elif position[0]=="after":
            idx = self._index_of_object(self.root.content, position[1])
            self.root.content.insert(idx+1,new_object.root)
        elif position[0]=="after_index":
            self.root.content.insert(position[1]+1,new_object.root)
        elif position[0]=="before_index":
            self.root.content.insert(position[1],new_object.root)
        elif position[0]=="before":
            idx = self._index_of_object(self.root.content, position[1])
            try:
                self.root.content.insert(idx, new_object.root)
            except ValueError:
                print "ValueError when inserting:"
                print str(new_object),str(new_object.root)
                print "parent is: "
                print str(self.root)
                print "looking for this as one of the children: "
                print str(position[1])
                exit(1)
        else:
            raise Exception("Error: BaseGen:add: internal error, should "
                            "not get to here")
        self.children.append(new_object)

    def _index_of_object(self, alist, obj):
        '''Effectively implements list.index(obj) but returns the index of
            the first item in the list that *is* the supplied object

        '''
        for idx,body in enumerate(alist):
            if body is obj:
                return idx
        raise Exception("Object {0} not found in list".format(str(obj)))

    def start_sibling_loop(self, debug = False):
        from fparser.block_statements import Do
        index = len(self.root.content)-1
        found = False
        while not found and index>=0:
            if isinstance(self.root.content[index],Do):
                found = True
            else:
                index -= 1
        if not found:
            raise RuntimeError(
                "Error, expecting to find a loop but none were found")
        return self.root.content[index]

    def last_declaration(self):
        '''Returns the *last* occurrence of a Declaration in the list of
            siblings of this node

        '''
        from fparser.typedecl_statements import TypeDeclarationStatement
        for sibling in reversed(self.root.content):
            if isinstance(sibling, TypeDeclarationStatement):
                return sibling

        raise RuntimeError("Error, no variable declarations found")

    def start_parent_loop(self, debug = False):
        from fparser.block_statements import Subroutine, EndSubroutine, Do
        from fparser.statements import Comment
        if debug:
            print "Entered before_parent_loop"
            print "The type of the current node is "+str(type(self.root))
            print "If the current node is a Do loop then move up to the "
            "top of the do loop nest"
        current = self.root
        local_current = self
	while isinstance(current.parent,Do):
            if debug:
                print "Parent is a do loop so moving to the parent"
	    current = current.parent
            local_current = local_current.parent
        if debug:
            print "The type of the current node is now "+str(type(current))
        if isinstance(current,Do):
            if (debug):
                print "The current node is a do loop"
                print "The type of parent is "+str(type(current.parent))
                print "Finding the loops position in its parent ..."
            index = current.parent.content.index(current)
            if (debug):
                print "The loops index is ",index
	    parent = current.parent
            local_current = local_current.parent
        else:
            if debug:
                print "The type of the current node is not a do loop"
                print "Assume the do loop will be appended as a child and find the last childs index"
            index = len(current.content)-1
            if debug:
                 print "The last childs index is ",index
            parent = current
        if debug:
            print "The type of the object at the index is "+str(type(parent.content[index]))
            print "If preceding node is a directive then move back one"
        if index == 0:
            if debug:
                print "current index is 0 so finish"
        elif isinstance(parent.content[index-1],OMPDirective):
            if debug:
                print "preceding node is a directive so find out what type ..."
                print "type is "+parent.content[index-1].position
                print "directive is "+str(parent.content[index-1])
            if parent.content[index-1].position == "begin":
                if debug:
                    print "type of directive is begin so move back one"
                index -= 1
            else:
                if debug:
                    print "directive type is not begin so finish"
        else:
            if debug:
                print "preceding node is not a directive so finish"
        if debug:
            print "type of final location ",type(parent.content[index])
            print "code for final location ",str(parent.content[index])
        return local_current, parent.content[index]

class ProgUnitGen(BaseGen):
    ''' functionality relevant to program units (currently modules, subroutines) '''
    def __init__(self,parent, sub):
        BaseGen.__init__(self,parent,sub)

    def add(self,content,position=["auto"]):
        '''specialise the add method to provide module and subroutine
           specific intelligent adding of use statements, implicit
           none statements and declarations if the position argument
           is set to auto (which is the default)'''
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
                index = 0
                # skip over any use statements
                index = self._skip_use_and_comments(index)
                # skip over implicit none if it exists
                index = self._skip_imp_none_and_comments(index)
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
            elif isinstance(content, ImplicitNoneGen):
                # does implicit none already exist?
                for child in self._children:
                    if isinstance(child,ImplicitNoneGen):
                        return
                # skip over any use statements
                index = 0
                index = self._skip_use_and_comments(index)
            else:
                index=len(self.root.content)-1 
            self.root.content.insert(index,content.root)
            self._children.append(content)

    def _skip_use_and_comments(self,index):
        ''' skip over any use statements and comments in the ast '''
        import fparser
        while isinstance(self.root.content[index],
                         fparser.statements.Use) \
        or isinstance(self.root.content[index],
                      fparser.statements.Comment):
            index += 1
        # now roll back to previous Use
        while isinstance(self.root.content[index-1],
                      fparser.statements.Comment):
            index -= 1
        return index

    def _skip_imp_none_and_comments(self,index):
        ''' skip over an implicit none statement if it exists and any
        comments before it '''
        import fparser
        end_index = index
        while isinstance(self.root.content[index],
                         fparser.typedecl_statements.Implicit) \
        or isinstance(self.root.content[index],
                      fparser.statements.Comment):
            if isinstance(self.root.content[index],
                         fparser.typedecl_statements.Implicit):
                end_index = index + 1
                break
            else:
                index = index + 1
        return end_index


class ModuleGen(ProgUnitGen):
    ''' create a fortran module '''
    def __init__(self,name="",contains=True,implicitnone=True):
        from fparser import api

        code='''\
module vanilla
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
        ProgUnitGen.__init__(self,None,module)
        if implicitnone:
            self.add(ImplicitNoneGen(self))

    def add_raw_subroutine(self, content):
        ''' adds a subroutine to the module that is a raw f2py parse object.
            This is used for inlining kernel subroutines into a module.
        '''
        from parse import KernelProcedure
        if not isinstance(content, KernelProcedure):
            raise Exception(
                "Expecting a KernelProcedure type but received " +
                str(type(content)))
        content.ast.parent = self.root
        # add content after any existing subroutines
        index = len(self.root.content) - 1
        self.root.content.insert(index, content.ast)

    def add(self,content,position=["auto"]):
        ''' specialise the add method to include a module specific check '''
        if not content.parent==self:
            # this is an error as a module can not have a parent
            # in the current version of f2pygen
            raise Exception("The requested parent is "+str(type(content.parent))+" but it should be "+str(type(self.root)),str(type(self)))
        ProgUnitGen.add(self,content,position)

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

class CommentGen(BaseGen):

    def __init__(self,parent,content):
        from fparser import api
        reader=FortranStringReader("! content\n")
        reader.set_mode(True, True) # free form, strict
        subline=reader.next()

        from fparser.block_statements import Comment
        my_comment=Comment(parent.root,subline)
        my_comment.content=content

        BaseGen.__init__(self,parent,my_comment)

class DirectiveGen(BaseGen):

    def __init__(self, parent, language, position, directive_type, content):

        self._supported_languages = ["omp"]
        self._language = language
        self._directive_type = directive_type

        from fparser import api
        reader=FortranStringReader("! content\n")
        reader.set_mode(True, True) # free form, strict
        subline=reader.next()

        if language == "omp":
            my_comment=OMPDirective(parent.root,subline, position, directive_type)
            my_comment.content="$omp"
            if position == "end":
                my_comment.content+=" end"
            my_comment.content+=" "+directive_type
            if content!="":
                my_comment.content+=" "+content
        else:
            raise RuntimeError("Error, unsupported directive language. Expecting one of "+str(self._supported_languages)+" but found '"+language+"'")

        BaseGen.__init__(self,parent,my_comment)


class ImplicitNoneGen(BaseGen):

    def __init__(self,parent):

        if type(parent) is not ModuleGen and type(parent) is not SubroutineGen:
            raise Exception("The parent of ImplicitNoneGen must be a module or a subroutine, but found {0}".format(type(parent)))
        from fparser import api
        reader=FortranStringReader("IMPLICIT NONE\n")
        reader.set_mode(True, True) # free form, strict
        subline=reader.next()

        from fparser.typedecl_statements import Implicit
        my_imp_none = Implicit(parent.root,subline)

        BaseGen.__init__(self,parent,my_imp_none)

class SubroutineGen(ProgUnitGen):

    def __init__(self,parent,name="",args=[],index=None, implicitnone=False):
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
        ProgUnitGen.__init__(self,parent,self._sub)
        if implicitnone:
            self.add(ImplicitNoneGen(self))

    @property
    def args(self):
        return self._sub.args

    @args.setter
    def args(self, namelist):
        ''' sets the subroutine arguments to the values in the list provide.'''
        self._sub.args = namelist

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

class AllocateGen(BaseGen):
    def __init__(self,parent,content):
        from fparser.statements import Allocate
        reader=FortranStringReader("allocate(dummy)")
        reader.set_mode(True, False) # free form, strict
        myline=reader.next()
        self._decl=Allocate(parent.root,myline)
        if isinstance(content,str):
            self._decl.items=[content]
        elif isinstance(content,list):
            self._decl.items=content
        else:
            raise RuntimeError("AllocateGen expected the content argument to be a str or a list, but found {0}".format(type(content)))
        BaseGen.__init__(self,parent,self._decl)

class DeallocateGen(BaseGen):
    def __init__(self,parent,content):
        from fparser.statements import Deallocate
        reader=FortranStringReader("deallocate(dummy)")
        reader.set_mode(True, False) # free form, strict
        myline=reader.next()
        self._decl=Deallocate(parent.root,myline)
        if isinstance(content,str):
            self._decl.items=[content]
        elif isinstance(content,list):
            self._decl.items=content
        else:
            raise RuntimeError("DeallocateGen expected the content argument to be a str or a list, but found {0}".format(type(content)))
        BaseGen.__init__(self,parent,self._decl)

class DeclGen(BaseGen):
    def __init__(self,parent,datatype="",entity_decls=[],intent="",pointer=False,kind="",dimension="",allocatable=False):

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
        if allocatable is not False:
            my_attrspec.append("allocatable")
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

class IfThenGen(BaseGen):
    ''' Generate a fortran if, then, end if statement. '''

    def __init__(self, parent, clause):

        reader = FortranStringReader("if (dummy) then\nend if")
        reader.set_mode(True, True) # free form, strict
        ifthenline = reader.next()
        endifline = reader.next()

        from fparser.block_statements import IfThen, EndIfThen
        my_if = IfThen(parent.root,ifthenline)
        my_if.expr = clause
        my_endif=EndIfThen(my_if,endifline)
        my_if.content.append(my_endif)

        BaseGen.__init__(self,parent,my_if)

    def add(self,content,position=["auto"]):

        if position[0]=="auto" or position[0]=="append":
            if position[0]=="auto" and ( isinstance(content,UseGen) \
                                      or isinstance(content,DeclGen) \
                                      or isinstance(content,TypeDeclGen) ):
                # a use and declarations can not appear in an if statement so pass on to parent
                self.parent.add(content)
            else:
                # append at the end of the loop. This is not a simple append as
                # the last element in the if is the "end if" so we insert at the
                # penultimate location
                BaseGen.add(self,content,position=["insert",len(self.root.content)-1])
        else:
            BaseGen.add(self,content,position=position)

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
