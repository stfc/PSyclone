def createmodule(name): #,contains=True)
    from fparser import api

    code='''\
module vanilla
contains
end module vanilla
'''
    tree=api.parse(code,ignore_comments=False)
    module=tree.content[0]
    module.name=name
    endmod=module.content[1]
    endmod.name=name
    return module

def getLine(myString):
    from fparser.readfortran import FortranStringReader
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

def addsub(name,args,parent,index=None):
    from fparser import api

    from fparser.readfortran import FortranStringReader
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

def addcall(name,args,parent,index=None):
    from fparser import api

    #TODO check parent is a valid class
    #<class 'fparser.block_statements.Subroutine'>

    from fparser.readfortran import FortranStringReader
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

def adduse(name,parent,only=False,funcnames=[]):
    from fparser import api

    #TODO check parent is a valid class
    #<class 'fparser.block_statements.Subroutine'> or module

    from fparser.readfortran import FortranStringReader
    reader=FortranStringReader("use kern,only : func1_kern=>func1")
    reader.set_mode(True, True) # free form, strict
    myline=reader.next()

    from fparser.block_statements import Use
    use=Use(parent,myline)
    use.name=name
    use.isonly=only
    if funcnames==[]: use.isonly=False
    use.items=funcnames

    parent.content.insert(0,use)
    return use

def addtypedecl(name,attrspec,entity_decls,parent,index=None):
    from fparser import api

    #TODO check parent is a valid class
    #<class 'fparser.block_statements.Subroutine' or ???

    # class Type
    #name attrspec entity_decls

    from fparser.readfortran import FortranStringReader
    reader=FortranStringReader("type(vanillatype) :: vanilla")
    reader.set_mode(True, False) # free form, strict
    myline=reader.next()

    from fparser.typedecl_statements import Type
    typedecl=Type(parent,myline)
    typedecl.selector=('',name)
    typedecl.attrspec=attrspec
    typedecl.entity_decls=entity_decls

    # insert after any existing type declarations
    idx=0
    while isinstance(parent.content[idx],Type):
        idx+=1
    parent.content.insert(idx,typedecl)
    return typedecl
