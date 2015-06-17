#-------------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
#-------------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab

from f2pygen import ModuleGen, CommentGen, SubroutineGen, DoGen, CallGen, AllocateGen, DeallocateGen, IfThenGen, DeclGen, TypeDeclGen
import pytest

class TestDeclare:
    ''' pytest test for a declaration '''
    def test_no_replication_scalars(self):
        '''Check that the same scalar variable will only get declared once in
           a module and a subroutine'''
        variable_name = "arg_name"
        datatype = "integer"
        module = ModuleGen(name="testmodule")
        module.add(DeclGen(module, datatype=datatype, entity_decls=[variable_name]))
        module.add(DeclGen(module, datatype=datatype, entity_decls=[variable_name]))
        subroutine=SubroutineGen(module,name="testsubroutine")
        module.add(subroutine)
        subroutine.add(DeclGen(subroutine, datatype=datatype, entity_decls=[variable_name]))
        subroutine.add(DeclGen(subroutine, datatype=datatype, entity_decls=[variable_name]))
        generated_code=str(module.root)
        assert generated_code.count(variable_name) == 2
    def test_no_replication_types(self):
        '''Check that the same array variable will only get declared once in
           a module and a subroutine'''
        variable_name = "arg_name"
        datatype = "field_type"
        module = ModuleGen(name="testmodule")
        module.add(TypeDeclGen(module, datatype=datatype, entity_decls=[variable_name]))
        module.add(TypeDeclGen(module, datatype=datatype, entity_decls=[variable_name]))
        subroutine=SubroutineGen(module,name="testsubroutine")
        module.add(subroutine)
        subroutine.add(TypeDeclGen(subroutine, datatype=datatype, entity_decls=[variable_name]))
        subroutine.add(TypeDeclGen(subroutine, datatype=datatype, entity_decls=[variable_name]))
        generated_code=str(module.root)
        assert generated_code.count(variable_name) == 2

class TestIf:
    ''' pytest test for if statements. '''

    def test_if(self):
        ''' Check that an if gets created succesfully. '''
        module = ModuleGen(name="testmodule")
        clause = "a < b"
        fortran_if = IfThenGen(module, clause)
        module.add(fortran_if)
        lines=str(module.root).splitlines()
        assert "IF ("+clause+") THEN" in lines[3]
        assert "END IF" in lines[4]

    def test_if(self):
        ''' Check that the content of an if gets created successfully. '''
        module = ModuleGen(name="testmodule")
        clause = "a < b"
        if_statement = IfThenGen(module, clause)
        if_statement.add(CommentGen(if_statement, "HELLO"))
        module.add(if_statement)
        lines=str(module.root).splitlines()
        assert "IF ("+clause+") THEN" in lines[3]
        assert "!HELLO" in lines[4]
        assert "END IF" in lines[5]

class TestComment:
    ''' pytest tests for comments. '''
    def test_comment(self):
        ''' check that a comment gets created succesfully. '''
        module=ModuleGen(name="testmodule")
        content="HELLO"
        comment=CommentGen(module,content)
        module.add(comment)
        lines=str(module.root).splitlines()
        assert "!"+content in lines[3]

class TestAdd:
    ''' pytest tests for adding code. '''
    @pytest.mark.xfail(reason="unknown")
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
    ''' pytest tests for the ModuleGen class '''
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

class TestAllocate:
    ''' pytest tests for an allocate statement. '''
    def test_allocate_arg_str(self):
        ''' check that an allocate gets created succesfully with content being a string. '''
        module=ModuleGen(name="testmodule")
        content="hello"
        allocate=AllocateGen(module,content)
        module.add(allocate)
        lines=str(module.root).splitlines()
        assert "ALLOCATE ("+content+")" in lines[3]
    def test_allocate_arg_list(self):
        ''' check that an allocate gets created succesfully with content being a list. '''
        module=ModuleGen(name="testmodule")
        content=["hello","how","are","you"]
        content_str=""
        for idx,name in enumerate(content):
            content_str+=name
            if idx+1<len(content):
                content_str+=", "
        allocate=AllocateGen(module,content)
        module.add(allocate)
        lines=str(module.root).splitlines()
        assert "ALLOCATE ("+content_str+")" in lines[3]
    def test_allocate_incorrect_arg_type(self):
        ''' check that an allocate raises an error if an unknown type is passed. '''
        module=ModuleGen(name="testmodule")
        content=3
        with pytest.raises(RuntimeError):
            allocate=AllocateGen(module,content)

class TestDeallocate:
    ''' pytest tests for a deallocate statement. '''
    def test_deallocate_arg_str(self):
        ''' check that a deallocate gets created succesfully with content being a str. '''
        module=ModuleGen(name="testmodule")
        content="goodbye"
        deallocate=DeallocateGen(module,content)
        module.add(deallocate)
        lines=str(module.root).splitlines()
        assert "DEALLOCATE ("+content+")" in lines[3]
    def test_deallocate_arg_list(self):
        ''' check that a deallocate gets created succesfully with content being a list. '''
        module=ModuleGen(name="testmodule")
        content=["and","now","the","end","is","near"]
        content_str=""
        for idx,name in enumerate(content):
            content_str+=name
            if idx+1<len(content):
                content_str+=", "
        deallocate=DeallocateGen(module,content)
        module.add(deallocate)
        lines=str(module.root).splitlines()
        assert "DEALLOCATE ("+content_str+")" in lines[3]
    def test_deallocate_incorrect_arg_type(self):
        ''' check that a deallocate raises an error if an unknown type is passed. '''
        module=ModuleGen(name="testmodule")
        content=3
        with pytest.raises(RuntimeError):
            allocate=DeallocateGen(module,content)
