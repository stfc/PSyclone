#-------------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
#-------------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab

from f2pygen import ModuleGen, CommentGen, SubroutineGen, DoGen, CallGen,\
    AllocateGen, DeallocateGen, IfThenGen, DeclGen, TypeDeclGen,\
    ImplicitNoneGen, UseGen, DirectiveGen
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

    def test_subroutine_var_with_implicit_none(self):
        ''' test that a variable is added after an implicit none
        statement in a subroutine'''
        module = ModuleGen(name="testmodule")
        subroutine = SubroutineGen(module, name="testsubroutine",
                                   implicitnone=True)
        module.add(subroutine)
        subroutine.add(DeclGen(subroutine, datatype="integer",
                           entity_decls=["var1"]))
        idx_var = line_number(subroutine.root, "INTEGER var1")
        idx_imp_none = line_number(subroutine.root, "IMPLICIT NONE")
        print str(module.root)
        assert idx_var - idx_imp_none == 1, \
            "variable declation must be after implicit none"

    def test_subroutine_var_intent_in_with_directive(self):
        ''' test that a variable declared as intent in is added before
        a directive in a subroutine'''
        module = ModuleGen(name="testmodule")
        subroutine = SubroutineGen(module, name="testsubroutine",
                                   implicitnone=False)
        module.add(subroutine)
        subroutine.add(DirectiveGen(subroutine, "omp", "begin",
                                    "parallel", ""))
        subroutine.add(DeclGen(subroutine, datatype="integer",
                               intent="in", entity_decls=["var1"]))
        idx_par = line_number(subroutine.root, "!$omp parallel")
        idx_var = line_number(subroutine.root,
                              "INTEGER, intent(in) :: var1")
        print str(module.root)
        assert idx_par - idx_var == 1, \
            "variable declaration must be before directive"


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

def line_number(root, string_name):
    ''' f2pygen helper routine which returns the first index of the
    supplied string or -1 if it is not found '''
    lines = str(root).splitlines()
    for idx, line in enumerate(lines):
        if string_name in line:
            return idx
    return -1

def count_lines(root, string_name):
    '''f2pygen helper routine which returns the number of lines that
    contain the supplied string '''
    count = 0
    lines = str(root).splitlines()
    for curr_idx, line in enumerate(lines):
        if string_name in line:
            count += 1
    return count


class TestImplicitNone():
    ''' f2pygen:ImplicitNoneGen() tests '''

    # module tests
    def test_in_a_module(self):
        ''' test that implicit none can be added to a module in the
        correct location'''
        module=ModuleGen(name="testmodule", implicitnone=False)
        module.add(ImplicitNoneGen(module))
        in_idx = line_number(module.root,"IMPLICIT NONE")
        cont_idx = line_number(module.root,"CONTAINS")
        assert in_idx>-1, "IMPLICIT NONE not found"
        assert cont_idx>-1, "CONTAINS not found"
        assert cont_idx - in_idx == 1, "CONTAINS is not on the line after" +\
            " IMPLICIT NONE"

    def test_in_a_module_with_decs(self):
        ''' test that implicit none is added before any declaration
        statements in a module when auto (the default) is used for
        insertion '''
        module=ModuleGen(name="testmodule", implicitnone=False)
        module.add(DeclGen(module, datatype="integer",
                           entity_decls=["var1"]))
        module.add(TypeDeclGen(module, datatype="my_type",
                               entity_decls=["type1"]))
        module.add(ImplicitNoneGen(module))
        in_idx = line_number(module.root,"IMPLICIT NONE")
        assert in_idx == 1

    def test_in_a_module_with_use_and_decs(self):
        ''' test that implicit none is added after any use statements
        and before any declarations in a module when auto (the
        default) is used for insertion'''
        module=ModuleGen(name="testmodule", implicitnone=False)
        module.add(DeclGen(module, datatype="integer",
                           entity_decls=["var1"]))
        module.add(TypeDeclGen(module, datatype="my_type",
                               entity_decls=["type1"]))
        module.add(UseGen(module, "fred"))
        module.add(ImplicitNoneGen(module))
        in_idx = line_number(module.root, "IMPLICIT NONE")
        assert in_idx == 2

    def test_in_a_module_with_use_and_decs_and_comments(self):
        ''' test that implicit none is added after any use statements
        and before any declarations in a module in the presence of
        comments when auto (the default) is used for insertion'''
        module=ModuleGen(name="testmodule", implicitnone=False)
        module.add(DeclGen(module, datatype="integer",
                           entity_decls=["var1"]))
        module.add(TypeDeclGen(module, datatype="my_type",
                               entity_decls=["type1"]))
        module.add(UseGen(module, "fred"))
        for idx in [0,1,2,3]:
            module.add(CommentGen(module, " hello "+str(idx)),
                       position=["before_index", 2*idx])
        module.add(ImplicitNoneGen(module))
        in_idx = line_number(module.root, "IMPLICIT NONE")
        assert in_idx == 3

    def test_in_a_module_already_exists(self):
        ''' test that implicit none is not added to a module when one
        already exists'''
        module=ModuleGen(name="testmodule", implicitnone=True)
        module.add(ImplicitNoneGen(module))
        count = count_lines(module.root, "IMPLICIT NONE")
        print str(module.root)
        assert count == 1, \
            "There should only be one instance of IMPLICIT NONE"

    def test_in_a_subroutine(self):
        ''' test that implicit none can be added to a subroutine '''
        module = ModuleGen(name="testmodule")
        subroutine=SubroutineGen(module,name="testsubroutine")
        module.add(subroutine)
        subroutine.add(ImplicitNoneGen(subroutine))
        assert 'IMPLICIT NONE' in str(subroutine.root)

    def test_in_a_subroutine_with_decs(self):
        ''' test that implicit none is added before any declaration
        statements in a subroutine when auto (the default) is used for
        insertion '''
        module=ModuleGen(name="testmodule")
        sub = SubroutineGen(module, name="testsubroutine")
        module.add(sub)
        sub.add(DeclGen(sub, datatype="integer",
                           entity_decls=["var1"]))
        sub.add(TypeDeclGen(sub, datatype="my_type",
                               entity_decls=["type1"]))
        sub.add(ImplicitNoneGen(module))
        in_idx = line_number(sub.root,"IMPLICIT NONE")
        assert in_idx == 1

    def test_in_a_subroutine_with_use_and_decs(self):
        ''' test that implicit none is added after any use statements
        and before any declarations in a subroutine when auto (the
        default) is used for insertion'''
        module=ModuleGen(name="testmodule")
        sub = SubroutineGen(module, name="testsubroutine")
        module.add(sub)
        sub.add(DeclGen(sub, datatype="integer",
                        entity_decls=["var1"]))
        sub.add(TypeDeclGen(sub, datatype="my_type",
                            entity_decls=["type1"]))
        sub.add(UseGen(sub, "fred"))
        sub.add(ImplicitNoneGen(sub))
        in_idx = line_number(sub.root, "IMPLICIT NONE")
        assert in_idx == 2

    def test_in_a_subroutine_with_use_and_decs_and_comments(self):
        ''' test that implicit none is added after any use statements
        and before any declarations in a subroutine in the presence of
        comments when auto (the default) is used for insertion'''
        module=ModuleGen(name="testmodule")
        sub = SubroutineGen(module, name="testsubroutine")
        module.add(sub)
        sub.add(DeclGen(sub, datatype="integer",
                           entity_decls=["var1"]))
        sub.add(TypeDeclGen(sub, datatype="my_type",
                               entity_decls=["type1"]))
        sub.add(UseGen(sub, "fred"))
        for idx in [0,1,2,3]:
            sub.add(CommentGen(sub, " hello "+str(idx)),
                       position=["before_index", 2*idx])
        sub.add(ImplicitNoneGen(sub))
        in_idx = line_number(sub.root, "IMPLICIT NONE")
        assert in_idx == 3

    def test_in_a_subroutine_already_exists(self):
        ''' test that implicit none is not added to a subroutine when
        one already exists'''
        module=ModuleGen(name="testmodule")
        sub = SubroutineGen(module, name="testsubroutine", implicitnone=True)
        module.add(sub)
        sub.add(ImplicitNoneGen(sub))
        count = count_lines(sub.root, "IMPLICIT NONE")
        assert count == 1, \
            "There should only be one instance of IMPLICIT NONE"

    def test_exception_if_wrong_parent(self):
        ''' test that an exception is thrown if implicit none is added
        and the parent is not a module or a subroutine '''
        module=ModuleGen(name="testmodule")
        sub = SubroutineGen(module, name="testsubroutine")
        module.add(sub)
        do = DoGen(sub, "i", "1", "10")
        sub.add(do)
        with pytest.raises(Exception):
            do.add(ImplicitNoneGen(do))


class TestSubroutineGen():
    ''' f2pygen:SubroutineGen() tests '''

    def test_implicit_none_false(self):
        ''' test that implicit none is not added to the subroutine if
        not requested '''
        module=ModuleGen(name="testmodule")
        sub = SubroutineGen(module, name="testsubroutine", implicitnone=False)
        module.add(sub)
        count = count_lines(sub.root, "IMPLICIT NONE")
        assert count == 0, "IMPLICIT NONE SHOULD NOT EXIST"

    def test_implicit_none_true(self):
        ''' test that implicit none is added to the subroutine if
        requested '''
        module=ModuleGen(name="testmodule")
        sub = SubroutineGen(module, name="testsubroutine", implicitnone=True)
        module.add(sub)
        count = count_lines(sub.root, "IMPLICIT NONE")
        assert count == 1, "IMPLICIT NONE SHOULD EXIST"

    def test_implicit_none_default(self):
        ''' test that implicit none is not added to the subroutine by
        default '''
        module=ModuleGen(name="testmodule")
        sub = SubroutineGen(module, name="testsubroutine")
        module.add(sub)
        count = count_lines(sub.root, "IMPLICIT NONE")
        assert count == 0, "IMPLICIT NONE SHOULD NOT EXIST BY DEFAULT"
