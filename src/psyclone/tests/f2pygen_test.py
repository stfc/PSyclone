# ----------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2016.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
# ----------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab

''' Tests for the f2pygen module of PSyclone '''

from __future__ import absolute_import
from psyclone.f2pygen import ModuleGen, CommentGen, SubroutineGen, DoGen, \
    CallGen, AllocateGen, DeallocateGen, IfThenGen, DeclGen, TypeDeclGen,\
    ImplicitNoneGen, UseGen, DirectiveGen, AssignGen
from utils import line_number, count_lines
import pytest


def test_decl_no_replication_scalars():
    '''Check that the same scalar variable will only get declared once in
    a module and a subroutine'''
    variable_name = "arg_name"
    datatype = "integer"
    module = ModuleGen(name="testmodule")
    module.add(DeclGen(module, datatype=datatype,
                       entity_decls=[variable_name]))
    module.add(DeclGen(module, datatype=datatype,
                       entity_decls=[variable_name]))
    subroutine = SubroutineGen(module, name="testsubroutine")
    module.add(subroutine)
    subroutine.add(DeclGen(subroutine, datatype=datatype,
                           entity_decls=[variable_name]))
    subroutine.add(DeclGen(subroutine, datatype=datatype,
                           entity_decls=[variable_name]))
    generated_code = str(module.root)
    assert generated_code.count(variable_name) == 2


def test_decl_no_replication_types():
    '''Check that the same array variable will only get declared once in
    a module and a subroutine'''
    variable_name = "arg_name"
    datatype = "field_type"
    module = ModuleGen(name="testmodule")
    module.add(TypeDeclGen(module, datatype=datatype,
                           entity_decls=[variable_name]))
    module.add(TypeDeclGen(module, datatype=datatype,
                           entity_decls=[variable_name]))
    subroutine = SubroutineGen(module, name="testsubroutine")
    module.add(subroutine)
    subroutine.add(TypeDeclGen(subroutine, datatype=datatype,
                               entity_decls=[variable_name]))
    subroutine.add(TypeDeclGen(subroutine, datatype=datatype,
                               entity_decls=[variable_name]))
    generated_code = str(module.root)
    assert generated_code.count(variable_name) == 2


def test_subroutine_var_with_implicit_none():
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


def test_subroutine_var_intent_in_with_directive():
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


def test_if():
    ''' Check that an if gets created succesfully. '''
    module = ModuleGen(name="testmodule")
    clause = "a < b"
    fortran_if = IfThenGen(module, clause)
    module.add(fortran_if)
    lines = str(module.root).splitlines()
    assert "IF (" + clause + ") THEN" in lines[3]
    assert "END IF" in lines[4]


def test_if_content():
    ''' Check that the content of an if gets created successfully. '''
    module = ModuleGen(name="testmodule")
    clause = "a < b"
    if_statement = IfThenGen(module, clause)
    if_statement.add(CommentGen(if_statement, "HELLO"))
    module.add(if_statement)
    lines = str(module.root).splitlines()
    assert "IF (" + clause + ") THEN" in lines[3]
    assert "!HELLO" in lines[4]
    assert "END IF" in lines[5]


def test_if_with_position_before():
    ''' Check that IfThenGen.add() correctly uses the position
    argument if supplied. '''
    module = ModuleGen(name="testmodule")
    clause = "a < b"
    if_statement = IfThenGen(module, clause)
    com1 = CommentGen(if_statement, "HELLO")
    if_statement.add(com1)
    if_statement.add(CommentGen(if_statement, "GOODBYE"),
                     position=["before", com1.root])
    module.add(if_statement)
    lines = str(module.root).splitlines()
    assert "IF (" + clause + ") THEN" in lines[3]
    assert "!GOODBYE" in lines[4]
    assert "!HELLO" in lines[5]
    assert "END IF" in lines[6]


def test_if_with_position_append():
    ''' Check that IfThenGen.add() correctly uses the position
    argument when *append* is specified. '''
    module = ModuleGen(name="testmodule")
    clause = "a < b"
    if_statement = IfThenGen(module, clause)
    com1 = CommentGen(if_statement, "HELLO")
    if_statement.add(com1)
    if_statement.add(CommentGen(if_statement, "GOODBYE"),
                     position=["append"])
    module.add(if_statement)
    print str(module.root)
    lines = str(module.root).splitlines()
    assert "IF (" + clause + ") THEN" in lines[3]
    assert "!HELLO" in lines[4]
    assert "!GOODBYE" in lines[5]
    assert "END IF" in lines[6]


def test_if_add_use():
    ''' Check that IfThenGen.add() correctly handles the case
    when it is passed a UseGen object '''
    module = ModuleGen(name="testmodule")
    clause = "a < b"
    if_statement = IfThenGen(module, clause)
    if_statement.add(CommentGen(if_statement, "GOODBYE"))
    if_statement.add(UseGen(if_statement, name="dibna"))
    module.add(if_statement)
    print str(module.root)
    use_line = line_number(module.root, "USE dibna")
    if_line = line_number(module.root, "IF (" + clause + ") THEN")
    # The use statement must come before the if..then block
    assert use_line < if_line


def test_comment():
    ''' check that a comment gets created succesfully. '''
    module = ModuleGen(name="testmodule")
    content = "HELLO"
    comment = CommentGen(module, content)
    module.add(comment)
    lines = str(module.root).splitlines()
    assert "!" + content in lines[3]


def test_add_before():
    ''' add the new code before a particular object '''
    module = ModuleGen(name="testmodule")
    subroutine = SubroutineGen(module, name="testsubroutine")
    module.add(subroutine)
    loop = DoGen(subroutine, "it", "1", "10")
    subroutine.add(loop)
    call = CallGen(subroutine, "testcall")
    subroutine.add(call, position=["before", loop.root])
    lines = str(module.root).splitlines()
    # the call should be inserted before the loop
    print lines
    assert "SUBROUTINE testsubroutine" in lines[3]
    assert "CALL testcall" in lines[4]
    assert "DO it=1,10" in lines[5]


def test_mod_vanilla():
    ''' Check that we can create a basic, vanilla module '''
    module = ModuleGen()
    lines = str(module.root).splitlines()
    assert "MODULE" in lines[0]
    assert "IMPLICIT NONE" in lines[1]
    assert "CONTAINS" in lines[2]
    assert "END MODULE" in lines[3]


def test_mod_name():
    ''' Check that we can create a module with a specified name '''
    name = "test"
    module = ModuleGen(name=name)
    assert "MODULE " + name in str(module.root)


def test_mod_no_contains():
    ''' Check that we can switch-off the generation of a CONTAINS
    statement within a module '''
    module = ModuleGen(name="test", contains=False)
    assert "CONTAINS" not in str(module.root)


def test_mod_no_implicit_none():
    ''' Check that we can switch off the generation of IMPLICIT NONE
    within a module '''
    module = ModuleGen(name="test", implicitnone=False)
    assert "IMPLICIT NONE" not in str(module.root)


def test_failed_module_inline():
    ''' test that an error is thrown if the wrong type of object
    is passed to the add_raw_subroutine method '''
    module = ModuleGen(name="test")
    invalid_type = "string"
    with pytest.raises(Exception):
        module.add_raw_subroutine(invalid_type)


def test_allocate_arg_str():
    '''check that an allocate gets created succesfully with content being
    a string.'''
    module = ModuleGen(name="testmodule")
    content = "hello"
    allocate = AllocateGen(module, content)
    module.add(allocate)
    lines = str(module.root).splitlines()
    assert "ALLOCATE (" + content + ")" in lines[3]


def test_allocate_arg_list():
    '''check that an allocate gets created succesfully with content being
    a list.'''
    module = ModuleGen(name="testmodule")
    content = ["hello", "how", "are", "you"]
    content_str = ""
    for idx, name in enumerate(content):
        content_str += name
        if idx+1 < len(content):
            content_str += ", "
    allocate = AllocateGen(module, content)
    module.add(allocate)
    lines = str(module.root).splitlines()
    assert "ALLOCATE (" + content_str + ")" in lines[3]


def test_allocate_incorrect_arg_type():
    '''check that an allocate raises an error if an unknown type is
    passed.'''
    module = ModuleGen(name="testmodule")
    content = 3
    with pytest.raises(RuntimeError):
        _ = AllocateGen(module, content)


def test_deallocate_arg_str():
    '''check that a deallocate gets created succesfully with content
    being a str.'''
    module = ModuleGen(name="testmodule")
    content = "goodbye"
    deallocate = DeallocateGen(module, content)
    module.add(deallocate)
    lines = str(module.root).splitlines()
    assert "DEALLOCATE (" + content + ")" in lines[3]


def test_deallocate_arg_list():
    '''check that a deallocate gets created succesfully with content
    being a list.'''
    module = ModuleGen(name="testmodule")
    content = ["and", "now", "the", "end", "is", "near"]
    content_str = ""
    for idx, name in enumerate(content):
        content_str += name
        if idx+1 < len(content):
            content_str += ", "
    deallocate = DeallocateGen(module, content)
    module.add(deallocate)
    lines = str(module.root).splitlines()
    assert "DEALLOCATE (" + content_str + ")" in lines[3]


def test_deallocate_incorrect_arg_type():
    '''check that a deallocate raises an error if an unknown type is
    passed.'''
    module = ModuleGen(name="testmodule")
    content = 3
    with pytest.raises(RuntimeError):
        _ = DeallocateGen(module, content)


def test_imp_none_in_module():
    ''' test that implicit none can be added to a module in the
    correct location'''
    module = ModuleGen(name="testmodule", implicitnone=False)
    module.add(ImplicitNoneGen(module))
    in_idx = line_number(module.root, "IMPLICIT NONE")
    cont_idx = line_number(module.root, "CONTAINS")
    assert in_idx > -1, "IMPLICIT NONE not found"
    assert cont_idx > -1, "CONTAINS not found"
    assert cont_idx - in_idx == 1, "CONTAINS is not on the line after" +\
        " IMPLICIT NONE"


def test_imp_none_in_module_with_decs():
    ''' test that implicit none is added before any declaration
    statements in a module when auto (the default) is used for
    insertion '''
    module = ModuleGen(name="testmodule", implicitnone=False)
    module.add(DeclGen(module, datatype="integer",
                       entity_decls=["var1"]))
    module.add(TypeDeclGen(module, datatype="my_type",
                           entity_decls=["type1"]))
    module.add(ImplicitNoneGen(module))
    in_idx = line_number(module.root, "IMPLICIT NONE")
    assert in_idx == 1


def test_imp_none_in_module_with_use_and_decs():
    ''' test that implicit none is added after any use statements
    and before any declarations in a module when auto (the
    default) is used for insertion'''
    module = ModuleGen(name="testmodule", implicitnone=False)
    module.add(DeclGen(module, datatype="integer",
                       entity_decls=["var1"]))
    module.add(TypeDeclGen(module, datatype="my_type",
                           entity_decls=["type1"]))
    module.add(UseGen(module, "fred"))
    module.add(ImplicitNoneGen(module))
    in_idx = line_number(module.root, "IMPLICIT NONE")
    assert in_idx == 2


def test_imp_none_in_module_with_use_and_decs_and_comments():
    ''' test that implicit none is added after any use statements
    and before any declarations in a module in the presence of
    comments when auto (the default) is used for insertion'''
    module = ModuleGen(name="testmodule", implicitnone=False)
    module.add(DeclGen(module, datatype="integer",
                       entity_decls=["var1"]))
    module.add(TypeDeclGen(module, datatype="my_type",
                           entity_decls=["type1"]))
    module.add(UseGen(module, "fred"))
    for idx in [0, 1, 2, 3]:
        module.add(CommentGen(module, " hello "+str(idx)),
                   position=["before_index", 2*idx])
    module.add(ImplicitNoneGen(module))
    in_idx = line_number(module.root, "IMPLICIT NONE")
    assert in_idx == 3


def test_imp_none_in_module_already_exists():
    ''' test that implicit none is not added to a module when one
    already exists'''
    module = ModuleGen(name="testmodule", implicitnone=True)
    module.add(ImplicitNoneGen(module))
    count = count_lines(module.root, "IMPLICIT NONE")
    print str(module.root)
    assert count == 1, \
        "There should only be one instance of IMPLICIT NONE"


def test_imp_none_in_subroutine():
    ''' test that implicit none can be added to a subroutine '''
    module = ModuleGen(name="testmodule")
    subroutine = SubroutineGen(module, name="testsubroutine")
    module.add(subroutine)
    subroutine.add(ImplicitNoneGen(subroutine))
    assert 'IMPLICIT NONE' in str(subroutine.root)


def test_imp_none_in_subroutine_with_decs():
    ''' test that implicit none is added before any declaration
    statements in a subroutine when auto (the default) is used for
    insertion '''
    module = ModuleGen(name="testmodule")
    sub = SubroutineGen(module, name="testsubroutine")
    module.add(sub)
    sub.add(DeclGen(sub, datatype="integer",
                    entity_decls=["var1"]))
    sub.add(TypeDeclGen(sub, datatype="my_type",
                        entity_decls=["type1"]))
    sub.add(ImplicitNoneGen(module))
    in_idx = line_number(sub.root, "IMPLICIT NONE")
    assert in_idx == 1


def test_imp_none_in_subroutine_with_use_and_decs():
    ''' test that implicit none is added after any use statements
    and before any declarations in a subroutine when auto (the
    default) is used for insertion'''
    module = ModuleGen(name="testmodule")
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


def test_imp_none_in_subroutine_with_use_and_decs_and_comments():
    ''' test that implicit none is added after any use statements
    and before any declarations in a subroutine in the presence of
    comments when auto (the default) is used for insertion'''
    module = ModuleGen(name="testmodule")
    sub = SubroutineGen(module, name="testsubroutine")
    module.add(sub)
    sub.add(DeclGen(sub, datatype="integer",
                    entity_decls=["var1"]))
    sub.add(TypeDeclGen(sub, datatype="my_type",
                        entity_decls=["type1"]))
    sub.add(UseGen(sub, "fred"))
    for idx in [0, 1, 2, 3]:
        sub.add(CommentGen(sub, " hello "+str(idx)),
                position=["before_index", 2*idx])
    sub.add(ImplicitNoneGen(sub))
    in_idx = line_number(sub.root, "IMPLICIT NONE")
    assert in_idx == 3


def test_imp_none_in_subroutine_already_exists():
    ''' test that implicit none is not added to a subroutine when
    one already exists'''
    module = ModuleGen(name="testmodule")
    sub = SubroutineGen(module, name="testsubroutine", implicitnone=True)
    module.add(sub)
    sub.add(ImplicitNoneGen(sub))
    count = count_lines(sub.root, "IMPLICIT NONE")
    assert count == 1, \
        "There should only be one instance of IMPLICIT NONE"


def test_imp_none_exception_if_wrong_parent():
    ''' test that an exception is thrown if implicit none is added
    and the parent is not a module or a subroutine '''
    module = ModuleGen(name="testmodule")
    sub = SubroutineGen(module, name="testsubroutine")
    module.add(sub)
    dogen = DoGen(sub, "i", "1", "10")
    sub.add(dogen)
    with pytest.raises(Exception):
        dogen.add(ImplicitNoneGen(dogen))


def test_subgen_implicit_none_false():
    ''' test that implicit none is not added to the subroutine if
    not requested '''
    module = ModuleGen(name="testmodule")
    sub = SubroutineGen(module, name="testsubroutine", implicitnone=False)
    module.add(sub)
    count = count_lines(sub.root, "IMPLICIT NONE")
    assert count == 0, "IMPLICIT NONE SHOULD NOT EXIST"


def test_subgen_implicit_none_true():
    ''' test that implicit none is added to the subroutine if
    requested '''
    module = ModuleGen(name="testmodule")
    sub = SubroutineGen(module, name="testsubroutine", implicitnone=True)
    module.add(sub)
    count = count_lines(sub.root, "IMPLICIT NONE")
    assert count == 1, "IMPLICIT NONE SHOULD EXIST"


def test_subgen_implicit_none_default():
    ''' test that implicit none is not added to the subroutine by
    default '''
    module = ModuleGen(name="testmodule")
    sub = SubroutineGen(module, name="testsubroutine")
    module.add(sub)
    count = count_lines(sub.root, "IMPLICIT NONE")
    assert count == 0, "IMPLICIT NONE SHOULD NOT EXIST BY DEFAULT"


def test_subgen_args():
    ''' Test that the args property works as expected '''
    module = ModuleGen(name="testmodule")
    sub = SubroutineGen(module, name="testsubroutine",
                        args=["arg1", "arg2"])
    my_args = sub.args
    assert len(my_args) == 2


def test_directive_wrong_type():
    ''' Check that we raise an error if we request a Directive of
    unrecognised type '''
    from psyclone.psyGen import Node
    parent = Node()
    with pytest.raises(RuntimeError) as err:
        _ = DirectiveGen(parent,
                         "some_dir_type", "begin", "do",
                         "schedule(static)")
    assert "unsupported directive language" in str(err)


def test_ompdirective_wrong():
    ''' Check that we raise an error if we request an OMP Directive of
    unrecognised type '''
    from psyclone.psyGen import Node
    parent = Node()
    with pytest.raises(RuntimeError) as err:
        _ = DirectiveGen(parent,
                         "omp", "begin", "dosomething",
                         "schedule(static)")
    assert "unrecognised directive type" in str(err)


def test_ompdirective_wrong_posn():
    ''' Check that we raise an error if we request an OMP Directive with
    an invalid position '''
    from psyclone.psyGen import Node
    parent = Node()
    with pytest.raises(RuntimeError) as err:
        _ = DirectiveGen(parent,
                         "omp", "start", "do",
                         "schedule(static)")
    assert "unrecognised position 'start'" in str(err)


def test_ompdirective_type():
    ''' Check that we can query the type of an OMP Directive '''
    from psyclone.psyGen import Node
    parent = Node()
    dirgen = DirectiveGen(parent,
                          "omp", "begin", "do",
                          "schedule(static)")
    ompdir = dirgen.root
    assert ompdir.type == "do"


def test_basegen_add_auto():
    ''' Check that attempting to call add on BaseGen raises an error if
    position is "auto"'''
    from psyclone.psyGen import Node
    from psyclone.f2pygen import BaseGen
    parent = Node()
    bgen = BaseGen(parent, parent)
    obj = Node()
    with pytest.raises(Exception) as err:
        bgen.add(obj, position=['auto'])
    assert "auto option must be implemented by the sub" in str(err)


def test_basegen_add_invalid_posn():
    '''Check that attempting to call add on BaseGen with an invalid
    position argument raises an error'''
    from psyclone.psyGen import Node
    from psyclone.f2pygen import BaseGen
    parent = Node()
    bgen = BaseGen(parent, parent)
    obj = Node()
    with pytest.raises(Exception) as err:
        bgen.add(obj, position=['wrong'])
    assert "supported positions are ['append', 'first'" in str(err)


def test_basegen_append():
    '''Check that we can append an object to the tree'''
    module = ModuleGen(name="testmodule")
    sub = SubroutineGen(module, name="testsubroutine")
    module.add(sub)
    sub.add(DeclGen(sub, datatype="integer",
                    entity_decls=["var1"]))
    sub.add(CommentGen(sub, " hello"), position=["append"])
    cindex = line_number(sub.root, "hello")
    assert cindex == 3


def test_basegen_append_default():
    ''' Check if no position argument is supplied to BaseGen.add() then it
    defaults to appending '''
    from psyclone.f2pygen import BaseGen
    module = ModuleGen(name="testmodule")
    sub = SubroutineGen(module, name="testsubroutine")
    module.add(sub)
    BaseGen.add(sub, DeclGen(sub, datatype="integer",
                             entity_decls=["var1"]))
    BaseGen.add(sub, CommentGen(sub, " hello"))
    cindex = line_number(sub.root, "hello")
    assert cindex == 3


def test_basegen_first():
    '''Check that we can insert an object as the first child'''
    module = ModuleGen(name="testmodule")
    sub = SubroutineGen(module, name="testsubroutine")
    module.add(sub)
    sub.add(DeclGen(sub, datatype="integer",
                    entity_decls=["var1"]))
    sub.add(CommentGen(sub, " hello"), position=["first"])
    cindex = line_number(sub.root, "hello")
    assert cindex == 1


def test_basegen_after_index():
    '''Check that we can insert an object using "after_index"'''
    module = ModuleGen(name="testmodule")
    sub = SubroutineGen(module, name="testsubroutine")
    module.add(sub)
    sub.add(DeclGen(sub, datatype="integer",
                    entity_decls=["var1"]))
    sub.add(DeclGen(sub, datatype="integer",
                    entity_decls=["var2"]))
    sub.add(CommentGen(sub, " hello"), position=["after_index", 1])
    # The code checked by line_number() *includes* the SUBROUTINE
    # statement (which is obviously not a child of the SubroutineGen
    # object) and therefore the index it returns is 1 greater than we
    # might expect.
    assert line_number(sub.root, "hello") == 3


def test_basegen_before_error():
    '''Check that we raise an error when attempting to insert an object
    before another object that is not present in the tree'''
    module = ModuleGen(name="testmodule")
    sub = SubroutineGen(module, name="testsubroutine")
    module.add(sub)
    sub.add(DeclGen(sub, datatype="integer",
                    entity_decls=["var1"]))
    sub.add(DeclGen(sub, datatype="integer",
                    entity_decls=["var2"]))
    # Create an object but do not add it as a child of sub
    dgen = DeclGen(sub, datatype="real",
                   entity_decls=["rvar1"])
    # Try to add an object before the orphan dgen
    with pytest.raises(RuntimeError) as err:
        sub.add(CommentGen(sub, " hello"), position=["before", dgen])
    assert "Failed to find supplied object" in str(err)


def test_basegen_last_declaration_no_vars():
    '''Check that we raise an error when requesting the position of the
    last variable declaration if we don't have any variables'''
    module = ModuleGen(name="testmodule")
    sub = SubroutineGen(module, name="testsubroutine")
    module.add(sub)
    # Request the position of the last variable declaration
    # even though we haven't got any
    with pytest.raises(RuntimeError) as err:
        sub.last_declaration()
    assert "no variable declarations found" in str(err)


def test_basegen_start_parent_loop_dbg(capsys):
    '''Check the debug option to the start_parent_loop method'''
    module = ModuleGen(name="testmodule")
    sub = SubroutineGen(module, name="testsubroutine")
    module.add(sub)
    loop = DoGen(sub, "it", "1", "10")
    sub.add(loop)
    call = CallGen(loop, "testcall")
    loop.add(call)
    call.start_parent_loop(debug=True)
    out, _ = capsys.readouterr()
    print out
    expected = ("Parent is a do loop so moving to the parent\n"
                "The type of the current node is now <class "
                "'fparser.block_statements.Do'>\n"
                "The type of parent is <class "
                "'fparser.block_statements.Subroutine'>\n"
                "Finding the loops position in its parent ...\n"
                "The loop's index is  0\n")
    assert expected in out


def test_basegen_start_parent_loop_not_first_child_dbg(capsys):
    '''Check the debug option to the start_parent_loop method when the loop
    is not the first child of the subroutine'''
    module = ModuleGen(name="testmodule")
    sub = SubroutineGen(module, name="testsubroutine")
    module.add(sub)
    call0 = CallGen(sub, "testcall")
    sub.add(call0)
    loop = DoGen(sub, "it", "1", "10")
    sub.add(loop)
    call = CallGen(loop, "testcall")
    loop.add(call)
    call.start_parent_loop(debug=True)
    out, _ = capsys.readouterr()
    print out
    expected = ("Parent is a do loop so moving to the parent\n"
                "The type of the current node is now <class "
                "'fparser.block_statements.Do'>\n"
                "The type of parent is <class "
                "'fparser.block_statements.Subroutine'>\n"
                "Finding the loops position in its parent ...\n"
                "The loop's index is  1\n")
    assert expected in out


def test_basegen_start_parent_loop_omp_begin_dbg(capsys):
    '''Check the debug option to the start_parent_loop method when we have
    an OpenMP begin directive'''
    module = ModuleGen(name="testmodule")
    sub = SubroutineGen(module, name="testsubroutine")
    module.add(sub)
    dgen = DirectiveGen(sub, "omp", "begin", "do", "schedule(static)")
    sub.add(dgen)
    loop = DoGen(sub, "it", "1", "10")
    sub.add(loop)
    call = CallGen(loop, "testcall")
    loop.add(call)
    call.start_parent_loop(debug=True)
    out, _ = capsys.readouterr()
    print out
    expected = ("Parent is a do loop so moving to the parent\n"
                "The type of the current node is now <class "
                "'fparser.block_statements.Do'>\n"
                "The type of parent is <class "
                "'fparser.block_statements.Subroutine'>\n"
                "Finding the loops position in its parent ...\n"
                "The loop's index is  1\n"
                "The type of the object at the index is <class "
                "'fparser.block_statements.Do'>\n"
                "If preceding node is a directive then move back one\n"
                "preceding node is a directive so find out what type ...\n")
    assert expected in out


def test_basegen_start_parent_loop_omp_end_dbg(capsys):
    '''Check the debug option to the start_parent_loop method when we have
    an OpenMP end directive'''
    module = ModuleGen(name="testmodule")
    sub = SubroutineGen(module, name="testsubroutine")
    module.add(sub)
    dgen = DirectiveGen(sub, "omp", "end", "do", "")
    sub.add(dgen)
    loop = DoGen(sub, "it", "1", "10")
    sub.add(loop)
    call = CallGen(loop, "testcall")
    loop.add(call)
    call.start_parent_loop(debug=True)
    out, _ = capsys.readouterr()
    print out
    expected = ("Parent is a do loop so moving to the parent\n"
                "The type of the current node is now <class "
                "'fparser.block_statements.Do'>\n"
                "The type of parent is <class "
                "'fparser.block_statements.Subroutine'>\n"
                "Finding the loops position in its parent ...\n"
                "The loop's index is  1\n"
                "The type of the object at the index is <class "
                "'fparser.block_statements.Do'>\n"
                "If preceding node is a directive then move back one\n"
                "preceding node is a directive so find out what type ...\n")

    assert expected in out


def test_basegen_start_parent_loop_no_loop_dbg():
    '''Check the debug option to the start_parent_loop method when we have
    no loop'''
    module = ModuleGen(name="testmodule")
    sub = SubroutineGen(module, name="testsubroutine")
    module.add(sub)
    dgen = DirectiveGen(sub, "omp", "end", "do", "")
    sub.add(dgen)
    call = CallGen(sub, name="testcall", args=["a", "b"])
    sub.add(call)
    with pytest.raises(RuntimeError) as err:
        call.start_parent_loop(debug=True)
    assert "This node has no enclosing Do loop" in str(err)


def test_progunitgen_multiple_generic_use():
    '''Check that we correctly handle the case where duplicate use statements
    are added'''
    module = ModuleGen(name="testmodule")
    sub = SubroutineGen(module, name="testsubroutine")
    module.add(sub)
    sub.add(UseGen(sub, name="fred"))
    sub.add(UseGen(sub, name="fred"))
    assert count_lines(sub.root, "USE fred") == 1


def test_progunitgen_multiple_use1():
    '''Check that we correctly handle the case where duplicate use statements
    are added but one is specific'''
    module = ModuleGen(name="testmodule")
    sub = SubroutineGen(module, name="testsubroutine")
    module.add(sub)
    sub.add(UseGen(sub, name="fred"))
    sub.add(UseGen(sub, name="fred", only=True, funcnames=["astaire"]))
    assert count_lines(sub.root, "USE fred") == 1


def test_progunitgen_multiple_use2():
    '''Check that we correctly handle the case where the same module
    appears in two use statements but, because the first use is
    specific, the second, generic use is included.

    '''
    module = ModuleGen(name="testmodule")
    sub = SubroutineGen(module, name="testsubroutine")
    module.add(sub)
    sub.add(UseGen(sub, name="fred", only=True, funcnames=["astaire"]))
    sub.add(UseGen(sub, name="fred"))
    assert count_lines(sub.root, "USE fred") == 2


def test_progunit_multiple_use3():
    '''Check that we correctly handle the case where the same module is
    specified in two UseGen objects statements both of which are
    specific and they have overlapping variable names.

    '''
    module = ModuleGen(name="testmodule")
    sub = SubroutineGen(module, name="testsubroutine")
    module.add(sub)
    funcnames = ["a", "b", "c"]
    sub.add(UseGen(sub, name="fred", only=True, funcnames=funcnames))
    funcnames = ["c", "d"]
    sub.add(UseGen(sub, name="fred", only=True, funcnames=funcnames))
    gen = str(sub.root)
    expected = (
        "      USE fred, ONLY: d\n"
        "      USE fred, ONLY: a, b, c")
    assert expected in gen
    assert count_lines(sub.root, "USE fred") == 2
    # ensure that the input list does not get modified
    assert funcnames == ["c", "d"]


def test_adduse_empty_only():
    ''' Test that the adduse module method works correctly when we specify
    that we want it to be specific but then don't provide a list of
    entities for the only qualifier '''
    module = ModuleGen(name="testmodule")
    sub = SubroutineGen(module, name="testsubroutine")
    module.add(sub)
    from psyclone.f2pygen import adduse
    # Add a use statement with only=True but an empty list of entities
    adduse("fred", sub.root, only=True, funcnames=[])
    assert count_lines(sub.root, "USE fred") == 1
    assert count_lines(sub.root, "USE fred, only") == 0


def test_adduse():
    ''' Test that the adduse module method works correctly when we use a
    call object as our starting point '''
    module = ModuleGen(name="testmodule")
    sub = SubroutineGen(module, name="testsubroutine")
    module.add(sub)
    call = CallGen(sub, name="testcall", args=["a", "b"])
    sub.add(call)
    from psyclone.f2pygen import adduse
    adduse("fred", call.root, only=True, funcnames=["astaire"])
    gen = str(sub.root)
    expected = ("    SUBROUTINE testsubroutine()\n"
                "      USE fred, ONLY: astaire\n")
    assert expected in gen


def test_adduse_default_funcnames():
    ''' Test that the adduse module method works correctly when we do
    not specify a list of funcnames '''
    module = ModuleGen(name="testmodule")
    sub = SubroutineGen(module, name="testsubroutine")
    module.add(sub)
    call = CallGen(sub, name="testcall", args=["a", "b"])
    sub.add(call)
    from psyclone.f2pygen import adduse
    adduse("fred", call.root)
    gen = str(sub.root)
    expected = ("    SUBROUTINE testsubroutine()\n"
                "      USE fred\n")
    assert expected in gen


def test_declgen_wrong_type():
    ''' Check that we raise an appropriate error if we attempt to create
    a DeclGen for an unsupported type '''
    module = ModuleGen(name="testmodule")
    sub = SubroutineGen(module, name="testsubroutine")
    module.add(sub)
    with pytest.raises(RuntimeError) as err:
        _ = DeclGen(sub, datatype="complex",
                    entity_decls=["rvar1"])
    assert "Only integer and real are currently supported" in str(err)


def test_declgen_missing_names():
    ''' Check that we raise an error if we attempt to create a DeclGen
    without naming the variable(s) '''
    module = ModuleGen(name="testmodule")
    sub = SubroutineGen(module, name="testsubroutine")
    module.add(sub)
    with pytest.raises(RuntimeError) as err:
        _ = DeclGen(sub, datatype="integer")
    assert ("Cannot create a variable declaration without specifying "
            "the name" in str(err))


def test_typedeclgen_names():
    ''' Check that the names method of TypeDeclGen works as expected '''
    module = ModuleGen(name="testmodule")
    sub = SubroutineGen(module, name="testsubroutine")
    module.add(sub)
    dgen = TypeDeclGen(sub, datatype="my_type",
                       entity_decls=["type1"])
    sub.add(dgen)
    names = dgen.names
    assert len(names) == 1
    assert names[0] == "type1"


def test_typedeclgen_missing_names():
    ''' Check that we raise an error if we attempt to create TypeDeclGen
    without naming the variables '''
    module = ModuleGen(name="testmodule")
    sub = SubroutineGen(module, name="testsubroutine")
    module.add(sub)
    with pytest.raises(RuntimeError) as err:
        _ = TypeDeclGen(sub, datatype="my_type")
    assert ("Cannot create a declaration of a derived-type variable "
            "without specifying" in str(err))


def test_typedeclgen_multiple_use():
    '''Check that we correctly handle the case where data of the same type
    has already been declared. '''

    module = ModuleGen(name="testmodule")
    sub = SubroutineGen(module, name="testsubroutine")
    module.add(sub)
    # first declaration
    datanames = ["type1"]
    sub.add(TypeDeclGen(sub, datatype="my_type",
                        entity_decls=datanames))
    gen = str(sub.root)
    # second declaration
    datanames = ["type1", "type2"]
    sub.add(TypeDeclGen(sub, datatype="my_type",
                        entity_decls=datanames))
    gen = str(sub.root)
    print gen
    expected = (
        "      TYPE(my_type) type2\n"
        "      TYPE(my_type) type1")
    assert expected in gen
    # check input data is not modified
    assert datanames == ["type1", "type2"]


def test_typedeclgen_multiple_use2():
    '''Check that we do not correctly handle the case where data of a
    different type with the same name has already been declared.'''

    module = ModuleGen(name="testmodule")
    sub = SubroutineGen(module, name="testsubroutine")
    module.add(sub)
    # first declaration
    datanames = ["type1"]
    sub.add(TypeDeclGen(sub, datatype="my_type",
                        entity_decls=datanames))
    gen = str(sub.root)
    # second declaration
    datanames = ["type1", "type2"]
    sub.add(TypeDeclGen(sub, datatype="my_type2",
                        entity_decls=datanames))
    gen = str(sub.root)
    print gen
    expected = (
        "      TYPE(my_type2) type1, type2\n"
        "      TYPE(my_type) type1")
    assert expected in gen
    # check input data is not modified
    assert datanames == ["type1", "type2"]


def test_declgen_multiple_use():
    '''Check that we correctly handle the case where data of the same type
    has already been delared.'''

    module = ModuleGen(name="testmodule")
    sub = SubroutineGen(module, name="testsubroutine")
    module.add(sub)
    # first declaration
    datanames = ["i1"]
    sub.add(DeclGen(sub, datatype="integer",
                    entity_decls=datanames))
    gen = str(sub.root)
    # second declaration
    datanames = ["i1", "i2"]
    sub.add(DeclGen(sub, datatype="integer",
                    entity_decls=datanames))
    gen = str(sub.root)
    print gen
    expected = (
        "      INTEGER i2\n"
        "      INTEGER i1")
    assert expected in gen
    # check input data is not modified
    assert datanames == ["i1", "i2"]


def test_declgen_multiple_use2():
    '''Check that we don't correctly handle the case where data of a
    different type has already been delared. '''

    module = ModuleGen(name="testmodule")
    sub = SubroutineGen(module, name="testsubroutine")
    module.add(sub)
    # first declaration
    datanames = ["data1"]
    sub.add(DeclGen(sub, datatype="real",
                    entity_decls=datanames))
    gen = str(sub.root)
    # second declaration
    datanames = ["data1", "data2"]
    sub.add(DeclGen(sub, datatype="integer",
                    entity_decls=datanames))
    gen = str(sub.root)
    print gen
    expected = (
        "      INTEGER data1, data2\n"
        "      REAL data1")
    assert expected in gen
    # check input data is not modified
    assert datanames == ["data1", "data2"]


@pytest.mark.xfail(reason="No way to add body of DEFAULT clause")
def test_selectiongen():
    ''' Check that SelectionGen works as expected '''
    from psyclone.f2pygen import SelectionGen
    module = ModuleGen(name="testmodule")
    sub = SubroutineGen(module, name="testsubroutine")
    module.add(sub)
    sgen = SelectionGen(sub, expr="my_var")
    sub.add(sgen)
    agen = AssignGen(sgen, lhs="happy", rhs=".TRUE.")
    sgen.addcase("1", [agen])
    # TODO how do we specify what happens in the default case?
    sgen.adddefault()
    gen = str(sub.root)
    print gen
    expected = ("SELECT CASE ( my_var )\n"
                "CASE ( 1 )\n"
                "        happy = .TRUE.\n"
                "CASE DEFAULT\n"
                "      END SELECT")
    assert expected in gen
    assert False


def test_selectiongen_addcase():
    ''' Check that SelectionGen.addcase() works as expected when no
    content is supplied'''
    from psyclone.f2pygen import SelectionGen
    module = ModuleGen(name="testmodule")
    sub = SubroutineGen(module, name="testsubroutine")
    module.add(sub)
    sgen = SelectionGen(sub, expr="my_var")
    sub.add(sgen)
    sgen.addcase("1")
    gen = str(sub.root)
    print gen
    expected = ("      SELECT CASE ( my_var )\n"
                "        CASE ( 1 )\n"
                "      END SELECT")
    assert expected in gen


@pytest.mark.xfail(reason="Adding a CASE to a SELECT TYPE does not work")
def test_typeselectiongen():
    ''' Check that SelectionGen works as expected for a type '''
    from psyclone.f2pygen import SelectionGen
    module = ModuleGen(name="testmodule")
    sub = SubroutineGen(module, name="testsubroutine")
    module.add(sub)
    sgen = SelectionGen(sub, expr="my_var=>another_var", typeselect=True)
    sub.add(sgen)
    agen = AssignGen(sgen, lhs="happy", rhs=".TRUE.")
    sgen.addcase("fspace", [agen])
    sgen.adddefault()
    gen = str(sub.root)
    print gen
    assert "SELECT TYPE ( my_var=>another_var )" in gen
    assert "TYPE IS ( fspace )" in gen


def test_modulegen_add_wrong_parent():
    ''' Check that attempting to add an object to a ModuleGen fails
    if the object's parent is not that ModuleGen '''
    module = ModuleGen(name="testmodule")
    module_wrong = ModuleGen(name="another_module")
    sub = SubroutineGen(module_wrong, name="testsubroutine")
    with pytest.raises(RuntimeError) as err:
        module.add(sub)
    print str(err)
    assert "because it is not a descendant of it or of any of" in str(err)


def test_do_loop_with_increment():
    ''' Test that we correctly generate code for a do loop with
    non-unit increment '''
    module = ModuleGen(name="testmodule")
    sub = SubroutineGen(module, name="testsub")
    module.add(sub)
    dogen = DoGen(sub, "it", "1", "10", step="2")
    sub.add(dogen)
    count = count_lines(sub.root, "DO it=1,10,2")
    assert count == 1


def test_do_loop_add_after():
    ''' Test that we correctly generate code for a do loop when adding a
    child to it with position *after* '''
    module = ModuleGen(name="testmodule")
    sub = SubroutineGen(module, name="testsub")
    module.add(sub)
    dogen = DoGen(sub, "it", "1", "10", step="2")
    sub.add(dogen)
    assign1 = AssignGen(dogen, lhs="happy", rhs=".TRUE.")
    dogen.add(assign1)
    assign2 = AssignGen(dogen, lhs="sad", rhs=".FALSE.")
    dogen.add(assign2, position=["before", assign1.root])
    a1_line = line_number(sub.root, "happy = ")
    a2_line = line_number(sub.root, "sad = ")
    assert a1_line > a2_line


def test_basegen_previous_loop_no_loop():
    '''Check that we raise an error when requesting the position of the
    previous loop if we don't have a loop '''
    module = ModuleGen(name="testmodule")
    sub = SubroutineGen(module, name="testsubroutine")
    module.add(sub)
    # Request the position of the last loop
    # even though we haven't got one
    with pytest.raises(RuntimeError) as err:
        sub.previous_loop()
    assert "no loop found - there is no previous loop" in str(err)
