from f2pygen import ModuleGen, CommentGen, SubroutineGen, DoGen, CallGen
import pytest

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


