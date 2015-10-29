#-------------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
#-------------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab

# internal classes requiring tests
# PSy,Invokes,Dependencies,NameSpaceFactory,NameSpace,Invoke,Node,Schedule,LoopDirective,OMPLoopDirective,Loop,Call,Inf,SetInfCall,Kern,Arguments,InfArguments,Argument,KernelArgument,InfArgument

# user classes requiring tests
# PSyFactory, TransInfo, Transformation
from psyGen import TransInfo, Transformation, PSyFactory, NameSpace, NameSpaceFactory, GenerationError
from dynamo0p3 import DynKern, DynKernMetadata
from fparser import api as fpapi

class TestPSyFactoryClass:
    ''' PSyFactory class unit tests '''

    def test_invalid_api(self):
        ''' test that psyfactory raises appropriate error when an invalid api is supplied '''
        import pytest
        with pytest.raises(GenerationError):
            psy_factory = PSyFactory(api = "invalid")

    def test_psyfactory_valid_return_object(self):
        ''' test that psyfactory returns a psyfactory object for all supported inputs '''
        psy_factory = PSyFactory()
        assert isinstance(psy_factory,PSyFactory)
        for api in ["", "gunghoproto", "dynamo0.1", "gocean0.1"]:
            psy_factory = PSyFactory(api = api)
            assert isinstance(psy_factory,PSyFactory)

    # TBD need to find a way to create a valid info object to pass to create so we can check creation
    #def test_create_valid_return_object(self):
    #    from ghproto import GHProtoPSy
    #    psy = PSyFactory().create(None)
    #    assert isinstance(psy,GHProtoPSy)
import pytest

class TestTransformationClass:
    ''' Transformation class unit tests '''

    def test_base_class_not_callable(self):
        ''' make sure we can not instantiate abstract Transformation class directly '''
        with pytest.raises(TypeError):        
            t=Transformation()

class TestTransInfoClass:
    ''' TransInfo class unit tests '''

    def test_new_module(self):
        ''' check that we can change the module where we look for transformations.
            There should be no transformations available as the new module uses a
            different transformation base class '''
        from test_files import dummy_transformations
        t=TransInfo(module=dummy_transformations)
        assert t.num_trans==0

    def test_new_baseclass(self):
        ''' check that we can change the transformations baseclass. There should
            be no transformations available as the default transformations module
            does not use the specified base class '''
        from test_files.dummy_transformations import LocalTransformation
        t=TransInfo(base_class=LocalTransformation)
        assert t.num_trans==0

    def test_new_module_and_baseclass(self):
        ''' check that we can change the module where we look for transformations
            and the baseclass. There should be one transformation available as the
            module specifies one test transformation using the specified base
            class '''
        from test_files import dummy_transformations
        t=TransInfo(module=dummy_transformations,base_class=dummy_transformations.LocalTransformation)
        assert t.num_trans==1

    def test_list_valid_return_object(self):
        ''' check the list method returns the valid type '''
        t=TransInfo()
        assert isinstance(t.list,str)

    def test_list_return_data(self):
        ''' check the list method returns sensible information '''
        t=TransInfo()
        assert t.list.find("available")!=-1

    def test_invalid_low_number(self):
        ''' check an out-of-range low number for get_trans_num method raises correct exception '''
        t=TransInfo()
        with pytest.raises(GenerationError):
            transform=t.get_trans_num(0)

    def test_invalid_high_number(self):
        ''' check an out-of-range high number for get_trans_num method raises correct exception '''
        t=TransInfo()
        with pytest.raises(GenerationError):
            transform=t.get_trans_num(999)

    def test_valid_return_object_from_number(self):
        ''' check get_trans_num method returns expected type of instance '''
        t=TransInfo()
        transform=t.get_trans_num(1)
        assert isinstance(transform,Transformation)

    def test_invalid_name(self):
        ''' check get_trans_name method fails correctly when an invalid name is provided '''
        t=TransInfo()
        with pytest.raises(GenerationError):
            transform=t.get_trans_name("invalid")

    def test_valid_return_object_from_name(self):
        ''' check get_trans_name method return the correct object type '''
        t=TransInfo()
        transform=t.get_trans_name("LoopFuse")
        assert isinstance(transform,Transformation)

class TestNameSpaceClass(object):
    ''' NameSpace class unit tests '''

    def test_fail_context_label(self):
        ''' check an error is raised if one of context and label is not None '''
        namespace = NameSpace()
        with pytest.raises(RuntimeError):
            namespace.create_name(context="dummy_context")
        with pytest.raises(RuntimeError):
            namespace.create_name(label="dummy_context")

    def test_case_sensitive_names(self):
        ''' tests that in the case sensitive option, names that only differ by
            case are treated as being distinct'''
        namespace_cs = NameSpace(case_sensitive=True)
        name = "Rupert"
        name1 = namespace_cs.create_name(root_name=name)
        name2 = namespace_cs.create_name(root_name=name.lower())
        assert name1 == name
        assert name2 == name.lower()

    def test_case_insensitive_names(self):
        ''' tests that in the case insensitive option (the default), names that
           only differ by case are treated as being the same '''
        namespace = NameSpace()
        name = "Rupert"
        name1 = namespace.create_name(root_name=name)
        name2 = namespace.create_name(root_name=name.lower())
        assert name1 == name.lower()
        assert name2 == name1+"_1"

    def test_new_labels(self):
        ''' tests that different labels and contexts are treated as being distinct '''
        namespace = NameSpace()
        name = "Rupert"
        name1 = namespace.create_name(root_name=name,context="home",label="me")
        name2 = namespace.create_name(root_name=name,context="work",label="me")
        name3 = namespace.create_name(root_name=name,context="home",label="a bear")
        name4 = namespace.create_name(root_name=name,context="work",label="a bear")
        assert name1 == name.lower()
        assert name2 == name1+"_1"
        assert name3 == name1+"_2"
        assert name4 == name1+"_3"

    def test_new_labels_case_sensitive(self):
        ''' tests that different labels and contexts are treated as being distinct for case sensitive names'''
        namespace = NameSpace(case_sensitive=True)
        name = "Rupert"
        name1 = namespace.create_name(root_name=name,context="home",label="me")
        name2 = namespace.create_name(root_name=name,context="work",label="me")
        name3 = namespace.create_name(root_name=name,context="home",label="Me")
        name4 = namespace.create_name(root_name=name,context="Work",label="me")
        assert name1 == name
        assert name2 == name1+"_1"
        assert name3 == name1+"_2"
        assert name4 == name1+"_3"

    def test_existing_labels(self):
        ''' tests that existing labels and contexts return the previous name '''
        namespace = NameSpace()
        name = "Rupert"
        name1 = namespace.create_name(root_name=name,context="home",label="me")
        name2 = namespace.create_name(root_name=name,context="work",label="me")
        name3 = namespace.create_name(root_name=name,context="home",label="Me")
        name4 = namespace.create_name(root_name=name,context="Work",label="me")
        assert name1 == name.lower()
        assert name2 == name1+"_1"
        assert name3 == name1
        assert name4 == name2

    def test_existing_labels_case_sensitive(self):
        ''' tests that existing labels and contexts return the previous name '''
        namespace = NameSpace(case_sensitive=True)
        name = "Rupert"
        name1 = namespace.create_name(root_name=name,context="home",label="me")
        name2 = namespace.create_name(root_name=name,context="Work",label="Me")
        name3 = namespace.create_name(root_name=name,context="home",label="me")
        name4 = namespace.create_name(root_name=name,context="Work",label="Me")
        assert name1 == name
        assert name2 == name1+"_1"
        assert name3 == name1
        assert name4 == name2

    def test_reserved_names(self):
        ''' tests that reserved names are not returned by the name space manager '''
        namea = "PSyclone"
        nameb = "Dynamo"
        namespace = NameSpace()
        namespace.add_reserved_name(namea)
        name1 = namespace.create_name(root_name=namea.lower())
        assert name1 == namea.lower()+"_1"
        namespace.add_reserved_names([nameb.lower()])
        name1 = namespace.create_name(root_name=nameb)
        assert name1 == nameb.lower()+"_1"

    def test_reserved_names_case_sensitive(self):
        ''' tests that reserved names are not returned by the case sensitive name space manager '''
        namea = "PSyclone"
        nameb = "Dynamo"
        namespace = NameSpace(case_sensitive=True)
        namespace.add_reserved_name(namea)
        name1 = namespace.create_name(root_name=namea)
        assert name1 == namea+"_1"
        name1 = namespace.create_name(root_name=namea.lower())
        assert name1 == namea.lower()
        namespace.add_reserved_names([nameb])
        name1 = namespace.create_name(root_name=nameb)
        assert name1 == nameb+"_1"
        name1 = namespace.create_name(root_name=nameb.lower())
        assert name1 == nameb.lower()

    def test_reserved_name_exists(self):
        ''' tests that an error is generated if a reserved name has already been used as a name '''
        name = "PSyclone"
        namespace = NameSpace()
        name1 = namespace.create_name(root_name=name)
        with pytest.raises(RuntimeError):
            namespace.add_reserved_name(name)
        with pytest.raises(RuntimeError):
            namespace.add_reserved_name(name.lower())

    def test_reserved_name_exists_case_sensitive(self):
        ''' tests that an error is generated if a reserved name has already been used as a name '''
        name = "PSyclone"
        namespace = NameSpace(case_sensitive=True)
        name1 = namespace.create_name(root_name=name) 
        namespace.add_reserved_name(name.lower())
        with pytest.raises(RuntimeError):
            namespace.add_reserved_name(name)
        with pytest.raises(RuntimeError):
            namespace.add_reserved_names([name])

    def test_anonymous_name(self):
        ''' tests that anonymous names are successfully created ''' 
        namespace = NameSpace()
        name1 = namespace.create_name()
        assert name1 == "anon"
        name2 = namespace.create_name()
        assert name2 == "anon_1"

    def test_internal_name_clashes(self):
        ''' tests that names that are generated internally by the namespace
           manager can be used as root names'''
        anon_name = "Anon"
        namespace = NameSpace()
        name1 = namespace.create_name()
        name2 = namespace.create_name(root_name=anon_name)
        assert name1 == anon_name.lower()
        assert name2 == name1+"_1"
        name3 = namespace.create_name(root_name=anon_name+"_1")
        assert name3 == name2+"_1"

    def test_internal_name_clashes_case_sensitive(self):
        '''tests that names that are generated internally by the case
           sensitive namespace manager can be used as root names'''
        anon_name = "Anon"
        namespace = NameSpace(case_sensitive=True)
        name1 = namespace.create_name()
        name2 = namespace.create_name(root_name=anon_name)
        assert name2 == anon_name
        name3 = namespace.create_name(root_name=anon_name.lower())
        assert name3 == anon_name.lower()+"_1"

class TestNameSpaceFactoryClass(object):
    ''' tests that the NameSpaceFactory class is working correctly '''

    def test_create(self):
        ''' tests that a NameSpace object is returned from the create method '''
        nsf = NameSpaceFactory()
        ns = nsf.create()
        assert isinstance(ns, NameSpace)

    def test_singleton(self):
        ''' test that the same NameSpace object is returned from different NameSpaceFactory's by default '''
        nsf = NameSpaceFactory()
        ns1 = nsf.create()
        nsf = NameSpaceFactory()
        ns2 = nsf.create()
        assert ns1 == ns2

    def test_reset(self):
        ''' test that different NameSpace objects are returned from different
            NameSpaceFactory's when the reset option is set'''
        nsf = NameSpaceFactory()
        ns1 = nsf.create()
        nsf = NameSpaceFactory(reset=True)
        ns2 = nsf.create()
        assert ns1 != ns2

# Kern class test
def test_kern_class_view(capsys):
    ''' tests the view method in the Kern class. The simplest way to
    do this is via the dynamo0.3 subclass '''
    meta = '''
module dummy_mod
  type, extends(kernel_type) :: dummy_type
     type(arg_type), meta_args(1) =    &
          (/ arg_type(gh_field,gh_write,w1) &
           /)
     integer, parameter :: iterates_over = cells
   contains
     procedure() :: code => dummy_code
  end type dummy_type
contains
  subroutine dummy_code()
  end subroutine dummy_code
end module dummy_mod
'''
    ast = fpapi.parse(meta, ignore_comments=False)
    metadata = DynKernMetadata(ast)
    my_kern = DynKern()
    my_kern.load_meta(metadata)
    my_kern.view()
    out, err = capsys.readouterr()
    expected_output = \
        "KernCall dummy_code(field_1) [module_inline=False]"
    assert expected_output in out
