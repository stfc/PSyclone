# -----------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
# -----------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab

''' Performs py.test tests on the psygen module '''

# internal classes requiring tests
# PSy,Invokes,Dependencies,NameSpaceFactory,NameSpace,Invoke,Node,Schedule,
# LoopDirective,OMPLoopDirective,Loop,Call,Inf,SetInfCall,Kern,Arguments,
# InfArguments,Argument,KernelArgument,InfArgument

# user classes requiring tests
# PSyFactory, TransInfo, Transformation
import pytest
from psyGen import TransInfo, Transformation, PSyFactory, NameSpace, \
    NameSpaceFactory, GenerationError
from dynamo0p3 import DynKern, DynKernMetadata
from fparser import api as fpapi


# PSyFactory class unit tests

def test_invalid_api():
    '''test that psyfactory raises appropriate error when an invalid api
    is supplied'''
    with pytest.raises(GenerationError):
        _ = PSyFactory(api="invalid")


def test_psyfactory_valid_return_object():
    '''test that psyfactory returns a psyfactory object for all supported
    inputs'''
    psy_factory = PSyFactory()
    assert isinstance(psy_factory, PSyFactory)
    from config import SUPPORTEDAPIS
    apis = SUPPORTEDAPIS
    apis.insert(0, "")
    for api in apis:
        psy_factory = PSyFactory(api=api)
        assert isinstance(psy_factory, PSyFactory)


def test_psyfactory_valid_dm_flag():
    '''test that a PSyFactory instance raises an exception if the
    optional distributed_memory flag is set to an invalid value
    and does not if the value is valid '''
    with pytest.raises(GenerationError) as excinfo:
        _ = PSyFactory(distributed_memory="ellie")
    assert "distributed_memory flag" in str(excinfo.value)
    _ = PSyFactory(distributed_memory=True)
    _ = PSyFactory(distributed_memory=False)


# TBD need to find a way to create a valid info object to pass to
# create so we can check creation
# def test_create_valid_return_object():
#     from ghproto import
#     GHProtoPSy psy = PSyFactory().create(None) assert
#     isinstance(psy,GHProtoPSy)


# Transformation class unit tests

def test_base_class_not_callable():
    '''make sure we can not instantiate abstract Transformation class
    directly'''
    with pytest.raises(TypeError):
        _ = Transformation()


# TransInfo class unit tests

def test_new_module():
    '''check that we can change the module where we look for
    transformations.  There should be no transformations
    available as the new module uses a different
    transformation base class'''
    from test_files import dummy_transformations
    trans = TransInfo(module=dummy_transformations)
    assert trans.num_trans == 0


def test_new_baseclass():
    '''check that we can change the transformations baseclass. There
    should be no transformations available as the default
    transformations module does not use the specified base
    class'''
    from test_files.dummy_transformations import LocalTransformation
    trans = TransInfo(base_class=LocalTransformation)
    assert trans.num_trans == 0


def test_new_module_and_baseclass():
    '''check that we can change the module where we look for
    transformations and the baseclass. There should be one
    transformation available as the module specifies one test
    transformation using the specified base class '''
    from test_files import dummy_transformations
    trans = TransInfo(module=dummy_transformations,
                      base_class=dummy_transformations.LocalTransformation)
    assert trans.num_trans == 1


def test_list_valid_return_object():
    ''' check the list method returns the valid type '''
    trans = TransInfo()
    assert isinstance(trans.list, str)


def test_list_return_data():
    ''' check the list method returns sensible information '''
    trans = TransInfo()
    assert trans.list.find("available") != -1


def test_invalid_low_number():
    '''check an out-of-range low number for get_trans_num method raises
    correct exception'''
    trans = TransInfo()
    with pytest.raises(GenerationError):
        _ = trans.get_trans_num(0)


def test_invalid_high_number():
    '''check an out-of-range high number for get_trans_num method raises
    correct exception'''
    trans = TransInfo()
    with pytest.raises(GenerationError):
        _ = trans.get_trans_num(999)


def test_valid_return_object_from_number():
    ''' check get_trans_num method returns expected type of instance '''
    trans = TransInfo()
    transform = trans.get_trans_num(1)
    assert isinstance(transform, Transformation)


def test_invalid_name():
    '''check get_trans_name method fails correctly when an invalid name
    is provided'''
    trans = TransInfo()
    with pytest.raises(GenerationError):
        _ = trans.get_trans_name("invalid")


def test_valid_return_object_from_name():
    ''' check get_trans_name method return the correct object type '''
    trans = TransInfo()
    transform = trans.get_trans_name("LoopFuse")
    assert isinstance(transform, Transformation)


# NameSpace class unit tests

def test_fail_context_label():
    '''check an error is raised if one of context and label is not None'''
    namespace = NameSpace()
    with pytest.raises(RuntimeError):
        namespace.create_name(context="dummy_context")
    with pytest.raises(RuntimeError):
        namespace.create_name(label="dummy_context")


def test_case_sensitive_names():
    ''' tests that in the case sensitive option, names that only differ by
    case are treated as being distinct'''
    namespace_cs = NameSpace(case_sensitive=True)
    name = "Rupert"
    name1 = namespace_cs.create_name(root_name=name)
    name2 = namespace_cs.create_name(root_name=name.lower())
    assert name1 == name
    assert name2 == name.lower()


def test_case_insensitive_names():
    ''' tests that in the case insensitive option (the default), names that
    only differ by case are treated as being the same '''
    namespace = NameSpace()
    name = "Rupert"
    name1 = namespace.create_name(root_name=name)
    name2 = namespace.create_name(root_name=name.lower())
    assert name1 == name.lower()
    assert name2 == name1 + "_1"


def test_new_labels():
    '''tests that different labels and contexts are treated as being
    distinct'''
    namespace = NameSpace()
    name = "Rupert"
    name1 = namespace.create_name(root_name=name, context="home",
                                  label="me")
    name2 = namespace.create_name(root_name=name, context="work",
                                  label="me")
    name3 = namespace.create_name(root_name=name, context="home",
                                  label="a bear")
    name4 = namespace.create_name(root_name=name, context="work",
                                  label="a bear")
    assert name1 == name.lower()
    assert name2 == name1+"_1"
    assert name3 == name1+"_2"
    assert name4 == name1+"_3"


def test_new_labels_case_sensitive():
    '''tests that different labels and contexts are treated as being
    distinct for case sensitive names'''
    namespace = NameSpace(case_sensitive=True)
    name = "Rupert"
    name1 = namespace.create_name(root_name=name, context="home",
                                  label="me")
    name2 = namespace.create_name(root_name=name, context="work",
                                  label="me")
    name3 = namespace.create_name(root_name=name, context="home",
                                  label="Me")
    name4 = namespace.create_name(root_name=name, context="Work",
                                  label="me")
    assert name1 == name
    assert name2 == name1+"_1"
    assert name3 == name1+"_2"
    assert name4 == name1+"_3"


def test_existing_labels():
    '''tests that existing labels and contexts return the previous name'''
    namespace = NameSpace()
    name = "Rupert"
    name1 = namespace.create_name(root_name=name, context="home",
                                  label="me")
    name2 = namespace.create_name(root_name=name, context="work",
                                  label="me")
    name3 = namespace.create_name(root_name=name, context="home",
                                  label="Me")
    name4 = namespace.create_name(root_name=name, context="Work",
                                  label="me")
    assert name1 == name.lower()
    assert name2 == name1+"_1"
    assert name3 == name1
    assert name4 == name2


def test_existing_labels_case_sensitive():
    '''tests that existing labels and contexts return the previous name'''
    namespace = NameSpace(case_sensitive=True)
    name = "Rupert"
    name1 = namespace.create_name(root_name=name, context="home",
                                  label="me")
    name2 = namespace.create_name(root_name=name, context="Work",
                                  label="Me")
    name3 = namespace.create_name(root_name=name, context="home",
                                  label="me")
    name4 = namespace.create_name(root_name=name, context="Work",
                                  label="Me")
    assert name1 == name
    assert name2 == name1+"_1"
    assert name3 == name1
    assert name4 == name2


def test_reserved_names():
    '''tests that reserved names are not returned by the name space
    manager'''
    namea = "PSyclone"
    nameb = "Dynamo"
    namespace = NameSpace()
    namespace.add_reserved_name(namea)
    name1 = namespace.create_name(root_name=namea.lower())
    assert name1 == namea.lower()+"_1"
    namespace.add_reserved_names([nameb.lower()])
    name1 = namespace.create_name(root_name=nameb)
    assert name1 == nameb.lower()+"_1"


def test_reserved_names_case_sensitive():
    '''tests that reserved names are not returned by the case sensitive
    name space manager'''
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


def test_reserved_name_exists():
    '''tests that an error is generated if a reserved name has already
    been used as a name'''
    name = "PSyclone"
    namespace = NameSpace()
    _ = namespace.create_name(root_name=name)
    with pytest.raises(RuntimeError):
        namespace.add_reserved_name(name)
    with pytest.raises(RuntimeError):
        namespace.add_reserved_name(name.lower())


def test_reserved_name_exists_case_sensitive():
    '''tests that an error is generated if a reserved name has already
    been used as a name'''
    name = "PSyclone"
    namespace = NameSpace(case_sensitive=True)
    _ = namespace.create_name(root_name=name)
    namespace.add_reserved_name(name.lower())
    with pytest.raises(RuntimeError):
        namespace.add_reserved_name(name)
    with pytest.raises(RuntimeError):
        namespace.add_reserved_names([name])


def test_anonymous_name():
    ''' tests that anonymous names are successfully created '''
    namespace = NameSpace()
    name1 = namespace.create_name()
    assert name1 == "anon"
    name2 = namespace.create_name()
    assert name2 == "anon_1"


def test_internal_name_clashes():
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


def test_internal_name_clashes_case_sensitive():
    '''tests that names that are generated internally by the case
    sensitive namespace manager can be used as root names'''
    anon_name = "Anon"
    namespace = NameSpace(case_sensitive=True)
    _ = namespace.create_name()
    name2 = namespace.create_name(root_name=anon_name)
    assert name2 == anon_name
    name3 = namespace.create_name(root_name=anon_name.lower())
    assert name3 == anon_name.lower()+"_1"


# tests that the NameSpaceFactory class is working correctly

def test_create():
    '''tests that a NameSpace object is returned from the create method'''
    nsf = NameSpaceFactory()
    nspace = nsf.create()
    assert isinstance(nspace, NameSpace)


def test_singleton():
    '''test that the same NameSpace object is returned from different
    NameSpaceFactory's by default'''
    nsf = NameSpaceFactory()
    ns1 = nsf.create()
    nsf = NameSpaceFactory()
    ns2 = nsf.create()
    assert ns1 == ns2


def test_reset():
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
    out, _ = capsys.readouterr()
    expected_output = \
        "KernCall dummy_code(field_1) [module_inline=False]"
    assert expected_output in out
