# -----------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
# -----------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab

''' Performs py.test tests on the psygen module '''

# Since this is a file containing tests which often have to get in and
# change the internal state of objects we disable pylint's warning
# about such accesses
# pylint: disable=protected-access

# internal classes requiring tests
# PSy,Invokes,Dependencies,NameSpaceFactory,NameSpace,Invoke,Node,Schedule,
# LoopDirective,OMPLoopDirective,Loop,Call,Inf,SetInfCall,Kern,Arguments,
# InfArguments,Argument,KernelArgument,InfArgument

# user classes requiring tests
# PSyFactory, TransInfo, Transformation
import os
import pytest
from fparser import api as fpapi
from psyclone.psyGen import TransInfo, Transformation, PSyFactory, NameSpace, \
    NameSpaceFactory, OMPParallelDoDirective, PSy, \
    OMPParallelDirective, OMPDoDirective, OMPDirective, Directive
from psyclone.psyGen import GenerationError, FieldNotFoundError, HaloExchange
from psyclone.dynamo0p3 import DynKern, DynKernMetadata
from psyclone.parse import parse
from psyclone.transformations import OMPParallelLoopTrans, DynamoLoopFuseTrans
from psyclone.generator import generate

BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3")


# PSyFactory class unit tests

def test_invalid_api():
    '''test that psyfactory raises appropriate error when an invalid api
    is supplied'''
    with pytest.raises(GenerationError):
        _ = PSyFactory(api="invalid")


def test_psyfactory_valid_return_object():  # pylint: disable=invalid-name
    '''test that psyfactory returns a psyfactory object for all supported
    inputs'''
    psy_factory = PSyFactory()
    assert isinstance(psy_factory, PSyFactory)
    from psyclone.config import SUPPORTEDAPIS
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


# PSy class unit tests

def test_psy_base_err(monkeypatch):
    ''' Check that we cannot call gen or psy_module on the base class
    directly '''
    # We have no easy way to create the extra information which
    # the PSy constructor requires. Therefore, we use a PSyFactory
    # object and monkey-patch it so that it has a name attribute.
    factory = PSyFactory()
    monkeypatch.setattr(factory, "name",
                        value="fred", raising=False)
    psy = PSy(factory)
    with pytest.raises(NotImplementedError) as excinfo:
        _ = psy.gen
    assert "must be implemented by subclass" in str(excinfo)
    with pytest.raises(NotImplementedError) as excinfo:
        _ = psy.psy_module
    assert "must be implemented by subclass" in str(excinfo)


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


def test_valid_return_object_from_number():  # pylint: disable=invalid-name
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


def test_valid_return_object_from_name():  # pylint: disable=invalid-name
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


def test_existing_labels_case_sensitive():  # pylint: disable=invalid-name
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


def test_reserved_names_case_sensitive():  # pylint: disable=invalid-name
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


def test_reserved_name_exists_case_sensitive():  # pylint: disable=invalid-name
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


def test_intern_name_clash_case_sensitive():  # pylint: disable=invalid-name
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

# tests for class Call


def test_same_name_invalid():
    '''test that we raise an error if the same name is passed into the
    same kernel or built-in instance. We need to choose a particular
    API to check this although the code is in psyGen.py '''
    with pytest.raises(GenerationError) as excinfo:
        _, _ = generate(
            os.path.join(BASE_PATH, "1.10_single_invoke_same_name.f90"),
            api="dynamo0.3")
    assert ("Argument 'f1' is passed into kernel 'testkern_code' code "
            "more than once") in str(excinfo.value)


def test_same_name_invalid_array():
    '''test that we raise an error if the same name is passed into the
    same kernel or built-in instance. In this case arguments have
    array references and mixed case. We need to choose a particular
    API to check this although the code is in psyGen.py. '''
    with pytest.raises(GenerationError) as excinfo:
        _, _ = generate(
            os.path.join(BASE_PATH, "1.11_single_invoke_same_name_array.f90"),
            api="dynamo0.3")
    assert ("Argument 'f1(1, n)' is passed into kernel 'testkern_code' code "
            "more than once") in str(excinfo.value)


def test_derived_type_deref_naming():
    ''' Test that we do not get a name clash for dummy arguments in the PSy
    layer when the name generation for the component of a derived type
    may lead to a name already taken by another argument. '''
    _, invoke = parse(
        os.path.join(BASE_PATH, "1.12_single_invoke_deref_name_clash.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke)
    generated_code = str(psy.gen)
    print generated_code
    output = (
        "    SUBROUTINE invoke_0_testkern_type"
        "(a, f1_my_field, f1_my_field_1, m1, m2)\n"
        "      USE testkern, ONLY: testkern_code\n"
        "      USE mesh_mod, ONLY: mesh_type\n"
        "      REAL(KIND=r_def), intent(in) :: a\n"
        "      TYPE(field_type), intent(inout) :: f1_my_field\n"
        "      TYPE(field_type), intent(in) :: f1_my_field_1, m1, m2\n")
    assert output in generated_code


FAKE_KERNEL_METADATA = '''
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

# Kern class test


def test_kern_class_view(capsys):
    ''' tests the view method in the Kern class. The simplest way to
    do this is via the dynamo0.3 subclass '''
    ast = fpapi.parse(FAKE_KERNEL_METADATA, ignore_comments=False)
    metadata = DynKernMetadata(ast)
    my_kern = DynKern()
    my_kern.load_meta(metadata)
    my_kern.view()
    out, _ = capsys.readouterr()
    expected_output = \
        "KernCall dummy_code(field_1) [module_inline=False]"
    assert expected_output in out


def test_call_local_vars():
    ''' Check that calling the abstract local_vars() method of Call raises
    the expected exception '''
    from psyclone.psyGen import Call, Arguments
    my_arguments = Arguments(None)

    class KernType(object):
        ''' temporary dummy class '''
        def __init__(self):
            self.iterates_over = "stuff"
    my_ktype = KernType()

    class DummyClass(object):
        ''' temporary dummy class '''
        def __init__(self, ktype):
            self.module_name = "dummy_module"
            self.ktype = ktype

    dummy_call = DummyClass(my_ktype)
    my_call = Call(None, dummy_call, "dummy", my_arguments)
    with pytest.raises(NotImplementedError) as excinfo:
        my_call.local_vars()
    assert "Call.local_vars should be implemented" in str(excinfo.value)


def test_written_arg():
    ''' Check that we raise the expected exception when Kern.written_arg()
    is called for a kernel that doesn't have an argument that is written
    to '''
    from psyclone.psyGen import Kern
    # Change the kernel metadata so that the only kernel argument has
    # read access
    import fparser
    fparser.logging.disable('CRITICAL')
    # If we change the meta-data then we trip the check in the parser.
    # Therefore, we change the object produced by parsing the meta-data
    # instead
    ast = fpapi.parse(FAKE_KERNEL_METADATA, ignore_comments=False)
    metadata = DynKernMetadata(ast)
    for descriptor in metadata.arg_descriptors:
        if descriptor.access == "gh_write":
            descriptor._access = "gh_read"
    my_kern = DynKern()
    my_kern.load_meta(metadata)
    with pytest.raises(FieldNotFoundError) as excinfo:
        Kern.written_arg(my_kern,
                         mapping={"write": "gh_write", "readwrite": "gh_inc"})
    assert "does not have an argument with gh_write or gh_inc access" in \
        str(excinfo.value)


def test_ompdo_directive_class_view(capsys):
    '''tests the view method in the OMPDoDirective class. We create a
    sub-class object then call this method from it '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")

    cases = [
        {"current_class": OMPParallelDoDirective,
         "current_string": "[OMP parallel do]"},
        {"current_class": OMPDoDirective, "current_string": "[OMP do]"},
        {"current_class": OMPParallelDirective,
         "current_string": "[OMP parallel]"},
        {"current_class": OMPDirective, "current_string": "[OMP]"},
        {"current_class": Directive, "current_string": ""}]
    for case in cases:
        for dist_mem in [False, True]:

            psy = PSyFactory("dynamo0.3", distributed_memory=dist_mem).\
                create(invoke_info)
            invoke = psy.invokes.invoke_list[0]
            schedule = invoke.schedule
            otrans = OMPParallelLoopTrans()

            if dist_mem:
                idx = 3
            else:
                idx = 0

            _, _ = otrans.apply(schedule.children[idx])
            omp_parallel_loop = schedule.children[idx]

            # call the OMPDirective view method
            case["current_class"].view(omp_parallel_loop)

            out, _ = capsys.readouterr()
            expected_output = (
                "Directive" + case["current_string"] + "\n"
                "    Loop[type='',field_space='w1',it_space='cells']\n"
                "        KernCall testkern_code(a,f1,f2,m1,m2) "
                "[module_inline=False]")

            assert expected_output in out


def test_call_abstract_methods():
    ''' Check that calling __str__() and gen_code() on the base Call
    class raises the expected exception '''
    from psyclone.psyGen import Call
    # Monkey-patch a GenerationError object to mock-up suitable
    # arguments to create a Call
    fake_call = GenerationError("msg")
    fake_ktype = GenerationError("msg")
    fake_ktype.iterates_over = "something"
    fake_call.ktype = fake_ktype
    fake_call.module_name = "a_name"
    from psyclone.psyGen import Arguments
    fake_arguments = Arguments(None)
    my_call = Call(fake_call, fake_call, name="a_name",
                   arguments=fake_arguments)
    with pytest.raises(NotImplementedError) as excinfo:
        my_call.__str__()
    assert "Call.__str__ should be implemented" in str(excinfo.value)

    with pytest.raises(NotImplementedError) as excinfo:
        my_call.gen_code(None)
    assert "Call.gen_code should be implemented" in str(excinfo.value)


def test_haloexchange_unknown_halo_depth():  # pylint: disable=invalid-name
    '''test the case when the halo exchange base class is called without
    a halo depth'''
    halo_exchange = HaloExchange(None, None, None, None, None)
    assert halo_exchange._halo_depth == "unknown"


def test_globalsum_view(capsys):
    '''test the view method in the GlobalSum class. The simplest way to do
    this is to use a dynamo0p3 builtin example which contains a scalar and
    then call view() on that.'''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.9.0_inner_prod_builtin.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    psy.invokes.invoke_list[0].schedule.view()
    output, _ = capsys.readouterr()
    print output
    expected_output = ("GlobalSum[scalar='asum']")
    assert expected_output in output


def test_args_filter():
    '''the args_filter() method is in both Loop() and Arguments() classes
    with the former method calling the latter. This example tests the
    case when unique is set to True and therefore any replicated names
    are not returned. The simplest way to do this is to use a
    dynamo0p3 example which includes two kernels which share argument
    names. We choose dm=False to make it easier to fuse the loops.'''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1.2_multi_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=False).create(invoke_info)
    # fuse our loops so we have more than one Kernel in a loop
    schedule = psy.invokes.invoke_list[0].schedule
    ftrans = DynamoLoopFuseTrans()
    schedule, _ = ftrans.apply(schedule.children[0],
                               schedule.children[1])
    # get our loop and call our method ...
    loop = schedule.children[0]
    args = loop.args_filter(unique=True)
    expected_output = ["a", "f1", "f2", "m1", "m2", "f3"]
    for arg in args:
        assert arg.name in expected_output
    assert len(args) == len(expected_output)


def test_args_filter2():
    '''the args_filter() method is in both Loop() and Arguments() classes
    with the former method calling the latter. This example tests the cases
    when one or both of the intent and type arguments are not specified.'''
    _, invoke_info = parse(os.path.join(BASE_PATH, "10_operator.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    loop = schedule.children[3]

    # arg_accesses
    args = loop.args_filter(arg_accesses=["gh_read"])
    expected_output = ["chi", "a"]
    for arg in args:
        assert arg.name in expected_output
    assert len(args) == len(expected_output)

    # arg_types
    args = loop.args_filter(arg_types=["gh_operator", "gh_integer"])
    expected_output = ["mm_w0", "a"]
    for arg in args:
        assert arg.name in expected_output
    assert len(args) == len(expected_output)

    # neither
    args = loop.args_filter()
    expected_output = ["chi", "mm_w0", "a"]
    for arg in args:
        assert arg.name in expected_output
    assert len(args) == len(expected_output)


def test_reduction_var_error():
    '''Check that we raise an exception if the zero_reduction_variable()
    method is provided with an incorrect type of argument'''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    for dist_mem in [False, True]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)
        schedule = psy.invokes.invoke_list[0].schedule
        call = schedule.calls()[0]
        # args[1] is of type gh_field
        call._reduction_arg = call.arguments.args[1]
        with pytest.raises(GenerationError) as err:
            call.zero_reduction_variable(None)
        assert ("zero_reduction variable should be one of ['gh_real', "
                "'gh_integer']") in str(err)


def test_reduction_sum_error():
    '''Check that we raise an exception if the reduction_sum_loop()
    method is provided with an incorrect type of argument'''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    for dist_mem in [False, True]:
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=dist_mem).create(invoke_info)
        schedule = psy.invokes.invoke_list[0].schedule
        call = schedule.calls()[0]
        # args[1] is of type gh_field
        call._reduction_arg = call.arguments.args[1]
        with pytest.raises(GenerationError) as err:
            call.reduction_sum_loop(None)
        assert (
            "unsupported reduction access 'gh_write' found in DynBuiltin:"
            "reduction_sum_loop(). Expected one of '['gh_sum']") in str(err)


def test_call_multi_reduction_error(monkeypatch):
    '''Check that we raise an exception if we try to create a Call (a
    Kernel or a Builtin) with more than one reduction in it. Since we have
    a rule that only Builtins can write to scalars we need a built-in that
    attempts to perform two reductions. '''
    from psyclone import dynamo0p3_builtins
    monkeypatch.setattr(dynamo0p3_builtins, "BUILTIN_DEFINITIONS_FILE",
                        value=os.path.join(BASE_PATH,
                                           "multi_reduction_builtins_mod.f90"))
    for dist_mem in [False, True]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH, "16.4.1_multiple_scalar_sums2.f90"),
            api="dynamo0.3", distributed_memory=dist_mem)
        with pytest.raises(GenerationError) as err:
            _ = PSyFactory("dynamo0.3",
                           distributed_memory=dist_mem).create(invoke_info)
        assert (
            "PSyclone currently only supports a single reduction in a kernel "
            "or builtin" in str(err))


def test_invoke_name():
    ''' Check that specifying the name of an invoke in the Algorithm
    layer results in a correctly-named routine in the PSy layer '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.0.1_single_named_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    gen = str(psy.gen)
    print gen
    assert "SUBROUTINE invoke_important_invoke" in gen


def test_multi_kern_named_invoke():
    ''' Check that specifying the name of an invoke containing multiple
    kernel invocations result in a correctly-named routine in the PSy layer '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.9_named_multikernel_invokes.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    gen = str(psy.gen)
    print gen
    assert "SUBROUTINE invoke_some_name" in gen


def test_named_multi_invokes():
    ''' Check that we generate correct code when we have more than one
    named invoke in an Algorithm file '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "3.2_multi_functions_multi_named_invokes.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    gen = str(psy.gen)
    print gen
    assert "SUBROUTINE invoke_my_first(" in gen
    assert "SUBROUTINE invoke_my_second(" in gen


def test_named_invoke_name_clash():
    ''' Check that we do not get a name clash when the name of a variable
    in the PSy layer would normally conflict with the name given to the
    subroutine generated by an Invoke. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.11_named_invoke_name_clash.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    gen = str(psy.gen)
    print gen
    assert "SUBROUTINE invoke_a(invoke_a_1, b, c, istp, rdt," in gen
    assert "TYPE(field_type), intent(inout) :: invoke_a_1" in gen


def test_invalid_reprod_pad_size():
    '''Check that we raise an exception if the pad size in config.py is
    set to an invalid value '''
    from psyclone import config
    keep = config.REPROD_PAD_SIZE
    config.REPROD_PAD_SIZE = 0
    for distmem in [True, False]:
        _, invoke_info = parse(
            os.path.join(BASE_PATH,
                         "15.9.0_inner_prod_builtin.f90"),
            distributed_memory=distmem,
            api="dynamo0.3")
        psy = PSyFactory("dynamo0.3",
                         distributed_memory=distmem).create(invoke_info)
        invoke = psy.invokes.invoke_list[0]
        schedule = invoke.schedule
        from psyclone.transformations import Dynamo0p3OMPLoopTrans, \
            OMPParallelTrans
        otrans = Dynamo0p3OMPLoopTrans()
        rtrans = OMPParallelTrans()
        # Apply an OpenMP do directive to the loop
        schedule, _ = otrans.apply(schedule.children[0], reprod=True)
        # Apply an OpenMP Parallel directive around the OpenMP do directive
        schedule, _ = rtrans.apply(schedule.children[0])
        invoke.schedule = schedule
        with pytest.raises(GenerationError) as excinfo:
            _ = str(psy.gen)
        assert (
            "REPROD_PAD_SIZE in config.py should be a positive "
            "integer") in str(excinfo.value)
    config.REPROD_PAD_SIZE = keep


def test_argument_depends_on():
    '''Check that the depends_on method returns the appropriate boolean
    value for arguments with combinations of read and write access'''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.5_multikernel_invokes.f90"),
                           distributed_memory=False, api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    arg_f1_inc_1 = schedule.children[0].children[0].arguments.args[0]
    arg_f1_inc_2 = schedule.children[2].children[0].arguments.args[0]
    arg_f2_read_1 = schedule.children[0].children[0].arguments.args[2]
    arg_f2_inc = schedule.children[1].children[0].arguments.args[0]
    arg_f2_read_2 = schedule.children[2].children[0].arguments.args[1]
    # different names returns False
    assert not arg_f2_inc._depends_on(arg_f1_inc_1)
    # same name both reads returns False
    assert not arg_f2_read_1._depends_on(arg_f2_read_2)
    # same name both incs (write to read) returns True
    assert arg_f1_inc_2._depends_on(arg_f1_inc_1)
    # read to write returns True
    assert arg_f2_read_1._depends_on(arg_f2_inc)
    # write to read returns True
    assert arg_f2_inc._depends_on(arg_f2_read_1)
    # same name both writes (the 4.5 example only uses inc) returns True
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.1_builtin_and_normal_kernel_invoke.f90"),
        distributed_memory=False, api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    arg_f1_write_1 = schedule.children[0].children[0].arguments.args[1]
    arg_f1_write_2 = schedule.children[1].children[0].arguments.args[1]
    assert arg_f1_write_1._depends_on(arg_f1_write_2)


def test_argument_find_argument():
    '''Check that the find_argument method returns the first dependent
    argument in a list of nodes, or None if none are found'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.3.4_multi_axpy_invoke.f90"),
        distributed_memory=True, api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # 1: returns none if none found
    f1_first_read = schedule.children[0].children[0].arguments.args[1]
    # a) empty node list
    assert not f1_first_read._find_argument([])
    # b) check many reads
    call_nodes = schedule.calls()
    assert not f1_first_read._find_argument(call_nodes)
    # 2: returns first dependent kernel arg when there are many
    # dependencies (check first read returned)
    f3_write = schedule.children[3].children[0].arguments.args[3]
    f3_first_read = schedule.children[0].children[0].arguments.args[2]
    result = f3_write._find_argument(call_nodes)
    assert result == f3_first_read
    # 3: haloexchange node
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.1_builtin_and_normal_kernel_invoke.f90"),
        distributed_memory=True, api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # a) kern arg depends on halo arg
    m2_read_arg = schedule.children[3].children[0].arguments.args[4]
    m2_halo_field = schedule.children[2].field
    result = m2_read_arg._find_argument(schedule.children)
    assert result == m2_halo_field
    # b) halo arg depends on kern arg
    result = m2_halo_field._find_argument([schedule.children[3].children[0]])
    assert result == m2_read_arg
    # 4: globalsum node
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.10.1_sum_field_builtin.f90"),
        distributed_memory=True, api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # a) globalsum arg depends on kern arg
    kern_asum_arg = schedule.children[3].children[0].arguments.args[0]
    glob_sum_arg = schedule.children[2].scalar
    result = kern_asum_arg._find_argument(schedule.children)
    assert result == glob_sum_arg
    # b) kern arg depends on globalsum arg
    result = glob_sum_arg._find_argument([schedule.children[3].children[0]])
    assert result == kern_asum_arg


@pytest.mark.xfail(reason="gh_readwrite not yet supported in PSyclone")
def test_globalsum_arg():
    '''Check that the globalsum argument is defined as gh_readwrite and
    points to the globalsum node'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.10.1_sum_field_builtin.f90"),
        distributed_memory=True, api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    glob_sum = schedule.children[2]
    glob_sum_arg = glob_sum.scalar
    assert glob_sum_arg.access == "gh_readwrite"
    assert glob_sum_arg.call == glob_sum


@pytest.mark.xfail(reason="gh_readwrite not yet supported in PSyclone")
def test_haloexchange_arg():
    '''Check that the haloexchange argument is defined as gh_readwrite and
    points to the haloexchange node'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.1_builtin_and_normal_kernel_invoke.f90"),
        distributed_memory=True, api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    halo_exchange = schedule.children[2]
    halo_exchange_arg = halo_exchange.field
    assert halo_exchange_arg.access == "gh_readwrite"
    assert halo_exchange_arg.call == halo_exchange


def test_argument_forward_dependence():  # pylint: disable=invalid-name
    '''Check that forward_dependence method returns the first dependent
    argument after the current Node in the schedule or None if none
    are found.'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.3.4_multi_axpy_invoke.f90"),
        distributed_memory=True, api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    f1_first_read = schedule.children[0].children[0].arguments.args[1]
    # 1: internal var computed set to False
    assert not f1_first_read._fd_computed
    # 2: initial internal value set to None
    assert not f1_first_read._fd_value
    # 3: returns none if none found (check many reads)
    assert not f1_first_read.forward_dependence()
    # 4: computed set to True once run
    assert f1_first_read._fd_computed
    # 5: returns first dependent kernel arg when there are many
    # dependencies (check first read returned)
    f3_write = schedule.children[3].children[0].arguments.args[3]
    f3_next_read = schedule.children[4].children[0].arguments.args[2]
    result = f3_write.forward_dependence()
    assert result == f3_next_read
    # 6: haloexchange dependencies
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "4.5_multikernel_invokes.f90"),
        distributed_memory=True, api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    f2_prev_arg = schedule.children[14].children[0].arguments.args[1]
    f2_halo_field = schedule.children[15].field
    f2_next_arg = schedule.children[17].children[0].arguments.args[0]
    # a) previous kern arg depends on halo arg
    result = f2_prev_arg.forward_dependence()
    assert result == f2_halo_field
    # b) halo arg depends on following kern arg
    result = f2_halo_field.forward_dependence()
    assert result == f2_next_arg
    # 7: globalsum dependencies
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.10.1_sum_field_builtin.f90"),
        distributed_memory=True, api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    prev_arg = schedule.children[0].children[0].arguments.args[0]
    sum_arg = schedule.children[1].children[0].arguments.args[1]
    global_sum_arg = schedule.children[2].scalar
    next_arg = schedule.children[3].children[0].arguments.args[0]
    # a) prev kern arg depends on sum
    result = prev_arg.forward_dependence()
    assert result == sum_arg
    # b) sum arg depends on global sum arg
    result = sum_arg.forward_dependence()
    assert result == global_sum_arg
    # c) global sum arg depends on next kern arg
    result = global_sum_arg.forward_dependence()
    assert result == next_arg


def test_argument_backward_dependence():  # pylint: disable=invalid-name
    '''Check that backward_dependence method returns the first dependent
    argument before the current Node in the schedule or None if none
    are found.'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.3.4_multi_axpy_invoke.f90"),
        distributed_memory=True, api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    f1_last_read = schedule.children[6].children[0].arguments.args[1]
    # 1: internal var computed set to False
    assert not f1_last_read._bd_computed
    # 2: initial internal value set to None
    assert not f1_last_read._bd_value
    # 3: returns none if none found (check many reads)
    assert not f1_last_read.backward_dependence()
    # 4: computed set to True once run
    assert f1_last_read._bd_computed
    # 5: returns first dependent kernel arg when there are many
    # dependencies (check first read returned)
    f3_write = schedule.children[3].children[0].arguments.args[3]
    f3_prev_read = schedule.children[2].children[0].arguments.args[2]
    result = f3_write.backward_dependence()
    assert result == f3_prev_read
    # 6: haloexchange dependencies
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "4.5_multikernel_invokes.f90"),
        distributed_memory=True, api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    f2_prev_arg = schedule.children[14].children[0].arguments.args[1]
    f2_halo_field = schedule.children[15].field
    f2_next_arg = schedule.children[17].children[0].arguments.args[0]
    # a) following kern arg depends on halo arg
    result = f2_next_arg.backward_dependence()
    assert result == f2_halo_field
    # b) halo arg depends on previous kern arg
    result = f2_halo_field.backward_dependence()
    assert result == f2_prev_arg
    # 7: globalsum dependencies
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.10.1_sum_field_builtin.f90"),
        distributed_memory=True, api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    prev_arg = schedule.children[0].children[0].arguments.args[0]
    sum_arg = schedule.children[1].children[0].arguments.args[1]
    global_sum_arg = schedule.children[2].scalar
    next_arg = schedule.children[3].children[0].arguments.args[0]
    # a) next kern arg depends on global sum arg
    result = next_arg.backward_dependence()
    assert result == global_sum_arg
    # b) global sum arg depends on sum arg
    result = global_sum_arg.backward_dependence()
    assert result == sum_arg
    # c) sum depends on prev kern arg
    result = sum_arg.backward_dependence()
    assert result == prev_arg


def test_node_depth():
    '''Test that the Node class depth method returns the correct value
    for a Node in a tree '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "1_single_invoke.f90"),
        distributed_memory=True, api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    assert schedule.depth == 1
    for child in schedule.children:
        assert child.depth == 2
    for child in schedule.children[3].children:
        assert child.depth == 3


def test_node_args():
    '''Test that the Node class args method returns the correct arguments
    for Nodes that do not have arguments themselves'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "4_multikernel_invokes.f90"),
        distributed_memory=False, api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    loop1 = schedule.children[0]
    kern1 = loop1.children[0]
    loop2 = schedule.children[1]
    kern2 = loop2.children[0]
    # 1) Schedule (not that this is useful)
    all_args = kern1.arguments.args
    all_args.extend(kern2.arguments.args)
    schedule_args = schedule.args
    for idx, arg in enumerate(all_args):
        assert arg == schedule_args[idx]
    # 2) Loop1
    loop1_args = loop1.args
    for idx, arg in enumerate(kern1.arguments.args):
        assert arg == loop1_args[idx]
    # 3) Loop2
    loop2_args = loop2.args
    for idx, arg in enumerate(kern2.arguments.args):
        assert arg == loop2_args[idx]
    # 4) Loopfuse
    ftrans = DynamoLoopFuseTrans()
    schedule, _ = ftrans.apply(schedule.children[0], schedule.children[1],
                               same_space=True)
    loop = schedule.children[0]
    kern1 = loop.children[0]
    kern2 = loop.children[1]
    loop_args = loop.args
    kern_args = kern1.arguments.args
    kern_args.extend(kern2.arguments.args)
    for idx, arg in enumerate(kern_args):
        assert arg == loop_args[idx]


def test_call_args():
    '''Test that the call class args method returns the appropriate
    arguments '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.1_builtin_and_normal_kernel_invoke.f90"),
        distributed_memory=False, api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    kern = schedule.children[0].children[0]
    builtin = schedule.children[1].children[0]
    # 1) kern
    for idx, arg in enumerate(kern.args):
        assert arg == kern.arguments.args[idx]
    # 2) builtin
    for idx, arg in enumerate(builtin.args):
        assert arg == builtin.arguments.args[idx]


def test_haloexchange_args():
    '''Test that the haloexchange class args method returns the appropriate
    argument '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "1_single_invoke.f90"),
        distributed_memory=True, api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    for haloexchange in schedule.children[:2]:
        assert len(haloexchange.args) == 1
        assert haloexchange.args[0] == haloexchange.field


def test_globalsum_args():
    '''Test that the globalsum class args method returns the appropriate
    argument '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.10.1_sum_field_builtin.f90"),
        distributed_memory=True, api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    global_sum = schedule.children[2]
    assert len(global_sum.args) == 1
    assert global_sum.args[0] == global_sum.scalar


def test_node_forward_dependence():
    '''Test that the Node class forward_dependence method returns the
    closest dependent Node after the current Node in the schedule or
    None if none are found.'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.3.4_multi_axpy_invoke.f90"),
        distributed_memory=True, api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    read4 = schedule.children[4]
    # 1: returns none if none found
    # a) check many reads
    assert not read4.forward_dependence()
    # b) check no dependencies for a call
    assert not read4.children[0].forward_dependence()
    # 2: returns first dependent kernel arg when there are many
    # dependencies
    # a) check first read returned
    writer = schedule.children[3]
    next_read = schedule.children[4]
    assert writer.forward_dependence() == next_read
    # a) check writer returned
    first_loop = schedule.children[0]
    assert first_loop.forward_dependence() == writer
    # 3: haloexchange dependencies
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "4.5_multikernel_invokes.f90"),
        distributed_memory=True, api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    prev_loop = schedule.children[14]
    halo_field = schedule.children[15]
    next_loop = schedule.children[17]
    # a) previous loop depends on halo exchange
    assert prev_loop.forward_dependence() == halo_field
    # b) halo exchange depends on following loop
    assert halo_field.forward_dependence() == next_loop

    # 4: globalsum dependencies
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.10.1_sum_field_builtin.f90"),
        distributed_memory=True, api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    prev_loop = schedule.children[0]
    sum_loop = schedule.children[1]
    global_sum_loop = schedule.children[2]
    next_loop = schedule.children[3]
    # a) prev loop depends on sum loop
    assert prev_loop.forward_dependence() == sum_loop
    # b) sum loop depends on global sum loop
    assert sum_loop.forward_dependence() == global_sum_loop
    # c) global sum loop depends on next loop
    assert global_sum_loop.forward_dependence() == next_loop


def test_node_backward_dependence():
    '''Test that the Node class backward_dependence method returns the
    closest dependent Node before the current Node in the schedule or
    None if none are found.'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.3.4_multi_axpy_invoke.f90"),
        distributed_memory=True, api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # 1: loop no backwards dependence
    loop3 = schedule.children[2]
    assert not loop3.backward_dependence()
    # 2: loop to loop backward dependence
    # a) many steps
    last_loop_node = schedule.children[6]
    prev_dep_loop_node = schedule.children[3]
    assert last_loop_node.backward_dependence() == prev_dep_loop_node
    # b) previous
    assert prev_dep_loop_node.backward_dependence() == loop3
    # 3: haloexchange dependencies
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "4.5_multikernel_invokes.f90"),
        distributed_memory=True, api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    loop13 = schedule.children[14]
    halo_exchange = schedule.children[16]
    loop16 = schedule.children[17]
    # a) following loop node depends on halo exchange node
    result = loop16.backward_dependence()
    assert result == halo_exchange
    # b) halo exchange node depends on previous loop node
    result = halo_exchange.backward_dependence()
    assert result == loop13
    # 4: globalsum dependencies
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.10.1_sum_field_builtin.f90"),
        distributed_memory=True, api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    loop1 = schedule.children[0]
    loop2 = schedule.children[1]
    global_sum = schedule.children[2]
    loop3 = schedule.children[3]
    # a) loop3 depends on global sum
    assert loop3.backward_dependence() == global_sum
    # b) global sum depends on loop2
    assert global_sum.backward_dependence() == loop2
    # c) loop2 (sum) depends on loop1
    assert loop2.backward_dependence() == loop1


def test_call_forward_dependence():
    '''Test that the Call class forward_dependence method returns the
    closest dependent call after the current call in the schedule or
    None if none are found. This is achieved by loop fusing first.'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.3.4_multi_axpy_invoke.f90"),
        distributed_memory=False, api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    ftrans = DynamoLoopFuseTrans()
    for _ in range(6):
        schedule, _ = ftrans.apply(schedule.children[0], schedule.children[1],
                                   same_space=True)
    read4 = schedule.children[0].children[4]
    # 1: returns none if none found
    # a) check many reads
    assert not read4.forward_dependence()
    # 2: returns first dependent kernel arg when there are many
    # dependencies
    # a) check first read returned
    writer = schedule.children[0].children[3]
    next_read = schedule.children[0].children[4]
    assert writer.forward_dependence() == next_read
    # a) check writer returned
    first_loop = schedule.children[0].children[0]
    assert first_loop.forward_dependence() == writer


def test_call_backward_dependence():
    '''Test that the Call class backward_dependence method returns the
    closest dependent call before the current call in the schedule or
    None if none are found. This is achieved by loop fusing first.'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.3.4_multi_axpy_invoke.f90"),
        distributed_memory=False, api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    ftrans = DynamoLoopFuseTrans()
    for _ in range(6):
        schedule, _ = ftrans.apply(schedule.children[0], schedule.children[1],
                                   same_space=True)
    # 1: loop no backwards dependence
    call3 = schedule.children[0].children[2]
    assert not call3.backward_dependence()
    # 2: call to call backward dependence
    # a) many steps
    last_call_node = schedule.children[0].children[6]
    prev_dep_call_node = schedule.children[0].children[3]
    assert last_call_node.backward_dependence() == prev_dep_call_node
    # b) previous
    assert prev_dep_call_node.backward_dependence() == call3


def test_omp_forward_dependence():
    '''Test that the forward_dependence method works for Directives,
    returning the closest dependent Node after the current Node in the
    schedule or None if none are found. '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.3.4_multi_axpy_invoke.f90"),
        distributed_memory=True, api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    from psyclone.transformations import DynamoOMPParallelLoopTrans
    otrans = DynamoOMPParallelLoopTrans()
    for child in schedule.children:
        schedule, _ = otrans.apply(child)
    read4 = schedule.children[4]
    # 1: returns none if none found
    # a) check many reads
    assert not read4.forward_dependence()
    # b) check no dependencies for the loop
    assert not read4.children[0].forward_dependence()
    # 2: returns first dependent kernel arg when there are many
    # dependencies
    # a) check first read returned
    writer = schedule.children[3]
    next_read = schedule.children[4]
    assert writer.forward_dependence() == next_read
    # b) check writer returned
    first_omp = schedule.children[0]
    assert first_omp.forward_dependence() == writer
    # 3: directive and globalsum dependencies
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.10.1_sum_field_builtin.f90"),
        distributed_memory=True, api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    schedule, _ = otrans.apply(schedule.children[0])
    schedule, _ = otrans.apply(schedule.children[1])
    schedule, _ = otrans.apply(schedule.children[3])
    prev_omp = schedule.children[0]
    sum_omp = schedule.children[1]
    global_sum_loop = schedule.children[2]
    next_omp = schedule.children[3]
    # a) prev omp depends on sum omp
    assert prev_omp.forward_dependence() == sum_omp
    # b) sum omp depends on global sum loop
    assert sum_omp.forward_dependence() == global_sum_loop
    # c) global sum loop depends on next omp
    assert global_sum_loop.forward_dependence() == next_omp


def test_directive_backward_dependence():  # pylint: disable=invalid-name
    '''Test that the backward_dependence method works for Directives,
    returning the closest dependent Node before the current Node in
    the schedule or None if none are found.'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.3.4_multi_axpy_invoke.f90"),
        distributed_memory=True, api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    from psyclone.transformations import DynamoOMPParallelLoopTrans
    otrans = DynamoOMPParallelLoopTrans()
    for child in schedule.children:
        schedule, _ = otrans.apply(child)
    # 1: omp directive no backwards dependence
    omp3 = schedule.children[2]
    assert not omp3.backward_dependence()
    # 2: omp to omp backward dependence
    # a) many steps
    last_omp_node = schedule.children[6]
    prev_dep_omp_node = schedule.children[3]
    assert last_omp_node.backward_dependence() == prev_dep_omp_node
    # b) previous
    assert prev_dep_omp_node.backward_dependence() == omp3
    # 3: globalsum dependencies
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.10.1_sum_field_builtin.f90"),
        distributed_memory=True, api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    schedule, _ = otrans.apply(schedule.children[0])
    schedule, _ = otrans.apply(schedule.children[1])
    schedule, _ = otrans.apply(schedule.children[3])
    omp1 = schedule.children[0]
    omp2 = schedule.children[1]
    global_sum = schedule.children[2]
    omp3 = schedule.children[3]
    # a) omp3 depends on global sum
    assert omp3.backward_dependence() == global_sum
    # b) global sum depends on omp2
    assert global_sum.backward_dependence() == omp2
    # c) omp2 (sum) depends on omp1
    assert omp2.backward_dependence() == omp1


def test_node_is_valid_location():
    '''Test that the Node class is_valid_location method returns True if
    the new location does not break any data dependencies, otherwise it
    returns False'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "1_single_invoke.f90"),
        distributed_memory=True, api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # 1: new node argument is invalid
    node = schedule.children[0]
    with pytest.raises(GenerationError) as excinfo:
        node.is_valid_location("invalid_node_argument")
    assert "argument is not a Node, it is a 'str'." in str(excinfo.value)
    # 2: optional position argument is invalid
    with pytest.raises(GenerationError) as excinfo:
        node.is_valid_location(node, position="invalid_node_argument")
    assert "The position argument in the psyGen" in str(excinfo.value)
    assert "method must be one of" in str(excinfo.value)
    # 3: parents of node and new_node are not the same
    with pytest.raises(GenerationError) as excinfo:
        node.is_valid_location(schedule.children[3].children[0])
    assert ("the node and the location do not have the same "
            "parent") in str(excinfo.value)
    # 4: positions are the same
    prev_node = schedule.children[0]
    node = schedule.children[1]
    next_node = schedule.children[2]
    # a) before this node
    with pytest.raises(GenerationError) as excinfo:
        node.is_valid_location(node, position="before")
    assert "the node and the location are the same" in str(excinfo.value)
    # b) after this node
    with pytest.raises(GenerationError) as excinfo:
        node.is_valid_location(node, position="after")
    assert "the node and the location are the same" in str(excinfo.value)
    # c) after previous node
    with pytest.raises(GenerationError) as excinfo:
        node.is_valid_location(prev_node, position="after")
    assert "the node and the location are the same" in str(excinfo.value)
    # d) before next node
    with pytest.raises(GenerationError) as excinfo:
        node.is_valid_location(next_node, position="before")
    assert "the node and the location are the same" in str(excinfo.value)
    # 5: valid no previous dependency
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.3.4_multi_axpy_invoke.f90"),
        distributed_memory=True, api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # 6: valid no prev dep
    node = schedule.children[2]
    assert node.is_valid_location(schedule.children[0])
    # 7: valid prev dep (after)
    node = schedule.children[6]
    assert node.is_valid_location(schedule.children[3], position="after")
    # 8: invalid prev dep (before)
    assert not node.is_valid_location(schedule.children[3], position="before")
    # 9: valid no following dep
    node = schedule.children[4]
    assert node.is_valid_location(schedule.children[6], position="after")
    # 10: valid following dep (before)
    node = schedule.children[0]
    assert node.is_valid_location(schedule.children[3], position="before")
    # 11: invalid following dep (after)
    node = schedule.children[0]
    assert not node.is_valid_location(schedule.children[3], position="after")


def test_dag_names():
    '''test that the dag_name method returns the correct value for the
    node class and its specialisations'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "1_single_invoke.f90"),
        distributed_memory=True, api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    from psyclone.psyGen import Schedule
    assert super(Schedule, schedule).dag_name == "node_0"
    assert schedule.dag_name == "schedule"
    assert schedule.children[0].dag_name == "checkhaloexchange(f2)_0"
    assert schedule.children[3].dag_name == "loop_3"
    schedule.children[3].loop_type = "colour"
    assert schedule.children[3].dag_name == "loop_[colour]_3"
    schedule.children[3].loop_type = ""
    assert (schedule.children[3].children[0].dag_name ==
            "kernel_testkern_code_5")
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.10.1_sum_field_builtin.f90"),
        distributed_memory=True, api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    global_sum = schedule.children[2]
    assert global_sum.dag_name == "globalsum(asum)_2"
    builtin = schedule.children[1].children[0]
    assert builtin.dag_name == "builtin_sum_field_4"


def test_openmp_pdo_dag_name():
    '''Test that we generate the correct dag name for the OpenMP parallel
    do node'''
    _, info = parse(os.path.join(BASE_PATH,
                                 "15.2.0_copy_field_builtin.f90"),
                    api="dynamo0.3", distributed_memory=False)
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    from psyclone.transformations import DynamoOMPParallelLoopTrans
    otrans = DynamoOMPParallelLoopTrans()
    # Apply OpenMP parallelisation to the loop
    schedule, _ = otrans.apply(schedule.children[0])
    assert schedule.children[0].dag_name == "OMP_parallel_do_1"


def test_omp_dag_names():
    '''Test that we generate the correct dag names for omp parallel, omp
    do, omp directive and directive nodes'''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "1_single_invoke.f90"),
                    api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(info)
    invoke = psy.invokes.get('invoke_0_testkern_type')
    schedule = invoke.schedule
    from psyclone.transformations import Dynamo0p3OMPLoopTrans, \
        OMPParallelTrans
    olooptrans = Dynamo0p3OMPLoopTrans()
    ptrans = OMPParallelTrans()
    # Put an OMP PARALLEL around this loop
    child = schedule.children[0]
    oschedule, _ = ptrans.apply(child)
    # Put an OMP DO around this loop
    schedule, _ = olooptrans.apply(oschedule.children[0].children[0])
    # Replace the original loop schedule with the transformed one
    omp_par_node = schedule.children[0]
    assert omp_par_node.dag_name == "OMP_parallel_1"
    assert omp_par_node.children[0].dag_name == "OMP_do_2"
    omp_directive = super(OMPParallelDirective, omp_par_node)
    assert omp_directive.dag_name == "OMP_directive_1"
    print type(omp_directive)
    directive = super(OMPDirective, omp_par_node)
    assert directive.dag_name == "directive_1"

EXPECTED = (
    "digraph {\n"
    "	schedule_start\n"
    "	schedule_end\n"
    "	loop_0_start\n"
    "	loop_0_end\n"
    "		loop_0_end -> schedule_end [color=blue]\n"
    "		schedule_start -> loop_0_start [color=blue]\n"
    "	kernel_testkern_code_2\n"
    "		kernel_testkern_code_2 -> loop_0_end [color=blue]\n"
    "		loop_0_start -> kernel_testkern_code_2 [color=blue]\n"
    "}")


def test_node_dag_no_graphviz(tmpdir):
    '''test that dag generation does nothing if graphviz is not
    installed. If graphviz is installed on this system we need to make
    the test think that it is not. We do this by messing with
    sys.modules. '''
    import sys
    keep = None
    try:
        import graphviz
        keep = sys.modules['graphviz']
        sys.modules['graphviz'] = None
    except ImportError:
        pass
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "1_single_invoke.f90"),
        distributed_memory=False, api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    my_file = tmpdir.join('test')
    schedule.dag(file_name=my_file.strpath)
    assert not os.path.exists(my_file.strpath)
    if keep:
        sys.modules['graphviz'] = keep

EXPECTED2 = (
    "digraph {\n"
    "	schedule_start\n"
    "	schedule_end\n"
    "	loop_0_start\n"
    "	loop_0_end\n"
    "		loop_0_end -> loop_1_start [color=green]\n"
    "		schedule_start -> loop_0_start [color=blue]\n"
    "	kernel_testkern_qr_code_2\n"
    "		kernel_testkern_qr_code_2 -> loop_0_end [color=blue]\n"
    "		loop_0_start -> kernel_testkern_qr_code_2 [color=blue]\n"
    "	loop_1_start\n"
    "	loop_1_end\n"
    "		loop_1_end -> schedule_end [color=blue]\n"
    "		loop_0_end -> loop_1_start [color=red]\n"
    "	kernel_testkern_qr_code_4\n"
    "		kernel_testkern_qr_code_4 -> loop_1_end [color=blue]\n"
    "		loop_1_start -> kernel_testkern_qr_code_4 [color=blue]\n"
    "}")


def test_node_dag(tmpdir):
    '''test that dag generation works correctly. Skip the test if
    graphviz is not installed'''
    try:
        import graphviz
    except ImportError:
        return
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "4.1_multikernel_invokes.f90"),
        distributed_memory=False, api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    my_file = tmpdir.join('test')
    schedule.dag(file_name=my_file.strpath)
    result = my_file.read()
    assert EXPECTED2 in result
    my_file = tmpdir.join('test.svg')
    result = my_file.read()
    for name in ["<title>schedule_start</title>",
                 "<title>schedule_end</title>",
                 "<title>loop_0_start</title>",
                 "<title>loop_0_end</title>",
                 "<title>kernel_testkern_qr_code_2</title>",
                 "<title>kernel_testkern_qr_code_4</title>",
                 "<svg", "</svg>", "blue", "green", "red"]:
        assert name in result
    with pytest.raises(GenerationError) as excinfo:
        schedule.dag(file_name=my_file.strpath, file_format="rubbish")
    assert "unsupported graphviz file format" in str(excinfo.value)
