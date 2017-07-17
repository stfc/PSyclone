# -----------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2015.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
# -----------------------------------------------------------------------------
# Author R. Ford and A. R. Porter, STFC Daresbury Lab

''' A module to perform pytest unit and functional tests on the parse
function. '''

import os
import pytest
from psyclone.parse import parse, ParseError


def test_continuators_kernel():
    '''Tests that an input kernel file with long lines that already has
       continuators to make the code conform to the line length limit
       does not cause an error. '''
    _, _ = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                              "test_files", "dynamo0p3",
                              "1.1_single_invoke_qr.f90"),
                 api="dynamo0.3", line_length=True)


def test_continuators_algorithm():
    '''Tests that an input algorithm file with long lines that already has
       continuators to make the code conform to the line length limit
       does not cause an error. '''
    _, _ = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                              "test_files", "dynamo0p3",
                              "13.2_alg_long_line_continuator.f90"),
                 api="dynamo0.3", line_length=True)


def test_get_builtin_defs_wrong_api():
    ''' Check that we raise an appropriate error if we call
    get_builtin_defs() with an invalid API '''
    import psyclone.parse as pparse
    with pytest.raises(ParseError) as excinfo:
        _, _ = pparse.get_builtin_defs('invalid_api')
    assert "check_api: Unsupported API 'invalid_api'" in str(excinfo.value)


def test_kerneltypefactory_wrong_api():
    ''' Check that we raise an appropriate error if we try to create
    a KernelTypeFactory with an invalid API '''
    from psyclone.parse import KernelTypeFactory
    with pytest.raises(ParseError) as excinfo:
        _ = KernelTypeFactory(api="invalid_api")
    assert "check_api: Unsupported API 'invalid_api'" in str(excinfo.value)


def test_kerneltypefactory_default_api():
    ''' Check that the KernelTypeFactory correctly defaults to using
    the default API '''
    from psyclone.parse import KernelTypeFactory
    from psyclone.config import DEFAULTAPI
    factory = KernelTypeFactory(api="")
    assert factory._type == DEFAULTAPI


def test_kerneltypefactory_create_broken_type():
    ''' Check that we raise an error if the KernelTypeFactory.create()
    method encounters an unrecognised API. '''
    from psyclone.parse import KernelTypeFactory
    factory = KernelTypeFactory(api="")
    # Deliberately break the 'type' (API) of this factory
    factory._type = "invalid_api"
    with pytest.raises(ParseError) as excinfo:
        _ = factory.create(None, name="axpy")
    assert ("KernelTypeFactory: Internal Error: Unsupported kernel type"
            in str(excinfo.value))


def test_broken_builtin_metadata():
    ''' Check that we raise an appropriate error if there is a problem
    with the meta-data describing the built-ins for a given API '''
    from psyclone import dynamo0p3_builtins
    # The file containing broken meta-data for the built-ins
    defs_file = os.path.join(
        os.path.dirname(os.path.abspath(__file__)),
        "test_files", "dynamo0p3", "broken_builtins_mod.f90")
    from psyclone.parse import BuiltInKernelTypeFactory
    factory = BuiltInKernelTypeFactory(api="dynamo0.3")
    with pytest.raises(ParseError) as excinfo:
        _ = factory.create(dynamo0p3_builtins.BUILTIN_MAP,
                           defs_file, name="axpy")
    assert ("Failed to parse the meta-data for PSyclone built-ins in" in
            str(excinfo.value))


def test_unrecognised_builtin():
    ''' Check that we raise an error if we call the BuiltInKernelTypeFactory
    with an unrecognised built-in name '''
    from psyclone import dynamo0p3_builtins
    from psyclone.parse import BuiltInKernelTypeFactory
    factory = BuiltInKernelTypeFactory()
    with pytest.raises(ParseError) as excinfo:
        _ = factory.create(dynamo0p3_builtins.BUILTIN_MAP,
                           None,
                           name="not_a_builtin")
    assert ("unrecognised built-in name. Got 'not_a_builtin' but"
            in str(excinfo.value))


def test_builtin_with_use():
    ''' Check that we raise an error if we encounter a use statement for
    a built-in operation '''
    with pytest.raises(ParseError) as excinfo:
        _, _ = parse(
            os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3",
                         "15.0.3_builtin_with_use.f90"),
            api="dynamo0.3")
    assert ("A built-in cannot be named in a use statement but "
            "'set_field_scalar' is used from module 'fake_builtin_mod' in "
            in str(excinfo.value))


def test_element_unpack():
    ''' Check that the unpack method of the Element class behaves as
    expected when passed a string '''
    from psyclone.parse import Element
    ele = Element()
    output = ele.unpack("andy")
    assert str(output) == "andy"


def test_too_many_names_invoke():
    ''' Test that we raise the expected error when the invoke contains
    more than one name=xxx argument. '''
    with pytest.raises(ParseError) as err:
        _, _ = parse(
            os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3",
                         "1.0.2_many_named_invoke.f90"),
            api="dynamo0.3")
    print str(err)
    assert "An invoke must contain one or zero " in str(err)


def test_wrong_named_invoke():
    ''' Test that we raise the expected error when the invoke contains
    a named argument where the argument is not called 'name' '''
    with pytest.raises(ParseError) as err:
        _, _ = parse(
            os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3",
                         "1.0.3_wrong_named_arg_invoke.f90"),
            api="dynamo0.3")
    print str(err)
    assert (
        "The arguments to an invoke() must be either kernel calls or an "
        "(optional) name=" in str(err))


def test_wrong_type_named_invoke():
    ''' Test that we raise the expected error when the invoke contains
    a named argument but its value is not a string '''
    with pytest.raises(ParseError) as err:
        _, _ = parse(
            os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3",
                         "1.0.4_wrong_type_named_arg_invoke.f90"),
            api="dynamo0.3")
    assert ("The (optional) name of an invoke must be specified as a "
            "string" in str(err))


def test_duplicate_named_invoke():
    ''' Test that we raise the expected error when an algorithm file
    contains two invokes that are given the same name '''
    with pytest.raises(ParseError) as err:
        _, _ = parse(
            os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3",
                         "3.3_multi_functions_multi_invokes_name_clash.f90"),
            api="dynamo0.3")
    assert "Found multiple named invoke()'s with the same name" in str(err)
