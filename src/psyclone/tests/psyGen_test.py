# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2024, Science and Technology Facilities Council.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# * Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# -----------------------------------------------------------------------------
# Authors: R. W. Ford, A. R. Porter, S. Siso and N. Nobre, STFC Daresbury Lab
# Modified: I. Kavcic, L. Turner, O. Brunt and J. G. Wallwork, Met Office
# -----------------------------------------------------------------------------

''' Performs py.test tests on the psyGen module '''


# internal classes requiring tests
# PSy, Invokes, Invoke, Kern, Argumen1ts, Argument, KernelArgument

# user classes requiring tests
# PSyFactory, TransInfo, Transformation
import os
import pytest

from fparser import api as fpapi, logging
from fparser.two import Fortran2003

from psyclone.configuration import Config
from psyclone.core.access_type import AccessType
from psyclone.domain.common.psylayer import PSyLoop
from psyclone.domain.lfric import lfric_builtins, LFRicKern, LFRicKernMetadata
from psyclone.domain.lfric.transformations import LFRicLoopFuseTrans
from psyclone.dynamo0p3 import (DynInvokeSchedule, DynGlobalSum,
                                DynKernelArguments)
from psyclone.errors import FieldNotFoundError, GenerationError, InternalError
from psyclone.generator import generate
from psyclone.gocean1p0 import GOKern
from psyclone.parse.algorithm import parse, InvokeCall
from psyclone.psyGen import (TransInfo, Transformation, PSyFactory,
                             InlinedKern, object_index, HaloExchange, Invoke,
                             DataAccess, Kern, Arguments, CodedKern, Argument,
                             GlobalSum, InvokeSchedule, BuiltIn)
from psyclone.psyir.nodes import (Assignment, BinaryOperation, Container,
                                  Literal, Loop, Node, KernelSchedule, Call,
                                  colored, Schedule)
from psyclone.psyir.symbols import (DataSymbol, RoutineSymbol, REAL_TYPE,
                                    ImportInterface, ContainerSymbol, Symbol,
                                    INTEGER_TYPE, UnresolvedType, SymbolTable)
from psyclone.tests.lfric_build import LFRicBuild
from psyclone.tests.test_files import dummy_transformations
from psyclone.tests.test_files.dummy_transformations import LocalTransformation
from psyclone.tests.utilities import get_invoke
from psyclone.transformations import (Dynamo0p3RedundantComputationTrans,
                                      Dynamo0p3KernelConstTrans,
                                      Dynamo0p3OMPLoopTrans,
                                      Dynamo0p3ColourTrans, OMPParallelTrans)


BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3")
GOCEAN_BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                "test_files", "gocean1p0")


# Module fixtures

@pytest.fixture(scope="module", autouse=True)
def setup():
    '''Make sure that all tests here use dynamo0.3 as API.'''
    Config.get().api = "dynamo0.3"
    yield
    Config._instance = None


# Tests for utilities

def test_object_index():
    ''' Tests for the object_index() utility. '''
    two = "two"
    my_list = ["one", two, "three"]
    assert object_index(my_list, two) == 1
    with pytest.raises(InternalError) as err:
        _ = object_index(my_list, None)
    assert "Cannot search for None item in list" in str(err.value)
    with pytest.raises(ValueError) as err:
        _ = object_index(my_list, "four")
    assert "Item 'four' not found in list" in str(err.value)


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
    _config = Config.get()
    apis = _config.supported_apis[:]
    apis.insert(0, "")
    for api in apis:
        psy_factory = PSyFactory(api=api)
        assert isinstance(psy_factory, PSyFactory)


def test_psyfactory_valid_dm_flag():
    ''' Test that a PSyFactory instance raises an exception if the
    optional distributed_memory flag is set to an invalid value
    and does not if the value is valid. '''
    with pytest.raises(TypeError) as excinfo:
        _ = PSyFactory(distributed_memory="ellie")
    assert "distributed_memory flag" in str(excinfo.value)
    _ = PSyFactory(distributed_memory=True)
    _ = PSyFactory(distributed_memory=False)


def test_psyfactory_create_error():
    ''' Test that the create() method raises the expected error for an
    unsupported API. '''
    psy_factory = PSyFactory()
    # Break the internal type of the factory
    psy_factory._type = "wrong"
    with pytest.raises(InternalError) as err:
        psy_factory.create(None)
    assert ("Unsupported API type 'wrong' found. Expected one of [" in
            str(err.value))


# Transformation class unit tests

def test_base_class_not_callable():
    '''Make sure we can not instantiate abstract Transformation class
    directly.'''
    with pytest.raises(TypeError):
        _ = Transformation()  # pylint: disable=abstract-class-instantiated


def test_transformation_init_name():
    '''Test that the name() method behaves in the expected way.

    '''
    class TestTrans(Transformation):
        '''Utility transformation that subclasses Transformation to stop it
        being abstract in order to test the non-abstract
        transformation methods.

        '''
        def apply(self, _1):
            '''Dummy apply method to ensure this transformation is not
            abstract.'''

    trans = TestTrans()
    assert trans.name == "TestTrans"


# TransInfo class unit tests

def test_new_module():
    '''check that we can change the module where we look for
    transformations.  There should be no transformations
    available as the new module uses a different
    transformation base class'''
    trans = TransInfo(module=dummy_transformations)
    assert trans.num_trans == 0


def test_new_baseclass():
    '''check that we can change the transformations baseclass. There
    should be no transformations available as the default
    transformations module does not use the specified base
    class'''
    trans = TransInfo(base_class=LocalTransformation)
    assert trans.num_trans == 0


def test_new_module_and_baseclass():
    '''check that we can change the module where we look for
    transformations and the baseclass. There should be one
    transformation available as the module specifies one test
    transformation using the specified base class '''
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
    transform = trans.get_trans_name("ColourTrans")
    assert isinstance(transform, Transformation)


# Tests for class Invokes

def test_invokes_get():
    '''Test the get() method of the Invokes class.'''
    # Making an Invokes object is not easy so we do a full PSy generation.
    _, invoke = parse(
        os.path.join(BASE_PATH, "1.0.1_single_named_invoke.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke)
    # Check that the get isn't case sensitive and doesn't require the
    # leading "invoke_" text.
    inv = psy.invokes.get("important_INVOKE")
    # Stored name has "invoke_" prepended.
    assert inv._name == "invoke_important_invoke"
    # No matching name found.
    with pytest.raises(RuntimeError) as err:
        psy.invokes.get("missing")
    assert ("Cannot find an invoke named 'missing' or 'invoke_missing' in "
            "['invoke_important_invoke']" in str(err.value))


# Tests for class InvokeCall

def test_invokes_can_always_be_printed():
    '''Test that an Invoke instance can always be printed (i.e. is
    initialised fully)'''
    inv = Invoke(None, None, None, None)
    assert inv.__str__() == "invoke()"

    invoke_call = InvokeCall([], "TestName")
    inv = Invoke(invoke_call, 12, DynInvokeSchedule, None)
    # Name is converted to lower case if set in constructor of InvokeCall:
    assert inv.__str__() == "invoke_testname()"

    invoke_call._name = None
    inv = Invoke(invoke_call, 12, DynInvokeSchedule, None)
    assert inv.__str__() == "invoke_12()"

    # Last test case: one kernel call - to avoid constructing
    # the InvokeCall, parse an existing Fortran file"

    _, invoke = parse(
        os.path.join(BASE_PATH, "1.12_single_invoke_deref_name_clash.f90"),
        api="dynamo0.3")

    alg_invocation = invoke.calls[0]
    inv = Invoke(alg_invocation, 0, DynInvokeSchedule, None)
    assert inv.__str__() == \
        "invoke_0_testkern_type(a, f1_my_field, f1 % my_field, m1, m2)"


def test_invoke_container():
    ''' Test the setting of the container associated with an Invoke. '''
    _, invoke = parse(
        os.path.join(BASE_PATH, "1.12_single_invoke_deref_name_clash.f90"),
        api="dynamo0.3")
    alg_invocation = invoke.calls[0]
    # An isolated Invoke object has no associated Container
    inv = Invoke(alg_invocation, 0, DynInvokeSchedule, None)
    assert inv._schedule.parent is None
    # Creating an Invokes object requires a PSy object but the construction of
    # the latter also creates the former. Therefore, we just create a PSy
    # object and check that a Container with the correct parent/child
    # relationships is created.
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke)
    assert isinstance(psy.container, Container)
    assert psy.invokes.invoke_list[0].schedule.parent is psy.container
    assert psy.container.children == [psy.invokes.invoke_list[0].schedule]


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


def test_derived_type_deref_naming(tmpdir):
    ''' Test that we do not get a name clash for dummy arguments in the PSy
    layer when the name generation for the component of a derived type
    may lead to a name already taken by another argument.

    '''
    _, invoke = parse(
        os.path.join(BASE_PATH, "1.12_single_invoke_deref_name_clash.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke)
    generated_code = str(psy.gen)

    assert LFRicBuild(tmpdir).code_compiles(psy)

    output = (
        "    SUBROUTINE invoke_0_testkern_type"
        "(a, f1_my_field, f1_my_field_1, m1, m2)\n"
        "      USE testkern_mod, ONLY: testkern_code\n"
        "      USE mesh_mod, ONLY: mesh_type\n"
        "      REAL(KIND=r_def), intent(in) :: a\n"
        "      TYPE(field_type), intent(in) :: f1_my_field, f1_my_field_1, "
        "m1, m2\n")
    assert output in generated_code


FAKE_KERNEL_METADATA = '''
module dummy_mod
  use argument_mod
  type, extends(kernel_type) :: dummy_type
     type(arg_type), meta_args(3) =                             &
          (/ arg_type(gh_field, gh_real, gh_write,     w3),     &
             arg_type(gh_field, gh_real, gh_readwrite, wtheta), &
             arg_type(gh_field, gh_real, gh_inc,       w1)      &
           /)
     integer :: operates_on = cell_column
   contains
     procedure, nopass :: code => dummy_code
  end type dummy_type
contains
  subroutine dummy_code()
  end subroutine dummy_code
end module dummy_mod
'''


# InvokeSchedule class tests

def test_invokeschedule_node_str():
    ''' Check the node_str method of the InvokeSchedule class. We need an
    Invoke object for this which we get using the dynamo0.3 API. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.9.1_X_innerproduct_Y_builtin.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    # Create a plain InvokeSchedule
    sched = InvokeSchedule('name', None, None)
    # Manually supply it with an Invoke object created with the Dynamo API.
    sched._invoke = psy.invokes.invoke_list[0]
    output = sched.node_str()
    assert colored("InvokeSchedule", InvokeSchedule._colour) in output


def test_invokeschedule_can_be_printed():
    ''' Check the InvokeSchedule class can always be printed'''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.9.1_X_innerproduct_Y_builtin.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)

    # For this test use the generic class
    psy.invokes.invoke_list[0].schedule.__class__ = InvokeSchedule
    output = str(psy.invokes.invoke_list[0].schedule)

    assert "InvokeSchedule:\n" in output


def test_invokeschedule_gen_code_with_preexisting_globals():
    ''' Check the InvokeSchedule gen_code adds pre-existing SymbolTable global
    variables into the generated f2pygen code. Multiple globals imported from
    the same module will be part of a single USE statement.'''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.9.1_X_innerproduct_Y_builtin.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)

    # Add some globals into the SymbolTable before calling gen_code()
    schedule = psy.invokes.invoke_list[0].schedule
    my_mod = ContainerSymbol("my_mod")
    schedule.symbol_table.add(my_mod)
    global1 = DataSymbol('gvar1', REAL_TYPE, interface=ImportInterface(my_mod))
    global2 = DataSymbol('gvar2', REAL_TYPE, interface=ImportInterface(my_mod))
    schedule.symbol_table.add(global1)
    schedule.symbol_table.add(global2)

    assert "USE my_mod, ONLY: gvar1, gvar2" in str(psy.gen)


# Kern class test

def test_kern_get_kernel_schedule():
    ''' Tests the get_kernel_schedule method in the Kern class.
    '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    kern = schedule.children[0].loop_body[0]
    kern_schedule = kern.get_kernel_schedule()
    assert isinstance(kern_schedule, KernelSchedule)


def test_codedkern_node_str():
    '''Tests the node_str method in the CodedKern class. The simplest way
    to do this is via the dynamo0.3 subclass.

    '''
    ast = fpapi.parse(FAKE_KERNEL_METADATA, ignore_comments=False)
    metadata = LFRicKernMetadata(ast)
    my_kern = LFRicKern()
    my_kern.load_meta(metadata)
    out = my_kern.node_str()
    expected_output = (
        colored("CodedKern", LFRicKern._colour) +
        " dummy_code(field_1,field_2,field_3) [module_inline=False]")
    assert expected_output in out


def test_codedkern_module_inline_getter_and_setter():
    ''' Check that the module_inline setter changes the module inline
    attribute to all the same kernels in the invoke'''
    # Use LFRic example with a repeated CodedKern
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "4.6_multikernel_invokes.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    coded_kern_1 = schedule.children[0].loop_body[0]
    coded_kern_2 = schedule.children[1].loop_body[0]

    # By default they are not module-inlined
    assert not coded_kern_1.module_inline
    assert not coded_kern_2.module_inline
    assert "module_inline=False" in coded_kern_1.node_str()
    assert "module_inline=False" in coded_kern_2.node_str()

    # It can be turned on (and both kernels change)
    coded_kern_1.module_inline = True
    assert coded_kern_1.module_inline
    assert coded_kern_2.module_inline
    assert "module_inline=True" in coded_kern_1.node_str()
    assert "module_inline=True" in coded_kern_2.node_str()

    # It can not be turned off
    with pytest.raises(TypeError) as err:
        coded_kern_2.module_inline = False
    assert ("The module inline parameter only accepts the type boolean "
            "'True' since module-inlining is irreversible. But found: 'False'"
            in str(err.value))
    assert coded_kern_1.module_inline
    assert coded_kern_2.module_inline

    # And it doesn't accept other types
    with pytest.raises(TypeError) as err:
        coded_kern_2.module_inline = 3
    assert ("The module inline parameter only accepts the type boolean "
            "'True' since module-inlining is irreversible. But found: '3'"
            in str(err.value))


def test_codedkern_module_inline_gen_code(tmpdir):
    ''' Check that a CodedKern with module-inline gets copied into the
    local module appropriately when the PSy-layer is generated'''
    # Use LFRic example with a repeated CodedKern
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "4.6_multikernel_invokes.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    coded_kern = schedule.children[0].loop_body[0]
    gen = str(psy.gen)

    # Without module-inline the subroutine is used by a module import
    assert "USE ru_kernel_mod, ONLY: ru_code" in gen
    assert "SUBROUTINE ru_code(" not in gen

    # With module-inline the subroutine does not need to be imported
    coded_kern.module_inline = True

    # Fail if local routine symbol does not already exist
    with pytest.raises(GenerationError) as err:
        gen = str(psy.gen)
    assert ("Cannot generate this kernel call to 'ru_code' because it "
            "is marked as module-inline but no such subroutine exist in "
            "this module." in str(err.value))

    # Create the symbol and try again, it now must succeed
    schedule.ancestor(Container).symbol_table.new_symbol(
            "ru_code", symbol_type=RoutineSymbol)

    gen = str(psy.gen)
    assert "USE ru_kernel_mod, ONLY: ru_code" not in gen
    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_codedkern_module_inline_kernel_in_multiple_invokes(tmpdir):
    ''' Check that module-inline works as expected when the same kernel
    is provided in different invokes'''
    # Use LFRic example with the kernel 'testkern_qr_mod' repeated once in
    # the first invoke and 3 times in the second invoke.
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "3.1_multi_functions_multi_invokes.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)

    # By default the kernel is imported once per invoke
    gen = str(psy.gen)
    assert gen.count("USE testkern_qr_mod, ONLY: testkern_qr_code") == 2

    # Module inline kernel in invoke 1
    schedule = psy.invokes.invoke_list[0].schedule
    for coded_kern in schedule.walk(CodedKern):
        if coded_kern.name == "testkern_qr_code":
            coded_kern.module_inline = True
    # A top-level RoutineSymbol must now exist
    schedule.ancestor(Container).symbol_table.new_symbol(
            "testkern_qr_code", symbol_type=RoutineSymbol)
    gen = str(psy.gen)

    # After this, one invoke uses the inlined top-level subroutine
    # and the other imports it (shadowing the top-level symbol)
    assert gen.count("USE testkern_qr_mod, ONLY: testkern_qr_code") == 1
    assert LFRicBuild(tmpdir).code_compiles(psy)

    # Module inline kernel in invoke 2
    schedule = psy.invokes.invoke_list[1].schedule
    for coded_kern in schedule.walk(CodedKern):
        if coded_kern.name == "testkern_qr_code":
            coded_kern.module_inline = True
    gen = str(psy.gen)
    # After this, no imports are remaining and both use the same
    # top-level implementation
    assert gen.count("USE testkern_qr_mod, ONLY: testkern_qr_code") == 0
    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_codedkern_lower_to_language_level(monkeypatch):
    ''' Check that a generic CodedKern can be lowered to a subroutine call
    with the appropriate arguments'''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    kern = schedule.children[0].loop_body[0]

    # TODO 1010: LFRic still needs psy.gen to create symbols. But these must
    # eventually be created automatically before the gen() call, for now we
    # manually create the symbols that appear in the PSyIR tree.
    schedule.symbol_table.add(Symbol("f1_proxy"))
    schedule.symbol_table.add(Symbol("f2_proxy"))
    schedule.symbol_table.add(Symbol("m1_proxy"))
    schedule.symbol_table.add(Symbol("m2_proxy"))
    schedule.symbol_table.add(Symbol("ndf_w1"))
    schedule.symbol_table.add(Symbol("undf_w1"))
    schedule.symbol_table.add(Symbol("map_w1"))
    schedule.symbol_table.add(Symbol("ndf_w2"))
    schedule.symbol_table.add(Symbol("undf_w2"))
    schedule.symbol_table.add(Symbol("map_w2"))
    schedule.symbol_table.add(Symbol("ndf_w3"))
    schedule.symbol_table.add(Symbol("undf_w3"))
    schedule.symbol_table.add(Symbol("map_w3"))

    # TODO #1085 LFRic Arguments do not have a translation to PSyIR
    # yet, we monkeypatch a dummy expression for now:
    monkeypatch.setattr(DynKernelArguments, "psyir_expressions",
                        lambda x: [Literal("1", INTEGER_TYPE)])

    # In DSL-level it is a CodedKern with no children
    assert isinstance(kern, CodedKern)
    assert len(kern.children) == 0
    number_of_arguments = len(kern.arguments.psyir_expressions())

    lowered = kern.lower_to_language_level()

    # In language-level it is a Call with arguments as children
    call = schedule.children[0].loop_body[0]
    assert call is lowered
    assert not isinstance(call, CodedKern)
    assert isinstance(call, Call)
    assert call.routine.name == 'testkern_code'
    assert len(call.children) == number_of_arguments
    assert isinstance(call.children[0], Literal)

    # A RoutineSymbol and the ContainerSymbol from where it is imported are
    # in the symbol table
    rsymbol = call.scope.symbol_table.lookup('testkern_code')
    assert isinstance(rsymbol, RoutineSymbol)
    assert isinstance(rsymbol.interface, ImportInterface)
    csymbol = rsymbol.interface.container_symbol
    assert isinstance(csymbol, ContainerSymbol)
    assert csymbol.name == "testkern_mod"


def test_kern_coloured_text():
    '''Check that the coloured_name method of both CodedKern and BuiltIn
    return what we expect.

    '''
    # Use LFRic example with both a CodedKern and a BuiltIn
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.14.4_builtin_and_normal_kernel_invoke.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    ckern = schedule.children[0].loop_body[0]
    bkern = schedule.children[1].loop_body[0]
    ret_str = ckern.coloured_name(True)
    assert colored("CodedKern", ckern._colour) in ret_str
    ret_str = bkern.coloured_name(True)
    assert colored("BuiltIn", bkern._colour) in ret_str


def test_kern_children_validation():
    '''Test that children added to Kern are validated. A Kern node does not
    accept any children.

    '''
    # We use a subclass (CodedKern->LFRicKern) to test this functionality.
    ast = fpapi.parse(FAKE_KERNEL_METADATA, ignore_comments=False)
    metadata = LFRicKernMetadata(ast)
    kern = LFRicKern()
    kern.load_meta(metadata)

    with pytest.raises(GenerationError) as excinfo:
        kern.addchild(Literal("2", INTEGER_TYPE))
    assert ("Item 'Literal' can't be child 0 of 'CodedKern'. CodedKern "
            "is a LeafNode and doesn't accept children.") in str(excinfo.value)


def test_inlinedkern_children_validation():
    '''Test that children added to InlinedKern are validated. An InlinedKern
    must have one child that is a Schedule (which is created by its
    constructor).

    '''
    ikern = InlinedKern(None)

    with pytest.raises(GenerationError) as excinfo:
        ikern.addchild(Literal("2", INTEGER_TYPE))
    assert ("Item 'Literal' can't be child 1 of 'InlinedKern'. The valid "
            "format is: 'Schedule'.") in str(excinfo.value)


def test_inlinedkern_node_str():
    '''Test the node_str() method of InlinedKern.'''
    ikern = InlinedKern(Schedule())
    text = ikern.node_str(colour=False)
    assert text == "InlinedKern[]"


def test_call_abstract_methods():
    ''' Check that calling the abstract methods of Kern raises
    the expected exceptions '''

    class KernType:
        ''' temporary dummy class '''
        def __init__(self):
            self.iterates_over = "stuff"
    my_ktype = KernType()

    class DummyClass:
        ''' temporary dummy class '''
        def __init__(self, ktype):
            self.module_name = "dummy_module"
            self.ktype = ktype

    class DummyArguments(Arguments):
        ''' temporary dummy class '''
        # This is a mock class, we can disable expected pylint warnings
        # pylint: disable=abstract-method, unused-argument
        def __init__(self, call, parent_call, check):
            Arguments.__init__(self, parent_call)

    dummy_call = DummyClass(my_ktype)
    my_call = Kern(None, dummy_call, "dummy", DummyArguments)
    with pytest.raises(NotImplementedError) as excinfo:
        my_call.local_vars()
    assert "Kern.local_vars should be implemented" in str(excinfo.value)

    with pytest.raises(NotImplementedError) as excinfo:
        my_call.gen_code(None)
    assert "Kern.gen_code should be implemented" in str(excinfo.value)


def test_arguments_abstract():
    ''' Check that we raise NotImplementedError if any of the virtual methods
    of the Arguments class are called. '''
    my_arguments = Arguments(None)
    with pytest.raises(NotImplementedError) as err:
        _ = my_arguments.acc_args
    assert ("Arguments.acc_args must be implemented in sub-class"
            in str(err.value))
    with pytest.raises(NotImplementedError) as err:
        _ = my_arguments.scalars
    assert ("Arguments.scalars must be implemented in sub-class"
            in str(err.value))
    with pytest.raises(NotImplementedError) as err:
        _ = my_arguments.raw_arg_list()
    assert ("Arguments.raw_arg_list must be implemented in sub-class"
            in str(err.value))
    with pytest.raises(NotImplementedError) as err:
        _ = my_arguments.append("var", "type")
    assert ("Arguments.append must be implemented in sub-class"
            in str(err.value))


def test_incremented_arg():
    ''' Check that we raise the expected exception when
    CodedKern.incremented_arg() is called for a kernel that does not have
    an argument that is incremented '''
    # Change the kernel metadata so that the the incremented kernel
    # argument has read access
    logging.disable(logging.CRITICAL)
    # If we change the metadata then we trip the check in the parser.
    # Therefore, we change the object produced by parsing the metadata
    # instead
    ast = fpapi.parse(FAKE_KERNEL_METADATA, ignore_comments=False)
    metadata = LFRicKernMetadata(ast)
    for descriptor in metadata.arg_descriptors:
        if descriptor.access == AccessType.INC:
            descriptor._access = AccessType.READ
    my_kern = LFRicKern()
    my_kern.load_meta(metadata)
    with pytest.raises(FieldNotFoundError) as excinfo:
        CodedKern.incremented_arg(my_kern)
    assert ("does not have an argument with gh_inc access"
            in str(excinfo.value))


def test_kern_is_coloured1():
    ''' Check that the is_coloured method behaves as expected. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    kern = schedule.walk(Kern)[0]
    assert not kern.is_coloured()
    # Colour the loop around the kernel
    ctrans = Dynamo0p3ColourTrans()
    ctrans.apply(schedule[0])
    assert kern.is_coloured()
    # Test when the Kernel appears to have no parent loop
    schedule.addchild(kern.detach())
    assert not kern.is_coloured()


def test_kern_is_coloured2():
    ''' Check that the is_coloured() works, independent of which loop in a
    loop nest is of type "colour". '''
    table = SymbolTable()
    # Create the loop variables
    for idx in range(3):
        table.new_symbol(f"cell{idx}", symbol_type=DataSymbol,
                         datatype=INTEGER_TYPE)
    # Create a loop nest of depth 3 containing the kernel, innermost first
    my_kern = LFRicKern()
    loops = [PSyLoop.create(table.lookup("cell0"),
                            Literal("1", INTEGER_TYPE),
                            Literal("10", INTEGER_TYPE),
                            Literal("1", INTEGER_TYPE), [my_kern])]
    loops.append(PSyLoop.create(table.lookup("cell1"),
                                Literal("1", INTEGER_TYPE),
                                Literal("10", INTEGER_TYPE),
                                Literal("1", INTEGER_TYPE), [loops[-1]]))
    loops.append(PSyLoop.create(table.lookup("cell2"),
                                Literal("1", INTEGER_TYPE),
                                Literal("10", INTEGER_TYPE),
                                Literal("1", INTEGER_TYPE), [loops[-1]]))
    # As we're using the generic Loop class, we have to manually set the list
    # of valid Loop types
    for loop in loops:
        loop._valid_loop_types = ["colour", ""]
    # We have no coloured loops at this point
    assert not my_kern.is_coloured()
    # Test that things work as expected, independent of which loop is coloured
    for loop in loops:
        loop.loop_type = "colour"
        assert my_kern.is_coloured()
        loop.loop_type = ""
    assert not my_kern.is_coloured()


def test_haloexchange_unknown_halo_depth():
    '''test the case when the halo exchange base class is called without
    a halo depth'''
    halo_exchange = HaloExchange(None)
    assert halo_exchange._halo_depth is None


def test_globalsum_node_str():
    '''test the node_str method in the GlobalSum class. The simplest way
    to do this is to use a dynamo0p3 builtin example which contains a
    scalar and then call node_str() on that.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.9.1_X_innerproduct_Y_builtin.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    gsum = None
    for child in psy.invokes.invoke_list[0].schedule.children:
        if isinstance(child, DynGlobalSum):
            gsum = child
            break
    assert gsum
    output = gsum.node_str()
    expected_output = (colored("GlobalSum", GlobalSum._colour) +
                       "[scalar='asum']")
    assert expected_output in output


def test_globalsum_children_validation():
    '''Test that children added to GlobalSum are validated. A GlobalSum node
    does not accept any children.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.9.1_X_innerproduct_Y_builtin.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    gsum = None
    for child in psy.invokes.invoke_list[0].schedule.children:
        if isinstance(child, DynGlobalSum):
            gsum = child
            break
    with pytest.raises(GenerationError) as excinfo:
        gsum.addchild(Literal("2", INTEGER_TYPE))
    assert ("Item 'Literal' can't be child 0 of 'GlobalSum'. GlobalSum is a"
            " LeafNode and doesn't accept children.") in str(excinfo.value)


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
    ftrans = LFRicLoopFuseTrans()
    ftrans.apply(schedule.children[0], schedule.children[1])
    # get our loop and call our method ...
    loop = schedule.children[0]
    args = loop.args_filter(unique=True)
    expected_output = ["a", "f1", "f2", "m1", "m2", "f3"]
    for arg in args:
        assert arg.name in expected_output
    assert len(args) == len(expected_output)


def test_args_filter2():
    ''' The args_filter() method is in both Loop() and Arguments() classes
    with the former method calling the latter. This example tests the cases
    when one or both of the intent and type arguments are not specified. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "10_operator.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    loop = schedule.children[3]

    # arg_accesses
    args = loop.args_filter(arg_accesses=[AccessType.READ])
    expected_output = ["coord", "a"]
    for arg in args:
        assert arg.name in expected_output
    assert len(args) == len(expected_output)

    # arg_types
    args = loop.args_filter(arg_types=["gh_operator", "gh_scalar"])
    expected_output = ["mm_w0", "a"]
    for arg in args:
        assert arg.name in expected_output
    assert len(args) == len(expected_output)

    # neither
    args = loop.args_filter()
    expected_output = ["coord", "mm_w0", "a"]
    for arg in args:
        assert arg.name in expected_output
    assert len(args) == len(expected_output)


def test_reduction_var_error(dist_mem):
    ''' Check that we raise an exception if the zero_reduction_variable()
    method is provided with an incorrect type of argument. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=dist_mem).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    call = schedule.kernels()[0]
    # args[1] is of type gh_field
    call._reduction_arg = call.arguments.args[1]
    with pytest.raises(GenerationError) as err:
        call.zero_reduction_variable(None)
    assert ("Kern.zero_reduction_variable() should be a scalar but "
            "found 'gh_field'." in str(err.value))


def test_reduction_var_invalid_scalar_error(dist_mem):
    ''' Check that we raise an exception if the zero_reduction_variable()
    method is provided with an incorrect intrinsic type of scalar
    argument (other than 'real' or 'integer').

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "1.7_single_invoke_3scalar.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=dist_mem).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    call = schedule.kernels()[0]
    # args[5] is a scalar of data type gh_logical
    call._reduction_arg = call.arguments.args[5]
    with pytest.raises(GenerationError) as err:
        call.zero_reduction_variable(None)
    assert ("Kern.zero_reduction_variable() should be either a 'real' "
            "or an 'integer' scalar but found scalar of type 'logical'."
            in str(err.value))


def test_reduction_sum_error(dist_mem):
    ''' Check that we raise an exception if the reduction_sum_loop()
    method is provided with an incorrect type of argument. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=dist_mem).create(invoke_info)
    schedule = psy.invokes.invoke_list[0].schedule
    call = schedule.kernels()[0]
    # args[1] is of type gh_field
    call._reduction_arg = call.arguments.args[1]
    with pytest.raises(GenerationError) as err:
        call.reduction_sum_loop(None)
    assert ("Unsupported reduction access 'gh_inc' found in LFRicBuiltIn:"
            "reduction_sum_loop(). Expected one of ['gh_sum']."
            in str(err.value))


def test_call_multi_reduction_error(monkeypatch, dist_mem):
    ''' Check that we raise an exception if we try to create a Call (a
    Kernel or a Built-in) with more than one reduction in it. Since we have
    a rule that only Built-ins can write to scalars we need a Built-in that
    attempts to perform two reductions.

    '''
    monkeypatch.setattr(lfric_builtins, "BUILTIN_DEFINITIONS_FILE",
                        value=os.path.join(BASE_PATH,
                                           "multi_reduction_builtins_mod.f90"))
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "16.4.1_multiple_scalar_sums2.f90"),
        api="dynamo0.3")
    with pytest.raises(GenerationError) as err:
        _ = PSyFactory("dynamo0.3",
                       distributed_memory=dist_mem).create(invoke_info)
    assert ("PSyclone currently only supports a single reduction in a kernel "
            "or builtin" in str(err.value))


def test_reduction_no_set_precision(dist_mem):
    '''Test that the zero_reduction_variable() method generates correct
    code when a reduction argument does not have a defined
    precision. Only a zero value (without precision i.e. 0.0 not
    0.0_r_def) is generated in this case.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.8.1_sum_X_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=dist_mem).create(invoke_info)

    # A reduction argument will always have a precision value so we
    # need to monkeypatch it.
    schedule = psy.invokes.invoke_list[0].schedule
    builtin = schedule.walk(BuiltIn)[0]
    arg = builtin.arguments.args[0]
    arg._precision = ""

    generated_code = str(psy.gen)

    if dist_mem:
        zero_sum_decls = (
            "      USE scalar_mod, ONLY: scalar_type\n"
            "      USE mesh_mod, ONLY: mesh_type\n"
            "      REAL, intent(out) :: asum\n"
            "      TYPE(field_type), intent(in) :: f1\n"
            "      TYPE(scalar_type) global_sum\n"
            "      INTEGER df\n")
    else:
        zero_sum_decls = (
            "      REAL, intent(out) :: asum\n"
            "      TYPE(field_type), intent(in) :: f1\n"
            "      INTEGER df\n")
    assert zero_sum_decls in generated_code

    zero_sum_output = (
        "      ! Zero summation variables\n"
        "      !\n"
        "      asum = 0.0\n")
    assert zero_sum_output in generated_code


def test_invokes_wrong_schedule_gen_code():
    ''' Check that the invoke.schedule reference points to an InvokeSchedule
    when using the gen_code. Otherwise rise an error. '''
    # Use LFRic example with a repeated CodedKern
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "4.6_multikernel_invokes.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)

    # Set the invoke.schedule to something else other than a InvokeSchedule
    psy.invokes.invoke_list[0].schedule = Node()
    with pytest.raises(GenerationError) as err:
        _ = psy.gen
    assert ("An invoke.schedule element of the invoke_list is a 'Node', "
            "but it should be an 'InvokeSchedule'." in str(err.value))


def test_invoke_name():
    ''' Check that specifying the name of an invoke in the Algorithm
    layer results in a correctly-named routine in the PSy layer '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "1.0.1_single_named_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    gen = str(psy.gen)

    assert "SUBROUTINE invoke_important_invoke" in gen


def test_multi_kern_named_invoke(tmpdir):
    ''' Check that specifying the name of an invoke containing multiple
    kernel invocations result in a correctly-named routine in the PSy layer '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.9_named_multikernel_invokes.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    gen = str(psy.gen)

    assert "SUBROUTINE invoke_some_name" in gen
    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_named_multi_invokes(tmpdir):
    ''' Check that we generate correct code when we have more than one
    named invoke in an Algorithm file '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "3.2_multi_functions_multi_named_invokes.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    gen = str(psy.gen)

    assert "SUBROUTINE invoke_my_first(" in gen
    assert "SUBROUTINE invoke_my_second(" in gen
    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_named_invoke_name_clash(tmpdir):
    ''' Check that we do not get a name clash when the name of a variable
    in the PSy layer would normally conflict with the name given to the
    subroutine generated by an Invoke. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.11_named_invoke_name_clash.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    gen = str(psy.gen)

    assert ("SUBROUTINE invoke_a(invoke_a_1, b, istp, rdt, d, e, ascalar, "
            "f, c, g, qr)") in gen
    assert "TYPE(field_type), intent(in) :: invoke_a_1" in gen

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_invalid_reprod_pad_size(monkeypatch, dist_mem):
    '''Check that we raise an exception if the pad size in psyclone.cfg is
    set to an invalid value '''
    # Make sure we monkey patch the correct Config object
    config = Config.get()
    monkeypatch.setattr(config._instance, "_reprod_pad_size", 0)
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.9.1_X_innerproduct_Y_builtin.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=dist_mem).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    otrans = Dynamo0p3OMPLoopTrans()
    rtrans = OMPParallelTrans()
    # Apply an OpenMP do directive to the loop
    otrans.apply(schedule.children[0], {"reprod": True})
    # Apply an OpenMP Parallel directive around the OpenMP do directive
    rtrans.apply(schedule.children[0])
    with pytest.raises(GenerationError) as excinfo:
        _ = str(psy.gen)
    assert (
        f"REPROD_PAD_SIZE in {Config.get().filename} should be a positive "
        f"integer" in str(excinfo.value))


def test_argument_properties():
    ''' Check the default values for properties of a generic
    argument instance. Also check that when the internal values
    are changed, the related property methods return the
    updated values (where applicable).

    '''
    # Instantiate a generic argument and check the default
    # property values
    arg = Argument(None, None, None)
    assert arg.is_literal is False
    assert arg.argument_type == "field"
    assert arg.precision is None
    assert arg.data_type is None
    assert arg.module_name is None

    # Change the internal values and check the properties
    # again (note that the "argument_type" property is
    # hard-coded to return "field")
    arg._is_literal = True
    arg._precision = "i_def"
    arg._data_type = "field_type"
    arg._module_name = "operator_mod"
    assert arg.is_literal is True
    assert arg.precision == "i_def"
    assert arg.data_type == "field_type"
    assert arg.module_name == "operator_mod"


def test_argument_infer_datatype():
    '''
    Check that a generic argument inferred datatype is a UnresolvedType.
    '''
    arg = Argument(None, None, None)
    assert isinstance(arg.infer_datatype(), UnresolvedType)


def test_argument_depends_on():
    '''Check that the depends_on method returns the appropriate boolean
    value for arguments with combinations of read and write access'''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.5_multikernel_invokes.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    arg_f1_inc_1 = schedule.children[0].loop_body[0].arguments.args[0]
    arg_f1_inc_2 = schedule.children[2].loop_body[0].arguments.args[0]
    arg_f2_read_1 = schedule.children[0].loop_body[0].arguments.args[2]
    arg_f2_inc = schedule.children[1].loop_body[0].arguments.args[0]
    arg_f2_read_2 = schedule.children[2].loop_body[0].arguments.args[1]
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
        os.path.join(BASE_PATH,
                     "15.14.4_builtin_and_normal_kernel_invoke.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    arg_f1_write_1 = schedule.children[0].loop_body[0].arguments.args[1]
    arg_f1_write_2 = schedule.children[1].loop_body[0].arguments.args[0]
    assert arg_f1_write_1._depends_on(arg_f1_write_2)


def test_argument_find_argument():
    ''' Check that the find_argument method returns the first dependent
    argument in a list of nodes, or None if none are found. '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.14.1_multi_aX_plus_Y_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # 1: returns none if none found
    f1_first_read = schedule.children[0].loop_body[0].arguments.args[2]
    # a) empty node list
    assert not f1_first_read._find_argument([])
    # b) check many reads
    call_nodes = schedule.kernels()
    assert not f1_first_read._find_argument(call_nodes)
    # 2: returns first dependent kernel arg when there are many
    # dependencies (check first read returned)
    f3_write = schedule.children[3].loop_body[0].arguments.args[0]
    f3_first_read = schedule.children[0].loop_body[0].arguments.args[3]
    result = f3_write._find_argument(call_nodes)
    assert result == f3_first_read
    # 3: haloexchange node
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.14.4_builtin_and_normal_kernel_invoke.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # a) kern arg depends on halo arg
    m2_read_arg = schedule.children[4].loop_body[0].arguments.args[4]
    m2_halo_field = schedule.children[3].field
    result = m2_read_arg._find_argument(schedule.children)
    assert result == m2_halo_field
    # b) halo arg depends on kern arg
    result = m2_halo_field._find_argument([schedule.children[4].loop_body[0]])
    assert result == m2_read_arg
    # 4: globalsum node
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.14.3_sum_setval_field_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # a) globalsum arg depends on kern arg
    kern_asum_arg = schedule.children[3].loop_body[0].arguments.args[1]
    glob_sum_arg = schedule.children[2].scalar
    result = kern_asum_arg._find_argument(schedule.children)
    assert result == glob_sum_arg
    # b) kern arg depends on globalsum arg
    result = glob_sum_arg._find_argument([schedule.children[3].loop_body[0]])
    assert result == kern_asum_arg


def test_argument_find_read_arguments():
    '''Check that the find_read_arguments method returns the appropriate
    arguments in a list of nodes.'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.14.1_multi_aX_plus_Y_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # 1: returns [] if not a writer. f1 is read, not written.
    f1_first_read = schedule.children[0].loop_body[0].arguments.args[2]
    call_nodes = schedule.kernels()
    assert f1_first_read._find_read_arguments(call_nodes) == []
    # 2: return list of readers (f3 is written to and then read by
    # three following calls)
    f3_write = schedule.children[3].loop_body[0].arguments.args[0]
    result = f3_write._find_read_arguments(call_nodes[4:])
    assert len(result) == 3
    for idx in range(3):
        loop = schedule.children[idx+4]
        assert result[idx] == loop.loop_body[0].arguments.args[3]
    # 3: Return empty list if no readers (f2 is written to but not
    # read)
    f2_write = schedule.children[0].loop_body[0].arguments.args[0]
    assert f2_write._find_read_arguments(call_nodes[1:]) == []
    # 4: Return list of readers before a subsequent writer
    f3_write = schedule.children[3].loop_body[0].arguments.args[0]
    result = f3_write._find_read_arguments(call_nodes)
    assert len(result) == 3
    for idx in range(3):
        loop = schedule.children[idx]
        assert result[idx] == loop.loop_body[0].arguments.args[3]


def test_globalsum_arg():
    ''' Check that the globalsum argument is defined as gh_readwrite and
    points to the GlobalSum node '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.14.3_sum_setval_field_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    glob_sum = schedule.children[2]
    glob_sum_arg = glob_sum.scalar
    assert glob_sum_arg.access == AccessType.READWRITE
    assert glob_sum_arg.call == glob_sum


def test_haloexchange_arg():
    '''Check that the HaloExchange argument is defined as gh_readwrite and
    points to the HaloExchange node'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.14.4_builtin_and_normal_kernel_invoke.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    halo_exchange = schedule.children[2]
    halo_exchange_arg = halo_exchange.field
    assert halo_exchange_arg.access == AccessType.READWRITE
    assert halo_exchange_arg.call == halo_exchange


def test_argument_forward_read_dependencies():
    '''Check that the forward_read_dependencies method returns the appropriate
    arguments in a schedule.'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.14.1_multi_aX_plus_Y_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # 1: returns [] if not a writer. f1 is read, not written.
    f1_first_read = schedule.children[0].loop_body[0].arguments.args[2]
    _ = schedule.kernels()
    assert f1_first_read.forward_read_dependencies() == []
    # 2: return list of readers (f3 is written to and then read by
    # three following calls)
    f3_write = schedule.children[3].loop_body[0].arguments.args[0]
    result = f3_write.forward_read_dependencies()
    assert len(result) == 3
    for idx in range(3):
        loop = schedule.children[idx+4]
        assert result[idx] == loop.loop_body[0].arguments.args[3]
    # 3: Return empty list if no readers (f2 is written to but not
    # read)
    f2_write = schedule.children[0].loop_body[0].arguments.args[0]
    assert f2_write.forward_read_dependencies() == []


def test_argument_forward_dependence(monkeypatch, annexed):
    '''Check that forward_dependence method returns the first dependent
    argument after the current Node in the schedule or None if none
    are found. We also test when annexed is False and True as it
    affects how many halo exchanges are generated.

    '''
    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.14.1_multi_aX_plus_Y_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    f1_first_read = schedule.children[0].loop_body[0].arguments.args[2]
    # 1: returns none if none found (check many reads)
    assert not f1_first_read.forward_dependence()
    # 2: returns first dependent kernel arg when there are many
    # dependencies (check first read returned)
    f3_write = schedule.children[3].loop_body[0].arguments.args[0]
    f3_next_read = schedule.children[4].loop_body[0].arguments.args[3]
    result = f3_write.forward_dependence()
    assert result == f3_next_read
    # 3: haloexchange dependencies
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "4.5_multikernel_invokes.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    if annexed:
        index = 7
    else:
        index = 8
    f2_prev_arg = schedule.children[index-1].loop_body[0].arguments.args[0]
    f2_halo_field = schedule.children[index].field
    f2_next_arg = schedule.children[index+1].loop_body[0].arguments.args[1]
    # a) previous kern arg depends on halo arg
    result = f2_prev_arg.forward_dependence()
    assert result == f2_halo_field
    # b) halo arg depends on following kern arg
    result = f2_halo_field.forward_dependence()
    assert result == f2_next_arg
    # 4: globalsum dependencies
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.14.3_sum_setval_field_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    prev_arg = schedule.children[0].loop_body[0].arguments.args[1]
    sum_arg = schedule.children[1].loop_body[0].arguments.args[0]
    global_sum_arg = schedule.children[2].scalar
    next_arg = schedule.children[3].loop_body[0].arguments.args[1]
    # a) prev kern arg depends on sum
    result = prev_arg.forward_dependence()
    assert result == sum_arg
    # b) sum arg depends on global sum arg
    result = sum_arg.forward_dependence()
    assert result == global_sum_arg
    # c) global sum arg depends on next kern arg
    result = global_sum_arg.forward_dependence()
    assert result == next_arg


def test_argument_backward_dependence(monkeypatch, annexed):
    '''Check that backward_dependence method returns the first dependent
    argument before the current Node in the schedule or None if none
    are found. We also test when annexed is False and True as it
    affects how many halo exchanges are generated.

    '''
    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.14.1_multi_aX_plus_Y_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    f1_last_read = schedule.children[6].loop_body[0].arguments.args[2]
    # 1: returns none if none found (check many reads)
    assert not f1_last_read.backward_dependence()
    # 2: returns first dependent kernel arg when there are many
    # dependencies (check first read returned)
    f3_write = schedule.children[3].loop_body[0].arguments.args[0]
    f3_prev_read = schedule.children[2].loop_body[0].arguments.args[3]
    result = f3_write.backward_dependence()
    assert result == f3_prev_read
    # 3: haloexchange dependencies
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "4.5_multikernel_invokes.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    if annexed:
        index = 7
    else:
        index = 8
    f2_prev_arg = schedule.children[index-1].loop_body[0].arguments.args[0]
    f2_halo_field = schedule.children[index].field
    f2_next_arg = schedule.children[index+1].loop_body[0].arguments.args[1]
    # a) following kern arg depends on halo arg
    result = f2_next_arg.backward_dependence()
    assert result == f2_halo_field
    # b) halo arg depends on previous kern arg
    result = f2_halo_field.backward_dependence()
    assert result == f2_prev_arg
    # 4: globalsum dependencies
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.14.3_sum_setval_field_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    prev_arg = schedule.children[0].loop_body[0].arguments.args[1]
    sum_arg = schedule.children[1].loop_body[0].arguments.args[0]
    global_sum_arg = schedule.children[2].scalar
    next_arg = schedule.children[3].loop_body[0].arguments.args[1]
    # a) next kern arg depends on global sum arg
    result = next_arg.backward_dependence()
    assert result == global_sum_arg
    # b) global sum arg depends on sum arg
    result = global_sum_arg.backward_dependence()
    assert result == sum_arg
    # c) sum depends on prev kern arg
    result = sum_arg.backward_dependence()
    assert result == prev_arg


def test_call_args():
    '''Test that the call class args method returns the appropriate
    arguments '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH,
                     "15.14.4_builtin_and_normal_kernel_invoke.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    kern = schedule.children[0].loop_body[0]
    builtin = schedule.children[1].loop_body[0]
    # 1) kern
    for idx, arg in enumerate(kern.args):
        assert arg == kern.arguments.args[idx]
    # 2) builtin
    for idx, arg in enumerate(builtin.args):
        assert arg == builtin.arguments.args[idx]


def test_haloexchange_can_be_printed():
    '''Test that the HaloExchange class can always be printed'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "1_single_invoke.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    for haloexchange in schedule.children[:2]:
        assert "HaloExchange[field='" in str(haloexchange)
        assert "', type='" in str(haloexchange)
        assert "', depth=" in str(haloexchange)
        assert ", check_dirty=" in str(haloexchange)


def test_haloexchange_node_str():
    ''' Test the node_str() method of HaloExchange. '''

    # We have to use the LFRic (Dynamo0.3) API as that's currently the only
    # one that supports halo exchanges.
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "1_single_invoke.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # We have to manually call the correct node_str() method as the one we want
    # to test is overridden in LFRicHaloExchange.
    out = HaloExchange.node_str(schedule.children[2])
    assert (colored("HaloExchange", HaloExchange._colour) +
            "[field='m1', type='None', depth=None, check_dirty=True]" in out)


def test_haloexchange_children_validation():
    '''Test that children added to HaloExchange are validated. A HaloExchange
    node does not accept any children.

    '''
    haloex = HaloExchange(None)
    with pytest.raises(GenerationError) as excinfo:
        haloex.addchild(Literal("2", INTEGER_TYPE))
    assert ("Item 'Literal' can't be child 0 of 'HaloExchange'. HaloExchange "
            "is a LeafNode and doesn't accept children.") in str(excinfo.value)


def test_haloexchange_args():
    '''Test that the haloexchange class args method returns the appropriate
    argument '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "1_single_invoke.f90"),
        api="dynamo0.3")
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
        os.path.join(BASE_PATH, "15.14.3_sum_setval_field_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    global_sum = schedule.children[2]
    assert len(global_sum.args) == 1
    assert global_sum.args[0] == global_sum.scalar


def test_call_forward_dependence():
    '''Test that the Call class forward_dependence method returns the
    closest dependent call after the current call in the schedule or
    None if none are found. This is achieved by loop fusing first.'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.14.1_multi_aX_plus_Y_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    ftrans = LFRicLoopFuseTrans()
    for _ in range(6):
        ftrans.apply(schedule.children[0], schedule.children[1],
                     {"same_space": True})
    read4 = schedule.children[0].loop_body[4]
    # 1: returns none if none found
    # a) check many reads
    assert not read4.forward_dependence()
    # 2: returns first dependent kernel arg when there are many
    # dependencies
    # a) check first read returned
    writer = schedule.children[0].loop_body[3]
    next_read = schedule.children[0].loop_body[4]
    assert writer.forward_dependence() == next_read
    # a) check writer returned
    first_loop = schedule.children[0].loop_body[0]
    assert first_loop.forward_dependence() == writer


def test_call_backward_dependence():
    '''Test that the Call class backward_dependence method returns the
    closest dependent call before the current call in the schedule or
    None if none are found. This is achieved by loop fusing first.'''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "15.14.1_multi_aX_plus_Y_builtin.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    ftrans = LFRicLoopFuseTrans()
    for _ in range(6):
        ftrans.apply(schedule.children[0], schedule.children[1],
                     {"same_space": True})
    # 1: loop no backwards dependence
    call3 = schedule.children[0].loop_body[2]
    assert not call3.backward_dependence()
    # 2: call to call backward dependence
    # a) many steps
    last_call_node = schedule.children[0].loop_body[6]
    prev_dep_call_node = schedule.children[0].loop_body[3]
    assert last_call_node.backward_dependence() == prev_dep_call_node
    # b) previous
    assert prev_dep_call_node.backward_dependence() == call3


def test_haloexchange_halo_depth_get_set():
    '''test that the halo_exchange getter and setter work correctly '''
    halo_depth = 4
    halo_exchange = HaloExchange(None)
    # getter
    assert halo_exchange.halo_depth is None
    # setter
    halo_exchange.halo_depth = halo_depth
    assert halo_exchange.halo_depth == halo_depth


def test_haloexchange_vector_index_depend():
    ''' Check that _find_read_arguments does not return a halo exchange as
    a read dependence if the source node is a halo exchange and its
    field is a vector and the other halo exchange accesses a different
    element of the vector

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "4.9_named_multikernel_invokes.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    first_e_field_halo_exchange = schedule.children[3]
    field = first_e_field_halo_exchange.field
    all_nodes = schedule.walk(Node)
    following_nodes = all_nodes[5:]
    result_list = field._find_read_arguments(following_nodes)
    assert len(result_list) == 1
    assert result_list[0].call.name == 'ru_code'


def test_find_write_arguments_for_write():
    '''When backward_write_dependencies or forward_write_dependencies in
    class Argument are called from a field argument that does not read
    then we should return an empty list. This test checks this
    functionality. We use the LFRic (Dynamo0.3) API to create the
    required objects.

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "1.5.1_single_invoke_write_multi_fs.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    loop = schedule.children[13]
    kernel = loop.loop_body[0]
    field_writer = kernel.arguments.args[7]
    node_list = field_writer.backward_write_dependencies()
    assert node_list == []
    node_list = field_writer.forward_write_dependencies()
    assert node_list == []


def test_find_w_args_hes_no_vec(monkeypatch, annexed):
    '''When backward_write_dependencies, forward_read_dependencies, or
    forward_write_dependencies are called and a dependence is found
    between two halo exchanges, then the field must be a vector
    field. If the field is not a vector then an exception is
    raised. This test checks that the exception is raised
    correctly. Also test with and without annexed dofs being computed
    as this affects the generated code.

    '''
    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "4.9_named_multikernel_invokes.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    if annexed:
        index = 4
    else:
        index = 5
    halo_exchange_e_v3 = schedule.children[index]
    field_e_v3 = halo_exchange_e_v3.field
    monkeypatch.setattr(field_e_v3, "_vector_size", 1)
    with pytest.raises(InternalError) as excinfo:
        _ = field_e_v3.backward_write_dependencies()
    assert ("DataAccess.overlaps(): vector sizes differ for field 'e' in two "
            "halo exchange calls. Found '1' and '3'" in str(excinfo.value))
    halo_exchange_e_v2 = schedule.children[index-1]
    field_e_v2 = halo_exchange_e_v2.field
    with pytest.raises(InternalError) as excinfo:
        _ = field_e_v2.forward_read_dependencies()
    assert ("DataAccess.overlaps(): vector sizes differ for field 'e' in two "
            "halo exchange calls. Found '3' and '1'" in str(excinfo.value))
    with pytest.raises(InternalError) as excinfo:
        _ = field_e_v2.forward_write_dependencies()
    assert ("DataAccess.overlaps(): vector sizes differ for field 'e' in two "
            "halo exchange calls. Found '3' and '1'" in str(excinfo.value))


def test_find_w_args_hes_diff_vec(monkeypatch, annexed):
    '''When backward_write_dependencies, forward_read_dependencies, or
    forward_write_dependencies are called and a dependence is found
    between two halo exchanges, then the associated fields must be
    equal size vectors. If the fields are not vectors of equal size
    then an exception is raised. This test checks that the exception
    is raised correctly. Also test with and without annexed dofs being
    computed as this affects the generated code.

    '''
    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "4.9_named_multikernel_invokes.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    if annexed:
        index = 4
    else:
        index = 5
    halo_exchange_e_v3 = schedule.children[index]
    field_e_v3 = halo_exchange_e_v3.field
    monkeypatch.setattr(field_e_v3, "_vector_size", 2)
    with pytest.raises(InternalError) as excinfo:
        _ = field_e_v3.backward_write_dependencies()
    assert ("DataAccess.overlaps(): vector sizes differ for field 'e' in two "
            "halo exchange calls. Found '2' and '3'" in str(excinfo.value))
    halo_exchange_e_v2 = schedule.children[index-1]
    field_e_v2 = halo_exchange_e_v2.field
    with pytest.raises(InternalError) as excinfo:
        _ = field_e_v2.forward_read_dependencies()
    assert ("DataAccess.overlaps(): vector sizes differ for field 'e' in two "
            "halo exchange calls. Found '3' and '2'" in str(excinfo.value))
    with pytest.raises(InternalError) as excinfo:
        _ = field_e_v2.forward_write_dependencies()
    assert ("DataAccess.overlaps(): vector sizes differ for field 'e' in two "
            "halo exchange calls. Found '3' and '2'" in str(excinfo.value))


def test_find_w_args_hes_vec_idx(monkeypatch, annexed):
    '''When backward_write_dependencies, forward_read_dependencies or
    forward_write_dependencies are called, and a dependence is found
    between two halo exchanges, then the vector indices of the two
    halo exchanges must be different. If the vector indices have the
    same value then an exception is raised. This test checks that the
    exception is raised correctly. Also test with and without annexed
    dofs being computed as this affects the generated code.

    '''
    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "4.9_named_multikernel_invokes.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    if annexed:
        index = 4
    else:
        index = 5
    halo_exchange_e_v3 = schedule.children[index]
    field_e_v3 = halo_exchange_e_v3.field
    halo_exchange_e_v2 = schedule.children[index-1]
    monkeypatch.setattr(halo_exchange_e_v2, "_vector_index", 3)
    with pytest.raises(InternalError) as excinfo:
        _ = field_e_v3.backward_write_dependencies()
    assert ("DataAccess:update_coverage() The halo exchange vector indices "
            "for 'e' are the same. This should never happen"
            in str(excinfo.value))
    halo_exchange_e_v2 = schedule.children[index-1]
    field_e_v2 = halo_exchange_e_v2.field
    with pytest.raises(InternalError) as excinfo:
        _ = field_e_v2.forward_read_dependencies()
    assert ("DataAccess:update_coverage() The halo exchange vector indices "
            "for 'e' are the same. This should never happen"
            in str(excinfo.value))
    with pytest.raises(InternalError) as excinfo:
        _ = field_e_v2.forward_write_dependencies()
    assert ("DataAccess:update_coverage() The halo exchange vector indices "
            "for 'e' are the same. This should never happen"
            in str(excinfo.value))


def test_find_w_args_hes_vec_no_dep(monkeypatch, annexed):
    ''' When _find_write_arguments, or _find_read_arguments, are called,
    halo exchanges with the same field but a different index should
    not depend on each other. This test checks that this behaviour is
    working correctly. Also test with and without annexed
    dofs being computed as this affects the generated code.

    '''
    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "4.9_named_multikernel_invokes.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    if annexed:
        index = 4
    else:
        index = 5
    halo_exchange_e_v3 = schedule.children[index]
    field_e_v3 = halo_exchange_e_v3.field
    # There are two halo exchanges before e_v3 which should not count
    # as dependencies.
    node_list = field_e_v3.backward_write_dependencies()
    assert node_list == []
    halo_exchange_e_v1 = schedule.children[index-2]
    field_e_v1 = halo_exchange_e_v1.field
    # There are two halo exchanges after e_v1 which should not count
    # as dependencies. We should only get the read access from a
    # kernel.
    node_list = field_e_v1.forward_read_dependencies()
    assert len(node_list) == 1
    assert isinstance(node_list[0].call, LFRicKern)
    # There are two halo exchanges after e_v1 which should not count
    # as dependencies and a read access from a kernel, so there should
    # be no write dependencies.
    node_list = field_e_v1.forward_write_dependencies()
    assert node_list == []


def test_check_vect_hes_differ_wrong_argtype():
    '''when the check_vector_halos_differ method is called from a halo
    exchange object the argument being passed should be a halo
    exchange. If this is not the case an exception should be
    raised. This test checks that this exception is working correctly.
    '''

    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    halo_exchange = schedule.children[0]
    with pytest.raises(GenerationError) as excinfo:
        # pass an incorrect object to the method
        halo_exchange.check_vector_halos_differ(psy)
    assert (
        "the argument passed to HaloExchange.check_vector_halos_differ() "
        "is not a halo exchange object" in str(excinfo.value))


def test_check_vec_hes_differ_diff_names():
    ''' When the check_vector_halos_differ method is called from a halo
    exchange object the argument being passed should be a halo
    exchange with an argument having the same name as the local halo
    exchange argument name. If this is not the case an exception
    should be raised. This test checks that this exception is working
    correctly.

    '''

    _, invoke_info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    halo_exchange = schedule.children[0]
    # Obtain another halo exchange object which has an argument with a
    # different name
    different_halo_exchange = schedule.children[1]
    with pytest.raises(GenerationError) as excinfo:
        # Pass halo exchange with different name to the method
        halo_exchange.check_vector_halos_differ(different_halo_exchange)
    assert (
        "the halo exchange object passed to "
        "HaloExchange.check_vector_halos_differ() has a "
        "different field name 'f2' to self 'f1'" in str(excinfo.value))


def test_find_w_args_multiple_deps_error(monkeypatch, annexed, tmpdir):
    ''' When _find_write_arguments finds a write that causes it to return
    there should not be any previous dependencies. This test checks
    that an error is raised if this is not the case. We test with
    annexed dofs is True and False as different numbers of halo
    exchanges are created.

    '''

    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)

    _, invoke_info = parse(
        os.path.join(BASE_PATH, "8.3_multikernel_invokes_vector.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # Create halo exchanges between the two loops via redundant
    # computation
    if annexed:
        index = 1
    else:
        index = 4
    rc_trans = Dynamo0p3RedundantComputationTrans()
    rc_trans.apply(schedule.children[index], {"depth": 2})
    del schedule.children[index]
    loop = schedule.children[index+2]
    kernel = loop.loop_body[0]
    d_field = kernel.arguments.args[0]
    with pytest.raises(InternalError) as excinfo:
        d_field.backward_write_dependencies()
    assert (
        "Found a writer dependence but there are already dependencies"
        in str(excinfo.value))

    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_find_write_arguments_no_more_nodes(monkeypatch, annexed):
    ''' When _find_write_arguments has looked through all nodes but has
    not returned it should mean that is has not found any write
    dependencies. This test checks that an error is raised if this is
    not the case. We test with and without computing annexed dofs as
    different numbers of halo exchanges are created.

    '''

    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)

    _, invoke_info = parse(
        os.path.join(BASE_PATH, "4.9_named_multikernel_invokes.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    if annexed:
        index = 3
    else:
        index = 4
    del schedule.children[index]
    loop = schedule.children[index+1]
    kernel = loop.loop_body[0]
    e_field = kernel.arguments.args[5]
    with pytest.raises(InternalError) as excinfo:
        e_field.backward_write_dependencies()
    assert (
        "no more nodes but there are already dependencies"
        in str(excinfo.value))


def test_find_w_args_multiple_deps(monkeypatch, annexed):
    '''_find_write_arguments should return as many halo exchange
    dependencies as the vector size of the associated field. This test
    checks that this is the case and that the returned objects are
    what is expected. We test with annexed dofs is True and False as
    different numbers of halo exchanges are created.

    '''

    config = Config.get()
    dyn_config = config.api_conf("dynamo0.3")
    monkeypatch.setattr(dyn_config, "_compute_annexed_dofs", annexed)

    _, invoke_info = parse(
        os.path.join(BASE_PATH, "8.3_multikernel_invokes_vector.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # create halo exchanges between the two loops via redundant
    # computation
    if annexed:
        index = 1
    else:
        index = 4
    rc_trans = Dynamo0p3RedundantComputationTrans()
    rc_trans.apply(schedule.children[index], {"depth": 2})
    loop = schedule.children[index+3]
    kernel = loop.loop_body[0]
    d_field = kernel.arguments.args[0]
    vector_size = d_field.vector_size
    result_list = d_field.backward_write_dependencies()
    # we have as many dependencies as the field vector size
    assert vector_size == len(result_list)
    indices = set()
    for result in result_list:
        # each dependence is a halo exchange nodes
        assert isinstance(result.call, HaloExchange)
        # the name of the halo exchange field and the initial
        # field are the same
        assert result.name == d_field.name
        # the size of the halo exchange field vector and the initial
        # field vector are the same
        assert result.vector_size == vector_size
        indices.add(result.call.vector_index)
    # each of the indices are unique (otherwise the set would be
    # smaller)
    assert len(indices) == vector_size


def test_kern_ast():
    ''' Test that we can obtain the fparser2 AST of a kernel. '''
    _, invoke = get_invoke("nemolite2d_alg_mod.f90", "gocean1.0", idx=0)
    sched = invoke.schedule
    kern = sched.coded_kernels()[0]
    assert isinstance(kern, GOKern)
    assert kern.ast
    assert isinstance(kern.ast, Fortran2003.Program)


def test_dataaccess_vector():
    ''' Test that the DataAccess class works as expected when we have a
    vector field argument that depends on more than one halo exchange
    (due to halo exchanges working separately on components of
    vectors).

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "4.9_named_multikernel_invokes.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule

    # e from halo exchange vector 1
    halo_exchange_e_v1 = schedule.children[3]
    field_e_v1 = halo_exchange_e_v1.field
    # e from halo exchange vector 2
    halo_exchange_e_v2 = schedule.children[4]
    field_e_v2 = halo_exchange_e_v2.field
    # e from halo exchange vector 3
    halo_exchange_e_v3 = schedule.children[5]
    field_e_v3 = halo_exchange_e_v3.field
    # e from a kernel argument
    loop = schedule.children[6]
    kernel = loop.loop_body[0]
    e_arg = kernel.arguments.args[5]

    access = DataAccess(e_arg)
    assert not access.covered

    access.update_coverage(field_e_v3)
    assert not access.covered
    access.update_coverage(field_e_v2)
    assert not access.covered

    with pytest.raises(InternalError) as excinfo:
        access.update_coverage(field_e_v3)
    assert (
        "Found more than one dependent halo exchange with the same vector "
        "index" in str(excinfo.value))

    access.update_coverage(field_e_v1)
    assert access.covered

    access.reset_coverage()
    assert not access.covered
    assert not access._vector_index_access


def test_dataaccess_same_vector_indices(monkeypatch):
    ''' If update_coverage() is called from DataAccess and the arguments
    are the same vector field, and the field vector indices are the
    same then check that an exception is raised. This particular
    exception is difficult to raise as it is caught by an earlier
    method (overlaps()).

    '''
    _, invoke_info = parse(
        os.path.join(BASE_PATH, "4.9_named_multikernel_invokes.f90"),
        api="dynamo0.3")
    psy = PSyFactory("dynamo0.3",
                     distributed_memory=True).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    # e for this halo exchange is for vector component 2
    halo_exchange_e_v2 = schedule.children[4]
    field_e_v2 = halo_exchange_e_v2.field
    # modify e from vector component 3 to be component 2
    halo_exchange_e_v3 = schedule.children[5]
    field_e_v3 = halo_exchange_e_v3.field
    monkeypatch.setattr(halo_exchange_e_v3, "_vector_index", 2)

    # Now raise an exception with our erroneous vector indices (which
    # are the same but should not be), but first make sure that the
    # overlaps() method returns True otherwise an earlier exception
    # will be raised.
    access = DataAccess(field_e_v2)
    monkeypatch.setattr(access, "overlaps", lambda arg: True)

    with pytest.raises(InternalError) as excinfo:
        access.update_coverage(field_e_v3)
    assert (
        "The halo exchange vector indices for 'e' are the same. This should "
        "never happen" in str(excinfo.value))


def test_modified_kern_line_length(kernel_outputdir, monkeypatch):
    '''Modified Fortran kernels are written to file linewrapped at 132
    characters. This test checks that this linewrapping works.

    '''
    psy, invoke = get_invoke("1_single_invoke.f90", api="dynamo0.3", idx=0)
    sched = invoke.schedule
    kernels = sched.walk(Kern)
    # This example does not conform to the <name>_code, <name>_mod
    # convention so monkeypatch it to avoid the PSyIR code generation
    # raising an exception. This limitation is the subject of issue
    # #520.
    monkeypatch.setattr(kernels[0], "_module_name", "testkern_mod")
    ktrans = Dynamo0p3KernelConstTrans()
    ktrans.apply(kernels[0], {"number_of_layers": 100})
    # Generate the code (this triggers the generation of new kernels)
    _ = str(psy.gen)
    filepath = os.path.join(str(kernel_outputdir), "testkern_0_mod.f90")
    assert os.path.isfile(filepath)
    # Check that the argument list is line wrapped as it is longer
    # than 132 characters.
    with open(filepath, encoding="utf-8") as testfile:
        assert "map_w2, &\n&ndf_w3" in testfile.read()


def test_walk():
    '''Tests the walk functionality.'''

    # This function contains only umask(ji,jj,jk) = ji*jj*jk/r
    _, invoke = get_invoke("explicit_do.f90", "nemo", 0)

    # Test without stop type: one assignment
    assignment_list = invoke.schedule.walk(Assignment)
    assert len(assignment_list) == 1

    # Three binary operators: *, *, /
    binary_op_list = invoke.schedule.walk(BinaryOperation)
    assert len(binary_op_list) == 3

    # Now the same tests, but stop at any Loop --> no assignment
    # or binary operation should be found"
    assignment_list = invoke.schedule.walk(Assignment, Loop)
    assert not assignment_list

    binary_op_list = invoke.schedule.walk(BinaryOperation, Loop)
    assert not binary_op_list
