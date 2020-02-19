# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2019, Science and Technology Facilities Council.
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
# ----------------------------------------------------------------------------
# Author: S. Siso, STFC Daresbury Lab

'''Tests the GlobalsToArgumentsTransformation for the  GOcean 1.0 API.'''

from __future__ import absolute_import, print_function
import os
import pytest
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
from psyclone.psyir.symbols import DataType

API = "gocean1.0"

# Each os.path.dirname() move up in the folder hierarchy
BASEPATH = os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(
    os.path.dirname(os.path.abspath(__file__))))), "test_files")


def test_globalstoargumentstrans_wrongapi():
    ''' Check the KernelGlobalsToArguments with an API other than GOcean1p0'''
    from psyclone.transformations import KernelGlobalsToArguments, \
        TransformationError

    trans = KernelGlobalsToArguments()
    path = os.path.join(BASEPATH, "dynamo0p3")
    _, invoke_info = parse(os.path.join(path, "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    kernel = invoke.schedule.coded_kernels()[0]
    with pytest.raises(TransformationError) as err:
        trans.apply(kernel)
    assert "The KernelGlobalsToArguments transformation is currently only " \
           "supported for the GOcean API but got an InvokeSchedule of " \
           "type:" in str(err.value)


def test_globalstoargumentstrans(monkeypatch):
    ''' Check the GlobalsToArguments transformation with a single kernel
    invoke and a global variable.'''
    from psyclone.transformations import KernelGlobalsToArguments, \
        TransformationError
    from psyclone.psyGen import Argument
    from psyclone.psyir.backend.fortran import FortranWriter
    from psyclone.psyir.symbols import DataSymbol

    trans = KernelGlobalsToArguments()
    assert trans.name == "KernelGlobalsToArguments"
    assert str(trans) == "Convert the global variables used inside the " \
        "kernel into arguments and modify the InvokeSchedule to pass them" \
        " in the kernel call."

    # Construct a testing InvokeSchedule
    _, invoke_info = parse(os.path.join(BASEPATH, "gocean1p0",
                                        "single_invoke_kern_with_use.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    notkernel = invoke.schedule.children[0]
    kernel = invoke.schedule.coded_kernels()[0]

    # Monkeypatch resolve_deferred to avoid module searching and importing
    # in this test. In this case we assume it is a REAL
    def set_to_real(variable):
        variable._datatype = DataType.REAL
    monkeypatch.setattr(DataSymbol, "resolve_deferred", set_to_real)

    # Test with invalid node
    with pytest.raises(TransformationError) as err:
        trans.apply(notkernel)
    assert ("The KernelGlobalsToArguments transformation can only be applied"
            " to CodedKern nodes but found 'GOLoop' instead."
            in str(err.value))

    # Test transforming a single kernel
    trans.apply(kernel)

    # The transformation;
    # 1) Has imported the symbol into the InvokeSchedule
    assert invoke.schedule.symbol_table.lookup("rdt")
    assert invoke.schedule.symbol_table.lookup("model_mod")
    var = invoke.schedule.symbol_table.lookup("rdt")
    container = invoke.schedule.symbol_table.lookup("model_mod")
    assert var.is_global
    assert var.interface.container_symbol == container

    # 2) Has added the symbol as the last argument in the kernel call
    assert isinstance(kernel.args[-1], Argument)
    assert kernel.args[-1].name == "rdt"

    # 3) Has converted the Kernel Schedule symbol into an argument which is
    # in also the last position
    ksymbol = kernel.get_kernel_schedule().symbol_table.lookup("rdt")
    assert ksymbol.is_argument
    assert kernel.get_kernel_schedule().symbol_table.argument_list[-1] == \
        ksymbol
    assert len(kernel.get_kernel_schedule().symbol_table.argument_list) == \
        len(kernel.args) + 2  # GOcean kernels have 2 implicit arguments

    # Check the kernel code is generated as expected
    fwriter = FortranWriter()
    kernel_code = fwriter(kernel.get_kernel_schedule())
    assert "subroutine kernel_with_use_code(ji,jj,istep,ssha,tmask,rdt)" \
        in kernel_code
    assert "real, intent(inout) :: rdt" in kernel_code

    # Check that the PSy-layer generated code now contains the use statement
    # and argument call
    generated_code = str(psy.gen)
    assert "USE model_mod, ONLY: rdt" in generated_code
    assert "CALL kernel_with_use_code(i, j, oldu_fld, cu_fld%data, " \
           "cu_fld%grid%tmask, rdt)" in generated_code
    assert invoke.schedule.symbol_table.lookup("model_mod")
    assert invoke.schedule.symbol_table.lookup("rdt")


def test_globalstoargumentstrans_constant(monkeypatch):
    ''' Check the GlobalsToArguments transformation when the global is
    also a constant value, in this case the argument should be read-only.'''
    from psyclone.transformations import KernelGlobalsToArguments
    from psyclone.psyir.backend.fortran import FortranWriter
    from psyclone.psyir.symbols import DataSymbol

    trans = KernelGlobalsToArguments()

    # Construct a testing InvokeSchedule
    _, invoke_info = parse(os.path.join(BASEPATH, "gocean1p0",
                                        "single_invoke_kern_with_use.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    kernel = invoke.schedule.coded_kernels()[0]

    # Monkeypatch resolve_deferred to avoid module searching and importing
    # in this test. In this case we assume it is a constant INTEGER
    def set_to_const_int(variable):
        variable._datatype = DataType.INTEGER
        variable.constant_value = 1
    monkeypatch.setattr(DataSymbol, "resolve_deferred", set_to_const_int)

    # Test transforming a single kernel
    trans.apply(kernel)

    fwriter = FortranWriter()
    kernel_code = fwriter(kernel.get_kernel_schedule())

    assert "subroutine kernel_with_use_code(ji,jj,istep,ssha,tmask,rdt)" \
        in kernel_code
    assert "integer, intent(in) :: rdt" in kernel_code


def test_globalstoargumentstrans_unsupported_gocean_scalar(monkeypatch):
    ''' Check the GlobalsToArguments transformation when the global is
    a type not supported by the GOcean infrastructure raises an Error'''
    from psyclone.transformations import KernelGlobalsToArguments
    from psyclone.psyir.symbols import DataSymbol

    trans = KernelGlobalsToArguments()

    # Construct a testing InvokeSchedule
    _, invoke_info = parse(os.path.join(BASEPATH, "gocean1p0",
                                        "single_invoke_kern_with_use.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    kernel = invoke.schedule.coded_kernels()[0]

    # In this case we set it to be of type CHARACTER as that is not supported
    # in the GOcean infrastructure.
    def set_to_char(variable):
        variable._datatype = DataType.CHARACTER
    monkeypatch.setattr(DataSymbol, "resolve_deferred", set_to_char)

    # Test transforming a single kernel
    with pytest.raises(TypeError) as err:
        trans.apply(kernel)
    assert "The global variable 'rdt' could not be promoted to an argument " \
        "because the GOcean infrastructure does not have any scalar type " \
        "equivalent to the PSyIR DataType.CHARACTER type." in str(err.value)



def test_globalstoarguments_multiple_kernels():
    ''' Check the KernelGlobalsToArguments transformation with an invoke with
    three kernel calls, two of them duplicated and the third one sharing the
    same imported module'''
    from psyclone.transformations import KernelGlobalsToArguments
    from psyclone.psyir.backend.fortran import FortranWriter
    fwriter = FortranWriter()

    # Construct a testing InvokeSchedule
    _, invoke_info = parse(os.path.
                           join(BASEPATH, "gocean1p0",
                                "single_invoke_three_kernels_with_use.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    trans = KernelGlobalsToArguments()

    expected = [
        ["subroutine kernel_with_use_code(ji,jj,istep,ssha,tmask,rdt)",
         "real, intent(inout) :: rdt"],
        ["subroutine kernel_with_use2_code(ji,jj,istep,ssha,tmask,cbfr,rdt)",
         "real, intent(inout) :: cbfr\n  real, intent(inout) :: rdt"],
        ["subroutine kernel_with_use_code(ji,jj,istep,ssha,tmask,rdt)",
         "real, intent(inout) :: rdt"]]

    for num, kernel in enumerate(invoke.schedule.coded_kernels()):
        kschedule = kernel.get_kernel_schedule()

        # In this case we hardcode all globals to REAL to avoid searching and
        # importing of modules during this test.
        for var in kschedule.symbol_table.global_datasymbols:
            var._datatype = DataType.REAL

        trans.apply(kernel)

        # Check the kernel code is generated as expected
        kernel_code = fwriter(kschedule)
        assert expected[num][0] in kernel_code
        assert expected[num][1] in kernel_code

    generated_code = str(psy.gen)

    # The following assert checks that globals from the same module are
    # imported in a single use statement and that duplicated globals are
    # just imported once
    assert "SUBROUTINE invoke_0(oldu_fld, cu_fld)\n" \
           "      USE kernel_with_use2_mod, ONLY: kernel_with_use2_code\n" \
           "      USE kernel_with_use_mod, ONLY: kernel_with_use_code\n" \
           "      USE model_mod, ONLY: rdt, cbfr\n" \
           "      TYPE(r2d_field), intent(inout) :: cu_fld\n" in generated_code

    # Check the kernel calls have the global passed as last argument
    assert "CALL kernel_with_use_code(i, j, oldu_fld, cu_fld%data, " \
           "cu_fld%grid%tmask, rdt)" in generated_code
    assert "CALL kernel_with_use2_code(i, j, oldu_fld, cu_fld%data, " \
           "cu_fld%grid%tmask, cbfr, rdt)" in generated_code


def test_globalstoarguments_noglobals():
    ''' Check the KernelGlobalsToArguments transformation can be applied to
    a kernel that does not contain any global without any effect '''
    from psyclone.transformations import KernelGlobalsToArguments

    # Parse a file to get an initialised GOKernelsArguments object
    _, invoke_info = parse(os.path.join(BASEPATH, "gocean1p0",
                                        "single_invoke.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    kernel = invoke.schedule.coded_kernels()[0]
    trans = KernelGlobalsToArguments()
    before_code = str(psy.gen)
    trans.apply(kernel)
    after_code = str(psy.gen)
    assert before_code == after_code
    # TODO #11: When support for logging is added, we could warn the user that
    # no globals were found.


def test_globalstoargumentstrans_clash_symboltable(monkeypatch):
    ''' Check the GlobalsToArguments transformation with a symbol name clash
    produces the expected error.'''
    from psyclone.transformations import KernelGlobalsToArguments
    from psyclone.psyir.symbols import DataSymbol

    trans = KernelGlobalsToArguments()
    # Construct a testing InvokeSchedule
    _, invoke_info = parse(os.path.join(BASEPATH, "gocean1p0",
                                        "single_invoke_kern_with_use.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    kernel = invoke.schedule.coded_kernels()[0]

    # Monkeypatch resolve_deferred to avoid module searching and importing
    # in this test. In this case we assume it is a REAL
    def set_to_real(variable):
        variable._datatype = DataType.REAL
    monkeypatch.setattr(DataSymbol, "resolve_deferred", set_to_real)

    # Add 'rdt' into the symbol table
    kernel.root.symbol_table.add(DataSymbol("rdt", DataType.REAL))

    # Test transforming a single kernel
    with pytest.raises(KeyError) as err:
        trans.apply(kernel)
    assert "Couldn't copy 'rdt: <DataType.REAL, Scalar, Global(container=" \
        "'model_mod')>' into the SymbolTable. The name 'rdt' is already used" \
        " by another symbol." in str(err.value)

# FIXEM: Probably we don't need this test anymore
def test_globalstoargumentstrans_clash_namespace_after(monkeypatch):
    ''' Check the GlobalsToArguments transformation adds the module and global
    variable names into the NameSpaceManager to prevent clashes'''
    from psyclone.transformations import KernelGlobalsToArguments
    from psyclone.psyir.symbols import DataSymbol

    trans = KernelGlobalsToArguments()
    # Construct a testing InvokeSchedule
    _, invoke_info = parse(os.path.join(BASEPATH, "gocean1p0",
                                        "single_invoke_kern_with_use.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    kernel = invoke.schedule.coded_kernels()[0]

    # Monkeypatch resolve_deferred to avoid module searching and importing
    # in this test. In this case we assume it is a REAL
    def set_to_real(variable):
        variable._datatype = DataType.REAL
    monkeypatch.setattr(DataSymbol, "resolve_deferred", set_to_real)

    # Transform and generate the new code
    trans.apply(kernel)
    _ = str(psy.gen)

    
    # Create new 'rdt' and 'model_mod' variables using the NameSpaceManager
    # newname1 = invoke._name_space_manager.create_name(
    #             root_name="rdt", context="PSyVars", label="rdt")
    # newname2 = invoke._name_space_manager.create_name(
    #            root_name="model_mod", context="PSyVars", label="model_mod")

    # Created names should not clash with the imported module and globalvar
    # assert newname1 == "rdt_1"
    # assert newname2 == "model_mod_1"


# FIXEM: Probably we don't need this test anymore
def test_globalstoargumentstrans_clash_namespace_before(monkeypatch):
    ''' Check the GlobalsToArguments generation will break if the global
    variable name has already been used in the NameSpaceManager'''
    from psyclone.transformations import KernelGlobalsToArguments
    from psyclone.psyir.symbols import DataSymbol

    trans = KernelGlobalsToArguments()
    # Construct a testing InvokeSchedule
    _, invoke_info = parse(os.path.join(BASEPATH, "gocean1p0",
                                        "single_invoke_kern_with_use.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    kernel = invoke.schedule.coded_kernels()[0]

    # Monkeypatch resolve_deferred to avoid module searching and importing
    # in this test. In this case we assume it is a REAL
    def set_to_real(variable):
        variable._datatype = DataType.REAL
    monkeypatch.setattr(DataSymbol, "resolve_deferred", set_to_real)

    # Reserve 'rdt'
    # _ = invoke._name_space_manager.add_reserved_name("rdt")

    # Transform and generate the new code
    # trans.apply(kernel)
    # with pytest.raises(KeyError) as err:
    #     _ = str(psy.gen)
    # assert "The imported variable 'rdt' is already defined in the NameSpace" \
    #     "Manager of the Invoke." in str(err.value)
