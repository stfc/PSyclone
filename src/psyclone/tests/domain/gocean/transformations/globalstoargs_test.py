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
# ----------------------------------------------------------------------------
# Authors: A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified by R. W. Ford, STFC Daresbury Lab

''' Tests the KernelImportsToArguments Transformation for the GOcean
1.0 API.'''

import os
import pytest
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory, InvokeSchedule
from psyclone.psyir.symbols import DataSymbol, REAL_TYPE, INTEGER_TYPE, \
    CHARACTER_TYPE, Symbol
from psyclone.transformations import KernelImportsToArguments, \
    TransformationError

API = "gocean1.0"

# Each os.path.dirname() move up in the folder hierarchy
BASEPATH = os.path.join(os.path.dirname(os.path.dirname(os.path.dirname(
    os.path.dirname(os.path.abspath(__file__))))), "test_files")


def test_kernelimportstoargumentstrans_wrongapi():
    ''' Check the KernelImportsToArguments with an API other than GOcean1p0'''

    trans = KernelImportsToArguments()
    path = os.path.join(BASEPATH, "dynamo0p3")
    _, invoke_info = parse(os.path.join(path, "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    kernel = invoke.schedule.coded_kernels()[0]
    with pytest.raises(TransformationError) as err:
        trans.apply(kernel)
    assert "The KernelImportsToArguments transformation is currently only " \
           "supported for the GOcean API but got an InvokeSchedule of " \
           "type:" in str(err.value)


@pytest.mark.xfail(reason="#649 symbols declared in outer, module scope but "
                   "accessed inside kernel are not identified.")
def test_kernelimportsstoargumentstrans_no_outer_module_import():
    ''' Check that we reject kernels that access data that is declared in the
    enclosing module. '''
    trans = KernelImportsToArguments()
    path = os.path.join(BASEPATH, "gocean1p0")
    _, invoke_info = parse(os.path.join(path,
                                        "single_invoke_kern_with_global.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    kernel = invoke.schedule.coded_kernels()[0]
    with pytest.raises(TransformationError) as err:
        trans.apply(kernel)
    assert ("accesses a variable that is not declared in local scope" in
            str(err.value))


def test_kernelimportstoargumentstrans_no_wildcard_import():
    ''' Check that the transformation rejects kernels with wildcard
    imports. '''
    trans = KernelImportsToArguments()
    path = os.path.join(BASEPATH, "gocean1p0")
    _, invoke_info = parse(os.path.join(
        path, "single_invoke_kern_with_unqualified_use.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    kernel = invoke.schedule.coded_kernels()[0]
    with pytest.raises(TransformationError) as err:
        trans.apply(kernel)
    assert ("'kernel_with_use_code' has a wildcard import of symbols from "
            "container 'model_mod'" in str(err.value))


@pytest.mark.xfail(reason="Transformation does not set modified property "
                   "of kernel - #663")
@pytest.mark.usefixtures("kernel_outputdir")
def test_kernelimportstoargumentstrans(monkeypatch):
    ''' Check the KernelImportsToArguments transformation with a single kernel
    invoke and an imported variable.'''
    from psyclone.psyGen import Argument
    from psyclone.psyir.backend.fortran import FortranWriter

    trans = KernelImportsToArguments()
    assert trans.name == "KernelImportsToArguments"
    assert str(trans) == "Convert the imported variables used inside the " \
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

    # Monkeypatch resolve_type to avoid module searching and importing
    # in this test. In this case we assume it is a REAL
    def set_to_real(variable):
        variable._datatype = REAL_TYPE
    monkeypatch.setattr(DataSymbol, "resolve_type", set_to_real)

    # Test with invalid node
    with pytest.raises(TransformationError) as err:
        trans.apply(notkernel)
    assert ("The KernelImportsToArguments transformation can only be applied"
            " to CodedKern nodes but found 'GOLoop' instead."
            in str(err.value))

    # Test transforming a single kernel
    trans.apply(kernel)

    assert kernel.modified

    # The transformation;
    # 1) Has imported the symbol into the InvokeSchedule
    assert invoke.schedule.symbol_table.lookup("rdt")
    assert invoke.schedule.symbol_table.lookup("model_mod")
    var = invoke.schedule.symbol_table.lookup("rdt")
    container = invoke.schedule.symbol_table.lookup("model_mod")
    assert var.is_import
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


@pytest.mark.usefixtures("kernel_outputdir")
def test_kernelimportstoargumentstrans_constant(monkeypatch):
    ''' Check the KernelImportsToArguments transformation when the import is
    also a constant value, in this case the argument should be read-only.'''
    from psyclone.psyir.backend.fortran import FortranWriter
    from psyclone.psyir.nodes import Literal

    trans = KernelImportsToArguments()

    # Construct a testing InvokeSchedule
    _, invoke_info = parse(os.path.join(BASEPATH, "gocean1p0",
                                        "single_invoke_kern_with_use.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    kernel = invoke.schedule.coded_kernels()[0]

    # Monkeypatch resolve_type to avoid module searching and importing
    # in this test. In this case we assume it is a constant INTEGER
    def create_data_symbol(arg):
        symbol = DataSymbol(arg.name, INTEGER_TYPE,
                            interface=arg.interface,
                            is_constant=True,
                            initial_value=Literal("1", INTEGER_TYPE))
        return symbol

    monkeypatch.setattr(DataSymbol, "resolve_type", create_data_symbol)
    monkeypatch.setattr(Symbol, "resolve_type", create_data_symbol)

    # Test transforming a single kernel
    trans.apply(kernel)

    fwriter = FortranWriter()
    kernel_code = fwriter(kernel.get_kernel_schedule())

    assert "subroutine kernel_with_use_code(ji, jj, istep, ssha, tmask, rdt)" \
        in kernel_code
    assert "integer, intent(in) :: rdt" in kernel_code


def test_kernelimportstoargumentstrans_unsupported_gocean_scalar(monkeypatch):
    ''' Check the KernelImportsToArguments transformation when the import is
    a type not supported by the GOcean infrastructure raises an Error'''

    trans = KernelImportsToArguments()

    # Construct a testing InvokeSchedule
    _, invoke_info = parse(os.path.join(BASEPATH, "gocean1p0",
                                        "single_invoke_kern_with_use.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    kernel = invoke.schedule.coded_kernels()[0]

    # In this case we set it to be of type CHARACTER as that is not supported
    # in the GOcean infrastructure.
    def create_data_symbol(arg):
        symbol = DataSymbol(arg.name, CHARACTER_TYPE,
                            interface=arg.interface)
        return symbol
    monkeypatch.setattr(Symbol, "resolve_type", create_data_symbol)

    # Test transforming a single kernel
    with pytest.raises(TypeError) as err:
        trans.apply(kernel)
    assert ("The imported variable 'rdt' could not be promoted to an argument "
            "because the GOcean infrastructure does not have any scalar type "
            "equivalent to the PSyIR Scalar<CHARACTER, UNDEFINED> type."
            in str(err.value))


@pytest.mark.usefixtures("kernel_outputdir")
def test_kernelimportstoarguments_multiple_kernels(monkeypatch):
    ''' Check the KernelImportsToArguments transformation with an invoke with
    three kernel calls, two of them duplicated and the third one sharing the
    same imported module'''
    from psyclone.psyir.backend.fortran import FortranWriter
    fwriter = FortranWriter()

    # Construct a testing InvokeSchedule
    _, invoke_info = parse(os.path.
                           join(BASEPATH, "gocean1p0",
                                "single_invoke_three_kernels_with_use.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    trans = KernelImportsToArguments()

    # The kernels are checked before the psy.gen, so they don't include the
    # modified suffix.
    expected = [
        ["subroutine kernel_with_use_code(ji, jj, istep, ssha, tmask, rdt)",
         "real, intent(inout) :: rdt"],
        ["subroutine kernel_with_use2_code(ji, jj, istep, ssha, tmask, cbfr,"
         " rdt)",
         "real, intent(inout) :: cbfr\n  real, intent(inout) :: rdt"],
        ["subroutine kernel_with_use_code(ji, jj, istep, ssha, tmask, rdt)",
         "real, intent(inout) :: rdt"]]

    # Monkeypatch the resolve_type() methods to avoid searching and
    # importing of module during this test.
    def create_data_symbol(arg):
        symbol = DataSymbol(arg.name, REAL_TYPE,
                            interface=arg.interface)
        return symbol
    monkeypatch.setattr(Symbol, "resolve_type", create_data_symbol)
    monkeypatch.setattr(DataSymbol, "resolve_type", create_data_symbol)

    for num, kernel in enumerate(invoke.schedule.coded_kernels()):
        kschedule = kernel.get_kernel_schedule()

        trans.apply(kernel)

        # Check the kernel code is generated as expected
        kernel_code = fwriter(kschedule)
        assert expected[num][0] in kernel_code
        assert expected[num][1] in kernel_code

    generated_code = str(psy.gen)

    # The following assert checks that imports from the same module are
    # imported, since the kernels are marked as modified, new suffixes are
    # given in order to differentiate each of them.
    assert ("USE kernel_with_use_1_mod, ONLY: kernel_with_use_1_code\n"
            in generated_code)
    assert ("USE kernel_with_use2_0_mod, ONLY: kernel_with_use2_0_code\n"
            in generated_code)
    assert ("USE kernel_with_use_0_mod, ONLY: kernel_with_use_0_code\n"
            in generated_code)

    # Check the kernel calls have the imported symbol passed as last argument
    assert "CALL kernel_with_use_0_code(i, j, oldu_fld, cu_fld%data, " \
           "cu_fld%grid%tmask, rdt)" in generated_code
    assert "CALL kernel_with_use_1_code(i, j, oldu_fld, cu_fld%data, " \
           "cu_fld%grid%tmask, rdt)" in generated_code
    assert "CALL kernel_with_use2_0_code(i, j, oldu_fld, cu_fld%data, " \
           "cu_fld%grid%tmask, cbfr, rdt)" in generated_code


@pytest.mark.usefixtures("kernel_outputdir")
def test_kernelimportstoarguments_noimports(fortran_writer):
    ''' Check the KernelImportsToArguments transformation can be applied to
    a kernel that does not contain any import without any effect '''

    # Parse a file to get an initialised GOKernelsArguments object
    _, invoke_info = parse(os.path.join(BASEPATH, "gocean1p0",
                                        "single_invoke.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    kernel = invoke.schedule.coded_kernels()[0]

    before_code = fortran_writer(psy.container)
    trans = KernelImportsToArguments()
    trans.apply(kernel)
    after_code = fortran_writer(psy.container)

    # Check that both are the same
    assert before_code == after_code
    # TODO #11: When support for logging is added, we could warn the user that
    # no imports were found.


def test_kernelimportstoargumentstrans_clash_symboltable(monkeypatch):
    ''' Check the KernelImportsToArguments transformation with a symbol name
    clash produces the expected error.'''

    trans = KernelImportsToArguments()
    # Construct a testing InvokeSchedule
    _, invoke_info = parse(os.path.join(BASEPATH, "gocean1p0",
                                        "single_invoke_kern_with_use.f90"),
                           api=API)
    psy = PSyFactory(API).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    kernel = invoke.schedule.coded_kernels()[0]

    # Monkeypatch Symbol.resolve_type to avoid module searching and
    # importing in this test. In this case we assume the symbol is a
    # DataSymbol of REAL type.
    def create_real(variable):
        return DataSymbol(variable.name, REAL_TYPE,
                          interface=variable.interface)
    monkeypatch.setattr(Symbol, "resolve_type", create_real)

    # Add 'rdt' into the symbol table
    kernel.ancestor(InvokeSchedule).symbol_table.add(
        DataSymbol("rdt", REAL_TYPE))

    # Test transforming a single kernel
    with pytest.raises(KeyError) as err:
        trans.apply(kernel)
    assert ("Couldn't copy 'rdt: DataSymbol<Scalar<REAL, UNDEFINED>, "
            "Import(container='model_mod')>' into the SymbolTable. The name "
            "'rdt' is already used by another symbol." in str(err.value))
