# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2025, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Lab
# Modified by: R. W. Ford, STFC Daresbury Lab
#              I. Kavcic, Met Office
#              S. Siso, STFC Daresbury Lab
#              J. Henrichs, Bureau of Meteorology

''' Module containing tests for kernel transformations. '''

import os
import re
import pytest

from psyclone.configuration import Config
from psyclone.domain.lfric.lfric_builtins import LFRicBuiltIn
from psyclone.generator import GenerationError
from psyclone.psyGen import Kern
from psyclone.psyir.nodes import Routine, FileContainer, IntrinsicCall, Call
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE
from psyclone.psyir.transformations import TransformationError
from psyclone.transformations import (
    ACCRoutineTrans, OMPDeclareTargetTrans, LFRicKernelConstTrans)

from psyclone.tests.gocean_build import GOceanBuild
from psyclone.tests.lfric_build import LFRicBuild
from psyclone.tests.utilities import get_invoke


def setup_module():
    '''
    This setup routine ensures that any pre-exisiting Config object is
    wiped when this module is first entered and the teardown function below
    guarantees it for subsequent tests.  (Necessary when running tests in
    parallel.)
    '''
    Config._instance = None


def teardown_function():
    ''' This function is called automatically after every test in this
    file. It ensures that any existing configuration object is deleted. '''
    Config._instance = None


def test_new_kernel_file(kernel_outputdir, monkeypatch, fortran_reader):
    ''' Check that we write out the transformed kernel to the CWD. '''
    # Ensure kernel-output directory is uninitialised
    config = Config.get()
    monkeypatch.setattr(config, "_kernel_naming", "multiple")
    psy, invoke = get_invoke("nemolite2d_alg_mod.f90", api="gocean", idx=0)
    sched = invoke.schedule
    kern = sched.coded_kernels()[0]
    rtrans = ACCRoutineTrans()
    rtrans.apply(kern)
    # Generate the code (this triggers the generation of a new kernel)
    code = str(psy.gen).lower()
    # Work out the value of the tag used to re-name the kernel
    tag = re.search('use continuity(.+?)_mod', code).group(1)
    assert f"use continuity{tag}_mod, only : continuity{tag}_code" in code
    assert f"call continuity{tag}_code(" in code
    # The kernel and module name should have gained the tag just identified
    # and be written to the CWD
    filename = os.path.join(str(kernel_outputdir), f"continuity{tag}_mod.f90")
    assert os.path.isfile(filename)
    # Parse the new kernel file
    psyir = fortran_reader.psyir_from_file(filename)
    # Check that the module has the right name
    assert isinstance(psyir, FileContainer)
    module = psyir.children[0]
    assert module.name == f"continuity{tag}_mod"

    # Check that the subroutine has the right name
    for sub in psyir.walk(Routine):
        if sub.name == f"continuity{tag}_code":
            break
    else:
        assert False, f"Failed to find subroutine named continuity{tag}_code"

    # If compilation fails this will raise an exception
    GOceanBuild(kernel_outputdir).compile_file(filename)


def test_new_kernel_dir(kernel_outputdir):
    ''' Check that we write out the transformed kernel to a specified
    directory. '''
    psy, invoke = get_invoke("nemolite2d_alg_mod.f90", api="gocean", idx=0)
    sched = invoke.schedule
    kern = sched.coded_kernels()[0]
    rtrans = ACCRoutineTrans()
    rtrans.apply(kern)
    # Generate the code (this triggers the generation of a new kernel)
    _ = str(psy.gen)
    file_list = os.listdir(str(kernel_outputdir))
    assert len(file_list) == 1
    assert file_list[0] == 'continuity_0_mod.f90'


def test_new_kern_no_clobber(kernel_outputdir, monkeypatch):
    ''' Check that we create a new kernel with a new name when kernel-naming
    is set to 'multiple' and we would otherwise get a name clash. '''
    # Ensure kernel-output directory is uninitialised
    config = Config.get()
    monkeypatch.setattr(config, "_kernel_naming", "multiple")
    psy, invoke = get_invoke("1_single_invoke.f90", api="lfric", idx=0)
    sched = invoke.schedule
    kernels = sched.walk(Kern)
    kern = kernels[0]
    old_mod_name = kern.module_name[:].lower()
    if old_mod_name.endswith("_mod"):
        old_mod_name = old_mod_name[:-4]
    # Create a file with the same name as we would otherwise generate
    with open(os.path.join(str(kernel_outputdir),
                           old_mod_name+"_0_mod.f90"),
              "w", encoding="utf-8") as ffile:
        ffile.write("some code")
    rtrans = ACCRoutineTrans()
    rtrans.apply(kern)
    # Generate the code (this triggers the generation of a new kernel)
    _ = str(psy.gen).lower()
    filename = os.path.join(str(kernel_outputdir), old_mod_name+"_1_mod.f90")
    assert os.path.isfile(filename)


@pytest.mark.parametrize(
    "mod_name,sub_name",
    [("testkern_mod", "testkern"),
     ("testkern", "testkern_code"),
     ("testkern1_mod", "testkern2_code")])
def test_kernel_module_name(kernel_outputdir, mod_name, sub_name, monkeypatch):
    '''Check that there is no limitation on kernel and module names. In
    particular check that the names do not have to conform to the
    <name>_mod, <name>_code convention.

    '''
    # Argument kernel_outputdir is needed to capture the files created by
    # the rename_and_write() call
    # pylint: disable=unused-argument
    _, invoke = get_invoke("1_single_invoke.f90", api="lfric", idx=0)
    sched = invoke.schedule
    kernels = sched.coded_kernels()
    kern = kernels[0]
    ktrans = LFRicKernelConstTrans()
    ktrans.apply(kern, {"number_of_layers": 100})
    # Modify the kernel module and subroutine names.
    monkeypatch.setattr(kern, "_module_name", mod_name)
    monkeypatch.setattr(kern, "_name", sub_name)
    # Generate the code - no exception should be raised when the names
    # do not conform to the <name>_mod, >name>_code convention.
    kern.rename_and_write()


@pytest.mark.parametrize(
    "mod_name,sub_name",
    [("testkern_mod", "testkern_code"),
     ("testkern_MOD", "testkern_CODE"),
     ("TESTKERN_mod", "testkern_code"),
     ("testkern_mod", "TESTKERN_code"),
     ("TESTKERN_MoD", "TESTKERN_CoDe")])
def test_kern_case_insensitive(mod_name, sub_name, kernel_outputdir,
                               monkeypatch):
    '''Check that the test to see if a kernel conforms to the <name>_mod,
    <name>_code convention is case insensitive. This check also tests that the
    removal of _mod to create part of the output filename is case
    insensitive.

    '''
    _, invoke = get_invoke("1_single_invoke.f90", api="lfric", idx=0)
    sched = invoke.schedule
    kernels = sched.walk(Kern)
    kern = kernels[0]
    ktrans = LFRicKernelConstTrans()
    ktrans.apply(kern, {"number_of_layers": 100})
    monkeypatch.setattr(kern, "_module_name", mod_name)
    monkeypatch.setattr(kern, "_name", sub_name)
    # Generate the code - this should not raise an exception.
    kern.rename_and_write()
    filename = os.path.join(str(kernel_outputdir), mod_name[:8]+"_0_mod.f90")
    assert os.path.isfile(filename)


def test_new_kern_single_error(kernel_outputdir, monkeypatch):
    ''' Check that we do not overwrite an existing, different kernel if
    there is a name clash and kernel-naming is 'single'. '''
    # Ensure kernel-output directory is uninitialised
    config = Config.get()
    monkeypatch.setattr(config, "_kernel_naming", "single")
    _, invoke = get_invoke("1_single_invoke.f90", api="lfric", idx=0)
    sched = invoke.schedule
    kernels = sched.coded_kernels()
    kern = kernels[0]
    old_mod_name = kern.module_name[:].lower()
    if old_mod_name.endswith("_mod"):
        old_mod_name = old_mod_name[:-4]
    # Create a file with the same name as we would otherwise generate
    with open(os.path.join(str(kernel_outputdir),
                           old_mod_name+"_0_mod.f90"),
              "w", encoding="utf-8") as ffile:
        ffile.write("some code")
    rtrans = ACCRoutineTrans()
    rtrans.apply(kern)
    # Generate the code - this should raise an error as we get a name
    # clash and the content of the existing file is not the same as that
    # which we would generate
    with pytest.raises(GenerationError) as err:
        kern.rename_and_write()
    assert (f"transformed version of this Kernel 'testkern_0_mod.f90' already "
            f"exists in the kernel-output directory ({kernel_outputdir}) "
            f"but is not the same as the current, transformed kernel and the "
            f"kernel-renaming scheme is set to 'single'" in str(err.value))


def test_new_same_kern_single(kernel_outputdir, monkeypatch):
    ''' Check that we do not overwrite an existing, identical kernel if
    there is a name clash and kernel-naming is 'single'. '''
    # Ensure kernel-output directory is uninitialised
    config = Config.get()
    monkeypatch.setattr(config, "_kernel_naming", "single")
    rtrans = ACCRoutineTrans()
    _, invoke = get_invoke("4_multikernel_invokes.f90", api="lfric",
                           idx=0)
    sched = invoke.schedule
    # Apply the same transformation to both kernels. This should produce
    # two, identical transformed kernels.
    new_kernels = []
    for kern in sched.coded_kernels():
        rtrans.apply(kern)
        new_kernels.append(kern)

    # Generate the code - we should end up with just one transformed kernel
    new_kernels[0].rename_and_write()
    new_kernels[1].rename_and_write()
    assert new_kernels[1]._name == "testkern_0_code"
    assert new_kernels[1].module_name == "testkern_0_mod"
    out_files = os.listdir(str(kernel_outputdir))
    assert out_files == [new_kernels[1].module_name+".f90"]


def test_transform_kern_with_interface(kernel_outputdir):
    '''
    Test that we can transform a polymorphic kernel - i.e. one where
    there is more than one subroutine implementation in order to support
    different precisions.

    '''
    rtrans = ACCRoutineTrans()
    psy, invoke = get_invoke("26.8_mixed_precision_args.f90",
                             api="lfric", idx=0)
    sched = invoke.schedule
    kernels = sched.coded_kernels()
    # Have to use 'force' because the test kernel contains a WRITE which
    # becomes a CodeBlock.
    rtrans.apply(kernels[0], options={"force": True})
    kernels[0].rename_and_write()
    out_files = os.listdir(str(kernel_outputdir))
    filename = os.path.join(str(kernel_outputdir), out_files[0])
    assert os.path.isfile(filename)
    with open(filename,
              "r", encoding="utf-8") as ffile:
        contents = ffile.read()
    # Check that the interface name has been updated.
    assert "interface mixed_0_code" in contents
    assert ("module procedure :: mixed_code_32, mixed_code_64"
            in contents)
    # Check that the subroutines themselves haven't been renamed.
    assert "subroutine mixed_code_32" in contents
    assert "subroutine mixed_code_64" in contents
    # But they have been transformed.
    assert ('''real*4, dimension(op_ncell_3d,ndf_w0,ndf_w0), intent(in) :: op

    !$acc routine seq''' in contents)
    assert ('''real*8, dimension(op_ncell_3d,ndf_w0,ndf_w0), intent(in) :: op

    !$acc routine seq''' in contents)
    assert LFRicBuild(kernel_outputdir).code_compiles(psy)
    kernels = sched.coded_kernels()
    rtrans.apply(kernels[1], options={"force": True})
    assert LFRicBuild(kernel_outputdir).code_compiles(psy)


# The following tests test the MarkRoutineForGPUMixin validation, for this
# it uses the ACCRoutineTrans as instance of this Mixin.

def test_gpumixin_validate_wrong_node_type():
    '''
    Test that the MarkRoutineForGPUMixin.validate_it_can_run_on_gpu() method
    rejects a node of the wrong type.

    '''
    rtrans = ACCRoutineTrans()
    with pytest.raises(TransformationError) as err:
        rtrans.apply(FileContainer("fred"))
    assert ("The ACCRoutineTrans must be applied to a sub-class of Kern or "
            "Routine but got 'FileContainer'" in str(err.value))


def test_gpumixin_validate_no_schedule(monkeypatch):
    '''
    Test that the MarkRoutineForGPUMixin.validate_it_can_run_on_gpu() method
    catches any errors generated when attempting to get the PSyIR of a kernel.

    '''
    _, invoke = get_invoke("1_single_invoke.f90", api="lfric", idx=0)
    sched = invoke.schedule
    kernels = sched.walk(Kern)
    kern = kernels[0]
    # We monkeypatch the 'get_callees' method of LFRicKern so that it
    # just raises an exception.

    def broken(_1_):
        raise GenerationError("this is just a test")
    monkeypatch.setattr(kern, "get_callees", broken)

    rtrans = ACCRoutineTrans()
    with pytest.raises(TransformationError) as err:
        rtrans.validate(kern)
    assert ("Failed to create PSyIR for kernel 'testkern_code'. Cannot "
            "transform such a kernel." in str(err.value))


def test_gpumixin_validate_no_import(fortran_reader):
    '''
    Test the MarkRoutineForGPUMixin.validate_it_can_run_on_gpu() method
    rejects a kernel that accesses imported data unless that data is known to
    be a compile-time constant.

    '''
    code = '''\
module my_mod
  use other_mod, only: some_data
contains
  subroutine my_sub(arg)
    integer :: arg
    arg = arg + some_data
  end subroutine my_sub
end module my_mod'''
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    rtrans = ACCRoutineTrans()
    with pytest.raises(TransformationError) as err:
        rtrans.validate(routine)
    assert ("Transformation Error: routine 'my_sub' accesses the symbol "
            "'some_data: Symbol<Import(container='other_mod')>' which is "
            "imported. If this symbol represents data "
            "then it must first be converted to a routine argument using the "
            "KernelImportsToArguments transformation." in str(err.value))
    # Specialise the imported symbol and make it constant.
    sym = psyir.children[0].symbol_table.lookup("some_data")
    sym.specialise(DataSymbol, datatype=INTEGER_TYPE, is_constant=True)
    # Validation should now pass.
    rtrans.validate(routine)


def test_gpumixin_validate_no_cblock(fortran_reader):
    '''
    Test the MarkRoutineForGPUMixin.validate_it_can_run_on_gpu() method rejects
    a kernel that contains a CodeBlock.

    '''
    code = '''\
module my_mod
  integer :: some_data
contains
  subroutine my_sub(arg)
    integer :: arg
    write(*,*) arg + some_data
  end subroutine my_sub
end module my_mod'''
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.walk(Routine)[0]
    rtrans = ACCRoutineTrans()
    with pytest.raises(TransformationError) as err:
        rtrans.validate(routine)
    assert ("Transformation Error: Cannot safely apply ACCRoutineTrans to "
            "routine 'my_sub' because its PSyIR contains one or more "
            "CodeBlocks:\n"
            "  WRITE(*, *)" in str(err.value))
    assert ("You may use 'options={'force': True}' to override this check."
            in str(err.value))
    # Using 'force' will override the check.
    rtrans.validate(routine, options={'force': True})
    # However, if the CodeBlock contains a problematic data access then
    # that is still picked up.
    new_code = code.replace("integer :: some_data",
                            "use some_mod, only: some_data")
    psyir = fortran_reader.psyir_from_source(new_code)
    routine = psyir.walk(Routine)[0]
    with pytest.raises(TransformationError) as err:
        rtrans.validate(routine, options={'force': True})
    assert ("Transformation Error: routine 'my_sub' accesses the symbol "
            "'some_data: Symbol<Import" in str(err.value))


def test_gpumixin_validate_no_call():
    '''
    Test the MarkRoutineForGPUMixin.validate_it_can_run_on_gpu() method rejects
    a kernel that calls another routine.

    '''
    psy, invoke = get_invoke("1.15_invoke_kern_with_call.f90", api="lfric",
                             idx=0)
    sched = invoke.schedule
    kernel = sched.coded_kernels()[0]
    rtrans = ACCRoutineTrans()
    with pytest.raises(TransformationError) as err:
        rtrans.validate(kernel)
    assert ("Kernel 'testkern_with_call_code' calls another routine "
            "'call xyz2llr(coord(1), coord(2), coord(3), lon, lat, radius)' "
            "which is not available on the accelerator device and therefore "
            "cannot have ACCRoutineTrans applied to it"
            in str(err.value))

    # The same error happens for unsupported GPU intrinsics
    kschedules = kernel.get_callees()
    call = kschedules[0].walk(Call)[0]
    call.replace_with(
        IntrinsicCall.create(IntrinsicCall.Intrinsic.GET_COMMAND))
    with pytest.raises(TransformationError) as err:
        rtrans.validate(kernel)
    assert ("Kernel 'testkern_with_call_code' calls intrinsic 'GET_COMMAND' "
            "which is not available on the default accelerator device. Use "
            "the 'device_string' option to specify a different device."
            in str(err.value))


@pytest.mark.parametrize(
    "rtrans, expected_directive",
    [(ACCRoutineTrans(), "!$acc routine"),
     (OMPDeclareTargetTrans(), "!$omp declare target")])
def test_kernel_gpu_annotation_trans(rtrans, expected_directive,
                                     fortran_writer):
    ''' Check that the GPU annotation transformations insert the
    proper directive inside PSyKAl kernel code '''
    _, invoke = get_invoke("1_single_invoke.f90", api="lfric", idx=0)
    sched = invoke.schedule
    kern = sched.coded_kernels()[0]
    rtrans.apply(kern)

    # Check that the directive has been added to the kernel code
    kschedules = kern.get_callees()
    code = fortran_writer(kschedules[0])
    assert expected_directive in code


@pytest.mark.parametrize(
    "rtrans",
    [ACCRoutineTrans(), OMPDeclareTargetTrans()])
def test_kernel_gpu_annotation_device_id(rtrans, fortran_reader):
    ''' Check that the GPU annotation transformations validations
    check the intrinsics using the provided device id. '''

    code = '''
    function myfunc(a)
        integer :: a
        real :: myfunc
        myfunc = REAL(a)
    end function
    '''
    psyir = fortran_reader.psyir_from_source(code)
    routine = psyir.children[0]
    # The routine is valid
    rtrans.validate(routine)
    # But not if we are targeting "nvidia-repr" or an invalid device
    with pytest.raises(TransformationError) as err:
        rtrans.validate(routine, options={'device_string':
                                          'nvfortran-uniform'})
    assert ("routine 'myfunc' calls intrinsic 'REAL' which is not available on"
            " the 'nvfortran-uniform' accelerator device. Use the "
            "'device_string' option to specify a different device."
            in str(err.value))
    with pytest.raises(ValueError) as err:
        rtrans.validate(routine, options={'device_string':
                                          'unknown-device'})
    assert ("Unsupported device_string value 'unknown-device', the supported "
            "values are '' (default), 'nvfortran-all', 'nvfortran-uniform'"
            in str(err.value))


def test_1kern_trans(kernel_outputdir):
    ''' Check that we generate the correct code when an invoke contains
    the same kernel more than once but only one of them is transformed. '''
    psy, invoke = get_invoke("4_multikernel_invokes.f90", api="lfric",
                             idx=0)
    sched = invoke.schedule
    kernels = sched.coded_kernels()
    # We will transform the second kernel but not the first
    kern = kernels[1]
    rtrans = ACCRoutineTrans()
    rtrans.apply(kern)
    # Generate the code (this triggers the generation of a new kernel)
    code = str(psy.gen).lower()
    tag = re.search('use testkern(.+?)_mod', code).group(1)
    # We should have a USE for the original kernel and a USE for the new one
    assert f"use testkern{tag}_mod, only : testkern{tag}_code" in code
    assert "use testkern_mod, only : testkern_code" in code
    # Similarly, we should have calls to both the original and new kernels
    assert "call testkern_code(" in code
    assert f"call testkern{tag}_code(" in code
    first = code.find("call testkern_code(")
    second = code.find(f"call testkern{tag}_code(")
    assert first < second
    assert LFRicBuild(kernel_outputdir).code_compiles(psy)


def test_2kern_trans(kernel_outputdir):
    ''' Check that we generate correct code when we transform two kernels
    within a single invoke. '''
    psy, invoke = get_invoke("4.5.2_multikernel_invokes.f90", api="lfric",
                             idx=0)
    sched = invoke.schedule
    kernels = sched.walk(Kern)
    assert len(kernels) == 5
    ktrans = LFRicKernelConstTrans()
    ktrans.apply(kernels[1], {"number_of_layers": 100})
    ktrans.apply(kernels[2], {"number_of_layers": 100})
    # Generate the code (this triggers the generation of new kernels)
    code = str(psy.gen).lower()
    # Find the tags added to the kernel/module names
    for match in re.finditer('use testkern_any_space_2(.+?)_mod', code):
        tag = match.group(1)
        assert (f"use testkern_any_space_2{tag}_mod, only : "
                f"testkern_any_space_2{tag}_code" in code)
        assert f"call testkern_any_space_2{tag}_code(" in code
        filepath = os.path.join(str(kernel_outputdir),
                                f"testkern_any_space_2{tag}_mod.f90")
        assert os.path.isfile(filepath)
        with open(filepath, encoding="utf-8") as infile:
            assert "nlayers = 100" in infile.read()
    assert "use testkern_any_space_2_mod, only" not in code
    assert "call testkern_any_space_2_code(" not in code
    assert LFRicBuild(kernel_outputdir).code_compiles(psy)


def test_gpumixin_builtin_no_trans():
    ''' Check that we reject attempts to transform built-in kernels. '''
    _, invoke = get_invoke("15.1.1_X_plus_Y_builtin.f90",
                           api="lfric", idx=0)
    sched = invoke.schedule
    kernels = sched.walk(LFRicBuiltIn)
    rtrans = ACCRoutineTrans()
    with pytest.raises(TransformationError) as err:
        rtrans.apply(kernels[0])
    assert ("ACCRoutineTrans to a built-in kernel is not yet supported and "
            "kernel 'x_plus_y' is of type " in str(err.value))
