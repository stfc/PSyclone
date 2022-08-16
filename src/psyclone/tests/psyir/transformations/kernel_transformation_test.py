# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2022, Science and Technology Facilities Council.
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

''' Module containing tests for kernel transformations. '''

from __future__ import absolute_import, print_function
import os
import re
import pytest

from psyclone.configuration import Config
from psyclone.domain.lfric.lfric_builtins import LFRicBuiltIn
from psyclone.generator import GenerationError
from psyclone.psyGen import Kern
from psyclone.psyir.nodes import Routine, FileContainer
from psyclone.psyir.symbols import SymbolError
from psyclone.psyir.transformations import TransformationError
from psyclone.transformations import ACCRoutineTrans, \
    Dynamo0p3KernelConstTrans, KernelModuleInlineTrans

from psyclone.tests.gocean_build import GOceanBuild
from psyclone.tests.lfric_build import LFRicBuild
from psyclone.tests.utilities import get_invoke


def setup_module():
    '''
    This setup routine ensures that and pre-exisiting Config object is
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
    psy, invoke = get_invoke("nemolite2d_alg_mod.f90", api="gocean1.0", idx=0)
    sched = invoke.schedule
    kern = sched.coded_kernels()[0]
    rtrans = ACCRoutineTrans()
    rtrans.apply(kern)
    # Generate the code (this triggers the generation of a new kernel)
    code = str(psy.gen).lower()
    # Work out the value of the tag used to re-name the kernel
    tag = re.search('use continuity(.+?)_mod', code).group(1)
    assert f"use continuity{tag}_mod, only: continuity{tag}_code" in code
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
    found = False
    for sub in psyir.walk(Routine):
        if sub.name == f"continuity{tag}_code":
            found = True
            break
    assert found

    # If compilation fails this will raise an exception
    GOceanBuild(kernel_outputdir).compile_file(filename)


def test_new_kernel_dir(kernel_outputdir):
    ''' Check that we write out the transformed kernel to a specified
    directory. '''
    psy, invoke = get_invoke("nemolite2d_alg_mod.f90", api="gocean1.0", idx=0)
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
    psy, invoke = get_invoke("1_single_invoke.f90", api="dynamo0.3", idx=0)
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
def test_kernel_module_name(mod_name, sub_name, monkeypatch):
    '''Check that there is no limitation on kernel and module names. In
    particular check that the names do not have to conform to the
    <name>_mod, <name>_code convention.

    '''
    _, invoke = get_invoke("1_single_invoke.f90", api="dynamo0.3", idx=0)
    sched = invoke.schedule
    kernels = sched.coded_kernels()
    kern = kernels[0]
    ktrans = Dynamo0p3KernelConstTrans()
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
    _, invoke = get_invoke("1_single_invoke.f90", api="dynamo0.3", idx=0)
    sched = invoke.schedule
    kernels = sched.walk(Kern)
    kern = kernels[0]
    ktrans = Dynamo0p3KernelConstTrans()
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
    _, invoke = get_invoke("1_single_invoke.f90", api="dynamo0.3", idx=0)
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
    _, invoke = get_invoke("4_multikernel_invokes.f90", api="dynamo0.3",
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


def test_1kern_trans(kernel_outputdir):
    ''' Check that we generate the correct code when an invoke contains
    the same kernel more than once but only one of them is transformed. '''
    psy, invoke = get_invoke("4_multikernel_invokes.f90", api="dynamo0.3",
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
    assert f"use testkern{tag}_mod, only: testkern{tag}_code" in code
    assert "use testkern_mod, only: testkern_code" in code
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
    psy, invoke = get_invoke("4.5.2_multikernel_invokes.f90", api="dynamo0.3",
                             idx=0)
    sched = invoke.schedule
    kernels = sched.walk(Kern)
    assert len(kernels) == 5
    ktrans = Dynamo0p3KernelConstTrans()
    ktrans.apply(kernels[1], {"number_of_layers": 100})
    ktrans.apply(kernels[2], {"number_of_layers": 100})
    # Generate the code (this triggers the generation of new kernels)
    code = str(psy.gen).lower()
    # Find the tags added to the kernel/module names
    for match in re.finditer('use testkern_any_space_2(.+?)_mod', code):
        tag = match.group(1)
        assert (f"use testkern_any_space_2{tag}_mod, only: "
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


def test_builtin_no_trans():
    ''' Check that we reject attempts to transform built-in kernels. '''
    _, invoke = get_invoke("15.1.1_X_plus_Y_builtin.f90",
                           api="dynamo0.3", idx=0)
    sched = invoke.schedule
    kernels = sched.walk(LFRicBuiltIn)
    rtrans = ACCRoutineTrans()
    with pytest.raises(TransformationError) as err:
        rtrans.apply(kernels[0])
    assert ("ACCRoutineTrans to a built-in kernel is not yet supported and "
            "kernel 'x_plus_y' is of type " in str(err.value))


def test_no_inline_global_var():
    ''' Check that we refuse to in-line a kernel that accesses a global
    variable. '''
    inline_trans = KernelModuleInlineTrans()
    _, invoke = get_invoke("single_invoke_kern_with_global.f90",
                           api="gocean1.0", idx=0)
    sched = invoke.schedule
    kernels = sched.walk(Kern)
    with pytest.raises(TransformationError) as err:
        inline_trans.apply(kernels[0])
    assert ("'kernel_with_global_code' contains accesses to data (variable "
            "'alpha') that are not present in the Symbol Table(s) "
            "within KernelSchedule scope." in str(err.value))


# Class KernelTrans

# Method validate

def test_kernel_trans_validate(monkeypatch):
    '''Check that the validate method in the class KernelTrans raises an
    exception if the reference is not found in any of the symbol
    tables. KernelTrans can't be instantiated as it is abstract so use
    a the subclass.

    '''
    kernel_trans = KernelModuleInlineTrans()
    _, invoke = get_invoke("single_invoke_kern_with_global.f90",
                           api="gocean1.0", idx=0)
    sched = invoke.schedule
    kernels = sched.walk(Kern)
    kernel = kernels[0]

    def raise_symbol_error():
        '''Simple function that raises SymbolError.'''
        raise SymbolError("error")
    monkeypatch.setattr(kernel, "get_kernel_schedule", raise_symbol_error)
    with pytest.raises(TransformationError) as err:
        kernel_trans.apply(kernel)
    assert ("'kernel_with_global_code' contains accesses to data that are "
            "not present in the Symbol Table(s). Cannot transform such a "
            "kernel." in str(err.value))

    def raise_gen_error():
        '''Simple function that raises GenerationError.'''
        raise GenerationError("error")
    monkeypatch.setattr(kernel, "get_kernel_schedule", raise_gen_error)
    with pytest.raises(TransformationError) as err:
        kernel_trans.apply(kernel)
    assert ("Failed to create PSyIR for kernel 'kernel_with_global_code'. "
            "Cannot transform such a kernel." in str(err.value))
