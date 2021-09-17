# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018-2021, Science and Technology Facilities Council.
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
# Modified by: R. W. Ford, STFC Daresbury Lab,
#              I. Kavcic, Met Office.

''' Module containing tests for kernel transformations. '''

from __future__ import absolute_import, print_function
import os
import re
import pytest

from fparser.two.utils import walk
from psyclone.psyir.transformations import TransformationError
from psyclone.transformations import ACCRoutineTrans, \
    Dynamo0p3KernelConstTrans
from psyclone.psyGen import Kern
from psyclone.generator import GenerationError
from psyclone.configuration import Config
from psyclone.psyir.nodes import Container, Routine, FileContainer

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


def test_accroutine_err(monkeypatch):
    ''' Check that we raise the expected error if we can't find the
    source of the kernel subroutine. '''
    import fparser
    _, invoke = get_invoke("1_single_invoke.f90", api="dynamo0.3", idx=0)
    sched = invoke.schedule
    kernels = sched.coded_kernels()
    kern = kernels[0]
    assert isinstance(kern, Kern)
    # Edit the fparser1 AST of the kernel so that it does not have a
    # subroutine of the correct name
    ast = kern._module_code
    mod = ast.content[0]
    # Find the subroutine statement
    for child in mod.content:
        if isinstance(child, fparser.one.block_statements.Subroutine):
            sub = child
    # Find the end subroutine statement
    for child in sub.content:
        if isinstance(child, fparser.one.block_statements.EndSubroutine):
            end = child
    monkeypatch.setattr(sub, "name", "some_other_name")
    monkeypatch.setattr(end, "name", "some_other_name")
    rtrans = ACCRoutineTrans()
    with pytest.raises(TransformationError) as err:
        _ = rtrans.apply(kern)
    assert(
        "Failed to create PSyIR version of kernel code for kernel "
        "'testkern_code'. Error reported is Generation Error: Unexpected "
        "kernel AST. Could not find subroutine: testkern_code."
        in str(err.value))


def test_accroutine_module_use():
    ''' Check that ACCRoutineTrans rejects a kernel if it contains a module
    use statement. '''
    _, invoke = get_invoke("single_invoke_kern_with_use.f90", api="gocean1.0",
                           idx=0)
    sched = invoke.schedule
    kernels = sched.walk(Kern)
    rtrans = ACCRoutineTrans()
    with pytest.raises(TransformationError) as err:
        _ = rtrans.apply(kernels[0])
    assert ("imported interface: ['rdt']. If these symbols represent data then"
            " they must first" in str(err.value))


def test_accroutine():
    ''' Test that we can transform a kernel by adding a "!$acc routine"
    directive to it. '''
    from psyclone.gocean1p0 import GOKern
    from fparser.two import Fortran2003
    _, invoke = get_invoke("nemolite2d_alg_mod.f90", api="gocean1.0", idx=0)
    sched = invoke.schedule
    kern = sched.coded_kernels()[0]
    assert isinstance(kern, GOKern)
    rtrans = ACCRoutineTrans()
    assert rtrans.name == "ACCRoutineTrans"
    new_kern, _ = rtrans.apply(kern)
    # The transformation should have populated the fparser2 AST of
    # the kernel...
    assert new_kern._fp2_ast
    assert isinstance(new_kern._fp2_ast, Fortran2003.Program)
    # Check AST contains directive
    comments = walk(new_kern._fp2_ast.content, Fortran2003.Comment)
    assert len(comments) == 1
    assert str(comments[0]) == "!$acc routine"
    # Check that directive is in correct place (end of declarations)
    gen = str(new_kern._fp2_ast)
    assert ("REAL(KIND = go_wp), DIMENSION(:, :), INTENT(IN) :: sshn, sshn_u, "
            "sshn_v, hu, hv, un, vn\n"
            "    !$acc routine\n"
            "    ssha(ji, jj) = 0.0_go_wp\n" in gen)


def test_accroutine_empty_kernel():
    ''' Check that the directive goes at the end of the declarations,
    even when the rest of the kernel is empty. '''
    _, invoke = get_invoke("1_single_invoke.f90", api="dynamo0.3", idx=0)
    sched = invoke.schedule
    kernels = sched.coded_kernels()
    rtrans = ACCRoutineTrans()
    new_kern, _ = rtrans.apply(kernels[0])
    # Check that directive is in correct place (end of declarations)
    gen = str(new_kern._fp2_ast).lower()
    assert "!$acc routine\n  end subroutine testkern_code" in gen


def test_new_kernel_file(kernel_outputdir, monkeypatch, fortran_reader):
    ''' Check that we write out the transformed kernel to the CWD. '''
    # Ensure kernel-output directory is uninitialised
    config = Config.get()
    monkeypatch.setattr(config, "_kernel_naming", "multiple")
    psy, invoke = get_invoke("nemolite2d_alg_mod.f90", api="gocean1.0", idx=0)
    sched = invoke.schedule
    kern = sched.coded_kernels()[0]
    rtrans = ACCRoutineTrans()
    _, _ = rtrans.apply(kern)
    # Generate the code (this triggers the generation of a new kernel)
    code = str(psy.gen).lower()
    # Work out the value of the tag used to re-name the kernel
    tag = re.search('use continuity(.+?)_mod', code).group(1)
    assert ("use continuity{0}_mod, only: continuity{0}_code".format(tag)
            in code)
    assert "call continuity{0}_code(".format(tag) in code
    # The kernel and module name should have gained the tag just identified
    # and be written to the CWD
    filename = os.path.join(str(kernel_outputdir),
                            "continuity{0}_mod.f90".format(tag))
    assert os.path.isfile(filename)
    # Parse the new kernel file
    psyir = fortran_reader.psyir_from_file(filename)
    # Check that the module has the right name
    assert isinstance(psyir, FileContainer)
    module = psyir.children[0]
    assert module.name == "continuity{0}_mod".format(tag)
    # Check that the subroutine has the right name
    found = False
    for sub in psyir.walk(Routine):
        if sub.name == "continuity{0}_code".format(tag):
            found = True
            break
    assert found

    from psyclone.tests.gocean1p0_build import GOcean1p0Build
    # If compilation fails this will raise an exception
    GOcean1p0Build(kernel_outputdir).compile_file(filename)


def test_new_kernel_dir(kernel_outputdir):
    ''' Check that we write out the transformed kernel to a specified
    directory. '''
    psy, invoke = get_invoke("nemolite2d_alg_mod.f90", api="gocean1.0", idx=0)
    sched = invoke.schedule
    kern = sched.coded_kernels()[0]
    rtrans = ACCRoutineTrans()
    _, _ = rtrans.apply(kern)
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
                           old_mod_name+"_0_mod.f90"), "w") as ffile:
        ffile.write("some code")
    rtrans = ACCRoutineTrans()
    _, _ = rtrans.apply(kern)
    # Generate the code (this triggers the generation of a new kernel)
    _ = str(psy.gen).lower()
    filename = os.path.join(str(kernel_outputdir), old_mod_name+"_1_mod.f90")
    assert os.path.isfile(filename)


@pytest.mark.parametrize(
    "mod_name,sub_name",
    [("testkern_mod", "testkern"),
     ("testkern", "testkern_code"),
     ("testkern1_mod", "testkern2_code")])
def test_kernel_module_name(mod_name, sub_name, kernel_outputdir,
                            monkeypatch):
    '''Check that there is no limitation on kernel and module names. In
    particular check that the names do not have to conform to the
    <name>_mod, <name>_code convention.

    '''
    _, invoke = get_invoke("1_single_invoke.f90", api="dynamo0.3", idx=0)
    sched = invoke.schedule
    kernels = sched.coded_kernels()
    kern = kernels[0]
    ktrans = Dynamo0p3KernelConstTrans()
    _, _ = ktrans.apply(kern, {"number_of_layers": 100})
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
    _, _ = ktrans.apply(kern, {"number_of_layers": 100})
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
                           old_mod_name+"_0_mod.f90"), "w") as ffile:
        ffile.write("some code")
    rtrans = ACCRoutineTrans()
    new_kern, _ = rtrans.apply(kern)
    # Generate the code - this should raise an error as we get a name
    # clash and the content of the existing file is not the same as that
    # which we would generate
    with pytest.raises(GenerationError) as err:
        new_kern.rename_and_write()
    assert ("transformed version of this Kernel 'testkern_0_mod.f90' already "
            "exists in the kernel-output directory ({0}) but is not the same "
            "as the current, transformed kernel and the kernel-renaming "
            "scheme is set to 'single'".format(str(kernel_outputdir))
            in str(err.value))


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
        new_kern, _ = rtrans.apply(kern)
        new_kernels.append(new_kern)

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
    _, _ = rtrans.apply(kern)
    # Generate the code (this triggers the generation of a new kernel)
    code = str(psy.gen).lower()
    tag = re.search('use testkern(.+?)_mod', code).group(1)
    # We should have a USE for the original kernel and a USE for the new one
    assert "use testkern{0}_mod, only: testkern{0}_code".format(tag) in code
    assert "use testkern_mod, only: testkern_code" in code
    # Similarly, we should have calls to both the original and new kernels
    assert "call testkern_code(" in code
    assert "call testkern{0}_code(".format(tag) in code
    first = code.find("call testkern_code(")
    second = code.find("call testkern{0}_code(".format(tag))
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
    _, _ = ktrans.apply(kernels[1], {"number_of_layers": 100})
    _, _ = ktrans.apply(kernels[2], {"number_of_layers": 100})
    # Generate the code (this triggers the generation of new kernels)
    code = str(psy.gen).lower()
    # Find the tags added to the kernel/module names
    for match in re.finditer('use testkern_any_space_2(.+?)_mod', code):
        tag = match.group(1)
        assert ("use testkern_any_space_2{0}_mod, only: "
                "testkern_any_space_2{0}_code".format(tag) in code)
        assert "call testkern_any_space_2{0}_code(".format(tag) in code
        filepath = os.path.join(str(kernel_outputdir),
                                "testkern_any_space_2{0}_mod.f90".format(tag))
        assert os.path.isfile(filepath)
        assert "nlayers = 100" in open(filepath).read()
    assert "use testkern_any_space_2_mod, only" not in code
    assert "call testkern_any_space_2_code(" not in code
    assert LFRicBuild(kernel_outputdir).code_compiles(psy)


def test_builtin_no_trans():
    ''' Check that we reject attempts to transform built-in kernels. '''
    from psyclone.domain.lfric.lfric_builtins import LFRicBuiltIn
    _, invoke = get_invoke("15.1.1_X_plus_Y_builtin.f90",
                           api="dynamo0.3", idx=0)
    sched = invoke.schedule
    kernels = sched.walk(LFRicBuiltIn)
    rtrans = ACCRoutineTrans()
    with pytest.raises(TransformationError) as err:
        _ = rtrans.apply(kernels[0])
    assert ("ACCRoutineTrans to a built-in kernel is not yet supported and "
            "kernel 'x_plus_y' is of type " in str(err.value))


def test_no_inline_global_var():
    ''' Check that we refuse to in-line a kernel that accesses a global
    variable. '''
    from psyclone.transformations import KernelModuleInlineTrans
    inline_trans = KernelModuleInlineTrans()
    _, invoke = get_invoke("single_invoke_kern_with_global.f90",
                           api="gocean1.0", idx=0)
    sched = invoke.schedule
    kernels = sched.walk(Kern)
    with pytest.raises(TransformationError) as err:
        _, _ = inline_trans.apply(kernels[0])
    assert ("'kernel_with_global_code' contains accesses to data (variable "
            "'alpha') that are not captured in the PSyIR Symbol Table(s) "
            "within KernelSchedule scope." in str(err.value))


# Class KernelTrans

# Method validate

def test_kernel_trans_validate(monkeypatch):
    '''Check that the validate method in the class KernelTrans raises an
    exception if the reference is not found in any of the symbol
    tables. KernelTrans can't be instantiated as it is abstract so use
    a the subclass.

    '''
    from psyclone.transformations import KernelModuleInlineTrans
    kernel_trans = KernelModuleInlineTrans()
    _, invoke = get_invoke("single_invoke_kern_with_global.f90",
                           api="gocean1.0", idx=0)
    sched = invoke.schedule
    kernels = sched.walk(Kern)
    kernel = kernels[0]

    def dummy_func():
        '''Simple Dummy function that raises SymbolError.'''
        from psyclone.psyir.symbols import SymbolError
        raise SymbolError("error")
    monkeypatch.setattr(kernel, "get_kernel_schedule", dummy_func)
    with pytest.raises(TransformationError) as err:
        _, _ = kernel_trans.apply(kernel)
    assert ("'kernel_with_global_code' contains accesses to data that are "
            "not captured in the PSyIR Symbol Table(s) (error)."
            "" in str(err.value))
