# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018, Science and Technology Facilities Council.
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

''' Module containing tests for kernel transformations. '''

from __future__ import absolute_import, print_function
import os
import re
import pytest
from fparser.two.utils import walk_ast
from psyclone_test_utils import get_invoke, code_compiles
from psyclone.transformations import TransformationError, ACCRoutineTrans
from psyclone.psyGen import Kern
from psyclone.generator import GenerationError
from psyclone.configuration import Config


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
    kernels = sched.kern_calls()
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
    assert ("Failed to find subroutine source for kernel testkern_code"
            in str(err))


def test_accroutine():
    ''' Test that we can transform a kernel by adding a "!$acc routine"
    directive to it. '''
    from psyclone.gocean1p0 import GOKern
    from fparser.two import Fortran2003
    _, invoke = get_invoke("nemolite2d_alg_mod.f90", api="gocean1.0", idx=0)
    sched = invoke.schedule
    kern = sched.children[0].children[0].children[0]
    assert isinstance(kern, GOKern)
    rtrans = ACCRoutineTrans()
    assert rtrans.name == "ACCRoutineTrans"
    new_kern, _ = rtrans.apply(kern)
    # The transformation should have populated the fparser2 AST of
    # the kernel...
    assert new_kern._fp2_ast
    assert isinstance(new_kern._fp2_ast, Fortran2003.Program)
    # Check AST contains directive
    comments = walk_ast(new_kern._fp2_ast.content, [Fortran2003.Comment])
    assert len(comments) == 1
    assert str(comments[0]) == "!$acc routine"
    # Check that directive is in correct place (end of declarations)
    gen = str(new_kern._fp2_ast)
    assert ("REAL(KIND = go_wp), DIMENSION(:, :), INTENT(IN) :: sshn, sshn_u, "
            "sshn_v, hu, hv, un, vn\n"
            "    !$acc routine\n"
            "    ssha (ji, jj) = 0.0_go_wp\n" in gen)


def test_accroutine_empty_kernel():
    ''' Check that the directive goes at the end of the declarations,
    even when the rest of the kernel is empty. '''
    _, invoke = get_invoke("1_single_invoke.f90", api="dynamo0.3", idx=0)
    sched = invoke.schedule
    kernels = sched.kern_calls()
    rtrans = ACCRoutineTrans()
    new_kern, _ = rtrans.apply(kernels[0])
    # Check that directive is in correct place (end of declarations)
    gen = str(new_kern._fp2_ast).lower()
    assert "!$acc routine\n  end subroutine testkern_code" in gen


def test_new_kernel_file(tmpdir, monkeypatch):
    ''' Check that we write out the transformed kernel to the CWD. '''
    from fparser.two import Fortran2003, parser
    from fparser.common.readfortran import FortranFileReader
    # Ensure kernel-output directory is uninitialised
    config = Config.get()
    monkeypatch.setattr(config, "_kernel_output_dir", "")
    # Change to temp dir (so kernel written there)
    _ = tmpdir.chdir()
    psy, invoke = get_invoke("nemolite2d_alg_mod.f90", api="gocean1.0", idx=0)
    sched = invoke.schedule
    kern = sched.children[0].children[0].children[0]
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
    filename = os.path.join(str(tmpdir), "continuity{0}_mod.f90".format(tag))
    assert os.path.isfile(filename)
    # Parse the new kernel file
    f2003_parser = parser.ParserFactory().create()
    reader = FortranFileReader(filename)
    prog = f2003_parser(reader)
    # Check that the module has the right name
    modules = walk_ast(prog.content, [Fortran2003.Module_Stmt])
    assert str(modules[0].items[1]) == "continuity{0}_mod".format(tag)
    # Check that the subroutine has the right name
    subs = walk_ast(prog.content, [Fortran2003.Subroutine_Stmt])
    found = False
    for sub in subs:
        if str(sub.items[1]) == "continuity{0}_code".format(tag):
            found = True
            break
    assert found
    # Check that the kernel type has been re-named
    dtypes = walk_ast(prog.content, [Fortran2003.Derived_Type_Def])
    names = walk_ast(dtypes[0].content, [Fortran2003.Type_Name])
    assert str(names[0]) == "continuity{0}_type".format(tag)
    # TODO #281 check compilation of code (needs Joerg's extension of
    # compilation testing to GOcean)


def test_new_kernel_dir(tmpdir, monkeypatch):
    ''' Check that we write out the transformed kernel to a specified
    directory. '''
    # Set the output directory in the configuration object
    config = Config.get()
    monkeypatch.setattr(config, "_kernel_output_dir", str(tmpdir))
    psy, invoke = get_invoke("nemolite2d_alg_mod.f90", api="gocean1.0", idx=0)
    sched = invoke.schedule
    kern = sched.children[0].children[0].children[0]
    rtrans = ACCRoutineTrans()
    _, _ = rtrans.apply(kern)
    # Generate the code (this triggers the generation of a new kernel)
    _ = str(psy.gen)
    file_list = os.listdir(str(tmpdir))
    assert len(file_list) == 1
    assert file_list[0] == 'continuity_0_mod.f90'


def test_new_kern_no_clobber(tmpdir, monkeypatch):
    ''' Check that we create a new kernel with a new name when kernel-naming
    is set to 'multiple' and we would otherwise get a name clash. '''
    # Ensure kernel-output directory is uninitialised
    config = Config.get()
    monkeypatch.setattr(config, "_kernel_output_dir", "")
    monkeypatch.setattr(config, "_kernel_naming", "multiple")
    # Change to temp dir (so kernel written there)
    _ = tmpdir.chdir()
    psy, invoke = get_invoke("1_single_invoke.f90", api="dynamo0.3", idx=0)
    sched = invoke.schedule
    kernels = sched.walk(sched.children, Kern)
    kern = kernels[0]
    old_mod_name = kern.module_name[:]
    # Create a file with the same name as we would otherwise generate
    with open(os.path.join(str(tmpdir),
                           old_mod_name+"_0_mod.f90"), "w") as ffile:
        ffile.write("some code")
    rtrans = ACCRoutineTrans()
    _, _ = rtrans.apply(kern)
    # Generate the code (this triggers the generation of a new kernel)
    _ = str(psy.gen).lower()
    filename = os.path.join(str(tmpdir), old_mod_name+"_1_mod.f90")
    assert os.path.isfile(filename)


def test_new_kern_single_error(tmpdir, monkeypatch):
    ''' Check that we do not overwrite an existing, different kernel if
    there is a name clash and kernel-naming is 'single'. '''
    # Ensure kernel-output directory is uninitialised
    config = Config.get()
    monkeypatch.setattr(config, "_kernel_output_dir", "")
    monkeypatch.setattr(config, "_kernel_naming", "single")
    # Change to temp dir (so kernel written there)
    _ = tmpdir.chdir()
    _, invoke = get_invoke("1_single_invoke.f90", api="dynamo0.3", idx=0)
    sched = invoke.schedule
    kernels = sched.kern_calls()
    kern = kernels[0]
    old_mod_name = kern.module_name[:]
    # Create a file with the same name as we would otherwise generate
    with open(os.path.join(str(tmpdir),
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
            "scheme is set to 'single'".format(str(tmpdir)) in str(err))


def test_new_same_kern_single(tmpdir, monkeypatch):
    ''' Check that we do not overwrite an existing, identical kernel if
    there is a name clash and kernel-naming is 'single'. '''
    # Ensure kernel-output directory is uninitialised
    config = Config.get()
    monkeypatch.setattr(config, "_kernel_output_dir", "")
    monkeypatch.setattr(config, "_kernel_naming", "single")
    rtrans = ACCRoutineTrans()
    # Change to temp dir (so kernel written there)
    _ = tmpdir.chdir()
    _, invoke = get_invoke("4_multikernel_invokes.f90", api="dynamo0.3",
                           idx=0)
    sched = invoke.schedule
    # Apply the same transformation to both kernels. This should produce
    # two, identical transformed kernels.
    new_kernels = []
    for kern in sched.kern_calls():
        new_kern, _ = rtrans.apply(kern)
        new_kernels.append(new_kern)

    # Generate the code - we should end up with just one transformed kernel
    new_kernels[0].rename_and_write()
    new_kernels[1].rename_and_write()
    assert new_kernels[1]._name == "testkern_0_code"
    assert new_kernels[1].module_name == "testkern_0_mod"
    out_files = os.listdir(str(tmpdir))
    assert out_files == [new_kernels[1].module_name+".f90"]


def test_1kern_trans(tmpdir, monkeypatch, f90, f90flags):
    ''' Check that we generate the correct code when an invoke contains
    the same kernel more than once but only one of them is transformed. '''
    # Ensure kernel-output directory is uninitialised
    config = Config.get()
    monkeypatch.setattr(config, "_kernel_output_dir", "")
    # Change to temp dir (so kernel written there)
    _ = tmpdir.chdir()
    psy, invoke = get_invoke("4_multikernel_invokes.f90", api="dynamo0.3",
                             idx=0)
    sched = invoke.schedule
    kernels = sched.kern_calls()
    # We will transform the second kernel but not the first
    kern = kernels[1]
    rtrans = ACCRoutineTrans()
    _, _ = rtrans.apply(kern)
    # Generate the code (this triggers the generation of a new kernel)
    code = str(psy.gen).lower()
    tag = re.search('use testkern(.+?)_mod', code).group(1)
    # We should have a USE for the original kernel and a USE for the new one
    assert "use testkern{0}_mod, only: testkern{0}_code".format(tag) in code
    assert "use testkern, only: testkern_code" in code
    # Similarly, we should have calls to both the original and new kernels
    assert "call testkern_code(" in code
    assert "call testkern{0}_code(".format(tag) in code
    first = code.find("call testkern_code(")
    second = code.find("call testkern{0}_code(".format(tag))
    assert first < second
    assert code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)


def test_2kern_trans(tmpdir, monkeypatch, f90, f90flags):
    ''' Check that we generate correct code when we transform two kernels
    within a single invoke. '''
    # Ensure kernel-output directory is uninitialised
    config = Config.get()
    monkeypatch.setattr(config, "_kernel_output_dir", "")
    # Change to temp dir (so kernel written there)
    _ = tmpdir.chdir()
    psy, invoke = get_invoke("4.5.2_multikernel_invokes.f90", api="dynamo0.3",
                             idx=0)
    sched = invoke.schedule
    kernels = sched.walk(sched.children, Kern)
    assert len(kernels) == 5
    rtrans = ACCRoutineTrans()
    _, _ = rtrans.apply(kernels[1])
    _, _ = rtrans.apply(kernels[2])
    # Generate the code (this triggers the generation of new kernels)
    code = str(psy.gen).lower()
    # Find the tags added to the kernel/module names
    for match in re.finditer('use testkern_any_space_2(.+?)_mod', code):
        tag = match.group(1)
        assert ("use testkern_any_space_2{0}_mod, only: "
                "testkern_any_space_2{0}_code".format(tag) in code)
        assert "call testkern_any_space_2{0}_code(".format(tag) in code
        assert os.path.isfile(
            os.path.join(str(tmpdir),
                         "testkern_any_space_2{0}_mod.f90".format(tag)))
    assert "use testkern_any_space_2_mod, only" not in code
    assert "call testkern_any_space_2_code(" not in code
    assert code_compiles("dynamo0.3", psy, tmpdir, f90, f90flags)


def test_builtin_no_trans():
    ''' Check that we reject attempts to transform built-in kernels. '''
    from psyclone.dynamo0p3_builtins import DynBuiltIn
    _, invoke = get_invoke("15.1.1_X_plus_Y_builtin.f90",
                           api="dynamo0.3", idx=0)
    sched = invoke.schedule
    kernels = sched.walk(sched.children, DynBuiltIn)
    rtrans = ACCRoutineTrans()
    with pytest.raises(TransformationError) as err:
        _ = rtrans.apply(kernels[0])
    assert ("ACCRoutineTrans to a built-in kernel is not yet supported and "
            "kernel 'x_plus_y' is of type " in str(err))


def test_no_inline_before_trans(monkeypatch):
    ''' Check that we reject attempts to transform kernels that have been
    marked for module in-lining. Issue #229. '''
    from psyclone.transformations import KernelModuleInlineTrans
    psy, invoke = get_invoke("4.5.2_multikernel_invokes.f90", api="dynamo0.3",
                             idx=0)
    sched = invoke.schedule
    kernels = sched.walk(sched.children, Kern)
    assert len(kernels) == 5
    inline_trans = KernelModuleInlineTrans()
    rtrans = ACCRoutineTrans()
    _, _ = inline_trans.apply(kernels[1])
    with pytest.raises(TransformationError) as err:
        _, _ = rtrans.apply(kernels[1])
    assert "because it will be module-inlined" in str(err)
    # Monkeypatch the validate() routine so we can check that we catch
    # the error at code-generation time
    monkeypatch.setattr(rtrans, "validate", lambda kern: None)
    _, _ = rtrans.apply(kernels[1])
    with pytest.raises(NotImplementedError) as err:
        _ = str(psy.gen).lower()
    assert "Cannot module-inline a transformed kernel " in str(err)


def test_no_inline_after_trans(monkeypatch):
    ''' Check that we reject attempts to inline a previously transformed
    kernel. Issue #229. '''
    from psyclone.transformations import KernelModuleInlineTrans
    _, invoke = get_invoke("4.5.2_multikernel_invokes.f90", api="dynamo0.3",
                           idx=0)
    sched = invoke.schedule
    kernels = sched.walk(sched.children, Kern)
    assert len(kernels) == 5
    # Transform the kernel first
    inline_trans = KernelModuleInlineTrans()
    rtrans = ACCRoutineTrans()
    _, _ = rtrans.apply(kernels[1])
    # Then attempt to inline it
    with pytest.raises(TransformationError) as err:
        _, _ = inline_trans.apply(kernels[1])
    assert "because it has previously been transformed" in str(err)
    # Monkeypatch the validate() routine so we can check that we catch
    # the error at the psyGen level too.
    monkeypatch.setattr(inline_trans, "validate", lambda node, inline: None)
    with pytest.raises(NotImplementedError) as err:
        _, _ = inline_trans.apply(kernels[1])
    assert "Cannot module-inline a transformed kernel " in str(err)
