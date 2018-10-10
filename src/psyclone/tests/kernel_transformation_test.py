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
from psyclone_test_utils import get_invoke
from psyclone.transformations import TransformationError, ACCRoutineTrans
from psyclone.psyGen import Kern
from psyclone import configuration
from psyclone_test_utils import code_compiles

def teardown_function():
    ''' This function is called automatically after every test in this
    file. It ensures that any existing configuration object is deleted. '''
    from psyclone.configuration import ConfigFactory
    ConfigFactory._instance = None


def test_accroutine_err(monkeypatch):
    ''' Check that we raise the expected error if we can't find the
    source of the kernel subroutine. '''
    import fparser
    _, invoke = get_invoke("1_single_invoke.f90", api="dynamo0.3", idx=0)
    sched = invoke.schedule
    kernels = sched.walk(sched.children, Kern)
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
    comments = Fortran2003.walk_ast(new_kern._fp2_ast.content,
                                    [Fortran2003.Comment])
    assert len(comments) == 1
    assert str(comments[0]) == "!$acc routine"
    # Check that directive is in correct place (end of declarations)
    gen = str(new_kern._fp2_ast)
    assert ("REAL(KIND = wp), DIMENSION(:, :), INTENT(IN) :: sshn, sshn_u, "
            "sshn_v, hu, hv, un, vn\n"
            "    !$acc routine\n"
            "    ssha (ji, jj) = 0.0_wp\n" in gen)


def test_accroutine_empty_kernel():
    ''' Check that the directive goes at the end of the declarations,
    even when the rest of the kernel is empty. '''
    psy, invoke = get_invoke("1_single_invoke.f90", api="dynamo0.3", idx=0)
    sched = invoke.schedule
    kernels = sched.walk(sched.children, Kern)
    rtrans = ACCRoutineTrans()
    new_kern, _ = rtrans.apply(kernels[0])
    # Check that directive is in correct place (end of declarations)
    gen = str(new_kern._fp2_ast).lower()
    print(gen)
    assert ("!$acc routine\n  end subroutine testkern_code" in gen)


def test_new_kernel_file(tmpdir, monkeypatch):
    ''' Check that we write out the transformed kernel to the CWD. '''
    from psyclone.gocean1p0 import GOKern
    from fparser.two import Fortran2003, parser
    from fparser.common.readfortran import FortranFileReader
    # Ensure kernel-output directory is uninitialised
    config = configuration.ConfigFactory().create()
    monkeypatch.setattr(config, "_kernel_output_dir", "")
    # Change to temp dir (so kernel written there)
    old_pwd = tmpdir.chdir()
    psy, invoke = get_invoke("nemolite2d_alg_mod.f90", api="gocean1.0", idx=0)
    sched = invoke.schedule
    kern = sched.children[0].children[0].children[0]
    rtrans = ACCRoutineTrans()
    new_kern, _ = rtrans.apply(kern)
    # Generate the code (this triggers the generation of a new kernel)
    code = str(psy.gen).lower()
    # Work out the value of the random tag used to re-name the kernel
    tag = re.search('use continuity(.+?)_mod', code).group(1)
    assert ("use continuity{0}_mod, only: continuity{0}_code".format(tag)
            in code)
    assert "call continuity{0}_code(".format(tag) in code
    # The kernel and module name should have gained some random chars and be
    # written to the CWD
    filename = os.path.join(str(tmpdir), "continuity{0}_mod.f90".format(tag))
    assert os.path.isfile(filename)
    # Parse the new kernel file
    f2003_parser = parser.ParserFactory().create()
    reader = FortranFileReader(filename)
    prog = f2003_parser(reader)
    # Check that the module has the right name
    modules = Fortran2003.walk_ast(prog.content, [Fortran2003.Module_Stmt])
    assert str(modules[0].items[1]) == "continuity{0}_mod".format(tag)
    # Check that the subroutine has the right name
    subs = Fortran2003.walk_ast(prog.content, [Fortran2003.Subroutine_Stmt])
    found = False
    for sub in subs:
        if str(sub.items[1]) == "continuity{0}_code".format(tag):
            found = True
            break
    assert found
    # TODO check compilation of code (needs Joerg's extension of compilation
    # testing to GOcean)


def test_new_kernel_dir(tmpdir, monkeypatch):
    ''' Check that we write out the transformed kernel to a specified
    directory. '''
    config = configuration.ConfigFactory().create()
    monkeypatch.setattr(config, "_kernel_output_dir", str(tmpdir))
    psy, invoke = get_invoke("nemolite2d_alg_mod.f90", api="gocean1.0", idx=0)
    sched = invoke.schedule
    kern = sched.children[0].children[0].children[0]
    rtrans = ACCRoutineTrans()
    new_kern, _ = rtrans.apply(kern)
    # Generate the code (this triggers the generation of a new kernel)
    _ = str(psy.gen)
    file_list = os.listdir(str(tmpdir))
    assert len(file_list) == 1
    assert re.match('continuity_(.+?)_mod.f90', file_list[0])


def test_1kern_trans(tmpdir, monkeypatch):
    ''' Check that we generate the correct code when an invoke contains
    the same kernel more than once but only one of them is transformed. '''
    # Ensure kernel-output directory is uninitialised
    config = configuration.ConfigFactory().create()
    monkeypatch.setattr(config, "_kernel_output_dir", "")
    # Change to temp dir (so kernel written there)
    old_pwd = tmpdir.chdir()
    psy, invoke = get_invoke("4_multikernel_invokes.f90", api="dynamo0.3",
                             idx=0)
    sched = invoke.schedule
    kernels = sched.walk(sched.children, Kern)
    # We will transform the second kernel but not the first
    kern = kernels[1]
    rtrans = ACCRoutineTrans()
    new_kern, _ = rtrans.apply(kern)
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


def test_2kern_trans(tmpdir, monkeypatch, f90, f90flags):
    ''' Check that we generate correct code when we transform two kernels
    within a single invoke. '''
    # Ensure kernel-output directory is uninitialised
    config = configuration.ConfigFactory().create()
    monkeypatch.setattr(config, "_kernel_output_dir", "")
    # Change to temp dir (so kernel written there)
    old_pwd = tmpdir.chdir()
    psy, invoke = get_invoke("4.5.2_multikernel_invokes.f90", api="dynamo0.3",
                             idx=0)
    sched = invoke.schedule
    kernels = sched.walk(sched.children, Kern)
    assert len(kernels) == 5
    rtrans = ACCRoutineTrans()
    new_kern1, _ = rtrans.apply(kernels[1])
    new_kern2, _ = rtrans.apply(kernels[2])
    # Generate the code (this triggers the generation of new kernels)
    code = str(psy.gen).lower()
    # Find the random tags added to the kernel/module names
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

