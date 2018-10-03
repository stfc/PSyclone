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

from __future__ import absolute_import
import os
import pytest
from psyclone_test_utils import get_invoke
from psyclone.transformations import TransformationError, ACCRoutineTrans
from psyclone import configuration


def test_accroutine_err(monkeypatch):
    ''' Check that we raise the expected error if we can't find the
    source of the kernel subroutine. '''
    from psyclone.psyGen import Kern
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
    # Keep a record of the original module name for this kernel
    old_mod_name = kern.module_name[:]
    old_kern_name = kern.name[:]
    rtrans = ACCRoutineTrans()
    new_kern, _ = rtrans.apply(kern)
    # Generate the code (this triggers the generation of a new kernel)
    code = str(psy.gen).lower()
    assert "use continuity_mod, only: continuity_code" in code
    assert "call continuity_code(" in code
    # The kernel (and module) name should be unchanged and be written
    # to the CWD
    filename = os.path.join(str(tmpdir), old_mod_name+".f90")
    assert os.path.isfile(filename)
    # Parse the new kernel file
    f2003_parser = parser.ParserFactory().create()
    reader = FortranFileReader(filename)
    prog = f2003_parser(reader)
    # Check that the module has the right name
    modules = Fortran2003.walk_ast(prog.content, [Fortran2003.Module_Stmt])
    assert str(modules[0].items[1]) == old_mod_name
    # Check that the subroutine has the right name
    subs = Fortran2003.walk_ast(prog.content, [Fortran2003.Subroutine_Stmt])
    found = False
    for sub in subs:
        if str(sub.items[1]) == old_kern_name:
            found = True
            break
    assert found
    # TODO check compilation of code (needs Joerg's extension of compilation
    # testing to GOcean)


def test_new_kern_no_clobber(tmpdir, monkeypatch):
    ''' Check that we create a new kernel with a new name if no-clobber
    is set and we would otherwise get a name clash. '''
    from psyclone.psyGen import Kern
    # Ensure kernel-output directory is uninitialised
    config = configuration.ConfigFactory().create()
    monkeypatch.setattr(config, "_kernel_output_dir", "")
    # Change to temp dir (so kernel written there)
    old_pwd = tmpdir.chdir()
    psy, invoke = get_invoke("1_single_invoke.f90", api="dynamo0.3", idx=0)
    sched = invoke.schedule
    kernels = sched.walk(sched.children, Kern)
    kern = kernels[0]
    old_mod_name = kern.module_name[:]
    rtrans = ACCRoutineTrans()
    new_kern, _ = rtrans.apply(kern)
    # Generate the code (this triggers the generation of a new kernel)
    code = str(psy.gen).lower()
    filename = os.path.join(str(tmpdir), old_mod_name+".f90")
    print(filename)
    import pdb; pdb.set_trace()
    assert os.path.isfile(filename)

