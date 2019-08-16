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
# -----------------------------------------------------------------------------
# Authors: R. Ford and A. R. Porter, STFC Daresbury Lab

''' Module containing py.test tests for the transformation of
    the PSy representation of NEMO code '''

from __future__ import print_function, absolute_import
import os
import pytest
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory, TransInfo, InternalError
from psyclone.transformations import TransformationError
from psyclone import nemo
from fparser.two import Fortran2003
from fparser.common.readfortran import FortranStringReader

# Constants
API = "nemo"
# Location of the Fortran files associated with these tests
BASE_PATH = os.path.join(os.path.dirname(
    os.path.dirname(os.path.dirname(os.path.abspath(__file__)))),
                         "test_files")


def test_implicit_loop_trans():
    ''' Check that we get the correct schedule when we apply the explicit
    loop transformation to an implicit loop. '''
    _, invoke_info = parse(os.path.join(BASE_PATH, "implicit_do.f90"),
                           api=API, line_length=False)
    psy = PSyFactory(API).create(invoke_info)
    exp_trans = TransInfo().get_trans_name('NemoExplicitLoopTrans')
    assert isinstance(psy, nemo.NemoPSy)
    sched = psy.invokes.invoke_list[0].schedule
    assert isinstance(sched.children[0], nemo.NemoImplicitLoop)
    new_loop, _ = exp_trans.apply(sched.children[0])
    # The code being tested has a triply-nested implicit loop so applying
    # the transform once gives an outer explicit loop and an inner,
    # doubly-nested implicit loop
    loops = sched.walk(nemo.NemoLoop)
    assert len(loops) == 2
    assert loops[0].loop_type == "levels"
    new_loop, _ = exp_trans.apply(new_loop.loop_body[0])
    loops = sched.walk(nemo.NemoLoop)
    assert len(loops) == 3
    assert loops[1].loop_type == "lat"
    new_loop, _ = exp_trans.apply(new_loop.loop_body[0])
    loops = sched.walk(nemo.NemoLoop)
    # We should still have 3 loops since the last transformation
    # should have created an explicit loop containing a kernel
    assert len(loops) == 3
    assert loops[2].loop_type == "lon"
    assert isinstance(loops[2].loop_body[0], nemo.NemoKern)
    # Finally, check the generated code
    gen_code = str(psy.gen)
    assert ("  INTEGER :: jk\n"
            "  INTEGER :: jj\n"
            "  INTEGER :: ji\n"
            "  DO jk = 1, jpk, 1\n"
            "    DO jj = 1, jpj, 1\n"
            "      DO ji = 1, jpi, 1\n"
            "        umask(ji, jj, jk) = 0.0D0\n"
            "      END DO\n"
            "    END DO\n"
            "  END DO\n" in gen_code)


@pytest.mark.xfail(reason="Code being transformed already declares ji and jj "
                   "and so we get duplicate declarations. Need to query the "
                   "SymbolTable - #381.")
def test_implicit_loop_sched2():
    ''' Check that we get the correct schedule when we transform an implicit
    loop over the i-j slab within an explicit loop levels. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "explicit_over_implicit.f90"),
                           api=API, line_length=False)
    psy = PSyFactory(API).create(invoke_info)
    exp_trans = TransInfo().get_trans_name('NemoExplicitLoopTrans')
    sched = psy.invokes.invoke_list[0].schedule
    loop_levels = sched.children[0]
    _, _ = exp_trans.apply(loop_levels.children[0])
    # We should have 3 loops (one from the explicit loop over levels and
    # the other two from the implicit loops over ji and jj).
    loops = sched.walk(nemo.NemoLoop)
    assert len(loops) == 3
    assert loop_levels.children[0].loop_type == "lat"
    kerns = sched.kern_calls()
    assert not kerns
    _, _ = exp_trans.apply(loop_levels.children[0].children[0])
    gen_code = str(psy.gen)
    assert ("  INTEGER :: jj\n"
            "  INTEGER :: ji\n"
            "  DO jk = 1, jpk\n"
            "    DO jj = 1, jpj, 1\n"
            "      DO ji = 1, jpi, 1\n"
            "        umask(ji, jj, jk) = vmask(ji, jj, jk) + 1.0\n"
            "      END DO\n"
            "    END DO\n"
            "  END DO\n"
            "END PROGRAM explicit_over_implicit" in gen_code)
    # Check that we haven't got duplicate declarations of the loop vars
    assert gen_code.count("INTEGER :: ji") == 1


def test_exp_loop_unrecognised_implicit(parser):
    ''' Check that we raise the expected error if we encounter an
    unrecognised form of implicit loop. '''
    exp_trans = TransInfo().get_trans_name('NemoExplicitLoopTrans')
    # Array syntax used in an unsupported index location
    reader = FortranStringReader("program test_prog\n"
                                 "real, dimension(3,3,3,3) :: umask\n"
                                 "umask(:, :, :, :) = 0.0D0\n"
                                 "end program test_prog\n")
    prog = parser(reader)
    psy = PSyFactory(API).create(prog)
    sched = psy.invokes.invoke_list[0].schedule
    with pytest.raises(TransformationError) as err:
        exp_trans.apply(sched.children[0])
    assert ("Array section in unsupported dimension (4) for code "
            "'umask(:, :, :, :) = 0.0D0'" in str(err))


def test_exp_loop_missing_spec(parser):
    '''Test that the ExplicitLoop transformation still works when the
    fparser2 AST is missing a Specification_Part for the routine.

    '''
    from fparser.two.utils import walk_ast
    reader = FortranStringReader("program atest\nreal :: umask(1,1,1,1)\n"
                                 "umask(:, :, :) = 0.0\nend program atest\n")
    prog = parser(reader)
    psy = PSyFactory(API).create(prog)
    sched = psy.invokes.invoke_list[0].schedule
    # Remove the specification part
    spec = walk_ast(prog.content, [Fortran2003.Specification_Part])
    prog.content[0].content.remove(spec[0])
    # Check that we can transform OK
    exp_trans = TransInfo().get_trans_name('NemoExplicitLoopTrans')
    _, _ = exp_trans.apply(sched.children[0])
    gen_code = str(psy.gen)
    assert ("PROGRAM atest\n"
            "  INTEGER :: jk\n"
            "  DO jk = 1, jpk, 1\n"
            "    umask(:, :, jk) = 0.0\n"
            "  END DO\n"
            "END PROGRAM atest" in gen_code)


def test_implicit_range_err(parser):
    ''' Check that we raise the expected error if we encounter an implicit
    loop with an explicit range (since we don't yet support that). '''
    exp_trans = TransInfo().get_trans_name('NemoExplicitLoopTrans')
    # Array syntax with an explicit range
    reader = FortranStringReader("program atest\n"
                                 "umask(1:jpi, 1, :) = 0.0D0\n"
                                 "end program atest\n")
    prog = parser(reader)
    psy = PSyFactory(API).create(prog)
    sched = psy.invokes.invoke_list[0].schedule
    with pytest.raises(NotImplementedError) as err:
        exp_trans.apply(sched.children[0])
    assert ("Support for implicit loops with specified bounds is not yet "
            "implemented: 'umask(1 : jpi, 1, :) = 0.0D0'" in str(err))


def test_implicit_loop_different_rank():
    ''' Test that we reject implicit loops if the index positions of the
    colons differs. This is a restriction that could be lifted by
    using e.g. SIZE(zvab, 1) as the upper loop limit or (with a lot more
    work) by interrogating the parsed code to figure out the loop bound. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "array_section_index_mismatch.f90"),
                           api=API, line_length=False)
    psy = PSyFactory(API).create(invoke_info)
    sched = psy.invokes.invoke_list[0].schedule
    loop = sched.children[1]
    trans = TransInfo().get_trans_name('NemoExplicitLoopTrans')
    with pytest.raises(TransformationError) as err:
        _ = trans.apply(loop)
    assert ("implicit loops are restricted to cases where all array "
            "range specifications occur" in str(err))
    loop = sched.children[2]
    with pytest.raises(InternalError) as err:
        _ = trans.apply(loop)
    assert ("Expecting a colon for index 3 but array only has 2 "
            "dimensions: zab(" in str(err))


def test_explicit_loop_validate():
    ''' Test for the validate method of NemoExplicitLoopTrans. '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "explicit_over_implicit.f90"),
                           api=API, line_length=False)
    psy = PSyFactory(API).create(invoke_info)
    exp_trans = TransInfo().get_trans_name('NemoExplicitLoopTrans')
    sched = psy.invokes.invoke_list[0].schedule
    # Attempt to apply the transformation to an explicit do loop
    with pytest.raises(TransformationError) as err:
        _ = exp_trans.apply(sched.children[0])
    assert ("Cannot apply NemoExplicitLoopTrans to something that is "
            "not a NemoImplicitLoop (got " in str(err))
