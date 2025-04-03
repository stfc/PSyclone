# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021-2025, Science and Technology Facilities Council.
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
# Author: R. W. Ford, STFC Daresbury Lab
# Modifications: A. R. Porter, STFC Daresbury Lab


'''This module tests the LFRicPSy class found in the LFRic domain.
'''

import os

from psyclone.configuration import Config
from psyclone.domain.lfric import LFRicPSy, LFRicInvokes
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSy
from psyclone.tests.lfric_build import LFRicBuild

BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         os.pardir, os.pardir, "test_files", "dynamo0p3")


class DummyInvokeInfo():
    '''A dummy class that contains the minimum information required to
    allow LFRicPSy to be called.

    :param str name: the name to use for this dummy invoke. Defaults
        to an empty string.

    '''
    def __init__(self, name=""):
        self.name = name
        self.calls = []


def test_lfricpsy():
    ''' Check that an instance of LFRicPSy can be created successfully.'''

    invoke_info = DummyInvokeInfo()
    lfric_psy = LFRicPSy(invoke_info)
    assert isinstance(lfric_psy, LFRicPSy)
    assert issubclass(LFRicPSy, PSy)
    assert isinstance(lfric_psy._invokes, LFRicInvokes)


def test_lfricpsy_kind():
    '''Check that an instance of LFRicPSy captures any precision (kind
    values) for literals.

    '''
    # 1: no literal kind value gives the default r_def (even though it
    # is not required).
    _, invoke_info = parse(os.path.join(
        BASE_PATH, "15.12.3_single_pointwise_builtin.f90"), api="lfric")
    lfric_psy = LFRicPSy(invoke_info)
    result = str(lfric_psy.gen)
    assert "use constants_mod\n" in result
    assert "f1_data(df) = 0.0\n" in result
    # 2: Literal kind value is declared (trying with two cases to check)
    for kind_name in ["r_solver", "r_tran"]:
        invoke_info.calls[0].kcalls[0].args[1]._text = f"0.0_{kind_name}"
        invoke_info.calls[0].kcalls[0].args[1]._datatype = ("real", kind_name)
        lfric_psy = LFRicPSy(invoke_info)
        result = str(lfric_psy.gen).lower()
        assert "use constants_mod\n" in result
        assert f"f1_data(df) = 0.0_{kind_name}" in result


def test_lfricpsy_names():
    '''Check that the name() and orig_name() methods of LFRicPSy behave as
    expected.

    '''
    supplied_name = "hello"
    invoke_info = DummyInvokeInfo(name=supplied_name)
    lfric_psy = LFRicPSy(invoke_info)
    assert lfric_psy.name == supplied_name + "_psy"
    assert lfric_psy.orig_name == supplied_name


def test_lfricpsy_gen_no_invoke():
    '''Check that the gen() method of LFRicPSy behaves as expected for a
    minimal psy-layer when the algorithm layer does not contain any
    invoke calls.

    '''
    expected_result = (
        "module hello_psy\n"
        "  use constants_mod\n"
        "  implicit none\n"
        "  public\n"
        "\n"
        "  contains\n"
        "\n"
        "end module hello_psy\n")
    lfric_psy = LFRicPSy(DummyInvokeInfo(name="hello"))
    result = lfric_psy.gen
    assert str(result) == expected_result


def test_lfricpsy_gen(monkeypatch, tmpdir):
    '''Check that the gen() method of LFRicPSy behaves as expected when
    generating a psy-layer from an algorithm layer containing invoke
    calls. Simply check that the PSy-layer code for the invoke call is
    generated as we check the rest of the generation for the
    LFRicPSy() gen() method in the previous test.

    '''
    # Since we're testing the LFRicPSy constructor directly, we have to
    # monkeypatch the Config object in order to guarantee that distributed
    # memory is enabled.
    monkeypatch.setattr(Config.get(), "_distributed_mem", True)

    _, invoke_info = parse(
        os.path.join(
            BASE_PATH, "15.14.4_builtin_and_normal_kernel_invoke.f90"),
        api="lfric")
    # Make sure we have distributed memory enabled, otherwise we can
    # get errors in parallel builds if a previous jobs leave this
    # to be false.
    config = Config.get()
    config.distributed_memory = True
    lfric_psy = LFRicPSy(invoke_info)
    result = str(lfric_psy.gen)
    assert (
        "    do cell = loop0_start, loop0_stop, 1\n"
        "      call testkern_code(nlayers_f1, ginger, f1_data, "
        "f2_data, m1_data, m2_data, ndf_w1, undf_w1, "
        "map_w1(:,cell), ndf_w2, undf_w2, map_w2(:,cell), ndf_w3, undf_w3, "
        "map_w3(:,cell))\n"
        "    enddo\n"
        "\n"
        "    ! Set halos dirty/clean for fields modified in the above "
        "loop(s)\n"
        "    call f1_proxy%set_dirty()\n"
        "    do df = loop1_start, loop1_stop, 1\n"
        "      ! Built-in: setval_c (set a real-valued field to a real "
        "scalar value)\n"
        "      f1_data(df) = 0.0_r_def\n"
        "    enddo\n" in result)
    assert LFRicBuild(tmpdir).code_compiles(lfric_psy)
