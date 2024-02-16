# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2024, Science and Technology Facilities Council.
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
# Author A. R. Porter, STFC Daresbury Lab
# Modified L. Turner, Met Office

''' This module tests the DynProxies class using pytest. '''

import os
import pytest
from psyclone.domain.lfric import LFRicConstants, LFRicKern
from psyclone.dynamo0p3 import DynProxies
from psyclone.errors import InternalError
from psyclone.f2pygen import ModuleGen, SubroutineGen
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory
from psyclone.psyir import symbols

BASE_PATH = os.path.join(
    os.path.dirname(os.path.dirname(os.path.dirname(
        os.path.abspath(__file__)))), "test_files", "dynamo0p3")
TEST_API = "dynamo0.3"


def test_creation():
    '''
    Test that the constructor of DynProxies populates the symbol table with
    the expected symbols and associated tags.
    '''
    _, info = parse(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    invoke = psy.invokes.invoke_list[0]
    proxies = DynProxies(invoke)
    tags = proxies._symbol_table.get_tags()
    assert "f1:data" in tags
    sym = proxies._symbol_table.lookup_with_tag("f1:data")
    assert isinstance(sym, symbols.DataSymbol)
    assert "f2:data" in tags


def test_invoke_declarations():
    '''
    Test the _invoke_declarations() method, primarily by checking the
    generated declarations in output code.

    '''
    _, info = parse(os.path.join(BASE_PATH,
                                 "8.3_multikernel_invokes_vector.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    invoke = psy.invokes.invoke_list[0]
    proxies = DynProxies(invoke)
    amod = ModuleGen("test_mod")
    node = SubroutineGen(amod, name="a_sub")
    amod.add(node)
    proxies._invoke_declarations(node)
    code = str(amod.root).lower()
    assert ("real(kind=r_def), pointer, dimension(:) :: f1_1_data => null(), "
            "f1_2_data => null(), f1_3_data => null()" in code)
    assert "type(field_proxy_type) f1_proxy(3)" in code
    assert ("r_def" in
            invoke.invokes.psy.infrastructure_modules["constants_mod"])


def test_initialise():
    '''
    Test the initialise() method.

    '''
    _, info = parse(os.path.join(BASE_PATH,
                                 "10.6_operator_no_field_scalar.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    invoke = psy.invokes.invoke_list[0]
    proxies = DynProxies(invoke)
    amod = ModuleGen("test_mod")
    node = SubroutineGen(amod, name="a_sub")
    amod.add(node)
    proxies._invoke_declarations(node)
    proxies.initialise(node)
    code = str(amod.root).lower()
    assert "initialise field and/or operator proxies" in code
    assert ("r_def" in
            invoke.invokes.psy.infrastructure_modules["constants_mod"])
    assert "my_mapping_proxy = my_mapping%get_proxy()" in code
    assert "my_mapping_local_stencil => my_mapping_proxy%local_stencil" in code


def test_initialise_errors(monkeypatch):
    '''
    Check that the initialise method raises the expected errors when it
    encounters arguments of UnsupportedType.

    '''
    _, info = parse(os.path.join(BASE_PATH,
                                 "10.6_operator_no_field_scalar.f90"),
                    api=TEST_API)
    psy = PSyFactory(TEST_API, distributed_memory=True).create(info)
    invoke = psy.invokes.invoke_list[0]
    kern = invoke.schedule.walk(LFRicKern)[0]
    proxies = DynProxies(invoke)
    amod = ModuleGen("test_mod")
    node = SubroutineGen(amod, name="a_sub")
    amod.add(node)
    proxies._invoke_declarations(node)
    # Monkeypatch the first kernel argument so that it is of an unrecognised
    # type.
    monkeypatch.setattr(kern.args[0], "_argument_type", "gh_wrong")
    # Have to ensure the suffix lookup works too.
    monkeypatch.setattr(LFRicConstants, "ARG_TYPE_SUFFIX_MAPPING",
                        {"gh_wrong": "data"})
    with pytest.raises(InternalError) as err:
        proxies.initialise(node)
    assert ("Kernel argument 'my_mapping' of type 'gh_wrong' not handled in "
            "DynProxies.initialise()" in str(err.value))

    # Now monkey patch the list of valid operator names so that the kernel
    # argument is recognised as an operator.
    monkeypatch.setattr(LFRicConstants, "VALID_OPERATOR_NAMES", ["gh_wrong"])
    with pytest.raises(InternalError) as err:
        proxies.initialise(node)
    assert ("Kernel argument 'my_mapping' is a recognised operator but its "
            "type ('gh_wrong') is not supported by DynProxies.initialise()"
            in str(err.value))
