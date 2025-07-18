# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2019-2025, Science and Technology Facilities Council.
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
# Modified R. W. Ford, STFC Daresbury Lab


''' Module which performs pytest set-up specific to the PSyIR tests. '''

import pytest

from psyclone.parse import ModuleManager
import psyclone.psyir.frontend.fparser2 as fp2
from psyclone.psyir.symbols import DataSymbol, INTEGER_TYPE


@pytest.fixture(scope="function")
def disable_declaration_check(monkeypatch):
    ''' By default a Reference checks that it has a corresponding entry in
    the Symbol Table. However, this could make constructing tests very
    long winded so this fixture simply disables the check.

    TODO #754 fix all tests so that this fixture is not required.

    '''
    monkeypatch.setattr(
        fp2, "_find_or_create_unresolved_symbol",
        lambda _1, name, symbol_type=None, datatype=None: DataSymbol(
            name, INTEGER_TYPE))


@pytest.fixture(name="clear_module_manager", scope="function", autouse=True)
def modmanager_fixture(monkeypatch, request):
    '''
    A fixture that ensures every test gets a fresh ModuleManager instance as
    otherwise changes to search paths or file creation/removal is not detected.
    '''
    monkeypatch.setattr(ModuleManager, '_instance', None)
