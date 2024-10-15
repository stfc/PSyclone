# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2024, Science and Technology Facilities Council.
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

'''
This module contains pytest tests for the LFRicHaloDepths class.

'''
from psyclone.domain.lfric import LFRicHaloDepths
from psyclone.psyir.symbols import DataSymbol
from psyclone.tests.utilities import get_invoke

API = "lfric"


def test_lfric_halo_depth_ctor():
    '''
    Test that the constructor creates the set of Symbols representing any
    halo depths.

    '''
    # An invoke with no kernels that operate on halo cells.
    _, invoke = get_invoke("1.2_multi_invoke.f90", API, idx=0)    
    hdepths = LFRicHaloDepths(invoke)
    assert not hdepths._halo_depth_vars
    # An invoke with three kernels, two of which operate on halo cells
    # to different depths.
    _, invoke2 = get_invoke("1.4.2_multi_into_halos_invoke.f90", API, idx=0)
    hdepths2 = LFRicHaloDepths(invoke2)
    assert len(hdepths2._halo_depth_vars) == 2
    for sym in hdepths2._halo_depth_vars:
        assert isinstance(sym, DataSymbol)
        assert sym.name in ["hdepth", "other_depth"]
               
