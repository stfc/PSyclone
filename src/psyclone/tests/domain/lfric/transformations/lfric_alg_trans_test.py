# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council
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
# Author R. W. Ford, STFC Daresbury Lab

'''Module containing tests for the translation of PSyIR to LFRic
Algorithm PSyIR.

'''
from __future__ import absolute_import

from fparser.two.parser import ParserFactory
from fparser.common.readfortran import FortranStringReader

from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.nodes import Call

from psyclone.domain.lfric.algorithm import \
    LFRicAlgorithmInvokeCall, LFRicKernelFunctor, LFRicBuiltinFunctor
from psyclone.domain.lfric.transformations import LFRicAlgTrans, \
    LFRicInvokeCallTrans


def create_psyir(code):
    ''' Utility to create a PSyIR tree from Fortran code.

    :param str code: Fortran code encoded as a string

    :returns: psyir tree representing the Fortran code
    :rtype: :py:class:`psyclone.psyir.nodes.Node`

    '''
    fortran_reader = FortranStringReader(code)
    f2008_parser = ParserFactory().create(std="f2008")
    parse_tree = f2008_parser(fortran_reader)

    psyir_reader = Fparser2Reader()
    psyir = psyir_reader.generate_psyir(parse_tree)

    return psyir


def test_init():
    '''Check that an LFRicAlgTrans instance can be created correctly, has
    the expected defaults, deals with any __init__ arguments and its
    name method returns the expected value.

    '''
    alg_trans = LFRicAlgTrans()
    assert alg_trans.name == "LFRicAlgTrans"
    assert isinstance(alg_trans, LFRicAlgTrans)
    assert isinstance(alg_trans._invoke_trans, LFRicInvokeCallTrans)


def test_apply():
    '''Test that the apply method behaves as expected.

    '''
    code = (
        "module alg_mod\n"
        "  use kern_mod\n"
        "  use field_mod, only : field\n"
        "  type(field) :: field1\n"
        "  contains\n"
        "  subroutine alg1()\n"
        "    call invoke(kern(1.0))\n"
        "    call not_invoke(kern(1.0))\n"
        "    call invoke(setval_c(field1,1.0))\n"
        "  end subroutine alg1\n"
        "  subroutine alg2()\n"
        "    call invoke(kern(1.0))\n"
        "  end subroutine alg2\n"
        "end module alg_mod\n")

    psyir = create_psyir(code)
    alg_trans = LFRicAlgTrans()
    assert len(psyir.walk(Call)) == 4
    assert len(psyir.walk(LFRicAlgorithmInvokeCall)) == 0
    assert len(psyir.walk(LFRicKernelFunctor)) == 0
    assert len(psyir.walk(LFRicBuiltinFunctor)) == 0

    alg_trans.apply(psyir)

    assert len(psyir.walk(Call)) == 4
    assert len(psyir.walk(LFRicAlgorithmInvokeCall)) == 3
    assert len(psyir.walk(LFRicKernelFunctor)) == 2
    assert len(psyir.walk(LFRicBuiltinFunctor)) == 1
