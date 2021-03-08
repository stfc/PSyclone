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

'''Module containing tests for the translation of PSyIR to PSyclone
Algorithm PSyIR.

'''
from __future__ import absolute_import
import pytest

from fparser.two.parser import ParserFactory
from fparser.common.readfortran import FortranStringReader

from psyclone.psyir.frontend.fparser2 import Fparser2Reader
from psyclone.psyir.nodes import Reference
from psyclone.domain.common.algorithm import \
    AlgorithmInvokeCall, KernelLayerRef, psyir_to_algpsyir
from psyclone.errors import InternalError


def check_kernel(klr, name):
    '''Utility routine that checks that the kernel layer metadata
    reference argument has the expected structure.

    :param klr: the KernelLayerRef node being tested.
    :type klr: :py:class:`psyclone.domain.common.algorithm.KernelLayerRef`

    :param str name: the name of the symbol within a reference that is \
        an argument to klr.

    '''
    assert type(klr) == KernelLayerRef
    assert klr.symbol.name == name
    assert len(klr.children) == 1
    arg = klr.children[0]
    assert type(arg) == Reference
    assert arg.symbol.name == "field"


def test_invoke_kern():
    '''Test that an invoke containing a reference to metadata describing a
    kernel within a PSyclone algorithm layer is transformed into
    PSyclone-specific AlgorithmInvokeCall and KernelLayerRef classes.

    '''
    code = (
        "module algorithm_mod\n"
        "use kern_mod, only: kern\n"
        "use field_mod, only: r2d_field\n"
        "contains\n"
        "  subroutine alg()\n"
        "    type(r2d_field) :: field\n"
        "    call invoke(kern(field))\n"
        "  end subroutine alg\n"
        "end module algorithm_mod\n")

    fortran_reader = FortranStringReader(code)
    f2008_parser = ParserFactory().create(std="f2008")
    parse_tree = f2008_parser(fortran_reader)

    psyir_reader = Fparser2Reader()
    psyir = psyir_reader.generate_psyir(parse_tree)

    psyir_to_algpsyir(psyir)

    invoke = psyir.children[0][0]
    assert type(invoke) == AlgorithmInvokeCall
    assert len(invoke.children) == 1
    check_kernel(invoke.children[0], "kern")


def test_multi_invoke_kern():
    '''Test that multiple invokes each containing more than one reference
    to a kernel's metadata within a PSyclone algorithm layer are
    transformed into PSyclone-specific AlgorithmInvokeCall and
    KernelLayerRef classes.

    '''
    code = (
        "module algorithm_mod\n"
        "use field_mod, only: r2d_field\n"
        "use kern_mod, only: kern1, kern2\n"
        "contains\n"
        "  subroutine alg()\n"
        "    type(r2d_field) :: field\n"
        "    call invoke(kern1(field), &\n"
        "                kern2(field))\n"
        "    call invoke(kern1(field), &\n"
        "                kern2(field))\n"
        "  end subroutine alg\n"
        "end module algorithm_mod\n")

    fortran_reader = FortranStringReader(code)
    f2008_parser = ParserFactory().create(std="f2008")
    parse_tree = f2008_parser(fortran_reader)

    psyir_reader = Fparser2Reader()
    psyir = psyir_reader.generate_psyir(parse_tree)

    psyir_to_algpsyir(psyir)

    for index in [0, 1]:
        invoke = psyir.children[0][index]
        assert type(invoke) == AlgorithmInvokeCall
        assert len(invoke.children) == 2
        check_kernel(invoke.children[0], "kern1")
        check_kernel(invoke.children[1], "kern2")


def test_arg_error():
    '''Test that we get the expected exception when a ****

    '''
    code = (
        "module algorithm_mod\n"
        "contains\n"
        "  subroutine alg()\n"
        "    call invoke('hello')\n"
        "  end subroutine alg\n"
        "end module algorithm_mod\n")

    fortran_reader = FortranStringReader(code)
    f2008_parser = ParserFactory().create(std="f2008")
    parse_tree = f2008_parser(fortran_reader)

    psyir_reader = Fparser2Reader()
    psyir = psyir_reader.generate_psyir(parse_tree)

    with pytest.raises(InternalError) as info:
        psyir_to_algpsyir(psyir)
    assert ("Unsupported argument type found. Expecting a reference to kernel "
            "metadata, but found 'Literal'." in str(info.value))
