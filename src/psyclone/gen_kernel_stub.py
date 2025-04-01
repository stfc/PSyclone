# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2025, Science and Technology Facilities Council
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
# Author R. Ford STFC Daresbury Lab
# Modified work Copyright (c) 2017 by J. Henrichs, Bureau of Meteorology
# Modified: I. Kavcic and L. Turner, Met Office
#           A. R. Porter and N. Nobre, STFC Daresbury Lab

''' Contains a Python function to generate an empty kernel
    subroutine with the required arguments and datatypes (which we
    call a stub) when presented with Kernel Metadata.
'''

import os
import re

import fparser
from psyclone.domain.lfric import (
    LFRicKern, LFRicKernMetadata, FormalKernelArgsFromMetadata)
from psyclone.domain.lfric.kernel import LFRicKernelMetadata
from psyclone.errors import GenerationError
from psyclone.parse.utils import ParseError
from psyclone.configuration import Config, LFRIC_API_NAMES


def generate(filename, api=""):
    '''
    Generates an empty kernel subroutine with the required arguments
    and datatypes (which we call a stub) when presented with Kernel
    Metadata. This is useful for Kernel developers to make sure
    they are using the correct arguments in the correct order.  The
    Kernel Metadata must be presented in the standard Kernel
    format.

    :param str filename: the name of the file for which to create a
                         kernel stub for.
    :param str api: the name of the API for which to create a kernel
                    stub. Must be one of the supported stub APIs.

    :returns: root of fparser1 parse tree for the stub routine.
    :rtype: :py:class:`fparser.one.block_statements.Module`

    :raises GenerationError: if an invalid stub API is specified.
    :raises IOError: if filename does not specify a file.
    :raises ParseError: if the given file could not be parsed.

    '''
    if api not in LFRIC_API_NAMES:
        raise GenerationError(
            f"Kernel stub generator: Unsupported API '{api}' specified. "
            f"Supported APIs are {LFRIC_API_NAMES[0]}.")
    else:
        Config.get().api = api

    if not os.path.isfile(filename):
        raise IOError(f"Kernel stub generator: File '{filename}' not found.")

    from psyclone.psyir.frontend import fortran
    from psyclone.psyir import nodes
    from psyclone.psyir.symbols import DataTypeSymbol
    from psyclone.errors import InternalError
    freader = fortran.FortranReader()
    try:
        kern_psyir = freader.psyir_from_file(filename)
    except ValueError as err:
        raise ParseError(f"Kernel stub generator: Code appears to be invalid "
                         f"Fortran: {err}.") from err

    table = kern_psyir.children[0].symbol_table
    for sym in table.symbols:
        if isinstance(sym, DataTypeSymbol) and not sym.is_import:
            break
    else:
        raise InternalError("No DataTypeSymbol found.")

    metadata = LFRicKernelMetadata.create_from_psyir(sym)
    new_table = FormalKernelArgsFromMetadata.mapping(metadata)
    mod_name = re.sub(r"_type$", r"_mod", sym.name)
    new_container = nodes.Container(mod_name)
    # Add the metadata
    new_container.symbol_table.add(sym)
    kern_name = metadata.procedure_name
    new_routine = nodes.Routine.create(kern_name, new_table, [])
    new_container.addchild(new_routine)
    return new_container
