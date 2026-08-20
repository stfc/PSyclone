# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2017-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Contains a Python function to generate an empty kernel
    subroutine with the required arguments and datatypes (which we
    call a stub) when presented with Kernel Metadata.
'''

from __future__ import print_function
import os

from psyclone.domain.common.kernel import KernelInfo
from psyclone.domain.lfric import LFRicKern, LFRicKernelMetadata
from psyclone.errors import GenerationError
from psyclone.parse.utils import ParseError
from psyclone.configuration import Config, LFRIC_API_NAMES
from psyclone.psyir.backend.fortran import FortranWriter


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

    :returns: the kernel stub of the metadata in the given kernel file.
    :rtype: str

    :raises GenerationError: if an invalid stub API is specified.
    :raises IOError: if filename does not specify a file.
    :raises ParseError: if the given file could not be parsed.

    '''
    if api not in LFRIC_API_NAMES:
        raise GenerationError(
            f"Kernel stub generator: Unsupported API '{api}' specified. "
            f"Supported APIs are {LFRIC_API_NAMES[0]}.")
    Config.get().api = api

    if not os.path.isfile(filename):
        raise IOError(f"Kernel stub generator: File '{filename}' not found.")

    try:
        kernel_info = KernelInfo.create_from_file(
            LFRicKernelMetadata, filename)
    except ParseError as error:
        raise ParseError(f"Kernel stub generator: Code appears to be invalid "
                         f"Fortran: {error}.") from error

    kernel = LFRicKern()
    kernel.load_meta(kernel_info.metadata)

    return FortranWriter()(kernel.gen_stub)
