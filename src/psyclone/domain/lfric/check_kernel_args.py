# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023, Science and Technology Facilities Council.
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

'''This module implements a function that checks that LFRic kernel
arguments are consistent with their metadata.

'''
from psyclone.domain.lfric.kernel import LFRicKernelContainer
from psyclone.domain.lfric.num_kernel_args import NumKernelArgs
from psyclone.errors import GenerationError
from psyclone.psyir.nodes import Container, Routine


def get_kernel_metadata(kernel):
    '''Utility method to extract the kernel metadata from an LFRic
    kernel.

    :param kernel: LFRic kernel PSyIR.
    :type kernel: :py:class:`psyclone.psyir.nodes.Container`

    :returns: LFRic kernel metadata.
    :rtype: :py:class:`psyclone.domain.lfric.kernel.LFRicKernelMetadata`

    :raises TransformationError: if the supplied kernel argument \
        is not a PSyIR container.
    :raises TransformationError: if the supplied kernel argument \
        does not contain an LFRicKernelContainer as the root node \
        or first child of the root node.

    '''
    if not isinstance(kernel, Container):
        raise Exception(
            f"A PSyIR Container (an LFRic kernel module) was expected, "
            f"but found '{type(kernel).__name__}'.")
    if isinstance(kernel, LFRicKernelContainer):
        kernel_metadata = kernel.metadata
    elif kernel.children and isinstance(
            kernel.children[0], LFRicKernelContainer):
        kernel_metadata = kernel.children[0].metadata
    else:
        raise Exception(
            "LFRic kernel PSyIR should contain an "
            "LFRicKernelContainer as the root or first child of the "
            "root but this was not found.")
    return kernel_metadata


def get_kernel_routine(kernel, routine_name):
    ''' xxx '''
    # TODO Check kernel and routine_name arguments
    for routine in kernel.walk(Routine):
        if routine.name.lower() == routine_name.lower():
            return routine
    raise Exception(
        f"Routine {routine_name} was not found in kernel {kernel.name}. Is "
        f"it an interface?")


def check_kernel_args(kernel):
    '''Check whether LFRic kernel arguments are consistent with their
    metadata.

    # TODO #xxx Check argument types and intents match

    :param kernel: the kernel to check.
    :type kernel: :py:class:`psyclone.psyir.nodes.Container`

    :raises GenerationError: if the number of arguments specified by
        the metadata does not match the number of arguments in the
        kernel routine.

    '''
    if not isinstance(kernel, Container):
        raise TypeError(
            f"kernel argument to check_kernel_args function should be a "
            f"Container but found '{type(kernel).__name__}'.")
    kernel_metadata = get_kernel_metadata(kernel)
    routine_name = kernel_metadata.procedure_name
    kernel_routine = get_kernel_routine(kernel, routine_name)
    kernel_args = kernel_routine.symbol_table.argument_list
    num_kernel_args = len(kernel_args)
    expected_num_kernel_args = NumKernelArgs.mapping(kernel_metadata)
    if expected_num_kernel_args != num_kernel_args:
        raise GenerationError(
            f"The kernel metadata in '{kernel_metadata.name}' specifies that "
            f"there should be {expected_num_kernel_args} kernel arguments, "
            f"but the kernel routine '{routine_name}' has {num_kernel_args}.")
