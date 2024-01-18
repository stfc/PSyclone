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
# Author R. W. Ford, STFC Daresbury Lab

'''Module containing LFRic-specific utility functions.'''

from psyclone.psyir.nodes import Node, Container, FileContainer
from psyclone.errors import GenerationError, InternalError


def find_container(psyir):
    '''Utility method that finds and returns the LFRic kernel container
    (module) from LFRic kernel PSyIR.

    Finds the first Container in the supplied PSyIR that is not a
    FileContainer. Also validates that the PSyIR contains at most one
    FileContainer which, if present, contains a Container.

    :param psyir: the PSyIR to search for a Container.
    :type psyir: :py:class:`psyclone.psyir.nodes.Node`

    :returns: the first Container that is not a FileContainer.
    :rtype: :py:class:`psyclone.psyir.nodes.Container`

    :raises TypeError: if the supplied psyir argument does not contain
        PSyIR.
    :raises GenerationError: if the supplied PSyIR does not contain
        any containers.
    :raises InternalError: if there are two Containers and the second is a
        FileContainer. This is invalid PSyIR.
    :raises GenerationError: if there are two Containers and the first is
        not a FileContainer.
    :raises GenerationError: if there are more than two Containers.
    :raises GenerationError: if no Containers are found.

    '''
    if not isinstance(psyir, Node):
        raise TypeError(
            f"In the find_container function, expected the 'psyir' argument "
            f"to be a PSyIR Node but found '{type(psyir).__name__}'.")

    containers = psyir.walk(Container)
    if not containers:
        raise GenerationError(
            "An LFRic kernel must have at least one Container as (modules "
            "are specified as containers) but the supplied PSyIR does not "
            "contain any.")

    if len(containers) == 1:
        if isinstance(containers[0], FileContainer):
            raise GenerationError(
                "If the LFRic kernel PSyIR contains a single container, it "
                "should not be a FileContainer (as that means the kernel "
                "source is not within a module).")
        return containers[0]

    if len(containers) == 2:
        if isinstance(containers[1], FileContainer):
            raise InternalError(
                "The supplied PSyIR contains two Containers but the innermost "
                "is a FileContainer. This is invalid PSyIR.")
        if not isinstance(containers[0], FileContainer):
            raise GenerationError(
                "The supplied PSyIR contains two Containers and the "
                "outermost one is not a FileContainer. This is not a valid "
                "LFRic kernel.")
        return containers[1]

    raise GenerationError(
        "The supplied PSyIR contains more than two Containers. This is not "
        "a valid LFRic kernel.")


def metadata_name_from_module_name(module_name):
    '''Utility that returns the expected kernel metadata name given the
    kernel module name for an LFRic kernel. It is possible to infer
    this as LFRic requires the names to be written in a particular
    form.

    :param str module_name: the name of the kernel module.

    :returns: the expected name of the kernel metadata.
    :rtype: str

    :except TypeError: if the module_name argument is not the expected
        type.
    :except GenerationError: if the module name does not conform to
        the LFRic kernel naming conventions.

    '''
    if not isinstance(module_name, str):
        raise TypeError(
            f"Expected the module_name argument to the "
            f"metadata_name_from_module_name utility to be a string, but "
            f"found '{type(module_name).__name__}'.")

    len_mod = len("_mod")
    if module_name.lower().endswith("_mod") and len(module_name) > len_mod:
        root_name = module_name[:-len_mod]
    else:
        raise GenerationError(
            f"LFRic module names should end with \"_mod\", with at least "
            f"one preceding character, but found a module called "
            f"'{module_name}'.")
    return f"{root_name}_type"
