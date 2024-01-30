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

'''Tests for the utils module containing LFRic-specific utility functions.'''

import pytest

from psyclone.domain.lfric.utils import (
    find_container, metadata_name_from_module_name)
from psyclone.errors import GenerationError, InternalError
from psyclone.psyir.nodes import Routine, FileContainer, Container
from psyclone.psyir.symbols import SymbolTable


# find_container

def test_find_container_not_node():
    '''Test that the find_container utility raises an exception if the
    provided argument is not a PSyIR Node. '''
    with pytest.raises(TypeError) as info:
        _ = find_container(None)
    assert ("In the find_container function, expected the 'psyir' argument "
            "to be a PSyIR Node but found 'NoneType'." in str(info.value))


def test_find_container_no_container():
    '''Test that the find_container utility raises an exception if the
    provided PSyIR does not contain a Container (i.e. there must be a
    module in LFRic kernels).

    '''
    with pytest.raises(GenerationError) as info:
        _ = find_container(Routine.create("test", SymbolTable(), []))
    assert ("An LFRic kernel must have at least one Container as (modules "
            "are specified as containers) but the supplied PSyIR does not "
            "contain any." in str(info.value))


def test_find_container_no_module():
    '''Test that the find_container utility raises an exception if the
    provided PSyIR contains a single Container but that is a
    FileContainer (i.e. there must be a module in LFRic kernels).

    '''
    with pytest.raises(GenerationError) as info:
        _ = find_container(FileContainer.create("filename", SymbolTable(), []))
    assert ("If the LFRic kernel PSyIR contains a single container, it should "
            "not be a FileContainer (as that means the kernel source is "
            "not within a module)." in str(info.value))


def test_find_container_inner_filecontainer():
    '''Test that the find_container utility raises an exception if the
    provided PSyIR contains two Containers but the inner one is a
    FileContainer (this should be a generic container for a module).

    '''
    psyir = FileContainer.create("filename", SymbolTable(), [])
    file_container = FileContainer.create("filename2", SymbolTable(), [])
    psyir.children.append(file_container)
    with pytest.raises(InternalError) as info:
        _ = find_container(psyir)
    assert ("The supplied PSyIR contains two Containers but the innermost "
            "is a FileContainer. This is invalid PSyIR." in str(info.value))


def test_find_container_outer_not_filecontainer():
    '''Test that the find_container utility raises an exception if the
    provided PSyIR does contains two Containers but the outer
    container is not a FileContainer.

    '''
    psyir = Container.create("mod_name", SymbolTable(), [])
    container = Container.create("mod_name2", SymbolTable(), [])
    psyir.children.append(container)
    with pytest.raises(GenerationError) as info:
        _ = find_container(psyir)
    assert ("The supplied PSyIR contains two Containers and the outermost "
            "one is not a FileContainer. This is not a valid LFRic kernel."
            in str(info.value))


def test_find_container_multi_module():
    '''Test that the find_container utility raises an exception if the
    provided PSyIR contains more than two Containers as this is an
    invalid LFRic kernel.

    '''
    container1 = Container.create("mod_name1", SymbolTable(), [])
    container2 = Container.create("mod_name2", SymbolTable(), [])
    psyir = FileContainer.create(
        "filename", SymbolTable(), [container1, container2])
    with pytest.raises(GenerationError) as info:
        _ = find_container(psyir)
    assert ("The supplied PSyIR contains more than two Containers. This is "
            "not a valid LFRic kernel." in str(info.value))


def test_find_container_working():
    '''Test that the find_container utility find the correct Container,
    independent of starting point.

    '''
    module = Container.create("mod_name", SymbolTable(), [])
    psyir = FileContainer.create(
        "filename", SymbolTable(), [module])
    result = find_container(psyir)
    assert result is module
    result = find_container(module)
    assert result is module


# metadata_name_from_module_name

def test_metadata_name_wrong_type():
    '''Test that the expected exception is raised if the supplied module
    name is the wrong type.

    '''
    with pytest.raises(TypeError) as info:
        _ = metadata_name_from_module_name(None)
    assert ("Expected the module_name argument to the "
            "metadata_name_from_module_name utility to be a string, but "
            "found 'NoneType'." in str(info.value))


@pytest.mark.parametrize("name", ["", "_mod", "module"])
def test_metadata_name_no_mod(name):
    '''Test that the expected exception is raised if the supplied module
    name does not end in _mod or does not have at least one character
    that precedes _mod.

    '''
    with pytest.raises(GenerationError) as info:
        _ = metadata_name_from_module_name(name)
    assert (f"LFRic module names should end with \"_mod\", with at least "
            f"one preceding character, but found a module called '{name}'."
            in str(info.value))


@pytest.mark.parametrize("name,expected", [
    ("example_mod", "example_type"), ("x_mod", "x_type"),
    ("something_mod_mod", "something_mod_type")])
def test_metadata_name_ok(name, expected):
    '''Test that the funtion returns the expected output if valid input is
    provided (*_mod in the input is output as *_type).

    '''
    result = metadata_name_from_module_name(name)
    assert result == expected
