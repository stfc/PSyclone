# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Tests for the utils module containing LFRic-specific utility functions.'''

import pytest

from psyclone.domain.lfric.utils import find_container
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
