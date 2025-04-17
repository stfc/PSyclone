# -----------------------------------------------------------------------------
# Copyright (c) 2022-2023 Science and Technology Facilities Council.
# All rights reserved.
#
# Modifications made as part of the fparser project are distributed
# under the following license:
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.
#
# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.
#
# 3. Neither the name of the copyright holder nor the names of its
# contributors may be used to endorse or promote products derived from
# this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# -----------------------------------------------------------------------------

""" Module containing tests for the ModuleUse class. """

import pytest
from fparser.two.symbol_table import ModuleUse


def test_moduse_constructor():
    """Checks for the ModuleUse constructor."""
    # Name only.
    muse1 = ModuleUse("Nancy")
    assert muse1.name == "nancy"
    assert muse1.only_list is None
    assert muse1.rename_list is None
    assert muse1.wildcard_import is True
    # pylint: disable=use-implicit-booleaness-not-comparison
    assert muse1.symbol_names == []
    # Only-list.
    muse2 = ModuleUse("peggy", only_list=[("amazon", None), ("swallow", None)])
    assert sorted(muse2.only_list) == ["amazon", "swallow"]
    assert muse2.wildcard_import is False
    assert muse2.rename_list is None
    assert muse2.symbol_names == ["amazon", "swallow"]
    # Only-list with renaming.
    muse3 = ModuleUse("roger", only_list=[("swallow", None), ("amazon", "dingy")])
    assert sorted(muse3.only_list) == ["amazon", "swallow"]
    assert muse3.get_declared_name("amazon") == "dingy"
    assert muse3.symbol_names == ["swallow", "amazon"]
    # Rename list.
    muse4 = ModuleUse("susan", rename_list=[("swallow", "amazon"), ("boat", "dingy")])
    assert sorted(muse4.rename_list) == ["boat", "swallow"]
    assert muse4.get_declared_name("swallow") == "amazon"
    assert muse4.get_declared_name("boat") == "dingy"
    assert muse4.symbol_names == ["swallow", "boat"]


def test_moduse_constructor_validate():
    """Checks for the type-checking performed by the constructor."""
    # Module name.
    with pytest.raises(TypeError) as err:
        _ = ModuleUse(1)
    assert "name of the module must be a str but got" in str(err.value)
    # Check that it calls _validate_tuple_list.
    with pytest.raises(TypeError) as err:
        _ = ModuleUse("titty", only_list="polly")
    assert "If present, the only_list must be a list but got 'str'" in str(err.value)
    with pytest.raises(TypeError) as err:
        _ = ModuleUse("titty", rename_list="polly")
    assert "If present, the rename_list must be a list but got 'str'" in str(err.value)
    with pytest.raises(TypeError) as err:
        _ = ModuleUse("titty", only_list=[("polly", 1)])
    assert (
        "If present, the only_list must be a list of 2-tuples of "
        "(str, str | NoneType) but got: [('polly', 1)]" in str(err.value)
    )
    with pytest.raises(TypeError) as err:
        _ = ModuleUse("titty", rename_list=[("polly", 1)])
    assert (
        "If present, the rename_list must be a list of 2-tuples of "
        "(str, str) but got: [('polly', 1)]" in str(err.value)
    )


def test_moduse_validate_tuple_list():
    """Checks for the _validate_tuple_list() method."""
    with pytest.raises(TypeError) as err:
        _ = ModuleUse._validate_tuple_list("only", ["polly"])
    assert (
        "If present, the only_list must be a list of 2-tuples but "
        "got: ['polly']" in str(err.value)
    )
    with pytest.raises(TypeError) as err:
        _ = ModuleUse._validate_tuple_list("only", [("polly", "gibber", 1)])
    assert (
        "If present, the only_list must be a list of 2-tuples but "
        "got: [('polly', 'gibber', 1)]" in str(err.value)
    )


def test_moduse_update():
    """Checks for the update() method."""
    moduse = ModuleUse("flint")
    assert moduse.wildcard_import is True
    # Wrong argument type.
    with pytest.raises(TypeError) as err:
        moduse.update("hello")
    assert (
        "update() must be supplied with an instance of ModuleUse but got "
        "'str'" in str(err.value)
    )
    # Wrong module name.
    moduse1 = ModuleUse("houseboat", only_list=[("cannon", None)])
    with pytest.raises(ValueError) as err:
        moduse.update(moduse1)
    assert (
        "ModuleUse supplied to update() is for module 'houseboat' but this "
        "ModuleUse is for module 'flint'" in str(err.value)
    )
    # Add an import with an ONLY: clause.
    moduse2 = ModuleUse("flint", only_list=[("cannon", None)])
    moduse.update(moduse2)
    assert moduse.wildcard_import is True
    assert moduse.only_list == ["cannon"]
    assert moduse.rename_list is None
    # Add an import with a Rename_List.
    moduse3 = ModuleUse("Flint", rename_list=[("uncle", "jim")])
    moduse.update(moduse3)
    assert moduse.only_list == ["cannon"]
    assert moduse.rename_list == ["uncle"]
    assert moduse.get_declared_name("uncle") == "jim"
    assert moduse.symbol_names == ["cannon", "uncle"]
    # Add further Only and Rename lists.
    moduse4 = ModuleUse("Flint", rename_list=[("rogEr", "giBber")])
    moduse.update(moduse4)
    assert moduse.get_declared_name("uncle") == "jim"
    assert moduse.get_declared_name("roger") == "gibber"
    assert moduse.symbol_names == ["cannon", "uncle", "roger"]
    assert sorted(moduse.rename_list) == ["roger", "uncle"]
    moduse5 = ModuleUse("Flint", only_list=[("maTe", "peggy")])
    moduse.update(moduse5)
    assert sorted(moduse.only_list) == ["cannon", "mate"]
    assert moduse.get_declared_name("roger") == "gibber"
    assert moduse.get_declared_name("mate") == "peggy"
    assert moduse.symbol_names == ["cannon", "uncle", "roger", "mate"]
    # Adding a wildcard import.
    moduse6 = ModuleUse("flInt")
    moduse5.update(moduse6)
    assert moduse5.wildcard_import is True


def test_moduse_lookup():
    """Tests for the lookup() method."""
    moduse = ModuleUse("flint")
    with pytest.raises(KeyError):
        moduse.lookup("polly")
    moduse2 = ModuleUse("flint", only_list=[("mate", None), ("cannON", None)])
    sym = moduse2.lookup("caNNon")
    assert sym.name == "cannon"
    # As this symbol is imported, we don't know its type.
    assert sym.primitive_type == "unknown"
