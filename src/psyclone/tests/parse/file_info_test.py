# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2024-2025, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Laboratory.


"""Module containing tests for the FileInfo class."""

import logging
import os
import pytest

from psyclone.psyir.nodes import Node
from psyclone.parse import FileInfo, FileInfoFParserError

SOURCE_DUMMY = """\
program main
    real :: a
    a = 0.0
end program main
"""


def test_file_info_constructor():
    """
    Check that a FileInfo can be constructed and that its initial
    properties are set correctly.

    """
    finfo: FileInfo = FileInfo("missing.txt")
    assert finfo._filename == "missing.txt"
    assert finfo._source_code is None
    assert finfo.filename == "missing.txt"
    assert finfo.basename == "missing"


def test_file_info_missing_file(caplog):
    """
    Test FileInfo.source() performs the expected logging and then raises
    the expected exception if the file cannot be found.

    """
    finfo = FileInfo("missing.txt")
    with caplog.at_level(logging.INFO):
        with pytest.raises(FileNotFoundError) as err:
            _ = finfo.get_source_code()
    assert "Source file 'missing.txt': loading source code" in caplog.text
    assert "'missing.txt'" in str(err.value)


def test_file_info_cached_source_code(tmpdir, caplog):
    """
    Check that the contents of the file have been cached
    and that the cache was used if reading it a 2nd time.
    """
    fname = os.path.join(tmpdir, "a_file.txt")
    content = "module andy\n\nend module"
    with open(fname, "w", encoding="utf-8") as fout:
        fout.write(content)
    finfo = FileInfo(fname, cache_active=True)
    with caplog.at_level(logging.INFO):
        input1 = finfo.get_source_code()
    assert input1 == content
    assert "a_file.txt': loaded OK" in caplog.text
    # Check that the contents have been cached.
    caplog.clear()
    with caplog.at_level(logging.INFO):
        input2 = finfo.get_source_code()
    assert caplog.text == ""
    assert input2 is input1
    assert finfo._source_code_hash_sum is not None
    assert finfo._cache_data_load is None
    assert finfo._cache_data_save is None

    # Load fparser tree to start caching
    with caplog.at_level(logging.INFO):
        finfo.get_fparser_tree()
    assert finfo._cache_data_save is not None
    assert "No cache file '" in caplog.text
    assert "Cache file updated with hashsum " in caplog.text

    caplog.clear()
    with caplog.at_level(logging.INFO):
        finfo = FileInfo(fname, cache_active=True)
        input1 = finfo.get_fparser_tree()
    assert finfo._cache_data_load is not None
    assert finfo._cache_data_save is None
    assert "Using cache file '" in caplog.text
    assert "Using cache of fparser tree with hashsum " in caplog.text


def test_file_info_no_cached_source_code(tmpdir):
    """
    Check that the contents of the file have not been cached.
    """
    fname = os.path.join(tmpdir, "a_file.txt")
    content = "module andy\n\nend module"
    with open(fname, "w", encoding="utf-8") as fout:
        fout.write(content)
    finfo = FileInfo(fname)
    finfo.get_source_code()
    assert finfo._source_code_hash_sum is None
    assert finfo._cache_data_load is None
    assert finfo._cache_data_save is None

    # Load fparser tree and check that no caching is performed.
    finfo.get_fparser_tree()
    assert finfo._cache_data_save is None

    finfo = FileInfo(fname)
    finfo.get_fparser_tree()
    assert finfo._cache_data_load is None
    assert finfo._cache_data_save is None


def test_file_info_decode_error(tmpdir):
    """
    Check that FileInfo.get_source_code() handles a decoding error when reading
    a file.

    """
    fname = os.path.join(tmpdir, "a_file.txt")
    # Test content includes a byte that cannot be decoded as utf-8.
    content = "Ju\xb5st\nA\nTest"
    with open(fname, "w", encoding="latin") as fout:
        fout.write(content)
    finfo = FileInfo(fname)
    # Content of file has been read with problematic byte skipped.
    assert finfo.get_source_code() == "Just\nA\nTest"


def test_file_info_source_fparser_psyir(tmpdir):
    """
    Check that read source, fparser and psyir always result
    in the same objects.

    """
    filename = os.path.join(tmpdir, "testfile_a.f90")
    try:
        os.remove(filename)
    except FileNotFoundError:
        pass
    with open(filename, "w", encoding="utf-8") as fout:
        fout.write(SOURCE_DUMMY)

    file_info: FileInfo = FileInfo(filename, cache_active=True)

    source_code = file_info.get_source_code()
    source_code2 = file_info.get_source_code()
    assert source_code is source_code2

    fparser_tree = file_info.get_fparser_tree()
    fparser_tree2 = file_info.get_fparser_tree()
    assert fparser_tree is fparser_tree2

    psyir_node = file_info.get_psyir()
    psyir_node2 = file_info.get_psyir()
    assert psyir_node is psyir_node2

    # For coverage check
    file_info._cache_save()


def test_file_info_load_from_cache(tmpdir):
    """
    Check that the caching to file really works.

    """
    filename = os.path.join(tmpdir, "testfile_b.f90")
    filename_cache = os.path.join(tmpdir, "testfile_b.psyclone")

    try:
        os.remove(filename)
    except FileNotFoundError:
        pass

    try:
        os.remove(filename_cache)
    except FileNotFoundError:
        pass

    with open(filename, "w", encoding="utf-8") as fout:
        fout.write(SOURCE_DUMMY)

    file_info: FileInfo = FileInfo(filename, cache_active=True)

    # Call save_cache just for code coverage
    file_info._cache_save()
    file_info._cache_load()

    # No cache exists
    assert file_info._cache_data_load is None
    assert file_info._cache_data_save is None

    # Load file which triggers storing things to cache
    psyir_node = file_info.get_psyir()
    assert isinstance(psyir_node, Node)

    # Save cache is used
    assert file_info._cache_data_load is None
    assert file_info._cache_data_save is not None

    # Load new file
    file_info: FileInfo = FileInfo(filename, cache_active=True)

    # No cache used
    assert file_info._cache_data_load is None
    assert file_info._cache_data_save is None

    # Load file which triggers storing things to cache
    psyir_node = file_info.get_psyir()
    assert isinstance(psyir_node, Node)

    # Loaded and saved to cache
    assert file_info._cache_data_load is not None
    assert file_info._cache_data_save is None


def test_file_info_load_from_cache_corrupted(tmpdir, caplog):
    """
    Test handling of corrupt cache file

    """
    filename = os.path.join(tmpdir, "testfile_c.f90")
    filename_cache = os.path.join(tmpdir, "testfile_c.psyclone")

    with open(filename, "w", encoding="utf-8") as fout:
        fout.write(SOURCE_DUMMY)

    filename_cache = os.path.join(tmpdir, "testfile_c.psycache")
    with open(filename_cache, "w", encoding="utf-8") as fout:
        fout.write("GARBAGE")

    #
    # Step 1) Load with corrupted cache file
    #

    file_info: FileInfo = FileInfo(filename, cache_active=True)

    # No cache exists
    assert file_info._cache_data_load is None
    assert file_info._cache_data_save is None

    # Load with damaged cache file
    with caplog.at_level(logging.INFO):
        psyir_node = file_info.get_psyir()
    assert isinstance(psyir_node, Node)
    assert "Error while reading cache file - ignoring: " in caplog.text
    assert "testfile_c.f90': Running fparser" in caplog.text
    assert "Cache file updated with hashsum " in caplog.text

    # No cache exists
    assert file_info._cache_data_load is None
    assert file_info._cache_data_save is not None

    #
    # Step 2) Cache file should now be restored
    #

    file_info: FileInfo = FileInfo(filename, cache_active=True)

    # No cache exists
    assert file_info._cache_data_load is None
    assert file_info._cache_data_save is None

    # Load with damaged cache file
    psyir_node = file_info.get_psyir()
    assert isinstance(psyir_node, Node)

    # Cache was loaded
    assert file_info._cache_data_load is not None

    # Cache not updated
    assert file_info._cache_data_save is None


def test_file_info_source_changed(tmpdir, caplog):
    """
    Make sure that cache is not used if source code
    file changed, hence, its checksum.

    """
    filename = os.path.join(tmpdir, "testfile_d.f90")

    with open(filename, "w", encoding="utf-8") as fout:
        fout.write(SOURCE_DUMMY)

    #
    # Step 1) Create cache file
    #

    file_info: FileInfo = FileInfo(filename, cache_active=True)
    psyir_node = file_info.get_psyir()
    assert isinstance(psyir_node, Node)

    assert file_info._cache_data_load is None
    assert file_info._cache_data_save is not None

    #
    # Step 2) Change source code
    #

    with open(filename, "w", encoding="utf-8") as fout:
        fout.write(SOURCE_DUMMY + "\n! I'm a comment to change the hashsum")

    #
    # Step 3) Cache file should not be used
    #

    file_info: FileInfo = FileInfo(filename, cache_active=True)

    # Load, but not from cache
    with caplog.at_level(logging.INFO):
        psyir_node = file_info.get_psyir()
    assert "Cache hashsum mismatch: source " in caplog.text
    assert "Cache file updated with hashsum " in caplog.text

    # Cache was not loaded
    assert file_info._cache_data_load is None

    # Cache updated
    assert file_info._cache_data_save is not None


def test_file_info_source_with_bugs(tmpdir):
    """
    Make sure that error is triggered if there's an error.

    """
    filename = os.path.join(tmpdir, "testfile_bug.f90")

    with open(filename, "w", encoding="utf-8") as fout:
        fout.write(SOURCE_DUMMY + "arbitrary words to trigger error")

    file_info: FileInfo = FileInfo(filename, cache_active=True)

    with pytest.raises(FileInfoFParserError) as einfo:
        file_info.get_psyir()

    assert ("FileInfoFParserError: Failed to create"
            " fparser tree: at line 5") in (
        str(einfo.value))

    # Call it a 2nd time for coverage of not attempting to create it a 2nd time
    with pytest.raises(FileInfoFParserError) as einfo:
        file_info.get_psyir()
    assert "previous attempt failed" in str(einfo.value)


def test_file_info_cachefile_not_writable(tmpdir, caplog):
    """
    Test for a cachefile that is not writable (can't be created).

    """
    filename = os.path.join(tmpdir, "testfile_e.f90")

    with open(filename, "w", encoding="utf-8") as fout:
        fout.write(SOURCE_DUMMY)

    file_info: FileInfo = FileInfo(filename, cache_active=True)

    # Overwrite the path of the cache file to one which
    # can't be simply created (non-existing path)
    file_info._cache_path = "/I_DONT_EXIST_PATH/FILE/cache.psycache"

    # If only the source code is requested, this won't
    # raise any errors since no cache is used for this,
    # but it just returns the source code.
    source_code = file_info.get_source_code()
    assert source_code == SOURCE_DUMMY

    # If the psyir, hence, fparser tree is requested, creating
    # the cache will fail, but the psyir node itself will
    # still be returned.
    with caplog.at_level(logging.WARN):
        psyir_node = file_info.get_psyir()
    assert isinstance(psyir_node, Node)
    assert "Unable to write to cache file: " in caplog.text


def test_file_info_cachefile_pickle_dump_exception(tmpdir, monkeypatch,
                                                   caplog):
    """
    Check that we handle any exception raised during pickling.

    """
    filename = os.path.join(tmpdir, "testfile_f.f90")
    with open(filename, "w", encoding="utf-8") as fout:
        fout.write(SOURCE_DUMMY)

    file_info: FileInfo = FileInfo(filename, cache_active=True)

    def fun_exception(a, b):
        raise Exception()

    monkeypatch.setattr("pickle.dump", fun_exception)

    with caplog.at_level(logging.WARN):
        psyir_node = file_info.get_psyir()
    assert isinstance(psyir_node, Node)
    assert "Error while storing cache data - ignoring:" in caplog.text

    assert file_info._cache_data_load is None
    assert file_info._cache_data_save is None


def test_file_info_source_psyir_test(tmpdir):
    """
    Here, we generate a dummy psyir cached node to load it.
    This is not yet possible (TODO #2786), but the code
    for this already exists and is tested here.

    """
    filename = os.path.join(tmpdir, "testfile_g.f90")

    with open(filename, "w", encoding="utf-8") as fout:
        fout.write(SOURCE_DUMMY)

    # Create cache
    file_info: FileInfo = FileInfo(filename, cache_active=True)
    file_info.get_psyir()

    # Load again for coverage case
    file_info.get_psyir()

    # Load from cache
    file_info: FileInfo = FileInfo(filename, cache_active=True)

    fparser_tree = file_info.get_fparser_tree()
    fparser_tree2 = file_info.get_fparser_tree()
    assert fparser_tree is fparser_tree2

    psyir_node = Node("dummy")
    file_info._cache_data_load._psyir_node = psyir_node

    psyir_node2 = file_info.get_psyir()
    assert psyir_node is psyir_node2


def test_fparser_error():
    """
    Test that fparser raises an FileInfoFParserError
    error if a file was not found
    """
    file_info = FileInfo(filepath="dummy")

    # Catch special exception
    with pytest.raises(FileInfoFParserError) as einfo:
        file_info.get_fparser_tree()

    assert "FileInfo: No such file or directory 'dummy'" in str(
        einfo.value
    )
