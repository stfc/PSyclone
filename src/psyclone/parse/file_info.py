# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2024, Science and Technology Facilities Council.
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
# Modifications: M. Schreiber, Univ. Grenoble Alpes

"""This module contains the FileInfo class.

"""

import os
import hashlib
import pickle
import copy
from typing import List, Union
from psyclone.psyir.frontend.fortran import FortranReader, FortranStringReader
from fparser.two import Fortran2003
from fparser.two.parser import ParserFactory
from psyclone.configuration import Config
from psyclone.psyir.nodes.file_container import FileContainer
from psyclone.errors import PSycloneError


class FileInfoFParserError(PSycloneError):
    """Triggered when generation of FParser tree failed"""

    def __init__(self, value: str):
        super().__init__(value)
        self.value = "FParser Error: " + str(value)


class _CacheFileInfo:
    def __init__(self):
        # Hash sum
        self._source_code_hash_sum: hashlib._Hash = None

        # Fparser tree
        self._fparser_tree: Fortran2003 = None

        # Psyir node
        self._psyir_node: FileContainer = None


class FileInfo:
    """This class stores mostly cached information about files:
    - it stores the original filename
    - it will read the source of the file and cache it
    - it will parse it with fparser and cache it
    - it will parse it with psyir and cache it

    :param str filename: the name of the source file (including path) that this
                         object holds information on.

    """

    def __init__(
        self, filepath: str, cache_active: bool = False, cache_path: str = None
    ):
        """Constructor

        :param filepath: Path to the file containing the source code
        :type filepath: str

        :param cache_active: Cache the fparser tree (TODO: psyir
            once psyir pickling is supported)
        :type cache_active: bool

        """
        # Full path to file
        self._filepath: str = filepath

        # Cache active
        self._cache_active = cache_active

        # Path where to store cache files
        self._cache_path = cache_path

        # Source code:
        self._source_code: str = None

        # Source code hash sum:
        self._source_code_hash_sum: hashlib._Hash = None

        # Fparser node
        self._fparser_tree: Fortran2003 = None

        # Psyir node
        self._psyir_node: FileContainer = None

        # List of modules in file
        # We want to have this ordered to search in the same order
        # as in the source code, hence, use a list.
        self._module_name_list: List[str] = None

        # Cache with data
        self._cache: _CacheFileInfo = None

        # Single cache file
        self._filepath_cache = None

    def _get_filepath_cache(self):
        """Return the filepath of the cache.

        This can't be done in the constructor since the hashcode
        of the source code is required first.
        """

        assert self._source_code_hash_sum is not None

        if self._cache_active:
            if self._cache_path is None or self._cache_path == "":
                # If cache path is not specified, we use the source code path
                # E.g.,
                # path/to/file.f90 => path/to/file.psycache
                (filepath_no_ext, _) = os.path.splitext(self._filepath)
                self._filepath_cache = filepath_no_ext + ".psycache"

            else:
                (path, _) = os.path.split(self._filepath)
                self._filepath_cache = (
                    path + self._source_code_hash_sum[:55] + ".psycache"
                )

            return self._filepath_cache

        raise Exception("Cache file path requested, but caching disabled")

    def get_basename(self) -> str:
        """
        :returns: the base name (i.e. without path or suffix) of the filename
                  that this FileInfo object represents.
        :rtype: str

        """

        if self._filepath is None:
            return None

        # Remove the path from the filename.
        basename = os.path.basename(self._filepath)
        # splitext returns (root, ext) and it's `root` that we want.
        return os.path.splitext(basename)[0]

    def get_filepath(self) -> str:
        """
        :returns: the full filename with the path that this FileInfo object
            represents.
        :rtype: str

        """
        return self._filepath

    def get_module_name_list(self) -> Union[str, None]:
        """
        :returns: a list of all module names
        :rtype: List[str] | None

        """
        return self._module_name_list

    def get_source_code(self, verbose: bool = False) -> str:
        """Returns the contents of the file. The first time, it
        will be read from the file, but the data is then cached.

        If any decoding errors are encountered then the associated character(s)
        are simply skipped. This is because this class is intended for reading
        Fortran source and the only way such characters can appear is if they
        are in comments.

        :param verbose: Produce some verbose output
        :type verbose: str

        :returns: the contents of the file (utf-8 encoding).
        :rtype: str

        """
        if self._source_code is None:
            # Specifying errors='ignore' simply skips any characters that
            # result in decoding errors. (Comments in a code may contain all
            # sorts of weird things.)

            if verbose:
                print(
                    f"- Source file '{self._filepath}': "
                    f"Loading source code"
                )

            try:
                with open(
                    self._filepath, "r", encoding="utf-8", errors="ignore"
                ) as file_in:
                    self._source_code = file_in.read()
            except FileNotFoundError:
                raise FileNotFoundError(
                    f"No such file or directory: '{self._filepath}'"
                )

            # Compute hash sum which will be used to check cache of fparser
            self._source_code_hash_sum = self.get_source_code_hash_sum()

        return self._source_code

    def get_source_code_hash_sum(self) -> "hashlib._Hash":
        return hashlib.md5(self.get_source_code().encode()).hexdigest()

    def _cache_load(
        self, verbose: bool = False, indent: str = ""
    ) -> Union[_CacheFileInfo, None]:
        """Load data from the cache file if possible.
        This also checks for matching checksums after loading the data
        from the cache.

        :param verbose: Produce some verbose output
        :type verbose: str

        :return: Class with cached information, otherwise None
        :rtype: Union[_CacheFileInfo, None]
        """

        if not self._cache_active:
            return None

        # Get source code to load it in case it's not yet loaded
        self.get_source_code()

        assert self._source_code_hash_sum is not None, (
            "Source code needs to be loaded before fparser or psyir "
            "representation is loaded."
        )

        # Check whether cache was already loaded
        if self._cache is not None:
            return self._cache

        # Load cache file
        try:
            filehandler = open(self._get_filepath_cache(), "rb")
            if verbose:
                print(
                    f"{indent}- Using cache file "
                    f"'{self._get_filepath_cache()}'"
                )
        except FileNotFoundError:
            if verbose:
                print(
                    f"{indent}- No cache file "
                    f"'{self._get_filepath_cache()}' found"
                )
            return None

        # Unpack cache file
        try:
            cache: _CacheFileInfo = pickle.load(filehandler)
        except Exception as ex:
            print(
                "{indent}- Error while reading cache file - ignoring: "
                + str(ex)
            )
            return None

        # Verify checksums
        if cache._source_code_hash_sum != self._source_code_hash_sum:
            if verbose:
                print(
                    f"  - Cache hashsum mismatch: "
                    f"source {self._source_code_hash_sum} "
                    f"vs. cache {cache._source_code_hash_sum}"
                )
            return None

        for key in ["_source_code_hash_sum", "_fparser_tree", "_psyir_node"]:
            if key not in cache.__dict__.keys():
                return None

        self._cache = cache
        self._source_code_hash_sum = self._cache._source_code_hash_sum

        return self._cache

    def _cache_save(
        self,
        verbose: bool = False,
    ) -> None:
        """Save the following elements to a cache file:
        - hash sum of code
        - fparser tree
        - potentially psyir nodes

        :param verbose: Produce some verbose output
        :type verbose: str

        :return: Return class with cached data if cache was updated,
            otherwise None
        :rtype: Union[_CacheFileInfo, None]
        """

        if not self._cache_active:
            return None

        if self._source_code_hash_sum is None:
            # Nothing to cache
            return None

        cache_updated = False
        if self._cache is None:
            # Cache doesn't exist => prepare data to write to file
            self._cache = _CacheFileInfo()
            self._cache._source_code_hash_sum = self._source_code_hash_sum

        else:
            assert (
                self._cache._source_code_hash_sum == self._source_code_hash_sum
            )

        if (
            self._cache._fparser_tree is None
            and self._fparser_tree is not None
        ):
            # Make copies of it since they could be modified later
            # With this, we can also figure out potential issues with
            # the serialization in fparser
            self._cache._fparser_tree = copy.deepcopy(self._fparser_tree)
            cache_updated = True

        if self._cache._psyir_node is None and self._psyir_node is not None:
            # TODO #2786: Serialization of psyir tree not possible
            #
            # E.g., this call fails: copy.deepcopy(self._psyir_node)
            #
            # Uncomment this code if serialization of psyir tree is
            # possible and it will work.
            # self._cache._psyir_node = copy.deepcopy(self._psyir_node)
            # cache_updated = True
            pass

        if not cache_updated:
            return None

        # Save to cache file
        try:
            filehandler = open(self._get_filepath_cache(), "wb")
        except Exception as err:
            if verbose:
                print("  - Unable to write to cache file" + str(err))
            return None

        # Unpack cache file
        try:
            pickle.dump(self._cache, filehandler)
        except Exception as err:
            print("Error while storing cache data - ignoring: " + str(err))
            return None

        if verbose:
            print(
                f"  - Cache file updated with "
                f"hashsum '{self._cache._source_code_hash_sum}"
            )
        return self._cache

    def get_fparser_tree(self, verbose: bool = False) -> Fortran2003.Program:
        """Returns the fparser Fortran2008 representation of the source code.

        :param verbose: Produce some verbose output
        :type verbose: str

        :returns: fparser representation.
        :rtype: FileContainer

        :raises FileInfoFParserError: if fparser had issues
        """
        if self._fparser_tree is not None:
            return self._fparser_tree

        if verbose:
            print(f"- Source file '{self._filepath}': " f"Running fparser")

        source_code = self.get_source_code()
        assert self._source_code_hash_sum is not None

        # Check for cache
        cache = self._cache_load(verbose=verbose)

        if cache is not None:
            if cache._fparser_tree is not None:
                if verbose:
                    print(
                        f"  - Using cache of fparser tree "
                        f"with hashsum {cache._source_code_hash_sum}"
                    )

                # Use cached version
                self._fparser_tree = self._cache._fparser_tree
                return self._fparser_tree

        try:
            reader = FortranStringReader(
                source_code, include_dirs=Config.get().include_paths
            )
            parser = ParserFactory().create(std="f2008")
            self._fparser_tree = parser(reader)

        except Exception as err:
            raise FileInfoFParserError(
                f"Failed to get fparser tree for file '{self._filepath}: "
                + str(err)
            )

        # We directly call the cache saving routine here in case that the
        # fparser tree will be modified later on.
        self._cache_save(verbose=verbose)

        return self._fparser_tree

    def get_psyir_node(
        self, verbose: bool = False, indent: str = ""
    ) -> FileContainer:
        """Returns the psyclone FileContainer of the file.

        :param verbose: Produce some verbose output
        :type verbose: str

        :returns: psyclone file container node.
        :rtype: FileContainer

        """
        if self._psyir_node is not None:
            return self._psyir_node

        # Check for cache
        cache = self._cache_load(verbose=verbose, indent=indent)

        if cache is not None:
            if cache._psyir_node is not None:
                # Use cached version
                if verbose:
                    print(f"{indent}- Using cache of fparser tree")

                self._psyir_node = self._cache._psyir_node
                return self._psyir_node

        if verbose:
            print(f"{indent}- Running psyir for '{self._filepath}'")

        fortran_reader = FortranReader()

        fparse_tree = self.get_fparser_tree()
        self._psyir_node = fortran_reader.psyir_from_fparse_tree(fparse_tree)

        self._cache_save(verbose=verbose)

        return self._psyir_node
