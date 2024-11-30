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

'''This module contains the FileInfo class.

'''

import hashlib
import copy
import os
import pickle

from fparser.two import Fortran2003
from fparser.two.parser import ParserFactory

from psyclone.psyir.frontend.fortran import FortranStringReader
from psyclone.configuration import Config
from psyclone.psyir.nodes import FileContainer
from psyclone.errors import PSycloneError
from psyclone.psyir.frontend.fparser2 import Fparser2Reader


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
    """This class stores mostly cached information about source code:
    - it stores the original filename
    - it will read the source of the file and cache it
    - it will parse it with fparser and cache it
    - it will parse it with psyir (depends on TODO #2786 "and cache it")

    """
    def __init__(self,
                 filepath: str,
                 cache_active: bool = False,
                 cache_path: str = None
                 ):
        """Constructor

        :param _filename: Path to the file that this
                         object holds information on.
        :param cache_active: Use caching of intermediate representations

        """

        # Full path to file
        self._filename: str = filepath

        # Use cache features
        self._cache_active: bool = cache_active

        # Cache filepath
        self._cache_path = cache_path

        # Source code:
        self._source_code: str = None

        # Source code hash sum:
        self._source_code_hash_sum: hashlib._Hash = None

        # Fparser node
        self._fparser_tree: Fortran2003 = None

        # Psyir node
        self._psyir_node: FileContainer = None

        # Filepath to cache
        self._cache_filename = None

        # Cache to load data from
        self._cache_data_load: _CacheFileInfo = None

        # Cache to store data
        self._cache_data_save: _CacheFileInfo = None

    def _get_filepath_cache(self):
        """Return the filepath of the cache.

        This can't be done in the constructor since the hashcode
        of the source code is required first.
        """

        assert self._cache_active, (
            "Cache file path requested, but caching disabled")

        if self._cache_filename is not None:
            return self._cache_filename

        if self._cache_path is None:
            # If cache path is not specified, we use the source code path
            # E.g.,
            # path/to/file.f90 => path/to/file.psycache
            (filepath_no_ext, _) = os.path.splitext(self._filename)

            self._cache_filename = filepath_no_ext + ".psycache"
            return self._cache_filename

        # Cache path was specified.
        # We assume this path is shared amongst many 
        (path, _) = os.path.split(self._filename)
        return (
            path + self._source_code_hash_sum[:55] + ".psycache"
        )

    @property
    def basename(self):
        '''
        :returns: the base name (i.e. without path or suffix) of the filename
                  that this FileInfo object represents.
        :rtype: str
        '''

        assert self._filename is not None

        # Remove the path from the filename.
        basename = os.path.basename(self._filename)
        # splitext returns (root, ext) and it's `root` that we want.
        return os.path.splitext(basename)[0]

    # ------------------------------------------------------------------------
    @property
    def filename(self):
        '''
        :returns: the full filename that this FileInfo object represents.
        :rtype: str
        '''
        return self._filename

    def get_source_code(self, verbose: bool = False) -> str:
        '''Returns the source code of the file. The first time, it
        will be read from the file, but the data is then cached.

        If any decoding errors are encountered then the associated character(s)
        are simply skipped. This is because this class is intended for reading
        Fortran source and the only way such characters can appear is if they
        are in comments.

        :param verbose: If `True`, produce some verbose output

        :returns: the contents of the file (utf-8 encoding).

        '''
        if self._source_code is None:
            # Specifying errors='ignore' simply skips any characters that
            # result in decoding errors. (Comments in a code may contain all
            # sorts of weird things.)

            if verbose:
                print(
                    f"- Source file '{self._filename}': "
                    f"Loading source code"
                )

            try:
                with open(
                    self._filename, "r", encoding="utf-8", errors="ignore"
                ) as file_in:
                    self._source_code = file_in.read()
            except FileNotFoundError:
                raise FileNotFoundError(
                    f"No such file or directory: '{self._filename}'"
                )

            # Compute hash sum which will be used to check cache of fparser
            self._source_code_hash_sum = hashlib.md5(
                self._source_code.encode()
            ).hexdigest()

        return self._source_code

    @property
    def source_code(self, verbose: bool = False) -> str:
        return self.get_source_code(verbose)

    def _cache_load(
        self,
        verbose: bool = False,
        indent: str = ""
    ):
        """Load data from the cache file if possible.
        This also checks for matching checksums after loading the data
        from the cache.

        :param verbose: Produce some verbose output
        """

        if not self._cache_active:
            return

        # Load the source code in case it's not yet loaded.
        # This also fills in the hash sum
        self.get_source_code()

        assert self._source_code_hash_sum is not None, (
            "Source code needs to be loaded before fparser or psyir "
            "representation is loaded."
        )

        # Check whether cache was already loaded
        if self._cache_data_load is not None:
            return self._cache_data_load

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
            assert key in cache.__dict__.keys()

        self._cache_data_load = cache
        self._source_code_hash_sum = \
            self._cache_data_load._source_code_hash_sum

    def _cache_save(
        self,
        verbose: bool = False,
    ) -> None:
        """Save the following elements to a cache file:
        - hash sum of code
        - fparser tree
        - potentially psyir nodes

        :param verbose: Produce some verbose output
        """

        if not self._cache_active:
            return None

        if self._source_code_hash_sum is None:
            # Nothing to cache
            return None

        cache_updated = False
        if self._cache_data_save is None:
            # Cache doesn't exist => prepare data to write to file
            self._cache_data_save = _CacheFileInfo()
            self._cache_data_save._source_code_hash_sum = (
                self._source_code_hash_sum)

        assert self._cache_data_save._source_code_hash_sum == (
                    self._source_code_hash_sum)

        if (
            self._cache_data_save._fparser_tree is None
            and self._fparser_tree is not None
        ):
            # Make copies of it since they could be modified later
            # With this, we can also figure out potential issues with
            # the serialization in fparser
            self._cache_data_save._fparser_tree = \
                copy.deepcopy(self._fparser_tree)
            cache_updated = True

        if self._cache_data_save._psyir_node is None and (
                self._psyir_node is not None):
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

        # Open cache file
        try:
            filehandler = open(self._get_filepath_cache(), "wb")
        except Exception as err:
            if verbose:
                print("  - Unable to write to cache file" + str(err))
            return None

        # Dump to cache file
        try:
            pickle.dump(self._cache_data_save, filehandler)
        except Exception as err:
            # Invalidate cache
            self._cache_data_save = None
            print("Error while storing cache data - ignoring: " + str(err))
            return None

        if verbose:
            print(
                f"  - Cache file updated with "
                f"hashsum '{self._cache_data_save._source_code_hash_sum}"
            )

    def get_fparser_tree(
                self,
                verbose: bool = False,
                save_to_cache: bool = True
            ) -> Fortran2003.Program:
        """Returns the fparser Fortran2008 representation of the source code.

        :param save_to_cache: Cache is updated if fparser was
            not loaded from cache.
        :param verbose: Produce some verbose output

        :returns: fparser representation.

        :raises FileInfoFParserError: if fparser had issues
        """
        if self._fparser_tree is not None:
            return self._fparser_tree

        if verbose:
            print(f"- Source file '{self._filename}': " f"Running fparser")

        source_code = self.get_source_code()
        assert self._source_code_hash_sum is not None

        # Check for cache
        self._cache_load(verbose=verbose)

        if self._cache_data_load is not None:
            if self._cache_data_load._fparser_tree is not None:
                if verbose:
                    print(
                        f"  - Using cache of fparser tree with hashsum"
                        f" {self._cache_data_load._source_code_hash_sum}"
                    )

                # Use cached version
                self._fparser_tree = self._cache_data_load._fparser_tree
                return self._fparser_tree

        try:
            reader = FortranStringReader(
                source_code, include_dirs=Config.get().include_paths
            )
            parser = ParserFactory().create(std="f2008")
            self._fparser_tree = parser(reader)

        except Exception as err:
            raise FileInfoFParserError(
                "Failed to get fparser tree: " + str(err)
            ) from err

        # We directly call the cache saving routine here in case that the
        # fparser tree will be modified later on.
        if save_to_cache:
            self._cache_save(verbose=verbose)

        return self._fparser_tree

    def get_psyir_node(
            self,
            verbose: bool = False,
            indent: str = ""
            ) -> FileContainer:
        """Returns the psyclone FileContainer of the file.

        :param verbose: Produce some verbose output

        :returns: psyclone file container node.

        """
        if self._psyir_node is not None:
            return self._psyir_node

        # Check for cache
        self._cache_load(verbose=verbose, indent=indent)

        if self._cache_data_load is not None:
            if self._cache_data_load._psyir_node is not None:
                # Use cached version
                if verbose:
                    print(f"{indent}- Using cache of fparser tree")

                self._psyir_node = self._cache_data_load._psyir_node
                return self._psyir_node

        if verbose:
            print(f"{indent}- Running psyir for '{self._filename}'")

        # First, we get the fparser tree
        # TODO #2786: use 'save_to_cache=False' if TODO is resolved
        fparse_tree = self.get_fparser_tree(
                verbose=verbose,
                save_to_cache=True
            )

        # We generate PSyIR from the fparser tree
        _, filename = os.path.split(self.filename)
        processor = self._processor = Fparser2Reader()
        self._psyir_node = processor.generate_psyir(fparse_tree, filename)

        # TODO #2786: Uncomment if psyir nodes are serializable
        # self._cache_save(verbose=verbose)

        return self._psyir_node
