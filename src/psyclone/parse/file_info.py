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
# Modifications: M. Schreiber, Univ. Grenoble Alpes

'''This module contains the FileInfo class.

'''

import copy
import hashlib
import logging
import os
import pickle
from typing import Optional, Union, Iterable

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
        self.value = "FileInfoFParserError: " + str(value)


class _CacheFileInfo:
    """Class which is used to store all information
    which can be cached to a file and read back from a file.
    """
    def __init__(self):
        # Hash sum
        self._source_code_hash_sum: hashlib._Hash = None

        # Fparser tree
        self._fparser_tree: Fortran2003.Program = None

        # Psyir node
        self._psyir_node: FileContainer = None


class FileInfo:
    """This class stores mostly cached information about source code:
    - it stores the original filename
    - it will read the source of the file and cache it
    - it will parse it with fparser and cache it
    - it will construct the PSyIR (depends on TODO #2786 "and cache it")

    :param filepath: Path to the file that this
        object holds information on. Can also be set to 'None' in case of
        providing fparser / PSyIR node in a different way.
    :param use_caching: Use caching of intermediate representations
    :param cache_path: Path to directory where to put cache files.
        If it is provided, the file name for caching will include
        the hash sum to avoid conflicting file names.
        This allows using, e.g., `~/.cache/psyclone` as a cache
        directory for all cached files.
        See _get_filepath_cache() for more information.
    :param resolve_imports: whether to resolve imports. It can be a list
        of module names to provide finer control.

    """
    def __init__(self,
                 filepath: str,
                 cache_active: Optional[bool] = False,
                 cache_path: Optional[str] = None,
                 resolve_imports: Union[bool, Iterable[str]] = False
                 ):

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
        self._fparser_tree: Fortran2003.Program = None

        # Flag indicating that, based on a previous attempt,
        # the fparser tree can't be generated due to an error
        self._fparser_tree_triggers_error: bool = False

        # Psyir node
        self._psyir_node: FileContainer = None

        # Filepath of cache
        self._cache_filename = None

        # This reference to `_CacheFileInfo` is created when loading
        # cached information from a cache file.
        # In case the checksums mismatch, no object will be referenced.
        # Consequently, this object will always have a checksum matching
        # the one from the source code.
        self._cache_data_load: _CacheFileInfo = None

        # This reference is used whenever writing cache data to the
        # persistent storage.
        # It will also be partly updated if the `psyir` or
        # `fparser tree` was created in the meantime and a cache update
        # is requested.
        self._cache_data_save: _CacheFileInfo = None

        # Whether to resolve imports. It can be a list of module names to
        # provide finer control.
        self._resolve_imports: Union[bool, Iterable[str]] = resolve_imports

    def _get_cache_filepath(self):
        """Return the filepath of the cache.

        This also supports having a shared caching directory,
        e.g., in `$HOME/.cache/psyclone/`.

        This sets up unique cache file names based on the
        hashcode. Consequently, this can't be done in the
        constructor since the hashcode of the source code
        is required first.
        """

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
        # We assume this path is shared amongst different projects
        # where psyclone is used for. We can't just use the file
        # name of the source file itself, since the same one
        # could be used in different projects and lead to
        # conflicting file names. Hence, we use a hashsum based
        # on the content of the source code itself.
        # This also avoids having separate cache files for files
        # with the same content spread across different projects.

        # Therefore, we associate each cache file to a hashsum.
        # The hashsum is truncated so that the entire filename
        # fits in a 64 byte character string.
        return os.path.join(
            self._cache_path, self._source_code_hash_sum[:55] + ".psycache"
        )

    @property
    def basename(self):
        '''
        :returns: the base name (i.e. without path or suffix) of the filename
                  that this FileInfo object represents.
        :rtype: str
        '''

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

    def get_source_code(self) -> str:
        '''Returns the source code of the file. The first time, it
        will be read from the file, but the data is then cached.

        If any decoding errors are encountered then the associated character(s)
        are simply skipped. This is because this class is intended for reading
        Fortran source and the only way such characters can appear is if they
        are in comments.

        :returns: the contents of the file (utf-8 encoding).

        '''
        if self._source_code:
            return self._source_code

        logger = logging.getLogger(__name__)
        logger.info(f"Source file '{self._filename}': loading source code")

        try:
            # Specifying errors='ignore' simply skips any characters that
            # result in decoding errors. (Comments in a code may contain all
            # sorts of weird things.)
            with open(
                self._filename, "r", encoding="utf-8", errors="ignore"
            ) as file_in:
                self._source_code = file_in.read()
        except FileNotFoundError as err:
            raise FileNotFoundError(
                f"FileInfo: No such file or directory '{self._filename}'."
            ) from err

        logger.info(f"Source file '{self._filename}': loaded OK")

        if self._cache_active:
            # Update the hash sum
            self._source_code_hash_sum = hashlib.md5(
                self._source_code.encode()).hexdigest()

        return self._source_code

    def _cache_load(self) -> _CacheFileInfo:
        """Load fparser parse tree from the cache file if possible.

        This also checks for matching checksums after loading the data
        from the cache.
        The checksum is based solely on a hashsum of the source code itself,
        see code below.

        """

        if not self._cache_active:
            return

        # Load the source code in case it's not yet loaded.
        # This also fills in the hash sum
        self.get_source_code()

        # Check whether cache was already loaded
        if self._cache_data_load is not None:
            return self._cache_data_load

        logger = logging.getLogger(__name__)

        # Load cache file.
        # Warning: There could be race conditions, e.g., in parallel builds.
        # In the worst case some content is read which is incomplete or
        # basically garbage. This will lead either to an Exception from the
        # unpickling or a non-matching checksum which is both caught below.
        try:
            filehandler = open(self._get_cache_filepath(), "rb")
            logger.info(
                f"Using cache file '{self._get_cache_filepath()}'")
        except FileNotFoundError:
            logger.info(
                f"No cache file '{self._get_cache_filepath()}' found")
            return None

        # Unpack cache file
        try:
            cache: _CacheFileInfo = pickle.load(filehandler)
        except Exception as ex:
            logger.warn(
                f"Error while reading cache file - ignoring: {str(ex)}")
            return None

        # Verify checksums
        if cache._source_code_hash_sum != self._source_code_hash_sum:
            logger.info(
                f"Cache hashsum mismatch: "
                f"source {self._source_code_hash_sum} "
                f"vs. cache {cache._source_code_hash_sum}")
            return None

        self._cache_data_load = cache

    def _cache_save(self) -> None:
        """Save the following elements to a cache file:
        - hash sum of code
        - fparser tree
        - in future work, potentially also psyir nodes too
          (requires TODO #2786).

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

        if (
            self._cache_data_save._fparser_tree is None
            and self._fparser_tree is not None
        ):
            # No fparser tree was loaded so far into the cache object
            # AND
            # an fparser tree was loaded and stored to Fileinfo.
            #
            # Consequently, we cache the fparser tree to the cache file.
            # We create a deepcopy of this fparser tree to ensure that we cache
            # the original tree and not a modified fparser tree node if the
            # cache is updated (based on potentially future work of also
            # caching the PSyIR)

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

        logger = logging.getLogger(__name__)

        # Open cache file
        try:
            # Atomically attempt to open the new kernel file (in case
            # this is part of a parallel build)
            # We first remove the cache file and then open it.
            # If the file exists, it throws an exception.
            # This is not a perfect solution, but avoids parallel
            # writing access of the same file.

            # We first remove a potentially existing file.
            try:
                os.remove(self._get_cache_filepath())
            except FileNotFoundError:
                pass

            # Then we open it in exclusive mode.
            # If it already exists, an exception would be raised.
            fd = os.open(self._get_cache_filepath(),
                         os.O_CREAT | os.O_WRONLY | os.O_EXCL)

            filehandler = os.fdopen(fd, "wb")
        except Exception as err:
            logger.warning("Unable to write to cache file: " + str(err))
            return None

        # Dump to cache file
        try:
            pickle.dump(self._cache_data_save, filehandler)
        except Exception as err:
            # Invalidate cache
            self._cache_data_save = None
            logger.warning("Error while storing cache data - ignoring: " +
                           str(err))
            return None

        logger.info(
            f"Cache file updated with hashsum "
            f"{self._cache_data_save._source_code_hash_sum}")

    def get_fparser_tree(
                self,
                save_to_cache_if_cache_active: bool = True
            ) -> Fortran2003.Program:
        """Returns the fparser Fortran2003.Program representation of the
        source code (including Fortran2008).

        :param save_to_cache_if_cache_active: Cache is updated if fparser was
            not loaded from cache.

        :returns: fparser representation.

        :raises FileInfoFParserError: if fparser had issues
        """
        if self._fparser_tree is not None:
            return self._fparser_tree

        if self._fparser_tree_triggers_error:
            # Raises an exception if we were not able to create the
            # fparser tree before.
            raise FileInfoFParserError(
                "Failed to create fparser tree (previous attempt failed)"
            )

        logger = logging.getLogger(__name__)

        logger.info(f"Source file '{self._filename}': Running fparser")

        try:
            source_code = self.get_source_code()
        except FileNotFoundError as err:
            raise FileInfoFParserError(
                f"File '{self._filename}' not found:\n{str(err)}")

        # Check for cache
        self._cache_load()

        if self._cache_data_load is not None:
            if self._cache_data_load._fparser_tree is not None:
                logger.info(
                    f"Using cache of fparser tree with hashsum"
                    f" {self._cache_data_load._source_code_hash_sum}"
                )

                # Use cached version
                self._fparser_tree = self._cache_data_load._fparser_tree
                return self._fparser_tree

        try:
            config = Config.get()
            reader = FortranStringReader(
                source_code, include_dirs=config.include_paths)
            parser = ParserFactory().create(std=config.fortran_standard)
            self._fparser_tree = parser(reader)

        except Exception as err:
            self._fparser_tree_triggers_error = True
            raise FileInfoFParserError(
                "Failed to create fparser tree: " + str(err)
            ) from err

        # We directly call the cache saving routine here in case that the
        # fparser tree will be modified later on.
        if save_to_cache_if_cache_active:
            self._cache_save()

        return self._fparser_tree

    def get_psyir(self) -> FileContainer:
        """
        :returns: the psyclone FileContainer of the file.
        """
        if self._psyir_node is not None:
            return self._psyir_node

        logger = logging.getLogger(__name__)

        # Check for cache
        self._cache_load()

        if self._cache_data_load is not None:
            if self._cache_data_load._psyir_node is not None:
                # Use cached version
                logger.info("Using cache of PSyIR")

                self._psyir_node = self._cache_data_load._psyir_node
                return self._psyir_node

        logger.info(f"Generating PSyIR for '{self._filename}'")

        # First, we get the fparser tree
        fparse_tree = self.get_fparser_tree(
                # TODO #2786: If this TODO is resolved, set this to False
                # and uncomment the self._cache_save below.
                save_to_cache_if_cache_active=True
            )

        # We generate PSyIR from the fparser tree using the same options as
        # were provided to the PSyclone invocation.
        _, filename = os.path.split(self.filename)
        processor = Fparser2Reader(
            resolve_modules=self._resolve_imports
        )
        self._psyir_node = processor.generate_psyir(fparse_tree, filename)

        # TODO #2786: Uncomment if psyir nodes are serializable
        # self._cache_save()

        return self._psyir_node
