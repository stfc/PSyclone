# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2026, Science and Technology Facilities Council.
# All rights reserved.
# -----------------------------------------------------------------------------
"""PSyIR-first parsing of LFRic kernel source and metadata."""

import os

from psyclone.configuration import (
    Config, GOCEAN_API_NAMES, LFRIC_API_NAMES)
from psyclone.parse.utils import check_api, check_line_length, ParseError
from psyclone.psyir.frontend.fortran import FortranReader


def get_kernel_filepath(module_name, kernel_paths, alg_filename):
    """Locate the Fortran file containing ``module_name``.

    Kernel paths are searched recursively. If none are supplied, only the
    directory containing the algorithm file is searched.
    """
    search_name = f"{module_name}.f90"
    matches = set()
    search_directory = (
        os.path.abspath(os.path.dirname(alg_filename))
        if alg_filename else os.getcwd()
    )

    for kernel_path in kernel_paths:
        search_directory = os.path.abspath(kernel_path)
        if not os.access(search_directory, os.R_OK):
            raise ParseError(
                "kernel.py:get_kernel_filepath: Supplied kernel search path "
                f"does not exist or cannot be read: {search_directory}"
            )
        for root, _, filenames in os.walk(search_directory):
            matches.update(
                os.path.join(root, filename)
                for filename in filenames
                if filename.lower() == search_name
            )

    if not kernel_paths:
        matches.update(
            os.path.join(search_directory, filename)
            for filename in os.listdir(search_directory)
            if filename.lower() == search_name
        )

    if not matches:
        raise ParseError(
            f"Kernel file '{module_name}.[fF]90' not found in "
            f"{search_directory}"
        )
    if len(matches) > 1:
        raise ParseError(
            "kernel.py:get_kernel_filepath: More than one match for kernel "
            f"file '{module_name}.[fF]90' found! {matches}"
        )
    return matches.pop()


def get_kernel_psyir(source_code):
    """Parse Fortran kernel source text and return language-level PSyIR."""
    if not isinstance(source_code, str):
        raise TypeError(
            "Kernel source must be supplied as a string but found "
            f"'{type(source_code).__name__}'."
        )
    try:
        return FortranReader().psyir_from_source(source_code)
    except Exception as err:
        raise ParseError(
            "Failed to parse kernel source. Is the Fortran correct?"
        ) from err


def get_kernel_psyir_from_file(file_path):
    """Parse a Fortran kernel file and return language-level PSyIR."""
    try:
        return FortranReader().psyir_from_file(file_path)
    except Exception as err:
        raise ParseError(
            f"Failed to parse kernel code '{file_path}'. Is the Fortran "
            "correct?"
        ) from err


def get_kernel_psyir_for_module(
    module_name, alg_filename, kernel_paths, line_length
):
    """Locate and parse the source for one kernel module."""
    file_path = get_kernel_filepath(
        module_name, kernel_paths, alg_filename
    )
    if line_length:
        check_line_length(file_path)
    return get_kernel_psyir_from_file(file_path)


# pylint: disable=too-few-public-methods
class KernelTypeFactory:
    """Create immutable API-specific kernel metadata from PSyIR."""

    def __init__(self, api=""):
        check_api(api)
        self._type = api or Config.get().api

    def create(self, psyir, name=None):
        """Create LFRic kernel metadata from a complete PSyIR tree."""
        if self._type in LFRIC_API_NAMES:
            # Avoid an import cycle through the domain package.
            # pylint: disable=import-outside-toplevel
            from psyclone.domain.lfric.kernel import LFRicKernMetadata

            return LFRicKernMetadata.create_from_psyir(psyir, name=name)
        if self._type in GOCEAN_API_NAMES:
            # pylint: disable=import-outside-toplevel
            from psyclone.domain.gocean.kernel import GOceanKernelMetadata

            return GOceanKernelMetadata.create_from_kernel_psyir(
                psyir, name=name)
        raise ParseError(
            "KernelTypeFactory:create: Unsupported PSyIR-first kernel type "
            f"'{self._type}' found."
        )


class BuiltInKernelTypeFactory(KernelTypeFactory):
    """Create LFRic built-in metadata from its Fortran definitions."""

    # pylint: disable=arguments-differ,arguments-renamed
    def create(self, builtin_names, builtin_defs_file, name=None):
        """Find, parse and extract metadata for a named built-in."""
        if name not in builtin_names:
            raise ParseError(
                "BuiltInKernelTypeFactory:create unrecognised built-in name. "
                f"Got '{name}' but expected one of {builtin_names}"
            )
        file_path = os.path.join(
            os.path.dirname(os.path.abspath(__file__)), builtin_defs_file
        )
        if not os.path.isfile(file_path):
            raise ParseError(
                f"BuiltInKernelTypeFactory:create Kernel '{name}' is a "
                "recognised Built-in but cannot find file "
                f"'{file_path}' containing the meta-data describing the "
                f"Built-in operations for API '{self._type}'"
            )
        try:
            psyir = get_kernel_psyir_from_file(file_path)
        except ParseError as err:
            raise ParseError(
                "BuiltInKernelTypeFactory:create: Failed to parse the "
                "meta-data for PSyclone built-ins in file "
                f"'{file_path}'."
            ) from err
        return super().create(psyir, name)


__all__ = [
    "BuiltInKernelTypeFactory",
    "KernelTypeFactory",
    "get_kernel_filepath",
    "get_kernel_psyir",
    "get_kernel_psyir_for_module",
    "get_kernel_psyir_from_file",
]
