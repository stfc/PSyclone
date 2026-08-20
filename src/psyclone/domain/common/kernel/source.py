# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council.
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

"""Kernel-source discovery and parsing utilities."""

import os

from psyclone.parse.utils import check_line_length, ParseError
from psyclone.psyir.frontend.fortran import FortranReader


def find_kernel_file(module_name, kernel_paths, alg_filename=None):
    """Locate the Fortran file containing ``module_name``.

    Search paths are recursive. If no paths are supplied then only the
    directory containing the algorithm file is searched.

    :param str module_name: name of the kernel module.
    :param kernel_paths: directories in which to search.
    :type kernel_paths: Iterable[str]
    :param Optional[str] alg_filename: associated algorithm filename.

    :returns: absolute path of the unique matching file.
    :rtype: str

    :raises ParseError: if a path is unreadable or the match is not unique.
    """
    search_name = f"{module_name}.f90".lower()
    matches = set()
    search_directory = (
        os.path.abspath(os.path.dirname(alg_filename))
        if alg_filename else os.getcwd()
    )

    for kernel_path in kernel_paths:
        search_directory = os.path.abspath(kernel_path)
        if not os.access(search_directory, os.R_OK):
            raise ParseError(
                "Supplied kernel search path does not exist or cannot be "
                f"read: {search_directory}"
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
            "More than one match for kernel file "
            f"'{module_name}.[fF]90' found: {matches}"
        )
    return matches.pop()


def parse_fortran_source(source):
    """Translate Fortran source text into language-level PSyIR."""
    if not isinstance(source, str):
        raise TypeError(
            "Kernel source must be supplied as a string but found "
            f"'{type(source).__name__}'."
        )
    try:
        return FortranReader().psyir_from_source(source)
    except Exception as err:
        raise ParseError(
            "Failed to parse kernel source. Is the Fortran correct?"
        ) from err


def parse_fortran_file(file_path, line_length=False):
    """Translate a Fortran file into language-level PSyIR."""
    if line_length:
        check_line_length(file_path)
    try:
        return FortranReader().psyir_from_file(file_path)
    except Exception as err:
        raise ParseError(
            f"Failed to parse kernel code '{file_path}'. Is the Fortran "
            "correct?"
        ) from err
