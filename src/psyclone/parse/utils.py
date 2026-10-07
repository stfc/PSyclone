# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2019-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''Utility module containing classes and functions that are used by
the parser modules.

'''

import io

from fparser.two.parser import ParserFactory
from fparser.common.readfortran import FortranFileReader
from fparser.two.utils import FortranSyntaxError
from psyclone.configuration import Config
from psyclone.line_length import FortLineLength
from psyclone.errors import PSycloneError, InternalError


# Exceptions

class ParseError(PSycloneError):
    '''Provides a PSyclone-specific error class for the situation when
    the PSyclone code parsing finds an error in the input.

    :param str value: the message associated with the error.

    '''
    def __init__(self, value):
        PSycloneError.__init__(self, value)
        self.value = "Parse Error: " + str(value)

# support functions


def check_api(api):
    '''Check that the supplied API is valid.

    :param str api: The API to check.
    :raises ParseError: if the supplied API is not recognised.

    '''
    _config = Config.get()

    if api not in _config.supported_apis:
        raise ParseError(
            f"utils.py:check_api: Unsupported API '{api}' specified. "
            f"Supported types are {_config.supported_apis}.")


def check_line_length(filename):
    '''Check that the code contained within the filename file
    conforms to the 132 line length limit.

    :param str filename: The name of the file containing the code.

    :raises InternalError: if the specified file can not be opened or read.
    :raises ParseError: if one of more lines are longer than the 132
                        line length limit.
    '''
    fll = FortLineLength()
    try:
        with io.open(filename, "r", encoding='utf8') as myfile:
            code_str = myfile.read()
    except IOError as excinfo:
        raise InternalError(
            f"In utils.py:check_line_length: {excinfo}") from excinfo

    if fll.long_lines(code_str):
        raise ParseError(
            f"File '{filename}' does not conform to the specified {fll.length}"
            f" line-length limit. Either correct the file or change the "
            f"'-l/--limit' setting on the PSyclone command line.")


def parse_fp2(filename, ignore_comments: bool = True):
    '''Parse a Fortran source file contained in the file 'filename' using
    fparser2.

    :param str filename: source file (including path) to read.
    :param ignore_comments: whether to remove the comments from the input
                           file. Default is True.
    :returns: fparser2 AST for the source file.
    :rtype: :py:class:`fparser.two.Fortran2003.Program`
    :raises ParseError: if the file could not be parsed.

    '''
    # We get the directories to search for any Fortran include files from
    # our configuration object.
    config = Config.get()
    try:
        reader = FortranFileReader(filename, include_dirs=config.include_paths,
                                   ignore_comments=ignore_comments)
    except IOError as error:
        raise ParseError(
            f"algorithm.py:parse_fp2: Failed to parse file '{filename}'. "
            f"Error returned was ' {error} '.") from error
    parser = ParserFactory().create(std=config.fortran_standard)
    try:
        parse_tree = parser(reader)
    except FortranSyntaxError as msg:
        raise ParseError(
            f"algorithm.py:parse_fp2: Syntax error in file '{filename}':\n"
            f"{msg}") from msg
    return parse_tree
