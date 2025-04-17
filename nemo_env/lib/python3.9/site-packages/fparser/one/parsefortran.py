#!/usr/bin/env python
# Modified work Copyright (c) 2017-2018 Science and Technology
# Facilities Council
# Original work Copyright (c) 1999-2008 Pearu Peterson

# All rights reserved.

# Modifications made as part of the fparser project are distributed
# under the following license:

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:

# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.

# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.

# 3. Neither the name of the copyright holder nor the names of its
# contributors may be used to endorse or promote products derived from
# this software without specific prior written permission.

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

# --------------------------------------------------------------------

# The original software (in the f2py project) was distributed under
# the following license:

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:

#   a. Redistributions of source code must retain the above copyright notice,
#      this list of conditions and the following disclaimer.
#   b. Redistributions in binary form must reproduce the above copyright
#      notice, this list of conditions and the following disclaimer in the
#      documentation and/or other materials provided with the distribution.
#   c. Neither the name of the F2PY project nor the names of its
#      contributors may be used to endorse or promote products derived from
#      this software without specific prior written permission.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR
# ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
# DAMAGE.

"""Provides FortranParser.
"""
# Author: Pearu Peterson <pearu@cens.ioc.ee>
# Created: May 2006

import logging

from fparser.one.block_statements import BeginSource
from fparser.common.utils import AnalyzeError

__autodoc__ = ["FortranParser"]
__all__ = ["FortranParser"]


class FortranParser:
    """
    Parser of FortranReader structure.

    Use .parse() method for parsing, parsing result is saved in .block
    attribute.
    """

    cache = {}

    def __init__(self, reader, ignore_comments=True):
        self.reader = reader
        logging.getLogger(__name__).setLevel(logging.DEBUG)
        if reader.id in self.cache:
            parser = self.cache[reader.id]
            self.block = parser.block
            self.is_analyzed = parser.is_analyzed
            logging.getLogger(__name__).info("using cached %s", (reader.id))
        else:
            self.cache[reader.id] = self
            self.block = None
            self.is_analyzed = False
        self.ignore_comments = ignore_comments
        return

    def get_item(self):
        """
        Retrieves the next item from the reader.
        """
        try:
            item = self.reader.next(ignore_comments=self.ignore_comments)
            return item
        except StopIteration:
            pass
        return

    def put_item(self, item):
        """
        Pushes the given item to the reader.
        """
        self.reader.fifo_item.insert(0, item)
        return

    def parse(self):
        """Parses the program specified in the reader object."""
        if self.block is not None:
            return
        try:
            self.block = BeginSource(self)
        except KeyboardInterrupt:
            raise
        except Exception as error:
            reader = self.reader
            logger = logging.getLogger(__name__)
            while reader is not None:
                message = reader.format_message(
                    "FATAL ERROR",
                    "while processing line",
                    reader.linecount,
                    reader.linecount,
                )
                logger.critical(message)
                reader = reader.reader
            logger.debug("An error occurred during parsing.", exc_info=error)
            logger.critical("STOPPED PARSING")
            raise error
        return

    def analyze(self):
        """
        Attempts to analyse the parsed Fortran. It is not clear what for.
        """
        if self.is_analyzed:
            return
        if self.block is None:
            logging.getLogger(__name__).info("Nothing to analyze.")
            return

        try:
            self.block.analyze()
        except AnalyzeError:
            pass
        self.is_analyzed = True
        return
