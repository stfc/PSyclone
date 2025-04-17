# -*- coding: utf-8 -*-
##############################################################################
# Copyright (c) 2017 Science and Technology Facilities Council
#
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
##############################################################################
# Modified M.Hambley, UK Met Office
##############################################################################
"""
Helps with testing methods which write to the standard logger.
"""
import logging


class CaptureLoggingHandler(logging.Handler):
    """
    Records logged output for later examination.

    This is a standard handler for the built-in Python logging system.
    To make use of it simply register an instance with the logger using a
    command such as
    "logging.getLogger(__class__).addHandler( CaptureLoggingHandler() )"

    Any log message raised while this handler is in use will be recorded to
    a memory buffer for later use. This object has attributes 'debug', 'info',
    'warning', 'error' and 'critical', each of which is a list of logged
    messages. Only the message text is recorded, everything else is lost.
    """

    def __init__(self, *args, **kwargs):
        """
        Constructs an instance of CaptureLoggingHandler using the arguments
        accepted by logging.Handler.
        """
        super(CaptureLoggingHandler, self).__init__(*args, **kwargs)
        self.reset()

    def emit(self, record):
        """
        Handles a logged event. The event is passed from the logging system
        and recorded for future inspection.

        :param :py:class:`logging.LogRecord` record The event being logged.
        """
        self.messages[record.levelname.lower()].append(record.getMessage())

    def reset(self):
        """
        Empties the log of previous messages.
        """
        self.messages = {
            "debug": [],
            "info": [],
            "warning": [],
            "error": [],
            "critical": [],
        }
