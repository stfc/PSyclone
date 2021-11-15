# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2021, Science and Technology Facilities Council.
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
# Author: J. Henrichs, Bureau of Meteorology

'''
This module provides a class with all GOcean related constants.
'''

# Imports
from __future__ import print_function, absolute_import

from psyclone.configuration import Config


# pylint: disable=too-few-public-methods
class NemoConstants(object):
    '''This class stores all Nemo constants.
    It stores all values in class variables (to avoid re-evaluating them).
    At this stage it only contains the variables that might be used in
    psyGen.
    # TODO #1223 - Add more constants into this object (if required).
    '''

    HAS_BEEN_INITIALISED = False

    def __init__(self):
        if NemoConstants.HAS_BEEN_INITIALISED:
            return

        NemoConstants.HAS_BEEN_INITIALISED = True

        # Valid intrinsic types of kernel argument data, used in psyGen.
        NemoConstants.VALID_INTRINSIC_TYPES = []

        # psyGen argument types
        NemoConstants.VALID_ARG_TYPE_NAMES = []

        # psyGen names of internal scalar argument types.
        NemoConstants.VALID_SCALAR_NAMES = ["rscalar", "iscalar"]

        config = Config.get().api_conf("nemo")
        NemoConstants.VALID_LOOP_TYPES = config.get_valid_loop_types()


# =============================================================================
# Documentation utils: The list of module members that we wish AutoAPI to
# generate documentation for (see https://psyclone-ref.readthedocs.io).
__all__ = ['NemoConstants']
