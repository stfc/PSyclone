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
This module provides a class with all LFRic related constants.
'''

# Imports
from __future__ import print_function, absolute_import


# pylint: disable=too-few-public-methods
class LFRicConstants(object):
    '''This class stores all LFRic constants. Note that some constants
    depend on values in the config files, so this class can only be
    used after the config file can be read.
    It stores all values in class variables (to avoid re-evaluating them).
    '''

    HAS_BEEN_INITIALISED = False

    def __init__(self):
        if LFRicConstants.HAS_BEEN_INITIALISED:
            return

        LFRicConstants.HAS_BEEN_INITIALISED = True

        # Evaluators: quadrature
        LFRicConstants.VALID_QUADRATURE_SHAPES = \
            ["gh_quadrature_xyoz", "gh_quadrature_face", "gh_quadrature_edge"]
        LFRicConstants.VALID_EVALUATOR_SHAPES = \
            LFRicConstants.VALID_QUADRATURE_SHAPES + ["gh_evaluator"]


# =============================================================================
# Documentation utils: The list of module members that we wish AutoAPI to
# generate documentation for (see https://psyclone-ref.readthedocs.io).
__all__ = ['LFRicConstants']
