# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2021, Science and Technology Facilities Council.
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
# Author J. Henrichs, Bureau of Meteorology
# Modified I. Kavcic, Met Office
# Modified R. W. Ford, STFC Daresbury Lab

'''Module for the LFRic domain.
'''

# The order here is not alphabetical, but important because
# there are various dependencies between the modules (e.g.
# KernCallAccArgList imports KernCallArgList, ArgOrdering
# imports LFRicArgDescriptor, ...).
from psyclone.domain.lfric.function_space import FunctionSpace
from psyclone.domain.lfric.lfric_arg_descriptor import LFRicArgDescriptor
from psyclone.domain.lfric.lfric_constants import LFRicConstants
from psyclone.domain.lfric.arg_ordering import ArgOrdering
from psyclone.domain.lfric.kern_call_arg_list import KernCallArgList
from psyclone.domain.lfric.kern_call_acc_arg_list import KernCallAccArgList
from psyclone.domain.lfric.kern_stub_arg_list import KernStubArgList
from psyclone.domain.lfric.kernel_interface import KernelInterface

# The entities in the __all__ list are made available to import directly from
# this package e.g.:
# from psyclone.domain.lfric import FunctionSpace

__all__ = [
    'ArgOrdering',
    'FunctionSpace',
    'KernCallAccArgList',
    'KernCallArgList',
    'KernelInterface',
    'KernStubArgList',
    'LFRicArgDescriptor',
    'LFRicConstants']
