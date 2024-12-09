# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2020-2024, Science and Technology Facilities Council.
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
# Author: A. R. Porter, STFC Daresbury Lab
# Modified: R. W. Ford, N. Nobre and S. Siso, STFC Daresbury Lab

'''A transformation script that adds profiling information with user-provided
region names.

In order to use this script you must first install PSyclone. See
README.md in the top-level psyclone directory.

Once you have psyclone installed, this may be used by doing:

 $ psyclone -s ./profile_trans.py some_source_file.f90

'''

from psyclone.psyir.transformations import ProfileTrans
from psyclone.psyir.nodes import Loop

# Set up some loop_type inference rules in order to reference useful domain
# loop constructs by name
Loop.set_loop_type_inference_rules({"levels": {"variable": "jk"}})


def trans(psyir):
    ''' Adds profiling around loops over levels.

    :param psyir: the PSyIR of the provided file.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`
    '''

    p_trans = ProfileTrans()

    loops = psyir.walk(Loop)
    loop_counter = 0
    for loop in loops:
        if loop.loop_type == "levels":
            # We only put profiling around loops over levels
            loop_counter += 1
            p_trans.apply(loop, {"region_name": ("kloop", f"{loop_counter}")})

    print(psyir.view())
