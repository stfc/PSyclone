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
# Modified: R. W. Ford and N. Nobre, STFC Daresbury Lab

'''A transformation script that adds profiling information.

In order to use this script you must first install PSyclone. See
README.md in the top-level psyclone directory.

Once you have psyclone installed, this may be used by doing:

 $ psyclone -api nemo -s ./profile_trans.py some_source_file.f90

This should produce a lot of output, ending with generated
Fortran. Note that the Fortran source files provided to PSyclone must
have already been preprocessed (if required).

'''

from __future__ import print_function, absolute_import
from psyclone.psyir.transformations import ProfileTrans
from psyclone.psyir.nodes import Loop


def trans(psy):
    '''A PSyclone-script compliant transformation function. Adds
    profiling to an invoke.

    :param psy: The PSy layer object to apply transformations to.
    :type psy: :py:class:`psyclone.psyGen.PSy`
    '''
    # Since "Backslashes may not appear inside the expression
    # portions of f-strings" via PEP 498, use chr(10) for '\n'
    print(f"Invokes found:\n"
          f"{chr(10).join([str(name) for name in psy.invokes.names])}\n")

    p_trans = ProfileTrans()

    for invoke in psy.invokes.invoke_list:

        sched = invoke.schedule
        if not sched:
            print("Invoke {invoke.name} has no Schedule! Skipping...")
            continue

        # Enclose all children of the schedule within a single profile region
        p_trans.apply(sched.children)
        print(sched.view())
