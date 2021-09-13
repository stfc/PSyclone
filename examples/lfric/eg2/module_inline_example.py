# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2021, Science and Technology Facilities Council
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
# Author R. Ford STFC Daresbury Lab
# Modified by S. Siso, and A. R. Porter, STFC Daresbury Laboratory

''' Example showing the use of the module-inline transformation for the
    LFRic domain. '''
from __future__ import print_function
import os
from psyclone.parse.algorithm import parse
from psyclone.psyGen import PSyFactory, Kern
from psyclone.transformations import KernelModuleInlineTrans


def inline():
    ''' Function exercising the module-inline transformation. '''
    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "single_invoke.x90"),
                    api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(info)
    invokes = psy.invokes
    print(psy.invokes.names)
    invoke = invokes.get("invoke_0_testkern_type")
    schedule = invoke.schedule
    schedule.view()
    kern = schedule.walk(Kern)[0]
    # setting module inline directly
    kern.module_inline = True
    schedule.view()
    # unsetting module inline via a transformation
    trans = KernelModuleInlineTrans()
    trans.apply(kern, {"inline": False})
    schedule.view()
    # setting module inline via a transformation
    trans.apply(kern)
    schedule.view()
    print(str(psy.gen))


if __name__ == "__main__":
    inline()
