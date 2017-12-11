# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017, Science and Technology Facilities Council
#
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

import os

def trans(psy):
    from psyclone import claw
    # Just get the first invoke
    invoke = psy.invokes.invoke_list[0]
    invoke.schedule.view()
    # Get the kernel
    kern = invoke.schedule.children[0].children[0].children[0]
    print kern.name
    # Invoke claw on the kernel using the claw_trans script in
    # this file
    claw.trans([invoke], [kern.name], os.path.abspath(__file__))

def claw_trans():
    pass

if __name__ == "__main__":
    from psyclone.parse import parse
    from psyclone.psyGen import PSyFactory
    src_path = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                            "..", "..", "..", "src", "psyclone", "tests",
                            "test_files", "gocean1p0")
    _, info = parse(os.path.join(src_path, "single_invoke.f90"),
                    api="gocean1.0")
    psy = PSyFactory("gocean1.0").create(info)
    trans(psy)

    print "All done!"
