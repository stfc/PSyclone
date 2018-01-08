# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2018, Science and Technology Facilities Council
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
    '''
    An example PSyclone transformation script. Here we might
    transform the AST of the PSy layer before using CLAW to transform
    any kernels.
    '''
    from psyclone import claw
    # Just get the first invoke
    invoke = psy.invokes.invoke_list[0]
    invoke.schedule.view()
    # Get the kernel
    kern = invoke.schedule.children[3].children[0]
    print "Kernel name: ", kern.name
    # Invoke Claw on the kernel using the claw_trans function in
    # this file. This will produce a new source file(s) for each kernel
    # object.
    kern_names = claw.trans([kern], os.path.abspath(__file__),
                            mode="keep")
    print "Transformed kernel names: ", kern_names
    # Update selected kernels in selected invokes to use the transformed
    # kernel(s). If multiple invokes use the same kernel and we want them
    # to all be replaced by the transformed version then we do that here.
    # On the other hand, if an invoke in a different Algorithm uses the
    # same kernel then we may have already transformed it and thus may
    # have a clash. We therefore set mode="keep" to ensure we don't
    # overwrite any previously transformed kernels.


def claw_trans(xast):
    '''
    This is the function called by CLAW. It is passed the AST of the
    kernel to be transformed.
    '''
    from claw.tatsu.primitive import Loop
    from claw.tatsu.xcodeml.abstraction import NestedDoStatement
    from claw.tatsu.xcodeml.xnode.common import Xcode
    from claw.tatsu.xcodeml.xnode import XnodeUtil

    node = xast.firstChild()
    print type(node)
    do_loops = xast.matchAll(Xcode.F_DO_STATEMENT)
    print "Found {0} do loops".format(len(do_loops))
    pragmas = xast.matchAll(Xcode.F_PRAGMA_STATEMENT)
    print "Found {0} pragmas".format(len(pragmas))

    # Perform a simple loop interchange (inner becomes outer)
    # using Claw primitives
    nested_loop = NestedDoStatement(do_loops[0])
    Loop.reorder(nested_loop, ["jj", "ji"])

    # An example of how we might directly manipulate the AST
    for pragma in pragmas:
        # If this is a CLAW pragma then delete it
        if "claw" in pragma.value().lower():
            XnodeUtil.safeDelete(pragma)

    return xast


if __name__ == "__main__":
    '''
    Entry point for running this script from the command line
    '''
    from psyclone.parse import parse
    from psyclone.psyGen import PSyFactory
    src_path = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                            "..", "..", "..", "src", "psyclone", "tests",
                            "test_files", "dynamo0p3")
    _, info = parse(os.path.join(src_path, "1_single_invoke.f90"),
                    api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(info)
    # Mimic PSyclone by calling the trans() function defined in this
    # script to transform the supplied PSy layer.
    trans(psy)

    print str(psy.gen)
    print "All done!"
