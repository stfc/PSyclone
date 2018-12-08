# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2018, Science and Technology Facilities Council.
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
# ----------------------------------------------------------------------------
# Authors R. W. Ford and A. R. Porter, STFC Daresbury Lab

'''
API-agnostic tests for various transformation classes.
'''

from __future__ import absolute_import, print_function
import pytest


def test_accloop():
    ''' Generic tests for the ACCLoopTrans transformation class '''
    from psyclone.transformations import ACCLoopTrans
    from psyclone.psyGen import Node, ACCLoopDirective
    trans = ACCLoopTrans()
    assert trans.name == "ACCLoopTrans"
    assert str(trans) == "Adds an 'OpenACC loop' directive to a loop"

    pnode = Node()
    cnode = Node()
    tdir = trans._directive(pnode, [cnode])
    assert isinstance(tdir, ACCLoopDirective)


def test_accparallel():
    ''' Generic tests for the ACCParallelTrans class '''
    from psyclone.transformations import ACCParallelTrans
    acct = ACCParallelTrans()
    assert acct.name == "ACCParallelTrans"


def test_accenterdata():
    ''' Generic tests for the ACCEnterDataTrans class '''
    from psyclone.transformations import ACCEnterDataTrans
    acct = ACCEnterDataTrans()
    assert acct.name == "ACCEnterDataTrans"
    assert str(acct) == "Adds an OpenACC 'enter data' directive"


def test_omploop_no_collapse():
    ''' Check that the OMPLoopTrans.directive() method rejects the
    collapse argument '''
    from psyclone.psyGen import Node
    from psyclone.transformations import OMPLoopTrans
    trans = OMPLoopTrans()
    pnode = Node()
    cnode = Node()
    with pytest.raises(NotImplementedError) as err:
        _ = trans._directive(pnode, cnode, collapse=2)
    assert ("The COLLAPSE clause is not yet supported for '!$omp do' "
            "directives" in str(err))
