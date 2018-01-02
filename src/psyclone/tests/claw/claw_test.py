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

''' Tests for the CLAW interface implemented in PSyclone '''

import pytest

def _fake_check_call(args, env=None):
    '''
    Function to be used to monkeypatch the check_call() function of
    the subprocess module.
    :param list args: List of items from which to construct system call
    :raises: subprocess.CalledProcessError
    '''
    from subprocess import CalledProcessError
    raise CalledProcessError(1, " ".join(args))


def test_omni_fe_error(monkeypatch):
    ''' Check that we raise the expected exception if the Omni frontend
    fails '''
    from psyclone.claw import omni_frontend
    import subprocess
    monkeypatch.setattr(subprocess, "check_call", _fake_check_call)
    with pytest.raises(subprocess.CalledProcessError) as err:
        omni_frontend("some_file.f90", "some_file.xml", ".")
    assert "F_Front -I. some_file.f90 -o some_file.xml" in str(err)


def test_run_claw(monkeypatch):
    ''' Check the _run_claw() routine in the claw module '''
    from psyclone.claw import _run_claw
    import subprocess
    monkeypatch.setattr(subprocess, "check_call", _fake_check_call)
    with pytest.raises(subprocess.CalledProcessError) as err:
        _run_claw(["."], "some_file.xml", "some_file.f90", "some_script.py")
    output = str(err)
    print output
    assert "java -Xmx200m -Xms200m -cp" in str(err)
    assert "jython.jar claw.ClawX2T --config-path=" in output
    assert ("-M. -f some_file.f90 -o some_file.xml.tmp.xml -script "
            "some_script.py some_file.xml" in output)


def test_api_from_ast():
    ''' Test for the utility routine that gives us the name of the PSyclone
    API to which a kernel object belongs '''
    from psyclone.dynamo0p3 import DynKern
    from psyclone.gocean1p0 import GOKern
    from psyclone.transformations import TransformationError
    from psyclone.claw import _api_from_ast
    dkern = DynKern()
    api = _api_from_ast(dkern)
    assert api == "dynamo0.3"

    gkern = GOKern()
    api = _api_from_ast(gkern)
    assert api == "gocean1.0"

    no_kern = "not a kernel"
    with pytest.raises(TransformationError) as err:
        _ = _api_from_ast(no_kern)
    assert "Cannot determine API for kernel" in str(err)
