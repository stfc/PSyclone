# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017, Science and Technology Facilities Council
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
# Author A. R. Porter, STFC Daresbury Lab

''' Tests for the install/set-up related functionality of
PSyclone '''

import os

BASE_PATH = os.path.dirname(os.path.abspath(__file__))


def test_vn_generation(monkeypatch, tmpdir):
    ''' Check that running setup.py generates the expected Python
    module file containing the current version of PSyclone '''
    import sys
    sys_path = sys.path
    # BASE_PATH points to some_path/PSyclone/src/psyclone/tests and we
    # need some_path/PSyclone
    tail = ""
    head = BASE_PATH
    while tail != "src":
        head, tail = os.path.split(head)
    # Monkeypatch sys.path so that it includes the locations of setup.py
    # and our scratch directory
    monkeypatch.setattr(sys, "path", value=sys_path + [head, str(tmpdir)])
    # Now import the setup module and call the write_version_py() routine
    # to generate a new module containing the version number.
    import setup
    fname = os.path.join(str(tmpdir), "tmp_version.py")
    setup.write_version_py(filename=fname)
    # Check that the resulting module contains what we expect
    assert os.path.isfile(fname)
    import tmp_version
    assert tmp_version.version == setup.VERSION
    assert tmp_version.short_version == setup.SHORT_VERSION
    # Again but using the default filename. We monkeypatch setup.__file__ to
    # be a file in our scratch directory so that the resulting
    # version.py is written in our scratch space.
    monkeypatch.setattr(setup, "__file__", fname)
    tmpdir.mkdir("src")
    tmpdir.mkdir(os.path.join("src", "psyclone"))
    setup.write_version_py()
    assert os.path.isfile(os.path.join(str(tmpdir),
                                       "src", "psyclone", "version.py"))
