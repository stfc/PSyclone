# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2023-2024, Science and Technology Facilities Council.
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
# Author: Joerg Henrichs, Bureau of Meteorology

'''This module tests the ReadWriteInfo class.'''


from psyclone.core import Signature
from psyclone.psyir.tools import ReadWriteInfo


def test_read_write_info():
    '''Test the ReadWriteInfo constructor.
    '''

    rwi = ReadWriteInfo()
    assert rwi.set_of_all_used_vars == set()
    assert rwi.read_list == []
    assert rwi.signatures_read == []
    assert rwi.write_list == []
    assert rwi.signatures_written == []


def test_add_read():
    '''Test adding read variables with and without modules. '''

    rwi = ReadWriteInfo()
    sig_b = Signature("b")
    rwi.add_read(sig_b)
    correct = set()
    correct.add(("", sig_b))
    assert rwi.set_of_all_used_vars == correct
    assert rwi.read_list == [("", sig_b)]
    assert rwi.signatures_read == [sig_b]

    # Check that the results are sorted as expected: even though
    # 'a' is added later, it must be first in the output list:
    sig_a = Signature("a")
    rwi.add_read(sig_a)
    correct.add(("", sig_a))
    assert rwi.set_of_all_used_vars == correct
    assert rwi.read_list == [("", sig_a), ("", sig_b)]
    assert rwi.signatures_read == [sig_a, sig_b]

    sig_c = Signature("c")
    rwi.add_read(sig_c, "c_mod")
    correct.add(("c_mod", sig_c))
    assert rwi.set_of_all_used_vars == correct
    assert rwi.read_list == [("", sig_a), ("", sig_b), ("c_mod", sig_c)]
    assert rwi.signatures_read == [sig_a, sig_b, sig_c]

    assert rwi.is_read(sig_a) is True


def test_add_write():
    '''Test adding written variables with and without modules. '''

    rwi = ReadWriteInfo()
    sig_b = Signature("b")
    rwi.add_write(sig_b)
    correct = set()
    correct.add(("", sig_b))
    assert rwi.set_of_all_used_vars == correct
    assert rwi.write_list == [("", sig_b)]
    assert rwi.signatures_written == [sig_b]

    # Check that the results are sorted as expected: even though
    # 'a' is added later, it must be first in the output list:
    sig_a = Signature("a")
    rwi.add_write(sig_a)
    correct.add(("", sig_a))
    assert rwi.set_of_all_used_vars == correct
    assert rwi.write_list == [("", sig_a), ("", sig_b)]
    assert rwi.signatures_written == [sig_a, sig_b]

    sig_c = Signature("c")
    rwi.add_write(sig_c, "c_mod")
    correct.add(("c_mod", sig_c))
    assert rwi.set_of_all_used_vars == correct
    assert rwi.write_list == [("", sig_a), ("", sig_b), ("c_mod", sig_c)]
    assert rwi.signatures_written == [sig_a, sig_b, sig_c]

    assert rwi.is_read(sig_a) is False
