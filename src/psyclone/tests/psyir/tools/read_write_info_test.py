# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2023-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

'''This module tests the ReadWriteInfo class.'''

import logging

from psyclone.core import Signature
from psyclone.psyir.tools import ReadWriteInfo


def test_read_write_info() -> None:
    '''Test the ReadWriteInfo constructor.
    '''

    rwi = ReadWriteInfo()
    assert rwi.all_used_vars_list == []
    assert rwi.read_list == []
    assert rwi.signatures_read == []
    assert rwi.write_list == []
    assert rwi.signatures_written == []


def test_add_read() -> None:
    '''Test adding read variables with and without modules. '''

    rwi = ReadWriteInfo()
    sig_b = Signature("b")
    rwi.add_read(sig_b)
    correct = []
    correct.append(("", sig_b))
    assert rwi.all_used_vars_list == correct
    assert rwi.read_list == [("", sig_b)]
    assert rwi.signatures_read == [sig_b]

    # Check that the results are sorted as expected: even though
    # 'a' is added later, it must be first in the output list:
    sig_a = Signature("a")
    rwi.add_read(sig_a)
    correct.insert(0, ("", sig_a))
    assert rwi.all_used_vars_list == correct
    assert rwi.read_list == [("", sig_a), ("", sig_b)]
    assert rwi.signatures_read == [sig_a, sig_b]

    sig_c = Signature("c")
    rwi.add_read(sig_c, "c_mod")
    correct.append(("c_mod", sig_c))
    assert rwi.all_used_vars_list == correct
    assert rwi.read_list == [("", sig_a), ("", sig_b), ("c_mod", sig_c)]
    assert rwi.signatures_read == [sig_a, sig_b, sig_c]

    assert rwi.is_read(sig_a) is True


def test_add_write() -> None:
    '''Test adding written variables with and without modules. '''

    rwi = ReadWriteInfo()
    sig_b = Signature("b")
    rwi.add_write(sig_b)
    correct = []
    correct.append(("", sig_b))
    assert rwi.all_used_vars_list == correct
    assert rwi.write_list == [("", sig_b)]
    assert rwi.signatures_written == [sig_b]

    # Check that the results are sorted as expected: even though
    # 'a' is added later, it must be first in the output list:
    sig_a = Signature("a")
    rwi.add_write(sig_a)
    correct.insert(0, ("", sig_a))
    assert rwi.all_used_vars_list == correct
    assert rwi.write_list == [("", sig_a), ("", sig_b)]
    assert rwi.signatures_written == [sig_a, sig_b]

    sig_c = Signature("c")
    rwi.add_write(sig_c, "c_mod")
    correct.append(("c_mod", sig_c))
    assert rwi.all_used_vars_list == correct
    assert rwi.write_list == [("", sig_a), ("", sig_b), ("c_mod", sig_c)]
    assert rwi.signatures_written == [sig_a, sig_b, sig_c]

    assert rwi.is_read(sig_a) is False


def test_remove_var(caplog) -> None:
    '''Tests removing accesses to a variable.
    '''

    rwi = ReadWriteInfo()
    sig_a = Signature("a")
    sig_b = Signature("b")
    sig_c = Signature("c")
    sig_d = Signature("d")
    sig_e = Signature("e")
    rwi.add_read(sig_a)
    rwi.add_read(sig_b, "my_mod")
    rwi.add_read(sig_c)
    rwi.add_write(sig_c)
    rwi.add_write(sig_d)
    rwi.add_write(sig_e, "other_mod")

    # Note that the lists are sorted, first key being the modules:
    assert rwi.read_list == [("", sig_a), ("", sig_c), ("my_mod", sig_b)]
    assert rwi.write_list == [("", sig_c), ("", sig_d), ("other_mod", sig_e)]

    # Remove from read list only:
    rwi.remove(sig_a)
    assert rwi.read_list == [("", sig_c), ("my_mod", sig_b)]
    assert rwi.write_list == [("", sig_c), ("", sig_d), ("other_mod", sig_e)]

    # Remove from write list only:
    rwi.remove(sig_d)
    assert rwi.read_list == [("", sig_c), ("my_mod", sig_b)]
    assert rwi.write_list == [("", sig_c), ("other_mod", sig_e)]

    # sig_b must have the module name specified,
    # otherwise it must not be removed, and a warning must be logged:
    with caplog.at_level(logging.WARNING,
                         logger="psyclone.psyir.tools.read_write_info"):
        rwi.remove(sig_b)
    assert ("Variable 'b' is to be removed from ReadWriteInfo, but it's "
            "neither in the list of read variables" in caplog.text)

    assert rwi.read_list == [("", sig_c), ("my_mod", sig_b)]
    assert rwi.write_list == [("", sig_c), ("other_mod", sig_e)]

    # Remove from read list with the correct module name:
    rwi.remove(sig_b, "my_mod")
    assert rwi.read_list == [("", sig_c)]
    assert rwi.write_list == [("", sig_c), ("other_mod", sig_e)]

    # Remove from the write list with the correct module name
    rwi.remove(sig_e, "other_mod")
    assert rwi.read_list == [("", sig_c)]
    assert rwi.write_list == [("", sig_c)]

    # Test that a symbol is removed from both lists:
    rwi.remove(sig_c)
    assert rwi.read_list == []
    assert rwi.write_list == []
