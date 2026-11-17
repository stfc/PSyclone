# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2018-2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

''' Tests for the gen_kernel_stub module. '''

import os
import pytest

from psyclone.errors import GenerationError
from psyclone.gen_kernel_stub import generate
from psyclone.parse.algorithm import ParseError


def test_failures():
    '''Tests various failures of the generate() call.
    '''
    # Test file not found
    with pytest.raises(IOError) as err:
        generate("/does_not_exist", api="lfric")
    assert "File '/does_not_exist' not found" in str(err.value)

    # Check invalid API
    with pytest.raises(GenerationError) as err:
        generate("filename", api="invalid")
    assert ("Error: Kernel stub generator: Unsupported API 'invalid' "
            "specified." in str(err.value))

    # Trapping Fortran errors:
    with pytest.raises(ParseError) as err:
        # Use this python file to trigger invalid Fortran
        generate(__file__, api="lfric")
    assert ("Kernel stub generator: Code appears to be invalid "
            "Fortran" in str(err.value))


def test_gen_success():
    ''' Test for successful completion of the generate() function. '''
    base_path = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             "test_files", "lfric")
    stub_string = generate(os.path.join(base_path, "testkern_mod.F90"),
                           api="lfric")
    assert isinstance(stub_string, str)
