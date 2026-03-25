# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2026, Science and Technology Facilities Council.
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
# Authors R. W. Ford, A. R. Porter and S. Siso, STFC Daresbury Lab
# Modified I. Kavcic, A. Coughtrie, L. Turner and O. Brunt, Met Office,
#          C. M. Maynard, Met Office/University of Reading,
#          J. Henrichs, Bureau of Meteorology.

"""
This module tests the run-time checks functionality with the LFRic API
using pytest.
"""


from pathlib import Path
import pytest

from psyclone.configuration import Config
from psyclone.tests.lfric_build import LFRicBuild
from psyclone.tests.utilities import get_invoke


# constants
TEST_API = "lfric"


@pytest.mark.parametrize("level", [("warn", "LOG_LEVEL_WARNING"),
                                   ("error", "LOG_LEVEL_ERROR")])
def test_lfricinvoke_runtime(level: tuple[str, str],
                             tmp_path: Path,
                             monkeypatch: pytest.MonkeyPatch) -> None:
    '''Test that run-time checks are added to the PSy-layer via LFRicInvoke
    in the expected way (correct location and correct code).

    '''
    level_string, log_level = level
    # run-time checks are off by default so switch them on
    config = Config.get()
    lfric_config = config.api_conf("lfric")
    monkeypatch.setattr(lfric_config, "_run_time_checks", level_string)
    psy, _ = get_invoke("1_single_invoke.f90", TEST_API, idx=0, dist_mem=True)
    generated_code = str(psy.gen)
    assert "use testkern_mod, only : testkern_code" in generated_code
    assert f"use log_mod, only : {log_level}, log_event" in generated_code
    assert "use fs_continuity_mod" in generated_code
    assert "use mesh_mod, only : mesh_type" in generated_code
    expected = (
        "    m2_proxy = m2%get_proxy()\n"
        "    m2_data => m2_proxy%data\n"
        "\n"
        "    ! Perform run-time checks\n"
        "    ! Check field function space and kernel metadata function spac"
        "es are compatible\n"
        "    if (f1%which_function_space() /= W1) then\n"
        "      call log_event(\"In alg 'single_invoke' invoke 'invoke_0_tes"
        "tkern_type', the field 'f1' is passed to kernel 'testkern_code' but "
        "its function space is not compatible with the function space specifi"
        f"ed in the kernel metadata 'w1'.\", {log_level})\n"
        "    end if\n"
        "    if (f2%which_function_space() /= W2) then\n"
        "      call log_event(\"In alg 'single_invoke' invoke 'invoke_0_tes"
        "tkern_type', the field 'f2' is passed to kernel 'testkern_code' but "
        "its function space is not compatible with the function space specifi"
        f"ed in the kernel metadata 'w2'.\", {log_level})\n"
        "    end if\n"
        "    if (m1%which_function_space() /= W2) then\n"
        "      call log_event(\"In alg 'single_invoke' invoke 'invoke_0_tes"
        "tkern_type', the field 'm1' is passed to kernel 'testkern_code' but "
        "its function space is not compatible with the function space specifi"
        f"ed in the kernel metadata 'w2'.\", {log_level})\n"
        "    end if\n"
        "    if (m2%which_function_space() /= W3) then\n"
        "      call log_event(\"In alg 'single_invoke' invoke 'invoke_0_tes"
        "tkern_type', the field 'm2' is passed to kernel 'testkern_code' but "
        "its function space is not compatible with the function space specifi"
        f"ed in the kernel metadata 'w3'.\", {log_level})\n"
        "    end if\n"
        "\n"
        "    ! Check that read-only fields are not modified\n"
        "    if (f1_proxy%vspace%is_readonly()) then\n"
        "      call log_event(\"In alg 'single_invoke' invoke 'invoke_0_tes"
        "tkern_type', field 'f1' is on a read-only function space but is modi"
        f"fied by kernel 'testkern_code'.\", {log_level})\n"
        "    end if\n"
        "\n"
        "    ! Initialise number of layers\n")
    assert expected in generated_code
    assert LFRicBuild(tmp_path).code_compiles(psy)


def test_lfricinvoke_runtime_disabled(tmp_path) -> None:
    '''Test that no tests are added if they are disabled. This is the same
    example as the previous one, so just check that the generated code does
    not contain any "LOG_LEVEL" strings.
    '''

    psy, _ = get_invoke("1_single_invoke.f90", TEST_API, idx=0, dist_mem=True)
    generated_code = str(psy.gen)

    assert "LOG_LEVEL" not in generated_code
    assert LFRicBuild(tmp_path).code_compiles(psy)


def test_lfricruntimechecks_anyspace(tmp_path: Path,
                                     monkeypatch: pytest.MonkeyPatch) -> None:
    '''Test that run-time checks are not added for fields where the kernel
    metadata specifies anyspace.

    '''
    # run-time checks are off by default so switch them on
    config = Config.get()
    lfric_config = config.api_conf("lfric")
    monkeypatch.setattr(lfric_config, "_run_time_checks", "error")
    psy, _ = get_invoke("11_any_space.f90", TEST_API, idx=0, dist_mem=True)
    generated_code = str(psy.gen)
    assert "use function_space_mod, only : BASIS, DIFF_BASIS" in generated_code
    assert "use log_mod, only : LOG_LEVEL_ERROR, log_event" in generated_code
    assert "use fs_continuity_mod, only : W0\n" in generated_code
    assert "use mesh_mod, only : mesh_type" in generated_code
    expected2 = (
        "    c_proxy(3) = c(3)%get_proxy()\n"
        "    c_3_data => c_proxy(3)%data\n"
        "\n"
        "    ! Perform run-time checks\n"
        "    ! Check field function space and kernel metadata function spac"
        "es are compatible\n"
        "    if (c(1)%which_function_space() /= W0) then\n"
        "      call log_event(\"In alg 'any_space_example' invoke 'invoke_0"
        "_testkern_any_space_1_type', the field 'c' is passed to kernel 'test"
        "kern_any_space_1_code' but its function space is not compatible with"
        " the function space specified in the kernel metadata 'w0'.\", LOG_LE"
        "VEL_ERROR)\n"
        "    end if\n"
        "\n"
        "    ! Check that read-only fields are not modified\n"
        "    if (a_proxy%vspace%is_readonly()) then\n"
        "      call log_event(\"In alg 'any_space_example' invoke 'invoke_0"
        "_testkern_any_space_1_type', field 'a' is on a read-only function sp"
        "ace but is modified by kernel 'testkern_any_space_1_code'.\", LOG_LE"
        "VEL_ERROR)\n"
        "    end if\n"
        "\n"
        "    ! Initialise number of layers\n")
    assert expected2 in generated_code
    assert LFRicBuild(tmp_path).code_compiles(psy)


def test_lfricruntimechecks_vector(tmp_path: Path,
                                   monkeypatch: pytest.MonkeyPatch) -> None:
    ''' Test that run-time checks work for vector fields. '''
    # run-time checks are off by default so switch them on
    config = Config.get()
    lfric_config = config.api_conf("lfric")
    monkeypatch.setattr(lfric_config, "_run_time_checks", "error")
    psy, _ = get_invoke("8_vector_field_2.f90", TEST_API,
                        dist_mem=True, idx=0)

    generated_code = str(psy.gen)
    assert ("use testkern_coord_w0_2_mod, only : testkern_coord_w0_2_code"
            in generated_code)
    assert "use log_mod, only : LOG_LEVEL_ERROR, log_event" in generated_code
    assert "use fs_continuity_mod, only : W0\n" in generated_code
    assert "use mesh_mod, only : mesh_type" in generated_code
    expected2 = (
        "    f1_proxy = f1%get_proxy()\n"
        "    f1_data => f1_proxy%data\n"
        "\n"
        "    ! Perform run-time checks\n"
        "    ! Check field function space and kernel metadata function spac"
        "es are compatible\n"
        "    if (chi(1)%which_function_space() /= W0) then\n"
        "      call log_event(\"In alg 'vector_field' invoke 'invoke_0_test"
        "kern_coord_w0_2_type', the field 'chi' is passed to kernel 'testkern"
        "_coord_w0_2_code' but its function space is not compatible with the "
        "function space specified in the kernel metadata 'w0'.\", "
        "LOG_LEVEL_ERROR)\n"
        "    end if\n"
        "    if (f1%which_function_space() /= W0) then\n"
        "      call log_event(\"In alg 'vector_field' invoke 'invoke_0_test"
        "kern_coord_w0_2_type', the field 'f1' is passed to kernel 'testkern_"
        "coord_w0_2_code' but its function space is not compatible with the "
        "function space specified in the kernel metadata 'w0'.\", "
        "LOG_LEVEL_ERROR)\n"
        "    end if\n"
        "\n"
        "    ! Check that read-only fields are not modified\n"
        "    if (chi_proxy(1)%vspace%is_readonly()) then\n"
        "      call log_event(\"In alg 'vector_field' invoke 'invoke_0_test"
        "kern_coord_w0_2_type', field 'chi' is on a read-only function space "
        "but is modified by kernel 'testkern_coord_w0_2_code'.\", "
        "LOG_LEVEL_ERROR)\n"
        "    end if\n"
        "    if (f1_proxy%vspace%is_readonly()) then\n"
        "      call log_event(\"In alg 'vector_field' invoke 'invoke_0_test"
        "kern_coord_w0_2_type', field 'f1' is on a read-only function space "
        "but is modified by kernel 'testkern_coord_w0_2_code'.\", "
        "LOG_LEVEL_ERROR)\n"
        "    end if\n"
        "\n"
        "    ! Initialise number of layers\n")
    assert expected2 in generated_code
    assert LFRicBuild(tmp_path).code_compiles(psy)


def test_lfricruntimechecks_multikern(tmp_path: Path,
                                      monkeypatch: pytest.MonkeyPatch) -> None:
    ''' Test that run-time checks work when there are multiple kernels and
    at least one field is specified as being on a given function space
    more than once. In this case we want to avoid checking the same
    thing twice.

    '''
    # run-time checks are off by default so switch them on
    config = Config.get()
    lfric_config = config.api_conf("lfric")
    monkeypatch.setattr(lfric_config, "_run_time_checks", "error")
    psy, _ = get_invoke("1.2_multi_invoke.f90", TEST_API, idx=0,
                        dist_mem=True)
    generated_code = str(psy.gen)
    assert "use testkern_mod, only : testkern_code" in generated_code
    assert "use log_mod, only : LOG_LEVEL_ERROR, log_event" in generated_code
    assert "use mesh_mod, only : mesh_type" in generated_code
    assert "use fs_continuity_mod, only" in generated_code
    expected2 = (
        "    f3_proxy = f3%get_proxy()\n"
        "    f3_data => f3_proxy%data\n"
        "\n"
        "    ! Perform run-time checks\n"
        "    ! Check field function space and kernel metadata function spac"
        "es are compatible\n"
        "    if (f1%which_function_space() /= W1) then\n"
        "      call log_event(\"In alg 'multi_invoke' invoke 'invoke_0', th"
        "e field 'f1' is passed to kernel 'testkern_code' but its function sp"
        "ace is not compatible with the function space specified in the kerne"
        "l metadata 'w1'.\", LOG_LEVEL_ERROR)\n"
        "    end if\n"
        "    if (f2%which_function_space() /= W2) then\n"
        "      call log_event(\"In alg 'multi_invoke' invoke 'invoke_0', th"
        "e field 'f2' is passed to kernel 'testkern_code' but its function sp"
        "ace is not compatible with the function space specified in the kerne"
        "l metadata 'w2'.\", LOG_LEVEL_ERROR)\n"
        "    end if\n"
        "    if (m1%which_function_space() /= W2) then\n"
        "      call log_event(\"In alg 'multi_invoke' invoke 'invoke_0', th"
        "e field 'm1' is passed to kernel 'testkern_code' but its function sp"
        "ace is not compatible with the function space specified in the kerne"
        "l metadata 'w2'.\", LOG_LEVEL_ERROR)\n"
        "    end if\n"
        "    if (m2%which_function_space() /= W3) then\n"
        "      call log_event(\"In alg 'multi_invoke' invoke 'invoke_0', th"
        "e field 'm2' is passed to kernel 'testkern_code' but its function sp"
        "ace is not compatible with the function space specified in the kerne"
        "l metadata 'w3'.\", LOG_LEVEL_ERROR)\n"
        "    end if\n"
        "    if (f3%which_function_space() /= W2) then\n"
        "      call log_event(\"In alg 'multi_invoke' invoke 'invoke_0', th"
        "e field 'f3' is passed to kernel 'testkern_code' but its function sp"
        "ace is not compatible with the function space specified in the kerne"
        "l metadata 'w2'.\", LOG_LEVEL_ERROR)\n"
        "    end if\n"
        "    if (m2%which_function_space() /= W2) then\n"
        "      call log_event(\"In alg 'multi_invoke' invoke 'invoke_0', th"
        "e field 'm2' is passed to kernel 'testkern_code' but its function sp"
        "ace is not compatible with the function space specified in the kerne"
        "l metadata 'w2'.\", LOG_LEVEL_ERROR)\n"
        "    end if\n"
        "    if (m1%which_function_space() /= W3) then\n"
        "      call log_event(\"In alg 'multi_invoke' invoke 'invoke_0', th"
        "e field 'm1' is passed to kernel 'testkern_code' but its function sp"
        "ace is not compatible with the function space specified in the kerne"
        "l metadata 'w3'.\", LOG_LEVEL_ERROR)\n"
        "    end if\n"
        "\n"
        "    ! Check that read-only fields are not modified\n"
        "    if (f1_proxy%vspace%is_readonly()) then\n"
        "      call log_event(\"In alg 'multi_invoke' invoke 'invoke_0', fi"
        "eld 'f1' is on a read-only function space but is modified by kernel "
        "'testkern_code'.\", LOG_LEVEL_ERROR)\n"
        "    end if\n"
        "\n"
        "    ! Initialise number of layers\n")
    assert expected2 in generated_code
    assert LFRicBuild(tmp_path).code_compiles(psy)


def test_lfricruntimechecks_builtins(tmp_path: Path,
                                     monkeypatch: pytest.MonkeyPatch) -> None:
    '''Test that run-time checks work when there are builtins.'''
    # run-time checks are off by default so switch them on
    config = Config.get()
    lfric_config = config.api_conf("lfric")
    monkeypatch.setattr(lfric_config, "_run_time_checks", "error")
    psy, _ = get_invoke("15.1.1_X_plus_Y_builtin.f90", TEST_API, idx=0,
                        dist_mem=True)
    generated_code = str(psy.gen)
    assert "use log_mod, only : LOG_LEVEL_ERROR, log_event" in generated_code
    assert "use mesh_mod, only : mesh_type" in generated_code
    assert "type(field_type), intent(in) :: f3" in generated_code
    expected_code2 = (
        "    f2_proxy = f2%get_proxy()\n"
        "    f2_data => f2_proxy%data\n"
        "\n"
        "    ! Perform run-time checks\n"
        "    ! Check that read-only fields are not modified\n"
        "    if (f3_proxy%vspace%is_readonly()) then\n"
        "      call log_event(\"In alg 'single_invoke' invoke 'invoke_0', f"
        "ield 'f3' is on a read-only function space but is modified by kernel"
        " 'x_plus_y'.\", LOG_LEVEL_ERROR)\n"
        "    end if\n"
        "\n"
        "    ! Create a mesh object\n")
    assert expected_code2 in generated_code
    assert LFRicBuild(tmp_path).code_compiles(psy)


def test_lfricruntimechecks_anydiscontinuous(
        tmp_path: Path,
        monkeypatch: pytest.MonkeyPatch) -> None:
    '''Test that run-time checks work when we have checks for a field
    function space being consistent with an any_discontinuous_*
    function space.

    '''
    # run-time checks are off by default so switch them on
    config = Config.get()
    lfric_config = config.api_conf("lfric")
    monkeypatch.setattr(lfric_config, "_run_time_checks", "error")
    psy, _ = get_invoke("11.4_any_discontinuous_space.f90", TEST_API, idx=0,
                        dist_mem=True)
    generated_code = str(psy.gen)
    assert ("use testkern_any_discontinuous_space_op_1_mod, only : testkern_"
            "any_discontinuous_space_op_1_code") in generated_code
    assert "use log_mod, only : LOG_LEVEL_ERROR, log_event" in generated_code
    assert "use mesh_mod, only : mesh_type" in generated_code
    expected2 = (
        "    op4_proxy = op4%get_proxy()\n"
        "    op4_local_stencil => op4_proxy%local_stencil\n"
        "\n"
        "    ! Perform run-time checks\n"
        "    ! Check field function space and kernel metadata function spac"
        "es are compatible\n"
        "    if (f1(1)%which_function_space() /= W3 .AND. f1(1)%which_funct"
        "ion_space() /= WTHETA .AND. f1(1)%which_function_space() /= W2V .AND"
        ". f1(1)%which_function_space() /= W2VTRACE .AND. f1(1)%which_funct"
        "ion_space() /= W2BROKEN) then\n"
        "      call log_event(\"In alg 'any_discontinuous_space_op_example_"
        "1' invoke 'invoke_0_testkern_any_discontinuous_space_op_1_type', the"
        " field 'f1' is passed to kernel 'testkern_any_discontinuous_space_op"
        "_1_code' but its function space is not compatible with the function "
        "space specified in the kernel metadata 'any_discontinuous_space_1'."
        "\", LOG_LEVEL_ERROR)\n"
        "    end if\n"
        "    if (f2%which_function_space() /= W3 .AND. f2%which_function_sp"
        "ace() /= WTHETA .AND. f2%which_function_space() /= W2V .AND. f2%whic"
        "h_function_space() /= W2VTRACE .AND. f2%which_function_space() /= "
        "W2BROKEN) then\n"
        "      call log_event(\"In alg 'any_discontinuous_space_op_example_"
        "1' invoke 'invoke_0_testkern_any_discontinuous_space_op_1_type', the"
        " field 'f2' is passed to kernel 'testkern_any_discontinuous_space_op"
        "_1_code' but its function space is not compatible with the function "
        "space specified in the kernel metadata 'any_discontinuous_space_2'."
        "\", LOG_LEVEL_ERROR)\n"
        "    end if\n"
        "\n"
        "    ! Check that read-only fields are not modified\n"
        "    if (f2_proxy%vspace%is_readonly()) then\n"
        "      call log_event(\"In alg 'any_discontinuous_space_op_example_"
        "1' invoke 'invoke_0_testkern_any_discontinuous_space_op_1_type', fie"
        "ld 'f2' is on a read-only function space but is modified by kernel '"
        "testkern_any_discontinuous_space_op_1_code'.\", LOG_LEVEL_ERROR)\n"
        "    end if\n"
        "\n"
        "    ! Initialise number of layers\n")
    assert expected2 in generated_code
    assert LFRicBuild(tmp_path).code_compiles(psy)


def test_lfricruntimechecks_anyw2(tmp_path: Path,
                                  monkeypatch: pytest.MonkeyPatch) -> None:
    '''Test that run-time checks work when we have checks for a field
    function space being consistent with an anyw2 function
    space.

    '''
    # run-time checks are off by default so switch them on
    config = Config.get()
    lfric_config = config.api_conf("lfric")
    monkeypatch.setattr(lfric_config, "_run_time_checks", "error")
    psy, _ = get_invoke("21.1_single_invoke_multi_anyw2.f90", TEST_API,
                        idx=0, dist_mem=True)
    generated_code = str(psy.gen)
    assert ("use testkern_multi_anyw2_mod, only : testkern_multi_anyw2_code\n"
            in generated_code)
    assert "use log_mod, only : LOG_LEVEL_ERROR, log_event" in generated_code
    expected2 = (
        "\n"
        "    ! Perform run-time checks\n"
        "    ! Check field function space and kernel metadata function spac"
        "es are compatible\n"
        "    if (f1%which_function_space() /= W2 .AND. f1%which_function_sp"
        "ace() /= W2H .AND. f1%which_function_space() /= W2V .AND. f1%which_f"
        "unction_space() /= W2BROKEN) then\n"
        "      call log_event(\"In alg 'single_invoke_multi_anyw2' invoke '"
        "invoke_0_testkern_multi_anyw2_type', the field 'f1' is passed to ker"
        "nel 'testkern_multi_anyw2_code' but its function space is not compat"
        "ible with the function space specified in the kernel metadata 'any_w"
        "2'.\", LOG_LEVEL_ERROR)\n"
        "    end if\n"
        "    if (f2%which_function_space() /= W2 .AND. f2%which_function_sp"
        "ace() /= W2H .AND. f2%which_function_space() /= W2V .AND. f2%which_f"
        "unction_space() /= W2BROKEN) then\n"
        "      call log_event(\"In alg 'single_invoke_multi_anyw2' invoke '"
        "invoke_0_testkern_multi_anyw2_type', the field 'f2' is passed to ker"
        "nel 'testkern_multi_anyw2_code' but its function space is not compat"
        "ible with the function space specified in the kernel metadata 'any_w"
        "2'.\", LOG_LEVEL_ERROR)\n"
        "    end if\n"
        "    if (f3%which_function_space() /= W2 .AND. f3%which_function_sp"
        "ace() /= W2H .AND. f3%which_function_space() /= W2V .AND. f3%which_f"
        "unction_space() /= W2BROKEN) then\n"
        "      call log_event(\"In alg 'single_invoke_multi_anyw2' invoke '"
        "invoke_0_testkern_multi_anyw2_type', the field 'f3' is passed to ker"
        "nel 'testkern_multi_anyw2_code' but its function space is not compat"
        "ible with the function space specified in the kernel metadata 'any_w"
        "2'.\", LOG_LEVEL_ERROR)\n"
        "    end if\n"
        "\n"
        "    ! Check that read-only fields are not modified\n"
        "    if (f1_proxy%vspace%is_readonly()) then\n"
        "      call log_event(\"In alg 'single_invoke_multi_anyw2' invoke '"
        "invoke_0_testkern_multi_anyw2_type', field 'f1' is on a read-only fu"
        "nction space but is modified by kernel 'testkern_multi_anyw2_code'."
        "\", LOG_LEVEL_ERROR)\n"
        "    end if\n"
        "\n"
        "    ! Initialise number of layers\n")
    assert expected2 in generated_code
    assert LFRicBuild(tmp_path).code_compiles(psy)
