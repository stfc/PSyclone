# -----------------------------------------------------------------------------
# SPDX-FileCopyrightText: Copyright (c) 2026 Science and Technology
#                         Facilities Council
# SPDX-License-Identifier: BSD-3-Clause
# See the full LICENSE file in the project root for details.
# -----------------------------------------------------------------------------

"""Tests for the KernelInlineTrans transformation."""

import pytest

from fparser.common.readfortran import FortranStringReader
from psyclone.domain.common.transformations import (
    KernelInlineTrans, KernelModuleInlineTrans)
from psyclone.psyGen import CodedKern
from psyclone.psyir.nodes import (
    ArrayReference, Assignment, Call, CodeBlock, Fparser2CodeBlock, Literal,
    Node, Reference)
from psyclone.psyir.symbols import ImportInterface, ScalarType
from psyclone.psyir.transformations import TransformationError
from psyclone.tests.lfric_build import LFRicBuild
from psyclone.tests.utilities import get_invoke
from psyclone.transformations import LFRicColourTrans


def _add_lfric_kernel_body(kern, use_map=False, statement_count=1):
    """Add a simple executable body to an otherwise empty test kernel.

    The test kernel source uses wildcard imports for the LFRic kind symbols.
    Make these imports explicit so that InlineTrans can prove that the body
    has no unresolved outer-scope accesses.

    :param kern: the module-inlined LFRic kernel.
    :param bool use_map: whether to index the output field using its dofmap.
    :param int statement_count: number of assignments to add.

    """
    routine = kern.get_callees()[0]
    constants = routine.symbol_table.lookup("constants_mod")
    routine.symbol_table.lookup("i_def").interface = ImportInterface(
        constants)
    routine.symbol_table.lookup("r_def").interface = ImportInterface(
        constants)

    arguments = routine.symbol_table.argument_list
    if kern.iterates_over == "domain":
        scalar_idx = 2
        field_idx = 3
    else:
        scalar_idx = 1
        field_idx = 2
    for idx in range(1, statement_count + 1):
        index = Literal(str(idx), ScalarType.integer_type())
        if use_map:
            index = ArrayReference.create(arguments[8], [index])
        lhs = ArrayReference.create(arguments[field_idx], [index])
        routine.addchild(Assignment.create(
            lhs, Reference(arguments[scalar_idx])))


def test_kernel_inline_trans_str_and_invalid_target():
    """Test the transformation description and target-type validation."""
    trans = KernelInlineTrans()
    assert str(trans) == (
        "Mark a PSyKAl kernel for inlining when it is lowered.")

    with pytest.raises(TransformationError) as err:
        trans.apply(Node())
    assert ("Target of a KernelInlineTrans must be a sub-class of "
            "psyGen.CodedKern but got 'Node'" in str(err.value))


def test_kernel_inline_trans_marks_only_target_call():
    """The transformation marks only the supplied call site."""
    _, invoke = get_invoke("4.2_multikernel_invokes.f90", "lfric",
                           idx=0, dist_mem=False)
    kernels = invoke.schedule.walk(CodedKern)
    trans = KernelInlineTrans()

    assert not kernels[0].inline
    trans.apply(kernels[0])
    trans.apply(kernels[0])
    assert kernels[0].inline
    assert not kernels[1].inline

    copied_kernels = invoke.schedule.copy().walk(CodedKern)
    assert copied_kernels[0].inline
    assert not copied_kernels[1].inline


def test_kernel_inline_trans_defers_body_validation(parser, capsys):
    """Routine-body restrictions are checked only during lowering."""
    _, invoke = get_invoke("1_single_invoke.f90", "lfric",
                           idx=0, dist_mem=False)
    kernel = invoke.schedule.walk(CodedKern)[0]
    KernelModuleInlineTrans().apply(kernel)
    routine = kernel.get_callees()[0]
    reader = FortranStringReader("""
        subroutine test()
          write(*,*) 'unsupported'
        end subroutine test
    """)
    statement = parser(reader).children[0].children[1]
    routine.addchild(Fparser2CodeBlock(
        statement, CodeBlock.Structure.STATEMENT))

    # Setting the flag is intentionally a locality-only operation.
    KernelInlineTrans().apply(kernel)
    assert kernel.inline
    lowered = kernel.lower_to_language_level()
    assert isinstance(lowered, Call)
    assert capsys.readouterr().out == (
        "Deferred-Inline failed for kernel 'testkern_code_inlined_' due to: "
        "Transformation Error: Routine 'testkern_code_inlined_' contains "
        "one or more CodeBlocks and therefore cannot be inlined. (If you "
        "are confident that the code may safely be inlined despite this "
        "then use the `permit_codeblocks=True` argument to "
        "InlineTrans.apply() to override.)\n")


def test_kernel_inline_trans_lfric_colouring(tmpdir):
    """Test deferred LFRic arguments, loop bounds and generated code."""
    psy, invoke = get_invoke("1_single_invoke.f90", "lfric",
                             idx=0, dist_mem=False)
    kernel = invoke.schedule.walk(CodedKern)[0]
    KernelModuleInlineTrans().apply(kernel)
    _add_lfric_kernel_body(kernel, use_map=True)
    KernelInlineTrans().apply(kernel)

    # Colouring after the inline request changes arguments that are only
    # constructed when the kernel is lowered.
    LFRicColourTrans().apply(invoke.schedule.children[0])
    code = str(psy.gen)

    assert "call testkern_code_inlined_" not in code
    assert "do cell = loop1_start, last_edge_cell_all_colours(colour), 1" \
        in code
    assert ("f1_data(map_w1(1 - 1 + LBOUND(map_w1, dim=1),"
            "cmap(colour,cell))) = a" in code)
    # Code generation lowers a copy and must preserve the DSL-level original.
    assert isinstance(invoke.schedule.walk(CodedKern)[0], CodedKern)
    assert invoke.schedule.walk(CodedKern)[0].inline
    assert LFRicBuild(tmpdir).code_compiles(psy)


def test_kernel_inline_trans_gocean(capsys):
    """The deferred transformation is generic across CodedKern APIs."""
    psy, invoke = get_invoke("single_invoke.f90", "gocean",
                             idx=0, dist_mem=False)
    kernel = invoke.schedule.walk(CodedKern)[0]
    KernelModuleInlineTrans().apply(kernel)
    KernelInlineTrans().apply(kernel)

    code = str(psy.gen)
    assert "call compute_cu_code_inlined_" not in code
    assert "0.5d0" in code
    assert capsys.readouterr().out == (
        "Deferred-Inline successful for kernel 'compute_cu_code_inlined_'\n")


def test_kernel_inline_trans_empty_body_does_not_skip_sibling():
    """Removing an empty marked kernel must not skip a following kernel."""
    _, invoke = get_invoke("4.2_multikernel_invokes.f90", "lfric",
                           idx=0, dist_mem=False)
    schedule = invoke.schedule
    second_loop = schedule.children[1].detach()
    second_kernel = second_loop.loop_body[0].detach()
    schedule.children[0].loop_body.addchild(second_kernel)
    kernels = schedule.walk(CodedKern)
    KernelModuleInlineTrans().apply(kernels[0])
    KernelInlineTrans().apply(kernels[0])

    schedule.lower_to_language_level()
    assert len(schedule.children[0].loop_body.children) == 1
    assert isinstance(schedule.children[0].loop_body[0], Call)


@pytest.mark.parametrize("statement_count", [0, 2])
def test_kernel_inline_trans_domain_splicing(statement_count):
    """A domain kernel may inline to zero or multiple statements."""
    _, invoke = get_invoke("25.1_kern_two_domain.f90", "lfric",
                           idx=0, dist_mem=False)
    schedule = invoke.schedule
    # Keep one domain loop so that the final number of statements is solely
    # determined by the inlined routine body.
    schedule.children[1].detach()
    kernel = schedule.walk(CodedKern)[0]
    KernelModuleInlineTrans().apply(kernel)
    if statement_count:
        _add_lfric_kernel_body(kernel, statement_count=statement_count)
    KernelInlineTrans().apply(kernel)

    schedule.lower_to_language_level()
    assert len(schedule.children) == statement_count
    assert all(isinstance(child, Assignment) for child in schedule.children)


def test_kernel_inline_trans_rechecks_callees(monkeypatch, capsys):
    """Polymorphism introduced after marking is caught during lowering."""
    _, invoke = get_invoke("1_single_invoke.f90", "lfric",
                           idx=0, dist_mem=False)
    kernel = invoke.schedule.walk(CodedKern)[0]
    KernelModuleInlineTrans().apply(kernel)
    KernelInlineTrans().apply(kernel)
    routine = kernel.get_callees()[0]
    monkeypatch.setattr(Call, "get_callees", lambda _: [routine, routine])

    lowered = kernel.lower_to_language_level()
    assert isinstance(lowered, Call)
    assert capsys.readouterr().out == (
        "Deferred-Inline failed for kernel 'testkern_code_inlined_' due to: "
        "Transformation Error: Cannot inline routine "
        "'testkern_code_inlined_' because its call has 2 possible callees. "
        "The 'allow_no_args_check_if_only_one_callee' option requires exactly "
        "one callee.\n")
