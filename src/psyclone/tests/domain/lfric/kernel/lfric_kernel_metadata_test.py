# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2022-2024, Science and Technology Facilities Council
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
# Authors: R. W. Ford and A. R. Porter, STFC Daresbury Lab

'''Module containing tests for the LFRicKernelMetadata class.

'''
import pytest

from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003

from psyclone.domain.lfric.kernel import (
    ColumnwiseOperatorArgMetadata, EvaluatorTargetsMetadata, FieldArgMetadata,
    FieldVectorArgMetadata, InterGridArgMetadata, InterGridVectorArgMetadata,
    LFRicKernelMetadata, MetaMeshArgMetadata, MetaFuncsArgMetadata,
    OperatesOnMetadata, OperatorArgMetadata, MetaRefElementArgMetadata,
    ScalarArgMetadata, ShapesMetadata)
from psyclone.errors import InternalError
from psyclone.parse.utils import ParseError
from psyclone.psyir.symbols import DataTypeSymbol, REAL_TYPE, \
    UnsupportedFortranType

# pylint: disable=too-many-statements


def test_init_noargs():
    '''Test that a LFRicKernelMetadata instance can be created
    successfully when no arguments are provided.

    '''
    meta = LFRicKernelMetadata()
    assert isinstance(meta, LFRicKernelMetadata)
    assert meta._operates_on is None
    assert meta._shapes is None
    assert meta._evaluator_targets is None
    assert meta._meta_args is None
    assert meta._meta_funcs is None
    assert meta._meta_ref_element is None
    assert meta._meta_mesh is None
    assert meta._procedure_name is None
    assert meta._name is None


def test_init_args():
    '''Test that a LFRicKernelMetadata instance can be created
    successfully when valid arguments are provided.

    '''
    scalar_arg = ScalarArgMetadata("GH_REAL", "GH_READ")
    meta_funcs_arg = MetaFuncsArgMetadata("w0", basis_function=True)
    meta_ref_element_arg = MetaRefElementArgMetadata("normals_to_faces")
    meta_mesh_arg = MetaMeshArgMetadata("adjacent_face")
    meta = LFRicKernelMetadata(
        operates_on="DOMAIN", shapes=["GH_EVALUATOR"],
        evaluator_targets=["W0"], meta_args=[scalar_arg],
        meta_funcs=[meta_funcs_arg], meta_ref_element=[meta_ref_element_arg],
        meta_mesh=[meta_mesh_arg], procedure_name="KERN_CODE",
        name="kern_type")

    assert isinstance(meta._operates_on, OperatesOnMetadata)
    assert meta._operates_on.operates_on == "domain"
    assert isinstance(meta._shapes, ShapesMetadata)
    assert meta._shapes.shapes == ["gh_evaluator"]
    assert isinstance(meta._evaluator_targets, EvaluatorTargetsMetadata)
    assert meta._evaluator_targets.evaluator_targets == ["w0"]
    assert meta.meta_args == [scalar_arg]
    # For some reason the equality test does not work for meta_funcs,
    # so use the fortran output instead to check for validity.
    assert (meta._meta_funcs.fortran_string() ==
            "type(FUNC_TYPE) :: META_FUNCS(1) = (/ &\n"
            "    func_type(w0, gh_basis)/)\n")
    assert meta.meta_ref_element == [meta_ref_element_arg]
    assert meta.meta_mesh == [meta_mesh_arg]
    assert meta.procedure_name == "KERN_CODE"
    assert meta.name == "kern_type"


def test_init_args_error():
    '''Test that a LFRicKernelMetadata instance raises the expected
    exceptions when invalid argument values are provided.

    '''
    with pytest.raises(ValueError) as info:
        _ = LFRicKernelMetadata(operates_on="invalid")
    assert ("The 'OPERATES_ON' metadata should be a recognised value "
            "(one of ['cell_column', 'domain', 'dof']) but found 'invalid'."
            in str(info.value))

    with pytest.raises(TypeError) as info:
        _ = LFRicKernelMetadata(shapes="invalid")
    assert ("ShapesMetadata values should be provided as a list but found "
            "'str'." in str(info.value))

    with pytest.raises(TypeError) as info:
        _ = LFRicKernelMetadata(evaluator_targets="invalid")
    assert ("EvaluatorTargetsMetadata values should be provided as a list but "
            "found 'str'." in str(info.value))

    with pytest.raises(TypeError) as info:
        _ = LFRicKernelMetadata(meta_args="error")
    assert ("MetaArgsMetadata values should be provided as a list but found "
            "'str'." in str(info.value))

    with pytest.raises(TypeError) as info:
        _ = LFRicKernelMetadata(meta_funcs="invalid")
    assert ("MetaFuncsMetadata values should be provided as a list but found "
            "'str'." in str(info.value))

    with pytest.raises(TypeError) as info:
        _ = LFRicKernelMetadata(meta_ref_element="invalid")
    assert ("MetaRefElementMetadata values should be provided as a list but "
            "found 'str'." in str(info.value))

    with pytest.raises(TypeError) as info:
        _ = LFRicKernelMetadata(meta_mesh="invalid")
    assert ("MetaMeshMetadata values should be provided as a list but found "
            "'str'." in str(info.value))

    with pytest.raises(ValueError) as info:
        _ = LFRicKernelMetadata(procedure_name="1_invalid")
    assert ("Expected procedure_name to be a valid Fortran name but found "
            "'1_invalid'." in str(info.value))

    with pytest.raises(ValueError) as info:
        _ = LFRicKernelMetadata(name="1_invalid")
    assert "Invalid Fortran name '1_invalid' found." in str(info.value)


def test_validation_error_str():
    '''Test that the _validation_error_str utility method behaves as
    expected.

    '''
    lfric_kernel_metadata = LFRicKernelMetadata()
    # Kernel name and procedure name not set.
    result = lfric_kernel_metadata._validation_error_str("xxx")
    assert result == "xxx in kernel metadata 'unset' for procedure 'unset'."
    # Kernel name set and procedure name not set.
    lfric_kernel_metadata.name = "kernel_name"
    result = lfric_kernel_metadata._validation_error_str("xxx")
    assert (result == "xxx in kernel metadata 'kernel_name' for procedure "
            "'unset'.")
    # Kernel name and procedure name set.
    lfric_kernel_metadata.procedure_name = "procedure_name"
    result = lfric_kernel_metadata._validation_error_str("xxx")
    assert (result == "xxx in kernel metadata 'kernel_name' for procedure "
            "'procedure_name'.")
    # Kernel name not set and procedure name set.
    lfric_kernel_metadata._name = None
    result = lfric_kernel_metadata._validation_error_str("xxx")
    assert (result == "xxx in kernel metadata 'unset' for procedure "
            "'procedure_name'.")


def test_validate():
    '''Test that the validate method behaves as expected.'''

    # _get_kernel_type called (exception raised)
    lfric_kernel_metadata = LFRicKernelMetadata(operates_on="cell_column")
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata.validate()
    assert (
        "Kernel metadata with 'operates_on != domain' must have at least one "
        "meta_args argument that is a field, field vector, intergrid field, "
        "intergrid vector field, LMA operator or CMA operator (in order to "
        "determine the appropriate iteration space), however this metadata "
        "has none in kernel metadata 'unset' for procedure 'unset'."
        in str(info.value))

    # OK
    meta_args = [FieldArgMetadata("gh_real", "gh_write", "w0")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    lfric_kernel_metadata.validate()


def test_get_kernel_type():
    '''Test that the _get_kernel_type method behaves as expected.'''

    # inter-grid
    meta_args = [
        InterGridArgMetadata("gh_real", "gh_read", "w0", "gh_coarse"),
        InterGridArgMetadata("gh_real", "gh_read", "w1", "gh_fine")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    assert lfric_kernel_metadata._get_kernel_type() == "inter-grid"

    # cma (assembly example)
    meta_args = [
        ColumnwiseOperatorArgMetadata("gh_real", "gh_write", "w0", "w1"),
        OperatorArgMetadata("gh_real", "gh_read", "w0", "w1")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    assert lfric_kernel_metadata._get_kernel_type() == "cma-assembly"

    # domain
    meta_args = [FieldArgMetadata("gh_real", "gh_read", "w3")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="domain", meta_args=meta_args)
    assert lfric_kernel_metadata._get_kernel_type() == "domain"

    # general-purpose
    meta_args = [FieldArgMetadata("gh_real", "gh_read", "w0")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    assert lfric_kernel_metadata._get_kernel_type() == "general-purpose"


def test_validate_generic_kernel():
    '''Test that the _validate_generic_kernel method behaves as
    expected.

    '''
    lfric_kernel_metadata = LFRicKernelMetadata(operates_on="cell_column")
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata._validate_generic_kernel()
    assert ("Kernel metadata with 'operates_on != domain' must have at least "
            "one meta_args argument that is a field, field vector, intergrid "
            "field, intergrid vector field, LMA operator or CMA operator (in "
            "order to determine the appropriate iteration space), however "
            "this metadata has none in kernel metadata 'unset' for "
            "procedure 'unset'." in str(info.value))

    operator_arg = OperatorArgMetadata("GH_REAL", "GH_READ", "W0", "W1")
    integer_field_arg = FieldArgMetadata("GH_INTEGER", "GH_READ", "W0")
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=[operator_arg, integer_field_arg])
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata._validate_generic_kernel()
    assert ("Kernel metadata with a meta_args operator argument must only "
            "contain meta_args real-valued field arguments, however found a "
            "field of type 'gh_integer' in kernel metadata 'unset' for "
            "procedure 'unset'." in str(info.value))

    # OK
    meta_args = [FieldArgMetadata("GH_REAL", "GH_READ", "W0")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    lfric_kernel_metadata._validate_generic_kernel()


def test_validate_general_purpose_kernel():
    '''Test that the _validate_general_purpose_kernel method behaves as
    expected.

    '''
    # Calls generic constraints method.
    lfric_kernel_metadata = LFRicKernelMetadata(operates_on="cell_column")
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata._validate_general_purpose_kernel()
    assert (
        "Kernel metadata with 'operates_on != domain' must have at least one "
        "meta_args argument that is a field, field vector, intergrid field, "
        "intergrid vector field, LMA operator or CMA operator (in order to "
        "determine the appropriate iteration space), however this metadata "
        "has none in kernel metadata 'unset' for procedure 'unset'."
        in str(info.value))

    # Does not operate over the domain.
    lfric_kernel_metadata = LFRicKernelMetadata(operates_on="domain")
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata._validate_general_purpose_kernel()
    assert ("A general purpose kernel should not operate on a domain, "
            "however this does in kernel metadata 'unset' for procedure "
            "'unset'." in str(info.value))

    # operates_on == cell_column only supports field, field vector,
    # LMA operator, or scalar meta_arg arguments.
    meta_args = [InterGridArgMetadata("GH_REAL", "GH_READ", "W0", "GH_FINE")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata._validate_general_purpose_kernel()
    assert ("General purpose kernels with 'operates_on == cell_column' should "
            "only have meta_arg arguments of type field, field vector, LMA "
            "operator or scalar, but found 'inter-grid' in kernel metadata "
            "'unset' for procedure 'unset'." in str(info.value))

    # OK
    meta_args = [FieldArgMetadata("GH_REAL", "GH_READ", "W0")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    lfric_kernel_metadata._validate_general_purpose_kernel()


def test_validate_domain_kernel():
    '''Test that the _validate_domain_kernel method behaves as
    expected.

    '''
    # Check validate_generic_kernel is called.
    lfric_kernel_metadata = LFRicKernelMetadata(operates_on="cell_column")
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata._validate_domain_kernel()
    assert ("Kernel metadata with 'operates_on != domain' must have at least "
            "one meta_args argument that is a field, field vector, intergrid "
            "field, intergrid vector field, LMA operator or CMA operator (in "
            "order to determine the appropriate iteration space), however "
            "this metadata has none in kernel metadata 'unset' for procedure "
            "'unset'." in str(info.value))

    # operates_on == domain.
    meta_args = [FieldArgMetadata("GH_REAL", "GH_READ", "W0")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="dof", meta_args=meta_args)
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata._validate_domain_kernel()
    assert ("Domain kernels should have their operates_on metadata set to "
            "'domain', but found 'dof' in kernel metadata 'unset' for "
            "procedure 'unset'." in str(info.value))

    # Only scalar, field and field vector args.
    meta_args = [InterGridArgMetadata("GH_REAL", "GH_READ", "W0", "GH_FINE")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="domain", meta_args=meta_args)
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata._validate_domain_kernel()
    assert ("Domain kernels should only have meta_arg arguments of type "
            "field, field vector, or scalar, but found 'inter-grid' in "
            "kernel metadata 'unset' for procedure 'unset'."
            in str(info.value))

    # All fields on discontinuous function spaces.
    meta_args = [FieldArgMetadata("GH_REAL", "GH_READ", "W0")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="domain", meta_args=meta_args)
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata._validate_domain_kernel()
    assert ("Domain kernels meta_arg arguments of type field, or field vector "
            "should be on a discontinuous function space, but found 'w0' in "
            "kernel metadata 'unset' for procedure 'unset'."
            in str(info.value))

    # stencil accesses not allowed.
    meta_args = [FieldArgMetadata(
        "gh_real", "gh_read", "w3", stencil="region")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="domain", meta_args=meta_args)
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata._validate_domain_kernel()
    assert ("Domain kernels meta_arg arguments of type field, or field vector "
            "should not have any stencil accesses, but found a stencil of "
            "type 'region' in kernel metadata 'unset' for procedure 'unset'."
            in str(info.value))

    # No basis/diff basis functions.
    meta_args = [FieldArgMetadata(
        "gh_real", "gh_read", "w3")]
    meta_funcs = [MetaFuncsArgMetadata("w3", basis_function=True)]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="domain", meta_args=meta_args, meta_funcs=meta_funcs)
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata._validate_domain_kernel()
    assert ("Domain kernels should not specify basis or differential basis "
            "functions metadata, but this does in kernel metadata "
            "'unset' for procedure 'unset'." in str(info.value))

    # No mesh properties.
    meta_args = [FieldArgMetadata(
        "gh_real", "gh_read", "w3")]
    meta_mesh = [MetaMeshArgMetadata("adjacent_face")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="domain", meta_args=meta_args, meta_mesh=meta_mesh)
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata._validate_domain_kernel()
    assert ("Domain kernels should not specify mesh property metadata, "
            "but this does in kernel metadata 'unset' for procedure 'unset'."
            in str(info.value))

    # OK
    meta_args = [FieldArgMetadata("gh_real", "gh_read", "w3")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="domain", meta_args=meta_args)
    lfric_kernel_metadata._validate_domain_kernel()


def test_cma_kernel_type():
    '''Test that the _cma_kernel_type method behaves as expected.'''

    # cma assembly kernel
    meta_args = [
        ColumnwiseOperatorArgMetadata("gh_real", "gh_write", "w0", "w1"),
        OperatorArgMetadata("gh_real", "gh_read", "w0", "w1")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    assert lfric_kernel_metadata._cma_kernel_type() == "assembly"

    # cma apply kernel
    meta_args = [
        ColumnwiseOperatorArgMetadata("gh_real", "gh_read", "w3", "w1"),
        FieldArgMetadata("gh_real", "gh_read", "w1"),
        FieldArgMetadata("gh_real", "gh_write", "w3")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    assert lfric_kernel_metadata._cma_kernel_type() == "apply"

    # cma matrix-matrix kernel
    meta_args = [
        ColumnwiseOperatorArgMetadata("gh_real", "gh_write", "w3", "w2"),
        ColumnwiseOperatorArgMetadata("gh_real", "gh_read", "w3", "w2"),
        ScalarArgMetadata("gh_real", "gh_read")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    assert lfric_kernel_metadata._cma_kernel_type() == "matrix-matrix"


def test_validate_generic_cma_kernel():
    '''Test that the _validate_generic_cma_kernel method behaves as
    expected.

    '''
    # Check validate_generic_kernel is called.
    lfric_kernel_metadata = LFRicKernelMetadata(operates_on="cell_column")
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata._validate_generic_cma_kernel()
    assert ("Kernel metadata with 'operates_on != domain' must have at least "
            "one meta_args argument that is a field, field vector, intergrid "
            "field, intergrid vector field, LMA operator or CMA operator (in "
            "order to determine the appropriate iteration space), however "
            "this metadata has none in kernel metadata 'unset' for "
            "procedure 'unset'." in str(info.value))

    # Operates on cell_column.
    lfric_kernel_metadata = LFRicKernelMetadata(operates_on="domain")
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata._validate_generic_cma_kernel()
    assert ("A CMA kernel should only operate on a 'cell_column', but found "
            "'domain' in kernel metadata 'unset' for procedure 'unset'."
            in str(info.value))

    # At least one cma operator.
    meta_args = [FieldArgMetadata(
        "gh_real", "gh_read", "w3")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata._validate_generic_cma_kernel()
    assert ("A CMA kernel should contain at least one cma operator argument "
            "but none are specified in the meta_args metadata in kernel "
            "metadata 'unset' for procedure 'unset'." in str(info.value))

    # No intergrid arguments.
    meta_args = [
        ColumnwiseOperatorArgMetadata("gh_real", "gh_read", "w3", "w2"),
        InterGridArgMetadata("gh_real", "gh_read", "w3", "gh_coarse")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata._validate_generic_cma_kernel()
    assert ("A CMA kernel should not contain any intergrid arguments, but at "
            "least one was found in kernel metadata 'unset' for procedure "
            "'unset'." in str(info.value))

    # No field vector arguments.
    meta_args = [
        ColumnwiseOperatorArgMetadata("gh_real", "gh_read", "w3", "w2"),
        FieldVectorArgMetadata("gh_real", "gh_read", "w3", "3")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata._validate_generic_cma_kernel()
    assert ("A CMA kernel should not contain any field vector arguments, but "
            "at least one was found in kernel metadata 'unset' for procedure "
            "'unset'." in str(info.value))

    # No stencils allowed.
    meta_args = [
        ColumnwiseOperatorArgMetadata("gh_real", "gh_read", "w3", "w2"),
        FieldArgMetadata("gh_real", "gh_read", "w3", stencil="region")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata._validate_generic_cma_kernel()
    assert ("A CMA kernel should not contain any fields with stencil "
            "accesses, but at least one was found in kernel metadata 'unset' "
            "for procedure 'unset'." in str(info.value))

    # OK
    meta_args = [
        ColumnwiseOperatorArgMetadata("gh_real", "gh_read", "w3", "w2")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    lfric_kernel_metadata._validate_generic_cma_kernel()


def test_validate_cma_assembly_kernel():
    '''Test that the _validate_cma_assembly_kernel method behaves as
    expected.

    '''
    # Check validate_generic_cma_kernel is called.
    lfric_kernel_metadata = LFRicKernelMetadata(operates_on="domain")
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata._validate_cma_assembly_kernel()
    assert ("A CMA kernel should only operate on a 'cell_column', but found "
            "'domain' in kernel metadata 'unset' for procedure 'unset'."
            in str(info.value))

    # One CMA operator.
    meta_args = [
        ColumnwiseOperatorArgMetadata("gh_real", "gh_read", "w3", "w2"),
        ColumnwiseOperatorArgMetadata("gh_real", "gh_read", "w0", "w1")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata._validate_cma_assembly_kernel()
    assert ("A CMA assembly kernel should contain one CMA operator argument, "
            "however 2 were found in kernel metadata 'unset' for procedure "
            "'unset'." in str(info.value))

    # CMA operator write access.
    meta_args = [
        ColumnwiseOperatorArgMetadata("gh_real", "gh_read", "w0", "w1")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata._validate_cma_assembly_kernel()
    assert ("A CMA assembly kernel should contain one CMA operator argument "
            "with write access, however it has access 'gh_read' in kernel "
            "metadata 'unset' for procedure 'unset'." in str(info.value))

    meta_args = [
        ColumnwiseOperatorArgMetadata("gh_real", "gh_write", "w0", "w1")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata._validate_cma_assembly_kernel()
    assert ("A CMA assembly kernel should contain at least one LMA operator "
            "but none were found in kernel metadata 'unset' for procedure "
            "'unset'." in str(info.value))

    # All args except CMA are read-only
    meta_args = [
        ColumnwiseOperatorArgMetadata("gh_real", "gh_write", "w0", "w1"),
        OperatorArgMetadata("gh_real", "gh_write", "w0", "w1")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata._validate_cma_assembly_kernel()
    assert ("A CMA assembly kernel should have all arguments as read-only "
            "apart from the CMA argument, but found non-CMA argument with "
            "access 'gh_write' in kernel metadata 'unset' for procedure "
            "'unset'." in str(info.value))

    # OK.
    meta_args = [
        ColumnwiseOperatorArgMetadata("gh_real", "gh_write", "w0", "w1"),
        OperatorArgMetadata("gh_real", "gh_read", "w0", "w1")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    lfric_kernel_metadata._validate_cma_assembly_kernel()


def test_validate_cma_apply_kernel():
    '''Test that the _validate_cma_apply_kernel method behaves as
    expected.

    '''
    # Check validate_generic_cma_kernel is called.
    lfric_kernel_metadata = LFRicKernelMetadata(operates_on="domain")
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata._validate_cma_apply_kernel()
    assert ("A CMA kernel should only operate on a 'cell_column', but found "
            "'domain' in kernel metadata 'unset' for procedure 'unset'."
            in str(info.value))

    # Only field or CMA operator arguments.
    meta_args = [
        ColumnwiseOperatorArgMetadata("gh_real", "gh_read", "w3", "w1"),
        FieldArgMetadata("gh_real", "gh_read", "w1"),
        FieldArgMetadata("gh_real", "gh_write", "w3"),
        ScalarArgMetadata("gh_real", "gh_read")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata._validate_cma_apply_kernel()
    assert ("A CMA apply kernel should only contain field or CMA operator "
            "arguments, but found 'scalar' in kernel metadata 'unset' for "
            "procedure 'unset'." in str(info.value))

    # One CMA operator.
    meta_args = [
        ColumnwiseOperatorArgMetadata("gh_real", "gh_write", "w0", "w1"),
        ColumnwiseOperatorArgMetadata("gh_real", "gh_write", "w2", "w3")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata._validate_cma_apply_kernel()
    assert ("A CMA apply kernel should contain one CMA operator argument, "
            "however found 2 in kernel metadata 'unset' for procedure 'unset'."
            in str(info.value))

    # CMA operator read-only.
    meta_args = [
        ColumnwiseOperatorArgMetadata("gh_real", "gh_write", "w0", "w1")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata._validate_cma_apply_kernel()
    assert ("A CMA apply kernel should contain one CMA operator argument "
            "with read access, however the operator has access 'gh_write' in "
            "kernel metadata 'unset' for procedure 'unset'."
            in str(info.value))

    # Two field arguments (but none).
    meta_args = [
        ColumnwiseOperatorArgMetadata("gh_real", "gh_read", "w0", "w1")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata._validate_cma_apply_kernel()
    assert ("A CMA apply kernel should contain two field arguments, but none "
            "were found in kernel metadata 'unset' for procedure 'unset'."
            in str(info.value))

    # Two field arguments (but == 1).
    meta_args = [
        ColumnwiseOperatorArgMetadata("gh_real", "gh_read", "w0", "w1"),
        FieldArgMetadata("gh_real", "gh_read", "w0")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata._validate_cma_apply_kernel()
    assert ("A CMA apply kernel should contain two field arguments, but 1 "
            "was found in kernel metadata 'unset' for procedure 'unset'."
            in str(info.value))

    # Two field arguments (but > 2).
    meta_args = [
        ColumnwiseOperatorArgMetadata("gh_real", "gh_read", "w0", "w1"),
        FieldArgMetadata("gh_real", "gh_read", "w0"),
        FieldArgMetadata("gh_real", "gh_read", "w1"),
        FieldArgMetadata("gh_real", "gh_read", "w2")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata._validate_cma_apply_kernel()
    assert ("A CMA apply kernel should contain two field arguments, but 3 "
            "were found in kernel metadata 'unset' for procedure 'unset'."
            in str(info.value))

    # One field read and one field written.
    meta_args = [
        ColumnwiseOperatorArgMetadata("gh_real", "gh_read", "w0", "w1"),
        FieldArgMetadata("gh_real", "gh_read", "w2"),
        FieldArgMetadata("gh_real", "gh_read", "w3")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata._validate_cma_apply_kernel()
    assert ("A CMA apply kernel should contain two field arguments, one of "
            "which is read and the other written, but found 'gh_read' and "
            "'gh_read' in kernel metadata 'unset' for procedure 'unset'."
            in str(info.value))

    # Function space of written field matches CMA operator (read then write).
    meta_args = [
        ColumnwiseOperatorArgMetadata("gh_real", "gh_read", "w0", "w1"),
        FieldArgMetadata("gh_real", "gh_read", "w2"),
        FieldArgMetadata("gh_real", "gh_write", "w3")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata._validate_cma_apply_kernel()
    assert ("In a CMA apply kernel, the function space of the written field "
            "must match the function space of the CMA operator's 'to' "
            "function space, but found 'w3' and 'w0' respectively in kernel "
            "metadata 'unset' for procedure 'unset'." in str(info.value))

    # Function space of written field matches CMA operator (write then read).
    meta_args = [
        ColumnwiseOperatorArgMetadata("gh_real", "gh_read", "w0", "w1"),
        FieldArgMetadata("gh_real", "gh_write", "w3"),
        FieldArgMetadata("gh_real", "gh_read", "w2")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata._validate_cma_apply_kernel()
    assert ("In a CMA apply kernel, the function space of the written field "
            "must match the function space of the CMA operator's 'to' "
            "function space, but found 'w3' and 'w0' respectively in kernel "
            "metadata 'unset' for procedure 'unset'." in str(info.value))

    # Function space of read field matches CMA operator.
    meta_args = [
        ColumnwiseOperatorArgMetadata("gh_real", "gh_read", "w3", "w1"),
        FieldArgMetadata("gh_real", "gh_read", "w2"),
        FieldArgMetadata("gh_real", "gh_write", "w3")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata._validate_cma_apply_kernel()
    assert ("In a CMA apply kernel, the function space of the read field "
            "must match the function space of the CMA operator's 'from' "
            "function space, but found 'w2' and 'w1' respectively in kernel "
            "metadata 'unset' for procedure 'unset'." in str(info.value))

    # OK.
    meta_args = [
        ColumnwiseOperatorArgMetadata("gh_real", "gh_read", "w3", "w1"),
        FieldArgMetadata("gh_real", "gh_read", "w1"),
        FieldArgMetadata("gh_real", "gh_write", "w3")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    lfric_kernel_metadata._validate_cma_apply_kernel()


def test_validate_cma_matrix_kernel():
    '''Test that the _validate_cma_matrix_matrix_kernel method behaves as
    expected.

    '''
    # Check validate_generic_cma_kernel is called.
    lfric_kernel_metadata = LFRicKernelMetadata(operates_on="domain")
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata._validate_cma_matrix_matrix_kernel()
    assert ("A CMA kernel should only operate on a 'cell_column', but found "
            "'domain' in kernel metadata 'unset' for procedure 'unset'."
            in str(info.value))

    # CMA operators or scalars.
    meta_args = [
        ColumnwiseOperatorArgMetadata("gh_real", "gh_write", "w3", "w2"),
        FieldArgMetadata("gh_real", "gh_write", "w3")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata._validate_cma_matrix_matrix_kernel()
    assert ("A CMA matrix-matrix kernel must only contain CMA operators or "
            "scalars, but found 'field' in kernel metadata 'unset' for "
            "procedure 'unset'." in str(info.value))

    # One CMA argument written to.
    meta_args = [
        ColumnwiseOperatorArgMetadata("gh_real", "gh_read", "w3", "w2")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata._validate_cma_matrix_matrix_kernel()
    assert ("A CMA matrix-matrix kernel must write to exactly one CMA "
            "operator argument, but found 0 writers in kernel metadata "
            "'unset' for procedure 'unset'." in str(info.value))

    # Other arguments must also be read only but remaining args will
    # be scalars which must already be read only so no more checks
    # required. However, in case things change in the future, let's
    # check that a scalar must be read only.
    with pytest.raises(ValueError) as info:
        ScalarArgMetadata("gh_real", "gh_write")
    assert ("The 'access descriptor' metadata should be a recognised value "
            "(one of ['gh_read', 'gh_sum']) but found 'gh_write'."
            in str(info.value))

    # OK.
    meta_args = [
        ColumnwiseOperatorArgMetadata("gh_real", "gh_write", "w3", "w2"),
        ColumnwiseOperatorArgMetadata("gh_real", "gh_read", "w3", "w2"),
        ScalarArgMetadata("gh_real", "gh_read")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    lfric_kernel_metadata._validate_cma_matrix_matrix_kernel()


def test_validate_intergrid_kernel():
    '''Test that the _validate_intergrid_kernel method behaves as
    expected.

    '''
    # Check validate_generic_kernel is called.
    lfric_kernel_metadata = LFRicKernelMetadata(operates_on="cell_column")
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata._validate_intergrid_kernel()
    assert ("Kernel metadata with 'operates_on != domain' must have at least "
            "one meta_args argument that is a field, field vector, intergrid "
            "field, intergrid vector field, LMA operator or CMA operator (in "
            "order to determine the appropriate iteration space), however "
            "this metadata has none in kernel metadata 'unset' for procedure "
            "'unset'." in str(info.value))

    # operates_on == cell_column.
    meta_args = [
        InterGridArgMetadata("gh_real", "gh_read", "w0", "gh_coarse")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="domain", meta_args=meta_args)
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata._validate_intergrid_kernel()
    assert ("An intergrid kernel should only operate on a 'cell_column', but "
            "found 'domain' in kernel metadata 'unset' for procedure 'unset'."
            in str(info.value))

    # All args are inter-grid args.
    meta_args = [
        InterGridArgMetadata("gh_real", "gh_read", "w0", "gh_coarse"),
        ScalarArgMetadata("gh_real", "gh_read")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata._validate_intergrid_kernel()
    assert ("An intergrid kernel should only have intergrid arguments, but "
            "found 'scalar' in kernel metadata 'unset' for procedure 'unset'."
            in str(info.value))

    # At least one arg on coarse mesh.
    meta_args = [
        InterGridArgMetadata("gh_real", "gh_read", "w0", "gh_fine")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata._validate_intergrid_kernel()
    assert ("An intergrid kernel should have at least one intergrid argument "
            "on the coarse mesh, but none were found in kernel metadata "
            "'unset' for procedure 'unset'." in str(info.value))

    # All args on coarse mesh are on the same function space.
    meta_args = [
        InterGridArgMetadata("gh_real", "gh_read", "w0", "gh_coarse"),
        InterGridArgMetadata("gh_real", "gh_read", "w1", "gh_coarse")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata._validate_intergrid_kernel()
    assert ("An intergrid kernel should have all of its arguments, that are "
            "on the coarse mesh, on the same function space. However, 'w1' "
            "and 'w0' differ in kernel metadata 'unset' for procedure 'unset'."
            in str(info.value))

    # At least one arg on fine mesh.
    meta_args = [
        InterGridArgMetadata("gh_real", "gh_read", "w0", "gh_coarse"),
        InterGridArgMetadata("gh_real", "gh_read", "w0", "gh_coarse")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata._validate_intergrid_kernel()
    assert ("An intergrid kernel should have at least one intergrid argument "
            "on the fine mesh, but none were found in kernel metadata 'unset' "
            "for procedure 'unset'." in str(info.value))

    # All args on fine mesh are on the same function space.
    meta_args = [
        InterGridArgMetadata("gh_real", "gh_read", "w0", "gh_coarse"),
        InterGridArgMetadata("gh_real", "gh_read", "w0", "gh_coarse"),
        InterGridArgMetadata("gh_real", "gh_read", "w1", "gh_fine"),
        InterGridArgMetadata("gh_real", "gh_read", "w2", "gh_fine")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata._validate_intergrid_kernel()
    assert ("An intergrid kernel should have all of its arguments, that are "
            "on the fine mesh, on the same function space. However, 'w2' and "
            "'w1' differ in kernel metadata 'unset' for procedure 'unset'."
            in str(info.value))

    # Fine mesh and coarse mesh function spaces should differ.
    meta_args = [
        InterGridArgMetadata("gh_real", "gh_read", "w0", "gh_coarse"),
        InterGridArgMetadata("gh_real", "gh_read", "w0", "gh_coarse"),
        InterGridArgMetadata("gh_real", "gh_read", "w0", "gh_fine"),
        InterGridArgMetadata("gh_real", "gh_read", "w0", "gh_fine")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    with pytest.raises(ParseError) as info:
        lfric_kernel_metadata._validate_intergrid_kernel()
    assert ("An intergrid kernel should have different function spaces for "
            "the arguments on the coarse mesh and the arguments on the fine "
            "mesh. However, both are on 'w0' in kernel metadata 'unset' for "
            "procedure 'unset'." in str(info.value))

    # OK.
    meta_args = [
        InterGridArgMetadata("gh_real", "gh_read", "w0", "gh_coarse"),
        InterGridArgMetadata("gh_real", "gh_read", "w0", "gh_coarse"),
        InterGridArgMetadata("gh_real", "gh_read", "w1", "gh_fine"),
        InterGridArgMetadata("gh_real", "gh_read", "w1", "gh_fine")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    lfric_kernel_metadata._validate_intergrid_kernel()


# TODO issue #1953. The metadata below is invalid. Only one of gh_shape
# or gh_evaluator_targets should exist, but this is not yet checked.
METADATA = (
    "type, extends(kernel_type) :: testkern_type\n"
    "   type(arg_type), dimension(7) :: meta_args =                       &\n"
    "        (/ arg_type(gh_scalar,   gh_real, gh_read),                  &\n"
    "           arg_type(gh_field,    gh_real, gh_inc,  w1),              &\n"
    "           arg_type(gh_field*3,  gh_real, gh_read, w2),              &\n"
    "           arg_type(gh_field, gh_real, gh_read, w2, "
    "mesh_arg=gh_coarse),                                                 &\n"
    "           arg_type(gh_field*3, gh_real, gh_read, w2, "
    "mesh_arg=gh_fine),                                                   &\n"
    "           arg_type(gh_operator, gh_real, gh_read, w2, w3),          &\n"
    "           arg_type(gh_columnwise_operator, gh_real, gh_read, w3, "
    "w0)                                                                  &\n"
    "         /)\n"
    "   type(func_type), dimension(2) :: meta_funcs =                     &\n"
    "        (/ func_type(w1, gh_basis),                                  &\n"
    "           func_type(w2, gh_basis, gh_diff_basis)                    &\n"
    "        /)\n"
    "   type(reference_element_data_type), dimension(2) ::                &\n"
    "     meta_reference_element =                                        &\n"
    "        (/ reference_element_data_type(normals_to_horizontal_faces), &\n"
    "           reference_element_data_type(normals_to_vertical_faces)    &\n"
    "        /)\n"
    "   type(mesh_data_type) :: meta_mesh(1) =                            &\n"
    "        (/ mesh_data_type(adjacent_face) /)\n"
    "   integer :: gh_shape = gh_quadrature_XYoZ\n"
    "   integer :: gh_evaluator_targets(2) = (/ w0, w3 /)\n"
    "   integer :: operates_on = cell_column\n"
    " contains\n"
    "   procedure, nopass :: code => testkern_code\n"
    "end type testkern_type\n")

PROGRAM = (
    f"module dummy\n"
    f"{METADATA}"
    f"contains\n"
    f"  subroutine kern()\n"
    f"  end subroutine kern\n"
    f"end module dummy\n")


def test_create_from_psyir(fortran_reader):
    '''Test that an instance of the LFRicKernelMetadata class can be
    created from PSyIR.

    '''
    kernel_psyir = fortran_reader.psyir_from_source(PROGRAM)
    symbol = kernel_psyir.children[0].symbol_table.lookup("testkern_type")
    metadata = LFRicKernelMetadata.create_from_psyir(symbol)

    assert metadata.operates_on == "cell_column"
    assert metadata.shapes == ["gh_quadrature_xyoz"]
    assert metadata.evaluator_targets == ["w0", "w3"]

    assert isinstance(metadata.meta_args, list)
    assert len(metadata.meta_args) == 7
    assert isinstance(metadata.meta_args[0], ScalarArgMetadata)
    assert isinstance(metadata.meta_args[1], FieldArgMetadata)
    assert isinstance(metadata.meta_args[2], FieldVectorArgMetadata)
    assert isinstance(metadata.meta_args[3], InterGridArgMetadata)
    assert isinstance(metadata.meta_args[4], InterGridVectorArgMetadata)
    assert isinstance(metadata.meta_args[5], OperatorArgMetadata)
    assert isinstance(metadata.meta_args[6], ColumnwiseOperatorArgMetadata)

    assert isinstance(metadata.meta_funcs, list)
    assert isinstance(metadata.meta_funcs[0], MetaFuncsArgMetadata)
    assert isinstance(metadata.meta_funcs[1], MetaFuncsArgMetadata)

    assert isinstance(metadata.meta_ref_element, list)
    assert len(metadata.meta_ref_element) == 2
    assert isinstance(metadata.meta_ref_element[0], MetaRefElementArgMetadata)
    assert isinstance(metadata.meta_ref_element[1], MetaRefElementArgMetadata)

    assert isinstance(metadata.meta_mesh, list)
    assert len(metadata.meta_mesh) == 1
    assert isinstance(metadata.meta_mesh[0], MetaMeshArgMetadata)

    assert metadata.procedure_name == "testkern_code"
    assert metadata.name == "testkern_type"


def test_create_from_psyir_error():
    '''Test that create_from_psyir raises the expected exceptions.'''

    with pytest.raises(TypeError) as info:
        _ = LFRicKernelMetadata.create_from_psyir(None)
    assert ("Expected a DataTypeSymbol but found a NoneType."
            in str(info.value))

    with pytest.raises(InternalError) as info:
        _ = LFRicKernelMetadata.create_from_psyir(
            DataTypeSymbol("x", REAL_TYPE))
    assert ("Expected kernel metadata to be stored in the PSyIR as an "
            "UnsupportedFortranType, but found ScalarType." in str(info.value))


@pytest.mark.parametrize("procedure_format", ["", "code =>"])
def test_create_from_fparser2(procedure_format):
    '''Test that an instance of the LFRicKernelMetadata class can be
    created from Fortran encoded as an fparser2 tree. Test with all
    optional metadata. Test using standard and alternative (no 'code
    =>') format for procedure metadata.

    '''
    fortran_metadata = METADATA.replace(procedure_format, "")
    fparser2_tree = LFRicKernelMetadata.create_fparser2(
        fortran_metadata, Fortran2003.Derived_Type_Def)
    metadata = LFRicKernelMetadata.create_from_fparser2(fparser2_tree)
    assert isinstance(metadata, LFRicKernelMetadata)
    assert metadata.operates_on == "cell_column"
    assert metadata.shapes == ["gh_quadrature_xyoz"]
    assert metadata.evaluator_targets == ["w0", "w3"]

    assert isinstance(metadata.meta_args, list)
    assert len(metadata.meta_args) == 7
    assert isinstance(metadata.meta_args[0], ScalarArgMetadata)
    assert isinstance(metadata.meta_args[1], FieldArgMetadata)
    assert isinstance(metadata.meta_args[2], FieldVectorArgMetadata)
    assert isinstance(metadata.meta_args[3], InterGridArgMetadata)
    assert isinstance(metadata.meta_args[4], InterGridVectorArgMetadata)
    assert isinstance(metadata.meta_args[5], OperatorArgMetadata)
    assert isinstance(metadata.meta_args[6], ColumnwiseOperatorArgMetadata)
    assert isinstance(metadata.meta_funcs, list)
    assert isinstance(metadata.meta_funcs[0], MetaFuncsArgMetadata)
    assert isinstance(metadata.meta_funcs[1], MetaFuncsArgMetadata)

    assert isinstance(metadata.meta_ref_element, list)
    assert len(metadata.meta_ref_element) == 2
    assert isinstance(metadata.meta_ref_element[0], MetaRefElementArgMetadata)
    assert isinstance(metadata.meta_ref_element[1], MetaRefElementArgMetadata)

    assert isinstance(metadata.meta_mesh, list)
    assert len(metadata.meta_mesh) == 1
    assert isinstance(metadata.meta_mesh[0], MetaMeshArgMetadata)

    assert metadata.procedure_name == "testkern_code"
    assert metadata.name == "testkern_type"


def test_create_from_fparser2_no_optional():
    '''Test that an instance of the LFRicKernelMetadata class can be
    created from Fortran encoded as an fparser2 tree. Test with no
    optional metadata.

    '''
    metadata = (
        "type, extends(kernel_type) :: testkern_type\n"
        "   type(arg_type), dimension(1) :: meta_args =       &\n"
        "        (/ arg_type(gh_scalar,   gh_real, gh_read) /)\n"
        "   integer :: operates_on = cell_column\n"
        " contains\n"
        "   procedure, nopass :: code => testkern_code\n"
        "end type testkern_type\n")

    fparser2_tree = LFRicKernelMetadata.create_fparser2(
        metadata, Fortran2003.Derived_Type_Def)
    metadata = LFRicKernelMetadata.create_from_fparser2(fparser2_tree)
    assert metadata.shapes is None
    assert metadata.evaluator_targets is None
    assert metadata.meta_funcs is None


def test_create_from_fparser2_error():
    '''Test that the expected exceptions are raised when invalid input is
    provided to the create_from_fparser2 method.

    '''
    # from check_fparser2 method
    with pytest.raises(TypeError) as info:
        _ = LFRicKernelMetadata.create_from_fparser2("hello")
    assert ("Expected kernel metadata to be encoded as an fparser2 "
            "Derived_Type_Def object but found type 'str' with value "
            "'hello'." in str(info.value))

    # invalid metadata declaration
    metadata = (
        "type, extends(kernel_type) :: testkern_type\n"
        "   integer :: invalid_var = invalid_value\n"
        "end type testkern_type\n")
    fparser2_tree = LFRicKernelMetadata.create_fparser2(
        metadata, Fortran2003.Derived_Type_Def)
    with pytest.raises(ParseError) as info:
        _ = LFRicKernelMetadata.create_from_fparser2(fparser2_tree)
    assert ("Found unexpected metadata declaration 'INTEGER :: invalid_var = "
            "invalid_value' in 'TYPE, EXTENDS(kernel_type) :: testkern_type\n"
            "  INTEGER :: invalid_var = invalid_value\nEND TYPE "
            "testkern_type'." in str(info.value))

    # no 'extends(kernel_type)'
    fparser2_tree = LFRicKernelMetadata.create_fparser2(METADATA.replace(
        ", extends(kernel_type)", ""), Fortran2003.Derived_Type_Def)
    with pytest.raises(ParseError) as info:
        _ = LFRicKernelMetadata.create_from_fparser2(fparser2_tree)
    assert ("The metadata type declaration should extend kernel_type, but "
            "found 'TYPE :: testkern_type' in TYPE :: "
            "testkern_type\n  TYPE(arg_type), DIMENSION(7)" in str(info.value))

    # metadata type extends incorrect type
    fparser2_tree = LFRicKernelMetadata.create_fparser2(METADATA.replace(
        "kernel_type", "invalid_type"), Fortran2003.Derived_Type_Def)
    with pytest.raises(ParseError) as info:
        _ = LFRicKernelMetadata.create_from_fparser2(fparser2_tree)
    assert ("The metadata type declaration should extend kernel_type, but "
            "found 'TYPE, EXTENDS(invalid_type) :: testkern_type' in TYPE, "
            "EXTENDS(invalid_type) :: testkern_type\n  TYPE(arg_type), "
            "DIMENSION(7)" in str(info.value))


def test_lower_to_psyir():
    '''Test that the metadata can be lowered to an UnsupportedFortranType
    symbol.

    '''
    metadata = LFRicKernelMetadata.create_from_fortran_string(METADATA)
    symbol = metadata.lower_to_psyir()
    assert isinstance(symbol, DataTypeSymbol)
    assert symbol.name == metadata.name
    assert isinstance(symbol.datatype, UnsupportedFortranType)
    assert symbol.datatype.declaration == metadata.fortran_string()


def test_get_procedure_name_error(fortran_reader):
    '''Test that all the exceptions are raised as expected in the
    _get_procedure_name method.

    '''
    kernel_psyir = fortran_reader.psyir_from_source(PROGRAM.replace(
        "procedure, nopass :: code => testkern_code", ""))
    datatype = kernel_psyir.children[0].symbol_table.lookup(
        "testkern_type").datatype
    metadata = LFRicKernelMetadata()
    reader = FortranStringReader(datatype.declaration)
    spec_part = Fortran2003.Derived_Type_Def(reader)
    with pytest.raises(ParseError) as info:
        metadata._get_procedure_name(spec_part)
    assert "Expecting a type-bound procedure, but found" in str(info.value)

    kernel_psyir = fortran_reader.psyir_from_source(PROGRAM)
    datatype = kernel_psyir.children[0].symbol_table.lookup(
        "testkern_type").datatype
    metadata = LFRicKernelMetadata()
    reader = FortranStringReader(datatype.declaration)
    spec_part = Fortran2003.Derived_Type_Def(reader)
    binding = spec_part.children[2]
    binding.children[1] = binding.children[0]
    with pytest.raises(ParseError) as info:
        metadata._get_procedure_name(spec_part)
    assert ("Expecting a specific binding for the type-bound procedure, "
            "but found" in str(info.value))

    kernel_psyir = fortran_reader.psyir_from_source(PROGRAM.replace(
        "code", "hode"))
    datatype = kernel_psyir.children[0].symbol_table.lookup(
        "testkern_type").datatype
    metadata = LFRicKernelMetadata()
    reader = FortranStringReader(datatype.declaration)
    spec_part = Fortran2003.Derived_Type_Def(reader)
    with pytest.raises(ParseError) as info:
        metadata._get_procedure_name(spec_part)
    assert ("Expecting the type-bound procedure binding-name to be 'code' "
            "if there is a procedure name, but found 'hode'"
            in str(info.value))


def test_get_procedure_name(fortran_reader):
    '''Test utility function that takes metadata in an fparser2 tree and
    returns the procedure metadata name, or None is there is no
    procedure name.

    '''
    kernel_psyir = fortran_reader.psyir_from_source(PROGRAM)
    datatype = kernel_psyir.children[0].symbol_table.lookup(
        "testkern_type").datatype
    metadata = LFRicKernelMetadata()
    reader = FortranStringReader(datatype.declaration)
    spec_part = Fortran2003.Derived_Type_Def(reader)
    assert metadata._get_procedure_name(spec_part) == \
        "testkern_code"

    # No procedure name
    del spec_part.children[2]
    result = metadata._get_procedure_name(spec_part)
    assert result is None


def test_fortran_string():
    '''Test that the metadata can be written out as a fortran string and
    that it raises the expected exception if there is an error.

    '''
    instance = LFRicKernelMetadata()
    with pytest.raises(ValueError) as info:
        instance.fortran_string()
    assert ("Values for operates_on, meta_args and name must "
            "be provided before calling the fortran_string method, but found "
            "'None', 'None' and 'None' respectively."
            in str(info.value))

    metadata = LFRicKernelMetadata.create_from_fortran_string(METADATA)
    result = metadata.fortran_string()
    expected = (
        "TYPE, PUBLIC, EXTENDS(kernel_type) :: testkern_type\n"
        "  type(ARG_TYPE) :: META_ARGS(7) = (/ &\n"
        "    arg_type(gh_scalar, gh_real, gh_read), &\n"
        "    arg_type(gh_field, gh_real, gh_inc, w1), &\n"
        "    arg_type(gh_field*3, gh_real, gh_read, w2), &\n"
        "    arg_type(gh_field, gh_real, gh_read, w2, mesh_arg=gh_coarse), &\n"
        "    arg_type(gh_field*3, gh_real, gh_read, w2, mesh_arg=gh_fine), &\n"
        "    arg_type(gh_operator, gh_real, gh_read, w2, w3), &\n"
        "    arg_type(gh_columnwise_operator, gh_real, gh_read, w3, w0)/)\n"
        "  type(FUNC_TYPE) :: META_FUNCS(2) = (/ &\n"
        "    func_type(w1, gh_basis), &\n"
        "    func_type(w2, gh_basis, gh_diff_basis)/)\n"
        "  type(REFERENCE_ELEMENT_DATA_TYPE) :: "
        "META_REFERENCE_ELEMENT(2) = (/ &\n"
        "    reference_element_data_type(normals_to_horizontal_faces), &\n"
        "    reference_element_data_type(normals_to_vertical_faces)/)\n"
        "  type(MESH_DATA_TYPE) :: META_MESH(1) = (/ &\n"
        "    mesh_data_type(adjacent_face)/)\n"
        "  INTEGER :: GH_SHAPE = gh_quadrature_xyoz\n"
        "  INTEGER :: GH_EVALUATOR_TARGETS(2) = (/w0, w3/)\n"
        "  INTEGER :: OPERATES_ON = cell_column\n"
        "  CONTAINS\n"
        "    PROCEDURE, NOPASS :: testkern_code\n"
        "END TYPE testkern_type\n")
    assert result == expected


def test_fortran_string_no_procedure():
    '''Test that the metadata can be written out as a fortran string when
    there is no procedure name supplied.

    '''
    metadata = LFRicKernelMetadata.create_from_fortran_string(
        METADATA.replace(
            " contains\n   procedure, nopass :: code => testkern_code\n", ""))
    result = metadata.fortran_string()
    expected = (
        "TYPE, PUBLIC, EXTENDS(kernel_type) :: testkern_type\n"
        "  type(ARG_TYPE) :: META_ARGS(7) = (/ &\n"
        "    arg_type(gh_scalar, gh_real, gh_read), &\n"
        "    arg_type(gh_field, gh_real, gh_inc, w1), &\n"
        "    arg_type(gh_field*3, gh_real, gh_read, w2), &\n"
        "    arg_type(gh_field, gh_real, gh_read, w2, mesh_arg=gh_coarse), &\n"
        "    arg_type(gh_field*3, gh_real, gh_read, w2, mesh_arg=gh_fine), &\n"
        "    arg_type(gh_operator, gh_real, gh_read, w2, w3), &\n"
        "    arg_type(gh_columnwise_operator, gh_real, gh_read, w3, w0)/)\n"
        "  type(FUNC_TYPE) :: META_FUNCS(2) = (/ &\n"
        "    func_type(w1, gh_basis), &\n"
        "    func_type(w2, gh_basis, gh_diff_basis)/)\n"
        "  type(REFERENCE_ELEMENT_DATA_TYPE) :: "
        "META_REFERENCE_ELEMENT(2) = (/ &\n"
        "    reference_element_data_type(normals_to_horizontal_faces), &\n"
        "    reference_element_data_type(normals_to_vertical_faces)/)\n"
        "  type(MESH_DATA_TYPE) :: META_MESH(1) = (/ &\n"
        "    mesh_data_type(adjacent_face)/)\n"
        "  INTEGER :: GH_SHAPE = gh_quadrature_xyoz\n"
        "  INTEGER :: GH_EVALUATOR_TARGETS(2) = (/w0, w3/)\n"
        "  INTEGER :: OPERATES_ON = cell_column\n"
        "END TYPE testkern_type\n")
    assert result == expected


def test_getter_kernel_type():
    '''Test that the LFRicKernelMetadata kernel_type getter works as
    expected.

    '''
    meta_args = [FieldArgMetadata("gh_real", "gh_read", "w3")]
    lfric_kernel_metadata = LFRicKernelMetadata(
        operates_on="domain", meta_args=meta_args)
    assert lfric_kernel_metadata.kernel_type == "domain"


def test_setter_getter_operates_on():
    '''Test that the LFRicKernelMetadata operates_on setters and getters
    work as expected.

    '''
    metadata = LFRicKernelMetadata()
    assert metadata.operates_on is None
    with pytest.raises(ValueError) as info:
        metadata.operates_on = "invalid"
    assert ("The 'OPERATES_ON' metadata should be a recognised value "
            "(one of ['cell_column', 'domain', 'dof']) but found "
            "'invalid'." in str(info.value))
    metadata.operates_on = "DOMAIN"
    assert metadata.operates_on == "domain"


def test_setter_getter_shapes():
    '''Test that the LFRicKernelMetadata shapes setters and getters work
    as expected.

    '''
    metadata = LFRicKernelMetadata()
    assert metadata.shapes is None
    with pytest.raises(TypeError) as info:
        metadata.shapes = "invalid"
    assert ("ShapesMetadata values should be provided as a list but found "
            "'str'." in str(info.value))
    shapes = ["gh_quadrature_face", "gh_evaluator"]
    metadata.shapes = shapes
    assert metadata.shapes == shapes


def test_setter_getter_evaluator_targets():
    '''Test that the LFRicKernelMetadata evaluator_targets setters and
    getters work as expected.

    '''
    metadata = LFRicKernelMetadata()
    assert metadata.evaluator_targets is None
    with pytest.raises(TypeError) as info:
        metadata.evaluator_targets = "invalid"
    assert ("EvaluatorTargetsMetadata values should be provided as a list but "
            "found 'str'." in str(info.value))
    evaluator_targets = ["w0", "w1", "w2", "w3"]
    metadata.evaluator_targets = evaluator_targets
    assert metadata.evaluator_targets == evaluator_targets


def test_setter_getter_meta_args():
    '''Test that the LFRicKernelMetadata meta_args setters and getters
    work as expected.

    '''
    metadata = LFRicKernelMetadata()
    assert metadata.meta_args is None
    with pytest.raises(TypeError) as info:
        metadata.meta_args = "error"
    assert ("MetaArgsMetadata values should be provided as a list but found "
            "'str'." in str(info.value))
    with pytest.raises(TypeError) as info:
        metadata.meta_args = []
    assert ("The MetaArgsMetadata list should contain at least one entry, "
            "but it is empty." in str(info.value))
    with pytest.raises(TypeError) as info:
        metadata.meta_args = ["error"]
    assert ("The MetaArgsMetadata list should be a list containing objects of "
            "type CommonMetaArgMetadata but found 'error', which is of type "
            "'str'." in str(info.value))

    scalar_arg = ScalarArgMetadata("GH_REAL", "GH_READ")
    meta_args = [scalar_arg]
    metadata.meta_args = meta_args
    assert metadata.meta_args == meta_args


def test_setter_getter_meta_funcs():
    '''Test that the LFRicKernelMetadata meta_funcs setters and
    getters work as expected.

    '''
    metadata = LFRicKernelMetadata()
    assert metadata.meta_funcs is None
    with pytest.raises(TypeError) as info:
        metadata.meta_funcs = "invalid"
    assert ("MetaFuncsMetadata values should be provided as a list but "
            "found 'str'." in str(info.value))
    meta_funcs_arg = MetaFuncsArgMetadata("w0", basis_function=True)
    meta_funcs = [meta_funcs_arg]
    metadata.meta_funcs = meta_funcs
    assert metadata.meta_funcs == meta_funcs


def test_setter_getter_meta_ref_element():
    '''Test that the LFRicKernelMetadata meta_ref_element setters and
    getters work as expected.

    '''
    metadata = LFRicKernelMetadata()
    assert metadata.meta_ref_element is None
    with pytest.raises(TypeError) as info:
        metadata.meta_ref_element = "invalid"
    assert ("MetaRefElementMetadata values should be provided as a list but "
            "found 'str'." in str(info.value))
    meta_ref_element_arg = MetaRefElementArgMetadata("normals_to_faces")
    meta_ref_element = [meta_ref_element_arg]
    metadata.meta_ref_element = meta_ref_element
    assert metadata.meta_ref_element == meta_ref_element


def test_setter_getter_meta_mesh():
    '''Test that the LFRicKernelMetadata meta_mesh setters and
    getters work as expected.

    '''
    metadata = LFRicKernelMetadata()
    assert metadata.meta_mesh is None
    with pytest.raises(TypeError) as info:
        metadata.meta_mesh = "invalid"
    assert ("MetaMeshMetadata values should be provided as a list but "
            "found 'str'." in str(info.value))
    meta_mesh_arg = MetaMeshArgMetadata("adjacent_face")
    meta_mesh = [meta_mesh_arg]
    metadata.meta_mesh = meta_mesh
    assert metadata.meta_mesh == meta_mesh


def test_setter_getter_procedure_name():
    '''Test that the LFRicKernelMetadata procedure_name setters and
    getters work as expected.

    '''
    metadata = LFRicKernelMetadata()
    assert metadata.procedure_name is None
    with pytest.raises(ValueError) as info:
        metadata.procedure_name = "1_invalid"
    assert ("Expected procedure_name to be a valid Fortran name but found "
            "'1_invalid'." in str(info.value))
    metadata.procedure_name = "KERN_CODE"
    assert metadata.procedure_name == "KERN_CODE"
    metadata.procedure_name = None
    assert metadata.procedure_name is None


def test_setter_getter_name():
    '''Test that the LFRicKernelMetadata name setters and getters work as
    expected.

    '''
    metadata = LFRicKernelMetadata()
    assert metadata.name is None
    with pytest.raises(ValueError) as info:
        metadata.name = "1_invalid"
    assert "Invalid Fortran name '1_invalid' found." in str(info.value)
    metadata.name = "kern_type"
    assert metadata.name == "kern_type"


def test_meta_args_get():
    '''Test that the meta_args_get method behaves as expected.'''
    # Empty meta_args list.
    metadata = LFRicKernelMetadata()
    assert metadata.meta_args_get([]) == []

    # types is not a list or CommonMetaArgMetadata subclass.
    with pytest.raises(TypeError) as info:
        metadata.meta_args_get(None)
    assert ("Expected a subclass of CommonMetaArgMetadata or a list for the "
            "'types' argument, but found 'NoneType'." in str(info.value))

    # types has invalid values in its list.
    with pytest.raises(TypeError) as info:
        metadata.meta_args_get([None])
    assert ("Expected list entries in the 'types' argument to be subclasses "
            "of CommonMetaArgMetadata, but found 'NoneType'."
            in str(info.value))

    meta_args = [
        ScalarArgMetadata("gh_real", "gh_read"),
        FieldArgMetadata("gh_real", "gh_read", "w0"),
        OperatorArgMetadata("gh_real", "gh_read", "w0", "w1")]
    metadata = LFRicKernelMetadata(meta_args=meta_args)

    # OK (types single value)
    result = metadata.meta_args_get(ScalarArgMetadata)
    assert len(result) == 1
    assert result[0] is metadata.meta_args[0]

    # OK (types list)
    result = metadata.meta_args_get([FieldArgMetadata, OperatorArgMetadata])
    assert len(result) == 2
    assert result[0] is metadata.meta_args[1]
    assert result[1] is metadata.meta_args[2]

    # OK (empty list matches nothing)
    result = metadata.meta_args_get([])
    assert result == []


# pylint: disable=unidiomatic-typecheck
def test_field_utility():
    '''Test the LFRicKernelMetadata class field_meta_args_on_fs utility
    method.

    '''
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_INC", "W0"),
        FieldVectorArgMetadata("GH_REAL", "GH_READ", "W0", "3")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    metadata.validate()
    result = metadata.field_meta_args_on_fs(FieldArgMetadata, "w0")
    assert len(result) == 1
    assert type(result[0]) is FieldArgMetadata
    result = metadata.field_meta_args_on_fs(FieldArgMetadata, "w1")
    assert len(result) == 0
    result = metadata.field_meta_args_on_fs(
        [FieldArgMetadata, FieldVectorArgMetadata], "w0")
    assert len(result) == 2
    assert type(result[0]) is FieldArgMetadata
    assert type(result[1]) is FieldVectorArgMetadata


def test_operator_utility():
    '''Test the LFRicKernelMetadata class operator_meta_args_on_fs utility
    method.

    '''
    meta_args = [
        ColumnwiseOperatorArgMetadata("GH_REAL", "GH_READWRITE", "W0", "W1"),
        OperatorArgMetadata("GH_REAL", "GH_READ", "W0", "W1")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    metadata.validate()
    result = metadata.operator_meta_args_on_fs(
        ColumnwiseOperatorArgMetadata, "w0")
    assert len(result) == 1
    assert type(result[0]) is ColumnwiseOperatorArgMetadata
    result = metadata.operator_meta_args_on_fs(OperatorArgMetadata, "w1")
    assert len(result) == 1
    assert type(result[0]) is OperatorArgMetadata
    result = metadata.operator_meta_args_on_fs(FieldArgMetadata, "w2")
    assert len(result) == 0
    result = metadata.operator_meta_args_on_fs(
        [OperatorArgMetadata, ColumnwiseOperatorArgMetadata], "w0")
    assert len(result) == 2
    assert type(result[0]) is ColumnwiseOperatorArgMetadata
    assert type(result[1]) is OperatorArgMetadata
