"""Tests for LFRic kernel metadata implementation."""

from dataclasses import FrozenInstanceError

import pytest

from psyclone.configuration import Config
from psyclone.domain.lfric import LFRicConstants
from psyclone.domain.lfric.lfric_kern import LFRicKern
from psyclone.domain.lfric.kernel import (
    ColumnwiseOperatorArgMetadata, FieldArgMetadata,
    FieldVectorArgMetadata, InterGridArgMetadata,
    InterGridVectorArgMetadata, LFRicKernelMetadata,
    LFRicKernelContainer,
    MetaFuncsArgMetadata, MetaMeshArgMetadata,
    MetaRefElementArgMetadata, OperatorArgMetadata, ScalarArgMetadata,
    ScalarArrayArgMetadata)
from psyclone.domain.lfric.kernel import metadata as metadata_mod
from psyclone.domain.common.kernel import Kernel
from psyclone.domain.lfric.transformations import RaisePSyIR2LFRicKernTrans
from psyclone.errors import InternalError
from psyclone.parse.utils import ParseError
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import Container
from psyclone.psyir.symbols import (
    DataTypeSymbol, ScalarType, StructureType, SymbolTable)


def _expression(source):
    """Create a PSyIR expression for node-level metadata tests."""
    return FortranReader().psyir_from_expression(source)


def _kernel_metadata(arguments, **kwargs):
    """Create language-level metadata with useful defaults."""
    values = {"operates_on": "cell_column", "meta_args": arguments,
              "procedure_name": "test_code", "name": "test_type"}
    values.update(kwargs)
    return LFRicKernelMetadata(**values)


def test_parse_metadata():
    """Test extraction of representative LFRic metadata from PSyIR."""
    Config.get().api = "lfric"
    mdata_code = '''
module testkern_field_mod
  type, extends(kernel_type) :: testkern_field_type
     type(arg_type), dimension(8) :: meta_args =                  &
          (/ arg_type(gh_scalar, gh_real,    gh_read),            &
             arg_type(gh_field,  gh_real,    gh_readinc, w0),     &
             arg_type(gh_field,  gh_real,    gh_inc,     w1),     &
             arg_type(gh_field*3,gh_integer, gh_read,    w2),     &
             arg_type(gh_field,  gh_integer, gh_write,   wtheta), &
             arg_type(gh_field,  gh_integer, gh_read,    w3),     &
             arg_type(gh_scalar, gh_integer, gh_read),            &
             arg_type(gh_scalar, gh_logical, gh_read)             &
           /)
     type(func_type), dimension(2) :: meta_funcs =  &
          (/ func_type(w1, gh_basis),               &
             func_type(w3, gh_basis, gh_diff_basis) &
           /)
     integer :: operates_on = cell_column
     integer :: gh_shape = gh_quadrature_XYoZ
   contains
     procedure, nopass :: code => testkern_field_code
  end type testkern_field_type
contains
  subroutine testkern_field_code()
  end subroutine testkern_field_code
end module testkern_field_mod
'''
    kernel_metadata = FortranReader().psyir_from_source(mdata_code)
    ktype = Kernel.create_from_psyir(
        LFRicKernelMetadata,
        kernel_metadata, name="testkern_field_type").metadata

    assert isinstance(ktype, LFRicKernelMetadata)
    assert ktype.name == "testkern_field_type"
    assert ktype.iterates_over == "cell_column"
    assert ktype.algorithm_arg_count == 8
    assert ktype.eval_shapes == ("gh_quadrature_xyoz",)

    kernel = LFRicKern()
    kernel.load_meta(ktype)
    assert kernel.name == "testkern_field_code"
    assert kernel.iterates_over == "cell_column"

    # Metadata lookup remains case-insensitive.
    assert Kernel.create_from_psyir(
        LFRicKernelMetadata,
        kernel_metadata, name="TeStKeRn_FiElD_TyPe").metadata == ktype


def test_lfric_kernel_container(fortran_reader):
    """Preserve construction and lowering of the specialised LFRic kernel
    container.
    """
    metadata = _kernel_metadata(
        (FieldArgMetadata("gh_real", "gh_write", "w0"),))
    container = LFRicKernelContainer("name", None)
    assert container.name == "name"
    assert container.metadata is None

    container = LFRicKernelContainer.create(
        "name", metadata, SymbolTable(), [])
    assert container.name == "name"
    assert container.metadata == metadata
    assert container.children == []

    source = (
        f"module test_mod\n{metadata.fortran_string()}contains\n"
        "subroutine test_code()\nend subroutine test_code\n"
        "end module test_mod\n")
    root = fortran_reader.psyir_from_source(source)
    RaisePSyIR2LFRicKernTrans().apply(
        root, {"metadata_name": "test_type"})
    assert isinstance(root.children[0], LFRicKernelContainer)
    root.children[0].lower_to_language_level()
    assert isinstance(root.children[0], Container)
    assert root.children[0].symbol_table.lookup("test_type")


def test_argument_metadata_classes():
    """Test construction, normalisation and output of argument records."""
    scalar = ScalarArgMetadata("GH_REAL", "GH_READ")
    assert scalar.fortran_string() == "arg_type(gh_scalar, gh_real, gh_read)"
    with pytest.raises(FrozenInstanceError):
        scalar.access = "gh_write"
    with pytest.raises(ValueError, match="Reduction access"):
        ScalarArgMetadata("gh_integer", "gh_reduction")
    with pytest.raises(TypeError, match="scalar datatype.*'int'"):
        ScalarArgMetadata(1, "gh_read")
    with pytest.raises(ValueError, match="scalar access descriptor"):
        ScalarArgMetadata("gh_real", "invalid")

    array = ScalarArrayArgMetadata("GH_REAL", "GH_READ", 2)
    assert array.fortran_string() == (
        "arg_type(gh_scalar_array, gh_real, gh_read, 2)")
    for ndims in (0, "2"):
        with pytest.raises(ValueError, match="number of scalar-array"):
            ScalarArrayArgMetadata("gh_real", "gh_read", ndims)

    field = FieldArgMetadata(
        "GH_REAL", "GH_READ", "W0", stencil="XORY1D", nlayers="NL",
        ndata=2)
    assert field.fortran_string() == (
        "arg_type(gh_field, gh_real, gh_read, w0, stencil(xory1d), "
        "nlayers='nl', ndata='2')")
    assert FieldArgMetadata(
        "gh_real", "gh_write", "w0", nlayers=None).nlayers == ""
    assert FieldArgMetadata(
        "gh_real", "gh_write", "w0", ndata=None).ndata == "1"
    for name, value in (("nlayers", "not a valid name"),
                        ("ndata", "0")):
        with pytest.raises(ValueError, match=name):
            FieldArgMetadata(
                "gh_real", "gh_write", "w0", **{name: value})

    vector = FieldVectorArgMetadata(
        "gh_real", "gh_read", "w1", "3", stencil="cross",
        nlayers="nl", ndata="2")
    assert vector.fortran_string() == (
        "arg_type(gh_field*3, gh_real, gh_read, w1, stencil(cross), "
        "nlayers='nl', ndata='2')")
    for length in (None, "bad", "1"):
        with pytest.raises(ValueError, match="Vector length"):
            FieldVectorArgMetadata("gh_real", "gh_read", "w0", length)

    intergrid = InterGridArgMetadata(
        "gh_real", "gh_read", "w0", "GH_COARSE", ndata="2")
    assert intergrid.fortran_string().endswith(
        "ndata='2', mesh_arg=gh_coarse)")
    intergrid_vector = InterGridVectorArgMetadata(
        "gh_real", "gh_read", "w1", "GH_FINE", "2", nlayers="nl")
    assert intergrid_vector.fortran_string().endswith(
        "nlayers='nl', mesh_arg=gh_fine)")
    for length in (None, "bad", "1"):
        with pytest.raises(ValueError, match="Vector length"):
            InterGridVectorArgMetadata(
                "gh_real", "gh_read", "w1", "gh_fine", length)

    operator = OperatorArgMetadata(
        "GH_REAL", "GH_READ", "W0", "W1")
    assert operator.fortran_string() == (
        "arg_type(gh_operator, gh_real, gh_read, w0, w1)")
    cma = ColumnwiseOperatorArgMetadata(
        "gh_real", "gh_write", "w0", "w1")
    assert cma.fortran_string().startswith(
        "arg_type(gh_columnwise_operator,")


def test_auxiliary_metadata_classes():
    """Test the meta-functions, reference-element and mesh records."""
    funcs = MetaFuncsArgMetadata("W0", True, True)
    assert funcs.fortran_string() == (
        "func_type(w0, gh_basis, gh_diff_basis)")
    for basis, diff in ((False, False),):
        with pytest.raises(ValueError, match="At least one"):
            MetaFuncsArgMetadata("w0", basis, diff)
    with pytest.raises(TypeError, match="flags must be booleans"):
        MetaFuncsArgMetadata("w0", 1, False)

    ref = MetaRefElementArgMetadata("NORMALS_TO_HORIZONTAL_FACES")
    assert ref.fortran_string() == (
        "reference_element_data_type(normals_to_horizontal_faces)")
    mesh = MetaMeshArgMetadata("ADJACENT_FACE")
    assert mesh.fortran_string() == "mesh_data_type(adjacent_face)"


def test_language_metadata_validation_and_queries():
    """Test top-level language metadata validation and query helpers."""
    field = FieldArgMetadata("gh_real", "gh_write", "w0")
    operator = OperatorArgMetadata("gh_real", "gh_read", "w0", "w1")
    metadata = _kernel_metadata(
        [field, operator], shapes=["GH_EVALUATOR"],
        evaluator_targets=["W0"],
        meta_funcs=[MetaFuncsArgMetadata("w0", True, False)])
    assert metadata.shapes == ("gh_evaluator",)
    assert metadata.kernel_type == "general-purpose"
    assert metadata.meta_args_get(FieldArgMetadata) == [field]
    assert metadata.meta_args_get([FieldArgMetadata, OperatorArgMetadata]) == [
        field, operator]
    assert metadata.field_meta_args_on_fs(FieldArgMetadata, "w0") == [field]
    assert metadata.field_meta_args_on_fs([FieldArgMetadata], "w1") == []
    assert metadata.operator_meta_args_on_fs(OperatorArgMetadata, "w1") == [
        operator]
    assert metadata.operator_meta_args_on_fs([OperatorArgMetadata], "w2") == []
    for name in ("shapes", "evaluator_targets", "meta_args", "meta_funcs",
                 "meta_ref_element", "meta_mesh"):
        with pytest.raises(TypeError, match=f"Expected {name}"):
            _kernel_metadata([field], **{name: "invalid"})
    with pytest.raises(TypeError, match="meta_args entries"):
        _kernel_metadata([object()])
    with pytest.raises(TypeError, match="meta_funcs entries"):
        _kernel_metadata([field], meta_funcs=[object()])
    with pytest.raises(ValueError, match="operates_on is required"):
        LFRicKernelMetadata(meta_args=[field])


def test_language_metadata_kernel_categories():
    """Test identification and invalid combinations of kernel categories."""
    scalar = ScalarArgMetadata("gh_real", "gh_read")
    field = FieldArgMetadata("gh_real", "gh_write", "w3")
    with pytest.raises(ParseError, match="at least one field or operator"):
        _kernel_metadata([scalar])
    with pytest.raises(ParseError, match="real-valued field"):
        _kernel_metadata([
            OperatorArgMetadata("gh_real", "gh_read", "w0", "w1"),
            FieldArgMetadata("gh_integer", "gh_write", "w0")])

    assert _kernel_metadata([field], operates_on="domain").kernel_type == (
        "domain")
    assert _kernel_metadata(
        [scalar, field], operates_on="domain").kernel_type == "domain"
    with pytest.raises(ParseError, match="only contain scalar or field"):
        _kernel_metadata([
            OperatorArgMetadata("gh_real", "gh_read", "w0", "w1")],
            operates_on="domain")
    with pytest.raises(ParseError, match="basis functions or mesh"):
        _kernel_metadata(
            [field], operates_on="domain",
            meta_funcs=[MetaFuncsArgMetadata("w3", True)])
    with pytest.raises(ParseError, match="basis functions or mesh"):
        _kernel_metadata(
            [field], operates_on="domain",
            meta_mesh=[MetaMeshArgMetadata("adjacent_face")])
    with pytest.raises(
            ParseError, match="only accept fields on discontinuous"):
        _kernel_metadata([
            FieldArgMetadata("gh_real", "gh_write", "w0")],
            operates_on="domain")

    coarse = InterGridArgMetadata(
        "gh_real", "gh_read", "w0", "gh_coarse")
    fine = InterGridVectorArgMetadata(
        "gh_real", "gh_write", "w1", "gh_fine", "2")
    assert _kernel_metadata([coarse, fine]).kernel_type == "inter-grid"
    with pytest.raises(ParseError, match="operate on cell_column"):
        _kernel_metadata([coarse, fine], operates_on="domain")
    with pytest.raises(ParseError, match="only contain inter-grid"):
        _kernel_metadata([coarse, fine, scalar])
    with pytest.raises(ParseError, match="both mesh types"):
        _kernel_metadata([coarse])
    same_space = InterGridArgMetadata(
        "gh_real", "gh_write", "w0", "gh_fine")
    with pytest.raises(ParseError, match="different function spaces"):
        _kernel_metadata([coarse, same_space])


def test_language_metadata_cma_categories():
    """Test classification and validation of CMA kernels."""
    cma_read = ColumnwiseOperatorArgMetadata(
        "gh_real", "gh_read", "w0", "w1")
    cma_write = ColumnwiseOperatorArgMetadata(
        "gh_real", "gh_write", "w0", "w1")
    lma = OperatorArgMetadata("gh_real", "gh_read", "w0", "w1")
    read_field = FieldArgMetadata("gh_real", "gh_read", "w1")
    write_field = FieldArgMetadata("gh_real", "gh_write", "w0")
    scalar = ScalarArgMetadata("gh_real", "gh_read")

    assert _kernel_metadata([cma_write, lma]).kernel_type == "cma-assembly"
    assert _kernel_metadata(
        [cma_read, read_field, write_field]).kernel_type == "cma-apply"
    assert _kernel_metadata(
        [cma_write, cma_read, scalar]).kernel_type == "cma-matrix-matrix"
    with pytest.raises(ParseError, match="operate on cell_column"):
        _kernel_metadata([cma_write], operates_on="domain")
    with pytest.raises(ParseError, match="assembly kernel"):
        _kernel_metadata([cma_read, lma])
    with pytest.raises(ParseError, match="apply kernel requires"):
        _kernel_metadata([cma_read, read_field])
    with pytest.raises(ParseError, match="one read and one written"):
        _kernel_metadata([
            cma_read, read_field,
            FieldArgMetadata("gh_real", "gh_read", "w0")])
    with pytest.raises(ParseError, match="field spaces must match"):
        _kernel_metadata([
            cma_read, read_field,
            FieldArgMetadata("gh_real", "gh_write", "w2")])
    with pytest.raises(ParseError, match="matrix-matrix"):
        _kernel_metadata([cma_read, scalar])
    with pytest.raises(ParseError, match="matrix-matrix"):
        _kernel_metadata([
            cma_write, ScalarArrayArgMetadata(
                "gh_real", "gh_read", 1)])
    with pytest.raises(ParseError, match="vector or stencil"):
        _kernel_metadata([
            cma_read,
            FieldVectorArgMetadata("gh_real", "gh_read", "w0", "2")])
    with pytest.raises(ParseError, match="default NDATA and NLAYERS"):
        _kernel_metadata([
            cma_read,
            FieldArgMetadata("gh_real", "gh_read", "w0", ndata="2")])
    with pytest.raises(ParseError, match="real-valued fields"):
        _kernel_metadata([
            cma_read, FieldArgMetadata("gh_integer", "gh_read", "w0")])
    with pytest.raises(ParseError, match="only read LMA operators"):
        _kernel_metadata([
            cma_write,
            OperatorArgMetadata("gh_real", "gh_write", "w0", "w1")])


def test_language_metadata_access_constraints():
    """Test invalid field, scalar and whole-kernel access combinations."""
    with pytest.raises(ParseError, match="must have one of.*accesses"):
        _kernel_metadata([
            FieldArgMetadata("gh_real", "gh_readwrite", "w0")])
    with pytest.raises(ParseError, match="read-only function space 'wchi'"):
        _kernel_metadata([
            FieldArgMetadata("gh_real", "gh_write", "wchi")])
    with pytest.raises(ParseError, match="must not write/update a scalar"):
        _kernel_metadata([
            FieldArgMetadata("gh_real", "gh_read", "w0"),
            ScalarArgMetadata("gh_real", "gh_reduction")])
    with pytest.raises(ParseError, match="at least one argument.*updated"):
        _kernel_metadata([
            FieldArgMetadata("gh_real", "gh_read", "w0")])


@pytest.mark.parametrize("case, message", [
    ("missing-space", "Function space 'w1'.*does not exist"),
    ("repeated-space", "Function space 'w0'.*repeated"),
    ("missing-shape", "must also supply gh_shape"),
    ("unneeded-shape", "specifies gh_shape but does not need"),
    ("wrong-target-shape", "requires gh_shape=gh_evaluator"),
    ("missing-target", "targets are not present"),
])
def test_language_metadata_evaluator_errors(case, message):
    """Test inconsistent evaluator metadata combinations."""
    functions = [MetaFuncsArgMetadata("w0", True)]
    cases = {
        "missing-space": {
            "meta_funcs": [MetaFuncsArgMetadata("w1", True)],
            "shapes": ["gh_evaluator"]},
        "repeated-space": {
            "meta_funcs": functions + [
                MetaFuncsArgMetadata("w0", False, True)],
            "shapes": ["gh_evaluator"]},
        "missing-shape": {"meta_funcs": functions},
        "unneeded-shape": {"shapes": ["gh_quadrature_xyoz"]},
        "wrong-target-shape": {
            "meta_funcs": functions, "shapes": ["gh_quadrature_xyoz"],
            "evaluator_targets": ["w0"]},
        "missing-target": {
            "meta_funcs": functions, "shapes": ["gh_evaluator"],
            "evaluator_targets": ["w1"]},
    }
    with pytest.raises(ParseError, match=message):
        _kernel_metadata([
            FieldArgMetadata("gh_real", "gh_write", "w0")], **cases[case])


def test_language_metadata_domain_dof_constraints(monkeypatch):
    """Test restrictions specific to domain and dof kernels."""
    with pytest.raises(ParseError, match="only contain scalar and field"):
        _kernel_metadata([
            OperatorArgMetadata("gh_real", "gh_write", "w0", "w1")],
            operates_on="dof")
    with pytest.raises(ParseError, match="may not request evaluator"):
        _kernel_metadata([
            FieldArgMetadata("gh_real", "gh_write", "w0")],
            operates_on="dof",
            meta_ref_element=[MetaRefElementArgMetadata(
                "normals_to_horizontal_faces")])
    with pytest.raises(ParseError, match="must use one function space"):
        _kernel_metadata([
            FieldArgMetadata("gh_real", "gh_write", "w0"),
            FieldArgMetadata("gh_real", "gh_read", "w1")],
            operates_on="dof")

    # Built-ins are separately validated and therefore bypass these checks.
    from psyclone.domain.lfric.lfric_builtins import BUILTIN_MAP
    monkeypatch.setitem(BUILTIN_MAP, "test_type", object())
    assert _kernel_metadata([
        FieldArgMetadata("gh_real", "gh_write", "w0")],
        operates_on="dof").kernel_type == "general-purpose"


def test_language_metadata_intergrid_constant_error(monkeypatch):
    """Inter-grid validation requires exactly two API mesh types."""
    arguments = [
        InterGridArgMetadata(
            "gh_real", "gh_read", "w0", "gh_coarse"),
        InterGridArgMetadata(
            "gh_real", "gh_write", "w1", "gh_fine"),
    ]
    monkeypatch.setattr(LFRicConstants, "VALID_MESH_TYPES", ["gh_coarse"])
    with pytest.raises(InternalError, match="exactly two mesh types"):
        _kernel_metadata(arguments)


def test_language_metadata_fortran_output():
    """Test complete Fortran serialisation and lowering to PSyIR."""
    metadata = _kernel_metadata(
        [FieldArgMetadata("gh_real", "gh_write", "w0")],
        shapes=["gh_evaluator", "gh_quadrature_xyoz"],
        evaluator_targets=["w0"],
        meta_funcs=[MetaFuncsArgMetadata("w0", True)],
        meta_ref_element=[MetaRefElementArgMetadata(
            "normals_to_horizontal_faces")],
        meta_mesh=[MetaMeshArgMetadata("adjacent_face")])
    output = metadata.fortran_string()
    assert "DIMENSION(2) :: gh_shape" in output
    assert "meta_reference_element" in output
    assert "meta_mesh" in output
    assert "code => test_code" in output
    symbol = metadata.lower_to_psyir()
    assert symbol.name == "test_type"
    assert isinstance(symbol.datatype, StructureType)

    single_shape = _kernel_metadata(
        [FieldArgMetadata("gh_real", "gh_write", "w0")],
        shapes=["gh_evaluator"],
        meta_funcs=[MetaFuncsArgMetadata("w0", True)],
        procedure_name=None)
    assert "INTEGER :: gh_shape = gh_evaluator" in (
        single_shape.fortran_string())
    with pytest.raises(ValueError, match="are required"):
        LFRicKernelMetadata(
            operates_on="cell_column",
            meta_args=[FieldArgMetadata("gh_real", "gh_write", "w0")],
        ).fortran_string()


@pytest.mark.parametrize("expression, message", [
    ("1", "Expected a metadata constructor.*Literal"),
    ("func_type(w0, gh_basis)", "arg_type constructor"),
    ("arg_type(gh_scalar, gh_real)", "at least three"),
    ("arg_type(gh_field, gh_real, gh_read, w0, w1, w2, w3, w4)",
     "at most 7"),
    ("arg_type(gh_field+2, gh_real, gh_read, w0)", "multiplication"),
    ("arg_type(gh_scalar*2, gh_real, gh_read)",
     "Vector notation is only supported for gh_field"),
    ("arg_type(gh_scalar, gh_real, gh_read, w0)",
     "Scalar metadata must have three"),
    ("arg_type(gh_scalar_array, gh_real, gh_read)",
     "Scalar-array metadata must have four"),
    ("arg_type(gh_operator, gh_real, gh_read, w0)",
     "Operator metadata must have five"),
    ("arg_type(invalid, gh_real, gh_read)", "first argument"),
    ("arg_type(gh_field, gh_real, gh_read)", "function space"),
    ("arg_type(gh_field, gh_real, gh_read, w0, other(x1d))",
     "Expected stencil"),
    ("arg_type(gh_field, gh_real, gh_read, w0, bad=gh_coarse)",
     "Unexpected named"),
    ("arg_type(gh_field, gh_real, gh_read, w0, stencil(x1d), w1)",
     "Unexpected positional"),
    ("arg_type(gh_field, gh_real, gh_read, w0, stencil(x1d, 1, 2))",
     "Expected stencil.*extent"),
    ("arg_type(gh_field, gh_real, gh_read, w0, stencil(x1d, 0))",
     "Stencil extent must be a positive"),
    ("arg_type(gh_field, gh_real, gh_read, w0, stencil(x1d, 1))",
     "fixed stencil extents"),
    ("arg_type(gh_field, gh_real, gh_write, w0, stencil(x1d))",
     "must be read-only"),
])
def test_parse_arg_errors(expression, message):
    """Test invalid forms of the arg_type constructor."""
    error = NotImplementedError if "fixed stencil" in message else ParseError
    with pytest.raises(error, match=message):
        metadata_mod._parse_arg(_expression(expression))


def test_parse_arg_variants():
    """Test parsing every supported arg_type constructor variant."""
    cases = [
        ("arg_type(gh_scalar, gh_real, gh_read)", ScalarArgMetadata),
        ("arg_type(gh_scalar_array, gh_real, gh_read, 2)",
         ScalarArrayArgMetadata),
        ("arg_type(gh_operator, gh_real, gh_read, w0, w1)",
         OperatorArgMetadata),
        ("arg_type(gh_columnwise_operator, gh_real, gh_read, w0, w1)",
         ColumnwiseOperatorArgMetadata),
        ("arg_type(gh_field, gh_real, gh_read, w0)", FieldArgMetadata),
        ("arg_type(gh_field*2, gh_real, gh_read, w0)",
         FieldVectorArgMetadata),
        ("arg_type(gh_field, gh_real, gh_read, w0, mesh_arg=gh_coarse)",
         InterGridArgMetadata),
        ("arg_type(gh_field*2, gh_real, gh_read, w0, "
         "mesh_arg=gh_fine)", InterGridVectorArgMetadata),
        ("arg_type(gh_field, gh_real, gh_read, w0, stencil(x1d), "
         "nlayers='nl', ndata='2')", FieldArgMetadata),
    ]
    for expression, expected_type in cases:
        assert isinstance(
            metadata_mod._parse_arg(_expression(expression)),
            expected_type)


@pytest.mark.parametrize("expression, message", [
    ("1", "Expected a metadata constructor.*Literal"),
    ("func_type(w0, 1)", "Invalid meta_funcs"),
    ("func_type(w0, gh_basis + gh_diff_basis)",
     "Expected a metadata name or literal.*BinaryOperation"),
    ("arg_type(w0, gh_basis)", "func_type constructor"),
    ("func_type(w0)", "requires a function space"),
    ("func_type(w0, gh_basis, gh_diff_basis, gh_basis)",
     "requires a function space"),
    ("func_type(w0, gh_basis, gh_basis)", "must not be repeated"),
    ("func_type(w0, invalid)", "Invalid meta_funcs"),
])
def test_parse_func_errors(expression, message):
    """Test invalid func_type constructor forms."""
    with pytest.raises(ParseError, match=message):
        metadata_mod._parse_func(_expression(expression))


def test_create_language_metadata_from_psyir_errors():
    """Test type checking when creating language metadata from a symbol."""
    for invalid in ("not a symbol", Container("node")):
        with pytest.raises(TypeError, match="Expected a DataTypeSymbol"):
            LFRicKernelMetadata.create_from_psyir(invalid)
    symbol = DataTypeSymbol("bad_type", ScalarType.real_type())
    with pytest.raises(TypeError, match="StructureType"):
        LFRicKernelMetadata.create_from_psyir(symbol)


@pytest.mark.parametrize("component, constructor", [
    ("meta_reference_element", "reference_element_data_type"),
    ("meta_mesh", "mesh_data_type"),
])
def test_optional_metadata_requires_constructor(
        component, constructor, fortran_reader):
    """Optional metadata arrays must contain structure constructors."""
    metadata = _kernel_metadata([
        FieldArgMetadata("gh_real", "gh_write", "w0")])
    source = metadata.fortran_string().replace(
        "  CONTAINS", f"  type({constructor}) :: {component}(1) = (/1/)\n"
        "  CONTAINS")
    with pytest.raises(ParseError,
                       match="Expected a metadata constructor.*Literal"):
        Kernel.create_from_psyir(
            LFRicKernelMetadata, fortran_reader.psyir_from_source(
                f"module test_mod\n{source}\ncontains\n"
                "subroutine test_code()\nend subroutine\nend module"))


@pytest.mark.parametrize("component, message", [
    ("integer :: unexpected = 1",
     "Unexpected LFRic metadata component"),
    ("type(reference_element_data_type), dimension(1) :: &\n"
     "meta_reference_element = (/ mesh_data_type(adjacent_face) /)",
     "meta_reference_element entries must use"),
    ("type(reference_element_data_type), dimension(2) :: &\n"
     "meta_reference_element = (/ &\n"
     "reference_element_data_type(normals_to_horizontal_faces), &\n"
     "reference_element_data_type(normals_to_horizontal_faces) /)",
     "meta_reference_element must not contain duplicates"),
    ("type(mesh_data_type), dimension(1) :: meta_mesh = (/ &\n"
     "reference_element_data_type(normals_to_horizontal_faces) /)",
     "meta_mesh entries must use"),
    ("type(mesh_data_type), dimension(2) :: meta_mesh = (/ &\n"
     "mesh_data_type(adjacent_face), mesh_data_type(adjacent_face) /)",
     "meta_mesh must not contain duplicates"),
])
def test_declaration_component_errors(component, message):
    """Test invalid optional components in language-level metadata."""
    source = f"""
module bad_mod
  type, extends(kernel_type) :: bad_type
    type(arg_type), dimension(1) :: meta_args = (/ &
      arg_type(gh_field, gh_real, gh_write, w0) /)
    {component}
    integer :: operates_on = cell_column
  end type bad_type
end module bad_mod
"""
    root = FortranReader().psyir_from_source(source)
    symbol = root.walk(Container)[1].symbol_table.lookup("bad_type")
    with pytest.raises(ParseError, match=message):
        LFRicKernelMetadata.create_from_psyir(symbol)


def test_declaration_errors():
    """Test errors found in complete language-level StructureTypes."""
    reader = FortranReader()
    root = reader.psyir_from_source(
        "module bad_mod\ntype :: bad_type\nend type bad_type\n"
        "end module bad_mod")
    symbol = root.walk(Container)[1].symbol_table.lookup("bad_type")
    with pytest.raises(ParseError, match="must extend kernel_type"):
        LFRicKernelMetadata.create_from_psyir(symbol)

    root = reader.psyir_from_source(
        "module bad_mod\ntype, extends(kernel_type) :: bad_type\n"
        "integer :: operates_on = cell_column\nend type bad_type\n"
        "end module bad_mod")
    symbol = root.walk(Container)[1].symbol_table.lookup("bad_type")
    with pytest.raises(ParseError, match="No meta_args"):
        LFRicKernelMetadata.create_from_psyir(symbol)


def test_rich_metadata_parsing_and_evaluator_target():
    """Parse all optional arrays and infer an evaluator target from a write."""
    Config.get().api = "lfric"
    mdata_code = '''
module rich_mod
  type, extends(kernel_type) :: rich_type
    type(arg_type), dimension(1) :: meta_args = (/ &
      arg_type(gh_field, gh_real, gh_write, w0) /)
    type(func_type), dimension(1) :: meta_funcs = (/ &
      func_type(w0, gh_basis) /)
    type(reference_element_data_type), dimension(1) :: &
      meta_reference_element = (/ &
      reference_element_data_type(normals_to_horizontal_faces) /)
    type(mesh_data_type) :: meta_mesh(1) = (/ &
      mesh_data_type(adjacent_face) /)
    integer, dimension(2) :: gh_shape = (/ &
      gh_evaluator, gh_quadrature_xyoz /)
    integer :: operates_on = cell_column
  contains
    procedure, nopass :: code => rich_code
  end type rich_type
contains
  subroutine rich_code()
  end subroutine rich_code
end module rich_mod
'''
    kernel_metadata = FortranReader().psyir_from_source(mdata_code)
    ktype = Kernel.create_from_psyir(
        LFRicKernelMetadata, kernel_metadata, name="rich_type").metadata

    assert ktype.eval_targets == ("w0",)
    assert len(ktype.meta_ref_element) == 1
    assert len(ktype.meta_mesh) == 1


def test_create_from_psyir_discovery_errors():
    """Test metadata discovery errors and module-name inference."""
    reader = FortranReader()
    with pytest.raises(TypeError, match="Expected PSyIR"):
        Kernel.create_from_psyir(LFRicKernelMetadata, "not psyir")
    root = reader.psyir_from_source("subroutine code()\nend subroutine code")
    with pytest.raises(ParseError, match="does not contain a module"):
        Kernel.create_from_psyir(LFRicKernelMetadata, root)

    root = reader.psyir_from_source("module abc\nend module abc")
    with pytest.raises(ParseError, match="too short"):
        Kernel.create_from_psyir(LFRicKernelMetadata, root)
    root = reader.psyir_from_source("module kernel\nend module kernel")
    with pytest.raises(ParseError, match="does not have '_mod'"):
        Kernel.create_from_psyir(LFRicKernelMetadata, root)
    root = reader.psyir_from_source("module kernel_mod\nend module kernel_mod")
    with pytest.raises(ParseError, match="kernel_type does not exist"):
        Kernel.create_from_psyir(LFRicKernelMetadata, root)

    two_modules = '''
module one_mod
  type, extends(kernel_type) :: common_type
  end type common_type
end module one_mod
module two_mod
  type, extends(kernel_type) :: common_type
  end type common_type
end module two_mod
'''
    root = reader.psyir_from_source(two_modules)
    with pytest.raises(ParseError, match="required for multiple modules"):
        Kernel.create_from_psyir(LFRicKernelMetadata, root)
    with pytest.raises(ParseError, match="not unique"):
        Kernel.create_from_psyir(LFRicKernelMetadata, root, "common_type")


def test_missing_bound_procedure(fortran_reader):
    """Test that a named type-bound procedure must be implemented."""
    code = '''
module missing_mod
  type, extends(kernel_type) :: missing_type
    type(arg_type), dimension(1) :: meta_args = (/ &
      arg_type(gh_field, gh_real, gh_write, w0) /)
    integer :: operates_on = cell_column
  contains
    procedure, nopass :: code => absent_code
  end type missing_type
    end module missing_mod
'''
    psyir = fortran_reader.psyir_from_source(code)
    with pytest.raises(ParseError, match="absent_code.*not found"):
        Kernel.create_from_psyir(LFRicKernelMetadata, psyir)

    invalid = code.replace("gh_real", "invalid_datatype").replace(
        "absent_code", "present_code").replace(
            "end type missing_type", "end type missing_type\ncontains\n"
            "  subroutine present_code()\n  end subroutine present_code")
    invalid_psyir = fortran_reader.psyir_from_source(invalid)
    with pytest.raises(ParseError, match="Invalid kernel metadata"):
        Kernel.create_from_psyir(LFRicKernelMetadata, invalid_psyir)


def test_generic_interface_procedure(fortran_reader):
    """Test resolution of multiple implementations through an interface."""
    code = '''
module generic_mod
  type, extends(kernel_type) :: generic_type
    type(arg_type), dimension(1) :: meta_args = (/ &
      arg_type(gh_field, gh_real, gh_write, w0) /)
    integer :: operates_on = cell_column
  end type generic_type
  interface generic_code
    module procedure code_one, code_two
  end interface generic_code
contains
  subroutine code_one()
  end subroutine code_one
  subroutine code_two()
  end subroutine code_two
end module generic_mod
'''
    psyir = fortran_reader.psyir_from_source(code)
    kernel = Kernel.create_from_psyir(LFRicKernelMetadata, psyir)
    assert kernel.procedure_name == "generic_code"
    assert len(kernel.procedures) == 2

    no_interface = code.replace(
        "  interface generic_code\n"
        "    module procedure code_one, code_two\n"
        "  end interface generic_code\n", "")
    no_interface_psyir = fortran_reader.psyir_from_source(no_interface)
    with pytest.raises(ParseError, match="exactly one generic interface"):
        Kernel.create_from_psyir(LFRicKernelMetadata, no_interface_psyir)

    missing_implementation = code.replace(
        "module procedure code_one, code_two",
        "module procedure code_one, absent_code").replace(
            "  subroutine code_two()\n  end subroutine code_two\n", "")
    missing_psyir = fortran_reader.psyir_from_source(missing_implementation)
    with pytest.raises(ParseError, match="Not all procedures"):
        Kernel.create_from_psyir(LFRicKernelMetadata, missing_psyir)
