"""Tests for LFRic kernel metadata implementation."""

from dataclasses import FrozenInstanceError

import pytest

from psyclone.configuration import Config
from psyclone.core import AccessType
from psyclone.domain.lfric.lfric_kern import LFRicKern
from psyclone.domain.lfric.kernel import (
    ColumnwiseOperatorArgMetadata, FieldArgMetadata,
    FieldVectorArgMetadata, InterGridArgMetadata,
    InterGridVectorArgMetadata, KernelProcedure, LFRicArgDescriptor,
    LFRicFuncDescriptor, LFRicKernMetadata, LFRicKernelMetadata,
    LFRicPropertyMetadata, MetaFuncsArgMetadata, MetaMeshArgMetadata,
    MetaRefElementArgMetadata, OperatorArgMetadata, ScalarArgMetadata,
    ScalarArrayArgMetadata, find_metadata_symbol)
from psyclone.domain.lfric.kernel import metadata as metadata_mod
from psyclone.parse.utils import ParseError
from psyclone.parse.kernel import get_kernel_psyir, KernelTypeFactory
from psyclone.psyir.frontend.fortran import FortranReader
from psyclone.psyir.nodes import Container
from psyclone.psyir.symbols import (
    DataTypeSymbol, ScalarType, StructureType)


def _expression(source):
    """Create a PSyIR expression for node-level metadata tests."""
    return FortranReader().psyir_from_expression(source)


def _descriptor(access=AccessType.WRITE, argument_type="gh_field",
                function_space="w0", **kwargs):
    """Create a consumer-facing argument descriptor with useful defaults."""
    return LFRicArgDescriptor(
        access, function_space, 0, None, argument_type, "gh_real", **kwargs)


def _kernel_metadata(arguments, **kwargs):
    """Create language-level metadata with useful defaults."""
    values = {"operates_on": "cell_column", "meta_args": arguments,
              "procedure_name": "test_code", "name": "test_type"}
    values.update(kwargs)
    return LFRicKernelMetadata(**values)


def _consumer_metadata(arg_descriptors, **kwargs):
    """Create consumer-facing metadata with useful defaults."""
    values = {"name": "test_type", "iterates_over": "cell_column",
              "procedure": KernelProcedure("test_code"),
              "arg_descriptors": tuple(arg_descriptors),
              "psyir": Container("test")}
    values.update(kwargs)
    return LFRicKernMetadata(**values)


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
    kernel_metadata = get_kernel_psyir(mdata_code)
    ktype = KernelTypeFactory(api="lfric").create(
        kernel_metadata, name="testkern_field_type")

    assert isinstance(ktype, LFRicKernMetadata)
    assert ktype.name == "testkern_field_type"
    assert ktype.iterates_over == "cell_column"
    assert ktype.nargs == 8
    assert ktype.eval_shapes == ("gh_quadrature_xyoz",)

    kernel = LFRicKern()
    kernel.load_meta(ktype)
    assert kernel.name == "testkern_field_code"
    assert kernel.iterates_over == "cell_column"


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
        "GH_REAL", "GH_READ", "W0", "XORY1D", "NL", 2)
    assert field.fortran_string() == (
        "arg_type(gh_field, gh_real, gh_read, w0, stencil(xory1d), "
        "nlevels='nl', ndata='2')")
    assert FieldArgMetadata(
        "gh_real", "gh_write", "w0", ndata=None).ndata == "1"

    vector = FieldVectorArgMetadata(
        "gh_real", "gh_read", "w1", "3", "cross", "nl", "2")
    assert vector.fortran_string() == (
        "arg_type(gh_field*3, gh_real, gh_read, w1, stencil(cross), "
        "nlevels='nl', ndata='2')")
    for length in (None, "bad", "1"):
        with pytest.raises(ValueError, match="Vector length"):
            FieldVectorArgMetadata("gh_real", "gh_read", "w0", length)

    intergrid = InterGridArgMetadata(
        "gh_real", "gh_read", "w0", "GH_COARSE", ndata="2")
    assert intergrid.fortran_string().endswith(
        "ndata='2', mesh_arg=gh_coarse)")
    intergrid_vector = InterGridVectorArgMetadata(
        "gh_real", "gh_read", "w1", "GH_FINE", "2", nlevels="nl")
    assert intergrid_vector.fortran_string().endswith(
        "nlevels='nl', mesh_arg=gh_fine)")

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
        [field, operator], shapes=["GH_QUADRATURE_XYOZ"],
        evaluator_targets=["W0"])
    assert metadata.shapes == ("gh_quadrature_xyoz",)
    assert metadata.kernel_type == "general-purpose"
    assert metadata.meta_args_get(FieldArgMetadata) == [field]
    assert metadata.meta_args_get([FieldArgMetadata, OperatorArgMetadata]) == [
        field, operator]
    assert metadata.field_meta_args_on_fs(FieldArgMetadata, "w0") == [field]
    assert metadata.field_meta_args_on_fs([FieldArgMetadata], "w1") == []
    assert metadata.operator_meta_args_on_fs(OperatorArgMetadata, "w1") == [
        operator]
    assert metadata.operator_meta_args_on_fs([OperatorArgMetadata], "w2") == []
    metadata.validate()

    for name in ("shapes", "evaluator_targets", "meta_args", "meta_funcs",
                 "meta_ref_element", "meta_mesh"):
        with pytest.raises(TypeError, match=f"Expected {name}"):
            _kernel_metadata([field], **{name: "invalid"})
    with pytest.raises(TypeError, match="meta_args entries"):
        _kernel_metadata([object()])
    with pytest.raises(TypeError, match="meta_funcs entries"):
        _kernel_metadata([field], meta_funcs=[object()])


def test_language_metadata_kernel_categories():
    """Test identification and invalid combinations of kernel categories."""
    scalar = ScalarArgMetadata("gh_real", "gh_read")
    field = FieldArgMetadata("gh_real", "gh_write", "w0")
    with pytest.raises(ParseError, match="at least one field or operator"):
        _kernel_metadata([scalar]).validate()
    with pytest.raises(ParseError, match="real-valued field"):
        _kernel_metadata([
            OperatorArgMetadata("gh_real", "gh_read", "w0", "w1"),
            FieldArgMetadata("gh_integer", "gh_write", "w0")]).validate()

    assert _kernel_metadata([field], operates_on="domain").kernel_type == (
        "domain")
    with pytest.raises(ParseError, match="only contain scalar or field"):
        _kernel_metadata([
            OperatorArgMetadata("gh_real", "gh_read", "w0", "w1")],
            operates_on="domain").validate()
    with pytest.raises(ParseError, match="basis functions or mesh"):
        _kernel_metadata(
            [field], operates_on="domain",
            meta_funcs=[MetaFuncsArgMetadata("w0", True)]).validate()
    with pytest.raises(ParseError, match="basis functions or mesh"):
        _kernel_metadata(
            [field], operates_on="domain",
            meta_mesh=[MetaMeshArgMetadata("adjacent_face")]).validate()

    coarse = InterGridArgMetadata(
        "gh_real", "gh_read", "w0", "gh_coarse")
    fine = InterGridVectorArgMetadata(
        "gh_real", "gh_write", "w1", "gh_fine", "2")
    assert _kernel_metadata([coarse, fine]).kernel_type == "inter-grid"
    with pytest.raises(ParseError, match="operate on cell_column"):
        _kernel_metadata([coarse, fine], operates_on="domain").validate()
    with pytest.raises(ParseError, match="only contain inter-grid"):
        _kernel_metadata([coarse, fine, scalar]).validate()
    with pytest.raises(ParseError, match="both mesh types"):
        _kernel_metadata([coarse]).validate()
    same_space = InterGridArgMetadata(
        "gh_real", "gh_write", "w0", "gh_fine")
    with pytest.raises(ParseError, match="different function spaces"):
        _kernel_metadata([coarse, same_space]).validate()


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
        _kernel_metadata([cma_write], operates_on="domain").validate()
    with pytest.raises(ParseError, match="assembly kernel"):
        _kernel_metadata([cma_read, lma]).validate()
    with pytest.raises(ParseError, match="apply kernel requires"):
        _kernel_metadata([cma_read, read_field]).validate()
    with pytest.raises(ParseError, match="one read and one written"):
        _kernel_metadata([
            cma_read, read_field,
            FieldArgMetadata("gh_real", "gh_read", "w0")]).validate()
    with pytest.raises(ParseError, match="field spaces must match"):
        _kernel_metadata([
            cma_read, read_field,
            FieldArgMetadata("gh_real", "gh_write", "w2")]).validate()
    with pytest.raises(ParseError, match="matrix-matrix"):
        _kernel_metadata([cma_read, scalar]).validate()
    with pytest.raises(ParseError, match="matrix-matrix"):
        _kernel_metadata([
            cma_write, ScalarArrayArgMetadata(
                "gh_real", "gh_read", 1)]).validate()


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
        shapes=["gh_evaluator"], procedure_name=None)
    assert "INTEGER :: gh_shape = gh_evaluator" in (
        single_shape.fortran_string())
    with pytest.raises(ValueError, match="are required"):
        LFRicKernelMetadata().fortran_string()


def test_parser_helpers():
    """Test the small PSyIR-node helpers and their error handling."""
    assert metadata_mod._name(_expression("W0")) == "w0"
    assert metadata_mod._name(_expression("'VALUE'")) == "value"
    assert len(metadata_mod._array_values(
        _expression("(/w0, w1/)")
    )) == 2
    assert len(metadata_mod._array_values(
        _expression("w0"))) == 1
    with pytest.raises(ParseError, match="metadata constructor"):
        metadata_mod._call_name(_expression("w0"))
    with pytest.raises(ParseError, match="metadata name or literal"):
        metadata_mod._name(_expression("w0 + w1"))


@pytest.mark.parametrize("expression, message", [
    ("func_type(w0, gh_basis)", "arg_type constructor"),
    ("arg_type(gh_scalar, gh_real)", "at least three"),
    ("arg_type(gh_field, gh_real, gh_read, w0, w1, w2, w3, w4)",
     "at most 7"),
    ("arg_type(gh_field+2, gh_real, gh_read, w0)", "multiplication"),
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
         "nlevels='nl', ndata='2')", FieldArgMetadata),
    ]
    for expression, expected_type in cases:
        assert isinstance(
            metadata_mod._parse_arg(_expression(expression)),
            expected_type)


@pytest.mark.parametrize("expression, message", [
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


def test_consumer_descriptor_methods():
    """Test compatibility properties and representations of descriptors."""
    scalar = LFRicArgDescriptor(
        AccessType.READ, None, 0, None, "gh_scalar", "gh_real")
    assert scalar.stencil is None
    assert not scalar.function_spaces
    assert "argument_type='gh_scalar'" in str(scalar)
    field = _descriptor(stencil_type="x1d", stencil_extent=None)
    assert field.stencil == {"type": "x1d", "extent": None}
    assert field.function_spaces == ("w0",)
    operator = _descriptor(
        argument_type="gh_operator", function_space=None,
        function_space_to="w1", function_space_from="w2")
    assert operator.function_spaces == ("w1", "w2")
    func = LFRicFuncDescriptor("w0", ("gh_basis", "gh_diff_basis"))
    assert repr(func) == (
        "LFRicFuncDescriptor(func_type(w0, gh_basis, gh_diff_basis))")
    assert "operator_names=(gh_basis, gh_diff_basis)" in str(func)


def test_consumer_metadata_validation():
    """Test validation performed by the consumer-facing metadata record."""
    valid = _consumer_metadata([_descriptor()])
    assert valid.nargs == 1
    assert valid._ast is valid.psyir

    with pytest.raises(ParseError, match="read-only function space"):
        _consumer_metadata([_descriptor(function_space="wchi")])
    with pytest.raises(ParseError, match="must not write.*scalar"):
        _consumer_metadata([_descriptor(
            argument_type="gh_scalar", function_space=None)])
    with pytest.raises(ParseError, match="at least one argument"):
        _consumer_metadata([_descriptor(access=AccessType.READ)])

    basis = LFRicFuncDescriptor("w0", ("gh_basis",))
    with pytest.raises(ParseError, match="does not exist"):
        _consumer_metadata([_descriptor()], func_descriptors=(
            LFRicFuncDescriptor("w1", ("gh_basis",)),),
                           eval_shapes=("gh_quadrature_xyoz",))
    with pytest.raises(ParseError, match="repeated in meta_funcs"):
        _consumer_metadata(
            [_descriptor()], func_descriptors=(basis, basis),
            eval_shapes=("gh_quadrature_xyoz",))
    with pytest.raises(ParseError, match="must also supply gh_shape"):
        _consumer_metadata([_descriptor()], func_descriptors=(basis,))
    with pytest.raises(ParseError, match="does not need an evaluator"):
        _consumer_metadata([_descriptor()], eval_shapes=("gh_evaluator",))
    with pytest.raises(ParseError, match="requires gh_shape=gh_evaluator"):
        _consumer_metadata(
            [_descriptor()], func_descriptors=(basis,),
            eval_shapes=("gh_quadrature_xyoz",), eval_targets=("w0",))
    with pytest.raises(ParseError, match="not present in meta_args"):
        _consumer_metadata(
            [_descriptor()], func_descriptors=(basis,),
            eval_shapes=("gh_evaluator",), eval_targets=("w1",))


def test_consumer_cma_and_domain_validation():
    """Test consumer restrictions for CMA, domain and dof kernels."""
    with pytest.raises(ParseError, match="vector or stencil"):
        _consumer_metadata(
            [_descriptor(vector_size=2)], cma_operation="apply")
    with pytest.raises(ParseError, match="default NDATA and NLEVELS"):
        _consumer_metadata(
            [_descriptor(ndata="2")], cma_operation="apply")

    operator = _descriptor(argument_type="gh_operator")
    with pytest.raises(ParseError, match="only contain scalar and field"):
        _consumer_metadata([operator], iterates_over="domain")
    with pytest.raises(ParseError, match="may not request evaluator"):
        _consumer_metadata(
            [_descriptor()], iterates_over="domain",
            reference_element=LFRicPropertyMetadata(("property",)))
    with pytest.raises(ParseError, match="one function space"):
        _consumer_metadata(
            [_descriptor(), _descriptor(function_space="w1")],
            iterates_over="dof")
    builtin = _consumer_metadata(
        [_descriptor()], iterates_over="dof", name="x_plus_y")
    assert builtin.iterates_over == "dof"


def test_create_language_metadata_from_psyir_errors():
    """Test type checking when creating language metadata from a symbol."""
    with pytest.raises(TypeError, match="Expected a DataTypeSymbol"):
        LFRicKernelMetadata.create_from_psyir("not a symbol")
    symbol = DataTypeSymbol("bad_type", ScalarType.real_type())
    with pytest.raises(TypeError, match="StructureType"):
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
    kernel_metadata = get_kernel_psyir(mdata_code)
    ktype = KernelTypeFactory(api="lfric").create(
        kernel_metadata, name="rich_type")

    assert ktype.eval_targets == ("w0",)
    assert len(ktype.reference_element.properties) == 1
    assert len(ktype.mesh.properties) == 1


def test_create_from_fortran_string_errors(monkeypatch):
    """Test the public complete-source constructor's error paths."""
    with pytest.raises(TypeError, match="source must be supplied as a string"):
        LFRicKernMetadata.create_from_fortran_string(1)

    def broken_reader(_self, _source):
        """Stand in for a frontend failure."""
        raise RuntimeError("broken frontend")

    monkeypatch.setattr(FortranReader, "psyir_from_source", broken_reader)
    with pytest.raises(ValueError, match="Failed to translate"):
        LFRicKernMetadata.create_from_fortran_string("not Fortran")


def test_find_metadata_symbol_errors():
    """Test metadata discovery errors and module-name inference."""
    reader = FortranReader()
    with pytest.raises(TypeError, match="Expected PSyIR"):
        find_metadata_symbol("not psyir")
    root = reader.psyir_from_source("subroutine code()\nend subroutine code")
    with pytest.raises(ParseError, match="does not contain a module"):
        find_metadata_symbol(root)

    root = reader.psyir_from_source("module abc\nend module abc")
    with pytest.raises(ParseError, match="too short"):
        find_metadata_symbol(root)
    root = reader.psyir_from_source("module kernel\nend module kernel")
    with pytest.raises(ParseError, match="does not have '_mod'"):
        find_metadata_symbol(root)
    root = reader.psyir_from_source("module kernel_mod\nend module kernel_mod")
    with pytest.raises(ParseError, match="kernel_type does not exist"):
        find_metadata_symbol(root)

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
        find_metadata_symbol(root)
    with pytest.raises(ParseError, match="not unique"):
        find_metadata_symbol(root, "common_type")


def test_missing_bound_procedure():
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
    with pytest.raises(ParseError, match="absent_code.*not found"):
        LFRicKernMetadata.create_from_fortran_string(code)

    invalid = code.replace("gh_real", "invalid_datatype").replace(
        "absent_code", "present_code").replace(
            "end type missing_type", "end type missing_type\ncontains\n"
            "  subroutine present_code()\n  end subroutine present_code")
    with pytest.raises(ParseError, match="Invalid LFRic metadata"):
        LFRicKernMetadata.create_from_fortran_string(invalid)


def test_generic_interface_procedure():
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
    metadata = LFRicKernMetadata.create_from_fortran_string(code)
    assert metadata.procedure.name == "generic_code"
    assert metadata.procedure.ast is None
    assert len(metadata.procedure.implementations) == 2

    no_interface = code.replace(
        "  interface generic_code\n"
        "    module procedure code_one, code_two\n"
        "  end interface generic_code\n", "")
    with pytest.raises(ParseError, match="exactly one generic interface"):
        LFRicKernMetadata.create_from_fortran_string(no_interface)

    missing_implementation = code.replace(
        "module procedure code_one, code_two",
        "module procedure code_one, absent_code").replace(
            "  subroutine code_two()\n  end subroutine code_two\n", "")
    with pytest.raises(ParseError, match="Not all procedures"):
        LFRicKernMetadata.create_from_fortran_string(missing_implementation)


def test_descriptor_from_operator_metadata():
    """Test conversion of an operator's from/to function spaces."""
    entry = OperatorArgMetadata("gh_real", "gh_read", "w0", "w1")
    descriptor = metadata_mod._descriptor_from_metadata(entry, 3)
    assert descriptor.function_space == "w1"
    assert descriptor.function_space_to == "w0"
    assert descriptor.function_space_from == "w1"
    assert descriptor.metadata_index == 3
