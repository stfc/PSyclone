''' xxx '''
import pytest

from psyclone.domain.lfric import LFRicTypes
from psyclone.domain.lfric.formal_kernel_args_from_metadata import (
    FormalKernelArgsFromMetadata)
from psyclone.domain.lfric.kernel import (
    ScalarArgMetadata, FieldArgMetadata, LFRicKernelMetadata,
    FieldVectorArgMetadata, OperatorArgMetadata, ColumnwiseOperatorArgMetadata)

from psyclone.psyir.symbols import SymbolTable, DataSymbol


def call_method(method_name, *args, metadata=None):
    ''' xxx '''
    cls = FormalKernelArgsFromMetadata
    cls._info = SymbolTable()
    cls._metadata = metadata
    getattr(cls, method_name)(*args)
    return cls

def check_single_symbol(method_name, datasymbol_name, symbol_name, *args, metadata=None):
    ''' xxx '''
    cls = call_method(method_name, *args, metadata=metadata)
    lfric_class = LFRicTypes(datasymbol_name)
    assert isinstance(cls._info.lookup(symbol_name), lfric_class)
    assert len(cls._info.argument_list) == 1


def test_cell_position():
    ''' Test _cell_position method. '''
    check_single_symbol("_cell_position", "CellPositionDataSymbol", "cell")


def test_mesh_height():
    ''' Test _mesh_height method. '''
    check_single_symbol("_mesh_height", "MeshHeightDataSymbol", "nlayers")


def test_mesh_ncell2d_no_halos():
    ''' Test _mesh_ncell2d_no_halos method. '''
    check_single_symbol("_mesh_ncell2d_no_halos", "LFRicIntegerScalarDataSymbol", "ncell_2d_no_halos")


def test_mesh_ncell2d():
    ''' Test _mesh_ncell2d method. '''
    check_single_symbol("_mesh_ncell2d", "LFRicIntegerScalarDataSymbol", "ncell_2d")


def test_cell_map():
    ''' Test _cell_map method. '''
    cls = call_method("_cell_map")
    lfric_class = LFRicTypes("LFRicIntegerScalarDataSymbol")
    assert isinstance(cls._info.lookup("cell_map"), DataSymbol)
    assert isinstance(cls._info.lookup("ncell_f_per_c_x"), lfric_class)
    assert isinstance(cls._info.lookup("ncell_f_per_c_y"), lfric_class)
    assert isinstance(cls._info.lookup("ncell_f"), lfric_class)
    assert len(cls._info.argument_list) == 4


def test_scalar():
    ''' Test _scalar method. '''
    # At least one field arg is required for the metadata to be valid
    # even though we only want to test the scalar metadata.
    field_meta_arg = FieldArgMetadata("GH_REAL", "GH_WRITE", "W3")
    scalar_meta_arg = ScalarArgMetadata("GH_REAL", "GH_READ")
    metadata = LFRicKernelMetadata(operates_on="cell_column", meta_args=[field_meta_arg, scalar_meta_arg])
    metadata.validate()
    check_single_symbol("_scalar", "LFRicRealScalarDataSymbol", "rscalar_2", scalar_meta_arg, metadata=metadata)


def test_field():
    ''' Test _field method. '''
    field_meta_arg = FieldArgMetadata("GH_REAL", "GH_WRITE", "W3")
    metadata = LFRicKernelMetadata(operates_on="cell_column", meta_args=[field_meta_arg])
    metadata.validate()
    check_single_symbol("_field", "RealFieldDataSymbol", "rfield_1", field_meta_arg, metadata=metadata)


def test_field_vector():
    ''' Test _field_vector method. '''
    field_meta_arg = FieldVectorArgMetadata("GH_REAL", "GH_WRITE", "W3", "3")
    metadata = LFRicKernelMetadata(operates_on="cell_column", meta_args=[field_meta_arg])
    metadata.validate()
    cls = call_method("_field_vector", field_meta_arg, metadata=metadata)
    lfric_class = LFRicTypes("RealFieldDataSymbol")
    assert isinstance(cls._info.lookup("rfield_1_v1"), lfric_class)
    assert isinstance(cls._info.lookup("rfield_1_v2"), lfric_class)
    assert isinstance(cls._info.lookup("rfield_1_v3"), lfric_class)
    assert len(cls._info.argument_list) == 3


def test_operator():
    ''' Test _operator method. '''
    operator_meta_arg = OperatorArgMetadata("GH_REAL", "GH_WRITE", "W3", "W2")
    metadata = LFRicKernelMetadata(operates_on="cell_column", meta_args=[operator_meta_arg])
    metadata.validate()
    cls = call_method("_operator", operator_meta_arg, metadata=metadata)
    lfric_class = LFRicTypes("LFRicIntegerScalarDataSymbol")
    assert isinstance(cls._info.lookup("op_1_ncell_3d"), lfric_class)
    lfric_class = LFRicTypes("OperatorDataSymbol")
    assert isinstance(cls._info.lookup("op_1"), lfric_class)
    assert len(cls._info.argument_list) == 2


def check_common_cma_symbols(fs1, fs2):
    ''' xxx '''
    operator_meta_arg = ColumnwiseOperatorArgMetadata("GH_REAL", "GH_WRITE", fs1, fs2)
    metadata = LFRicKernelMetadata(operates_on="cell_column", meta_args=[operator_meta_arg])
    metadata.validate()
    cls = call_method("_cma_operator", operator_meta_arg, metadata=metadata)
    lfric_class = LFRicTypes("OperatorDataSymbol")
    assert isinstance(cls._info.lookup("cma_op_1"), lfric_class)
    lfric_class = LFRicTypes("LFRicIntegerScalarDataSymbol")
    assert isinstance(cls._info.lookup("nrow_cma_op_1"), lfric_class)
    assert isinstance(cls._info.lookup("bandwidth_cma_op_1"), lfric_class)
    assert isinstance(cls._info.lookup("alpha_cma_op_1"), lfric_class)
    assert isinstance(cls._info.lookup("beta_cma_op_1"), lfric_class)
    assert isinstance(cls._info.lookup("gamma_m_cma_op_1"), lfric_class)
    assert isinstance(cls._info.lookup("gamma_p_cma_op_1"), lfric_class)
    return cls


def test_cma_operator():
    ''' Test _cma_operator method. '''
    # to/from function spaces differ
    cls = check_common_cma_symbols("W3", "W2")
    lfric_class = LFRicTypes("LFRicIntegerScalarDataSymbol")
    assert isinstance(cls._info.lookup("ncol_cma_op_1"), lfric_class)
    assert len(cls._info.argument_list) == 8

    # to/from function spaces are the same
    cls = check_common_cma_symbols("W3", "W3")
    with pytest.raises(KeyError):
        cls._info.lookup("ncol_cma_op_1")
    assert len(cls._info.argument_list) == 7
