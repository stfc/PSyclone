from psyclone.domain.lfric import ArgIndexToMetadataIndex
from psyclone.domain.lfric.kernel import ScalarArgMetadata, FieldArgMetadata, LFRicKernelMetadata

def test_scalar():
    '''Test that the index calculation works for scalars with all
    supported datatypes. Note, we need one field for the metadata to
    be valid.

    '''
    meta_args = [
        FieldArgMetadata("GH_REAL", "GH_INC", "W0"),
        ScalarArgMetadata("GH_REAL", "GH_READ"),
        ScalarArgMetadata("GH_INTEGER", "GH_READ"),
        ScalarArgMetadata("GH_LOGICAL", "GH_READ")]
    metadata = LFRicKernelMetadata(
        operates_on="cell_column", meta_args=meta_args)
    metadata.validate()
    
    lookup = ArgIndexToMetadataIndex.mapping(metadata)

    assert len(lookup) == 4
    assert lookup[1] == 0
    assert lookup[2] == 1
    assert lookup[3] == 2
    assert lookup[4] == 3
