"""Tests for the PSyIR-based kernel metadata parser."""

from psyclone.configuration import Config
from psyclone.domain.lfric.lfric_kern import LFRicKern
from psyclone.domain.lfric.kernel import LFRicKernMetadata
from psyclone.parse.kernel import get_kernel_psyir, KernelTypeFactory


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
