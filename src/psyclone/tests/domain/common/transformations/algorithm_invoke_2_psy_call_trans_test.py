''' Module containing pytest unit tests for the AlgorithmInvoke2PSyCallTrans
transformation.

'''
from psyclone.psyir.symbols import RoutineSymbol, DataTypeSymbol, \
    StructureType, Symbol, REAL_TYPE, DataSymbol, INTEGER_TYPE
from psyclone.domain.common.algorithm import AlgorithmInvokeCall, \
    KernelFunctor
from psyclone.domain.common.transformations import AlgTrans
from psyclone.domain.common.transformations import AlgorithmInvoke2PSyCallTrans


def test_aic_defroutinerootname():
    '''Check that the _def_routine_root_name() internal method behaves as
    expected.

    '''
    trans = AlgorithmInvoke2PSyCallTrans()
    symbol_name = "dummy"
    kernel_functor = KernelFunctor(DataTypeSymbol(symbol_name, REAL_TYPE))
    routine = RoutineSymbol("hello")
    index = 3
    call = AlgorithmInvokeCall(routine, index)
    call.children = [kernel_functor]
    assert (trans._def_routine_root_name(call) ==
            f"invoke_{index}_{symbol_name}")

    call.children.append(kernel_functor.copy())
    assert trans._def_routine_root_name(call) == f"invoke_{index}"

    for name in [" a  description ", "' a__description '",
                 "\" a  description \""]:
        call._name = name
        assert call._def_routine_root_name() == "invoke_a__description"

    # lowering should not prepend "invoke" if the invoke call has a
    # name starting with "invoke"
    symbol_name = "dummy"
    kernel_functor = KernelFunctor(DataTypeSymbol(symbol_name, REAL_TYPE))
    routine = RoutineSymbol("hello")
    index = 3
    call = AlgorithmInvokeCall(routine, index, name="invoke_1")
    call.children = [kernel_functor]
    assert trans._def_routine_root_name(call) == "invoke_1"


def test_aic_defroutineroot_name_error():
    '''Check that the _def_routine_root_name() internal method raises the
    expected exception if the supplied name is invalid.

    '''
    symbol_name = "dummy"
    kernel_functor = KernelFunctor(DataTypeSymbol(symbol_name, REAL_TYPE))
    routine = RoutineSymbol("hello")
    index = 3
    call = AlgorithmInvokeCall(routine, index)
    call.children = [kernel_functor]
    assert call._def_routine_root_name() == f"invoke_{index}_{symbol_name}"

    call.children.append(kernel_functor.copy())
    assert call._def_routine_root_name() == f"invoke_{index}"

    for name in ["1name", "name!", "nameʑ", "ʒʓʔʕʗʘʙʚʛʜʝʞ"]:
        call._name = name
        with pytest.raises(TypeError) as info:
            _ = call._def_routine_root_name()
        print(name)
        assert (f"AlgorithmInvokeCall:_def_routine_root_name() the (optional) "
                f"name of an invoke must be a string containing a valid name "
                f"(with any spaces replaced by underscores) but found "
                f"'{name}'." in str(info.value))


def test_aic_createpsylayersymbolrootnames(fortran_reader):
    '''Check that the create_psylayer_symbol_root_names method behaves in
    the expected way when the name comes from a subroutine, a module
    and when it has a filecontainer, i.e. it creates and stores a root
    name for a routine symbol and a container symbol. Also check that
    it raises the expected exception if no FileContainer or Routine
    nodes are found in the tree.

    '''
    code = (
        "subroutine alg1()\n"
        "  use kern_mod, only : kern\n"
        "  use field_mod, only : field_type\n"
        "  type(field_type) :: field1\n"
        "  call invoke(kern(field1))\n"
        "end subroutine alg1\n")

    # FileContainer and Routine (subroutine)
    psyir = fortran_reader(code)
    AlgTrans().apply(psyir)
    invoke = psyir.children[0][0]
    _check_alg_names(invoke, "psy_alg1")

    # Routine, no FileContainer
    psyir = create_alg_psyir(code)
    psyir = psyir.children[0]
    psyir.detach()
    invoke = psyir[0]
    _check_alg_names(invoke, "psy_alg1")

    code = (
        "module my_mod\n"
        "contains\n"
        "subroutine alg1()\n"
        "  use kern_mod, only : kern\n"
        "  use field_mod, only : field_type\n"
        "  type(field_type) :: field1\n"
        "  call invoke(kern(field1))\n"
        "end subroutine alg1\n"
        "end module my_mod\n")

    # File container and module
    psyir = create_alg_psyir(code)
    invoke = psyir.children[0].children[0][0]
    _check_alg_names(invoke, "psy_my_mod")

    # Module, no FileContainer
    psyir = create_alg_psyir(code)
    psyir = psyir.children[0]
    psyir.detach()
    invoke = psyir.children[0][0]
    _check_alg_names(invoke, "psy_my_mod")

    # No modules or FileContainers (should not happen)
    invoke._psylayer_container_root_name = None
    invoke.detach()
    with pytest.raises(InternalError) as error:
        invoke.create_psylayer_symbol_root_names()
    assert "No Routine or Container node found." in str(error.value)
