from psyclone.psyir.nodes import (
    Allocate, Literal, IntrinsicCall, Reference)
from psyclone.psyir.symbols import (ArrayType, DataSymbol, INTEGER_TYPE,
                                    IntrinsicSymbol)


#def test_allocate_init():
#    '''Test that an Allocate can be created as expected.'''
#    alloc = IntrinsicCall(IntrinsicCall.Intrinsic.ALLOCATE)
#    assert isinstance(alloc.routine, IntrinsicSymbol)
#    assert str(alloc) == "IntrinsicCall[name='ALLOCATE']"
#    assert alloc.routine.name.lower() == "allocate"
#    assert alloc.parent is None
#    assert alloc.children == []


def test_intrinsiccall_create():
    ''' '''
    sym = DataSymbol("my_array", ArrayType(INTEGER_TYPE,
                                           [ArrayType.Extent.DEFERRED]))
    alloc = IntrinsicCall.create(IntrinsicCall.Intrinsic.ALLOCATE,
                                 [Reference(sym)])
