from psyclone.domain.common.psylayer.global_reduction import (
    GlobalReduction, ReductionOp)

'''
Module containing pytest tests for the GlobalReduction class.
'''

def test_globalsum_node_str():
    '''test the node_str method in the GlobalSum class. The simplest way
    to do this is to use an LFRic builtin example which contains a
    scalar and then call node_str() on that.

    '''
    gred = GlobalReduction(ReductionOp.SUM,
                           scalar="a")
    output = str(gred)
    expected_output = (colored("GlobalSum", GlobalSum._colour) +
                       "[scalar='asum']")
    assert expected_output in output


def test_globalsum_children_validation():
    '''Test that children added to GlobalSum are validated. A GlobalSum node
    does not accept any children.

    '''
    _, invoke_info = parse(os.path.join(BASE_PATH,
                                        "15.9.1_X_innerproduct_Y_builtin.f90"),
                           api="lfric")
    psy = PSyFactory("lfric", distributed_memory=True).create(invoke_info)
    gsum = None
    for child in psy.invokes.invoke_list[0].schedule.children:
        if isinstance(child, LFRicGlobalSum):
            gsum = child
            break
    with pytest.raises(GenerationError) as excinfo:
        gsum.addchild(Literal("2", INTEGER_TYPE))
    assert ("Item 'Literal' can't be child 0 of 'GlobalSum'. GlobalSum is a"
            " LeafNode and doesn't accept children.") in str(excinfo.value)


