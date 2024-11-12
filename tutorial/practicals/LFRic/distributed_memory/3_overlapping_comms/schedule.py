'''A PSyclone transformation script that outputs a textual
representation of the PSyIR representing the PSy-layer for the first
invoke found in the algorithm layer code.

'''


def trans(psyir):
    '''Output a textual view of the PSyIR representing the PSy-layer for
    the first invoke found in the algorithm layer code.

    :param psyir: the PSyIR of the PSy-layer.
    :type psyir: :py:class:`psyclone.psyir.nodes.FileContainer`

    '''
    # Get the subroutine of the first PSy-layer invoke
    first_invoke_subroutine = psyir.children[0].children[0]
    # Take a look at the PSy-layer PSyIR
    print(first_invoke_subroutine.view())
