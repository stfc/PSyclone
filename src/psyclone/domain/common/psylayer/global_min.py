from psyclone.domain.common.psylayer.global_reduction import GlobalReduction


class GlobalMin(GlobalReduction):
    '''
    Represents a reduction to compute the global minimum value of a scalar.

    '''
    _text_name = "GlobalMin"
