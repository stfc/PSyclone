
from psyclone.domain.common.psylayer.global_reduction import GlobalReduction


class GlobalSum(GlobalReduction):
    '''
    Generic GlobalSum class which can be added to a Schedule.

    '''
    _text_name = "GlobalSum"
