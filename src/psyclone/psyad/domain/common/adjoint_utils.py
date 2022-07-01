#: The prefix we will prepend to a routine, container and metadata
#: names when generating the adjoint. If the original name contains
#: the tl prefix, then this is removed.
ADJOINT_NAME_PREFIX = "adj_"
TL_NAME_PREFIX = "tl_"


def create_adjoint_name(tl_name):
    '''Create an adjoint name from the supplied tangent linear name. This
    is done by stripping the TL_NAME_PREFIX from the name if it exists
    and then adding the ADJOINT_NAME_PREFIX. The adjoint name is also
    lower-cased.

    :param str: the tangent-linear name.

    :returns: the adjoint name.
    :rtype: str

    '''
    adj_name = tl_name.lower()
    if adj_name.startswith(TL_NAME_PREFIX):
        adj_name = adj_name[len(TL_NAME_PREFIX):]
    return ADJOINT_NAME_PREFIX + adj_name


__all__ = ["create_adjoint_name"]
