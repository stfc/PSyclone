def trans(psy):
    '''sample transformation script to demonstrate the use of asynchronous
    halo exchanges with overlapping compute and communication for the
    most costly halo exchanges in the (current version of the) model

    '''
    from psyclone.transformations import \
        Dynamo0p3RedundantComputationTrans, \
        Dynamo0p3AsyncHaloExchangeTrans, \
        MoveTrans

    schedule = psy.invokes.invoke_list[0].schedule
    schedule.view()

    # This transformation removes the halo exchange associated with
    # the grad_p field. This transformation is not be needed if
    # annexed_dofs is set to True in the config file.
    rc_trans = Dynamo0p3RedundantComputationTrans()
    rc_trans.apply(schedule.children[0], depth=1)
    schedule.view()

    # This transformation splits the three synchronous halo exchanges
    # (for fields p, hb_inv and u_normalisation) into asynchronous
    # (halo_exchange_start and halo_exchange_end) ones.
    ahex_trans = Dynamo0p3AsyncHaloExchangeTrans()
    for index in [3, 2, 1]:
        schedule, _ = ahex_trans.apply(schedule.children[index])
    schedule.view()

    # This transformation moves the start of the three halo exchanges
    # before the setval_c loop offering the potential for overlap
    # between communication and computation.
    mtrans = MoveTrans()
    for index in [5, 4, 3]:
        schedule, _ = mtrans.apply(schedule.children[index],
                                   schedule.children[0])
    schedule.view()

    return psy
