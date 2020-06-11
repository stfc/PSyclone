def trans(psy):
    ''' Python script intended to be passed to PSyclone's generate()
    function via the -s option. Applies OpenCL to the Schedule so
    that PSyclone will generate an OpenCL PSy layer. '''

    from psyclone.psyGen import TransInfo
    tinfo = TransInfo()
    globaltrans = tinfo.get_trans_name('KernelGlobalsToArguments')
    cltrans = tinfo.get_trans_name('OCLTrans')

    for invoke in psy.invokes.invoke_list:
        print("Converting to OpenCL invoke: " + invoke.name)
        schedule = invoke.schedule

        # Skip invoke_2
        if invoke.name == "invoke_2":
            continue

        # Remove the globals from inside each kernel
        for kern in schedule.kernels():
            print("Remove glovals from kernel: " + kern.name)
            # if kern.name == "time_smooth_code":
            #    import pdb; pdb.set_trace()
            globaltrans.apply(kern)

        # Transform invoke to OpenCL
        cltrans.apply(schedule)

    return psy
