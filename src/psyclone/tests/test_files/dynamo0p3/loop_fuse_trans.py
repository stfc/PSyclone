'''
    A test module that provides a script to perform loop fusion on the
    first two loops of an invoke called 'invoke_0'. This module does
    not perform any error checking. It is used by the test system to
    ensure that transformation scripts work correctly
'''


def trans(psy):
    ''' a test loop fusion transformation for use with the transformation
    unit tests '''
    from psyclone.transformations import LoopFuseTrans
    invoke = psy.invokes.get("invoke_0")
    schedule = invoke.schedule
    loop1 = schedule.children[3]
    loop2 = schedule.children[4]
    transform = LoopFuseTrans()
    schedule, _ = transform.apply(loop1, loop2)
    invoke.schedule = schedule
    return psy
