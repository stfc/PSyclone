'''
    A test module containing valid python but the trans() function
    raises an attribute error. This was previously used to catch
    whether a trans() function exists or not.
'''


def trans(psy):
    ''' a valid trans function which produces an attribute error as
    we have mistyped apply()'''
    from psyGen import Loop
    from transformations import ColourTrans
    ctrans = ColourTrans()
    schedule = psy.invokes.get("invoke_0_testkern_type").schedule
    for child in schedule.children:
        if isinstance(child, Loop) and child.field_space != "w3":
            cschedule, _ = ctrans.appy(child)
    return psy
