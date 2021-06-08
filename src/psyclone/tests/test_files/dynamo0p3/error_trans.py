'''
    A test module containing valid python but the trans() function
    raises an attribute error. This was previously used to catch
    whether a trans() function exists or not.
'''

from __future__ import absolute_import
from psyclone.psyGen import Loop
from psyclone.transformations import ColourTrans


def trans(psy):
    ''' a valid trans function which produces an attribute error as
    we have mistyped apply()'''
    ctrans = ColourTrans()
    schedule = psy.invokes.get("invoke_0_testkern_type").schedule
    for child in schedule.children:
        if isinstance(child, Loop) and child.field_space != "w3":
            # The no-member issue below is intentional
            # pylint: disable=no-member
            ctrans.appy(child)
    return psy
