'''
    A test module containing valid python but the trans() function
    raises an attribute error. This was previously used to catch
    whether a trans() function exists or not.
'''

from psyclone.psyGen import Loop
from psyclone.transformations import ColourTrans


def trans(psyir):
    ''' a valid trans function which produces an attribute error as
    we have mistyped apply()'''
    ctrans = ColourTrans()
    for child in psyir.walk(Loop):
        if isinstance(child, Loop) and child.field_space != "w3":
            # The no-member issue below is intentional
            # pylint: disable=no-member
            ctrans.appy(child)
