'''
    A test module containing valid python but not a trans() function
    which is required for transformation scripts. Therefore an error
    should be produced by PSyclone if this file is referenced as a
    transformation script.
'''


def nottrans(psy):
    ''' first invalid name examples for a trans function '''
    return psy


def tran(psy):
    ''' second invalid name example for a trans function '''
    return psy
