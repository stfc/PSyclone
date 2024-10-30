'''
    A test module containing valid python but not a trans() function
    which is required for transformation scripts. Therefore an error
    should be produced by PSyclone if this file is referenced as a
    transformation script.
'''


def nottrans(psyir):
    ''' first invalid name examples for a trans function '''


def tran(psyir):
    ''' second invalid name example for a trans function '''
