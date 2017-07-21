#-------------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
#-------------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab

import abc
class LocalTransformation(object):
    ''' abstract baseclass for a transformation. Use of abc means it can not be instantiated. '''
    __metaclass__ = abc.ABCMeta
    @abc.abstractmethod
    def name(self):
        return

class TestTrans(LocalTransformation):
    ''' A placeholder test transformation '''
    def __init__(self):
        pass
    def __str__(self):
        return "A test transformation"
    @property
    def name(self):
        return "testTrans"
