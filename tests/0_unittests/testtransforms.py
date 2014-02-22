# Copyright 2013 STFC, all rights reserved

import abc
class localtransformation(object):
    ''' abstract baseclass for a transformation. Use of abc means it can not be instantiated. '''
    __metaclass__ = abc.ABCMeta
    @abc.abstractmethod
    def name(self):
        return

class testTrans(localtransformation):
    ''' A placeholder test transformation '''
    def __init__(self):
        pass
    def __str__(self):
        return "A test transformation"
    @property
    def name(self):
        return "testTrans"
