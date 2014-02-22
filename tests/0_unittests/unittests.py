# Copyright 2013 STFC, all rights reserved
import sys
version_info=sys.version_info
if version_info[0]>2 or (version_info[0]==2 and version_info[1]>=7):
    #unittest features we want exist from python 2.7
    try:
        import unittest
    except ImportError:
        print "Error, can not find unittest"
        exit(1)
elif version_info[0]==2 and version_info[1]>=4:
    #unittest2 is a backport of unittest for python 2.4 to 2.7
    try:
        import unittest2 as unittest
    except ImportError:
        print "Error, can not find unittest2. unittest2 is used instead of unittest as you are running a python version between 2.4 and 2.7. Please either install unittest2 or update your python to 2.7+"
        exit(1)
else:
    print "Error, you need at least python 2.3 to run unittest, or it's backport unittest2."
    exit(1)

try:
    from optimise import transformations, transformation, GenerationError
except ImportError:
    print "Error in import of transformations from optimise. Check your PYTHONPATH"
    raise

class TransformationsClassTests(unittest.TestCase):

    def setUp(self):
        pass

    def tearDown(self):
        pass

    def test_new_module(self):
        import testtransforms
        t=transformations(module=testtransforms)
        self.assertTrue(t.numTrans==0)

    def test_new_baseclass(self):
        from testtransforms import localtransformation
        t=transformations(baseclass=localtransformation)
        self.assertTrue(t.numTrans==0)

    def test_new_module_and_baseclass(self):
        import testtransforms
        t=transformations(module=testtransforms,baseclass=testtransforms.localtransformation)
        self.assertTrue(t.numTrans==1)

    def test_list_valid_return_object(self):
        t=transformations()
        self.assertTrue(isinstance(t.list,str))

    def test_list_return_data(self):
        t=transformations()
        self.assertTrue(t.list.find("available")!=-1)

    def test_invalid_low_number(self):
        t=transformations()
        with self.assertRaises(GenerationError):
            transform=t.getTransNum(0)

    def test_invalid_high_number(self):
        t=transformations()
        with self.assertRaises(GenerationError):
            transform=t.getTransNum(999)

    def test_valid_return_object_from_number(self):
        t=transformations()
        transform=t.getTransNum(1)
        self.assertTrue(isinstance(transform,transformation))

    def test_invalid_name(self):
        t=transformations()
        with self.assertRaises(GenerationError):
            transform=t.getTransName("fail")

    def test_valid_return_object_from_name(self):
        t=transformations()
        transform=t.getTransName("testTrans")
        self.assertTrue(isinstance(transform,transformation))

class TransformationClassTests(unittest.TestCase):

    def setUp(self):
        pass

    def tearDown(self):
        pass

    def test_base_class_not_callable(self):
        with self.assertRaises(TypeError):        
            t=transformation()

if __name__ == '__main__':
    try:
        unittest.main()
    except Exception as e:
        print e
