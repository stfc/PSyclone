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
    from generator import generate,GenerationError,ParseError
except ImportError:
    print "Error in generator module import. Check your PYTHONPATH"
    raise

class AlgGeneratorTestCases(unittest.TestCase):

    def setUp(self):
        pass

    def tearDown(self):
        pass

    def test_single_set(self):
        # single set infrastructure routine specified in an invoke call
        alg,psy=generate("1_single_set.f90")
        self.assertTrue(str(alg).find("CALL invoke_0(one)")!=-1 and \
                        str(psy).find("one%data = 1.0")!=-1)

    def test_mixed_kernel_and_set(self):
        # single set infrastructure routine and single kernel routine specified in an invoke call
        alg,psy=generate("2_mixed_kernel_and_set.f90")
        print alg
        print psy
        self.assertTrue(str(alg).find("CALL invoke_0(one, f2, f3)")!=-1 and \
                        str(psy).find("one%data = 1.0")!=-1)

    def test_multiple_set(self):
        # two set infrastructure routines specified in an invoke call
        alg,psy=generate("3_multiple_set.f90")
        self.assertTrue(str(alg).find("CALL invoke_0(one, two)")!=-1 and \
                        str(psy).find("one%data = 1.0")!=-1 and \
                        str(psy).find("two%data = 2.0")!=-1)

if __name__ == '__main__':
    try:
        unittest.main()
    except Exception as e:
        print e
    
