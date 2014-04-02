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

    def test_non_existant_file(self):
        with self.assertRaises(IOError):
            generate("nofile.f90")

    def test_single_invoke(self):
        # single function specified in an invoke call
        alg,psy=generate("1_single_function.f90")
        self.assertTrue(str(alg).find("USE psy_single_function, ONLY: invoke_testkern_type")!=-1 and \
                        str(alg).find("CALL invoke_testkern_type(f1, f2, m1)")!=-1)

    def test_single_invoke_undeclared(self):
        #with self.assertRaises(AssertionError):
        with self.assertRaises(ParseError):
            generate("2_undeclared_function.f90")
    
    def test_multi_invoke(self):
        # two functions specified in an invoke call
        alg,psy=generate("3_two_functions_shared_arguments.f90")
        self.assertTrue(str(alg).find("USE psy_two_functions_shared_arguments, ONLY: invoke_0")!=-1 and \
                        str(alg).find("CALL invoke_0(f1, f2, m1, m2, f3, m3)")!=-1)

    @unittest.skip("names not yet supported in parser")
    def test_multi_invoke_named(self):
        # two functions specified in an invoke call and the invocation is named
        alg,psy=generate("4_explicit_name.f90")
        self.assertTrue(str(alg).find("USE psy, ONLY: invoke_multikern_kern")!=-1 and \
                        str(alg).find("CALL invoke_multikern_kern(f1, f2, f3, m1, m3, m2)")!=-1)

    def test_multi_single_invoke(self):
        # multiple invoke's (2 of) each with a single function
        alg,psy=generate("5_two_single-function_invokes.f90")
        self.assertTrue(str(alg).find("USE psy, ONLY: invoke_testkern1_kern") and \
                        str(alg).find("USE psy, ONLY: invoke_testkern2_kern") and \
                        str(alg).find("CALL invoke_testkern1_kern(f1, f2, m1, m2)") and \
                        str(alg).find("CALL invoke_testkern2_kern(f1, f3, m1, m3)"))

    def test_other_calls_invoke(self):
        # other calls in the algorithm layer
        alg,psy=generate("6_other_calls.f90")
        self.assertTrue(str(alg).find("USE psy_other_calls, ONLY: invoke_testkern_type")!=-1 and \
                        str(alg).find("CALL invoke_testkern_type(f1, f2, m1)")!=-1)

if __name__ == '__main__':
    try:
        unittest.main()
    except Exception as e:
        print e
    
