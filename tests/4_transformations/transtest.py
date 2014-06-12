# Copyright 2014 STFC, all rights reserved
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
    from parse import parse
    from psyGen import PSy
    from transformations import SwapTrans,LoopFuseTrans
except ImportError:
    print "Error in generator,parse,psyGen or transformations module import. Check your PYTHONPATH"
    raise

class TransformationTestCases(unittest.TestCase):

    def setUp(self):
        pass

    def tearDown(self):
        pass

    def test_trans(self):
        ast,info=parse("3_two_functions_shared_arguments.f90")
        psy=PSy(info)
        invokes=psy.invokes
        invoke=invokes.get("invoke_0")
        schedule=invoke.schedule
        loop1=schedule.children[0]
        loop2=schedule.children[1]
        trans=SwapTrans()
        schedule,memento=trans.apply(loop1,loop2)
        gen=str(psy.gen)
        # testkern1_code call should now be after testkern2_code call
        self.assertTrue(gen.rfind("testkern1_code")>gen.rfind("testkern2_code"))

    def test_loop_fuse_trans(self):
        ast,info=parse("3_two_functions_shared_arguments.f90")
        psy=PSy(info)
        invokes=psy.invokes
        invoke=invokes.get("invoke_0")
        schedule=invoke.schedule
        loop1=schedule.children[0]
        loop2=schedule.children[1]
        trans=LoopFuseTrans()
        schedule,memento=trans.apply(loop1,loop2)
        gen=str(psy.gen)
        for idx,line in enumerate(gen.split('\n')):
            if line.find("DO column=1,topology")!=-1: do_idx=idx
            if line.find("CALL testkern1_code(")!=-1: call1_idx=idx
            if line.find("CALL testkern2_code(")!=-1: call2_idx=idx
            if line.find("END DO")!=-1: enddo_idx=idx
        # 4 lines should be in sequence as calls have been fused into one loop
        self.assertTrue(enddo_idx-call2_idx==1 and call2_idx-call1_idx==1 and call1_idx-do_idx==1 )

if __name__ == '__main__':
    try:
        unittest.main()
    except Exception as e:
        print e
