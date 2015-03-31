#-------------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
#-------------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab

import os
from generator import generate
import pytest

class TestAlgGenClassGungHoProto:
    ''' AlgGen class unit tests for the GungHoProto API. Tests for
    correct code transformation. We use the generate function as parse
    and PSyFactory need to be called before AlgGen so it is simpler to
    use the generate function'''

    @pytest.mark.xfail(reason="unknown")
    def test_single_invoke(self):
        ''' single function specified in an invoke call'''
        alg,psy=generate(os.path.join(os.path.dirname(os.path.abspath(__file__)),"test_files","gunghoproto","1_single_function.f90"), api = "gunghoproto")
        assert (str(alg).find("USE psy_single_function, ONLY: invoke_testkern_type")!=-1 and \
                  str(alg).find("CALL invoke_testkern_type(f1, f2, m1)")!=-1)

    @pytest.mark.xfail(reason="unknown")
    def test_multi_invoke(self):
        ''' two functions specified in an invoke call'''
        alg,psy=generate(os.path.join(os.path.dirname(os.path.abspath(__file__)),"test_files","gunghoproto","3_two_functions_shared_arguments.f90"), api = "gunghoproto")
        assert (str(alg).find("USE psy_two_functions_shared_arguments, ONLY: invoke_0")!=-1 and \
                str(alg).find("CALL invoke_0(f1, f2, m1, m2, f3, m3)")!=-1)

    @pytest.mark.xfail(reason="bug 1")
    def test_multi_invoke_named(self):
        ''' two functions specified in an invoke call and the
        invocation is named'''
        alg,psy=generate(os.path.join(os.path.dirname(os.path.abspath(__file__)),"test_files","gunghoproto","4_explicit_name.f90"), api = "gunghoproto")
        assert (str(alg).find("USE psy, ONLY: invoke_multikern_kern")!=-1 and \
                str(alg).find("CALL invoke_multikern_kern(f1, f2, f3, m1, m3, m2)")!=-1)

    @pytest.mark.xfail(reason="unknown")
    def test_multi_single_invoke(self):
        ''' multiple invoke's (2 of) each with a single function'''
        alg,psy=generate(os.path.join(os.path.dirname(os.path.abspath(__file__)),"test_files","gunghoproto","5_two_single-function_invokes.f90"), api = "gunghoproto")
        self.assertTrue(str(alg).find("USE psy, ONLY: invoke_testkern1_kern") and \
                        str(alg).find("USE psy, ONLY: invoke_testkern2_kern") and \
                        str(alg).find("CALL invoke_testkern1_kern(f1, f2, m1, m2)") and \
                        str(alg).find("CALL invoke_testkern2_kern(f1, f3, m1, m3)"))

    @pytest.mark.xfail(reason="unknown")
    def test_other_calls_invoke(self):
        ''' other calls in the algorithm layer '''
        alg,psy=generate(os.path.join(os.path.dirname(os.path.abspath(__file__)),"test_files","gunghoproto","6_other_calls.f90"), api = "gunghoproto")
        assert (str(alg).find("USE psy_other_calls, ONLY: invoke_testkern_type")!=-1 and \
                str(alg).find("CALL invoke_testkern_type(f1, f2, m1)")!=-1)

    def test_single_set(self):
        ''' single set infrastructure routine specified in an invoke call '''
        alg,psy=generate(os.path.join(os.path.dirname(os.path.abspath(__file__)),"test_files","gunghoproto","1_single_set.f90"), api = "gunghoproto")
        assert (str(alg).find("CALL invoke_0(one)")!=-1 and \
                str(psy).find("one%data = 1.0")!=-1)

    @pytest.mark.xfail(reason="unknown")
    def test_mixed_kernel_and_set(self):
        ''' single set infrastructure routine and single kernel
        routine specified in an invoke call '''
        alg,psy=generate(os.path.join(os.path.dirname(os.path.abspath(__file__)),"test_files","gunghoproto","2_mixed_kernel_and_set.f90"), api = "gunghoproto")
        assert (str(alg).find("CALL invoke_0(one, f2, f3)")!=-1 and \
                str(psy).find("one%data = 1.0")!=-1)

    def test_multiple_set(self):
        ''' two set infrastructure routines specified in an invoke call '''
        alg,psy=generate(os.path.join(os.path.dirname(os.path.abspath(__file__)),"test_files","gunghoproto","3_multiple_set.f90"), api = "gunghoproto")
        assert (str(alg).find("CALL invoke_0(one, two)")!=-1 and \
                str(psy).find("one%data = 1.0")!=-1 and \
                str(psy).find("two%data = 2.0")!=-1)

class TestAlgGenClassDynamo0p1:
    ''' AlgGen class unit tests for the Dynamo0.1 API. We use the
    generate function as parse and PSyFactory need to be called before
    AlgGen so it is simpler to use this'''

    @pytest.mark.xfail(reason="unknown")
    def test_single_invoke_dynamo0p1(self):
        ''' test for correct code transformation for a single function specified in an invoke call for the
        gunghoproto api '''
        alg,psy=generate(os.path.join(os.path.dirname(os.path.abspath(__file__)),"test_files","dynamo0p1","algorithm","1_single_function.f90"),api="dynamo0.1")
        assert (str(alg).find("USE psy_single_function, ONLY: invoke_testkern_type")!=-1 and \
                  str(alg).find("CALL invoke_testkern_type(f1, f2, m1)")!=-1)
