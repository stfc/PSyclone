# -----------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
# -----------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab

import os
from generator import generate, GenerationError
from algGen import NoInvokesError
import pytest

BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3")


class TestAlgGenClassDynamo0p3:
    ''' AlgGen class unit tests for the Dynamo0.3 API. We use the
    generate function, as parse and PSyFactory need to be called before
    AlgGen so it is simpler to use this'''

    def test_single_function_invoke(self):
        ''' single kernel specified in an invoke call'''
        alg, _ = generate(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                          api="dynamo0.3")
        gen = str(alg)
        assert "USE psy_single_invoke, ONLY: invoke_0_testkern_type" in gen
        assert "CALL invoke_0_testkern_type(a, f1, f2, m1, m2)" in gen

    def test_single_function_invoke_qr(self):
        ''' single function specified in an invoke call which requires a
        quadrature rule'''
        alg, _ = generate(os.path.join(BASE_PATH, "1.1_single_invoke_qr.f90"),
                          api="dynamo0.3")
        gen = str(alg)
        assert "USE testkern_qr, ONLY: testkern_qr_type" in gen
        assert ("CALL invoke_0_testkern_qr_type(f1, f2, m1, a, m2, istp, qr)"
                in gen)

    def test_multi_function_invoke(self):
        ''' two functions specified in an invoke call'''
        alg, _ = generate(os.path.join(BASE_PATH, "1.2_multi_invoke.f90"),
                          api="dynamo0.3")
        gen = str(alg)
        assert "USE psy_multi_invoke, ONLY: invoke_0" in gen
        assert "CALL invoke_0(a, f1, f2, m1, m2, f3)" in gen

    def test_single_function_multi_invokes(self):
        ''' three invokes, each containing a single function '''
        alg, _ = generate(os.path.join(BASE_PATH, "3_multi_invokes.f90"),
                          api="dynamo0.3")
        gen = str(alg)
        assert "USE testkern, ONLY: testkern_type" in gen
        assert "USE testkern_qr, ONLY: testkern_qr_type" in gen
        assert "CALL invoke_0_testkern_type(a, f1, f2, m1, m2)" in gen
        assert "CALL invoke_2_testkern_type(a, f1, f2, m1, m2)" in gen
        assert ("CALL invoke_1_testkern_qr_type(f1, f2, m1, a, m2, istp, qr)"
                in gen)

    def test_multi_function_multi_invokes(self):
        ''' two invokes, each containing multiple functions '''
        alg, _ = generate(os.path.join(
            BASE_PATH, "3.1_multi_functions_multi_invokes.f90"),
            api="dynamo0.3")
        gen = str(alg)
        print gen
        assert "USE psy_multi_functions_multi_invokes, ONLY: invoke_1" in gen
        assert "USE psy_multi_functions_multi_invokes, ONLY: invoke_0" in gen
        assert "CALL invoke_0(a, f1, f2, m1, m2, istp, qr)" in gen
        assert "CALL invoke_1(f1, f2, m1, a, m2, istp, qr)" in gen

    def test_multi_function_invoke_qr(self):
        '''three functions specified in an invoke call, two of which which
        requires a quadrature rule'''
        alg, _ = generate(os.path.join(
            BASE_PATH, "1.3_multi_invoke_qr.f90"), api="dynamo0.3")
        gen = str(alg)
        print gen
        assert "USE testkern_qr, ONLY: testkern_qr_type" in gen
        assert "USE testkern, ONLY: testkern_type" in gen
        assert "CALL invoke_0(f1, f2, m1, a, m2, istp, m3, f3, qr)" in gen

    def test_invoke_argnames(self):
        ''' invoke call arguments which are arrays '''
        alg, _ = generate(os.path.join(
            BASE_PATH, "5_alg_field_array.f90"), api="dynamo0.3")
        gen = str(alg)
        print gen
        assert "USE psy_single_function, ONLY: invoke_0" in gen
        assert ("CALL invoke_0(f0(1), f1(1, 1), f1(2, index), b(1), "
                "f1(index, index2(index3)), iflag(2), a(index1), "
                "iflag(index2(index3)), qr)" in gen)

    @pytest.mark.xfail(reason="multi qr values not yet supported in psy layer")
    def test_multiple_qr_per_invoke(self):
        ''' invoke functions require different quadrature rules '''
        alg, _ = generate(os.path.join(
            BASE_PATH, "6_multiple_QR_per_invoke.f90"), api="dynamo0.3")
        gen = str(alg)
        assert "USE psy_multi_qr_per_invoke, ONLY: invoke_0" in gen
        assert "CALL invoke_0(f1, f2, f3, f4, f0, qr0, qr1)" in gen

    @pytest.mark.xfail(reason="multi qr values not yet supported in psy layer")
    def test_qr_argnames(self):
        ''' qr call arguments which are arrays '''
        alg, _ = generate(os.path.join(BASE_PATH, "7_QR_field_array.f90"),
                          api="dynamo0.3")
        gen = str(alg)
        assert "USE psy_qr_field_array, ONLY: invoke_0" in gen
        assert ("CALL invoke_0(f1, f2, f3, f4, f0, qr0(i, j), qr0(i, j + 1), "
                "qr1(i, k(l)))" in gen)

    # simple stencil code generation
    def test_single_stencil(self):
        ''' test extent value is passed correctly from the algorithm layer '''
        path = os.path.join(BASE_PATH, "19.1_single_stencil.f90")
        alg, _ = generate(path, api="dynamo0.3")
        output = str(alg)
        assert ("CALL invoke_0_testkern_stencil_type(f1, f2, f3, f4, "
                "f2_extent)" in output)

    # stencil code with wrong number of arguments
    def test_single_stencil_broken(self):
        '''test we raise an exception when we do not pass a stencil argument
        '''
        path = os.path.join(BASE_PATH, "19.2_single_stencil_broken.f90")
        with pytest.raises(GenerationError) as excinfo:
            alg, _ = generate(path, api="dynamo0.3")
        assert "expected '5' arguments in the algorithm layer but found '4'" \
            in str(excinfo.value)

    # single invoke, single field, single stencil of type xory1d
    def test_single_stencil_xory1d(self):
        '''test extent and dimension values are passed correctly from the
        algorithm layer when xory1d is specified'''
        path = os.path.join(BASE_PATH, "19.3_single_stencil_xory1d.f90")
        alg, _ = generate(path, api="dynamo0.3")
        output = str(alg)
        print output
        assert ("CALL invoke_0_testkern_stencil_xory1d_type(f1, f2, "
                "f3, f4, f2_extent, f2_direction)") in output

    # single invoke, single field, single stencil, literal value
    def test_single_stencil_literal(self):
        ''' test extent value is passed correctly from the algorithm layer '''
        path = os.path.join(BASE_PATH, "19.4_single_stencil_literal.f90")
        alg, _ = generate(path, api="dynamo0.3")
        output = str(alg)
        assert "CALL invoke_0_testkern_stencil_type(f1, f2, f3, f4)" \
            in output

    # single invoke, single field, single stencil of type xory1d literal
    def test_single_stencil_xory1d_literal(self):
        '''test dimension value is recognised and not passed if either
        x_direction or y_direction'''
        path = os.path.join(
            BASE_PATH, "19.5_single_stencil_xory1d_literal.f90")
        alg, _ = generate(path, api="dynamo0.3")
        output = str(alg)
        print output
        assert ("CALL invoke_0_testkern_stencil_xory1d_type(f1, f2, "
                "f3, f4)") in output

    # single invoke, single field, single stencil of type xory1d scalar
    def test_single_stencil_xory1d_scalar(self):
        '''test we raise an error if a value is passed for the direction
        argument'''
        path = os.path.join(BASE_PATH, "19.6_single_stencil_xory1d_value.f90")
        with pytest.raises(GenerationError) as excinfo:
            alg, _ = generate(path, api="dynamo0.3")
        assert ("literal is not a valid value for a stencil direction"
                in str(excinfo.value))

    # single kernel, multiple simple stencils
    def test_multiple_stencils(self):
        '''more than one stencil in a kernel'''
        path = os.path.join(BASE_PATH, "19.7_multiple_stencils.f90")
        alg, _ = generate(path, api="dynamo0.3")
        output = str(alg)
        print output
        assert ("CALL invoke_0_testkern_stencil_multi_type(f1, f2, "
                "f3, f4, f2_extent, f3_extent, f3_direction)") in output

    # single kernel, multiple simple stencils same name
    def test_multiple_stencil_same_name(self):
        '''more than one stencil in a kernel with the same name for extent'''
        path = os.path.join(BASE_PATH, "19.8_multiple_stencils_same_name.f90")
        alg, _ = generate(path, api="dynamo0.3")
        output = str(alg)
        print output
        assert ("CALL invoke_0_testkern_stencil_multi_type(f1, f2, "
                "f3, f4, extent, f3_direction)") in output

    # single kernel, multiple stencils same name for direction
    def test_multiple_stencil_same_name_direction(self):
        ''' more than one stencil in a kernel with the same name for direction
        '''
        path = os.path.join(BASE_PATH, "19.9_multiple_stencils_same_name.f90")
        alg, _ = generate(path, api="dynamo0.3")
        output = str(alg)
        print output
        assert ("CALL invoke_0_testkern_stencil_multi_2_type(f1, f2, "
                "f3, f4, extent, direction)") in output

    # multiple kernels in an invoke with stencils
    def test_multiple_kernels_stencils(self):
        '''more than one kernel with stencils'''
        path = os.path.join(BASE_PATH, "19.10_multiple_kernels_stencils.f90")
        alg, _ = generate(path, api="dynamo0.3")
        output = str(alg)
        print output
        assert ("USE psy_multiple_stencil, ONLY: invoke_0") in output
        assert ("CALL invoke_0(f1, f2, f3, f4, f2_extent, f3_extent, extent, "
                "f3_direction, direction)") in output

    # single kernel, multiple stencils same name for direction different case
    def test_multiple_stencil_same_name_case(self):
        '''more than one stencil in a kernel with the same names but different
        case'''
        path = os.path.join(
            BASE_PATH, "19.11_multiple_stencils_mixed_case.f90")
        alg, _ = generate(path, api="dynamo0.3")
        output = str(alg)
        print output
        assert ("CALL invoke_0_testkern_stencil_multi_2_type(f1, f2, "
                "f3, f4, extent, direction)") in output


class TestAlgGenClassDynamo0p1:
    ''' AlgGen class unit tests for the Dynamo0.1 API. We use the
    generate function as parse and PSyFactory need to be called before
    AlgGen so it is simpler to use this'''

    @pytest.mark.xfail(reason="unknown")
    def test_single_invoke_dynamo0p1(self):
        ''' test for correct code transformation for a single function
        specified in an invoke call for the gunghoproto api '''
        alg, _ = generate(
            os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p1", "algorithm",
                         "1_single_function.f90"), api="dynamo0.1")
        gen = str(alg)
        assert "USE psy_single_function, ONLY: invoke_testkern_type" in gen
        assert "CALL invoke_0_testkern_type(f1, f2, m1)" in gen

    def test_zero_invoke_dynamo0p1(self):
        '''test that an exception is raised if the specified file does not
        contain any actual invoke() calls'''
        with pytest.raises(NoInvokesError):
            _, _ = generate(
                os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             "test_files", "dynamo0p1", "missing_invokes.f90"),
                api="dynamo0.1")
