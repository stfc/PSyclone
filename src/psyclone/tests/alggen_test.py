# -----------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
# -----------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab

''' Tests for the algorithm generation (re-writing) as implemented
    in algGen.py '''

import os
import pytest
from generator import generate, GenerationError
from algGen import NoInvokesError

BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3")


class TestAlgGenClassDynamo0p3(object):
    ''' AlgGen class unit tests for the Dynamo0.3 API. We use the
    generate function, as parse and PSyFactory need to be called before
    AlgGen so it is simpler to use this'''

    def test_single_function_invoke(self):
        ''' single kernel specified in an invoke call'''
        alg, _ = generate(os.path.join(BASE_PATH, "1_single_invoke.f90"),
                          api="dynamo0.3")
        gen = str(alg)
        assert "USE single_invoke_psy, ONLY: invoke_0_testkern_type" in gen
        assert "CALL invoke_0_testkern_type(a, f1, f2, m1, m2)" in gen

    def test_single_function_named_invoke(self):
        ''' Test that we correctly handle a named invoke call '''
        alg, _ = generate(
            os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3",
                         "1.0.1_single_named_invoke.f90"),
            api="dynamo0.3")
        gen = str(alg)
        print gen
        assert "USE single_invoke_psy, ONLY: invoke_important_invoke" in gen
        assert "CALL invoke_important_invoke(a, f1, f2, m1, m2)" in gen

    def test_invoke_named_invoke(self):
        ''' Test that we correctly handle a named invoke call where the
        naming string already begins with "invoke_" '''
        alg, _ = generate(
            os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3",
                         "1.0.5_invoke_named_invoke.f90"),
            api="dynamo0.3")
        gen = str(alg)
        print gen
        assert "USE single_invoke_psy, ONLY: invoke_important" in gen
        assert "CALL invoke_important(a, f1, f2, m1, m2)" in gen

    def test_multi_kernel_named_invoke(self):
        ''' Test that we correctly handle a named invoke call that contains
        more than one kernel '''
        alg, _ = generate(
            os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3",
                         "4.9_named_multikernel_invokes.f90"),
            api="dynamo0.3")
        gen = str(alg)
        print gen
        assert "USE multikernel_invokes_7_psy, ONLY: invoke_some_name" in gen
        assert (
            "CALL invoke_some_name(a, b, c, istp, rdt, d, ascalar, f, g, e)"
            in gen)

    def test_multi_position_named_invoke(self):
        ''' Test that we correctly handle a named invoke call that contains
        more than one kernel when the name= clause appears at different
        points in the Invoke argument list '''
        alg, _ = generate(
            os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3",
                         "4.10_multi_position_named_invokes.f90"),
            api="dynamo0.3")
        gen = str(alg)
        print gen
        assert "USE multikernel_invokes_7_psy, ONLY: invoke_name_first" in gen
        assert "USE multikernel_invokes_7_psy, ONLY: invoke_name_middle" in gen
        assert "USE multikernel_invokes_7_psy, ONLY: invoke_name_last" in gen
        assert (
            "CALL invoke_name_first(a, b, c, istp, rdt, d, ascalar, f, g, e)"
            in gen)
        assert (
            "CALL invoke_name_middle(a, b, c, istp, rdt, d, ascalar, f, g, e)"
            in gen)
        assert (
            "CALL invoke_name_last(a, b, c, istp, rdt, d, ascalar, f, g, e)"
            in gen)

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
        assert "USE multi_invoke_psy, ONLY: invoke_0" in gen
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

    def test_named_multi_invokes(self):
        ''' Check that we generate correct code when we have more than one
        named invoke in an Algorithm file '''
        alg, _ = generate(
            os.path.join(BASE_PATH,
                         "3.2_multi_functions_multi_named_invokes.f90"),
            api="dynamo0.3")
        gen = str(alg)
        assert "USE testkern, ONLY: testkern_type" in gen
        assert "USE testkern_qr, ONLY: testkern_qr_type" in gen
        assert ("USE multi_functions_multi_invokes_psy, ONLY: "
                "invoke_my_first" in gen)
        assert ("USE multi_functions_multi_invokes_psy, ONLY: "
                "invoke_my_second" in gen)
        assert "CALL invoke_my_first(a, f1, f2," in gen
        assert "CALL invoke_my_second(f1, f2, m1, a, m2" in gen

    def test_multi_function_multi_invokes(self):
        ''' two invokes, each containing multiple functions '''
        alg, _ = generate(
            os.path.join(BASE_PATH, "3.1_multi_functions_multi_invokes.f90"),
            api="dynamo0.3")
        gen = str(alg)
        print gen
        assert "USE multi_functions_multi_invokes_psy, ONLY: invoke_1" in gen
        assert "USE multi_functions_multi_invokes_psy, ONLY: invoke_0" in gen
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
        assert "USE single_function_psy, ONLY: invoke_0" in gen
        assert ("CALL invoke_0(f0(1), f1(1, 1), f1(2, index), b(1), "
                "f1(index, index2(index3)), iflag(2), a(index1), "
                "iflag(index2(index3)), qr)" in gen)

    @pytest.mark.xfail(reason="multi qr values not yet supported in psy layer")
    def test_multiple_qr_per_invoke(self):
        ''' invoke functions require different quadrature rules '''
        alg, _ = generate(os.path.join(
            BASE_PATH, "6_multiple_QR_per_invoke.f90"), api="dynamo0.3")
        gen = str(alg)
        assert "USE multi_qr_per_invoke_psy, ONLY: invoke_0" in gen
        assert "CALL invoke_0(f1, f2, f3, f4, f0, qr0, qr1)" in gen

    @pytest.mark.xfail(reason="multi qr values not yet supported in psy layer")
    def test_qr_argnames(self):
        ''' qr call arguments which are arrays '''
        alg, _ = generate(os.path.join(BASE_PATH, "7_QR_field_array.f90"),
                          api="dynamo0.3")
        gen = str(alg)
        assert "USE qr_field_array_psy, ONLY: invoke_0" in gen
        assert ("CALL invoke_0(f1, f2, f3, f4, f0, qr0(i, j), qr0(i, j + 1), "
                "qr1(i, k(l)))" in gen)

    def test_deref_derived_type_args(self):
        ''' Test the case where a kernel argument is specified as both a
        component of a derived type and as the result of a call to a
        type-bound procedure '''
        alg, _ = generate(
            os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3",
                         "1.6.2_single_invoke_1_int_from_derived_type.f90"),
            api="dynamo0.3")
        gen = str(alg)
        print gen
        assert (
            "CALL invoke_0(f1, my_obj%iflag, f2, m1, m2, my_obj%get_flag(), "
            "my_obj%get_flag(switch), my_obj%get_flag(int_wrapper%data))"
            in gen)

    def test_multi_deref_derived_type_args(self):
        ''' Test the case where a given kernel argument is specified using
        different derived types in the same invoke. '''
        alg, _ = generate(
            os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3",
                         "1.6.3_single_invoke_multiple_derived_types.f90"),
            api="dynamo0.3")
        gen = str(alg)
        print gen
        assert (
            "CALL invoke_0(f1, obj_a%iflag, f2, m1, m2, obj_b%iflag, "
            "obj_a%obj_b, obj_b%obj_a)"
            in gen)

    def test_op_and_scalar_and_qr_derived_type_args(self):
        ''' Test the case where the operator, scalar and qr arguments to a
        kernel are all supplied by de-referencing derived types '''
        alg, _ = generate(
            os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3",
                         "10.6.1_operator_no_field_scalar_deref.f90"),
            api="dynamo0.3")
        gen = str(alg)
        print gen
        assert (
            "CALL invoke_0_testkern_operator_nofield_scalar_type("
            "opbox%my_mapping, box%b(1), qr%get_instance(qr3, 9, 3))" in gen)

    def test_vector_field_arg_deref(self):
        ''' Test that we generate a correct invoke call when a kernel
        argument representing a field vector is obtained by de-referencing a
        derived type '''
        alg, _ = generate(
            os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p3",
                         "8.1_vector_field_deref.f90"),
            api="dynamo0.3")
        gen = str(alg)
        print gen
        assert "CALL invoke_0_testkern_chi_type(f1, box%chi)" in gen

    def test_single_stencil(self):
        ''' test extent value is passed correctly from the algorithm layer '''
        path = os.path.join(BASE_PATH, "19.1_single_stencil.f90")
        alg, _ = generate(path, api="dynamo0.3")
        output = str(alg)
        assert ("CALL invoke_0_testkern_stencil_type(f1, f2, f3, f4, "
                "f2_extent)" in output)

    def test_single_stencil_broken(self):
        '''test we raise an exception when we do not pass a stencil argument
        '''
        path = os.path.join(BASE_PATH, "19.2_single_stencil_broken.f90")
        with pytest.raises(GenerationError) as excinfo:
            _, _ = generate(path, api="dynamo0.3")
        assert "expected '5' arguments in the algorithm layer but found '4'" \
            in str(excinfo.value)

    def test_single_stencil_xory1d(self):
        '''test extent and dimension values are passed correctly from the
        algorithm layer when xory1d is specified'''
        path = os.path.join(BASE_PATH, "19.3_single_stencil_xory1d.f90")
        alg, _ = generate(path, api="dynamo0.3")
        output = str(alg)
        print output
        assert ("CALL invoke_0_testkern_stencil_xory1d_type(f1, f2, "
                "f3, f4, f2_extent, f2_direction)") in output

    def test_single_stencil_literal(self):
        ''' test extent value is passed correctly from the algorithm layer '''
        path = os.path.join(BASE_PATH, "19.4_single_stencil_literal.f90")
        alg, _ = generate(path, api="dynamo0.3")
        output = str(alg)
        assert "CALL invoke_0_testkern_stencil_type(f1, f2, f3, f4)" \
            in output

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

    def test_multiple_stencils(self):
        '''more than one stencil in a kernel'''
        path = os.path.join(BASE_PATH, "19.7_multiple_stencils.f90")
        alg, _ = generate(path, api="dynamo0.3")
        output = str(alg)
        print output
        assert ("CALL invoke_0_testkern_stencil_multi_type(f1, f2, "
                "f3, f4, f2_extent, f3_extent, f3_direction)") in output

    def test_multiple_stencil_same_name_direction(self):
        ''' more than one stencil in a kernel with the same name for direction
        '''
        path = os.path.join(BASE_PATH, "19.9_multiple_stencils_same_name.f90")
        alg, _ = generate(path, api="dynamo0.3")
        output = str(alg)
        print output
        assert ("CALL invoke_0_testkern_stencil_multi_2_type(f1, f2, "
                "f3, f4, extent, direction)") in output

    def test_multiple_kernels_stencils(self):
        '''more than one kernel with stencils'''
        path = os.path.join(BASE_PATH, "19.10_multiple_kernels_stencils.f90")
        alg, _ = generate(path, api="dynamo0.3")
        output = str(alg)
        print output
        assert "USE multiple_stencil_psy, ONLY: invoke_0" in output
        assert ("CALL invoke_0(f1, f2, f3, f4, f2_extent, f3_extent, extent, "
                "f3_direction, direction)") in output

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

    def test_single_stencil_xory1d_scalar(self):
        '''test we raise an error if a value is passed for the direction
        argument'''
        path = os.path.join(BASE_PATH, "19.6_single_stencil_xory1d_value.f90")
        with pytest.raises(GenerationError) as excinfo:
            _, _ = generate(path, api="dynamo0.3")
        assert ("literal is not a valid value for a stencil direction"
                in str(excinfo.value))

    def test_multiple_stencil_same_name(self):
        '''more than one stencil in a kernel with the same name for extent'''
        path = os.path.join(BASE_PATH, "19.8_multiple_stencils_same_name.f90")
        alg, _ = generate(path, api="dynamo0.3")
        output = str(alg)
        print output
        assert ("CALL invoke_0_testkern_stencil_multi_type(f1, f2, "
                "f3, f4, extent, f3_direction)") in output


class TestAlgGenClassDynamo0p1(object):
    ''' AlgGen class unit tests for the Dynamo0.1 API. We use the
    generate function as parse and PSyFactory need to be called before
    AlgGen so it is simpler to use this'''

    @pytest.mark.xfail(reason="unknown")
    def test_single_invoke_dynamo0p1(self):
        ''' test for correct code transformation for a single function
        specified in an invoke call for the dynamo0.1 api '''
        alg, _ = generate(
            os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files", "dynamo0p1", "algorithm",
                         "1_single_function.f90"), api="dynamo0.1")
        gen = str(alg)
        assert "USE single_function_psy, ONLY: invoke_testkern_type" in gen
        assert "CALL invoke_0_testkern_type(f1, f2, m1)" in gen

    def test_zero_invoke_dynamo0p1(self):
        ''' Test that an exception is raised if the specified file does
        not contain any actual invoke() calls '''
        with pytest.raises(NoInvokesError):
            _, _ = generate(
                os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             "test_files", "dynamo0p1", "missing_invokes.f90"),
                api="dynamo0.1")
