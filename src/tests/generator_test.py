# -----------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
# -----------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab

'''
    A module to perform pytest unit and functional tests on the generator
    function.
'''

from generator import generate, GenerationError
import pytest
import os


def delete_module(modname):
    '''a function to remove a module from Python's internal modules
       list. This is useful as some tests affect others by importing
       modules.'''
    from sys import modules
    del modules[modname]
    for mod in modules.values():
        try:
            delattr(mod, modname)
        except AttributeError:
            pass

# a set of unit tests for the generate function


def test_non_existant_filename():
    ''' checks that algGen raises appropriate error when a
    non-existant filename is supplied '''
    with pytest.raises(IOError):
        generate("non_existant_file.f90")


def test_invalid_api():
    ''' checks that algGen raises appropriate error when an invalid
        api is supplied '''
    with pytest.raises(GenerationError):
        generate(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                              "test_files", "dynamo0p1", "algorithm",
                              "1_single_function.f90"), api="invalid")


def test_invalid_kernel_path():
    ''' checks that algGen raises appropriate error when an invalid
        search path for kernel source files is supplied '''
    with pytest.raises(IOError):
        generate(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                              "test_files", "dynamo0p1", "algorithm",
                              "1_single_function.f90"),
                 api="dynamo0.1",
                 kernel_path="does_not_exist")


def test_wrong_kernel_path():
    ''' checks that algGen raises appropriate error when the kernel
        code cannot be found in the specified search path '''
    root_path = os.path.dirname(os.path.abspath(__file__))
    with pytest.raises(IOError):
        generate(os.path.join(root_path,
                              "test_files", "dynamo0p1", "algorithm",
                              "1_single_function.f90"),
                 api="dynamo0.1",
                 kernel_path=os.path.join(root_path,
                                          "test_files", "gocean0p1"))


def test_correct_kernel_path():
    ''' checks that algGen succeeds when the location of the kernel
        source code is *not* the same as that of the algorithm code '''
    root_path = os.path.dirname(os.path.abspath(__file__))
    _, _ = generate(os.path.join(root_path,
                                 "test_files", "dynamo0p1", "algorithm",
                                 "1_single_function.f90"),
                    api="dynamo0.1",
                    kernel_path=os.path.join(root_path, "test_files",
                                             "dynamo0p1", "kernels"))


def test_same_kernel_path():
    ''' checks that the generator succeeds when the search directory
        is the same as the algorithm code directory and a path is
        specified '''
    path = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                        "test_files", "dynamo0p1", "algorithm")
    _, _ = generate(os.path.join(path, "1_single_function.f90"),
                    api="dynamo0.1", kernel_path=path)


def test_similar_kernel_name():
    ''' checks that the generator does not match incorrect files '''
    root_path = os.path.dirname(os.path.abspath(__file__))
    _, _ = generate(os.path.join(root_path, "test_files", "dynamo0p1",
                                 "algorithm", "1_single_function.f90"),
                    api="dynamo0.1",
                    kernel_path=os.path.join(root_path, "test_files",
                                             "dynamo0p1", "kernels2"))


def test_recurse_correct_kernel_path():
    '''checks that the generator succeeds when the location of the kernel
       source code is *not* the same as that of the algorithm code and
       recursion through subdirectories is required'''
    root_path = os.path.dirname(os.path.abspath(__file__))
    _, _ = generate(os.path.join(root_path, "test_files", "dynamo0p1",
                                 "algorithm", "1_single_function.f90"),
                    api="dynamo0.1",
                    kernel_path=os.path.join(root_path, "test_files",
                                             "dynamo0p1", "kernels3"))


def test_script_file_not_found():
    ''' checks that generator.py raises an appropriate error when a
        script file is supplied that can't be found in the Python path.
        In this case the script path is supplied'''
    root_path = os.path.dirname(os.path.abspath(__file__))
    with pytest.raises(IOError):
        _, _ = generate(os.path.join(root_path, "test_files", "dynamo0p3",
                                     "1_single_invoke.f90"),
                        api="dynamo0.3", script_name="./non_existant.py")


def test_script_file_not_found_relative():
    ''' checks that generator.py raises an appropriate error when a script
        file is supplied that can't be found in the Python path. In
        this case the script path is not supplied so must be found via the
        PYTHONPATH variable'''
    root_path = os.path.dirname(os.path.abspath(__file__))
    with pytest.raises(GenerationError):
        _, _ = generate(os.path.join(root_path, "test_files", "dynamo0p3",
                                     "1_single_invoke.f90"),
                        api="dynamo0.3", script_name="non_existant.py")


def test_script_file_too_short():
    ''' checks that generator.py raises an appropriate error when a
        script file name is too short to contain the '.py' extension'''
    root_path = os.path.dirname(os.path.abspath(__file__))
    with pytest.raises(GenerationError):
        _, _ = generate(os.path.join(root_path, "test_files", "dynamo0p3",
                                     "1_single_invoke.f90"),
                        api="dynamo0.3",
                        script_name=os.path.join(root_path, "test_files",
                                                 "dynamo0p3", "xyz"))


def test_script_file_no_extension():
    ''' checks that generator.py raises an appropriate error when a
        script file does not have an extension'''
    root_path = os.path.dirname(os.path.abspath(__file__))
    with pytest.raises(GenerationError):
        _, _ = generate(os.path.join(root_path, "test_files", "dynamo0p3",
                                     "1_single_invoke.f90"),
                        api="dynamo0.3",
                        script_name=os.path.join(root_path, "test_files",
                                                 "dynamo0p3",
                                                 "invalid_script_name"))


def test_script_file_wrong_extension():
    ''' checks that generator.py raises an appropriate error when a
        script file does not have the '.py' extension'''
    root_path = os.path.dirname(os.path.abspath(__file__))
    with pytest.raises(GenerationError):
        _, _ = generate(os.path.join(root_path, "test_files", "dynamo0p3",
                                     "1_single_invoke.f90"),
                        api="dynamo0.3",
                        script_name=os.path.join(root_path, "test_files",
                                                 "dynamo0p3",
                                                 "1_single_invoke.f90"))


def test_script_invalid_content():
    ''' checks that generator.py raises an appropriate error when a
        script file does not contain valid python '''
    root_path = os.path.dirname(os.path.abspath(__file__))
    with pytest.raises(GenerationError):
        _, _ = generate(os.path.join(root_path, "test_files", "dynamo0p3",
                                     "1_single_invoke.f90"),
                        api="dynamo0.3",
                        script_name=os.path.join(
                            "test_files", "dynamo0p3", "error.py"))


def test_script_no_trans():
    ''' checks that generator.py raises an appropriate error when a
        script file does not contain a trans() function '''
    root_path = os.path.dirname(os.path.abspath(__file__))
    with pytest.raises(GenerationError):
        _, _ = generate(os.path.join(root_path, "test_files", "dynamo0p3",
                                     "1_single_invoke.f90"),
                        api="dynamo0.3",
                        script_name=os.path.join("test_files", "dynamo0p3",
                                                 "no_trans.py"))


def test_script_null_trans():
    ''' checks that generator.py works correctly when the trans()
        function in a valid script file does no transformations (it
        simply passes input to output). In this case the valid
        script file has an explicit path and must therefore exist at
        this location. '''
    root_path = os.path.dirname(os.path.abspath(__file__))
    alg1, psy1 = generate(os.path.join(root_path, "test_files", "dynamo0p3",
                                       "1_single_invoke.f90"),
                          api="dynamo0.3")
    alg2, psy2 = generate(os.path.join(root_path, "test_files", "dynamo0p3",
                                       "1_single_invoke.f90"),
                          api="dynamo0.3",
                          script_name=os.path.join(root_path, "test_files",
                                                   "dynamo0p3",
                                                   "null_trans.py"))
    # remove module so we do not affect any following tests
    delete_module("null_trans")
    # we need to remove the first line before comparing output as
    # this line is an instance specific header
    assert '\n'.join(str(alg1).split('\n')[1:]) == \
        '\n'.join(str(alg2).split('\n')[1:])
    assert '\n'.join(str(psy1).split('\n')[1:]) == \
        '\n'.join(str(psy2).split('\n')[1:])


def test_script_null_trans_relative():
    ''' checks that generator.py works correctly when the trans()
        function in a valid script file does no transformations (it
        simply passes input to output). In this case the valid
        script file contains no path and must therefore be found via
        the PYTHOPATH path list. '''
    root_path = os.path.dirname(os.path.abspath(__file__))
    alg1, psy1 = generate(os.path.join(root_path, "test_files", "dynamo0p3",
                                       "1_single_invoke.f90"),
                          api="dynamo0.3")
    # set up the python path so that null_trans.py can be found
    os.sys.path.append(os.path.join(root_path, "test_files", "dynamo0p3"))
    alg2, psy2 = generate(os.path.join(root_path, "test_files", "dynamo0p3",
                                       "1_single_invoke.f90"),
                          api="dynamo0.3", script_name="null_trans.py")
    # remove imported module so we do not affect any following tests
    delete_module("null_trans")
    os.sys.path.pop()
    # we need to remove the first line before comparing output as
    # this line is an instance specific header
    assert '\n'.join(str(alg1).split('\n')[1:]) == \
        '\n'.join(str(alg2).split('\n')[1:])
    assert str(psy1) == str(psy2)


def test_script_trans():
    ''' checks that generator.py works correctly when a
        transformation is provided as a script, i.e. it applies the
        transformations correctly. We use loop fusion as an
        example.'''
    from parse import parse
    from psyGen import PSyFactory
    from transformations import LoopFuseTrans
    root_path = os.path.dirname(os.path.abspath(__file__))
    base_path = os.path.join(root_path, "test_files", "dynamo0p3")
    # first loop fuse explicitly (without using generator.py)
    parse_file = os.path.join(base_path, "4_multikernel_invokes.f90")
    _, invoke_info = parse(parse_file, api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    invoke = psy.invokes.get("invoke_0")
    schedule = invoke.schedule
    loop1 = schedule.children[0]
    loop2 = schedule.children[1]
    trans = LoopFuseTrans()
    schedule, _ = trans.apply(loop1, loop2)
    invoke.schedule = schedule
    generated_code_1 = psy.gen
    # second loop fuse using generator.py and a script
    _, generated_code_2 = generate(parse_file, api="dynamo0.3",
                                   script_name=os.path.join(
                                       base_path, "loop_fuse_trans.py"))
    # remove module so we do not affect any following tests
    delete_module("loop_fuse_trans")
    # third - check that the results are the same ...
    assert str(generated_code_1) == str(generated_code_2)

# gocean1.0 API-specific tests


def test01_kernels_different_grid_offsets_one_invoke():
    ''' Check that the parser raises an error if two kernels in a
        single invoke specify different index offsets '''
    with pytest.raises(GenerationError):
        generate(os.path.join(os.path.dirname(
            os.path.abspath(__file__)), "test_files", "gocean1p0",
            "test01_different_grid_offsets_one_invoke.f90"),
            api="gocean1.0")


def test02_kernels_different_grid_offsets_two_invokes():
    ''' Check that the parser raises an error if the two kernels
        in different invokes specify different index offsets. '''
    with pytest.raises(GenerationError):
        generate(os.path.join(os.path.dirname(
            os.path.abspath(__file__)), "test_files", "gocean1p0",
            "test02_different_grid_offsets_two_invokes.f90"),
            api="gocean1.0")
