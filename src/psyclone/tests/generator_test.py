# -----------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
# -----------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab

'''
A module to perform pytest unit and functional tests on the code in
the generator.py file. This includes the generate and the main
functions.
'''

from __future__ import absolute_import
import os
import tempfile
import pytest
from psyclone.generator import generate, GenerationError, main
from psyclone.parse import ParseError

BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "test_files")


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
        generate(os.path.join(BASE_PATH, "dynamo0p1", "algorithm",
                              "1_single_function.f90"), api="invalid")


def test_invalid_kernel_path():
    ''' checks that algGen raises appropriate error when an invalid
        search path for kernel source files is supplied '''
    with pytest.raises(IOError):
        generate(os.path.join(BASE_PATH, "dynamo0p1", "algorithm",
                              "1_single_function.f90"),
                 api="dynamo0.1",
                 kernel_path="does_not_exist")


def test_wrong_kernel_path():
    ''' checks that algGen raises appropriate error when the kernel
        code cannot be found in the specified search path '''
    with pytest.raises(IOError):
        generate(os.path.join(BASE_PATH, "dynamo0p3",
                              "1.1.0_single_invoke_xyoz_qr.f90"),
                 api="dynamo0.3",
                 kernel_path=os.path.join(BASE_PATH, "gocean0p1"))


def test_correct_kernel_path():
    ''' checks that algGen succeeds when the location of the kernel
        source code is *not* the same as that of the algorithm code '''
    _, _ = generate(os.path.join(BASE_PATH, "dynamo0p1", "algorithm",
                                 "1_single_function.f90"),
                    api="dynamo0.1",
                    kernel_path=os.path.join(BASE_PATH,
                                             "dynamo0p1", "kernels"))


def test_same_kernel_path():
    ''' checks that the generator succeeds when the search directory
        is the same as the algorithm code directory and a path is
        specified '''
    path = os.path.join(BASE_PATH, "dynamo0p1", "algorithm")
    _, _ = generate(os.path.join(path, "1_single_function.f90"),
                    api="dynamo0.1", kernel_path=path)


def test_similar_kernel_name():
    ''' checks that the generator does not match incorrect files '''
    _, _ = generate(os.path.join(BASE_PATH, "dynamo0p1",
                                 "algorithm", "1_single_function.f90"),
                    api="dynamo0.1",
                    kernel_path=os.path.join(BASE_PATH,
                                             "dynamo0p1", "kernels2"))


def test_recurse_correct_kernel_path():
    '''checks that the generator succeeds when the location of the kernel
       source code is *not* the same as that of the algorithm code and
       recursion through subdirectories is required'''
    _, _ = generate(os.path.join(BASE_PATH, "dynamo0p1",
                                 "algorithm", "1_single_function.f90"),
                    api="dynamo0.1",
                    kernel_path=os.path.join(BASE_PATH,
                                             "dynamo0p1", "kernels3"))


def test_script_file_not_found():
    ''' checks that generator.py raises an appropriate error when a
        script file is supplied that can't be found in the Python path.
        In this case the script path is supplied'''
    with pytest.raises(IOError):
        _, _ = generate(os.path.join(BASE_PATH, "dynamo0p3",
                                     "1_single_invoke.f90"),
                        api="dynamo0.3", script_name="./non_existant.py")


def test_script_file_not_found_relative():
    ''' checks that generator.py raises an appropriate error when a script
        file is supplied that can't be found in the Python path. In
        this case the script path is not supplied so must be found via the
        PYTHONPATH variable'''
    with pytest.raises(GenerationError):
        _, _ = generate(os.path.join(BASE_PATH, "dynamo0p3",
                                     "1_single_invoke.f90"),
                        api="dynamo0.3", script_name="non_existant.py")


def test_script_file_too_short():
    ''' checks that generator.py raises an appropriate error when a
        script file name is too short to contain the '.py' extension'''
    with pytest.raises(GenerationError):
        _, _ = generate(os.path.join(BASE_PATH, "dynamo0p3",
                                     "1_single_invoke.f90"),
                        api="dynamo0.3",
                        script_name=os.path.join(BASE_PATH,
                                                 "dynamo0p3", "xyz"))


def test_script_file_no_extension():
    ''' checks that generator.py raises an appropriate error when a
        script file does not have an extension'''
    with pytest.raises(GenerationError):
        _, _ = generate(os.path.join(BASE_PATH, "dynamo0p3",
                                     "1_single_invoke.f90"),
                        api="dynamo0.3",
                        script_name=os.path.join(BASE_PATH, "dynamo0p3",
                                                 "invalid_script_name"))


def test_script_file_wrong_extension():
    ''' checks that generator.py raises an appropriate error when a
        script file does not have the '.py' extension'''
    with pytest.raises(GenerationError):
        _, _ = generate(os.path.join(BASE_PATH, "dynamo0p3",
                                     "1_single_invoke.f90"),
                        api="dynamo0.3",
                        script_name=os.path.join(BASE_PATH, "dynamo0p3",
                                                 "1_single_invoke.f90"))


def test_script_invalid_content():
    ''' checks that generator.py raises an appropriate error when a
        script file does not contain valid python '''
    with pytest.raises(GenerationError):
        _, _ = generate(os.path.join(BASE_PATH, "dynamo0p3",
                                     "1_single_invoke.f90"),
                        api="dynamo0.3",
                        script_name=os.path.join(
                            BASE_PATH, "dynamo0p3", "error.py"))


def test_script_invalid_content_runtime():
    ''' checks that generator.py raises an appropriate error when a
        script file contains valid python syntactically but produces a
        runtime exception. '''
    with pytest.raises(GenerationError):
        _, _ = generate(os.path.join(BASE_PATH, "dynamo0p3",
                                     "1_single_invoke.f90"),
                        api="dynamo0.3",
                        script_name=os.path.join(
                            BASE_PATH, "dynamo0p3", "runtime_error.py"))


def test_script_no_trans():
    ''' checks that generator.py raises an appropriate error when a
        script file does not contain a trans() function '''
    with pytest.raises(GenerationError) as excinfo:
        _, _ = generate(os.path.join(BASE_PATH, "dynamo0p3",
                                     "1_single_invoke.f90"),
                        api="dynamo0.3",
                        script_name=os.path.join(BASE_PATH, "dynamo0p3",
                                                 "no_trans.py"))
    assert 'attempted to import' in str(excinfo.value)


def test_script_attr_error():
    ''' checks that generator.py raises an appropriate error when a
        script file contains a trans() function which raises an
        attribute error. This is what we previously used to check for
        a script file not containing a trans() function.'''
    with pytest.raises(GenerationError) as excinfo:
        _, _ = generate(os.path.join(BASE_PATH, "dynamo0p3",
                                     "1_single_invoke.f90"),
                        api="dynamo0.3",
                        script_name=os.path.join(BASE_PATH,
                                                 "dynamo0p3",
                                                 "error_trans.py"))
    assert 'object has no attribute' in str(excinfo.value)


def test_script_null_trans():
    ''' checks that generator.py works correctly when the trans()
        function in a valid script file does no transformations (it
        simply passes input to output). In this case the valid
        script file has an explicit path and must therefore exist at
        this location. '''
    alg1, psy1 = generate(os.path.join(BASE_PATH, "dynamo0p3",
                                       "1_single_invoke.f90"),
                          api="dynamo0.3")
    alg2, psy2 = generate(os.path.join(BASE_PATH, "dynamo0p3",
                                       "1_single_invoke.f90"),
                          api="dynamo0.3",
                          script_name=os.path.join(BASE_PATH,
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
    alg1, psy1 = generate(os.path.join(BASE_PATH, "dynamo0p3",
                                       "1_single_invoke.f90"),
                          api="dynamo0.3")
    # set up the python path so that null_trans.py can be found
    os.sys.path.append(os.path.join(BASE_PATH, "dynamo0p3"))
    alg2, psy2 = generate(os.path.join(BASE_PATH, "dynamo0p3",
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
    from psyclone.parse import parse
    from psyclone.psyGen import PSyFactory
    from psyclone.transformations import LoopFuseTrans
    root_path = os.path.dirname(os.path.abspath(__file__))
    base_path = os.path.join(root_path, "test_files", "dynamo0p3")
    # first loop fuse explicitly (without using generator.py)
    parse_file = os.path.join(base_path, "4_multikernel_invokes.f90")
    _, invoke_info = parse(parse_file, api="dynamo0.3")
    psy = PSyFactory("dynamo0.3").create(invoke_info)
    invoke = psy.invokes.get("invoke_0")
    schedule = invoke.schedule
    loop1 = schedule.children[3]
    loop2 = schedule.children[4]
    trans = LoopFuseTrans()
    schedule.view()
    schedule, _ = trans.apply(loop1, loop2)
    invoke.schedule = schedule
    generated_code_1 = psy.gen
    schedule.view()
    # second loop fuse using generator.py and a script
    _, generated_code_2 = generate(parse_file, api="dynamo0.3",
                                   script_name=os.path.join(
                                       base_path, "loop_fuse_trans.py"))
    # remove module so we do not affect any following tests
    delete_module("loop_fuse_trans")
    # third - check that the results are the same ...
    assert str(generated_code_1) == str(generated_code_2)


DYN03_BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                               "test_files", "dynamo0p3")


def test_alg_lines_too_long_tested():
    ''' Test that the generate function causes an exception if the
    line_length argument is set to True and the algorithm file has
    lines longer than 132 characters. We use the dynamo0.3 API in this
    case but could have chosen any. '''
    alg_filename = os.path.join(DYN03_BASE_PATH, "13_alg_long_line.f90")
    with pytest.raises(ParseError) as excinfo:
        _, _ = generate(alg_filename, api="dynamo0.3", line_length=True)
    assert 'algorithm file does not conform' in str(excinfo.value)


def test_alg_lines_too_long_not_tested():
    ''' Test that the generate function returns successfully if the
    line_length argument is not set (as it should default to False)
    when the algorithm file has lines longer than 132 characters. We
    use the dynamo0.3 API in this case but could have chosen any.'''
    alg_filename = os.path.join(DYN03_BASE_PATH, "13_alg_long_line.f90")
    _, _ = generate(alg_filename, api="dynamo0.3")


def test_kern_lines_too_long_tested():
    ''' Test that the generate function raises an exception if the
    line_length argument is set to True and a Kernel file has
    lines longer than 132 characters. We use the dynamo0.3 API in this
    case but could have chosen any. '''
    alg_filename = os.path.join(DYN03_BASE_PATH, "13.1_kern_long_line.f90")
    with pytest.raises(ParseError) as excinfo:
        _, _ = generate(alg_filename, api="dynamo0.3", line_length=True)
    assert 'kernel file' in str(excinfo.value)
    assert 'does not conform' in str(excinfo.value)


def test_kern_lines_too_long_not_tested():
    ''' Test that the generate function returns successfully if the
    line_length argument is not set (as it should default to False)
    when a kernel file has lines longer than 132 characters. We
    use the dynamo0.3 API in this case but could have chosen any.'''
    alg_filename = os.path.join(DYN03_BASE_PATH, "13.1_kern_long_line.f90")
    _, _ = generate(alg_filename, api="dynamo0.3")


def test_continuators():
    '''Tests that input files with long lines that already have
       continuators to make the code conform to the line length limit
       do not cause an error '''
    _, _ = generate(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "1.1.0_single_invoke_xyoz_qr.f90"),
                    api="dynamo0.3", line_length=True)


def test_main_invalid_api(capsys):
    '''Tests that we get the expected output and the code exits
    with an error if the supplied API is not known'''
    filename = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             "test_files", "dynamo0p3",
                             "1_single_invoke.f90"))
    with pytest.raises(SystemExit) as excinfo:
        main([filename, "-api", "madeup"])
    # the error code should be 1
    assert str(excinfo.value) == "1"
    output, _ = capsys.readouterr()
    expected_output = ("Unsupported API 'madeup' specified. Supported API's "
                       "are ['gunghoproto', 'dynamo0.1', 'dynamo0.3', "
                       "'gocean0.1', 'gocean1.0'].\n")
    assert output == expected_output


def test_main_expected_fatal_error(capsys):
    '''Tests that we get the expected output and the code exits with an
    error when an expected fatal error is returned from the generate
    function.'''
    filename = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             "test_files", "dynamo0p3",
                             "2_incorrect_number_of_args.f90"))
    with pytest.raises(SystemExit) as excinfo:
        main([filename])
    # the error code should be 1
    assert str(excinfo.value) == "1"
    output, _ = capsys.readouterr()
    expected_output = ("\"Parse Error: Kernel 'testkern_type' called from the "
                       "algorithm layer with an insufficient number of "
                       "arguments as specified by the metadata. Expected at "
                       "least '5' but found '4'.\"\n")
    assert output == expected_output


def test_main_unexpected_fatal_error(capsys, monkeypatch):
    '''Tests that we get the expected output and the code exits with an
    error when an unexpected fatal error is returned from the generate
    function.'''
    # sabotage the code so one of our constant lists is now an int
    from psyclone import dynamo0p3
    monkeypatch.setattr(dynamo0p3, "CONTINUOUS_FUNCTION_SPACES",
                        value=1)
    filename = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             "test_files", "dynamo0p3",
                             "1_single_invoke.f90"))
    with pytest.raises(SystemExit) as excinfo:
        main([filename])
    # the error code should be 1
    assert str(excinfo.value) == "1"
    output, _ = capsys.readouterr()
    expected_output = (
        "Error, unexpected exception, please report to the authors:\n"
        "Description ...\n"
        "argument of type 'int' is not iterable\n"
        "Type ...\n"
        "<type 'exceptions.TypeError'>\n"
        "Stacktrace ...\n")
    assert expected_output in output


def test_main_fort_line_length(capsys):
    '''Tests that the fortran line length object works correctly. Without
    the -l option one of the generated psy-layer lines would be longer
    than 132 characters'''
    filename = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             "test_files", "dynamo0p3",
                             "10.3_operator_different_spaces.f90"))
    main([filename, '-api', 'dynamo0.3', '-l'])
    output, _ = capsys.readouterr()
    for line in output.split('\n'):
        assert len(line) <= 132


def test_main_no_invoke_alg_stdout(capsys):
    '''Tests that the main() function outputs the original algorithm input
    file to stdout when the algorithm file does not contain an invoke and that
    it does not produce any psy output.'''

    # pass in a kernel file as that has no invokes in it
    kern_filename = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                  "test_files", "dynamo0p3",
                                  "testkern.F90"))
    main([kern_filename])
    out, _ = capsys.readouterr()

    kern_file = open(kern_filename)
    kern_str = kern_file.read()
    expected_output = ("Warning: 'Algorithm Error: Algorithm file contains no "
                       "invoke() calls: refusing to generate empty PSy code'\n"
                       "Transformed algorithm code:\n") + kern_str + "\n"
    assert expected_output == out


def test_main_write_psy_file(capsys):
    '''Tests that the main() function outputs successfully writes the
    generated psy output to a specified file'''

    alg_filename = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p3",
                                 "1_single_invoke.f90"))

    filetemp_psy = tempfile.NamedTemporaryFile()
    psy_filename = filetemp_psy.name
    filetemp_psy.close()
    # no need to delete the file as it has not been created

    main([alg_filename, '-opsy', psy_filename])

    # check psy file is created
    assert os.path.isfile(psy_filename)

    # extract psy file content
    psy_file = open(psy_filename)
    psy_str = psy_file.read()

    # check content of generated psy file by comparing it with stdout
    main([alg_filename])
    stdout, _ = capsys.readouterr()

    assert psy_str in stdout


def test_main_no_invoke_alg_file(capsys):
    '''Tests that the main() function outputs the original algorithm input
    file to file when the algorithm file does not contain an invoke and that
    it does not produce any psy output.'''

    # pass in a kernel file as that has no invokes in it
    kern_filename = (os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                  "test_files", "dynamo0p3",
                                  "testkern.F90"))

    filetemp_alg = tempfile.NamedTemporaryFile()
    alg_filename = filetemp_alg.name
    filetemp_psy = tempfile.NamedTemporaryFile()
    psy_filename = filetemp_psy.name
    filetemp_alg.close()
    filetemp_psy.close()
    # no need to delete the files as they have not been created

    main([kern_filename, '-oalg', alg_filename, '-opsy', psy_filename])
    stdout, _ = capsys.readouterr()

    # check stdout contains warning
    kern_file = open(kern_filename)
    kern_str = kern_file.read()
    expected_stdout = ("Warning: 'Algorithm Error: Algorithm file contains "
                       "no invoke() calls: refusing to generate empty PSy "
                       "code'\n")
    assert expected_stdout == stdout

    # check alg file has same output as input file
    expected_file = open(alg_filename)
    expected_alg_str = expected_file.read()
    assert expected_alg_str == kern_str
    os.remove(alg_filename)

    # check psy file is not created
    assert not os.path.isfile(psy_filename)
