# -----------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
# -----------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab

'''
    This module provides the main PSyclone command line script which
    takes an algorithm file as input and produces modified algorithm
    code and generated PSy code. A function is also provided which has
    the same functionality as the command line script but can be
    called from within another Python program.
'''

import argparse
import sys
import os
import traceback
from parse import parse, ParseError
from psyGen import PSyFactory, GenerationError
from algGen import AlgorithmError
from config import SUPPORTEDAPIS, DEFAULTAPI
from line_length import FortLineLength


def generate(filename, api="", kernel_path="", script_name=None,
             line_length=False):
    '''Takes a GungHo algorithm specification as input and outputs the
    associated generated algorithm and psy codes suitable for
    compiling with the specified kernel(s) and GungHo
    infrastructure. Uses the :func:`parse.parse` function to parse the
    algorithm specification, the :class:`psyGen.PSy` class to generate
    the PSy code and the :class:`algGen.Alg` class to generate the
    modified algorithm code.

    :param str filename: The file containing the algorithm specification.
    :param str kernel_path: The directory from which to recursively
                            search for the files containing the kernel
                            source (if different from the location of the
                            algorithm specification)
    :param str script_name: A script file that can apply optimisations
                            to the PSy layer (can be a path to a file or
                            a filename that relies on the PYTHONPATH to
                            find the module).
    :param bool line_length: A logical flag specifying whether we care
                             about line lengths being longer than 132
                             characters. If so, the input (algorithm
                             and kernel) code is checked to make sure
                             that it conforms. The default is False.
    :return: The algorithm code and the psy code.
    :rtype: ast
    :raises IOError: if the filename or search path do not exist

    For example:

    >>> from generator import generate
    >>> psy, alg = generate("algspec.f90")
    >>> psy, alg = generate("algspec.f90", kernel_path="src/kernels")
    >>> psy, alg = generate("algspec.f90", script_name="optimise.py")
    >>> psy, alg = generate("algspec.f90", line_length=True)

    '''

    if api == "":
        api = DEFAULTAPI
    else:
        if api not in SUPPORTEDAPIS:
            raise GenerationError(
                "generate: Unsupported API '{0}' specified. Supported "
                "types are {1}.".format(api, SUPPORTEDAPIS))

    if not os.path.isfile(filename):
        raise IOError("file '{0}' not found".format(filename))
    if (len(kernel_path) > 0) and (not os.access(kernel_path, os.R_OK)):
        raise IOError("kernel search path '{0}' not found".format(kernel_path))
    try:
        from algGen import Alg
        ast, invoke_info = parse(filename, api=api, invoke_name="invoke",
                                 kernel_path=kernel_path,
                                 line_length=line_length)
        psy = PSyFactory(api).create(invoke_info)
        if script_name is not None:
            sys_path_appended = False
            try:
                # a script has been provided
                filepath, filename = os.path.split(script_name)
                if filepath:
                    # a path to a file has been provided
                    # we need to check the file exists
                    if not os.path.isfile(script_name):
                        raise IOError("script file '{0}' not found".
                                      format(script_name))
                    # it exists so we need to add the path to the python
                    # search path
                    sys_path_appended = True
                    sys.path.append(filepath)
                filename, fileext = os.path.splitext(filename)
                if fileext != '.py':
                    raise GenerationError(
                        "generator: expected the script file '{0}' to have "
                        "the '.py' extension".format(filename))
                try:
                    transmod = __import__(filename)
                except ImportError:
                    raise GenerationError(
                        "generator: attempted to import '{0}' but script file "
                        "'{1}' has not been found".
                        format(filename, script_name))
                except SyntaxError:
                    raise GenerationError(
                        "generator: attempted to import '{0}' but script file "
                        "'{1}' is not valid python".
                        format(filename, script_name))
                if callable(getattr(transmod, 'trans', None)):
                    try:
                        psy = transmod.trans(psy)
                    except Exception:
                        exc_type, exc_value, exc_traceback = sys.exc_info()
                        lines = traceback.format_exception(exc_type, exc_value,
                                                           exc_traceback)
                        e_str = '{\n' +\
                            ''.join('    ' + line for line in lines[2:]) + '}'
                        raise GenerationError(
                            "Generator: script file '{0}'\nraised the "
                            "following exception during execution "
                            "...\n{1}\nPlease check your script".format(
                                script_name, e_str))
                else:
                    raise GenerationError(
                        "generator: attempted to import '{0}' but script file "
                        "'{1}' does not contain a 'trans()' function".
                        format(filename, script_name))
            except Exception as msg:
                if sys_path_appended:
                    os.sys.path.pop()
                raise msg
            if sys_path_appended:
                os.sys.path.pop()
        alg = Alg(ast, psy)
    except Exception as msg:
        raise msg
    return alg.gen, psy.gen

if __name__ == "__main__":

    PARSER = argparse.ArgumentParser(
        description='Run the PSyclone code generator on a particular file')
    PARSER.add_argument('-oalg', help='filename of transformed algorithm code')
    PARSER.add_argument(
        '-opsy', help='filename of generated PSy code')
    PARSER.add_argument(
        '-api', default=DEFAULTAPI, help='choose a particular api from {0}, '
        'default {1}'.format(str(SUPPORTEDAPIS), DEFAULTAPI))
    PARSER.add_argument('filename', help='algorithm-layer source code')
    PARSER.add_argument('-s', '--script', help='filename of a PSyclone'
                        ' optimisation script')
    PARSER.add_argument(
        '-d', '--directory', default="", help='path to root of directory '
        'structure containing kernel source code')
    PARSER.add_argument(
        '-l', '--limit', dest='limit', action='store_true', default=False,
        help='limit the fortran line length to 132 characters')

    ARGS = PARSER.parse_args()
    if ARGS.api not in SUPPORTEDAPIS:
        print "Unsupported API '{0}' specified. Supported API's are "\
            "{1}.".format(ARGS.api, SUPPORTEDAPIS)
        exit(1)
    try:
        ALG, PSY = generate(ARGS.filename, api=ARGS.api,
                            kernel_path=ARGS.directory,
                            script_name=ARGS.script,
                            line_length=ARGS.limit)
    except AlgorithmError as error:
        print "Warning:", error
        exit(0)
    except (OSError, IOError, ParseError, GenerationError,
            RuntimeError) as error:
        _, EXC_VALUE, _ = sys.exc_info()
        print EXC_VALUE
        exit(1)
    except Exception as error:
        print "Error, unexpected exception:\n"
        EXC_TYPE, EXC_VALUE, EXC_TRACEBACK = sys.exc_info()
        print EXC_TYPE
        print EXC_VALUE
        traceback.print_tb(EXC_TRACEBACK)
        exit(1)
    if ARGS.limit:
        FLL = FortLineLength()
        PSY_STR = FLL.process(str(PSY))
        ALG_STR = FLL.process(str(ALG))
    else:
        PSY_STR = str(PSY)
        ALG_STR = str(ALG)
    if ARGS.oalg is not None:
        MY_FILE = open(ARGS.oalg, "w")
        MY_FILE.write(ALG_STR)
        MY_FILE.close()
    else:
        print "Transformed algorithm code:\n", ALG_STR
    if ARGS.opsy is not None:
        MY_FILE = open(ARGS.opsy, "w")
        MY_FILE.write(PSY_STR)
        MY_FILE.close()
    else:
        print "Generated psy layer code:\n", PSY_STR
