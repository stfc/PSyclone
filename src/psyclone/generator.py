#!/usr/bin/env python2.7
# -*- coding: utf-8 -*-
# -----------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
# -----------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab
# Modified work Copyright (c) 2018 by J. Henrichs, Bureau of Meteorology


'''
    This module provides the PSyclone 'main' routine which is intended
    to be driven from the bin/psyclone executable script. 'main'
    takes an algorithm file as input and produces modified algorithm
    code and generated PSy code. A function, 'generate', is also provided
    which has the same functionality as 'main' but can be called
    from within another Python program.
'''

import argparse
import sys
import os
import traceback
from psyclone.parse import parse, ParseError
from psyclone.psyGen import PSyFactory, GenerationError
from psyclone.algGen import NoInvokesError
from psyclone.config import SUPPORTEDAPIS, DEFAULTAPI, DISTRIBUTED_MEMORY
from psyclone.line_length import FortLineLength
from psyclone.version import __VERSION__


def handle_script(script_name, psy):
    '''Loads and applies the specified script to the given psy layer.
    The 'trans' function of the script is called with psy as parameter.
    :param script_name: Name of the script to load.
    :type script_name: string
    :param psy: The psy layer to which the script is applied.
    :type psy: :py:class:`psyclone.psyGen.PSy`
    :raises IOError: If the file is not found.
    :raises GenerationError: if the file does not have .py extension
        or can not be imported.
    :raises GenerationError: if trans() can not be called.
    :raises GenerationError: if any exception is raised when trans()
        was called.
    '''
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


def generate(filename, api="", kernel_path="", script_name=None,
             line_length=False,
             distributed_memory=DISTRIBUTED_MEMORY):
    # pylint: disable=too-many-arguments
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
    :param bool distributed_memory: A logical flag specifying whether to
                                    generate distributed memory code. The
                                    default is set in the config.py file.
    :return: The algorithm code and the psy code.
    :rtype: ast
    :raises IOError: if the filename or search path do not exist

    For example:

    >>> from generator import generate
    >>> alg, psy = generate("algspec.f90")
    >>> alg, psy = generate("algspec.f90", kernel_path="src/kernels")
    >>> alg, psy = generate("algspec.f90", script_name="optimise.py")
    >>> alg, psy = generate("algspec.f90", line_length=True)
    >>> alg, psy = generate("algspec.f90", distributed_memory=False)

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
        from psyclone.algGen import Alg
        ast, invoke_info = parse(filename, api=api, invoke_name="invoke",
                                 kernel_path=kernel_path,
                                 line_length=line_length)
        psy = PSyFactory(api, distributed_memory=distributed_memory).\
            create(invoke_info)
        if script_name is not None:
            handle_script(script_name, psy)
        alg = Alg(ast, psy)
    except Exception:
        raise
    return alg.gen, psy.gen


def main(args):

    ''' Parses and checks the command line arguments, calls the generate
    function if all is well, catches any errors and outputs the
    results
    '''
    # pylint: disable=too-many-statements
    parser = argparse.ArgumentParser(
        description='Run the PSyclone code generator on a particular file')
    parser.add_argument('-oalg', help='filename of transformed algorithm code')
    parser.add_argument(
        '-opsy', help='filename of generated PSy code')
    parser.add_argument(
        '-api', default=DEFAULTAPI, help='choose a particular api from {0}, '
        'default {1}'.format(str(SUPPORTEDAPIS), DEFAULTAPI))
    parser.add_argument('filename', help='algorithm-layer source code')
    parser.add_argument('-s', '--script', help='filename of a PSyclone'
                        ' optimisation script')
    parser.add_argument(
        '-d', '--directory', default="", help='path to root of directory '
        'structure containing kernel source code')
    parser.add_argument(
        '-l', '--limit', dest='limit', action='store_true', default=False,
        help='limit the fortran line length to 132 characters')
    parser.add_argument(
        '-dm', '--dist_mem', dest='dist_mem', action='store_true',
        help='generate distributed memory code')
    parser.add_argument(
        '-nodm', '--no_dist_mem', dest='dist_mem', action='store_false',
        help='do not generate distributed memory code')
    parser.set_defaults(dist_mem=DISTRIBUTED_MEMORY)

    parser.add_argument(
        '-v', '--version', dest='version', action="store_true",
        help='Display version information ({0})'.format(__VERSION__))

    args = parser.parse_args(args)

    if args.api not in SUPPORTEDAPIS:
        print "Unsupported API '{0}' specified. Supported API's are "\
            "{1}.".format(args.api, SUPPORTEDAPIS)
        exit(1)

    if args.version:
        print "PSyclone version: {0}".format(__VERSION__)

    # pylint: disable=broad-except
    try:
        alg, psy = generate(args.filename, api=args.api,
                            kernel_path=args.directory,
                            script_name=args.script,
                            line_length=args.limit,
                            distributed_memory=args.dist_mem)
    except NoInvokesError:
        _, exc_value, _ = sys.exc_info()
        print "Warning: {0}".format(exc_value)
        # no invoke calls were found in the algorithm file so we need
        # not need to process it, or generate any psy layer code so
        # output the original algorithm file and set the psy file to
        # be empty
        alg_file = open(args.filename)
        alg = alg_file.read()
        psy = ""
    except (OSError, IOError, ParseError, GenerationError,
            RuntimeError):
        _, exc_value, _ = sys.exc_info()
        print exc_value
        exit(1)
    except Exception:
        print "Error, unexpected exception, please report to the authors:"
        exc_type, exc_value, exc_tb = sys.exc_info()
        print "Description ..."
        print exc_value
        print "Type ..."
        print exc_type
        print "Stacktrace ..."
        traceback.print_tb(exc_tb, limit=10, file=sys.stdout)
        exit(1)
    if args.limit:
        fll = FortLineLength()
        psy_str = fll.process(str(psy))
        alg_str = fll.process(str(alg))
    else:
        psy_str = str(psy)
        alg_str = str(alg)
    if args.oalg is not None:
        my_file = open(args.oalg, "w")
        my_file.write(alg_str)
        my_file.close()
    else:
        print "Transformed algorithm code:\n", alg_str

    if not psy_str:
        # empty file so do not output anything
        pass
    elif args.opsy is not None:
        my_file = open(args.opsy, "w")
        my_file.write(psy_str)
        my_file.close()
    else:
        print "Generated psy layer code:\n", psy_str


if __name__ == "__main__":
    main(sys.argv[1:])
