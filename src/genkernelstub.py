# -----------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2015.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
# -----------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab

'''A python script and python function to generate an empty kernel
    subroutine with the required arguments and datatypes (which we
    call a stub) when presented with Kernel Metadata.
'''

import argparse
import fparser
from fparser import api as fpapi
from dynamo0p3 import DynKern, DynKernMetadata
from psyGen import GenerationError
from parse import ParseError
from config import SUPPORTEDSTUBAPIS, DEFAULTSTUBAPI
import os
import sys
import traceback
from line_length import FortLineLength


def generate(filename, api=""):

    '''Generates an empty kernel subroutine with the required arguments
       and datatypes (which we call a stub) when presented with Kernel
       Metadata. This is useful for Kernel developers to make sure
       they are using the correct arguments in the correct order.  The
       Kernel Metadata must be presented in the standard Kernel
       format.
    '''
    if api == "":
        api = DEFAULTSTUBAPI
    if api not in SUPPORTEDSTUBAPIS:
        print "Unsupported API '{0}' specified. Supported API's are {1}.".\
              format(api, SUPPORTEDSTUBAPIS)
        raise GenerationError(
            "generate: Unsupported API '{0}' specified. Supported types are "
            "{1}.".format(api, SUPPORTEDSTUBAPIS))

    if not os.path.isfile(filename):
        raise IOError("file '{0}' not found".format(filename))

    # drop cache
    fparser.parsefortran.FortranParser.cache.clear()
    fparser.logging.disable('CRITICAL')
    try:
        ast = fpapi.parse(filename, ignore_comments=False)
    except AttributeError:
        raise ParseError("Code appears to be invalid Fortran")

    metadata = DynKernMetadata(ast)
    kernel = DynKern()
    kernel.load_meta(metadata)
    return kernel.gen_stub

if __name__ == "__main__":

    PARSER = argparse.ArgumentParser(description="Create Kernel stub code from"
                                                 " Kernel metadata")
    PARSER.add_argument("-o", "--outfile", help="filename of output")
    PARSER.add_argument("-api", default=DEFAULTSTUBAPI,
                        help="choose a particular api from {0}, default {1}".
                        format(str(SUPPORTEDSTUBAPIS), DEFAULTSTUBAPI))
    PARSER.add_argument('filename', help='Kernel metadata')
    PARSER.add_argument(
        '-l', '--limit', dest='limit', action='store_true', default=False,
        help='limit the fortran line length to 132 characters')

    ARGS = PARSER.parse_args()

    try:
        STUB = generate(ARGS.filename, api=ARGS.api)
    except (IOError, ParseError, GenerationError, RuntimeError) as error:
        print "Error:", error
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
        STUB_STR = FLL.process(str(STUB))
    else:
        STUB_STR = str(STUB)
    if ARGS.outfile is not None:
        MY_FILE = open(ARGS.outfile, "w")
        MY_FILE.write(STUB_STR)
        MY_FILE.close()
    else:
        print "Kernel stub code:\n", STUB_STR
