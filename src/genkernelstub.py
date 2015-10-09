# -----------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2015.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
# -----------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab

import argparse
import fparser
from fparser import api as fpapi
from dynamo0p3 import DynKern, DynKernMetadata
from psyGen import GenerationError
from parse import ParseError
from config import SUPPORTEDSTUBAPIS
import os
import sys
import traceback


def generate(filename, api=""):

    if api == "":
        from config import DEFAULTSTUBAPI
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

    from config import SUPPORTEDSTUBAPIS, DEFAULTSTUBAPI
    parser = argparse.ArgumentParser(description="Create Kernel stub code from"
                                                 " Kernel metadata")
    parser.add_argument("-o", "--outfile", help="filename of output")
    parser.add_argument("-api", default=DEFAULTSTUBAPI,
                        help="choose a particular api from {0}, default {1}".
                             format(str(SUPPORTEDSTUBAPIS), DEFAULTSTUBAPI))
    parser.add_argument('filename', help='Kernel metadata')
    args = parser.parse_args()

    try:
        stub = generate(args.filename, api=args.api)
    except (IOError, ParseError, GenerationError, RuntimeError) as e:
        print "Error:", e
        exit(1)
    except Exception as e:
        print "Error, unexpected exception:\n"
        exc_type, exc_value, exc_traceback = sys.exc_info()
        print exc_type
        print exc_value
        traceback.print_tb(exc_traceback)
        exit(1)

    print dir(args)

    if args.outfile is not None:
        file = open(args.outfile, "w")
        file.write(str(stub))
        file.close()
    else:
        print "Kernel stub code:\n", stub
