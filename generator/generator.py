# Copyright 2013 STFC, all rights reserved
import argparse
import sys
import os
import traceback
from parse import parse,ParseError
from optimise import PSy,GenerationError

def generate(filename):
    '''
    Takes a GungHo algorithm specification as input and outputs the associated generated algorithm and psy codes suitable for compiling with the specified kernel(s) and GungHo infrastructure. Uses the :func:`parse.parse` function to parse the algorithm specification, the :class:`optimise.PSy` class to generate the PSy code and the :class:`algGen.Alg` class to generate the modified algorithm code.

    :param str filename: The file containing the algorithm specification.
    :return: The algorithm code and the psy code.
    :rtype: ast
    :raises IOError: if the filename does not exist

    For example:

    >>> from generator import generate
    >>> psy,alg=generate("algspec.f90")

    '''

    if not os.path.isfile(filename):
        raise IOError, "file '%s' not found" % (filename)
    try:
        from algGen import Alg
        ast,invokeInfo=parse(filename,invoke_name="invoke")
        psy=PSy(invokeInfo)
        alg=Alg(ast,psy)
        #invokes=psy.invokes
        #print str(invokes)
        #print str(invokes.names)
        #invoke=invokes.get("invoke_2")
        #print str(invoke)
        #schedule=invoke.schedule
        #print str(schedule)
        #schedule.transform.list
        #schedule.transform.apply.sayHello()
        #schedule.transform.undo
        #schedule.transform.redo

    except Exception as msg:
        # add contextual information here?
        raise
    return alg.gen,psy.gen

if __name__=="__main__":

    parser = argparse.ArgumentParser(description='Run the GungHo code generator on a particular file')
    parser.add_argument('-oalg', help='filename of tranformed algorithm code')
    parser.add_argument('-opsy', help='filename of generated psy code')
    parser.add_argument('filename', help='algorithm-layer source code')
    args = parser.parse_args()
    try:
        alg,psy=generate(args.filename)
    except (OSError, IOError, ParseError,GenerationError,RuntimeError) as e:
        print "Error:",e
        exit(1)
    except Exception as e:
        print "Error, unexpected exception:\n"
        exc_type, exc_value, exc_traceback = sys.exc_info()
        print exc_type
        print exc_value
        traceback.print_tb(exc_traceback)
        exit(1)
    if args.oalg is not None:
        file = open(args.oalg, "w")
        file.write(str(alg))
        file.close()
    else:
        print "Transformed algorithm code:\n",alg
    if args.opsy is not None:
        file = open(args.opsy, "w")
        file.write(str(psy))
        file.close()
    else:
        print "Generated psy layer code:\n",psy

