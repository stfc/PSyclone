#!/usr/bin/env python
import os
import sys
### START UPDATE SYS.PATH ###
### END UPDATE SYS.PATH ###
try:
    from iocbio.optparse_gui import OptionParser
except ImportError:
    from optparse import OptionParser
from fparser.script_options import set_read_options

def runner (parser, options, args):
    from fparser.readfortran import  FortranFileReader
    for filename in args:
        reader = FortranFileReader(filename)
        if options.task=='show':
            for item in reader:
                print >> sys.stdout, item
                sys.stdout.flush()
        else:
            raise NotImplementedError(`options.task`)
        

def main ():
    parser = OptionParser()
    set_read_options(parser)
    if hasattr(parser, 'runner'):
        parser.runner = runner
    options, args = parser.parse_args()
    runner(parser, options, args)
    return

if __name__=="__main__":
    main()
