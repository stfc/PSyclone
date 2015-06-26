#!/usr/bin/env python
import os
import sys
### START UPDATE SYS.PATH ###
### END UPDATE SYS.PATH ###
try:
    from iocbio.optparse_gui import OptionParser
except ImportError:
    from optparse import OptionParser
from fparser.script_options import set_f2003_options

def runner (parser, options, args):
    from fparser.api import Fortran2003
    from fparser.readfortran import  FortranFileReader
    for filename in args:
        reader = FortranFileReader(filename)
        if options.mode != 'auto':
            reader.set_mode_from_str(options.mode)
        try:
            program = Fortran2003.Program(reader)
            print program
        except Fortran2003.NoMatchError, msg:
            print 'parsing %r failed at %s' % (filename, reader.fifo_item[-1])
            print 'started at %s' % (reader.fifo_item[0])
            print 'quiting'
            return

def main ():
    parser = OptionParser()
    set_f2003_options(parser)
    if hasattr(parser, 'runner'):
        parser.runner = runner
    options, args = parser.parse_args()
    runner(parser, options, args)
    return

if __name__=="__main__":
    main()
