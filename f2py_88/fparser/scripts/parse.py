#!/usr/bin/env python
import os
import sys
### START UPDATE SYS.PATH ###
### END UPDATE SYS.PATH ###
try:
    from iocbio.optparse_gui import OptionParser
except ImportError:
    from optparse import OptionParser
from fparser.script_options import set_parse_options

def runner (parser, options, args):
    from fparser.readfortran import  FortranFileReader
    from fparser.parsefortran import  FortranParser
    for filename in args:
        reader = FortranFileReader(filename)
        if options.mode != 'auto':
            reader.set_mode_from_str(options.mode)
        parser = FortranParser(reader)
        parser.parse()
        parser.analyze()
        if options.task=='show':
            print parser.block.torepr(4)
        elif options.task == 'none':
            pass
        else:
            raise NotImplementedError(`options.task`)
        

def main ():
    parser = OptionParser()
    set_parse_options(parser)
    if hasattr(parser, 'runner'):
        parser.runner = runner
    options, args = parser.parse_args()
    runner(parser, options, args)
    return

if __name__=="__main__":
    main()
