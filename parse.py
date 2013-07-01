#!/usr/bin/env python

import os
from optparse import OptionParser
parser = OptionParser("usage: %prog fortran_file")
parser.add_option("-p","--psy",dest="psy", default='psy',
                  help="name of psy module, default 'psy'")
parser.add_option("-o","--outputfile",dest="fileName", default='',
                  help="name of the output file")
(options, args) = parser.parse_args()
if len(args) != 1:
  parser.error("incorrect number of arguments")
fileName=args[0]
if not(os.path.isfile(args[0])):
  parser.error("file does not exist")
    
from ghtools import algParse

algCalls=algParse(fileName,psyName=options.psy)

for call in algCalls:
  print call,"["
  args=call.args
  for arg in args.argList:
    print "  ",arg
  print "]"
