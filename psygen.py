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
    
from ghtools import algParse,psyGen

algCalls=algParse(fileName,psyName=options.psy)
result=psyGen(algCalls,psyName=options.psy,infName="types",kernName="kern")

# output the generated code
if options.fileName=='':
  print result
else:
  with open(options.fileName, "w") as text_file:
    text_file.write(str(result))

exit(0)

        #if False:
        #  # precheck code - to be sorted out later
        #  for id,name in enumerate(stmt.items):
        #    call=addcall("precheck",[quotes(stmt.designator),quotes(id+1),stmt.items[id]],sub)
        #    use=adduse("inf",sub,only=True,funcnames=["precheck"])


