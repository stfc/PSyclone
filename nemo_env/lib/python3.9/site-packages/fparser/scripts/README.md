This directory contains scripts that are potentially useful for
running and/or testing fparser. The scripts and their use are described
below.

fparser2.py
-----------

Runs fparser2 on an input file and displays the de-parsed code. To see
any command line options add -h. This script can be used to choose
between parsing Fortran 2003 and Fortran 2008. An example of the use
of this script is:

> cat tmp.f90
program test
end program test
> fparser2.py tmp.f90
PROGRAM test
END PROGRAM test

fparser2_bench.py
-----------------

Generates a synthetic Fortran source benchmark in memory and then
measures the time taken by fparser2 to parse it.

parse.py
--------

Runs fparser1 on an input file and displays the parser's
representation of that code. To see any command line options add
-h. An example of the use of this script is:

> cat tmp.f90
program test
end program test
> parse.py tmp.f90
BeginSource
  blocktype='beginsource'
  name="<open file 'tmp3.f90', mode 'r' at 0x7f9bd0952f60> mode=free"
  content:
    Program
      blocktype='program'
      name='test'
      item=Line('program test',(1, 1),None,None,<reader>)
      content:
    EndProgram
      blocktype='program'
      name='test'
      item=Line('end program test',(2, 2),None,None,<reader>)

read.py
-------

Runs the fparser file reader on an input file and displays the file
reader's representation of that code. This is useful for testing that
the file reader is working as expected. Note the file reader is
required by both the fparser2.py and parse.py scripts. An example of
the use of this script is:

> cat tmp.f90
program test
end program test
> read.py tmp.f90 
line #1'program test'
line #2'end program test'

script_options.py
-----------------

Utility script used by fparser2.py, parse.py and read.py. This is
responsible for managing the common command line options in these files.
