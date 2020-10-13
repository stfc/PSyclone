# PSyclone and NEMO, Tutorial 1 #

Welcome to the first part of the tutorial on using PSyclone with the
NEMO ocean model (www.nemo-ocean.eu). For this tutorial we will be
using a standalone, single-source-file mini-app (tra_adv.F90) based on
a tracer-advection routine that has been extracted from the full NEMO
source. The original version of this mini-app was kindly provided by
Silvia Mocavero of CMCC.

## Prerequisites ##

You will need a Linux shell with a working Python installation in
which PSyclone installed. (See the top-level README.md for
installation instructions.) If you wish to compile and run the
generated code then you will also need a Fortran compiler: gfortran is
fine.

Check that PSyclone is installed and working correctly by doing:

    $ psyclone -h

You should see help information, beginning with:

    usage: psyclone [-h] [-oalg OALG] [-opsy OPSY] [-okern OKERN] [-api API] [-s SCRIPT] [-d DIRECTORY] [-I INCLUDE]
                [-l {off,all,output}] [-dm] [-nodm] [--kernel-renaming {multiple,single}] [--profile {invokes,kernels}]
                [--config CONFIG] [-v]
                filename

By default, PSyclone expects to process code written to the LFRic
API. In order to specify that we are processing NEMO code we must
specify `-api nemo`:

    $ psyclone -api nemo tra_adv.F90

This command should result in PSyclone processing the supplied Fortran
and then re-generating it and writing it to stdout:

    Transformed algorithm code:
    None
    Generated psy layer code:
     PROGRAM tra_adv
      USE iso_c_binding, ONLY: C_INT64_T
      INTEGER, PARAMETER :: wp = 8

Note that there is no algorithm code because NEMO does not follow the
PSyKAl separation of concerns. Instead, PSyclone treats it as though
it is a manually-written PSy layer.

In order to compile the output Fortran we need it to be written to
a file instead of stdout. This is achieved with the -opsy flag so
that doing:

    $ psyclone -api nemo -opsy psy.f90 tra_adv.F90

will create a new file, `psy.f90`, containing the generated Fortran
code. As it stands, this file does not contain standards-compliant
Fortran because it has a number of lines that are more than 132
characters in length. There are two possible solutions to this: tell
PSyclone that it must limit the length of output lines or tell your
Fortran compiler to allow non-standard line lengths. Since not all
Fortran compilers allow the line-length limit to be ignored, we use
PSyclone:

    $ psyclone -api nemo -opsy psy.f90 -l output tra_adv.F90

(Note that if we also wanted PSyclone to validate that the incoming
code was standards compliant then we could specify `-l all` instead).

Compiling the generated code is then as simple as (assuming gfortran is
the Fortran compiler):

    $ gfortran -o tra_adv.exe psy.f90

The mini-app picks-up the domain size and number of iterations from
environment variables. The file ../domain_setup.sh contains example
settings for bash and ../domain_setup.csh is the equivalent if you are
using csh or tcsh. You can either cut-n-paste the commands into your
shell or do (for csh):

    $ source ../domain_setup.csh

or (for bash):

    $ . ../domain_setup.sh

Once the environment variables are set, you are ready to execute the
mini-app:

    $ ./tra_adv.exe 
    Tracer-advection Mini-app:
    Domain is  100x 100 grid points
    Performing   10 iterations
    Mini-app finished.
