# PSyclone GOcean PSyData Profiling Example

## Introduction

This is a very simple test that shows how to use the profiling
support in PSyclone. It is a stand alone program that can be compiled
and run. 

## Compilation
The makefile supports compiling and linking with the following PSyclone
profile wrapper libraries:
- template
- simple_timing
- dl_timer
- drhook
- lfric
- tau
- vernier

By default (``make`` without an argument) the ``template`` library will 
be used, which just prints the name of the regions called.
In order to test any of the other libraries, just use the
command ``make <wrapper library name>`` and use the name listed above
for ``<wrapper library name>``. The name of the executable will be
``profile_test.<wrapper library name>``. There is also a target ``make all``
which will create executables for all libraries listed above.    

You have to compile the GOcean infrastructure library
dl_esm_inf, and the corresponding profile wrapper library in
``lib/profiling``. By default, the compilation uses the version
of the dl_esm_inf library provided as a git submodule (under
``external/dl_esm_inf ``- see
https://psyclone.readthedocs.io/en/latest/developer_guide/working_practises.html)
within the PSyclone repository (set the environment variable``INF_DIR``
for the ``make`` command to pick a different version). The default build
uses the "template" profiling library in ``lib/profiling/template``.
More detailed instructions for compiling these libraries are are given in
the corresponding subdirectories.

If you are using the TAU profiling library, you need to install
it yourself, and make sure that the ``tau_f90.sh`` compiler wrapper
is in your path. The Makefile will automatically call ``tau_f90.sh``, there
is no need to set ``$F90`` in this case.

If you are using ``dl_timer``, ``vernier`` or ``drhook``, you need to
compile these libraries yourself first, and modify the ``Makefile`` in
this directory to specify the required linking parameters. The ``Makefile``
supports the following environment variables that can be defined
to find the various software packages:

### INF_DIR:
The location of the dl_esm_inf infrastructure library, it defaults to
``../../../../external/dl_esm_inf/finite_difference``,
which is the version included in PSyclone.

### DL_TIMER_ROOT:
The location of the dl_timer library. It defaults to
``../../../../../dl_timer``, i.e. it is assumed that dl_timer
is installed next to PSyclone.

### DRHOOK_DIR:
The location of DrHook. It defaults to
``../../../../../drhook``, i.e. it is assumed that DrHook is
installed next to PSyclone.

### LFRIC_DIR
The location of the LFRic infrastructure library. It defaults to
``../../../../external/lfric_infrastructure/src``,
which is a version of the LFRic infrastructure library that
is included in PSyclone. In spite of the dependence on LFRic, the
LFRic timer profiling wrapper library can be used with with any application.
Note that this variable is not directly used by this Makefile,
but the lfric_timer wrapper library will use (and compile if
required) the LFRic infrastructure files.

### VERNIER_DIR
The location of the Vernier library. It defaults to
``../../../../../Vernier``, i.e. it is assumed that Vernier is
installed next to PSyclone.

The makefile here will invoke psyclone with the ``--profile invokes``
flag, which will add profiling around the two invokes used in the example.

### Note:
The actual runtime is extremely short, so likely the profiling
tool used will report 0 seconds for each of the invokes.

### Note for LFRic wrapper library
The LFRic timer library writes its output to a file ``timer.txt``
(and it will overwrite this file if it should already exist).

## Running
The output will depend on the wrapper library used. For the ``template``
library, you should see:
```
 ...
 profile_PSyDataInit called
 ...
 PreStart called for module 'psy_test' region 'invoke_0-r0'
 PostEnd called for module 'psy_test' region 'invoke_0-r0'
 PreStart called for module 'psy_test' region 'invoke_1_update_field-r0'
 PostEnd called for module 'psy_test' region 'invoke_1_update_field-r0'
 ...  
 profile_PSyDataShutdown called
```
