# Simple Stand-alone Timer Library

This library is a simple stand-alone timer library. It counts
the number of calls for each region, and reports minumum, maximum
and average times. This library is not thread-safe, and not
MPI aware (e.g. maximum reported is per process, not across all
processes).

## Dependencies

This stand-alone profiling library is based on the PSyData base class,
which is included in PSyclone as a Jinja template (see the
[developer's guide](https://psyclone-dev.readthedocs.io/en/latest/psy_data.html#psydata-base-class)
). Since the profiling API does not need access to any fields or variables,
only the static subroutines and ``PreStart`` and ``PostEnd`` are implemented,
the ``PreDeclare`` and ``ProvideVariable`` methods are not created at all.

It uses the ProfileData type to store the module/region name (done by the base
class) and then stores timing and counting information in this derived type.


## Compilation
A makefile is provided for compilation. The environment variables
``$F90`` and ``$F90FLAGS`` can be set to point to the Fortran compiler
and flags to use. They default to ``gfortran`` and the empty string.

```sh
F90=gfortran F90FLAGS=-g make
```

In order to link this timer library with your application, you must provide the
location of this library as an include path (so that the module file is found),
and specify the library at link time:

```sh
gfortran -c  -I PATH-TO-PSYCLONE/lib/profiling/simple_timing some_file.f90
gfortran some_file.o -L PATH-TO-PSYCLONE/lib/profiling/simple_timing -lsimple_timing
```

Sample output:

```
===========================================
module::region                                         count           sum                     min             average                 max
psy_inputoutput::eliminate_one_node_islands_code           1     0.128906250             0.128906250             0.128906250             0.128906250    
psy_time_step_mod::swlon_adjust_code                      11      1.19921875             0.105468750             0.109019883             0.113281250    
psy_time_step_mod::swlon_code                             11      4.38281250             0.394531250             0.398437500             0.406250000    
psy_time_step_mod::swlon_update_code                      11      1.86718750             0.167968750             0.169744313             0.171875000    
psy_time_step_mod::swlat_adjust_code                      11      1.23828125             0.109375000             0.112571023             0.117187500    
psy_time_step_mod::swlat_code                             11      4.87890625             0.437500000             0.443536937             0.445312500    
psy_time_step_mod::swlat_update_code                      11      1.87500000             0.167968750             0.170454547             0.179687500    
===========================================
```
