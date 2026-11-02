# PSyData Examples

This directory contains all examples that use the PSyData API. They show:
- [Extraction](./extract) of kernel input and output parameters.
- The use of various [profiling](./profile) tools outside of OpenMP regions.
- The use of various [openmp-profiling](./omp_profile) tools when applied
  inside an OpenMP region, i.e. the profiling library is called in parallel
  by threads.
- Verification that parameters declared [read-only](./readonly) are indeed
  not changed in a kernel.
- Verification that specified variables in a kernel are within a user-specified
  range using the [ValueRangeCheck](./value_range_check) transformation.

Detailed instructions are in the various subdirectories.
