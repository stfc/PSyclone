# Runnable Examples

This directory contains some standalone, runnable examples of LFRic code.
They are based on the LFRic infrastructure library included in
``<PSYCLONEHOME>/external/lfric_infrastructure/src``.

The examples in this subdirectory show:
- A [full_example](./full_example) of a stand-alone LFRic-based code. It
  shows the use of the infrastructure library to create LFRic code.
  PSyclone is used to process two invoke statements.
- A stand-alone example using [NetCDF](./full_example_netcdf) to read
  in a mesh.
- [Extraction](./full_example_extract) of input and output parameters
  of a kernel to a NetCDF file.

Detailed instructions are in the ``README.md`` files in the corresponding
subdirectories.
