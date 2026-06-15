# PSyclone GOcean PSyData Value Range Check Example

**Author:** J. Henrichs, Bureau of Meteorology

This is a simple example that shows how to use the value range check
support in PSyclone. It is a stand-alone program that can be compiled
and run. 

## Compilation

A simple makefile is provided to compile the example. The following
dependencies are needed:

- the infrastructure library ``liblfric.a`` provided in
  ``<PSYCLONEHOME>/external/lfric_infrastructure/src``
- the value range check library from
      ``<PSYCLONEHOME>/lib/value_range_check/lfric``

The infrastructure and value range check wrapper libraries will be compiled
if they are not available.

The following environment variables can be set to define the compiler
you want to use:
```shell
export F90=gfortran
export F90FLAGS="-Wall -g -fcheck=bound"
```
## Running

The binary can be executed using ``value_range_check``. You need to set
the environment variable ``$PSY_VALUE_RANGE`` (see
[Value Range Check](https://psyclone.readthedocs.io/en/latest/user_guide/psy_data.html#psydata-value-range-check) in the PSyclone manual for details.


```shell
PSY_VALUE_RANGE="field1_data=0:7" ./value_range_check
PSyData: Variable 'field1_data' has the value '8.0000000000000000' at index/indices 77 in module 'main_psy', region 'invoke_testkern_w0-testkern_w0_code-r0', which is not between '0.0000000000000000' and '7.0000000000000000'.
...
 Mesh has           5 layers.
 minmax of field1   1.0000000000000000        8.0000000000000000     

```

Note that the value range for ``field1`` was set to be less than 8 on purpose
to trigger this message (``field1`` is expected to become 8).
