# AMD ROCTx Wrapper

This is a wrapper library that maps the [PSyclone profiling API](
https://psyclone.readthedocs.io/en/latest/user_guide/profiling.html#profiling) to the
AMD ROCTx library. ROCTx provides code annotation capabilities for profiling GPU applications
with AMD's ROCm profiling tools.

Unlike some of the other profiling tools, the use of this library does *not* require
that calls to ``profile_PSyDataInit()`` and ``profile_PSyDataShutdown()`` be inserted
into the application.

This wrapper supports the ``profile_PSyDataStart()`` and ``profile_PSyDataStop()`` API
calls that may be used in order to limit the region of code that is profiled at runtime.
These use the ROCTx ``roctxProfilerPause()`` and ``roctxProfilerResume()`` functions.

## Dependencies

This wrapper uses the **rocprofiler-sdk-roctx** library from ROCm, which provides
the full ROCTx API including profiler control functions. The library
(``librocprofiler-sdk-roctx.so``) is required at link time and runtime.

The ROCTx library is typically located at:
- ``$ROCM_PATH/lib/librocprofiler-sdk-roctx.so``

For documentation on ROCTx, see:
- https://rocm.docs.amd.com/projects/rocprofiler-sdk/en/latest/how-to/using-rocprofiler-sdk-roctx.html

## Compilation

A ``Makefile`` is provided and just executing `make` should build the wrapper
library. By default the ``amdflang`` compiler is used but ``gfortran`` works too.
Running ``make`` will produce ``libroctx_prof.a`` and ``profile_psy_data_mod.mod``.

When compiling the application that has been instrumented for profiling, the
location of the ``profile_psy_data_mod.mod`` file must be provided as an
include/module path, e.g. ``-I/path/to/psyclone/lib/profiling/amd``.

### Linking the wrapper library

At the link stage, the location of the wrapper library AND the ROCTx library
must be provided:

```shell
amdflang <my object files> \
    -L<PATH-TO-PSYCLONE>/lib/profiling/amd -lroctx_prof \
    -L$ROCM_PATH/lib -lrocprofiler-sdk-roctx
```

**Note**: The ``<PATH-TO-PSYCLONE>`` differs depending on whether the
wrapper library is compiled in a clone of PSyclone repository or in a
PSyclone [installation](./../../README.md#installation).

## Profiling Your Application

Once the application has been built with ROCTx instrumentation, it may be
profiled using AMD's profiling tools:

### Using rocprofv3 (recommended for ROCm 6.0+)

```shell
# Trace marker regions
rocprofv3 --marker-trace --output-format pftrace -- ./your_app
```

This generates a ``marker_api_trace.csv`` file (prefixed with process ID) containing:
- Domain: MARKER_CORE_API
- Function: The region name (module:region format)
- Process_Id, Thread_Id
- Start_Timestamp, End_Timestamp (in nanoseconds)

To collect not just ROCTx markers, but also kernel, memory copy, and HIP API traces, use `--runtime-trace` instead. For more
detailed usage of AMD profiling tools, refer to the AMD profiling guide series:
- [Performance Profiling on AMD GPUs – Part 1: Foundations](https://rocm.blogs.amd.com/software-tools-optimization/profiling-guide/intro/README.html)
- [Performance Profiling on AMD GPUs – Part 2: Basic Usage](https://rocm.blogs.amd.com/software-tools-optimization/profiling-guide/novice/README.html)
- [Performance Profiling on AMD GPUs – Part 3: Advanced Usage](https://rocm.blogs.amd.com/software-tools-optimization/profiling-guide/advanced/README.html)

## ROCTx API Reference

The wrapper uses the following ROCTx functions:

| ROCTx Function | PSyclone API | Description |
|---------------|--------------|-------------|
| `roctxRangePushA()` | `PreStart()` | Start a named profiling range |
| `roctxRangePop()` | `PostEnd()` | End the current profiling range |
| `roctxProfilerPause()` | `profile_PSyDataStop()` | Pause profiling |
| `roctxProfilerResume()` | `profile_PSyDataStart()` | Resume profiling |
