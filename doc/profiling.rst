.. _profiling:

Profiling
=========
PSyclone has the ability to define regions that can be profiled
with various performance measurement tools. The profiling can
be enabled automatically using command line parameters like::

    psyclone --profile kernels ...

Or more fine grained by applying a profiling transformation in
a transformation script.

PSyclone can be used with a variety of existing profiling tools.
It currently supports TAU, Dr Hook, dl_timer, and even comes
with a simple stand-alone timer library.
An application needs to be linked with the included wrapper
library that interfaces between the PSyclone API and the
tool-specific API.


Profiling API
-------------
In order to be used with different profiling tools, PSyclone supports
a simple API. For each existing profling tools a simple interface
library needs to be implemented that maps the PSyclone profiling calls
to the corresponding call for the profiling tool. 

PSyclone utilises 4 profiling calls:

ProfileInit()
~~~~~~~~~~~~~
This method needs to be called once to initialise the profiling tool.
At this stage this call is not automatically inserted by PSyclone, so
it is the responsibility of the user to add the call to an appropriate
location in the application::

   use profiler_mod, only : ProfileInit
   ...
   call ProfileInit()

The 'appropriate' location might depend on the profiling library used. 
For example, it might be necessary to invoke this before or after
a call to ``MPI_Init()``.


ProfileFinalise()
~~~~~~~~~~~~~~~~~
At the end of the program the function ``ProfileFinalise()`` must be called.
It will make sure that the measurements are printed or written to file
correctly, and that the profiling tool is closed correctly. Again at
this stage iit is necessary to manually insert the call at an appropriate
location::

    use profiler_mod, only : ProfileFinalise
    ...
    call ProfileFinalise()

And still the appropriate location might depend on the profiling library
used (e.g. before or after a call to ``MPI_Init()``).


ProfileStart()
~~~~~~~~~~~~~~
The ``ProfileStart`` functions defines the begin of a region to be measured. 
In general it is up to the user what exactly a region is. Calls to
``ProfileStart`` should
not be manually inserted into the code. Either use a PSyclone command
line option or the profiling transform.

This is the code sequence which is created by PSyclone::

    use profiler_mod, only : ProfileData, ProfilerStart
    ...
    type(ProfileData), save :: profiler_data
    ...
    call ProfileStart("Module", "Region", profiler_data)

The profiler_data argument is static (saved), so it can be used to
store data used by the underlying tool.


ProfileEnd()
~~~~~~~~~~~~~
The ``ProfileEnd`` functions define the end of a region to be measured.
Each ``ProfileStart`` must have exactly one corresponding call to
``ProfileEnd``. In general it is up to the user to define exactly what
a region is. Calls to ``ProfileEnd`` should not be manually inserted 
into the code, instead use either a PSyclone command line option or
the profile transform.

This is the full code sequence created by PSyclone for a region::

    use profiler_mod, only : ProfileData, ProfileStart, ProfileEnd
    ...
    type(ProfileData), save :: profile
    ...
    call ProfileStart("Module", "Region", profile)
    ...
    call ProfileEnd(profile)


Profiling Command Line Options
------------------------------
PSyclone offers two command line options to automatically instrument
code with profiling regions. It can create profile regions around
a full invoke (including all kernel calls in this invoke), and/or
around each individual invoke. 

The option ``--profile invokes` will automatically add a call to 
``ProfileStart`` and ProfileEnd`` at the begin and end of every
invoke subroutine created by PSyclone. All kernels called within
this invoke subroutine will be included in the profiled region.

The option ``--profile kernels` will add call to ``ProfileStart``
calls before any loops created by PSyclone, and a ``ProfileEnd``
call at the end of the loop.  Two caveats::

    1. in some APIs (for example dynamo when using distributed
       memory) additional minor codemight get included in a
       profiled kernel section, for example setDirty() calls
       (expensive calls like HaloExchange will be  excluded). 
    2. If loop transforms are applied using a script, the
       ability to automatically insert profiling calls is
       reduced. It is recommended to only use automatic
       profiling of kernels without usage of additional
       transform, or explicitly add the profiling transform
       as part of the transformation script.

PSyclone will modify the schedule to insert the profiling regions.
Here an example of a schedule created when instrumenting invokes
- all children of a Profile-Node will be part of the profiling region::

And here the same schedule when instrumenting kernels::

Both options can be specified at the same time::



Profiling Transform
-------------------
How to use the transform

Profiling API
-------------
PSyclone comes with wrapper libraries to support usage of
TAU, DrHook, dl_timer and a simple non-thread-safe timing
library. 

Any user can create similar wrapper libraries for
other profiling tools by providing a corresponding Fortran
module. The four profiling calls described
in section `SOMEWHERE` needs to be implemented, and an
opaque, user-defined type ``ProfileData`` needs to be 
provided in the module.

The examples in the contrib directory show various ways
of how the opaque data type can be used to interface
with existing profiling tools - for example by storing 
an index used by the profiling tool, or how to store pointers
to the profiling data to be able to print all results in
a ProfileFinalise() subroutine.

