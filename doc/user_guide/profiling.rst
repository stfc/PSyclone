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
It currently supports dl_timer, Dr Hook, and comes with a simple
stand-alone timer library.
An application needs to be able to find the module files for the 
profile wrapper, and needs to be linked with the included wrapper
library that interfaces between the PSyclone API and the
tool-specific API. It is the responsibility of the user to
supply the corresponding compiler command line options when building
the application that incorporates the PSyclone-generated code.


.. _ProfilingAPI:

Profiling API
-------------
In order to be used with different profiling tools, PSyclone supports
a simple profiling API. For each existing profiling tool a simple interface
library needs to be implemented that maps the PSyclone profiling calls
to the corresponding call for the profiling tool. 

PSyclone utilises 4 profiling calls which are described in the following
sub-sections.

ProfileInit()
~~~~~~~~~~~~~
This method needs to be called once to initialise the profiling tool.
At this stage this call is not automatically inserted by PSyclone, so
it is the responsibility of the user to add the call to an appropriate
location in the application::

   use profile_mod, only : ProfileInit
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
this stage it is necessary to manually insert the call at an appropriate
location::

    use profile_mod, only : ProfileFinalise
    ...
    call ProfileFinalise()

And again the appropriate location might depend on the profiling library
used (e.g. before or after a call to ``MPI_Finalize()``).


ProfileStart()/ProfileEnd()
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The ``ProfileStart`` and ``ProfileEnd`` functions define the beginning and
end of a region to be measured. 
In general it is up to the user what exactly a region is, arbitrary code
can be enclosed, as long as it is guaranteed that each call of
``ProfileStart`` is matched with exactly one corresponding call to
``ProfileEnd``. PSyclone supplies one saved (static) variable of type
``ProfileData`` to each matching Start/End pair.

This is the code sequence which is created by PSyclone::

    use profile_mod, only : ProfileData, ProfileStart, ProfileEnd
    ...
    type(ProfileData), save :: profiler_data
    ...
    call ProfileStart("Module", "Region", profiler_data)
    ...
    call ProfileEnd(profiler_data)

PSyclone guarantees that different ProfileStart/End pairs have
different ``ProfileData`` variables.


Profiling Command Line Options
------------------------------
PSyclone offers two command line options to automatically instrument
code with profiling regions. It can create profile regions around
a full invoke (including all kernel calls in this invoke), and/or
around each individual kernel. 

The option ``--profile invokes`` will automatically add a call to 
``ProfileStart`` and ``ProfileEnd`` at the beginning and end of every
invoke subroutine created by PSyclone. All kernels called within
this invoke subroutine will be included in the profiled region.

The option ``--profile kernels`` will add a call to ``ProfileStart``
before any loops created by PSyclone, and a ``ProfileEnd``
call at the end of the loop.  Two caveats:

1. In some APIs (for example dynamo when using distributed
   memory) additional minor code might get included in a
   profiled kernel section, for example setDirty() calls
   (expensive calls like HaloExchange are excluded). 

2. If transformations are applied using a script, the profiling nodes
   added to the PSyIR could be applied to the wrong location and cause
   errors.

In order to avoid the second issue, automatic profiling using
``--profile`` is not allowed together with a transformation
script. On the other hand, since it is possible to write scripts
that are more flexible in handling a modified PSyIR, you can use the
command line option ``--force-profile``. It takes the same
parameters as ``--profile``, and will allow you to combine a
transformation script together with automatic profiling. Use
this option at your own risk!

It is also the responsibility of the user to manually add
the calls to ``ProfileInit`` and ``ProfileFinalise`` to
the code base.

PSyclone will modify the schedule of each invoke to insert the
profiling regions. Below we show an example of a schedule created
when instrumenting invokes - all children of a Profile-Node will
be part of the profiling region, including all loops created by
PSyclone and all kernel calls::

    GOInvokeSchedule[invoke='invoke_1',Constant loop bounds=True]
        [Profile]
            Loop[type='outer',field_space='cu',it_space='internal_pts']
                Loop[type='inner',field_space='cu',it_space='internal_pts']
                    CodedKern compute_unew_code(unew_fld,uold_fld,z_fld,cv_fld,h_fld,tdt,dy) [module_inline=False]
            Loop[type='outer',field_space='cv',it_space='internal_pts']
                Loop[type='inner',field_space='cv',it_space='internal_pts']
                    CodedKern compute_vnew_code(vnew_fld,vold_fld,z_fld,cu_fld,h_fld,tdt,dy) [module_inline=False]
            Loop[type='outer',field_space='ct',it_space='internal_pts']
                Loop[type='inner',field_space='ct',it_space='internal_pts']
                    CodedKern compute_pnew_code(pnew_fld,pold_fld,cu_fld,cv_fld,tdt,dx,dy) [module_inline=False]

And now the same schedule when instrumenting kernels. In this case
each loop nest and kernel call will be contained in a separate
region::

    GOInvokeSchedule[invoke='invoke_1',Constant loop bounds=True]
        [Profile]
            Loop[type='outer',field_space='cu',it_space='internal_pts']
                Loop[type='inner',field_space='cu',it_space='internal_pts']
                    CodedKern compute_unew_code(unew_fld,uold_fld,z_fld,cv_fld,h_fld,tdt,dy) [module_inline=False]
        [Profile]
            Loop[type='outer',field_space='cv',it_space='internal_pts']
                Loop[type='inner',field_space='cv',it_space='internal_pts']
                    CodedKern compute_vnew_code(vnew_fld,vold_fld,z_fld,cu_fld,h_fld,tdt,dy) [module_inline=False]
        [Profile]
            Loop[type='outer',field_space='ct',it_space='internal_pts']
                Loop[type='inner',field_space='ct',it_space='internal_pts']
                    CodedKern compute_pnew_code(pnew_fld,pold_fld,cu_fld,cv_fld,tdt,dx,dy) [module_inline=False]

Both options can be specified at the same time::

    GOInvokeSchedule[invoke='invoke_1',Constant loop bounds=True]
        [Profile]
            [Profile]
                Loop[type='outer',field_space='cu',it_space='internal_pts']
                    Loop[type='inner',field_space='cu',it_space='internal_pts']
                        CodedKern compute_unew_code(unew_fld,uold_fld,z_fld,cv_fld,h_fld,tdt,dy) [module_inline=False]
            [Profile]
                Loop[type='outer',field_space='cv',it_space='internal_pts']
                    Loop[type='inner',field_space='cv',it_space='internal_pts']
                        CodedKern compute_vnew_code(vnew_fld,vold_fld,z_fld,cu_fld,h_fld,tdt,dy) [module_inline=False]
            [Profile]
                Loop[type='outer',field_space='ct',it_space='internal_pts']
                    Loop[type='inner',field_space='ct',it_space='internal_pts']
                        CodedKern compute_pnew_code(pnew_fld,pold_fld,cu_fld,cv_fld,tdt,dx,dy) [module_inline=False]



Profiling in Scripts - ProfileRegionTransform
---------------------------------------------
The greatest flexibility is achieved by using the profiler
transformation explicitly in a transformation script. The script
takes either a single PSyIR Node or a list of PSyIR Nodes as argument,
and will insert a Profile Node into the PSyIR, with the 
specified nodes as children. At code creation time the
listed children will all be enclosed in one profile region.
As an example::

    from psyclone.transformations import ProfileRegionTrans

    t=TransInfo()
    p_trans= ProfileRegionTrans()
    schedule=psy.invokes.get('invoke_0').schedule
    schedule.view()
    
    # Enclose all children within a single profile region
    newschedule, _ = p_trans.apply(schedule.children[1:3])
    newschedule.view()

.. warning::
 
    It is the responsibility of the user to make sure that a profile
    region is only created inside a multi-threaded region if the
    profiling library used is thread-safe!


Interface to Third Party Profiling Tools 
----------------------------------------
PSyclone comes with wrapper libraries to support usage of
Dr Hook, dl_timer, NVTX (NVIDIA Tools Extension library),
and a simple non-thread-safe timing
library. Support for further profiling libraries will be
added in the future. To compile the wrapper libraries,
change into the directory ``lib/profiling`` of PSyclone
and type ``make`` to compile all wrappers. If only some
of the wrappers are required, you can either use
``make wrapper-name`` (e.g. ``make drhook``), or change
into the corresponding directory and use ``make``. The
corresponding README files contain additional parameters
that can be set in order to find third party profiling tools.

Any user can create similar wrapper libraries for
other profiling tools by providing a corresponding Fortran
module. The four profiling calls described
in the section about the ProfilingAPI_ must be implemented,
and an opaque, user-defined type ``ProfileData`` needs to be 
provided in the module.

Note that the ``ProfileEnd`` call does not have the module
or region name as an argument. If this is
required by the profiling library, this data must
be stored in the ``ProfileData`` object so that it is
available in the ``ProfileEnd`` call.

The examples in the lib/profiling directory show various ways
in which the opaque data type can be used to interface
with existing profiling tools - for example by storing 
an index used by the profiling tool in ``ProfileData``, or 
by storing pointers to the profiling data to be able to 
print all results in a ProfileFinalise() subroutine.

