.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2018-2020, Science and Technology Facilities Council.
.. All rights reserved.
..
.. Redistribution and use in source and binary forms, with or without
.. modification, are permitted provided that the following conditions are met:
..
.. * Redistributions of source code must retain the above copyright notice, this
..   list of conditions and the following disclaimer.
..
.. * Redistributions in binary form must reproduce the above copyright notice,
..   this list of conditions and the following disclaimer in the documentation
..   and/or other materials provided with the distribution.
..
.. * Neither the name of the copyright holder nor the names of its
..   contributors may be used to endorse or promote products derived from
..   this software without specific prior written permission.
..
.. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
.. "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
.. LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
.. FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
.. COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
.. INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
.. BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
.. LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
.. CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
.. LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
.. ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
.. POSSIBILITY OF SUCH DAMAGE.
.. -----------------------------------------------------------------------------
.. Written by J. Henrichs, Bureau of Meteorology
.. Modified by A. R. Porter, STFC Daresbury Lab
.. Modified by R. W. Ford, STFC Daresbury Lab

.. _profiling:

Profiling
=========
PSyclone has the ability to define regions that can be profiled
with various performance measurement tools. The profiling can
be enabled automatically using command line parameters like::

    psyclone --profile kernels ...

Or, for finer-grained control, it may be applied via a profiling
transformation within a transformation script.


PSyclone can be used with a variety of existing profiling tools.
It currently supports dl_timer, Dr Hook, the nvidia GPU profiling toos
and it comes with a simple
stand-alone timer library. The PSyData API (:ref:`dev_guide:psy_data`)
is utilised to implement wrapper libraries that connect the PSyclone
application to the profiling libraries. Certain adjustments to
the application's build environment are required:

- The compiler needs to be able to find the module files for the
  wrapper of the selected profiling library.
- The application needs to be linked with the wrapper library
  that interfaces between the PSyclone API and the
  tool-specific API.
- The tool-specific library also needs to be linked in.

It is the responsibility of the user to supply the corresponding
compiler command line options when building
the application that incorporates the PSyclone-generated code.


.. _profiling_third_party_tools:

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
Below are short descriptions of each of the various wrapper
libraries that come with PSyclone:

``lib/profiling/template``
    This is a simple library that just prints out the name
    as regions are entered and exited. It could act as a
    template to develop new wrapper libraries, hence its
    name.

``lib/profiling/simple_timing``
    This is a simple, stand-alone library that uses Fortran
    system calls to measure the execution time, and reports
    average, minimum and maximum execution time for all regions.
    It is not MPI aware (i.e. it will just report independently
    for each MPI process), and not thread-safe.

``lib/profiling/dl_timer``
    This wrapper uses the apeg-dl_timer library. In order to use
    this wrapper, you must download and install the dl_timer library
    from ``https://bitbucket.org/apeg/dl_timer``. This library has
    various compile-time options and may be built with MPI or OpenMP
    support. Additional link options might therefore be required
    (e.g. enabling OpenMP, or linking with MPI).

``lib/profiling/drhook``
    This wrapper uses the DrHook library. You need to contact
    ECMWF to obtain a copy of DrHook.

``lib/profiling/nvidia``
    This is a wrapper library that maps the PSyclone profiling API
    to the NVIDIA Tools Extension library (NVTX). This library is
    available from ``https://developer.nvidia.com/cuda-toolkit``.


Any user can create similar wrapper libraries for
other profiling tools by providing a corresponding Fortran
module. The functions that need to be implemented are described in
the developer's guide (:ref:`dev_guide:psy_data`).

Most libraries in ``lib/profiling`` need to be linked in
with the corresponding 3rd party profiling tool. The
exception is the template- and simple_timing-library,
which are stand alone. The profiling example in
``examples/gocean/eg5`` can be used with any of the
wrapper libraries (except nvidia) to see how they work.

.. _required_profiling_calls:

Required Modifications to the Program
-------------------------------------
In order to guarantee that any profiling library is properly
initialised, PSyclone's profiling wrappers utilise two additional
function calls that the user must manually insert into the program:

profile_PSyDataInit()
~~~~~~~~~~~~~~~~~~~~~
This method needs to be called once to initialise the profiling tool.
At this stage this call is not automatically inserted by PSyclone, so
it is the responsibility of the user to add the call to an appropriate
location in the application::

   use profile_psy_data_mod, only : profile_PSyDataInit
   ...
   call profile_PSyDataInit()

The 'appropriate' location might depend on the profiling library used. 
For example, it might be necessary to invoke this before or after
a call to ``MPI_Init()``.


profile_PSyDataShutdown()
~~~~~~~~~~~~~~~~~~~~~~~~~
At the end of the program the function ``profile_PSyDataShutdown()``
must be called.
It will make sure that the measurements are printed, files are flushed,
and that the profiling tool is closed correctly. Again at
this stage it is necessary to manually insert the call at an appropriate
location::

    use profile_psy_data_mod, only : profile_PSyDataShutdown
    ...
    call profile_PSyDataShutdown()

And again the appropriate location might depend on the profiling library
used (e.g. before or after a call to ``MPI_Finalize()``).



Profiling Command Line Options
------------------------------
PSyclone offers two command line options to automatically instrument
code with profiling regions. It can create profile regions around
a full invoke (including all kernel calls in this invoke), and/or
around each individual kernel. 

The option ``--profile invokes`` will automatically add calls to 
start and end a profile region at the beginning and end of every
invoke subroutine created by PSyclone. All kernels called within
this invoke subroutine will be included in the profiled region.

The option ``--profile kernels`` will surround each outer loop
created by PSyclone with start and end profiling calls.

.. note:: In some APIs (for example dynamo when using distributed
          memory) additional minor code might get included in a
          profiled kernel section, for example setDirty() calls
          (expensive calls like HaloExchange are excluded).

.. note:: It is still the responsibility of the user to manually
    add the calls to ``profile_PSyDataInit`` and 
    ``profile_PSyDataShutdown`` to the
    code base (see :ref:`required_profiling_calls`).

PSyclone will modify the schedule of each invoke to insert the
profiling regions. Below we show an example of a schedule created
when instrumenting invokes - all children of a Profile-Node will
be part of the profiling region, including all loops created by
PSyclone and all kernel calls (note that for brevity, the nodes
holding the loop bounds have been omitted for all but the first loop)::

    GOInvokeSchedule[invoke='invoke_1',Constant loop bounds=True]
        0: [Profile]
            Schedule[]
                0: Loop[type='outer',field_space='go_cu',it_space='go_internal_pts']
                    Literal[value:'2']
                    Literal[value:'jstop']
                    Literal[value:'1']
                    Schedule[]
                        0: Loop[type='inner',field_space='go_cu',
                                it_space='go_internal_pts']
                            ...
                            Schedule[]
                                0: CodedKern compute_unew_code(unew_fld,uold_fld,z_fld,
                                           cv_fld,h_fld,tdt,dy) [module_inline=False]
                1: Loop[type='outer',field_space='cv',it_space='internal_pts']
                    ...
                    Schedule[]
                        0: Loop[type='inner',field_space='cv',it_space='internal_pts']
                            ...
                            Schedule[]
                                0: CodedKern compute_vnew_code(vnew_fld,vold_fld,z_fld,
                                           cu_fld,h_fld,tdt,dy) [module_inline=False]
                2: Loop[type='outer',field_space='ct',it_space='internal_pts']
                    ...
                    Schedule[]
                        0: Loop[type='inner',field_space='ct',it_space='internal_pts']
                            ...
                            Schedule[]
                                0: CodedKern compute_pnew_code(pnew_fld,pold_fld,cu_fld,
                                           cv_fld,tdt,dx,dy) [module_inline=False]

And now the same schedule when instrumenting kernels. In this case
each loop nest and kernel call will be contained in a separate
region::

    GOInvokeSchedule[invoke='invoke_1',Constant loop bounds=True]
        0: [Profile]
            Schedule[]
                0: Loop[type='outer',field_space='go_cu',it_space='go_internal_pts']
                    ...
                    Schedule[]
                        0: Loop[type='inner',field_space='go_cu',
                                it_space='go_internal_pts']
                            ...
                            Schedule[]
                                0: CodedKern compute_unew_code(unew_fld,uold_fld,z_fld,
                                        cv_fld,h_fld,tdt,dy) [module_inline=False]
        1: [Profile]
            Schedule[]
                0: Loop[type='outer',field_space='go_cv',it_space='go_internal_pts']
                    ...
                    Schedule[]
                            0: Loop[type='inner',field_space='go_cv',
                                it_space='go_internal_pts']
                                ...
                                Schedule[]
                                    0: CodedKern compute_vnew_code(vnew_fld,vold_fld,z_fld,
                                        cu_fld,h_fld,tdt,dy) [module_inline=False]
        2: [Profile]
            Schedule[]
                0: Loop[type='outer',field_space='go_ct',it_space='go_internal_pts']
                    ...
                    Schedule[]
                        0: Loop[type='inner',field_space='go_ct',
                                it_space='go_internal_pts']
                            ...
                            Schedule[]
                                0: CodedKern compute_pnew_code(pnew_fld,pold_fld,
                                        cu_fld,cv_fld,tdt,dx,dy) [module_inline=False]

Both options can be specified at the same time::

    GOInvokeSchedule[invoke='invoke_1',Constant loop bounds=True]
        0: [Profile]
            Schedule[]
                0: [Profile]
                    Schedule[]
                        0: Loop[type='outer',field_space='go_cu',
                                it_space='go_internal_pts']
                            ...
                            Schedule[]
                                0: Loop[type='inner',field_space='go_cu',
                                        it_space='go_internal_pts']
                                    ...
                                    Schedule[]
                                        0: CodedKern compute_unew_code(unew_fld,uold_fld,
                                                ...) [module_inline=False]
                1: [Profile]
                    Schedule[]
                        0: Loop[type='outer',field_space='go_cv',
                                it_space='go_internal_pts']
                            ...
                            Schedule[]
                                    0: Loop[type='inner',field_space='go_cv',
                                        it_space='go_internal_pts']
                                        ...
                                        Schedule[]
                                            0: CodedKern compute_vnew_code(vnew_fld,vold_fld,
                                                ...) [module_inline=False]
                2: [Profile]
                    Schedule[]
                        0: Loop[type='outer',field_space='go_ct',
                                it_space='go_internal_pts']
                            ...
                            Schedule[]
                                0: Loop[type='inner',field_space='go_ct',
                                        it_space='go_internal_pts']
                                    ...
                                    Schedule[]
                                        0: CodedKern compute_pnew_code(pnew_fld,pold_fld,
                                                ...) [module_inline=False]


Profiling in Scripts - ``ProfileTrans``
---------------------------------------
The greatest flexibility is achieved by using the profiler
transformation explicitly in a transformation script. The script
takes either a single PSyIR Node or a list of PSyIR Nodes as argument,
and will insert a Profile Node into the PSyIR, with the 
specified nodes as children. At code creation time the
listed children will all be enclosed in one profile region.
As an example::

    from psyclone.psyir.transformations import ProfileTrans

    p_trans = ProfileTrans()
    schedule = psy.invokes.get('invoke_0').schedule
    schedule.view()
    
    # Enclose some children within a single profile region
    newschedule, _ = p_trans.apply(schedule.children[1:3])
    newschedule.view()

The profiler transformation also allows the profile name to be set
explicitly, rather than being automatically created (see
:ref:`profile_names` for details). This allows for potentially
more intuitive names or finer grain control over profiling
(as particular regions could be provided with the same profile
names). For example::

    invoke = psy.invokes.invoke_list[0]
    schedule = invoke.schedule
    profile_trans = ProfileTrans()
    # Use the actual psy-layer module and subroutine names.
    options = {"region_name": (psy.name, invoke.name)}
    profile_trans.apply(schedule.children, options=options)
    # Use own names and repeat for different regions to aggregate profile.
    options = {"region_name": ("my_location", "my_region")}
    profile_trans.apply(schedule[0].children[1:2], options=options)
    profile_trans.apply(schedule[0].children[5:7], options=options)

.. warning::

   If "region_name" is misspelt in the options dictionary then the
   option will be silently ignored. This is true for all
   options. Issue #613 captures this problem.
   
.. warning::
 
    It is the responsibility of the user to make sure that a profile
    region is only created inside a multi-threaded region if the
    profiling library used is thread-safe!

.. _profile_names:

Naming Profiling Regions
------------------------
A profile region derives its name from two components:

`module_name`
    A string identifying the psy-layer containing this 
    profile node.
`region_name`
    A string identifying the invoke containing 
    this profile node and its location within the invoke
    (where necessary).

By default PSyclone will generate appropriate names to uniquely
determine a particular region. Since those names can be
somewhat cryptic, alternative names can be specified by the user
when adding profiling via a transformation script, see
:ref:`dev_guide:psy_data_parameters_to_constructor`.

The automatic name generation depends on the API according
to the following rules:

For the `nemo` api,

* the `module_name` string is set to the name of the parent
  function/subroutine/program. This name is unique as Fortran requires
  these names to be unique within a program.

* the `region_name` is set to an `r` (standing for region) followed by
  an integer which uniquely identifies the profile within the parent
  function/subroutine/program (based on the profile node's position in
  the PSyIR representation relative to any other profile nodes).

For the `dynamo` and `gocean` api's,

* the `module_name` string is set to the module name of the generated
  PSy-layer. This name should be unique by design (otherwise module
  names would clash when compiling).

* the `region_name` is set to the name of the invoke in which it
  resides, followed by a `:` and a kernel name if the
  profile region contains a single kernel, and is completed by `:r`
  (standing for region) followed by an integer which uniquely
  identifies the profile within the invoke (based on the profile
  node's position in the PSyIR representation relative to any other
  profile nodes). For example::

    InvokeSchedule[invoke='invoke_0', dm=True]
      0: Profile[]
          Schedule[]
              0: Profile[]
                  Schedule[]
                      0: HaloExchange[field='f2', type='region', depth=1,
                                      check_dirty=True]
                      1: HaloExchange[field='m1', type='region', depth=1,
                                      check_dirty=True]
                      2: HaloExchange[field='m2', type='region', depth=1,
                                      check_dirty=True]
              1: Profile[]
                  Schedule[]
                      0: Loop[type='', field_space='w1', it_space='cells',
                              upper_bound='cell_halo(1)']
                          Literal[value:'1', DataType.INTEGER]
                          Literal[value:'mesh%get_last_halo_cell(1)',
                                  DataType.INTEGER]
                          Literal[value:'1', DataType.INTEGER]
                          Schedule[]
                              0: CodedKern testkern_code(a,f1,f2,m1,m2)
                                 [module_inline=False]
                      1: Profile[]
                          Schedule[]
                              0: Loop[type='', field_space='w1',
                                      it_space='cells',
                                      upper_bound='cell_halo(1)']
                                  Literal[value:'1', DataType.INTEGER]
                                  Literal[value:'mesh%get_last_halo_cell(1)',
                                          DataType.INTEGER]
                                  Literal[value:'1', DataType.INTEGER]
                                  Schedule[]
                                      0: CodedKern testkern_code(a,f1,f2,m1,m2)
                                         [module_inline=False]
              2: Loop[type='', field_space='w1', it_space='cells',
                      upper_bound='cell_halo(1)']
                  Literal[value:'1', DataType.INTEGER]
                  Literal[value:'mesh%get_last_halo_cell(1)', DataType.INTEGER]
                  Literal[value:'1', DataType.INTEGER]
                  Schedule[]
                      0: CodedKern testkern_qr_code(f1,f2,m1,a,m2,istp)
                         [module_inline=False]

This is the code created for this example::

     MODULE container
      CONTAINS
      SUBROUTINE invoke_0(a, f1, f2, m1, m2, istp, qr)
        ...
        CALL psy_data_3%PreStart("multi_functions_multi_invokes_psy", "invoke_0:r0", &
                                     0, 0)
        CALL psy_data%PreStart("multi_functions_multi_invokes_psy", "invoke_0:r1", 0, 0)
        IF (f2_proxy%is_dirty(depth=1)) THEN
          CALL f2_proxy%halo_exchange(depth=1)
        END IF 
        IF (m1_proxy%is_dirty(depth=1)) THEN
          CALL m1_proxy%halo_exchange(depth=1)
        END IF 
        IF (m2_proxy%is_dirty(depth=1)) THEN
          CALL m2_proxy%halo_exchange(depth=1)
        END IF 
        CALL psy_data%PreEnd()
        CALL psy_data_1%PreStart("multi_functions_multi_invokes_psy", "invoke_0:r2", &
                                     0, 0)
        DO cell=1,mesh%get_last_halo_cell(1)
          CALL testkern_code(...)
        END DO 
        ...
        CALL psy_data_2%PreStart("multi_functions_multi_invokes_psy", &
                          "invoke_0:testkern_code:r3", 0, 0)
        DO cell=1,mesh%get_last_halo_cell(1)
          CALL testkern_code(...)
        END DO 
        ...
        CALL psy_data_2%PostEnd()
        CALL psy_data_1%PostEnd()
        ...
        DO cell=1,mesh%get_last_halo_cell(1)
          CALL testkern_qr_code(...)
        END DO 
        ...
        CALL psy_data_3%PostEnd()
        ...
      END SUBROUTINE invoke_0
    END MODULE container

