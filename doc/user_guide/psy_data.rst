.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2019-2020, Science and Technology Facilities Council.
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

.. highlight:: fortran

.. _psy_data:

PSyData API
===========
PSyclone provides transformations that will insert callbacks to
an external library at runtime. These callbacks allow third-party
libraries to access data structures at specified locations in the
code. Some example use cases are:

Profiling:
  By inserting callbacks before and after a region of code,
  performance measurements can be added. PSyclone provides
  wrapper libraries for some common performance profiling tools,
  see :ref:`profiling` for details.

Kernel Data Extraction:
  PSyclone provides the ability to add callbacks that provide access
  to all input variables before, and output variables after a kernel
  invocation. This can be used to automatically create tests for
  a kernel, or to write a stand-alone driver that just calls one
  kernel, which can be used for performance tuning. An example
  library that extracts input- and output-data into a netcdf file
  is included with PSyclone (see :ref:`psyke_netcdf`).

In-situ Visualisation:
  By giving access to output fields of a kernel, an in-situ visualisation
  library can be used to plot fields while a (PSyclone-processed)
  application is running. There is no example library available at
  this stage, but the API has been designed with this application in mind.

Access Verification:
  The callbacks can be used to make sure a field declared as read-only
  is not modified during a kernel (either because of an incorrect
  declaration, or because memory is overwritten). A checksum can be
  used to detect any changes to a read-only field. Again, this is 
  a feature for the future and not available at this stage.

The PsyData API should be general enough to allow these and other
applications to be developed and used.

PSyclone provides transformations that will insert callbacks to
the PSyData API, for example ``ProfileTrans``, ``GOceanExtractTrans``
and ``LFRicExtractTrans``. A user can develop additional transformations
and corresponding runtime libraries for additional functionality.
Refer to :ref:`dev_guide:psy_data` for full details about the PSyData API.
