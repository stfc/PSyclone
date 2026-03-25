# Runnable Examples

**Author:** J. Henrichs, Bureau of Meteorology

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

<!--
## Licence

-----------------------------------------------------------------------------

BSD 3-Clause License

Copyright (c) 2021-2026, Science and Technology Facilities Council.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.

* Neither the name of the copyright holder nor the names of its
  contributors may be used to endorse or promote products derived from
  this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.

------------------------------------------------------------------------------
-->
