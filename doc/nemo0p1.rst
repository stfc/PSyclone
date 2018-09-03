.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2017-2018, Science and Technology Facilities Council
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
.. Written by A. R. Porter, STFC Daresbury Lab
      
.. _nemo0.1-api:

NEMO 0.1 API
============

In contrast to the other APIs supported by PSyclone, the NEMO API is
designed to work with source code that does *not* follow the PSyKAl
separation of concerns. Instead, the NEMO source code is treated as if
it were a manually written PSy layer with all kernels in-lined. This
approach relies upon the NEMO coding standards in order to reason
about the code being processed. Rather than construct a Schedule for
the PSy layer from scratch (as is done for other APIs), the Schedule
is constructed by parsing the supplied Fortran code and generating a
higher-level representation.

Algorithm
---------

Since NEMO source is treated as a pre-existing PSy layer, this API
does not have the concept of an Algorithm layer.

Example
-------

A typical fragment of NEMO source code (taken from the traldf_iso
routine) is shown below::

        DO jn = 1, kjpt
         zdit (1,:,:) = 0._wp     ;     zdit (jpi,:,:) = 0._wp
         zdjt (1,:,:) = 0._wp     ;     zdjt (jpi,:,:) = 0._wp

         DO jk = 1, jpkm1
            DO jj = 1, jpjm1
               DO ji = 1, fs_jpim1
                  zdit(ji,jj,jk) = ( ptb(ji+1,jj  ,jk,jn) - ptb(ji,jj,jk,jn) ) * umask(ji,jj,jk)
                  zdjt(ji,jj,jk) = ( ptb(ji  ,jj+1,jk,jn) - ptb(ji,jj,jk,jn) ) * vmask(ji,jj,jk)
               END DO
            END DO
         END DO

PSyclone uses fparser to parse such source code and then generates an internal representation of it::
  
    Loop[type='tracers',field_space='None',it_space='None']
         Loop[type='levels',field_space='None',it_space='None']
             Loop[type='lat',field_space='None',it_space='None']
                 KernCall[Implicit]
         Loop[type='levels',field_space='None',it_space='None']
             Loop[type='lat',field_space='None',it_space='None']
                 KernCall[Implicit]
         Loop[type='levels',field_space='None',it_space='None']
             Loop[type='lat',field_space='None',it_space='None']
                 KernCall[Implicit]
         Loop[type='levels',field_space='None',it_space='None']
             Loop[type='lat',field_space='None',it_space='None']
                 KernCall[Implicit]
         Loop[type='levels',field_space='None',it_space='None']
             Loop[type='lat',field_space='None',it_space='None']
                 Loop[type='lon',field_space='None',it_space='None']
                     KernCall[]
