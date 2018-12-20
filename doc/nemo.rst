.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2018, Science and Technology Facilities Council
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
      
.. _nemo-api:

NEMO API
========

In contrast to the other APIs supported by PSyclone, the NEMO API is
designed to work with source code that does *not* follow the PSyKAl
separation of concerns. Instead, the NEMO source code is treated as if
it were a manually written PSy layer with all kernels in-lined. This
approach relies upon the NEMO Coding Conventions
:cite:`nemo_code_conv` in order to reason about the code being
processed. Rather than construct a Schedule for the PSy layer from
scratch (as is done for other APIs), the Schedule is constructed by
parsing the supplied Fortran code and generating a higher-level
representation.

.. note:: the NEMO API is currently only a prototype. The known issues are listed in :ref:`limitations`.

Algorithm
---------

Since NEMO source is treated as a pre-existing PSy layer, this API
does not have the concept of an Algorithm layer.

Constructing the PSyIRe
-----------------------

Transformations in PSyclone are applied to an Internal Representation,
the "PSyIRe." In contrast to the other APIs where the PSyIRe is
constructed from scratch, for NEMO PSyclone must parse the existing
Fortran and create a higher-level representation of it. This is done
using rules based upon the NEMO Coding Conventions :cite:`nemo_code_conv`.
These rules are described in the following sections.

Loops
+++++

Explicit
^^^^^^^^

PSyclone recognises the following loop types, based on the name of the loop
variable:

===============  =============
Loop type        Loop variable
===============  =============
Vertical levels  jk
Latitude         ji
Longitude        jj
Tracer species   jn
===============  =============

PSyclone currently assumes that each of these loop types may be safely
parallelised. In practice this will not always be the case (e.g. when
performing a tri-diagonal solve) and this implementation will need to
be refined.

Implicit
^^^^^^^^

The use of Fortran array notation is encouraged in the NEMO Coding
Conventions and is employed throughout the NEMO code base. PSyclone
therefore also recognises the loops implied by this notation. The type
of loop is inferred from the position of the colon in the array
subscripts. Since NEMO uses `(ji,jj,jk)` index ordering (longitude,
latitude, levels) this means that loop types are determined
according to the following table:

===========  ===============  ===========
Array index  Loop type        Loop limits
===========  ===============  ===========
1            Longitude        1, jpi
2            Latitude         1, jpj
3            Vertical levels  1, jpk
===========  ===============  ===========

Currently PSyclone will convert every implicit loop it encounters in
one of these dimensions into an explicit loop over the full
sub-domain. This is because it has been found by CMCC and Intel that
the Intel compiler is better able to OpenMP-parallelise explicit loops.
However, as with all compiler-specific optimisations, this situation
may change and will depend on the compiler being used.

If an implicit loop is encountered in an array index other than 1-3 then
currently PSyclone will raise an error.

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

PSyclone uses fparser2 to parse such source code and then generates an
internal representation of it::
  
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

.. _limitations:

Limitations
-----------

The NEMO API is currently only a prototype implementation. Here
we list the current, known limitations/issues:

 1. When converting implicit loops into explicit loops, the
    declaration of the loop variables is repeated (there is an
    x-failing test for this);
 2. Scalar variables inside loops are not made private when
    parallelising using OpenMP;
 3. All recognised loops (levels, latitude etc.) are assumed to be
    parallelisable. This will not always be the case (e.g. tridiagonal
    solve has a loop-carried dependence in the vertical);
 4. Loops/kernels within CASE statements are not found;
 5. Labelled do-loops are not handled (i.e. they will probably end up
    being put inside a code block);
 6. ``NemoKern._load_from_loop()`` and ``_load_from_implicit_loop()``
    both need to be implemented. Currently they do nothing but they
    should e.g. work out which variables are private to the kernel;
 7. Loops are currently only permitted to contain one kernel.  This
    restriction will have to be lifted in order to permit loop fusion;
 8. Array slices with specified bounds (e.g. umask(1:10)) are not yet
    supported and will raise an exception;
 9. When generating new variable names, no attempt is made to avoid
    clashing with variables already present in the NEMO source.
 10. The psyGen.Node base class now has an _ast property to hold a
     pointer into the associated fparser2 AST. However, the psyGen.Kern
     class already has an _fp2_ast property that points to the whole
     fparser2 AST of the kernel code. This will be rationalised in
     #241.
