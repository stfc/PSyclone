.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2018-2024, Science and Technology Facilities Council.
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
.. Modified by R. W. Ford, STFC Daresbury Lab

.. _nemo-api:

NEMO API
========

In contrast to the other APIs supported by PSyclone, the NEMO API is
designed to work with source code that does *not* follow the PSyKAl
separation of concerns. Instead, the NEMO source code is treated as if
it were a manually written PSy layer with all kernels in-lined. This
approach relies upon the NEMO Coding Conventions
:cite:`nemo_code_conv` in order to reason about the code being
processed. Rather than construct an InvokeSchedule for the PSy layer from
scratch (as is done for other APIs), the InvokeSchedule is constructed by
parsing the supplied Fortran code and generating a higher-level
representation.

.. note:: the NEMO API is currently only a prototype. The known issues are listed in :ref:`limitations`.

Algorithm
---------

Since NEMO source is treated as a pre-existing PSy layer, this API
does not have the concept of an Algorithm layer.

Constructing the PSyIR
-----------------------

Transformations in PSyclone are applied to an Internal Representation,
the "PSyIR." In contrast to the other APIs where the PSyIR is
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
Conventions :cite:`nemo_code_conv` (section 4.2) and is employed
throughout the NEMO code base. The Coding Conventions mandate that the
shape of every array in such expressions must be specified, e.g.::

    onedarraya(:) = onedarrayb(:) + onedarrayc(:)
    twodarray (:,:) = scalar * anothertwodarray(:,:)

PSyclone therefore also recognises the loops implied by this
notation.

Note, not all uses of Fortran array notation in NEMO imply a loop. For
instance::

  ascalar = afunc(twodarray(:,:))

is actually a function call which is passed a reference to ``twodarray``.
However, if the quantity being assigned to is actually an array, e.g.::

  twodarray2(:,:) = afunc(twodarray(:,:))

then this does represent a loop. However, currently PSyclone does not
recognise any occurrences of array notation that are themselves within
an array access or function call. It is therefore not yet possible to
transform such implicit loops into explicit loops. It is hoped that this
limitation will be removed in future releases of PSyclone by adding the
ability to discover the interface to functions such as ``afunc`` and thus
determining whether they return scalar or array quantities.

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

PSyclone uses fparser2 to parse such source code and then generates the PSy
Internal Representation of it::

    Loop[type='tracers',field_space='None',it_space='None']
        Loop[type='None',field_space='None',it_space='None']
        Loop[type='None',field_space='None',it_space='None']
        Loop[type='None',field_space='None',it_space='None']
        Loop[type='None',field_space='None',it_space='None']
        Loop[type='levels',field_space='None',it_space='None']
            Loop[type='lat',field_space='None',it_space='None']
                Loop[type='lon',field_space='None',it_space='None']
                    CodedKern[]

Transformations
---------------

The following transformations are specific to the NEMO API.

####

.. autoclass:: psyclone.domain.nemo.transformations.NemoArrayRange2LoopTrans
    :noindex:
    :members: apply, validate

####

.. autoclass:: psyclone.domain.nemo.transformations.NemoOuterArrayRange2LoopTrans
    :noindex:
    :members: apply, validate

####

.. autoclass:: psyclone.domain.nemo.transformations.NemoAllArrayRange2LoopTrans
    :noindex:
    :members: apply, validate

####

.. autoclass:: psyclone.domain.nemo.transformations.NemoArrayAccess2LoopTrans
    :noindex:
    :members: apply, validate

####

.. autoclass:: psyclone.domain.nemo.transformations.NemoAllArrayAccess2LoopTrans
    :noindex:
    :members: apply, validate

.. _limitations:

Limitations
-----------

The NEMO API is currently under development. Here
we list the current, known limitations/issues:

 1. Scalar variables inside loops are not made private when
    parallelising using OpenMP;
 2. Labelled do-loops are not handled (i.e. they will be put inside a
    'CodeBlock' in the PSyIR);
 3. Loops are currently only permitted to contain one kernel.  This
    restriction will have to be lifted in order to permit loop fusion;
 4. The psyir.nodes.Node base class now has an _ast property to hold a
    pointer into the associated fparser2 AST. However, the psyGen.Kern
    class already has an _fp2_ast property that points to the whole
    fparser2 AST of the kernel code. This will be rationalised in
    #241;
