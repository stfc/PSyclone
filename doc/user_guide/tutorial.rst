.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2020-2021, Science and Technology Facilities Council.
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
.. Written by R. W. Ford and A. R. Porter, STFC Daresbury Lab
.. Modified by I. Kavcic, Met Office

.. _tutorial:

Tutorial
========

PSyclone provides a tutorial, part of which uses Jupyter notebooks. This can be
launched from a browser using binder:
`<https://mybinder.org/v2/gh/stfc/psyclone/master?filepath=tutorial%2Fnotebooks%2Fintroduction.ipynb/>`_.

If PSyclone is installed on your system then you can run the tutorial
locally. First find the tutorial. If you have installed PSyclone using
``pip`` then the examples may be found in ``share/psyclone/tutorial/notebooks``
under your Python installation (see :ref:`here <getting-going-env-loc>` for
possible locations). Next, copy the tutorial to a local writable space (as
the notebooks will be modified as you go through the tutorial) and
change directory to this tutorial. Lastly, start up the tutorial's
introduction ``jupyter-notebook introduction.ipynb``.

Another part of the tutorial are `practicals
<https://github.com/stfc/PSyclone/tree/master/tutorial/practicals>`_
that provide hands-on introductions to various functionality contained in the
:ref:`LFRic <dynamo0.3-api>` and :ref:`NEMO <nemo-api>` APIs. The sections
in the included directories are worked through using PSyclone in a "normal"
Linux environment.

.. note:: Hands-on practicals are not currently included in a
          PSyclone installation.
