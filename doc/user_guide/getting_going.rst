.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2017-2021, Science and Technology Facilities Council.
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

.. _getting-going:

Getting Going
=============

.. _getting-going-download:

Download
--------

The following instructions are intended for a PSyclone user who wants
to work with a released version of the code. If you are a developer or
wish to test a specific branch of PSyclone from the GitHub repository
please see :ref:`dev-installation` in the
`Developer Guide <https://psyclone-dev.readthedocs.io/>`_.

PSyclone is available on the Python Package Index
`(PyPI) <https://pypi.org/>`_ and is hosted on GitHub:

``https://github.com/stfc/PSyclone``

The latest release is |release| and the latest stable version is on
the master branch.

There are two ways to install PSyclone. The first one is directly from
PyPI using ``pip install``, see :ref:`getting-going-env-pypi` for
more detailed information.

Alternatively, PSyclone can be downloaded from GitHub - either see |release|
in the ``Tags`` `tab <https://github.com/stfc/PSyclone/tags>`_
on the PSyclone page or download and extract the latest release of
PSyclone directly, e.g.

.. parsed-literal::
   > wget \https://github.com/stfc/PSyclone/archive/\ |release|\ .tar.gz
   > tar zxf \ |release|\ .tar.gz
   > ls
   PSyclone-\ |release|\

After the source package is downloaded and unpacked, it can be installed
using ``pip install``, albeit in a slightly different way to the PyPI
installation, see :ref:`getting-going-env-src` for more detailed
information.

Hereon the location where you download or clone PSyclone (including the
PSyclone directory itself) will be referred to as ``<PSYCLONEHOME>``.

.. _getting-going-env:

Environment
-----------

In order to use PSyclone (including running the test suite and
building documentation) you will need to install it. Before starting
the installation process, please refer to the
:ref:`Dependencies <getting-going-depend>` section below.

.. _getting-going-env-pypi:

Installation from PyPI
^^^^^^^^^^^^^^^^^^^^^^

The simplest, and recommended, installation process is from
`PyPI <https://pypi.org/project/PSyclone/>`_ using ``pip``::

   > pip install psyclone

for the latest available release, or::

   > pip install psyclone==X.Y.Z

where ``X.Y.Z`` is the specific PSyclone release version (e.g. |release|).

By default, ``pip`` will attempt a system-wide install. If you wish
to do a user-local install instead then supply the ``--user`` flag::

   > pip install --user psyclone

PSyclone can also be installed to a specific location using ``--install-option``
(see ``pip`` `documentation <https://pip.pypa.io/en/stable/cli/pip_install/>`_
for more detailed information)::

   > pip install --install-option="--prefix=/my/install/path" psyclone==X.Y.Z

Depending on the installation option (e.g. system-wide, user), PSyclone
will be installed in different :ref:`locations <getting-going-env-loc>`.

.. _getting-going-env-src:

Installation from source
^^^^^^^^^^^^^^^^^^^^^^^^

PSyclone can also be installed from a
:ref:`downloaded <getting-going-download>` release or repository clone. The
simplest way to do this is to use ``pip`` with the supplied ``setup.py``::

   > cd <PSYCLONEHOME>
   > pip install .

As above, this attempts a system-wide install. For a user-local install use::

   > pip install --user .

and for a specific location use::

   > pip install --install-option="--prefix=/my/install/path" .

If for some reason you would rather not use ``pip`` then you can run the
setup manually::

   > python setup.py install

or, if you do not have root access::

   > python setup.py install --user

or::

   > python setup.py install --install-option="--prefix=/my/install/path"

As for the :ref:`PyPI installation <getting-going-env-pypi>`, different
installation options lead to different
:ref:`locations <getting-going-env-loc>` of PSyclone installation.

.. _getting-going-env-loc:

Location and structure of PSyclone installation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Location of installed Pyclone scripts, modules and other accompanying
resources is similar to other Python packages:

* The ``psyclone`` :ref:`script <psyclone_command>` is located
  in ``<python-base-prefix>/bin`` directory (depending on your Linux
  distribution, you may need to add this location to your ``$PATH``).

* The PSyclone Python modules are located in
  ``<python-base-prefix>/lib/pythonX.Y/site-packages`` directory (where
  ``X.Y`` is the version of Python that you are using).

* The :ref:`configuration file <getting-going-configuration>`,
  :ref:`examples <examples>`, :ref:`tutorial <tutorial>` and
  :ref:`libraries <libraries>` are installed in
  ``<python-base-prefix>/share/psyclone`` directory.

For a system-wide installation on Linux, ``<python-base-prefix>`` will
likely be ``/usr`` and if a user-local installation is performed
it will likely be ``~/.local``.

For an installation to a specific location, ``<python-base-prefix>``
is simply the path given to the
``--install-option="--prefix=/my/install/path"``. Note that if using
this method, it will be necessary to take further action to ensure
PSyclone can find the :ref:`configuration file <getting-going-configuration>`
installed as a part of this process.

.. _getting-going-env-win:

Windows environment
^^^^^^^^^^^^^^^^^^^

PSyclone can also be installed in `Python Windows environment
<https://www.python.org/downloads/windows/>`_ using ``pip`` as described
above. There are some differences in directory structure from Linux,
for instance the script directory is usually called ``Scripts`` instead
of ``bin`` and the modules directory ``Lib`` instead of ``lib``.

Installation in an `Anaconda Python
<https://www.anaconda.com/products/individual>`_ environment on
Windows also needs to be done using ``pip`` as ``conda install`` for
PSyclone is currently not supported.

.. _getting-going-depend:

Dependencies
------------

PSyclone is written in Python so needs Python 3 to be installed on the
target machine. PSyclone is regularly tested with Python 3.6 and 3.8
but should work with any version >= 3.6. (The last PSyclone release to
support Python 2.7 was version 2.1.0.)

PSyclone immediately relies on five external Python packages; ``six``,
``configparser``, ``fparser``, ``sympy``, and ``pyparsing``. (Note that
the use of ``six`` is being phased out now that Python 2.7 is not
supported.) The easiest way to satisfy the Python dependencies is to
use the `PyPI installation <https://packaging.python.org/installing>`_
and ``pip``.

If everything is working correctly then using ``pip`` to install PSyclone::

   > pip install psyclone

will automatically install the Python dependencies.

.. warning:: Starting with the release 1.6.1, PSyclone will install a
             specific release of ``fparser`` (version specified in the
             ``setup.py`` script).

In addition to the mandatory dependencies just described, PSyclone
also has optional dependencies on both ``graphviz`` and ``termcolor``.
PSyclone can use ``graphviz`` to produce a visualisation of a schedule's
dependency graph. If this is desired then the Python package
``graphviz`` (for the Python bindings) as well as the ``graphviz`` package
itself must be installed. If the ``graphviz`` package is not available
then the associated PSyclone routines will return silently and no
visualisations will be produced. The Python package ``termcolor`` is
used for pretty-printing a schedule in terminals that support coloured
text. If the package is not available then the schedule is simply
printed in plain text without colour highlighting.


System-specific set-up
^^^^^^^^^^^^^^^^^^^^^^

:ref:`system_specific_setup` instructions are available for Ubuntu 14.04.2 and
OpenSUSE 42.2.

fparser
^^^^^^^

The ``fparser`` package (https://github.com/stfc/fparser) is a Fortran
parser originally developed as a part of the `f2py project
<http://www.f2py.com/>`_.

``fparser`` is available from the Python Package
Index and thus may be installed using ``pip``
(https://packaging.python.org/installing/#requirements-for-installing-packages):
::

   > pip install fparser

If you do not have sufficient permissions to perform a system-wide install
then you can instruct ``pip`` to do a user-local install:
::

   > pip install --user fparser

Should you wish to remove ``fparser`` then simply do:
::

   > pip uninstall fparser

If you have already installed ``fparser`` and want to upgrade to the
latest version simply do:
::

   > pip install fparser --upgrade


.. warning:: Due to the above-mentioned reliance of PSyclone on a specific
             ``fparser`` release, it is not advisable to install ``fparser``
             independently unless it is not to be used with PSyclone. An
             exception is installation of PSyclone from source for
             development purposes, see :ref:`dev-installation` in the
             `Developer Guide <https://psyclone-dev.readthedocs.io/>`_.

pyparsing
^^^^^^^^^

PSyclone requires ``pyparsing``, a library designed to allow parsers to
be built in Python. PSyclone uses ``pyparsing`` to parse Fortran regular
expressions as ``fparser`` does not fully parse these (see
`here <https://github.com/pyparsing>`__ for more information).

PSyclone has been tested with ``pyparsing`` versions 1.5.2, 2.0.1 and 2.2.0.

You can test whether ``pyparsing`` is already installed on your machine by
typing ``import pyparsing`` from the Python command line. If ``pyparsing``
is installed, this command will complete successfully. If ``pyparsing`` is
installed you can check its version by typing
``pyparsing.__version__`` after successfully importing it.

If ``pyparsing`` is not installed on your system then it may be installed
from the Python Package Index using ``pip``:
::

   > pip install pyparsing

Should you wish to, uninstalling is simply performed by doing:
::

   > pip uninstall pyparsing

If you do not have sufficient privileges for a system-wide install then
you can instruct ``pip`` to do a user-local install:
::

   > pip install --user pyparsing

Alternatively, you could follow `these instructions
<https://github.com/pyparsing/pyparsing>`_.


SymPy
^^^^^

PSyclone requires ``sympy``, a library for symbolic mathematics. PSyclone
uses ``sympy`` to reason about expression being equal or not, e.g. ``i+j``
and ``j+i``. PSyclone has been tested with ``sympy`` versions 1.7.1.

You can test whether ``sympy`` is already installed on your machine by
typing ``import sympy`` from the Python command line. If ``sympy``
is installed, this command will complete successfully. If ``sympy`` is
installed you can check its version by typing
``sympy.__version__`` after successfully importing it.

If ``sympy`` is not installed on your system then it may be installed
from the Python Package Index using ``pip``:
::

   > pip install sympy

Should you wish to, uninstalling is simply performed by doing:
::

   > pip uninstall sympy

If you do not have sufficient privileges for a system-wide install then
you can instruct ``pip`` to do a user-local install:
::

   > pip install --user sympy

Alternatively, you could follow the instructions on the `SymPy web page
<https://docs.sympy.org/latest/install.html>`_.

Graphviz
^^^^^^^^

The data dependencies of a PSyIR schedule determine the validity of
changes to this schedule.
PSyclone supports the visualisation of these dependencies as
a graph using ``graphviz``. This visualisation is not needed to use
PSyclone.

If the Python bindings to ``graphviz`` are not installed on your system
then it may be installed from the Python Package Index using ``pip``:
::

   > sudo pip install graphviz

Should you wish to, uninstalling is simply performed by doing:
::

   > sudo pip uninstall graphviz

If you do not have sufficient privileges for a system-wide install then
you can instruct ``pip`` to do a user-local install:
::

   > pip install --user graphviz

If ``graphviz`` itself is not installed on your system and your system
supports the ``apt`` package manager then see below, otherwise please
refer to the download and install instructions which are available
`here <https://graphviz.org/download/>`__.

If your system supports the ``apt`` package manager then it can be
installed and removed in the following way:
::

   > sudo apt install graphviz
   > sudo apt remove graphviz

termcolor
^^^^^^^^^

By default, the ``view()`` method available on any PSyIR (PSyclone
Internal Representation) object prints a plain-text representation
to standard-out. However, if the ``termcolor`` package is available
then PSyclone uses this to add colour highlighting to the output text.

Installation (and uninstallation) of this package can be done via
``pip`` in exactly the same way as for ``graphviz``, as described above.

.. _getting-going-configuration:

Configuration
-------------

Various aspects of PSyclone are configured through a configuration
file, ``psyclone.cfg``. The default version of this file is installed
to ``<python-base-prefix>/shared/psyclone/`` during the installation
process. Similar to what is described :ref:`above
<getting-going-env-loc>`, if a system-wide installation is being
performed then this will likely be ``/usr/share/psyclone/``.
If a user-local installation is performed (``--user`` flag to
``pip install``) then the location will be something like
``~/.local/share/psyclone/``.

.. warning::

   If PSyclone is installed to a non-standard location (e.g. by specifying
   the ``--install-option="--prefix=...`` option to ``pip install``) then
   PSyclone will not be able to find the configuration file at execution
   time. There are two solutions to this: 1. copy the configuration file to
   a location where PSyclone will find it (see :ref:`configuration`) or
   2. set the ``PSYCLONE_CONFIG`` environment variable to the full-path to
   the configuration file, e.g.::

   > export PSYCLONE_CONFIG=/some/path/PSyclone/config/psyclone.cfg

.. warning::

   When installing in 'editable' mode (``-e`` flag to ``pip``), ``pip``
   does *not* install the configuration file. You will have to take one
   of the two actions described above.

See :ref:`configuration` for details of the settings contained within
the config file.

Test
----

PSyclone contains an extensive test suite, but this test suite is not
part of a standard installation. If you want to run the full test 
suite, you need to install PSyclone from source, see :ref:`above
<getting-going-env-src>` or  :ref:`dev-installation` in the
`Developer Guide <https://psyclone-dev.readthedocs.io/>`_.

.. _getting-going-run:

Run
---

You are now ready to try running PSyclone on the :ref:`examples <examples>`.
One way of doing this is to use the ``psyclone`` driver script. Assuming it
is on your ``PATH``::

   > psyclone
   usage: psyclone [-h] [-oalg OALG] [-opsy OPSY] [-okern OKERN] [-api API]
                   [-s SCRIPT] [-d DIRECTORY] [-I INCLUDE] [-l {off,all,output}]
                   [-dm] [-nodm] [--kernel-renaming {multiple,single}]
                   [--profile {invokes,kernels}] [--config CONFIG] [-v]
                   filename
   psyclone: error: the following arguments are required: filename

As indicated above, the ``psyclone`` script takes the name of the
Fortran source file containing the algorithm specification (in terms
of calls to ``invoke()``). It parses this, finds the necessary kernel
source files and produces two Fortran files. The first contains the
:ref:`PSy, middle layer <PSy-layer>` and the second a re-write of the
:ref:`algorithm code <algorithm-layer>` to use that layer. These files
are named according to the user-supplied arguments (options ``-oalg``
and ``-opsy``). If those arguments are not supplied then the script writes
the generated/re-written Fortran to the terminal. For details of the other
command-line arguments please see the :ref:`psyclone_command` Section.

Examples are provided in the ``examples`` directory of the PSyclone Git
repository - if you have cloned the repository then ``EGS_HOME`` in
what follows is the root ``PSyclone`` directory. Alternatively, if you
have installed PSyclone using ``pip`` then they may be found in the
``share/psyclone`` directory under your Python installation (see
:ref:`above <getting-going-env-loc>` for location of PSyclone installation.
In this case you should copy the whole ``examples`` directory to some
convenient location (hereafter called ``EGS_HOME``) before attempting to
carry out the following instructions. Depending on your precise setup, you
may also need to set ``PSYCLONE_CONFIG`` to the full-path to the PSyclone
configuration file (see :ref:`getting-going-configuration`).

There are seven subdirectories, three of which (``lfric``, ``gocean``
and ``nemo``) correspond to the different APIs/domains that are
supported by PSyclone. (Note, that we are currently in the process of
renaming the ``dynamo0.3`` API to ``lfric``.)  In this case we are
going to use one of the LFRic examples::

   > cd <EGS_HOME>/examples/lfric/eg1
   > psyclone -api dynamo0.3 -d ../code -nodm -oalg alg.f90 \
       -opsy psy.f90 ./single_invoke.x90


You should see two new files created, called ``alg.f90`` (containing
the re-written algorithm layer) and ``psy.f90`` (containing the
generated PSy- or middle-layer). Since this is an LFRic example the
Fortran source code has dependencies on the LFRic system and
therefore cannot be compiled stand-alone.

The PSy-layer that PSyclone creates is constructed using the PSyclone Internal
Representation (:ref:`PSyIR <psyir-ug>`). Accessing this is demonstrated
by the ``print_psyir_trans.py`` script in the second LFRic example::

  > cd <EGS_HOME>/examples/lfric/eg2
  > psyclone -api dynamo0.3 -d ../code -s ./print_psyir_trans.py \
      -opsy psy.f90 -oalg alg.f90 ./multi_invoke_mod.x90

Take a look at the ``print_psyir_trans.py`` script for more information. *Hint*;
you can insert a single line in that script in order to break into the Python
interpreter during exection: ``import pdb; pdb.set_trace()``. This then enables
interactive exploration of the PSyIR if you are interested. Alternatively,
you can play with some interactive examples on `Binder <https://github.com/stfc/PSyclone#user-content-try-it-on-binder>`_.
