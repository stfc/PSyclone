.. _Ubuntu14.03.3:

System-specific set-up
======================

This section provides system-specific information on how to set-up
your system to use PSyclone.

Ubuntu 14.04.3
--------------

This guide has been tested with a vanilla installation of Ubuntu 14.04.3.

.. _users:

User set-up
+++++++++++

Get a terminal window. You can do this by pressing <ctrl><Alt><t>
together, or click the top left "search" icon and type "terminal".

Install PSyclone
^^^^^^^^^^^^^^^^

Change directory to where you would like to place the code (where
<PSYCLONEHOME> refers to where you would like to place the code):
::

   > cd <PSYCLONEHOME>

Now download and extract the latest release of PSyclone, e.g.:

.. parsed-literal::

   > wget https://github.com/stfc/PSyclone/archive/\ |release|\ .tar.gz
   > gunzip \ |release|\ .tar.gz
   > tar xf \ |release|\ .tar
   > rm \ |release|\ .tar

Set your python path and path appropriately:

.. parsed-literal::

   > cd PSyclone-\ |release|\ 
   > export PYTHONPATH=`pwd`/src:${PYTHONPATH}
   > export PATH=`pwd`/bin:${PATH}

You may want to set these paths permanently (e.g. by editing your
${HOME}/.bashrc file if you run the BASH shell).

Install Python packages using apt package manager
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

numpy is required to run PSyclone
::

   > sudo apt-get install python-numpy

pyparsing is required by PSyclone
::

   > sudo apt-get install python-pyparsing

.. _install_pip:

Install pip
^^^^^^^^^^^

The pip tool enables Python packages to be installed from the Python
Package Index (https://packaging.python.org/installing/). Install it like so:
::

   > sudo apt-get install python-pip

.. _install_fparser:

Install fparser
^^^^^^^^^^^^^^^

fparser is also required by PSyclone but is not available from the
Ubuntu software centre. It can instead be installed from the
Python Package Index using pip:
::

   > sudo pip install fparser

Uninstalling is simply a matter of doing:
::

   > sudo pip uninstall fparser

If you do not have sufficient privileges for a system-wide install then
you can do:
::

   > pip install --user fparser

(The ``--user`` flag requests that the packages be installed locally for
the current user rather than requiring root access.) In order for Python
to find such locally-installed packages the necessary directory must be
added to the PYTHONPATH, e.g.:
::

   > export PYTHONPATH=/home/a_user/.local/lib/python2.7/site-packages:${PYTHONPATH}

Alternatively, if pip is not an option, a tarball of the latest release
may be downloaded from https://github.com/stfc/fparser/releases. Simply
unpack the tarball and ensure that the resulting
``fparser-x.y.z/src/fparser`` directory is in your PYTHONPATH.

PSyclone supports the ability to output a schedule dependency graph
using the graphviz package. This is optional and the associated
routine will silently return if the graphviz bindings are not
installed. To output a graph you need to install the graphviz package
::

   > sudo apt-get install graphviz

and the Python bindings to the graphviz package
::

   > sudo pip install graphviz

If you just want to use PSyclone then you've installed all you need
and you're ready to go to the getting-going :ref:`getting-going-run`
section.

Developer set-up
++++++++++++++++

This section adds software that is used to develop and test
PSyclone. Note, we assume you have already installed the software
described in the :ref:`users` section.

pytest
^^^^^^

Install pytest for running python tests
::

   > sudo apt-get install python-pytest

You can now run the PSyclone python tests:

.. parsed-literal::

   > cd PSyclone-\ |release|\ /src/psyclone/tests
   > py.test

Documentation
^^^^^^^^^^^^^

Install Sphinx for creating PSyclone documentation 
::

   > sudo apt-get install python-sphinx

You can now build html and latex documentation (but not pdf)
::

   > cd doc
   > make html
   > make latex

Install texlive for the PSyclone pdf documentation.

.. warning:
    These installs are large. It may be possible to install a subset of texlive-latex-extra but the authors do not know what this subset is.

::

   > sudo apt-get install texlive
   > sudo apt-get install texlive-latex-extra

You can now build the pdf documentation
::

   > cd doc
   > make latexpdf

Static code tests and style checking
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Install the standalone pep8 tool
::

   > sudo apt-get install pep8

You can now test whether the Python code conforms to the pep8
standards
::

   > pep8 code.py

Install the standalone pylint tool
::

   > sudo apt-get install pylint

You can now test how well the Python code conforms to the pylint
standards
::

   > pylint code.py

Finally, install useful pytest extensions using pip:
::

   > sudo pip install pytest-cov
   > sudo pip install pytest-pep8
   > sudo pip install pytest-pylint
   > sudo pip install pytest-flakes
   > sudo pip install pytest-pep257

If you don't have root access then you can specify the ``--user`` argument to
install packages in a user-local directory -- see the instructions on
:ref:`install_fparser` above.

Should you wish to remove the above packages at any point, simply instruct
pip to uninstall them, e.g.:
::
   
   > sudo pip uninstall pytest-cov

OK, you're all set up.
