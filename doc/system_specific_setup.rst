System-Specific Set-up
======================
This chapter is split into two sections: the first section
describes the user setup. It includes all steps necessary
to be able to use PSyclone. And while you could obviously do
some development, none of the required tools for testing,
documentation creation will be installed.

The second section describes the additional installation of
all required tools to run tests, and create documentation.

Both sections have detailled instructions for Ubuntu 16 and 
OpenSUSE 42 - if you are working with a different Linux
distribution some adjustments will be necessary.

User Set-up
-----------
This section provides system-specific information on how to
set-up your system to use PSyclone for users.
It has been tested with a vanilla installation of Ubuntu 16**
and OpenSUSE 42****.
You need a terminal window for entering the command into.

.. _Ubuntu14.03.3:
.. _users_ubuntu14:

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

Most required dependencies are installed using the python
pip tool. It enables Python packages to be installed from
Python.


Installing PIP on Ubuntu
^^^^^^^^^^^^^^^^^^^^^^^^

Package Index (https://packaging.python.org/installing/). Install it like so:
::

   > sudo apt-get install python-pip


Installing PIP on OpenSUSE
^^^^^^^^^^^^^^^^^^^^^^^^^^
While the vanilla OpenSUSE installation includes PIP,
the version installed only works for Python 3. So the
python 2 version of PIP still needs to be installed
::

    > zypper install python-pip

Note: PIP for python2 on OpenSUSE is called pip2.7. So you need
to replace the ``pip`` command with ``pip2.7`` in all commands further down!


Installation of Other Dependencies
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
To remove some warnings it is recommended to update PIP
to the latest version
::

    > sudo pip install --upgrade pip

Next you need to install the fparser and pyparsing packages
::
    > sudo pip install fparser pyparsing 

PIP has the ability to install packages either system-wide
(which requires root privileges) as above, or for a user only
(in ~/.local). While the latter is only useful for one
particular user, it means that PSyclone can be installed
using PIP without needing root privileges. In order to install
a package for a user, add the --user command line option to
all pip commands, e.g.:
::
    > pip2 install --user fparser pyparsing

The ``--user`` flag requests that the packages be installed locally for the current user rather than requiring root access.
In order for Python to find such locally-installed packages the necessary directory must be added to the PYTHONPATH, e.g.:
::

   > export PYTHONPATH=/home/a_user/.local/lib/python2.7/site-packages:${PYTHONPATH}

Uninstalling is simply a matter of doing:
::

   > sudo pip uninstall fparser pyparsing


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

If you just want to use PSyclone then you've installed all you need and you're ready to go to the getting-going :ref:`getting-going-run` section.

Developer set-up
----------------

This section adds software that is used to develop and test
PSyclone. Note, we assume you have already installed the software
described in the :ref:`users_ubuntu14` section.

pytest
^^^^^^

Install pytest for running python tests
::

   > sudo pip install pytest

You can now run the PSyclone python tests:

.. parsed-literal::

   > cd PSyclone-\ |release|\ /src/psyclone/tests
   > py.test

Documentation
^^^^^^^^^^^^^

Install Sphinx for creating PSyclone documentation 
::

   > sudo pip install sphinx

You can now build html and latex documentation (but not pdf)
::

   > cd doc
   > make html
   > make latex

Install texlive for the PSyclone pdf documentation.

.. warning:
    These installs are large. It may be possible to install a subset of texlive-latex-extra but the authors do not know what this subset is.

::

   > sudo apt-get install --no-install-recommends texlive
   > sudo apt-get install --no-install-recommends texlive-latex-extra
   > sudo apt-get install --no-install-recommends texlive-latexmk

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

.. _openSUSE42.2:

openSUSE 42.2
-------------
This guide has been tested with a vanilla installation of openSUSE 42.2.

.. _users_opensuse42:

User set-up
+++++++++++

Open a terminal or konsole.

Installation of  required tools
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
The default openSUSE 42.2 installation only installs pip for python3,
so the python2 versison needs to be installed manually:
::

    > zypper install python-pip
    > sudo pip2.7 install --upgrade pip

The update of pip is only necessary to remove a warning message
that would otherwise be printed.

For developers the following packages are also required:
::

    > zypper install git


Installation of PSyclone
^^^^^^^^^^^^^^^^^^^^^^^^

Now download and extract the latest release of PSyclone:
::

   > wget https://github.com/stfc/PSyclone/archive/1.4.1.tar.gz
   > gunzip 1.4.1.tar.gz
   > tar xf 1.4.1.tar
   > rm 1.4.1.tar

Set your python path appropriately:
::

   > cd PSyclone-1.4.1
   > export PYTHONPATH=`pwd`/src:${PYTHONPATH}

You may want to set your python path permanently (e.g. by editing your
${HOME}/.bashrc file if you run the BASH shell).

Install Python packages using pip package manager
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Using pip2.7 to install all required dependencies. Note that some
packages will install additional dependencies.
::
    > sudo pip2.7 install fparser
    > sudo pip2.7 install pyparsing

If you need to remove any of those packages, just use
::
    > sudo pip uninstall <packagename>

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


PSyclone supports the ability to output a schedule dependency graph
using the graphviz package. This is optional and the associated
routine will silently return if the graphviz bindings are not
installed. To output a graph you need to install the python bindings
to the graphviz package
::

    > sudo pip2.7 install graphviz
If you just want to use PSyclone then you've installed all you need
and you're ready to go to the getting-going :ref:`getting-going-run`
section.

Developer set-up
++++++++++++++++

This section adds software that is used to develop and test
PSyclone. Note, we assume you have already installed the software
described in the :ref:`users_opensuse42` section.

pytest
^^^^^^

Install pytest for running python tests
::

   > sudo pip2.7 install pytest

You can now run the PSyclone python tests
::

   > cd PSyclone_trunk/src/tests
   > py.test

Documentation
^^^^^^^^^^^^^

Install Sphinx for creating PSyclone documentation 
::
    > sudo pip2.7 install sphinx

CHECK!!!  now you need install psyclone before html.
When installing locally, you need to add the dir to PYTHONPATH!!!!!!!!
$HOME/local/lib/python2.7/site-packages

You can now build html and latex documentation (but not pdf)
::

   > cd doc
   > make html
   > make latex
Install texlive for the PSyclone pdf documentation.

.. warning:
    The following command installs the minimum number of packages
    in order to create the pdf documentation. It is important to
    install the packages in one zypper command, since otherwise
    depending on used file system snapshots might be created after
    each package, which can add up to several GB of data.
    The minimum installation installs around 130 packages,
    requiring around 300 MB.

::

   > sudo zypper install texlive-latex

   > sudo zypper install --no-recommends texlive-latex texlive-latexmk \
   texlive-babel-english texlive-cmap texlive-psnfss texlive-fncychap  \
   texlive-fancyhdr texlive-titlesec texlive-tabulary texlive-varwidth \
   texlive-framed texlive-fancyvrb texlive-float texlive-wrapfig       \
   texlive-parskip texlive-upquote texlive-capt-of texlive-needspace   \
   texlive-metafont texlive-makeindex texlive-times texlive-helvetic   \
   texlive-courier texlive-dvips



You can now build the pdf documentation
::

   > cd doc
   > make latexpdf

Static code tests and style checking
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Install the standalone pep8 tool
::

    > sudo pip2.7 install pep8

You can now test whether the Python code conforms to the pep8
standards
::

    > pep8 code.py

Install the standalone pylint tool
::

    > sudo pip2.7 install pylint

You can now test how well the Python code conforms to the pylint
standards
::

    > pylint code.py

Finally, install useful pytest extensions using pip:
::

    > sudo pip2.7 install pytest-cov
    > sudo pip2.7 install pytest-pep8
    > sudo pip2.7 install pytest-pylint
    > sudo pip2.7 install pytest-flakes
    > sudo pip2.7 install pytest-pep257

If you don't have root access then you can specify the ``--user`` argument to
install packages in a user-local directory -- see the instructions on
:ref:`install_fparser` above.

Should you wish to remove the above packages at any point, simply instruct
pip to uninstall them, e.g.:
::
    > sudo pip uninstall pytest-cov

OK, you're all set up.
