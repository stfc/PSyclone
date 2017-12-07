.. _system_specific_setup:

System-specific set-up
======================
This chapter is split into two sections: :ref:`setup_user`
describes the setup for a user of PSyclone. It includes all steps necessary
to be able to use PSyclone. And while you could obviously do
some development, none of the required tools for testing or
documentation creation will be installed.

:ref:`dev_setup` describes the additional installation of
all required tools to run tests, and create documentation.

Both sections have detailed instructions for Ubuntu 16.04.2 and 
OpenSUSE 42.2 - if you are working with a different Linux
distribution some adjustments will be necessary.

.. _setup_user:

User set-up
-----------

This section provides system-specific information on how to
set-up your system to use PSyclone for users.
It has been tested with a vanilla installation of Ubuntu 16.04.2
and OpenSUSE 42.2. You need a terminal window for entering the commands into.

Installing dependencies
^^^^^^^^^^^^^^^^^^^^^^^
Most required dependencies are installed from the 
Python Package Index (https://packaging.python.org/installing/)
using the program pip ("PIP Installs Packages"). Besides ``pip``
it is also recommended to install the graphviz package to be
able to visualise dependency graphs. This is optional and the associated
routine will silently return if the graphviz bindings are not
installed.

.. _ubuntu_user:

Installing dependencies on Ubuntu
+++++++++++++++++++++++++++++++++

On Ubuntu ``pip`` and ``graphviz`` are installed using ``apt-get``. Remember
that graphviz is optional and that you'll need to install the graphviz
package in addition to the Python bindings.
::

   > sudo apt-get install python-pip graphviz

.. _opensuse_user:

Installing dependencies on OpenSUSE
+++++++++++++++++++++++++++++++++++

While the vanilla OpenSUSE installation includes ``pip``,
the installed version only works for Python 3. So the
python 2 version of PIP still needs to be installed. Note
that the graphviz package is installed by default.
::

    > sudo zypper install python-pip

.. warning::
    PIP for python2 on OpenSUSE is called ``pip2.7``. So you need
    to replace the ``pip`` command with ``pip2.7`` in all commands in
    the following sections. 



Installing PSyclone
^^^^^^^^^^^^^^^^^^^
Change your working directory to where you would like to place the code and 
download the latest stable release of PSyclone. 

.. parsed-literal::

   > cd <PSYCLONEHOME>
   > wget https://github.com/stfc/PSyclone/archive/\ |release|\ .tar.gz
   > gunzip \ |release|\ .tar.gz
   > tar xf \ |release|\ .tar
   > rm \ |release|\ .tar
   > cd PSyclone-\ |release|
   > export PYTHONPATH=`pwd`/src:${PYTHONPATH}
   > export PATH=`pwd`/bin:${PATH}

This sets up your python path and path appropriately. You may want to set
these paths permanently (e.g. by editing your ${HOME}/.bashrc file if you run
the BASH shell). You can also use the latest version using git, as described
in :ref:`psyclone_from_git`.

Common installation
^^^^^^^^^^^^^^^^^^^
To avoid warnings during the dependency installation, it is recommended to update ``pip``
to the latest version::

    > sudo pip install --upgrade pip

.. warning::
    As mentioned in :ref:`opensuse_user`: on OpenSUSE this commands needs to be
    ``sudo pip2.7 install --upgrade pip``.

Next you need to install the ``fparser`` and ``pyparsing`` packages::

    > sudo pip install fparser pyparsing 

.. tip::

    With ``pip`` it is possible to install packages either system-wide
    (which requires root privileges) as above, or for a single user only
    (in ~/.local). While the latter is only useful for one
    particular user, it means that PSyclone can be installed
    using ``pip`` without needing root privileges. In order to install
    a package for a user, add the --user command line option to
    all pip commands. This flag requests that the packages be installed locally
    for the current user rather than requiring root access::

        > pip install --user fparser pyparsing

    You may remove the use of ``sudo`` and add the ``--user`` option to all
    pip commands described in this document.

    Uninstalling is simply a matter of doing::

       > sudo pip uninstall fparser pyparsing

PSyclone supports the ability to output a schedule dependency graph
using the graphviz package. This is optional and the associated
routine will silently return if the graphviz bindings are not
installed. If you have the graphviz package installed (see
especially section :ref:`ubuntu_user` if you are on Ubuntu), you also need
to install the python bindings to the graphviz package:
::

   > sudo pip install graphviz

If you just want to use PSyclone then you've installed all you need
and you are ready to go to the getting-going :ref:`getting-going-run` section.

.. _dev_setup:

Developer set-up
----------------

This section adds software that is used to develop and test
PSyclone. It includes all packages for testing and creation of
documentation in html and pdf. We assume you have already installed the software
described in the :ref:`setup_user` section.

.. _psyclone_from_git:

Installing PSyclone From Git
^^^^^^^^^^^^^^^^^^^^^^^^^^^^
For development it is recommended to get a copy of PSyclone using git to get 
access to the latest development version.

Installing git for Ubuntu
+++++++++++++++++++++++++
You need to install the git package::

    > sudo apt-get install git


Installing git on OpenSUSE
++++++++++++++++++++++++++
You need to install the git package::

    >> sudo zypper --no-recommends install git


Cloning PSyclone using git
++++++++++++++++++++++++++
Cloning PSyclone from git and setting up your environment is done as follows::

   > cd <PSYCLONEHOME>
   > git clone https://github.com/stfc/PSyclone.git
   > cd PSyclone
   > pip install --user -e .

Note that the "-e" flag causes the project to be installed in 'editable' mode
so that any changes to the PSyclone source take effect immediately. On OpenSUSE
it is necessary to add $HOME/.local/bin to your $PATH.


Installing documentation tools
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Install Sphinx for creating PSyclone documentation
::

   > sudo pip install sphinx

You can now build html documentation::

   > cd doc
   > make html

The latex package is required to create the pdf documentation
for PSyclone. Installing the full dependencies can take up several GB,
the instructions for Ubuntu and OpenSUSE only install a minimal subset.

Installing LaTeX on Ubuntu
++++++++++++++++++++++++++
The following three packages need to be installed to create the pdf documentation.
It is recommended to install the packages in one ``apt-get`` command, since
otherwise, depending on your filesystem, unnecessary snapshots might be created
that take up additional space. The ``--no-install-recommends`` option
significantly reduces the number of installed packages::

   > sudo apt-get install --no-install-recommends texlive \
   texlive-latex-extra latexmk

Installing LaTeX on OpenSUSE
++++++++++++++++++++++++++++
The following command installs the minimum number of packages
in order to create the pdf documentation - around 130 packages all
in all, requiring approximately 300 MB.


.. warning::

    It is important to install the packages in one ``zypper`` command, since
    otherwise, depending on your filesystem, unnecessary snapshots might be
    created after each package, which can add up to several GB of data.

::

   > sudo zypper install --no-recommends texlive-latex texlive-latexmk \
   texlive-babel-english texlive-cmap texlive-psnfss texlive-fncychap  \
   texlive-fancyhdr texlive-titlesec texlive-tabulary texlive-varwidth \
   texlive-framed texlive-fancyvrb texlive-float texlive-wrapfig       \
   texlive-parskip texlive-upquote texlive-capt-of texlive-needspace   \
   texlive-metafont texlive-makeindex texlive-times texlive-helvetic   \
   texlive-courier texlive-dvips


Creating pdf documentation
++++++++++++++++++++++++++

You can now build the pdf documentation using
::

   > cd doc
   > make latexpdf

Installing testing tools
^^^^^^^^^^^^^^^^^^^^^^^^
The following modules are recommended to get access to testing and
formatting tools::

   > sudo pip install pytest pep8 pylint==1.6.5 pytest-cov pytest-pep8 \
   pytest-pylint pytest-flakes pytest-pep257

.. warning::
    It appears that the 1.7 series of ``pylint`` has a bug (at least up to 1.7.2)
    and does not work properly with PSyclone - it aborts with a
    "maximum recursion depth exceeded" error message. It is therefore
    recommended to use version 1.6.5, as specified in the above ``pip`` command.


You can now run the PSyclone python tests::

   > cd PSyclone.git
   > py.test

In order to see whether the Python code conforms to the pep8
standards, use::

   > pep8 code.py

Verifying the pylint standards is done with::

   > pylint code.py


OK, you're all set up.
