.. _system_specific_setup:

System-specific Set-up for Users
================================

This chapter describes the setup for a user of PSyclone. It includes
all steps necessary to be able to use PSyclone. And while you could
obviously do some development, none of the required tools for testing
or documentation creation will be installed.

The :ref:`dev_guide:system_specific_dev_setup` in the Developers' Guide
describes the additional installation of all required tools to run
tests and create documentation.

Detailed instructions are provided for Ubuntu 16.04.2 and 
OpenSUSE 42.2 - if you are working with a different Linux
distribution some adjustments will be necessary.
You will need a terminal window open in which to enter the commands.

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

The vanilla OpenSUSE installation includes ``pip`` for Python 3.
Note that the graphviz package is installed by default.
::

    > sudo zypper install python-pip



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
