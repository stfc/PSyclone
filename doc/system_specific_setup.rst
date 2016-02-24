.. _Ubuntu14.03.3:

System-specific set-up
======================

This section provides system-specific information on how to set-up your system to use PSyclone.

Ubuntu 14.04.3
--------------

This guide has been tested with a vanilla installation of Ubuntu 14.04.3.

.. _users:

User set-up
+++++++++++

Get a terminal window. You can do this by pressing <ctrl><Alt><t>
together, or click the top left "search" icon and type "terminal".

You will need subversion to download the PSyclone software. In the
terminal type:
::

    > sudo apt-get install subversion

Check out a copy of PSyclone to your required location (where <PSYCLONEHOME> refers to where you would like to place the code)

::

   > cd <PSYCLONEHOME>
   > svn co https://puma.nerc.ac.uk/svn/GungHo_svn/PSyclone/trunk PSyclone_trunk

Set your python path appropriately
::

   > cd PSyclone_trunk
   > export PYTHONPATH=`pwd`/src:`pwd`/f2py_93

You may want to set your python path permanently (e.g. by editing your
${HOME}/.bashrc file if you run the BASH shell).

numpy is required to run PSyclone
::

   > sudo apt-get install python-numpy

pyparsing is required by PSyclone
::

   > sudo apt-get install python-pyparsing

If you just want to use PSyclone then you've installed all you need
and you're ready to go to the getting-going :ref:`getting-going-run`
section.

Developer set-up
++++++++++++++++

This section adds software that is used to develop and test
PSyclone. Note, we assume you have already installed the software
described in the :ref:`users` section.

Install pytest for running python tests
::

   > sudo apt-get install python-pytest

You can now run the PSyclone python tests
::

   > cd PSyclone_trunk/src/tests
   > py.test

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

Finally, include useful pytest extensions. To do this you will first
need to install pip
::

    > sudo apt-get install python-pip

You can now install the extensions
::

    > sudo pip install pytest-cov
    > sudo pip install pytest-pep8
    > sudo pip install pytest-pylint
    > sudo pip install pytest-flakes
    > sudo pip install pytest-pep257

OK, you're all set up.
