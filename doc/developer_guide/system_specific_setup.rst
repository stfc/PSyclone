.. _system_specific_dev_setup:

System-specific Developer Set-up
================================

Section :ref:`user_guide:system_specific_setup` in the PSyclone User Guide
describes the setup for a user of PSyclone. It includes all steps necessary
to be able to use PSyclone. And while you could obviously do
some development, none of the required tools for testing or
documentation creation will be installed.

This section adds software that is used to develop and test
PSyclone. It includes all packages for testing and creation of
documentation in html and pdf. We assume you have already installed
the software described in the :ref:`user_guide:system_specific_setup` section.

It contains detailed instructions for Ubuntu 16.04.2 and 
OpenSUSE 42.2 - if you are working with a different Linux
distribution some adjustments will be necessary.

.. _psyclone_from_git:

Installing PSyclone From GitHub
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
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


Cloning PSyclone Using git
++++++++++++++++++++++++++
Cloning PSyclone from git and setting up your environment is done as follows::

   > cd <PSYCLONEHOME>
   > git clone --recursive https://github.com/stfc/PSyclone.git
   > cd PSyclone
   > pip install --user -e .

Note that the "-e" flag causes the project to be installed in
'editable' mode so that any changes to the PSyclone source take effect
immediately. However, it also means that the PSyclone configuration
file is not installed so you will have to do that manually (see the
:ref:`configuration` section of the User Guide).

.. warning::

   On OpenSUSE it is necessary to add `$HOME/.local/bin` to
   your `$PATH` if you have done a user-local install.


Installing Documentation Tools
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Install Sphinx along with bibtex support for creating PSyclone documentation::

   > sudo pip install sphinx sphinxcontrib.bibtex

You can now build html documentation::

   > cd doc
   > make html

The latex package is required to create the pdf documentation
for PSyclone. Installing the full dependencies can take up several GB,
the instructions for Ubuntu and OpenSUSE only install a minimal subset.

Installing LaTeX on Ubuntu
++++++++++++++++++++++++++
The following four packages need to be installed to create the pdf documentation.
It is recommended to install the packages in one ``apt-get`` command, since
otherwise, depending on your filesystem, unnecessary snapshots might be created
that take up additional space. The ``--no-install-recommends`` option
significantly reduces the number of installed packages::

   > sudo apt-get install --no-install-recommends texlive \
   texlive-latex-extra latexmk tex-gyre

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


Creating PDF Documentation
++++++++++++++++++++++++++

You can now build the pdf documentation using
::

   > cd doc
   > make latexpdf

Creating Markdown Documentation
+++++++++++++++++++++++++++++++

The documentation for PSyclone's Examples and Tutorials is written in
`GitHub-flavoured Markdown
<https://github.github.com/gfm/>`_. Although this requires no special
tools to create, checking that it renders correctly means pushing your
changes to GitHub and then visiting the file in GitHub's source
browser. This may be avoided by using `grip
<https://github.com/joeyespo/grip>`_. Once installed, simply start
`grip` in the directory containing the file you are working on and
point your web browser at the URL it displays, e.g.::

 > grip
 * Serving Flask app "grip.app" (lazy loading)
 * Environment: production
   WARNING: This is a development server. Do not use it in a production deployment.
   Use a production WSGI server instead.
 * Debug mode: off
 * Running on http://localhost:6419/ (Press CTRL+C to quit)


Installing Testing Tools
^^^^^^^^^^^^^^^^^^^^^^^^

You can install the necessary dependencies to run the PSyclone tests with::

    > pip install psyclone[test]

or when using the git version::

    > pip install .[test]


The test dependencies are canonically documented in PSyclone's setup.py
under the ``extras_requires`` section. This installs the recommended
tools to get access to testing and formatting tools.

.. warning::
    It appears that the 1.7 series of ``pylint`` has a bug (at least up to 1.7.2)
    and does not work properly with PSyclone - it aborts with a
    "maximum recursion depth exceeded" error message. It is therefore
    recommended to avoid pylint 1.7.


You can now run the PSyclone python tests::

   > cd PSyclone.git
   > pytest

In order to see whether the Python code conforms to the pep8
standards, use::

   > pycodestyle code.py

.. note::
    ``pycodestyle`` is a replacement for the older ``pep8`` program.


Verifying the pylint standards is done with::

   > pylint code.py


OK, you're all set up.

Installing Tools for PSyData Wrapper Libraries
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
If you intend to compile the PSyData wrapper libraries or develop new libraries,
you might need to install Jinja2 (most wrapper libraries require Jinja2 though
some, like the NVIDIA GPU profiling wrapper, do not need it). You can install
the necessary dependencies to create all PSyData wrapper libraries with::

    > pip install psyclone[psydata]

or when using the git version::

    > pip install .[psydata]


Check :ref:`psy_data` and especially the section :ref:`jinja` for more details.
