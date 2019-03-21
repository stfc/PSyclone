Generating Documentation
========================

PSyclone must be installed (see the top-level [README](../README.md)
file) before attempting to generate the documentation.

The documentation for PSyclone is created using
[Sphinx](http://sphinx-doc.org). Documentation can be created in a
number of formats, such as html, latex and pdf. To see the options
"cd" to the "doc" directory and type "make" if on a linux system or
run the "make.bat" script if on a windows system.

Documentation dependencies
==========================

The dependencies required to build the documentation are listed in
the [setup.py](../setup.py) script (under ``extras_required``),
and can be installed automatically with:

    pip install psyclone[doc]

For a development installation, run the following from the root
of the repository:

    pip install -e .[doc]

In both cases ``latexmk`` is also required to be installed on
the system in order to generate the
[pdf documentation](../psyclone.pdf).
