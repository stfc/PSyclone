Generating Documentation
========================

PSyclone documentation for ['stable' (last release)](https://psyclone.readthedocs.io/en/stable/)
and ['latest' (last master commit)](https://psyclone.readthedocs.io/en/latest/)
are published online.

To generate the documentation locally the 'doc' PSyclone dependencies
must be installed (e.g. `pip install -e .[doc]`).
The documentation is created using
[Sphinx](https://www.sphinx-doc.org). Documentation can be created in a
number of formats, such as html, latex and pdf. To see the options
"cd" to either the "user_guide" or "developer_guide" directory and
type "make" if on a linux system or run the "make.bat" script if on a
windows system.

Documentation dependencies
==========================

The dependencies required to build the documentation are listed in
the [setup.py](../setup.py) script (under ``extras_required``),
and can be installed automatically with:

    pip install psyclone[doc]

For a development installation, run the following from the root
of the repository:

    pip install -e .[doc]

For generating the pdf versions of the documentation and the doxygen reference
guide, ``latexmk`` and ``doxygen`` respectively need to be installed in the
system.

The user documentation can make use of Sphinx's math support for
html if `dvipng` is installed. This library is not listed as
a dependency for the `doc` target due to its dependency on latex. But
if `dvipng` is available on a system,
Sphinx's math support can be enabled for the PSyclone documentation by
setting the environment variable `$SPHINXTAG` to `-t has_dvipng`.
The pdf output will always use Sphinx's math support.
