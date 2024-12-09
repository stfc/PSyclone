Generating Documentation
========================

PSyclone has both a
[User Guide](https://psyclone.readthedocs.io/en/stable/) and a
[Developers' Guide](https://psyclone-dev.readthedocs.io/en/stable/).
The sources for these are contained in the "user_guide" and
"developer_guide" directories, respectively.

PSyclone must be installed (see the top-level [README](../README.md)
file) before attempting to generate any documentation.

The documentation for PSyclone is created using
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

In both cases ``latexmk`` is also required to be installed on
the system in order to generate the
[pdf documentation](../psyclone.pdf).

The user documentation can make use of Sphinx's math support for
html if `dvipng` is installed. This library is not listed as
a dependency for the `doc` target due to its dependency on latex. But
if `dvipng` is available on a system,
Sphinx's math support can be enabled for the PSyclone documentation by
setting the environment variable `$SPHINXTAG` to `-t has_dvipng`.
The pdf output will always use Sphinx's math support.
