<p align="left">
  <img width="460" src="doc/logo/psyclone_v1.0.png">
</p>

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.11190458.svg)](https://doi.org/10.5281/zenodo.11190458)
![Build Status](https://github.com/stfc/PSyclone/workflows/PSyclone%20tests%20and%20examples/badge.svg)
[![codecov](https://codecov.io/gh/stfc/PSyclone/branch/master/graph/badge.svg)](https://codecov.io/gh/stfc/PSyclone)

PSyclone is a source-to-source Fortran compiler designed to programmatically
optimise, parallelise and instrument HPC applications via user-provided
transformation scripts. By encapsulating the performance-portability aspects
(e.g. whether to parallelise with OpenMP or OpenACC), these scripts enable
a separation of concerns between the scientific implementation and the
optimisation choices. This allows each aspect to be explored and developed
largely independently.
Additionally, PSyclone supports the development of kernel-based Fortran-embedded
DSLs following the PSyKAl model developed in the
[GungHo project](https://www.metoffice.gov.uk/research/foundation/dynamics/next-generation).

PSyclone is currently used to support the [LFRic mixed finite-element PSyKAl
DSL](https://psyclone.readthedocs.io/en/latest/dynamo0p3.html)
for the [UK MetOffice's next generation modelling
system](https://www.metoffice.gov.uk/research/modelling-systems/lfric/) and
the [GOcean finite-difference PSyKAl
DSL](https://psyclone.readthedocs.io/en/latest/gocean1p0.html) for a [prototype
2D ocean modelling system](https://gtr.ukri.org/projects?ref=NE%2FL01209X%2F1).
It is also used to [insert GPU offloading
directives](https://psyclone.readthedocs.io/en/latest/nemo.html) into existing
directly-addressed MPI applications such as the
[NEMO ocean model](https://www.nemo-ocean.eu/).

For more detailed information see the [psyclone.pdf](psyclone.pdf) in this
directory or the [PSyclone User Guide](http://psyclone.readthedocs.io).

# Installation #

You can install the latest release of psyclone from PyPI by using:

    $ pip install psyclone

or, if you want an isolated installation in a [python virtual
environment](https://docs.python.org/3/library/venv.html):

    $ python -m venv <virtual_env_name>
    $ source <virtual_env_name>/bin/activate
    $ pip install psyclone

Alternatively, you can install the latest upstream version of psyclone by
cloning this repository and using:

    $ pip install .

For more information about the installation process see
[this section of the User Guide](https://psyclone.readthedocs.io/en/latest/system_specific_setup.html).

<!---  TODO #2627
# Try it on Binder #

Some of the examples are available as Jupyter notebooks. These may
be launched using Binder from the links below. (Note that the first time
this is done, Binder has to construct a Container and install the necessary
software. This can take several minutes. You can track its progress
by clicking the 'show' link next to the 'Build logs' heading.)

 * [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/stfc/psyclone/master?filepath=tutorial%2Fnotebooks%2Fintroduction.ipynb) The **PSyclone Tutorial**. Note that we currently recommend following the more up-to-date README files under the `tutorials/practicals` directory.

 * [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/stfc/psyclone/master?filepath=examples%2Fgocean%2Feg1%2Fopenmp.ipynb) Uses PSyclone's GOcean API to process example code that conforms to the PSyKAl separation of concerns. Transformations are applied in order to fuse various loops before parallelising the result with OpenMP.
 
 * [![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/stfc/psyclone/master?filepath=examples%2Fgocean%2Feg1%2Fdag.ipynb) demonstrates the generation of a DAG for the PSy layer of the previous example.
--->

# Structure #

Path                | Description
------------------- | -----------
bin/                | Top-level driver scripts for PSyclone and the PSyclone kernel tool
changelog      	    | Information on changes between releases
doc/           	    | Documentation source using Sphinx
examples/      	    | Simple examples
psyclone.pdf   	    | Generated documentation
README.md      	    | This file
README.gource  	    | Information on how to generate a gource video from the repository
README.uml     	    | Information on how to create UML class diagrams from the source using pyreverse
src/psyclone   	    | The Python source code
src/psyclone/tests/ | Unit and functional tests using pytest
tutorial/notebooks  | Tutorial using Jupyter notebooks
tutorial/practicals | Hands-on exercises using a local installation of PSyclone
