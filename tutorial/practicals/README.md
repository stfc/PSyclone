# PSyclone Hands-On Practicals

Welcome to the PSyclone hands-on practicals. Unlike those parts of the
tutorial that use Jupyter notebooks, the sections in the directories
below this one work through using PSyclone in a 'normal' Linux environment.
There are two sections to the practicals:

1. Transforming [existing code](nemo/README.md)
2. Using the [LFRic DSL](LFRic/README.md)

## Prerequisites

These practicals require:

* Some knowledge of how to use the Linux command line;
* Knowledge of how to build and run Fortran applications;
* Basic Python programming.

## Requirements

The environment and tools required for the practicals are described
below.

### Linux Environment

In order to work through the PSyclone hands-on practicals you will
need access to a terminal (command-line) on a computer running a Linux
(or MacOS) environment. If you are on Microsoft Windows then we
recommend using the Windows Subsystem for Linux (WSL). Alternatively,
you can run a Linux virtual machine (e.g. using VirtualBox or
VMWare). Whichever of these you use, we recommend that you install
Ubuntu 20.04 LTS.

* Please see
  https://docs.microsoft.com/en-us/windows/wsl/install-win10 for
  instructions on getting started with WSL. Note that WSL requires
  Windows 10 version 1607 (the Anniversary update) or better.

   * In order to have multiple windows open simultaneously you will
     need to install an X-server such as VcXsrv
     (https://sourceforge.net/projects/vcxsrv/). Once you have that
     running, set DISPLAY=localhost:0.0 in your linux terminal and you
     will then be able to launch GUI applications.

* Please see https://www.oracle.com/virtualization/technologies/vm/virtualbox.html
  for instructions on getting started with VirtualBox and the system requirements.
  Here are the basic steps:
  
   * Download the ubuntu 20.04.1 ISO from
     [here](https://ubuntu.com/download/desktop);
   * Install Virtual Box from
     https://www.oracle.com/virtualization/technologies/vm/downloads/virtualbox-downloads.html
     (using the Windows hosts link for Windows);
   * Select new in the Virtual Box manager page;
   * Choose a name for the VM and linux and ubuntu (64-bit) as the options;
   * Click start;
   * Find the ubuntu iso from the search box and add it, then follow the
     install instructions;
   * If your ubuntu screen does not resize run Devices -> Insert Guest CD.
     After doing this and after reboot you should be able to resize the VM
     screen. If not, try clicking on View -> Auto-resize guest display.
   * In order to enable copy-n-paste between the host and virtual machines
     please follow the instructions at
     https://www.howtogeek.com/187535/how-to-copy-and-paste-between-a-virtualbox-host-machine-and-a-guest-machine/

* To install VMWare:

   * As above, download the ubuntu 20.04.01 ISO;
   * Install VMware player from
     https://www.vmware.com/uk/products/workstation-player/workstation-player-evaluation.html;
   * You may be told to restart your system and re-run the installer;
   * Confirm you are using it for non-commercial use and skip the update
     to pro;
   * Click on create a new VM, find the ubuntu ISO and install;
   * If you get "Your host does not meet minimum requirements to run VMware
     Player with Hyper-V or Device/Credential Guard enabled. Refer to VMware
     KB article 76918 at https://kb.vmware.com/kb/76918 for more details."
     then it is probably best to install Virtual Box instead. If not, google
     how to fix the problem.


In the instructions below we assume that you are using Ubuntu. If you
are using some other Linux distribution you will have to change the
package-manager commands as appropriate.

### Python

Your Linux environment must have a recent version of Python
installed. Additionally, we recommend working with a Python virtual
environment which virtualenvwrapper makes quite straightforward (see
e.g. https://docs.python-guide.org/dev/virtualenvs/). Some of the
commands in the remainder of this document assume a virtual
environment so would need to be changed if one is not used (e.g. use
the `--user` flag to any pip commands so as to install packages as a
local user).

We recommend using pip to install Python packages. To get pip (for Python 3):

```bash
$ sudo apt install python3-pip
```

If you are going to use virtualenvwrapper then install it with pip:

```bash
$ sudo pip3 install virtualenvwrapper
```

and then perform some environment setup:

```bash
$ export WORKON_HOME=~/Envs
$ mkdir -p $WORKON_HOME
$ export VIRTUALENVWRAPPER_PYTHON=/usr/bin/python3
$ source /usr/local/bin/virtualenvwrapper.sh
$ mkvirtualenv tutorial
```

Note that the 1st, 3rd and 4th steps will have to be performed each
time you open a shell and therefore you may want to put them in your
`${HOME}/.bashrc` file.


At this point you will be working within the 'tutorial'
environment. To leave this use the 'deactivate' command:

```bash
$ deactivate
```

To return to it:

```bash
$ workon tutorial
```

For the session on running LFRic examples, you will need numpy and
matplotlib packages to plot outputs of runs:

```bash
$ pip install numpy matplotlib
```

In order to get colour highlighting when viewing the PSyclone Internal
Representation, we recommend installing the termcolor package:

```bash
$ pip install termcolor
```

### Git

PSyclone lives in a git repository hosted on
[GitHub](https://github.com/stfc/PSyclone). All of the hands-on
practicals are also in this repository. Your Linux environment must
therefore have git installed (try typing `git` at the command
prompt). See
e.g. https://git-scm.com/book/en/v2/Getting-Started-Installing-Git for
guidance on how to do this but on Ubuntu:

```bash
$ sudo apt install git
```

### Fortran Compiler

The majority of the PSyclone tutorials require that your system have a
Fortran compiler available. The Gnu Fortran compiler (gfortran) is
free and easily installed, e.g. on Ubuntu:

```bash
$ sudo apt install gfortran
```

For the session on adding OpenACC to NEMO, you may wish to be able to
compile the generated OpenACC code. This requires version 8 or higher
of gfortran. Alternatively, the NVIDIA HPC SDK is also freely
available (https://developer.nvidia.com/hpc-sdk). This has the
advantage that the NVIDIA Fortran compiler is also capable of
compiling OpenACC code to target multi-core CPUs as well as GPUs.

LFRic code is routinely built with the Gnu Fortran compiler 6.1.0. and
later, and it is proven to work with the latest GCC 10.2.0.

### Make

Many of the PSyclone tutorials have code that is built using a
Makefile. Your Linux environment will therefore need `make`
installed. Ideally this will be Gnu Make but if that is not available
the Makefiles can still be used after a little editing. Gnu Make is
already installed in Ubuntu.

### NetCDF library (LFRic examples)

The NetCDF library (https://www.unidata.ucar.edu/software/netcdf/),
together with its Fortran and C++ bindings, is required for reading in
LFRic mesh files. The latest netcdf-c-4.7.3, NetCDF Fortran binding
netcdf-fortran-4.5.2 and C++ binding netcdf-cxx4-4.3.1 are proven to
work for the LFRic code and are advisable versions for these
practicals. If you're using gfortran then the simplest way of installing
these is probably to use your package manager, e.g. on Ubuntu:

```bash
$ sudo apt install libnetcdf-dev libnetcdff-dev
```

(see e.g. this page for an overview
https://cloud-gc.readthedocs.io/en/latest/chapter04_developer-guide/install-basic.html#install-netcdf-with-package-manager). If
you're not using gfortran then you will probably have to build NetCDF
yourself. This process is described here:
https://www.unidata.ucar.edu/software/netcdf/documentation/NUG/getting_and_building_netcdf.html.

## Setup and Validation

In order to check that you have everything set-up correctly and are
ready for the PSyclone hands-on practicals, perform the following steps:

As mentioned earlier, using a Python virtual environment is optional
(but recommended). If you are using one and are not already in it then
activate it:

```bash
$ workon tutorial
```

Clone the PSyclone repository (and its submodules) and then install it
(if you're not using a Python virtual environment you will probably
want to supply the `--user` flag to `pip install`):

```bash
$ git clone --recursive https://github.com/stfc/PSyclone.git
$ cd PSyclone
$ pip install .[psydata]
```

(Note, the square brackets in the last command must be included!)

Finally, check that you can run one of the examples:

```bash
$ cd examples/lfric/eg4
$ make
```

This should produce Fortran output ending with:

```fortran
        END SUBROUTINE invoke_jacobi_iteration
  END MODULE solver_mod_psy
```

At this point you have PSyclone installed and working correctly.


In order to test that you have a working Fortran compiler along with
the necessary dependencies then doing:

```bash
$ cd examples/lfric/eg17/full_example_netcdf
$ make
```

should result in an executable named `example` being compiled. This
assumes that you are using gfortran with the NetCDF libraries
installed in standard locations (as they will be if you use a package
manager). If either of those things are not true then you will need to
set F90 and F90FLAGS appropriately.
