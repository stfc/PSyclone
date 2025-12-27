.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2019-2025, Science and Technology Facilities Council.
.. All rights reserved.
..
.. Redistribution and use in source and binary forms, with or without
.. modification, are permitted provided that the following conditions are met:
..
.. * Redistributions of source code must retain the above copyright notice, this
..   list of conditions and the following disclaimer.
..
.. * Redistributions in binary form must reproduce the above copyright notice,
..   this list of conditions and the following disclaimer in the documentation
..   and/or other materials provided with the distribution.
..
.. * Neither the name of the copyright holder nor the names of its
..   contributors may be used to endorse or promote products derived from
..   this software without specific prior written permission.
..
.. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
.. "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
.. LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
.. FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
.. COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
.. INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
.. BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
.. LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
.. CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
.. LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
.. ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
.. POSSIBILITY OF SUCH DAMAGE.
.. -----------------------------------------------------------------------------
.. Written by J. Henrichs, , Bureau of Meteorology
.. Modified by A. R. Porter, STFC Daresbury Lab

.. testsetup::

    import os
    from psyclone.parse import ModuleManager

.. _module_manager:



Module Manager
##############

PSyclone uses a ``ModuleManager`` to handle searching for files containing
Fortran modules. This object acts as the top-level interface to the
code making up a program. It may be used to obtain the PSyIR for each
Container (Fortran module) in a code. It is used by the Container import
interface and the :ref:`psyke`. For the latter it
is used
to discover all of the source files required to make a standalone driver.

The :ref_guide:`ModuleManager psyclone.parse.html#psyclone.parse.ModuleManager`
is a singleton which must be obtained via
`ModuleManager.get()`. Having obtained the instance, it may be used to
search for a particular module via the `get_module_info` method:

.. automethod:: psyclone.parse.ModuleManager.get_module_info
    :no-index:

Any PSyclone command line option ``-d`` (see :ref:`psyclone_command`)
will be added to the ``ModuleManager`` as recursive search
paths. Internally, the ``ModuleManager`` uses caching to avoid
repeatedly searching directories, and it will only access search paths
as required. For example, if it should happen that the first search
path is sufficient to find all modules during the lifetime of the
module manager, no other search path will ever be accessed.  The
caching also implies that the ModuleManager will *not* detect any new
files created during its lifetime.

Rather than rely on any particular naming convention to identify which
Fortran source file contains a given module, a measure of the
'similarity' of the target module name and the base name (i.e. the
filename stripped of any path and suffix) of any source file is used
to identify likely candidates. The standard Python
``difflib.SequenceMatcher.ratio`` method is used to obtain the
similarity score. If the score is above a certain threshold (currently
set to 0.7 in the ``ModuleManager`` class) then the file is read, its
contents cached within a :ref_guide:`FileInfo
psyclone.parse.html#psyclone.parse.FileInfo` object, and a regular
expression used to determine whether or not it does contain the target
module. This approach has been designed to minimise IO activity since
this could get very costly on the types of shared filesystem common on
HPC resources. The use of a ``FileInfo`` object also facilitates the
decoupling of the concept of a file from that of a module since the
former can contain more than one of the latter.

The ``ModuleManager`` will return a :ref_guide:`ModuleInfo
psyclone.parse.html#psyclone.parse.ModuleInfo` object to make
information about a module available.
Similar to the ``ModuleManager``, a ``ModuleInfo`` object relies heavily on
caching to avoid repeatedly reading a source file or parsing it. The side
effect is that changes to a source file during the lifetime of the
``ModuleManager`` will not be reflected in its information.

The ``ModuleManager`` also provides a static function that will sort
a list of module dependencies, so that compiling the modules in this order
(or adding them in this order to a file) will allow compilation, i.e. any
module will only depend on previously defined modules:

.. automethod:: psyclone.parse.ModuleManager.sort_modules
    :no-index:

Once a ``ModuleInfo`` has been obtained, its primary role is to provide
access to the PSyIR of the ``Container`` representing the module:

.. automethod:: psyclone.parse.ModuleInfo.get_psyir
    :no-index:

However, it also provides methods (``get_used_module_names``,
``get_used_symbols_from_modules``) for interrogating the parse tree which
can be useful if it is not possible to represent this in PSyIR.

An example usage of the ``ModuleManager`` and ``ModuleInfo`` objects,
which prints the filenames of all modules used in ``tl_testkern_mod``:

.. testcode ::

    mod_manager = ModuleManager.get()
    # Add the path to the PSyclone LFRic example codes:
    mod_manager.add_search_path("../../src/psyclone/tests/test_files/"
                                "lfric")

    testkern_info = mod_manager.get_module_info("tl_testkern_mod")

    used_mods = testkern_info.get_used_module_names()
    # Sort the modules so we get a reproducible output ordering
    used_mods_list = sorted(list(used_mods))
    for module_name in used_mods_list:
        mod_info = mod_manager.get_module_info(module_name)
        print("Module:", module_name, os.path.basename(mod_info.filename))

.. testoutput::

    Module: argument_mod argument_mod.f90
    Module: constants_mod constants_mod.f90
    Module: fs_continuity_mod fs_continuity_mod.f90
    Module: kernel_mod kernel_mod.f90



FileInfo
========

FileInfo is a class that is used to store information about Fortran files.

This information can include:

- The source code itself
- The fparser tree information
- The PSyIR tree information

All this information is gathered in this single class since this also
allows for caching of it, see next section



Caching
=======

The `ModuleManager` and `FileInfo` support a caching of the
fparser tree representation of a source code.
(Support for PSyIR is planned)

This caching has to be **explicitly enabled** in the constructor
of `ModuleManager`.


.. testcode ::

    ModuleManager.get().cache_active = True


Most of the time in the PSyIR generation is currently spent in the
fparser tree generation. Consequently, this leads to significant
speed-ups in the process of reading and parsing the source code
of modules.



Default cache file locations
----------------------------


The default cache file is named the same way as the source file,
but replaces the file extension with `.psycache`. E.g., a cache file
for the source file `foo.f90` will be called `foo.psycache`.



(Global) cache file folder
--------------------------

To avoid storing cache files together with source code files,
a path can be provided to the module manager.

.. testcode ::

    mod_manager = ModuleManager.get()
    mod_manager.cache_active = True
    mod_manager.cache_path = "/tmp/my_cache_path"

A cache file name will then be created based on the hashsum of each
source code file. The combination of the provided `cache_path` and
the cache file name will then be used as the storage location.

Note, that the cache path directory must exist.



Caching algorithm
-----------------

The caching algorithm to obtain the fparser tree OR PSyIR is briefly described as follows:

- If fparser tree / PSyIR was read before: RETURN fparser tree or PSyIR
- If source code is not yet read:

    - Read the content of the file
    - Create the source's checksum.
- Read cache file if it exists:

    - If the checksum of the cache is the same as the one of the source:

        - load the fparser tree / PSyIR from the cache file and RETURN fparser tree or PSyIR
- Create the fparser tree / PSyIR from the source code
- Save cache file IF it was not loaded before:

    - Update cache information
    - Store to cache file
- RETURN fparser tree or PSyIR
