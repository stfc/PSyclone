.. -----------------------------------------------------------------------------
.. BSD 3-Clause License
..
.. Copyright (c) 2019-2024, Science and Technology Facilities Council.
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

    # Define SOURCE_FILE to point to an existing gocean 1.0 file.
    SOURCE_FILE = ("../../src/psyclone/tests/test_files/"
        "gocean1p0/test11_different_iterates_over_one_invoke.f90")

Module Manager
##############

PSyclone uses a `ModuleManager` to handle searching for files containing
Fortran modules. This object acts as the top-level interface to the
code making up a program. It may be used to obtain the PSyIR for each
Container (Fortran module) in a code. It is used by the Container import
interface and the PSyclone Kernel Extractor. For the latter it is used
to discover all of the source files required to make a standalone driver.

.. autoclass:: psyclone.parse.ModuleManager
    :members:

Any PSyclone command line option ``-d`` (see :ref:`psyclone_command`)
will be added to the ``ModuleManager`` as recursive search paths. The
``ModuleManager`` is a singleton and it can be queried for information about
any module. It internally uses caching to avoid repeatedly searching
directories, and it will only access search paths as required. For example,
if the first search path will be sufficient to find all modules during the
lifetime of the module manager, no other search path will ever be accessed.
The caching also implies that the ModuleManager will *not* detect any new files
created during its lifetime.

The ``ModuleManager`` also provides a static function that will sort
a list of module dependencies, so that compiling the modules in this order
(or adding them in this order to a file) will allow compilation, i.e. any
module will only depend on previously defined modules.

The ``ModuleManager`` will return a ``ModuleInfo`` object to make information
about a module available:

.. autoclass:: psyclone.parse.ModuleInfo
    :members:

Similar to the ``ModuleManager``, a ``ModuleInfo`` object will heavily rely on
caching to avoid repeatedly reading a source file or parsing it. The side
effect is that changes to a source file during the lifetime of the
``ModuleManager`` will not be reflected in its information.

At this stage, the ``ModuleInfo`` can be used to get the original source
code of a module as string and to query a module about modules and symbols
it depends on. It uses the fparser parse tree to detect this information (which
means it can handle files that are not supported by PSyIR, e.g. files with
preprocessor directives).

An example usage of the ``ModuleManager`` and ``ModuleInfo`` objects,
which prints the filenames of all modules used in ``tl_testkern_mod``:

.. testcode ::

    mod_manager = ModuleManager.get()
    # Add the path to the PSyclone LFRic example codes:
    mod_manager.add_search_path("../../src/psyclone/tests/test_files/"
                                "dynamo0p3")

    testkern_info = mod_manager.get_module_info("tl_testkern_mod")

    used_mods = testkern_info.get_used_modules()
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
