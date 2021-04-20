# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2021, Science and Technology Facilities Council.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
# * Redistributions of source code must retain the above copyright notice, this
#   list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
#
# * Neither the name of the copyright holder nor the names of its
#   contributors may be used to endorse or promote products derived from
#   this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
# -----------------------------------------------------------------------------
# Author: A. J. Voysey, Met Office

''' Test exception classes to ensure consistent __repr__ & __str__ methods. '''

from __future__ import absolute_import

import sys
import pkgutil
import pytest
import psyclone
import inspect
import os
import importlib


def all_sub_exceptions(expt):
    ''' Recursively find the set of all the exceptions which are subclasses of
        the given exception class. '''

    new_sub_except = [add_except for sub_except in expt.__subclasses__()
                     for add_except in all_sub_exceptions(sub_except)]
    return set(expt.__subclasses__()).union(new_sub_except)


def test_exception_repr():
    ''' Test the properties of Exception classes defined by Psyclone. '''

    modules = dict()
    allclass = set()

    # Recursively walk through the psyclone module, importing sub-modules.
    # Store any class definitions we come across.

    modules["psyclone"] = True
    while any(not_imported for _, not_imported in modules.items()):
        for module, is_pkg in list(modules.items()):
            # if the submodule is also a package (and so not yet imported),
            # then parse it now...
            if is_pkg:
                # import this submodule, and record that it no longer needs
                # to be imported
                module_handle = importlib.import_module(module)
                modules[module] = False

                # record any classes defined in it
                module_path = module_handle.__path__
                for _, obj in inspect.getmembers(module_handle):
                    if inspect.isclass(obj):
                        if obj.__module__ == module:
                            allclass.add(obj)

                # As it is a package, look for submodules
                for _, sub_mod, s_is_pkg in pkgutil.iter_modules(module_path):

                    # record the submodule exists
                    full_sub_mod_name = module + "." + sub_mod
                    modules[full_sub_mod_name] = s_is_pkg

                    # if the new submodule is *not* a package, import it and
                    # look for defined classes now. If it is a package, it will
                    # be imported on a later iteration.
                    if not s_is_pkg:
                        s_m_handle = importlib.import_module(full_sub_mod_name)
                        for _, obj in inspect.getmembers(s_m_handle):
                            if inspect.isclass(obj):
                                if obj.__module__ == full_sub_mod_name:
                                    allclass.add(obj)

    # Find the set of all the exceptions (all defined classes which are
    # subclasses of the Exception clss.)
    # The intersection of this set and the set of classes we found earlier
    # is the set of all exceptions defined by Psyclone

    all_excpetions = all_sub_exceptions(Exception)
    all_psyclone_exceptions = all_excpetions.intersection(allclass)

    # Different vertions of pytest behave differently with repect to their
    # handeling of an exception's representation. This can lead to some tests
    # passing/failing assertions depending on which pytest version is
    # installed.
    #
    # To avoid this we will enforce the following conditions for exceptions
    # defined by psyclone: -
    # i) Exceptions will implement their own __str__ and __repr__ methods.
    # ii) These will not be the same as each other for a given exception.
    # iii) The string returned by the __str__ method will not be contained
    #      with that returned by the __repr__ method,
    #
    # When these conditions are met, assertion behaviour is consistent across
    # all pytest verions.

    for psy_except in list(all_psyclone_exceptions):

        # Ensure there are __str__ & __repr__ methods implemented which are not
        # inherited from the parent Exception class
        assert psy_except.__str__ is not Exception.__str__
        assert psy_except.__repr__ is not Exception.__repr__

        # Simulate argements to the exception constructor
        if sys.version_info[0] == 3:
            arglist = list(inspect.getfullargspec(psy_except).args)
        else:
            arglist = list(inspect.getargspec(psy_except.__init__).args)
        args = [None for arg in arglist if arg != 'self']

        # Check that the _str__ & __repr__ do not return the same struing, and
        # that one is not contained within the other
        if len(args) > 0:
            assert str(psy_except(*args)) != str(repr(psy_except(*args)))
            assert str(psy_except(*args)) not in repr(psy_except(*args))
        else:
            assert str(psy_except()) != repr(psy_except())
            assert str(psy_except()) not in repr(psy_except())
