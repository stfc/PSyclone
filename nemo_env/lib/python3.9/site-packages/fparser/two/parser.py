# Modified work Copyright (c) 2018-2023 Science and Technology
# Facilities Council.
# Original work Copyright (c) 1999-2008 Pearu Peterson

# All rights reserved.

# Modifications made as part of the fparser project are distributed
# under the following license:

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:

# 1. Redistributions of source code must retain the above copyright
# notice, this list of conditions and the following disclaimer.

# 2. Redistributions in binary form must reproduce the above copyright
# notice, this list of conditions and the following disclaimer in the
# documentation and/or other materials provided with the distribution.

# 3. Neither the name of the copyright holder nor the names of its
# contributors may be used to endorse or promote products derived from
# this software without specific prior written permission.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

# --------------------------------------------------------------------

# The original software (in the f2py project) was distributed under
# the following license:

# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:

#   a. Redistributions of source code must retain the above copyright notice,
#      this list of conditions and the following disclaimer.
#   b. Redistributions in binary form must reproduce the above copyright
#      notice, this list of conditions and the following disclaimer in the
#      documentation and/or other materials provided with the distribution.
#   c. Neither the name of the F2PY project nor the names of its
#      contributors may be used to endorse or promote products derived from
#      this software without specific prior written permission.

# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE FOR
# ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
# DAMAGE.

"""This file provides utilities to create a Fortran parser suitable
for a particular standard."""

import inspect
import logging
import sys
from fparser.two.symbol_table import SYMBOL_TABLES


def get_module_classes(input_module):
    """
    Return all classes local to a module.

    :param module input_module: the module containing the classes.

    :returns: list of class names and types.
    :rtype: List[Tuple[str, type]]

    """
    module_cls_members = []
    module_name = input_module.__name__
    # First find all classes in the module. This includes imported classes.
    all_cls_members = inspect.getmembers(sys.modules[module_name], inspect.isclass)
    # next only keep classes that are specified in the module.
    for name, cls in all_cls_members:
        if cls.__module__ == module_name:
            module_cls_members.append((name, cls))
    return module_cls_members


class ParserFactory:
    """Creates a parser suitable for the specified Fortran standard."""

    def create(self, std=None):
        """Creates a class hierarchy suitable for the specified Fortran
        standard. Also sets-up the list of classes that define scoping
        regions in the global SymbolTables object and clears any existing
        symbol table information.

        :param str std: the Fortran standard. Choices are 'f2003' or \
                        'f2008'. 'f2003' is the default.
        :return: a Program class (not object) for use with the Fortran reader
        :rtype: :py:class:`fparser.two.Fortran2003.Program`

        :raises ValueError: if the supplied value for the std parameter \
                            is invalid

        For example:

        >>> from fparser.two.parser import ParserFactory
        >>> f2003_parser = ParserFactory().create()
        >>> f2003_parser = ParserFactory().create(std='f2003')
        >>> f2008_parser = ParserFactory().create(std='f2008')
        >>> # Assuming that a reader has already been created ...
        >>> ast = f2008_parser(reader)
        >>> print(ast)

        """
        # Clear any existing symbol tables.
        SYMBOL_TABLES.clear()

        # find all relevant classes in our Fortran2003 file as we
        # always need these.
        # pylint: disable=import-outside-toplevel
        from fparser.two import Fortran2003

        f2003_cls_members = get_module_classes(Fortran2003)
        if not std:
            # default to f2003.
            std = "f2003"

        if std == "f2003":
            # we already have our required list of classes so call _setup
            # to setup our class hierarchy.
            self._setup(f2003_cls_members)
            # The class hierarchy has been set up so return the top
            # level class that we start from when parsing Fortran code.
            return Fortran2003.Program
        if std == "f2008":
            # we need to find all relevent classes in our Fortran2003
            # and Fortran2008 files and then ensure that where classes
            # have the same name we return the Fortran2008 class
            # i.e. where Fortran2008 extends Fortran2003 we return
            # Fortran2008.
            # First find all Fortran2008 classes.
            from fparser.two import Fortran2008

            f2008_cls_members = inspect.getmembers(
                sys.modules[Fortran2008.__name__], inspect.isclass
            )

            # next add in Fortran2003 classes if they do not already
            # exist as a Fortran2008 class.
            f2008_class_names = [i[0] for i in f2008_cls_members]
            for local_cls in f2003_cls_members:
                if local_cls[0] not in f2008_class_names:
                    f2008_cls_members.append(local_cls)
            # we now have our required list of classes so call _setup
            # to setup our class hierarchy.
            self._setup(f2008_cls_members)
            # The class hierarchy has been set up so return the top
            # level class that we start from when parsing Fortran
            # code. Fortran2008 does not extend the top level class so
            # we return the Fortran2003 one.
            return Fortran2003.Program

        raise ValueError(f"'{std}' is an invalid standard")

    def _setup(self, input_classes):
        """Perform some Python magic to create the connections between classes
        and populate the baseclass with this information. This has
        been lifted from the original implementation and no attempt
        has been made to tidy up the code, other than making it
        conformant to the coding rules.

        :param list input_classes: a list of tuples each containing a \
        class name and a class.

        """
        # pylint: disable=import-outside-toplevel
        from fparser.two import Fortran2003

        class_type = type(Fortran2003.Base)

        # Reset subclasses dictionary in case this function has been
        # called before. If this is not done then multiple calls to
        # the ParserFactory create method may not work correctly.
        Fortran2003.Base.subclasses = {}
        base_classes = {}

        for _, cls in input_classes:
            # ?? classtype is set to Base so why have issubclass?
            if (
                isinstance(cls, class_type)
                and issubclass(cls, Fortran2003.Base)
                and not cls.__name__.endswith("Base")
            ):
                base_classes[cls.__name__] = cls

        # OPTIMIZE subclass_names tree.
        #
        def _closest_descendants_with_match(clsname):
            """
            Starting at the named class, searches down the tree defined by the
            classes named in the `subclass_names` list to find the closest that
            have `match` methods. If the current class does not have a
            `match` method then this method is called again for each of
            the classes in its `subclass_names` list.

            :param str clsname: The name of the class from which to search.

            :returns: names of 'nearest' subclasses with `match` methods.
            :rtype: List[str | NoneType]

            """
            if clsname not in base_classes:
                error_string = f"Not implemented: {clsname}"
                logging.getLogger(__name__).debug(error_string)
                return []
            # remove this code when all classes are implemented.
            cls = base_classes[clsname]
            if hasattr(cls, "match"):
                # This class has a `match` method so no need to search further
                # down the tree.
                return [clsname]
            # clsname doesn't have a `match` method so we look at each of its
            # subclasses and find the nearest class in each that does have a
            # `match` method.
            bits = []
            for names in getattr(cls, "subclass_names", []):
                list1 = _closest_descendants_with_match(names)
                for names1 in list1:
                    if names1 not in bits:
                        bits.append(names1)
            return bits

        # Dict in which to store optimised list of subclass names for each cls.
        local_subclass_names = {}

        for cls in base_classes.values():
            if not hasattr(cls, "subclass_names"):
                continue
            # The optimised list of subclass names will only include subclasses
            # that have `match` methods.
            opt_subclass_names = []
            for names in cls.subclass_names:
                for names1 in _closest_descendants_with_match(names):
                    if names1 not in opt_subclass_names:
                        opt_subclass_names.append(names1)
            local_subclass_names[cls] = opt_subclass_names[:]

        # Now that we've optimised the list of subclass names for each class,
        # use this information to initialise the Base.subclasses dictionary:
        for clsname, cls in base_classes.items():
            if not hasattr(cls, "subclass_names"):
                message = f"{clsname} class is missing subclass_names list"
                logging.getLogger(__name__).debug(message)
                continue
            subclass_names = local_subclass_names.get(cls, [])
            try:
                bits = Fortran2003.Base.subclasses[clsname]
            except KeyError:
                Fortran2003.Base.subclasses[clsname] = bits = []
            for name in subclass_names:
                if name in base_classes:
                    bits.append(base_classes[name])
                else:
                    message = f"{name} not implemented needed by {clsname}"
                    logging.getLogger(__name__).debug(message)

        # Double-check that all required classes have been constructed.
        for cls in base_classes.values():
            subclass_names = local_subclass_names.get(cls, [])
            use_names = getattr(cls, "use_names", [])
            for name in use_names + subclass_names:
                if name not in base_classes:
                    message = f"{name} not defined, used by {cls.__name__}"
                    logging.getLogger(__name__).debug(message)
