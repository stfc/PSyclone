# -----------------------------------------------------------------------------
# BSD 3-Clause License
#
# Copyright (c) 2017-2018, Science and Technology Facilities Council
#
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
# Author: A. R. Porter, STFC Daresbury Lab

'''
Module implementing the interface between PSyclone and the Claw
compiler.

The primary method provided by this module is `trans()` which is
designed to be used from within a PSyclone transformation script.
'''

import os
from .claw_config import *
from .transformations import TransformationError


def omni_frontend(fort_file, xml_file, mod_search_paths):
    '''
    Runs the front-end of the OMNI compiler (which must be on the user's PATH)

    :param str fort_file: the name of the Fortran file to process
    :param str xml_file: the XcodeML/F file to create
    :param mod_search_paths: list of locations to search for xmod files
    :type mod_search_paths: list of str
    '''
    from subprocess import check_call, CalledProcessError

    inc_args = ["-I{0}".format(path) for path in mod_search_paths]
    mod_path = " ".join(inc_args)
    try:
        check_call(["F_Front", mod_path, fort_file, "-o", str(xml_file)])
    except CalledProcessError as err:
        print "The Omni frontend raised an error ({0}) when processing " \
            "file {1}".format(str(err), fort_file)
        raise err
    print "omni_frontend: produced XCodeML file: {0}".format(xml_file)


def trans(kernel_list, script_file, naming_mode=None):
    '''
    PSyclone interface to CLAW

    Applies the (Jython) script_file to the specified kernels in the
    specified invokes using CLAW. Transformed kernels are renamed
    and written to the current working directory. All kernel dependencies
    (i.e. other Fortran modules) must have been passed through the
    Front-end of the OMNI compiler to generate .xmod files. The location(s)
    of these .xmod files must be provided in the Claw configuration
    file (see the PSyclone documentation for details).

    :param kernel_list: List of kernel objects to transform
    :type kernel_list: List of objects of type :py:class:`psyclone.psyGen.Kern`
    :param str script_file: Claw Jython script to perform transformation
    :param str naming_mode: How to handle any name clashes for transformed
                            kernels. One of ["overwrite", "keep", "abort"].
                            Defaults to "keep" if not specified.
    :return: Dictionary of re-named kernels, indexed by orig names
    :rtype: dict
    '''
    import tempfile
    from . import claw_config

    if naming_mode is None:
        # By default we ensure that the name of any newly-transformed
        # kernel is unique (since the same kernel may have been
        # transformed differently for some other invoke in a different
        # Algorithm file).
        _naming_mode = "keep"
    else:
        _naming_mode = naming_mode

    # Create dictionary containing mapping from original to new kernel
    # names
    new_kern_names = {}

    # Omni cares about line lengths so get a line-length limiter
    from .line_length import FortLineLength
    fll = FortLineLength()

    # We have the fparser1 AST for each kernel but CLAW works with the
    # XcodeML/F AST. We must therefore generate a Fortran file for each
    # kernel and then use the OMNI frontend to get the XcodeML/F
    # representation.
    for kern in kernel_list:

        # Use the fparser AST to generate a (temporary) Fortran file
        fortran = kern._module_code.tofortran()
        fortran_limited = fll.process(fortran)
        # delete=False is required to ensure the file is not deleted
        # when it is closed
        fort_file = tempfile.NamedTemporaryFile(delete=False, suffix=".f90")
        fort_file.write(fortran_limited)
        fort_file.close()
        # Create a name for the output xml file
        xml_name = fort_file.name[:]
        xml_name += ".xml"

        # Work out which API this is from the fparser AST and use this to
        # look-up the path to the Omni-compiled infrastructure modules that
        # a kernel may depend upon. We must do it this way since this
        # routine is intended to be used from a transformation script rather
        # than from directly within PSyclone.
        api = _api_from_ast(kern)
        if api not in claw_config.OMNI_MODULES_PATH:
            raise TransformationError(
                "No location specified for Omni-compiled infrastructure "
                "for API {0}. Please add to claw_config.py".format(api))
        mod_search_path = claw_config.OMNI_MODULES_PATH[api]

        # Run OMNI to get temporary XML file
        omni_frontend(fort_file.name, xml_name, [mod_search_path])

        # Generate a new name for the transformed kernel and alter the
        # XcodeML/F so that it uses the new name
        try:
            mod_name, type_name, kern_name = _rename_kernel(xml_name,
                                                            kern._name,
                                                            _naming_mode)
        except IOError:
            raise TransformationError(
                "Failed to find file {0} containing the XcodeML/F "
                "representation of kernel {1}. Is the Omni frontend on your "
                "PATH and working?".format(xml_name, kern.name))

        new_file_name = mod_name + ".f90"
        new_kern_names[kern.name] = kern_name

        # Update the kernel object with its new name (and corresponding
        # module name). These properties are then picked-up at code-gen
        # time for the associated USE and CALL statements.
        kern._module_name = mod_name
        kern._name = kern_name

        # Run the CLAW script on the XML file and generate a new kernel
        # file. This call is SLOW because it requires the startup of a
        # Java VM.
        _run_claw([mod_search_path], xml_name,
                  new_file_name, script_file)

    return new_kern_names


def _run_claw(xmod_search_path, xml_file, output_file, script_file):
    '''
    Transform the XcodeML representation in xml_file using CLAW and
    then use OMNI to de-compile it back to Fortran in output_file

    :param xmod_search_path: list of dirs in which to find xmod files
    :type xmod_search_path: list of str
    :param str xml_file: the file containing XcodeML/F to transform
    :param str output_file: the Fortran file to create
    :param str script_file: the Jython CLAW script specifying the
                            transformations
    '''
    from subprocess import check_call, CalledProcessError
    from . import claw_config

    xmod_paths = ["-M{0}".format(path) for path in xmod_search_path]

    # We have to provide a name for the output file that will contain
    # the transformed XML in order that it can be given to the Omni
    # backend
    intermediate_xml_file = "{0}.tmp.xml".format(xml_file)

    # Ensure the Claw Python module is on the JYTHONPATH
    my_env = os.environ.copy()
    my_env["JYTHONPATH"] = claw_config.CLAW_PYTHON_PATH

    try:
        check_call(["/usr/bin/java", "-Xmx200m", "-Xms200m",
                    "-cp", CLASS_PATH,
                    "claw.ClawX2T",
                    "--config-path={0}".format(CLAW_CONFIG_FILE_DIR),
                    "--schema={0}".format(os.path.join(CLAW_CONFIG_FILE_DIR,
                                                       "claw_config.xsd")),
                    "-w", str(NUM_OUTPUT_COLUMNS), "-l",
                    " ".join(xmod_paths),
                    "-f", output_file,
                    "-o", intermediate_xml_file,
                    "-script", script_file,
                    xml_file],
                   env=my_env)
    except CalledProcessError as err:
        print "Execution of CLAW failed:"
        print str(err)
        raise err


def _rename_kernel(xml_file, kernel_name, mode):
    '''
    Process the supplied XcodeML and re-name the specified kernel

    :param str xml_file: Full path to the file containing the XcodeML/F
                         representation of the kernel
    :param str kernel_name: Name of the kernel subroutine to be renamed
    :param str mode: How to handle name clashes
    :return: Tupe of names of transformed module, kernel-type and kernel
    :rtype: 3-tuple of str

    :raises IOError: if supplied file is not found
    :raises TransformationError: if renaming the kernel would cause a clash
                                 with a previously re-named kernel (in the
                                 CWD) and mode=="abort"
    '''

    # We read and write files in the current working directory
    pwd = os.getcwd()

    # Read the XCodeML into a buffer and parse it to get a DOM
    xml_string = ""
    with open(xml_file, "r") as xfile:
        xml_string = xfile.read()
    from xml.dom import minidom
    xmldoc = minidom.parseString(xml_string)

    # Query the XML doc to determine the name of the module that contains
    # our kernel
    orig_mod_name = _get_kernel_module(xmldoc, kernel_name)

    # Get the name of the original Fortran source file
    progNode = xmldoc.firstChild
    orig_file = progNode.getAttribute("source")
    if not orig_file:
        orig_file = orig_mod_name + ".f90"

    # Remove the .[fF]90 suffix and also any "_mod" if the file follows
    # the PSyclone naming convention
    if orig_file.endswith("_mod.f90") or orig_file.endswith("_mod.F90"):
        # File follows PSyclone naming convention
        old_base_name = orig_file[:-8]
    elif orig_file.endswith(".f90") or orig_file.endswith(".F90"):
        old_base_name = orig_file[:-4]
    else:
        # The filename doesn't end in .[fF]90. This shouldn't happen!
        raise TransformationError(
            "Internal error: filename '{0}' for module does not end in "
            ".[Ff]90.".format(orig_file))

    # Determine the new suffix to use by looking at what files are already
    # in our working directory
    new_suffix = "_claw0"
    current_files = os.listdir(pwd)
    # Convert all suffixes to .f90 to simplify things below (we don't
    # want to end up with two files with the same name and only differing
    # in whether they are .f90 or .F90)
    current_files_lower = []
    for afile in current_files:
        if afile.endswith(".f90"):
            current_files_lower.append(afile)
        elif afile.endswith(".F90"):
            current_files_lower.append(afile[-3]+"f90")

    if mode == "keep":
        name_idx = -1
        while True:
            name_idx += 1
            new_suffix = "_claw{0}".format(name_idx)
            new_name = old_base_name + new_suffix + "_mod.f90"
            print "new_name = ", new_name
            if new_name not in current_files_lower:
                # There isn't a src file with this name so we're done
                break
    elif mode == "overwrite":
        # We don't care whether there's already a src file with the new name
        pass
    elif mode == "abort":
        filename = old_base_name+new_suffix+"_mod.f90"
        if filename in current_files_lower:
            raise TransformationError(
                "Kernel file {0} already exists and renaming mode is '{1}' "
                "so refusing to overwrite".format(filename, mode))


    # Use the suffix we have determined to create a new module name
    if orig_mod_name.endswith("_mod"):
        new_mod_name = orig_mod_name[:-4] + new_suffix
    else:
        new_mod_name = orig_mod_name + new_suffix
    # Our new module name will conform to the PSyclone convention of
    # ending in "_mod"
    new_mod_name += "_mod"

    # Use the suffix we have determined to create a new kernel name
    if kernel_name.endswith("_code"):
        idx = kernel_name.find("_code")
        new_kern_name = kernel_name[:-5] + new_suffix
    else:
        new_kern_name = kernel_name + new_suffix
    # The new name for the kernel subroutine will conform to the
    # PSyclone convention of ending in "_code"
    new_kern_name += "_code"

    # Query the XML to determine the name of the type that contains
    # the kernel subroutine as a type-bound procedure
    orig_type_name = _get_type_by_binding_name(xmldoc, kernel_name)
    if orig_type_name.endswith("_type"):
        new_type_name = orig_type_name[:-5] + new_suffix
    else:
        new_type_name = orig_type_name + new_suffix
    # The new name for the type containing kernel metadata will
    # conform to the PSyclone convention of ending in "_type"
    new_type_name += "_type"

    # Construct a dictionary for mapping from old kernel/type/module
    # names to the corresponding new ones
    rename_map = {orig_mod_name: new_mod_name,
                  kernel_name: new_kern_name,
                  orig_type_name: new_type_name}

    # Re-write the necessary text nodes and attributes
    names = xmldoc.getElementsByTagName("name")
    for name in names:
        try:
            new_value = rename_map[name.firstChild.data]
            for child in name.childNodes[:]:
                name.removeChild(child)
            new_txt = xmldoc.createTextNode(new_value)
            name.appendChild(new_txt)
        except KeyError:
            # This is not one of the names we are looking for
            pass
    mod_defs = xmldoc.getElementsByTagName("FmoduleDefinition")
    for mod in mod_defs:
        if mod.getAttribute("name") in rename_map:
            mod.setAttribute("name", rename_map[mod.getAttribute("name")])

    # Write the modified XML back to the same file
    with open(xml_file, "w") as xfile:
        xfile.write(xmldoc.toxml())

    return (new_mod_name, new_type_name, new_kern_name)


def _api_from_ast(kern):
    '''
    Work out which PSyclone API the supplied kernel is for

    :param kern: Kernel object to examine
    :type kern: :py:class:`psyclone.psyGen.Kern`
    :return: Name of the corresponding API
    :rtype: str
    '''
    from psyclone.dynamo0p3 import DynKern
    from psyclone.gocean1p0 import GOKern
    if isinstance(kern, DynKern):
        return "dynamo0.3"
    elif isinstance(kern, GOKern):
        return "gocean1.0"
    else:
        raise TransformationError(
            "Cannot determine API for kernel of type {0}".format(type(kern)))


def _get_type_by_binding_name(xmldoc, kern_name):
    '''
    Query the XML doc to find the name of the type with which the
    named kernel is associated.

    :param xmldoc: minidom XML document object holding XCodeML/F
    :param str kern_name: name of the kernel subroutine
    :return: Name of the type which has the specified kernel as a
             type-bound procedure
    :rtype: str
    :raises: TODO
    '''
    type_defs = xmldoc.getElementsByTagName("FstructType")
    if not type_defs:
        # TODO use correct type of exception
        raise Exception("XCodeML/F does not contain any type definitions")

    tindex = ""
    for tdef in type_defs:
        tbound_procs = tdef.getElementsByTagName("typeBoundProcedure")
        if tbound_procs is None:
            continue
        for proc in tbound_procs:
            bindings = proc.getElementsByTagName("binding")
            names = bindings[0].getElementsByTagName("name")
            name = names[0].firstChild.data
            if name == kern_name:
                # This is the matching Type definition so store its
                # type index
                tindex = tdef.getAttribute("type")
                break
        if tindex:
            break

    if not tindex:
        raise Exception("Failed to find a Type definition containing a "
                        "type-bound procedure with name {0}".format(kern_name))

    # Now we have the type index, we can find its name
    gdeclns = xmldoc.getElementsByTagName("globalDeclarations")
    symbol_lists = gdeclns[0].getElementsByTagName("symbols")
    for symbol in symbol_lists:
        id_list = symbol.getElementsByTagName("id")
        for id_node in id_list:
            if id_node.getAttribute("type") == tindex:
                # This is the matching symbol
                names = id_node.getElementsByTagName("name")
                return names[0].firstChild.data
    raise Exception("Could not find symbol ID for the derived type "
                    "with type={0}".format(tindex))


def _get_kernel_module(xmldoc, kern_name):
    '''
    Query the XML doc to find the name of the module which contains
    the named kernel.

    :param xmldoc: minidom XML document object holding XCodeML/F
    :param str kern_name: name of the kernel subroutine
    :return: Name of the module which contains the specified kernel
    :rtype: str
    :raises: TODO
    '''
    modules = xmldoc.getElementsByTagName("FmoduleDefinition")
    for module in modules:
        contains = module.getElementsByTagName("FcontainsStatement")
        funcs = contains[0].getElementsByTagName("FfunctionDefinition")
        for func in funcs:
            for child in func.childNodes:
                if child.nodeType == child.ELEMENT_NODE and \
                   child.tagName == "name":
                    func_name = child.firstChild.data
                    if func_name == kern_name:
                        # We've found our kernel so we need the name
                        # of this module
                        return module.getAttribute("name")
                    else:
                        # This isn't our kernel
                        break
    # We didn't find the named kernel
    return ""
