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


def trans(invoke_list, kernel_list, script_file):
    '''
    PSyclone interface to CLAW

    Applies the (Jython) script_file to the specified kernels in the
    specified invokes using CLAW. Transformed kernels are renamed
    and written to the current working directory. All kernel dependencies
    (i.e. other Fortran modules) must have been passed through the
    Front-end of the OMNI compiler to generate .xmod files. The location(s)
    of these .xmod files must be provided in the claw_config.py configuration
    file.

    :param invoke_list: List of invoke objects
    :type invoke_list: List of `py:class:Invoke`
    :param kernel_list: List of names of kernels to transform
    :type kernel_list: List of str
    :param str script_file: Claw Jython script to perform transformation
    :return: Dictionary of re-named kernels, indexed by orig names
    :rtype: dict
    '''
    import tempfile
    from .psyGen import Kern
    from . import claw_config

    # Create dictionary containing mapping from original to new kernel
    # names
    new_kern_names = {}
    # Create a dictionary to hold which kernels are used by which invoke
    unique_kern_list = set(kernel_list)
    invokes_by_kernel = {name: [] for name in unique_kern_list}
    # Create a dictionary so that we can look-up the kernel object(s)
    # from its name. Each entry is a list in case the same kernel is
    # used more than once.
    # TODO if the same kernel *is* used more than once, is each kernel
    # object in the AST unique?
    kernel_obj_list = {name: [] for name in unique_kern_list}

    for invoke in invoke_list:
        kernels = invoke.schedule.walk(invoke.schedule.children, Kern)
        for kern in kernels:
            invokes_by_kernel[kern.name].append(invoke)
            kernel_obj_list[kern.name].append(kern)
    # Sanity check that we have at least one invoke using each kernel
    for kern, inv_list in invokes_by_kernel.items():
        if len(inv_list) < 1:
            raise TransformationError("Huh")

    # Omni cares about line lengths so get a line-length limiter
    from .line_length import FortLineLength
    fll = FortLineLength()

    # We have the fparser1 AST for each kernel but CLAW works with the
    # XcodeML/F AST. We must therefore generate a Fortran file for each
    # kernel and then use the OMNI frontend to get the XcodeML/F
    # representation.
    for name, kern_list in kernel_obj_list.items():

        # Use the fparser AST to generate a (temporary) Fortran file
        fortran = kern_list[0]._module_code.tofortran()
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
        # a kernel may depend upon.
        api = _api_from_ast(kern_list[0])
        if api not in claw_config.OMNI_MODULES_PATH:
            raise TransformationError(
                "No location specified for Omni-compiled infrastructure "
                "for API {0}. Please add to claw_config.py".format(api))
        mod_search_path = claw_config.OMNI_MODULES_PATH[api]

        # Run OMNI to get temporary XML file
        omni_frontend(fort_file.name, xml_name, [mod_search_path])

        # Generate name for transformed kernel and accompanying module
        # TODO do this properly!
        new_kernel_name = name + "_claw"
        new_kern_names[name] = new_kernel_name
        new_mod_name = new_kernel_name + "_mod"
        new_file_name = new_mod_name + ".f90"

        # Update the invokes to use this name for the kernel - this means
        # modifying any USE statements as well as the calls themselves
        for kern in kern_list:
            kern._module_name = new_mod_name
            kern._name = new_kernel_name

        # Alter the XcodeML/F so that it uses the new kernel name
        _rename_kernel(xml_name, name, new_kernel_name)

        # Run the CLAW script on the XML file and generate a new kernel
        # file
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
    import os
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


def _rename_kernel(xml_file, old_name, new_name):
    '''
    Process the supplied XcodeML and re-name the specified kernel
    '''
    with open(xml_file, "r") as xfile:
        # TODO parse the xml file and re-name the necessary elements
        pass
    return


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
