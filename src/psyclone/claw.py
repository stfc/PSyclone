''' Top-level driver script allowing the Claw compiler to be executed from
either the command-line or from other Python code '''

import os
from .claw_config import *
from .transformations import TransformationError

def _claw_driver(argv):
    ''' Top level python driver for Claw compiler '''

    # Set-up command-line argument parser
    import argparse
    parser = argparse.ArgumentParser(description='Run Claw on a Fortran file')
    parser.add_argument("filename", 
                        help="Full path and name of target Fortran file")
    parser.add_argument('-s', '--script', help='filename of a Claw'
                        ' transformation script')
    args = parser.parse_args(argv)

    fortran_file = args.filename
    script_file = args.script
    print "Processing file {0} using recipe {1}".format(fortran_file,
                                                        script_file)
    transform_kernel(fortran_file, script_file)


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
    of these .xmod files must be provided in the xmod_search_path.

    :param invoke_list: List of invoke objects
    :type invoke_list: List of `py:class:Invoke`
    :param kernel_list: List of names of kernels to transform
    :type kernel_list: List of str
    :param str script_file: Claw Jython script to perform transformation
    '''
    import tempfile
    from psyclone.psyGen import Kern
    from psyclone import claw_config

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

    # We have the fparser1 AST for each kernel but CLAW works with the
    # XcodeML/F AST. We must therefore generate a Fortran file for each
    # kernel and then use the OMNI frontend to get the XcodeML/F
    # representation.
    for name, kern_list in kernel_obj_list.items():

        # Use the fparser AST to generate a (temporary) Fortran file
        fortran = kern_list[0]._module_code.tofortran()
        # delete=False is required to ensure the file is not deleted
        # when it is closed
        fort_file = tempfile.NamedTemporaryFile(delete=False, suffix=".f90")
        fort_file.write(fortran)
        fort_file.close()
        # Create a name for the output xml file
        xml_name = fort_file.name[:]
        xml_name += ".xml"

        # Work out which API this is from the fparser AST
        api = _api_from_ast(kern_list[0])
        if api not in claw_config.OMNI_MODULES_PATH:
            raise TransformationError(
                "No location specified for OMNI-compiled infrastructure "
                "for API {0}. Please add to claw_config.py".format(api))
        mod_search_path = claw_config.OMNI_MODULES_PATH[api]

        # Run OMNI to get temporary XML file
        omni_frontend(fort_file.name, xml_name, [mod_search_path])

        # Generate name for transformed kernel and accompanying module
        # TODO do this properly!
        new_kernel_name = name + "_claw"
        new_mod_name = new_kernel_name + "_mod"
        new_file_name = "new_mod_name" + ".f90"

        # Update the invokes to use this name for the kernel - this means
        # modifying any USE statements as well as the calls themselves
        for kern in kern_list:
            kern._module_name = new_mod_name
            kern._name = new_kernel_name

        # Alter the XcodeML/F so that it uses the new kernel name
        rename_kernel(xml_name, new_kernel_name)

        # Run the CLAW script on the XML file and generate a new kernel
        # file
        _run_claw([mod_search_path], xml_name,
                  new_file_name, script_file)


def transform_kernel(fort_file, script_file):
    '''
    :param str fort_file: The Fortran file to transform
    :param str script_file: The Jython script specifying the transformation(s)
    '''
    from os import path
    # Use the OMNI frontend to generate the XcodeML representation of
    # the Fortran file
    (dir_path, input_fortran_file) = path.split(fort_file)
    if input_fortran_file.endswith(".F90"):
        xml_file = input_fortran_file.replace(".F90", ".xml", 1)
        output_fortran_file = input_fortran_file.replace(".F90", ".new.F90", 1)
    elif input_fortran_file.endswith(".f90"):
        xml_file = input_fortran_file.replace(".f90", ".xml", 1)
        output_fortran_file = input_fortran_file.replace(".f90", ".new.f90", 1)
    else:
        raise Exception("Fortran file must have .f90 or .F90 suffix but "
                        "got {0}".format(input_fortran_file))

    xml_file = path.join(dir_path, xml_file)
    # TODO add cmd-line option for specifying module search path
    omni_frontend(fort_file, xml_file, [])

    output_fortran_file = path.join(dir_path, output_fortran_file)

    _run_claw(["xmod search path here"], xml_file,
              output_fortran_file, script_file)


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
    from subprocess import call

    xmod_paths = ["-M{0}".format(path) for path in xmod_search_path]

    call(["/usr/bin/java", "-Xmx200m", "-Xms200m",
          "-cp", CLASS_PATH,
          "claw.ClawX2T",
          "--config-path={0}".format(CLAW_CONFIG_FILE_DIR),
          "--schema={0}".format(os.path.join(CLAW_CONFIG_FILE_DIR,
                                             "claw_config.xsd")),
          "-w", str(NUM_OUTPUT_COLUMNS), "-l",
          " ".join(xmod_paths),
          #"-M/home/kbc59144/Projects/code_fragments",
          #"-M/home/kbc59144/MyInstalls/fincludes",
          #"-o", "claw_5f_example_f90_out.xml",
          "-f", output_file,
          "-script", script_file,
          xml_file])


def rename_kernel(xml_file, name):
    '''
    Process the supplied XcodeML and re-name the specified kernel
    '''
    pass


def _api_from_ast(kern):
    '''
    Work out which PSyclone API the supplied kernel is for
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


if __name__ == "__main__":
    ''' Entry point for this driver when run from command line '''
    import sys
    _claw_driver(sys.argv[1:])
