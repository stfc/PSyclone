''' Top-level driver script allowing the Claw compiler to be executed from
either the command-line or from other Python code '''

from claw_config import *


def claw_driver(argv):
    ''' Top level python driver for Claw compiler '''
    from os import path

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


def transform_kernel(fort_file, script_file):
    '''
    :param str fort_file: The Fortran file to transform
    :param str script_file: The Jython script specifying the transformation(s)
    '''
    from subprocess import call
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
    output_fortran_file = path.join(dir_path, output_fortran_file)

    call(["F_Front", fort_file, "-o", str(xml_file)])
    print "Produced XCodeML file: {0}".format(xml_file)

    # Then transform this XcodeML representation using CLAW and use OMNI to
    # de-compile it back to Fortran
    call(["/usr/bin/java", "-Xmx200m", "-Xms200m", "-cp", CLASS_PATH,
          "claw.ClawX2T", "--config-path={0}".format(CLAW_CONFIG_FILE_DIR),
          "--schema={0}".format(os.path.join(CLAW_CONFIG_FILE_DIR,
                                             "claw_config.xsd")),
          "-w", str(NUM_OUTPUT_COLUMNS), "-l",
          "-M/home/kbc59144/Projects/code_fragments",
          "-M/home/kbc59144/MyInstalls/fincludes",
          "-o", "claw_5f_example_f90_out.xml",
          "-f", output_fortran_file,
          "-script", script_file,
          xml_file])


if __name__ == "__main__":
    ''' Entry point for this driver when run from command line '''
    import sys
    claw_driver(sys.argv[1:])
