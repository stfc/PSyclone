'''
    Module containing configuration parameters detailing the locations
    where CLAW and Jython are installed
'''

import os

# Location of the java binary (or just its name if it is on your PATH)
JAVA_BINARY = "java"

# Location of the Jython Jar
JYTHON_JAR = "/home/kbc59144/MyInstalls/jython2.7.0/jython.jar"

# Root directory of the CLAW installation
CLAW_INSTALL_ROOT = "/home/kbc59144/MyInstalls"

# Dictionary (indexed by PSyclone API name) containing the location of
# OMNI-compiled Fortran modules use'd by any kernels to be transformed
# by CLAW.
# TODO move this location out of the tests directory if/when we decide
# that this approach is correct
OMNI_MODULES_PATH = {"dynamo0.3":
                     os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                  "tests", "test_files", "dynamo0p3",
                                  "infrastructure"),
                     "gocean1.0":
                     "/home/kbc59144/Projects/dl_esm_inf/finite_difference/"
                     "src"}

# ################################################################
# Users should not have to edit anything below this line
# ################################################################

# Location of the CLAW configuration file (read by CLAW)
CLAW_CONFIG_FILE_DIR = os.path.join(CLAW_INSTALL_ROOT, "etc")
# Width of output Fortran
NUM_OUTPUT_COLUMNS = 80
# Location that CLAW is installed to
CLAW_INSTALL_PATH = os.path.join(CLAW_INSTALL_ROOT, "share")
# Location that CLAW Python module is installed to
CLAW_PYTHON_PATH = os.path.join(CLAW_INSTALL_ROOT, "lib")

# The OMNI and CLAW jars
OMNI_JARS = ["om-exc-tools.jar",
             "om-f-back.jar",
             "om-c-back.jar"]
CLAW_JARS = ["claw-x2t-shenron.jar",
             "claw-x2t-tatsu.jar",
             "claw-x2t-wani.jar",
             "commons-cli.jar",
             "antlr4.jar",
             "antlr4-runtime.jar"]

# Build the CLASSPATH required to run CLAW
OMNI_JAR_FILES = [os.path.join(CLAW_INSTALL_PATH, "xcalablemp", jar) for
                  jar in OMNI_JARS]
CLAW_JAR_FILES = [os.path.join(CLAW_INSTALL_PATH, "claw", jar) for
                  jar in CLAW_JARS]
JAR_FILES = OMNI_JAR_FILES + CLAW_JAR_FILES + [JYTHON_JAR]
CLASS_PATH = ":".join(JAR_FILES)
