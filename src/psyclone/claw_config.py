'''
    Module containing configuration parameters detailing the locations
    where CLAW and Jython are installed
'''

import os

# Location of the Jython Jar
JYTHON_JAR = "/home/kbc59144/MyInstalls/jython2.7.0/jython.jar"
# Root directory of the CLAW installation
CLAW_INSTALL_ROOT = "/home/kbc59144/MyInstalls"

# Location of the CLAW configuration file (read by CLAW)
CLAW_CONFIG_FILE_DIR = os.path.join(CLAW_INSTALL_ROOT, "etc")
# Width of output Fortran
NUM_OUTPUT_COLUMNS = 80
# Location that CLAW is installed to
CLAW_INSTALL_PATH = os.path.join(CLAW_INSTALL_ROOT, "share")

# The OMNI and CLAW jars
OMNI_JARS = ["om-exc-tools.jar",
             "om-common.jar",
             "om-f-back.jar",
             "om-c-back.jar"]
CLAW_JARS = ["om-cx2x-claw.jar",
             "commons-cli.jar",
             "om-cx2x-xcodeml.jar",
             "antlr4.jar",
             "antlr4-runtime.jar"]

# Build the CLASSPATH required to run CLAW
OMNI_JAR_FILES = [os.path.join(CLAW_INSTALL_PATH, "xcalablemp", jar) for
                  jar in OMNI_JARS]
CLAW_JAR_FILES = [os.path.join(CLAW_INSTALL_PATH, "claw", jar) for
                  jar in CLAW_JARS]
JAR_FILES = OMNI_JAR_FILES + CLAW_JAR_FILES + [JYTHON_JAR]
CLASS_PATH = ":".join(JAR_FILES)
