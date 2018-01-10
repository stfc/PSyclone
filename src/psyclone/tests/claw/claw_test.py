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
# Author: A. R. Porter, STFC Daresbury Lab

''' Tests for the CLAW interface implemented in PSyclone '''

import os
import pytest
import utils
from psyclone.transformations import TransformationError


# constants
BASE_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                         "..", "test_files")

def _fake_check_call(args, env=None):  # pylint:disable=unused-argument
    '''
    Function to be used to monkeypatch the check_call() function of
    the subprocess module.
    :param list args: List of items from which to construct system call
    :raises: subprocess.CalledProcessError
    '''
    from subprocess import CalledProcessError
    raise CalledProcessError(1, " ".join(args))


def test_validate_omni(monkeypatch, tmpdir):
    ''' Tests for the _validate_omni_setup routine '''
    from psyclone import claw_config
    from psyclone import claw
    # Create a fake 'F_Front' file in our tmpdir
    _ = tmpdir.chdir()
    with open("F_Front", "w") as ffile:
        ffile.write("Fake Omni frontend")
    # Monkeypatch PATH so that our fake F_Front is on it
    monkeypatch.setenv('PATH', str(tmpdir))
    # Now monkeypatch the OMNI_MODULES_PATH dict so that it contains
    # a non-existant path
    apis = claw_config.OMNI_MODULES_PATH.keys()
    monkeypatch.setitem(claw_config.OMNI_MODULES_PATH, apis[0],
                        "/not/a/path")
    with pytest.raises(TransformationError) as err:
        claw._validate_omni_setup()
    assert ("location (/not/a/path) for Omni-compiled modules for the "
            "{0} API does not exist".format(apis[0]) in str(err))

    # Monkeypatch PATH so that we can't find F_Front
    monkeypatch.setenv('PATH', "")
    with pytest.raises(TransformationError) as err:
        claw._validate_omni_setup()
    assert ("frontend of the Omni compiler (F_Front) cannot be found. Please "
            "ensure that it is on your PATH ()" in str(err))
    
    monkeypatch.setenv('PATH', "/not/a/path")
    with pytest.raises(TransformationError) as err:
        claw._validate_omni_setup()
    assert ("frontend of the Omni compiler (F_Front) cannot be found. Please "
            "ensure that it is on your PATH (/not/a/path)" in str(err))


def test_validate_claw(monkeypatch, tmpdir):
    ''' Tests for the _validate_claw_setup routine '''
    from psyclone import claw_config
    from psyclone import claw

    # Specify an unqualified name for the java binary that cannot be
    # found on our PATH
    monkeypatch.setattr(claw_config, "JAVA_BINARY",
                        "this_binary_does_not_exist")
    with pytest.raises(TransformationError) as err:
        claw._validate_claw_setup()
    assert ("java binary (this_binary_does_not_exist) specified in the "
            "PSyclone configuration file cannot be found on your PATH."
            in str(err))
    
    not_a_file = str(os.path.join("not", "a", "file"))

    # Point the JAVA_BINARY at a specific file that doesn't exist
    monkeypatch.setattr(claw_config, "JAVA_BINARY", not_a_file)
    with pytest.raises(TransformationError) as err:
        claw._validate_claw_setup()
    assert ("but the specified java binary ({0}) does not exist".
            format(not_a_file) in str(err))

    # Now create a file and point JAVA_BINARY at that
    fake_java = str(os.path.join(str(tmpdir), "my_java"))
    with open(fake_java, "w") as ffile:
        ffile.write("Fake java binary")
    monkeypatch.setattr(claw_config, "JAVA_BINARY", fake_java)

    # Monkeypatch the CLAW install location to be something that does not exist
    monkeypatch.setattr(claw_config, "CLAW_INSTALL_PATH", not_a_file)
    with pytest.raises(TransformationError) as err:
        claw._validate_claw_setup()
    assert ("location of the CLAW installation ({0}) specified in "
            "the PSyclone configuration file does not exist".
            format(not_a_file) in str(err))

    # Repeat but with the fake java binary now to be found on our PATH
    monkeypatch.setattr(claw_config, "JAVA_BINARY", "my_java")
    monkeypatch.setenv("PATH", str(tmpdir))
    with pytest.raises(TransformationError) as err:
        claw._validate_claw_setup()
    assert ("location of the CLAW installation ({0}) specified in "
            "the PSyclone configuration file does not exist".
            format(not_a_file) in str(err))

    # Monkeypatch the CLAW install location to be our tmpdir
    monkeypatch.setattr(claw_config, "CLAW_INSTALL_PATH", str(tmpdir))
    # Break the class-path by changing it to a non-existant file
    monkeypatch.setattr(claw_config, "CLASS_PATH", not_a_file)
    with pytest.raises(TransformationError) as err:
        claw._validate_claw_setup()
    assert ("File {0} in the CLASS_PATH used when running "
            "CLAW does not exist".format(not_a_file) in str(err))

    # Monkeypatch the class-path to point to an existing file (doesn't
    # have to be an actual jar file)
    monkeypatch.setattr(claw_config, "CLASS_PATH", fake_java)
    # Break the location of the Jython jar
    monkeypatch.setattr(claw_config, "JYTHON_JAR", not_a_file)
    with pytest.raises(TransformationError) as err:
        claw._validate_claw_setup()
    assert ("CLAW uses Jython but the jar file ({0}) specified "
            "in".format(not_a_file) in str(err))

    # Monkeypatch the location of the Jython jar to be the file
    # we created earlier in this test
    monkeypatch.setattr(claw_config, "JYTHON_JAR", fake_java)
    # Break the location of the CLAW-python interface file
    monkeypatch.setattr(claw_config, "CLAW_PYTHON_PATH", not_a_file)
    with pytest.raises(TransformationError) as err:
        claw._validate_claw_setup()
    assert ("interface file (ClawTransform.py) cannot be found" in str(err))


def test_omni_fe_error(monkeypatch):
    ''' Check that we raise the expected exception if the Omni frontend
    fails '''
    from psyclone.claw import omni_frontend
    import subprocess
    monkeypatch.setattr(subprocess, "check_call", _fake_check_call)
    with pytest.raises(subprocess.CalledProcessError) as err:
        omni_frontend("some_file.f90", "some_file.xml", ".")
    assert "F_Front -I. some_file.f90 -o some_file.xml" in str(err)


def test_run_claw_error(monkeypatch):
    ''' Check that we handle errors in the claw._run_claw() routine '''
    from psyclone.claw import _run_claw
    # Monkeypatch the subprocess.check_call() method so that it raises
    # an exception
    import subprocess
    monkeypatch.setattr(subprocess, "check_call", _fake_check_call)
    with pytest.raises(TransformationError) as err:
        _run_claw(["."], "some_file.xml", "some_file.f90", "some_script.py")
    output = str(err)
    print output
    assert "java -Xmx200m -Xms200m -cp" in str(err)
    assert "jython.jar claw.ClawX2T --config-path=" in output
    assert ("-M. -f some_file.f90 -o some_file.xml.tmp.xml -script "
            "some_script.py some_file.xml" in output)


@utils.CLAW
def test_run_claw_broken_classpath(monkeypatch):
    ''' Check that we raise the expected error if the classpath provided
    in the configuration file is not correct '''
    from psyclone.claw import _run_claw
    from psyclone import claw_config
    monkeypatch.setattr(claw_config, "CLASS_PATH", "broken")
    monkeypatch.setattr(claw, "_validate_omni_setup", lambda: None)
    monkeypatch.setattr(claw, "_validate_claw_setup", lambda: None)
    with pytest.raises(TransformationError) as err:
        _run_claw(["."], "some_file.xml", "some_file.f90", "some_script.py")
    assert "Could not find or load main class claw.ClawX2T" in str(err)



def test_api_from_ast():
    ''' Test for the utility routine that gives us the name of the PSyclone
    API to which a kernel object belongs '''
    from psyclone.dynamo0p3 import DynKern
    from psyclone.gocean1p0 import GOKern
    from psyclone.claw import _api_from_ast
    dkern = DynKern()
    api = _api_from_ast(dkern)
    assert api == "dynamo0.3"

    gkern = GOKern()
    api = _api_from_ast(gkern)
    assert api == "gocean1.0"

    no_kern = "not a kernel"
    with pytest.raises(TransformationError) as err:
        _ = _api_from_ast(no_kern)
    assert "Cannot determine API for kernel" in str(err)


def test_trans(tmpdir, monkeypatch):
    ''' Tests for the trans() routine '''
    import subprocess
    from psyclone.parse import parse
    from psyclone.psyGen import PSyFactory
    from psyclone import claw
    _, invoke_info = parse(os.path.join(BASE_PATH, "dynamo0p3",
                                        "1_single_invoke.f90"),
                           api="dynamo0.3")
    psy = PSyFactory("dynamo0.3", distributed_memory=False).create(invoke_info)
    invoke = psy.invokes.invoke_list[0]
    kern = invoke.schedule.children[0].children[0]
    orig_name = kern.name[:]
    script_file = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                               "claw_trans.py")
    # Change to the pytest-supplied tmp dir so that we don't mess up our
    # space with generated files
    _ = tmpdir.chdir()

    if not utils.TEST_CLAW:
        # Monkeypatch subprocess.check_call() so that it does
        # nothing. This means that we don't actually run Omni or Claw.
        monkeypatch.setattr(subprocess, "check_call",
                            lambda args, env=None: None)
        # We must also monkeypatch our validation routines
        monkeypatch.setattr(claw, "_validate_omni_setup", lambda: None)
        monkeypatch.setattr(claw, "_validate_claw_setup", lambda: None)

        with pytest.raises(TransformationError) as err:
            _ = claw.trans([kern], script_file)
        # Check that we've raised the correct error about not finding the
        # XML output of the Omni Frontend
        assert "XcodeML/F representation of kernel {0}".format(orig_name) in \
            str(err)
        # Since we're not running Omni, we don't generate any xml files so also
        # monkeypatch the kernel-renaming routine so that it does nothing.
        monkeypatch.setattr(claw, "_rename_kernel",
                            lambda xml, name, mode: ("testkern_claw0_mod",
                                                     "testkern_claw0_type",
                                                     "testkern_claw0_code"))
    # Keep a copy of our kernel object as transforming it modifies it
    import copy
    orig_kern = copy.deepcopy(kern)
    new_names = claw.trans([kern], script_file)
    assert new_names[orig_name] == "testkern_claw0_code"
    # Repeat test for un-changed copy of kernel but this time explicitly
    # specifying the naming mode to use
    new_names = claw.trans([orig_kern], script_file, naming_mode="keep")
    assert new_names[orig_name] == "testkern_claw0_code"
    # Exercise the catch for missing infrastructure location for an API
    monkeypatch.setattr(claw, "_api_from_ast",
                        lambda kern: "not_an_api")
    with pytest.raises(TransformationError) as err:
        _ = claw.trans([orig_kern], script_file, naming_mode="keep")
    assert ("No location specified for Omni-compiled infrastructure for "
            "API not_an_api" in str(err))


def test_rename_kern_with_mod(tmpdir):
    ''' Check that _rename_kernel() works as it should when the file/
    module names follow the PSyclone convention of having '_mod' '''
    import shutil
    from psyclone.claw import _rename_kernel
    # We use a copy of an XML file we prepared earlier so as not to have
    # to rely on Omni being installed
    orig_xml_file = os.path.join(BASE_PATH, "gocean1p0", "next_sshu_mod.xml")
    oldpwd = tmpdir.chdir()
    shutil.copy(orig_xml_file, str(tmpdir))
    xml_file = os.path.join(str(tmpdir), "next_sshu_mod.xml")
    new_mod, new_type, new_name = _rename_kernel(xml_file,
                                                 "next_sshu_code", "keep")
    assert new_name == "next_sshu_claw0_code"
    assert new_mod == "next_sshu_claw0_mod"
    assert new_type == "next_sshu_claw0_type"

    with open("next_sshu_claw0_mod.f90", "w") as ffile:
        ffile.write("Hello")
    shutil.copy(orig_xml_file, str(tmpdir))
    new_mod, new_type, new_name = _rename_kernel(xml_file,
                                                 "next_sshu_code", "keep")
    assert new_name == "next_sshu_claw1_code"
    assert new_mod == "next_sshu_claw1_mod"
    assert new_type == "next_sshu_claw1_type"

    shutil.copy(orig_xml_file, str(tmpdir))
    with pytest.raises(TransformationError) as err:
        _ = _rename_kernel(xml_file, "next_sshu_code", "abort")
    assert ("next_sshu_claw0_mod.f90 already exists and renaming mode is "
            "'abort'" in str(err))


def test_rename_no_filename(tmpdir):
    ''' Test that _rename_kernel() works OK if the original filename
    is missing from the XcodeML/F '''
    import shutil
    from xml.dom import minidom
    from psyclone.claw import _rename_kernel
    # We use a copy of an XML file we prepared earlier so as not to have
    # to rely on Omni being installed
    orig_xml_file = os.path.join(BASE_PATH, "gocean1p0", "next_sshu_mod.xml")
    oldpwd = tmpdir.chdir()
    orig_xml = ""
    # We read-in the XML file, parse it and then manipulate the DOM to
    # remove the name of the Fortran source file
    with open(orig_xml_file, "r") as xfile:
        orig_xml = xfile.read()
    xdoc = minidom.parseString(orig_xml)
    prog_node = xdoc.firstChild
    prog_node.removeAttribute("source")
    new_file = os.path.join(str(tmpdir), "next_sshu_mod.xml")
    with open(new_file, "w") as xfile:
        xfile.write(xdoc.toxml())
    # Now that we've created the XML file without the name of the original
    # Fortran source file, try re-naming the kernel
    new_mod, new_type, new_name = _rename_kernel(new_file,
                                                 "next_sshu_code", "keep")
    assert new_name == "next_sshu_claw0_code"
    assert new_type == "next_sshu_claw0_type"
    assert new_mod == "next_sshu_claw0_mod"


def test_rename_kern_no_mod(tmpdir):
    ''' Check that _rename_kernel() works as it should when the Fortran/
    module names do not follow the convention of having '_mod' '''
    import shutil
    from psyclone.claw import _rename_kernel
    # We use a copy of an XML file we prepared earlier so as not to have
    # to rely on Omni being installed
    orig_xml_file = os.path.join(BASE_PATH, "dynamo0p3", "testkern.xml")
    oldpwd = tmpdir.chdir()
    shutil.copy(orig_xml_file, str(tmpdir))
    xml_file = os.path.join(str(tmpdir), "testkern.xml")
    _, _, new_name = _rename_kernel(xml_file, "testkern_code", "keep")
    assert new_name == "testkern_claw0_code"

    # Check that we get a different kernel name if "keep" is specified and
    # there would be a clash - create a fake renamed kern file first
    with open("testkern_claw0_mod.f90", "w") as ffile:
        ffile.write("Hello")
    shutil.copy(orig_xml_file, str(tmpdir))
    _, _, new_name = _rename_kernel(xml_file, "testkern_code", "keep")
    assert new_name == "testkern_claw1_code"

    shutil.copy(orig_xml_file, str(tmpdir))
    with pytest.raises(TransformationError) as err:
        _ = _rename_kernel(xml_file, "testkern_code", "abort")
    assert "testkern_claw0_mod.f90 already exists and renaming mode is" \
        in str(err)

    # Overwrite any existing transformed kernel
    shutil.copy(orig_xml_file, str(tmpdir))
    _, _, new_name = _rename_kernel(xml_file, "testkern_code", "overwrite")
    assert new_name == "testkern_claw0_code"

    # Now check the transformed XCodeML
    new_base_name = "testkern_claw0"
    new_kern_name = new_base_name + "_code"
    new_kern_type_name = new_base_name + "_type"
    new_mod_name = new_base_name + "_mod"

    from xml.dom import minidom
    with open(xml_file, "r") as xfile:
        xml_doc = minidom.parse(xfile)
        # Kernel is a type-bound procedure in the meta-data
        procs = xml_doc.getElementsByTagName("typeBoundProcedure")
        proc_name_list = []
        for proc in procs:
            bindings = proc.getElementsByTagName("binding")
            names = bindings[0].getElementsByTagName("name")
            proc_name_list.append(names[0].firstChild.data)
        assert new_kern_name in proc_name_list
        # Global symbols
        gsymbols = xml_doc.getElementsByTagName("globalSymbols")
        gids = gsymbols[0].getElementsByTagName("id")
        for gid in gids:
            if gid.getAttribute("sclass") == "ffunc":
                names = gid.getElementsByTagName("name")
                assert names[0].firstChild.data == new_mod_name
        # Global declarations
        gdeclns = xml_doc.getElementsByTagName("globalDeclarations")
        modefs = gdeclns[0].getElementsByTagName("FmoduleDefinition")
        assert modefs[0].getAttribute("name") == new_mod_name
        symbols = modefs[0].getElementsByTagName("symbols")
        symbol_ids = symbols[0].getElementsByTagName("id")
        found_ftype = False
        for sid in symbol_ids:
            class_attr = sid.getAttribute("sclass")
            if class_attr == "ftype_name":
                # The symbol table may hold more than one ftype (because it
                # includes symbols from use'd modules too) so
                # we can only be sure once we've seen them all
                names = sid.getElementsByTagName("name")
                if names[0].firstChild.data == new_kern_type_name:
                    found_ftype = True
            elif class_attr == "ffunc":
                names = sid.getElementsByTagName("name")
                assert names[0].firstChild.data == new_kern_name
        assert found_ftype
        # Function/routine definitions
        func_list = xml_doc.getElementsByTagName("FfunctionDefinition")
        names = func_list[0].getElementsByTagName("name")
        assert names[0].firstChild.data == new_kern_name
        sym_list = func_list[0].getElementsByTagName("id")
        for sym in sym_list:
            if sym.getAttribute("sclass") == "ffunc":
                names = sym.getElementsByTagName("name")
                assert names[0].firstChild.data == new_kern_name
