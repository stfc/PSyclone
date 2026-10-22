#!/usr/bin/env python3

##############################################################################
# (c) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file COPYRIGHT
# which you should have received as part of this distribution
##############################################################################

'''
This module reads in a psyclone_info.yaml file.
'''
from pathlib import Path
from string import Template
from typing import Any, Optional
import yaml


class PsycloneRule:
    """
    This class stores a single rule for a PSyclone phase, i.e. one script
    name (or reserved name like `exclude` or `file_specific`) and the list
    of files to which to apply the rule. It also manages the default script
    parameter and the file-specific overwrites

    :param rule: the name of the rule  the script name, or a
        special term like `exclude` or `file_specific`.
    :param file_list: the list of files to which to apply the rule to.
    """

    def __init__(self, name: str,
                 info: dict[str, Any]) -> None:
        self._name = name
        # While the values of the parameters can be anything (e.g. a list),
        # from the view of this script which calls PSyclone and passes the
        # parametervon on, they are just strings (which will be converted
        # into the corresponding Python type in PSyclone).
        self._default_params: dict[str, Any]
        if "params" in info:
            self._default_params = info["params"]
        else:
            self._default_params = {}

        if "files" not in info:
            raise ValueError(f"Rule '{name}' must have a 'files' attribute, "
                             f"got '{info}'.")
        self._file_list: list[tuple[str, dict[str, Any]]] = []
        files = info["files"]
        # Support shortcut to specify files as a list without using `-` for
        # each file.
        # This can't be used to specify
        if isinstance(files, str):
            files = {i: {} for i in files.split()}
        for file in files:
            if isinstance(file, str):
                params = {}
            else:
                file, params = next(iter(file.items()))
                if params is None:
                    params = {}
            if file in ["\\*", "'*'", '"*"']:
                file = "*"
            self._file_list.append((file, params))

    def get_yaml_dict(self) -> dict[str, Any]:
        """
        :returns: a dictionary representation to be used as yaml file.
        """
        d = {self.name: {"params": self._default_params,
                         "files": []}}
        for file, params in self._file_list:
            d[self.name]["files"].append({file: params})
        return d

    @property
    def name(self) -> str:
        """
        :returns: the name of the rule (typically the script name, or
            special term like `exclude` or `file_specific`).
        """
        return self._name

    def get_matching_pattern(
            self, fpath: Path) -> Optional[tuple[str, dict[str, Any]]]:
        """
        Find the last file pattern matching the supplied path.

        :returns: the matching pattern and its parameters, including rule
            defaults, or None if no pattern matches.
        """
        file_str = str(fpath)
        for pattern, overrides in self._file_list[::-1]:
            if pattern in file_str or pattern == "*":
                params = self._default_params.copy()
                params.update(overrides)
                return pattern, params
        return None


class PsyclonePhase:
    """
    This class stores the set of rules and settings for one specific PSyclone
    phase.

    :param name: the name of this phase.
    :param base_paths: the list of base paths to search for file-specific
        scripts.
    :param script_root: the root directory to find optimisation scripts in
        (e.g. .../lfric_atm/optimisation)
    :param template_data: optional dictionary of template data to be used
        when substituting variables in the script_dir.
    """

    # Define some special 'script paths' strings to indicate special results.
    # They use :: in the name, which is typically not a valid name for a
    # script.
    RESULT_EXCLUDE = Path("::RESULT_EXCLUDE")
    RESULT_NO_SCRIPT = Path("::RESULT_NO_SCRIPT")

    # Define special 'script names' strings to indicate special results.
    # They are used to indicate that a file_specific script should be used,
    # that the file should not be processed, or that it should be processed
    # but without any PSyclone transformation script.
    FILE_SPECIFIC = "file_specific"
    EXCLUDE = "exclude"
    NO_SCRIPT = "no_script"

    def __init__(self, name: str,
                 base_paths: list[Path],
                 script_root: Path,
                 template_data: Optional[dict[str, str]] = None) -> None:
        self._base_paths = base_paths
        self._script_root = script_root
        # This will be initialised/updated each time when reading an info file.
        self._opt_path = Path()
        self._relative_script_dir: str = ""
        self._name: str = name
        self._comment: str = ""
        self._api: str = ""
        self._file_list: Optional[str] = None
        self._rules: list[PsycloneRule] = []
        self._template_data = template_data if template_data else {}
        self._default_parameter = {}

    @property
    def name(self) -> str:
        """
        :returns: the name of this phase.
        """
        return self._name

    @property
    def comment(self) -> str:
        """
        :returns: the comment for this phase.
        """
        return self._comment

    @property
    def api(self) -> str:
        """
        :returns: the PSyclone command line options.
        """
        return self._api

    @property
    def file_list(self) -> Optional[str]:
        """
        :returns: the file pattern to which this phase applies, if specified.
        """
        return self._file_list

    @property
    def opt_path(self) -> Path:
        """
        :return: the optimisation root as absolute path (including site-
            and platform-specific settings, and subdirectory, e.g. psykal
            or transmute).
        """
        return self._opt_path

    def update(self, info: dict[str, Any]) -> None:
        """
        Update this PSyclone information with data taken from a
        PSyclone info yaml file. This function is used to initially
        read in a first specification, or update a specification based
        on an additional file being read in later.

        :param info: the yaml information taken from a PSyclone control file.
        """
        for phase in info:
            if phase == "comment":
                self._comment = info["comment"]
            elif phase == "api":
                self._api = info["api"]
            elif phase == "file_list":
                self._file_list = info["file_list"]
            elif phase == "script_dir":
                script_template = Template(info["script_dir"])
                self._relative_script_dir = (
                    script_template.substitute(self._template_data))
            else:
                # Must be a script name (or special term like `exclude` or
                # `file_specific`). Store this as a rule:
                pr = PsycloneRule(phase, info[phase])
                self._rules.append(pr)

        # Store the potentially updated optimisation root path, i.e. the site-
        # and platform-specific location, followed by a script dir (typically
        # transmute or psykal). This path is used in a few places.
        self._opt_path = self._script_root / self._relative_script_dir

    def get_yaml_dict(self) -> dict[str, Any]:
        """
        :returns: returns ths data in this object in a dictionary, suitable
            to write them back as a yaml file.
        """

        yaml_dict: dict[str, Any] = {"comment": self.comment,
                                     "api": self.api,
                                     "script_dir": self._relative_script_dir}
        if self.file_list is not None:
            yaml_dict["file_list"] = self.file_list

        for rule in self._rules:
            yaml_dict.update(rule.get_yaml_dict())

        return yaml_dict

    def file_specific_script(self, fpath: Path) -> Optional[Path]:
        """
        Searches for a file-specific optimisation script. It will search
        both under the source and the build directories of the project
        directory.

        :param fpath: the file path of the Fortran file.
        :returns: the path of the file-specific optimisation script, or
            None if no such file exists.
        """
        relative_path = None
        # The source file might be either in build_output (e.g. a preprocessed
        # .X90 file), or still in source (.x90 file). Check if the file
        # is in one of the two sub-trees, and use the relative path to
        # check if there is a file-specific optimisation script
        for base_path in self._base_paths:
            try:
                relative_path = fpath.relative_to(base_path)
            except ValueError:
                # The file is not under the `base_path` - keep on checking
                pass

        if relative_path:
            # The file was under either source or build. Check if there
            # is a file-specific optimisation script:
            local_transformation_script = (self.opt_path /
                                           (relative_path.with_suffix('.py')))
            if local_transformation_script.exists():
                return local_transformation_script
        return None

    def get_script(self, fpath: Path) -> tuple[Path, dict[str, Any]]:
        """
        Return the script and its parameters for a given filename.

        This function will also provided to Fab's PSyclone step, and as such
        it will receive the config object, even though it is not used.

        :param fpath: the Fortran source file for which to find a
            transformation script.

        :returns: the path to the transformation script (or a result marker
            if no script should be used) and its parameters.
        """

        # Search starting from the end, so last rule wins
        for rule in self._rules[::-1]:
            matching = rule.get_matching_pattern(fpath)
            if matching is None:
                continue
            pattern, params = matching

            if rule.name == PsyclonePhase.EXCLUDE:
                return PsyclonePhase.RESULT_EXCLUDE, {}

            if rule.name == PsyclonePhase.NO_SCRIPT:
                return PsyclonePhase.RESULT_NO_SCRIPT, {}

            # file_specific may fall through if only wildcard patterns
            # matched and no companion script exists.
            if rule.name == PsyclonePhase.FILE_SPECIFIC:
                script = self.file_specific_script(fpath)
                if script:
                    return script, params
                if pattern == "*":
                    continue
                raise FileNotFoundError(
                    f"Cannot find explicitly requested script for '{fpath}'.")

            opt_script = self.opt_path / rule.name
            if not opt_script.exists():
                raise FileNotFoundError(f"Cannot find script "
                                        f"'{opt_script}' for '{fpath}'.")
            return opt_script, params

        return PsyclonePhase.RESULT_EXCLUDE, {}


class PsycloneControl:
    """
    This class stores the information from psyclone_control.yaml file(s).
    Several files can be read, and latter information will extend the rules
    from previous files, and replace the phases executed.

    Details of each phase will be stored in PSyclonePhase instances.

    :param script_root: the root directory to find optimisation scripts in
        (e.g. .../lfric_atm/optimisation)
    :param base_paths:
    :param fab_base: The FabBase derived application script. This is required
        to get site, platform and config information when searching for
        PSyclone scripts to be executed.
    """

    def __init__(self,
                 script_root: Path,
                 base_paths: list[Path],
                 template_data: Optional[dict[str, str]] = None) -> None:
        # Keep a copy in case that the user modifies the list later
        self._base_paths = base_paths[:]
        self._script_root = script_root
        self._all_phases: list[str] = []
        self._psyclone_info: dict[str, PsyclonePhase] = {}
        # A list of all PSyclone info files that were read.
        # Only used to add useful comments to the yaml output.
        self._all_files_read: list[Path] = []
        if not template_data:
            self._template_data = {}
        else:
            self._template_data = template_data

    @property
    def all_phases(self) -> list[str]:
        """
        :returns: the list of all PSyclone phases to execute.
        """
        return self._all_phases

    def get_info(self, phase: str) -> PsyclonePhase:
        """
        Returns the PSyclone information for the specified phase.

        :param phase: the name of the phase.

        :returns: the PSyclone Information for the specified phase.
        """
        return self._psyclone_info[phase]

    def to_yaml(self) -> str:
        """
        This returns a string representation of the combined read yaml files.
        This is useful to be logged to show the actual details used.

        :returns: the string represenation in yaml format of this PSyclone
            control instance.
        """

        yaml_dict = {"phases": self._all_phases}
        for phase in self._all_phases:
            yaml_dict[phase] = self._psyclone_info[phase].get_yaml_dict()

        files_read = '\n'.join(f"#       {i}" for i in self._all_files_read)
        yaml_string = yaml.dump(yaml_dict, default_flow_style=False,
                                sort_keys=False)
        return f"# Files read:\n{files_read}\n{yaml_string}"

    def read(self, file_path: Path) -> None:
        """
        Reads a yaml file, and extends the potentially existing information.
        Any phases specified in the new read yaml file will replace the
        phases to be executed (i.e. will overwrite what was previously
        specified). Any new rule sets will be added as new PsyclonePhase
        instance. Rules for an exiting phase will be appended to the
        existing information. The precedence handling means that any later
        rule will overwrite any previous rule.

        :param file_path: the file_path to read.
        """

        with open(file_path, "r", encoding="utf8") as stream:
            dependencies = yaml.safe_load(stream)
            print("READ", dependencies)
            self._all_files_read.append(file_path.resolve())

        # First take phases (if available)
        if dependencies.get("phases", None):
            self._all_phases = dependencies["phases"]

        for key in dependencies:
            if key == "phases":
                # Already handled
                continue
            if key not in self._psyclone_info:
                self._psyclone_info[key] = PsyclonePhase(key,
                                                         self._base_paths,
                                                         self._script_root,
                                                         self._template_data)

            self._psyclone_info[key].update(dependencies[key])


if __name__ == "__main__":
    # This is a simple test to read in a psyclone_control.yaml file and
    # print it back out again. It is useful to check that the file can be
    # read in and that the output is as expected.
    import argparse

    parser = argparse.ArgumentParser(
        description="Read in a PSyclone control yaml file, and print it back "
                    "out again.")
    parser.add_argument("file", type=Path,
                        help="The PSyclone control yaml file to read.")
    args = parser.parse_args()

    psyclone_control = PsycloneControl(Path("."), [Path(".")],
                                       template_data={"target": "test_target"})
    psyclone_control.read(args.file)
    print(psyclone_control.to_yaml())
