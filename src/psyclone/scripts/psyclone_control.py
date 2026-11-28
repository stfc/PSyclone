#!/usr/bin/env python3

##############################################################################
# (c) Crown copyright Met Office. All rights reserved.
# For further details please refer to the file COPYRIGHT
# which you should have received as part of this distribution
##############################################################################

'''
This module reads in a psyclone_info.yaml file.
'''
import os
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

    :param name: the name of the rule, i.e. the script name, or a
        special term like `exclude` or `file_specific`.
    :param info: the yaml information of the rule (`params` and `files`).
    :param first_rule: the first rule with the same name (if this rule is a
        repetition of a script name). Its parameters are used as defaults,
        updated with the parameters of this rule.

    :raises ValueError: if a file-specific parameter is not one of the
        parameters declared for the rule, or if a repeated rule specifies
        a parameter that is not declared in the first rule.
    """

    def __init__(self, name: str,
                 info: dict[str, Any],
                 first_rule: Optional["PsycloneRule"] = None) -> None:
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

        if first_rule:
            first_params = first_rule._default_params
            unknown = set(self._default_params) - set(first_params)
            if unknown:
                raise ValueError(
                    f"Rule '{name}' is repeated and specifies parameter(s) "
                    f"{sorted(unknown)} which are not listed in the first "
                    f"instance of the rule ({sorted(first_params)}).")
            merged = first_params.copy()
            merged.update(self._default_params)
            self._default_params = merged

        if "files" not in info:
            raise ValueError(f"Rule '{name}' must have a 'files' attribute, "
                             f"got '{info}'.")
        self._file_list: list[tuple[str, dict[str, Any]]] = []
        files = info["files"]
        # Support shortcut to specify files as a list without using `-` for
        # each file. This can't be used to specify parameters for individual
        # files, but makes the rules shorter and easier to read.
        if isinstance(files, str):
            files = {i: {} for i in files.split()}
        if not files:
            # Could be None
            files = []
        for file in files:
            if isinstance(file, str):
                params = {}
            else:
                file, params = next(iter(file.items()))
                if params is None:
                    params = {}
            unknown = set(params) - set(self._default_params)
            if unknown:
                raise ValueError(
                    f"Rule '{name}': file '{file}' specifies parameter(s) "
                    f"{sorted(unknown)} which are not listed in the "
                    f"'params' of this rule "
                    f"({sorted(self._default_params)}).")
            if file in ["\\*", "'*'", '"*"']:
                file = "*"
            self._file_list.append((file, params))

    def get_yaml_dict(self) -> dict[str, Any]:
        """
        :returns: a dictionary representation to be used as an entry of the
            `rules` list in a yaml file.
        """
        files = [{file: params} for file, params in self._file_list]
        content = {"params": self._default_params, "files": files}
        if self.name in PsyclonePhase.SPECIAL_RULES:
            return {self.name: content}
        return {"script": self.name, **content}

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
    :param script_paths: the root directories to search for optimisation
        scripts (e.g. .../lfric_atm/optimisation), in order of precedence.
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
    SPECIAL_RULES = (FILE_SPECIFIC, EXCLUDE, NO_SCRIPT)

    # The template used to determine the output file if the control file
    # does not specify an `output_template`.
    DEFAULT_OUTPUT_TEMPLATE = "{rel_path}/{name}"

    def __init__(self, name: str,
                 script_paths: list[Path]) -> None:
        self._script_paths = list(script_paths)
        # This will be initialised/updated each time when reading an info file.
        self._opt_paths: list[Path] = list(self._script_paths)
        self._relative_script_dir: str = ""
        self._name: str = name
        self._comment: str = ""
        self._api: str = ""
        self._file_pattern: Optional[str] = None
        self._output_template: str = self.DEFAULT_OUTPUT_TEMPLATE
        self._rules: list[PsycloneRule] = []
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
    def file_pattern(self) -> Optional[str]:
        """
        :returns: the file pattern to which this phase applies, if specified.
        """
        return self._file_pattern

    @property
    def output_template(self) -> str:
        """
        :returns: the template used to determine the output file.
        """
        return self._output_template

    def get_output_path(self, source_file: Path, input_path: Path,
                        output_path: Path) -> Path:
        """
        Determines the output file for a source file by evaluating the
        output template of this phase. The template can use these
        variables (using the syntax `{name}`, as in an f-string):

        - `rel_path`: the directory of the source file, relative to the
          input path.
        - `stem`: the stem of the source file (name without suffix).
        - `suffix`: the suffix of the source file (including the dot).
        - `name`: the name of the source file (without any directory).

        :param source_file: the source file to process.
        :param input_path: the root directory of the source files.
        :param output_path: the root directory for the output files.

        :returns: the output file.

        :raises ValueError: if the template uses an unknown variable, or
            results in a path outside of the output path.
        """
        values = {"rel_path": str(source_file.parent.relative_to(input_path)),
                  "stem": source_file.stem,
                  "suffix": source_file.suffix,
                  "name": source_file.name}
        try:
            relative = self._output_template.format(**values)
        except (KeyError, IndexError, ValueError) as err:
            raise ValueError(
                f"Invalid output_template '{self._output_template}' in phase "
                f"'{self._name}': {err}. Valid variables are "
                f"{sorted(values)}.") from err
        result = Path(os.path.normpath(output_path / relative))
        if not result.is_relative_to(output_path):
            raise ValueError(
                f"The output_template '{self._output_template}' in phase "
                f"'{self._name}' results in '{result}' for '{source_file}', "
                f"which is outside of the output path '{output_path}'.")
        return result

    @property
    def opt_paths(self) -> list[Path]:
        """
        :return: all optimisation directories in which scripts are searched
            (in order of precedence). Each is an absolute path including the
            site- and platform-specific settings, and subdirectory (e.g.
            psykal or transmute).
        """
        return self._opt_paths[:]

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
            elif phase == "file_pattern":
                self._file_pattern = info["file_pattern"]
            elif phase == "output_template":
                self._output_template = info["output_template"]
            elif phase == "script_dir":
                script_template = Template(info["script_dir"])
                self._relative_script_dir = (
                    script_template.substitute(os.environ))
            elif phase == "rules":
                for item in info["rules"]:
                    self._add_rule(item)
            else:
                raise ValueError(f"Unknown entry '{phase}' in phase "
                                 f"'{self._name}', rules must be listed "
                                 f"under 'rules'.")

        # Store the potentially updated optimisation root path, i.e. the site-
        # and platform-specific location, followed by a script dir (typically
        # transmute or psykal). This path is used in a few places.
        self._opt_paths = [root / self._relative_script_dir
                           for root in self._script_paths]

    def _add_rule(self, item: dict[str, Any]) -> None:
        """
        Adds a rule from an entry of the `rules` list. A script rule is
        specified as `script: name`, followed by its `params` and `files`.
        A special rule (`exclude`, `no_script` or `file_specific`) is
        specified by using the special name as only key, with the `params`
        and `files` as its value. If a rule with the same name exists
        already, the defaults of its parameters are taken from the first
        instance.

        :param item: the yaml data of the rule.

        :raises ValueError: if the entry is not a valid rule.
        """
        if not isinstance(item, dict):
            raise ValueError(f"Invalid rule '{item}' in phase "
                             f"'{self._name}', expected a mapping.")
        if "script" in item:
            name = item["script"]
            info = {key: value for key, value in item.items()
                    if key != "script"}
        elif len(item) == 1 and next(iter(item)) in self.SPECIAL_RULES:
            name, info = next(iter(item.items()))
            info = info or {}
        else:
            raise ValueError(
                f"Invalid rule '{item}' in phase '{self._name}': a rule must "
                f"either have a 'script' entry, or be one of "
                f"{sorted(self.SPECIAL_RULES)}.")
        first_rule = next((r for r in self._rules if r.name == name), None)
        self._rules.append(PsycloneRule(name, info, first_rule))

    def get_yaml_dict(self) -> dict[str, Any]:
        """
        :returns: returns ths data in this object in a dictionary, suitable
            to write them back as a yaml file.
        """

        yaml_dict: dict[str, Any] = {"comment": self.comment,
                                     "api": self.api,
                                     "script_dir": self._relative_script_dir}
        if self.file_pattern is not None:
            yaml_dict["file_pattern"] = self.file_pattern
        yaml_dict["output_template"] = self.output_template

        yaml_dict["rules"] = [rule.get_yaml_dict() for rule in self._rules]

        return yaml_dict

    def file_specific_script(self, fpath: Path,
                             input_path: Path) -> Optional[Path]:
        """
        Searches for a file-specific optimisation script in all script
        search paths. The script is expected to have the same path relative
        to the script directory as the source file has relative to the
        input path, but with the suffix `.py`.

        :param fpath: the file path of the Fortran file.
        :param input_path: the root directory of the source files.

        :returns: the path of the file-specific optimisation script, or
            None if no such file exists (including if the file is not
            under the input path).
        """
        try:
            relative_path = fpath.relative_to(input_path)
        except ValueError:
            return None

        for opt_path in self._opt_paths:
            local_transformation_script = (
                opt_path / relative_path.with_suffix('.py'))
            if local_transformation_script.exists():
                return local_transformation_script
        return None

    def get_script(self, fpath: Path,
                   input_path: Path) -> tuple[Path, dict[str, Any]]:
        """
        Return the script and its parameters for a given filename.

        This function will also provided to Fab's PSyclone step, and as such
        it will receive the config object, even though it is not used.

        :param fpath: the Fortran source file for which to find a
            transformation script.
        :param input_path: the root directory of the source files, used to
            find file-specific scripts.

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
                script = self.file_specific_script(fpath, input_path)
                if script:
                    return script, params
                if pattern == "*":
                    continue
                raise FileNotFoundError(
                    f"Cannot find explicitly requested script for '{fpath}'.")

            candidates = [opt_path / rule.name for opt_path in self._opt_paths]
            for opt_script in candidates:
                if opt_script.exists():
                    return opt_script, params
            searched = ", ".join(f"'{i}'" for i in candidates)
            raise FileNotFoundError(f"Cannot find script '{rule.name}' for "
                                    f"'{fpath}'. Searched: {searched}.")

        return PsyclonePhase.RESULT_EXCLUDE, {}


class PsycloneControl:
    """
    This class stores the information from psyclone_control.yaml file(s).
    Several files can be read, and latter information will extend the rules
    from previous files, and replace the phases executed.

    Details of each phase will be stored in PSyclonePhase instances.

    :param script_paths: the root directories to search for optimisation
        scripts (e.g. .../lfric_atm/optimisation), in order of precedence.
    :param fab_base: The FabBase derived application script. This is required
        to get site, platform and config information when searching for
        PSyclone scripts to be executed.
    """

    def __init__(self,
                 script_paths: list[Path]) -> None:
        # Keep a copy in case that the user modifies the list later
        self._script_paths = list(script_paths)
        self._all_phases: list[str] = []
        self._psyclone_info: dict[str, PsyclonePhase] = {}
        # A list of all PSyclone info files that were read.
        # Only used to add useful comments to the yaml output.
        self._all_files_read: list[Path] = []

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
                                                         self._script_paths)

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

    psyclone_control = PsycloneControl(Path("."), [Path(".")])
    psyclone_control.read(args.file)
    print(psyclone_control.to_yaml())
