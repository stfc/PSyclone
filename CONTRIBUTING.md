# Contribution Guidelines

All PSyclone development is coordinated through GitHub Issues and Pull Requests.

## Reporting issues

Before creating a new Issue, please check that the feature or problem you are considering
is not already discussed in the [PSyclone documentation](https://psyclone.readthedocs.io/en/latest).
or covered by the existing list of [Issues](https://github.com/stfc/PSyclone/issues). If in doubt,
please create a new Issue and give it the `question` label to mark it for attention.

When creating a new Issue, please give it a descriptive title, possibly including the key
component of PSyclone involved in square brackets, e.g. "[PSyIR] Add support for some
arkane Fortran feature". In the description please provide a summary of the problem or the
feature that the Issue is intended to tackle.

## Preparing your code

If this is the first time you have performed PSyclone development work then you will
probably want to read the [Working with PSyclone from GitHub](https://psyclone.readthedocs.io/en/latest/developer_guide/working_practises.html)
section of the Developer Guide.

During development work please use the Issue to make notes of any design decisions or
problems encountered. Please also tag all commit messages with the Issue number e.g.:

    git commit -m "#<issue_number> make an amazing change" some/modified/file.py
    
so that all related commits will show up in the Issue.

PSyclone is written in Python which brings with it a host of benefits. However, Python is
also very flexible and with great flexibility comes great responsibility. The PSyclone
project therefore has fairly strict [coding and documentation standards](https://psyclone.readthedocs.io/en/latest/developer_guide/coding-style.html)
in order to ensure the robustness and maintainability of the code base.

## Policy for AI generated code in PSyclone

AI coding assistants are welcome in PSyclone development the same way any other tool is welcome,
with the expectation that a human contributor is responsible for everything they submit, including
the authorship, adherance to the licensing and comprehension of the submitted code. AI is not
permitted as a substitute of that comprehension. Contributors are responsible for ensuring
that the AI tool complies with their employer's policies.

## Creating a Pull Request

Once your code is ready, submit a [GitHub PR](https://github.com/stfc/PSyclone/pulls)

When the PR is ready for review please give it the "ready for review" label and request
a review from one or more maintainers: Andy [arporter], Sergi [sergisiso], Aidan [LonelyCat124]
or Joerg [hiker] are your current options. Although you can request a review from multiple people,
only one review actually needs to be performed.

A review is performed in GitHub by creating comments on points in the code that need attention.
The PR author needs to respond to each of these comments and the result is a 'conversation'.
It is up to the reviewer to decide when a conversation can be marked as 'resolved' (i.e. the
point has been dealt with to their satisfaction).

## Guidelines (for reviewers) to Perform a PSyclone Code Review

 1. Replace the "ready for review" label on the PR with the "under review" label.

 2. Check that the automated tests succeeded:
   - The flake8 linter should pass (run locally with `flake8 .`)
   - pytest must report 0 failures (run locally with `pytest -n auto src/psyclone/tests`)
   - Test code coverage must be 100% (run locally with `pytest --cov-report term-missing --cov psyclone`)
   - Check that examples still work (run locally with `cd examples; make`)
   - Docs build and linkcheck reports no broken links (run locally with `cd doc; make html SPHINXOPTS="-W --keep-going"`)
   - If this closes an issue, no remaining TODOs in the code reference the issue. (run locally with `GITHUB_TOKEN=<your personal token> python utils/check_closed_issue_refs.py`)
    
 3. Check validations currently not fully automated
   - Check that any x-failing tests are unaffected by the current Issue. (i.e. should they now pass?)
   - Check that the branch passed the integration test. This needs to be triggered with a reviewer approval and includes compilation tests (run locally with `pytest --compile --compileopencl ...`), compilation examples (run locally with `cd examples; make compile`) and NEMO and LFRic integration tests.
   - If the integration tests completed successfully, performance numbers for NEMO and LFRic will be uploaded [into the Integration Test results](https://psyclone.readthedocs.io/en/latest/developer_guide/integration-test.html). The reviewer can check that there hasn't been any performance degradation, but note that currently the runner does not have exclusive access to the testing system and the performance may sometimes be impacted by other users in the system.
   - If the LFRic integration test changes the lfric_app hash, the reviewer must also consider the changes in the associated https://github.com/stfc/lfric_apps (and sometimes https://github.com/stfc/lfric_core) PRs.

 4. Check progress on ongoing work necessary on modified code.
   - If a test using the `disable_declaration_check` fixture is modified then the test should also be modified to remove the need for this fixture. If this results in this fixture no longer being used in any tests then the fixture should also be removed and issue #754 closed.
   - If a method signature without typehints is modified, then typehints should have been added and the docstring must be updated to the current desired format. 

 5. Use the "Files changed" tab on the pull request to review all code changes.
   - Check that all code modifications are correct, as pythonic as possible, well commented and easy to understand.
   - Check that the docstring in the containing method/function for any new/modified code describes its arguments (using Sphinx markup notation).
   - Check that new documentation sections or examples have been added if the PR bring significant new functionality.
   - Check that the copyright/license details are correct in any modified files.
   - Use pylint to analyse the code. The reviewer should take pylint suggestions judiciously.
   - Comments and requests for changes may be made in-line on the "Files changed" tab. This makes it easier for the developer to see which part of the code is being discussed.

Once the review is complete there are two options:

 1. If the reviewer is happy with the pull request, then they can proceed to merge the branch onto master (see below)
 2. If the reviewer is requesting changes, then the "under review" label on the PR should be replaced by "reviewed with actions" and it is then up to the original developer to address the reviewer's concerns.

## Merging a branch to master

Once a pull-request has passed code review, the code reviewer should merge the associated branch onto master:

 1. Check-out the most recent version of the branch and ensure it is up-to-date with master.
 2. Check that all tests pass in this version (sanity check)
 3. Update the changelog in the top-level directory (using the text describing the Issue)
    ```bash
    cd PSyclone; vi changelog
    ```
 4. Commit these changes and push branch to GitHub:
    ```bash
    git add changelog
    git commit -m "#<issue-number> update changelog"
    git push
    ```
 5. On the pull-request page on GitHub, request the branch be merged to master
 6. Delete the original branch
 7. Close the associated issue (if appropriate and if GitHub hasn't already done so)
 8. If necessary accept the stfc/lfric_core and stfc/lfric_app changes.

## Launching the Integration Tests

Note that only users with the Admin role are able to launch integration tests and see the results.

If you are a reviewer, go to the [PSyclone Github Actions tab](https://github.com/stfc/PSyclone/actions), then click on the "Push to private" workflow in the list on the left-hand side. This will display the "This workflow has a workflow_dispatch event trigger." message. Click on the corresponding "Run workflow" button, select the appropriate branch and Run. After a few seconds a new "Push to private" action will appear with a yellow icon saying "pending"; click this, go to "Review" and press "Approve". This will push the selected branch to a private, mirror repository which will then run the integration tests. After at least an hour, go to the [private mirror repository](https://github.com/stfc/PSyclone-mirror/actions/) to see the results.

## Verifying that test kernels/algorithms are actually used in tests

To count the number of times each testkern is used by each of the test algorithms (assuming the latter are all named something like 1*.f90):

    cd test_files/dynamo0p3
    for i in testkern_*.f90; do echo $i; j=`echo $i | sed -e 's/.f90//'`; grep -c $j 1*.f90 | awk 'BEGIN{FS=":"};
    {count+=$2};END{print count}'; done

To count the number of times each test algorithm is actually used in a test:

    cd test_files/dynamo0p3
    for i in 1*.f90; do echo $i; grep -c $i ../../*.py | awk 'BEGIN{FS=":"};{count+=$2};END{print count}'; done

Be aware that, particularly for long filenames, this grep may not work because the name might be broken
over more than one line in the Python file.

##  Guidelines for createing PSyclone Release

1. Create a "new release" Issue.
2. Create a release candidate PR and ensure that it works with a released version of fparser. As part of this:
- Ensure that the line in the `PSyclone/.github/workflows/python-package.yml` file that separately installs the fparser submodule from the git submodule (`pip install external/fparser`) is commented out.
- Ensure that the version of fparser pointed to by the git submodule (PSyclone/external/fparser) is consistent with the release of fparser specified in `setup.py`. `cd submodule_name; git checkout master <or required version>; git pull; cd ..; git add submodule_name; git commit (if you want to at this point)`
- Ensure that the `fparser` entry under `dependencies` in `pyproject.toml` specifies the *precise* version of fparser to use (e.g. 'fparser==0.2.4').
- Update the version number in `src/psyclone/version.py`, `doc/reference_guide/doxygen.config`. Also check that the copyright dates in the latter file as well as the `doc/{user_guide,developer_guide,reference_guide/source}/conf.py` files are correct.
- *Temporarily* remove the trailing "-dev" from the version strings in src/psyclone/version.py. i.e. change:
```python
__SHORT_VERSION__ = f"{__MAJOR__:d}.{__MINOR__:d}-dev"
__VERSION__ = f"{__MAJOR__:d}.{__MINOR__:d}.{__MICRO__:d}-dev"
```
to
```python
__SHORT_VERSION__ = f"{__MAJOR__:d}.{__MINOR__:d}"
__VERSION__ = f"{__MAJOR__:d}.{__MINOR__:d}.{__MICRO__:d}"
```
3. Go through the standard review process. Once approved the changelog should be updated with the release number and date.
4. Use GitHub to create a draft new release: https://github.com/stfc/PSyclone/releases. It will create a new tag with the release number and it will auto-generate release notes if you ask it too but these will typically require editing to make them more user friendly.
5. Once the master branch on GitHub has all these changes from step 3 (and completed the CI) and all PSyclone developers are happy with the draft release in step 4, use GitHub to create the release using the draft.
6. If read the docs fails then login and force rebuild manually.
7. A 'Upload Release to PyPI' action should automatically be submitted, but this needs to be manually approved. If if fails, follow the documentation in: https://packaging.python.org/tutorials/packaging-projects.
8. Check that a) pypi has created a new release, b) read the docs stable and latest has documentation from the latest release.
9. Update PSyclone in upstream Spack and Conda:
   * Create a fork of the Spack repo (https://github.com/spack/spack);
   * Create a branch in this fork for the version update;
   * Use `spack edit py-psyclone` to update the PSyclone package;
   * Create a pull request on the Spack repo for this branch;
10. Tell lfric list and PSyclone teams we have a new release and summarise relevant new features.
11. Optionally: tell Karen, Holly, Marion that we have a new release and summarise interesting new features.
12. Close issue.

After the release:

1. Revert your changes to `version.py`;
2. Revert your changes for fparser.
