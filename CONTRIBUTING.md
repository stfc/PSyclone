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

    git commit -m "#111 make an amazing change" some/modified/file.py
    
so that all related commits will show up in the Issue.

PSyclone is written in Python which brings with it a host of benefits. However, Python is
also very flexible and with great flexibility comes great responsibility. The PSyclone
project therefore has fairly strict [coding and documentation standards](https://psyclone.readthedocs.io/en/latest/developer_guide/coding-style.html)
in order to ensure the robustness and maintainability of the code base.

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

## Guidelines for Performing a PSyclone Code Review

 1. Replace the "ready for review" label on the PR with the "under review" label. This helps to prevent multiple
    people attempting to review the same thing and makes it clear at what stage in the work-flow the PR is.
 2. Check that branch is up-to-date with master (the pull request should report that the branch can be "merged automatically").  If not, return to developer for them to bring it up-to-date.
 3. py.test must report 0 failures.
 4. Check that any x-failing tests are unaffected by the current Issue. (i.e. should they now pass?)
 5. Check that there are no TODOs in the code that refer to the current Issue/PR.
 6. If a test using the `disable_declaration_check` fixture is modified then the test should also be modified to remove the need for this fixture. If this results in this fixture no longer being used in any tests then the fixture should also be removed and issue #754 closed.
 7. Use the "Files changed" tab on the pull request to review all code changes. Check that all code modifications are as pythonic as possible, well commented, easy to understand and that the code is correct. Comments and requests for changes may be made in-line on the "Files changed" tab. This makes it easier for the developer to see which part of the code is being discussed.
 8. If code changes suggested in 7. are not minor then return to developer to address.
 9. Check that the docstring in the containing method/function for any new/modified code describes its arguments (using Sphinx markup notation).
 10. Check that any new/modified code is covered by the test suite e.g. `py.test --cov-report term-missing --cov psyclone.dynamo0p3` or see the report produced by [CodeCov](https://codecov.io/gh/stfc/PSyclone) after GitHub Actions has run the test suite. To see this report, look for the CodeCov comment on the PR (in the conversation) and follow the "Continue to review full report at Codecov" link. Once there, go to the "Diff" tab and look at any files for which the diff coverage is not 100%. (Occasionally CodeCov fails to work properly. In that case, you can see the raw coverage reports at the end of the detailed output for the GitHub Action.)
 11. We are working towards having the source in any given file being 100% covered by an associated test file. Therefore please check that
this is the case for any new code added in the PR.
 12. If the Fortran being generated by PSyclone has changed then check that the test suite includes an option to
    compile it. Use `py.test --compile --compileopencl ...` to check compilation of the new code (this must be run from the `tests` directory in order to pick-up the compilation-related test fixtures). This check is also performed by the integration tests.
 13. Check that the copyright/author details are correct in any modified files.
 14. Check that any new/modified code passes flake8(`flake8 .` or just `flake8 <python file>`)
 15. Check that any new/modified code passes pylint, i.e. is free from errors (but see note below) and has a score > 9/10.
 16. Generate documentation (`cd doc; make html SPHINXOPTS="-W --keep-going"`) and check that it is up-to-date if new functionality has been added in the ticket.
 17. Check that the examples work when compilation is enabled (`make compile` in the `examples` directory (this is also done by the integration tests). Consider whether the ticket includes significant new functionality that would benefit from being demonstrated in an example (and if so, whether it is).
 18. Launch and verify the correctness and performance of the integration tests (see instructions below).
 19. If the integration tests completed successfully, performance numbers for NEMO and LFRic will be uploaded [into the Integration Test results](https://psyclone.readthedocs.io/en/latest/developer_guide/integration-test.html). The reviewer can check that there hasn't been any performance degradation, but note that currently the runner does not have exclusive access to the testing system and the performance may sometimes be impacted by other users in the system.
20. If the LFRic integration test changes the lfric_app hash, the reviewer must also consider the changes in the associated https://github.com/stfc/lfric_apps (and sometimes https://github.com/stfc/lfric_core) PRs.

Note that pylint doesn't understand pytest and therefore reports spurious errors of the form:

    "Module 'pytest' has no 'raises' member (no-member)"

these can safely be ignored.

Once the review is complete there are two options:

 1. If the reviewer is happy with the pull request, then they can proceed to merge the branch onto master (see below)
 2. If the reviewer is requesting changes, then the "under review" label on the PR should be replaced by "reviewed with actions" and it is then up to the original developer to address the reviewer's concerns.

## Merging a branch to master

Once a pull-request has passed code review, the code reviewer should merge the associated branch onto master:

 1. Check-out the most recent version of the branch
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

1. Create a "new release" Issue. e.g. #337
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
