All PSyclone development is coordinated through GitHub Issues and Pull Requests.

Before creating a new Issue, please check that the feature or problem you are considering
is not already discussed in the [PSyclone documentation](https://psyclone.readthedocs.io/en/latest).
or covered by the existing list of [Issues](https://github.com/stfc/PSyclone/issues). If in doubt,
please create a new Issue and give it the `question` label to mark it for attention.

If this is the first time you have performed PSyclone development work then you will
probably want to read the [Working with PSyclone from GitHub](https://psyclone.readthedocs.io/en/latest/developer_guide/working_practises.html)
section of the Developer Guide.

When creating a new Issue, please give it a descriptive title, possibly including the key
component of PSyclone involved in square brackets, e.g. "[PSyIR] Add support for some
arkane Fortran feature". In the description please provide a summary of the problem or the
feature that the Issue is intended to tackle.

During development work please use the Issue to make notes of any design decisions or
problems encountered. Please also tag all commit messages with the Issue number e.g.:

    git commit -m "#111 make an amazing change" some/modified/file.py
    
so that all related commits will show up in the Issue.

PSyclone is written in Python which brings with it a host of benefits. However, Python is
also very flexible and with great flexibility comes great responsibility. The PSyclone
project therefore has fairly strict coding and documentation standards in order to
ensure the robustness and maintainability of the code base. The coding standards are
summarised on the [code-review page](https://github.com/stfc/PSyclone/wiki/CodeReview)
of the GitHub PSyclone wiki. Similarly, the requirements for in-code documentation are
also on the [wiki](https://github.com/stfc/PSyclone/wiki/InterfaceMarkup).
