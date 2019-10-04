All PSyclone development is coordinated through GitHub Issues and Pull Requests.

Before creating a new Issue, please check that the feature or problem you are considering
is not already discussed in the [User Guide](https://psyclone.readthedocs.io/en/latest) or
[Developers' Guide](https://psyclone-dev.readthedocs.io/en/latest) or covered by the
existing list of [Issues](https://github.com/stfc/PSyclone/issues). If in doubt, please
create a new Issue and give it the `question` label to mark it for attention.

If this is the first time you have performed PSyclone development work then you will
probably want to read the [Getting Going](https://psyclone.readthedocs.io/en/stable/getting_going.html)
section of the User Guide and the [Working with PSyclone from GitHub](https://psyclone.readthedocs.io/en/stable/developers.html#working-with-psyclone-from-github)
section of the Developers' Guide. You may also find the [Reference Guide](https://psyclone-ref.readthedocs.io/en/latest/index.html)
useful for understanding the overall code structure.

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
