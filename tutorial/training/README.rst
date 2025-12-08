PSyclone Training
=================

This is the training material for the usage of PSyclone, a code
transformation and generator tool. PSyclone was originally developed
for the UK Met Office’s new LFRic numerical weather prediction system,
but it’s features of modifying large codes based on scripts have found
other use cases, for example NOAA’s Tsunami model MOST (Method of
Splitting Tsunamis) and Nemo (Nucleus for European Modelling of the
Ocean) are both using it.

There are three different user groups that
will be using PSyclone. Firstly, research who will be using any model
that uses PSyclone for code transformation will see PSyclone being used
during the build process, and in case of errors might need to be able
to have a very high-level understanding to diagnose the reason for the
error. Secondly, natural scientists who want to write code using the
code generation capabilities will need to be aware of PSyclone’s rules.
Lastly, HPC experts who are writing recipes (or scripts) to optimise
code need to have a much deeper understanding of PSyclone’s internal
data structure, behaviour and features.

The 2-day training course is a sequence of presentations, followed
by a hands-on session to allow the trainees to try the concepts
introduced in a simple test case. Some of the hands-on sessions are
more open ended, and trainees are encouraged to go back to these
examples and implement better solutions later.

The trainings material is split into four parts, and the files
and documentation here are only the hands-on section. The full
training is accompanied by a presentations
(a slightly outdated version of the slides are available on the `wiki
<https://github.com/stfc/PSyclone/wiki/Presentations>`_). It
contains the following session (in order in which they are presented):


PSyclone for LFRic Users
------------------------
This introductory session is “PSyclone for LFRic Users”, which
is part of the official LFRic training. It can be used
stand-alone to introduce anyone working with LFRic to what PSyclone
is and what it does. But it is also a valuable
introduction for HPC experts, and as such is included here.
The hands-on session is in the directory
``tutorial/training/psyclone_for_lfric_users``.

PSyclone GOcean DSL
-------------------
The material in ``tutorial/training/gocean`` contains an
introduction to PSyclone domain-specific-language features (DSL).
It uses the easier ``gocean`` API to introduce the user to
writing kernels, applying transformations, and writing
transformation scripts. It is the longest session in this trainings
material and will take one full day.

PSyclone Transformation API
---------------------------
PSyclone can also be used as a transformation tool for generic
Fortran code, i.e. not using its DSL code-creation features.
The material in ``tutorial/training/transformation`` contains
material related to transforming existing Fortran code.

PSyclone LFRic DSL
------------------
This section introduces the LFRic DSL. This DSL is a lot more
complex than the GOcean API used previously. Many of the concepts
from the previous sections will apply without modifications, but
due to the more complex data structures this DSL uses, it's
harder to understand.
