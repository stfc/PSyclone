.. _tutorial:

Tutorial
========

PSyclone provides a hands-on tutorial. The easiest way to follow it is reading
the `Readme files in github <https://github.com/stfc/PSyclone/tree/master/tutorial/practicals>`_.
The tutorial is divided into two sections, a first section that introduces
PSyclone and how to
`use it to transform generic Fortran code  <https://github.com/stfc/PSyclone/tree/master/tutorial/practicals/generic>`_
(this is the recommended starting point for everybody).
And a second section about
`the LFRic DSL <https://github.com/stfc/PSyclone/tree/master/tutorial/practicals/LFRic>`_
(this is only recommended for people interested in PSyKAL DSLs and LFRic in particular).

To do the proposed hands-on you will need a linux shell with Python installed and to
download the hands-on directory with:

.. code-block:: bash

    git clone --recursive git@github.com:stfc/PSyclone.git
    cd PSyclone
    # If psyclone isn't already installed you can use 'pip' in this folder to
    # install a version that matches the downloaded tutorials
    pip install .
    cd tutorial/practicals

