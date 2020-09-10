.. when generating html no "Bibliography" section is generated so we
   do it manually
.. only:: html

    Bibliography
    ============

.. if this bib file processing happens *before* any source files that
   contain citations then those citations aren't handled correctly.
   We therefore put a "zz" at the start of the name of the current file
   to ensure it is processed last.

.. This file is included from the user_guide and developer_guide
   subdirectories. We therefore use a relative path which works from
   both subdirectories to reference the bib file here:

.. bibliography:: ../bibliography/references.bib
