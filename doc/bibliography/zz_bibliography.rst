.. when generating html no "Bibliography" section is generated so we
   do it manually
.. only:: html

    Bibliography
    ============

.. if this bib file processing happens *before* any source files that
   contain citations then those citations aren't handled correctly.
   We therefore put a "zz" at the start of the name of the current file
   to ensure it is processed last.
.. bibliography:: references.bib
