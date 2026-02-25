# Images Directory

This directory contains the images used in the training.
Each image is stored first as a .svg file. This format allows
to modify the image in an appropriate editor (and is independent
of resolution). While .svg files can be used in github markdown,
the images end up being too big when displayed. Using
`<img ...` as embedded html did also not work (and will likely
be rejected by linter).

So, the svg images are therefore being exported as png (with the
appropriate resolution of 360x560), which is then displayed
as expected by github in a browser.

Never modify the png files directly, instead modify the .svg
files, and re-export the images.
