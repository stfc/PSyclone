# PSyclone Jupyter-Based Tutorial

Welcome to the Jupyter-based parts of the PSyclone tutorial. It
provides material on fparser (the Fortran parser used by PSyclone),
the PSyclone Internal Representation and the NEMO API.  To get started
you will need a working Jupyter installation:

```bash
$ pip install jupyter
```

If you are working in a Linux environment that has a web browser
available then you can start the tutorial by doing:

```bash
$ jupyter-notebook introduction.ipynb
```

This should open a browser window displaying the "Welcome to the
PSyclone Tutorial" page.

If you are using the Windows Subsystem for Linux and do not have a
web-browser installed under it then you can use the `--no-browser`
option:

```bash
$ jupyter-notebook --no-browser
[I 10:56:40.327 NotebookApp] Writing notebook server cookie secret to /home/me/.local/share/jupyter/runtime/notebook_cookie_secret
[I 10:56:46.410 NotebookApp] Serving notebooks from local directory: /home/me/Projects/PSyclone/tutorial/notebooks
[I 10:56:46.411 NotebookApp] Jupyter Notebook 6.1.5 is running at:
[I 10:56:46.412 NotebookApp] http://localhost:8888/?token=5192928a6ac972470f65f303c276271bcf12
[I 10:56:46.414 NotebookApp]  or http://127.0.0.1:8888/?token=5192928a6ac972470f65f303c276271bcf12
[I 10:56:46.415 NotebookApp] Use Control-C to stop this server and shut down all kernels (twice to skip confirmation).
[C 10:56:46.529 NotebookApp]

    To access the notebook, open this file in a browser:
        file:///home/me/.local/share/jupyter/runtime/nbserver-463-open.html
    Or copy and paste one of these URLs:
        http://localhost:8888/?token=5192928a6ac972470f65f303c276271bcf12
     or http://127.0.0.1:8888/?token=5192928a6ac972470f65f303c276271bcf12
```

Once the server is up and running, do as it says and cut-n-paste one
of the provided URLs into your web browser running under Windows.
This should then display the list of files in the current directory.
Click on `introduction.ipynb` in order to begin the tutorial.
