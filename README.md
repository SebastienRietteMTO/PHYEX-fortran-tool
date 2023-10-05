# PHYEX-fortran-tool

This set of tools is primarily designed to be applied on the [PHYEX](https://github.com/UMR-CNRM/PHYEX)
repository. But they are normally generic enough to be used in other contexts.

The tools can be used with the command line (the pft.py utility can be found in the bin directory)
or as a python package (pypft directory). The documentation is in the doc directory and some examples
are in the examples directory. 

The fxtran binary is needed to use pypft. You can:
  - use an existing installation and make sure it can be found using your PATH variable;
  - use an existing installation providing the exact path with the 'parser' option;
  - install fxtran using the INSTALL.sh script (in the bin directory)

To use the python package or the pfpt.py command line tool, you must source the env.sh
file (in the bin directory). E.g. '. \<path to the current directory\>/bin/env.sh'
