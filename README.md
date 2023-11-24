# Python-fortran-tool

This set of tools is primarily designed to be applied on the [PHYEX](https://github.com/UMR-CNRM/PHYEX)
repository. But they are normally generic enough to be used in other contexts.

The tools can be used with the command line (the pyft\_tool.py utility can be found in the bin directory)
or as a python package (src/pyft directory). The documentation is in the doc directory and some examples
are in the examples directory. 

The fxtran binary is needed to use pyft. You can:
  - use an existing installation and make sure it can be found using your PATH variable;
  - use an existing installation providing the exact path with the '--parser' option;
  - automatically install fxtran using the INSTALL.sh script (in the bin directory)

To use the python package or the pfpt\_tool.py command line tool, you must source the env.sh
file (in the bin directory). E.g. '. \<path to the current directory\>/bin/env.sh'.

Prerequisites:
  - an internet connexion (with access to the github servers) is needed only for the installation of fxtran
  - python > 3.8 (but only tested with version 3.10)
  - a C compiler (tested with cc 11.4.0)

Quick Start Guide:
  - open a terminal on a system satisfying the prerequisites and enter the following commands
  - if you don't have a github ssh key or don't know what it is:
    > git clone https://github.com/UMR-CNRM/pyft.git  
    > ./pyft/bin/INSTALL.sh
  - if you have a github ssh key:
    > git clone git@github.com:UMR-CNRM/pyft.git  
    > ./pyft/bin/INSTALL.sh --ssh
