# User's guide

## Introduction

This repository contains a python script, pft.py, that reads a FORTRAN code,
parse it in xml, performs some manipulation, reverts it in FORTRAN and
writes it back on disk.

This scripts suppose that the original source code is written using UTF-8
encoding. If not, some special characters could be altered by the double
conversion. Apart from this, the resulting FORTRAN source code is exactly
the same as the INPUT source code if no manipulation is performed.

DEPENDENCIES: [fxtran](https://github.com/pmarguinaud/fxtran) must be installed.

LIMITATIONS:

 - Depending on where there are put, pre-processor directives can break
   the parsing by fxtran
 - Other encoding than UTF-8 is not supported

## Tool options

If only one file name is given, the output file will replace the input file.

### Parser options

**\--parser** can be used to specify the full path to the fxtran executable.
Usefull if fxtran is not found or if a specific version must be used.

**\--parserOption** the list of available options can be found in the fxtran
documentation. If no option is provided, the defaults one will be used (the
list of default options can be seen with "pft.py -h").

### Input and output

**\--renamefF** transforms in upper case the file extension.

**\--renameFf** transforms in upper case the file extension.

**\--xml=filename** writes the xml file in filename.

**\--dryRun** prevents the FORTRAN writting.

### Dealing with variables

**\--showVariables** displays a list of all the declared variables
with some characteristics.

**\-removeVariable** removes the declaration of a local variable or
of a dummy argument. In the case of a dummy argument, it is also suppresssed
from the argument of the subroutine.

### Cosmetics

**\--upperCase** puts the FORTRAN code into upper case letters.

**\--lowerCase** puts the FORTRAN code into lower case letters.

### Checks

**\--checkIMPLICIT=Warn\|Err** if the 'IMPLICIT NONE' statment is missing,
issue a warning if option is 'Warn'; otherwise issue an error message and
raise an exception.

**\--checkINTENT=Warn\|Err** if an INTENT attribute is missing for a
dummy argument, issue a warning if option is 'Warn'; otherwise issue
an error message and raise an exception.
