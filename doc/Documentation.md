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

## Concepts

Especially when a FORTRAN source file contains several subroutine, functions
or type declaration, it is necessary to specify on which part of the source
code a modification must be done.
This is achieved through the locality concept.
The locality is a string representing a kind of path to access the source code
fragment on which the action must be performed.
A locality is a succession of path elements separated by '/'; each path elements
has one of the following forms:

 - **module:_NAME_** to refer to the module named _NAME_
 - **sub:_NAME_** to refer to the subroutine named _NAME_
 - **func:_NAME_** to refer to the function named _NAME_
 - **type:_NAME_** to refer to the definition of the type named _NAME_


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

**\--removeVariable** removes the declaration of a local variable or
of a dummy argument. In the case of a dummy argument, it is also suppresssed
from the argument of the subroutine.
This options takes two argument, the first one describes where the variable
is declared (to distinguish between several variables holding the same name
but in different subroutines) and the second one is the variable name.
The first argument is a locality as described in [Concepts](#concepts).

**\--attachArraySpecToEntity** move the array declaration attributes to the right
part of the declaration statement (e.g. "REAL, DIMENSION(5) :: X" becomes "REAL :: X(5)")

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

### Applications

**\--deleteDrHook** removes DrHook statements.

**\--changeIfStatementsInIfConstructs** transforms one line 'IF' contructs
in 'IF-THEN' constructs
