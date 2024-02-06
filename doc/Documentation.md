# User's guide

## Introduction

This repository contains a python script, pyft.py, that reads a FORTRAN code,
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
This is achieved through the scope concept.
The scope is a string representing a kind of path to access the source code
fragment on which the action must be performed.
A scope is a succession of path elements separated by '/'; each path elements
has one of the following forms:

 - **module:_NAME_** to refer to the module named _NAME_
 - **sub:_NAME_** to refer to the subroutine named _NAME_
 - **func:_NAME_** to refer to the function named _NAME_
 - **type:_NAME_** to refer to the definition of the type named _NAME_


## Tool options

If only one file name is given, the output file will replace the input file.

### Parser options

**\--parser=PATH** can be used to specify the full path to the fxtran executable.
Usefull if one wants to use a specific version.

**\--parserOption=OPTIONS** the list of available options can be found in the fxtran
documentation. If no option is provided, the defaults one will be used (the
list of default options can be seen with "pyft.py -h"). In case this option
is used, the default options will be replaced by the ones specified.

**\--wrapH** Wrap .h file content into a MODULE to enable the parsing by fxtran.

### Input and output

**\--renamefF** transforms in upper case the file extension.

**\--renameFf** transforms in lower case the file extension.

**\--xml=filename** writes the xml file (after transformation) in filename.

**\--dryRun** prevents the FORTRAN writting.

### General options

**\--simplify** some transformations may render other parts of the code useless.
If this option is set, these parts are automatically removed.

**\--logLevel=LEVEL** specifies the log level to use (e.g. debug).
With the info level, execution time and number of calls are printed
for each called functions. In addition, with the debug level, input
and output of all the called functions are printed.

**\--enableCache** activates a cache to obtain the node's parent faster.

### Dealing with variables

**\--showVariables** displays a list of all the declared variables
with some characteristics.

**\--removeVariable WHERE VARNAME** removes the declaration of a local variable, a module variable or
of a dummy argument. In the case of a dummy argument, it is also suppresssed
from the argument of the subroutine. In case of a module variable, if the module
becomes unused, the use statement is also removed.
This options takes two argument, the first one describes where the variable
is declared (to distinguish between several variables holding the same name
but in different subroutines) and the second one is the variable name.
The first argument is a scope as described in [Concepts](#concepts).

**\--addVariable WHERE VARNAME DECLARATION POSITION** adds a new variable.
The first argument describes where the variable
must be declared (it is a scope as described in [Concepts](#concepts)).
The second one is the variable name, the third one is the declarative statement to insert,
and the fourth one is the position (python indexing) the new variable will have in the
calling statment of the routine (non-integer value for a local variable)

**--addModuleVariable WHERE MODULENAME VARNAME** adds a USE statement with an ONLY attribute. The first
argument describes where the variable must be declared (it is a scope as
described in [Concepts](#concepts)). The second one is the module name and
the third one is the variable name.

**\--attachArraySpecToEntity** move the array declaration attributes to the right
part of the declaration statement (e.g. "REAL, DIMENSION(5) :: X" becomes "REAL :: X(5)")

**\--showUnusedVariables [WHERE]** lists the unused varibales. Without argument all the
unused variables are shown. If one argument is given it is the scope (as described
in [Concepts](#concepts)) where unused variables are searched for.

**\--removeUnusedLocalVariables WHERE EXCLUDE** remove unused local variables. Without argument all the
unused variables are suppressed. If one argument is given it is the scope (as described
in [Concepts](#concepts)) where unused variables are searched for.

**\--removePHYEXUnusedLocalVariables WHERE EXCLUDE** variation aroud the \--removeUnusedLocalVariables
to deal with the variables declared in the mnh\_expand directives

**\--addExplicitArrayBounds** Adds explicit bounds to arrays that already have parentheses.

**\--addArrayParentheses** Adds parentheses to arrays.

**\--modifyAutomaticArrays DECL#START#END** modifies all automatic
arrays declaration in subroutine and functions. The declaration is replaced by the DECL template,
the START template is inserted as first executable statement and the END template as last executable
statement. Each template can use the following place holders: "{doubledotshape}", "{shape}", "{lowUpList}",
"{name}" and "{type}" which are, respectively modified into ":, :, :", "I, I:J, 0:I", "1, I, I, J, 0, I",
"A", "REAL" if the original declaration statement was "A(I, I:J, 0:I)". The template
"{type}, DIMENSION({doubledotshape}), ALLOCATABLE :: {name}#ALLOCATE({name}({shape}))#DEALLOCATE({name})"
replaces automatic arrays by allocatables.

**\--replaceAutomaticWithAllocatable** replace all automatic arrays with allocatables

### Cosmetics

**\--upperCase** puts the FORTRAN code into upper case letters.

**\--lowerCase** puts the FORTRAN code into lower case letters.

**\--changeIfStatementsInIfConstructs** transforms one line 'IF' contructs
in 'IF-THEN' constructs

**\--reDimKlonArrayToScalar** remove NIJ, NI or NJ dimension to all 1D and 2D arrays:
these arrays become scalar.

**\--indent** correct the indentation of the source code.

**\--removeIndent** remove the indentation

**\--removeEmptyLines** remove empty lines.

**\--removeComments** remove the comments.

**\--updateSpaces** suppress and/or add spaces. Delimiters and operators must be surrounded by spaces.
Commas must be followed by a space. Lines must not end with spaces. Parenthesis must not be
surrounded by spaces...

**\--alignContinuation** align the beginings of continued lines.

**\--addBeginContinuation** add missing continuation characters ('&') at the begining of lines.

**\--removeBeginContinuation** remove continuation characters ('&') at the begining of lines.

**\--removeALLContinuation** remove all continuation characters('&').

**\--pretify** equivalent to --indent --upperCase --removeEmptyLines --updateSpaces
--addBeginContinuation --alignContinuation

**\--minify** equivalent to --removeIndent --upperCase --removeEmptyLines --removeComments
--updateSpaces --removeALLContinuation

### Checks

**\--checkIMPLICIT=Warn\|Err** if the 'IMPLICIT NONE' statment is missing,
issue a warning if option is 'Warn'; otherwise issue an error message and
raise an exception.

**\--checkINTENT=Warn\|Err** if an INTENT attribute is missing for a
dummy argument, issue a warning if option is 'Warn'; otherwise issue
an error message and raise an exception.

### Dealing with statements

**\--removeCall WHERE CALLNAME** removes call statements. The first argument describes from where the
call statements must be suppressed (it is a scope as described in [Concepts](#concepts)).
The second argument is the subprogram name.

**\--removePrints** removes print statements. The argument describes from where the
call statements must be suppressed (it is a scope as described in [Concepts](#concepts)).

**\--inlineContainedSubroutines** inline containted subroutines in main routine.

### Miscellaneous

**\--showScopes** print the different scopes found in the source code.

### Applications

**\--deleteDrHook** removes DrHook statements.

**\--deleteBudgetDDH** delete Budget/DDH use.

**\--deleteNonColumnCallsPHYEX** delete call to PHYEX routines that needs information on horizontal
points (multiple column dependency).

**\--removeIJLoops** remove DO loops on I and J dimensions (1, KLON).

**\--expandAllArraysPHYEX** expand all array syntax (computing and where block) using PHYEX conventions.

**\--expandAllArrays** expand all array syntax (computing and where block).

**\--inlineContainedSubroutinesPHYEX** inline containted subroutines in main routine using PHYEX conventions.

**\--addStack** add local arrays to the stack.

**\--addIncludes** add .h includes in the file and remove the INCLUDE statement.

**\--checkStackArginCall** check in all CALL statements if YLSTACK must be present.

**\--mnhExpand** apply the mnh\_expand directives using DO loops.

**\--mnhExpandConcurrent** apply the mnh\_expand directives using DO CONCURRENT loops.

### Preprocessor

**\--applyCPPifdef** This option is followed by the list of defined or undefined CPP keys.
All #ifdef and #ifndef concerning these keys are evaluated. Undefined keys are preceded by
a percentage sign '%' (e.g. if we use '--applyCPPifdef K', '#ifdef K' is evaluated to True;
whereas if we use '--applyCPPifdef %K', '#ifdef K' is evaluated to False.
But the method does not evaluate more complicated cpp directives such as '#if defined'.

### Tree

**\--tree** Directories where source code must be searched for.

**\--descTree** File name where the description of the tree is stored. If the file doesn't
exist, it will be created using the \--tree option.

**\--plotCompilTree** File name for compilation dependency graph (.dot or image extension).
If \--descTree is used, the descTree file will be used, otherwise the tree (provided
with the \--tree option) is explored. See \--plotMaxUpper and \--plotMaxLower options.

**\--plotExecTree** File name for the calling graph (.dot or image extension).
If \--descTree is used, the descTree file will be used, otherwise the tree (provided
with the \--tree option) is explored. See \--plotMaxUpper and \--plotMaxLower options.

**\--plotMaxUpper** Maximum number of elements to plot, upper than the central element.

**\--plotMaxLower** Maximum number of elements to plot, lower than the central element.
