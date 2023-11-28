#!/usr/bin/env python3

from pyft import PYFT

def isint(s):
    """
    :param s: string to test for intergerness
    :return: True if s represent an int
    """
    try:
        int(s)
    except ValueError:
        return False
    else:
        return True

if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser(description='Python FORTRAN tool')

    parser.add_argument('--simplify', default=False, action='store_true',
                        help='After a deletion, recursively deletes the code ' + \
                             'and variables that have become useless')
    parser.add_argument('--logLevel', default='warning',
                        help='Provide logging level. Example --logLevel debug (default is warning)' )

    #Inputs and outputs
    gInOut = parser.add_argument_group('Input and output')
    gInOut.add_argument('INPUT', help='FORTRAN input file')
    gInOut.add_argument('OUTPUT', default=None, help='FORTRAN output file', nargs='?')
    gInOut.add_argument('--renamefF', default=False, action='store_true',
                        help='Put file extension in upper case')
    gInOut.add_argument('--renameFf', default=False, action='store_true',
                        help='Put file extension in lower case')
    gInOut.add_argument('--xml', default=None, type=str,
                        help='Output file for xml')
    gInOut.add_argument('--dryRun', default=False, action='store_true',
                        help='Dry run without writing the FORTRAN file (the xml ' + \
                             'is still written')

    #fxtran
    gParser = parser.add_argument_group('fxtran parser relative options')
    gParser.add_argument('--parser', default=None, type=str,
                         help='Path to the fxtran parser binary')
    gParser.add_argument('--parserOption', nargs='*', action='append',
                         help='Option to pass to fxtran, defaults' + \
                              ' to {}'.format(str(PYFT.DEFAULT_FXTRAN_OPTIONS)))
    gParser.add_argument('--wrapH', default=False, action='store_true',
                         help='Wrap .h file content into a MODULE to enable the reading')

    #Variables
    gVariables = parser.add_argument_group('Options to deal with variables')
    gVariables.add_argument('--showVariables', default=False, action='store_true',
                            help='Show the declared variables')
    gVariables.add_argument('--removeVariable', nargs=2, action='append',
                            metavar=('WHERE', 'VARNAME'),
                            help="Variable to remove from declaration. The first argument " + \
                                 "is the SUBROUTINE/FUNCTION/MODULE/TYPE where the variable " + \
                                 "is declared. It is '/'-separated path with each element having " + \
                                 "the form 'module:<name of the module>', 'sub:<name of the subroutine>', " + \
                                 "'func:<name of the function>' or 'type:<name of the type>'. " + \
                                 "The second argument is the variable name")
    gVariables.add_argument('--attachArraySpecToEntity', default=False, action='store_true',
                            help='Find all T-decl-stmt elements that have a child element attribute' + \
                                 ' with attribute-N=DIMENSION and move the attribute into EN-N elements')
    gVariables.add_argument('--addVariable', nargs=4, action='append',
                            metavar=('WHERE', 'VARNAME', 'DECLARATION', 'POSITION'),
                            help='Add a variable. First argument is the scope (as for ' + \
                                 'the --removeVariable option. The second is the variable ' + \
                                 'name, the third is the declarative statement to insert, ' + \
                                 'the fourth is the position (python indexing) the new ' + \
                                 'variable will have in the calling statment of the ' + \
                                 'routine (non-integer value for a local variable).')
    gVariables.add_argument('--addModuleVariable', nargs=3, action='append',
                            metavar=('WHERE', 'MODULENAME', 'VARNAME'),
                            help='Add a USE statement. The first argument is the scope (as for ' + \
                                 'the --removeVariable option). The second is the module ' + \
                                 'name; the third is the variable name.')
    gVariables.add_argument('--showUnusedVariables', nargs='?', action='append',
                            metavar='WHERE', default=None,
                            help='Show a list of unused variables in the entire code ' + \
                                 'or in the scope (if specified).')
    gVariables.add_argument('--removeUnusedLocalVariables', nargs=2, action='append',
                            metavar=('WHERE', 'EXCLUDE'), default=None,
                            help='Remove unused local variables in the specified scope ' + \
                                 '(use the special scope name ALL to apply on the entire ' + \
                                 'code), excluding some variables (comma-separated list or NONE ' + \
                                 'to exclude nothing).')
    gVariables.add_argument('--removePHYEXUnusedLocalVariables', nargs=2, action='append',
                            metavar=('WHERE', 'EXCLUDE'), default=None,
                            help='Remove unused local variables in the specified scope ' + \
                                 '(use the special scope name ALL to apply on the entire ' + \
                                 'code), excluding some variables (comma-separated list or NONE ' + \
                                 'to exclude nothing). This option takes into account the ' + \
                                 'mnh_expand directives to prevent from removing useful variables.')

    #Cosmetics
    gCosmetics = parser.add_argument_group('Cosmetics options')
    gCosmetics.add_argument('--upperCase', default=False, action='store_true',
                            help='Put FORTRAN code in upper case letters')
    gCosmetics.add_argument('--lowerCase', default=False, action='store_true',
                            help='Put FORTRAN code in lower case letters')
    gCosmetics.add_argument('--changeIfStatementsInIfConstructs', default=False, action='store_true',
                            help='Find all if-statement and convert it to if-then-statement')
    gCosmetics.add_argument('--reDimKlonArrayToScalar', default=False, action='store_true',
                            help='Remove NIJ, NI or NJ dimension to all 1D and 2D arrays : these arrays become scalar')
    gCosmetics.add_argument('--indent', default=False, action='store_true',
                            help='Correct indentation')
    gCosmetics.add_argument('--removeIndent', default=False, action='store_true',
                            help='Remove indentation')
    gCosmetics.add_argument('--removeEmptyLines', default=False, action='store_true',
                            help='Remove empty lines')
    gCosmetics.add_argument('--removeComments', default=False, action='store_true',
                            help='Remove comments')
    gCosmetics.add_argument('--updateSpaces', default=False, action='store_true',
                            help='Updates spaces around operators, commas, parenthesis and at the end of line')
    gCosmetics.add_argument('--alignContinuation', default=False, action='store_true',
                            help='Align the beginings of continued lines')
    gCosmetics.add_argument('--addBeginContinuation', default=False, action='store_true',
                            help='Add missing continuation characters (\'&\') at the begining of lines')
    gCosmetics.add_argument('--removeBeginContinuation', default=False, action='store_true',
                            help='Remove continuation characters (\'&\') at the begining of lines')
    gCosmetics.add_argument('--removeALLContinuation', default=False, action='store_true',
                            help='Remove all continuation characters(\'&\')')
    gCosmetics.add_argument('--prettify', default=False, action='store_true',
                            help='Prettify the source code (indentation, spaces...)')
    gCosmetics.add_argument('--minify', default=False, action='store_true',
                            help='Simplify the source code (indentation, spaces...)')
    
    #Applications
    gApplications = parser.add_argument_group('Options to apply upper level transformation')
    gApplications.add_argument('--deleteDrHook', default=False, action='store_true',
                               help='Delete DR HOOK use')
    gApplications.add_argument('--deleteBudgetDDH', default=False, action='store_true',
                               help='Delete Budget/DDH use')
    gApplications.add_argument('--deleteNonColumnCallsPHYEX', default=False, action='store_true',
                               help='Delete call to PHYEX routines that needs information on horizontal ' + \
                                    'points (multiple column dependency')
    gApplications.add_argument('--removeIJLoops', default=False, action='store_true',
                               help='Remove DO loops on I and J dimensions (1,KLON)')
    gApplications.add_argument('--expandAllArraysPHYEX', default=False, action='store_true',
                               help='Expand all array syntax (computing and where block) ' + \
                               'using PHYEX conventions')
    gApplications.add_argument('--expandAllArrays', default=False, action='store_true',
                               help='Expand all array syntax (computing and where block) ' + \
                                    'using mnh directives if present')
    gApplications.add_argument('--inlineContainedSubroutines', default=False, action='store_true',
                               help='Inline containted subroutines in main routine')
    gApplications.add_argument('--addStack', nargs=2, action='append',metavar=('TYPE', 'MODEL'),
                               help='Add local arrays to the stack')
    gApplications.add_argument('--addIncludes', default=False, action='store_true',
                               help='Add .h includes in the file and remove the INCLUDE statement')  
    gApplications.add_argument('--checkStackArginCall', default=False, action='store_true',
                               help='Check in all CALL statements if YLSTACK must be present')
    gApplications.add_argument('--mnhExpand', default=False, action='store_true',
                               help='Apply the mnh_expand directives with DO loops')
    gApplications.add_argument('--mnhExpandConcurrent', default=False, action='store_true',
                               help='Apply the mnh_expand directives with DO CONCURRENT loops')

    #Checks
    gChecks = parser.add_argument_group('Check options')
    gChecks.add_argument('--checkIMPLICIT', choices={'Warn', 'Err'}, default=None,
                         help='Send a warning or raise an error if the "IMPLICIT NONE" ' + \
                              'is missing')
    gChecks.add_argument('--checkINTENT', choices={'Warn', 'Err'}, default=None,
                         help='Send a warning or raise an error if the "INTENT" ' + \
                              'attribute is missing for a dummy argument')

    #Statements
    gStatement = parser.add_argument_group('Statements options')
    gStatement.add_argument('--removeCall', nargs=2, action='append',
                            metavar=('WHERE', 'CALLNAME'),
                            help="Call to remove from the source code. The first argument " + \
                                 "is the SUBROUTINE/FUNCTION/MODULE where the call statements " + \
                                 "have to be removed. It is '/'-separated path with each element having " + \
                                 "the form 'module:<name of the module>', 'sub:<name of the subroutine>' or " + \
                                 "'func:<name of the function>'. Use 'ALL' to suppress all the call " + \
                                 "statements regardless where there are. " + \
                                 "The second argument is the subprogram name")
    gStatement.add_argument('--removePrints', action='append',
                            help="Remove print statements from the source code. The argument " + \
                                 "is the SUBROUTINE/FUNCTION/MODULE where the print statements " + \
                                 "have to be removed. It is '/'-separated path with each element having " + \
                                 "the form 'module:<name of the module>', 'sub:<name of the subroutine>' or " + \
                                 "'func:<name of the function>'. Use 'ALL' to suppress all the call " + \
                                 "statements regardless where there are.")

    #Misc
    gMisc = parser.add_argument_group('Miscellaneous')
    gMisc.add_argument('--showScopes', default=False, action='store_true',
                       help='Show the different scopes found in the source code')

    #Tree
    gTree = parser.add_argument_group('Tree')
    gMisc.add_argument('--tree', default=None, action='append',
                       help='Directories where source code must be searched for')


    #Preprocessor
    gCpp = parser.add_argument_group('Preprocessor')
    gCpp.add_argument('--applyCPPifdef', nargs='*', action='append',
                      help="This option is followed by the list of defined or undefined CPP keys. " + \
                           "All #ifdef and #ifndef concerning these keys are evaluated. " + \
                           "Undefined keys are preceded by a percentage sign.")


    args = parser.parse_args()
    simplify = {'simplify': args.simplify}

    #Opening and reading of the FORTRAN file
    if args.parserOption is None:
        parserOptions = PYFT.DEFAULT_FXTRAN_OPTIONS.copy()
    else:
        parserOptions = [el for elements in args.parserOption for el in elements]
    if args.addIncludes:
        parserOptions = [opt for opt in parserOptions if opt not in ('-no-include', '-noinclude')]
    pft = PYFT(args.INPUT, args.OUTPUT, parser=args.parser, parserOptions=parserOptions,
               verbosity=args.logLevel, wrapH=args.wrapH, tree=args.tree)

    #File name manipulations
    if args.renamefF: pft.renameUpper()
    if args.renameFf: pft.renameLower()

    #Variables
    if args.showVariables: pft.showVarList()
    if args.attachArraySpecToEntity: pft.attachArraySpecToEntity()
    if args.removeVariable is not None: pft.removeVar(args.removeVariable, **simplify)
    if args.addVariable is not None: pft.addVar([[v[0], v[1], v[2], (int(v[3]) if isint(v[3]) else None)] for v in args.addVariable])
    if args.addModuleVariable is not None: pft.addModuleVar([[v[0], v[1], v[2]] for v in args.addModuleVariable])
    if args.showUnusedVariables is not None:
        if len(args.showUnusedVariables) == 1 and args.showUnusedVariables[0] is None:
            pft.showUnusedVar()
        else:
            pft.showUnusedVar(args.showUnusedVariables)
    if args.removeUnusedLocalVariables is not None:
        for where, exclude in args.removeUnusedLocalVariables:
            pft.removeUnusedLocalVar(where if where != 'ALL' else None,
                                     [item.strip() for item in exclude.split(',')] if exclude != 'NONE' else None,
                                     **simplify)
    if args.removePHYEXUnusedLocalVariables is not None:
        for where, exclude in args.removePHYEXUnusedLocalVariables:
            pft.removePHYEXUnusedLocalVar(where if where != 'ALL' else None,
                                          [item.strip() for item in exclude.split(',')] if exclude != 'NONE' else None,
                                          **simplify)

    #Applications
    if args.addStack is not None: pft.addStack(args.addStack[0][0], args.addStack[0][1])
    if args.addIncludes: pft.addIncludes()
    if args.checkStackArginCall: pft.checkStackArginCall()
    if args.deleteDrHook: pft.deleteDrHook(**simplify)
    if args.deleteBudgetDDH: pft.deleteBudgetDDH(**simplify)
    if args.deleteNonColumnCallsPHYEX: pft.deleteNonColumnCallsPHYEX(**simplify)
    if args.inlineContainedSubroutines: pft.inlineContainedSubroutines()
    if args.expandAllArrays: pft.removeArraySyntax()
    if args.expandAllArraysPHYEX: pft.expandAllArraysPHYEX()
    if args.removeIJLoops: pft.removeIJLoops()
    assert not (args.mnhExpand and args.mnhExpandConcurrent), "Only one of --mnhExpand and --mnhExpandConcurrent"
    if args.mnhExpand: pft.removeArraySyntax(everywhere=False)
    if args.mnhExpandConcurrent: pft.removeArraySyntax(concurrent=True, everywhere=False)

    #Cosmetics
    if args.upperCase: pft.upperCase()
    if args.lowerCase: pft.lowerCase()
    if args.changeIfStatementsInIfConstructs: pft.changeIfStatementsInIfConstructs()
    if args.reDimKlonArrayToScalar: pft.reDimKlonArrayToScalar()
    if args.indent: pft.indent()
    if args.removeIndent: pft.indent(indent_programunit=0, indent_branch=0)
    if args.removeEmptyLines: pft.removeEmptyLines()
    if args.removeComments: pft.removeComments()
    if args.updateSpaces: pft.updateSpaces()
    kw_updateCnt = dict(align=args.alignContinuation,
                        addBegin=args.addBeginContinuation,
                        removeBegin=args.removeBeginContinuation,
                        removeALL=args.removeALLContinuation)
    if True in kw_updateCnt.values(): pft.updateContinuation(**kw_updateCnt)
    if args.prettify:
        pft.indent()
        pft.upperCase()
        pft.removeEmptyLines()
        pft.updateSpaces()
        pft.updateContinuation()
    if args.minify:
        pft.indent(indent_programunit=0, indent_branch=0)
        pft.upperCase()
        pft.removeComments()
        pft.removeEmptyLines()
        pft.updateSpaces()
        pft.updateContinuation(align=False, removeALL=True, addBegin=False)

    #Checks
    if args.checkIMPLICIT is not None: pft.checkImplicitNone(args.checkIMPLICIT == 'Err')
    if args.checkINTENT is not None: pft.checkIntent(args.checkINTENT == 'Err')

    #Statements
    if args.removeCall is not None:
        for rc in args.removeCall: pft.removeCall(rc[1], None if rc[0] == 'ALL' else rc[0], **simplify)
    if args.removePrints is not None:
        for rp in args.removePrints: pft.removePrints(None if rp == 'ALL' else rp, **simplify)

    #Misc
    if args.showScopes: pft.showScopesList()

    #Preprocessor
    if args.applyCPPifdef: pft.applyCPPifdef([k for l in args.applyCPPifdef for k in l])

    #Writing
    if args.xml is not None: pft.writeXML(args.xml)
    if not args.dryRun:
        pft.write()

    #Closing
    pft.close()
