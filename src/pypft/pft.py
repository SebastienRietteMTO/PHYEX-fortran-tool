#!/usr/bin/env python3

import os

from pypft.variables import Variables
from pypft.cosmetics import Cosmetics
from pypft.applications import Applications
from pypft.scope import Scope
from pypft.statements import Statements
from pypft.util import tostring, tofortran, isint, fortran2xml, set_verbosity, print_infos

class PFT(Variables, Cosmetics, Applications, Scope, Statements):
    DEFAULT_FXTRAN_OPTIONS = ['-construct-tag', '-no-cpp', '-line-length', '9999']
    MANDATORY_FXTRAN_OPTIONS = ['-construct-tag']

    def __init__(self, filename, output=None, parser=None, parserOptions=None):
        """
        :param filename: Input file name containing FORTRAN code
        :param output: Output file name, None to replace input file
        :param parser: path to the fxtran parser
        :param parserOptions: dictionnary holding the parser options
        """
        self._filename = filename
        self._originalName = filename
        assert os.path.exists(filename), 'Input filename must exist'
        self._output = output
        self._parser = 'fxtran' if parser is None else parser
        self._parserOptions = self.DEFAULT_FXTRAN_OPTIONS if parserOptions is None else parserOptions
        for option in self.MANDATORY_FXTRAN_OPTIONS:
            if option not in self._parserOptions:
                self._parserOptions.append(option)
        self._ns, self._xml = fortran2xml(self._filename, self._parser, self._parserOptions)

    @property
    def xml(self):
        """
        Returns the xml as a string
        """
        return tostring(self._xml)

    @property
    def fortran(self):
        """
        Returns the FORTRAN as a string
        """
        return tofortran(self._xml)

    def renameUpper(self):
        """
        The output file will have an upper case extension
        """
        self._rename(str.upper)

    def renameLower(self):
        """
        The output file will have a lower case extension
        """
        self._rename(str.lower)

    def _rename(self, mod):
        """
        The output file will have a modified extension.
        :param mod: function to apply to the file extension
        """
        def _trans_ext(path, mod):
            p, e = os.path.splitext(path)
            return p + mod(e)
        if self._output is None:
            self._filename = _trans_ext(self._filename, mod)
        else:
            self._output = _trans_ext(self._output, mod)

    def write(self):
        """
        Writes the output FORTRAN file
        """
        with open(self._filename if self._output is None else self._output, 'w') as f:
            f.write(self.fortran)
        if self._output is None and self._filename != self._originalName:
            #We must perform an in-place update of the file, but the output file
            #name has been updated. Then, we must remove the original file.
            os.unlink(self._originalName)

    def writeXML(self, filename):
        """
        Writes the output XML file
        :param filename: XML output file name
        """
        with open(filename, 'w') as f:
           f.write(self.xml)

if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser(description='PHYEX FORTRAN tool')

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
                              ' to {}'.format(str(PFT.DEFAULT_FXTRAN_OPTIONS)))

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
    
    #Applications
    gApplications = parser.add_argument_group('Options to apply upper level transformation')
    gApplications.add_argument('--deleteDrHook', default=False, action='store_true',
                               help='Delete DR HOOK use')
    gApplications.add_argument('--deleteBudgetDDH', default=False, action='store_true',
                               help='Delete Budget/DDH use')
    gApplications.add_argument('--deleteNonColumnCalls', default=False, action='store_true',
                               help='Delete call to routines that needs information on horizontal points (multiple column dependency')
    gApplications.add_argument('--expandDoLoops', default=False, action='store_true',
                               help='Expand array syntax into explicit DO loops' + \
                               'apply changeIfStatementsInIfConstructs as well')
    gApplications.add_argument('--expandWhere', default=False, action='store_true',
                               help='Expand where into explicit DO loops')
    gApplications.add_argument('--removeIJLoops', default=False, action='store_true',
                               help='Remove DO loops on I and J dimensions (1,KLON)')
    gApplications.add_argument('--expandAllArrays', default=False, action='store_true',
                               help='Expand all array syntax (computing and where block)' + \
                               'apply changeIfStatementsInIfConstructs as well')
    gApplications.add_argument('--inlineContainedSubroutines', default=False, action='store_true',
                               help='Inline containted subroutines in main routine')
    gApplications.add_argument('--addStack', nargs=2, action='append',metavar=('TYPE', 'MODEL'),
                               help='Add local arrays to the stack')
    gApplications.add_argument('--addIncludes', default=False, action='store_true',
                               help='Add .h includes in the file and remove the INCLUDE statement')  
    gApplications.add_argument('--applyCPP', default=False, action='store_true',
                               help='Apply ifdef key')
    gApplications.add_argument('--checkStackArginCall', default=False, action='store_true',
                               help='Check in all CALL statements if YLSTACK must be present')
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

    args = parser.parse_args()
    simplify = {'simplify': args.simplify}
    set_verbosity(args.logLevel)

    #Opening and reading of the FORTRAN file
    if args.parserOption is None:
        parserOptions = None
    else:
        parserOptions = [el for elements in args.parserOption for el in elements]
    pft = PFT(args.INPUT, args.OUTPUT, parser=args.parser, parserOptions=parserOptions)

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
    if args.applyCPP: pft.applyCPP()
    if args.deleteDrHook: pft.deleteDrHook(**simplify)
    if args.deleteBudgetDDH: pft.deleteBudgetDDH(**simplify)
    if args.deleteNonColumnCalls: pft.deleteNonColumnCalls(**simplify)
    if args.inlineContainedSubroutines: pft.inlineContainedSubroutines()
    if args.expandDoLoops: pft.removeArraySyntax(expandDoLoops = True)
    if args.expandWhere: pft.removeArraySyntax(expandWhere = True)
    if args.expandAllArrays: pft.removeArraySyntax(expandDoLoops = True, expandWhere = True)
    if args.removeIJLoops: pft.removeIJLoops()

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

    #Writing
    if args.xml is not None: pft.writeXML(args.xml)
    if not args.dryRun:
        pft.write()

    #Infos
    print_infos()
