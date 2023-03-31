#!/usr/bin/env python3

import os
import subprocess
import xml.etree.ElementTree as ET
from io import StringIO
import logging

from pypft.variables import Variables
from pypft.cosmetics import Cosmetics
from pypft.util import tostring, tofortran

class PFT(Variables, Cosmetics):
    DEFAULT_FXTRAN_OPTIONS = ['-construct-tag', '-no-include', '-line-length', '9999']

    def __init__(self, filename, output=None, parser=None, parserOptions=None):
        """
        :param filename: Input file name containing FORTRAN code
        :param output: Output file name, None to replace input file
        :param fxtran: path to the fxtran parser
        """
        self._filename = filename
        self._originalName = filename
        assert os.path.exists(filename), 'Input filename must exist'
        self._output = output
        self._parser = 'fxtran' if parser is None else parser
        self._parserOptions = self.DEFAULT_FXTRAN_OPTIONS if parserOptions is None else parserOptions
        self._F2xml()

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

    def _F2xml(self):
        """
        :param filename: Input file name containing FORTRAN code
        :return: xml as an ElementTree
        """
        #Namespace registration
        self._ns = {'f': 'http://fxtran.net/#syntax'}
        ET.register_namespace('f', 'http://fxtran.net/#syntax')
        #Alternatively, we could load a first time to find out the namespaces, then reload
        #it after having registered the right namespace. The folowing code snippet
        #allows to capture the declared namespaces.
        #ns = dict([node for _, node in ET.iterparse(StringIO(self.xml), events=['start-ns'])])

        self._xml = subprocess.run([self._parser, self._filename,
                                    '-o', '-'] + self._parserOptions,
                                   stdout=subprocess.PIPE, check=True,
                                   encoding='UTF-8').stdout
        self._xml = ET.fromstring(self._xml, parser=ET.XMLParser(encoding='UTF-8'))

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
    gVariables.add_argument('--removeVariable', nargs='*', action='append',
                         help='Variable name to remove from declaration')

    #Cosmetics
    gCosmetics = parser.add_argument_group('Cosmetics options')
    gCosmetics.add_argument('--upperCase', default=False, action='store_true',
                           help='Put FORTRAN code in upper case letters')
    gCosmetics.add_argument('--lowerCase', default=False, action='store_true',
                           help='Put FORTRAN code in lower case letters')

    #Checks
    gChecks = parser.add_argument_group('Check options')
    gChecks.add_argument('--checkIMPLICIT', choices={'Warn', 'Err'}, default=None,
                         help='Send a warning or raise an error if the "IMPLICIT NONE" ' + \
                              'is missing')
    gChecks.add_argument('--checkINTENT', choices={'Warn', 'Err'}, default=None,
                         help='Send a warning or raise an error if the "INTENT" ' + \
                              'attribute is missing for a dummy argument')

    args = parser.parse_args()

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
    rvar = [el for elements in args.removeVariable for el in elements]
    if len(rvar) != 0: pft.removeVar(rvar)

    #Cosmetics
    if args.upperCase: pft.upperCase()
    if args.lowerCase: pft.lowerCase()

    #Checks
    if args.checkIMPLICIT is not None: pft.checkImplicitNone(args.checkIMPLICIT == 'Err')
    if args.checkINTENT is not None: pft.checkIntent(args.checkINTENT == 'Err')

    #Writing
    if args.xml is not None: pft.writeXML(args.xml)
    if not args.dryRun:
        pft.write()
