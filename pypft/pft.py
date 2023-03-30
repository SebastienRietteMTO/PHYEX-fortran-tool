#!/usr/bin/env python3

import os
import subprocess
import xml.etree.ElementTree as ET
from io import StringIO
import logging

from pypft.variables import Variables
from pypft.cosmetics import Cosmetics

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
        return ET.tostring(self._xml, method='xml', encoding='UTF-8').decode('UTF-8')

    @property
    def fortran(self):
        """
        Returns the FORTRAN as a string
        """
        #When fxtran encounters an UTF-8 character, it replaces it by *2* entities
        #We must first transform each of these entities to its corresponding binary value
        #(this is done by tostring), then we must consider the result as bytes
        #to decode these two bytes into UTF-8 (this is done by encode('raw_...').decode('UTF-8'))
        r = ET.tostring(self._xml, method='text', encoding='UTF-8').decode('UTF-8')
        try:
            r = r.encode('raw_unicode_escape').decode('UTF-8')
        except UnicodeDecodeError:
            logging.warning('This file certainly contains a strange character: {}'.format(self._filename))
        return r

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
    parser.add_argument('INPUT', help='FORTRAN input file')
    parser.add_argument('OUTPUT', default=None, help='FORTRAN output file', nargs='?')

    gParser = parser.add_argument_group('fxtran parser relative options')
    gParser.add_argument('--parser', default=None, type=str,
                         help='Path to the fxtran parser binary')
    gParser.add_argument('--parserOption', nargs='*', action='append',
                         help='Option to pass to fxtran, defaults' + \
                         ' to {}'.format(str(PFT.DEFAULT_FXTRAN_OPTIONS)))

    gFilename = parser.add_argument_group('Options to deal with file names')
    gFilename.add_argument('--renamefF', default=False, action='store_true',
                           help='Put file extension in upper case')
    gFilename.add_argument('--renameFf', default=False, action='store_true',
                           help='Put file extension in lower case')

    gVariables = parser.add_argument_group('Options to deal with variables')
    gVariables.add_argument('--showVariables', default=False, action='store_true',
                           help='Show the declared variables')

    gCosmetics = parser.add_argument_group('Cosmetics options')
    gCosmetics.add_argument('--upperCase', default=False, action='store_true',
                           help='Put FORTRAN code in upper case letters')
    gCosmetics.add_argument('--lowerCase', default=False, action='store_true',
                           help='Put FORTRAN code in lower case letters')

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

    #Cosmetics
    if args.upperCase: pft.upperCase()
    if args.lowerCase: pft.lowerCase()

    #Writing of the FORTRAN file
    pft.write()
