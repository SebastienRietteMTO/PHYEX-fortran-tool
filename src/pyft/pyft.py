#!/usr/bin/env python3

import os
import sys

from pyft.variables import Variables
from pyft.cosmetics import Cosmetics
from pyft.applications import Applications
from pyft.scope import Scope
from pyft.statements import Statements
from pyft.util import tostring, tofortran, fortran2xml, set_verbosity, print_infos, PYFTError

class PYFT(Variables, Cosmetics, Applications, Scope, Statements):
    DEFAULT_FXTRAN_OPTIONS = ['-construct-tag', '-no-include', '-no-cpp', '-line-length', '9999']
    MANDATORY_FXTRAN_OPTIONS = ['-construct-tag']

    def __init__(self, filename, output=None, parser=None, parserOptions=None, verbosity=None, wrapH=False):
        """
        :param filename: Input file name containing FORTRAN code
        :param output: Output file name, None to replace input file
        :param parser: path to the fxtran parser
        :param parserOptions: dictionnary holding the parser options
        :param verbosity: if not None, sets the verbosity level
        :param wrapH: if True, content of .h file is put in a .F90 file (to force
                      fxtran to recognize it as free form) inside a module (to
                      enable the reading of files containing only a code part)
        """
        if not sys.version_info >= (3, 8):
            #At least version 3.7 for ordered dictionary
            #At least verison 3.8 for namsepace wildcard (use of '{*}' in find or findall)
            raise PYFTError("pyft needs at least version 3.8 of python")
        self._filename = filename
        self._originalName = filename
        assert os.path.exists(filename), 'Input filename must exist'
        self._output = output
        self._parser = 'fxtran' if parser is None else parser
        self._parserOptions = self.DEFAULT_FXTRAN_OPTIONS if parserOptions is None else parserOptions
        for option in self.MANDATORY_FXTRAN_OPTIONS:
            if option not in self._parserOptions:
                self._parserOptions.append(option)
        self._ns, self._xml = fortran2xml(self._filename, self._parser, self._parserOptions, wrapH)
        if verbosity is not None:
            set_verbosity(verbosity)

    def close(self):
        print_infos()

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
