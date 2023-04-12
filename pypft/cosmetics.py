"""
This module implements functions to deal with cosmetics
"""

from util import (copy_doc,
                  ETnon_code, needEtree)

@needEtree
def upperCase(doc):
    """
    :param doc: etree to use
    :return: same doc but with upper case letters for FORTRAN code
    """
    for elem in doc.iter():
        if (not ETnon_code(elem)) and  elem is not None and elem.text is not None:
            elem.text = elem.text.upper()
    return doc

@needEtree
def lowerCase(doc):
    """
    :param doc: etree to use
    :return: same doc but with lower case letters for FORTRAN code
    """
    for elem in doc.iter():
        if (not ETnon_code(elem)) and  elem is not None and elem.text is not None:
            elem.text = elem.text.lower()
    return doc


class Cosmetics():
    @copy_doc(upperCase)
    def upperCase(self):
        self._xml = upperCase(doc=self._xml)

    @copy_doc(lowerCase)
    def lowerCase(self):
        self._xml = lowerCase(doc=self._xml)
