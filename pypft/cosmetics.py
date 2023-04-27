"""
This module implements functions to deal with cosmetics
"""
import xml.etree.ElementTree as ET
from util import (copy_doc, ETgetParent, debugDecor,
                  ETnon_code, needEtree)

@debugDecor
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

@debugDecor
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

@debugDecor
def changeIfStatementsInIfConstructs(doc):
    """
    Find all if-stmt and convert it to if-then-stmt
    E.g., before :
    IF(A=B) print*,"C
    after :
    IF(A=B) THEN
        print*,"C
    END IF
    :param doc: etree to use
    :return: modified doc
    """
    ifstmt = doc.findall('.//{*}if-stmt')
    for item in ifstmt:
        par = ETgetParent(doc,item)
        # Convert if-stmt to if-then-stmt and save current indentation from last sibling
        item.tag = '{http://fxtran.net/#syntax}if-then-stmt'
        curr_indent = par[par[:].index(item)-1].tail.replace('\n', '')
        # Indentation is applied on current item.tail (for next Fortran line)
        item[0].tail += 'THEN\n' + curr_indent + '  '
        # Add end-if-stmt to the parent of the if-stmt
        endiftag = ET.Element('{http://fxtran.net/#syntax}end-if-stmt')
        endiftag.tail = '\n' + curr_indent + 'END IF'
        item.append(endiftag)
        par[par[:].index(item)].extend(endiftag)
        # Remove cnt tag if any
        for i in item.findall('./{*}cnt'):
            item.remove(i)

class Cosmetics():
    @copy_doc(upperCase)
    def upperCase(self):
        self._xml = upperCase(doc=self._xml)

    @copy_doc(lowerCase)
    def lowerCase(self):
        self._xml = lowerCase(doc=self._xml)

    @copy_doc(changeIfStatementsInIfConstructs)
    def changeIfStatementsInIfConstructs(self):
        return changeIfStatementsInIfConstructs(doc=self._xml)
