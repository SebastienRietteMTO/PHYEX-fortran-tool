"""
This module implements functions to deal with cosmetics
"""
import xml.etree.ElementTree as ET
from util import (copy_doc, getParent, debugDecor,
                  non_code, alltext)

@debugDecor
def upperCase(doc):
    """
    :param doc: etree to use
    :return: same doc but with upper case letters for FORTRAN code
    """
    for elem in doc.iter():
        if (not non_code(elem)) and  elem is not None and elem.text is not None:
            elem.text = elem.text.upper()
    return doc

@debugDecor
def lowerCase(doc):
    """
    :param doc: etree to use
    :return: same doc but with lower case letters for FORTRAN code
    """
    for elem in doc.iter():
        if (not non_code(elem)) and  elem is not None and elem.text is not None:
            elem.text = elem.text.lower()
    return doc

@debugDecor
def changeIfStatementsInIfConstructs(doc,singleItem=''):
    """
    Convert if-stmt to if-then-stmt. If singleItem is not filled, conversion to all doc is performed.
    E.g., before :
    IF(A=B) print*,"C
    after :
    IF(A=B) THEN
        print*,"C
    END IF
    :param doc: etree to use or parent of singleItem
    :param singleItem: single item in case transformation is applied on one if-stmt only
    :return: modified doc
    """
    if singleItem:
        ifstmt = [singleItem]
    else:
        ifstmt = doc.findall('.//{*}if-stmt')
    for item in ifstmt:
        par = getParent(doc,item)
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

@debugDecor
def reDimKlonArrayToScalar(doc):
    """
    Remove NIJ, NI or NJ dimension to all 1D and 2D arrays : these arrays become scalar.
    To apply after applications:removeIJLoops and attachArraySpecToEntity
    Applied on computation (index loop removal) and variable declarations (dimension removal)
    :param doc: xml fragment
    """
    # Remove dimensions in variable statements
    decls = doc.findall('.//{*}T-decl-stmt/{*}EN-decl-LT/{*}EN-decl')
    for decl in decls:
        varsShape= decl.findall('.//{*}shape-spec-LT')
        for varShape in varsShape:
            n = varShape.findall('.//{*}shape-spec')
            if (len(n) == 1 and (alltext(n[0]) == 'D%NIJT' or alltext(n[0]) == 'D%NJT' or alltext(n[0]) == 'D%NIT')) \
            or (len(n) == 2 and (alltext(n[0]) == 'D%NIT' and alltext(n[1]) == 'D%NJT')):
                par = getParent(doc,varShape,level=2)
                par.remove(getParent(doc,varShape))
                break
    # Remove in index loop in computation (possible after removeIJLoops has been applied) 
    parensR = doc.findall('.//{*}parens-R')
    for el in parensR:
        n = el.findall('.//{*}n')
        if (len(n) == 1 and (alltext(n[0]) == 'JI' or alltext(n[0]) == 'JJ' or alltext(n[0]) == 'JIJ')) \
        or (len(n) == 2 and (alltext(n[0]) == 'JI' and alltext(n[1]) == 'JJ')):
            par = getParent(doc,el)
            par.remove(el)
                
                
class Cosmetics():
    @copy_doc(upperCase)
    def upperCase(self):
        self._xml = upperCase(doc=self._xml)

    @copy_doc(lowerCase)
    def lowerCase(self):
        self._xml = lowerCase(doc=self._xml)
        
    @copy_doc(reDimKlonArrayToScalar)
    def reDimKlonArrayToScalar(self, *args, **kwargs):
        return reDimKlonArrayToScalar(self._xml, *args, **kwargs)

    @copy_doc(changeIfStatementsInIfConstructs)
    def changeIfStatementsInIfConstructs(self):
        return changeIfStatementsInIfConstructs(doc=self._xml)
