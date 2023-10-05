"""
This module implements functions to deal with cosmetics
"""
import xml.etree.ElementTree as ET
import re
from pypft.util import (copy_doc, getParent, debugDecor, non_code, alltext)
from pypft.variables import getVarList
from pypft.scope import getScopesList, getScopePath

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
def indent(doc, indent_programunit=0, indent_branch=2, align_continuation=True):
    """
    :param doc: etree to use
    :param indent_programunit: number of space characters inside program unit
    :param indent_branch: 
    :return: same doc doc but with indentation corrected
    """
    def set_level(e, level):
        if e.tail is not None:
            e.tail = e.tail.replace('\t', '  ')
            while '\n ' in e.tail:
                e.tail = e.tail.replace('\n ', '\n')
            e.tail = e.tail.replace('\n', '\n' + ' ' * level)

    def indent_recur(elem, level):
        """
        :param elem: dom element
        :param level: current level for elem
        """
        blocs = ['file', 'program-unit', 'do-construct', 'interface-construct',
                 'if-construct', 'if-block',
                 'where-construct', 'where-block',
                 'selectcase-construct', 'selectcase-block']
        progstmt = ['subroutine-stmt', 'program-stmt', 'module-stmt', 'function-stmt',
                    'submodule-stmt', 'procedure-stmt', 'interface-stmt']
        endprogstmt = ['end-' + s for s in progstmt]
        interbranchstmt = ['else-stmt', 'else-where-stmt']
        branchstmt = ['if-then-stmt', 'do-stmt', 'where-construct-stmt'] + interbranchstmt
        endbranchstmt = ['end-if-stmt', 'end-do-stmt', 'end-where-stmt']

        currlevel = level
        laste = None
        firstnumselect = True
        for e in elem:
            #Indentation does not apply to these lines (eg SUBROUTINE statement, DO construct)
            #but apply to the lines inside
            if e.tag.split('}')[1] in progstmt:
                currlevel += indent_programunit
            elif e.tag.split('}')[1] in branchstmt:
                currlevel += indent_branch

            set_level(e, currlevel) #Add indentation *to the tail*, thus for the next line

            if elem.tag.split('}')[1] == 'selectcase-construct':
                #Structure is:
                #<selectcase-construct>
                #<selectcase-block><select-case-stmt>SELECT CASE (...)</select-case-stmt>   \n +2
                #</selectcase-block>
                #<selectcase-block><case-stmt>CASE<case-selector>(...)</case-selector></case-stmt>  \n +4
                #statement  \n +4
                #statement  \n +2
                #</selectcase-block>
                #<selectcase-block><case-stmt>CASE<case-selector>(...)</case-selector></case-stmt>  \n +4
                #statement  \n +4
                #statement  \n +0
                #<end-select-case-stmt>END SELECT</end-select-case-stmt></selectcase-block></selectcase-construct>
                if firstnumselect:
                    firstnumselect = False
                else:
                    #previous line was a CASE line, we must indent it only once
                    set_level(laste[-1], level + indent_branch)
                indent_recur(e, level + indent_branch * 2) #statements are indented twice
                if e[-1].tag.split('}')[1] == 'end-select-case-stmt':
                    set_level(e[-2], level)
                
            elif e.tag.split('}')[1] in blocs:
                #This xml tag contains other tags, we iterate on them
                if e[0].tag.split('}')[1] in interbranchstmt:
                    #Structure is <if-construct><if-block><if-then-stmt>IF...THEN</if-then-stmt>
                    #             statement (the identation of the ELSE line is in the tail of this stetement)
                    #             </if-block><if-block><else-stmt>ELSE</else-stmt>
                    #             statement
                    #             <end-if-stmt>ENDIF</end-if-stmt></if-block></if-construct>
                    set_level(laste[-1], level)
                indent_recur(e, currlevel)

            #This line contains the end statement, we must remove the indentation contained
            #in the tail of the previous item
            if e.tag.split('}')[1] in endprogstmt + endbranchstmt:
                set_level(laste, level)
            laste = e

    indent_recur(doc, 0)
    return doc

@debugDecor
def removeEmptyLines(doc):
    """
    :param doc: etree to use
    :return: same doc doc but without empty lines
    """
    for e in doc.iter():
        if e.tail is not None:
            e.tail.replace('\t', '  ')
            e.tail = re.sub(r"\n[  \n]*\n", r"\n", e.tail)
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
    Conversion is not done if 'CYLE' is found in action-stmt 
    :param doc: etree to use or parent of singleItem
    :param singleItem: single if-stmt; in case transformation is applied on one if-stmt only
    :return: modified doc
    """
    if singleItem:
        ifstmt = [singleItem]
    else:
        ifstmt = doc.findall('.//{*}if-stmt')
    for item in ifstmt:
        cycleStmt = item.findall('.//{*}cycle-stmt')
        if len(cycleStmt) == 0:
            par = getParent(doc,item)
            # Convert if-stmt to if-then-stmt and save current indentation from last sibling
            item.tag = '{http://fxtran.net/#syntax}if-then-stmt'
            if par[par[:].index(item)-1].tail: # if tail of previous sibling exists
                curr_indent = par[par[:].index(item)-1].tail.replace('\n', '')
            else: # no tail = no indentation
                curr_indent = ""
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
    locations  = getScopesList(doc,withNodes='tuple')
    locations.reverse()
    for loc in locations:
        # For all subroutines or modi_ interface (but not mode)
        if 'sub:' in loc[0] or 'MODI_' in loc[0]:
            varArray,varArrayNamesList = [], []
            scopepath = getScopePath(doc,loc[1])
            varList = getVarList(doc,scopepath)
            for var in varList:
                if var['as']:
                    varArray.append(var)
                    varArrayNamesList.append(var['n'])
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
            # Remove (:) for klon array in call-statement
            calls = loc[1].findall('.//{*}call-stmt')
            for call in calls:
                namedEs = call.findall('.//{*}named-E')
                for namedE in namedEs:
                    subs=namedE.findall('.//{*}section-subscript')
                    if '%' in alltext(namedE):
                        print("WARNING: " + alltext(namedE) + " is assumed not to be on klon dimensions, otherwise, do not use type variables")
                    else:
                        if len(subs) == 1 and alltext(subs[0]) == ':': # ':' alone
                                varName = alltext(namedE.find('.//{*}n'))
                                ind=varArrayNamesList.index(varName)
                                upperBound = str(varArray[ind]['as'][0][1])
                                if upperBound == 'D%NIJT' or upperBound == 'D%NIT' or upperBound == 'D%NJT':
                                    RLT = namedE.find('.//{*}R-LT')
                                    par = getParent(namedE,RLT)
                                    par.remove(RLT)    
class Cosmetics():
    @copy_doc(upperCase)
    def upperCase(self):
        self._xml = upperCase(doc=self._xml)

    @copy_doc(lowerCase)
    def lowerCase(self):
        self._xml = lowerCase(doc=self._xml)

    @copy_doc(indent)
    def indent(self):
        self._xml = indent(doc=self._xml)

    @copy_doc(removeEmptyLines)
    def removeEmptyLines(self):
        self._xml = removeEmptyLines(doc=self._xml)
        
    @copy_doc(reDimKlonArrayToScalar)
    def reDimKlonArrayToScalar(self, *args, **kwargs):
        return reDimKlonArrayToScalar(self._xml, *args, **kwargs)

    @copy_doc(changeIfStatementsInIfConstructs)
    def changeIfStatementsInIfConstructs(self):
        return changeIfStatementsInIfConstructs(doc=self._xml)
