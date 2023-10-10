"""
This module implements functions to deal with cosmetics
"""
import xml.etree.ElementTree as ET
import re
from pypft.util import (copy_doc, getParent, getSiblings, debugDecor, non_code, alltext)
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
def indent(doc, indent_programunit=0, indent_branch=2, excl_directives=None):
    """
    :param doc: etree to use
    :param indent_programunit: number of space characters inside program unit
    :param indent_branch: number of space characters fr other branches (do, if...)
    :param excl_directives: some lines are directives and must stay unindented. The cpp
                            directives are automatically recognized by fxtran but others
                            appear as FORTRAN comments and must be indentified here. This
                            option can take the following values:
                             - None: to recognize as directives the lines begining
                                     with '!$OMP' (default)
                             - []: to suppress the exclusion
                             - [...]: to give another list of line beginings to consider
    :return: same doc doc but with indentation corrected
    """

    if excl_directives is None:
        excl_directives = ['!$OMP']
    def set_level(e, level, n):
        """
        :param e: element whose tail must be modifies
        :param level: level of indentation
        :param n: next element
        """
        if e.tail is not None:
            e.tail = e.tail.replace('\t', '  ')
            excl = n is not None and \
                   (n.tag.split('}')[1] == 'cpp' or \
                    n.tag.split('}')[1] == 'C' and any([n.text.startswith(d) for d in excl_directives]))
            if not excl:
                e.tail = re.sub('\n[ ]*', '\n' + ' ' * level, e.tail)

    def indent_recur(elem, level, in_construct):
        """
        :param elem: dom element
        :param level: current level for elem
        :param in_construct: True if we are inside a construct
        """
        blocs = ['file', 'program-unit', 'if-block', 'where-block', 'selectcase-block']
        progstmt = ['subroutine-stmt', 'program-stmt', 'module-stmt', 'function-stmt',
                    'submodule-stmt', 'procedure-stmt', 'interface-stmt']
        endprogstmt = ['end-' + s for s in progstmt]
        interbranchstmt = ['else-stmt', 'else-where-stmt']
        branchstmt = ['if-then-stmt', 'where-construct-stmt'] + interbranchstmt
        endbranchstmt = ['end-if-stmt', 'end-where-stmt']

        currlevel = level
        laste = None
        firstnumselect = True
        for ie, e in enumerate(elem):
            #Indentation does not apply to these lines (eg SUBROUTINE statement, DO construct)
            #but apply to the lines inside
            if e.tag.split('}')[1] in progstmt:
                currlevel += indent_programunit
            elif e.tag.split('}')[1] in branchstmt + [in_construct + '-stmt']:
                currlevel += indent_branch

            #Add indentation *to the tail*, thus for the next line
            set_level(e, currlevel, elem[ie + 1] if ie + 1 < len(elem) else None)

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
                    set_level(laste[-1], level + indent_branch, e)
                indent_recur(e, level + indent_branch * 2, "") #statements are indented twice
                if e[-1].tag.split('}')[1] == 'end-select-case-stmt':
                    set_level(e[-2], level, e[-1])
                
            elif e.tag.split('}')[1] in blocs or e.tag.split('}')[1].endswith('-construct'):
                #This xml tag contains other tags, we iterate on them
                if e[0].tag.split('}')[1] in interbranchstmt:
                    #Structure is <if-construct><if-block><if-then-stmt>IF...THEN</if-then-stmt>
                    #             statement (the identation of the ELSE line is in the tail of this stetement)
                    #             </if-block><if-block><else-stmt>ELSE</else-stmt>
                    #             statement
                    #             <end-if-stmt>ENDIF</end-if-stmt></if-block></if-construct>
                    set_level(laste[-1], level, e)
                construct = e.tag.split('}')[1][:-10] if e.tag.split('}')[1].endswith('-construct') else ""
                indent_recur(e, currlevel, construct)

            #This line contains the end statement, we must remove the indentation contained
            #in the tail of the previous item
            if e.tag.split('}')[1] in endprogstmt + endbranchstmt + ['end-' + in_construct + '-stmt']:
                set_level(laste, level, e)
            laste = e

    indent_recur(doc, 0, "")
    return doc

@debugDecor
def removeEmptyLines(doc):
    """
    :param doc: etree to use
    :return: same doc doc but without empty lines
    """
    e = doc.find('{*}file')
    if e.text is not None:
        e.text = e.text.replace('\n', '')
    for e in doc.iter():
        if e.tail is not None and '\n' in e.tail:
            e.tail = e.tail.replace('\t', '  ')
            e.tail = re.sub(r"\n[  \n]*\n", r"\n", e.tail)
    return doc

@debugDecor
def removeComments(doc, excl_directives=None):
    """
    :param doc: etree to use
    :return: same doc doc but without the comments
    :param excl_directives: some lines are directives and must stay unindented. The cpp
                            directives are automatically recognized by fxtran but others
                            appear as FORTRAN comments and must be indentified here. This
                            option can take the following values:
                             - None: to recognize as directives the lines begining
                                     with '!$OMP' or '!$mnh' (default)
                             - []: to suppress the exclusion
                             - [...]: to give another list of line beginings to consider
    """
    if excl_directives is None:
        excl_directives = ['!$OMP', '!$mnh']

    def recur(elem):
        tail_upper = None
        for ie in range(len(elem))[::-1]: #Loop from the end to the begining
            e = elem[ie]
            if e.tag.split('}')[1] == 'C' and not any([e.text.startswith(d) for d in excl_directives]):
                #Don't loose the tail (containing new line character and indentation)
                if ie != 0:
                    #It exists an element before, we add the current tail to this previsous element
                    if elem[ie - 1].tail is None:
                        elem[ie - 1].tail = e.tail
                    elif e.tail is not None:
                        elem[ie - 1].tail += e.tail
                else:
                    #The's no previsous element, tail is givent back the container element
                    tail_upper = e.tail
                elem.remove(e)
            if len(e) >= 1:
                tail = recur(e) #recursive call to inner elements
                if tail is not None:
                    #The first element was a comment, its tail must be added to the text attribute
                    if e.text is None:
                        e.text = tail
                    else:
                        e.text += tail
        return tail_upper

    recur(doc)
    return doc

@debugDecor
def updateContinuation(doc, align=True, removeALL=False, addBegin=True, removeBegin=False):
    """
    :param doc: etree to use
    :param align: True to align begin of continued lines
    :param removeALL: True to suppress all the continuation line characters ('&')
    :param addBegin: True to add missing continuation line characters ('&') at the begining of lines
    :param removeBegin: True to suppress continuation line characters ('&') at the begining of lines

    When suppressed, the '&' are replaced by a space character
    Comments after a '&' are lost
    """

    assert not (align and removeALL), "We cannot remove and align at the same time"
    assert not (addBegin and (removeALL or removeBegin)), \
           "We cannot remove ans add, at the same time, continuation characters"

    def recur_reverse(elem, tail):
        tail_upper = None
        for ie in range(len(elem))[::-1]: #Loop from the end to the begining
            e = elem[ie]
            if e.tag.split('}')[1] == 'cnt':
                #Search for comments after the cnt node
                commentsAfter = []
                j = ie + 1
                while j < len(elem) and elem[j].tag.split('}')[1] == 'C':
                    commentsAfter.append(elem[j])
                    j += 1
                nextNode = elem[j] if j < len(elem) else None

                #Is it a '&' at the end of a line (or at the begining)?
                isend = (e.tail is not None and '\n' in e.tail) or len(commentsAfter) > 0

                #Add missing continuation character at the begining of line
                if isend and addBegin:
                    if e.tail is not None and \
                       e.tail.replace('\n', '').replace('\t', '').lstrip(' ') != '':
                        #tail contains text, probably an endding ')', after a carriage return
                        #Thus, there is no '&' to begin line
                        new = ET.Element('{http://fxtran.net/#syntax}cnt')
                        new.text = '&'
                        i = 0
                        while e.tail[i] in (' ', '\n', '\t'): i += 1
                        new.tail = ' ' + e.tail[i:]
                        e.tail = e.tail[:i]
                        elem.insert(ie + 1, new)
                    elif nextNode.tag.split('}')[1] != 'cnt':
                        #There is no '&' to begin next line
                        new = ET.Element('{http://fxtran.net/#syntax}cnt')
                        new.text = '&'
                        new.tail = ' '
                        elem.insert(ie + 1, new)

                #Suppression
                if removeALL or (removeBegin and not isend):
                    for c in commentsAfter[::-1]:
                        elem.remove(c)
                    elem.remove(e) #OK because we loop in reverse order
                    if ie != 0:
                        if elem[ie - 1].tail is None:
                            elem[ie - 1].tail = " "
                        else:
                            elem[ie - 1].tail += " "
                    else:
                        tail_upper = " "

            #Recursively enter blocs
            if len(e) >= 1:
                tail = recur_reverse(e, indent)
                if tail is not None:
                    if e.text is None:
                        e.text = tail
                    else:
                        e.text += tail

    def recur_direct(elem, ct, in_cnt):
        """
        :param ct: current text
        :param in_cnt: -1 if we are not in a statement spanning several lines
                       elswhere contains the number of spaces to add
        """
        ignoreComment = False
        if align:
            for ie, e in enumerate(list(elem)):
                #It is a '&' character marking the end of the line
                isendcnt = e.tag.split('}')[1] == 'cnt' and \
                           ((e.tail is not None and '\n' in e.tail) or \
                            (ie + 1 < len(elem) and elem[ie + 1].tag.split('}')[1] == 'C'))
                ignoreComment = ignoreComment or \
                                (isendcnt and \
                                 (ie + 1 < len(elem) and elem[ie + 1].tag.split('}')[1] == 'C'))

                if isendcnt or ignoreComment:
                    #Number of spaces for alignment not already determined (first line of the continuation)
                    if isendcnt and in_cnt == -1:
                        #Search for the container statement
                        topstmt = elem
                        while not topstmt.tag.split('}')[1].endswith('-stmt'):
                            topstmt = getParent(doc, topstmt)

                        #Character to align on
                        if topstmt.tag.split('}')[1].endswith('a-stmt'):
                            l = ('=>', '=', '\(')
                        elif topstmt.tag.split('}')[1].endswith('call-stmt'):
                            l = ('\(', 'call[ ]+\w', 'call ', 'call')
                        elif topstmt.tag.split('}')[1].endswith('if-stmt'):
                            l = ('\(', '\)', 'if ', 'if')
                        elif topstmt.tag.split('}')[1].endswith('where-stmt'):
                            l = ('\(', '\)', 'where ', 'where')
                        elif topstmt.tag.split('}')[1].endswith('forall-stmt'):
                            l = ('\(', '\)', 'forall ', 'forall')
                        elif topstmt.tag.split('}')[1].endswith('namelist-stmt'):
                            l = ('/.*/', '/', 'namelist')
                        elif topstmt.tag.split('}')[1].endswith('subroutine-stmt'):
                            l = ('\(', 'subroutine[ ]+\w', 'subroutine ', 'subroutine')
                        elif topstmt.tag.split('}')[1].endswith('use-stmt'):
                            l = (':', 'use[ ]+\w', 'use ', 'use')
                        elif topstmt.tag.split('}')[1].endswith('T-decl-stmt'):
                            l = ('::', '\w,', '\w ', '\w')
                        elif topstmt.tag.split('}')[1].endswith('print-stmt'):
                            l = ('print', )
                        elif topstmt.tag.split('}')[1].endswith('write-stmt'):
                            l = ('\)', 'write[ ]*\(', 'write[ ]*', 'write')
                        elif topstmt.tag.split('}')[1].endswith('procedure-stmt'):
                            l = ('module[ ]+procedure[ ]*', 'module[ ]*', 'module')
                        else:
                            l = ('::', ':', '\(', '=>', '=', '[', ':', '/')

                        #Compute indentation value
                        in_cnt = None
                        for c in l:
                            if in_cnt is None:
                                m = re.search(c, ct, flags=re.IGNORECASE)
                                if m is not None:
                                    if ie + 1 < len(elem) and elem[ie + 1].tag.split('}')[1] != 'cnt':
                                        #If there is no continuation character at the begining, align the text with
                                        #the position after the delimiter found
                                        in_cnt = m.end()
                                    else:
                                        in_cnt = m.end() - 1
                        if in_cnt is None:
                            in_cnt = 4

                    #Align the next line
                    if e.tail is not None:
                        e.tail = re.sub('\n[ ]*', '\n' + ' ' * in_cnt, e.tail)
                    else:
                        e.tail = '\n' + ' ' * in_cnt

                if e.tag.split('}')[1] not in ('C', 'cnt'):
                    ct += (e.text if e.text is not None else '')
                    ignoreComment = False
                                
                #Recursively enter the inner blocks
                if len(e) >=1:
                    ct, in_cnt = recur_direct(e, ct, in_cnt)

                #Text after the end of block
                ct += (e.tail if e.tail is not None else '')
                if '\n' in ct:
                    ct = ct.split('\n')[-1]
                    if e.tag.split('}')[1] not in ('cnt', 'C'):
                        in_cnt = -1

        return ct, in_cnt

    recur_reverse(doc, 0)
    recur_direct(doc, "", -1)
    return doc

__NO_VALUE__ = '__NO_VALUE__'
@debugDecor
def updateSpaces(doc, before_op=1, after_op=1, in_operator=True,
                      before_comma=0, after_comma=1,
                      before_parenthesis=0, after_parenthesis=0,
                      before_affectation=1, after_affectation=1, in_affectation=True,
                      before_range_delim=0, after_range_delim=0,
                      before_use_delim=0, after_use_delim=1,
                      before_decl_delim=1, after_decl_delim=1, in_decl_delim=True, after_type_decl=1,
                      before_eq_do=0, after_eq_do=0,
                      before_eq_call=0, after_eq_call=0,
                      before_eq_init=0, after_eq_init=0,
                      before_endcnt=1, after_begincnt=1,
                      after_ifwherecase=1, before_then=1, before_ifaction=1,
                      after_progunit=1,
                      end_of_line=True, after_name=0, in_name=True,
                      before_cmdsep=0, after_cmdsep=1,
                      adjacent_keywords=__NO_VALUE__, after_keywords=__NO_VALUE__):
    """
    :param doc: etree to use
    :param before_op, after_op: number of spaces before and after operators
    :param in_operator: True to suppress spaces in operators
    :param before_comma, after_comma: number of spaces before and after commas
    :param before_parenthesis, after_parenthesis: number of spaces before and after parenthesis
    :param before_affectation, after_affectation: number of spaces before and after affectations or associations
    :param in_affectation: True to suppress spaces in affectations and in association ('= >')
    :param before_range_delim, after_range_delim: number of spaces before and after range delimiters
    :param before_use_delim, after_use_delim: number of spaces before and after use delimiters (':')
    :param before_decl_delim, after_decl_delim: number of spaces before and after declaration and enumerator
                                                delimiter ('::')
    :param in_decl_delim: True to suppress spaces in declaration and enumerator delimiter (': :')
    :param after_type_decl: number of spaces after the type in a declaration w/o '::' (e.g. 'INTEGER I');
                            also for enumerators (minimum 1)
    :param before_eq_do, after_eq_do: number of spaces before and after '=' sign in DO and
                                      FORALL statements
    :param before_eq_call, after_eq_call: number of spaces before and after '=' sign in CALL statement
    :param before_eq_init, after_eq_init: number of spaces before and after '=' sign for init values
    :param before_endcnt, after_begincnt: number of spaces before a continuation chararcter at the
                                          end of the line and after a continuation character at the
                                          begining of a line
    :param after_ifwherecase: number of spaces after the IF, ELSEIF, WHERE, ELSEWHERE, SELECTCASE,
                              CASE and FORALL keywords
    :param before_then: number of spaces before the THEN keyword
    :param before_ifaction: number of spaces between IF condition and action in one-line IF statement
                            and between FORALL specification and affectation in one-line FORALL statement
    :param after_progunit: between the program unit type (e.g. SUBROUTINE) and its name
    :param end_of_line: True to suppress spaces at the end of the line
    :param after_name: number of spaces after an indentifier, type or attribute name
    :param in_name: True to suppress spaces in identifier names
    :param before_cmdsep, after_cmdsep: number of spaces before and after command separator (';')
    :param adjacent_keywords: describes the number of spaces to introduce between adjancent keywords
                              when this is legal (the list comes from the table
                              "6.2 Adjacent keywords where separating blanks are optional" of the
                              F2008 norm and has been complemented by "end select", "implicit none"
                              and "module procedure"; for the last two, a minimum of 1 is required).
                              The allowed dictionnary keys are:
                                  - block_data
                                  - double_precision
                                  - else_if
                                  - else_where
                                  - end_associate
                                  - end_block
                                  - end_block_data
                                  - end_critical
                                  - end_do
                                  - end_enum
                                  - end_file
                                  - end_forall
                                  - end_function
                                  - end_if
                                  - end_interface
                                  - end_module
                                  - end_procedure
                                  - end_program
                                  - end_selec
                                  - end_select
                                  - end_submodule
                                  - end_subroutine
                                  - end_team
                                  - end_type
                                  - end_where
                                  - go_to
                                  - in_out
                                  - select_case
                                  - select_type
                                  - implicit_none
                                  - module_procedure
                              For example, use {'end_do':1} to write 'END DO' or
                                               {'end_do':0} to write 'ENDDO' or
                                               {'end_do':None} to not update the writting
                              or use adjacent_keywords=None to disable everything
    :param after_keywords: describes the number of spaces to introduce after keywords.
                           Some keywords need a more sophisticated treatment and are controled
                           by specific keys (e.g. CASE).
                           The keys are the keyword in lowercase, some names can be tricky to guess
                           (e.g. the key for ENDFILE is 'end-file'). By default only a few are defined.
                           Use after_keywords=None to disable everything.

    :return: same doc doc but spaces updated

    To not update spaces, put None instead of an integer and False in booleans.
    For example, to not change number of spaces after a comma, use after_comma=None

    Updates are done in the following order: 
    """

    adja_key_desc = {
      'block_data': (1, './/{*}block-data-stmt'),
      'double_precision': (1, './/{*}intrinsic-T-spec/{*}T-N'),
      'else_if': (1, './/{*}else-if-stmt'),
      'else_where': (0, './/{*}else-where-stmt'),
      'end_associate': (1, './/{*}end-associate-stmt'),
      'end_block': (1, './/{*}end-block-stmt'),
      'end_block_data': (1, './/{*}end-block-data-stmt'),
      'end_critical': (1, './/{*}end-critical-stmt'),
      'end_do': (1, './/{*}end-do-stmt'),
      'end_enum': (1, './/{*}end-enum-stmt'),
      'end_file': (1, './/{*}end-file-stmt'),
      'end_forall': (1, './/{*}end-forall-stmt'),
      'end_function': (1, './/{*}end-function-stmt'),
      'end_if': (1, './/{*}end-if-stmt'),
      'end_interface': (1, './/{*}end-interface-stmt'),
      'end_module': (1, './/{*}end-module-stmt'),
      'end_procedure': (1, './/{*}end-procedure-stmt'),
      'end_program': (1, './/{*}end-program-stmt'),
      'end_selec': (1, './/{*}end-select-case-stmt'),
      'end_select': (1, './/{*}end-select-T-stmt'),
      'end_submodule': (1, './/{*}end-submodule-stmt'),
      'end_subroutine': (1, './/{*}end-subroutine-stmt'),
      'end_team': (1, './/{*}end-change-team-stmt'),
      'end_type': (1, './/{*}end-T-stmt'),
      'end_where': (1, './/{*}end-where-stmt'),
      'go_to': (0, './/{*}goto-stmt'),
      'in_out': (0, './/{*}intent-spec'),
      'select_case': (1, './/{*}select-case-stmt'),
      'select_type': (1, './/{*}select-T-stmt'),
      'implicit_none': (1, './/{*}implicit-none-stmt'),
      'module_procedure': (1, './/{*}procedure-stmt'),
    }

    after_key = {
      'print': 0,
      'call': 1,
      'use': 1,
      'do': 1,
      'end-file': 1,
      'save': 1,
    }

    assert adjacent_keywords is None or adjacent_keywords == __NO_VALUE__ or \
           all([k in adja_key_desc.keys()
                for k in adjacent_keywords]), "Unknown key in **adjacent_keywords"

    def getval_adja(key):
        if adjacent_keywords is None:
            return None
        elif adjacent_keywords == __NO_VALUE__:
            return adja_key_desc[key][0]
        else:
            return adjacent_keywords.get(key, adja_key_desc[key][0])

    def getval_after(key):
        key = key[:-5]
        if after_keywords != __NO_VALUE__:
            num = after_keywords.get(key, after_key.get(key, None))
        else:
            num = after_key.get(key, None)
        return num

    assert after_progunit is None or after_progunit >= 1
    assert after_type_decl is None or after_type_decl >= 1
    for k in ('implicit_none', 'module_procedure'):
        num = getval_adja(k)
        assert num is None or num >= 1, \
               "adjacent_keywords['" + k + "'] must be at least 1 (is " + str(num) + ")"
    for k in ('use', 'call', 'end-file', 'do'):
        num = getval_after(k + '-stmt')
        assert num is None or num >= 1, \
               "after_keywords['" + k + "'] must be at least 1 (is " + str(num) + ")"

    for e in doc.iter():
        #security
        if e.tail is None:
            e.tail = ""
        e.tail = e.tail.replace('\t', '  ')

        #Around parenthesis
        if before_parenthesis is not None:
            e.tail = re.sub(r"[  ]*\(", " " * before_parenthesis + r"(", e.tail)
            e.tail = re.sub(r"[  ]*\)", " " * before_parenthesis + r")", e.tail)
            if e.text is not None:
                e.text = re.sub(r"[  ]*\(", " " * before_parenthesis + r"(", e.text)
                e.text = re.sub(r"[  ]*\)", " " * before_parenthesis + r")", e.text)
        if after_parenthesis is not None:
            e.tail = re.sub(r"\([  ]*", "(" + " " * after_parenthesis, e.tail)
            e.tail = re.sub(r"\)[  ]*", ")" + " " * after_parenthesis, e.tail)
            if e.text is not None:
                e.text = re.sub(r"\([  ]*", "(" + " " * after_parenthesis, e.text)
                e.text = re.sub(r"\)[  ]*", ")" + " " * after_parenthesis, e.text)

        #Around commas
        if before_comma is not None:
            e.tail = re.sub(r"[  ]*,", " " * before_comma + r",", e.tail)
            if e.text is not None:
                e.text = re.sub(r"[  ]*,", " " * before_comma + r",", e.text)
        if after_comma is not None:
            e.tail = re.sub(r",[  ]*", "," + " " * after_comma, e.tail)
            if e.text is not None:
                e.text = re.sub(r",[  ]*", "," + " " * after_comma, e.text)

        #End of line
        if end_of_line:
            e.tail = re.sub(r"[  ]*\n", r"\n", e.tail)

        #In names or around names (identifier, type, attribute)
        if e.tag.split('}')[1] in ('N', 'T-N', 'attribute-N'):
            if in_name:
                for n in e.findall('{*}n'):
                    if n.tail is not None:
                        n.tail = n.tail.strip(' ')
            if e.tail is not None and after_name is not None:
                e.tail = ' ' * after_name + e.tail.lstrip(' ')

        #Around range delimiter
        elif e.tag.split('}')[1] == 'lower-bound' and e.tail is not None and ':' in e.tail:
            if before_range_delim is not None:
                e.tail = ' ' * before_range_delim + e.tail.lstrip(' ')
            if after_range_delim is not None:
                e.tail = e.tail.rstrip(' ') + ' ' * before_range_delim

        #Around ':' in USE statements
        elif e.tag.split('}')[1] == 'module-N' and e.tail is not None and ':' in e.tail:
            if before_use_delim is not None:
                e.tail = re.sub(r"[  ]*:", " " * before_use_delim + r":", e.tail)
            if after_use_delim is not None:
                e.tail = re.sub(r":[  ]*", ":" + " " * after_use_delim, e.tail)

        #Around and in '::' in declaration statements
        #After the type in a declaration
        elif e.tag.split('}')[1] in ('attribute', '_T-spec_') and e.tail is not None:
            if in_decl_delim:
                e.tail = re.sub(r":[  ]*:", r"::", e.tail)
            if before_decl_delim is not None:
                e.tail = re.sub(r"[ ]*(:[  ]*:)", ' ' * before_decl_delim + r"\1", e.tail)
            if after_decl_delim is not None:
                e.tail = re.sub(r"(:[  ]*:)[ ]*", r"\1" + ' ' * after_decl_delim,  e.tail)
            if e.tag.split('}')[1] == '_T-spec_' and after_type_decl is not None:
                e.tail = e.tail.rstrip(' ') + ' ' * after_type_decl

        #Around and in '::' in enumerators
        #After the enumerator keyword
        elif e.tag.split('}')[1] == 'enumerator-stmt' and e.text is not None:
            if ':' in e.text:
                if in_decl_delim:
                    e.text = re.sub(r":[  ]*:", r"::", e.text)
                if before_decl_delim is not None:
                    e.text = re.sub(r"[ ]*(:[  ]*:)", ' ' * before_decl_delim + r"\1", e.text)
                if after_decl_delim is not None:
                    e.text = re.sub(r"(:[  ]*:)[ ]*", r"\1" + ' ' * after_decl_delim,  e.text)
            elif after_type_decl is not None:
                e.text = e.text.rstrip(' ') + ' ' * after_type_decl

        #Between the program unit type and its name
        elif e.tag.split('}')[1] in ('subroutine-stmt', 'program-stmt', 'module-stmt', 'function-stmt',
                                     'submodule-stmt', 'procedure-stmt', 'interface-stmt',
                                     'end-subroutine-stmt', 'end-program-stmt',
                                     'end-module-stmt', 'end-function-stmt',
                                     'end-submodule-stmt', 'end-procedure-stmt', 'end-interface-stmt') and \
             after_progunit is not None:
            if e.text is not None:
                e.text = e.text.rstrip(' ') + ' ' * after_progunit

        #Around '=' sign in DO and FORALL statements
        elif e.tag.split('}')[1] in ('do-V', 'V') and e.tail is not None and '=' in e.tail:
            if before_eq_do is not None:
                e.tail = re.sub('[ ]*=', ' ' * before_eq_do + '=', e.tail)
            if  after_eq_do is not None:
                e.tail = re.sub('=[ ]*', '=' + ' ' * before_eq_do, e.tail)

        #Around '=' sign in CALL statements
        elif e.tag.split('}')[1] == 'arg-N' and e.tail is not None and '=' in e.tail:
            if before_eq_call is not None:
                e.tail = re.sub('[ ]*=', ' ' * before_eq_call + '=', e.tail)
            if  after_eq_call is not None:
                e.tail = re.sub('=[ ]*', '=' + ' ' * before_eq_call, e.tail)

        #Around '=' sign for init values
        elif e.tag.split('}')[1] in ('EN-N', 'named-constant') and e.tail is not None and '=' in e.tail:
            if before_eq_init is not None:
                e.tail = re.sub('[ ]*=', ' ' * before_eq_init + '=', e.tail)
            if after_eq_init is not None:
                e.tail = re.sub('=[ ]*', '=' + ' ' * before_eq_init, e.tail)
        #Around the command separator ';'
        elif e.tag.split('}')[1] == 'smc':
            if before_cmdsep is not None:
                p = getSiblings(doc, e, after=False)
                if len(p) != 0 and p[-1].tail is not None:
                    p[-1].tail = ' ' * before_cmdsep + p[-1].tail.lstrip(' ')
            if after_cmdsep is not None and e.tail is not None:
                e.tail = e.tail.rstrip(' ') + ' ' * after_cmdsep

        #Around and in association operators (affectation case done after)
        elif e.tag.split('}')[1] == 'associate-N' and e.tail is not None and '=' in e.tail:
            if before_affectation is not None:
                e.tail = re.sub('[ ]*=', ' ' * before_affectation + '=', e.tail)
            if after_affectation is not None:
                e.tail = re.sub('>[ ]*', '>' + ' ' * before_affectation, e.tail)
            if in_affectation:
                e.tail = re.sub(r'=[ ]*>', '=>', e.tail)

        #After a reserved keyword
        #elif after_keywords is not None and e.tag.split('}')[1].endswith('-stmt'):
        #    num = getval_after(e.tag.split('}')[1])
        #    if num is not None and e.text is not None:
        #        e.text = e.text.rstrip(' ') + ' ' * num

    #Another loop on elements
    #All the transformations are not put in a single loop because the following one act
    #on sub-elements. Putting them all in the same loop would prevent to control in which order
    #the different transformations occur.
    #For instance, the suppression on the space after the parenthesis must be done before
    #the adding of a space before a THEN keyword
    for e in doc.iter():
        #Around and in operators
        if e.tag.split('}')[1] == 'op-E': #op are always (?) in op-E nodes
            for o in e.findall('{*}op'):
                if before_op is not None:
                    io = list(e).index(o)
                    if io != 0:
                        p = e[io - 1]
                        if p.tail is None:
                            p.tail = ' ' * before_op
                        else:
                            p.tail = p.tail.rstrip(' ') + ' ' * before_op
                if after_op is not None:
                    if o.tail is None:
                        o.tail = ' ' * after_op
                    else:
                        o.tail = o.tail.lstrip(' ') + ' ' * after_op
                if in_operator:
                    for oo in o.findall('{*}o'):
                        if oo.tail is not None:
                            oo.tail = oo.tail.strip(' ')

        #Around and in affectation operators (association case done before)
        elif e.tag.split('}')[1] in ('a-stmt', 'pointer-a-stmt'): #a are always (?) in a-stmt or pointer-a-stmt nodes
            for a in e.findall('{*}a'):
                if before_affectation is not None:
                    p = e[list(e).index(a) - 1]
                    if p.tail is None:
                        p.tail = ' ' * before_affectation
                    else:
                        p.tail = p.tail.rstrip(' ') + ' ' * before_affectation
                if after_affectation is not None:
                    if a.tail is None:
                        a.tail = ' ' * after_affectation
                    else:
                        a.tail = a.tail.lstrip(' ') + ' ' * after_affectation
                if in_affectation:
                    a.text = a.text.replace(' ', '')

        #After a IF, WHERE, ELSEIF, ELSEWHERE, SELECTCASE, CASE and FORALL keyword, and before THEN keyword
        elif e.tag.split('}')[1] in ('if-stmt', 'if-then-stmt', 'else-if-stmt',
                                     'where-construct-stmt', 'else-where-stmt',
                                     'select-case-stmt', 'case-stmt',
                                     'forall-stmt', 'forall-construct-stmt'):
            if after_ifwherecase is not None and e.text is not None:
                if e.tag.split('}')[1] == 'case-stmt':
                    #the (eventual) parenthesis is not in the text of the node
                    e.text = e.text.rstrip(' ') + ' ' * after_ifwherecase
                else:
                    e.text = re.sub('[ ]*\(', ' ' * after_ifwherecase + '(', e.text, count=1)
            if e.tag.split('}')[1] == 'if-then-stmt' and before_then is not None:
                c = e.find('{*}condition-E')
                c.tail = re.sub('\)[ ]*([a-zA-Z]*$)', ')' + ' ' * before_then + r'\1', c.tail)
            elif e.tag.split('}')[1] == 'if-stmt' and before_ifaction is not None:
                c = e.find('{*}condition-E')
                c.tail = re.sub('\)[ ]*$', ')' + ' ' * before_ifaction, c.tail)
            elif e.tag.split('}')[1] == 'forall-stmt' and before_ifaction is not None:
                s = e.find('{*}forall-triplet-spec-LT')
                s.tail = re.sub('\)[ ]*$', ')' + ' ' * before_ifaction, c.tail)

    #Direct search to prevent using the costly getParent function
    if before_endcnt is not None or after_begincnt is not None:
        for e in doc.findall('.//{*}cnt/..'): #node containing continuation characters
            for c in e.findall('{*}cnt'): #continuation characters
                ic = list(e).index(c)
                if ic == 0:
                    #the string before the continuation character is in the parent text
                    p = e
                    pstring = p.text
                else:
                    #the string before the continuation character is in previsous sibling tail
                    p = e[ic - 1]
                    pstring = p.tail
                if '\n' in pstring and '\n' in c.tail:
                    #continuation character alone on a line
                    pass
                elif '\n' in pstring and after_begincnt is not None:
                    #continuation character at the begining of a line
                    c.tail = ' ' * after_begincnt + c.tail.lstrip(' ')
                elif before_endcnt is not None:
                    #continuation character at the end of a line (eventually followed by a comment)
                    if p == e:
                        p.text = p.text.rstrip(' ') + ' ' * before_endcnt
                    else:
                        p.tail = p.tail.rstrip(' ') + ' ' * before_endcnt

    #In adjacent keywords
    for k in adja_key_desc.keys():
        num = getval_adja(k)
        if num is not None:
            for n in doc.findall(adja_key_desc[k][1]):
                lf = "[ ]*".join(["(" + p + ")" for p in k.split('_')])
                repl = (" " * num).join([r"\{i}".format(i=i + 1) for i, _ in enumerate(k.split('_'))])
                n.text = re.sub(lf, repl, n.text, flags=re.IGNORECASE)

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
    def indent(self, *args, **kwargs):
        self._xml = indent(doc=self._xml, *args, **kwargs)

    @copy_doc(removeEmptyLines)
    def removeEmptyLines(self):
        self._xml = removeEmptyLines(doc=self._xml)

    @copy_doc(removeComments)
    def removeComments(self):
        self._xml = removeComments(doc=self._xml)

    @copy_doc(updateSpaces)
    def updateSpaces(self, *args, **kwargs):
        self._xml = updateSpaces(doc=self._xml, *args, **kwargs)

    @copy_doc(updateContinuation)
    def updateContinuation(self, *args, **kwargs):
        self._xml = updateContinuation(doc=self._xml, *args, **kwargs)
        
    @copy_doc(reDimKlonArrayToScalar)
    def reDimKlonArrayToScalar(self, *args, **kwargs):
        return reDimKlonArrayToScalar(self._xml, *args, **kwargs)

    @copy_doc(changeIfStatementsInIfConstructs)
    def changeIfStatementsInIfConstructs(self):
        return changeIfStatementsInIfConstructs(doc=self._xml)
