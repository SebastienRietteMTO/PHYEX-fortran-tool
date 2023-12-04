"""
This module implements functions to deal with variables
"""

import xml.etree.ElementTree as ET
from pyft.util import (copy_doc, PYFTError, debugDecor,
                       tostring, alltext, getFileName, removeFromList, getParent,
                       getSiblings, insertInList, fortran2xml, isExecutable, n2name)
from pyft.expressions import createArrayBounds
from pyft.scope import getScopeNode, getScopeChildNodes, getScopesList, getScopePath
from xml.etree.ElementTree import Element
import logging

@debugDecor
def getVarList(doc, scopePath=None):
    """
    :param doc: etree to use
    :param scopePath: restrict list to this scope or scope list (None to get
                         variables from all the scopes)
    :return: a list of dictionaries. Each one is for a different variable
             and has the following keys:
              - as: list of array specifications, [] for a scalar, None if unknown
              - asx: same but encoded in xml, [] for a scalar, None if unknown
              - n: name of the variable as written
              - i: intent
              - t: type specification, or None if unknown
              - arg: False if variable is a not dummy argument
                     argument position otherwise
              - use: false if variable is not a module variable
                     module name otherwise
              - opt: true if variable is optional
              -scope: its scope
    Notes: - variables are found in modules only if the 'ONLY' attribute is used
           - array specification and type is unknown for module variables
           - function is not able to follow the 'ASSOCIATE' statements
    """
    def decode_array_specs(array_specs):
        as_list = []
        asx_list = []
        for array_spec in array_specs:
            lb = array_spec.find('.//{*}lower-bound')
            ub = array_spec.find('.//{*}upper-bound')
            as_list.append([alltext(lb) if lb is not None else None, alltext(ub) if ub is not None else None])
            asx_list.append([tostring(lb) if lb is not None else None, tostring(ub) if ub is not None else None])
        return as_list, asx_list

    if scopePath is None:
        scopePath = [loc for loc in getScopesList(doc) if loc.split('/')[-1].split(':')[0] != 'type']
    else:
        if isinstance(scopePath, (str, Element)): scopePath = [scopePath]

    result = []
    for loc in scopePath:
        #We search for declaration  only in the nodes corresponding to the scope
        stmts = getScopeChildNodes(doc, loc)
        
        #Find dummy arguments
        dummy_args = [n2name(e).upper() for stmt in stmts for e in stmt.findall('.//{*}dummy-arg-LT/{*}arg-N/{*}N')]

        decl_stmts = [stmt for stmt in stmts
                      if stmt.tag.endswith('}T-decl-stmt') or stmt.tag.endswith('}component-decl-stmt')]
        #Loop on each declaration statement
        for decl_stmt in decl_stmts:
            t_spec = alltext(decl_stmt.find('.//{*}_T-spec_'))
            i_spec = decl_stmt.find('.//{*}intent-spec')
            if i_spec is not None: i_spec = i_spec.text
            opt_spec = False
            allattributes = decl_stmt.findall('.//{*}attribute/{*}attribute-N')
            for attribute in allattributes:
                if alltext(attribute).upper() == 'OPTIONAL': opt_spec = True
            #Dimensions declared with the DIMENSION attribute
            array_specs = decl_stmt.findall('.//{*}attribute//{*}array-spec//{*}shape-spec')
            as0_list, asx0_list = decode_array_specs(array_specs)
            
            #Loop on each declared variables
            en_decls = decl_stmt.findall('.//{*}EN-decl')
            for en_decl in en_decls:
                n = n2name(en_decl.find('.//{*}N')).upper()
                #Dimensions declared after the variable name
                array_specs = en_decl.findall('.//{*}array-spec//{*}shape-spec')
                as_list, asx_list = decode_array_specs(array_specs)

                result.append({'as': as_list if len(as0_list) == 0 else as0_list,
                               'asx': asx_list if len(asx0_list) == 0 else asx0_list,
                               'n': n, 'i': i_spec, 't': t_spec, 'arg': n in dummy_args,
                               'use':False, 'opt': opt_spec, 'scope': loc})

        #Loop on each use statement
        use_stmts = [stmt for stmt in stmts if stmt.tag.endswith('}use-stmt')]
        for use_stmt in use_stmts:
            module = n2name(use_stmt.find('.//{*}module-N').find('.//{*}N'))
            for v in use_stmt.findall('.//{*}use-N'):
                n = n2name(v.find('.//{*}N'))
                result.append({'as': None, 'asx': None, 't': None, 'i': None, 'arg': False,
                               'n': n, 'use': module, 'scope': loc})

    return result

@debugDecor
def showVarList(doc, scopePath=None):
    """
    Display on stdout a nice view of all the variables
    :param doc: etree to use
    :param scopePath: restrict list to this scope (None to loop
                         over all scopes)
    """
    for scope in [scopePath] if scopePath is not None else getScopesList(doc):
        print('List of variables declared in {}:'.format(scope))
        for v in getVarList(doc, scope):
            print('  Variable {}:'.format(v['n']))
            if v['use']:
                print('    is a variable taken in the {} module'.format(v['use']))
            else:
                isscalar = len(v['as']) == 0
                if isscalar:
                    print('    is scalar')
                else:
                    print('    is of rank {}, with dimensions {}'.format(len(v['as']),
                                        ', '.join([(':'.join([('' if s is None else s)
                                                              for s in v['as'][i]]))
                                                   for i in range(len(v['as']))])))
                if v['arg']:
                    intent = 'without intent' if v['i'] is None else 'with intent {}'.format(v['i']) 
                    print('    is a dummy argument {}'.format(intent))
                else:
                    print('    is a local variable')
            print()

@debugDecor
def attachArraySpecToEntity(doc):
    """
    Find all T-decl-stmt elements that have a child element 'attribute' with attribute-N="DIMENSION" and
    move the attribute into EN-N elements
    E.g., before :
    REAL, DIMENSION(D%NIJT,D%NKT) :: ZTLK, ZRT
    INTEGER, PARAMETER, DIMENSION(1,1) :: IBUEXTRAIND=(/18, 30/)
    after :
    REAL :: ZTLK(D%NIJT,D%NKT), ZRT(D%NIJT,D%NKT)
    INTEGER, PARAMETER  :: IBUEXTRAIND(1,1)=(/18, 30/)
    Limitations : "DIMENSION" must be in upper case in attribute.text
    :param doc: etree to use
    :return: modified doc
    """
    # Find all T-decl-stmt elements that have a child element 'attribute' with attribute-N="DIMENSION"
    decls = doc.findall('.//{*}T-decl-stmt')
    
    for decl in decls:
        array_spec = decl.find('./{*}attribute[{*}attribute-N="DIMENSION"]/{*}array-spec')
        attr_elem = decl.find('./{*}attribute[{*}attribute-N="DIMENSION"]/{*}array-spec/...')
        if array_spec is not None:
            # Check if EN-decl elements don't already have an array-spec child element
            c_arrayspec=decl.findall('./{*}EN-decl-LT/{*}EN-decl/{*}array-spec')
            if len(c_arrayspec) == 0:
                n = decl.findall('./{*}EN-decl-LT/{*}EN-decl/{*}EN-N')
                # Attach the array-spec element after the EN-N element
                for elem in n:
                    elem.append(array_spec)
                # Remove the dimension and array-spec elements
                removeFromList(doc, attr_elem, decl)

@debugDecor
def addExplicitArrayBounds(doc, node=None, varList=None, scope=None):
    """
    Replace ':' by explicit arrays bounds.
    :param doc: etree to use
    :param node: xml node in which ':' must be replaced (None to replace everywhere)
    :param varList: var list or None to compute it
    :param scope: scope. If node is not None, scope can be None (and the scope will be guessed)
                                              or must correspond to the node.
                         If node is None, scope can be None to search everywhere or be defined
                                          to restrain search to this scope or list of scopes.
    """
    #List of variables
    if varList is None:
        varList = getVarList(doc)

    #Scopes (as string path)
    if scope is None:
        scopes = [getParentScopeNode(doc, node)] if node is not None else getScopesList(doc)
    else:
        scopes = [scope] if not isinstance(scope, list) else scope
    scopes = [getScopePath(doc, scope) if isinstance(scope, ET.Element) else scope
              for scope in scopes]

    for scope in scopes: #loop on scope
        #We loop on some nodes: those contained in the scope if node is None or only on the provided node
        for childNode in [node] if node is not None else getScopeChildNodes(doc, scope):
            for parent4 in childNode.findall('.//{*}section-subscript/../../../..'): #named-E
                if parent4.find('./{*}R-LT/{*}component-R') is None:
                    #Shape of type members is unknown
                    for parent in parent4.findall('.//{*}section-subscript/..'):
                        for sub in parent.findall('.//{*}section-subscript'):
                            lower_used = sub.find('./{*}lower-bound') 
                            upper_used = sub.find('./{*}upper-bound')
                            #A slice can be A(:), A(I:) or A(:I), but not A(I)
                            #With A(:) and A(:I), lower_used is None
                            #With A(I:) lower_used.tail contains a ':'
                            #With A(I) lower_used.tail  doesn't contain a ':'
                            if lower_used is None or (lower_used.tail is not None and ':' in lower_used.tail):
                                if lower_used is None or upper_used is None:
                                    #At least one array bound is implicit
                                    varDesc = findVar(doc, n2name(parent4.find('.//{*}N')), scope, varList=varList)
                                    if varDesc is not None and varDesc['t'] is not None and not 'CHAR' in varDesc['t']: #module array or character
                                        lower_decl, upper_decl = varDesc['as'][list(parent).index(sub)]
                                        if lower_decl is None: lower_decl = '1'
                                        #When a bound is explicit, we keep it, otherwise we take the declared bound
                                        lowerBound = lower_decl if lower_used is None else alltext(lower_used)
                                        upperBound = upper_decl if upper_used is None else alltext(upper_used)
                                        if upperBound is not None: #case of implicit shape
                                            lowerXml, upperXml = createArrayBounds(lowerBound, upperBound, 'ARRAY')
                                            #We remove current explicit bounds or the ':', and replace them
                                            #by the new explicit declaration
                                            for n in sub:
                                                if n.tag.split('}')[1] in ('lower-bound', 'upper-bound'):
                                                    sub.remove(n)
                                                else:
                                                    raise PYFTError("Unexpected case, tag is {}".format(n.tag.split('}')[1]))
                                            sub.text = '' # Delete the initial ':'
                                            sub.extend([lowerXml, upperXml])

@debugDecor
def getImplicitNoneText(doc, loc):
    """
    :param doc: etree to use
    :param loc: scope to search for the implicit none statement
    :return: the "IMPLICIT NONE" text
    """
    if isinstance(loc, str):
        loc = getScopeNode(doc, loc)
    for node in getScopeChildNodes(doc, loc):
        if node.tag.endswith('}implicit-none-stmt'):
            return node.text
    return None

@debugDecor
def checkImplicitNone(doc, mustRaise=False): 
    """
    :param doc: xml fragment to use
    :param mustRaise: True to raise
    Issue a logging.warning if the "IMPLICIT NONE" statment is missing
    If mustRaise is True, issue a logging.error instead and raise an error
    """
    for loc, node in getScopesList(doc, withNodes='tuple'):
        #The IMPLICIT NONE statement is inherited from the top unit, control at top unit is enough
        #apart for INTERFACE blocs
        if loc.count('/') == 0 or \
           (loc.count('/') >= 2 and loc.split('/')[-2].startswith('interface:')):
          if getImplicitNoneText(doc, node) is None:
              message = "The 'IMPLICIT NONE' statment is missing in file '{file}' for {loc}."
              message = message.format(file=getFileName(doc), loc=loc)
              if mustRaise:
                  logging.error(message)
                  raise PYFTError(message)
              else:
                  logging.warning(message)

@debugDecor
def checkIntent(doc, mustRaise=False): 
    """
    :param doc: xml fragment to use
    :param mustRaise: True to raise
    Issue a logging.warning if some "INTENT" attributes are missing
    If mustRaise is True, issue a logging.error instead and raise an error
    """
    ok = True
    l = logging.error if mustRaise else logging.warn
    for v in getVarList(doc):
        if v['arg'] and v['i'] is None:
          l("The dummy argument {} as no INTENT attribute, in file '{}'".format(v['n'], getFileName(doc)))
          ok = False
    if not ok and mustRaise:
        raise PYFTError("There are dummy arguments without INTENT attribute in file '{}'".format(getFileName(doc)))

def _getDeclStmtTag(where):
    """
    Internal function
    :param where: a scope path
    :return: the declaration statement we can find in this scope path
    """
    if where.split('/')[-1].split(':')[0] == 'type':
        declStmt = 'component-decl-stmt'
    else:
        declStmt = 'T-decl-stmt'
    return declStmt

def _normalizeScope(scope):
    """
    Internal method to normalize a scope
    """
    return '/'.join([(k.lower() + ':' + w.upper())
                     for (k, w) in [component.split(':') for component in scope.split('/')]])

def _normalizeVarList(varList):
    """
    Internal method to normalize a varList (list of tuples made of scope and variable name)
    """
    return [(_normalizeScope(where), var.upper()) for (where, var) in varList]

def _normalizeUniqVarList(varList):
    """
    Internal method to suppress duplicates in varList (list of tuples made of scope and variable name)
    """
    return list(set(_normalizeVarList(varList)))

@debugDecor
def removeVar(doc, varList, simplify=False):
    """
    :param doc: xml fragment to use
    :param varList: list of variables to remove. Each item is a list or tuple of two elements.
                    The first one describes where the variable is declared, the second one is the name
                    of the variable. The first element is a '/'-separated path with each element
                    having the form 'module:<name of the module>', 'sub:<name of the subroutine>',
                    'func:<name of the function>' or 'type:<name of the type>'
    :param simplify: try to simplify code (if we delete a declaration statement that used a
                     variable as kind selector, and if this variable is not used else where,
                     we also delete it)
    Remove the variable from declaration, and from the argument list if needed
    """
    varList = _normalizeUniqVarList(varList)

    #Sort scopes by depth
    sortedVarList = {}
    for where, varName in varList:
        nb = where.count('/')
        sortedVarList[nb] = sortedVarList.get(nb, []) + [(where, varName.upper())]

    #Not found at start up
    found = {v:False for v in varList}

    varToRemoveIfUnused = []
    #Loop on varList starting by inner most variables
    nbList = [] if len(sortedVarList.keys()) == 0 else range(max(sortedVarList.keys()) + 1)[::-1]
    for nb in nbList:
        sortedVarList[nb] = sortedVarList.get(nb, [])
        #Loop on scopes
        for where in list(set([where for where, _ in sortedVarList[nb]])):
            #Variables searched in this scope
            varNames = list(set([v for (w, v) in sortedVarList[nb] if w == where]))
            declStmt = _getDeclStmtTag(where)
            #If where is "module:XX/sub:YY", getScopeNode returns the "program-unit" node
            #just above the subroutine declaration statement.
            #getScopeChildNodes returns all the node contained in the subroutine
            #excluding the subroutine and functions potentially included after a "contains" statement
            previous = None
            for node in getScopeChildNodes(doc, getScopeNode(doc, where)):
                deleted = False

                #Checks if variable is a dummy argument
                dummy_lst = node.find('{*}dummy-arg-LT') #This is the list of the dummy arguments
                if dummy_lst is not None:
                    #Loop over all dummy arguments
                    for arg in dummy_lst.findall('.//{*}arg-N'):
                        name = n2name(arg.find('.//{*}N')).upper()
                        for varName in [v for v in varNames if v == name]:
                            #This dummy arg is a searched variable, we remove it from the list
                            removeFromList(doc, arg, dummy_lst)

                #In case the variable is declared
                if node.tag.endswith('}' + declStmt):
                    #We are in a declaration statement
                    decl_lst = node.find('./{*}EN-decl-LT') #list of declaration in the current statment
                    for en_decl in decl_lst.findall('.//{*}EN-decl'):
                        name = n2name(en_decl.find('.//{*}N')).upper()
                        for varName in [v for v in varNames if v == name]:
                            #The argument is declared here, we suppress it from the declaration list
                            varNames.remove(varName)
                            removeFromList(doc, en_decl, decl_lst)
                    #In all the variables are suppressed from the declaration statement
                    if len(list(decl_lst.findall('./{*}EN-decl'))) == 0:
                        if simplify:
                            varToRemoveIfUnused.extend([[where, n2name(N)] for N in node.findall('.//{*}N')])
                        #We will delete the current node but we don't want to lose
                        #any text. So, we put the node's text in the tail of the previous node
                        if previous is not None and node.tail is not None:
                            if previous.tail is None: previous.tail = ''
                            previous.tail += node.tail
                        deleted = True
                        getParent(doc, node).remove(node)

                #In case the variable is a module variable
                if node.tag.endswith('}use-stmt'):
                    #We are in a use statement
                    use_lst = node.find('./{*}rename-LT')
                    if use_lst is not None:
                        for rename in use_lst.findall('.//{*}rename'):
                            name = n2name(rename.find('.//{*}N')).upper()
                            for varName in [v for v in varNames if v == name]:
                                varNames.remove(varName)
                                #The variable is declared here, we remove it from the list
                                removeFromList(doc, rename, use_lst)
                                #In case the variable was alone
                                attribute = node.find('{*}module-N').tail
                                if attribute is None: attribute = ''
                                attribute = attribute.replace(' ', '').replace('\n', '').replace('&', '').upper()
                                use_lst = node.find('./{*}rename-LT')
                                if len(use_lst) == 0 and attribute[0] == ',' and attribute[1:] == 'ONLY:':
                                    #If there is a 'ONLY' attribute, we suppress the use statement entirely
                                    if previous is not None and node.tail is not None:
                                        if previous.tail is None: previous.tail = ''
                                        previous.tail += node.tail
                                    deleted = True
                                    getParent(doc, node).remove(node)
                                elif len(use_lst) == 0:
                                    #there is no 'ONLY' attribute
                                    moduleName = getSiblings(doc, use_lst, before=True, after=False)[-1]
                                    previousTail = moduleName.tail
                                    if previousTail is not None:
                                        moduleName.tail = previousTail.replace(',', '')
                                    getParent(doc, use_lst).remove(use_lst)
                #end loop if all variables have been found
                if len(varNames) == 0: break
                #Store node for the following iteration
                if not deleted:
                    previous = node

            #If some variables have not been found, they are certainly declared one level upper
            if len(varNames) != 0:
                 newWhere = '/'.join(where.split('/')[:-1])
                 sortedVarList[nb - 1] = sortedVarList.get(nb - 1, []) + [(newWhere, varName) for varName in varNames]

    if simplify and len(varToRemoveIfUnused) > 0:
        removeVarIfUnused(doc, varToRemoveIfUnused, excludeDummy=True, simplify=True)

@debugDecor
def removeVarIfUnused(doc, varList, excludeDummy=False, excludeModule=False, simplify=False):
    """
    :param doc: xml fragment to use
    :param varList: list of variables to remove if unused. Each item is a list or tuple of two elements.
                    The first one describes where the variable is declared, the second one is the name
                    of the variable. The first element is a '/'-separated path with each element
                    having the form 'module:<name of the module>', 'sub:<name of the subroutine>' or
                    'func:<name of the function>'
    :param excludeDummy: if True, dummy arguments are always kept untouched
    :param excludeModule: if True, module variables are always kept untouched
    :param simplify: try to simplify code (if we delete a declaration statement that used a
                     variable as kind selector, and if this variable is not used else where,
                     we also delete it)
    :return: the varList without the unremovable variables
    If possible, remove the variable from declaration, and from the argument list if needed
    """
    varList = _normalizeUniqVarList(varList)
    if excludeModule:
        varList = [v for v in varList if v[0].split('/')[-1].split(':')[0] != 'module']

    varUsed = isVarUsed(doc, varList, dummyAreAlwaysUsed=excludeDummy)
    varListToRemove = []
    for scopePath, varName in varList:
        assert scopePath.split('/')[-1].split(':')[0] != 'type', \
          "The removeVarIfUnused cannot be used with type members"
        if not varUsed[(scopePath, varName)]:
            varListToRemove.append([scopePath, varName])
    removeVar(doc, varListToRemove, simplify=simplify)
    return varListToRemove

@debugDecor
def addVar(doc, varList):
    """
    :param doc: xml fragment to use
    :param varList: list of variable specification to insert in the xml code
                    a variable specification is a list of four elements:
                    - variable scope (path to module, subroutine, function or type declaration)
                    - variable name
                    - declarative statment
                    - position of the variable in the list of dummy argument,
                      None for a local variable
    """
    for (path, name, declStmt, pos) in varList:
        locNode = getScopeNode(doc, path)

        #Add variable to the argument list
        if pos is not None:
            argN = Element('f:arg-N')
            N = Element('f:N')
            n = Element('f:n')
            n.text = name
            N.append(n)
            argN.append(N)
            #search for a potential node, within the scope, with a list of dummy arguments
            argLst = [node.find('.//{*}dummy-arg-LT') for node in getScopeChildNodes(doc, locNode)]
            argLst = [node for node in argLst if node is not None]
            argLst = None if len(argLst) == 0 else argLst[0]
            if argLst is None:
               #This was a subroutine or function without dummy arguments
               locNode[0][0].tail = '(' 
               argLst = Element('f:dummy-arg-LT')
               argLst.tail = ')'
               locNode[0].insert(1, argLst)
            insertInList(pos, argN, argLst)

        #Declare the variable
        #The following test is needed in case several variables are added in the argument list
        #but the declaration statement is given only once for all the variables
        if declStmt is not None and declStmt != '':
            #Declaration statement tag according to path (member of type declaration or not)
            declStmtTag = _getDeclStmtTag(path)

            if path.split('/')[-1].split(':')[0] == 'type':
                #Add declaration statement in type declaration
                #Statement building
                fortranSource = "MODULE MODU_{var}\nTYPE TYP_{var}\n{decl}\nEND TYPE\nEND MODULE".format(var=name, decl=declStmt)
                _, xml = fortran2xml(fortranSource)
                ds = xml.find('.//{*}' + declStmtTag)
                previousTail = getSiblings(xml, ds, after=False)[-1].tail
                #node insertion
                #locNode[0] is the T-stmt node, locNode[-1] is the end-T-stmt node
                #locNode[-2] is the last node before the end-T-stmt node (last component, comment or the T-stmt node)
                ds.tail = locNode[-2].tail
                locNode[-2].tail = previousTail
                locNode.insert(-1, ds) #insert before last one

            else:
                #Add declaration statement (not type declaration case)
                #Statement building
                fortranSource = "SUBROUTINE SUB_{var}\n{decl}\nEND SUBROUTINE".format(var=name, decl=declStmt)
                _, xml = fortran2xml(fortranSource)
                ds = xml.find('.//{*}' + declStmtTag)
                previousTail = getSiblings(xml, ds, after=False)[-1].tail

                #node insertion index
                declLst = [node for node in getScopeChildNodes(doc, locNode) if node.tag.endswith('}' + declStmtTag)]
                if len(declLst) != 0:
                    #There already have declaration statements, we add the new one after them
                    index = list(locNode).index(declLst[-1]) + 1
                else:
                    #There is no declaration statement
                    stmtLst = [node for node in getScopeChildNodes(doc, locNode) if isExecutable(node)] #list of executable nodes
                    if len(stmtLst) == 0:
                        #There is no executable statement, we insert the declaration at the end
                        index = len(locNode) - 1 #Last node is the ending node (e.g. end-subroutine-stmt)
                    else:
                        #We insert the declaration just before the first executable statement
                        index = list(locNode).index(stmtLst[0])

                #node insertion
                if index != 0:
                    ds.tail = locNode[index - 1].tail
                    locNode[index - 1].tail = previousTail
                locNode.insert(index, ds)

@debugDecor
def addModuleVar(doc, moduleVarList):
    """
    :param doc: xml fragment to use
    :param moduleVarList: list of module variable specification to insert in the xml code
                          a module variable specification is a list of three elements:
                          - scope (path to module, subroutine, function or type declaration)
                          - module name
                          - variable name or None to add a USE statement without the ONLY attribute
    For example addModuleVar('sub:FOO', 'MODD_XX', 'Y') will add the following line in subroutine FOO:
    USE MODD_XX, ONLY: Y
    """
    for (path, moduleName, varName) in moduleVarList:
        locNode = getScopeNode(doc, path)

        #Statement building
        fortranSource = "SUBROUTINE FOO598756\nUSE {}".format(moduleName)
        if varName is not None:
            fortranSource += ', ONLY:{}'.format(varName)
        fortranSource += "\nEND SUBROUTINE"
        _, xml = fortran2xml(fortranSource)
        us = xml.find('.//{*}use-stmt')
        previousTail = getSiblings(xml, us, after=False)[-1].tail

        #node insertion index
        useLst = [node for node in getScopeChildNodes(doc, locNode) if node.tag.endswith('}use-stmt')]
        if len(useLst) != 0:
            #There already have use statements, we add the new one after them
            index = list(locNode).index(useLst[-1]) + 1
        else:
            #There is no use statement, we add the new node just after the first node
            index = 1

        us.tail = locNode[index - 1].tail
        locNode[index - 1].tail = previousTail
        locNode.insert(index, us)

@debugDecor
def isVarUsed(doc, varList, strictScope=False, dummyAreAlwaysUsed=False):
    """
    :param doc: xml fragment to search for variable usage
    :param varList: list of variables to remove if unused. Each item is a list or tuple of two elements.
                    The first one describes where the variable is declared, the second one is the name
                    of the variable. The first element is a '/'-separated path with each element
                    having the form 'module:<name of the module>', 'sub:<name of the subroutine>' or
                    'func:<name of the function>'
    :param strictScope: True to search strictly in scope
    :param dummyAreAlwaysUsed: Returns True if variable is a dummy argument
    :return: a dict whose keys are the elements of varList, and values are True when the variable is
             used, False otherwise

    If strictScope is True, the function will search for variable usage
    only in this scope. But this feature has a limited interest.

    If strictScope is False:
      - if scopePath is a subroutine/function in a contains section, 
        and if the variable is not declared in this scope, usages are
        searched in the module/subroutine/function upper that declared
        the variable and in all subroutines/functions in the contains section
      - if scopePath is a module/subroutine/function that has a
        contains sections, usages are searched in all subroutines/functions
        in the contains section

    To know if a variable can be removed, you must use strictScope=False
    """
    varList = _normalizeUniqVarList(varList)

    #Computes in which scopes variable must be searched
    if strictScope:
        locsVar = [([scopePath], varName) for scopePath, varName in varList]
    else:
        #Function to determine if var is declared in this scope, with cache
        allVar = {}
        allScopes = getScopesList(doc, withNodes='dict')
        def _varInLoc(var, loc):
            #Is the variable declared in this scope
            if not loc in allVar:
                allVar[loc] = getVarList(doc, allScopes[loc])
            return var.upper() in [v['n'].upper() for v in allVar[loc]]

        locsVar = {}
        for scopePath, varName in varList:
            loc = scopePath

            #Should we search in upper levels
            while('/' in loc and not _varInLoc(varName, loc)):
                #Declared upper, we must start the search one level upper
                loc = '/'.join(loc.split('/')[:-1])

            #We start search from here but we must include all routines in contains
            #that do not declare again the same variable name
            testScopes = [loc] #we must search in the current scope
            for l in allScopes.keys():
                if l.startswith(loc + '/') and \
                   l.split('/')[-1].split(':')[0] != 'type':
                    #l is a scope contained inside loc and is not a type declaration
                    if not _varInLoc(varName, l): #there is not another variable with same name declared inside
                        testScopes.append(l) #if variable is used here, it is used
            locsVar[(scopePath, varName)] = testScopes

    #For each scope to search, list all the variables used
    usedVar = {}
    for loc in list(set([item for sublist in locsVar.values() for item in sublist])):
        usedVar[loc] = []
        #Loop on all child in the scope
        for node in getScopeChildNodes(doc, allScopes[loc]):
            #we don't want use statement, it could be where the variable is declared, not a usage place
            if not node.tag.endswith('}use-stmt'):
                if node.tag.endswith('}T-decl-stmt'):
                    #We don't want the part with the list of declared variables, we only want
                    #to capture variables used in the kind selector or in the shape specification
                    Nnodes = node.findall('.//{*}_T-spec_//{*}N') + node.findall('.//{*}shape-spec//{*}N')
                else:
                    Nnodes = node.findall('.//{*}N')

                #We look for the variable name in these 'N' nodes.
                for N in Nnodes:
                    if dummyAreAlwaysUsed:
                        #No need to check if the variable is a dummy argument; because if it is one
                        #it will be found in the argument list of the subroutine/function and will
                        #be considered as used
                        usedVar[loc].append(n2name(N).upper())
                    else:
                        parPar = getParent(doc, N, 2) #parent of parent
                        #We exclude dummy argument list to really check if the variable is used
                        #and do not only appear as an argument of the subroutine/function
                        if parPar is None or not parPar.tag.endswith('}dummy-arg-LT'):
                            usedVar[loc].append(n2name(N).upper())

    
    result = {}
    for scopePath, varName in varList:
        assert scopePath.split('/')[-1].split(':')[0] != 'type', 'We cannot check type component usage'
        result[(scopePath, varName)] = any([varName.upper() in usedVar[loc] for loc in locsVar[(scopePath, varName)]])
        
    return result

def showUnusedVar(doc, scopePath=None):
    """
    Displays on stdout a list of unued variables
    :param doc: xml fragment to search for variable usage
    :param scopePath: scope to explore (None for all)
    """
    if scopePath is None:
        scopePath = [loc for loc in getScopesList(doc) if loc.split('/')[-1].split(':')[0] != 'type']
    else:
        if isinstance(scopePath, str): scopePath = [scopePath]

    varUsed = isVarUsed(doc, [(loc, v['n']) for loc in scopePath for v in getVarList(doc, loc)])
    for loc in scopePath:
        varList = [k[1].upper() for (k, v) in varUsed.items() if (not v) and k[0] == loc]
        if len(varList) != 0:
            print('Some variables declared in {} are unused:'.format(loc))
            print('  - ' + ('\n  - '.join(varList)))

@debugDecor
def removeUnusedLocalVar(doc, scopePath=None, excludeList=None, simplify=False):
    """
    Remove unused local variables (dummy and module variables are not suppressed)
    :param doc: xml fragment to search for variable usage
    :param scopePath: scope to explore (None for all)
    :param excludeList: list of variable names to exclude from removal (even if unused)
    :param simplify: try to simplify code (if we delete a declaration statement that used a
                     variable as kind selector, and if this variable is not used else where,
                     we also delete it)
    """
    if scopePath is None:
        scopePath = [loc for loc in getScopesList(doc) if loc.split('/')[-1].split(':')[0] != 'type']
    else:
        if isinstance(scopePath, str): scopePath = [scopePath]

    if excludeList is None:
        excludeList = []
    else:
        excludeList = [item.upper() for item in excludeList]

    allVar = [(loc, v['n']) for loc in scopePath
                            for v in getVarList(doc, loc) if v['n'].upper() not in excludeList]

    removeVarIfUnused(doc, allVar, excludeDummy=True, excludeModule=True, simplify=simplify)

@debugDecor
def findVar(doc, varName, currentScope, varList=None, array=None, exactScope=False):
        """
        Search for a variable in a list of declared variables
        :param doc: etree to use
        :param varName: variable name
        :param currentScope: current scope
        :param varList: list of declared variables such as returned by getVarList, None to build this list
        :param array: True to limit search to arrays, False to limit search to non array, None to return anything
        :param exactScope: True to limit search to variables declared in the currentScope
        :return: None if not found or the description of the variable

        The function is designed to return the declaration of a given variable.
        If we know that the variable is (is not) an array, the last declaration statement
        must (must not) be an array declaration. If the last declaration statement found doesn't
        correspond to what is expected, we don't return it.
        In case array is None, we return the last declaration statement without checking its kind.
        """
        if varList is None:
            varList = getVarList(doc)
        #Select all the variables declared in the current scope or upper, then select the last declared
        candidates = {v['scope']:v for v in varList
                      if v['n'].upper() == varName.upper() and \
                         (((not exactScope) and currentScope.startswith(v['scope'])) or \
                          (     exactScope  and currentScope == v['scope']         ))}
        if len(candidates) > 0:
            last = candidates[max(candidates, key=len)]
            if array is True and last.get('as', None) is not None and len(last['as']) > 0:
                return last
            elif array is False and len(last.get('as', [])) == 0:
                return last
            elif array is None:
                return last
            else:
                return None
        else:
            return None

class Variables():
    @copy_doc(getVarList)
    def getVarList(self):
        return getVarList(doc=self._xml)

    @copy_doc(showVarList)
    def showVarList(self):
        return showVarList(doc=self._xml)

    @copy_doc(attachArraySpecToEntity)
    def attachArraySpecToEntity(self):
        return attachArraySpecToEntity(doc=self._xml)

    @copy_doc(getImplicitNoneText)
    def getImplicitNoneText(self):
        return getImplicitNoneText(doc=self._xml)

    @copy_doc(checkImplicitNone)
    def checkImplicitNone(self, mustRaise=False):
        return checkImplicitNone(self._xml, mustRaise)

    @copy_doc(checkIntent)
    def checkIntent(self, mustRaise=False):
        return checkIntent(self._xml, mustRaise)

    @copy_doc(removeVar)
    def removeVar(self, *args, **kwargs):
        return removeVar(self._xml, *args, **kwargs)

    @copy_doc(addVar)
    def addVar(self, *args, **kwargs):
        return addVar(self._xml, *args, **kwargs)

    @copy_doc(addModuleVar)
    def addModuleVar(self, *args, **kwargs):
        return addModuleVar(self._xml, *args, **kwargs)

    @copy_doc(showUnusedVar)
    def showUnusedVar(self, *args, **kwargs):
        return showUnusedVar(self._xml, *args, **kwargs)

    @copy_doc(removeUnusedLocalVar)
    def removeUnusedLocalVar(self, *args, **kwargs):
        return removeUnusedLocalVar(self._xml, *args, **kwargs)

    @copy_doc(addExplicitArrayBounds)
    def addExplicitArrayBounds(self, *args, **kwargs):
        return addExplicitArrayBounds(self._xml, *args, **kwargs)
