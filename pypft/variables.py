"""
This module implements functions to deal with variables
"""

from util import (copy_doc, PFTError, debugDecor,
                  tostring, alltext, needEtree, getFileName, ETremoveFromList, ETgetParent,
                  ETgetSiblings, ETinsertInList, fortran2xml, ETisExecutableStmt, ETn2name)
from locality import ETgetLocalityNode, ETgetLocalityChildNodes, getLocalitiesList
from xml.etree.ElementTree import Element
import logging

@debugDecor
@needEtree
def getVarList(doc, localityPath=None):
    """
    :param doc: etree to use
    :param localityPath: restrict list to this locality (None to get
                         variables from all the localities)
    :return: a list of dictionaries. Each item is a variable. The associated
             dictionnary has the following keys:
              - as: list of array specifications, [] for a scalar, None if unknown
              - asx: same but encoded in xml, [] for a scalar, None if unknown
              - n: name of the variable as written
              - i: intent
              - t: type specification, or None if unknown
              - arg: False if variable is a not dummy argument
                     argument position otherwise
              - use: false if variable is not a module variable
                     module name otherwise
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

    if localityPath is None:
        #We search for declaration in the entire xml
        stmts = [doc]
    else:
        #We search for declaration  only in the nodes corresponding to the locality
        stmts = ETgetLocalityChildNodes(doc, localityPath)
        
    #Find dummy arguments
    dummy_args = [ETn2name(e) for stmt in stmts for e in stmt.findall('.//{*}dummy-arg-LT/{*}arg-N/{*}N')]

    result = []
    decl_stmts = [stmt for stmt in stmts
                  if stmt.tag.endswith('}T-decl-stmt') or stmt.tag.endswith('}component-decl-stmt')]
    #Loop on each declaration statement
    for decl_stmt in decl_stmts:
        t_spec = alltext(decl_stmt.find('.//{*}_T-spec_'))
        i_spec = decl_stmt.find('.//{*}intent-spec')
        if i_spec is not None: i_spec = i_spec.text

        #Dimensions declared with the DIMENSION attribute
        array_specs = decl_stmt.findall('.//{*}attribute//{*}array-spec//{*}shape-spec')
        as0_list, asx0_list = decode_array_specs(array_specs)
        
        #Loop on each declared variables
        en_decls = decl_stmt.findall('.//{*}EN-decl')
        for en_decl in en_decls:
            n = ETn2name(en_decl.find('.//{*}N'))
            #Dimensions declared after the variable name
            array_specs = en_decl.findall('.//{*}array-spec//{*}shape-spec')
            as_list, asx_list = decode_array_specs(array_specs)

            result.append({'as': as_list if len(as0_list) == 0 else as0_list,
                           'asx': asx_list if len(asx0_list) == 0 else asx0_list,
                           'n': n, 'i': i_spec, 't': t_spec, 'arg': n in dummy_args,
                           'use':False})

    #Loop on each use statement
    use_stmts = [stmt for stmt in stmts if stmt.tag.endswith('}use-stmt')]
    for use_stmt in use_stmts:
        module = ETn2name(use_stmt.find('.//{*}module-N').find('.//{*}N'))
        for v in use_stmt.findall('.//{*}use-N'):
            n = ETn2name(v.find('.//{*}N'))
            result.append({'as': None, 'asx': None, 't': None, 'i': None, 'arg': False,
                           'n': n, 'use': module})

    return result

@debugDecor
def showVarList(doc, localityPath=None):
    """
    Display on stdout a nice view of all the variables
    :param doc: etree to use
    :param localityPath: restrict list to this locality (None to loop
                         over all localities)
    """
    for locality in [localityPath] if localityPath is not None else getLocalitiesList(doc):
        print('List of variables declared in {}:'.format(locality))
        for v in getVarList(doc, locality):
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
@needEtree
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
                ETremoveFromList(attr_elem,decl)

@debugDecor
@needEtree
def getImplicitNoneText(doc):
    """
    :param doc: etree to use
    :return: the "IMPLICIT NONE" text
    """
    ins = doc.findall('.//{*}implicit-none-stmt')
    return ins[0].text if len(ins) != 0 else None

@debugDecor
def checkImplicitNone(doc, mustRaise=False): 
    """
    :param doc: xml fragment to use
    :param mustRaise: True to raise
    Issue a logging.warning if the "IMPLICIT NONE" statment is missing
    If mustRaise is True, issue a logging.error instead and raise an error
    """
    if getImplicitNoneText(doc) is None:
        message = "The 'IMPLICIT NONE' statment is missing in file '{}'.".format(getFileName(doc))
        if mustRaise:
            logging.error(message)
            raise PFTError(message)
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
    for v in getVarList(doc).values():
        if v['arg'] and v['i'] is None:
          l("The dummy argument {} as no INTENT attribute, in file '{}'".format(v['n'], getFileName(doc)))
          ok = False
    if not ok and mustRaise:
        raise PFTError("There are dummy arguments without INTENT attribute in file '{}'".format(getFileName(doc)))

def _getDeclStmtTag(where):
    """
    Internal function
    :param where: a locality path
    :return: the declaration statement we can find in this locality path
    """
    if where.split('/')[-1].split(':')[0] == 'type':
        declStmt = 'component-decl-stmt'
    else:
        declStmt = 'T-decl-stmt'
    return declStmt

def _normalizeUniqVarList(varList):
    """
    Internal method to suppress duplicates in varList (list of tuples made of localyty and variable name)
    """
    return list(set([('/'.join([(k.lower() + ':' + w.upper())
                                for (k, w) in [component.split(':') for component in where.split('/')]]),
                      var.upper())
                     for (where, var) in varList]))

@debugDecor
@needEtree
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

    for where, varName in varList:
        found = False
        varName = varName.upper()
        declStmt = _getDeclStmtTag(where)
        
        previous = None
        #If where is "module:XX/sub:YY", ETgetLocalityNode returns the "program-unit" node
        #just above the subroutine declaration statement.
        #ETgetLocalityChildNodes returns all the node contained in the subroutine
        #excluding the subroutine and functions potentially included after a "contains" statement
        for node in ETgetLocalityChildNodes(doc, ETgetLocalityNode(doc, where)):
            #Checks if variable is a dummy argument
            dummy_lst = node.find('{*}dummy-arg-LT') #This is the list of the dummy arguments (function or subroutine)
            if dummy_lst is not None:
                #Loop over all dummy arguments
                for arg in dummy_lst.findall('.//{*}arg-N'):
                    if ETn2name(arg.find('.//{*}N')).upper() == varName:
                        #The variable is a dummy arg, we remove it from the list
                        ETremoveFromList(arg, dummy_lst)

            #In case the variable is declared
            if node.tag.endswith('}' + declStmt):
                #We are in a declaration statement
                decl_lst = node.find('./{*}EN-decl-LT') #list of declaration in the current statment
                for en_decl in decl_lst.findall('.//{*}EN-decl'):
                    if ETn2name(en_decl.find('.//{*}N')).upper() == varName:
                        #The argument is declared here, we suppress it from the declaration list
                        found = True
                        ETremoveFromList(en_decl, decl_lst)
                        break #cannot be declared twice, we can exit the loop
                #In case the argument was alone on the declaration statement
                if len(list(decl_lst.findall('./{*}EN-decl'))) == 0:
                    if simplify:
                        varToRemoveIfUnused = [[where, ETn2name(N)] for N in node.findall('.//{*}N')]
                    #We will delete the current node but we don't want to lose
                    #any text. So, we put the node's text in the tail of the previous node
                    if previous is not None:
                        if previous.tail is None: previous.tail = ''
                        previous.tail += node.tail
                    ETgetParent(doc, node).remove(node)
                    if simplify:
                        removeVarIfUnused(doc, varToRemoveIfUnused, excludeDummy=True, simplify=True)

            #In case the variable is a module variable
            if node.tag.endswith('}use-stmt'):
                #We are in a use statement
                use_lst = node.find('./{*}rename-LT')
                if use_lst is not None:
                    for name in use_lst.findall('.//{*}rename'):
                        if ETn2name(name.find('.//{*}N')).upper() == varName:
                            found = True
                            #The variable is declared here, we remove it from the list
                            ETremoveFromList(name, use_lst)
                            #In case the variable was alone
                            attribute = node.find('{*}module-N').tail
                            if attribute is None: attribute = ''
                            attribute = attribute.replace(' ', '').replace('\n', '').replace('&', '').upper()
                            use_lst = node.find('./{*}rename-LT')
                            if len(use_lst) == 0 and attribute[0] == ',' and attribute[1:] == 'ONLY:':
                                #If there is a 'ONLY' attribute, we suppress the use statement entirely
                                if previous is not None:
                                    if previous.tail is None: previous.tail = ''
                                    previous.tail += node.tail
                                ETgetParent(doc, node).remove(node)
                            elif len(use_lst) == 0:
                                #there is no 'ONLY' attribute
                                moduleName = ETgetSiblings(doc, use_lst, before=True, after=False)[-1]
                                previousTail = moduleName.tail
                                if previousTail is not None:
                                    moduleName.tail = previousTail.replace(',', '')
                                ETgetParent(doc, use_lst).remove(use_lst)
                            break

            #Store node for the following iteration
            previous = node
        if not found:
            if '/' in where:
                #Variable is certainly declared in the level upper
                removeVar(doc, [('/'.join(where.split('/')[:-1]), varName)], simplify=simplify)

@debugDecor
def removeVarIfUnused(doc, varList, excludeDummy=False, simplify=False):
    """
    :param doc: xml fragment to use
    :param varList: list of variables to remove if unused. Each item is a list or tuple of two elements.
                    The first one describes where the variable is declared, the second one is the name
                    of the variable. The first element is a '/'-separated path with each element
                    having the form 'module:<name of the module>', 'sub:<name of the subroutine>' or
                    'func:<name of the function>'
    :param excludeDummy: if True, dummy arguments are always kept untouched
    :param simplify: try to simplify code (if we delete a declaration statement that used a
                     variable as kind selector, and if this variable is not used else where,
                     we also delete it)
    :return: the varList without the unremovable variables
    If possible, remove the variable from declaration, and from the argument list if needed
    """
    varList = _normalizeUniqVarList(varList)

    varUsed = isVarUsed(doc, varList, dummyAreAlwaysUsed=excludeDummy)
    varListToRemove = []
    for localityPath, varName in varList:
        assert localityPath.split('/')[-1].split(':')[0] != 'type', \
          "The removeVarIfUnused cannot be used with type members"
        if not varUsed[(localityPath, varName)]:
            varListToRemove.append([localityPath, varName])
    removeVar(doc, varListToRemove, simplify=simplify)
    return varListToRemove

@debugDecor
@needEtree
def addVar(doc, varList):
    """
    :param doc: xml fragment to use
    :param varList: list of variable specification to insert in the xml code
                    a variable specification is a list of four elements:
                    - variable locality (path to module, subroutine, function or type declaration)
                    - variable name
                    - declarative statment
                    - position of the variable in the list of dummy argument,
                      None for a local variable
    """
    for (path, name, declStmt, pos) in varList:
        locNode = ETgetLocalityNode(doc, path)

        #Add variable to the argument list
        if pos is not None:
            argN = Element('f:arg-N')
            N = Element('f:N')
            n = Element('f:n')
            n.text = name
            N.append(n)
            argN.append(N)
            #search for a potential node, within the scope, with a list of dummy arguments
            argLst = [node.find('.//{*}dummy-arg-LT') for node in ETgetLocalityChildNodes(doc, locNode)]
            argLst = [node for node in argLst if node is not None]
            argLst = None if len(argLst) == 0 else argLst[0]
            if argLst is None:
               #This was a subroutine or function without dummy arguments
               locNode[0][0].tail = '(' 
               argLst = Element('f:dummy-arg-LT')
               argLst.tail = ')'
               locNode[0].insert(1, argLst)
            ETinsertInList(pos, argN, argLst)

        #Declare the variable
        #The following test is needed in case several variables are added in the argument list
        #but the declaration statement is given only once for all the variables
        if declStmt is not None and declStmt != '':
            #Declaration statement tag according to path (memeber of type declaration or not)
            declStmtTag = _getDeclStmtTag(path)

            if path.split('/')[-1].split(':')[0] == 'type':
                #Add declaration statement in type declaration
                #Statement building
                fortranSource = "MODULE MODU_{var}\nTYPE TYP_{var}\n{decl}\nEND TYPE\nEND MODULE".format(var=name, decl=declStmt)
                _, xml = fortran2xml(fortranSource)
                ds = xml.find('.//{*}' + declStmtTag)
                previousTail = ETgetSiblings(xml, ds, after=False)[-1].tail
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
                previousTail = ETgetSiblings(xml, ds, after=False)[-1].tail

                #node insertion index
                declLst = [node for node in ETgetLocalityChildNodes(doc, locNode) if node.tag.endswith('}' + declStmtTag)]
                if len(declLst) != 0:
                    #There already have declaration statements, we add the new one after them
                    index = list(locNode).index(declLst[-1]) + 1
                else:
                    #There is no declaration statement
                    stmtLst = [node for node in ETgetLocalityChildNodes(doc, locNode) if ETisExecutableStmt(node)] #list of executable nodes
                    if len(stmtLst) == 0:
                        #There is no executable statement, we insert the declaration at the end
                        index = len(locNode) - 1 #Last node is the ending node (e.g. end-subroutine-stmt)
                    else:
                        #We insert the declaration just before the first executable statement
                        index = list(locNode).index(stmtLst[-1])

                #node insertion
                if index != 0:
                    ds.tail = locNode[index - 1].tail
                    locNode[index - 1].tail = previousTail
                locNode.insert(index, ds)

@debugDecor
@needEtree
def addModuleVar(doc, moduleVarList):
    """
    :param doc: xml fragment to use
    :param moduleVarList: list of module variable specification to insert in the xml code
                          a module variable specification is a list of three elements:
                          - locality (path to module, subroutine, function or type declaration)
                          - module name
                          - variable name or None to add a USE statement without the ONLY attribute
    For example addModuleVar('sub:FOO', 'MODD_XX', 'Y') will add the following line in subroutine FOO:
    USE MODD_XX, ONLY: Y
    """
    for (path, moduleName, varName) in moduleVarList:
        locNode = ETgetLocalityNode(doc, path)

        #Statement building
        fortranSource = "SUBROUTINE FOO598756\nUSE {}".format(moduleName)
        if varName is not None:
            fortranSource += ', ONLY:{}'.format(varName)
        fortranSource += "\nEND SUBROUTINE"
        _, xml = fortran2xml(fortranSource)
        us = xml.find('.//{*}use-stmt')
        previousTail = ETgetSiblings(xml, us, after=False)[-1].tail

        #node insertion index
        useLst = [node for node in ETgetLocalityChildNodes(doc, locNode) if node.tag.endswith('}use-stmt')]
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
@needEtree
def isVarUsed(doc, varList, strictLocality=False, dummyAreAlwaysUsed=False):
    """
    :param doc: xml fragment to search for variable usage
    :param varList: list of variables to remove if unused. Each item is a list or tuple of two elements.
                    The first one describes where the variable is declared, the second one is the name
                    of the variable. The first element is a '/'-separated path with each element
                    having the form 'module:<name of the module>', 'sub:<name of the subroutine>' or
                    'func:<name of the function>'
    :param strictLocality: True to search strictly in locality
    :param dummyAreAlwaysUsed: Returns True if variable is a dummy argument
    :return: a dict whose keys are the elements of varList, and values are True when the variable is
             used, False otherwise

    If strictLocality is True, the function will search for variable usage
    only in this locality. But this feature has a limited interest.

    If strictLocality is False:
      - if localityPath is a subroutine/function in a contains section, 
        and if the variable is not declared in this locality, usages are
        searched in the module/subroutine/function upper that declared
        the variable and in all subroutines/functions in the contains section
      - if localityPath is a module/subroutine/function that has a
        contains sections, usages are searched in all subroutines/functions
        in the contains section

    To know if a variable can be removed, you must use strictLocality=False
    """
    varList = _normalizeUniqVarList(varList)

    #Computes in which localities variable must be searched
    if strictLocality:
        locsVar = [([localityPath], varName) for localityPath, varName in varList]
    else:
        #Function to determine if var is declared in this locality, with cache
        allVar = {}
        allLocalities = getLocalitiesList(doc, withNodes='dict')
        def _varInLoc(var, loc):
            #Is the variable declared in this locality
            if not loc in allVar:
                allVar[loc] = getVarList(doc, allLocalities[loc])
            return var.upper() in [v['n'].upper() for v in allVar[loc]]

        locsVar = {}
        for localityPath, varName in varList:
            loc = localityPath

            #Should we search in upper levels
            while('/' in loc and not _varInLoc(varName, loc)):
                #Declared upper, we must start the search one level upper
                loc = '/'.join(loc.split('/')[:-1])

            #We start search from here but we must include all routines in contains
            #that do not declare again the same variable name
            testLocalities = [loc] #we must search in the current locality
            for l in allLocalities.keys():
                if l.startswith(loc + '/') and \
                   l.split('/')[-1].split(':')[0] != 'type':
                    #l is a locality contained inside loc and is not a type declaration
                    if not _varInLoc(varName, l): #there is not another variable with same name declared inside
                        testLocalities.append(l) #if variable is used here, it is used
            locsVar[(localityPath, varName)] = testLocalities

    #For each locality to search, list all the variables used
    usedVar = {}
    for loc in list(set([item for sublist in locsVar.values() for item in sublist])):
        usedVar[loc] = []
        #Loop on all child in the locality
        for node in ETgetLocalityChildNodes(doc, allLocalities[loc]):
            #we don't want use statement, it could be where the variable is declared, not a usage place
            if not node.tag.endswith('}use-stmt'):
                if node.tag.endswith('}T-decl-stmt'):
                    #We don't want the part with the list of declared variables, we only want
                    #to capture variables used in the kind selector
                    Nnodes = node.findall('.//{*}_T-spec_//{*}N')
                else:
                    Nnodes = node.findall('.//{*}N')

                #We look for the variable name in these 'N' nodes.
                for N in Nnodes:
                    if dummyAreAlwaysUsed:
                        #No need to check if the variable is a dummy argument; because if it is one
                        #it will be found in the argument list of the subroutine/function and will
                        #be considered as used
                        usedVar[loc].append(ETn2name(N).upper())
                    else:
                        parPar = ETgetParent(doc, N, 2) #parent of parent
                        #We exclude dummy argument list to really check if the variable is used
                        #and do not only appear as an argument of the subroutine/function
                        if parPar is None or not parPar.tag.endswith('}dummy-arg-LT'):
                            usedVar[loc].append(ETn2name(N).upper())

    
    result = {}
    for localityPath, varName in varList:
        assert localityPath.split('/')[-1].split(':')[0] != 'type', 'We cannot check type component usage'
        result[(localityPath, varName)] = any([varName.upper() in usedVar[loc] for loc in locsVar[(localityPath, varName)]])
        
    return result

def showUnusedVar(doc, localityPath=None):
    """
    Displays on stdout a list of unued variables
    :param doc: xml fragment to search for variable usage
    :param localityPath: locality to explore (None for all)
    """
    if localityPath is None:
        localityPath = [loc for loc in getLocalitiesList(doc) if loc.split('/')[-1].split(':')[0] != 'type']
    else:
        if isinstance(localityPath, str): localityPath = [localityPath]

    varUsed = isVarUsed(doc, [(loc, v['n']) for loc in localityPath for v in getVarList(doc, loc)])
    for loc in localityPath:
        varList = [k[1].upper() for (k, v) in varUsed.items if v and k[0] == loc]
        if len(varList) != 0:
            print('Some variables declared in {} are unused:'.format(loc))
            print('  - ' + ('\n  - '.join(varList)))

@debugDecor
def removeUnusedLocalVar(doc, localityPath=None):
    """
    Displays on stdout a list of unued variables
    :param doc: xml fragment to search for variable usage
    :param localityPath: locality to explore (None for all)
    """
    if localityPath is None:
        localityPath = [loc for loc in getLocalitiesList(doc) if loc.split('/')[-1].split(':')[0] != 'type']
    else:
        if isinstance(localityPath, str): localityPath = [localityPath]

    allVar = {loc: getVarList(doc, loc) for loc in localityPath}
    varUsed = isVarUsed(doc, [(loc, v['n']) for loc in localityPath for v in allVar[loc]])
    varlist = []
    for loc in localityPath:
        varList.extend([(loc, v['n']) for v in allVar[loc]
                        if (not v['arg']) and
                           (not varUsed[(loc, v['n'])])])
    removeVar(doc, varList)


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
