"""
This module implements functions to deal with variables
"""

from util import (copy_doc, PFTError,
                  tostring, alltext, needEtree, getFileName, ETremoveFromList, ETgetParent,
                  ETgetSiblings, ETinsertInList, fortran2xml, ETisExecutableStmt)
from locality import ETgetLocalityNode, ETgetLocalityChildNodes, getLocalitiesList
from xml.etree.ElementTree import Element
import logging

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
    dummy_args = [e.text for stmt in stmts for e in stmt.findall('.//{*}dummy-arg-LT/{*}arg-N/{*}N/{*}n')]

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
            n = alltext(en_decl.find('.//{*}n'))
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
        module = alltext(use_stmt.find('.//{*}module-N').find('.//{*}n'))
        for v in use_stmt.findall('.//{*}use-N'):
            n = alltext(v.find('.//{*}n'))
            result.append({'as': None, 'asx': None, 't': None, 'i': None, 'arg': False,
                           'n': n, 'use': module})

    return result

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

@needEtree
def getImplicitNoneText(doc):
    """
    :param doc: etree to use
    :return: the "IMPLICIT NONE" text
    """
    ins = doc.findall('.//{*}implicit-none-stmt')
    return ins[0].text if len(ins) != 0 else None

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

@needEtree
def removeVar(doc, varList):
    """
    :param doc: xml fragment to use
    :param varList: list of variables to remove. Each item is a list or tuple of two elements.
                    The first one describes where the variable is declared, the second one is the name
                    of the variable. The first element is a '/'-separated path with each element
                    having the form 'module:<name of the module>', 'sub:<name of the subroutine>',
                    'func:<name of the function>' or 'type:<name of the type>'
    Remove the variable from declaration, and from the argument list if needed
    """

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
                    if alltext(arg.find('.//{*}N/{*}n')).upper() == varName:
                        #The variable is a dummy arg, we remove it from the list
                        ETremoveFromList(arg, dummy_lst)

            #In case the variable is declared
            if node.tag.endswith('}' + declStmt):
                #We are in a declaration statement
                decl_lst = node.find('./{*}EN-decl-LT') #list of declaration in the current statment
                for en_decl in decl_lst:
                    if alltext(en_decl.find('.//{*}n')).upper() == varName:
                        #The argument is declared here, we suppress it from the declaration list
                        found = True
                        ETremoveFromList(en_decl, decl_lst)
                        break #cannot be declared twice, we can exit the loop
                #In case the argument was alone on the declaration statement
                if len(list(decl_lst.findall('./{*}EN-decl'))) == 0:
                    #We will delete the current node but we don't want to lose
                    #any text. So, we put the node's text in the tail of the previous node
                    if previous is not None:
                        if previous.tail is None: previous.tail = ''
                        previous.tail += node.tail
                    ETgetParent(doc, node).remove(node)

            #In case the variable is a module variable
            if node.tag.endswith('}use-stmt'):
                #We are in a use statement
                use_lst = node.find('./{*}rename-LT')
                if use_lst is not None:
                    for name in use_lst:
                        if alltext(name.find('.//{*}N/{*}n')).upper() == varName:
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
            raise PFTError("The variable {var} in {path} has not been found.".format(var=varName, path=where))

@needEtree
def addVar(doc, varList):
    """
    :param doc: xml fragment to use
    :param varList: list of variable specification to insert in the xml code
                    a variable specification is a list of three element:
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
