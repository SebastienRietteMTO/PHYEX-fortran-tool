from util import (copy_doc, PFTError, debugDecor,
                  getParent, n2name)

"""
This module implements the locality stuff
"""

localityStmt = {'module':'module-stmt',
               'func': 'function-stmt',
               'sub': 'subroutine-stmt',
               'type': 'T-stmt',
               'prog': 'program-stmt',
               'interface': 'interface-stmt'}
localityConstruct = {'module':'program-unit',
                     'func': 'program-unit',
                     'sub': 'program-unit',
                     'type': 'T-construct',
                     'prog': 'program-unit',
                     'interface': 'interface-construct'}
def _localityStmt(blocType):
    """
    Internal method
    :param blocType: kind of locality
    :return: (construct, beginStmt, endStmt)
    """
    assert blocType in localityStmt.keys()
    stmt = localityStmt[blocType]
    construct = localityConstruct[blocType]
    beginStmt, endStmt = stmt, 'end-{}-stmt'.format(stmt)
    return construct, beginStmt, endStmt

@debugDecor
def isLocalityNode(node):
    """
    :param node: node to test
    :return: True if node is a locality node (construct node around a
             module, subroutine, function or type declaration)
    """
    return any([node.tag.endswith('}' + construct) for construct in localityConstruct.values()])

@debugDecor
def getLocalityNode(doc, localityPath):
    """
    :param doc: xml fragment in which the locality path must be found
    :param localityPath: locality path (see the locality concept in documentation)
    :result: the locality node corresponding to the locality path

    In case of a type declaration locality, the function returns the 'T-construct'
    node surrounding the type declaration statement.
    Otherwise, the function retruns the 'program-unit' node in which the module,
    subroutine or function is declared.
    """
    where, remainingPath = (localityPath.split('/', maxsplit=1) + [None])[0:2]
    assert len(where.split(':')) == 2, "Path element must contain a ':'"
    blocType, blocName = where.split(':') 
    blocName = blocName.upper()
    construct, beginStmt, endStmt = _localityStmt(blocType)
    if doc.tag.endswith('}object'):
        top = doc.find('./{*}file')
    else:
        top = doc
    for bloc in top.findall('./{*}' + construct + '/{*}' + beginStmt) + \
                top.findall('./{*}interface-construct/{*}' + construct + '/{*}' + beginStmt):
        if _getNodeName(bloc) == blocName:
            if remainingPath is None:
                return getParent(doc, bloc)
            else:
                return getLocalityNode(getParent(doc, bloc), remainingPath)
    raise PFTError("The locality path {path} has not been found.".format(path=where))

@debugDecor
def getLocalityChildNodes(doc, locality):
    """
    :param doc: xml fragment in which the nodes must be found
    :param locality: path or node reprensenting the locality
    :return: list of child nodes

    The function returns all the nodes corresponding to the locality.
    If the locality is a module, function or subroutine that contain
    (after a 'contains' statement) other subroutines or functions, those
    subroutines or functions are excluded from the result; but the
    result contains the 'END' statement of the module/subroutine or function.
    """
    if isinstance(locality, str):
        locality = getLocalityNode(doc, locality)
    assert len(locality) != 0, 'The locality construct is empty'
    assert locality[0].tag.endswith('-stmt'), 'The node is not a locality node'
    result = []
    breakOnCOntains = False
    for node in locality:
        if node.tag.endswith('}contains-stmt'):
            breakOnCOntains = True
            break #we are outside of the targeted bloc
        result.append(node)
    if breakOnCOntains:
        result.append(locality[-1])
    return result

@debugDecor
def getParentLocalityNode(doc, item, mustRaise=True):
    """
    :param doc: xml fragment in which parent must be searched
    :param item: item whose locality parent is to be searched
    :param mustRaise: True to raise an exception if parent is not found
    :return: the locality parent node of item
    Example: if item is a call statement, result is the program-unit node
             in which the call statement is
    """
    result = getParent(doc, item)
    while result is not None and not isLocalityNode(result):
        result = getParent(doc, result)
    if result is None and mustRaise:
        raise PFTError("The locality parent has not been found.")
    return result

def _getNodeName(node):
    """
    Internal methode to compute the name of a locality
    :param node: program-unit node
    :return: name
    """
    #If there was no interface bloc, code could be n2name(node[0].find('.//{*}N'))
    #But (as of 7 Jul 2023), interface have two nested N
    N = node.find('.//{*}N')
    if N is not None and N.find('.//{*}N') is not None:
        #As of 7 Jul 2023, this is the case for interface statements
        N = N.find('.//{*}N')
    if N is not None:
        name = n2name(N).upper()
    else:
        name = '--UNKNOWN--'
    return name

def _getNodePath(node):
    """
    Internal methode to compute a path part from a node
    :param node: program-unit node
    :return: path part (e.g. module:MODU)
    """
    stmt = node[0].tag.split('}')[1]
    name = _getNodeName(node[0])
    return {v: k for (k, v) in localityStmt.items()}[stmt] + ':' + name

@debugDecor
def getLocalityPath(doc, item, includeItself=True):
    """
    :param doc: xml fragment in which the path must be found
    :param item: item whose path must be determined
    :param includeItself: include the item if it is a locality node
    :return: the full path of the structure containing item
    """
    if includeItself and isLocalityNode(item):
        result = [_getNodePath(item)]
    else:
        result = []
    item = getParentLocalityNode(doc, item, mustRaise=False)
    while item is not None:
        result = [_getNodePath(item)] + result
        item = getParentLocalityNode(doc, item, mustRaise=False)
    return '/'.join(result)

@debugDecor
def getLocalitiesList(doc, withNodes=False):
    """
    :param doc: xml document in which localities must be found
    :param withNodes: to return nodes in addition to path
    :return: if withNodes='dict', returns a dict whose keys are the path to localities
                                  (module, subroutines, functions and type declaration)
                                  present in doc; and values are the corresponding nodes.
             if withNodes='tuple', returns a list of tuples whith two elements, the first
                                   one is the path, the second one the node.
             otherwise, returns a list of paths.
    """
    def _getRecur(doc, node, basePath=''):
        #If node is the entire doc
        if node.tag.endswith('}object'):
            node = doc.find('./{*}file')
        results = []
        for child in node:
            if any([child.tag.endswith(struct) for struct in localityConstruct.values()]):
                path = _getNodePath(child) if basePath == '' else basePath + '/' + _getNodePath(child)
                results.append((path, child))
                results.extend(_getRecur(doc, child, path))
        return results

    result = _getRecur(doc, doc)
    if withNodes == 'tuple':
        return result
    elif withNodes == 'dict':
       return {path:node for (path, node) in result}
    else:
        return [r[0] for r in result]

class Locality():
    @copy_doc(getLocalitiesList)
    def getLocalitiesList(self):
        return getLocalitiesList(doc=self._xml)

    def showLocalitiesList(self):
        """
        Shows the list of localities found in the source code
        """
        print("These localities have been found in the source code:")
        print("\n".join(['  - ' + path for path in self.getLocalitiesList()]))
