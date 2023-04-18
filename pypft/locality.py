from util import (copy_doc, needEtree, PFTError,
                  alltext, ETgetParent, ETn2name)

"""
This module implements the locality stuff
"""

localityStmt = {'module':'module-stmt',
               'func': 'function-stmt',
               'sub': 'subroutine-stmt',
               'type': 'T-stmt'}
localityConstruct = {'module':'program-unit',
                     'func': 'program-unit',
                     'sub': 'program-unit',
                     'type': 'T-construct'}
def _localityStmt(blocType):
    """
    Internal method
    :param blocType: kind of locality
    :return: (construct, beginStmt, endStmt)
    """
    assert blocType in ('module', 'sub', 'func', 'type')
    stmt = localityStmt[blocType]
    construct = localityConstruct[blocType]
    beginStmt, endStmt = stmt, 'end-{}-stmt'.format(stmt)
    return construct, beginStmt, endStmt

def ETisLocalityNode(node):
    """
    :param node: node to test
    :return: True if node is a locality node (construct node around a
             module, subroutine, function or type declaration)
    """
    return any([node.tag.endswith('}' + construct) for construct in localityConstruct.values()])

def ETgetLocalityNode(doc, localityPath):
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
    for bloc in top.findall('./{*}' + construct + '/{*}' + beginStmt):
        if ETn2name(bloc.find('.//{*}N')).upper() == blocName:
            if remainingPath is None:
                return ETgetParent(doc, bloc)
            else:
                return ETgetLocalityNode(ETgetParent(doc, bloc), remainingPath)
    raise PFTError("The locality path {path} has not been found.".format(path=where))

def ETgetLocalityChildNodes(doc, locality):
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
        locality = ETgetLocalityNode(doc, locality)
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

def ETgetParentLocalityNode(doc, item, mustRaise=True):
    """
    :param doc: xml fragment in which parent must be searched
    :param item: item whose locality parent is to be searched
    :param mustRaise: True to raise an exception if parent is not found
    :return: the locality parent node of item
    Example: if item is a call statement, result is the program-unit node
             in which the call statement is
    """
    result = ETgetParent(doc, item)
    while result is not None and not ETisLocalityNode(result):
        result = ETgetParent(doc, result)
    if result is None and mustRaise:
        raise PFTError("The locality parent has not been found.")
    return result

def ETgetLocalityPath(doc, item, includeItself=True):
    """
    :param doc: xml fragment in which the path must be found
    :param item: item whose path must be determined
    :param includeItself: include the item if it is a locality node
    :return: the full path of the structure containing item
    """
    def _getNodePath(node):
        stmt = node[0].tag.split('}')[1]
        name = ETn2name(node[0].find('.//{*}N')).upper()
        return {v: k for (k, v) in localityStmt.items()}[stmt] + ':' + name

    if includeItself and ETisLocalityNode(item):
        result = [_getNodePath(item)]
    else:
        result = []
    item = ETgetParentLocalityNode(doc, item, mustRaise=False)
    while item is not None:
        result = [_getNodePath(item)] + result
        item = ETgetParentLocalityNode(doc, item, mustRaise=False)
    return '/'.join(result)

@needEtree
def getLocalitiesList(doc):
    """
    :param doc: xml document in which localities must be found
    :return: a list of localities (module, subroutines, functions and
             type declaration)
    """
    result = []
    for stmt in localityStmt.values():
        for item in doc.findall('.//{*}' + stmt):
            result.append(ETgetLocalityPath(doc, item))
    return result

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
