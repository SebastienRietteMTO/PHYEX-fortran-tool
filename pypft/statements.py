"""
This module includes functions to act on statements
"""

from util import copy_doc, needEtree, ETn2name, ETgetParent, ETnon_code
from locality import ETgetLocalityChildNodes, ETgetLocalityNode, getLocalitiesList, ETgetLocalityPath
from variables import removeVarIfUnused

@needEtree
def removeCall(doc, callName, localityPath, simplify=False):
    """
    :param doc: xml fragment to use
    :param callName: name of the subprogram calls to remove.
    :param localityPath: locality to explore (None for all). This is a '/'-separated path with each element
                         having the form 'module:<name of the module>', 'sub:<name of the subroutine>' or
                         'func:<name of the function>'
    :param simplify: try to simplify code (if we delete "CALL FOO(X)" and if X not used else where,
                     we also delete it; or if the call was alone inside a if-then-endif construct,
                     the construct is also removed, and variables used in the if condition are also
                     checked...)
    """
    callName = callName.upper()
    if localityPath is None:
        localityPath = [loc for loc in getLocalitiesList(doc) if loc.split('/')[-1].split(':')[0] != 'type']
    else:
        if isinstance(localityPath, str): localityPath = [localityPath]
    for loc in localityPath:
        #Loop on nodes composing the locality
        for node in ETgetLocalityChildNodes(doc, ETgetLocalityNode(doc, loc)):
            callNodes = [node] if node.tag.endswith('}call-stmt') else [] #In case node is a call statement
            callNodes += [cn for cn in node.findall('.//{*}call-stmt')] #If node is a construct with call statements
            callNodes = [cn for cn in callNodes
                         if ETn2name(cn.find('.//{*}named-E/{*}N')).upper() == callName] #filter by name
            for callNode in callNodes:
                removeStmtNode(doc, callNode, simplify, simplify)

@needEtree
def removePrints(doc, localityPath, simplify=False):
    """
    Removes all print statements
    :param doc: xml fragment to use
    :param localityPath: locality to explore (None for all). This is a '/'-separated path with each element
                         having the form 'module:<name of the module>', 'sub:<name of the subroutine>' or
                         'func:<name of the function>'
    :param simplify: try to simplify code (if we delete "print*, X" and if X is not used else where,
                     we also delete it; or if the print was alone inside a if-then-endif construct,
                     the construct is also removed, and variables used in the if condition are also
                     checked...)
    """
    if localityPath is None:
        localityPath = [loc for loc in getLocalitiesList(doc) if loc.split('/')[-1].split(':')[0] != 'type']
    else:
        if isinstance(localityPath, str): localityPath = [localityPath]
    for loc in localityPath:
        #Loop on nodes composing the locality
        for node in ETgetLocalityChildNodes(doc, ETgetLocalityNode(doc, loc)):
            printNodes = [node] if node.tag.endswith('}print-stmt') else [] #In case node is a print statement
            printNodes += [cn for cn in node.findall('.//{*}print-stmt')] #If node is a construct with print statements
            for printNode in printNodes:
                removeStmtNode(doc, printNode, simplify, simplify)

@needEtree
def removeStmtNode(doc, node, simplifyVar, simplifyStruct):
    """
    This function removes a node and:
      - suppress part of the code that must be removed at the same time (if-stmt case)
      - suppress variable that became useless (if simplifyVar is True)
      - suppress outer loop/if if useless (if simplifyStruct is True)
    :param doc: xml fragment to use
    :param node: node representing the statement to remove
    :param simplifyVar: try to simplify code (if we delete "CALL FOO(X)" and if X not used else where,
                        we also delete it; or if the call was alone inside a if-then-endif construct,
                        with simplifyStruct=True, the construct is also removed, and variables used
                        in the if condition are also checked...)
    :param simplifyStruct: try to simplify code (if we delete "CALL FOO(X)" and if the call was
                           alone inside a if-then-endif construct, the construct is also removed,
                           and variables used in the if condition (with simplifyVar=True) aro
                           also checked...)
    """
    assert node.tag.endswith('-stmt') or node.tag.endswith('-construct'), \
      "Don't know how to suppress only a part of a structure or of a statement"

    #This function removes inner statement to give a chance to identify and suppress unused variables
    #During this step, nodes are suppressed with simplifyStruct=False to prevent infinite loops
    #then the actual node is removed using _removeNode

    if node.tag.endswith('}do-construct'):
        for item in _nodesInDo(node):
            #removes inner statements
            removeStmtNode(doc, item, simplifyVar, False)
        _removeNode(doc, node, simplifyVar, simplifyStruct)
    elif node.tag.endswith('}if-construct'):
        for item in _nodesInIf(node):
            #removes inner statements
            removeStmtNode(doc, item, simplifyVar, False)
        _removeNode(doc, node, simplifyVar, simplifyStruct)
    elif node.tag.endswith('}where-construct'):
        for item in _nodesInWhere(node):
            #removes inner statements
            removeStmtNode(doc, item, simplifyVar, False)
        _removeNode(doc, node, simplifyVar, simplifyStruct)
    elif node.tag.endswith('}if-stmt'):
        _removeNode(doc, node.find('./{*}action-stmt')[0], simplifyVar, simplifyStruct)
        #We don't remove current node as it is removed automatically by _removeNode even with simplifyStruct==False
    elif node.tag.endswith('}action-stmt'):
        _removeNode(doc, node[0], simplifyVar, simplifyStruct, parent=node)
        #We don't remove current node as it is removed automatically by _removeNode even with simplifyStruct==False
    elif node.tag.endswith('}where-stmt'):
        _removeNode(doc, node.find('./{*}action-stmt')[0], simplifyVar, simplifyStruct)
        #We don't remove current node as it is removed automatically by _removeNode even with simplifyStruct==False
    else:
        #At least a-stmt, print-stmt
        _removeNode(doc, node, simplifyVar, simplifyStruct)

def _nodesInIf(ifNode):
    """
    Internal method to return nodes in if structure
    """
    nodes = []
    for block in ifNode.findall('./{*}if-block'):
        for item in [i for i in block if not (i.tag.endswith('}if-then-stmt') or \
                                              i.tag.endswith('}else-if-stmt') or \
                                              i.tag.endswith('}else-stmt') or \
                                              i.tag.endswith('}end-if-stmt'))]:
            if not ETnon_code(item): nodes.append(item)
    return nodes

def _nodesInWhere(whereNode):
    """
    Internal method to return nodes in where structure
    """
    nodes = []
    for block in whereNode.findall('./{*}where-block'):
        for item in [i for i in block if not (i.tag.endswith('}where-construct-stmt') or \
                                              i.tag.endswith('}else-where-stmt') or \
                                              i.tag.endswith('}end-where-stmt'))]:
            if not ETnon_code(item): nodes.append(item)
    return nodes

def _nodesInDo(doNode):
    """
    Internal method to return nodes in do structure
    """
    nodes = []
    for item in [i for i in doNode if not (i.tag.endswith('}do-stmt') or \
                                           i.tag.endswith('}end-do-stmt'))]:
        if not ETnon_code(item): nodes.append(item)
    return nodes

@needEtree
def _removeNode(doc, node, simplifyVar, simplifyStruct, parent=None):
    """
    This internal function actually removes nodes and (if simplify is True) try
    to suppress code parts that became useless (empty if, do...)

    The removeStmtNode function is in charge to remove the inner part of the statement to remove.
    The _removeNode removes the node itself and check if upper nodes may also be removed

    :param doc: xml fragment to use
    :param node: node to remove
    :param simplifyVar: try to simplify code (if we delete "CALL FOO(X)" and if X not used else where,
                        we also delete it; or if the call was alone inside a if-then-endif construct,
                        with simplifyStruct=True, the construct is also removed, and variables used
                        in the if condition are also checked...)
    :param simplifyStruct: try to simplify code (if we delete "CALL FOO(X)" and if the call was
                           alone inside a if-then-endif construct, the construct is also removed,
                           and variables used in the if condition (with simplifyVar=True) aro
                           also checked...)
    :param parent: parent node (if known) to save computation time
    """
    if parent is None:
        parent = ETgetParent(doc, node)

    varToCheck = [] #List of variables to check for suppression

    if simplifyVar:
        loc = ETgetLocalityPath(doc, node)
        if node.tag.endswith('}do-construct'):
            #Try to remove variables used in the loop
            varToCheck.extend([(loc, ETn2name(arg)) for arg in node.find('./{*}do-stmt').findall('.//{*}N')])
        elif node.tag.endswith('}if-construct') or node.tag.endswith('}if-stmt'):
            #Try to remove variables used in the conditions
            varToCheck.extend([(loc, ETn2name(arg)) for arg in node.findall('.//{*}condition-E//{*}N')])
        elif node.tag.endswith('}where-construct') or node.tag.endswith('}where-stmt'):
            #Try to remove variables used in the conditions
            varToCheck.extend([(loc, ETn2name(arg)) for arg in node.findall('.//{*}mask-E//{*}N')])
        elif node.tag.endswith('}call-stmt'):
            #We must check if we can suppress the variables used to call the subprogram
            args = node.find('./{*}arg-spec')
            if args is not None:
                varToCheck.extend([(loc, ETn2name(arg.find('.//{*}N'))) for arg in args])
        elif node.tag.endswith('}a-stmt') or node.tag.endswith('}print-stmt'):
            varToCheck.extend([(loc, ETn2name(arg)) for arg in node.findall('.//{*}N')])

    #Node suppression
    parent.remove(node)

    #Variable simplification
    removeVarIfUnused(doc, varToCheck, excludeDummy=True, simplify=simplifyVar)

    #If we have suppressed the statement in a if statement (one-line if) or else statement
    #we must suppress the entire if/where statement even when simplifyStruct is False
    if parent.tag.endswith('}action-stmt'):
        _removeNode(doc, ETgetParent(doc, parent), simplifyVar, simplifyStruct)

    elif simplifyStruct:
        if parent.tag.endswith('}do-construct') and len(_nodesInDo(parent)) == 0:
            _removeNode(doc, parent, simplifyVar, simplifyStruct)
        elif parent.tag.endswith('}if-block'):
            parPar = ETgetParent(doc, parent)
            if len(_nodesInIf(parPar)) == 0:
                _removeNode(doc, ETgetParent(doc, parent), simplifyVar, simplifyStruct)
        elif parent.tag.endswith('}where-block'):
            parPar = ETgetParent(doc, parent)
            if len(_nodesInWhere(parPar)) == 0:
                _removeNode(doc, ETgetParent(doc, parent), simplifyVar, simplifyStruct)


class Statements():
    @copy_doc(removeCall)
    def removeCall(self, *args, **kwargs):
        return removeCall(self._xml, *args, **kwargs)

    @copy_doc(removePrints)
    def removePrints(self, *args, **kwargs):
        return removePrints(self._xml, *args, **kwargs)


