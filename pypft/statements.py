"""
This module includes functions to act on statements
"""

from util import copy_doc, needEtree, ETn2name, ETgetParent, ETnon_code, ETgetSiblings, debugDecor, alltext
from locality import (ETgetLocalityChildNodes, ETgetLocalityNode, getLocalitiesList,
                      ETgetLocalityPath)
from variables import removeVarIfUnused

@debugDecor
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
    callNodes = []
    for loc in localityPath:
        #Loop on nodes composing the locality
        for node in ETgetLocalityChildNodes(doc, ETgetLocalityNode(doc, loc)):
            callNodes += [node] if node.tag.endswith('}call-stmt') else [] #In case node is a call statement
            callNodes += [cn for cn in node.findall('.//{*}call-stmt')] #If node is a construct with call statements
    callNodes = [cn for cn in callNodes
                 if ETn2name(cn.find('.//{*}named-E/{*}N')).upper() == callName] #filter by name
    removeStmtNode(doc, callNodes, simplify, simplify)

@debugDecor
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
    printNodes = []
    for loc in localityPath:
        #Loop on nodes composing the locality
        for node in ETgetLocalityChildNodes(doc, ETgetLocalityNode(doc, loc)):
            printNodes += [node] if node.tag.endswith('}print-stmt') else [] #In case node is a print statement
            printNodes += [cn for cn in node.findall('.//{*}print-stmt')] #If node is a construct with print statements
    removeStmtNode(doc, printNodes, simplify, simplify)

@debugDecor
@needEtree
def removeConstructNode(doc, node, simplifyVar, simplifyStruct):
    """
    This function removes a construct node and:
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

    If a statement is passed, it is suppressed by removeStmtNode
    """
    assert node.tag.endswith('-stmt') or node.tag.endswith('-construct'), \
      "Don't know how to suppress only a part of a structure or of a statement"

    #This function removes inner statement to give a chance to identify and suppress unused variables
    #During this step, nodes are suppressed with simplifyStruct=False to prevent infinite loops
    #then the actual node is removed using removeStmtNode

    if node.tag.endswith('-construct'):
        #inner nodes
        nodes = {'do-construct': _nodesInDo,
                 'if-construct': _nodesInIf,
                 'where-construct': _nodesInWhere,
                 'selectcase-construct': _nodesInCase}[node.tag.split('}')[1]](node)
        #sort nodes by type
        constructNodes, otherNodes = [], []
        for n in nodes:
            if n.tag.endswith('-construct'):
                constructNodes.append(n)
            else:
                otherNodes.append(n)
        #suppress all statements at once
        removeStmtNode(doc, otherNodes, simplifyVar, False)
        #suppress construct nodes one by one (recursive call)
        for n in constructNodes:
            removeConstructNode(doc, n, simplifyVar, False)
        #suppress current node
        removeStmtNode(doc, node, simplifyVar, simplifyStruct)
    else:
        #At least a-stmt, print-stmt
        removeStmtNode(doc, node, simplifyVar, simplifyStruct)

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

def _nodesInCase(caseNode):
    """
    Internal method to return nodes in do structure
    """
    nodes = []
    for block in caseNode.findall('./{*}selectcase-block'):
        for item in [i for i in block if not (i.tag.endswith('}select-case-stmt') or \
                                              i.tag.endswith('}case-stmt') or \
                                              i.tag.endswith('}end-select-case-stmt'))]:
            if not ETnon_code(item): nodes.append(item)
    return nodes

@debugDecor
@needEtree
def removeStmtNode(doc, nodes, simplifyVar, simplifyStruct):
    """
    This function removes a statement node and:
      - suppress variable that became useless (if simplifyVar is True)
      - suppress outer loop/if if useless (if simplifyStruct is True)
    :param doc: xml fragment to use
    :param nodes: node (or list of nodes) to remove
    :param simplifyVar: try to simplify code (if we delete "CALL FOO(X)" and if X not used else where,
                        we also delete it; or if the call was alone inside a if-then-endif construct,
                        with simplifyStruct=True, the construct is also removed, and variables used
                        in the if condition are also checked...)
    :param simplifyStruct: try to simplify code (if we delete "CALL FOO(X)" and if the call was
                           alone inside a if-then-endif construct, the construct is also removed,
                           and variables used in the if condition (with simplifyVar=True) aro
                           also checked...)
    """

    #In case the suppression of an if-stmt or where-stmt is asked,
    #we must start by the inner statement
    nodesToSuppress = []
    if not isinstance(nodes, list): nodes = [nodes]
    for node in nodes:
        if node.tag.endswith('}if-stmt') or node.tag.endswith('}where-stmt'):
            action = node.find('./{*}action-stmt')
            if action is not None and len(action) != 0:
                nodesToSuppress.append(action[0])
            else:
                nodesToSuppress.append(node)
        elif node.tag.endswith('}action-stmt'):
            if len(node) != 0:
                nodesToSuppress.append(node[0])
            else:
                nodesToSuppress.append(node)
        else:
            nodesToSuppress.append(node)

    varToCheck = [] #List of variables to check for suppression
    if simplifyVar:
        #Loop to identify all the potential variables to remove
        for node in nodesToSuppress:
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
                varToCheck.extend([(loc, ETn2name(arg)) for arg in node.findall('./{*}arg-spec//{*}N')])
                #And maybe, the subprogram comes from a module
                varToCheck.append((loc, ETn2name(node.find('./{*}procedure-designator//{*}N'))))
            elif node.tag.endswith('}a-stmt') or node.tag.endswith('}print-stmt'):
                varToCheck.extend([(loc, ETn2name(arg)) for arg in node.findall('.//{*}N')])
            elif node.tag.endswith('}selectcase-construct'):
                #Try to remove variables used in the selector and in conditions
                varToCheck.extend([(loc, ETn2name(arg)) for arg in node.findall('.//{*}case-E//{*}N')])
                varToCheck.extend([(loc, ETn2name(arg)) for arg in node.findall('.//{*}case-value//{*}N')])
    
    #Node suppression
    parents = {} #cache
    for node in nodesToSuppress:
        parent = ETgetParent(doc, node)
        parents[id(node)] = parent
        newlines = '\n' * (alltext(node).count('\n') if node.tag.endswith('-construct') else 0)
        if node.tail is not None or len(newlines) > 0:
            previous = ETgetSiblings(doc, node, after=False)
            if len(previous) == 0:
                previous = parent
            else:
                previous = previous[-1]
            if previous.tail is None: previous.tail = ''
            previous.tail = previous.tail + newlines + (node.tail if node.tail is not None else '')
        parent.remove(node)
    
    #Variable simplification
    removeVarIfUnused(doc, varToCheck, excludeDummy=True, simplify=simplifyVar)
    
    #List the new nodes to suppress
    newNodesToSuppress = []
    for node in nodesToSuppress:
        parent = parents[id(node)]
        #If we have suppressed the statement in a if statement (one-line if) or where statement
        #we must suppress the entire if/where statement even when simplifyStruct is False
        if parent.tag.endswith('}action-stmt'):
            newNodesToSuppress.append(ETgetParent(doc, parent))
    
        elif simplifyStruct:
            if parent.tag.endswith('}do-construct') and len(_nodesInDo(parent)) == 0:
                newNodesToSuppress.append(parent)
            elif parent.tag.endswith('}if-block'):
                parPar = ETgetParent(doc, parent)
                if len(_nodesInIf(parPar)) == 0:
                    newNodesToSuppress.append(parPar)
            elif parent.tag.endswith('}where-block'):
                parPar = ETgetParent(doc, parent)
                if len(_nodesInWhere(parPar)) == 0:
                    newNodesToSuppress.append(parPar)
            elif parent.tag.endswith('}selectcase-block'):
                parPar = ETgetParent(doc, parent)
                if len(_nodesInCase(parPar)) == 0:
                    newNodesToSuppress.append(parPar)

    constructNodes, otherNodes = [], []
    for n in newNodesToSuppress:
        if n.tag.endswith('-construct'):
            if n not in constructNodes: constructNodes.append(n)
        else:
            if n not in otherNodes: otherNodes.append(n)
    #suppress all statements at once
    if len(otherNodes) > 0:
        removeStmtNode(doc, otherNodes, simplifyVar, False)
    #suppress construct nodes one by one (recursive call)
    for n in constructNodes:
        removeConstructNode(doc, n, simplifyVar, False)

class Statements():
    @copy_doc(removeCall)
    def removeCall(self, *args, **kwargs):
        return removeCall(self._xml, *args, **kwargs)

    @copy_doc(removePrints)
    def removePrints(self, *args, **kwargs):
        return removePrints(self._xml, *args, **kwargs)


