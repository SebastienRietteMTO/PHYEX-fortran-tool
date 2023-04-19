"""
This module includes functions to act on statements
"""

from util import copy_doc, needEtree, ETn2name, ETgetParent
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
                removeStmtNode(doc, callNode, simplify)

@needEtree
def removeStmtNode(doc, node, simplify):
    """
    This function removes a node and:
      - suppress part of the code that must be removed at the same time (if-stmt case)
      - suppress variable that became useless (if simplify is True)
    :param doc: xml fragment to use
    :param node: node to remove
    :param simplify: try to simplify code (if we delete "CALL FOO(X)" and if X not used else where,
                     we also delete it; or if the call was alone inside a if-then-endif construct,
                     the construct is also removed, and variables used in the if condition are also
                     checked...)
    """
    varToCheck = [] #List of variables to check for suppression

    parent = ETgetParent(doc, node)
    loc = ETgetLocalityPath(doc, node)
    #In case of an if statement, we start removing the inner statement,
    #then the action-stmt node, then, at the end, the if-stmt node
    if node.tag.endswith('}if-stmt'):
        action = node.find('{*}action-stmt')
        if action is not None:
            #action-stmt has not been removed yet, we start by removing it
            removeStmtNode(doc, action, simplify)
        else:
            #action-stmt has already been removed, we must suppress the if-stmt node
            if simplify:
                #all the variables used in the conditional part must be checked for removal
                varToCheck.extend([(loc, ETn2name(arg)) for arg in node.findall('.//{*}N')])
            parent.remove(node)
    elif node.tag.endswith('}action-stmt'):
        if len(node) != 0:
            #the inner statement has not been removed yet, we start by removing it
            removeStmtNode(doc, node[0], simplify)
        else:
            #the inner statement has already been removed, we must suppress the action-stmt node
            parent.remove(node)
            removeStmtNode(doc, parent, simplify) #The parent (if-stmt node) must be removed
    else:
        if simplify:
            if node.tag.endswith('}call-stmt'):
                #We must check if we can suppress the variables used to call the subprogram
                args = node.find('./{*}arg-spec')
                if args is not None:
                    varToCheck.extend([(loc, ETn2name(arg.find('.//{*}N'))) for arg in args])
        parent.remove(node)
        if parent.tag.endswith('}action-stmt'):
            #If the removed statement is inside a if-stmt/action-stmt
            #we need to suppress the action-stmt node (this suprpession will induce
            #the supression of the if-stmt node)
            removeStmtNode(doc, parent, simplify)

    #Simplification
    removeVarIfUnused(doc, varToCheck, excludeDummy=True, simplify=simplify)

class Statements():
    @copy_doc(removeCall)
    def removeCall(self, *args, **kwargs):
        return removeCall(self._xml, *args, **kwargs)


