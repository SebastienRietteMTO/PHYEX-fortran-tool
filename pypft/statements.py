"""
This module includes functions to act on statements
"""
import xml.etree.ElementTree as ET
from util import (copy_doc, n2name, getParent, non_code, getSiblings, debugDecor, 
                  alltext,tostring, getIndexLoop)
from locality import (getLocalityChildNodes, getLocalityNode, getLocalitiesList,
                      getLocalityPath)
from variables import removeVarIfUnused


@debugDecor
def arrayRtoparensR(doc,arrayR):
    """
    Return a parensR node from an array-R
    :param loopIndex: string for the fortran loop index 
    :param lowerBound: string for the fortran lower bound of the do loop
    :param upperBound: string for the fortran upper bound of the do loop
    """
    
    subs=arrayR.findall('.//{*}section-subscript')
    parensR = ET.Element('{http://fxtran.net/#syntax}parens-R')
    parensR.text = '('
    parensR.tail = ')'
    elementLT = ET.Element('{http://fxtran.net/#syntax}element-LT')
    for i,sub in enumerate(subs): 
        element = ET.Element('{http://fxtran.net/#syntax}element')
        if alltext(sub) == ':': # (:) only
            pass
            #try to guess from the dimension declaration (need to first find the element from varList)
        elif sub.text == ':': # :INDEX e.g. (:IKTE); transform it to 1:IKTE
            upperBound = alltext(sub.findall('.//{*}lower-bound/{*}*/{*}*/{*}n')[0])
            element.insert(0,createNamedENn(getIndexLoop('1',upperBound)))      
        elif len(sub.findall('.//{*}upper-bound')) == 0:
            if ':' in alltext(sub): # INDEX: e.g. (IKTB:)
                print('sub tail')
                pass
            else:  # single literal-E : copy the object (e.g. IKA alone or operation such as IKE+1)
                element.insert(0,sub.findall('.//{*}lower-bound')[0]) 
        else:
            lowerBounds=sub.findall('.//{*}lower-bound')
            upperBounds=sub.findall('.//{*}upper-bound')
            lowerBound=alltext(lowerBounds[0])
            upperBound=alltext(upperBounds[0])
            element.insert(0,createNamedENn(getIndexLoop(lowerBound,upperBound)))
        elementLT.append(element)

    for i in range(len(elementLT)-1):
        elementLT[i].tail = ','
    parensR.insert(0,elementLT)
    return parensR

@debugDecor
def createDoStmt(loopIndexstr, lowerBoundstr, upperBoundstr):
    """
    Return a Do-construct node
    :param loopIndexstr: string for the fortran loop index 
    :param lowerBoundstr: string for the fortran lower bound of the do loop
    :param upperBoundstr: string for the fortran upper bound of the do loop
    """
    doConstruct = ET.Element('{http://fxtran.net/#syntax}do-construct')
    doStmt = ET.Element('{http://fxtran.net/#syntax}do-stmt')
    doStmt.text = 'DO '
    enddoStmt = ET.Element('{http://fxtran.net/#syntax}end-do-stmt')
    enddoStmt.text = 'END DO '
    enddoStmt.tail = '\n'
    doV = ET.Element('{http://fxtran.net/#syntax}do-V')
    doV.tail = '='
    doV.insert(0,createNamedENn(loopIndexstr))
    lowerBound, upperBound = createArrayBounds(lowerBoundstr, upperBoundstr,forDoLoop=True)
    doConstruct.insert(0,doStmt)
    doStmt.insert(0,doV)
    doStmt.insert(1,lowerBound)
    doStmt.insert(2,upperBound)
    doStmt.insert(3,enddoStmt)
    return doConstruct

@debugDecor
def createArrayBounds(lowerBoundstr, upperBoundstr, forDoLoop=False):
    """
    Return a lower-bound and upper-bound node
    :param lowerBoundstr: string for the fortran lower bound of an array
    :param upperBoundstr: string for the fortran upper bound of an array
    :param forDoLoop: True for do loops; False for arrays
    """
    lowerBound = ET.Element('{http://fxtran.net/#syntax}lower-bound')
    lowerBound.insert(0,createNamedENn(lowerBoundstr))
    upperBound = ET.Element('{http://fxtran.net/#syntax}upper-bound')
    upperBound.insert(0,createNamedENn(upperBoundstr))
    if forDoLoop:
        lowerBound.tail = ','
        upperBound.tail = '\n  '
    else:
        lowerBound.tail = ':'
    return lowerBound,upperBound    
    
@debugDecor
def createNamedENn(text=' '):
    """
    Return a <named-E/><N><n>text</n></N></named-E> node
    :param text: string
    """
    namedE = ET.Element('{http://fxtran.net/#syntax}named-E')
    N = ET.Element('{http://fxtran.net/#syntax}N')
    n = ET.Element('{http://fxtran.net/#syntax}n')
    n.text = text
    namedE.insert(0,N)
    N.insert(0,n)
    return namedE

@debugDecor
def createIfThenConstruct(nodeConditionE):
    """
    Return a if-construct IF THEN ENDIF <if-construct/><if-block><if-then-stmt><condition-E>conditionE node</condition-E></if-then-stmt></if-block></if-construct> node
    :param nodeConditionE: node that will be inserted as condition-E
    """
    ifConstruct = ET.Element('{http://fxtran.net/#syntax}if-construct')
    ifBlock = ET.Element('{http://fxtran.net/#syntax}if-block')
    ifThenStmt = ET.Element('{http://fxtran.net/#syntax}if-then-stmt')
    ifThenStmt.text = 'IF ('
    conditionE = ET.Element('{http://fxtran.net/#syntax}condition-E')
    conditionE.tail = ' ) THEN\n'
    endifStmt =  ET.Element('{http://fxtran.net/#syntax}end-if-stmt')
    endifStmt.text = 'END IF\n'
    nodeConditionE.tail = '' # Original condition-E .tail from WHERE-condition has a comma
    conditionE.insert(0,nodeConditionE)
    ifThenStmt.insert(0,conditionE)
    ifBlock.insert(0,ifThenStmt)
    ifBlock.insert(1,endifStmt)
    ifConstruct.insert(0,ifBlock)
    return ifConstruct

@debugDecor
def createIfThenElseConstruct(nodeConditionE):
    """
    Return a if-construct IF THEN ELSE ENDIF <if-construct/><if-block><if-then-stmt><condition-E>conditionE node
    </condition-E></if-then-stmt>
    </if-block><if-block>ELSE ENDIF</if-block></if-construct> node
    :param nodeConditionE: node that will be inserted as condition-E
    """
    ifConstruct = ET.Element('{http://fxtran.net/#syntax}if-construct')
    ifBlock = ET.Element('{http://fxtran.net/#syntax}if-block')
    ifThenStmt = ET.Element('{http://fxtran.net/#syntax}if-then-stmt')
    ifThenStmt.text = 'IF ('
    conditionE = ET.Element('{http://fxtran.net/#syntax}condition-E')
    conditionE.tail = ') THEN\n'
    elseBlock = ET.Element('{http://fxtran.net/#syntax}if-block')
    elseStmt = ET.Element('{http://fxtran.net/#syntax}else-stmt')
    elseStmt.text = 'ELSE\n'
    endifStmt =  ET.Element('{http://fxtran.net/#syntax}end-if-stmt')
    endifStmt.text = 'END IF\n'
    nodeConditionE.tail = '' # Original condition-E .tail from WHERE-condition has a comma
    conditionE.insert(0,nodeConditionE)
    ifThenStmt.insert(0,conditionE)
    ifBlock.insert(0,ifThenStmt)
    elseBlock.insert(0,elseStmt)
    elseBlock.insert(1,endifStmt)
    ifConstruct.insert(0,ifBlock)
    ifConstruct.insert(1,elseBlock)
    return ifConstruct
@debugDecor
def setFalseIfStmt(doc, flags, localityPath, simplify=False):
    """
    Set to .FALSE. a given boolean fortran flag before removing the node if simplify is True
    :param doc: xml fragment to use
    :param flags: list of strings of flags to set to .FALSE.
    :param localityPath: locality to explore (None for all). This is a '/'-separated path with each element
                         having the form 'module:<name of the module>', 'sub:<name of the subroutine>' or
                         'func:<name of the function>'
    :param simplify: try to simplify code (if the .FALSE. was alone inside a if-then-endif construct,
                     the construct is removed, and variables used in the if condition are also
                     checked)
    """
    FalseNode = ET.Element('{http://fxtran.net/#syntax}literal-E')
    FalseNode.text = ' .FALSE. '
    for flag in flags:
        flag = flag.upper()
    if localityPath is None:
        localityPath = [loc for loc in getLocalitiesList(doc) if loc.split('/')[-1].split(':')[0] != 'type']
    else:
        if isinstance(localityPath, str): localityPath = [localityPath]
    singleFalseBlock,multipleFalseBlock = [], []
    for loc in localityPath:
        #Loop on nodes composing the locality
        for node in getLocalityChildNodes(doc, getLocalityNode(doc, loc)):
            #Multiple flags conditions
            Node_opE = node.findall('.//{*}condition-E/{*}op-E')
            for node_opE in Node_opE:
                found = False
                for i,n in enumerate(node_opE):
                    if n.tag.endswith('}named-E') and alltext(n).upper() in flags:
                        found = True
                        if i == 0: # Append at first object of parent
                            node_opE.insert(0,FalseNode)
                        else:
                            node_opE.append(FalseNode)
                        getParent(node_opE,n).remove(n)
                if found:
                    multipleFalseBlock.append(node_opE)
            #Solo condition
            Node_namedE = node.findall('.//{*}condition-E/{*}named-E')
            for n in Node_namedE:
                if alltext(n).upper() in flags:
                    par = getParent(node,n)
                    par.append(FalseNode)
                    par.remove(n)
                    singleFalseBlock.append(getParent(node,getParent(node,par))) # <if-block><if-then-stmt>
    if simplify:
        removeStmtNode(doc, singleFalseBlock, simplify, simplify)
        evalFalseIfStmt(doc, multipleFalseBlock, simplify)
    
def evalFalseIfStmt(doc, nodes, simplify=False):
    """
    Evaluate if-stmt with multiple op-E and remove the nodes if only .FALSE. are present
    :param doc: xml fragment to use
    :param nodes: list of nodes of type op-E to evaluate (containing .FALSE.)
    :param simplify: try to simplify code (if if-block is removed, variables used in the if condition are also
                     checked)
    """
    nodes_torm = []
    for node in nodes:
        toRemove = True
        for n in node:
            if n.tag.endswith('}op') or (n.tag.endswith('}literal-E') and '.FALSE.' in alltext(n)):
                pass
            else:
                toRemove = False
                break
        if toRemove: 
            nodes_torm.append(getParent(doc,getParent(doc,getParent(doc,node)))) #<if-block><if-then-stmt><condition-E>
    removeStmtNode(doc, nodes_torm, simplify, simplify)

@debugDecor
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
        for node in getLocalityChildNodes(doc, getLocalityNode(doc, loc)):
            callNodes += [node] if node.tag.endswith('}call-stmt') else [] #In case node is a call statement
            callNodes += [cn for cn in node.findall('.//{*}call-stmt')] #If node is a construct with call statements
    callNodes = [cn for cn in callNodes
                 if n2name(cn.find('.//{*}named-E/{*}N')).upper() == callName] #filter by name
    removeStmtNode(doc, callNodes, simplify, simplify)

@debugDecor
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
        for node in getLocalityChildNodes(doc, getLocalityNode(doc, loc)):
            printNodes += [node] if node.tag.endswith('}print-stmt') else [] #In case node is a print statement
            printNodes += [cn for cn in node.findall('.//{*}print-stmt')] #If node is a construct with print statements
    removeStmtNode(doc, printNodes, simplify, simplify)

@debugDecor
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
            if not non_code(item): nodes.append(item)
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
            if not non_code(item): nodes.append(item)
    return nodes

def _nodesInDo(doNode):
    """
    Internal method to return nodes in do structure
    """
    nodes = []
    for item in [i for i in doNode if not (i.tag.endswith('}do-stmt') or \
                                           i.tag.endswith('}end-do-stmt'))]:
        if not non_code(item): nodes.append(item)
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
            if not non_code(item): nodes.append(item)
    return nodes

@debugDecor
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
            loc = getLocalityPath(doc, node)
            if node.tag.endswith('}do-construct'):
                #Try to remove variables used in the loop
                varToCheck.extend([(loc, n2name(arg)) for arg in node.find('./{*}do-stmt').findall('.//{*}N')])
            elif node.tag.endswith('}if-construct') or node.tag.endswith('}if-stmt'):
                #Try to remove variables used in the conditions
                varToCheck.extend([(loc, n2name(arg)) for arg in node.findall('.//{*}condition-E//{*}N')])
            elif node.tag.endswith('}where-construct') or node.tag.endswith('}where-stmt'):
                #Try to remove variables used in the conditions
                varToCheck.extend([(loc, n2name(arg)) for arg in node.findall('.//{*}mask-E//{*}N')])
            elif node.tag.endswith('}call-stmt'):
                #We must check if we can suppress the variables used to call the subprogram
                varToCheck.extend([(loc, n2name(arg)) for arg in node.findall('./{*}arg-spec//{*}N')])
                #And maybe, the subprogram comes from a module
                varToCheck.append((loc, n2name(node.find('./{*}procedure-designator//{*}N'))))
            elif node.tag.endswith('}a-stmt') or node.tag.endswith('}print-stmt'):
                varToCheck.extend([(loc, n2name(arg)) for arg in node.findall('.//{*}N')])
            elif node.tag.endswith('}selectcase-construct'):
                #Try to remove variables used in the selector and in conditions
                varToCheck.extend([(loc, n2name(arg)) for arg in node.findall('.//{*}case-E//{*}N')])
                varToCheck.extend([(loc, n2name(arg)) for arg in node.findall('.//{*}case-value//{*}N')])
    
    #Node suppression
    parents = {} #cache
    for node in nodesToSuppress:
        parent = getParent(doc, node)
        parents[id(node)] = parent
        newlines = '\n' * (alltext(node).count('\n') if node.tag.endswith('-construct') else 0)
        if node.tail is not None or len(newlines) > 0:
            previous = getSiblings(doc, node, after=False)
            if len(previous) == 0:
                previous = parent
            else:
                previous = previous[-1]
            if previous.tail is None: previous.tail = ''
            previous.tail = previous.tail + newlines + (node.tail if node.tail is not None else '')
        parent.remove(node)
    
    #Variable simplification
    removeVarIfUnused(doc, varToCheck, excludeDummy=True, excludeModule=True, simplify=simplifyVar)
    
    #List the new nodes to suppress
    newNodesToSuppress = []
    for node in nodesToSuppress:
        parent = parents[id(node)]
        #If we have suppressed the statement in a if statement (one-line if) or where statement
        #we must suppress the entire if/where statement even when simplifyStruct is False
        if parent.tag.endswith('}action-stmt'):
            newNodesToSuppress.append(getParent(doc, parent))
    
        elif simplifyStruct:
            if parent.tag.endswith('}do-construct') and len(_nodesInDo(parent)) == 0:
                newNodesToSuppress.append(parent)
            elif parent.tag.endswith('}if-block'):
                parPar = getParent(doc, parent)
                if len(_nodesInIf(parPar)) == 0:
                    newNodesToSuppress.append(parPar)
            elif parent.tag.endswith('}where-block'):
                parPar = getParent(doc, parent)
                if len(_nodesInWhere(parPar)) == 0:
                    newNodesToSuppress.append(parPar)
            elif parent.tag.endswith('}selectcase-block'):
                parPar = getParent(doc, parent)
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


