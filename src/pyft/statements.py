"""
This module includes functions to act on statements
"""
import xml.etree.ElementTree as ET
from pyft.util import (copy_doc, n2name, getParent, non_code, getSiblings, debugDecor, 
                       alltext,
                       PYFTError, getFileName)
from pyft.scope import getScopeChildNodes, getScopeNode, getScopesList, getScopePath, isScopeNode
from pyft.variables import removeVarIfUnused, getVarList, addVar, findVar, findArrayBounds, arrayR2parensR
from pyft.expressions import createExprPart, createArrayBounds
from pyft.cosmetics import changeIfStatementsInIfConstructs        
import re
import logging
        
@debugDecor
def createDoConstruct(loopVariables, indent=0, concurrent=False):
    """
    :param loopVariables: ordered dictionnary with loop variables as key and bounds as values.
                          Bounds are expressed with a 2-tuple.
                          Keys must be in the same order as the order used when addressing an element:
                            if loopVariables.keys is [JI, JK], arrays are addressed with (JI, JK)
    :param indent: current indentation
    :param concurrent: if False, output is made of nested 'DO' loops
                       if True, output is made of a single 'DO CONCURRENT' loop
    :return: (inner, outer, extraindent) with
              - inner the inner do-construct where statements must be added
              - outer the outer do-construct to be inserted somewhere
              - extraindent the number of added indentation (2 if concurrent else 2*len(loopVariables))
    """
    if concurrent:
        #<f:do-construct>
        #  <f:do-stmt>DO CONCURRENT (
        #    <f:forall-triplet-spec-LT>
        #      <f:forall-triplet-spec>
        #        <f:V><f:named-E><f:N><f:n>JIJ</f:n></f:N></f:named-E></f:V>=
        #        <f:lower-bound><f:named-E><f:N><f:n>IIJB</f:n></f:N></f:named-E></f:lower-bound>:
        #        <f:upper-bound><f:named-E><f:N><f:n>IIJE</f:n></f:N></f:named-E></f:upper-bound>
        #      </f:forall-triplet-spec>,
        #      <f:forall-triplet-spec>
        #        <f:V><f:named-E><f:N><f:n>JK</f:n></f:N></f:named-E></f:V>=
        #        <f:lower-bound><f:literal-E><f:l>1</f:l></f:literal-E></f:lower-bound>:
        #        <f:upper-bound><f:named-E><f:N><f:n>IKT</f:n></f:N></f:named-E></f:upper-bound>
        #      </f:forall-triplet-spec>
        #    </f:forall-triplet-spec-LT>)
        #  </f:do-stmt>
        #  statements
        #  <f:end-do-stmt>END DO</f:end-do-stmt>
        #</f:do-construct>
        triplets = []
        for v, (l, u) in list(loopVariables.items())[::-1]: #Better for vectorisation with some compilers
            V = ET.Element('{http://fxtran.net/#syntax}V')
            V.append(createExprPart(v))
            V.tail = '='
            lower, upper = createArrayBounds(l, u, 'DOCONCURRENT')

            triplet = ET.Element('{http://fxtran.net/#syntax}forall-triplet-spec')
            triplet.extend([V, lower, upper])

            triplets.append(triplet)

        tripletLT = ET.Element('{http://fxtran.net/#syntax}forall-triplet-spec-LT')
        tripletLT.tail = ')'
        for triplet in triplets[:-1]:
            triplet.tail = ', '
        tripletLT.extend(triplets)

        dostmt = ET.Element('{http://fxtran.net/#syntax}do-stmt')
        dostmt.text = 'DO CONCURRENT ('
        dostmt.tail = '\n'
        dostmt.append(tripletLT)
        enddostmt = ET.Element('{http://fxtran.net/#syntax}end-do-stmt')
        enddostmt.text = 'END DO'

        doconstruct = ET.Element('{http://fxtran.net/#syntax}do-construct')
        doconstruct.extend([dostmt, enddostmt])
        doconstruct.tail = '\n'
        inner = outer = doconstruct
        doconstruct[0].tail += (indent + 2) * ' ' #Indentation for the statement after DO
    else:
        #<f:do-construct>
        #  <f:do-stmt>DO
        #    <f:do-V><f:named-E><f:N><f:n>JRR</f:n></f:N></f:named-E></f:do-V> =
        #    <f:lower-bound><f:literal-E><f:l>1</f:l></f:literal-E></f:lower-bound>:
        #    <f:upper-bound><f:named-E><f:N><f:n>IKT</f:n></f:N></f:named-E></f:upper-bound>
        #  </f:do-stmt>\n
        #  statements \n
        #  <f:end-do-stmt>END DO</f:end-do-stmt>
        #</f:do-construct>\n
        def make_do(v, l, u):
            doV = ET.Element('{http://fxtran.net/#syntax}do-V')
            doV.append(createExprPart(v))
            doV.tail = '='
            lower, upper = createArrayBounds(l, u, 'DO')

            dostmt = ET.Element('{http://fxtran.net/#syntax}do-stmt')
            dostmt.text = 'DO '
            dostmt.tail = '\n'
            dostmt.extend([doV, lower, upper])

            enddostmt = ET.Element('{http://fxtran.net/#syntax}end-do-stmt')
            enddostmt.text = 'END DO'

            doconstruct = ET.Element('{http://fxtran.net/#syntax}do-construct')
            doconstruct.extend([dostmt, enddostmt])
            doconstruct.tail = '\n'
            return doconstruct

        outer = None
        inner = None
        for i, (v, (l, u)) in enumerate(list(loopVariables.items())[::-1]):
            doconstruct = make_do(v, l, u)
            doconstruct[0].tail += (indent + 2 * i + 2) * ' ' #Indentation for the statement after DO
            if outer is None:
                outer = doconstruct
                inner = doconstruct
            else:
                inner.insert(1, doconstruct)
                inner = doconstruct
                doconstruct.tail += (indent + 2 * i - 2) * ' ' #Indentation for the ENDDO statement
    return inner, outer, 2 if concurrent else 2 * len(loopVariables)

@debugDecor
def setFalseIfStmt(doc, flags, scopePath, simplify=False):
    """
    Set to .FALSE. a given boolean fortran flag before removing the node if simplify is True
    :param doc: xml fragment to use
    :param flags: list of strings of flags to set to .FALSE.
    :param scopePath: scope to explore (None for all). This is a '/'-separated path with each element
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
    if scopePath is None:
        scopePath = [loc for loc in getScopesList(doc) if loc.split('/')[-1].split(':')[0] != 'type']
    else:
        if isinstance(scopePath, str): scopePath = [scopePath]
    singleFalseBlock,multipleFalseBlock = [], []
    for loc in scopePath:
        #Loop on nodes composing the scope
        for node in getScopeChildNodes(doc, getScopeNode(doc, loc)):
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
def removeCall(doc, callName, scopePath, simplify=False):
    """
    :param doc: xml fragment to use
    :param callName: name of the subprogram calls to remove.
    :param scopePath: scope to explore (None for all). This is a '/'-separated path with each element
                         having the form 'module:<name of the module>', 'sub:<name of the subroutine>' or
                         'func:<name of the function>'
    :param simplify: try to simplify code (if we delete "CALL FOO(X)" and if X not used else where,
                     we also delete it; or if the call was alone inside a if-then-endif construct,
                     the construct is also removed, and variables used in the if condition are also
                     checked...)
    """
    callName = callName.upper()
    if scopePath is None:
        scopePath = [loc for loc in getScopesList(doc) if loc.split('/')[-1].split(':')[0] != 'type']
    else:
        if isinstance(scopePath, str): scopePath = [scopePath]
    callNodes = []
    for loc in scopePath:
        #Loop on nodes composing the scope
        for node in getScopeChildNodes(doc, getScopeNode(doc, loc)):
            callNodes += [node] if node.tag.endswith('}call-stmt') else [] #In case node is a call statement
            callNodes += [cn for cn in node.findall('.//{*}call-stmt')] #If node is a construct with call statements
    callNodes = [cn for cn in callNodes
                 if n2name(cn.find('.//{*}named-E/{*}N')).upper() == callName] #filter by name
    removeStmtNode(doc, callNodes, simplify, simplify)

@debugDecor
def removePrints(doc, scopePath, simplify=False):
    """
    Removes all print statements
    :param doc: xml fragment to use
    :param scopePath: scope to explore (None for all). This is a '/'-separated path with each element
                         having the form 'module:<name of the module>', 'sub:<name of the subroutine>' or
                         'func:<name of the function>'
    :param simplify: try to simplify code (if we delete "print*, X" and if X is not used else where,
                     we also delete it; or if the print was alone inside a if-then-endif construct,
                     the construct is also removed, and variables used in the if condition are also
                     checked...)
    """
    if scopePath is None:
        scopePath = [loc for loc in getScopesList(doc) if loc.split('/')[-1].split(':')[0] != 'type']
    else:
        if isinstance(scopePath, str): scopePath = [scopePath]
    printNodes = []
    for loc in scopePath:
        #Loop on nodes composing the scope
        for node in getScopeChildNodes(doc, getScopeNode(doc, loc)):
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
            loc = getScopePath(doc, node)
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
            previous.tail = previous.tail.replace('\n','') + (node.tail if node.tail is not None else '')
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
        removeStmtNode(doc, otherNodes, simplifyVar, simplifyStruct)
    #suppress construct nodes one by one (recursive call)
    for n in constructNodes:
        removeConstructNode(doc, n, simplifyVar, simplifyStruct)

@debugDecor
def removeArraySyntax(doc, concurrent=False, useMnhExpand=True, everywhere=True,
                      loopVar=None, reuseLoop=True, funcList=None,
                      updateMemSet=False, updateCopy=False):
    """
    Transform array syntax into DO loops
    :param doc: etree to use
    :param concurrent: use 'DO CONCURRENT' instead of simple 'DO' loops
    :param useMnhExpand: use the mnh directives to transform the entire bloc in a single loop
    :param everywhere: transform all array syntax in DO loops
    :param loopVar: None to create new variable for each added DO loop
                    or a function that return the name of the variable to use for the loop control.
                    This function returns a string (name of the variable), or True to create
                    a new variable, or False to not transform this statement
                    The functions takes as arguments:
                      - lower and upper bounds as defined in the declaration statement
                      - lower and upper bounds as given in the statement
                      - name of the array
                      - index of the rank
    :param reuseLoop: if True, try to reuse loop created whith everywhere=True
    :param funcList: list of entity names that must be recognized as array functions
                     (in addition to the intrisic ones) to discard from transformation
                     statements that make use of them. None is equivalent to an empty list.
    :param updateMemSet: True to put affectation to constante in DO loops
    :param updateCopy: True to put array copy in DO loops

    Notes: * With useMnhExpand, the function checks if the coding is conform to what is needed
             for the filepp/mnh_expand tool (to not breack compatibility with this tool)
           * Arrays are transformed only if ':' are used.
             A=A(:) is not transformed at all (or raises an exception if found in a WHERE block)
             A(:)=A is wrongly transformed into "DO...; A(J1)=A; ENDDO" and will produce a compilation error
             WHERE(L) X(:)=0. is not transformed at all (unknown behaviour in case of nested WHERE)
           * This function is not compatible with functions that return arrays:
             X(1:5)=FUNC(1) will be transformed into "DO J1=1,5; X(J1)=FUNC(1); ENDDO"
             x(1:5)=FUNC(X(1:5)) will be transformed into "DO J1=1,5; X(J1)=FUNC(X(J1)); ENDDO"
             But intrinsic functions (COUNT, ANY...) are recognised and corresponding statements
             are not transformed.
             The list of intrinsic array functions can be extended by user functions with the funcList
             argument.
    """

    #Developer notes:
    #We use recursivity to avoid the use of the 'getParent' function.
    #We start from the top node and call 'recur'.
    #
    #The 'recur' function loops over the different nodes and:
    # - search for mnh directives (if 'useMnhExpand' is True):
    #     - when it is an opening directive:
    #         - decode the directive to identify bounds and variables to use ('decode' function)
    #         - introduce the DO loops (with 'createDoConstruct')
    #         - activate the 'in_mnh' flag
    #     - when it is a closing directive:
    #         - deactivate the 'in_mnh' flag
    # - while the 'in_mnh' flag is activated:
    #     - update ('updateStmt' function, that uses 'arrayR2parensR') and put all statements in the DO loops
    # - in case (if 'everywhere' is True) statement is expressed using array-syntax:
    #     - find the bounds and guess a set of variables to use ('findArrayBounds' function)
    #     - introduce the DO loops (with 'createDoConstruct') if we cannot reuse the previous one
    #     - update ('updateStmt' function, that uses 'arrayR2parensR') and put all statements in the DO loops
    # - in case the statement contains other statements (SUBROUTINE, DO loop...), call 'recur' on it
    #
    #Because we iterate on the statements, the tree structure cannot be modified during the iteration.
    #All the modifications to apply are, instead, stored in objetcs ('toinsert', 'toremove' and 'varList') and
    #are applied afterwards.
    #
    #In addition, a number of instructions are needed to preserve and/or modify the indentation and can
    #somewhat obfuscate the source code.

    def decode(directive):
        """
        Decode mnh_expand directive
        :param directive: mnh directive text
        :return: (table, kind) where
                 table is a dictionnary: keys are variable names, values are tuples with first and last index
                 kind is 'array' or 'where'
        """
        #E.g. !$mnh_expand_array(JIJ=IIJB:IIJE,JK=1:IKT)
        #We expect that the indexes are declared in the same order as the one they appear in arrays
        #For the example given, arrays are addressed with (JIJ, JK)
        #For this example, return would be ('array', {'JIJ':('IIJB', 'IIJE'), 'JK':('1', 'IKT')})
        table = directive.split('(')[1].split(')')[0].split(',')
        table = {c.split('=')[0]:c.split('=')[1].split(':') for c in table} #ordered since python 3.7
        if directive.lstrip(' ').startswith('!$mnh_expand'):
            kind = directive[13:].lstrip(' ').split('(')[0].strip()
        else:
            kind = directive[17:].lstrip(' ').split('(')[0].strip()
        return table, kind

    def updateStmt(doc, e, table, kind, extraindent, parent, varList, currentScope):
        """
        Updates the statement given the table dictionnary '(:, :)' is replaced by '(JI, JK)' if
        table.keys() is ['JI', 'JK']
        :param e: statement to update
        :param table: dictionnary retruned by the decode function
        :param kind: kind of mnh directives: 'array' or 'where'
                                             or None if transformation is not governed by mnh directive
        :param varList: description of declared variables
        :param currentScope: current scope
        """

        def add_extra(node, extra):
            """Helper function to add indentation spaces"""
            if extra != 0 and (not node.tail is None) and '\n' in node.tail:
                #We add indentation after new line only
                #- if tail already contains a '\n' to discard
                #  a-stmt followed by a comment
                #  or '&' immediatly followed by something at the beginning of a line
                #- if not folowed by another new line (with optional space in between)
                node.tail = re.sub(r"(\n[ ]*)(\Z|[^\n ]+)", r"\1" + extra * ' ' + r"\2", node.tail)

        add_extra(e, extraindent) #Set indentation for the *next* node
        if e.tag.split('}')[1] == 'C':
            pass
        elif e.tag.split('}')[1] == 'cpp':
            i = list(parent).index(e)
            if i == 0:
                #In this case, it would be a solution to add an empty comment before the
                #node e to easilty control the indentation contained in the tail
                raise PYFTError("How is it possible?")
            parent[i - 1].tail = parent[i - 1].tail.rstrip(' ') #Remove the indentation
        elif e.tag.split('}')[1] == 'a-stmt':
            sss = e.findall('./{*}E-1/{*}named-E/{*}R-LT/{*}array-R/' + \
                            '{*}section-subscript-LT/{*}section-subscript')
            if len([ss for ss in sss if ':' in alltext(ss)]) != len(table):
                raise PYFTError("Inside code sections to transform in DO loops, all affectations must use ':'.\n" + \
                                "This is not the case in:\n{stmt}".format(stmt=alltext(e)))
            if e.find('./{*}E-1/{*}named-E/{*}N').tail is not None and kind is not None:
                raise PYFTError("To keep the compatibility with the filepp version of loop " + \
                                "expansion, nothing must appear between array names and opening " + \
                                "parethesis inside mnh directive sections.")
            #We loop on named-E nodes (and not directly on array-R nodes to prevent using the costly getParent)
            for namedE in e.findall('.//{*}R-LT/..'):
                arrayR2parensR(namedE, table, varList, currentScope) #Replace slices by variable
            for cnt in e.findall('.//{*}cnt'):
                add_extra(cnt, extraindent) #Add indentation spaces after continuation characters
        elif e.tag.split('}')[1] == 'if-stmt':
            logging.warning("An if statement is inside a code " + \
                            "section transformed in DO loop in {f}".format(f=getFileName(doc)))
            #Update the statement contained in the action node
            updateStmt(doc, e.find('./{*}action-stmt')[0], table, kind, 0, e, varList, currentScope)
        elif e.tag.split('}')[1] == 'if-construct':
            logging.warning("An if construct is inside a code " + \
                            "section transformed in DO loop in {f}".format(f=getFileName(doc)))
            for ifBlock in e.findall('./{*}if-block'): #Loop over the blocks (if, elseif, else)
                for child in ifBlock: #Loop over each statement inside the block
                    if child.tag.split('}')[1] not in ('if-then-stmt', 'else-if-stmt', 'else-stmt', 'end-if-stmt'):
                        updateStmt(doc, child, table, kind, extraindent, ifBlock, varList, currentScope)
                    else:
                        add_extra(child, extraindent) #Update indentation because the loop is here and not in recur
                        for cnt in child.findall('.//{*}cnt'):
                            add_extra(cnt, extraindent) #Add indentation spaces after continuation characters
        elif e.tag.split('}')[1] == 'where-stmt':
            #Where statement becomes if statement
            e.tag = e.tag.split('}')[0] + '}if-stmt'
            e.text = 'IF (' + e.text.split('(', 1)[1]
            updateStmt(doc, e.find('./{*}action-stmt')[0], table, kind, extraindent, e, varList, currentScope) #Update the action part
            mask = e.find('./{*}mask-E')
            mask.tag = mask.tag.split('}')[0] + '}condition-E' #rename the condition tag
            for namedE in mask.findall('.//{*}R-LT/..'):
                arrayR2parensR(namedE, table, varList, currentScope) #Replace slices by variable
            for cnt in e.findall('.//{*}cnt'):
                add_extra(cnt, extraindent) #Add indentation spaces after continuation characters
        elif e.tag.split('}')[1] == 'where-construct':
            if kind != 'where' and kind is not None:
                raise PYFTError('To keep the compatibility with the filepp version of loop " + \
                                "expansion, no where construct must appear in mnh_expand_array blocks.')
            #Where construct becomes if construct
            e.tag = e.tag.split('}')[0] + '}if-construct'
            for whereBlock in e.findall('./{*}where-block'): #Loop over the blocks (where, elsewhere)
                whereBlock.tag = whereBlock.tag.split('}')[0] + '}if-block'
                for child in whereBlock: #Loop over each statement inside the block
                    if child.tag.split('}')[1] == 'end-where-stmt':
                        #rename ENDWHERE into ENDIF
                        child.tag = child.tag.split('}')[0] + '}end-if-stmt'
                        child.text = 'END IF'
                        add_extra(child, extraindent) #Update indentation because the loop is here and not in recur
                    elif child.tag.split('}')[1] in ('where-construct-stmt', 'else-where-stmt'):
                        add_extra(child, extraindent) #Update indentation because the loop is here and not in recur
                        if child.tag.split('}')[1] == 'where-construct-stmt':
                            #rename WHERE into IF (the THEN part is attached to the condition)
                            child.tag = child.tag.split('}')[0] + '}if-then-stmt'
                            child.text = 'IF (' + child.text.split('(', 1)[1]
                        else:
                            #In where construct the same ELSEWHERE keyword is used with or without mask
                            #Whereas for if structure ELSEIF is used with a condition and ELSE without condition
                            if '(' in child.text:
                                #rename ELSEWHERE into ELSEIF
                                child.tag = child.tag.split('}')[0] + '}else-if-stmt'
                                child.text = 'ELSE IF (' + child.text.split('(', 1)[1]
                            else:
                                #rename ELSEWHERE into ELSE
                                child.tag = child.tag.split('}')[0] + '}else-stmt'
                                child.text = 'ELSE'
                        for mask in child.findall('./{*}mask-E'): #would a find be enough?
                            #add THEN
                            mask.tag = mask.tag.split('}')[0] + '}condition-E'
                            mask.tail += ' THEN'
                            for namedE in mask.findall('.//{*}R-LT/..'):
                                arrayR2parensR(namedE, table, varList, currentScope) #Replace slices by variable in the condition
                        for cnt in child.findall('.//{*}cnt'):
                            add_extra(cnt, extraindent) #Add indentation spaces after continuation characters
                    else:
                        updateStmt(doc, child, table, kind, extraindent, whereBlock, varList, currentScope)
        else:
            raise PYFTError('Unexpected tag found in mnh_expand directives: {t}'.format(t=e.tag.split('}')[1]))
        return e

    def closeLoop(loopdesc):
        """Helper function to deal with indetation"""
        if loopdesc:
            inner, outer, indent, extraindent = loopdesc
            if inner[-2].tail is not None:
                outer.tail = inner[-2].tail[:-extraindent] #tail of last statement in DO loop before transformation
            inner[-2].tail = '\n' + (indent + extraindent - 2) * ' ' #position of the ENDDO
        return False

    toinsert = [] #list of nodes to insert
    toremove = [] #list of nodes to remove
    varList = [] #list of variables
    def recur(elem, currentScope):
        in_mnh = False #are we in a DO loop created by a mnh directive
        in_everywhere = False #are we in a created DO loop (except loops created with mnh directive)
        tailSave = {} #Save tail content before transformation (to retrieve original indentation)
        currentScope = getScopePath(doc, elem) if isScopeNode(elem) else currentScope
        for ie, e in enumerate(list(elem)): #we loop on elements in the natural order
            if e.tag.split('}')[1] == 'C' and e.text.lstrip(' ').startswith('!$mnh_expand') and useMnhExpand:
                #This is an opening mnh directive
                if in_mnh:
                    raise PYFTError('Nested mnh_directives are not allowed')
                in_mnh = True
                in_everywhere = closeLoop(in_everywhere) #close other loop if needed

                #Directive decoding
                table, kind = decode(e.text)
                indent = len(e.tail) - len(e.tail.rstrip(' ')) #indentation of the next statement
                toremove.append((elem, e)) #we remove the directive itself
                if ie != 0:
                    #We add, to the tail of the previous node, the tail of the directive (except one \n)
                    if elem[ie - 1].tail is None: elem[ie - 1].tail = ''
                    elem[ie - 1].tail += e.tail.replace('\n', '', 1).rstrip(' ')
                #Building loop
                inner, outer, extraindent = createDoConstruct(table, indent=indent, concurrent=concurrent)
                toinsert.append((elem, outer, ie)) #Place to insert the loop

            elif e.tag.split('}')[1] == 'C' and e.text.lstrip(' ').startswith('!$mnh_end_expand') and useMnhExpand:
                #This is a closing mnh directive
                if not in_mnh:
                    raise PYFTError('End mnh_directive found before begin directive in {f}'.format(f=getFileName(doc)))
                if (table, kind) != decode(e.text):
                    raise PYFTError("Opening and closing mnh directives must be conform in {f}".format(f=getFileName(doc)))
                in_mnh = False
                toremove.append((elem, e)) #we remove the directive itself
                #We add, to the tail of outer DO loop, the tail of the directive (except one \n)
                outer.tail += e.tail.replace('\n', '', 1) #keep all but one new line characters
                elem[ie - 1].tail = elem[ie - 1].tail[:-2] #previous item controls the position of ENDDO

            elif in_mnh:
                #This statement is between the opening and closing mnh directive
                toremove.append((elem, e)) #we remove it from its old place
                inner.insert(-1, e) #Insert first in the DO loop
                updateStmt(doc, e, table, kind, extraindent, inner, varList, currentScope) #then update, providing new parent in argument

            elif everywhere and e.tag.split('}')[1] in ('a-stmt', 'if-stmt', 'where-stmt', 'where-construct'):
                #This node could contain array-syntax

                #Is the node written using array-syntax? Getting the first array...
                if e.tag.split('}')[1] == 'a-stmt':
                    #Left side of the assignment
                    arr = e.find('./{*}E-1/{*}named-E/{*}R-LT/{*}array-R/../..')
                    #Right side
                    is_memSet = False
                    is_copy = False
                    E2 = e.find('./{*}E-2')
                    num = len(E2.findall('.//{*}array-R')) #Number of arrays using array-syntax
                    if num == 0:
                        #It is an array initialisation when there is no array-syntax on the right side
                        #If array-syntax is used without explicit '(:)', it could be detected as an initialisation
                        is_memSet = True
                    elif len(E2) == 1 and E2[0].tag.split('}')[1] == 'named-E' and num == 1 and \
                        E2[0].find('.//{*}parens-R') is None:
                        #It is an array copy when there is only one child in the right hand side
                        #    and this child is a named-E and this child contains only
                        #    one array-R node and no parens-R
                        is_copy = True
                    #Discard?
                    if (is_memSet and not updateMemSet) or (is_copy and not updateCopy):
                        arr = None
                elif e.tag.split('}')[1] == 'if-stmt':
                    #We only deal with assignment in the if statement case
                    arr = e.find('./{*}action-stmt/{*}a-stmt/{*}E-1/{*}named-E/{*}R-LT/{*}array-R/../..')
                    if arr is not None:
                        #In this case we transform the if statement into an if-construct
                        changeIfStatementsInIfConstructs(doc, singleItem=e, parent=elem)
                        recur(e, currentScope) #to transform the content of the if
                        arr = None #to do nothing more on this node
                elif e.tag.split('}')[1] == 'where-stmt':
                    arr = e.find('./{*}mask-E//{*}named-E/{*}R-LT/{*}array-R/../..')
                elif e.tag.split('}')[1] == 'where-construct':
                    arr = e.find('./{*}where-block/{*}where-construct-stmt/{*}mask-E//{*}named-E/{*}R-LT/{*}array-R/../..')

                #Check if it is written using array-syntax and must not be excluded; then compute bounds
                if arr is None:
                    #There is no array-syntax
                    newtable = None
                elif len(set([alltext(a).count(':') for a in e.findall('.//{*}R-LT/{*}array-R')])) > 1:
                    #All the elements written using array-syntax don't have the same rank
                    #(can be due to function calls, eg: "X(:)=FUNC(Y(:,:))")
                    newtable = None
                elif len(set(['ALL', 'ANY', 'COSHAPE', 'COUNT', 'CSHIFT', 'DIMENSION',
                              'DOT_PRODUCT', 'EOSHIFT', 'LBOUND', 'LCOBOUND', 'MATMUL',
                              'MAXLOC', 'MAXVAL', 'MERGE', 'MINLOC', 'MINVAL', 'PACK',
                              'PRODUCT', 'REDUCE', 'RESHAPE', 'SHAPE', 'SIZE', 'SPREAD',
                              'SUM', 'TRANSPOSE', 'UBOUND', 'UCOBOUND', 'UNPACK'] + \
                              (funcList if funcList is not None else [])
                            ).intersection(set([n2name(N) for N in e.findall('.//{*}named-E/{*}N')]))) > 0:
                    #At least one intrinsic array function is used
                    newtable = None
                else:
                    if len(varList) == 0:
                        #Get all the variables declared in the tree
                        varList.extend(getVarList(doc))
                    #Guess a variable name
                    if arr is not None:
                        newtable, varNew = findArrayBounds(doc, arr, varList, currentScope, loopVar)
                        for v in varNew:
                            v['new'] = True
                        varList.extend(varNew)
                    else:
                        newtable = None

                if newtable is None:
                    #We cannot convert the statement (not in array-syntax, excluded or no variable found to loop)
                    in_everywhere = closeLoop(in_everywhere) #close previous loop if needed
                else:
                    #we have to transform the statement
                    if not (in_everywhere and table == newtable):
                        #No opened previous loop, or not coresponding
                        in_everywhere = closeLoop(in_everywhere) #close previous loop, if needed
                        #We must create a DO loop
                        if ie != 0 and elem[ie -1].tail is not None:
                            #Indentation of the current node, attached to the previous sibling
                            tail = tailSave.get(elem[ie -1], elem[ie -1].tail) #get tail before transformation
                            indent = len(tail) - len(tail.rstrip(' '))
                        else:
                            indent = 0
                        table = newtable #save the information on the newly build loop
                        kind = None #not built from mnh directives
                        #Building loop
                        inner, outer, extraindent = createDoConstruct(table, indent=indent, concurrent=concurrent)
                        toinsert.append((elem, outer, ie)) #place to insert the loop
                        in_everywhere = (inner, outer, indent, extraindent) #we are now in a loop
                    tailSave[e] = e.tail #save tail for future indentation computation
                    toremove.append((elem, e)) #we remove it from its old place
                    inner.insert(-1, e) #Insert first in the DO loop
                    updateStmt(doc, e, table, kind, extraindent, inner, varList, currentScope) #then update, providing new parent in argument
                    if not reuseLoop:
                        #Prevent from reusing this DO loop
                        in_everywhere = closeLoop(in_everywhere)

            else:
                in_everywhere = closeLoop(in_everywhere) #close loop if needed
                if len(e) >= 1:
                    #Iteration
                    recur(e, currentScope)
        in_everywhere = closeLoop(in_everywhere)

    recur(doc, getScopePath(doc, doc))
    #First, element insertion by reverse order (in order to keep the insertion index correct)
    for elem, outer, ie in toinsert[::-1]:
        elem.insert(ie, outer)
    #Then, suppression
    for parent, elem in toremove:
        parent.remove(elem)
    #And variable creation
    addVar(doc, [(v['scope'], v['n'], 'INTEGER :: {name}'.format(name=v['n']), None)
                 for v in varList if v.get('new', False)])

    return doc

class Statements():
    @copy_doc(removeCall)
    def removeCall(self, *args, **kwargs):
        return removeCall(self._xml, *args, **kwargs)

    @copy_doc(removePrints)
    def removePrints(self, *args, **kwargs):
        return removePrints(self._xml, *args, **kwargs)

    @copy_doc(removeArraySyntax)
    def removeArraySyntax(self, *args, **kwargs):
        return removeArraySyntax(self._xml, *args, **kwargs)

