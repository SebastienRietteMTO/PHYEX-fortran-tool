"""
This module implements functions for high-to-moderate level transformation
"""

import xml.etree.ElementTree as ET
from util import (copy_doc, debugDecor,getIndexLoop,
                  alltext,tostring,getParent,getSiblings,moveInGrandParent,fortran2xml)
from statements import (removeCall, setFalseIfStmt, createDoStmt,arrayRtoparensR,createArrayBounds,
                        createIfThenConstruct, createIfThenElseConstruct,removeStmtNode)
from variables import removeUnusedLocalVar, getVarList, addVar
from cosmetics import changeIfStatementsInIfConstructs
from locality import getLocalityChildNodes, getLocalityNode, getLocalitiesList, getLocalityPath
import copy

@debugDecor
def deleteDrHook(doc, simplify=False):
    """
    Remove DR_HOOK calls.
    If Simplify is True, also remove all variables only needed for these calls (ZHOOK_HANDLE,
    DR_HOOK, LHOOK, YOMHOOK, JPRB, PARKIND1)
    :param doc: etree to use
    :param simplify : if True, remove variables that are now unused
    """
    removeCall(doc, 'DR_HOOK', None, simplify=simplify)

@debugDecor
def deleteBudgetDDH(doc, simplify=False):
    """
    Remove Budget calls.
    If Simplify is True, also remove all variables only needed for these calls
    :param doc: etree to use
    :param simplify : if True, remove variables that are now unused
    """
    removeCall(doc, 'BUDGET_STORE_INIT_PHY', None, simplify=simplify)
    removeCall(doc, 'BUDGET_STORE_END_PHY', None, simplify=simplify)
    removeCall(doc, 'BUDGET_STORE_ADD_PHY', None, simplify=simplify)
    flag_torm = ['BUCONF%LBUDGET_SV','BUCONF%LBUDGET_TKE','BUCONF%LBUDGET_TH','BUCONF%LBUDGET_RI', \
    'BUCONF%LBUDGET_RV','BUCONF%LBUDGET_RG','BUCONF%LBUDGET_RS','BUCONF%LBUDGET_RH','BUCONF%LBUDGET_RR', \
    'BUCONF%LBUDGET_RC','BUCONF%LBUDGET_U','BUCONF%LBUDGET_V','BUCONF%LBUDGET_W']
    setFalseIfStmt(doc,flag_torm, None, simplify=simplify)

@debugDecor
def removeArraySyntax(doc,expandDoLoops=False,expandWhere=False):
    """
    Remove all the array syntax and replace it by do loops (WHERE not included for now)
    If index are present in fortran code, e.g. A(IIB:IIE), the DO loops index limits are kept
    If index are not present, e.g. A(:,;), the DO loops index limits are the complete size of the array
    :param doc: etree to use
    :param expandDoLoops: if True, expand Do loops
    :param expandWhere: if True, expand Where (TODO : does not work with nested where)
    """
    # If-Statements must be expanded first : may be applied locally (TODO adapt the function changeIfStatementsInIfConstructs)
    if expandDoLoops: changeIfStatementsInIfConstructs(doc)
    
    locations  = getLocalitiesList(doc,withNodes='tuple')
    for loc in locations:
        varList = getVarList(doc, loc)
        locNode = loc[1]
        varArray = []
        varArrayNamesList = []
        localIntegers = []
        Node_opE = locNode.findall('.//{*}a-stmt')
        for var in varList:
            if not var['as'] and var['t'] == 'INTEGER' and not var['arg']:
                localIntegers.append(var['n'])
            if var['as']:
                varArray.append(var)
                varArrayNamesList.append(var['n'])      
        
        loopIndexToCheck = []
        
        # Convert where-construct-stmt
        if expandWhere:
            whereConstruct = locNode.findall('.//{*}where-construct')
            for node_where in whereConstruct:
                node_opE = node_where.findall('.//{*}where-construct-stmt/{*}*/{*}*')[0] # The last * is for finding op-E but also single array-R masks
                # If found, convert all ':' alone to declared dimensions array 1:D%...
                subs=node_opE.findall('.//{*}section-subscript')
                for sub in subs:
                    if alltext(sub) == ':': # ':' alone
                        # Get the i sub-index of the current object of E-2
                        subPar = getParent(locNode,sub)
                        for j,el in enumerate(subPar):
                            if el == sub:
                                indextoTransform=j
                                break
                        # Get the variable object to find its declaration dimension
                        varName = alltext(getParent(node_opE,sub,level=4).findall('.//{*}N/{*}n')[0])
                        ind=varArrayNamesList.index(varName)
                        lowerBound = '1'
                        upperBound = str(varArray[ind]['as'][indextoTransform][1])
                        lowerXml,upperXml = createArrayBounds(lowerBound, upperBound)
                        sub.insert(0,lowerXml)
                        sub.insert(1,upperXml)
                        sub.text = '' # Delete the initial ':'
                
                # Replace the array-like index selection by index loop on all variables (array-R)
                arrayR = node_opE.findall('.//{*}array-R')
                for node in arrayR:
                    parensR=arrayRtoparensR(locNode,node)
                    par = getParent(node_opE,node)
                    par.insert(1,parensR)
                    par.remove(node)
        
        # expandDoLoops if possible
        for node_opE in Node_opE:
            anyArrayR = node_opE.findall('.//{*}array-R')        
            if len(anyArrayR) > 0:
                nodePar = getParent(locNode,node_opE)
                onlyNumbers = False
                if not nodePar.tag.endswith('}where-block') and expandDoLoops:
                    found_goldIndex = False
                    for n in node_opE.findall('.//{*}n'): #Premiere façon de faire : on cherche si les indices existent et si ils sont strictement égaux à JIJ,JK, JI ou JJ  
                        if alltext(n) == 'JK' or alltext(n) == 'JIJ' \
                        or alltext(n) == 'JI' or alltext(n) == 'JJ' :
                            found_goldIndex = True
                            break
                    varName = alltext(node_opE.findall('.//{*}E-1/{*}*/{*}*/{*}n')[0])
                    if not found_goldIndex and varName in varArrayNamesList:
                        #if brakets with indexes are found : use it as lower and upper bound
                        
                        # If found, convert all ':' alone to declared dimensions array 1:D%...
                        subsE2=node_opE.findall('.//{*}E-2//{*}section-subscript')
                        for sub in subsE2:
                            if alltext(sub) == ':': # ':' alone
                                # Get the i sub-index of the current object of E-2
                                subPar = getParent(locNode,sub)
                                for j,el in enumerate(subPar):
                                    if el == sub:
                                        indextoTransform=j
                                        break
                                # Get the variable object to find its declaration dimension
                                ind=varArrayNamesList.index(varName) #TODO varName is probably wrong here because multiple objects can be in E-2 and varName is from E-1
                                lowerBound = '1'
                                upperBound = str(varArray[ind]['as'][indextoTransform][1])
                                lowerXml,upperXml = createArrayBounds(lowerBound, upperBound)
                                sub.insert(0,lowerXml)
                                sub.insert(1,upperXml)
                                sub.text = '' # Delete the initial ':'
                        
                        # Build Do statement based on the type of section-subscript of E-1 + convert ':' alone to array-R for next step
                        subsE1=node_opE.findall('.//{*}E-1//{*}section-subscript')
                        doToBuild = []
                        for i,sub in enumerate(subsE1):
                            if alltext(sub) == ':': # ':' alone
                                # Creation of the array-R object needed to be converted futher in parens-R
                                ind=varArrayNamesList.index(varName)
                                lowerBound = '1'
                                upperBound = str(varArray[ind]['as'][i][1])
                                lowerXml,upperXml = createArrayBounds(lowerBound, upperBound)
                                sub.insert(0,lowerXml)
                                sub.insert(1,upperXml)
                                sub.text = '' # Delete the initial ':'
                                # Create DO statement
                                indexLoop = getIndexLoop(lowerBound, upperBound)
                                doToBuild.append(createDoStmt(indexLoop,lowerBound,upperBound))
                            elif sub.text == ':': # :INDEX e.g. (:IKTE); transform it to 1:IKTE
                                upperBound = alltext(sub.findall('.//{*}lower-bound/{*}*/{*}*/{*}n')[0]) # lower-bound for upper-bound (it is normal)
                                doToBuild.append(createDoStmt(getIndexLoop('1',upperBound),'1',upperBound))
                            elif len(sub.findall('.//{*}upper-bound')) == 0:
                                if ':' in alltext(sub): # INDEX: e.g. (IKTB:) ==> IKTB:size(node_opE)
                                    # Creation of the array-R object needed to be converted futher in parens-R
                                    ind=varArrayNamesList.index(varName)
                                    lowerBound = alltext(sub.findall('.//{*}lower-bound/{*}*/{*}*/{*}n')[0])
                                    upperBound = str(varArray[ind]['as'][i][1])
                                    lowerXml,upperXml = createArrayBounds(lowerBound, upperBound)
                                    sub.insert(0,lowerXml)
                                    sub.insert(1,upperXml)
                                    sub.text = '' # Delete the initial ':'
                                    indexLoop = getIndexLoop(lowerBound, upperBound)
                                    doToBuild.append(createDoStmt(indexLoop,lowerBound,upperBound))
                                else:  # single literal-E : copy the object (e.g. IKA alone or operation such as IKE+1)
                                    pass
                            else:
                                lowerBounds=sub.findall('.//{*}lower-bound')
                                upperBounds=sub.findall('.//{*}upper-bound')
                                lowerBound=alltext(lowerBounds[0])
                                upperBound=alltext(upperBounds[0])
                                if len(sub.findall('.//{*}literal-E')) == 2: #lower and upper Bounds
                                    onlyNumbers = True
                                    break
                                else:
                                    indexLoop = getIndexLoop(lowerBound, upperBound)
                                    doToBuild.append(createDoStmt(indexLoop,lowerBound,upperBound))
                        doToBuild.reverse() # To write first loops from the latest index (K or SV)

                        for dostmt in doToBuild:
                            if alltext(dostmt.findall('.//{*}do-V/{*}named-E/{*}N/{*}n')[0]) not in loopIndexToCheck:
                                loopIndexToCheck.append(alltext(dostmt.findall('.//{*}do-V/{*}named-E/{*}N/{*}n')[0]))
                        
                        # Replace the array-like index selection by index loop on all variables (array-R)
                        if not onlyNumbers:
                            arrayR = node_opE.findall('.//{*}array-R')
                            for node in arrayR:
                                parensR=arrayRtoparensR(locNode,node)
                                par = getParent(node_opE,node)
                                par.insert(1,parensR)
                                par.remove(node)
            
                            # Insert the DO loops
                            if len(doToBuild)>0:
                                # Protection for if-then stmt transformed that does not have \n 
                                if not node_opE.tail: #if NoneType
                                    node_opE.tail = '\n'
                                else:
                                    node_opE.tail = node_opE.tail + '\n'
                                # Specific treatment for Where-block
                                doToBuild[-1][0].insert(3,node_opE) # 0 = do-V; 1 = lower-bound; 2 = upper-bound, so insert at 3
                                # Insert nested DO-loops into the previous object of doToBuild until reach the 0e object
                                for i in range(len(doToBuild)-1):
                                    doToBuild[len(doToBuild)-1-(i+1)][0].insert(3,doToBuild[len(doToBuild)-1-i])
                                moveInGrandParent(locNode,node_opE,nestedObj=doToBuild)
    
        # expandWhere if possible
        if expandWhere:
            for node_where in whereConstruct:
                nodePar = getParent(locNode,node_where)
                Node_opE = node_where.findall('.//{*}a-stmt')
                doToBuild,finalToBuild = [], []
                for node_opE in Node_opE:
                    varName = alltext(node_opE.findall('.//{*}E-1/{*}*/{*}*/{*}n')[0])
                    # convert all ':' alone to declared dimensions array 1:D%...
                    subsE2=node_opE.findall('.//{*}E-2//{*}section-subscript')
                    for sub in subsE2:
                        if alltext(sub) == ':': # ':' alone
                            # Get the i sub-index of the current object of E-2
                            subPar = getParent(locNode,sub)
                            for j,el in enumerate(subPar):
                                if el == sub:
                                    indextoTransform=j
                                    break
                            # Get the variable object to find its declaration dimension
                            ind=varArrayNamesList.index(varName) #TODO varName is probably wrong here because multiple objects can be in E-2 and varName is from E-1
                            lowerBound = '1'
                            upperBound = str(varArray[ind]['as'][indextoTransform][1])
                            lowerXml,upperXml = createArrayBounds(lowerBound, upperBound)
                            sub.insert(0,lowerXml)
                            sub.insert(1,upperXml)
                            sub.text = '' # Delete the initial ':'
                    
                    # Build Do statement based on the type of section-subscript of E-1 and Convert E-1 element with ':' alone to array-R for next step
                    # TODO : pas propre, on créer des doToBuild qui ne servent à rien (seulement la première fois créé est sauvegardée)
                    # Ceci est pour faire une unique boucle nestée et y placer le IF-THEN stmt. Les indices des boucles sont basées sur E-1
                    # Ce qui n'est pas toujours correct mais devrait l'être (à vérifier)
                    subsE1=node_opE.findall('.//{*}E-1//{*}section-subscript')
                    for i,sub in enumerate(subsE1):
                        if alltext(sub) == ':': # ':' alone
                            # Creation of the array-R object needed to be converted futher in parens-R
                            ind=varArrayNamesList.index(varName)
                            lowerBound = '1'
                            upperBound = str(varArray[ind]['as'][i][1])
                            lowerXml,upperXml = createArrayBounds(lowerBound, upperBound)
                            sub.insert(0,lowerXml)
                            sub.insert(1,upperXml)
                            sub.text = '' # Delete the initial ':'
                            # Create the DO statement
                            indexLoop = getIndexLoop(lowerBound, upperBound)
                            doToBuild.append(createDoStmt(indexLoop,lowerBound,upperBound))
                        elif sub.text == ':': # :INDEX e.g. (:IKTE); transform it to 1:IKTE
                            upperBound = alltext(sub.findall('.//{*}lower-bound/{*}*/{*}*/{*}n')[0])
                            doToBuild.append(createDoStmt(getIndexLoop('1',upperBound),'1',upperBound))
                        elif len(sub.findall('.//{*}upper-bound')) == 0:
                            if ':' in alltext(sub): # INDEX: e.g. (IKTB:) ==> IKTB:size(node_opE)
                                # Creation of the array-R object needed to be converted futher in parens-R
                                ind=varArrayNamesList.index(varName)
                                lowerBound = alltext(sub.findall('.//{*}lower-bound/{*}*/{*}*/{*}n')[0])
                                upperBound = str(varArray[ind]['as'][i][1])
                                lowerXml,upperXml = createArrayBounds(lowerBound, upperBound)
                                sub.insert(0,lowerXml)
                                sub.insert(1,upperXml)
                                sub.text = '' # Delete the initial ':'
                                indexLoop = getIndexLoop(lowerBound, upperBound)
                                doToBuild.append(createDoStmt(indexLoop,lowerBound,upperBound))
                            else:  # single literal-E : copy the object (e.g. IKA alone or operation such as IKE+1)
                                pass
                        else:
                            lowerBounds=sub.findall('.//{*}lower-bound')
                            upperBounds=sub.findall('.//{*}upper-bound')
                            lowerBound=alltext(lowerBounds[0])
                            upperBound=alltext(upperBounds[0])
                            doToBuild.append(createDoStmt(getIndexLoop(lowerBound, upperBound),lowerBound,upperBound))
                    doToBuild.reverse() # To write first loops from the latest index (K or SV)
                    if len(finalToBuild) == 0:
                        for do in doToBuild:
                            finalToBuild.append(do)
                    # Replace the array-like index selection by index loop on all variables (array-R)
                    arrayR = node_opE.findall('.//{*}array-R')
                    for node in arrayR:
                        parensR=arrayRtoparensR(locNode,node)
                        par = getParent(node_opE,node)
                        par.insert(1,parensR)
                        par.remove(node)
                        
                # Create an Ifconstruct object from the condition in the WHERE statement
                whereBlocks = node_where.findall('.//{*}where-block')
                if len(whereBlocks) == 2: #ELSEWHERE present
                    ifConstruct = createIfThenElseConstruct(node_where.findall('.//{*}where-construct-stmt/{*}mask-E')[0])            
                else:
                    ifConstruct = createIfThenConstruct(node_where.findall('.//{*}where-construct-stmt/{*}mask-E')[0])
    
                # Loop on a-stmt object inserted in the if-construct
                ifBlocks = ifConstruct.findall('./{*}if-block')
                for i,whereBlock in enumerate(whereBlocks):
                    Node_opE = whereBlock.findall('.//{*}a-stmt')
                    for j,node_opE in enumerate(Node_opE):
                        ifBlocks[i].insert(1+j,node_opE) # 1 is for if-stmt or else-stmt already present
    
                # Insert the ifConstruct into the last object of finalToBuild before nesting the DO loop
                finalToBuild[-1][0].insert(3,ifConstruct) # 0 = do-V; 1 = lower-bound; 2 = upper-bound, so insert at 3
                # Insert nested DO-loops into the previous object of finalToBuild until reach the 0e object
                for i in range(len(finalToBuild)-1):
                    finalToBuild[len(finalToBuild)-1-(i+1)][0].insert(3,finalToBuild[len(finalToBuild)-1-i])
                moveInGrandParent(locNode,node_opE,nestedObj=finalToBuild)
    
                # Clean where-blocks and move first Do-stmt to where-construct parent
                allsiblings = nodePar.findall('./{*}*')
                ind=allsiblings.index(node_where)
                nodePar.insert(ind,node_where.findall('.//{*}do-construct')[0])
                nodePar.remove(node_where)
    
        # Before leaving the locality, check loop index presence
        for loopIndex in loopIndexToCheck:
            if loopIndex not in localIntegers:
                addVar(doc,[[loc[0],loopIndex,'INTEGER :: '+loopIndex,None]]) #TODO issue: le 2e argument ne semble pas s'appliquer ? il faut ajouter loopIndex dans le 3e argument pour effectivement l'ecrire



@debugDecor
def removePHYEXUnusedLocalVar(doc, localityPath=None, excludeList=None, simplify=False):
    """
    Remove unused local variables (dummy and module variables are not suppressed)
    This function is identical to variables.removeUnusedLocalVar except that this one
    is specific to the PHYEX code and take into account the mnh_expand directives.
    :param doc: xml fragment to search for variable usage
    :param localityPath: locality to explore (None for all)
    :param excludeList: list of variable names to exclude from removal (even if unused)
    :param simplify: try to simplify code (if we delete a declaration statement that used a
                     variable as kind selector, and if this variable is not used else where,
                     we also delete it)
    """

    #Look for variables needed for the mnh_expand directives
    for node in doc.findall('.//{*}C'):
        if node.text.startswith('!$mnh_expand_array(') or node.text.startswith('!$mnh_expand_where('):
            if excludeList is None: excludeList = []
            elems = node.text.split('(')[1].split(')')[0].split(',')
            excludeList.extend([v.strip().upper() for v in [e.split('=')[0] for e in elems]])
    return removeUnusedLocalVar(doc, localityPath=localityPath, excludeList=excludeList, simplify=simplify)

class Applications():
    @copy_doc(deleteDrHook)
    def deleteDrHook(self, *args, **kwargs):
        return deleteDrHook(self._xml, *args, **kwargs)

    @copy_doc(deleteBudgetDDH)
    def deleteBudgetDDH(self, *args, **kwargs):
        return deleteBudgetDDH(self._xml, *args, **kwargs)
    
    @copy_doc(removeArraySyntax)
    def removeArraySyntax(self, *args, **kwargs):
        return removeArraySyntax(self._xml, *args, **kwargs)

    @copy_doc(removePHYEXUnusedLocalVar)
    def removePHYEXUnusedLocalVar(self, *args, **kwargs):
        return removePHYEXUnusedLocalVar(self._xml, *args, **kwargs)
