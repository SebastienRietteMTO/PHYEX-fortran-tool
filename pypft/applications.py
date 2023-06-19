"""
This module implements functions for high-to-moderate level transformation
"""

import xml.etree.ElementTree as ET
from util import (copy_doc, debugDecor,getIndexLoop,
                  alltext,tostring,getParent,getSiblings,moveInGrandParent,fortran2xml,checkInDoWhile)
from statements import (removeCall, setFalseIfStmt, createDoStmt,arrayRtoparensR,createArrayBounds,
                        createIfThenElseConstruct,removeStmtNode)
from variables import removeUnusedLocalVar, getVarList, addVar, addModuleVar
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
def addStack(doc):
    """
    Add specific allocations of local arrays on the fly for GPU
    :param doc: etree to use
    :param simplify : if True, remove variables that are now unused
    """
    locations  = getLocalitiesList(doc,withNodes='tuple')
    addVar(doc,[[locations[0][0],'YDSTACK','TYPE(STACK) :: YDSTACK, YLSTACK',-1]])
    addModuleVar(doc, [[locations[0][0], 'STACK_MOD',None]])
    
    # Add include stack.h after the USE STACK_MOD
    modules = doc.findall('.//{*}use-stmt/{*}module-N/{*}N/{*}n')
    fortranSource = "SUBROUTINE FOO598756\n #include \"stack.h\" \nEND SUBROUTINE"
    _, xmlIncludeStack = fortran2xml(fortranSource)
    for mod in modules:
        if alltext(mod) == 'STACK_MOD':
            par = getParent(doc, mod, level=4)
            index = par[:].index(getParent(doc, mod, level=3))
            par.insert(index+1, xmlIncludeStack.find('.//{*}include'))

    # Add !$acc routine (ROUTINE_NAME) seq after subroutine-stmt
    routineName = doc.find('.//{*}subroutine-stmt/{*}subroutine-N/{*}N/{*}n')
    fortranSource = "SUBROUTINE FOO598756\n !$acc routine ("+ alltext(routineName) + ") seq \nEND SUBROUTINE"
    _, xmlAccRoutine = fortran2xml(fortranSource)
    routineStmt = doc.find('.//{*}subroutine-stmt')
    par = getParent(doc, routineStmt)
    index = par[:].index(routineStmt)
    par.insert(index+1, xmlAccRoutine.find('.//{*}C'))
    
    # Add YLSTACK = YDSTACK
    decls = doc.findall('.//{*}T-decl-stmt')
    lastDecl = decls[-1]
    par = getParent(doc,lastDecl)
    index = par[:].index(lastDecl)
    fortranSource = "SUBROUTINE FOO598756\n YLSTACK=YDSTACK \nEND SUBROUTINE"
    _, xml = fortran2xml(fortranSource)
    par.insert(index+1,xml.find('.//{*}a-stmt'))
 
    # Add alloc (local array)
    #fortranSource = "SUBROUTINE FOO598756\n alloc (ZTLK) \nEND SUBROUTINE"
    #_, xml = fortran2xml(fortranSource)
    #par.insert(index+2, xml.find('.//{*}broken-stmt'))
    
    # Add temp (local array)
    varList = getVarList(doc, locations[0][0])
    # First remove PARAMETER variable (containing literal-E in asx)
    #for var in varList:
    #    if  var['as']:
    #        for asx in var['asx'][0]:
    #            if asx and 'literal-E' in asx:
    #                print(var)   
    # Look for local arrays
    #for var in varList:
    #    if not var['arg'] and var['as']:
    #        print(var['n'])

    
    
    
    
@debugDecor
def removeArraySyntax(doc,expandDoLoops=False,expandWhere=False):
    """
    Remove all the array syntax and replace it by do loops (WHERE not included for now)
    If index are present in fortran code, e.g. A(IIB:IIE), the DO loops index limits are kept
    If index are not present, e.g. A(:,;), the DO loops index limits are the complete size of the array
    TODO: If the array-syntax is present in a DO WHILE loop, the transformation is not applied.
    TODO: Double nested WHERE are not transformed
    :param doc: etree to use
    :param expandDoLoops: if True, expand Do loops
    :param expandWhere: if True, expand Where
    """
    # If-Statements must be expanded first : may be applied locally (TODO adapt the function changeIfStatementsInIfConstructs)
    if expandDoLoops: changeIfStatementsInIfConstructs(doc)
    
    locations  = getLocalitiesList(doc,withNodes='tuple')
    # work on subroutines or functions only
    for loc in locations:
        if '/sub:' not in loc[0] or '/func:' not in loc[0]:
            locations.remove(loc)

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
        
        # Convert where-construct statements and else-where statements
        if expandWhere:
            whereConstruct = locNode.findall('.//{*}where-construct')
            for node_where in whereConstruct:
                inDoWhile = checkInDoWhile(locNode,node_where)
                if not inDoWhile:
                    node_opE = node_where.findall('.//{*}where-construct-stmt/{*}*/{*}*')[0] # The last * is for finding op-E but also single array-R masks
                    #node_opE.extend(node_where.findall('.//{*}else-where-stmt/{*}*'))
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
            
            # Else-where statements (to factorize with above)
            for node_where in whereConstruct:
                inDoWhile = checkInDoWhile(locNode,node_where)
                if not inDoWhile:
                    node_opEs = node_where.findall('.//{*}else-where-stmt/{*}*')
                    for node_opE in node_opEs:
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
                inDoWhile = checkInDoWhile(locNode,node_opE)
                if not nodePar.tag.endswith('}where-block') and expandDoLoops and not inDoWhile:
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
                                try: # If upperBound is an integer found in the variable declaration, then the expansion is not done (in general, small numbers)
                                    int(upperBound)
                                    onlyNumbers = True
                                    break
                                except:
                                    pass
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
                                    doToBuild.append(createDoStmt(getIndexLoop(lowerBound, upperBound),lowerBound,upperBound))
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
                                    doToBuild.append(createDoStmt(getIndexLoop(lowerBound, upperBound),lowerBound,upperBound))
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
                inDoWhile = checkInDoWhile(locNode,node_where)
                if not inDoWhile:
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
                                try: # If upperBound is an integer found in the variable declaration, then the expansion is not done (in general, small numbers)
                                    int(upperBound)
                                    onlyNumbers = True
                                    break
                                except:
                                    pass
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
                                if len(sub.findall('.//{*}literal-E')) == 2: #lower and upper Bounds
                                    onlyNumbers = True
                                    break
                                else:
                                    doToBuild.append(createDoStmt(getIndexLoop(lowerBound, upperBound),lowerBound,upperBound))
                        doToBuild.reverse() # To write first loops from the latest index (K or SV)
                        if len(finalToBuild) == 0:
                            for do in doToBuild:
                                finalToBuild.append(do)
                        # Replace the array-like index selection by index loop on all variables (array-R)
                        if not onlyNumbers:
                            arrayR = node_opE.findall('.//{*}array-R')
                            for node in arrayR:
                                parensR=arrayRtoparensR(locNode,node)
                                par = getParent(node_opE,node)
                                par.insert(1,parensR)
                                par.remove(node)
                            
                    # Create an Ifconstruct object from the condition in the WHERE statement
                    whereBlocks = node_where.findall('.//{*}where-block')
                    conditionsInWhere = node_where.findall('.//{*}where-construct-stmt/{*}mask-E')
                    conditionsInWhere.extend(node_where.findall('.//{*}else-where-stmt/{*}mask-E'))
                    ifConstruct = createIfThenElseConstruct(conditionsInWhere,nbelseBlock=len(whereBlocks)-1)
                        
                    # Loop on a-stmt object inserted in the if-construct
                    ifBlocks = ifConstruct.findall('./{*}if-block')
                    for i,whereBlock in enumerate(whereBlocks):
                        Node_opE = whereBlock.findall('.//{*}a-stmt')
                        k=0
                        for node_opE in Node_opE:
                            ifBlocks[i].insert(1+k,node_opE) # 1 is for if-stmt or else-stmt already present
                            k += 1
                            # If a comment is next to the a-stmt, i.e. next sibling = <C>, keep it
                            siblings = getSiblings(doc,node_opE,after=True,before=False)
                            if len(siblings)>0:
                                if siblings[0].tag.endswith('}C'):
                                    ifBlocks[i].insert(1+k,siblings[0]) # 1 is for if-stmt or else-stmt already present                  
                                    k += 1
    
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
def inlineContainedSubroutines(doc):
    """
    Inline all contained subroutines in the main subroutine
    Local variables in contained subroutines must have been removed first
    Steps :
        - Identify contained subroutines
        - Inline within contained subroutines look for all CALL statements, check if it is a containted routines; if yes, inline :
                - delete the original call statement and insert the inlined node
        - Inline the main routine
        - Delete the containted routines
    :param doc: xml fragment containing main and contained subroutine
    """
    locations  = getLocalitiesList(doc,withNodes='tuple')
    subsNames = []
    # Inline contained subroutines : look for sub: / sub:
    for loc in locations:
        if loc[0][:4] == 'sub:' and '/' in loc[0] and loc[1].tag.endswith('}program-unit'):
            subsNames.append(alltext(loc[1].find('.//{*}subroutine-N/{*}N/{*}n')))

    # Start by nested contained subroutines call, and end up with the last index = the main subroutine to treat 
    locations.reverse()
    for loc in locations: # For all subroutines (main + contained)
        if loc[0][:4] == 'sub:' and loc[1].tag.endswith('}program-unit'):
            callStmtsNn = loc[1].findall('.//{*}call-stmt/{*}procedure-designator/{*}named-E/{*}N/{*}n')
            for callStmtNn in callStmtsNn: # For all CALL statements
                if alltext(callStmtNn) in subsNames: # If name of the routine called = a contained subroutine
                    print(alltext(callStmtNn) + ' in ' + loc[0])
                    for locFound in locations:
                        if alltext(callStmtNn) == locFound[0][locFound[0].find('/sub:')+5:]:
                            callStmt = getParent(doc,callStmtNn,level=4)
                            par = getParent(doc,callStmt)
                            if par.tag.endswith('}action-stmt'): # Expand the if-construct if the call-stmt is in a one-line if-construct
                                changeIfStatementsInIfConstructs(doc,getParent(doc,par))
                            nodeInlined = inline(doc, locFound[1], callStmt)
                            allsiblings = par.findall('./{*}*')
                            index = allsiblings.index(callStmt)
                            par.remove(callStmt)
                            par.insert(index,nodeInlined)

    #Remove original containted subroutines and 'CONTAINS' statement
    contains = doc.find('.//{*}contains-stmt')
    par = getParent(doc,contains)
    par.remove(contains)
    for loc in locations:
        if loc[0][:4] == 'sub:' and '/' in loc[0] and loc[1].tag.endswith('}program-unit'):
            par = getParent(doc,loc[1])
            par.remove(loc[1])

def inline(doc, subContained, callStmt):
    """
    Inline a subContainted subroutine
    Local variables in contained subroutines must have been removed first
    Steps :
        - copy the subContained node
        - remove everything before the declarations variables and the variables declarations
        - from the callStmt, replace all the arguments by their names 
        - return the node to inline
    :param doc: xml fragment containing main and contained subroutine
    :param subContained: xml fragment corresponding to the sub: to inline
    :param callStmt : the call-stmt to get the values of the intent args
    """
    # Deep copy the object to possibly modify the original one multiple times
    node = copy.deepcopy(subContained)
    nodeToRemove = []
    declStmtFound = False # used to remove everything before a variable declaration
    # if any variable declaration (only local, declared in main routine), go directly to 2nd part of the removing algo
    if not node.findall('.//{*}T-decl-stmt'):
        declStmtFound = True

    # Remove all objects that is implicit none, comment or else until reach something interesting
    for n in node:
        if not declStmtFound:
            if not n.tag.endswith('}T-decl-stmt'):
                nodeToRemove.append(n)
            elif n.tag.endswith('}T-decl-stmt'):
                declStmtFound = True
                nodeToRemove.append(n)
        else:
            if n.tag.endswith('}T-decl-stmt') or n.tag.endswith('}C') or n.tag.endswith('}subroutine-stmt') or n.tag.endswith('}implicit-none-stmt') : # We also delete comments in the variable declaration block
                nodeToRemove.append(n)
            else:
                break
    nodeToRemove.append(node.find('.//{*}end-subroutine-stmt'))
    for n in nodeToRemove:
        node.remove(n)
        
    # Replace args
    loc = getLocalityPath(doc, subContained)
    varsRoutine = getVarList(doc,loc)
    varsCall = callStmt.findall('.//{*}arg')
    varsNamedNn = node.findall('.//{*}named-E/{*}N/{*}n')
    for i,var in enumerate(varsCall):
        # Check if arg is of type VAR=value
        if var.find('.//{*}arg-N'):
            n = var.findall('.//{*}n')
            if len(n):
                if alltext(n[0]) == alltext(var.find('.//{*}k')):
                    pass # Do nothing, var names are already matching
                else:
                    for el in varsNamedNn:
                        if el.text == alltext(var.find('.//{*}k')):
                            el.text = alltext(n[0])
            else: # var is literal-E or string-E (only possible as INTENT IN)
                # Two way of doing : 
                #   1- Initialize the contained subroutine variable as the value given in arg + Check if the variable is already declared, if not, declare it in the main routine
                #   2- Replace everywhere in the subroutine, the variable by its value.
                #  1.1
                #fortranSource = "SUBROUTINE FOO598756\n " + alltext(var) + "\nEND SUBROUTINE"
                #_, xml = fortran2xml(fortranSource)
                #node.insert(0,xml.find('.//{*}a-stmt'))
                # 1.2 not done yet while 2. works
                for el in varsNamedNn:
                        if el.text == alltext(var.find('.//{*}k')):
                            par = getParent(node,el)
                            allsiblings = par.findall('./{*}*')
                            index=allsiblings.index(el)
                            par.remove(el)
                            par.insert(index,var[1]) # 0 is the arg-N ; 1 is the value of arg (string-E or literal-E)
        else: # Classic argument (only the var name)
            n = var.findall('.//{*}n')
            if len(n):
                if alltext(n[0]) == varsRoutine[i]['n']:
                    pass # Do nothing, var names are already matching
                else:
                    for el in varsNamedNn:
                        if el.text == varsRoutine[i]['n']:
                            el.text = alltext(var)          
            else: # var is literal-E or string-E (only possible as INTENT IN)
                # see comment above
                # 1.1
                #fortranSource = "SUBROUTINE FOO598756\n " + varsRoutine[i]['n'] + "=" + alltext(var) + "\nEND SUBROUTINE"
                #_, xml = fortran2xml(fortranSource)
                #node.insert(0,xml.find('.//{*}a-stmt'))
                for el in varsNamedNn:
                        if el.text == varsRoutine[i]['n']:
                            par = getParent(node,el)
                            allsiblings = par.findall('./{*}*')
                            index=allsiblings.index(el)
                            par.remove(el)
                            par.insert(index,var)
    return node
    
@debugDecor            
def removeIJLoops(doc):
    """
    Remove all Do loops on JI and JJ : preparation to computation on Klev only
    and init former indexes JI,JJ,JIJ to first array element (= 1) JI=D%NIB, JJ=D%NJB, JIJ=D%NIJB
    WARNING : executed transformed-code will work only if inlineContainedSubroutines is applied first
    :param doc: xml fragment to search for variable usage
    """
    locations  = getLocalitiesList(doc,withNodes='tuple')
    indexToCheck = {'JI':'D%NIB','JJ':'D%NJB','JIJ':'D%NIJB'}
    for loc in locations:
        localNode = loc[1]
        doNodes = localNode.findall('.//{*}do-construct')  
        indexRemoved = []
        for doNode in doNodes:
            loopIndex = doNode.findall('.//{*}do-stmt/{*}do-V/{*}named-E/{*}N/{*}n')
            for loopI in loopIndex:
                if alltext(loopI) in indexToCheck.keys():
                    removeStmtNode(localNode,getParent(localNode,loopI,level=4),False,False) #TODO: the call to removeStmtNode alone adds an empty line after the END-DO stmt + add extra spaces to first children stmt (and end-stmt). Try on turb.F90
                    endDo = doNode.findall('.//{*}end-do-stmt')
                    getParent(localNode,endDo[0]).remove(endDo[0]) #remove end-do statement
                    if alltext(loopI) not in indexRemoved:
                        indexRemoved.append(alltext(loopI))
        
        # Add initialization of old index loop to D%NJB or D%NIB or D%NIJB
        # Not yet tested because, need to have inlined containted routines first
        if len(indexRemoved) > 0:
            lastDecl = localNode.findall('.//{*}T-decl-stmt')[-1] # The case where no T-decl-stmt is found, is not handled (it should not exist !)
            par = getParent(localNode,lastDecl)
            allsiblings = par.findall('./{*}*')
            index=allsiblings.index(lastDecl)
            for i,indexToAdd in enumerate(indexRemoved):
                #Statement building
                 fortranSource = "SUBROUTINE FOO598756\n " + indexToAdd + "=" + indexToCheck[indexToAdd] + "\nEND SUBROUTINE"
                 _, xml = fortran2xml(fortranSource)
                 par.insert(index+1,xml.find('.//{*}a-stmt'))
            
    
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
    @copy_doc(addStack)
    def addStack(self, *args, **kwargs):
        return addStack(self._xml, *args, **kwargs)
    
    @copy_doc(deleteDrHook)
    def deleteDrHook(self, *args, **kwargs):
        return deleteDrHook(self._xml, *args, **kwargs)

    @copy_doc(deleteBudgetDDH)
    def deleteBudgetDDH(self, *args, **kwargs):
        return deleteBudgetDDH(self._xml, *args, **kwargs)

    @copy_doc(removeIJLoops)
    def removeIJLoops(self, *args, **kwargs):
        return removeIJLoops(self._xml, *args, **kwargs)
    
    @copy_doc(removeArraySyntax)
    def removeArraySyntax(self, *args, **kwargs):
        return removeArraySyntax(self._xml, *args, **kwargs)
    
    @copy_doc(removePHYEXUnusedLocalVar)
    def removePHYEXUnusedLocalVar(self, *args, **kwargs):
        return removePHYEXUnusedLocalVar(self._xml, *args, **kwargs)
    
    @copy_doc(inlineContainedSubroutines)
    def inlineContainedSubroutines(self, *args, **kwargs):
        return inlineContainedSubroutines(self._xml, *args, **kwargs)
