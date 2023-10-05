"""
This module implements functions for high-to-moderate level transformation
"""

import xml.etree.ElementTree as ET
from util import (copy_doc, debugDecor,getIndexLoop, checkInDoWhile,
                  alltext,tostring,getParent,getSiblings,moveInGrandParent,fortran2xml)
from statements import (removeCall, setFalseIfStmt, createDoStmt,arrayRtoparensR, createArrayBounds,
                        createIfThenElseConstruct, removeStmtNode, expandWhereConstruct, expandArrays,
                        placeArrayRtoparensR, createNamedENn, convertColonArrayinDim,E2StmtToDoStmt)
from variables import removeUnusedLocalVar, getVarList, addVar, addModuleVar, removeVar
from cosmetics import changeIfStatementsInIfConstructs
from scope import getScopeChildNodes, getScopeNode, getScopesList, getScopePath
import copy

@debugDecor
def addIncludes(doc):
    """
    Remove the INCLUDE "file.h" statement and add the content of the .h with the option no-include absent of fxtran
    :param doc: etree to use
    """
    includeStmts = doc.findall('.//{*}include')
    for includeStmt in includeStmts:
        par = getParent(doc,includeStmt)
        par.remove(includeStmt)

@debugDecor
def deleteNonColumnCalls(doc, simplify=False):
    """
    Remove Routines that compute with different vertical columns not needed for AROME
    MODE_ROTATE_WIND, UPDATE_ROTATE_WIND
    If Simplify is True, also remove all variables only needed for these calls
    :param doc: etree to use
    :param simplify : if True, remove variables that are now unused
    """
    removeCall(doc, 'ROTATE_WIND', None, simplify=simplify)
    removeCall(doc, 'UPDATE_ROTATE_WIND', None, simplify=simplify)
    removeCall(doc, 'BL_DEPTH_DIAG_3D', None, simplify=simplify)
    removeCall(doc, 'TM06_H', None, simplify=simplify)
    removeCall(doc, 'TURB_HOR_SPLT', None, simplify=simplify)



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
def addStack(doc, declarationAllocType, model):
    """
    Add specific allocations of local arrays on the fly for GPU
    :param doc: etree to use
    :param declarationAllocType: string of the template for declaration + allocation
    Example for Mesonh : "{kind}, DIMENSION({doubledotshape}), ALLOCATABLE :: {name}#ALLOCATE({name}({shape}))"
    for Philippe's version before CPP : "temp ({kind}, {name}, ({shape}))#alloc ({name})"
    (not tested) for Philippe's version after CPP  : "{kind}, DIMENSION ({shape}) :: {name}; POINTER(IP_##{name}##_, {name})#IP_##{name}##_=YLSTACK%L;YLSTACK%L=YLSTACK%L+KIND({name})*SIZE({name});IF(YLSTACK%L>YLSTACK%U)CALL SOF(__FILE__, __LINE__)"
    :param model : 'MESONH' or 'AROME' for specific objects related to the allocator or stack
    """
    def getShape(var):
        """
        Return the shape of var as text
        :param var: variable element from getVarList
        return arrayTxt (e.g. 'D%NIJT,D%NKT') and doubledotshape (e.g. ':,:')
        """
        # Array dimensions
        arrayTxt = ''
        nb_dim=0
        for el in var['as']:
            if el[0] is None:
                arrayTxt+=str(el[1])+','
                nb_dim += 1
        tempArrayTxt = arrayTxt
        doubledotshape = ":,"*nb_dim
        tempdoubledotshape = doubledotshape
        if arrayTxt[-1] == ',': # remove last ',' if present
            tempArrayTxt = arrayTxt[:-1]
            tempdoubledotshape = doubledotshape[:-1]
        arrayTxt = tempArrayTxt
        doubledotshape = tempdoubledotshape
        return arrayTxt, doubledotshape

    tempdeclType, tempallocType = declarationAllocType.split('#')[0], declarationAllocType.split('#')[1]
    locations  = getScopesList(doc,withNodes='tuple')
    for loc in locations:
        if 'sub:' in loc[0]: # Do not add stack to MODULE object, but only to SUBROUTINES
            scopepath = getScopePath(doc,loc[1])
            varList = getVarList(doc,scopepath)
            # Look for all local arrays only (and not PARAMETER variables)
            localArrays, varListToRemove = [], []
            for var in varList:
                if not var['arg'] and var['as']:
                    parameterVar = False
                    for asx in var['asx'][0]: #remove PARAMETER variable (containing literal-E in asx
                        if asx and 'literal-E' in asx:
                            parameterVar = True
                    if not parameterVar:
                        localArrays.append(var)
                        varListToRemove.append([scopepath,var['n']])
                    
            # Remove the current declaration form
            removeVar(doc,varListToRemove,simplify=False)
            
             # Get the index of the last declaration object
            declStmts = loc[1].findall('.//{*}T-decl-stmt')
            par = getParent(loc[1],declStmts[-1])
            allsiblings = par.findall('./{*}*')
            index = allsiblings.index(declStmts[-1])
    
            # Handle text declarations
            for var in localArrays:
                index += 1
                declType = tempdeclType.replace('{kind}',var['t'])
                declType = declType.replace('{name}',var['n'])
                arrayTxt, doubledotshape = getShape(var)
                declType = declType.replace('{doubledotshape}',doubledotshape)
                declType = declType.replace('{shape}',arrayTxt)
                fortranSource = "SUBROUTINE FOO598756\n "+declType+" \nEND SUBROUTINE"
                _, xmlTypeRoutine = fortran2xml(fortranSource)
                declTypeXML = xmlTypeRoutine.find('.//{*}T-decl-stmt')
                if declTypeXML is None:
                    declTypeXML = xmlTypeRoutine.find('.//{*}broken-stmt') # For the non-conventional declaration type such as temp ()
                par.insert(index, declTypeXML)
            lastIndexDecl = index
            
            # Handle text allocations
            for var in localArrays:
                index += 1
                allocType = tempallocType.replace('{name}',var['n'])
                arrayTxt, doubledotshape = getShape(var)
                allocType = allocType.replace('{shape}',arrayTxt)
                fortranSource = "SUBROUTINE FOO598756\n "+allocType+" \nEND SUBROUTINE"
                _, xmlTypeRoutine = fortran2xml(fortranSource)
                allocTypeXML = xmlTypeRoutine.find('.//{*}allocate-stmt')
                if allocTypeXML is None:
                    allocTypeXML = xmlTypeRoutine.find('.//{*}broken-stmt') # For the non-conventional declaration type such as temp ()
                par.insert(index, allocTypeXML)
        
            if len(localArrays)>0:
                if model == 'AROME':
                    addDeclStackAROME(doc, loc, lastIndexDecl)
                #elif model == 'MESONH':
                    #addDeclStackMESONH() # TODO regarging specifications of LAERO Open-ACC branch
            
@debugDecor
def addDeclStackAROME(doc, loc, lastIndexDecl=0):
    """
    Prepare objects STACK_MOD, YLSTACK and YDSTACK for addStack
    :param doc: etree to use
    :param loc: scope to add the specific objets
    :param lastIndexDecl: index of the last declaration object added that may not be recognized by fxtran as a declaration (e.g. in case of "temp" statement)
    """
    addVar(doc,[[loc[0],'YDSTACK','TYPE(STACK) :: YDSTACK, YLSTACK',-1]])
    addModuleVar(doc, [[loc[0], 'STACK_MOD',None]])
        
    # Add include stack.h after the USE STACK_MOD
    modules = loc[1].findall('.//{*}use-stmt/{*}module-N/{*}N/{*}n')
    fortranSource = "SUBROUTINE FOO598756\n #include \"stack.h\" \nEND SUBROUTINE"
    _, xmlIncludeStack = fortran2xml(fortranSource)
    for mod in modules:
        if alltext(mod) == 'STACK_MOD':
            par = getParent(loc[1], mod, level=4)
            index = par[:].index(getParent(loc[1], mod, level=3))
            par.insert(index+1, xmlIncludeStack.find('.//{*}cpp'))

    # Add !$acc routine (ROUTINE_NAME) seq after subroutine-stmt
    routineName = loc[1].find('.//{*}subroutine-stmt/{*}subroutine-N/{*}N/{*}n')
    fortranSource = "SUBROUTINE FOO598756\n !$acc routine ("+ alltext(routineName) + ") seq \nEND SUBROUTINE"
    _, xmlAccRoutine = fortran2xml(fortranSource)
    routineStmt = loc[1].find('.//{*}subroutine-stmt')
    par = getParent(loc[1], routineStmt)
    index = par[:].index(routineStmt)
    par.insert(index+1, xmlAccRoutine.find('.//{*}C'))
    
    # in case, called by checkStackArginCall, get lastIndexDecl:
    if lastIndexDecl == 0:
        declStmts = loc[1].findall('.//{*}T-decl-stmt')
        par = getParent(loc[1],declStmts[-1])
        allsiblings = par.findall('./{*}*')
        lastIndexDecl = allsiblings.index(declStmts[-1])
    # Add YLSTACK = YDSTACK        
    fortranSource = "SUBROUTINE FOO598756\n YLSTACK=YDSTACK \nEND SUBROUTINE"
    _, xml = fortran2xml(fortranSource)
    par.insert(lastIndexDecl+5,xml.find('.//{*}a-stmt')) # 5 corresponds to 1 for new line, 4 for the 4 lines added in addDeclStackAROME
    
    #Update subroutines_wth_stack.txt
    f = open("subroutines_wth_stack.txt", "a")
    try: 
        f.write(loc[0].split('/sub:')[1] + '\n')
    except:
        f.write(loc[0].split('sub:')[1] + '\n') # Case for main call (rain_ice, shallow, turb, ice_adjust) that are not in modules
    f.close()

@debugDecor
def checkStackArginCall(doc):
    """
    Check in all CALL statements if YLSTACK must be present. It is based on a first call of addDeclStackMODEL that writes in subroutines_wth_stack.txt
    all the routines that need the STACK object
    :param doc: etree to use
    """
    f = open("subroutines_wth_stack.txt", "r")
    lines = f.readlines()
    routinesWthStack = []
    for l in lines:
        routinesWthStack.append(l.replace('\n',''))
    f.close()
    
    # Build <f:arg><f:arg-N n="YDSTACK"><f:k>YDSTACK</f:k></f:arg-N>=<f:named-E><f:N><f:n>YLSTACK</f:n></f:N></f:named-E></f:arg>
    fortranSource = "SUBROUTINE FOO598756\n CALL FOO(YLSTACK=YDSTACK) \nEND SUBROUTINE"
    _, xml = fortran2xml(fortranSource) 
    YLSTACKarg = xml.find('.//{*}arg')
    YLSTACKarg.text = ','
                   
    locations  = getScopesList(doc,withNodes='tuple')
    for loc in locations:
        if 'sub:' in loc[0]: # Do not work on MODULE scope
            addedOnce = False # becomes True as soon as YLSTACK has been added at least once, to check later if the declaration of YLSTACK is already present in the scope
            callStmts = loc[1].findall('.//{*}call-stmt')
            for callStmt in callStmts:
                routineName = callStmt.find('.//{*}procedure-designator/{*}named-E/{*}N/{*}n')
                if alltext(routineName) in routinesWthStack:
                    lastArg = callStmt.findall('.//{*}arg-spec/{*}arg/{*}named-E')[-1]
                    if not alltext(lastArg) == 'YDSTACK': # If the last argument is not YLSTACK, then add it
                       # Append YLSTACKarg as the last argument of the calling statement
                       callStmt_args = callStmt.find('.//{*}arg-spec')
                       callStmt_args.append(YLSTACKarg)
                       addedOnce = True
            # Check if the declaration of YLSTACK is already present in the scope
            if addedOnce:
                declStmts = loc[1].findall('.//{*}T-decl-stmt//{*}EN-decl')
                declStmtsTxt = []
                for el in declStmts:
                    declStmtsTxt.append(alltext(el))
                if 'YLSTACK' not in declStmtsTxt:
                    addDeclStackAROME(doc,loc)
            
@debugDecor
def applyCPP(doc, Lkeys=['REPRO48']):
    """
    Apply #ifdef for each CPP-key in Lkeys
    WARNING : this functions is in a basic form. It handles only :
        #ifdef <KEY>
        #ifndef <KEY>
        #else
        #endif
    :param doc: etree to use
    :param Lkeys: list of keys to apply CPP-like transformation of ifdef
    """
    def removeInBetweenCPP(par):
        # Remove everything between #cpp block
        for j,el in enumerate(par[index+1:]): #from the first element after #ifdef or #ifndef
            if j==0 and el.tag.endswith('}cpp'): #case with empty ifdef
                break
            if not el.tag.endswith('}cpp'):
                par.remove(el)
            else:
                break
    ifdefKeys = doc.findall('.//{*}cpp')
    toRemove = []
    removeElse = False
    for cppNode in ifdefKeys:
        par = getParent(doc,cppNode)
        if par is not None:
            allsiblings = par.findall('./{*}*')
            index = allsiblings.index(cppNode)
            cppTxt=alltext(cppNode).replace('#','') #e.g. ['ifndef', 'PHYEXMERGE']
            cppTxt=cppTxt.split(' ')
            if len(cppTxt) == 1: cppTxt.append('') #case #else and #endif
            if cppTxt[0] == 'ifdef':
                if cppTxt[1] in Lkeys:
                    # Keep the block between #ifdef and next cppNode
                    removeElse = True
                    toRemove.append(cppNode)
                else:
                    # Remove everything between #ifdef and next cppNode, Keep Else block
                    removeInBetweenCPP(par)
                    removeElse = False
            elif cppTxt[0] == 'ifndef':
                if cppTxt[1] in Lkeys:
                    # Remove everything between #ifndef and next cppNode
                    removeInBetweenCPP(par)
                    removeElse = False
                    toRemove.append(cppNode)
                else:
                    # Keep the block between #ifndef and next cppNode
                    removeElse = True
                    toRemove.append(cppNode)
            elif cppTxt[0] == 'else' and removeElse:
                removeInBetweenCPP(par)
                toRemove.append(cppNode)
            elif cppTxt[0] == 'endif':
                toRemove.append(cppNode)
            else:
                pass         
    # Remove all cpp keys
    for cppNode in toRemove:         
        par = getParent(doc,cppNode)
        par.remove(cppNode)

@debugDecor
def removeArraySyntax(doc,expandDoLoops=False,expandWhere=False):
    """
    Remove all the array syntax and replace it by do loops
    If index are present in fortran code, e.g. A(IIB:IIE), the DO loops index limits are kept
    If index are not present, e.g. A(:,;), the DO loops index limits are the complete size of the array
    TODO: If index and brakets are not repsent A = B + C, the transformation is not applied (yet)
    TODO: If the array-syntax is present in a DO WHILE loop, the transformation is not applied (yet).
    TODO: Double nested WHERE are not transformed
    :param doc: etree to use
    :param expandDoLoops: if True, expand Do loops
    :param expandWhere: if True, expand Where
    """   
    locations  = getScopesList(doc,withNodes='tuple')
    locations.reverse()
    for loc in locations:
        scopepath = getScopePath(doc,loc[1])
        varList = getVarList(doc,scopepath)
        if len(varList) > 0: # = 0 in head of module scope
            locNode = loc[1]
            varArray,varArrayNamesList,localIntegers, loopIndexToCheck = [], [], [], []
            Node_opE = locNode.findall('.//{*}a-stmt')
            whereConstruct = locNode.findall('.//{*}where-construct')
#            arginCalls = locNode.findall('.//{*}call-stmt/{*}arg-spec/{*}arg')
            for var in varList:
                if not var['as'] and var['t'] == 'INTEGER' and not var['arg']:
                    localIntegers.append(var['n'])
                if var['as']:
                    varArray.append(var)
                    varArrayNamesList.append(var['n'])            
            if expandWhere: 
                for node_where in whereConstruct:
                    inDoWhile = checkInDoWhile(locNode,node_where)
                    if not inDoWhile:
                        expandWhereConstruct(doc, node_where, locNode, varArrayNamesList, varArray)
            if expandDoLoops:
                for node_opE in Node_opE:
                    inDoWhile = checkInDoWhile(locNode,node_opE)
                    if not inDoWhile:
                        # Expand single-line if-statement if any in front of an a-stmt with section-subscript (locally)
                        if getParent(doc,node_opE,level=2).tag.endswith('}if-stmt') and len(node_opE.findall('.//{*}section-subscript')) > 0: # level=1 is action-stmt, need level=2 to get the if-stmt node
                            changeIfStatementsInIfConstructs(doc, singleItem=getParent(doc,node_opE,level=2))
                        loopIndexToCheck = expandArrays(doc, node_opE, locNode, varArrayNamesList, varArray, loopIndexToCheck)
                # Check loop index presence
                for loopIndex in loopIndexToCheck:
                    if loopIndex not in localIntegers:
                        addVar(doc,[[loc[0],loopIndex,'INTEGER :: '+loopIndex,None]]) #TODO minor issue: le 2e argument ne semble pas s'appliquer ? il faut ajouter loopIndex dans le 3e argument pour effectivement l'ecrire

@debugDecor
def inlineContainedSubroutines(doc):
    """
    Inline all contained subroutines in the main subroutine
    Steps :
        - Identify contained subroutines
        - Inline within contained subroutines look for all CALL statements, check if it is a containted routines; if yes, inline :
                - delete the original call statement and insert the inlined node
        - Inline the main routine
        - Delete the containted routines
    :param doc: xml fragment containing main and contained subroutine
    """
    def addExplicitArrayBounds(node, callNode, varList):
        """
        Add explicit arrays bounds (for further arrays expansion) in Call arguments.
        Used in call of elemental subroutine
        :param node: xml node to work on (the calling subroutine)
        :param callNode: xml node of the call statement
        :param varList: var list of node
        """
        varArray,varArrayNamesList,localIntegers = [], [], []
        for var in varList:
            if not var['as'] and var['t'] == 'INTEGER' and not var['arg']:
                localIntegers.append(var['n'])
            if var['as']:
                varArray.append(var)
                varArrayNamesList.append(var['n'])
        subs=callNode.findall('.//{*}section-subscript')
        for sub in subs:
            if alltext(sub) == ':': # ':' alone
                varName = alltext(getParent(callNode, sub, level=4).find('.//{*}N/{*}n'))
                convertColonArrayinDim(sub, callNode, varArrayNamesList, varArray, varName)
                
    locations  = getScopesList(doc,withNodes='tuple')
    containedRoutines = {}
    # Inline contained subroutines : look for sub: / sub:
    for loc in locations:
        if loc[0].count('sub:') >= 2:
            containedRoutines[alltext(loc[1].find('.//{*}subroutine-N/{*}N/{*}n'))] = loc[1]
    # Start by nested contained subroutines call, and end up with the last index = the main subroutine to treat 
    locations.reverse()
    for loc in locations: # For all subroutines (main + contained)
        if loc[0].count('sub:') >= 1:
            callStmtsNn = loc[1].findall('.//{*}call-stmt/{*}procedure-designator/{*}named-E/{*}N/{*}n')
            for callStmtNn in callStmtsNn: # For all CALL statements
                mainVarList = getVarList(doc,getScopePath(doc,loc[1]))
                for containedRoutine in containedRoutines:
                    if alltext(callStmtNn) == containedRoutine: # If name of the routine called = a contained subroutine
                        callStmt = getParent(doc,callStmtNn,level=4)
                        par = getParent(doc,callStmt)
                        if par.tag.endswith('}action-stmt'): # Expand the if-construct if the call-stmt is in a one-line if-construct
                            changeIfStatementsInIfConstructs(doc,getParent(doc,par))
                        # Specific case for ELEMENTAL subroutines: need to add explicit arrays bounds for further arrays expansion
                        prefix = containedRoutines[containedRoutine].findall('.//{*}prefix')
                        if len(prefix)>0:
                            if 'ELEMENTAL' in alltext(prefix[0]): 
                                arrayRincallStmt = callStmt.findall('.//{*}array-R') # If the call-stmt is already done in a loop such as any arrayR is present
                                if len(arrayRincallStmt) > 0:
                                    addExplicitArrayBounds(loc[1], callStmt, mainVarList)
                        #
                        subContaintedVarList = getVarList(doc,getScopePath(doc,containedRoutines[containedRoutine]))
                        # Inline
                        nodeInlined, localVarToAdd = inline(doc, containedRoutines[containedRoutine], callStmt, subContaintedVarList, mainVarList)
                        # Add local var to main routine
                        for var in localVarToAdd:
                            arrayTxt=''
                            if var['as']:
                                arrayTxt=', DIMENSION('
                                for el in var['as']:
                                    if el[0] is None:
                                        arrayTxt+=str(el[1])+','
                                tempArrayTxt = arrayTxt
                                if arrayTxt[-1] == ',': # remove last ',' if present
                                    tempArrayTxt = arrayTxt[:-1]
                                arrayTxt = tempArrayTxt
                                arrayTxt+=')'
                            addVar(doc,[[loc[0], var['n'], var['t']+arrayTxt+' :: '+var['n'], None]])
                            
                        # Specific case for ELEMENTAL subroutines: expand arrays within the inlined code (only E-2 arrays)
                        if len(prefix)>0:
                            if 'ELEMENTAL' in alltext(prefix[0]) and len(arrayRincallStmt) > 0:
                                loopIndexToCheck = []
                                Node_aStmt = nodeInlined.findall('.//{*}a-stmt')
                                doToBuild = []
                                doI=0
                                # Loop for the Do loops to build
                                while len(doToBuild) == 0:
                                    doToBuild = E2StmtToDoStmt(Node_aStmt[doI])
                                    doToBuild.reverse() # To write first loops from the latest index (K or SV)
                                    for dostmt in doToBuild:
                                        if alltext(dostmt.findall('.//{*}do-V/{*}named-E/{*}N/{*}n')[0]) not in loopIndexToCheck:
                                            loopIndexToCheck.append(alltext(dostmt.findall('.//{*}do-V/{*}named-E/{*}N/{*}n')[0]))
                                    doI += 1
                                # Replace the array-syntax by loop index in every a-stmt node
                                ArrayRLT = nodeInlined.findall('.//{*}R-LT') # Need to take R-LT (and not a-stmt) because some array-R are not in a-stmt such as IF(A(:,:)) THEN...
                                for node_opE in ArrayRLT: 
                                    placeArrayRtoparensR(doc, nodeInlined, node_opE) 
                                # Place the Do loops on top of the inlined routine
                                doToBuild[-1][-1].insert(-1,nodeInlined) # place the nodeInlined in the last object (inner loop)
                                # Insert nested DO-loops into the previous object of doToBuild until reach the 0e object
                                for i in range(len(doToBuild)-1):
                                    doToBuild[len(doToBuild)-1-(i+1)][0].insert(3,doToBuild[len(doToBuild)-1-i])
                                nodeInlined = doToBuild[0]
                            
                        # Remove call statement of the contained routines
                        allsiblings = par.findall('./{*}*')
                        index = allsiblings.index(callStmt)
                        par.remove(callStmt)
                        nodeInlined.tail='\n' #to avoid extra spaces in tail for #ifdef #else statements
                        par.insert(index,nodeInlined)
                        exit
                # Update containedRoutines
                containedRoutines = {}
                # Inline contained subroutines : look for sub: / sub:
                for locs in locations:
                    if locs[0].count('sub:') >= 2:
                        containedRoutines[alltext(locs[1].find('.//{*}subroutine-N/{*}N/{*}n'))] = locs[1]

    #Remove original containted subroutines and 'CONTAINS' statement
    #contains = doc.find('.//{*}contains-stmt')
    #if contains is not None:
    #    par = getParent(doc,contains)
    #    par.remove(contains)
    for loc in locations:
        if loc[0].count('sub:') >= 2:
            par = getParent(doc,loc[1])
            par.remove(loc[1])


def inline(doc, subContained, callStmt, subContaintedVarList, mainVarList):
    """
    Inline a subContainted subroutine
    Steps :
        - copy the subContained node
        - remove everything before the declarations variables and the variables declarations
        - from the callStmt, replace all the arguments by their names 
        - return the node to inline and the local variables to be added in the calling routine
    :param doc: xml fragment containing main and contained subroutine
    :param subContained: xml fragment corresponding to the sub: to inline
    :param callStmt : the call-stmt to get the values of the intent args
    :param subContaintedVarList: var list of the subContained subroutine
    :param mainVarList: var list of the main (calling) subroutine
    """
    def setPRESENTbyTrue(node, var, varsNamedNn):
        """
        Replace PRESENT(var) by .True. on node if var is found in varsNamedN
        :param node: xml node to work on (a contained subroutine)
        :param var: string of the name of the optional variable to check
        :param varsNamedNn : list of xml element named-E/N/n of node
        """
        for el in varsNamedNn: #On all named-E, check for the current arg to replace
                    if el.text == var:
                        potentialPresentNode = getParent(node, el, level=8)
                        if potentialPresentNode: # level 8 parent may not exit
                            presentNode = potentialPresentNode.findall('.//{*}named-E/{*}N/{*}n')
                            if len(presentNode) > 0:
                                if alltext(presentNode[0]).upper() == 'PRESENT': #Replace 'PRESENT()' by True
                                    TrueNode = ET.Element('{http://fxtran.net/#syntax}literal-E')
                                    TrueNode.text = ' .TRUE. '
                                    tail = potentialPresentNode.tail
                                    TrueNode.tail = tail
                                    par = getParent(node,potentialPresentNode)
                                    allsiblings = par.findall('./{*}*')
                                    index=allsiblings.index(potentialPresentNode)
                                    par.remove(potentialPresentNode)
                                    par.insert(index, TrueNode)
                                    
    
    # Deep copy the object to possibly modify the original one multiple times
    node = copy.deepcopy(subContained)
    nodeToRemove, varPermutted = [], []
    declStmtFound = False # used to remove everything before a variable declaration
    
    # Get local variables that are not present in the main routine for later addition
    localVarToAdd = []
    for var in subContaintedVarList:
        if not var['arg']: # for local variables only
            localVarToAdd.append(var)
            for mainVar in mainVarList:
                if var['n'] == mainVar['n']: # the local variable name in the subcontained is already present in the main routine, we do not add it
                    localVarToAdd.remove(var)

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
        
    # List of Arguments of the contained subroutine
    varsRoutine = [] 
    argsN = subContained.findall('.//{*}subroutine-stmt/{*}dummy-arg-LT/{*}arg-N')
    for args in argsN:
        varsRoutine.append(alltext(args))
    # List of Optional arguments
    varsOpt = []
    for var in subContaintedVarList:
        if var['arg']:
            if var['opt']: varsOpt.append(var['n'])
    # Permute args
    varsCall = callStmt.findall('.//{*}arg')
    varsNamedNn = node.findall('.//{*}named-E/{*}N/{*}n')
    for i,var in enumerate(varsCall): # For each Call arguments
        if var.find('.//{*}arg-N'): # Check if arg is of type VAR=value
            n = var.findall('.//{*}n')
            if alltext(var.find('.//{*}k')) in varsOpt: # First, check if the permutted argument is optional. If true, look for PRESENT(var) and replace it by True
                setPRESENTbyTrue(node, alltext(var.find('.//{*}k')), varsNamedNn)
                varsNamedNn = node.findall('.//{*}named-E/{*}N/{*}n') #Update the list of named-E for later
            if len(n):    
                if alltext(n[0]) == alltext(var.find('.//{*}k')):
                    if alltext(var.find('.//{*}k')) not in varPermutted: varPermutted.append(alltext(var.find('.//{*}k')))
                    pass # Do nothing, var names are already matching
                else:
                    for el in varsNamedNn:
                        if el.text == alltext(var.find('.//{*}k')):
                            subs=var.findall('.//{*}section-subscript')
                            onlyFullArray = False
                            if el.text not in varPermutted: varPermutted.append(el.text)
                            for sub in subs:
                                if alltext(sub) == ':': # ':' alone
                                    onlyFullArray = True #if at least one : is found
                                    pass
                                else: # but another dimension is not :
                                    onlyFullArray = False
                                    exit
                            if var.find('.//{*}R-LT') and not onlyFullArray: #variable with array or component, e.g. D%NIJB or ZA(JIJ,:IKB) but not only ':' such as ZA(:,:)
                                par = getParent(node,el)
                                allsiblings = par.findall('./{*}*')
                                index=allsiblings.index(el)
                                tail = el.tail
                                par.remove(el)
                                if var.find('.//{*}R-LT'): #variable with array or component, e.g. D%NIJB
                                    copyvar = copy.deepcopy(var.find('.//{*}named-E'))
                                else: # classic variable
                                    copyvar = copy.deepcopy(n[0])
                                copyvar.tail = tail
                                par.insert(index,copyvar)
                            else: # only copy the name of the variable (without ':')
                                el.text = alltext(n[0])
            else: # the argument given is a string-E or literal-E
                for el in varsNamedNn:
                        if el.text == alltext(var.find('.//{*}k')):
                            if el.text not in varPermutted: varPermutted.append(el.text)
                            par = getParent(node,el)
                            allsiblings = par.findall('./{*}*')
                            index=allsiblings.index(el)
                            par.remove(el)
                            par.insert(index,copy.deepcopy(var[1])) # 0 is the arg-N ; 1 is the value of arg (string-E or literal-E)
        else: # Classic argument (only the var name is given)
            n = var.findall('.//{*}n')
            if varsRoutine[i] in varsOpt: # First, check if the permutted argument is optional. If true, look for PRESENT(var) and replace it by True
                setPRESENTbyTrue(node, varsRoutine[i], varsNamedNn)
                varsNamedNn = node.findall('.//{*}named-E/{*}N/{*}n') #Update the list of named-E for later
            if len(n):
                if alltext(var) == varsRoutine[i]:
                    if varsRoutine[i] not in varPermutted: varPermutted.append(varsRoutine[i])
                    pass # Do nothing, var names are already matching
                else:
                    for el in varsNamedNn:
                        if el.text == varsRoutine[i]:
                            if el.text not in varPermutted: varPermutted.append(el.text)
                            subs=var.findall('.//{*}section-subscript')
                            onlyFullArray = False
                            for sub in subs:
                                if alltext(sub) == ':': # ':' alone
                                    onlyFullArray = True #if at least one : is found
                                    pass
                                else: # but another dimension is not :
                                    onlyFullArray = False
                                    exit
                            if not onlyFullArray: #variable with array or component, e.g. ICEP%TOTO(JIJ,:IKB) or ZA(JIJ,:IKB)
                                if getParent(node,el, level=2).findall('.//{*}R-LT'): # if the replaced variable already has an array information, we replace all subs ':' by the target variable's 'element'
                                    el.text = alltext(n[0]) # name of the variable is permutted
                                    if getParent(node,el, level=2).findall('.//{*}R-LT/{*}parens-R'): # the target is of type number
                                        elementLT = getParent(node,el, level=2).find('.//{*}parens-R/{*}element-LT')
                                        for j,sub in enumerate(subs):
                                            if alltext(sub) == ':':
                                                pass # do nothing, the target subs is kept
                                            else:
                                                new_el = ET.Element('{http://fxtran.net/#syntax}element')
                                                if j > 0:
                                                    new_el.text=','
                                                new_el.insert(0,sub.find('.//{*}lower-bound')[0])
                                                elementLT.insert(j,new_el)
                                    else: # the target is an array-R
                                        subsLT = getParent(node,el, level=2).find('.//{*}section-subscript-LT')
                                        for j,sub in enumerate(subs):
                                            if alltext(sub) == ':':
                                                pass # do nothing, the target subs is kept
                                            else:
                                                if j > 0:
                                                    sub.text=','
                                                subsLT.insert(j,sub)   
                                else: # the replaced variable is a simple namedE or component eg. ZA or D%NIJT
                                    par = getParent(node,el)
                                    allsiblings = par.findall('./{*}*')
                                    index=allsiblings.index(el)
                                    tail = el.tail # save the tail of the replaced object
                                    par.remove(el)
                                    copyvar = copy.deepcopy(var.find('.//{*}named-E'))
                                    copyvar.tail = tail
                                    par.insert(index,copyvar)
                            elif var.find('.//{*}R-LT/{*}component-R'): #R-LT/component-R with full array (:) such as ICEP%TOTO(:)
                                par = getParent(node,el)
                                allsiblings = par.findall('./{*}*')
                                index=allsiblings.index(el)
                                tail = el.tail # save the tail of the replaced object
                                par.remove(el) # remove the target
                                copyvar = copy.deepcopy(var.find('.//{*}named-E'))
                                copyvar.tail = tail
                                arrayR=copyvar.find('.//{*}array-R') #remove the array-R (:)
                                parArrayR = getParent(copyvar,arrayR)
                                parArrayR.remove(arrayR)
                                par.insert(index,copyvar)    
                            else: # R-LT/array Full array : only copy the name of the variable (without ':') such as ZA(:,:)
                                el.text = alltext(n[0])
            else: # var is literal-E or string-E (only possible as INTENT IN)
                for el in varsNamedNn:
                        if el.text == varsRoutine[i]:
                            if el.text not in varPermutted: varPermutted.append(el.text)
                            par = getParent(node,el)
                            allsiblings = par.findall('./{*}*')
                            index=allsiblings.index(el)
                            tail = el.tail
                            par.remove(el)
                            copyvar = copy.deepcopy(var)
                            copyvar.tail = tail
                            par.insert(index,copyvar)

    # Remove all statements related to optional arguments not given by the CALL statement in the main routine to the inlined-contained routine
    for var in subContaintedVarList:
        if var['arg']:
            if var['opt'] and var['n'] not in varPermutted:
                namedEs = node.findall('.//{*}named-E/{*}N/{*}n')
                for namedE in namedEs:
                    if namedE.text == var['n']:
                        par = getParent(node,namedE,level=4)
                        # The deletion of a variable is complex and depends on the context
                        # Context 1 : an a-stmt of type E1 = E2
                        level = 4
                        par = getParent(node, namedE, level=level)
                        while par:
                            if par.tag.endswith('}a-stmt'):
                                getParent(node, par).remove(par)
                            else:
                                level +=1
                            par = getParent(node, namedE, level=level)
                        # Context 2 : an if-stmt of type  : IF(PRESENT(variable) ...)
                        level = 4
                        par = getParent(node, namedE, level=level)
                        while par:
                            if par.tag.endswith('}if-stmt'):
                                getParent(node, par).remove(par)
                            else:
                                level +=1
                            par = getParent(node, namedE, level=level)
                        # Context 3 : an if-block of type  : IF(PRESENT(variable) THEN...
                        level = 4
                        par = getParent(node, namedE, level=level)
                        while par:
                            if par.tag.endswith('}if-block'):
                                getParent(node, par).remove(par)
                            else:
                                level +=1
                            par = getParent(node, namedE, level=level)
    return node, localVarToAdd
    
@debugDecor            
def removeIJLoops(doc):
    """
    ComputeInSingleColumn :
    - Remove all Do loops on JI and JJ for preparation to compute on Klev only
    - Initialize former indexes JI,JJ,JIJ to first array element : JI=D%NIB, JJ=D%NJB, JIJ=D%NIJB
    - Replace (:,*) on I,J/IJ dimension on argument with explicit (:,*) on CALL statements:
        e.g. CALL FOO(D, A(:,JK,1), B(:,:)) ==> CALL FOO(D, A(JIJ,JK,1), B(:,:)) only if the target argument is not an array
    WARNING : executed transformed-code will work only if inlineContainedSubroutines is applied first
    :param doc: xml fragment to search for variable usage
    """
    locations  = getScopesList(doc,withNodes='tuple')
    locations.reverse()
    locations = [item for item in locations if 'func:' not in item[0]] # Remove elemental function (essentially FWSED from ice4_sedimentation_stat)
    indexToCheck = {'JI':'D%NIB','JJ':'D%NJB','JIJ':'D%NIJB'}
    for loc in locations:
        localNode = loc[1]
        # Remove all Do loops on JI and JJ for preparation to compute on Klev only 
        doNodes = localNode.findall('.//{*}do-construct')  
        indexRemoved = []
        doNodes.reverse()
        # Look for all do-nodes, check if the loop-index is one of the authorized list (indexToCheck), if found, removes it
        for doNode in doNodes:
            loopIndex = doNode.findall('.//{*}do-stmt/{*}do-V/{*}named-E/{*}N/{*}n')
            for loopI in loopIndex:
                if alltext(loopI) in indexToCheck.keys():
                    endDo = getParent(localNode,loopI,level=5).findall('.//{*}end-do-stmt') # loopI's parent level=5 is a do-construct
                    removeStmtNode(localNode, getParent(localNode,loopI,level=4), False, False) #TODO: the call to removeStmtNode alone adds an empty line after the END-DO stmt + add extra spaces to first children stmt (and end-stmt). Try on turb.F90
                    getParent(localNode,endDo[-1]).remove(endDo[-1]) #remove end-do statement, the last END-DO corresponds to the first DO LOOP we remove, in case of nested DO-loops
                    if alltext(loopI) not in indexRemoved:
                        indexRemoved.append(alltext(loopI))
        # Replace (:,*) on I,J/IJ dimension on argument with explicit (:,*) on CALL statements (only if * are literal-E or string-E) ==> the target arg is not an array
        # Examples : PRM(:,JK,1) becomes PRM(JIJ,JK,1) ; but PRM(:,:,1) is not changed as the INTENT arg is an array, so the call argument is keeping as an array.
        if 'sub:' in loc[0]:
            varArray, varArrayNamesList, localIntegers = [], [], []
            varList = getVarList(doc, loc)
            for var in varList:
                if not var['as'] and var['t'] == 'INTEGER' and not var['arg']:
                    localIntegers.append(var['n'])
                if var['as']:
                    varArray.append(var)
                    varArrayNamesList.append(var['n'])
            calls = loc[1].findall('.//{*}call-stmt')
            for call in calls:
                namedEs = call.findall('.//{*}named-E')
                for namedE in namedEs:
                    subs=namedE.findall('.//{*}section-subscript')
                    if '%' in alltext(namedE):
                        #print("WARNING: " + alltext(namedE) + " is assumed not to be on klon dimensions, otherwise, do not use type variables")
                        pass
                    else:
                        if len(subs) > 1: # sub = 1 is treated on reDimKlonArrayToScalar
                            nb_subarray = 0  # Count all arrays in subs to check if there is no more than one on horizontal dimension (we assume horizontal dim. are packed in calling statements)
                            for sub in subs:
                                if ':' in alltext(sub):  # ':' alone or partial array such as IKB: or :IKE
                                    nb_subarray += 1
                            if nb_subarray == 1:
                                for i,sub in enumerate(subs):
                                    if alltext(sub) == ':': # ':' alone
                                        varName = alltext(namedE.find('.//{*}n'))
                                        ind=varArrayNamesList.index(varName)
                                        upperBound = str(varArray[ind]['as'][i][1])
                                        if upperBound == 'D%NIJT' or upperBound == 'D%NIT' or upperBound == 'D%NJT':
                                            sub.text = '' # remove the ':'
                                            lowerBound = ET.Element('{http://fxtran.net/#syntax}lower-bound')
                                            loopIndex = 'J' + upperBound.replace('D%N','')
                                            loopIndex = loopIndex.replace('T','')
                                            lowerBound.insert(0,createNamedENn(loopIndex))
                                            sub.insert(0,lowerBound)
                                            if loopIndex not in indexRemoved:
                                                indexRemoved.append(loopIndex)
        # Initialize former indexes JI,JJ,JIJ to first array element : JI=D%NIB, JJ=D%NJB, JIJ=D%NIJB
        if len(indexRemoved) > 0:
            lastDecl = localNode.findall('.//{*}T-decl-stmt')[-1] # The case where no T-decl-stmt is found, is not handled (it should not exist !)
            par = getParent(localNode,lastDecl)
            allsiblings = par.findall('./{*}*')
            index=allsiblings.index(lastDecl)
            for i,indexToAdd in enumerate(indexRemoved):
                #Statement building
                 fortranSource = "SUBROUTINE FOO598756\n " + indexToAdd + "=" + indexToCheck[indexToAdd] + "\nEND SUBROUTINE"
                 _, xml = fortran2xml(fortranSource)
                 newIndex = xml.find('.//{*}a-stmt')
                 newIndex.text = '\n'
                 par.insert(index+1,newIndex)
        # Check loop index presence at declaration of the scope
        for loopIndex in indexRemoved:
            if loopIndex not in localIntegers:
                addVar(doc,[[loc[0],loopIndex,'INTEGER :: '+loopIndex,None]]) #TODO minor issue: le 2e argument ne semble pas s'appliquer ? il faut ajouter loopIndex dans le 3e argument pour effectivement l'ecrire
  
@debugDecor
def removePHYEXUnusedLocalVar(doc, scopePath=None, excludeList=None, simplify=False):
    """
    Remove unused local variables (dummy and module variables are not suppressed)
    This function is identical to variables.removeUnusedLocalVar except that this one
    is specific to the PHYEX code and take into account the mnh_expand directives.
    :param doc: xml fragment to search for variable usage
    :param scopePath: scope to explore (None for all)
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
    return removeUnusedLocalVar(doc, scopePath=scopePath, excludeList=excludeList, simplify=simplify)

class Applications():
    @copy_doc(addStack)
    def addStack(self, *args, **kwargs):
        return addStack(self._xml, *args, **kwargs)  

    @copy_doc(checkStackArginCall)
    def checkStackArginCall(self, *args, **kwargs):
        return checkStackArginCall(self._xml, *args, **kwargs)  
    
    @copy_doc(applyCPP)
    def applyCPP(self, *args, **kwargs):
        return applyCPP(self._xml, *args, **kwargs)
    
    @copy_doc(addIncludes)
    def addIncludes(self, *args, **kwargs):
        return addIncludes(self._xml, *args, **kwargs)
    
    @copy_doc(deleteDrHook)
    def deleteDrHook(self, *args, **kwargs):
        return deleteDrHook(self._xml, *args, **kwargs)

    @copy_doc(deleteBudgetDDH)
    def deleteBudgetDDH(self, *args, **kwargs):
        return deleteBudgetDDH(self._xml, *args, **kwargs)

    @copy_doc(deleteNonColumnCalls)
    def deleteNonColumnCalls(self, *args, **kwargs):
        return deleteNonColumnCalls(self._xml, *args, **kwargs)

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
