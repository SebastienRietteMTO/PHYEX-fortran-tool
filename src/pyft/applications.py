"""
This module implements functions for high-to-moderate level transformation
"""

import logging
import xml.etree.ElementTree as ET
from pyft.util import (copy_doc, debugDecor,getIndexLoop,
                       alltext,tostring,getParent,getSiblings,moveInGrandParent,fortran2xml,
                       PYFTError, isint, getFileName, n2name)
from pyft.statements import (removeCall, setFalseIfStmt, removeStmtNode,
                             placeArrayRtoparensR, convertColonArrayinDim, E2StmtToDoStmt,
                             createDoConstruct)
from pyft.variables import removeUnusedLocalVar, getVarList, addVar, addModuleVar, removeVar, findVar
from pyft.cosmetics import changeIfStatementsInIfConstructs
from pyft.scope import getScopeChildNodes, getScopeNode, getScopesList, getScopePath, isScopeNode
from pyft.expressions import createExprPart, simplifyExpr
import copy
import re

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
            par.insert(index+1, xmlIncludeStack.find('.//{*}include'))

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
                            par = getParent(doc, callStmt) #update parent
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
                                            lowerBound.insert(0, createExprPart(loopIndex))
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
    #     - find the bounds and guess a set of variables to use ('find_bounds' function)
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

    
    def find_bounds(arr, varList, currentScope, loopVar):
        """
        Find bounds and loop variable given a array
        :param arr: array node (named-E node with a array-R child)
        :param varList: list of variables
        :param loopVar: see removeArraySyntax docstring
        :return table: as the table returned by the decode function, or None if unable to fill it
        """
        table = {} #ordered since python 3.7
        name = n2name(arr.find('./{*}N'))

        #Iteration on the different subscript
        for iss, ss in enumerate(arr.findall('./{*}R-LT/{*}array-R/{*}section-subscript-LT/{*}section-subscript')):
            #We are only interested by the subscript containing ':'
            #we must not iterate over the others, eg: X(:,1)
            if ':' in alltext(ss):
                #Look for lower and upper bounds for iteration and declaration
                lower_used = ss.find('./{*}lower-bound')
                upper_used = ss.find('./{*}upper-bound')
                varDesc = findVar(doc, name, currentScope, varList, array=True)
                if varDesc is not None:
                    lower_decl, upper_decl = varDesc['as'][iss]
                    if lower_decl is None: lower_decl = '1' #default lower index for FORTRAN arrays
                else:
                    lower_decl, upper_decl = None, None

                #name of the loop variable
                if loopVar is None:
                    #loopVar is not defined, we create a new variable for the loop
                    #only if lower and upper bounds have been found (easy way to discard character strings)
                    guess = lower_decl is not None and upper_decl is not None
                    varName = False
                else:
                    varName = loopVar(lower_decl, upper_decl,
                                      None if lower_used is None else alltext(lower_used),
                                      None if upper_used is None else alltext(upper_used), name, iss)
                    if varName is not False and varName in table.keys():
                        raise PYFTError(("The variable {var} must be used for the rank #{i1} whereas it " + \
                                         "is already used for rank #{i2} (for array {name}).").format(
                                           var=varName, i1=str(iss), i2=str(list(table.keys()).index(varName)),
                                           name=name))
                    if varName is not False and findVar(doc, varName, currentScope,
                                                        varList, array=False, exactScope=True) is None:
                        #We must declare the variable
                        varDesc = {'n':varName, 'scope':currentScope, 'new':True}
                        varList.append(varDesc)
                    #varName can be a string (name to use), True (to create a variable), False (to discard the array)
                    guess = varName is True
                if guess:
                    j = 1
                    #We look for a variable name that don't already exist
                    #We can reuse a newly created varaible only if it is not used for the previous indexes
                    #of the same statement
                    while any([v['n'] for v in varList
                               if ((v['n'].upper() == 'J' + str(j) and not v.get('new', False)) or
                                   'J' + str(j) in table.keys())]):
                        j += 1
                    varName = 'J' + str(j)
                    varDesc = {'n':varName, 'scope':currentScope, 'new':True}
                    if varDesc not in varList:
                        varList.append(varDesc)

                #fill table
                table[varName] = (lower_decl if lower_used is None else alltext(lower_used),
                                  upper_decl if upper_used is None else alltext(upper_used))

        return None if False in table.keys() else table

    def arrayR2parensR(namedE, table, varList, currentScope):
        """
        Transform a array-R into a parens-R node by replacing slices by variables
        In 'A(:)', the ':' is in a array-R node whereas in 'A(JL)', 'JL' is in a parens-R node.
        Both the array-R and the parens-R nodes are inside a R-LT node
        :param namedE: a named-E node
        :param table: dictionnary returned by the decode function
        :param varList: description of declared variables
        :param currentScope: current scope
        """
        #Before A(:): <f:named-E>
        #               <f:N><f:n>A</f:n></f:N>
        #               <f:R-LT>
        #                 <f:array-R>(
        #                   <f:section-subscript-LT>
        #                     <f:section-subscript>:</f:section-subscript>
        #                   </f:section-subscript-LT>)
        #                 </f:array-R>
        #               </f:R-LT>
        #              </f:named-E>
        #After  A(I): <f:named-E>
        #               <f:N><f:n>A</f:n></f:N>
        #               <f:R-LT>
        #                 <f:parens-R>(
        #                   <f:element-LT>
        #                     <f:element><f:named-E><f:N><f:n>I</f:n></f:N></f:named-E></f:element>
        #                   </f:element-LT>)
        #                 </f:parens-R>
        #               </f:R-LT>
        #             </f:named-E>

        RLT = namedE.find('./{*}R-LT')
        arrayR = RLT.find('./{*}array-R') #Not always in first position, eg: ICED%XRTMIN(:)
        if arrayR is not None:
            index = list(RLT).index(arrayR)
            parensR = ET.Element('{http://fxtran.net/#syntax}parens-R')
            parensR.text = '('
            parensR.tail = ')'
            elementLT = ET.Element('{http://fxtran.net/#syntax}element-LT')
            parensR.append(elementLT)
            ivar = -1
            for ss in RLT[index].findall('./{*}section-subscript-LT/{*}section-subscript'):
                element = ET.Element('{http://fxtran.net/#syntax}element')
                element.tail = ', '
                elementLT.append(element)
                if ':' in alltext(ss):
                    ivar += 1
                    v = list(table.keys())[ivar] #variable name
                    lower = ss.find('./{*}lower-bound')
                    upper = ss.find('./{*}upper-bound')
                    if lower is not None: lower = alltext(lower)
                    if upper is not None: upper = alltext(upper)
                    if lower is not None and ss.text is not None and ':' in ss.text:
                        #fxtran bug workaround
                        upper = lower
                        lower = None
                    if lower is None and upper is None:
                        #E.g. 'A(:)'
                        #In this case we use the DO loop bounds without checking validity with
                        #respect to the array declared bounds
                        element.append(createExprPart(v))
                    else:
                        #E.g.:
                        #!$mnh_expand_array(JI=2:15)
                        #A(2:15) or A(:15) or A(2:)
                        if lower is None:
                            #lower bound not defined, getting lower declared bound for this array
                            lower = findVar(doc, n2name(namedE.find('{*}N')),
                                            currentScope, varList, array=True)['as'][ivar][0]
                            if lower is None: lower = '1' #default fortran lower bound
                        elif upper is None:
                            #upper bound not defined, getting lower declared bound for this array
                            upper = findVar(doc, n2name(namedE.find('{*}N')),
                                    currentScope, varList, array=True)['as'][ivar][1]
                        #If the DO loop starts from JI=I1 and goes to JI=I2; and array bounds are J1:J2
                        #We compute J1-I1+JI and J2-I2+JI and they should be the same
                        #E.g: array bounds could be 'I1:I2' (becoming JI:JI) or 'I1+1:I2+1" (becoming JI+1:JI+1)
                        newlower = simplifyExpr(lower, add=v, sub=table[v][0])
                        newupper = simplifyExpr(upper, add=v, sub=table[v][1])
                        if newlower != newupper:
                            raise PYFTError(("Don't know how to do with an array declared with '{la}:{ua}' " + \
                                             "and a loop from '{ll}' to '{ul}'").format(la=lower, ua=upper,
                                                                                        ll=table[v][0],
                                                                                        ul=table[v][1]))
                        element.append(createExprPart(newlower))
                else:
                    element.append(ss.find('./{*}lower-bound'))
            element.tail = None #last element
            RLT.remove(RLT[index])
            RLT.insert(index, parensR)

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
                    raise PYFTError('End mnh_directive found before begin directive')
                if (table, kind) != decode(e.text):
                    raise PYFTError("Opening and closing mnh directives must be conform")
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
                    newtable = find_bounds(arr, varList, currentScope, loopVar) if arr is not None else None

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

def _loopVarPHYEX(lower_decl, upper_decl, lower_used, upper_used, name, i):
    """
    Try to guess the name of the variable to use for looping on indexes
    :param lower_decl, upper_decl: lower and upper bounds as defined in the declaration statement
    :param lower_used, upper_used: lower and upper bounds as given in the statement
    :param name: name of the array
    :param i: index of the rank
    :return: the variable name of False to discard this statement
    """
    if upper_decl is None or lower_decl is None:
        varName = False
    elif upper_decl.upper() in ('KSIZE', 'KPROMA', 'KMICRO',
                              'IGRIM', 'IGACC', 'IGDRY', 'IGWET'):
        varName = 'JL'
    elif upper_decl.upper() in ('D%NIJT', 'IIJ') or lower_decl.upper() in ('D%NIJT', 'IIJ') or \
         'D%NIJT' in upper_decl.upper() + lower_decl.upper():
        #REAL, DIMENSION(MERGE(D%NIJT, 0, PARAMI%LDEPOSC)), INTENT(OUT) :: PINDEP
        varName = 'JIJ'
    elif upper_decl.upper() in ('IKB', 'IKE', 'IKT', 'D%NKT') or \
         'D%NKT' in upper_decl.upper():
        #REAL, DIMENSION(MERGE(D%NIJT, 0, OCOMPUTE_SRC),  MERGE(D%NKT, 0, OCOMPUTE_SRC)), INTENT(OUT) :: PSIGS
        varName = 'JK'
    elif upper_decl.upper() == 'KSV' or lower_decl.upper() == 'KSV':
        varName = 'JSV'
    elif upper_decl.upper() == 'KRR':
        varName = 'JRR'
    elif upper_decl.upper() in ('D%NIT', 'IIE', 'IIU') or lower_decl.upper() == 'IIB':
        varName = 'JI'
    elif upper_decl.upper() in ('D%NJT', 'IJE', 'IJU') or lower_decl.upper() == 'IJB':
        varName = 'JJ'
    else:
        varName = False
    return varName

def expandAllArraysPHYEX(doc):
    """
    Transform array syntax into DO loops
    :param doc: etree to use
    """

    #For simplicity, all functions (not only array functions) have been searched in the PHYEX source code
    funcList = ['AA2', 'AA2W', 'AF3', 'AM3', 'ARTH', 'BB3', 'BB3W', 'COEFJ', 'COLL_EFFI', 'DELTA',
                'DELTA_VEC', 'DESDTI', 'DESDTW', 'DQSATI_O_DT_1D', 'DQSATI_O_DT_2D_MASK', 'DQSATI_O_DT_3D',
                'DQSATW_O_DT_1D', 'DQSATW_O_DT_2D_MASK', 'DQSATW_O_DT_3D', 'DSDD', 'DXF', 'DXM', 'DYF',
                'DYM', 'DZF', 'DZM', 'ESATI', 'ESATW', 'FUNCSMAX', 'GAMMA_INC', 'GAMMA_X0D', 'GAMMA_X1D',
                'GENERAL_GAMMA', 'GET_XKER_GWETH', 'GET_XKER_N_GWETH', 'GET_XKER_N_RACCS', 'GET_XKER_N_RACCSS',
                'GET_XKER_N_RDRYG', 'GET_XKER_N_SACCRG', 'GET_XKER_N_SDRYG', 'GET_XKER_N_SWETH',
                'GET_XKER_RACCS', 'GET_XKER_RACCSS', 'GET_XKER_RDRYG', 'GET_XKER_SACCRG', 'GET_XKER_SDRYG',
                'GET_XKER_SWETH', 'GX_M_M', 'GX_M_U', 'GX_U_M', 'GX_V_UV', 'GX_W_UW', 'GY_M_M', 'GY_M_V',
                'GY_U_UV', 'GY_V_M', 'GY_W_VW', 'GZ_M_M', 'GZ_M_W', 'GZ_U_UW', 'GZ_V_VW', 'GZ_W_M',
                'HYPGEO', 'ICENUMBER2', 'LEAST_LL', 'LNORTH_LL', 'LSOUTH_LL', 'LWEST_LL', 'MOMG',
                'MXF', 'MXM', 'MYF', 'MYM', 'MZF', 'MZM', 'QSATI_0D', 'QSATI_1D', 'QSATI_2D',
                'QSATI_2D_MASK', 'QSATI_3D', 'QSATMX_TAB', 'QSATW_0D', 'QSATW_1D', 'QSATW_2D',
                'QSATW_2D_MASK', 'QSATW_3D', 'RECT', 'REDIN', 'SINGL_FUNCSMAX', 'SM_FOES_0D', 'SM_FOES_1D',
                'SM_FOES_2D', 'SM_FOES_2D_MASK', 'SM_FOES_3D', 'SM_PMR_HU_1D', 'SM_PMR_HU_3D',
                'TIWMX_TAB', 'TO_UPPER', 'ZRIDDR', 'GAMMLN', 'COUNTJV2D', 'COUNTJV3D', 'UPCASE']

    return removeArraySyntax(doc, useMnhExpand=False, loopVar=_loopVarPHYEX, reuseLoop=False, funcList=funcList,
                             updateMemSet=True, updateCopy=True)
    

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
    
    @copy_doc(removePHYEXUnusedLocalVar)
    def removePHYEXUnusedLocalVar(self, *args, **kwargs):
        return removePHYEXUnusedLocalVar(self._xml, *args, **kwargs)
    
    @copy_doc(inlineContainedSubroutines)
    def inlineContainedSubroutines(self, *args, **kwargs):
        return inlineContainedSubroutines(self._xml, *args, **kwargs)

    @copy_doc(removeArraySyntax)
    def removeArraySyntax(self, *args, **kwargs):
        return removeArraySyntax(self._xml, *args, **kwargs)

    @copy_doc(expandAllArraysPHYEX)
    def expandAllArraysPHYEX(self, *args, **kwargs):
        return expandAllArraysPHYEX(self._xml, *args, **kwargs)
