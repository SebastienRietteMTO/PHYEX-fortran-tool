"""
This module implements functions for high-to-moderate level transformation
"""

import xml.etree.ElementTree as ET
from pyft.util import copy_doc, debugDecor, alltext, getParent, fortran2xml
from pyft.statements import (removeCall, setFalseIfStmt, removeStmtNode,
                             removeArraySyntax, createDoConstruct, createArrayBounds)
from pyft.variables import removeUnusedLocalVar, getVarList, addVar, addModuleVar, removeVar
from pyft.cosmetics import changeIfStatementsInIfConstructs
from pyft.scope import getScopesList, getScopePath
from pyft.expressions import createExprPart
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

    def getIndexLoop(lowerBound,upperBound):
        if 'KSIZE' in upperBound or 'KPROMA' in upperBound or 'KMICRO' in upperBound \
        or 'IGRIM' in upperBound or 'IGACC' in upperBound or 'IGDRY' in upperBound\
        or 'IGWET' in upperBound:
            n = 'JL'
        elif 'NIJ' in lowerBound or 'NIJ' in upperBound or 'IIJ' in lowerBound or 'IIJ' in upperBound:
            n = 'JIJ'
        elif 'IKB' in upperBound or 'IKE' in upperBound or 'KT' in upperBound:
            n = 'JK'
        elif 'SV' in lowerBound or 'SV' in upperBound:
            n = 'JSV'
        elif 'KRR' in upperBound:
            n = 'JRR'
        elif 'NIT' in upperBound or 'IIB' in lowerBound or 'IIE' in upperBound or 'IIU' in upperBound:
            n = 'JI'
        elif 'NJT' in upperBound or 'IJB' in lowerBound or 'IJE' in upperBound or 'IJU' in upperBound:
            n = 'JJ'
        else:
            exit
        return n

    def E2StmtToDoStmt(node_astmt):
        """
        Conversion function to remove the array syntax on the first arrayR of E-2 node for elemental subroutine
        For now, this function is used only to adapt array-syntax in CALL of an ELEMENTAL SUBROUTINE: 
        a lot of the functionnality taken from aStmtToDoStmt is not useful (such as onlyNumbers and the if + 2 elif cases within for loop on subsE2)
        :param node_astmt: a-stmt node to work on
        """
        doToBuild = []
        arrayR = node_astmt.findall('.//{*}E-2//{*}array-R')
        if len(arrayR) > 0:
            subsE2=arrayR[0].findall('.//{*}section-subscript')
            for i,sub in enumerate(subsE2):
                if len(sub.findall('.//{*}upper-bound')) == 0:
                    if ':' in alltext(sub): # INDEX: e.g. (IKTB:)
                        # Creation of the array-R object needed to be converted futher in parens-R
    #                    ind=varArrayNamesList.index(varName)
    #                    lowerBound = alltext(sub.findall('.//{*}lower-bound/{*}*/{*}*/{*}n')[0])
    #                    upperBound = str(varArray[ind]['as'][i][1])
    #                    lowerXml,upperXml = createArrayBounds(lowerBound, upperBound, 'ARRAY')
    #                    sub.insert(0,lowerXml)
    #                    sub.insert(1,upperXml)
    #                    sub.text = '' # Delete the initial ':'
    #                    doToBuild.append(createDoConstruct({getIndexLoop(lowerBound, upperBound):(lowerBound, upperBound)})[1])
                        pass # This case does not exit yet in the form of PHYEX 0.5.0 : case of calling an elemental subroutine with calling arg such as A(IKTB:)
                    else:  # single literal-E : copy the object (e.g. IKA alone or operation such as IKE+1)
                        pass
                else:
                    lowerBounds=sub.findall('.//{*}lower-bound')
                    upperBounds=sub.findall('.//{*}upper-bound')
                    lowerBound=alltext(lowerBounds[0])
                    upperBound=alltext(upperBounds[0])
                    if len(sub.findall('.//{*}literal-E')) == 2: #lower and upper Bounds
                        break
                    else:
                        doToBuild.append(createDoConstruct({getIndexLoop(lowerBound, upperBound):(lowerBound, upperBound)})[1])
        return doToBuild

    def placeArrayRtoparensR(doc, locNode, node_opE):
        """
        Convert ArrayR to parensR (remove the array-R and add parensR)
        :param doc: etree to use for parent retrieval
        :param node_opE: working node
        :param locNode: scope of the working node
        """
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
                elif sub.text is not None and sub.text.strip(' ') == ':': # :INDEX e.g. (:IKTE); transform it to 1:IKTE
                    u = sub.find('.//{*}upper-bound/{*}*/{*}*/{*}n')
                    if u is None:
                        #fxtran bug workaround
                        u = sub.find('.//{*}lower-bound/{*}*/{*}*/{*}n')
                    upperBound = alltext(u)
                    element.insert(0, createExprPart(getIndexLoop('1',upperBound)))      
                elif len(sub.findall('.//{*}upper-bound')) == 0:
                    if ':' in alltext(sub): # INDEX: e.g. (IKTB:)
                        pass
                    else:  # single literal-E : copy the object (e.g. IKA alone or operation such as IKE+1)
                        element.insert(0,sub.findall('.//{*}lower-bound')[0]) 
                else:
                    lowerBounds=sub.findall('.//{*}lower-bound')
                    upperBounds=sub.findall('.//{*}upper-bound')
                    lowerBound=alltext(lowerBounds[0])
                    upperBound=alltext(upperBounds[0])
                    element.insert(0, createExprPart(getIndexLoop(lowerBound,upperBound)))
                elementLT.append(element)
        
            for i in range(len(elementLT)-1):
                elementLT[i].tail = ','
            parensR.insert(0,elementLT)
            return parensR
    
        # Replace the array-like index selection by index loop on all variables (array-R)
        arrayR = node_opE.findall('.//{*}array-R')
        for node in arrayR:
            parensR=arrayRtoparensR(locNode,node)
            par = getParent(node_opE,node)
            par.insert(1,parensR)
            par.remove(node)

    def addExplicitArrayBounds(node, callNode, varList):
        """
        Add explicit arrays bounds (for further arrays expansion) in Call arguments.
        Used in call of elemental subroutine
        :param node: xml node to work on (the calling subroutine)
        :param callNode: xml node of the call statement
        :param varList: var list of node
        """
        def convertColonArrayinDim(sub, locNode, varArrayNamesList, varArray, varName):
            """
            Convert ':' in full array dimensions. Example if SIZE(A)=D%NKT, A(:) is converted to A(1:D%NKT) 
            :param sub: section-subscript node from the working node
            :param locNode: scope of the working node
            :param varArrayNamesList: list of all variable arrays names (list of string)
            :param varArray: list of all variables arrays (list of Dictionnaray returned from getVarList)
            :param varName : string of the variable in reading fortran ('A' in the example)
            """  
            # Get the i sub-index of the current object of E-2
            subPar = getParent(locNode,sub)
            for j,el in enumerate(subPar):
                if el == sub:
                    indextoTransform=j
                    break
            # Get the variable object to find its declaration dimension
            ind=varArrayNamesList.index(varName)
            lowerBound = '1'
            upperBound = str(varArray[ind]['as'][indextoTransform][1]) #e.g. D%NIJT; D%NKT; KSIZE; KPROMA etc
            lowerXml,upperXml = createArrayBounds(lowerBound, upperBound, 'ARRAY')
            sub.insert(0,lowerXml)
            sub.insert(1,upperXml)
            sub.text = '' # Delete the initial ':'    


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

    @copy_doc(expandAllArraysPHYEX)
    def expandAllArraysPHYEX(self, *args, **kwargs):
        return expandAllArraysPHYEX(self._xml, *args, **kwargs)
