"""
This module implements functions for high-to-moderate level transformation
"""

import xml.etree.ElementTree as ET
from pyft.util import copy_doc, debugDecor, alltext, getParent, fortran2xml
from pyft.statements import (removeCall, setFalseIfStmt, removeStmtNode,
                             removeArraySyntax, inlineContainedSubroutines)
from pyft.variables import (removeUnusedLocalVar, getVarList, addVar, addModuleVar,
                            removeVar)
from pyft.scope import getScopesList, getScopePath
from pyft.expressions import createExprPart

@debugDecor
def addIncludes(doc):
    """
    fxtran includes the file but:
    - it does not remove the INCLUDE "file.h" statement
    - include the file with its file node
    This function removes the INCLUDE statement and the file node
    :param doc: etree to use
    """
    #Remove the include statement
    includeStmts = doc.findall('.//{*}include')
    for includeStmt in includeStmts:
        par = getParent(doc, includeStmt)
        par.remove(includeStmt)
    #Remove the file node
    mainfile = doc.find('./{*}file')
    for file in mainfile.findall('.//{*}file'):
        par = getParent(doc, file)
        index = list(par).index(file)
        if file.tail is not None:
            file[-1].tail = file.tail if file[-1].tail is None else (file[-1].tail + file.tail)
        for node in file[::-1]:
            par.insert(index, node)
        par.remove(file)

@debugDecor
def deleteNonColumnCallsPHYEX(doc, simplify=False):
    """
    Remove PHYEX routines that compute with different vertical columns not needed for AROME
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
def inlineContainedSubroutinesPHYEX(doc, simplify=False):
    """
    Inline all contained subroutines in the main subroutine
    Steps :
        - Identify contained subroutines
        - Look for all CALL statements, check if it is a containted routines; if yes, inline
        - Delete the containted routines
    :param doc: xml fragment containing main and contained subroutine
    :param simplify: try to simplify code (construct or variables becoming useless)
    :param loopVar: None to create new variable for each added DO loop (around ELEMENTAL subroutine calls)
                    or a function that return the name of the variable to use for the loop control.
                    This function returns a string (name of the variable), or True to create
                    a new variable, or False to not transform this statement
                    The functions takes as arguments:
                      - lower and upper bounds as defined in the declaration statement
                      - lower and upper bounds as given in the statement
                      - name of the array
                      - index of the rank
    """
    return inlineContainedSubroutines(doc, simplify=simplify, loopVar=_loopVarPHYEX)

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
    if lower_used is not None and lower_used.upper() == 'IIJB' and \
       upper_used is not None and upper_used.upper() == 'IIJE':
        varName = 'JIJ'
    elif upper_decl is None or lower_decl is None:
        varName = False
    elif upper_decl.upper() in ('KSIZE', 'KPROMA', 'KMICRO',
                                'IGRIM', 'IGACC', 'IGDRY', 'IGWET'):
        varName = 'JL'
    elif upper_decl.upper() in ('D%NIJT', 'IIJE') or lower_decl.upper() in ('D%NIJT', 'IIJB') or \
         'D%NIJT' in upper_decl.upper() + lower_decl.upper():
        #REAL, DIMENSION(MERGE(D%NIJT, 0, PARAMI%LDEPOSC)), INTENT(OUT) :: PINDEP
        varName = 'JIJ'
    elif upper_decl.upper() in ('IKB', 'IKE', 'IKT', 'D%NKT', 'KT') or \
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
    
    @copy_doc(addIncludes)
    def addIncludes(self, *args, **kwargs):
        return addIncludes(self._xml, *args, **kwargs)
    
    @copy_doc(deleteDrHook)
    def deleteDrHook(self, *args, **kwargs):
        return deleteDrHook(self._xml, *args, **kwargs)

    @copy_doc(deleteBudgetDDH)
    def deleteBudgetDDH(self, *args, **kwargs):
        return deleteBudgetDDH(self._xml, *args, **kwargs)

    @copy_doc(deleteNonColumnCallsPHYEX)
    def deleteNonColumnCallsPHYEX(self, *args, **kwargs):
        return deleteNonColumnCallsPHYEX(self._xml, *args, **kwargs)

    @copy_doc(removeIJLoops)
    def removeIJLoops(self, *args, **kwargs):
        return removeIJLoops(self._xml, *args, **kwargs)
    
    @copy_doc(removePHYEXUnusedLocalVar)
    def removePHYEXUnusedLocalVar(self, *args, **kwargs):
        return removePHYEXUnusedLocalVar(self._xml, *args, **kwargs)
    
    @copy_doc(inlineContainedSubroutinesPHYEX)
    def inlineContainedSubroutinesPHYEX(self, *args, **kwargs):
        return inlineContainedSubroutinesPHYEX(self._xml, *args, **kwargs)

    @copy_doc(expandAllArraysPHYEX)
    def expandAllArraysPHYEX(self, *args, **kwargs):
        return expandAllArraysPHYEX(self._xml, *args, **kwargs)
