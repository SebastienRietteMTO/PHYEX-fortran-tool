"""
This module implements functions for high-to-moderate level transformation
"""

import xml.etree.ElementTree as ET
from pyft.util import copy_doc, debugDecor, alltext, getParent, fortran2xml, n2name, PYFTError
from pyft.statements import (removeCall, setFalseIfStmt, removeStmtNode,
                             removeArraySyntax, createDoConstruct, createArrayBounds)
from pyft.variables import (removeUnusedLocalVar, getVarList, addVar, addModuleVar,
                            removeVar, findArrayBounds, varSpec2stmt, renameVar, findVar,
                            addArrayParentheses, addExplicitArrayBounds, arrayR2parensR)
from pyft.cosmetics import changeIfStatementsInIfConstructs
from pyft.scope import getScopesList, getScopePath
from pyft.expressions import createExprPart
import copy
import re

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
def inlineContainedSubroutines(doc, simplify=False):
    """
    Inline all contained subroutines in the main subroutine
    Steps :
        - Identify contained subroutines
        - Inline within contained subroutines look for all CALL statements, check if it is a containted routines; if yes, inline :
                - delete the original call statement and insert the inlined node
        - Inline the main routine
        - Delete the containted routines
    :param doc: xml fragment containing main and contained subroutine
    :param simplify: try to simplify code (construct or variables becoming useless)
    """

    locations  = getScopesList(doc,withNodes='tuple')
    containedRoutines = {}
    # Inline contained subroutines : look for sub: / sub:
    for loc in locations:
        if loc[0].count('sub:') >= 2:
            containedRoutines[alltext(loc[1].find('.//{*}subroutine-N/{*}N/{*}n'))] = (loc[0], loc[1])
    # Start by nested contained subroutines call, and end up with the last index = the main subroutine to treat 
    locations.reverse()
    for loc in locations: # For all subroutines (main + contained)
        if loc[0].count('sub:') >= 1:
            callStmtsNn = loc[1].findall('.//{*}call-stmt/{*}procedure-designator/{*}named-E/{*}N/{*}n')
            for callStmtNn in callStmtsNn: # For all CALL statements
                for containedRoutine in containedRoutines:
                    if alltext(callStmtNn) == containedRoutine: # If name of the routine called = a contained subroutine
                        varList = getVarList(doc) #Must be recomputed to take into account new variables
                        callStmt = getParent(doc,callStmtNn,level=4)
                        par = getParent(doc,callStmt)
                        if par.tag.endswith('}action-stmt'): # Expand the if-construct if the call-stmt is in a one-line if-construct
                            changeIfStatementsInIfConstructs(doc,getParent(doc,par))
                            par = getParent(doc, callStmt) #update parent

                        # Specific case for ELEMENTAL subroutines
                        # Introduce DO-loops if it is called on arrays
                        prefix = containedRoutines[containedRoutine][1].findall('.//{*}prefix')
                        if len(prefix) > 0 and 'ELEMENTAL' in [p.text.upper() for p in prefix]:
                            #Add missing parentheses
                            addArrayParentheses(doc, callStmt, varList=varList, scope=loc[0])

                            #Add explcit bounds
                            addExplicitArrayBounds(doc, node=callStmt, varList=varList, scope=loc[1])

                            #Detect if subroutine is called on arrays
                            arrayRincallStmt = callStmt.findall('.//{*}array-R')
                            if len(arrayRincallStmt) > 0: #Called on arrays
                                # Look for an array affectation to guess the DO loops to put around the call
                                table, newVar = findArrayBounds(doc, getParent(doc, arrayRincallStmt[0], 2),
                                                                varList, loc[0], _loopVarPHYEX)

                                # Add declaration of loop index if missing
                                for varName in table.keys():
                                    if not findVar(doc, varName, loc[0], varList):
                                        v = {'as': [], 'asx': [],
                                             'n': varName, 'i': None, 't': 'INTEGER', 'arg': False,
                                             'use': False, 'opt': False, 'allocatable': False,
                                             'parameter': False, 'init': None, 'scope': loc[0]}
                                        addVar(doc, [[loc[0], v['n'], varSpec2stmt(v), None]])
                                        varList.append(v)

                                # Create the DO loops
                                inner, outer, _ = createDoConstruct(table)

                                # Move the call statement in the DO loops
                                inner.insert(-1, callStmt) #callStmt in the DO-loops
                                par.insert(list(par).index(callStmt), outer) #DO-loops near the original call stmt
                                par.remove(callStmt) #original call stmt removed
                                par = inner #Update parent
                                for namedE in callStmt.findall('./{*}arg-spec/{*}arg/{*}named-E'):
                                    # Replace slices by indexes if any
                                    if namedE.find('./{*}R-LT'):
                                        arrayR2parensR(namedE, table, varList, loc[0])

                        # Inline
                        nodeInlined, localVarToAdd, localUseToAdd = inline(doc,
                                                                           containedRoutines[containedRoutine][1],
                                                                           callStmt,
                                                                           loc[0],
                                                                           containedRoutines[containedRoutine][0],
                                                                           varList,
                                                                           simplify=simplify)
                        # Add local var and use to main routine
                        addVar(doc, [[loc[0], var['n'], varSpec2stmt(var), None] for var in localVarToAdd])
                        addModuleVar(doc, [[loc[0], n2name(use_stmt.find('.//{*}module-N//{*}N')),
                                            [n2name(v.find('.//{*}N')) for v in use_stmt.findall('.//{*}use-N')]]
                                           for use_stmt in localUseToAdd])

                        # Remove call statement of the contained routines
                        allsiblings = par.findall('./{*}*')
                        index = allsiblings.index(callStmt)
                        par.remove(callStmt)
                        if callStmt.tail is not None:
                            if nodeInlined[-1].tail is None:
                                nodeInlined[-1].tail = callStmt.tail
                            else:
                                nodeInlined[-1].tail = nodeInlined[-1].tail + callStmt.tail
                        for node in nodeInlined[::-1]:
                            #nodeInlined is a program-unit, we must insert the subelements
                            par.insert(index, node)
                # Update containedRoutines
                containedRoutines = {}
                # Inline contained subroutines : look for sub: / sub:
                for locs in locations:
                    if locs[0].count('sub:') >= 2:
                        containedRoutines[n2name(locs[1].find('.//{*}subroutine-N/{*}N'))] = (locs[0], locs[1])

    for loc in locations:
        if loc[0].count('sub:') >= 2:
            name = loc[0].split(':')[-1].upper() # Subroutine name
            nodes = [N for N in doc.findall('.//{*}N') if n2name(N).upper() == name] # All nodes refering the subroutine
            if all([N in loc[1].iter() for N in nodes]):
                # Subroutine name not used (apart in its definition scope)
                getParent(doc, loc[1]).remove(loc[1])


def inline(doc, subContained, callStmt, mainScope, subScope, varList, simplify=False):
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
    :param mainScope: scope of the main (calling) subroutine
    :param subScope: scope of the contained subroutine to include
    :param varList: var list of all scopes
    :param simplify: try to simplify code (construct or variables becoming useless)
    """
    def setPRESENTby(node, var, val):
        """
        Replace PRESENT(var) by .TRUE. if val is True, by .FALSE. otherwise on node if var is found
        :param node: xml node to work on (a contained subroutine)
        :param var: string of the name of the optional variable to check
        """
        for namedE in node.findall('.//{*}named-E/{*}N/..'):
            if n2name(namedE.find('./{*}N')).upper() == 'PRESENT':
                presentarg = n2name(namedE.find('./{*}R-LT/{*}parens-R/{*}element-LT/{*}element/{*}named-E/{*}N'))
                if presentarg.upper() == var.upper():
                    for n in namedE[:]:
                        namedE.remove(n)
                    namedE.tag = '{http://fxtran.net/#syntax}literal-E'
                    namedE.text = '.TRUE.' if val else '.FALSE.'

    # Deep copy the object to possibly modify the original one multiple times
    node = copy.deepcopy(subContained)
    
    # Get local variables that are not present in the main routine for later addition
    localVarToAdd = []
    subst = []
    for var in [v for v in varList
                if not v['arg'] and not v['use'] and v['scope'] == subScope]: #for local variables only
        if findVar(doc, var['n'], mainScope, varList):
           # Variable is already defined in main or upper, there is a name conflict,
           # the local variable must be renamed before being declared in the main routine
           newName = re.sub(r'_\d+$', '', var['n'])
           i = 1
           while findVar(doc, newName + '_' + str(i), subScope, varList):
               i += 1
           newName += '_' + str(i)
           renameVar(node, var['n'], newName)
           subst.append((var['n'], newName))
           var['n'] = newName
        var['scope'] = mainScope #important for findVar(.., mainScope,...) to find it
        localVarToAdd.append(var)

    #In case a substituted variable is used in the declaration of another variable
    for oldName, newName in subst:
        for v in localVarToAdd + varList:
            if v['as'] is not None:
               v['as'] = [[re.sub(r'\b' +oldName + r'\b', newName, dim[i])
                           if dim[i] is not None else None
                           for i in (0, 1)]
                          for dim in v['as']]

    # Remove all objects that is implicit none, comment or else until reach something interesting
    # USE statements are stored for later user
    # subroutine-stmt and end-subroutine-stmt are kept to ensure consistency (for removeStmtNode with simplify)
    localUseToAdd = node.findall('./{*}use-stmt')
    for n in node.findall('./{*}T-decl-stmt') + localUseToAdd + node.findall('./{*}implicit-none-stmt'):
        node.remove(n)
    while node[1].tag.split('}')[1] == 'C':
        node.remove(node[1])

    # Variable correspondance
    # For each dummy argument, we look for the calling arg name and shape
    # CALL FOO(Z(:))
    # SUBROUTINE FOO(P)
    # vartable = {'P':{'name': 'Z', dim=[':']}}
    vartable = {} #ordered dict
    for argN in subContained.findall('.//{*}subroutine-stmt/{*}dummy-arg-LT/{*}arg-N'):
        vartable[alltext(argN).upper()] = None #Not present by default
    for iarg, arg in enumerate(callStmt.findall('.//{*}arg')):
        key = arg.find('.//{*}arg-N')
        if key is not None:
            #arg is VAR=value
            dummyName = alltext(key).upper()
            argnode = arg[1]
        else:
            dummyName = list(vartable.keys())[iarg]
            argnode = arg[0]
        RLTarray = argnode.findall('.//{*}R-LT/{*}array-R')
        if len(RLTarray) > 0:
            #array
            if len(RLTarray) > 1 or \
               argnode.find('./{*}R-LT/{*}array-R') is None or \
               argnode.tag.split('}')[1] != 'named-E':
                #Only simple cases are treated
                raise PYFTError('Argument to complicated: ' + str(alltext(argnode)))
            dim = RLTarray[0].find('./{*}section-subscript-LT')[:]
        else:
            dim = None
        if dim is None:
            #A%B => argname = 'A%B'
            #Z(1) => argname = 'Z(1)'
            argname = "".join(argnode.itertext())
        else:
            #Z(:) => argname = 'Z'
            tmp = copy.deepcopy(argnode)
            RLT = tmp.find('./{*}R-LT')
            RLT.remove(RLT.find('./{*}array-R'))
            argname = "".join(tmp.itertext())
        vartable[dummyName] = {'node': argnode, 'name': argname, 'dim': dim}

    # Look for PRESENT(var) and replace it by True when variable is present, by False otherwise
    for dummyName in [dummyName for (dummyName, value) in vartable.items() if value is not None]:
        setPRESENTby(node, dummyName, True)
    for dummyName in [dummyName for (dummyName, value) in vartable.items() if value is None]:
        setPRESENTby(node, dummyName, False)

    # Look for usage of variable not present and delete corresponding code
    for dummyName in [dummyName for (dummyName, value) in vartable.items() if value is None]:
        for N in node.findall('.//{*}named-E/{*}N'):
            if n2name(N).upper() == dummyName:
                removed = False
                par = getParent(node, N, level=2) #parent is the  named-E node, we need at least the upper level
                allreadySuppressed = []
                while par and not removed and par not in allreadySuppressed:
                    toSuppress = None
                    tag = par.tag.split('}')[1]
                    if tag in ('a-stmt', 'print-stmt'):
                        # Context 1: an a-stmt of type E1 = E2
                        toSuppress = par
                    elif tag == 'call-stmt':
                        #We should rewrite the call statement without this optional argument
                        #But it is not easy: we must checke that the argument is really optional for the called
                        #routine and we must add (if not already present) keywords for following arguments
                        raise NotImplementedError('call-stmt not (yet?) implemented')
                    elif tag in ('if-stmt', 'where-stmt'):
                        # Context 2: an if-stmt of type  : IF(variable) ...
                        toSuppress = par
                    elif tag in ('if-then-stmt', 'else-if-stmt', 'where-construct-stmt', 'else-where-stmt'):
                        # Context 3: an if-block of type  : IF(variable) THEN...
                        # We delete the entire construct
                        toSuppress =  getParent(node, par, 2)
                    elif tag in ('select-case-stmt', 'case-stmt'):
                        # Context 4: SELECT CASE (variable)... or CASE (variable)
                        # We deleted the entire construct
                        toSuppress = getParent(node, par, 2)
                    elif tag.endswith('-block') or tag.endswith('-stmt') or tag.endswith('-construct'):
                        # action-stmt, do-stmt, forall-construct-stmt, forall-stmt,
                        # if-block, where-block, selectcase-block,
                        #  must not contain directly
                        # the variable but must contain other statements using the variable. We target the inner
                        # statement in the previous cases.
                        # Some cases may have been overlooked and should be added above.
                        raise PYFTError(("We shouldn't be here. A case may have been " + \
                                         "overlooked (tag={tag}).".format(tag=tag)))
                    if toSuppress is not None:
                        removed = True
                        if toSuppress not in allreadySuppressed:
                            # We do not simplify variables to prevent side-effect with the variable renaming
                            removeStmtNode(node, toSuppress, False, simplify)
                            allreadySuppressed.extend(list(toSuppress.iter()))
                    else:
                        par = getParent(node, par)

    # Loop on the dummy argument
    for name in vartable:
        dummy = vartable[name] #This is the dummy argument information
        # Loop on all variables in the contained routine
        # It is important to build again the list of nodes, because it may have changed during the previous
        # dummy argument substitution
        for namedE in node.findall('.//{*}named-E/{*}N/{*}n/../..'):
            if n2name(namedE.find('{*}N')).upper() == name:
                #0 Concatenation of n nodes (a name could be split over several n nodes)
                N = namedE.find('./{*}N')
                ns = N.findall('./{*}n')
                ns[0].text = n2name(N)
                for n in ns[1:]:
                    N.remove(n)

                #1 We get info about variables (such as declared in the main or in the contained routines)
                desc_main = findVar(doc, dummy['name'], mainScope, varList)
                desc_sub = findVar(doc, name, subScope, varList)
                #In case variable is used for the declaration of another variable, we must update desc_sub
                for n in range(len(varList)):
                    if varList[n]['as'] is not None:
                        varList[n]['as'] = [[re.sub(r'\b' + name + r'\b', dummy['name'], dim[i])
                                                  if dim[i] is not None else None
                                                  for i in (0, 1)]
                                                 for dim in varList[n]['as']]

                #3 We select the indexes (only for array argument and not structure argument containing an array)
                #  using the occurrence to replace inside the subcontained routine body
                RLT = namedE.find('./{*}R-LT')
                if RLT is not None and RLT[0].tag.split('}')[1] != 'component-R':
                    #The variable name is immediately followed by a parenthesis
                    assert RLT[0].tag.split('}')[1] in ('array-R', 'parens-R'), 'Internal error'
                    slices = RLT[0].findall('./{*}section-subscript-LT/{*}section-subscript')
                    slices += RLT[0].findall('./{*}element-LT/{*}element')
                else:
                    #No parenthesis
                    if (desc_main is not None and len(desc_main['as']) > 0) or \
                       len(desc_sub['as']) > 0 or dummy['dim'] is not None:
                        #No parenthesis, but this is an array, we add as many ':' as needed
                        if len(desc_sub['as']) > 0:
                            ndim = len(desc_sub['as'])
                        else:
                            #ELEMENTAL routine, dummy arg is scalar
                            if dummy['dim'] is not None:
                                #We use the variable passed as argument because parenthesis were used
                                ndim = len([d for d in dummy['dim'] if ':' in alltext(d)])
                            else:
                                #We use the declared version in main
                                ndim = len(desc_main['as'])
                        ns[0].text += '(' + (', '.join([':'] * ndim)) + ')'
                        updatedNamedE = createExprPart(alltext(namedE))
                        namedE.tag = updatedNamedE.tag
                        namedE.text = updatedNamedE.text
                        for n in namedE[:]:
                            namedE.remove(n)
                        namedE.extend(updatedNamedE[:])
                        slices = namedE.find('./{*}R-LT')[0].findall('./{*}section-subscript-LT/{*}section-subscript')
                    else:
                        #This is not an array
                        slices = []

                #4 New name (the resultig xml is not necessarily a valid fxtran xml)
                namedE.find('./{*}N')[0].text = dummy['name']

                #5 We update the indexes to take into account a different declaration (lower bound especially)
                #  in the main and in the contained routines.
                #  Moreover, we could need to add indexes
                if len(slices) > 0:
                    #This is an array
                    for isl, sl in enumerate(slices):
                        #0 Compute bounds for array
                        if len(desc_sub['as']) == 0 or desc_sub['as'][isl][1] is None:
                            #ELEMENTAL or array with implicit shape
                            for i in (0, 1): #0 for lower bound and 1 for upper bound
                                if dummy['dim'] is not None:
                                    #Parenthesis in the call statement
                                    tag = './{*}lower-bound' if i == 0 else './{*}upper-bound'
                                    desc_sub[i] = dummy['dim'][isl].find(tag)
                                else:
                                    desc_sub[i] = None
                                if desc_sub[i] is not None:
                                    #lower/upper limit was given in the call statement
                                    desc_sub[i] = alltext(desc_sub[i])
                                else:
                                    #if available we take lower/upper limit set in the declaration 
                                    if desc_main is not None and desc_main['as'][isl][1] is not None:
                                        #Declaration found in main, and not using implicit shape
                                        desc_sub[i] = desc_main['as'][isl][i]
                                        if i == 0 and desc_sub[i] is None: desc_sub[i] = '1' #Default FORTRAN value
                                    else:
                                        desc_sub[i] = "L" if i == 0 else "U"
                                        desc_sub[i] += "BOUND({name}, {isl})".format(name=dummy['name'], isl=isl + 1)
                        else:
                            desc_sub[0] = desc_sub['as'][isl][0]
                            if desc_sub[0] is None: desc_sub[0] = '1' #Default FORTRAN value
                            desc_sub[1] = desc_sub['as'][isl][1]

                        #1 Offset computation
                        #  if only a subset is passed
                        #  and/or if lower bound of array is different in main and in sub contained routine
                        #REAL, DIMENSION(M1:M2):: Z; CALL FOO(N1:N2)
                        #SUBROUTINE FOO(P); REAL, DIMENSION(K1:K2):: P; P(I1:I2)
                        #If M1 or K1 is not set, they defaults to 1; if not set, N1 defaults to M1 and I1 to K1
                        #P(I1:I2)=Z(I1-K1+N1:I2-K1+N1)
                        offset = 0
                        if dummy['dim'] is not None and not alltext(dummy['dim'][isl]).strip().startswith(':'):
                            offset = alltext(dummy['dim'][isl].find('./{*}lower-bound'))
                        else:
                            if desc_main is not None:
                                offset = desc_main['as'][isl][0]
                                if offset is None:
                                    offset = '1' #Default FORTRAN value
                                elif offset.strip().startswith('-'):
                                    offset = '(' + offset + ')'
                            else:
                                offset = "LBOUND({name}, {isl})".format(name=dummy['name'], isl=isl + 1)
                        if offset.upper() == desc_sub[0].upper():
                            offset = 0
                        else:
                            if desc_sub[0].strip().startswith('-'):
                                offset += '- (' + desc_sub[0] + ')'
                            else:
                                offset += '-' + desc_sub[0]

                        #2 Update index with the offset and add indexes instead of ':'
                        if sl.tag.split('}')[1] == 'element' or \
                           (sl.tag.split('}')[1] == 'section-subscript' and not ':' in alltext(sl)):
                            #Z(I) or last index of Z(:, I)
                            bounds = sl
                        else:
                            low = sl.find('./{*}lower-bound')
                            if low is None:
                                low = ET.Element('{http://fxtran.net/#syntax}lower-bound')
                                low.append(createExprPart(desc_sub[0]))
                                low.tail = sl.text #':'
                                sl.text = None
                                sl.insert(0, low)
                            up = sl.find('./{*}upper-bound')
                            if up is None:
                                up = ET.Element('{http://fxtran.net/#syntax}upper-bound')
                                up.append(createExprPart(desc_sub[1]))
                                sl.append(up)
                            bounds = [low, up]
                        if offset != 0:
                            for bound in bounds:
                                #bound[-1] is a named-E, literal-E, op-E...
                                if bound[-1].tail is None: bound[-1].tail = ''
                                bound[-1].tail += '+' + offset #Not valid fxtran xml

                    #We must add extra indexes
                    #CALL FOO(Z(:,1))
                    #SUBROUTINE FOO(P); REAL, DIMENSION(K1:K2):: P
                    #P(I) => Z(I, 1)
                    if dummy['dim'] is not None and len(dummy['dim']) > len(slices):
                        slices[-1].tail = ', '
                        par = getParent(namedE, slices[-1])
                        par.extend(dummy['dim'][len(slices):])

                #6 Convert (wrong) xml into text and into xml again (to obtain a valid fxtran xml)
                #  This double conversion is not sufficient in some case.
                #  E.g. variable (N/n tag) replaced by real value
                updatedNamedE = createExprPart(alltext(namedE))
                namedE.tag = updatedNamedE.tag
                namedE.text = updatedNamedE.text
                for n in namedE[:]:
                    namedE.remove(n)
                namedE.extend(updatedNamedE[:])

    node.remove(node.find('./{*}subroutine-stmt'))
    node.remove(node.find('./{*}end-subroutine-stmt'))

    return node, localVarToAdd, localUseToAdd
    
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
    
    @copy_doc(inlineContainedSubroutines)
    def inlineContainedSubroutines(self, *args, **kwargs):
        return inlineContainedSubroutines(self._xml, *args, **kwargs)

    @copy_doc(expandAllArraysPHYEX)
    def expandAllArraysPHYEX(self, *args, **kwargs):
        return expandAllArraysPHYEX(self._xml, *args, **kwargs)
