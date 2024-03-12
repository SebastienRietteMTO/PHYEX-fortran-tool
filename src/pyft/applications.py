"""
This module implements functions for high-to-moderate level transformation
"""

import xml.etree.ElementTree as ET
from pyft.cosmetics import indent
from pyft.util import (copy_doc, debugDecor, alltext, getParent, fortran2xml,
                       getFileName, n2name, isStmt, PYFTError)
from pyft.statements import (removeCall, setFalseIfStmt, removeStmtNode,
                             removeArraySyntax, inlineContainedSubroutines, insertStatement)
from pyft.variables import (removeUnusedLocalVar, getVarList, addVar, addModuleVar,
                            removeVar, modifyAutomaticArrays, findVar, addArrayParentheses,
                            findIndexArrayBounds, attachArraySpecToEntity)
from pyft.scope import getScopesList, getScopePath, getScopeChildNodes
from pyft.tree import addArgInTree, calledByScope, isUnderStopScopes
from pyft.expressions import createExpr, createExprPart
import logging

@debugDecor
def deleteNonColumnCallsPHYEX(doc, simplify=False):
    """
    Remove PHYEX routines that compute with different vertical columns not needed for AROME
    MODE_ROTATE_WIND, UPDATE_ROTATE_WIND
    If Simplify is True, also remove all variables only needed for these calls
    :param doc: etree to use
    :param simplify : if True, remove variables that are now unused
    """
    varList = None
    for subroutine in ('ROTATE_WIND', 'UPDATE_ROTATE_WIND', 'BL_DEPTH_DIAG_3D',
                       'TM06_H', 'TURB_HOR_SPLT'):
        #Remove call statements
        nb = removeCall(doc, subroutine, None, simplify=simplify)
        #Remove use statement
        if nb > 0:
            if varList is None:
                varList = getVarList(doc)
            removeVar(doc, [(v['scope'], v['n']) for v in varList
                            if v['n'] == subroutine], simplify=simplify)

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

def addMPPDB_CHECKS(doc):
    """
    Add MPPDB_CHEKS on all intent arrays on subroutines. ****** Not applied no modd_ routines. ********
    Example, for a BL89 routine with 4 arguments, 1 INTENT(IN), 2 INTENT(INOUT), 1 INTENT(OUT), it produces :
    IF (MPPDB_INITIALIZED) THEN
      !Check all IN arrays 
      CALL MPPDB_CHECK(PZZ, "BL89 beg:PZZ") 
      !Check all INOUT arrays 
      CALL MPPDB_CHECK(PDZZ, "BL89 beg:PDZZ") 
      CALL MPPDB_CHECK(PTHVREF, "BL89 beg:PTHVREF") 
   END IF
   ...
   IF (MPPDB_INITIALIZED) THEN
      !Check all INOUT arrays 
      CALL MPPDB_CHECK(PDZZ, "BL89 end:PDZZ") 
      CALL MPPDB_CHECK(PTHVREF, "BL89 end:PTHVREF") 
      !Check all OUT arrays 
      CALL MPPDB_CHECK(PLM, "BL89 end:PLM") 
   END IF
    :param doc: etree to use
    """
    locations  = getScopesList(doc,withNodes='tuple')
    mod_type = locations[0][0].split('/')[-1].split(':')[1][:4]
    if mod_type == 'MODD':
        pass
    else:
        for loc in locations:
            # Do not add MPPDB_CHEKS to :
            # - MODULE or FUNCTION object, 
            # - interface subroutine from a MODI
            # but only to SUBROUTINES
            if 'sub:' in loc[0] and 'func' not in loc[0] and 'interface' not in loc[0]:
                scopepath = getScopePath(doc,loc[1])
                subRoutineName = scopepath.split('/')[-1].split(':')[1]
                varList = getVarList(doc,scopepath)
                
                # Look for all intent arrays only
                arraysIn, arraysInOut, arraysOut = [], [], []
                for var in varList:
                     if var['arg'] and var['as']:
                         if var['i']=='IN':
                             arraysIn.append(var)
                         if var['i']=='INOUT':
                             arraysInOut.append(var)
                         if var['i']=='OUT':
                             arraysOut.append(var)
                # Check if there is any intent variables
                if len(arraysIn) + len(arraysInOut) + len(arraysOut) == 0: 
                    break
                
                # Add necessary module
                addModuleVar(doc, [(loc[0], 'MODE_MPPDB', None)])
    
                # Prepare some FORTRAN comments
                fortranSource = "SUBROUTINE FOO598756\n !Check all IN arrays \nEND SUBROUTINE"
                _, cfxtran = fortran2xml(fortranSource)
                commentIN = cfxtran.find('.//{*}C')
                fortranSource = "SUBROUTINE FOO598756\n !Check all INOUT arrays \nEND SUBROUTINE"
                _, cfxtran = fortran2xml(fortranSource)
                commentINOUT = cfxtran.find('.//{*}C')           
                fortranSource = "SUBROUTINE FOO598756\n !Check all OUT arrays \nEND SUBROUTINE"
                _, cfxtran = fortran2xml(fortranSource)
                commentOUT = cfxtran.find('.//{*}C')
                
                # 1) variables IN and INOUT block (beggining of the routine)
                if len(arraysIn) + len(arraysInOut) > 0:
                    fortranSource = "SUBROUTINE FOO598756\n IF (MPPDB_INITIALIZED) THEN\nEND IF \nEND SUBROUTINE"
                    _, ifMPPDBinitfxtran = fortran2xml(fortranSource)
                    ifMPPDBinit = ifMPPDBinitfxtran.find('.//{*}if-construct')
                    ifMPPDB = ifMPPDBinit.find('.//{*}if-block')
                    
                    # Variables IN
                    if len(arraysIn) > 0:
                        ifMPPDB.insert(1,commentIN)
                        for i,var in enumerate(arraysIn):
                            argsMPPDB = var['n'] + ", " + "\"" + subRoutineName + " beg:"+var['n'] + "\""
                            fortranSource = "SUBROUTINE FOO598756\n CALL MPPDB_CHECK(" + argsMPPDB + ") \nEND SUBROUTINE"
                            _, callMPPDBfxtran = fortran2xml(fortranSource)
                            callMPPDB = callMPPDBfxtran.find('.//{*}call-stmt')
                            ifMPPDB.insert(2+i,callMPPDB)
                    
                    # Variables INOUT
                    if len(arraysInOut) > 0:
                        shiftLineNumber = 2 if(len(arraysIn)>0) else 1
                        ifMPPDB.insert(len(arraysIn) + shiftLineNumber, commentINOUT)
                        for i,var in enumerate(arraysInOut):
                            argsMPPDB = var['n'] + ", " + "\"" + subRoutineName + " beg:"+var['n'] + "\""
                            fortranSource = "SUBROUTINE FOO598756\n CALL MPPDB_CHECK(" + argsMPPDB + ") \nEND SUBROUTINE"
                            _, callMPPDBfxtran = fortran2xml(fortranSource)
                            callMPPDB = callMPPDBfxtran.find('.//{*}call-stmt')
                            ifMPPDB.insert(len(arraysIn) + shiftLineNumber + 1 + i, callMPPDB)
                    
                    # Add the new IN and INOUT block 
                    insertStatement(doc,loc[0],indent(ifMPPDBinit),first=True)
                    
                # 2) variables INOUT and OUT block (end of the routine)
                if len(arraysInOut) + len(arraysOut) > 0:
                    fortranSource = "SUBROUTINE FOO598756\n IF (MPPDB_INITIALIZED) THEN\nEND IF \nEND SUBROUTINE"
                    _, ifMPPDBendfxtran = fortran2xml(fortranSource)
                    ifMPPDBend = ifMPPDBendfxtran.find('.//{*}if-construct')
                    ifMPPDB = ifMPPDBend.find('.//{*}if-block')
                    
                    # Variables INOUT
                    if len(arraysInOut) > 0:
                        ifMPPDB.insert(1,commentINOUT)
                        for i,var in enumerate(arraysInOut):
                            argsMPPDB = var['n'] + ", " + "\"" + subRoutineName + " end:"+var['n'] + "\""
                            fortranSource = "SUBROUTINE FOO598756\n CALL MPPDB_CHECK(" + argsMPPDB + ") \nEND SUBROUTINE"
                            _, callMPPDBfxtran = fortran2xml(fortranSource)
                            callMPPDB = callMPPDBfxtran.find('.//{*}call-stmt')
                            ifMPPDB.insert(2+i,callMPPDB)
                    
                    # Variables OUT
                    if len(arraysOut) > 0:
                        shiftLineNumber = 2 if(len(arraysInOut)>0) else 1
                        ifMPPDB.insert(len(arraysInOut)+shiftLineNumber,commentOUT)
                        for i,var in enumerate(arraysOut):
                            argsMPPDB = var['n'] + ", " + "\"" + subRoutineName + " end:"+var['n'] + "\""
                            fortranSource = "SUBROUTINE FOO598756\n CALL MPPDB_CHECK(" + argsMPPDB + ") \nEND SUBROUTINE"
                            _, callMPPDBfxtran = fortran2xml(fortranSource)
                            callMPPDB = callMPPDBfxtran.find('.//{*}call-stmt')
                            ifMPPDB.insert(len(arraysInOut) + shiftLineNumber + 1 + i, callMPPDB)

                    # Add the new INOUT and OUT block 
                    insertStatement(doc,loc[0],indent(ifMPPDBend),first=False)
@debugDecor
def addStack(doc, descTree, model, stopScopes, parser=None, parserOptions=None, wrapH=False):
    """
    Add specific allocations of local arrays on the fly for GPU
    :param doc: etree to use
    :param descTree: descTree file
    :param model : 'MESONH' or 'AROME' for specific objects related to the allocator or stack
    :param stopScopes: scope where we stop to add stack
    :param parser, parserOptions, wrapH: see the pyft class

    Stacks are added to all routines called by the scopes listed in stopScopes
    """
    if model == 'AROME':
        for scope in getScopesList(doc):
            #The AROME transformation needs an additional parameter
            #We apply the transformation only if the routine is called from a scope within stopScopes
            if scope in stopScopes or isUnderStopScopes(scope, descTree, stopScopes):
                #Intermediate transformation, needs cpp to be completed
                #This version would be OK if we didn't need to read again the files with fxtran after transformation
                #nb = modifyAutomaticArrays(doc,
                #                           declTemplate="temp({type}, {name}, ({shape}))",
                #                           startTemplate="alloc({name})",
                #                           scopes=scope)
    
                #Full transformation, using CRAY pointers
                #In comparison with the original transformation of Philippe, we do not call SOF with
                #__FILE__ and __LINE__ because it breaks future reading with fxtran
                nb = modifyAutomaticArrays(doc,
                            declTemplate="{type}, DIMENSION({shape}) :: {name}; POINTER(IP_{name}_, {name})",
                            startTemplate="IP_{name}_=YDSTACK%L;YDSTACK%L=YDSTACK%L+KIND({name})*SIZE({name});IF(YDSTACK%L>YDSTACK%U)CALL SOF('" + getFileName(doc) + ":{name}', 0)",
                            scopes=scope)
    
                if nb > 0:
                    #Some automatic arrays have been modified, we need to add an argument to the routine
                    addArgInTree(doc, scope, descTree, 'YDSTACK', 'TYPE (STACK) :: YDSTACK',
                                 -1, stopScopes, moduleVarList=[('STACK_MOD', ['STACK', 'SOF'])],
                                 parser=parser, parserOptions=parserOptions, wrapH=wrapH)
    elif model == 'MESONH':
        for scope in getScopesList(doc):
            #We apply the transformation only if the routine is called from a scope within stopScopes
            if scope in stopScopes or isUnderStopScopes(scope, descTree, stopScopes):
                nb = modifyAutomaticArrays(doc,
                            declTemplate="{type}, DIMENSION({doubledotshape}), POINTER, CONTIGUOUS :: {name}",
                            startTemplate="CALL MNH_MEM_GET({name}, {lowUpList})",
                            scopes=scope)
                if nb > 0:
                    #Some automatic arrays have been modified
                    #  we need to add the stack module,
                    addModuleVar(doc, [(scope, 'MODE_MNH_ZWORK',
                                        ['MNH_MEM_GET', 'MNH_MEM_POSITION_PIN', 'MNH_MEM_RELEASE'])])
                    #  to pin the memory position,
                    insertStatement(doc, scope,
                                    createExpr("CALL MNH_MEM_POSITION_PIN('{scope}')".format(scope=scope))[0], True)
                    #  and to realease the memory
                    insertStatement(doc, scope,
                                    createExpr("CALL MNH_MEM_RELEASE('{scope}')".format(scope=scope))[0], False)
    else:
        raise PYFTError('Stack is implemented only for AROME and MESONH models')

@debugDecor
def inlineContainedSubroutinesPHYEX(doc, descTree=None, simplify=False):
    """
    Inline all contained subroutines in the main subroutine
    Steps :
        - Identify contained subroutines
        - Look for all CALL statements, check if it is a containted routines; if yes, inline
        - Delete the containted routines
    :param doc: xml fragment containing main and contained subroutine
    :param descTree: description tree object (to update it with the inlining)
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
    return inlineContainedSubroutines(doc, descTree=descTree, simplify=simplify, loopVar=_loopVarPHYEX)

@debugDecor            
def removeIJDim(doc, descTree, stopScopes, parser=None, parserOptions=None, wrapH=False, simplify=False):
    """
    Transform routines to be called in a loop on columns
    :param doc: etree to use
    :param descTree: descTree file
    :param stopScopes: scope where we stop to add the D argument (if needed)
    :param parser, parserOptions, wrapH: see the pyft class
    :param simplify: try to simplify code (remove useless dimensions in call)

    ComputeInSingleColumn :
    - Remove all Do loops on JI and JJ
    - Initialize former indexes JI, JJ, JIJ to first array element: JI=D%NIB, JJ=D%NJB, JIJ=D%NIJB
    - If simplify is True, replace (:,*) on I/J/IJ dimension on argument with explicit (:,*) on CALL statements:
        e.g. CALL FOO(D, A(:,JK,1), B(:,:))
                   ==> CALL FOO(D, A(JIJ,JK,1), B(:,:)) only if the target argument is not an array
    """

    indexToCheck = {'JI':('D%NIB', 'D%NIT'),
                    'JJ':('D%NJB', 'D%NJT'),
                    'JIJ':('D%NIJB', 'D%NIJT')}

    def slice2index(namedE, scopeName, varList):
        """
        Transform a slice on the horizontal dimension into an index
        Eg.: X(1:D%NIJT, 1:D%NKT) => X(JIJ, 1:D%NKT) Be careful, this array is not contiguous.
             X(1:D%NIJT, JK) => X(JIJ, JK)
        :param namedE: array to transform
        :param scopeName: scope name where the array is
        :param varList: list of variables
        """
        # Find the variable declaration and the index to use
        var = findVar(doc, n2name(namedE.find('./{*}N')).upper(),
                                  scopeName, varList=varList)
        # Loop on all array dimensions
        for isub, sub in enumerate(namedE.findall('./{*}R-LT/{*}array-R/{*}section-subscript-LT' + \
                                                  '/{*}section-subscript')):
            if ':' in alltext(sub):
                loopIndex, _, _ = findIndexArrayBounds(doc, namedE, isub,
                                                       varList, scopeName, _loopVarPHYEX)
                if loopIndex in indexToCheck.keys(): # To be transformed
                    if sub.text == ':':
                        sub.text = None
                        lowerBound = ET.Element('{http://fxtran.net/#syntax}lower-bound')
                        sub.insert(0, lowerBound)
                    else:
                        lowerBound = sub.find('./{*}lower-bound')
                        lowerBound.tail = ''
                        for item in lowerBound:
                            lowerBound.remove(item)
                    upperBound = sub.find('./{*}upper-bound')
                    if upperBound is not None:
                        sub.remove(upperBound)
                    lowerBound.append(createExprPart(loopIndex))
                    if loopIndex not in indexRemoved:
                        indexRemoved.append(loopIndex)
        # Transform array-R/section-subscript-LT/section-subscript
        #      into parens-R>/element-LT/element if needed
        if not ':' in alltext(namedE.find('./{*}R-LT/{*}array-R/{*}section-subscript-LT')):
            ns = '{http://fxtran.net/#syntax}'
            namedE.find('./{*}R-LT/{*}array-R').tag = ns + 'parens-R'
            namedE.find('./{*}R-LT/{*}parens-R/{*}section-subscript-LT').tag = ns + 'element-LT'
            for ss in namedE.findall('./{*}R-LT/{*}parens-R/{*}element-LT/{*}section-subscript'):
                ss.tag = ns + 'element'
                lowerBound = ss.find('./{*}lower-bound')
                for item in lowerBound:
                    ss.append(item)
                ss.remove(lowerBound)

    # 0 - Preparation
    varList = getVarList(doc)
    addArrayParentheses(doc, varList=varList)
    expandAllArraysPHYEX(doc)
    if simplify:
        attachArraySpecToEntity(doc)
    varList = getVarList(doc) #Update list with indexes added by expandAllArraysPHYEX
    HupperBounds = [v[1] for v in indexToCheck.values()] #Upper bounds for horizontal dimensions

    #Loop on all scopes (reversed order); except functions (in particular FWSED from ice4_sedimentation_stat)
    for scope in [scope for scope in getScopesList(doc, withNodes='tuple')[::-1]
                  if 'func:' not in scope[0]]:
        scopeName, scopeNode = scope
        if scopeName in stopScopes or isUnderStopScopes(scopeName, descTree, stopScopes, includeInterfaces=True):
            indexRemoved = []
            #We build a fake scopeNode that don't contain the 'CONTAINS' part
            virtualScopeNode = ET.Element('virtual')
            virtualScopeNode.extend(getScopeChildNodes(doc, scopeNode))

            # 1 - Remove all DO loops on JI and JJ for preparation to compute on KLEV only 
            # Look for all do-nodes, check if the loop-index is one of the authorized list (indexToCheck),
            # if found, removes it
            for doNode in virtualScopeNode.findall('.//{*}do-construct')[::-1]:
                for loopI in doNode.findall('./{*}do-stmt/{*}do-V/{*}named-E/{*}N'):
                    loopIname = n2name(loopI).upper()
                    if loopIname in indexToCheck.keys():
                        # Move the content of the doNode (except do-stmt and end_do_stmt) in parent node
                        par = getParent(doc, doNode)
                        index = list(par).index(doNode)
                        for item in doNode[1:-1][::-1]:
                           par.insert(index, item)
                        par.remove(doNode) # remove the do_construct
                        if loopIname not in indexRemoved:
                            indexRemoved.append(loopIname)

            # 2 - Reduce horizontal dimensions for intrinsic array functions
            # SUM(X(:,:)) => SUM(X(JI, X))
            # In the simplify==True case, SUM(X(:,:)) becomes SUM(X(:)) by removing first dimension
            for intr in virtualScopeNode.findall('.//{*}R-LT/{*}parens-R/../..'):
                intrName = n2name(intr.find('./{*}N')).upper()
                if intrName in ('PACK', 'UNPACK', 'COUNT', 'MAXVAL', 'MINVAL', 'ALL', 'ANY', 'SUM'):
                    # Is it part of an expression or of an affectation statement?
                    # eg: CALL(UNPACK(Y(:), MASK=G(:,:)) * Z(:))
                    # or  X(:,:) = UNPACK(Y(:), MASK=G(:,:)) * Z(:)
                    # If yes, we also need to transform X and Z
                    # if not, only arrays inside the function are transformed
                    parToUse = intr
                    par = intr
                    while par is not None and not isStmt(par):
                        par = getParent(doc, par)
                        if par.tag.split('}')[1] in ('a-stmt', 'op-E'):
                            parToUse = par

                    # 2.1 Loop on all arrays in the expression using this intrinsic function
                    #     to replace horizontal dimensions by indexes
                    for namedE in parToUse.findall('.//{*}R-LT/{*}array-R/../..'):
                        slice2index(namedE, scopeName, varList)

                    # 2.2 Replace intrinsic function when argument becomes a scalar
                    if intr.find('.//{*}R-LT/{*}array-R') is None:
                        if intrName in ('MAXVAL', 'MINVAL', 'SUM', 'ALL', 'ANY'):
                            #eg: MAXVAL(X(:)) => MAXVAL(X(JI)) => X(JI)
                            parens = intr.find('./{*}R-LT/{*}parens-R')
                            parens.tag = '{http://fxtran.net/#syntax}parens-E'
                            intrPar = getParent(doc, intr)
                            intrPar.insert(list(intrPar).index(intr), parens)
                            intrPar.remove(intr)
                        elif intrName == 'COUNT':
                            #eg: COUNT(X(:)) => COUNT(X(JI)) => MERGE(1, 0., X(JI))
                            N = intr.find('./{*}N')
                            for item in N[1:]:
                                N.remove(item)
                            N.find('./{*}n').text = 'MERGE'
                            elementLT = intr.find('./{*}R-LT/{*}parens-R/{*}element-LT')
                            for v in (1, 0):
                                element = ET.Element('{http://fxtran.net/#syntax}element')
                                element.append(createExprPart(v))
                                element.tail = ', '
                                elementLT.insert(0, element)

            if simplify:
                # 3 - Remove useless dimensions
                # Arrays only on horizontal dimensions are transformed into scalars
                #  - at declaration "REAL :: P(D%NIT)" => "REAL :: P"
                #  - during call "CALL FOO(P(:)" => "CALL FOO(P)"
                #                "CALL FOO(Z(:,IK)" => "CALL FOO(Z(JIJ,IK)"
                #  - but "CALL FOO(Z(:,:)" is kept untouched
                # All arrays are transformed except IN/OUT arrays of the top subroutine (stopScopes)
                # that cannot be transformed into scalar

                # At least for rain_ice.F90, inlining must be performed before executing this code
                assert doc.find('.//{*}include') is None and doc.find('.//{*}include-stmt') is None, \
                       "inlining must be performed before removing horizontal dimensions"

                if scopeName in stopScopes:
                    #List of dummy arguments whose shape cannot be modified
                    preserveShape = [v['n'] for v in varList if (v['scope'] == scopeName and v['arg'])]
                else:
                    preserveShape = []

                # 4 - For all subroutines or modi_ interface
                if 'sub:' in scopeName:
                    # Remove dimensions in variable declaration statements
                    for decl in virtualScopeNode.findall('.//{*}T-decl-stmt/{*}EN-decl-LT/{*}EN-decl'):
                        name = n2name(decl.find('./{*}EN-N/{*}N')).upper()
                        if name not in preserveShape:
                            varsShape = decl.findall('.//{*}shape-spec-LT')
                            for varShape in varsShape:
                                n = varShape.findall('.//{*}shape-spec')
                                if (len(n) == 1 and alltext(n[0]) in HupperBounds) \
                                or (len(n) == 2 and (alltext(n[0]) in HupperBounds and \
                                                     alltext(n[1]) in HupperBounds)):
                                    #Transform array declaration into scalar declaration
                                    itemToRemove = getParent(doc, varShape)
                                    getParent(doc, itemToRemove).remove(itemToRemove)
                    # Remove suppressed dimensions "Z(JIJI)" => "Z"
                    # We cannot do this based upon declaration transformation because an array can be
                    # declared in one scope and used in another sub-scope
                    for namedE in virtualScopeNode.findall('.//{*}named-E/{*}R-LT/{*}parens-R/../..'):
                        if not n2name(namedE.find('./{*}N')).upper() in preserveShape:
                            var = findVar(doc, n2name(namedE.find('./{*}N')).upper(),
                                                  scopeName, varList=varList)
                            if var is not None and var['as'] is not None and len(var['as']) > 0:
                                subs = namedE.findall('./{*}R-LT/{*}parens-R/{*}element-LT/{*}element')
                                if (len(subs) == 1 and var['as'][0][1] in HupperBounds) or \
                                   (len(subs) == 2 and var['as'][0][1] in HupperBounds and \
                                                       var['as'][1][1] in HupperBounds):
                                    namedE.remove(namedE.find('./{*}R-LT'))
                    # Remove (:) or (:,:) for horizontal array in call-statement
                    # or replace ':' by index
                    for call in virtualScopeNode.findall('.//{*}call-stmt'):
                        for namedE in call.findall('./{*}arg-spec//{*}named-E'):
                            subs = namedE.findall('.//{*}section-subscript')
                            var = findVar(doc, n2name(namedE.find('./{*}N')).upper(),
                                          scopeName, varList=varList)
                            if len(subs) > 0 and (var is None or var['as'] is None or len(var['as']) < len(subs)):
                                #Before adding a warning, functions (especially unpack) must be recognised
                                #logging.warning(("Don't know if first dimension of {name} must be " + \
                                #                 "modified or not -> kept untouched"
                                #                ).format(name=alltext(namedE)))
                                remove = False #to remove completly the parentheses
                                index = False #to transform ':' into index
                            elif len(subs) >= 2 and \
                                 ':' in alltext(subs[0]) and var['as'][0][1] in HupperBounds and \
                                 ':' in alltext(subs[1]) and var['as'][1][1] in HupperBounds:
                                #eg: CALL(P(:, :)) with SIZE(P, 1) == D%NIT and SIZE(P, 2) == D%NJT
                                remove = len(subs) == 2
                                index = len(subs) > 2 and len([sub for sub in subs if ':' in alltext(sub)]) == 2
                            elif len(subs) >= 1 and \
                                 ':' in alltext(subs[0]) and var['as'][0][1] in HupperBounds:
                                #eg: CALL(P(:)) with SIZE(P, 1) == D%NJT
                                remove = len(subs) == 1
                                index = len(subs) > 1 and len([sub for sub in subs if ':' in alltext(sub)]) ==  1
                            else:
                               remove = False
                               index = False
                            if remove:
                                if n2name(namedE.find('./{*}N')).upper() in preserveShape:
                                    slice2index(namedE, scopeName, varList)
                                else:
                                    RLT = namedE.find('.//{*}R-LT')
                                    getParent(namedE, RLT).remove(RLT)
                            if index:
                                slice2index(namedE, scopeName, varList)

            # 4 - Values for removed indexes
            for loopIndex in indexRemoved:
                # Initialize former indexes JI,JJ,JIJ to first array element : JI=D%NIB, JJ=D%NJB, JIJ=D%NIJB
                insertStatement(doc, scopeName, createExpr(loopIndex + " = " + indexToCheck[loopIndex][0])[0], True)
                addArgInTree(doc, scopeName, descTree, 'D', 'TYPE(DIMPHYEX_t) :: D',
                             0, stopScopes, moduleVarList=[('MODD_DIMPHYEX', ['DIMPHYEX_t'])],
                             parser=parser, parserOptions=parserOptions, wrapH=wrapH)
                # Check loop index presence at declaration of the scope
                if findVar(doc, loopIndex, scopeName, varList=varList, exactScope=True) is None:
                    addVar(doc, [[scopeName, loopIndex, 'INTEGER :: ' + loopIndex, None]])


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
    elif upper_decl.upper() in ('D%NIT', 'IIE', 'IIU') or lower_decl.upper() == 'IIB' or \
         'D%NIT' in upper_decl.upper():
        varName = 'JI'
    elif upper_decl.upper() in ('D%NJT', 'IJE', 'IJU') or lower_decl.upper() == 'IJB' or \
         'D%NJT' in upper_decl.upper():
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
    
def addACC_data(doc):
    """
    1) Add after declaration:
    !$acc data present ( list of intent arrays)
    2) Add at the end of the routine
    !$acc end data
    :param doc: etree to use
    """
    locations  = getScopesList(doc,withNodes='tuple')
    mod_type = locations[0][0].split('/')[-1].split(':')[1][:4]
    if mod_type == 'MODD':
        pass
    else:
        for loc in locations:
            # Do not add !$acc data directives to :
            # - MODULE or FUNCTION object, 
            # - interface subroutine from a MODI
            # but only to SUBROUTINES
            if 'sub:' in loc[0] and 'func' not in loc[0] and 'interface' not in loc[0]:
                scopepath = getScopePath(doc,loc[1])
                varList = getVarList(doc,scopepath)
                
                # Look for all intent arrays only
                arraysIntent = []
                for var in varList:
                     if var['arg'] and var['as'] and 'TYPE' not in var['t']: #intent arrays, not of type TYPE (only REAL, INTEGER, CHARACTER)
                         arraysIntent.append(var['n'])
                # Check if there is any intent variables
                if len(arraysIntent)== 0: 
                    break
                
                #1) !$acc data present()
                list_var = "!$acc data present ( "
                count=0
                for var in arraysIntent:
                    if count>6:
                        list_var = list_var + '\n!$acc &              '
                        count=0
                    list_var = list_var + var + ", "
                    count+=1
                list_var_end = list_var[:-2] # remove last comma
                
                fortranSource = "SUBROUTINE FOO598756\n"+ list_var_end + ")\nEND SUBROUTINE"
                _, cfxtran = fortran2xml(fortranSource)
                comment = cfxtran.findall('.//{*}C')
                for com in comment:
                    insertStatement(doc,loc[0],indent(com),first=True) 
                
                #2) !$acc end data
                fortranSource = "SUBROUTINE FOO598756\n !$acc end data \nEND SUBROUTINE"
                _, cfxtran = fortran2xml(fortranSource)
                comment = cfxtran.find('.//{*}C')
                insertStatement(doc,loc[0],indent(comment),first=False)
   
class Applications():
    @copy_doc(addStack)
    def addStack(self, *args, **kwargs):
        return addStack(self._xml, *args, **kwargs)  
    
    @copy_doc(addMPPDB_CHECKS)
    def addMPPDB_CHECKS(self, *args, **kwargs):
        return addMPPDB_CHECKS(self._xml, *args, **kwargs)
    
    @copy_doc(addACC_data)
    def addACC_data(self, *args, **kwargs):
        return addACC_data(self._xml, *args, **kwargs)
        
    @copy_doc(deleteDrHook)
    def deleteDrHook(self, *args, **kwargs):
        return deleteDrHook(self._xml, *args, **kwargs)

    @copy_doc(deleteBudgetDDH)
    def deleteBudgetDDH(self, *args, **kwargs):
        return deleteBudgetDDH(self._xml, *args, **kwargs)

    @copy_doc(deleteNonColumnCallsPHYEX)
    def deleteNonColumnCallsPHYEX(self, *args, **kwargs):
        return deleteNonColumnCallsPHYEX(self._xml, *args, **kwargs)

    @copy_doc(removeIJDim)
    def removeIJDim(self, *args, **kwargs):
        return removeIJDim(self._xml, *args, **kwargs)
    
    @copy_doc(removePHYEXUnusedLocalVar)
    def removePHYEXUnusedLocalVar(self, *args, **kwargs):
        return removePHYEXUnusedLocalVar(self._xml, *args, **kwargs)
    
    @copy_doc(inlineContainedSubroutinesPHYEX)
    def inlineContainedSubroutinesPHYEX(self, *args, **kwargs):
        return inlineContainedSubroutinesPHYEX(self._xml, *args, **kwargs)

    @copy_doc(expandAllArraysPHYEX)
    def expandAllArraysPHYEX(self, *args, **kwargs):
        return expandAllArraysPHYEX(self._xml, *args, **kwargs)
