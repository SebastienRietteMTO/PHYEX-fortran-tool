"""
This module contains the functions to browse the tree
"""

import glob
import os
import logging
from pyft.util import debugDecor, copy_doc, PYFTError, n2name
from pyft.scope import getScopeChildNodes
from pyft.variables import getVarList, findVar
import json
import subprocess
import xml.etree.ElementTree as ET

@debugDecor
def getDirs(tree):
    """
    :param tree: list of directories composing the tree or None
    :return: list of directories and subdirectories
    """
    r = []
    if tree is not None:
        for t in tree:
            r += glob.glob("**/")
    return r

@debugDecor
def getFiles(tree):
    """
    :param tree: list of directories composing the tree or None
    :return: list of directories and subdirectories
    """
    filenames = []
    for t in tree:
        for filename in glob.glob(t + '/**/*', recursive=True):
            if os.path.splitext(filename)[1] not in ('', '.json', '.fypp', '.txt'):
                #We only keep files with extension
                filenames.append(filename)
    return filenames

@debugDecor
def descTree(tree, descTree, parser, parserOptions, wrapH):
    """
    Build the description tree file
    :param tree: list of directories composing the tree or None
    :param descTree: filename where the description of th tree will be stored
    :param parser, parserOptions, wrapH: see the pyft class
    """
    def extract_string(text):
        text = text.strip()
        if text[0] in ('"', "'"):
            assert text[-1] == text[0]
            text = text[1, -1]
        return text

    from pyft import PYFT
    if not os.path.exists(descTree):
        if tree is None:
            raise PYFTError('You must provide tree when descTree is set')
        options = parserOptions
        if options is not None and len(set(options).intersection(('-no-include', '-noinclude'))) == 0:
            #We must not include 'include files' when analysing the tree
            options.append('-no-include')

        result = {'cwd': os.getcwd(), 'compilation_tree': {}, 'execution_tree': {}, 'scopes': {}}

        useList = {}
        includeList = {}
        callList = {}
        funcList = {}
        #Loop on directory and files
        for filename in getFiles(tree):
            if os.path.isfile(filename):
                pft = PYFT(filename, parser=parser, parserOptions=options, wrapH=wrapH)
                filename = filename[2:] if filename.startswith('./') else filename
                varList = getVarList(pft._xml)

                #Fill scopes
                scopes = pft.getScopesList(withNodes='tuple')
                result['scopes'][filename] = [scope[0] for scope in scopes]

                #Fill trees
                result['compilation_tree'][filename] = []

                includeList[filename] = {}
                useList[filename] = {}
                callList[filename] = {}
                funcList[filename] = {}
                for scopeName, scopeNode in scopes:
                    scope = ET.Element('scope')
                    scope.extend(getScopeChildNodes(pft._xml, scopeNode))
                    #Fill compilation_tree
                    #Includes give directly the name of the source file but possibly without the directory
                    includeList[filename][scopeName] = [f.text
                                                    for f in scope.findall('.//{*}include/{*}filename')] #cpp
                    includeList[filename][scopeName].extend([extract_string(f.text)
                                               for f in scope.findall('.//{*}include/{*}filename/{*}S')]) #FORTRAN
        
                    #For use statements, we need to scan all the files to know which one contains the module
                    useList[filename][scopeName] = scope.findall('.//{*}use-stmt')
        
                    #Fill execution tree
                    #We need to scan all the files to find which one contains the subroutine/function
                    callList[filename][scopeName] = [n2name(c.find('./{*}procedure-designator/{*}named-E/{*}N')).upper()
                                                     for c in scope.findall('.//{*}call-stmt')]
                    #We cannot distinguish function from arrays
                    funcList[filename][scopeName] = []
                    for name in [n2name(c.find('./{*}N')).upper()
                                 for c in scope.findall('.//{*}named-E/{*}R-LT/{*}parens-R/../..')]:
                        #But we can exclude some names if they are declared as arrays
                        var = findVar(pft._xml, name, scopeName, varList=varList)
                        if var is None or var['as'] is None:
                            funcList[filename][scopeName].append(name)
                    

        #compilation_tree computation: include
        incInScope = {}
        for filename, incScopes in includeList.items():
            #Loop on scopes
            for scope, incList in incScopes.items():
                #Loop on each included file
                incInScope[scope] = []
                for inc in incList:
                    #Try to guess the right file
                    same = []
                    subdir = []
                    basename = []
                    #Loop on each file found in the source tree
                    for f in result['compilation_tree']:
                        if os.path.normpath(inc) == os.path.normpath(f):
                            #Exactly the same file name (including directories)
                            same.append(f)
                        elif (not os.path.isabs(f)) and \
                             os.path.realpath(inc) == os.path.realpath(os.path.join(os.path.dirname(inc), f)):
                            #The include statement refers to a file contained in the directory where inc is
                            subdir.append(f)
                        elif os.path.basename(inc) == os.path.basename(f):
                            #Same name excluding the directories
                            basename.append(f)
                    if len(same) > 1: same = subdir = basename = []
                    if len(subdir) > 1: subdir = basename = []
                    if len(basename) > 1: basename = []
                    found = True
                    if len(same) > 0:
                        incFilename = same[0]
                    elif len(subdir) > 0:
                        incFilename = subdir[0]
                    elif len(basename) > 0:
                        incFilename = basename[0]
                    else:
                        #We haven't found the file in the tree, we keep the inc untouched
                        found = False
                        incFilename = inc
                    result['compilation_tree'][filename].append(incFilename)
                    if found: incInScope[scope].append(incFilename)

        #compilation_tree computation: use
        for filename, uList in useList.items():
            #Loop on each use statement
            for use in [use for l in uList.values() for use in l]:
                moduleScopeName = 'module:' + n2name(use.find('./{*}module-N/{*}N')).upper()
                #Loop on scopes to find the module
                found = []
                for f, scopes in result['scopes'].items():
                    if moduleScopeName in scopes:
                        found.append(f)
                if len(found) == 1:
                    result['compilation_tree'][filename].append(found[0])
                else:
                    logging.warning(('Several or none file containing the scope {scope} have been found ' + \
                                     'for file {filename}').format(scope=moduleScopeName, filename=filename))

        #compilation_tree: cleaning (uniq values)
        for filename, depList in result['compilation_tree'].items():
            result['compilation_tree'][filename] = list(set(depList))

        #execution_tree: call statements
        allScopes = [scope for _, l in result['scopes'].items() for scope in l]
        result['execution_tree'] = {scope: [] for scope in allScopes}
        for canonicKind, progList in (('sub', callList), ('func', funcList)):
            for filename, callScopes in progList.items():
                #Loop on scopes
                for scope, cList in callScopes.items():
                    #Loop on calls
                    for c in set(cList):
                        foundInUse = []
                        foundElsewhere = []
                        foundInInclude = []
                        foundInContains = []
                        foundInSameScope = []

                        #We look for sub:c or interface:c
                        for kind in (canonicKind, 'interface'):
                            #Loop on each use statement in scope or in upper scopes
                            uList = [useList[filename][sc] for sc in useList[filename]
                                     if (sc == scope or scope.startswith(sc + '/'))]
                            for use in [use for l in uList for use in l]:
                                moduleScope = 'module:' + n2name(use.find('./{*}module-N/{*}N')).upper()
                                only = [n2name(n).upper() for n in use.findall('.//{*}use-N//{*}N')]
                                callScope = moduleScope + '/' + kind + ':' + c
                                if len(only) > 0:
                                    #There is a "ONLY" keyword
                                    if c in only and callScope in allScopes:
                                        foundInUse.append(callScope)
                                else:
                                    #There is no "ONLY"
                                    for _, scopes in result['scopes'].items():
                                        if callScope in scopes:
                                            foundInUse.append(callScope)
    
                            #Look for subroutine directly accessible
                            callScope = kind + ':' + c
                            for _, scopes in result['scopes'].items():
                                if callScope in scopes:
                                    foundElsewhere.append(callScope)
    
                            #Look for include files
                            callScope = kind + ':' + c
                            for incFile in incInScope[scope]:
                                if callScope in result['scopes'][incFile]:
                                    foundInInclude.append(callScope)
    
                            #Look for contained routines
                            callScope = scope + '/' + kind + ':' + c
                            if callScope in result['scopes'][filename]:
                                foundInContains.append(callScope)

                            #Look for routine in the same scope
                            if '/' in scope:
                                callScope = scope.rsplit('/', 1)[0] + '/' + kind + ':' + c
                            else:
                                callScope = kind + ':' + c
                            if callScope in result['scopes'][filename]:
                                foundInSameScope.append(callScope)

                        #Final selection
                        foundInUse = list(set(foundInUse)) #If a module is used several times
                        if len(foundInUse + foundInInclude + foundInContains + foundInSameScope) > 1:
                            logging.error(('Several definition of the program unit found for {callScope} ' + \
                                           'called in {scope}:').format(callScope=c, scope=scope))
                            logging.error('  found {i} time(s) in USE statements'.format(i=len(foundInUse)))
                            logging.error('  found {i} time(s) in include files'.format(i=len(foundInInclude)))
                            logging.error('  found {i} time(s) in CONTAINS block'.format(i=len(foundInContains)))
                            logging.error('  found {i} time(s) in the same scope'.format(i=len(foundInSameScope)))
                            print(foundInUse)
                            result['execution_tree'][scope].append('??')
                        elif len(foundInUse + foundInInclude + foundInContains + foundInSameScope) == 1:
                            r = (foundInUse + foundInInclude + foundInContains + foundInSameScope)[0]
                            if canonicKind != 'func' or r in allScopes:
                                result['execution_tree'][scope].append(r)
                        elif len(foundElsewhere) > 1:
                            logging.warning(('Several definition of the program unit found for {callScope} ' + \
                                             'called in {scope}').format(callScope=c, scope=scope))
                        elif len(foundElsewhere) == 1:
                            result['execution_tree'][scope].append(foundElsewhere[0])
                        else:
                            if canonicKind != 'func':
                                logging.warning(('No definition of the program unit found for {callScope} ' + \
                                                 'called in {scope}').format(callScope=c, scope=scope))

        #execution_tree: cleaning (uniq values)
        for scope, execList in result['execution_tree'].items():
            result['execution_tree'][scope] = list(set(execList))

        with open(descTree, 'w') as f:
            json.dump(result, f)

def toJson(descTree):
    if isinstance(descTree, str):
        with open(descTree, 'r') as f:
            return json.load(f)
    else:
        return descTree

def _recurList(node, descTreePart, level, down):
    """
    :param node: initial node
    :param descTreePart: 'compilation_tree' or 'execution_tree' part of a descTree object
    :param level: number of levels (0 to get only the initial node, None to get all nodes)
    :param down: True to get the nodes lower in the tree, False to get the upper ones
    :return: list of nodes lower or upper tahn initial node (recursively)
    """
    def recur(n, level):
        if down:
            result = descTreePart.get(n, [])
        else:
            result = [item for item, l in descTree.items() if n in l]
        if level is not None or level > 1:
            for r in list(result):
                result.extend(recur(r, None if level is None else level - 1, down))
    return recur(filename, level)

def needsFile(filename, descTree, level):
    """
    :param filename: initial file name
    :param descTree: tree description file (obtained by descTree) or its json equivalence
    :param level: number of levels (0 to get only the initial file, None to get all files)
    :return: list of file names needed by the initial file (recursively)
    """
    return _recurList(filename, descTree['compilation_tree'], True)

def neededByFile(filename, descTree, level):
    """
    :param filename: initial file name
    :param descTree: tree description file (obtained by descTree) or its json equivalence
    :param level: number of levels (0 to get only the initial file, None to get all files)
    :return: list of file names that needs the initial file (recursively)
    """
    return _recurList(filename, descTree['compilation_tree'], False)

def callsScopes(scope, descTree, level):
    """
    :param scope: initial scope
    :param descTree: tree description file (obtained by descTree) or its json equivalence
    :param level: number of levels (0 to get only the initial scope, None to get all scopes)
    :return: list of file scopes called by the initial scope (recursively)
    """
    return _recurList(scope, descTree['execution_tree'], True)

def neededByFile(scope, descTree, level):
    """
    :param scope: initial scope
    :param descTree: tree description file (obtained by descTree) or its json equivalence
    :param level: number of levels (0 to get only the initial scope, None to get all scopes)
    :return: list of file scopes that calls the initial scope (recursively)
    """
    return _recurList(scope, descTree['execution_tree'], False)

@debugDecor
def plotTree(centralNodeList, descTree, output, plotMaxUpper, plotMaxLower, kind, frame=False):
    """
    Compute a dependency graph
    :param centralNodeList: file, scope, list of files or list of scopes
    :param descTree: tree description file (obtained by descTree)
    :param output: output file name (.dot or .png extension)
    :param plotMaxUpper: Maximum number of elements to plot, upper than the central element
    :param plotMaxLower: Maximum number of elements to plot, lower than the central element
    :param kind: must be 'compilation_tree' or 'execution_tree'
    :param frame: True to plot a frame grouping the central nodes
    """
    assert kind in ('compilation_tree', 'execution_tree')
    def h(obj):
        result = str(hash(obj))
        if result[0] == '-':
            result = 'M' + result[1:] #to minus sign
        return result
    def createNode(node, label=None):
        result = ""
        if label is not None:
            result += "subgraph cluster_" + h(node) + " {\n"
            result += 'label="{label}"\n'.format(label=label)
        if kind == 'execution_tree':
            color = 'blue' if node.split('/')[-1].split(':')[0] == 'func' else 'green'
        else:
            color = 'black'
        result += h(node) + ' [label="{node}" color="{color}"]\n'.format(node=node, color=color)
        if label is not None:
            result += "}\n"
        return result
    def createLink(file1, file2):
        return h(file1) + ' -> ' + h(file2) + '\n'
    def createCluster(nodes, label=None):
        result = "subgraph cluster_R {\n"
        result += "{rank=same " + (' '.join([h(node) for node in nodes])) + "}\n"
        if label is not None:
            result += 'label="{label}"\n'.format(label=label)
        result += "}\n"
        return result
    def add(item):
        if item not in dot: dot.append(item)
    def filename(scope):
        if kind == 'compilation_tree':
            return None
        else:
            return [f for f, l in descTree['scopes'].items() if scope in l][0]
    def recur(node, level, down):
        if level is None or level > 0:
            if down:
                result = descTree[kind].get(node, [])
            else:
                result = [f for f, l in descTree[kind].items()
                          if node in l]
            for r in result:
                add(createNode(r, filename(r)))
                add(createLink(node, r) if down else createLink(r, node))
                if level is None or level > 1:
                    recur(r, None if level is None else level - 1, down)

    # Read the tree description file
    descTree = toJson(descTree)

    # Are all the central scopes in the same file
    printInFrame = False
    if kind == 'execution_tree':
        centralScopeFilenames = []
        for scope in centralNodeList:
            centralScopeFilenames.append(filename(scope))
        centralScopeFilenames = list(set(centralScopeFilenames))
        if len(centralScopeFilenames) == 1:
            frame = True
            printInFrame = True
        else:
            printInFrame = False

    dot = ["digraph D {\n"]
    if not isinstance(centralNodeList, list):
        centralNodeList = [centralNodeList]
    for centralNode in centralNodeList:
        add(createNode(centralNode, None if printInFrame else filename(centralNode)))
        recur(centralNode, plotMaxLower, True)
        recur(centralNode, plotMaxUpper, False)
    if frame:
        if kind == 'compilation_tree':
            frameText = None
        else:
            frameText = centralScopeFilenames[0] if printInFrame else None
        add(createCluster(centralNodeList, frameText))
    add("}\n")
    dot = ''.join(dot)
    fmt = os.path.splitext(output)[1].lower()[1:]
    if fmt == 'dot':
        with open(output, 'w') as f:
            f.write(dot)
    else:
        dotCommand = ['dot', '-T' + fmt, '-o', output]
        logging.info('Dot command: ' + ' '.join(dotCommand))
        subprocess.run(dotCommand, input=dot.encode('utf8'), check=True)
        
@debugDecor
def plotCompilTreeFromFile(filename, descTree, output, plotMaxUpper, plotMaxLower):
    """
    Compute the compilation dependency graph
    :param filename: central file
    :param descTree: tree description file (obtained by descTree)
    :param output: output file name (.dot or .png extension)
    :param plotMaxUpper: Maximum number of elements to plot, upper than the central element
    :param plotMaxLower: Maximum number of elements to plot, lower than the central element
    """
    return plotTree(filename, descTree, output, plotMaxUpper, plotMaxLower, 'compilation_tree', True)

@debugDecor
def plotExecTreeFromScope(scope, descTree, output, plotMaxUpper, plotMaxLower):
    """
    Compute the execution dependency graph
    :param scope: central scope
    :param descTree: tree description file (obtained by descTree)
    :param output: output file name (.dot or .png extension)
    :param plotMaxUpper: Maximum number of elements to plot, upper than the central element
    :param plotMaxLower: Maximum number of elements to plot, lower than the central element
    """
    return plotTree(scope, descTree, output, plotMaxUpper, plotMaxLower, 'execution_tree')

@debugDecor
def plotCompilTreeFromScope(scope, descTree, output, plotMaxUpper, plotMaxLower):
    """
    Compute the compilation dependency graph
    :param scope: central scope
    :param descTree: tree description file (obtained by descTree)
    :param output: output file name (.dot or .png extension)
    :param plotMaxUpper: Maximum number of elements to plot, upper than the central element
    :param plotMaxLower: Maximum number of elements to plot, lower than the central element
    """
    descTree = toJson(descTree)
    return plotTree([filename for filename, scopes in descTree['scopes'] if scope in scopes],
                    descTree, output, plotMaxUpper, plotMaxLower, 'compilation_tree')

@debugDecor
def plotExecTreeFromFile(filename, descTree, output, plotMaxUpper, plotMaxLower):
    """
    Compute the execution dependency graph
    :param filename: central filename
    :param descTree: tree description file (obtained by descTree)
    :param output: output file name (.dot or .png extension)
    :param plotMaxUpper: Maximum number of elements to plot, upper than the central element
    :param plotMaxLower: Maximum number of elements to plot, lower than the central element
    """
    descTree = toJson(descTree)
    return plotTree(descTree['scopes'][filename], descTree, output, plotMaxUpper, plotMaxLower,
                    'execution_tree', True)


class Tree():
    @copy_doc(getDirs)
    def getDirs(self):
        return getDirs(tree=self.tree)

    @copy_doc(descTree)
    def descTree(self, *args, **kwargs):
        return descTree(*args, **kwargs)

    @copy_doc(plotCompilTreeFromFile)
    def plotCompilTreeFromFile(self, *args, **kwargs):
        return plotCompilTreeFromFile(*args, **kwargs)

    @copy_doc(plotExecTreeFromFile)
    def plotExecTreeFromFile(self, *args, **kwargs):
        return plotExecTreeFromFile(*args, **kwargs)

    @copy_doc(plotCompilTreeFromScope)
    def plotCompilTreeFromFile(self, *args, **kwargs):
        return plotCompilTreeFromFile(*args, **kwargs)

    @copy_doc(plotExecTreeFromScope)
    def plotExecTreeFromFile(self, *args, **kwargs):
        return plotExecTreeFromFile(*args, **kwargs)
