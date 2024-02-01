"""
This module contains the functions to browse the tree
"""

import glob
import os
import logging
from pyft.util import debugDecor, copy_doc, PYFTError, n2name
import json
import subprocess

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
        result = {'cwd': os.getcwd(), 'compilation_tree': {}, 'execution_tree': {}, 'scopes': {}}

        useList = {}
        includeList = {}
        #Loop on directory and files
        for filename in getFiles(tree):
            if os.path.isfile(filename):
                pft = PYFT(filename, parser=parser, parserOptions=parserOptions, wrapH=wrapH)
                filename = filename[2:] if filename.startswith('./') else filename

                #Fill scopes
                result['scopes'][filename] = pft.getScopesList()

                #Fill compilation_tree
                result['compilation_tree'][filename] = []
                #Includes give directly the name of the source file but possibly without the directory
                includeList[filename] = [f.text for f in pft.findall('.//{*}include/{*}filename')] #cpp
                includeList[filename].extend([extract_string(f.text)
                                              for f in pft.findall('.//{*}include/{*}filename/{*}S')]) #FORTRAN

                #For use statements, we need to scan all the files to know which one contains the module
                useList[filename] = pft.findall('.//{*}use-stmt')

        #compilation_tree computation: include
        for filename, incList in includeList.items():
            #Loop on each included file
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
                if len(same) > 0:
                    result['compilation_tree'][filename].append(same[0])
                elif len(subdir) > 0:
                    result['compilation_tree'][filename].append(subdir[0])
                elif len(basename) > 0:
                    result['compilation_tree'][filename].append(basename[0])
                else:
                    #We haven't found the file in the tree, we keep the inc untouched
                    result['compilation_tree'][filename].append(inc)

        #compilation_tree computation: use
        for filename, uList in useList.items():
            #Loop on each use statement
            for use in uList:
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
        
        with open(descTree, 'w') as f:
            json.dump(result, f)

def toJson(descTree):
    if os.path.isfile(descTree):
        with open(descTree, 'r') as f:
            return json.load(f)
    else:
        return descTree

def needs(filename, descTree, level):
    """
    :param filename: initial file name
    :param descTree: tree description file (obtained by descTree) or its json equivalence
    :param level: number of levels (0 to get only the initial file, None to get all files)
    :return: list of file names needed by the initial file (recursively)
    """
    def recur(f, level):
        result = descTree['compilation_tree'].get(f, [])
        if level is not None or level > 1:
            for r in list(result):
                result.extend(recur(r, level - 1))
    return recur(filename, level)

@debugDecor
def plotCompilTree(filename, descTree, output, plotMaxUpper, plotMaxLower):
    """
    Compute the compilation dependency graph
    :param filename: central file
    :param descTree: tree description file (obtained by descTree)
    :param output: output file name (.dot or .png extension)
    :param plotMaxUpper: Maximum number of elements to plot, upper than the central element
    :param plotMaxLower: Maximum number of elements to plot, lower than the central element
    """
    def createNode(filename):
        return str(hash(filename)) + ' [label="{filename}"]'.format(filename=filename)
    def createLink(file1, file2):
        return str(hash(file1)) + ' -> ' + str(hash(file2))
    def add(item):
        if item not in dot: dot.append(item)
    def recur(filename, level, down):
        if level is None or level > 0:
            if down:
                result = descTree['compilation_tree'].get(filename, [])
            else:
                result = [f for f, l in descTree['compilation_tree'].items()
                          if filename in l]
            for r in result:
                add(createNode(r))
                add(createLink(filename, r) if down else createLink(r, filename))
                if level is None or level > 1:
                    recur(r, None if level is None else level - 1, down)

    # Read the tree description file
    descTree = toJson(descTree)

    dot = ["digraph D {"]
    add(createNode(filename))
    recur(filename, plotMaxLower, True)
    recur(filename, plotMaxUpper, False)
    add("}")
    dot = '\n'.join(dot)
    fmt = os.path.splitext(output)[1].lower()[1:]
    if fmt == 'dot':
        with open(output, 'w') as f:
            f.write(dot)
    else:
        dotCommand = ['dot', '-T' + fmt, '-o', output]
        logging.info('Dot command: ' + ' '.join(dotCommand))
        subprocess.run(dotCommand, input=dot.encode('utf8'), check=True)
        

class Tree():
    @copy_doc(getDirs)
    def getDirs(self):
        return getDirs(tree=self.tree)

    @copy_doc(descTree)
    def descTree(self, *args, **kwargs):
        return descTree(*args, **kwargs)

    @copy_doc(plotCompilTree)
    def plotCompilTree(self, *args, **kwargs):
        return plotCompilTree(*args, **kwargs)
