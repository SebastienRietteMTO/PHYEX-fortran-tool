import xml.etree.ElementTree as ET
from functools import wraps
import logging
import tempfile
import os
import subprocess
import logging
import time

"""
This module implements some tools to manipulate the xml
"""

################################################################################
### Verbosity, decorators and Exception

debugStats = {}

def debugDecor(func):
    """
    Defines a decorator to trace all function calling with arguments and results
    and count number of calls and time spent
    """
    @wraps(func)
    def wrapper(*args, **kwargs):
        #Logging call
        logger = logging.getLogger()
        if logger.isEnabledFor(logging.DEBUG):
            callstr = func.__name__ + \
                      '(' + ', '.join([str(a) for a in args] + \
                                      [k + '=' + str(v) for (k, v) in kwargs.items()]) + ')'
            logging.debug(callstr + ' --> ...')

        #Count and time
        if logger.isEnabledFor(logging.INFO):
            t0 = time.time()

        #effective call
        result = func(*args, **kwargs)

        #Count and time
        if logger.isEnabledFor(logging.INFO):
            import util
            if not func.__name__ in util.debugStats:
                util.debugStats[func.__name__] = dict(nb=0, totalTime=0)
            util.debugStats[func.__name__]['nb'] += 1
            duration = time.time() - t0
            util.debugStats[func.__name__]['totalTime'] += duration
            util.debugStats[func.__name__]['min'] = min(duration, util.debugStats[func.__name__].get('min', duration))
            util.debugStats[func.__name__]['max'] = max(duration, util.debugStats[func.__name__].get('max', duration))

        #logging result
        if logger.isEnabledFor(logging.DEBUG):
            logging.debug(callstr + ' --> ' + str(result))

        #return result
        return result
    return wrapper

def set_verbosity(level):
    """
    Set the verbosity level
    :param level: verbosity level used to set the logging module
    """
    logger = logging.getLogger()
    if isinstance(level, str):
        logger.setLevel(level=level.upper())
    else:
        logger.setLevel(level=level)

def print_infos():
    logger = logging.getLogger()
    if logger.isEnabledFor(logging.INFO):
        def _print(name, nb, min, max, mean):
            print('| ' + name.ljust(30) + '| ' + str(nb).ljust(14) + '| ' + \
                  str(min).ljust(23) + '| ' + str(max).ljust(23) + '| ' + str(mean).ljust(23) + '|')
        _print('Name of the function', '# of calls', 'Min (s)', 'Maxi (s)', 'Total (s)')
        import util
        for funcName, values in util.debugStats.items():
            _print(funcName, values['nb'], values['min'], values['max'], values['totalTime'])

def copy_doc(copy_func):
    """
    Decorator to copy the doc from other function
    """
    def wrapper(func):
        func.__doc__ = "This method applies the eponym function over " + \
                       "the entire xml tree.\n\n" + \
                       "Here's the doc for the eponym function:\n" + \
                       copy_func.__doc__
        return func
    return wrapper

class PFTError(Exception): pass

################################################################################
### Conversions

def fortran2xml(fortranSource, parser='fxtran', parserOptions=None):
    """
    :param fortranSource: a string containing a fortran source code
                          or a filename
    :param parser: path to the fxtran parser
    :param parserOptions: dictionnary holding the parser options
    :returns: (ns, xml) where ns is a namespace dictionnary and xml
              is an ET xml document
    """
    #Namespace registration
    ns = {'f': 'http://fxtran.net/#syntax'}
    #Alternatively, we could load a first time to find out the namespaces, then reload
    #it after having registered the right namespace. The folowing code snippet
    #allows to capture the declared namespaces.
    #ns = dict([node for _, node in ET.iterparse(StringIO(self.xml), events=['start-ns'])])
    for k, v in ns.items():
        ET.register_namespace(k, v)

    #Default options
    if parserOptions is None:
        import pypft
        parserOptions = pypft.PFT.DEFAULT_FXTRAN_OPTIONS

    #Call to fxtran
    with tempfile.NamedTemporaryFile(buffering=0, suffix='.F90') as f:
        if os.path.exists(fortranSource):
            #tempfile not needed in this case but I found easier to write code
            #like this to have only one subprocess call and automatic
            #deletion of the temporary file
            filename = fortranSource
        else:
            filename = f.name
            f.write(fortranSource.encode('UTF-8'))
        xml = subprocess.run([parser, filename,
                             '-o', '-'] + parserOptions,
                             stdout=subprocess.PIPE, check=True,
                             encoding='UTF-8').stdout
        xml = ET.fromstring(xml, parser=ET.XMLParser(encoding='UTF-8'))

    return ns, xml

def tostring(doc):
    """
    :param doc: an ET object
    :return: xml as a string
    """
    return ET.tostring(doc, method='xml', encoding='UTF-8').decode('UTF-8')

def tofortran(doc):
    """
    :param doc: an ET object
    :return: a string representing the FORTRAN source code
    """
    #When fxtran encounters an UTF-8 character, it replaces it by *2* entities
    #We must first transform each of these entities to its corresponding binary value
    #(this is done by tostring), then we must consider the result as bytes
    #to decode these two bytes into UTF-8 (this is done by encode('raw_...').decode('UTF-8'))
    r = ET.tostring(doc, method='text', encoding='UTF-8').decode('UTF-8')
    try:
        r = r.encode('raw_unicode_escape').decode('UTF-8')
    except UnicodeDecodeError:
        logging.warning("The file '{}' certainly contains a strange character".format(getFileName(doc)))
    return r

################################################################################
### Helper functions acting on the xml

def getFileName(doc):
    """
    :param doc: an ET object
    :return: the name of the input file name or 'unknown' if not available
             in the xml fragment provided
    """
    return doc.find('.//{*}file').attrib['name']

def n2name(N):
    """
    Helper function which returns the entity name enclosed in a N tag
    """
    return ''.join([e.text for e in N.findall('./{*}n')])

def alltext(doc):
    """ 
    Helper function to iterate on all text fragment and join them
    :param doc: xml fragment
    """
    return ''.join(doc.itertext())

def non_code(e):
    """ 
    :param e: element
    :return: True if e is non code (comment, text...)
    """
    return e.tag.split('}')[1] in {'cnt', 'C'}

def isExecutableStmt(e):
    """
    :param e: element
    :return: True if element is an executable statement
    """
    return isStmt(e) and \
           not e.tag.split('}')[1] in {'subroutine-stmt', 'end-subroutine-stmt',
                                       'function-stmt', 'end-function-stmt',
                                       'use-stmt', 'T-decl-stmt', 'component-decl-stmt',
                                       'T-stmt', 'end-T-stmt',
                                       'data-stmt', 'save-stmt',
                                       'implicit-none-stmt'}

def isStmt(e):
    """
    :param e: element
    :return: True if element is a statement
    """
    return e.tag.endswith('-stmt')

def removeFromList(doc, item, l):
    """
    :param doc: etre to use (containing the list)
    :param item: item to remove from list
    :param l: the parent of item (the list)
    """

    nodesToSuppress = [item]

    #Suppression of the comma
    i = list(l).index(item)
    if item.tail is not None and ',' in item.tail:
        #There's a comma just after the node
        tail = item.tail
        item.tail = tail.replace(',', '')
    elif i != 0 and ',' in l[i - 1].tail:
        #There's a comma just before the node
        tail = l[i - 1].tail
        l[i - 1].tail = tail.replace(',', '')
    else:
        found = False
        #We look for a comma in the first node after the current node that
        #is not after another item of the list
        j = i + 1
        while j < len(l) and not found:
            if non_code(l[j]):
                #This is a candidate
                if l[j].tail is not None and ',' in l[j].tail:
                    #Comma found and suppressed
                    found = True
                    tail = l[j].tail
                    l[j].tail = tail.replace(',', '')
                else:
                    j += 1
            else:
                #This is another item
                break

        #We look for a comma in the last node before the current node that
        #is not a comment or a contiuation character
        j = i - 1
        while j >= 0 and not found:
            if l[j].tail is not None and ',' in l[j].tail:
                #Comma found and suppressed
                found = True
                tail = l[j].tail
                l[j].tail = tail.replace(',', '')
            else:
                if non_code(l[j]):
                    #We can search before
                    j -= 1
                else:
                    #This is another item
                    break
               
        if not found and \
           len([e for e in l if not non_code(e)]) != 1:
            raise RuntimeError("Something went wrong here....")

    #Suppression of continuation characters
    if i + 1 < len(l) and l[i + 1].tag.endswith('}cnt'):
        #Node is followed by a continuation character
        reason = 'lastOnLine'
        #If the node is followed by a continuation character and is just after another
        #continuation character, we must supress the continuation character which is after.
        #    USE MODD, ONLY: X, &
        #                    Y, & !Variable to suppress, with its '&' character
        #                    Z
        #In addition, if the character found before is at the begining of the line,
        #it must also be removed. To know if it is at the begining of the line,
        #we can check if another continuation character is before.
        #    USE MODD, ONLY: X, &
        #                  & Y, & !Variable to suppress, with both '&' characters
        #                  & Z
    elif len([l[j] for j in range(i + 1, len(l)) if not non_code(l[j])]) == 0:
        #Node is the last of the list
        reason = 'last'
        #If the removed node is the last of the list and a continuation character is just before
        #We must suppress it.
        #    USE MODD, ONLY: X, &
        #                    Y !Variable to suppress, with the preceding '&'
        #In addition, if the character found before is at the begining of the line,
        #the preceding one must also be removed.
        #    USE MODD, ONLY: X, &
        #                  & Y !Variable to suppress, with 2 '&'
    else:
        #We must not suppress '&' characters
        reason = None
    if reason is not None:
        def _getPrecedingCnt(doc, l, i):
            """
            Return the index of the preceding node which is a continuation character
            :param doc: etree containig the list
            :param l: the list containig the node to suppress
            :param i: the index of the current node, i-1 is the starting index for the search
            :return: a tuple with three elements:
                      - the node containing the preceding '&' character
                      - the parent of the node containing the preceding '&' character
                      - index of the preceding '&' in the parent (previsous element of the tuple)
            Note:
              - In the general case the preceding '&' belongs to the same list:
                  USE MODD, ONLY: X, &
                                  Y
              - But it exists a special case, where the preceding '&' don't belong to
                the same list (in the following example, both '&' are attached to the parent):
                  USE MODD, ONLY: &
                                & X
            """
            j = i - 1
            while j >= 0 and l[j].tag.endswith('}C'):
                j -= 1
            if j >= 0 and l[j].tag.endswith('}cnt'):
                return l[j], l, j
            elif j == -1:
                #In the following special case, the '&' don't belong to the list but are siblings
                #of the list.
                #    USE MODD, ONLY: &
                #                  & X
                siblings = getSiblings(doc, l, before=True, after=False)
                j2 = len(siblings) - 1
                while j2 >= 0 and siblings[j2].tag.endswith('}C'):
                    j2 -= 1
                if j2 >= 0 and siblings[j2].tag.endswith('}cnt'):
                    return siblings[j2], siblings, j2
                else:
                    return None, None, None
            else:
                return None, None, None
        #We test if the preceding node (excluding comments) is a continuation character
        precCnt, newl, j = _getPrecedingCnt(doc, l, i)
        if precCnt is not None:
            #Preceding node is a continuation character
            nodesToSuppress.append(precCnt if reason == 'last' else l[i + 1])
            if j is not None:
                precCnt2, _, j2 = _getPrecedingCnt(doc, newl, j)
                if precCnt2 is not None:
                    #There is another continuation character before
                    nodesToSuppress.append(precCnt2 if reason == 'last' else precCnt)

    #Suppression of nodes
    for e in nodesToSuppress:
        #Get the tail of the previous children of the list and append the item's tail to be removed
        if e in l:
            parent = l
        else:
            #parent must be recomputed because previsous removal may have change it
            parent = getParent(doc, l)
        i = list(parent).index(e)
        if i != 0 and e.tail is not None:
            if parent[i - 1].tail is None:
                parent[i - 1].tail = ''
            parent[i - 1].tail = parent[i - 1].tail + e.tail
        parent.remove(e)

def getParent(doc, item, level=1):
    """
    :param doc: xml fragment in which parent must be searched
    :param item: item whose parent is to be searched
    :param level: number of degrees (1 to get the parent, 2 to get
                  the parent of the parent...)
    """
    assert level >= 1
    for p in doc.iter():
        if item in list(p):
            return p if level == 1 else getParent(doc, p, level - 1)

def getSiblings(doc, item, before=True, after=True):
    """
    :param doc: xml fragment in which siblings must be found
    :param item: item whose siblings are to be searched
    :param before: returns siblings before
    :param after: returns siblings after
    By default before and after are True so that all siblings are returned
    """

    siblings = getParent(doc, item).findall('./{*}*')
    if not after:
        siblings = siblings[:siblings.index(item)]
    if not before:
        siblings = siblings[siblings.index(item) + 1:]
    return [s for s in siblings if s != item]

def moveInGrandParent(doc,node,nestedObj=[]):
    """
    :param node: move the node in its great-parent and remove the parent node
    :param nestedObj: if present, move nestedObj[0] instead of node. The nestedObj must be contained such as node>...>...>nestedObj
    """
    par = getParent(doc,node)
    allsiblings = par.findall('./{*}*')
    ind=allsiblings.index(node)
    if nestedObj:
        par.insert(ind,nestedObj[0])
    else:
        par.insert(ind,node)
    par.remove(node)
    
def getIndexLoop(lowerBound,upperBound):
    if 'KSIZE' in upperBound or 'KPROMA' in upperBound:
        n = 'JL'
    elif 'NIJ' in lowerBound or 'NIJ' in upperBound or 'IIJ' in lowerBound or 'IIJ' in upperBound:
        n = 'JIJ'
    elif 'KT' in upperBound:
        n = 'JK'
    elif 'SV' in lowerBound or 'SV' in upperBound:
        n = 'JSV'
    elif 'KRR' in upperBound:
        n = 'JRR'
    elif 'NIT' in upperBound or 'IIB' in lowerBound or 'IIE' in upperBound:
        n = 'JI'
    elif 'NJT' in upperBound or 'IJB' in lowerBound or 'IJE' in upperBound:
        n = 'JJ'
    else:
        exit
    return n

def insertInList(pos, item, l):
    """
    :param pos: insertion position
    :param item: item to add to the list
    :param l: the parent of item (the list)
    """
    #insertion
    if pos < 0: pos = len(l) + 1 + pos
    l.insert(pos, item)
    if len(l) > 1:
        i = list(l).index(item) #effective position
        if i == len(l) - 1:
            #The item is the last one
            l[i - 1].tail = ', '
        else:
            l[i].tail = ', '

def isint(s):
    """
    :param s: string to test for intergerness
    :return: True if s represent an int
    """
    try:
        int(s)
    except ValueError:
        return False
    else:
        return True
