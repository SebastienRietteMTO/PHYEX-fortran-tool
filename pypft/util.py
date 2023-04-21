import xml.etree.ElementTree as ET
import xml.dom.minidom
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

def minidom2etree(doc):
    """
    :param doc: minidom object
    :return: ET object
    """
    return ET.fromstring(doc.toxml(encoding='UTF-8'), parser=ET.XMLParser(encoding='UTF-8'))

def etree2minidom(doc):
    """                             
    :param doc: ET object     
    :return: minidoc object             
    """ 
    return xml.dom.minidom.parseString(ET.tostring(doc, method='xml', encoding='UTF-8').decode('UTF-8'))

def needEtree(func):
    """
    :param func: function that needs an ET object
    :return a new func that can use a minidom object
    """
    @wraps(func)
    def wrapper(doc, *args, **kwargs):
        if isinstance(doc, xml.etree.ElementTree.Element):
            return func(doc, *args, **kwargs)
        else:
            return func(minidom2etree(doc), *args, **kwargs)
    return wrapper

def needMinidom(func):
    """
    :param func: function that needs a minidom object
    :return a new func that can use a ET object
    """
    @wraps(func)
    def wrapper(doc, *args, **kwargs):
        wrapper.__doc__ = func.__doc__
        if isinstance(doc, xml.etree.ElementTree.Element):
            return func(etree2minidom(doc), *args, **kwargs)
        else:
            return func(doc, *args, **kwargs)
    return wrapper

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

@needEtree
def tostring(doc):
    """
    :param doc: an ET object
    :return: xml as a string
    """
    return ET.tostring(doc, method='xml', encoding='UTF-8').decode('UTF-8')

@needEtree
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

@needEtree
def getFileName(doc):
    """
    :param doc: an ET object
    :return: the name of the input file name or 'unknown' if not available
             in the xml fragment provided
    """
    return doc.find('.//{*}file').attrib['name']

def ETn2name(N):
    """
    Helper function which returns the entity name enclosed in a N tag
    """
    return ''.join([e.text for e in N.findall('./{*}n')])

@needEtree
def alltext(doc):
    """ 
    Helper function to iterate on all text fragment and join them
    :param doc: xml fragment
    """
    return ''.join(doc.itertext())

def ETnon_code(e):
    """ 
    :param e: element
    :return: True if e is non code (comment, text...)
    """
    return e.tag.split('}')[1] in {'#text', 'cnt', 'C'}

def ETisExecutableStmt(e):
    """
    :param e: element
    :return: True if element is an executable statement
    """
    return ETisStmt(e) and \
           not e.tag.split('}')[1] in {'subroutine-stmt', 'end-subroutine-stmt',
                                       'function-stmt', 'end-function-stmt',
                                       'use-stmt', 'T-decl-stmt', 'component-decl-stmt',
                                       'T-stmt', 'end-T-stmt',
                                       'data-stmt', 'save-stmt',
                                       'implicit-none-stmt'}

def ETisStmt(e):
    """
    :param e: element
    :return: True if element is a statement
    """
    return e.tag.endswith('-stmt')

def ETremoveFromList(item, l):
    """
    :param item: item to remove from list
    :param l: the parent of item (the list)
    """

    #Suppression of the comma
    i = list(l).index(item)
    if item.tail is not None and ',' in item.tail:
        tail = item.tail
        item.tail = tail.replace(',', '')
    elif i != 0 and ',' in l[i - 1].tail:
        tail = l[i - 1].tail
        l[i - 1].tail = tail.replace(',', '')
    
    # Get the tail of the previous children of the list and append the item's tail to be removed
    tail=l[i-1].tail
    if i != 0 and type(tail) is str and type(item.tail) is str:
        l[i-1].tail=tail+item.tail

    #Suppression of the node
    l.remove(item)

def ETgetParent(doc, item, level=1):
    """
    :param doc: xml fragment in which parent must be searched
    :param item: item whose parent is to be searched
    :param level: number of degrees (1 to get the parent, 2 to get
                  the parent of the parent...)
    """
    assert level >= 1
    for p in doc.iter():
        if item in list(p):
            return p if level == 1 else ETgetParent(doc, p, level - 1)

def ETgetSiblings(doc, item, before=True, after=True):
    """
    :param doc: xml fragment in which siblings must be found
    :param item: item whose siblings are to be searched
    :param before: returns siblings before
    :param after: returns siblings after
    By default before and after are True so that all siblings are returned
    """

    siblings = ETgetParent(doc, item).findall('./{*}*')
    if not after:
        siblings = siblings[:siblings.index(item)]
    if not before:
        siblings = siblings[siblings.index(item) + 1:]
    return [s for s in siblings if s != item]

def ETinsertInList(pos, item, l):
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
