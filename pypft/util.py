import xml.etree.ElementTree as ET
import xml.dom.minidom
from functools import wraps
import logging

"""
This module implements some tools to manipulate the xml
"""

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

@needEtree
def getFileName(doc):
    """
    :param doc: an ET object
    :return: the name of the input file name or 'unknown' if not available
             in the xml fragment provided
    """
    return doc.find('.//{*}file').attrib['name']

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

def ETgetParent(doc, item):
    """
    :param doc: xml fragment in which parent must be searched
    :param item: item whose parent is to be searched
    """

    for p in doc.iter():
        if item in list(p):
            return p

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

localityStmt = {'module':'module-stmt',
               'func': 'function-stmt',
               'sub': 'subroutine-stmt',
               'type': 'T-stmt'}
localityConstruct = {'module':'program-unit',
                     'func': 'program-unit',
                     'sub': 'program-unit',
                     'type': 'T-construct'}
def _localityStmt(blocType):
    """
    Internal method
    :param blocType: kind of locality
    :return: (construct, beginStmt, endStmt)
    """
    assert blocType in ('module', 'sub', 'func', 'type')
    stmt = localityStmt[blocType]
    construct = localityConstruct[blocType]
    beginStmt, endStmt = stmt, 'end-{}-stmt'.format(stmt)
    return construct, beginStmt, endStmt

def ETisLocalityNode(node):
    """
    :param node: node to test
    :return: True if node is a locality node (construct node around a
             module, subroutine, function or type declaration)
    """
    return any([node.tag.endswith('}' + construct) for construct in localityConstruct.values()])

def ETgetLocalityNode(doc, localityPath):
    """
    :param doc: xml fragment in which the locality path must be found
    :param localityPath: locality path (see the locality concept in documentation)
    :result: the locality node corresponding to the locality path

    In case of a type declaration locality, the function returns the 'T-construct'
    node surrounding the type declaration statement.
    Otherwise, the function retruns the 'program-unit' node in which the module,
    subroutine or function is declared.
    """
    where, remainingPath = (localityPath.split('/', maxsplit=1) + [None])[0:2]
    assert len(where.split(':')) == 2, "Path element must contain a ':'"
    blocType, blocName = where.split(':') 
    blocName = blocName.upper()
    construct, beginStmt, endStmt = _localityStmt(blocType)
    if doc.tag.endswith('}object'):
        top = doc.find('./{*}file')
    else:
        top = doc
    for bloc in top.findall('./{*}' + construct + '/{*}' + beginStmt):
        if alltext(bloc.find('.//{*}N/{*}n')).upper() == blocName:
            if remainingPath is None:
                return ETgetParent(doc, bloc)
            else:
                return ETgetLocalityNode(ETgetParent(doc, bloc), remainingPath)
    raise PFTError("The locality path {path} has not been found.".format(path=where))

def ETgetLocalityChildNodes(doc, locality):
    """
    :param doc: xml fragment in which the nodes must be found
    :param locality: path or node reprensenting the locality
    :return: list of child nodes

    The function retruns all the nodes corresponding to the locality.
    If the locality is a module, function or subroutine that contain
    (after a 'contains' statement) other subroutines or functions, those
    subroutines or functions are excluded from the result.
    """
    if isinstance(locality, str):
        locality = ETgetLocalityNode(doc, locality)
    assert len(locality) != 0, 'The locality construct is empty'
    assert locality[0].tag.endswith('-stmt'), 'The node is not a locality node'
    endStmt = 'end-' + locality.tag
    result = []
    for node in locality:
        if node.tag.endswith('}' + endStmt) or node.tag.endswith('}contains-stmt'):
            break #we are outside of the targeted bloc
        result.append(node)
    return result

def ETgetParentLocalityNode(doc, item, mustRaise=True):
    """
    :param doc: xml fragment in which parent must be searched
    :param item: item whose locality parent is to be searched
    :param mustRaise: True to raise an exception if parent is not found
    :return: the locality parent node of item
    """
    result = ETgetParent(doc, item)
    while result is not None and not ETisLocalityNode(result):
        result = ETgetParent(doc, result)
    if result is None and mustRaise:
        raise PFTError("The locality parent has not been found.")
    return result

def ETgetLocalityPath(doc, item, includeItself=True):
    """
    :param doc: xml fragment in which the path must be found
    :param item: item whose path must be determined
    :param includeItself: include the item if it is a locality node
    :return: the full path of the structure containing item
    """
    def _getNodePath(node):
        stmt = node[0].tag.split('}')[1]
        name = alltext(node[0].find('.//{*}N/{*}n')).upper()
        return {v: k for (k, v) in localityStmt.items()}[stmt] + ':' + name

    if includeItself and ETisLocalityNode(item):
        result = [_getNodePath(item)]
    else:
        result = []
    item = ETgetParentLocalityNode(doc, item, mustRaise=False)
    while item is not None:
        result = [_getNodePath(item)] + result
        item = ETgetParentLocalityNode(doc, item, mustRaise=False)
    return '/'.join(result)
