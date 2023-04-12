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

