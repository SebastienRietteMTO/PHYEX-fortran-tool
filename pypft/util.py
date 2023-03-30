import xml.etree.ElementTree as ET
import xml.dom.minidom
from functools import wraps
import logging

"""
This module implements some tools to manipulate the xml
"""

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
    def wrapper(doc):
        if isinstance(doc, xml.etree.ElementTree.Element):
            return func(doc)
        else:
            return func(minidom2etree(doc))
    return wrapper

def needMinidom(func):
    """
    :param func: function that needs a minidom object
    :return a new func that can use a ET object
    """
    @wraps(func)
    def wrapper(doc):
        wrapper.__doc__ = func.__doc__
        if isinstance(doc, xml.etree.ElementTree.Element):
            return func(etree2minidom(doc))
        else:
            return func(doc)
    return wrapper

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
        logging.warning('The file certainly contains a strange character')
    return r

@needEtree
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
    return e.tag.split('}')[1] in {'#text', 'cnt', 'C'}


