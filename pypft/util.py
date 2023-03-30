import xml.etree.ElementTree as ET

"""
This module implements some tools to manipulate the xml
"""

def tostring(doc):
    """
    Helper function to retrun xml fragment as a string
    :param doc: xml fragment
    """
    return ET.tostring(doc, method='xml', encoding='UTF-8').decode('UTF-8')

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

