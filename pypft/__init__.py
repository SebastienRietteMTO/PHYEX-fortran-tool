import xml.etree.ElementTree as ET

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

from . import variables
from .pft import PFT
