"""
This module implements functions for high-to-moderate level transformation
"""

import xml.etree.ElementTree as ET
from util import (copy_doc, debugDecor,
                  alltext, needEtree, ETgetParent)
from statements import removeCall

@debugDecor
@needEtree
def deleteDrHook(doc, simplify=False):
    """
    Remove DR_HOOK calls.
    If Simplify is True, also remove all variables only needed for these calls (ZHOOK_HANDLE,
    DR_HOOK, LHOOK, YOMHOOK, JPRB, PARKIND1)
    :param doc: etree to use
    :param simplify : if True, remove variables that are now unused
    :return: modified doc
    """
    removeCall(doc, 'DR_HOOK', None, simplify=simplify)

class Applications():
    @copy_doc(deleteDrHook)
    def deleteDrHook(self, *args, **kwargs):
        return deleteDrHook(self._xml, *args, **kwargs)
        

