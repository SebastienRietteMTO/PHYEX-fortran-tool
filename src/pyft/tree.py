"""
This module contains the functions to browse the tree
"""

import glob
from pyft.util import debugDecor, copy_doc

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

class Tree():
    @copy_doc(getDirs)
    def getDirs(self):
        return getDirs(tree=self.tree)


