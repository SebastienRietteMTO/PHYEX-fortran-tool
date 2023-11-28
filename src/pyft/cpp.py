"""
This module implements functions for delaing with cpp directives
"""

from pyft.util import copy_doc, debugDecor, getParent, alltext, PYFTError, getFileName

@debugDecor
def applyCPPifdef(doc, keys):
    """
    Apply #ifdef / #ifndef only on some keys
    :param doc: etree to use
    :param keys: list of defined and undefined keys. The latter are preceded by a percentage sign '%'.
                 E.g. if K is in keys, "#ifdef K "will be evaluated to True
                      if %K is in keys, "#ifdef K" will be evaluated to False

    Warning: only #ifdef and #ifndef are treated and not "#if defined ..."

    If key K is in keys
      #ifdef K
      A
      #else
      B
      #endif
    is reduced to A
    If %K is in keys, code snippet is reduced to B.
    If neither K nor %K are present in keys, code is kept untouched.
    """

    #We make the hypothesis that #ifdef, #else and #endif have the same parent
    #Get all nodes containing #ifdef or #ifndef
    parents = set([getParent(doc, cppNode) for cppNode in doc.findall('.//{*}cpp')
                   if (cppNode.text.startswith('#ifdef ') or cppNode.text.startswith('#ifndef '))])

    #Iteration over nodes contained in each parent
    toRemove = []
    for par in parents:
        #we deal with nested #ifdef #ifndef and #if
        #We need to track #if cpp directives to discard #else and #endif related to these #if
        #Each time we enter an #ifdef, #ifndef or #if, we add a value to the keep list
        #True or False to keep or discard it, None not to touch it
        keep = [True]
        for node in par:
            if node.tag.split('}')[1] == 'cpp':
                if node.text.startswith('#ifdef '):
                    k = alltext(node).split(' ')[1].strip()
                    if k in keys:
                        toRemove.append((node, par))
                        keep.append(True)
                    elif '%' + k in keys:
                        toRemove.append((node, par))
                        keep.append(False)
                    else:
                        keep.append(None)
                        if False in keep:
                            toRemove.append((node, par))
                elif node.text.startswith('#ifndef '):
                    k = alltext(node).split(' ')[1].strip()
                    if k in keys:
                        toRemove.append((node, par))
                        keep.append(False)
                    elif '%' + k in keys:
                        toRemove.append((node, par))
                        keep.append(True)
                    else:
                        keep.append(None)
                        if False in keep:
                            toRemove.append((node, par))
                elif node.text.startswith('#if '):
                    if False in keep:
                        toRemove.append((node, par))
                    #We are in a #if,following #else / #endif  is associated to this #if
                    keep.append(None)
                elif node.text.startswith('#else'):
                    if keep[-1] is not None:
                        toRemove.append((node, par))
                        keep[-1] = not keep[-1]
                    elif False in keep:
                        toRemove.append((node, par))
                elif node.text.startswith('#endif'):
                    if keep[-1] is not None or False in keep:
                        toRemove.append((node, par))
                    keep.pop()
                elif node.text.startswith('#elifdef') or node.text.startswith('#elifndef'):
                    raise NotImplementedError("#elifdef and #elifndef not (yet?) implemented")
                else:
                    if False in keep:
                        toRemove.append((node, par))
            else:
                if False in keep:
                    toRemove.append((node, par))
        if len(keep) != 1:
            #We check the hypothesis done at the beginning
            raise PYFTError("#else or #endif hasn't the same parent as #ifdef or #ifndef in {f}".format(f=getFileName(doc)))
    #Suppress node in reverse order to attach tail to previous node
    for node, par in toRemove[::-1]:
        index = list(par).index(node)
        if index != 0:
            if node.tail is not None:
                if par[index - 1].tail is None:
                    par[index - 1].tail = ""
                #We only keep '\n' and spaces at the end (indentation)
                par[index - 1].tail += node.tail.count('\n') * '\n' + \
                                       (len(node.tail) - len(node.tail.rstrip(' '))) * ' '
        par.remove(node)


class Cpp():
    @copy_doc(applyCPPifdef)
    def applyCPPifdef(self, *args, **kwargs):
        return applyCPPifdef(self._xml, *args, **kwargs)
    

