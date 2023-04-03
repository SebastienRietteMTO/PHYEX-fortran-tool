"""
This module implements functions to deal with variables
"""

from . import copy_doc, PFTError
from util import (tostring, alltext, needEtree, getFileName, ETremoveFromList, ETgetParent,
                  ETgetSiblings)
import logging

@needEtree
def getVarList(doc):
    """
    :param doc: etree to use
    :return: a list of dictionaries. Each item is a variable. The associated
             dictionnary has the following keys:
              - as: list of array specifications
              - asx: same but encoded in xml
              - n: name of the variable as written
              - i: intent
              - t: type specification
              - arg: True if variable is a dummy argument
    """
    #TODO add a key to the dictionnary to specify the subroutine/module/function
    #     where the variable is declared
    def decode_array_specs(array_specs):
        as_list = []
        asx_list = []
        for array_spec in array_specs:
            lb = array_spec.find('.//{*}lower-bound')
            ub = array_spec.find('.//{*}upper-bound')
            as_list.append([alltext(lb) if lb is not None else None, alltext(ub) if ub is not None else None])
            asx_list.append([tostring(lb) if lb is not None else None, tostring(ub) if ub is not None else None])
        return as_list, asx_list
        
    #Find dummy arguments
    dummy_args = [e.text for e in doc.findall('.//{*}dummy-arg-LT/{*}arg-N/{*}N/{*}n')]

    result = []
    decl_stmts = doc.findall('.//{*}T-decl-stmt')
    #Loop on each declaration statement
    for decl_stmt in decl_stmts:
        t_spec = alltext(decl_stmt.find('.//{*}_T-spec_'))
        i_spec = decl_stmt.find('.//{*}intent-spec')
        if i_spec is not None: i_spec = i_spec.text

        #Dimensions declared with the DIMENSION attribute
        array_specs =decl_stmt.findall('.//{*}attribute//{*}array-spec//{*}shape-spec')
        as0_list, asx0_list = decode_array_specs(array_specs)
        
        #Loop on each declared variables
        en_decls = decl_stmt.findall('.//{*}EN-decl')
        for en_decl in en_decls:
            n = alltext(en_decl.find('.//{*}n'))
            #Dimensions declared after the variable name
            array_specs = en_decl.findall('.//{*}array-spec//{*}shape-spec')
            as_list, asx_list = decode_array_specs(array_specs)

            result.append({'as': as_list if len(as0_list) == 0 else as0_list,
                           'asx': asx_list if len(asx0_list) == 0 else asx0_list,
                           'n': n, 'i': i_spec, 't': t_spec, 'arg': n in dummy_args})
    return result

def showVarList(doc):
    """
    Display on stdout a nive view of all the variables
    :param doc: etree to use
    """
    for v in getVarList(doc):
        isscalar = len(v['as']) == 0
        print('Variable name {}:'.format(v['n']))
        if isscalar:
            print('  is scalar')
        else:
            print('  is of rank {}, with dimensions {}'.format(len(v['as']),
                                ', '.join([(':'.join([('' if s is None else s) for s in v['as'][i]])) for i in range(len(v['as']))])))
        if v['arg']:
            print('  is a dummy argument {}'.format('without intent' if v['i'] is None else 'with intent {}'.format(v['i'])))
        else:
            print('  is a local variable')
        print()

@needEtree
def getImplicitNoneText(doc):
    """
    :param doc: etree to use
    :return: the "IMPLICIT NONE" text
    """
    ins = doc.findall('.//{*}implicit-none-stmt')
    return ins[0].text if len(ins) != 0 else None

def checkImplicitNone(doc, mustRaise=False): 
    """
    :param doc: xml fragment to use
    :param mustRaise: True to raise
    Issue a logging.warning if the "IMPLICIT NONE" statment is missing
    If mustRaise is True, issue a logging.error instead and raise an error
    """
    if getImplicitNoneText(doc) is None:
        message = "The 'IMPLICIT NONE' statment is missing in file '{}'.".format(getFileName(doc))
        if mustRaise:
            logging.error(message)
            raise PFTError(message)
        else:
            logging.warning(message)

def checkIntent(doc, mustRaise=False): 
    """
    :param doc: xml fragment to use
    :param mustRaise: True to raise
    Issue a logging.warning if some "INTENT" attributes are missing
    If mustRaise is True, issue a logging.error instead and raise an error
    """
    ok = True
    l = logging.error if mustRaise else logging.warn
    for v in getVarList(doc).values():
        if v['arg'] and v['i'] is None:
          l("The dummy argument {} as no INTENT attribute, in file '{}'".format(v['n'], getFileName(doc)))
          ok = False
    if not ok and mustRaise:
        raise PFTError("There are dummy arguments without INTENT attribute in file '{}'".format(getFileName(doc)))

@needEtree
def removeVar(doc, varList):
    """
    :param doc: xml fragment to use
    :param varList: list of variables to remove. Each item is a list or tuple of two elements.
                    The first one describes where the variable is declared, the second one is the name
                    of the variable. The first element takes the form module:<name of the module>,
                    sub:<name of the subroutine>, func:<name of the function> or type:<name of the type>
    Remove the variable from declaration, and from the argument list if needed
    """

    for where, varName in varList:
        found = False
        #Usefull statements
        varName = varName.upper()
        assert len(where.split(':')) == 2, "First element must contain a ':'"
        blocType, blocName = where.split(':')
        blocName = blocName.upper()
        assert blocType in ('module', 'sub', 'func', 'type')
        tag = {'module':'module',
               'func': 'function',
               'sub': 'subroutine',
               'type': 'T'}[blocType]
        beginStmt, endStmt = '{}-stmt'.format(tag), 'end-{}-stmt'.format(tag)
        if blocType == 'type':
            declStmt = 'component-decl-stmt'
        else:
            declStmt = 'T-decl-stmt'

        #Search for the right bloc
        for bloc in doc.findall('.//{*}' + beginStmt):
            if alltext(bloc.find('.//{*}N/{*}n')).upper() == blocName:
                #Checks if variable is a dummy argument
                dummy_lst = bloc.find('{*}dummy-arg-LT') #This is the list of the dummy arguments (function or subroutine)
                if dummy_lst is not None:
                    #Loop over all dummy arguments
                    for arg in dummy_lst.findall('.//{*}arg-N'):
                        if alltext(arg.find('.//{*}N/{*}n')).upper() == varName:
                            #The variable is a dummy arg, we remove it from the list
                            ETremoveFromList(arg, dummy_lst)

                for sibling in ETgetSiblings(doc, bloc, before=False):
                    if sibling.tag.endswith('}' + endStmt) or sibling.tag.endswith('}contains-stmt'):
                        break #we are outside of the targeted bloc
                    if sibling.tag.endswith('}' + declStmt):
                        #We are in a declaration statement
                        decl_lst = sibling.find('./{*}EN-decl-LT') #list of declaration in the current statment
                        for en_decl in decl_lst:
                            if alltext(en_decl.find('.//{*}n')).upper() == varName:
                                #The argument is declared here, we suppress it from the declaration list
                                found = True
                                ETremoveFromList(en_decl, decl_lst)
                                break #cannot be declared twice, we can exit the loop
                        #In case the argument was alone on the declaration statement
                        if len(list(decl_lst.findall('./{*}EN-decl'))) == 0:
                            ETgetParent(doc, sibling).remove(sibling)
                break #bloc has been found
        if not found:
            raise PFTError("The variable {var} in {bloc} has not been found.".format(var=varName, bloc=blocName))

class Variables():
    @copy_doc(getVarList)
    def getVarList(self):
        return getVarList(doc=self._xml)

    @copy_doc(showVarList)
    def showVarList(self):
        return showVarList(doc=self._xml)

    @copy_doc(getImplicitNoneText)
    def getImplicitNoneText(self):
        return getImplicitNoneText(doc=self._xml)

    @copy_doc(checkImplicitNone)
    def checkImplicitNone(self, mustRaise=False):
        return checkImplicitNone(self._xml, mustRaise)

    @copy_doc(checkIntent)
    def checkIntent(self, mustRaise=False):
        return checkIntent(self._xml, mustRaise)

    @copy_doc(removeVar)
    def removeVar(self, *args, **kwargs):
        return removeVar(self._xml, *args, **kwargs)
