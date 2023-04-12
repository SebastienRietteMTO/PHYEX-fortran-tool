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

from . import variables
from . import applications
from . import cosmetics
from . import util
from .pft import PFT
