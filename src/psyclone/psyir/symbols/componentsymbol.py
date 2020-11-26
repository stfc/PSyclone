from psyclone.psyir.symbols.datasymbol import DataSymbol
from psyclone.psyir.symbols.symbol import Symbol


class ComponentSymbol(DataSymbol):
    '''
    '''
    def __init__(self, name, datatype, parent,
                 visibility=Symbol.DEFAULT_VISIBILITY,
                 constant_value=None, interface=None):
        super(ComponentSymbol, self).__init__(name, datatype,
                                              visibility=visibility,
                                              constant_value=constant_value,
                                              interface=interface)
        self._parent_structure = parent

    @property
    def name(self):
        return ".".join([self._parent_structure.name, self._name])

# For Sphinx auto-api
__all__ = ['ComponentSymbol']
