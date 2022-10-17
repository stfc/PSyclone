from fparser.common.readfortran import FortranStringReader
from fparser.two import Fortran2003
from fparser.two.parser import ParserFactory


# TODO issue #1886. This class has commonalities with GOcean metadata
# processing.
class CommonMetadata:
    ''' xxx '''

    @staticmethod
    def create_fparser2(fortran_string, encoding):
        '''Creates an fparser2 tree from a Fortran string. The resultant
        parent node of the tree will be the same type as the encoding
        argument if the string conforms to the encoding, otherwise an
        exception will be raised.

        :param str fortran_string: a string containing the metadata in \
           Fortran.
        :param encoding: the parent class with which we will encode the \
            Fortran string.
        :type encoding: type

        :returns: an fparser2 tree containing a metadata \
            argument.
        :rtype: :py:class:`fparser.two.Fortran2003.Base`

        :raises ValueError: if the Fortran string is not in the \
            expected form.

        '''
        # todo, integrate with common_arg
        _ = ParserFactory().create(std="f2003")
        reader = FortranStringReader(fortran_string)
        fparser2_tree = encoding(reader)
        if not fparser2_tree:
            raise ValueError(
                f"Expected kernel metadata to be a Fortran "
                f"{encoding.__name__}, but found '{fortran_string}'.")
        return fparser2_tree
