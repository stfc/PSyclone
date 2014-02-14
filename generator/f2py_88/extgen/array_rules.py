
import numpy

numpy_int64 = dict(\
    typenum = 'PyArray_INT64',
    ctype = 'PyArrayObject*',
    init = ' = NULL',
    title = 'a numpy array of 64-bit integers',
    argument_format = 'O&',
    argument_converter = 'pyobj_to_numpy_array_int64',
    return_format = 'N',
    require_numpy = True,
    )
