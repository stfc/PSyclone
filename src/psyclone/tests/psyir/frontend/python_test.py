from psyclone.psyir.frontend.python import PythonReader

def test_create():
    ''' xxx '''
    reader = PythonReader()
    assert isinstance(reader, PythonReader)

def test_psyir_from_source():
    ''' xxx '''
    reader = PythonReader()
    reader.psyir_from_source("a = 1")
    exit(1)
