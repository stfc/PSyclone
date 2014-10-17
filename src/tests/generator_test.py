from generator import generate, GenerationError

class TestGenerate:
   ''' unit tests for the generate function '''

   def test_non_existant_filename(self):
       ''' checks that algGen raises appropriate error when a non-existant filename is supplied '''       
       import pytest
       with pytest.raises(IOError):
           generate("non_existant_file.f90")

   def test_invalid_api(self):
       ''' checks that algGen raises appropriate error when an invalid api is supplied '''       
       import os
       import pytest
       with pytest.raises(GenerationError):
           generate(os.path.join("test_files","dynamo0p1","1_single_function.f90"), api="invalid")

