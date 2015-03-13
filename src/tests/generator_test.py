#-------------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
#-------------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab

from generator import generate, GenerationError
import pytest

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

   @pytest.mark.xfail(reason="Not yet implemented")
   def test_invalid_kernel_path(self):
      ''' checks that algGen raises appropriate error when an invalid search parth for kernel source files is supplied '''
      import os
      with pytest.raises(GenerationError):
         generate(os.path.join("test_files","dynamo0p1","1_single_function.f90"), api="dynamo0.1", kernel_path="does_not_exist")

