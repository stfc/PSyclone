#-------------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
#-------------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab

from generator import generate, GenerationError
import pytest
import os

class TestGenerate:
   ''' unit tests for the generate function '''

   def test_non_existant_filename(self):
       ''' checks that algGen raises appropriate error when a
           non-existant filename is supplied '''       
       import pytest
       with pytest.raises(IOError):
           generate("non_existant_file.f90")

   def test_invalid_api(self):
       ''' checks that algGen raises appropriate error when an invalid
           api is supplied '''       
       import pytest
       with pytest.raises(GenerationError):
           generate(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "test_files", "dynamo0p1", "algorithm",
                                 "1_single_function.f90"), api="invalid")

   def test_invalid_kernel_path(self):
      ''' checks that algGen raises appropriate error when an invalid
          search path for kernel source files is supplied '''
      with pytest.raises(IOError):
         generate(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                               "test_files", "dynamo0p1", "algorithm",
                               "1_single_function.f90"),
                  api="dynamo0.1",
                  kernel_path="does_not_exist")

   def test_wrong_kernel_path(self):
      ''' checks that algGen raises appropriate error when the kernel
          code cannot be found in the specified search path '''
      root_path = os.path.dirname(os.path.abspath(__file__))
      with pytest.raises(IOError):
         generate(os.path.join(root_path,
                               "test_files", "dynamo0p1", "algorithm",
                               "1_single_function.f90"),
                  api="dynamo0.1",
                  kernel_path=os.path.join(root_path,
                                           "test_files", "gocean0p1"))

   def test_correct_kernel_path(self):
      ''' checks that algGen succeeds when the location of the kernel
          source code is *not* the same as that of the algorithm code '''
      root_path = os.path.dirname(os.path.abspath(__file__))
      alg,psy=generate(os.path.join(root_path,
                                    "test_files", "dynamo0p1", "algorithm",
                                    "1_single_function.f90"), 
                       api="dynamo0.1",
                       kernel_path=os.path.join(root_path, "test_files",
                                                "dynamo0p1", "kernels"))

   def test_same_kernel_path(self):
      ''' checks that the generator succeeds when the search directory
          is the same as the algorithm code directory and a path is
          specified '''
      path=os.path.join(os.path.dirname(os.path.abspath(__file__)),
                        "test_files", "dynamo0p1", "algorithm")
      alg,psy=generate(os.path.join(path,"1_single_function.f90"), 
                       api="dynamo0.1",
                       kernel_path=path)

   def test_similar_kernel_name(self):
      ''' checks that the generator does not match incorrect files '''
      root_path = os.path.dirname(os.path.abspath(__file__))
      alg,psy=generate(os.path.join(root_path,
                                    "test_files", "dynamo0p1", "algorithm",
                                    "1_single_function.f90"), 
                       api="dynamo0.1",
                       kernel_path=os.path.join(root_path,
                                                "test_files", "dynamo0p1",
                                                "kernels2"))

   def test_recurse_correct_kernel_path(self):
      '''checks that the generator succeeds when the location of the kernel
         source code is *not* the same as that of the algorithm code and
         recursion through subdirectories is required'''
      root_path = os.path.dirname(os.path.abspath(__file__))
      alg,psy=generate(os.path.join(root_path,
                                    "test_files", "dynamo0p1", "algorithm",
                                    "1_single_function.f90"), 
                       api="dynamo0.1",
                       kernel_path=os.path.join(root_path,
                                                "test_files", "dynamo0p1",
                                                "kernels3"))

class TestGenerateGOcean1p0:

   def test01_kernels_different_grid_offsets_one_invoke(self):
      ''' Check that the parser raises an error if two kernels in a
          single invoke specify different index offsets '''
      with pytest.raises(GenerationError):
         generate(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                               "test_files", "gocean1p0",
                               "test01_different_grid_offsets_one_invoke.f90"),
                  api="gocean1.0")

   def test02_kernels_different_grid_offsets_two_invokes(self):
      ''' Check that the parser raises an error if the two kernels
          in different invokes specify different index offsets. '''
      with pytest.raises(GenerationError):
         generate(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                               "test_files", "gocean1p0",
                               "test02_different_grid_offsets_two_invokes.f90"),
                  api="gocean1.0")
