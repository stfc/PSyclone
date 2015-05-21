#-------------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
#-------------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab

# internal classes requiring tests
# PSy,Invokes,Dependencies,NameSpaceFactory,NameSpace,Invoke,Node,Schedule,LoopDirective,OMPLoopDirective,Loop,Call,Inf,SetInfCall,Kern,Arguments,InfArguments,Argument,KernelArgument,InfArgument

# user classes requiring tests
# PSyFactory, TransInfo, Transformation
from psyGen import TransInfo, Transformation, PSyFactory, GenerationError

class TestPSyFactoryClass:
    ''' PSyFactory class unit tests '''

    def test_invalid_api(self):
        ''' test that psyfactory raises appropriate error when an invalid api is supplied '''
        import pytest
        with pytest.raises(GenerationError):
            psy_factory = PSyFactory(api = "invalid")

    def test_psyfactory_valid_return_object(self):
        ''' test that psyfactory returns a psyfactory object for all supported inputs '''
        psy_factory = PSyFactory()
        assert isinstance(psy_factory,PSyFactory)
        for api in ["", "gunghoproto", "dynamo0.1", "gocean0.1"]:
            psy_factory = PSyFactory(api = api)
            assert isinstance(psy_factory,PSyFactory)

    # TBD need to find a way to create a valid info object to pass to create so we can check creation
    #def test_create_valid_return_object(self):
    #    from ghproto import GHProtoPSy
    #    psy = PSyFactory().create(None)
    #    assert isinstance(psy,GHProtoPSy)

class TestTransformationClass:
    ''' Transformation class unit tests '''

    def test_base_class_not_callable(self):
        ''' make sure we can not instantiate abstract Transformation class directly '''
        import pytest
        with pytest.raises(TypeError):        
            t=Transformation()

class TestTransInfoClass:
    ''' TransInfo class unit tests '''

    def test_new_module(self):
        ''' check that we can change the module where we look for transformations.
            There should be no transformations available as the new module uses a
            different transformation base class '''
        from test_files import dummy_transformations
        t=TransInfo(module=dummy_transformations)
        assert t.num_trans==0

    def test_new_baseclass(self):
        ''' check that we can change the transformations baseclass. There should
            be no transformations available as the default transformations module
            does not use the specified base class '''
        from test_files.dummy_transformations import LocalTransformation
        t=TransInfo(base_class=LocalTransformation)
        assert t.num_trans==0

    def test_new_module_and_baseclass(self):
        ''' check that we can change the module where we look for transformations
            and the baseclass. There should be one transformation available as the
            module specifies one test transformation using the specified base
            class '''
        from test_files import dummy_transformations
        t=TransInfo(module=dummy_transformations,base_class=dummy_transformations.LocalTransformation)
        assert t.num_trans==1

    def test_list_valid_return_object(self):
        ''' check the list method returns the valid type '''
        t=TransInfo()
        assert isinstance(t.list,str)

    def test_list_return_data(self):
        ''' check the list method returns sensible information '''
        t=TransInfo()
        assert t.list.find("available")!=-1

    def test_invalid_low_number(self):
        ''' check an out-of-range low number for get_trans_num method raises correct exception '''
        t=TransInfo()
        import pytest
        with pytest.raises(GenerationError):
            transform=t.get_trans_num(0)

    def test_invalid_high_number(self):
        ''' check an out-of-range high number for get_trans_num method raises correct exception '''
        t=TransInfo()
        import pytest
        with pytest.raises(GenerationError):
            transform=t.get_trans_num(999)

    def test_valid_return_object_from_number(self):
        ''' check get_trans_num method returns expected type of instance '''
        t=TransInfo()
        transform=t.get_trans_num(1)
        assert isinstance(transform,Transformation)

    def test_invalid_name(self):
        ''' check get_trans_name method fails correctly when an invalid name is provided '''
        t=TransInfo()
        import pytest
        with pytest.raises(GenerationError):
            transform=t.get_trans_name("invalid")

    def test_valid_return_object_from_name(self):
        ''' check get_trans_name method return the correct object type '''
        t=TransInfo()
        transform=t.get_trans_name("SwapTrans")
        assert isinstance(transform,Transformation)
