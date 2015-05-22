#-------------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2014.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
#-------------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab

from parse import parse
from psyGen import PSyFactory
from transformations import TransformationError, SwapTrans,\
                            LoopFuseTrans, GOceanOpenMPLoop
from generator import GenerationError
import os
import pytest

class TestTransformationsGHProto:

    @pytest.mark.xfail(reason="bug 3")
    def test_swap_trans(self):
        ''' test of a (test) swap transformation which swaps two entries 
            in a schedule '''
        ast,info=parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                    "test_files","gunghoproto",
                                    "3_two_functions_shared_arguments.f90"),
                       api="gunghoproto" )
        psy=PSyFactory("gunghoproto").create(info)
        invokes=psy.invokes
        invoke=invokes.get("invoke_0")
        schedule=invoke.schedule
        loop1=schedule.children[0]
        loop2=schedule.children[1]
        trans=SwapTrans()
        schedule,memento=trans.apply(loop1,loop2)
        gen=str(psy.gen)
        # testkern1_code call should now be after testkern2_code call
        assert gen.rfind("testkern1_code")>gen.rfind("testkern2_code")

    @pytest.mark.xfail(reason="bug 3")
    def test_loop_fuse_trans(self):
        ''' test of the loop-fuse transformation '''
        ast,info=parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                    "test_files","gunghoproto",
                                    "3_two_functions_shared_arguments.f90"),
                       api="gunghoproto")
        psy=PSyFactory("gunghoproto").create(info)
        invokes=psy.invokes
        invoke=invokes.get("invoke_0")
        schedule=invoke.schedule
        loop1=schedule.children[0]
        loop2=schedule.children[1]
        trans=LoopFuseTrans()
        schedule,memento=trans.apply(loop1,loop2)
        gen=str(psy.gen)
        for idx,line in enumerate(gen.split('\n')):
            if line.find("DO column=1,topology")!=-1: do_idx=idx
            if line.find("CALL testkern1_code(")!=-1: call1_idx=idx
            if line.find("CALL testkern2_code(")!=-1: call2_idx=idx
            if line.find("END DO")!=-1: enddo_idx=idx
        # 4 lines should be in sequence as calls have been fused into one loop
        assert enddo_idx-call2_idx==1 and call2_idx-call1_idx==1 and\
            call1_idx-do_idx==1

class TestTransformationsGOcean0p1:

    def test_openmp_loop_fuse_trans(self):
        ''' test of the OpenMP transformation of a fused loop '''
        ast,info=parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                    "test_files","gocean0p1",
                                    "openmp_fuse_test.f90"),
                       api="gocean0.1")
        psy=PSyFactory("gocean0.1").create(info)
        invokes=psy.invokes
        invoke=invokes.get('invoke_0')
        schedule=invoke.schedule
        lftrans=LoopFuseTrans()
        ompf=GOceanOpenMPLoop()

        # fuse all outer loops
        lf1_schedule,memento = lftrans.apply(schedule.children[0],
                                             schedule.children[1])
        lf2_schedule,memento = lftrans.apply(lf1_schedule.children[0],
                                             lf1_schedule.children[1])
        # fuse all inner loops
        lf3_schedule,memento = lftrans.apply(lf2_schedule.children[0].children[0],
                                             lf2_schedule.children[0].children[1])
        lf4_schedule,memento = lftrans.apply(lf3_schedule.children[0].children[0],
                                             lf3_schedule.children[0].children[1])

        # Add an OpenMP directive around the fused loop
        omp1_schedule,memento = ompf.apply(lf4_schedule.children[0])

        # Replace the original loop schedule with the transformed one
        psy.invokes.get('invoke_0')._schedule=omp1_schedule

        # Store the results of applying this code transformation as
        # a string
        gen=str(psy.gen)

        # Iterate over the lines of generated code
        for idx,line in enumerate(gen.split('\n')):
            if '!$omp parallel do' in line: omp_do_idx=idx
            if 'DO j=' in line: outer_do_idx=idx
            if 'DO i=' in line: inner_do_idx=idx

        # The OpenMP 'parallel do' directive must occur immediately before
        # the DO loop itself
        assert outer_do_idx-omp_do_idx==1 and outer_do_idx-inner_do_idx==-1

    @pytest.mark.xfail(reason="Not yet implemented")
    def test_loop_fuse_different_spaces(self):
        ''' Test that we raise an error if we attempt to fuse loops that are
            over different spaces '''
        ast,info=parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                    "test_files","gocean0p1",
                                    "fuse_different_spaces_test.f90"),
                       api="gocean0.1")
        psy=PSyFactory("gocean0.1").create(info)
        invokes=psy.invokes
        invoke=invokes.get('invoke_0')
        schedule=invoke.schedule
        lftrans=LoopFuseTrans()
        with pytest.raises(TransformationError):
            lf_schedule,memento = lftrans.apply(schedule.children[0],
                                                schedule.children[1])

    def test_openmp_loop_trans(self):
        ''' test of the OpenMP transformation of an all-points loop '''
        ast,info=parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                    "test_files","gocean0p1",
                                    "openmp_fuse_test.f90"),
                       api="gocean0.1")
        psy=PSyFactory("gocean0.1").create(info)
        invokes=psy.invokes
        invoke=invokes.get('invoke_0')
        schedule=invoke.schedule
        ompf=GOceanOpenMPLoop()

        omp1_schedule,memento = ompf.apply(schedule.children[0])

        # Replace the original loop schedule with the transformed one
        psy.invokes.get('invoke_0')._schedule=omp1_schedule

        # Store the results of applying this code transformation as
        # a string
        gen=str(psy.gen)

        omp_do_idx = -1
        # Iterate over the lines of generated code
        for idx,line in enumerate(gen.split('\n')):
            if '!$omp parallel do' in line: omp_do_idx=idx
            if 'DO j=' in line: outer_do_idx=idx
            if 'DO i=' in line:
                inner_do_idx=idx
                if omp_do_idx > -1: break

        # The OpenMP 'parallel do' directive must occur immediately before
        # the DO loop itself
        assert outer_do_idx-omp_do_idx==1 and inner_do_idx-outer_do_idx==1

class TestTransformationsGOcean1p0:

    def get_invoke(self, file, idx):
        ''' Utility method to get the idx'th invoke from the algorithm
            specified in file '''
        ast,info=parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                    "test_files", "gocean1p0",
                                    file),
                       api="gocean1.0")
        psy = PSyFactory("gocean1.0").create(info)
        invokes = psy.invokes
        invoke = invokes.get('invoke_'+str(idx))
        return psy,invoke

    def test_openmp_region_with_slice(self):
        ''' Test that we can pass the OpenMP PARALLEL region transformation
            a list of nodes specified as a slice '''
        psy,invoke = self.get_invoke("single_invoke_three_kernels.f90", 0)
        schedule = invoke.schedule

        from transformations import OpenMPRegion
        ompr = OpenMPRegion()

        omp_schedule,memento = ompr.apply(schedule.children[1:])

        # Replace the original loop schedule with the transformed one
        invoke._schedule = omp_schedule
        # Store the results of applying this code transformation as
        # a string
        gen = str(psy.gen)
        gen = gen.lower()
        print gen
        # Iterate over the lines of generated code
        within_omp_region = False
        call_count = 0
        for line in gen.split('\n'):
            if '!$omp parallel default' in line:
                within_omp_region = True
            if '!$omp end parallel' in line:
                within_omp_region = False
            if ' call ' in line and within_omp_region:
                call_count += 1

        assert call_count==2

    def test_openmp_region_no_slice(self):
        ''' Test that we can pass the OpenMP PARALLEL region transformation
            a list of nodes specified as node.children '''
        psy,invoke = self.get_invoke("single_invoke_three_kernels.f90", 0)
        schedule = invoke.schedule
        from transformations import OpenMPRegion
        ompr = OpenMPRegion()
        omp_schedule,memento = ompr.apply(schedule.children)
        # Replace the original loop schedule with the transformed one
        invoke._schedule = omp_schedule
        # Store the results of applying this code transformation as
        # a string
        gen = str(psy.gen)
        gen = gen.lower()
        # Iterate over the lines of generated code
        within_omp_region = False
        call_count = 0
        for line in gen.split('\n'):
            if '!$omp parallel default' in line:
                within_omp_region = True
            if '!$omp end parallel' in line:
                within_omp_region = False
            if ' call ' in line and within_omp_region:
                call_count += 1

        assert call_count==3

    def test_openmp_region_before_loops_trans(self):
        ''' Test of the OpenMP PARALLEL region transformation where
            we do the region transformation before the loop
            transformations. '''
        psy,invoke = self.get_invoke("single_invoke_two_kernels.f90", 0)
        schedule = invoke.schedule

        from transformations import OpenMPRegion, GOceanOpenMPOrphanLoop

        # Put all of the loops in the schedule within a single
        # OpenMP region
        ompr = OpenMPRegion()
        omp_schedule,memento = ompr.apply(schedule.children)

        # Put an OpenMP do directive around each loop contained
        # in the region
        ompl = GOceanOpenMPOrphanLoop()
        for child in omp_schedule.children[0].children:
            schedule,memento = ompl.apply(child)
            omp_schedule = schedule

        # Replace the original loop schedule with the transformed one
        invoke._schedule = omp_schedule

        # Store the results of applying this code transformation as
        # a string
        gen = str(psy.gen)

        # Iterate over the lines of generated code
        omp_region_idx = -1
        omp_do_idx = -1
        for idx,line in enumerate(gen.split('\n')):
            if '!$omp parallel default' in line: omp_region_idx = idx
            if '!$omp do' in line: omp_do_idx = idx
            if 'DO j=' in line: break

        assert omp_region_idx != -1
        assert omp_do_idx != -1
        assert omp_do_idx - omp_region_idx == 1

    def test_openmp_region_after_loops_trans(self):
        ''' Test of the OpenMP PARALLEL region transformation where we
            do the loop transformations before the region transformation '''
        psy,invoke = self.get_invoke("single_invoke_two_kernels.f90", 0)
        schedule = invoke.schedule

        from transformations import OpenMPRegion, GOceanOpenMPOrphanLoop

        # Put an OpenMP do directive around each loop contained
        # in the schedule
        ompl = GOceanOpenMPOrphanLoop()
        for child in schedule.children:
            omp_schedule,memento = ompl.apply(child)

        # Now put an OpenMP parallel region around that set of
        # loops
        ompr = OpenMPRegion()
        schedule,memento = ompr.apply(omp_schedule.children)

        # Replace the original loop schedule with the transformed one
        invoke._schedule = schedule

        # Store the results of applying this code transformation as
        # a string
        gen = str(psy.gen)

        # Iterate over the lines of generated code
        omp_region_idx = -1
        omp_do_idx = -1
        for idx,line in enumerate(gen.split('\n')):
            if '!$omp parallel default' in line: omp_region_idx = idx
            if '!$omp do' in line: omp_do_idx = idx
            if 'DO j=' in line: break

        assert omp_region_idx != -1
        assert omp_do_idx != -1
        assert omp_do_idx - omp_region_idx == 1

    def test_openmp_region_commutes_with_loop_trans(self):
        ''' Test that the OpenMP PARALLEL region and (orphan) loop
            transformations commute - i.e. we get the same result
            independent of the order in which they are applied. '''
        psy,invoke = self.get_invoke("single_invoke_two_kernels.f90", 0)
        schedule = invoke.schedule
        # Keep a copy of the original schedule 
        import copy
        orig_schedule = copy.deepcopy(schedule)

        from transformations import OpenMPRegion, GOceanOpenMPOrphanLoop

        # Put an OpenMP do directive around each loop contained
        # in the schedule
        ompl = GOceanOpenMPOrphanLoop()
        for child in schedule.children:
            omp_schedule,memento = ompl.apply(child)

        # Now put an OpenMP parallel region around that set of
        # loops
        ompr = OpenMPRegion()
        schedule,memento = ompr.apply(omp_schedule.children)

        # Replace the original loop schedule with the transformed one
        invoke._schedule = schedule

        # Store the results of applying this code transformation as
        # a string
        loop_before_region_gen = str(psy.gen)

        # Now we do it again but in the opposite order...

        # Put all of the loops in the schedule within a single
        # OpenMP region
        schedule = orig_schedule
        ompr = OpenMPRegion()
        omp_schedule,memento = ompr.apply(schedule.children)

        # Put an OpenMP do directive around each loop contained
        # in the region
        ompl = GOceanOpenMPOrphanLoop()
        for child in omp_schedule.children[0].children:
            schedule,memento = ompl.apply(child)
            omp_schedule = schedule

        # Replace the original loop schedule with the transformed one
        invoke._schedule = omp_schedule

        # Store the results of applying this code transformation as
        # a string
        region_before_loop_gen = str(psy.gen)

        assert region_before_loop_gen == loop_before_region_gen

    def test_openmp_region_node_not_child_of_schedule(self):
        ''' Test that we raise appropriate error if user attempts
            to put a region around a node that is not an immediate child
            of a schedule '''
        psy,invoke = self.get_invoke("single_invoke_three_kernels.f90", 0)
        schedule = invoke.schedule

        from transformations import OpenMPRegion, GOceanOpenMPLoop
        ompl = GOceanOpenMPLoop()
        ompr = OpenMPRegion()

        # Put an OpenMP parallel do around the first loop in the schedule
        lschedule,memento = ompl.apply(schedule.children[0])

        # Attempt to put an OpenMP parallel region around that same loop
        # (which is now a child of an OpenMP loop directive) and the
        # second loop in the schedule
        with pytest.raises(TransformationError):
            rschedule,memento = ompr.apply([schedule.children[0].children[0],
                                            schedule.children[1]])

    def test_openmp_orphan_loop_outside_region(self):
        ''' Test that a generation error is raised if we try and
            have an orphaned OpenMP loop that is not enclosed
            within a parallel region '''
        psy,invoke = self.get_invoke("single_invoke_three_kernels.f90", 0)
        schedule = invoke.schedule

        from transformations import OpenMPRegion, GOceanOpenMPOrphanLoop

        # Put an OpenMP do directive around each loop contained
        # in the schedule
        ompl = GOceanOpenMPOrphanLoop()
        ompr = OpenMPRegion()

        for child in schedule.children:
            omp_schedule,memento = ompl.apply(child)

        # Now enclose all but the last loop in a parallel region
        ompr_schedule,memento = ompr.apply(omp_schedule.children[0:-2])

        # Replace the original loop schedule with the transformed one
        invoke._schedule = ompr_schedule

        # Attempt to generate the transformed code
        with pytest.raises(GenerationError):
            gen = psy.gen

    def test_openmp_parallel_do_inside_parallel_region(self):
        ''' Test that a generation error is raised if we attempt
            to have an OpenMP parallel do within an OpenMP 
            parallel region '''
        psy,invoke = self.get_invoke("single_invoke_three_kernels.f90", 0)
        schedule = invoke.schedule

        from transformations import OpenMPRegion, GOceanOpenMPLoop
        ompl = GOceanOpenMPLoop()
        ompr = OpenMPRegion()

        # Put an OpenMP parallel do directive around all of the loops
        for child in schedule.children:
            omp_schedule,memento = ompl.apply(child)

        print "After first transformation:"
        omp_schedule.view()

        # Now enclose all of the children within a parallel region
        schedule,memento = ompr.apply(omp_schedule.children)

        print "After second transformation:"
        schedule.view()

        # Replace the original loop schedule with the transformed one
        invoke._schedule = schedule

        # Attempt to generate the transformed code
        with pytest.raises(GenerationError):
            gen = psy.gen

    def test_openmp_parallel_region_inside_parallel_do(self):
        ''' Test that a generation error is raised if we attempt
            to have an OpenMP parallel region within an OpenMP 
            parallel do '''
        psy,invoke = self.get_invoke("single_invoke_three_kernels.f90", 0)
        schedule = invoke.schedule

        from transformations import OpenMPRegion, GOceanOpenMPLoop
        ompl = GOceanOpenMPLoop()
        ompr = OpenMPRegion()

        # Put an OpenMP parallel do directive around one of the loops
        omp_schedule,memento = ompl.apply(schedule.children[1])

        # Now attempt to put a parallel region inside that parallel do
        with pytest.raises(TransformationError):
            schedule,memento = ompr.apply([schedule.children[1].children[0]])

    def test_openmp_parallel_do_around_parallel_region(self):
        ''' Test that a generation error is raised if we attempt
            to have an OpenMP parallel region around an OpenMP 
            parallel do '''
        psy,invoke = self.get_invoke("single_invoke_three_kernels.f90", 0)
        schedule = invoke.schedule

        from transformations import OpenMPRegion, GOceanOpenMPLoop
        ompl = GOceanOpenMPLoop()
        ompr = OpenMPRegion()

        # Put a parallel region around two of the loops
        omp_schedule,memento = ompr.apply(schedule.children[0:1])

        # Put an OpenMP parallel do directive around one of those loops
        # (which is now a child of the region directive)
        schedule,memento = ompl.apply(omp_schedule.children[0].children[0])

        # Replace the original loop schedule with the transformed one
        invoke._schedule = schedule

        # Attempt to generate the transformed code
        #with pytest.raises(GenerationError):
        gen = psy.gen

    @pytest.mark.xfail(reason="Not implemented")
    def test_openmp_region_with_children_of_different_types(self):
        ''' Test that we can generate code if we have an
            OpenMP parallel region enclosing children of different types. '''
        psy,invoke = self.get_invoke("single_invoke_three_kernels.f90", 0)
        schedule = invoke.schedule

        from transformations import OpenMPRegion, GOceanOpenMPOrphanLoop
        ompl = GOceanOpenMPOrphanLoop()
        ompr = OpenMPRegion()

        # Put an OpenMP do directive around one loop
        omp_schedule,memento = ompl.apply(schedule.children[1])

        # Now enclose all of the children within a parallel region
        schedule,memento = ompr.apply(omp_schedule.children)

        # Replace the original loop schedule with the transformed one
        invoke._schedule = schedule

        # Attempt to generate the transformed code
        gen = psy.gen

    @pytest.mark.xfail(reason="Test not implemented")
    def test_openmp_region_around_last_two_of_three_loops(self):
        ''' Test that we can generate code if we have an
            OpenMP parallel region enclosing children of different types. '''
        psy,invoke = self.get_invoke("single_invoke_three_kernels.f90", 0)
        schedule = invoke.schedule

        from transformations import OpenMPRegion, GOceanOpenMPOrphanLoop
        ompl = GOceanOpenMPOrphanLoop()
        ompr = OpenMPRegion()

    @pytest.mark.xfail(reason="Test not implemented")
    def test_openmp_region_around_first_two_of_three_loops(self):
        ''' Tests that we can safely put an OpenMP parallel region
            around the first two of three loops in a schedule '''
        psy,invoke = self.get_invoke("single_invoke_three_kernels.f90", 0)
        schedule = invoke.schedule

    @pytest.mark.xfail(reason="Test not implemented")
    def test_openmp_region_around_middle_of_three_loops(self):
        ''' Tests that we can safely put an OpenMP parallel region
            around the middle of three loops in a schedule '''
        psy,invoke = self.get_invoke("single_invoke_three_kernels.f90", 0)
        schedule = invoke.schedule
