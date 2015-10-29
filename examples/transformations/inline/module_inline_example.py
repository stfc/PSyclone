''' example showing the use of the module-inline transformation '''


def inline():
    ''' function exercising the module-inline transformation '''
    from parse import parse
    from psyGen import PSyFactory
    import os
    from transformations import KernelModuleInlineTrans

    _, info = parse(os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                 "..", "..", "..", "src", "tests",
                                 "test_files", "dynamo0p1", "algorithm",
                                 "1_single_function.f90"),
                    api="dynamo0.1")
    psy = PSyFactory("dynamo0.1").create(info)
    invokes = psy.invokes
    print psy.invokes.names
    invoke = invokes.get("invoke_0_testkern_type")
    schedule = invoke.schedule
    schedule.view()
    kern = schedule.children[0].children[0]
    # setting module inline directly
    kern.module_inline = True
    schedule.view()
    # unsetting module inline via a transformation
    trans = KernelModuleInlineTrans()
    schedule, _ = trans.apply(kern, inline=False)
    schedule.view()
    # setting module inline via a transformation
    schedule, _ = trans.apply(kern)
    schedule.view()
    print str(psy.gen)

if __name__ == "__main__":
    inline()
