''' xxx '''
from psyclone.psyir.visitor.fortran import FortranPSyIRVisitor

def trans(psy):
    ''' xxx '''
    nkern = 0
    for invoke in psy.invokes.invoke_list:
        schedule = invoke.schedule
        for kernel in schedule.kern_calls():
            nkern += 1
            kernel_schedule = kernel.get_kernel_schedule()
            fortran_psyir_visitor = FortranPSyIRVisitor()
            fortran_psyir_visitor.visit(kernel_schedule)
            print(fortran_psyir_visitor.code)
    print ("transformed {0} kernels".format(nkern))
