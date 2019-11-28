''' XXX '''
from psyclone.psyGen import Reference, Literal, UnaryOperation, \
    BinaryOperation, NaryOperation, Assignment, IfBlock, Loop, \
    KernelSchedule, Container
from psyclone.psyir.symbols import DataSymbol, SymbolTable, ArgumentInterface
from psyclone.psyir.backend.fortran import FortranWriter

# Symbol table and symbols
symbol_table = SymbolTable()
arg1 = DataSymbol("tmp1", "real", interface=ArgumentInterface(
    ArgumentInterface.Access.READWRITE))
symbol_table.add(arg1)
symbol_table.add(DataSymbol("tmp2", "real"))
symbol_table.add(DataSymbol("i", "integer"))
symbol_table.specify_argument_list([arg1])

# Nodes which do not have Nodes as children
zero = Literal("0.0")
one = Literal("1.0")
tmp1 = Reference("tmp1")
tmp2 = Reference("tmp2")

# Unary operation
oper = UnaryOperation.Operator.SIN
unaryoperation = UnaryOperation.create(oper,tmp2)

# Binary operation
oper = BinaryOperation.Operator.ADD
binaryoperation = BinaryOperation.create(oper, one, unaryoperation)

# Nary operation
oper = NaryOperation.Operator.MAX
naryoperation = NaryOperation.create(oper, [tmp1, tmp2, one])

# Assignments
assign1 = Assignment.create(tmp1, zero)
assign2 = Assignment.create(tmp2, zero)
assign3 = Assignment.create(tmp2, binaryoperation)
assign4 = Assignment.create(tmp2, naryoperation)

# If statement
if_condition = BinaryOperation.create(BinaryOperation.Operator.EQ, tmp1, zero)
ifblock = IfBlock.create(if_condition, [assign3, assign4])

# Loop
loop = Loop.create("i", zero, one, one, [ifblock])

# KernelSchedule
kernel_schedule = KernelSchedule.create("work", symbol_table,
                                        [assign2, loop])

# Container
container_symbol_table = SymbolTable()
container = Container.create("container", container_symbol_table, [kernel_schedule])

writer = FortranWriter()
result = writer(container)
print (str(result))
