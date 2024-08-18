''' Script to demonstrate the use on the line wrapping support in PSyclone'''

from psyclone.generator import generate
from psyclone.line_length import FortLineLength

# long_lines=True checks whether the input fortran conforms to the 132 line
# length limit
ALG, PSY = generate(
    "longlines.f90",
    line_length=True)
LINE_LENGTH = FortLineLength()
ALG_STR = LINE_LENGTH.process(str(ALG))
print(ALG_STR)
PSY_STR = LINE_LENGTH.process(str(PSY))
print(PSY_STR)
