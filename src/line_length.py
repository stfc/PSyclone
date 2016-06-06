# -----------------------------------------------------------------------------
# (c) The copyright relating to this work is owned jointly by the Crown,
# Met Office and NERC 2015.
# However, it has been created with the help of the GungHo Consortium,
# whose members are identified at https://puma.nerc.ac.uk/trac/GungHo/wiki
# -----------------------------------------------------------------------------
# Author R. Ford STFC Daresbury Lab

''' Provides support for breaking long fortran lines into smaller ones
to allow the code to conform to the maximum line length limits (132
for f90 free format is the default)'''


def find_break_point(line, max_index, key_list):
    ''' find the most appropriate break point for a fortran line '''

    for key in key_list:
        idx = line.rfind(key, 0, max_index)
        if idx > 0:
            return idx+len(key)
    raise Exception(
        "Error in find_break_point. No suitable break point found"
        " for line '" + line[:max_index] + "' and keys '" +
        str(key_list) + "'")


class FortLineLength(object):

    ''' This class take a free format fortran code as a string and
    line wraps any lines that are larger than the specified line
    length'''

    def __init__(self, line_length=132):
        self._line_length = line_length
        self._line_types = ["statement", "openmp_directive",
                            "openacc_directive", "comment",
                            "unknown"]
        self._cont_start = {"statement": "&",
                            "openmp_directive": "!$omp& ",
                            "openacc_directive": "!$acc& ",
                            "comment": "!& ",
                            "unknown": "&"}
        self._cont_end = {"statement": "&",
                          "openmp_directive": " &",
                          "openacc_directive": " &",
                          "comment": "",
                          "unknown": "&"}
        self._key_lists = {"statement": [", ", ",", " "],
                           "openmp_directive": [" ", ",", ")", "="],
                           "openacc_directive": [" ", ",", ")", "="],
                           "comment": [" ", ".", ","],
                           "unknown": [" ", ",", "=", "+", ")"]}
        import re
        self._stat = re.compile(r'^\s*(INTEGER|REAL|TYPE|CALL|SUBROUTINE|USE)',
                                flags=re.I)
        self._omp = re.compile(r'^\s*!\$OMP', flags=re.I)
        self._acc = re.compile(r'^\s*!\$ACC', flags=re.I)
        self._comment = re.compile(r'^\s*!')

    def long_lines(self, fortran_in):
        '''returns true if at least one of the lines in the input code is
           longer than the allowed length. Otherwise returns false '''
        for line in fortran_in.split('\n'):
            if len(line) > self._line_length:
                return True
        return False

    @property
    def length(self):
        ''' returns the maximum allowed line length'''
        return self._line_length

    def process(self, fortran_in):
        ''' takes fortran code as a string as input and output fortran
        code as a string with any long lines wrapped appropriately '''

        fortran_out = ""
        for line in fortran_in.split('\n'):
            if len(line) > self._line_length:
                line_type = self._get_line_type(line)

                c_start = self._cont_start[line_type]
                c_end = self._cont_end[line_type]
                key_list = self._key_lists[line_type]

                break_point = find_break_point(
                    line, self._line_length-len(c_end), key_list)
                fortran_out += line[:break_point] + c_end + "\n"
                line = line[break_point:]
                while len(line) + len(c_start) > self._line_length:
                    break_point = find_break_point(
                        line, self._line_length-len(c_end)-len(c_start),
                        key_list)
                    fortran_out += c_start + line[:break_point] + c_end + "\n"
                    line = line[break_point:]
                if line:
                    fortran_out += c_start + line + "\n"
            else:
                fortran_out += line + "\n"

        # We add an extra newline so remove it when we return
        return fortran_out[:-1]

    def _get_line_type(self, line):
        ''' Classes lines into diffrent types. This is required as
        directives need different continuation characters to fortran
        statements. It also enables us to know a little about the
        structure of the line which could be useful at some point.'''

        if self._stat.match(line):
            return "statement"
        if self._omp.match(line):
            return "openmp_directive"
        if self._acc.match(line):
            return "openacc_directive"
        if self._comment.match(line):
            return "comment"
        return "unknown"
