
__all__ = ['set_read_options', 'set_parse_options',
           'get_fortran_code_group']
from optparse import OptionGroup, NO_DEFAULT

def set_read_options(parser):
    parser.set_usage('''\
%prog [options] <Fortran files>

Description:
  %prog reads Fortran codes.''')
    parser.add_option('--task',
                      default = 'show',
                      choices = ['show'],
                      help = 'Specify reading task. Default: %default.'
                      )
    parser.add_option_group(get_fortran_code_group(parser))

def set_parse_options(parser):
    parser.set_usage('''\
%prog [options] <Fortran files>

Description:
  %prog parses Fortran codes.''')
    parser.add_option('--task',
                      default = 'show',
                      choices = ['show', 'none'],
                      help = 'Specify parsing result task. Default: %default.'
                      )
    parser.add_option_group(get_fortran_code_group(parser))

def set_f2003_options(parser):
    parser.set_usage('''\
%prog [options] <Fortran files>

Description:
  %prog parses Fortran codes using Fortran 2003 syntax rules.''')
    parser.add_option('--task',
                      default = 'show',
                      choices = ['show', 'none'],
                      help = 'Specify parsing result task. Default: %default.'
                      )
    parser.add_option_group(get_fortran_code_group(parser))
    
def get_fortran_code_group(parser):
    group = OptionGroup (parser, 'Fortran code options',
                         description = 'Specify information about Fortran codes.')
    group.add_option('--mode',
                      default = 'auto',
                      choices = ['auto', 'free', 'fix', 'f77', 'pyf'],
                      help = 'Specify Fortran code mode. Default: %default.'
                      )
    return group
