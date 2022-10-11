class MetaFuncsArgMetadata:
    ''' xxx '''
    def __init__(self, function_space, basis_function=False, diff_basis_function=False):
        self._function_space = function_space
        self._basis_function = basis_function
        self._diff_basis_function = diff_basis_function

    def create_from_fortran_string(fortran_string):
        ''' xxx '''
        string_list = fortran_string.split("(")
        string_list = string_list[1].split(")")
        string_list = string_list[0].split(",")
        function_space = string_list[0].strip()
        basis_function = False
        diff_basis_function = False
        if len(string_list) > 1:
            if string_list[1].strip().lower() == "gh_basis":
                basis_function = True
            if string_list[1].strip().lower() == "gh_diff_basis":
                diff_basis_function = True
        if len(string_list) > 2:
            if string_list[2].strip().lower() == "gh_basis":
                basis_function = True
            if string_list[2].strip().lower() == "gh_diff_basis":
                diff_basis_function = True
        return MetaFuncsArgMetadata(function_space, basis_function=basis_function, diff_basis_function=diff_basis_function)
        
    def fortran_string(self):
        ''' xxx '''
        args_str_list = [self._function_space]
        if self._basis_function:
            args_str_list.append("gh_basis")
        if self._diff_basis_function:
            args_str_list.append("gh_diff_basis")
        args_str = ", ".join(args_str_list)
        return(f"func_type({args_str})")
