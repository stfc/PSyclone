transform:
	./handle_fortran.py
	./var_access.py
	./sympy_test.py
	./dataflow.py

test_run test:
	./handle_fortran.py | grep "b = 3 \* x \* x - 2 \* x - 1"
	./var_access.py | grep "a: READ, b: WRITE+READWRITE+READ, bar: CALL, c: WRITE+READ+READWRITE, d: WRITE+READ, e: WRITE+READ, f: WRITE"
	./sympy_test.py | grep "b = 6 \* x - 2"
	./dataflow.py | grep "\"c = d \* a\" -> \"call bar(c, b)\" \[label=\"c\"\]"

allclean clean:
	# Nothing to clean here
