
EXAMPLES=$(sort $(wildcard 2.*GameOfLife*))
allclean_EXAMPLES=$(addprefix allclean_,$(EXAMPLES))
clean_EXAMPLES=$(addprefix clean_,$(EXAMPLES))
test_EXAMPLES=$(addprefix test_,$(EXAMPLES))
test_run_EXAMPLES=$(addprefix test_run_,$(EXAMPLES))

.PHONY: $(allclean_EXAMPLES) $(clean_EXAMPLES) $(compile_EXAMPLES) \
	    $(test_run_EXAMPLES) $(test_EXAMPLES)                      \
	    allclean clean default test test_run

default: test

test_run: $(test_run_EXAMPLES)

test: $(test_EXAMPLES)

clean: $(clean_EXAMPLES)
	make -C gol-lib clean

allclean: $(allclean_EXAMPLES)

$(allclean_EXAMPLES):
	$(MAKE) -C $(patsubst allclean_%,%,$@) allclean
	$(MAKE) -C $(patsubst allclean_%,%,$@)/solution allclean

$(clean_EXAMPLES):
	$(MAKE) -C $(patsubst clean_%,%,$@) clean
	$(MAKE) -C $(patsubst clean_%,%,$@)/solution clean

$(test_run_EXAMPLES):
	$(MAKE) -C $(patsubst test_run_%,%,$@)/solution test_run

$(test_EXAMPLES):
	$(MAKE) -C $(patsubst test_%,%,$@)/solution test
