
EXAMPLES=$(sort $(wildcard 2.*GameOfLife*))
allclean_EXAMPLES=$(addprefix allclean_,$(EXAMPLES))
clean_EXAMPLES=$(addprefix clean_,$(EXAMPLES))
test_EXAMPLES=$(addprefix test_,$(EXAMPLES))
test_run_EXAMPLES=$(addprefix test_run_,$(EXAMPLES))

.PHONY: $(allclean_EXAMPLES) $(clean_EXAMPLES) \
        $(test_EXAMPLES) $(test_run_EXAMPLES)  \
        allclean clean default test test_run

default: test

test: $(test_EXAMPLES)

test_run: $(test_run_EXAMPLES)

allclean: $(allclean_EXAMPLES)

$(allclean_EXAMPLES):
	$(MAKE) -C $(patsubst allclean_%,%,$@) allclean
	$(MAKE) -C $(patsubst allclean_%,%,$@)/solution allclean

clean: $(clean_EXAMPLES)
	make -C gol-lib clean

$(clean_EXAMPLES):
	$(MAKE) -C $(patsubst clean_%,%,$@) clean
	$(MAKE) -C $(patsubst clean_%,%,$@)/solution clean

$(test_EXAMPLES):
	$(MAKE) -C $(patsubst test_%,%,$@)/solution test

$(test_run_EXAMPLES):
	$(MAKE) -C $(patsubst test_run_%,%,$@)/solution test_run
