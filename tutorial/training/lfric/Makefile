
EXAMPLES=$(sort $(wildcard 4.*))
allclean_EXAMPLES=$(addprefix allclean_,$(EXAMPLES))
clean_EXAMPLES=$(addprefix clean_,$(EXAMPLES))
test_EXAMPLES=$(addprefix test_,$(EXAMPLES))
test_run_EXAMPLES=$(addprefix test_run_,$(EXAMPLES))

.PHONY: $(allclean_EXAMPLES) $(clean_EXAMPLES) \
        $(test_EXAMPLES) $(test_run_EXAMPLES)  \
        allclean clean run test

default: test

allclean: $(allclean_EXAMPLES)

$(allclean_EXAMPLES):
	$(MAKE) -C $(patsubst allclean_%,%,$@)/solution allclean

clean: $(clean_EXAMPLES)

$(clean_EXAMPLES):
	$(MAKE) -C $(patsubst clean_%,%,$@) clean
	$(MAKE) -C $(patsubst clean_%,%,$@)/solution clean


test: $(test_EXAMPLES)

$(test_EXAMPLES):
	$(MAKE) -C $(patsubst test_%,%,$@)/solution test

test_run: $(test_run_EXAMPLES)

$(test_run_EXAMPLES):
	$(MAKE) -C $(patsubst test_run_%,%,$@)/solution test_run

