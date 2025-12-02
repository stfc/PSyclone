
EXAMPLES=$(sort $(wildcard 4.*))
allclean_EXAMPLES=$(addprefix allclean_,$(EXAMPLES))
clean_EXAMPLES=$(addprefix clean_,$(EXAMPLES))
compile_EXAMPLES=$(addprefix compile_,$(EXAMPLES))
run_EXAMPLES=$(addprefix run_,$(EXAMPLES))
test_EXAMPLES=$(addprefix test_,$(EXAMPLES))

.PHONY: $(allclean_EXAMPLES) $(clean_EXAMPLES) $(compile_EXAMPLES) \
        $(run_EXAMPLES) $(test_EXAMPLES)

default: $(test_EXAMPLES)
clean: $(clean_EXAMPLES)
allclean: $(allclean_EXAMPLES)

test: $(test_EXAMPLES)

$(test_EXAMPLES):
	$(MAKE) -C $(patsubst clean_%,%,$@)/solution test

$(clean_EXAMPLES):
	$(MAKE) -C $(patsubst clean_%,%,$@) clean
	$(MAKE) -C $(patsubst clean_%,%,$@)/solution clean

$(allclean_EXAMPLES):
	$(MAKE) -C $(patsubst allclean_%,%,$@)/solution allclean
