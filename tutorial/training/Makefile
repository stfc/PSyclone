
EXAMPLES=$(sort $(wildcard 2.*GameOfLife*))
clean_EXAMPLES=$(addprefix clean_,$(EXAMPLES))
allclean_EXAMPLES=$(addprefix allclean_,$(EXAMPLES))

.PHONY: $(EXAMPLES) $(clean_EXAMPLES) $(allclean_EXAMPLES)

default: $(EXAMPLES)
clean: $(clean_EXAMPLES)
allclean: $(allclean_EXAMPLES)

$(EXAMPLES):
	$(MAKE) -C $@ test

$(clean_EXAMPLES):
	$(MAKE) -C $(patsubst clean_%,%,$@) clean

$(allclean_EXAMPLES):
	$(MAKE) -C $(patsubst allclean_%,%,$@) allclean
