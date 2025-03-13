
EXAMPLES=$(sort $(wildcard 2.*GameOfLife*))
clean_EXAMPLES=$(addprefix clean_,$(EXAMPLES))
allclean_EXAMPLES=$(addprefix allclean_,$(EXAMPLES))

.PHONY: test $(EXAMPLES) $(clean_EXAMPLES) $(allclean_EXAMPLES)

default: $(EXAMPLES)
clean: $(clean_EXAMPLES)
	make -C gol-lib clean

allclean: $(allclean_EXAMPLES)

test: $(EXAMPLES)

$(EXAMPLES):
	$(MAKE) -C $@/solution test

$(clean_EXAMPLES):
	$(MAKE) -C $(patsubst clean_%,%,$@) clean
	$(MAKE) -C $(patsubst clean_%,%,$@)/solution clean

$(allclean_EXAMPLES):
	$(MAKE) -C $(patsubst allclean_%,%,$@)/solution allclean
