
EXAMPLES=$(sort $(wildcard 2.*GameOfLife*))
allclean_EXAMPLES=$(addprefix allclean_,$(EXAMPLES))
clean_EXAMPLES=$(addprefix clean_,$(EXAMPLES))
compile_EXAMPLES=$(addprefix compile_,$(EXAMPLES))
run_EXAMPLES=$(addprefix run_,$(EXAMPLES))
test_EXAMPLES=$(addprefix test_,$(EXAMPLES))

dummy:
	echo $(run_EXAMPLES)

.PHONY: $(allclean_EXAMPLES) $(clean_EXAMPLES) $(compile_EXAMPLES) \
	    $(run_EXAMPLES) $(test_EXAMPLES)

default: $(test_EXAMPLES)

compile: $(compile_EXAMPLES)

run: $(run_EXAMPLES)

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

$(compile_EXAMPLES):
	$(MAKE) -C $(patsubst compile_%,%,$@)/solution compile

$(run_EXAMPLES):
	$(MAKE) -C $(patsubst run_%,%,$@)/solution run

$(test_EXAMPLES):
	$(MAKE) -C $(patsubst test_%,%,$@)/solution test
