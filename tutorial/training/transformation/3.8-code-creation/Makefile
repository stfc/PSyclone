EXE = gol

SCRIPT = add_if.py
include ../common.mk

SRC_X90 = combine_mod.x90 
SRC_F90 = gol.f90 compute_die_mod.f90 compute_born_mod.f90 \
		  count_neighbours_mod.f90 read_config_mod.f90     \
		  output_field_mod.f90 time_step_mod.f90

OBJ = $(SRC_F90:%.f90=%.o) $(SRC_X90:%.x90=%.o)

transform: $(SRC_X90:%.x90=%.f90)

test: 
	@# First test the code creation using the PSyIR API
	$(MAKE) clean
	PARSE_STRING="" $(MAKE) combine_mod.f90 | grep "Creating tree"
	@# Count the number of j-loops, which should be 2
	cat combine_mod.f90 | grep -c 'j = ystart, ystop' | grep 2

	@# Then test the code creation by parsing a string
	$(MAKE) clean
	PARSE_STRING="yes" $(MAKE) combine_mod.f90 | grep "Parsing string"
	@# Count the number of j-loops, which should be 2
	cat combine_mod.f90 | grep -c 'j = ystart, ystop' | grep 2
	
test_run: test_run-default test

$(EXE): $(OBJ)
	$(F90) $(F90FLAGS) $(OBJ) $(LDFLAGS) -o $(EXE)

# Generic compilation rule
# ------------------------
.precious: $(SRC_X90:%.x90=%.f90)

%.f90: %.x90 $(SCRIPT)
	$(PSYCLONE) -s ./$(SCRIPT) -o $*.f90 $<

%.o: %.f90
	$(F90) -c $(F90FLAGS) $<

# Dependencies:
# -------------
$(OBJ): $(INF_LIB)
gol.o: read_config_mod.o time_step_mod.o
time_step_mod.o: combine_mod.o compute_born_mod.o compute_die_mod.o \
	count_neighbours_mod.o output_field_mod.o

clean:
	rm -f $(OBJ) $(EXE) *.mod $(SRC_X90:%.x90=%.f90)
