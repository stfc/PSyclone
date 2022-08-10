DIRS := $(subst $(LFRIC_PATH),,\
		    $(shell find $(LFRIC_PATH) -type d -print))
LFRIC_INCLUDE_FLAGS := $(patsubst %,-I $(LFRIC_PATH)/%,$(DIRS))
F90FLAGS += $(LFRIC_INCLUDE_FLAGS)
