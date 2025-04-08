##############################################################################
# Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer
# For further details please refer to the file LICENCE.original which you
# should have received as part of this distribution.
##############################################################################
#
# Run this make file to generate PSyKAl source in WORKING_DIR from algorithms
# and kernels in SOURCE_DIR. Transformation scripts are sought in
# OPTIMISATION_PATH.
#
ALGORITHM_F_FILES := $(patsubst $(SOURCE_DIR)/%.X90, \
                                $(WORKING_DIR)/%.f90, \
                                $(shell find $(SOURCE_DIR) -name '*.X90' -print))

ALGORITHM_f_FILES := $(patsubst $(SOURCE_DIR)/%.x90, \
                                $(WORKING_DIR)/%.f90, \
                                $(shell find $(SOURCE_DIR) -name '*.x90' -print))

DIRECTORIES := $(patsubst $(SOURCE_DIR)%,$(WORKING_DIR)%, \
                          $(shell find $(SOURCE_DIR) -type d -printf '%p/\n'))
PSYCLONE_CONFIG_FILE ?= $(CORE_ROOT_DIR)/etc/psyclone.cfg

.PHONY: psyclone
psyclone: $(ALGORITHM_F_FILES) $(ALGORITHM_f_FILES)

include $(LFRIC_BUILD)/lfric.mk
include $(LFRIC_BUILD)/fortran.mk

MACRO_ARGS := $(addprefix -D,$(PRE_PROCESS_MACROS))

# Where an override file exists in the "psy" directory we invoke PSyclone, then
# delete the resulting PSy source. The override has been copied as part of the
# rest of the source.
#
$(WORKING_DIR)/%.f90: \
$$(SOURCE_DIR)/psy/$$(notdir $$*)_psy.f90 $(WORKING_DIR)/%_psy.f90
	$(call MESSAGE,Removing,$*_psy.f90)
	$Qrm $(WORKING_DIR)/$*_psy.f90

# Where an optimisation script exists for a specific file, use it.
#
$(WORKING_DIR)/%.f90 $(WORKING_DIR)/%_psy.f90: \
$(WORKING_DIR)/%.x90 $$(OPTIMISATION_PATH)/$$*.py | $$(dir $$@)
	$(call MESSAGE,PSyclone - local optimisation,$(subst $(SOURCE_DIR)/,,$<))
	$QPYTHONPATH=$(LFRIC_BUILD)/psyclone:$$PYTHONPATH psyclone -api lfric \
	           -l all -d $(WORKING_DIR) \
	           --config $(PSYCLONE_CONFIG_FILE) \
	           -s $(OPTIMISATION_PATH)/$*.py \
	           -okern $(WORKING_DIR) \
	           -oalg $(WORKING_DIR)/$*.f90 \
	           -opsy $(WORKING_DIR)/$*_psy.f90 $<

# Where a global optimisation script exists, use it.
#
$(WORKING_DIR)/%.f90 $(WORKING_DIR)/%_psy.f90: \
$(WORKING_DIR)/%.x90 $(OPTIMISATION_PATH)/global.py | $$(dir $$@)
	$(call MESSAGE,PSyclone - global optimisation,$(subst $(SOURCE_DIR)/,,$<))
	$QPYTHONPATH=$(LFRIC_BUILD)/psyclone:$$PYTHONPATH psyclone -api lfric \
	           -l all -d $(WORKING_DIR) \
	           --config $(PSYCLONE_CONFIG_FILE) \
	           -s $(OPTIMISATION_PATH)/global.py \
	           -okern $(WORKING_DIR) \
	           -oalg  $(WORKING_DIR)/$*.f90 \
	           -opsy $(WORKING_DIR)/$*_psy.f90 $<

# Where no optimisation script exists, don't use it.
#
$(WORKING_DIR)/%.f90 $(WORKING_DIR)/%_psy.f90: \
$(WORKING_DIR)/%.x90 | $$(dir $$@)
	$(call MESSAGE,PSyclone,$(subst $(SOURCE_DIR)/,,$<))
	$QPYTHONPATH=$(LFRIC_BUILD)/psyclone:$$PYTHONPATH psyclone -api lfric \
	           -l all -d $(WORKING_DIR) \
	           --config $(PSYCLONE_CONFIG_FILE) \
	           -okern $(WORKING_DIR) \
	           -oalg  $(WORKING_DIR)/$*.f90 \
	           -opsy $(WORKING_DIR)/$*_psy.f90 $<

.PRECIOUS: $(WORKING_DIR)/%.x90
# Perform preprocessing for big X90 files.
#
$(WORKING_DIR)/%.x90: $(SOURCE_DIR)/%.X90 | $$(dir $$@)
	$(call MESSAGE,Preprocessing, $(subst $(SOURCE_DIR)/,,$<))
	$Q$(FPP) $(FPPFLAGS) $(MACRO_ARGS) $< $@

# Little x90 files are just copied to the workspace.
#
$(WORKING_DIR)/%.x90: $(SOURCE_DIR)/%.x90 | $$(dir $$@)
	$(call MESSAGE,Copying, $(subst $(SOURCE_DIR)/,,$<))
	$Qcp $< $@

# Create directories in the workspace as needed.
#
$(DIRECTORIES):
	$(call MESSAGE,Creating,$@)
	$Qmkdir -p $@
