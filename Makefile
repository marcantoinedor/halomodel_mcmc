# Makefile to compile halo model

# Standard flags
FFLAGS = \
	-Warray-bounds \
	-fmax-errors=4 \
	-ffpe-trap=invalid,zero,overflow \
	-fimplicit-none \
	-O3 \
	-std=gnu \
	-ffree-line-length-none \
	-fdefault-real-8 \
	-fdefault-double-8 \
	-lgfortran \
	-lm

# Extra debugging flags
DEBUG_FLAGS = \
	-Wall \
	-fcheck=all \
	-fbounds-check \
	-fbacktrace \
	-Og

# Compiler
FC = gfortran 

# Source-code directory
SRC_DIR = src

# Build directory
BUILD_DIR = build

# Module directory
MOD_DIR = ../library/src

# Debug build directory
DEBUG_BUILD_DIR = debug_build

# Library directory
LIB_DIR = lib

# Executable directory
BIN_DIR = bin

# Objects
_OBJ = \
	constants.o \
	physics.o \
	fix_polynomial.o \
	array_operations.o \
	logical_operations.o \
	random_numbers.o \
	file_info.o \
	table_integer.o \
	special_functions.o \
	interpolate.o \
	solve_equations.o \
	string_operations.o \
	calculus_table.o \
	camb_stuff.o \
	cosmology_functions.o \
	limber.o \
	hmx.o

# Add prefixes of build directory to objects
OBJ = $(addprefix $(BUILD_DIR)/,$(_OBJ))
DEBUG_OBJ = $(addprefix $(DEBUG_BUILD_DIR)/,$(_OBJ))

# ?
make_dirs = @mkdir -p $(@D)

# Standard rules
all: bin lib
lib: $(LIB_DIR)/lib.a
bin: $(BIN_DIR)/halo_model

# Debugging rules
debug: FFLAGS += $(DEBUG_FLAGS)
debug: $(BIN_DIR)/halo_model_debug

# Rule to make object files
$(BUILD_DIR)/%.o: $(MOD_DIR)/%.f90
	$(make_dirs)
	$(FC) -c -o $@ $< -J$(BUILD_DIR) $(LDFLAGS) $(FFLAGS)

# Rule to make executable
$(BIN_DIR)/halo_model: $(OBJ) $(SRC_DIR)/halo_model.f90
	@echo "\nBuilding executable.\n"
	$(make_dirs)
	$(FC) -o $@ $^ -J$(BUILD_DIR) $(LDFLAGS) $(FFLAGS)

# Rule to make debugging objects
$(DEBUG_BUILD_DIR)/%.o: $(MOD_DIR)/%.f90
	$(make_dirs)
	$(FC) -c -o $@ $< -J$(DEBUG_BUILD_DIR) $(LDFLAGS) $(FFLAGS)

# Rule to make debugging executable
$(BIN_DIR)/halo_model_debug: $(DEBUG_OBJ) $(SRC_DIR)/halo_model.f90
	@echo "\nBuilding debugging executable.\n"
	$(FC) -o $@ $^ -J$(DEBUG_BUILD_DIR) $(LDFLAGS) $(FFLAGS)

# Rule to make static library
$(LIB_DIR)/lib.a: $(OBJ)
	@echo "\nBuilding static library.\n"
	$(make_dirs)
	$(AR) rc $@ $^

# Clean up
.PHONY: clean
clean:
	rm -f $(BIN_DIR)/halo_model
	rm -f $(LIB_DIR)/libhmx.a
	rm -f $(BUILD_DIR)/*.o
	rm -f $(BUILD_DIR)/*.mod
	rm -f $(SRC_DIR)/*.mod
	rm -f $(DEBUG_BUILD_DIR)/*.o
	rm -f $(DEBUG_BUILD_DIR)/*.mod
