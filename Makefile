# Compiler settings
FC = gfortran

# List of module files
MODULES = functions.mod converter.mod lattice_maker.mod

# Main program
PROGRAM = action

# Object files
OBJECTS = action.o convert.o lattice.o

# Source files
SOURCES = action.f90 convert.f90 lattice.f90

# Default target
all: $(PROGRAM)

# Main program compilation
$(PROGRAM): $(OBJECTS)
	$(FC) -o $(PROGRAM) $(OBJECTS)

#Compilation rules for object files
%.o:%.f90
	$(FC) -c -o $@ $<

# Compilation rules for module files
%.mod: %.f90
	$(FC) -c $<



# Clean target
clean:
	rm -f $(PROGRAM) $(MODULES) $(OBJECTS)