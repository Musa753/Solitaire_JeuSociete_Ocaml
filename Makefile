# Specify the compiler
OCAMLC=ocamlc

# Set compiler flags
OCAMLC_FLAGS=

# Define source files
SRC=solitaire.ml

# Define the executable name
EXE=solitaire

# Define the build target
all: $(EXE)

# Define the rule to build the executable
$(EXE): $(SRC)
	$(OCAMLC) $(OCAMLC_FLAGS) -o $@ $<

# Define a rule to clean up generated files
clean:
	rm -f $(EXE) *.cmi *.cmo
