CC = stack ghc --
# FLAGS = -threaded -O2
# FLAGS = -dynamic
BINDIR = bin
OBJDIR = objects
IDIR = interfaces
SOURCES = $(wildcard *.hs)
OBJECTS	= $(SOURCES:.hs=)

brainfk: $(SOURCES)
	@echo "Compiling brainfk"
	@$(CC) $(FLAGS) -odir $(OBJDIR) -hidir $(IDIR) -o "$(BINDIR)/brainfk" brainfk.hs

all: brainfk

clean:
	@echo "Removing binaries..."
	@rm -f $(BINDIR)/*

	@echo "Removing intermediate files..."
	@rm -f $(IDIR)/*

	@echo "Removing object files..."
	@rm -f $(OBJDIR)/*
