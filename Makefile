CC = stack ghc --
# FLAGS = -threaded -O2
# FLAGS = -dynamic
DIR = bin
SOURCES = $(wildcard *.hs)
OBJECTS	= $(SOURCES:.hs=)


%: 	
	@echo "Compiling $@"
	@$(CC) $(FLAGS) -o "$(DIR)/$@" $@.hs

all: 	$(OBJECTS)
	@echo "Made all"

clean:
	@echo "Removing binaries..."
	@rm -f $(DIR)/*
	
	@echo "Removing intermediate files..."
	@rm -f *.hi

	@echo "Removing object files..."
	@rm -f *.o
