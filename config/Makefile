.SUFFIXES: .f .F .F90 .f90 .o .mod
.SHELL: /bin/sh

# PATH options
srcdir = src
objdir = libclimelev/include
bindir = libclimelev/bin
libdir = libs

# Command-line options at make call
debug    ?= 0
parallel ?= 0 

## COMPILER CONFIGURATION ##
# (should be loaded from config directory)

<COMPILER_CONFIGURATION>

# Determine whether to use normal flags or debugging flags
DFLAGS   = $(DFLAGS_NODEBUG)
ifeq ($(debug), 1)
	DFLAGS   = $(DFLAGS_DEBUG)
endif

# Debugging flags with profiling output enabled
ifeq ($(debug), 2)
	DFLAGS   = $(DFLAGS_PROFILE)
endif

###############################################
##
## List of rules and source files
##
###############################################

include config/Makefile_climelev.mk

###############################################
##
## Compilation of complete programs
##
###############################################

# Compile the static library libyelmo,
# using Makefile located in $(YELMOROOT) directory 
yelmo-static: 
		$(MAKE) -C $(YELMOROOT) yelmo-static parallel=$(parallel)

test : $(objdir)/ncio.o $(climelev_libs)
		$(FC) $(DFLAGS) $(FFLAGS) -o ./libclimelev/bin/test.x test.f90 \
			$(LFLAGS) $(objdir)/ncio.o
		@echo " "
		@echo "    ./libclimelev/bin/test.x is ready."
		@echo " "

.PHONY : usage
usage:
	@echo ""
	@echo "    * USAGE * "
	@echo ""
	@echo " make test       : compiles test program"
	@echo " make clean      : cleans object files"
	@echo ""

clean:
	rm -f $(bindir)/*.x
	rm -f  *.x gmon.out $(objdir)/*.o $(objdir)/*.mod $(objdir)/*.a $(objdir)/*.so
	rm -rf *.x.dSYM
	rm -f ${YELMOROOT}/libyelmo/include/*.a ${YELMOROOT}/libyelmo/include/*.o 

