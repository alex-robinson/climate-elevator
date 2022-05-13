#############################################################
##							
## Rules for individual libraries or modules
##
#############################################################

$(objdir)/climate_elevator.o: $(srcdir)/climate_elevator.f90
	$(FC) $(DFLAGS) $(FFLAGS) -c -o $@ $<

#############################################################
##							
## List of library files
##
#############################################################

climelev_libs = 		$(objdir)/climate_elevator.o

