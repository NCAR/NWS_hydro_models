#========================================================================
# Makefile to compile the Snow-17/SAC,SCE and driver code for gauge calibrations
#========================================================================
#  Modified:
#  Andy Newman, Sept 2013
#  A. Wood, Dec 2015, updating for model that uses external PET & gfortran comp. 
#           Jan 2016, renamed some subroutines, replaced several for code
#                     that now runs in distributed fashion
#           Aug 2015, major overhaul of code, updated makefile
#  Elizabeth Clark, Feb 2017, adding documentation and separating executable
#                     from FC and FC77 definitions so that flags set
#                     properly even if compiler path is needed. Moved compiler
#                     type and executable to PART 0 so that user will be
#                     more likely to set it
#
#========================================================================
# PART 0: Define directory paths
#========================================================================

# Define core directory below which everything resides
F_MASTER_DIR = /home/hydrofcst/SHARP/models/nws/code/NWS_hydro_models/Snow17SacUH/src_bin/

# Location of the compiled modules
MOD_PATH = $(F_MASTER_DIR)/driver

# Define the executable and path
EXE = $(F_MASTER_DIR)/Snow17SacUH.exe

# Define the Fortran Compiler type
# This is used in if statements in this makefile to define flags that are
# specific to each compiler.
# DO NOT SPECIFY the full path to the executable here. You can do so below
#FC  = pgf90
#FC77 = pgf77
FC  = ifort
FC77 = ifort
#FC  = gfortran
#FC77 = gfortran

# Define the Fortran Compiler executable
# This is not used in the if statements in this makefile, but simply to do
# the actual compilation.
# MAKE SURE this is consistent with the compiler type defined above
#FCEXE = /opt/pgi-15.7/linux86-64/15.7/bin/pgf90
#FC77EXE = /opt/pgi-15.7/linux86-64/15.7/bin/pgf77
FCEXE  = ifort
FC77EXE = ifort
#FCEXE  = gfortran
#FC77EXE = gfortran

#========================================================================
# PART 1: Assemble all of the various sub-routines
#========================================================================

# Define directories
DRIVER_DIR = $(F_MASTER_DIR)/driver
sac_dir    = $(F_MASTER_DIR)/sac
snow19_dir = $(F_MASTER_DIR)/snow19
share_dir  = $(F_MASTER_DIR)/share

# share utilities
calib_UTIL= \
		nrtype.f90 \
		constants.f90 \
		def_namelists.f90 \
		interfaces.f90 \
		read_namelist.f90 \
		ascii_io.f90 \
		day_of_year.f90 \
		sfc_pressure.f90
UTIL = $(patsubst %, $(share_dir)/%, $(calib_UTIL))

# snow-19 code (fortran 90, different options)
calib_snow19= \
		zero19.f \
		rout19.f \
		aesc19.f \
		melt19.f \
		SNEW.f \
		SNOWT.f \
		SNOWPACK.f \
		adjc19.f \
		aeco19.f \
		updt19.f \
		SNDEPTH.f \
		PACK19.f \
		exsnow19.f
snow19 = $(patsubst %, $(snow19_dir)/%, $(calib_snow19))

# sac code (fortran 77)
calib_sac_77= \
		sac1.f \
		ex_sac1.f \
		duamel.f
sac_77 = $(patsubst %, $(sac_dir)/%, $(calib_sac_77))

#========================================================================
# PART 2: Define the libraries, driver programs, and executables
#========================================================================

# Define the libraries and path to include files
ifeq "$(FC)" "pgf90"
 LOCL_PATH = /usr/local
 LIB = -L$(LOCL_PATH)/lib
 INC = -I ./
endif

ifeq "$(FC77)" "pgf77"
 LOCL_PATH = /usr/local
 LIB77 = -L$(LOCL_PATH)/lib
 INC77 = -I ./
endif

# Define the driver routine
calib_DRIVER = multi_driver.f90

DRIVER = $(patsubst %, $(DRIVER_DIR)/%, $(calib_DRIVER))

#========================================================================
# PART 3: Compile the puppy
#========================================================================

# Define flags
ifeq "$(FC)" "gfortran"
  # do not use -fdefault-real-8 since this will change the outcomes
  FLAGS_DEBUG = -static -Wall -g -ffixed-line-length-none
  FLAGS = -O3 -fno-align-commons -ffixed-line-length-none
  FLAGS2 = -O3 -fno-align-commons -ffixed-line-length-none
endif

ifeq "$(FC)" "ifort"
  # do not use -autodouble since this will change the outcomes
  FLAGS_PROF = -static -debug -warn all -check all -FR -O0 -auto -WB -traceback -g -fltconsistency -fpe0
  FLAGS = -O3 -warn all -check all
  FLAGS2 = -O3
endif

ifeq "$(FC)" "pgf90"
  # do not use -r8 since this will change the outcomes
  FLAGS_PROF = -Bstatic -Mbackslash -g -Mchkptr -Mchkstk -Mpgicoff -Minform=inform -Ktrap=divz,inv -Mprof=lines,time
  FLAGS = -O3 -Kieee
  FLAGS2 = -O3
endif

ifeq "$(FC77)" "gfortran"
  # do not use -fdefault-real-8 since this will change the outcomes
  FLAGS77 = -O3 -fno-align-commons -ffixed-line-length-none
endif

ifeq "$(FC77)" "ifort"
  # do not use -autodouble since this will change the outcomes
  FLAGS77 = -O3 -f77rtl
endif

ifeq "$(FC77)" "pgf77"
  # do not use -r8 since this will change the outcomes
  FLAGS77 = -O3
endif

# -- Compile --

all: compile_calib link clean
debug: compile_debug link

check:
	echo test
	echo $(FCEXE)

# compile calibration code
compile_calib:
	$(FC77EXE) $(FLAGS77) -c $(sac_77) $(INC77)
	$(FCEXE) $(FLAGS2) -c $(snow19) $(INC)
	$(FCEXE) $(FLAGS) -c $(UTIL) $(DRIVER) \
	$(INC)

compile_debug:
	$(FC77EXE) $(FLAGS77) -g -fcheck=all -c $(sac_77) $(INC77)
	$(FCEXE) $(FLAGS2) -g -fcheck=all -c $(snow19) $(INC)
	$(FCEXE) $(FLAGS) -g -fcheck=all -c $(UTILS) $(DRIVER) \
	$(INC)

# link routines
link:
	$(FC) -fPIC -I./ $(LIB) -o $(EXE) *.o

# Remove object files
clean:
	rm -f *.o
	rm -f *.mod
