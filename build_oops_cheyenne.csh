#!/bin/csh -f
#------------------------------------------------------------------------------#
# The following is needed at run time:
#  unsetenv LD_LIBRARY_PATH
#  module purge
#  module load gnu cmake/3.7.2 netcdf
#  setenv BOOST_ROOT /glade/p/ral/nsap/jcsda/code/boost_1_64_0
#  setenv LD_LIBRARY_PATH "${LD_LIBRARY_PATH}:${BOOST_ROOT}/stage/lib"
#------------------------------------------------------------------------------#

#git clone https://github.com/JCSDA/OOPS.git
#cd OOPS

# enable modules
source /glade/u/apps/ch/opt/lmod/7.2.1/lmod/lmod/init/tcsh
# reset all modules
module purge
# reset the library path
unsetenv LD_LIBRARY_PATH

# Load modules
module load gnu cmake/3.7.2 netcdf
module list

# Define lapack path
setenv LAPACK_PATH /glade/p/ral/nsap/jcsda/code/lapack-3.7.0
setenv LAPACK_DIR  $LAPACK_PATH
setenv LAPACK_LIBRARIES "$LAPACK_PATH/lib64/liblapack.a;$LAPACK_PATH/lib64/libblas.a"

# Need eigen3 library
setenv EIGEN3_INCLUDE_DIR /glade/p/ral/nsap/jcsda/code/eigen/build

# Need boost library
setenv BOOST_ROOT /glade/p/ral/nsap/jcsda/code/boost_1_64_0

# Need ESMF library
# setenv ESMF_PATH /glade/p/work/svasquez/esmf_install/esmf_710bs33_gnu_openmpi_netcdf_O
# setenv ESMF_LIBRARIES ${ESMF_PATH}/lib/libO/Linux.gfortran.64.openmpi.default/libesmf.a
# setenv ESMF_INCLUDE_DIR ${ESMF_PATH}/mod/modO/Linux.gfortran.64.openmpi.default

# Need NETCDF library
setenv NETCDF_LIBRARIES "${NETCDF}/lib/libnetcdf.a;${NETCDF}/lib/libnetcdff.a"

# Need proper openmpi binaries
setenv OPENMPI_BINDIR "/glade/p/work/xinzhang/common_libs/bin"

setenv SRC $PWD
setenv BUILD ${SRC}/build

rm -rf ${BUILD}; mkdir ${BUILD}; cd ${BUILD}

set path = (${path} ${SRC}/ecbuild/bin $EIGEN3_INCLUDE_DIR)

# configure
ecbuild --build=debug -DCMAKE_CXX_COMPILER=${OPENMPI_BINDIR}/mpicxx -DCMAKE_C_COMPILER=${OPENMPI_BINDIR}/mpicc -DCMAKE_Fortran_COMPILER=${OPENMPI_BINDIR}/mpifort -DBOOST_ROOT=$BOOST_ROOT -DBoost_NO_SYSTEM_PATHS=ON -DLAPACK_PATH=$LAPACK_PATH -DLAPACK_LIBRARIES=$LAPACK_LIBRARIES -NETCDF_LIBRARIES=${NETCDF_LIBRARIES} -DNETCDF_PATH=${NETCDF} ${SRC}

# Compile
make VERBOSE=1 -j4

exit 0

# The following is needed at run time:
   unsetenv LD_LIBRARY_PATH
   module purge
   module load gnu cmake/3.7.2 netcdf
   setenv BOOST_ROOT /glade/p/ral/nsap/jcsda/code/boost_1_64_0
   setenv LD_LIBRARY_PATH "${LD_LIBRARY_PATH}:${BOOST_ROOT}/stage/lib"
