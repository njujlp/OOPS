#!/bin/csh -f

# Clean modules
source $MODULESHOME/init/csh

module purge

# Load Intel compilers, NetCDF and HDF5 libraries
module load intel/15.6.233
module load impi/5.1.2.150
module load hdf5/1.8.14 netcdf/4.3.0

# Load cmake
module use -a /contrib/modulefiles
module load cmake

# Load Boost and Eigen
module use -a /contrib/da/modulefiles
module load boost
module load eigen

module list

# Need eigen3 library
setenv EIGEN3_INCLUDE_DIR $EIGEN_ROOT

# Need NETCDF library
setenv NETCDF_LIBRARIES "${NETCDF}/lib/libnetcdf.a;${NETCDF}/lib/libnetcdff.a"

# Need FMS library (For FV3GFS model only)
setenv FMS_ROOT "/scratch4/NCEPDEV/global/save/Rahul.Mahajan/git/GFDL/FMS"
setenv FMS_LIBRARIES "${FMS_ROOT}/build/libfms.a"
setenv FMS_INCLUDES  "${FMS_ROOT}/build;${FMS_ROOT}/include"

# Define source and build directories
setenv SRC $PWD
setenv BUILD ${SRC}/build

rm -rf ${BUILD}
mkdir -p ${BUILD}
cd ${BUILD}

set path = ( ${SRC}/ecbuild/bin $path )

ecbuild \
    --build=debug \
    -DCMAKE_CXX_COMPILER=mpiicpc \
    -DCMAKE_C_COMPILER=mpiicc \
    -DCMAKE_Fortran_COMPILER=mpiifort \
    -DENABLE_CXX11=ON \
    -DBOOST_ROOT=$BOOST_ROOT -DBoost_NO_SYSTEM_PATHS=ON \
    -DNETCDF_LIBRARIES=$NETCDF_LIBRARIES -DNETCDF_PATH=$NETCDF \
    ${SRC}

# Compile
make VERBOSE=YES -j4

exit 0
