#!/bin/bash -f

export CC=gcc

export SRC=$PWD
export BUILD=${SRC}/build

export COMMON_LIBS=/home/gvernier/Sandboxes/common_libs/

export EIGEN3_INCLUDE_DIR=/usr/include/eigen3/

# Need boost library
export BOOST_ROOT=/home/gvernier/Sandboxes/boost_1_64_0/

export OPENMPI_BINDIR=/home/gvernier/Sandboxes/common_libs/bin

rm -rf ${BUILD}; mkdir ${BUILD}; cd ${BUILD}
#export NETCDF=
export NETCDF_LIBRARIES="/lib/libnetcdff.a;/lib/libnetcdf.a"
#export PNETCDF_LIBRARIES="${COMMON_LIBS}/lib/libpnetcdf.a"
export PATH=${PATH}:${SRC}/ecbuild/bin:$EIGEN3_INCLUDE_DIR

# configure
ecbuild --build=debug -D_GLIBCXX_USE_CXX11_ABI=0 -DNETCDF_Fortran=ON -DCMAKE_CXX_COMPILER=${OPENMPI_BINDIR}/mpicxx -DCMAKE_C_COMPILER=${OPENMPI_BINDIR}/mpicc -DCMAKE_Fortran_COMPILER=${OPENMPI_BINDIR}/mpifort -DBOOST_ROOT=$BOOST_ROOT -DBoost_NO_SYSTEM_PATHS=ON ${SRC}



# Compile
make VERBOSE=1 -j4

exit 0
