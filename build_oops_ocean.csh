#!/bin/bash -f
export CC=gcc

export SRC=$PWD
export BUILD=${SRC}/build

rm -rf ${BUILD}; mkdir ${BUILD}; cd ${BUILD}
#export NETCDF=
export NETCDF_LIBRARIES="/lib/libnetcdff.a;/lib/libnetcdf.a"
export PATH=${PATH}:${SRC}/ecbuild/bin
# configure
ecbuild --build=debug -DNETCDF_Fortran=ON ${SRC}

# Compile
make VERBOSE=1 -j4

exit 0
