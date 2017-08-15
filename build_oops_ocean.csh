#!/bin/bash -f

export CC=gcc

export SRC=$PWD
export BUILD=${SRC}/build

export COMMON_LIBS=/home/gvernier/Sandboxes/common_libs/

#export ESMF_PATH=$COMMON_LIBS #/glade/p/work/svasquez/esmf_install/esmf_710bs33_gnu_openmpi_netcdf_O
#export ESMF_LIBRARIES=${ESMF_PATH}/lib/libesmf.a
#export ESMF_INCLUDE_DIR=${ESMF_PATH}/mod/modO/Linux.gfortran.64.openmpi.default

export EIGEN3_INCLUDE_DIR=/usr/include/eigen3/

export OPENMPI_BINDIR=/home/gvernier/Sandboxes/common_libs/bin

rm -rf ${BUILD}; mkdir ${BUILD}; cd ${BUILD}
#export NETCDF=
export NETCDF_LIBRARIES="/lib/libnetcdff.a;/lib/libnetcdf.a"
#export PNETCDF_LIBRARIES="${COMMON_LIBS}/lib/libpnetcdf.a"
export PATH=${PATH}:${SRC}/ecbuild/bin:$EIGEN3_INCLUDE_DIR

# configure
#ecbuild --build=debug -DNETCDF_Fortran=ON -DESMF_LIBRARY=${ESMF_LIBRARIES} -DESMF_INCLUDE_DIR=${ESMF_INCLUDE_DIR} -DCMAKE_CXX_COMPILER=${OPENMPI_BINDIR}/mpicxx -DCMAKE_C_COMPILER=${OPENMPI_BINDIR}/mpicc -DCMAKE_Fortran_COMPILER=${OPENMPI_BINDIR}/mpifort ${SRC}
ecbuild --build=debug -DNETCDF_Fortran=ON -DCMAKE_CXX_COMPILER=${OPENMPI_BINDIR}/mpicxx -DCMAKE_C_COMPILER=${OPENMPI_BINDIR}/mpicc -DCMAKE_Fortran_COMPILER=${OPENMPI_BINDIR}/mpifort ${SRC}

# Compile
make VERBOSE=1 -j4

exit 0
