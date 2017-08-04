#!/bin/csh -f

#git clone https://github.com/JCSDA/OOPS

#cheyenne
source /glade/u/apps/ch/opt/lmod/7.2.1/lmod/lmod/init/tcsh
module purge
unsetenv LD_LIBRARY_PATH

#module load gnu netcdf ncarenv ncarcompilers openmpi ncl lapack cmake/3.7.2
module load gnu mpich/3.2 lapack cmake/3.7.2
module list

 unsetenv LAPACK_PATH
 unsetenv LAPACK_DIR
 unsetenv LAPACK_LIBRARIES

#setenv LAPACK_DIR /glade/p/work/vandenb/jcsda/LAPACK/CMAKE
#setenv LAPACK_PATH /glade/p/work/vandenb/jcsda/lapack/lib

#setenv LAPACK_PATH /glade/u/apps/ch/opt/netlib/3.7.0/gnu/4.8.5
#setenv LAPACK_DIR /glade/u/apps/ch/opt/netlib/3.7.0/gnu/4.8.5
#setenv LAPACK_LIBRARIES /glade/u/apps/ch/opt/netlib/3.7.0/gnu/4.8.5/lib
#setenv LAPACK_LIBRARIES $INTEL_COMPILER_BASE_PATH/mkl/lib

#setenv LAPACK_PATH /glade/p/work/vandenb/jcsda
#setenv LAPACK_PATH ~gvernier/lapack/build
#setenv LAPACK_LIBRARIES ~gvernier/lapack/build/lib

setenv EIGEN3_INCLUDE_DIR ~auligne/Bin

setenv BOOST_ROOT /glade/p/work/vandenb/jcsda/boost_1_64_0

setenv SRC $PWD
setenv BUILD ${SRC}/build

#setenv LD_LIBRARY_PATH "${LD_LIBRARY_PATH}:/glade/p/work/vandenb/jcsda/lapack"
#setenv LD_LIBRARY_PATH "${LD_LIBRARY_PATH}:/glade/u/apps/ch/opt/netlib/3.7.0/gnu/4.8.5/lib"
#setenv LD_LIBRARY_PATH "${LD_LIBRARY_PATH}:${LIBRARY_PATH}"
 setenv LD_LIBRARY_PATH "${LD_LIBRARY_PATH}:/glade/u/apps/ch/opt/netlib/3.7.0/gnu/4.8.5/lib"

echo "LD_LIBRARY_PATH = $LD_LIBRARY_PATH"

 exit

rm -rf ${BUILD}; mkdir ${BUILD}; cd ${BUILD}


set path = (${path} ${SRC}/ecbuild/bin ~auligne/Bin) # ~gvernier/boost/build/bin)

# configure with local LAPACK build
#ecbuild --build=debug -DBOOST_ROOT=$BOOST_ROOT -DBoost_NO_SYSTEM_PATHS=ON -DLAPACK_PATH=$LAPACK_PATH -DLAPACK_LIBRARIES=$LAPACK_LIBRARIES ${SRC}

# configure with NCAR LAPACK build
ecbuild --build=debug -DBOOST_ROOT=$BOOST_ROOT -DBoost_NO_SYSTEM_PATHS=ON ${SRC}

# Compile
 make -j1

exit 0
