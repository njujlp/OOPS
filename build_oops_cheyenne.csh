#!/bin/csh -f

#git clone https://github.com/JCSDA/OOPS
#cd OOPS

# enable modules
source /glade/u/apps/ch/opt/lmod/7.2.1/lmod/lmod/init/tcsh
# reset all modules
module purge
# reset the library path
unsetenv LD_LIBRARY_PATH

# Load modules
module load gnu mpich/3.2 cmake/3.7.2
module list

# Define lapack path
setenv LAPACK_PATH /glade/p/ral/nsap/jcsda/code/lapack-3.7.0
setenv LAPACK_DIR  $LAPACK_PATH
setenv LAPACK_LIBRARIES "$LAPACK_PATH/lib64/liblapack.a;$LAPACK_PATH/lib64/libblas.a"

# Need eigen3 library
setenv EIGEN3_INCLUDE_DIR /glade/p/ral/nsap/jcsda/code/eigen/build

# Need boost library
setenv BOOST_ROOT /glade/p/ral/nsap/jcsda/code/boost_1_64_0

setenv SRC $PWD
setenv BUILD ${SRC}/build

rm -rf ${BUILD}; mkdir ${BUILD}; cd ${BUILD}


set path = (${path} ${SRC}/ecbuild/bin $EIGEN3_INCLUDE_DIR) # ~gvernier/boost/build/bin)

# configure
ecbuild --build=debug -DBOOST_ROOT=$BOOST_ROOT -DBoost_NO_SYSTEM_PATHS=ON -DLAPACK_PATH=$LAPACK_PATH -DLAPACK_LIBRARIES=$LAPACK_LIBRARIES ${SRC}

# Compile
make VERBOSE=1 -j4

exit 0
