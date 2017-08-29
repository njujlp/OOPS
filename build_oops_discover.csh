#!/bin/sh 

#set -x

reset

source /usr/share/modules/init/sh
module purge
module load other/cmake-3.8.2

module load comp/intel-15.0.3.187
module load mpi/impi-5.1.3.258
module load lib/mkl-15.0.3.187

module use -a /discover/nobackup/rmahajan/EXT/modulefiles
module load boost eigen

export BUILD=`pwd`/oops_build
export SRC=`pwd`/oops-bundle-0.1.0-Source
export PATH=$PATH:${SRC}/ecbuild/bin

export BOOST_ROOT=/discover/nobackup/rmahajan/EXT/boost/1.64.0/include/
export EIGEN3_PATH=/discover/nobackup/rmahajan/EXT/eigen/3.3.4/include/

echo $BUILD
echo $SRC
rm -rf ${BUILD}
mkdir -p ${BUILD}
cd ${BUILD}

pwd
which ecbuild
ecbuild --build=release ${SRC}
#make -j4
make VERBOSE=YES -j4

cd ${BUILD}/oops
ctest
