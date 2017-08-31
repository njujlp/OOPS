#!/bin/ksh
# Example of script to run a Dirac-NICAS test with the QG model on Cheyenne
# The script requires 2 nodes, and use 4 MPI tasks on each one, with 9 OpenMP threads (36 cores per node on Cheyenne) 
#set -ex
#PBS -q regular
#PBS -l walltime=00:30:00
#PBS -l select=2:ncpus=36:mpiprocs=4
#PBS -j oe
#PBS -m n
#PBS -A NSAP0003

# Load modules
source /glade/u/apps/ch/opt/Lmod/7.3.14/lmod/7.3.14/init/ksh
module purge
module load gnu netcdf openmpi

# Go to test directory
OOPSDIR=/glade/u/home/menetrie/OOPS
cd ${OOPSDIR}/build/oops/qg/test

# Specify number of OpenMP threads
export OMP_NUM_THREADS=9

# Run!
mpirun ${OOPSDIR}/build/bin/qg_dirac.x testinput/dirac.nicas.json
