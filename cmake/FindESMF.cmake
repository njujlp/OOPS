# (C) Copyright 1996-2017 ECMWF.
# 
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0. 
# In applying this licence, ECMWF does not waive the privileges and immunities 
# granted to it by virtue of its status as an intergovernmental organisation nor
# does it submit to any jurisdiction.

# - Try to find ESMF
# Once done this will define
#  ESMF_FOUND - System has ESMF
#  ESMF_INCLUDE_DIRS - The ESMF include directories
#  ESMF_LIBRARIES - The libraries needed to use ESMF

find_path( ESMF_INCLUDE_DIR esmf.mod
           PATHS ${ESMF_PATH} ENV ESMF_PATH
                 ${ESMF_DIR}  ENV ESMF_DIR
           PATH_SUFFIXES mod/modO )

find_library( ESMF_LIBRARY NAMES ESMF esmf
              PATHS ${ESMF_PATH}/lib/libO ENV ESMF_PATH
                    ${ESMF_DIR}  ENV ESMF_DIR
              PATH_SUFFIXES )

set( ESMF_LIBRARIES    ${ESMF_LIBRARY} )
set( ESMF_INCLUDE_DIRS ${ESMF_INCLUDE_DIR} )

include(FindPackageHandleStandardArgs)

# handle the QUIETLY and REQUIRED arguments and set ESMF_FOUND to TRUE
# if all listed variables are TRUE
find_package_handle_standard_args( ESMF DEFAULT_MSG
                                   ESMF_LIBRARY ESMF_INCLUDE_DIR )

mark_as_advanced( ESMF_INCLUDE_DIR ESMF_LIBRARY )
