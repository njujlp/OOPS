list( APPEND oops_fv3gfs_src_files
ErrorCovariance.h
Fields.cc
Fields.h
Fortran.h
Geometry.cc
Geometry.h
Increment.cc
Increment.h
RunFV3.cc
RunFV3.h
State.cc
State.h
Traits.h
Variables.cc
Variables.h
fv3gfs_fields.F90
fv3gfs_fields_interface.F90
fv3gfs_geom_mod.F90
fv3gfs_setup.f90
fv3gfs_vars_mod.F90
)

set ( FMS_LIBRARIES $ENV{FMS_LIBRARIES} )
set ( FMS_INCLUDES $ENV{FMS_INCLUDES} )

ecbuild_add_library( TARGET   fv3gfs
                     DEFINITIONS ${cfdealloc}
                     SOURCES  ${oops_fv3gfs_src_files}
                     LIBS     oops ${LAPACK_LIBRARIES} ${NETCDF_LIBRARIES} ${FMS_LIBRARIES}
                     INCLUDES ${FMS_INCLUDES}
                     INSTALL_HEADERS LISTED
                     LINKER_LANGUAGE ${OOPS_LINKER_LANGUAGE}
                    )

