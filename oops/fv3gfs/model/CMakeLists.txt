list( APPEND oops_fv3gfs_src_files
ErrorCovariance.h
Fields.cc
Fields.h
Fortran.h
Geometry.cc
Geometry.h
Increment.cc
Increment.h
State.cc
State.h
Traits.h
Variables.cc
Variables.h
fv3gfs_fields.F90
fv3gfs_fields_interface.F90
fv3gfs_geom_mod.F90
fv3gfs_vars_mod.F90
)

ecbuild_add_library( TARGET   fv3gfs
                     DEFINITIONS ${cfdealloc}
                     SOURCES  ${oops_fv3gfs_src_files}
                     LIBS     oops ${LAPACK_LIBRARIES}
                     INSTALL_HEADERS LISTED
                     LINKER_LANGUAGE ${OOPS_LINKER_LANGUAGE}
                    )

