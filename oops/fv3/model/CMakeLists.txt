list( APPEND oops_fv3_src_files
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
fv3_misc_mod.f90
random_vectors_mod.F90
random_f.h
random_f.cc
fv3_fields.F90
fv3_fields_interface.F90
fv3_geom_mod.F90
fv3_vars_mod.F90
)

ecbuild_add_library( TARGET   fv3
                     DEFINITIONS ${cfdealloc}
                     SOURCES  ${oops_fv3_src_files}
                     LIBS     oops ${LAPACK_LIBRARIES} ${NETCDF_LIBRARIES}
                     INSTALL_HEADERS LISTED
                     LINKER_LANGUAGE ${OOPS_LINKER_LANGUAGE}
                    )

