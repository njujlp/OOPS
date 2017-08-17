list( APPEND oops_wrf_src_files
Fields.cc
Fields.h
Fortran.h
Geometry.cc
Geometry.h
Increment.cc
Increment.h
random_f.h
random_f.cc
random_vectors_mod.F90
State.cc
State.h
Traits.h
Variables.cc
Variables.h
wrf_constants.f90
wrf_fields.F90
wrf_fields_interface.F90
wrf_geom_mod.F90
wrf_invent_state.f90
wrf_vars_mod.F90
tools_nc.f90
)

if ( CMAKE_Fortran_COMPILER_ID MATCHES "Cray" )
  set( cfdealloc C_F_NO_DEALLOC )
endif()

ecbuild_add_library( TARGET   wrf
                     DEFINITIONS ${cfdealloc}
                     SOURCES  ${oops_wrf_src_files}
                     LIBS     oops ${LAPACK_LIBRARIES} ${NETCDF_LIBRARIES}
                     INSTALL_HEADERS LISTED
                     LINKER_LANGUAGE ${OOPS_LINKER_LANGUAGE}
                    )

