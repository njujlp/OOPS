list( APPEND oops_wrf_src_files
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
calc_pv.f90
laplacian_2d.f90
pv_operator.f90
random_vectors_mod.F90
random_f.h
random_f.cc
wrf_constants.f90
wrf_fields.F90
wrf_fields_interface.F90
wrf_geom_mod.F90
wrf_invent_state.f90
wrf_vars_mod.F90
)

if ( CMAKE_Fortran_COMPILER_ID MATCHES "Cray" )
  set( cfdealloc C_F_NO_DEALLOC )
endif()

ecbuild_add_library( TARGET   wrf
                     DEFINITIONS ${cfdealloc}
                     SOURCES  ${oops_wrf_src_files}
                     LIBS     oops ${LAPACK_LIBRARIES}
                     INSTALL_HEADERS LISTED
                     LINKER_LANGUAGE ${OOPS_LINKER_LANGUAGE}
                    )

