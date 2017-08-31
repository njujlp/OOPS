list( APPEND fv3gfs_test_input
  testinput/interfaces.json
  testinput/test.json
  testinput/input.nml
)

# Create Data directory for test input and symlink all files
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/testinput)
foreach(FILENAME ${fv3gfs_test_input})
    execute_process( COMMAND ${CMAKE_COMMAND} -E create_symlink
           ${CMAKE_CURRENT_SOURCE_DIR}/${FILENAME}
           ${CMAKE_CURRENT_BINARY_DIR}/${FILENAME} )
endforeach(FILENAME)

ecbuild_add_resources( TARGET   fv3gfs_test_scripts
                       SOURCES_PACK
                       ${fv3gfs_test_input}
                     )

file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/Data)

#####################################################################

ecbuild_add_test( TARGET  test_fv3gfs_geometry
                  BOOST
                  SOURCES executables/TestGeometry.cc
                  ARGS    "testinput/interfaces.json"
                  LIBS    fv3gfs )

ecbuild_add_test( TARGET  test_fv3gfs_state
                  BOOST
                  SOURCES executables/TestState.cc
                  ARGS    "testinput/interfaces.json"
                  LIBS    fv3gfs )

ecbuild_add_test( TARGET  test_fv3gfs_increment
                  BOOST
                  SOURCES executables/TestIncrement.cc
                  ARGS    "testinput/interfaces.json"
                  LIBS    fv3gfs )

