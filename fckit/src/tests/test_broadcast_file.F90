#include "fckit/fctest.h"

TESTSUITE( test_broadcast_file )

TESTSUITE_INIT
  use fckit_module
  call fckit_main%init()
  call fckit_log%set_fortran_unit(0)

  ! Write a json file
  OPEN (UNIT=199 , FILE="fctest_broadcast.json", STATUS='REPLACE')
  write(199,'(A)') '{"location":{"city":"Reading","company":"ECMWF","street":"Shinfield Road"},'//&
      &          '"records":[{"age":42,"name":"Anne"},{"age":36,"name":"Bob"}]}'
  CLOSE(199)

END_TESTSUITE_INIT

TESTSUITE_FINALIZE
  use fckit_module
  call fckit_main%final()
END_TESTSUITE_FINALIZE

TEST( broadcast_file_inline )
  use fckit_module
  implicit none
  type(fckit_mpi_comm) :: comm
  type(fckit_Configuration) :: config
  comm = fckit_mpi_comm()
  config = fckit_YAMLConfiguration( comm%broadcast_file("fctest_broadcast.json",0) )
  FCTEST_CHECK( config%has("location") )
END_TEST

TEST( broadcast_file_arg )
  use fckit_module
  implicit none
  type(fckit_mpi_comm) :: comm
  type(fckit_Configuration) :: config
  type(fckit_buffer) :: buffer
  comm = fckit_mpi_comm()
  buffer = comm%broadcast_file("fctest_broadcast.json",0)
  config = fckit_YAMLConfiguration( buffer )
  FCTEST_CHECK( config%has("location") )
  FCTEST_CHECK_EQUAL( buffer%owners(), 1 )
  call buffer%final()
END_TEST


END_TESTSUITE
