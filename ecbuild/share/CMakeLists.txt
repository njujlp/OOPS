# ecbuild_add_resources( TARGET ecbuild_share 
#	SOURCE_PACK ecmwf_license_header.txt
#	DONT_PACK_DIRS ecbuild )

install( DIRECTORY ecbuild/toolchains DESTINATION ${INSTALL_DATA_DIR} )
