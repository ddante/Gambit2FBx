cmake_minimum_required (VERSION 2.6)
project (Gambit2FBx Fortran)

message("PROJECT_BINARY_DIR = ${PROJECT_BINARY_DIR}")
#SET(EXECUTABLE_OUTPUT_PATH ../build)

SET(SOURCES SRC/Gambit_MeshType.f90
            SRC/FBx_MeshType.f90
            SRC/Gambit_IO.f90
	    SRC/Gambit_converter.f90
            SRC/FBx_ElementTopology.f90
	    SRC/FBx_IO.f90
	    SRC/Neu2FBx.f90)            
# Debugging options
SET(FORTRAN_PREPROCESS_FLAG "${FORTRAN_PREPROCESS_FLAG} -C")
SET(FORTRAN_PREPROCESS_FLAG "${FORTRAN_PREPROCESS_FLAG} -check uninit")
SET(FORTRAN_PREPROCESS_FLAG "${FORTRAN_PREPROCESS_FLAG} -ftrapuv")
SET(FORTRAN_PREPROCESS_FLAG "${FORTRAN_PREPROCESS_FLAG} -fpe0")
SET(FORTRAN_PREPROCESS_FLAG "${FORTRAN_PREPROCESS_FLAG} -check noarg_temp_created")

add_executable(Gambit2FBx.exe ${SOURCES})
SET_TARGET_PROPERTIES(Gambit2FBx.exe PROPERTIES COMPILE_FLAGS ${FORTRAN_PREPROCESS_FLAG} )