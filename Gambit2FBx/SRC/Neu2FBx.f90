Program Neu2FBx

  USE Gambit_MeshType
  USE FBx_MeshType
  USE Gambit_IO
  USE Gambit_converter
  USE FBx_IO

  IMPLICIT NONE
 
  CHARACTER(len=64) :: neu_file
  
  TYPE(Gambit_type) :: data_gambit
  TYPE(FBx_type)    :: data_fbx

  INTEGER :: UNIT
  !------------------------------------------

  IF (command_argument_count() < 1) THEN
       WRITE(*,*) 'ERROR: No mesh file'
       WRITE(*,*) 'STOP!'           
       STOP
   ENDIF

   CALL get_command_argument(1, neu_file)

   CALL read_Gambit_mesh(neu_file, data_gambit)

   CALL GambitMesh_conver_to_FBxMesh(data_gambit, data_fbx)
   
   WRITE(*,*) 'Writing converted file...'
   CALL write_FBx_mesh(neu_file, data_fbx)
   WRITE(*,*) '...Done'

END Program Neu2Fbx
