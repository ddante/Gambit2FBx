MODULE Gambit_IO

  USE Gambit_MeshType

  IMPLICIT NONE

CONTAINS
  
   !============================================
   SUBROUTINE read_Gambit_mesh(neu_file, Gambit)
   !============================================

     IMPLICIT NONE

     CHARACTER(len=64), INTENT(IN)  :: neu_file

     TYPE(Gambit_type), INTENT(OUT) :: Gambit
     !--------------------------------------------
     INTEGER :: UNIT, ierror
     !--------------------------------------------

     UNIT = 2

     OPEN(UNIT, FILE = TRIM(ADJUSTL(neu_file))//".neu", ACTION = 'READ', IOSTAT = ierror)

     IF (ierror /= 0) THEN        
        WRITE(*,*) 'ERROR: Impossible to open file ', TRIM(ADJUSTL(neu_file)),'.neu'
        WRITE(*,*) 'STOP!'
        STOP
     ENDIF

     WRITE(*,*) 'Reading headers...'
     CALL Read_headers_Gambit(UNIT, Gambit)
     WRITE(*,*) '...Done'
     
     WRITE(*,*) 'Reading node coordinates section...'
     CALL Read_coordinates_Gambit(UNIT, Gambit)
     WRITE(*,*) '...Done'

     WRITE(*,*) 'Reading elements section...'
     CALL Read_elemetns_Gmabit(UNIT, Gambit)
     WRITE(*,*) '...Done'

     WRITE(*,*) 'Reading groups section...'
     CALL Skip_section_Gambit(UNIT)
     WRITE(*,*) '...Done'

     WRITE(*,*) 'Reading boundary elements section...'
     CALL Read_boundary_Gambit(UNIT, Gambit)
     WRITE(*,*) '...Done'

     CLOSE(UNIT)
     
   END SUBROUTINE read_Gambit_mesh
   !==============================

   !===========================================
   SUBROUTINE Read_headers_Gambit(UNIT, Gambit)
   !===========================================

     IMPLICIT NONE

     INTEGER, INTENT(IN) :: UNIT
     
     TYPE(Gambit_type), INTENT(OUT) :: Gambit
     !---------------------------------------
     INTEGER :: dummy
     !---------------------------------------

     READ(UNIT, *); READ(UNIT,*); READ(UNIT,*)
     READ(UNIT, *); READ(UNIT,*); READ(UNIT,*)

     ! Read Npoint, Nelemt, N_bound, N_dim
     READ(UNIT, 100) Gambit%number_of_domain_nodes,       &
                     Gambit%number_of_domain_elements,    &
                     dummy,                               &
                     Gambit%number_of_boundaries,         &
                     Gambit%number_of_spatial_dimensions, &
                     dummy

100  FORMAT(6(1X,I9))

     READ(UNIT, *) ! ENDOFSECTION

   END SUBROUTINE Read_headers_Gambit
   !=================================

   !===============================================
   SUBROUTINE Read_coordinates_Gambit(UNIT, Gambit)
   !===============================================

     IMPLICIT NONE

     INTEGER, INTENT(IN) :: UNIT

     TYPE(Gambit_type), INTENT(OUT) :: Gambit
     !---------------------------------------
     INTEGER :: i, id
     !---------------------------------------
   
     READ(UNIT, *) ! NODAL COORDINATES

     ALLOCATE( Gambit%node_coordinates(Gambit%number_of_spatial_dimensions, &
                                  &    Gambit%number_of_domain_nodes) )

     DO i = 1, Gambit%number_of_domain_nodes
         READ(UNIT, *) id, Gambit%node_coordinates(:, i)
     ENDDO
     
     READ(UNIT, *) ! ENDOFSECTION

   END SUBROUTINE Read_coordinates_Gambit
   !=====================================

   !============================================
   SUBROUTINE Read_elemetns_Gmabit(UNIT, Gambit)
   !============================================

     IMPLICIT NONE
     
     INTEGER, INTENT(IN) :: UNIT

     TYPE(Gambit_type), INTENT(OUT) :: Gambit
     !---------------------------------------
     INTEGER :: i, id, ele_type
     !---------------------------------------

     READ(UNIT, *) ! ELEMENTS/CELLS

     ALLOCATE( Gambit%domain(Gambit%number_of_domain_elements) )
     ALLOCATE( Gambit%number_of_element_nodes(Gambit%number_of_domain_elements) )

     DO i = 1, Gambit%number_of_domain_elements

        READ(UNIT, 2000, ADVANCE='NO') id,       &
                                       ele_type, & 
                                       Gambit%number_of_element_nodes(i) 

        ALLOCATE( Gambit%domain(i)%domain_element(Gambit%number_of_element_nodes(i)) )
        
        READ(UNIT, *) Gambit%domain(i)%domain_element(:)
                                              
     ENDDO

2000 FORMAT(I8,1X,I2,1X,1X,I2)

     READ(UNIT, *) ! ENDOFSECTION

   END SUBROUTINE Read_elemetns_Gmabit
   !==================================

   !===================================
   SUBROUTINE Skip_section_Gambit(UNIT)
   !===================================
 
     IMPLICIT NONE

     INTEGER, INTENT(IN) :: UNIT
     !--------------------------
     CHARACTER(12)  :: ss
     !--------------------------

     READ(UNIT, *) ! ELEMENT GROUP 2.0.4

     !Skip all
     READ(UNIT,*) ss
     DO WHILE (ss .NE. 'ENDOFSECTION')
        READ(UNIT,*) ss
     ENDDO

   END SUBROUTINE Skip_section_Gambit
   !=================================

   !============================================
   SUBROUTINE Read_boundary_Gambit(UNIT, Gambit)
   !============================================

     IMPLICIT NONE
     
     INTEGER, INTENT(IN) :: UNIT

     TYPE(Gambit_type), INTENT(OUT) :: Gambit
     !---------------------------------------
     CHARACTER(12)  :: bound_name

     INTEGER :: i, j, itype, ele_type
     !---------------------------------------

     ALLOCATE ( Gambit%boundary(Gambit%number_of_boundaries) )

     DO i = 1, Gambit%number_of_boundaries

        READ(UNIT, *) ! BOUNDARY CONDITIONS 2.0.4
                
        READ(UNIT, 3000) bound_name, itype, Gambit%boundary(i)%number_of_bounday_elements

        IF (itype /= 1) THEN
           WRITE(*,*) 'Error: boundary definition on nodes not allowed'
           WRITE(*,*) 'STOP!'
           STOP           
        ENDIF        
        
        READ(bound_name, '(I32)') Gambit%boundary(i)%boundary_log

        ALLOCATE( Gambit%boundary(i)%domain_element_of_boundary &
              & ( Gambit%boundary(i)%number_of_bounday_elements) )

        ALLOCATE( Gambit%boundary(i)%face_of_domain_element_of_boundary &
              & ( Gambit%boundary(i)%number_of_bounday_elements) )

        ALLOCATE( Gambit%boundary(i)%element_type &
              & ( Gambit%boundary(i)%number_of_bounday_elements) )

        DO j = 1, Gambit%boundary(i)%number_of_bounday_elements

           READ(UNIT, 3001) Gambit%boundary(i)%domain_element_of_boundary(j),       &
                            Gambit%boundary(i)%element_type(j),                     &
                            Gambit%boundary(i)%face_of_domain_element_of_boundary(j)
        ENDDO
        
        READ(UNIT, *) ! ENDOFSECTION

     ENDDO
     
3000 FORMAT(A32,8I8)    
3001 FORMAT(I10, 2I5)

   END SUBROUTINE Read_boundary_Gambit
   !==================================

END MODULE Gambit_IO
