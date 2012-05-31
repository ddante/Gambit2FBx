MODULE FBx_IO

  USE FBx_MeshType

  IMPLICIT NONE

CONTAINS

  !=======================================
  SUBROUTINE write_FBx_mesh(neu_file, FBx)
  !=======================================
    IMPLICIT NONE

    CHARACTER(len=64), INTENT(IN) :: neu_file
    TYPE(FBx_type),    INTENT(IN) :: FBx
    !-----------------------------------------

   ! SELECT CASE(FBx%number_of_spatial_dimensions)
   ! CASE(2)

   !    CALL write_mesh_file(neu_file, FBx)

   ! CASE(3)

       !CALL write_geom_file(neu_file, FBx)
       CALL write_gmsh_file(neu_file, FBx)

   ! END SELECT    

  END SUBROUTINE write_FBx_mesh
  !============================

  !========================================
  SUBROUTINE write_mesh_file(neu_file, FBx)
  !========================================

    IMPLICIT NONE

    CHARACTER(len=64), INTENT(IN) :: neu_file
    TYPE(FBx_type),    INTENT(IN) :: FBx
    !-----------------------------------------
    INTEGER :: Num_triangles, Num_quadrangles
    INTEGER :: i, j, UNIT, ierror
    !-----------------------------------------

    UNIT = 3

    OPEN(UNIT, FILE = TRIM(ADJUSTL(neu_file))//".mesh", ACTION = 'WRITE', IOSTAT = ierror)

    IF (ierror /= 0) THEN        
        WRITE(*,*) 'ERROR: Impossible to open file ', TRIM(ADJUSTL(neu_file)),'.mesh'
        WRITE(*,*) 'STOP!'
        STOP
     ENDIF

     WRITE(UNIT, 1100)
     WRITE(UNIT, 1200) ! Dimension
     WRITE(UNIT, 1201) FBx%number_of_spatial_dimensions

     WRITE(UNIT, 2000) ! Vertices
     WRITE(UNIT, 1201) FBx%number_of_domain_nodes
     DO i = 1, FBx%number_of_domain_nodes
        WRITE(UNIT, 2001) FBx%node_coordinates(:, i), 0
     ENDDO

     WRITE(UNIT, 3000) ! Edges
     WRITE(UNIT, 1201) FBx%number_of_bounday_elements
     DO i = 1, FBx%number_of_bounday_elements
        DO j = 1, SIZE(FBx%fb_boundary(i)%domain_nodes_of_boundary) 
           WRITE(UNIT, FMT=1201, ADVANCE='NO') FBx%fb_boundary(i)%domain_nodes_of_boundary(j)
        ENDDO
        WRITE(UNIT, FMT=1201, ADVANCE='NO') FBx%fb_boundary(i)%boundary_log
        WRITE(UNIT,*)
     ENDDO

     Num_triangles   = COUNT(FBx%number_of_element_nodes == 3)
     Num_quadrangles = COUNT(FBx%number_of_element_nodes == 4)

     IF (Num_triangles > 0) THEN
        
         WRITE(UNIT, 4000) ! Tiangles
         WRITE(UNIT, 1201) Num_triangles
         DO i = 1, FBx%number_of_domain_elements
            IF( FBx%number_of_element_nodes(i) == 3) THEN               
               DO j = 1, FBx%number_of_element_nodes(i)                  
                  WRITE(UNIT, FMT=1201, ADVANCE='NO') FBx%domain(i)%domain_element(j) 
               ENDDO
               WRITE(UNIT, FMT=1201, ADVANCE='NO') 0
               WRITE(UNIT,*)
            ENDIF   
         ENDDO
         
      ENDIF
           
      IF (Num_quadrangles > 0) THEN
                 
         WRITE(UNIT, 5000) ! Quadrangles
         WRITE(UNIT, 1201) Num_quadrangles
         DO i = 1, FBx%number_of_domain_elements
            IF( FBx%number_of_element_nodes(i) == 4) THEN               
               DO j = 1, FBx%number_of_element_nodes(i)                  
                  WRITE(UNIT, FMT=1201, ADVANCE='NO') FBx%domain(i)%domain_element(j)
               ENDDO
               WRITE(UNIT, FMT=1201, ADVANCE='NO') 0
               WRITE(UNIT,*)
            ENDIF   
         ENDDO
         
      ENDIF

      WRITE(UNIT, 6000)
      
      CLOSE(UNIT)


1100 FORMAT('MeshVersionFormatted 0')
1200 FORMAT('Dimension')
1201 FORMAT(I12)
2000 FORMAT('Vertices')
2001 FORMAT(2E18.9,I5)
3000 FORMAT('Edges')
4000 FORMAT('Triangles')
5000 FORMAT('Quadrangles')
6000 FORMAT('End') 

  END SUBROUTINE write_mesh_file
  !=============================

  !========================================
  SUBROUTINE write_geom_file(neu_file, FBx)
  !========================================

    IMPLICIT NONE

    CHARACTER(len=64), INTENT(IN) :: neu_file
    TYPE(FBx_type),    INTENT(IN) :: FBx
    !-----------------------------------------
    INTEGER :: i, j, n, UNIT, ierror
    !-----------------------------------------

    UNIT = 3

    OPEN(UNIT, FILE = TRIM(ADJUSTL(neu_file))//".geom", ACTION = 'WRITE', IOSTAT = ierror)

    IF (ierror /= 0) THEN        
        WRITE(*,*) 'ERROR: Impossible to open file ', TRIM(ADJUSTL(neu_file)),'.neu'
        WRITE(*,*) 'STOP!'
        STOP
     ENDIF

     WRITE(UNIT, 1000) FBx%number_of_domain_nodes,    &
                       FBx%number_of_domain_elements, &
                       FBx%number_of_bounday_elements

     DO i = 1, FBx%number_of_domain_nodes
        WRITE(UNIT, 1021) FBx%node_coordinates(:, i)
     ENDDO

     DO i = 1, FBx%number_of_domain_elements
        DO j = 1, FBx%number_of_element_nodes(i) 
           WRITE(UNIT, FMT=1030, ADVANCE='NO') FBx%domain(i)%domain_element(j)
        ENDDO
        WRITE(UNIT,*)
     ENDDO

     DO i = 1, FBx%number_of_bounday_elements
         WRITE(UNIT, 1030) FBx%fb_boundary(i)%boundary_log
     ENDDO
     
     DO i = 1, FBx%number_of_bounday_elements
        DO j = 1, SIZE(FBx%fb_boundary(i)%domain_nodes_of_boundary) 
           WRITE(UNIT, FMT=1030, ADVANCE='NO') FBx%fb_boundary(i)%domain_nodes_of_boundary(j)
        ENDDO
        WRITE(UNIT,*)
     ENDDO
     
1000 FORMAT(3I12)
1021 FORMAT(3E18.9)
1030 FORMAT(I12)

     CLOSE(UNIT)
     
  END SUBROUTINE write_geom_file
  !=============================

  !======================================== 
  SUBROUTINE write_gmsh_file(neu_file, FBx)
  !======================================== 

    IMPLICIT NONE

    CHARACTER(len=64), INTENT(IN) :: neu_file
    TYPE(FBx_type),    INTENT(IN) :: FBx
    !-----------------------------------------
    INTEGER :: i, j, n, ele_type, UNIT, ierror
    !-----------------------------------------
    
    UNIT = 4

    OPEN(UNIT, FILE = TRIM(ADJUSTL(neu_file))//".msh", ACTION = 'WRITE', IOSTAT = ierror)

    IF (ierror /= 0) THEN        
        WRITE(*,*) 'ERROR: Impossible to open file ', TRIM(ADJUSTL(neu_file)),'.msh'
        WRITE(*,*) 'STOP!'
        STOP
     ENDIF

     WRITE(UNIT, '("$MeshFormat")')
     WRITE(UNIT, '("2.2 0 8")')
     WRITE(UNIT, '("$EndMeshFormat")')

     WRITE(UNIT, '("$Nodes")')
     WRITE(UNIT, 1000) FBx%number_of_domain_nodes

     IF (FBx%number_of_spatial_dimensions == 2) THEN

        DO i = 1, FBx%number_of_domain_nodes
           WRITE(UNIT, 1020) i, FBx%node_coordinates(:, i), 0.0
        ENDDO

     ELSE

        DO i = 1, FBx%number_of_domain_nodes
           WRITE(UNIT, 1020) i, FBx%node_coordinates(:, i)
        ENDDO

     ENDIF

     WRITE(UNIT, '("$EndNodes")')

     WRITE(UNIT, '("$Elements")')

     WRITE(UNIT, 1000) FBx%number_of_domain_elements + &
                       FBx%number_of_bounday_elements

     DO i = 1, FBx%number_of_domain_elements

        ele_type = gmsh_element_type(FBx%number_of_spatial_dimensions, &
                                     FBx%number_of_element_nodes(i) )

        WRITE(UNIT, FMT=1030, ADVANCE='NO') i, ele_type

        DO j = 1, FBx%number_of_element_nodes(i)
           
           WRITE(UNIT, FMT=1040, ADVANCE='NO') FBx%domain(i)%domain_element(j)

        ENDDO

        WRITE(UNIT, *)

     ENDDO

     DO i = 1, FBx%number_of_bounday_elements

        n = i + FBx%number_of_domain_elements

        ele_type = gmsh_face_type(FBx%number_of_spatial_dimensions, &
                                  SIZE(FBx%fb_boundary(i)%domain_nodes_of_boundary))

        WRITE(UNIT, FMT=1050, ADVANCE='NO') n, ele_type, FBx%fb_boundary(i)%boundary_log, &
                                                         FBx%fb_boundary(i)%boundary_log*100

         DO j = 1, SIZE(FBx%fb_boundary(i)%domain_nodes_of_boundary) 

           WRITE(UNIT, FMT=1040, ADVANCE='NO') FBx%fb_boundary(i)%domain_nodes_of_boundary(j)

        ENDDO

        WRITE(UNIT, *)

     ENDDO
     
     WRITE(UNIT, '("$EndElements")')
          
     CLOSE(UNIT)

1000 FORMAT(I19)
1020 FORMAT(I9, 3E24.16)
1030 FORMAT(I9, I3, " 3 0 0 0 ")
1040 FORMAT(I12)
1050 FORMAT(I9, I3, " 3 ", I3, I5, " 0 ")

  END SUBROUTINE write_gmsh_file
  !=============================

  !==========================================================
  FUNCTION gmsh_element_type(N_dim, ele_Ndofs) RESULT(e_type)
  !==========================================================

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: N_dim
    INTEGER, INTENT(IN) :: ele_Ndofs    

    INTEGER :: e_type
    !--------------------------------------

    SELECT CASE(N_dim)

    CASE(2) ! 2D
    !-------------------------------------------------------------

       SELECT CASE(ele_Ndofs)

       CASE(3)

          e_type = 2   ! 3-node triangle

       CASE(4)

          e_type = 3   ! 4-node quadrangle

       CASE DEFAULT
           
          WRITE(*,*) 'Element type of unknown type'
          WRITE(*,*) ' in: gmsh_element_type 2D'
          WRITE(*,*) 'STOP!'
          STOP 

       END SELECT

    CASE(3) ! 3D
    !-------------------------------------------------------------

       SELECT CASE(ele_Ndofs)

       CASE(4)

          e_type = 4   ! 4-node tetrahedron

       CASE(8)

          e_type = 5   ! 8-node hexahedron

       CASE(5)
          
          e_type = 7   ! 5-node pyramid

       CASE DEFAULT
           
          WRITE(*,*) 'Element type of unknown type'
          WRITE(*,*) ' in: gmsh_element_type 3D'
          WRITE(*,*) 'STOP!'
          STOP 

       END SELECT

    END SELECT
    
  END FUNCTION gmsh_element_type
  !=============================

  !=======================================================
  FUNCTION gmsh_face_type(N_dim, ele_Ndofs) RESULT(e_type)
  !=======================================================

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: N_dim
    INTEGER, INTENT(IN) :: ele_Ndofs    

    INTEGER :: e_type
    !--------------------------------------

    SELECT CASE(N_dim)

    CASE(2) ! 2D
    !-------------------------------------------------------------

       e_type = 1

    CASE(3) ! 3D
    !-------------------------------------------------------------

       SELECT CASE(ele_Ndofs)

       CASE(3)

          e_type = 2   ! 3-node triangle

       CASE(4)

          e_type = 3   ! 4-node quadrangle

       CASE DEFAULT
           
          WRITE(*,*) 'Element type of unknown type'
          WRITE(*,*) ' in: gmsh_face_type 3D'
          WRITE(*,*) 'STOP!'
          STOP 

       END SELECT

    END SELECT
    
  END FUNCTION gmsh_face_type  
  !==========================

END MODULE FBx_IO
