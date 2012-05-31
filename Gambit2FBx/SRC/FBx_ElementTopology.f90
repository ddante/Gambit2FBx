MODULE FBx_ElementTopology  

  IMPLICIT NONE
  
CONTAINS
    

!!$  !===================================================================
!!$  FUNCTION FBx_number_face_nodes(Ndim, ele_Ndof, face) RESULT(N_nodes)
!!$  !===================================================================
!!$
!!$    IMPLICIT NONE
!!$
!!$    INTEGER, INTENT(IN) :: Ndim
!!$    INTEGER, INTENT(IN) :: ele_Ndof
!!$    INTEGER, INTENT(IN) :: face
!!$
!!$    INTEGER :: N_nodes
!!$    !--------------------------------
!!$
!!$    SELECT CASE(Ndim)
!!$
!!$    CASE(2) ! 2D
!!$    !-------------------------------------------------------------
!!$
!!$       SELECT CASE(ele_Ndof)
!!$
!!$       CASE(3) ! Triangle
!!$
!!$          N_nodes = 2
!!$
!!$       CASE(4) ! Quadrangle
!!$
!!$          N_nodes = 2          
!!$
!!$        CASE DEFAULT  
!!$
!!$            WRITE(*,*) 'Element type of unknown type'
!!$            WRITE(*,*) 'STOP!'
!!$            STOP   
!!$
!!$       END SELECT
!!$  
!!$
!!$    CASE(3) ! 3D
!!$    !-------------------------------------------------------------
!!$
!!$       SELECT CASE(ele_Ndof)
!!$
!!$       CASE(4) ! Tetrahedron
!!$
!!$          N_nodes = 3
!!$       
!!$       !CASE(5)  Pyramid
!!$
!!$       !CASE(6)  Prism
!!$
!!$       CASE(8) ! Hexahedron
!!$
!!$          N_nodes = 4
!!$
!!$       CASE DEFAULT  
!!$
!!$            WRITE(*,*) 'Element type of unknown type'
!!$            WRITE(*,*) 'STOP!'
!!$            STOP 
!!$
!!$       END SELECT       
!!$       
!!$    END SELECT
!!$    
!!$  END FUNCTION FBx_number_face_nodes
!!$  !=================================

  !===========================================================
  FUNCTION FBx_face_of_element(G_ele_type, face) RESULT(nodes)
  !===========================================================

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: G_ele_type
    INTEGER, INTENT(IN) :: face

    INTEGER, DIMENSION(:), POINTER :: nodes
    !--------------------------------------

    SELECT CASE(G_ele_type)

    CASE(2) ! Quadrangle

       ALLOCATE(nodes(2))

       SELECT CASE(face)
       CASE(1); nodes = (/ 1, 2 /)
       CASE(2); nodes = (/ 2, 3 /)
       CASE(3); nodes = (/ 3, 4 /)
       CASE(4); nodes = (/ 4, 1 /)
       END SELECT

    CASE(3)  ! Triangle

       ALLOCATE(nodes(2))

       SELECT CASE(face)             
       CASE(1); nodes = (/ 2, 3 /)
       CASE(2); nodes = (/ 3, 1 /)
       CASE(3); nodes = (/ 1, 2 /)
       END SELECT

    CASE(4)  !Hexahedron

       ALLOCATE(nodes(4))

       SELECT CASE(face)
       CASE(1); nodes = (/ 1, 4, 3, 2 /)
       CASE(2); nodes = (/ 5, 6, 7, 8 /)
       CASE(3); nodes = (/ 1, 2, 6, 5 /)
       CASE(4); nodes = (/ 2, 3, 7, 6 /)
       CASE(5); nodes = (/ 3, 4, 8, 7 /)
       CASE(6); nodes = (/ 1, 5, 8, 4 /)
       END SELECT

   !CASE(5) ! Prsim

    CASE(6) ! Tetrahedron

       ALLOCATE(nodes(3))
!?
       SELECT CASE(face)
       CASE(1); nodes = (/ 2, 3, 4 /)
       CASE(2); nodes = (/ 1, 4, 3 /)
       CASE(3); nodes = (/ 4, 1, 2 /)
       CASE(4); nodes = (/ 2, 1, 3 /)
       END SELECT
!?
    CASE(7) !Pyramid

       SELECT CASE(face)

       CASE(1)
          ALLOCATE(nodes(3))
          nodes = (/ 4, 5, 3 /)
       CASE(2)
          ALLOCATE(nodes(3))
          nodes = (/ 1, 5, 4 /) 
       CASE(3)
          ALLOCATE(nodes(3))
          nodes = (/ 1, 2, 5 /) 
       CASE(4)
          ALLOCATE(nodes(3))
          nodes = (/ 2, 3, 5 /) 
       CASE(5)
          ALLOCATE(nodes(4))
          nodes = (/ 1, 4, 3, 2 /) 
       END SELECT

    CASE DEFAULT  

       WRITE(*,*) 'Element type of unknown type'
       WRITE(*,*) ' in: FBx_face_of_element'
       WRITE(*,*) 'STOP!'
       STOP   

    END SELECT
    
  END FUNCTION FBx_face_of_element
  !===============================
  

END MODULE FBx_ElementTopology
