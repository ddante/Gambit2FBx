MODULE Gambit_converter

  USE Gambit_MeshType
  USE FBx_MeshType
  USE Gambit_IO
  USE FBx_ElementTopology

  IMPLICIT NONE
  
  INTEGER, DIMENSION(3), PARAMETER  ::  GAMBIT_TRIANGLE_NODES &
                                     =   (/ 1, 2, 3 /) 
  INTEGER, DIMENSION(4), PARAMETER  ::  GAMBIT_QUADRANGLE_NODES &
                                     =   (/ 1, 2, 3, 4 /) 
  INTEGER, DIMENSION(4), PARAMETER  ::  GAMBIT_TETRAHEDRON_NODES &
                                     =   (/ 1, 4, 2, 3 /)
  INTEGER, DIMENSION(8), PARAMETER  ::  GAMBIT_HEXAHEDRON_NODES &
                                     =   (/ 1, 2, 4, 3, 5, 6, 8, 7 /)
  INTEGER, DIMENSION(5), PARAMETER  ::  GAMBIT_PYRAMID_NODES &
                                     =   (/ 1, 2, 4, 3, 5 /)

  INTEGER, DIMENSION(3), PARAMETER  ::  GAMBIT_TRIANGLE_FACES &
                                     =   (/ 3, 1, 2 /)
  INTEGER, DIMENSION(4), PARAMETER  ::  GAMBIT_QUADRANGLE_FACES &
                                     =   (/ 1, 2, 3, 4 /)
  INTEGER, DIMENSION(4), PARAMETER  ::  GAMBIT_TETRAHEDRON_FACES &
                                     =   (/ 2, 4, 1, 3 /)  
  INTEGER, DIMENSION(6), PARAMETER  ::  GAMBIT_HEXAHEDRON_FACES &
                                     =   (/ 3, 4, 5, 6, 1, 2 /)  
  INTEGER, DIMENSION(5), PARAMETER  ::  GAMBIT_PYRAMID_FACES &
                                     =   (/ 5, 3, 4, 1, 2 /)  

CONTAINS

  !===================================================
  SUBROUTINE GambitMesh_conver_to_FBxMesh(Gambit, FBx)
  !===================================================
    
    IMPLICIT NONE

    TYPE(Gambit_type), INTENT(IN) :: Gambit

    TYPE(FBx_type), INTENT(OUT) :: FBx
    !---------------------------------------------------
    INTEGER :: FBx_face, N_nodes, boundary_element
    INTEGER, DIMENSION(:), ALLOCATABLE :: nodes_index
    INTEGER, DIMENSION(:), ALLOCATABLE :: element_nodes
    INTEGER :: i, j, b
    !----------------------------------------------------

    FBx%number_of_spatial_dimensions = Gambit%number_of_spatial_dimensions
    FBx%number_of_domain_nodes       = Gambit%number_of_domain_nodes
    FBx%number_of_domain_elements    = Gambit%number_of_domain_elements

    ALLOCATE( FBx%node_coordinates(FBx%number_of_spatial_dimensions, &
                                 & FBx%number_of_domain_nodes) )

    FBx%node_coordinates = Gambit%node_coordinates

    ALLOCATE( FBx%domain(FBx%number_of_domain_elements) )
    ALLOCATE( FBx%number_of_element_nodes(FBx%number_of_domain_elements) )

    ! Domain elements ----
    DO i = 1, FBx%number_of_domain_elements

       FBx%number_of_element_nodes(i) = Gambit%number_of_element_nodes(i)

       ALLOCATE( FBx%domain(i)%domain_element(FBx%number_of_element_nodes(i)) )

       ! Nodes local numeration Gambit --> FBx 
       FBx%domain(i)%domain_element = Gambit2FBx_Nodes(FBx%number_of_spatial_dimensions, &
                                                     & FBx%number_of_element_nodes(i),   &
                                                     & Gambit%domain(i)%domain_element(:))       
    ENDDO

    ! Boundary elements ---
    FBx%number_of_bounday_elements = SUM(Gambit%boundary(:)%number_of_bounday_elements)

    ALLOCATE( FBx%fb_boundary(FBx%number_of_bounday_elements) )

    j = 0
    DO b = 1, Gambit%number_of_boundaries

       DO i = 1, Gambit%boundary(b)%number_of_bounday_elements

          j = j+1

          ! Boundary condition flag
          FBx%fb_boundary(j)%boundary_log = Gambit%boundary(b)%boundary_log

          ! Faces local numeration Gambit --> FBx 
          FBx_face = Gambit2FBx_Face(Gambit%boundary(b)%element_type(i), &
                                   & Gambit%boundary(b)%face_of_domain_element_of_boundary(i))
   
          N_nodes = number_face_nodes(Gambit%boundary(b)%element_type(i), &
                                      Gambit%boundary(b)%face_of_domain_element_of_boundary(i))

          ALLOCATE( nodes_index(N_nodes) )
                   
          nodes_index = FBx_face_of_element(Gambit%boundary(b)%element_type(i), &
                                            FBx_face)

          ! Domain element on the boundary
          boundary_element = Gambit%boundary(b)%domain_element_of_boundary(i)

          ALLOCATE( element_nodes(FBx%number_of_element_nodes(boundary_element)) )

          element_nodes = FBx%domain(boundary_element)%domain_element

          ALLOCATE( FBx%fb_boundary(j)%domain_nodes_of_boundary(N_nodes) )
          
          ! Global boundary nodes of the element          
          FBx%fb_boundary(j)%domain_nodes_of_boundary = element_nodes(nodes_index)

          DEALLOCATE( nodes_index, element_nodes )
            
       ENDDO

    ENDDO

  END SUBROUTINE GambitMesh_conver_to_FBxMesh
  !===========================================

  !========================================================================
  FUNCTION Gambit2FBx_Nodes(Ndim, ele_Ndof, Gambit_nodes) RESULT(FBx_nodes)
  !========================================================================

    IMPLICIT NONE

    INTEGER,               INTENT(IN) :: Ndim
    INTEGER,               INTENT(IN) :: ele_Ndof
    INTEGER, DIMENSION(:), INTENT(IN) :: Gambit_nodes

    INTEGER, DIMENSION(SIZE(Gambit_nodes)) :: FBx_nodes
    !--------------------------------------------------

    SELECT CASE(Ndim)

    CASE(2) ! 2D
    !-------------------------------------------------------------

       SELECT CASE(ele_Ndof)

       CASE(3); FBx_nodes = Gambit_nodes(GAMBIT_TRIANGLE_NODES)  

       CASE(4); FBx_nodes = Gambit_nodes(GAMBIT_QUADRANGLE_NODES)  

       CASE DEFAULT  

          WRITE(*,*) 'Element type unknown type'
          WRITE(*,*) ' in:  Gambit2FBx_Nodes 2D'
          WRITE(*,*) 'STOP!'
          STOP   

       END SELECT
  

    CASE(3) ! 3D
    !-------------------------------------------------------------

       SELECT CASE(ele_Ndof)

       CASE(4); FBx_nodes = Gambit_nodes(GAMBIT_TETRAHEDRON_NODES)

       CASE(5); FBx_nodes = Gambit_nodes(GAMBIT_PYRAMID_NODES)

       !CASE(6)  Prism

       CASE(8); FBx_nodes = Gambit_nodes(GAMBIT_HEXAHEDRON_NODES)

       CASE DEFAULT  

            WRITE(*,*) 'Element type of unknown type'
            WRITE(*,*) ' in:  Gambit2FBx_Nodes 3D'
            WRITE(*,*) 'STOP!'
            STOP 

       END SELECT       
       
    END SELECT
    
  END FUNCTION Gambit2FBx_Nodes
  !============================
  
  !=======================================================================
  FUNCTION  Gambit2FBx_Face(Gambit_ele_type, Gambit_face) RESULT(FBx_face)
  !=======================================================================

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: Gambit_ele_type
    INTEGER, INTENT(IN) :: Gambit_face

    INTEGER :: FBx_face
    !----------------------------------------

    SELECT CASE(Gambit_ele_type)
       
    !CASE(1) Segment

    CASE(2); FBx_face = GAMBIT_QUADRANGLE_FACES(Gambit_face)

    CASE(3); FBx_face = GAMBIT_TRIANGLE_FACES(Gambit_face)

    CASE(4); FBx_face = GAMBIT_HEXAHEDRON_FACES(Gambit_face)

    !CASE(5) Prism

    CASE(6); FBx_face = GAMBIT_TETRAHEDRON_FACES(Gambit_face)

    CASE(7); FBx_face = GAMBIT_PYRAMID_FACES(Gambit_face)

    CASE DEFAULT

       WRITE(*,*) 'Element type of unknown type'
       WRITE(*,*) ' in: Gambit2FBx_Face'
       WRITE(*,*) 'STOP!'
       STOP
       
    END SELECT    

  END FUNCTION Gambit2FBx_Face
  !===========================

  !=========================================================
  FUNCTION number_face_nodes(G_ele_type, id_face) RESULT(Np)
  !=========================================================

    IMPLICIT NONE

    INTEGER, INTENT(IN) :: G_ele_type
    INTEGER, INTENT(IN) :: id_face

    INTEGER :: Np
    !-------------------------------------

    SELECT CASE(G_ele_type)

    CASE(2) ! Quadrilateral

       Np = 2

    CASE(3) ! Triangle

       Np = 2

    CASE(4) ! Hexahedron 

       Np = 4

   !CASE(5) ! Prism

    CASE(6) ! Tetrahedron

       Np = 3

    CASE(7) ! Pyramid

       IF( id_face == 1 ) THEN

          Np = 4

       ELSEIF( id_face == 2 .OR. &
               id_face == 3 .OR. &
               id_face == 4 .OR. &
               id_face == 5 ) THEN

          Np = 3

       ELSE

          WRITE(*,*) 'ERROR: unknow Pyramid face'
          STOP

       ENDIF

    CASE DEFAULT

       WRITE(*,*) 'Element type of unknown type '
       WRITE(*,*) ' in: number_face_node'
       WRITE(*,*) 'STOP!'
       STOP 

    END SELECT

  END FUNCTION number_face_nodes
  !=============================

END MODULE Gambit_converter
