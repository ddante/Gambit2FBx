MODULE Gambit_MeshType

  !===============
  TYPE Gambit_type

     INTEGER :: number_of_spatial_dimensions
     INTEGER :: number_of_domain_nodes
     INTEGER :: number_of_domain_elements
     INTEGER :: number_of_boundaries
     
     REAL(KIND=8),  DIMENSION(:,:), POINTER  ::  node_coordinates

     TYPE(domain_type), DIMENSION(:), POINTER :: domain
     INTEGER,           DIMENSION(:), POINTER :: number_of_element_nodes

     TYPE(boundary_type), DIMENSION(:), POINTER :: boundary
            
  END type GAMBIT_type
  !===================


  !===============
  TYPE domain_type

     INTEGER, DIMENSION(:), POINTER :: domain_element
     
  END TYPE domain_type
  !===================
  

  !=================
  TYPE boundary_type

     INTEGER :: boundary_log
     INTEGER :: number_of_bounday_elements

     INTEGER, DIMENSION(:), POINTER :: domain_element_of_boundary
     INTEGER, DIMENSION(:), POINTER :: element_type
     INTEGER, DIMENSION(:), POINTER :: face_of_domain_element_of_boundary
          
  END TYPE boundary_type
  !=====================

  
END MODULE Gambit_MeshType

