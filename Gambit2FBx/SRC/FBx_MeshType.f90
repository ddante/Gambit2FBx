MODULE FBx_MeshType

  USE Gambit_MeshType

  !============
  TYPE FBx_type

     INTEGER :: number_of_spatial_dimensions
     INTEGER :: number_of_domain_nodes
     INTEGER :: number_of_domain_elements
     INTEGER :: number_of_bounday_elements

     REAL(KIND=8),  DIMENSION(:,:), POINTER  ::  node_coordinates
     
     TYPE(domain_type), DIMENSION(:), POINTER :: domain
     INTEGER,           DIMENSION(:), POINTER :: number_of_element_nodes

     TYPE(fb_boundary_type), DIMENSION(:), POINTER :: fb_boundary
            
  END type FBx_type
  !================
  
  !====================
  TYPE fb_boundary_type

     INTEGER :: boundary_log

     INTEGER, DIMENSION(:), POINTER :: domain_nodes_of_boundary
          
  END TYPE fb_boundary_type  
  !========================
  
  
END MODULE FBx_MeshType
