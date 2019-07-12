MODULE MASSFUNCTION
   USE array_operations
   ! USE cosmology_functions
   ! USE HMx

contains

   function get_mass_function(mass, q, p, a) result(mass_f)
      REAL(8):: mass, q, p, a, mass_f
      INTEGER :: icosmo, ihm
      ! TYPE(halomod) :: hmod
      ! TYPE(cosmology) :: cosm

      REAL(8), PARAMETER :: mmin = 1e7
      REAL(8), PARAMETER :: mmax = 1e17
      LOGICAL, PARAMETER :: verbose = .FALSE.

      ! Assigns the cosmological model
      icosmo = 1
      ! CALL assign_cosmology(icosmo,cosm,verbose)
      ! CALL init_cosmology(cosm)
      ! ! Assign the halo model
      ! ihm=3
      ! CALL assign_halomod(ihm,hmod,verbose)
      ! hmod%ST_q = q
      ! hmod%ST_p = p

      ! CALL init_halomod(mmin,mmax,a,hmod,cosm,verbose)
      mass_f = 3.0
      ! mass_f=mass_function(mass,hmod,cosm)
   end function
END MODULE MASSFUNCTION
