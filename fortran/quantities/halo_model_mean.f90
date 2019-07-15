PROGRAM halo_model

   USE array_operations
   USE cosmology_functions
   USE HMx
   USE string_operations

   IMPLICIT NONE
   REAL :: kmin, kmax, amin, amax, mean_f, a, mass, mass_length_real, name_real
   ! REAL, ALLOCATABLE :: k(:)
   INTEGER :: icosmo, ihm, field(1), i, j
   INTEGER :: nk, na, nf, nm, mass_length, name
   CHARACTER(len=256) :: fext, output, input, p_str, q_str, mass_length_str, a_str, mass_str, name_str
   TYPE(halomod) :: hmod
   TYPE(cosmology) :: cosm
   LOGICAL :: verbose2

   REAL, PARAMETER :: mmin = 1e7
   REAL, PARAMETER :: mmax = 1e17
   LOGICAL, PARAMETER :: verbose = .FALSE.
   LOGICAL, PARAMETER :: response = .FALSE.

   ! Assigns the cosmological model
   icosmo = 1
   CALL assign_cosmology(icosmo, cosm, verbose)
   CALL init_cosmology(cosm)
   ! Assign the halo model
   ihm = 3
   CALL assign_halomod(ihm, hmod, verbose)

   CALL get_command_argument(1, q_str)
   read (q_str, '(f10.0)') hmod%ST_q

   CALL get_command_argument(2, p_str)
   read (p_str, '(f10.0)') hmod%ST_p

   CALL get_command_argument(3, mass_length_str)
   read (mass_length_str, '(f10.0)') mass_length_real

   mass_length = INT(mass_length_real)
   CALL get_command_argument(4, a_str)
   read (a_str, '(f10.0)') a

   CALL get_command_argument(5, name_str)

   CALL init_halomod(mmin, mmax, a, hmod, cosm, verbose)
   fext = '.dat'
   input = '../mcmc/data/mass.dat'
   output = '../mcmc/data/data_'
   output = TRIM(output)//TRIM(name_str)
   output = TRIM(output)//TRIM(fext)
   OPEN (1, file=input)
   OPEN (2, file=output)
   DO j = 1, mass_length
      READ (1, *) mass_str
      READ (mass_str, '(f10.0)') mass
      mean_f = mass*mass_function(mass, hmod, cosm)
      WRITE (2, *) mean_f
   END DO

   CLOSE (1)
   CLOSE (2)

END PROGRAM
