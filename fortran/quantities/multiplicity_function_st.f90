PROGRAM halo_model

   USE array_operations
   USE cosmology_functions
   USE HMx
   USE string_operations

   IMPLICIT NONE
   REAL :: kmin, kmax, amin, amax, multiplicity_f, a, mass_length_real, name_real
   REAL, ALLOCATABLE :: m(:)
   INTEGER :: icosmo, ihm, field(1), j
   INTEGER :: nk, na, nf, nm, mass_length, nam, nme
   CHARACTER(len=256) :: fext, output1, output2, fbase, p_str, q_str, a_str
   TYPE(halomod) :: hmod
   TYPE(cosmology) :: cosm
   LOGICAL :: verbose2

   LOGICAL, PARAMETER :: verbose = .FALSE.
   LOGICAL, PARAMETER :: response = .FALSE.

   REAL :: mmin = 1e7
   REAL :: mmax = 1e17

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

   CALL get_command_argument(3, a_str)
   read (a_str, '(f10.0)') a

   CALL init_halomod(a, hmod, cosm, verbose)

   nm = 128
   CALL fill_array(log(mmin), log(mmax), m, nm)
   m = exp(m)

   fbase = 'data/q='
   fbase = TRIM(fbase)//TRIM(q_str)
   fbase = TRIM(fbase)//'p='
   fbase = TRIM(fbase)//TRIM(p_str)
   fbase = TRIM(fbase)//'/a='
   fbase = TRIM(fbase)//TRIM(a_str)

   output1 = TRIM(fbase)//'/mass.dat'
   output2 = TRIM(fbase)//'/multiplicity_function.dat'

   OPEN (1, file=output1)
   OPEN (2, file=output2)
   DO j = 1, nm
      WRITE (1, *) m(j)
      multiplicity_f = multiplicity_function(m(j), hmod, cosm)
      WRITE (2, *) multiplicity_f
   END DO

   CLOSE (1)
   CLOSE (2)

END PROGRAM

