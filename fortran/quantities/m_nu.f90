PROGRAM halo_model

   USE array_operations
   USE cosmology_functions
   USE HMx
   USE string_operations

   IMPLICIT NONE
   REAL :: kmin, kmax, amin, amax, numin, numax, nu, m
   REAL, ALLOCATABLE :: k(:), a(:), mass(:), nuX(:)
   REAL, ALLOCATABLE :: pow_li(:, :), pow_2h(:, :, :, :), pow_1h(:, :, :, :), pow_hm(:, :, :, :)
   INTEGER :: icosmo, ihm, field(1), i, j
   INTEGER :: nk, na, nf, nm, num
   CHARACTER(len=256) :: base, fbase, fbase2, fbase3, fext, output, p_str, q_str
   TYPE(halomod) :: hmod
   TYPE(cosmology) :: cosm
   LOGICAL :: verbose2 = .FALSE.

   LOGICAL, PARAMETER :: verbose = .FALSE.
   LOGICAL, PARAMETER :: response = .FALSE.

   REAL :: mmin = 1e7
   REAL :: mmax = 1e17
   ! Assigns the cosmological model
   icosmo = 1
   CALL assign_cosmology(icosmo, cosm, verbose)
   CALL init_cosmology(cosm)
   CALL print_cosmology(cosm)

   ! Assign the halo model
   ihm = 3
   CALL assign_halomod(ihm, hmod, verbose)

   ! Set number of k points and k range (log spaced)
   nk = 128
   kmin = 1e-3
   kmax = 1e2
   CALL fill_array(log(kmin), log(kmax), k, nk)
   k = exp(k)

   ! Set the number of scale factors and range (linearly spaced)
   amin = 0.1
   amax = 1.0
   na = 10
   CALL fill_array(amin, amax, a, na)

   ! Allocate arrays for power spectra
   ALLOCATE (pow_li(nk, na), pow_2h(1, 1, nk, na), pow_1h(1, 1, nk, na), pow_hm(1, 1, nk, na))

   ! Calculate halo model
   field = field_dmonly
   nf = 1

   ! Mass range
   nm = 128
   CALL fill_array(log(mmin), log(mmax), mass, nm)
   mass = exp(mass)

   ! Nu range
   num = 128
   numin = 0.1
   numax = 5.0
   CALL fill_array(numin, numax, nuX, num)

   ! Loop over scale factors and do calculation
   DO i = 1, na

      CALL init_halomod(a(i), hmod, cosm, verbose2)
      CALL print_halomod(hmod, cosm, verbose2)
      CALL calculate_HMx_a(field, nf, k, nk, pow_li(:, i), pow_2h(:, :, :, i), pow_1h(:, :, :, i), pow_hm(:, :, :, i), hmod, cosm, verbose2, response)

      fbase = 'data/quantities/'
      fext = '.dat'

      fbase2 = TRIM(fbase)//'nuM_'
      output = number_file(fbase2, i, fext)

      OPEN (1, file=output)
      DO j = 1, nm
         nu = nu_M(mass(j), hmod, cosm)
         WRITE (1, *) mass(j), nu
      END DO
      CLOSE (1)

      fbase2 = TRIM(fbase)//'Mnu_'
      output = number_file(fbase2, i, fext)
      OPEN (2, file=output)
      DO j = 1, num
         m = M_nu(nuX(j), hmod)
         WRITE (2, *) nuX(j), m
      END DO
      CLOSE (2)
   END DO

END PROGRAM
