PROGRAM halo_model

   USE array_operations
   USE cosmology_functions
   USE HMx
   USE string_operations

   IMPLICIT NONE
   REAL :: k, power_f, mass, nk_real, name_real, kmin, kmax, a(1), mmin
   REAL, ALLOCATABLE :: k_array(:)
   REAL, ALLOCATABLE :: pow_li(:, :), pow_2h(:, :, :, :), pow_1h(:, :, :, :), pow_hm(:, :, :, :)
   INTEGER :: icosmo, ihm, field(1), i, j, nf
   INTEGER :: nk, name
   CHARACTER(len=256) :: fext, output, input, p_str, q_str, nk_str, a_str, k_str, name_str, term1, term2, term3, mmin_str, fbase, axis
   TYPE(halomod) :: hmod
   TYPE(cosmology) :: cosm
   LOGICAL :: verbose2

!   Integration domain : to modify to find the importance of this on the power spectrum
   ! REAL, PARAMETER :: mmin = 1e7
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

   CALL get_command_argument(1, mmin_str)
   read (mmin_str, '(f10.0)') mmin
   ! Set number of k points and k range (log spaced)
   nk = 128
   kmin = 1e-3
   kmax = 1e2
   CALL fill_array(log(kmin), log(kmax), k_array, nk)
   k_array = exp(k_array)

   ! most relevant redshift for CFHTLenS
   a = 1./(1 + 0.7)

   CALL init_halomod(mmin, mmax, a(1), hmod, cosm, verbose)

   ! Allocate array for power spectrum
   ALLOCATE (pow_li(nk, 1), pow_2h(1, 1, nk, 1), pow_1h(1, 1, nk, 1), pow_hm(1, 1, nk, 1))

   ! Calculate halo model
   field = field_dmonly
   nf = 1
   CALL calculate_HMx_a(field, nf, k_array, nk, pow_li(:, 1), pow_2h(:, :, :, 1), pow_1h(:, :, :, 1), pow_hm(:, :, :, 1), hmod, cosm, verbose, response)

   fbase = 'data/mmin='
   fbase = TRIM(fbase)//TRIM(mmin_str)

   axis = '/k.dat'
   term1 = '1h.dat'
   term2 = '2h.dat'
   term3 = 'hm.dat'

   output = TRIM(fbase)//TRIM(axis)
   OPEN (2, file=output)
   DO j = 1, nk
      WRITE (2, *) k_array(j)
   END DO
   CLOSE (2)

   fext = '/pow_'
   fbase = TRIM(fbase)//TRIM(fext)

   output = TRIM(fbase)//TRIM(term1)
   OPEN (2, file=output)
   DO j = 1, nk
      WRITE (2, *) pow_1h(1, 1, j, 1)
   END DO
   CLOSE (2)

   output = TRIM(fbase)//TRIM(term2)
   OPEN (2, file=output)
   DO j = 1, nk
      WRITE (2, *) pow_2h(1, 1, j, 1)
   END DO
   CLOSE (2)

   output = TRIM(fbase)//TRIM(term3)
   OPEN (2, file=output)
   DO j = 1, nk
      WRITE (2, *) pow_hm(1, 1, j, 1)
   END DO
   CLOSE (2)

END PROGRAM
