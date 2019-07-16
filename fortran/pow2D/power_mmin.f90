PROGRAM halo_model

   USE array_operations
   USE cosmology_functions
   USE Limber
   USE HMx
   USE string_operations

   IMPLICIT NONE
   REAL ::power_f, mass, l_length_real, kmin, kmax, amin, amax, lmin, lmax, mmin
   REAL, ALLOCATABLE :: k(:), l_array(:), a(:), Cl(:)
   REAL, ALLOCATABLE :: pow_li(:, :), pow_2h(:, :, :, :), pow_1h(:, :, :, :), pow_hm(:, :, :, :)
   INTEGER :: icosmo, ihm, field(1), i, j, nf, ix(2)
   INTEGER :: nk, na, l_length, name
   CHARACTER(len=256) :: fbase, fbase2, fext, output, l_length_str, mmin_str
   TYPE(halomod) :: hmod
   TYPE(cosmology) :: cosm
   LOGICAL :: verbose2

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

   CALL get_command_argument(2, l_length_str)
   read (l_length_str, '(f10.0)') l_length_real

   l_length = INT(l_length_real)

   ! Set number of k points and k range (log spaced)
   nk = 128
   kmin = 1e-3
   kmax = 1e2
   CALL fill_array(log(kmin), log(kmax), k, nk)
   k = exp(k)

! Set the number of scale factors and range (linearly spaced)
   ! In lensing, we consider redshift between 0 and 3
   amin = 0.25
   amax = 1.0
   na = 10

   CALL fill_array(amin, amax, a, na)

   ! Set number of k points and k range (log spaced)
   lmin = 1e0
   lmax = 1e4

   CALL fill_array(lmin, lmax, l_array, l_length)

   ! Allocate output Cl
   ALLOCATE (Cl(l_length))

   ! Choose lens survey tracer_CFHTLenS_Kilbinger2013=4
   ix = tracer_CFHTLenS_Kilbinger2013
   ! ix(2)=tracer_CFHTLenS_Kilbinger2013

   ! Allocate array for power spectrum
   ALLOCATE (pow_li(nk, na), pow_2h(1, 1, nk, na), pow_1h(1, 1, nk, na), pow_hm(1, 1, nk, na))

   ! Calculate halo model
   field = field_dmonly
   nf = 1

   DO i = 1, na
      !TODO_statement
      CALL init_halomod(a(i), hmod, cosm, verbose)
      hmod%mmin = mmin
      CALL calculate_HMx_a(field, nf, k, nk, pow_li(:, i), pow_2h(:, :, :, i), pow_1h(:, :, :, i), pow_hm(:, :, :, i), hmod, cosm, verbose, response)

   END DO

   CALL xpow_pka(ix, l_array, Cl, l_length, k, a, pow_hm, nk, na, cosm)

   fbase = 'data/mmin='
   fext = '/power2D.dat'
   fbase = TRIM(fbase)//TRIM(mmin_str)
   output = TRIM(fbase)//TRIM(fext)
   OPEN (1, file=output)
   DO j = 1, l_length
      WRITE (1, *) l_array(j), Cl(j)
   END DO

END PROGRAM
