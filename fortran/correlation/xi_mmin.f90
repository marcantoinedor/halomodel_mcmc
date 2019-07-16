PROGRAM halo_model

   USE array_operations
   USE cosmology_functions
   USE Limber
   USE HMx
   USE string_operations

   IMPLICIT NONE
   REAL ::power_f, mass, l_length_real, kmin, kmax, amin, amax, lmin, lmax, thmin, thmax, l_max_real, mmin
   REAL, ALLOCATABLE :: k(:), l_array(:), a(:), Cl(:), th_tab(:), xi_tab(:, :)
   REAL, ALLOCATABLE :: pow_li(:, :), pow_2h(:, :, :, :), pow_1h(:, :, :, :), pow_hm(:, :, :, :)
   INTEGER :: icosmo, ihm, field(1), i, j, nf, ix(2)
   INTEGER :: nk, na, l_length, l_max, nth, m
   INTEGER, ALLOCATABLE :: iBessel(:)
   CHARACTER(len=256) :: fbase, fbase2, fext1, fext3, output1, output3, l_max_str, input, th_str, mmin_str, ext
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

   CALL get_command_argument(2, l_max_str)
   read (l_max_str, '(f10.0)') l_max_real
   l_max = INT(l_max_real)

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
   na = 8

   CALL fill_array(amin, amax, a, na)

   ! Set number of l points and l range (log spaced)
   lmin = 1.
   lmax = 1e4
   l_length = 30
   CALL fill_array(log(lmin), log(lmax), l_array, l_length)
   l_array = exp(l_array)

   ! Allocate output Cl
   ALLOCATE (Cl(l_length))

   ! Choose lens survey tracer_CFHTLenS_Kilbinger2013=4
   ix = tracer_CFHTLenS_Kilbinger2013
   ! get thetas (arcmin from CFHTLenS survey)
   input = 'CFHTLenS/thetas.dat'
   nth = 21
   ALLOCATE (th_tab(nth))

   OPEN (2, file=input)
   DO i = 1, nth
      READ (2, *) th_str
      READ (th_str, '(f10.0)') th_tab(i)
      ! Converting to degrees, really important
      th_tab(i) = th_tab(i)/60
   END DO
   CLOSE (2)

   ! Allocate array for power spectrum
   ALLOCATE (pow_li(nk, na), pow_2h(1, 1, nk, na), pow_1h(1, 1, nk, na), pow_hm(1, 1, nk, na))

   ALLOCATE (xi_tab(2, nth))

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

   l_max = INT(l_max_real)
   m = 2
   ALLOCATE (iBessel(m))
   iBessel(1) = 0
   iBessel(2) = 4

   CALL calculate_angular_xi(iBessel, m, th_tab, xi_tab, nth, l_array, Cl, l_length, l_max)

   fbase = 'data/mmin='
   fbase = TRIM(fbase)//TRIM(mmin_str)
   fext1 = '/xi1.dat'
   output1 = TRIM(fbase)//TRIM(fext1)

   fext3 = '/xi3.dat'
   output3 = TRIM(fbase)//TRIM(fext3)

   OPEN (1, file=output1)
   OPEN (2, file=output3)
   DO j = 1, nth
      WRITE (1, *) xi_tab(1, j)
      WRITE (2, *) xi_tab(2, j)
   END DO

   CLOSE (1)
   CLOSE (2)

END PROGRAM
