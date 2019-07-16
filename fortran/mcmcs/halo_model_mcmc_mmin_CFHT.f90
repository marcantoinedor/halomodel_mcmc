PROGRAM halo_model

   USE array_operations
   USE cosmology_functions
   USE Limber
   USE HMx
   USE string_operations

   IMPLICIT NONE
   REAL ::power_f, mass, l_length_real, name_real, nth_real, kmin, kmax, amin, amax, lmin, lmax, mmin, mmin_log
   REAL, ALLOCATABLE :: k(:), l_array(:), a(:), Cl(:), th_tab(:), xi_tab(:, :)
   REAL, ALLOCATABLE :: pow_li(:, :), pow_2h(:, :, :, :), pow_1h(:, :, :, :), pow_hm(:, :, :, :)
   INTEGER :: icosmo, ihm, field(1), i, j, nf, ix(2)
   INTEGER :: nk, na, l_length, name, nth, l_max, m
   INTEGER, ALLOCATABLE :: iBessel(:)
   CHARACTER(len=256) :: fext, output, input, p_str, q_str, l_length_str, a_str, l_str, th_str, name_str, lmax_str, nth_str, output1, output3, mmin_str
   TYPE(halomod) :: hmod
   TYPE(cosmology) :: cosm
   LOGICAL :: verbose2

   !

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

   CALL get_command_argument(3, mmin_str)
   read (mmin_str, '(f10.0)') mmin_log
   mmin = exp(log(10.)*mmin_log)

   CALL get_command_argument(4, nth_str)
   read (nth_str, '(f10.0)') nth_real

   ! l_length = INT(l_length_real)
   nth = INT(nth_real)

   CALL get_command_argument(5, name_str)

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
   na = 7
   CALL fill_array(amin, amax, a, na)

   ! Allocate l_array
   ! ALLOCATE(l_array(l_length))

   lmin = 1.
   lmax = 1e4
   l_length = 80
   CALL fill_array(log(lmin), log(lmax), l_array, l_length)
   l_array = exp(l_array)

   ! Allocate output Cl
   ALLOCATE (Cl(l_length))

   ! Choose lens survey tracer_CFHTLenS_Kilbinger2013=4
   ix = tracer_CFHTLenS_Kilbinger2013

   ! Allocate array for power spectrum
   ALLOCATE (pow_li(nk, na), pow_2h(1, 1, nk, na), pow_1h(1, 1, nk, na), pow_hm(1, 1, nk, na))

   ! Allocate arrays for angular correlation function
   ALLOCATE (th_tab(nth), xi_tab(2, nth))

   input = '/home/marco/halo_model/utils/CFHTthetas.dat'

   OPEN (1, file=input)
   DO i = 1, nth
      READ (1, *) th_str
      READ (th_str, '(f10.0)') th_tab(i)
      ! Converting to degrees
      th_tab(i) = th_tab(i)/60
   END DO
   CLOSE (1)

   ! Calculate halo model
   field = field_dmonly
   nf = 1

   DO i = 1, na
      !TODO_statement
      CALL init_halomod(a(i), hmod, cosm, verbose)
      CALL calculate_HMx_a(field, nf, k, nk, pow_li(:, i), pow_2h(:, :, :, i), pow_1h(:, :, :, i), pow_hm(:, :, :, i), hmod, cosm, verbose, response)

   END DO

   CALL xpow_pka(ix, l_array, Cl, l_length, k, a, pow_hm, nk, na, cosm)

! parameters for correlation functions
   l_max = 100000
   ! theta parameter
   m = 2
   ALLOCATE (iBessel(m))
   iBessel(1) = 0
   iBessel(2) = 4

   CALL calculate_angular_xi(iBessel, m, th_tab, xi_tab, nth, l_array, Cl, l_length, l_max)

   fext = '.dat'
   output1 = '/home/marco/mcmc/data/data_'
   output1 = TRIM(output1)//TRIM(name_str)
   output1 = TRIM(output1)//TRIM(fext)
   OPEN (2, file=output1)

   DO j = 1, nth
      WRITE (2, *) xi_tab(1, j)
   END DO

   DO j = 1, nth
      WRITE (2, *) xi_tab(2, j)
   END DO
   CLOSE (2)

END PROGRAM
