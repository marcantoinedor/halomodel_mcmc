PROGRAM halo_model

   USE array_operations
   USE cosmology_functions
   USE Limber
   USE HMx
   USE string_operations

   IMPLICIT NONE
   REAL :: kmin, kmax, amin, amax, lmin, lmax, icosmo_real, ihm_real
   REAL, ALLOCATABLE :: k(:), l_array(:), a(:), Cl(:), th_tab(:), xi_tab(:, :), xi_out(:)
   REAL, ALLOCATABLE :: pow_li(:, :), pow_2h(:, :, :, :), pow_1h(:, :, :, :), pow_hm(:, :, :, :)
   INTEGER :: icosmo, ihm, field(1), i, j, nf, ix(2)
   INTEGER :: nk, na, nl, name, nth, l_max, m
   INTEGER, ALLOCATABLE :: iBessel(:)
   CHARACTER(len=256) :: input, th_str, p_str, q_str, icosmo_str, ihm_str
   TYPE(halomod) :: hmod
   TYPE(cosmology) :: cosm

   LOGICAL, PARAMETER :: verbose = .FALSE.
   LOGICAL, PARAMETER :: response = .FALSE.

   ! Get arguments from call
   CALL get_command_argument(1, q_str)

   CALL get_command_argument(2, p_str)

   CALL get_command_argument(3, icosmo_str)

   CALL get_command_argument(4, ihm_str)

   ! Converting them to integers
   read (icosmo_str, '(f10.0)') icosmo_real
   read (ihm_str, '(f10.0)') ihm_real

   ! Assigns the cosmological model
   icosmo = INT(icosmo_real)

   CALL assign_cosmology(icosmo, cosm, verbose)
   CALL init_cosmology(cosm)

   ! Assign the halo model
   ihm = INT(ihm_real)
   CALL assign_halomod(ihm, hmod, verbose)

   ! Assign p and q to hmod object
   read (p_str, '(f10.0)') hmod%ST_p
   read (q_str, '(f10.0)') hmod%ST_q

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

   lmin = 1.
   lmax = 1e4
   nl = 80
   CALL fill_array(log(lmin), log(lmax), l_array, nl)
   l_array = exp(l_array)

   ! Allocate output Cl
   ALLOCATE (Cl(nl))

   ! Choose lens survey tracer_CFHTLenS=4
   ix = tracer_CFHTLenS

   ! number of data points in CFHTLenS survey
   nth = 21

   ! Allocate array for power spectrum
   ALLOCATE (pow_li(nk, na), pow_2h(1, 1, nk, na), pow_1h(1, 1, nk, na), pow_hm(1, 1, nk, na))

   ! Allocate arrays for angular correlation function
   ALLOCATE (th_tab(nth), xi_tab(2, nth), xi_out(2*nth))

   ! Path to CFHTLenS thetas data
   input = 'CFHTLenS/thetas.dat'

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

   CALL xpow_pka(ix, l_array, Cl, nl, k, a, pow_hm, nk, na, cosm)

! parameters for correlation functions
   l_max = 100000
   ! theta parameter
   m = 2
   ALLOCATE (iBessel(m))
   iBessel(1) = 0
   iBessel(2) = 4

   CALL calculate_angular_xi(iBessel, m, th_tab, xi_tab, nth, l_array, Cl, nl, l_max)

   DO j = 1, nth
      xi_out(j) = xi_tab(1, j)
      xi_out(nth + j) = xi_tab(2, j)
   END DO

   ! Send value via stdout to python
   WRITE (*, *) xi_out

END PROGRAM
