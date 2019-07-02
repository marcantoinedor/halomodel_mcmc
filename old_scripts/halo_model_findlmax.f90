PROGRAM halo_model

   USE array_operations
   USE cosmology_functions
   USE Limber
   USE HMx
   USE string_operations

   IMPLICIT NONE
   REAL ::power_f, mass, l_length_real, name_real, kmin, kmax, amin, amax, lmin, lmax, na_real, l_max_real, thmin, thmax
   REAL, ALLOCATABLE :: k(:), l_array(:), a(:), Cl(:), th_tab(:), xi_tab(:, :)
   REAL, ALLOCATABLE :: pow_li(:, :), pow_2h(:, :, :, :), pow_1h(:, :, :, :), pow_hm(:, :, :, :)
   INTEGER :: icosmo, ihm, field(1), i, j, nf, ix(2)
   INTEGER :: nk, na, l_length, name, l_max, nth
   CHARACTER(len=256) :: fbase, fbase2, fext, output, p_str, q_str, l_length_str, a_str, l_str, na_str, l_max_str, output1, output3
   TYPE(halomod) :: hmod
   TYPE(cosmology) :: cosm
   LOGICAL :: verbose2
   REAL :: coucou

!   Integration domain : to modify to find the importance of this on the power spectrum
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

   ! CALL get_command_argument(1, q_str)
   ! read( q_str, '(f10.0)' ) hmod%ST_q

   ! CALL get_command_argument(2, p_str)
   ! read( p_str, '(f10.0)' ) hmod%ST_p

   ! CALL get_command_argument(3, l_length_str)
   ! read( l_length_str, '(f10.0)' ) l_length_real

   ! l_length = INT(l_length_real)

   hmod%ST_q = 0.707
   hmod%ST_p = 0.3

   CALL get_command_argument(1, na_str)
   read (na_str, '(f10.0)') na_real

   CALL get_command_argument(2, l_max_str)
   read (l_max_str, '(f10.0)') l_max_real

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
   na = INT(na_real)

   CALL fill_array(amin, amax, a, na)

   ! Allocate output Cl
   ALLOCATE (Cl(l_length))

   lmin = 1.
   lmax = 1e4
   l_length = 100
   CALL fill_array(log(lmin), log(lmax), l_array, l_length)
   l_array = exp(l_array)

   thmin = 0.5/60
   thmax = 100/60
   nth = 100
   CALL fill_array(log(thmin), log(thmax), th_tab, nth)
   th_tab = exp(th_tab)

   ! Choose lens survey tracer_CFHTLenS=4
   ix = tracer_CFHTLenS
   ! ix(2)=tracer_CFHTLenS

   ! Allocate array for power spectrum
   ALLOCATE (pow_li(nk, na), pow_2h(1, 1, nk, na), pow_1h(1, 1, nk, na), pow_hm(1, 1, nk, na))

   ! Allocate arrays for angular correlation function
   ALLOCATE (xi_tab(3, nth))

   ! Calculate halo model
   field = field_dmonly
   nf = 1

   DO i = 1, na
      !TODO_statement
      CALL init_halomod(mmin, mmax, a(i), hmod, cosm, verbose)
      CALL calculate_HMx_a(field, nf, k, nk, pow_li(:, i), pow_2h(:, :, :, i), pow_1h(:, :, :, i), pow_hm(:, :, :, i), hmod, cosm, verbose, response)

   END DO

   CALL xpow_pka(ix, l_array, Cl, l_length, k, a, pow_hm, nk, na, cosm)

   l_max = INT(l_max_real)
   ! theta parameter

   CALL calculate_angular_xi(th_tab, xi_tab, nth, l_array, Cl, l_length, l_max)

   fbase = 'data/lmax='
   fbase2 = '/xi1.dat'
   fbase = TRIM(fbase)//TRIM(l_max_str)
   output1 = TRIM(fbase)//TRIM(fbase2)

   fbase = 'data/lmax='
   fbase = TRIM(fbase)//TRIM(l_max_str)
   fbase2 = '/xi3.dat'
   output3 = TRIM(fbase)//TRIM(fbase2)

   OPEN (1, file=output1)
   OPEN (3, file=output3)

   DO j = 1, nth
      WRITE (1, *) xi_tab(1, j)
      WRITE (3, *) xi_tab(3, j)
   END DO

   CLOSE (1)
   CLOSE (3)
   ! fbase='data/q='
   ! fbase2='p='
   ! fbase=trim(fbase) // trim(q_str)
   ! fbase2=trim(fbase2) // trim(p_str)
   ! fbase=trim(fbase) // trim(fbase2)
   ! fext='/power2D.dat'
   ! output=TRIM(fbase) // TRIM(fext)

   ! OPEN(1,file=output)
   ! DO j=1,l_length
   !     WRITE(1,*) l_array(j), Cl(j)
   ! END DO

END PROGRAM
