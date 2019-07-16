PROGRAM halo_model

   USE array_operations
   USE cosmology_functions
   USE Limber
   USE HMx
   USE string_operations

   IMPLICIT NONE
   REAL ::power_f, mass, l_length_real, name_real, kmin, kmax, amin, amax, lmin, lmax, na_real
   REAL, ALLOCATABLE :: k(:), l_array(:), a(:), Cl(:)
   REAL, ALLOCATABLE :: pow_li(:, :), pow_2h(:, :, :, :), pow_1h(:, :, :, :), pow_hm(:, :, :, :)
   INTEGER :: icosmo, ihm, field(1), i, j, nf, ix(2)
   INTEGER :: nk, na, l_length, name
   CHARACTER(len=256) :: fbase, fbase2, fext, output, p_str, q_str, l_length_str, a_str, l_str, na_str
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

   CALL get_command_argument(1, na_str)
   read (na_str, '(f10.0)') na_real

   na = INT(na_real)

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

   CALL fill_array(amin, amax, a, na)

   ! Set number of k points and k range (log spaced)
   lmin = 1.
   lmax = 1e4
   l_length = 100
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
      CALL calculate_HMx_a(field, nf, k, nk, pow_li(:, i), pow_2h(:, :, :, i), pow_1h(:, :, :, i), pow_hm(:, :, :, i), hmod, cosm, verbose, response)

   END DO

   CALL xpow_pka(ix, l_array, Cl, l_length, k, a, pow_hm, nk, na, cosm)

   fbase = 'data/na='
   fbase = trim(fbase)//trim(na_str)
   fext = '/pow2D.dat'
   output = TRIM(fbase)//TRIM(fext)

   OPEN (1, file=output)
   DO j = 1, l_length
      WRITE (1, *) Cl(j)
   END DO

END PROGRAM
