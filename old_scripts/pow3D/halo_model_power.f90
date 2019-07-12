PROGRAM halo_model

   USE array_operations
   USE cosmology_functions
   USE HMx
   USE string_operations

   IMPLICIT NONE
   REAL :: k, power_f, a, mass, k_length_real, name_real
   REAL, ALLOCATABLE :: k_array(:)
   REAL, ALLOCATABLE :: pow_li(:, :), pow_2h(:, :, :, :), pow_1h(:, :, :, :), pow_hm(:, :, :, :)
   INTEGER :: icosmo, ihm, field(1), i, j, nf
   INTEGER :: k_length, name
   CHARACTER(len=256) :: fext, output, input, p_str, q_str, k_length_str, a_str, k_str, name_str
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

   CALL get_command_argument(1, q_str)
   read (q_str, '(f10.0)') hmod%ST_q

   CALL get_command_argument(2, p_str)
   read (p_str, '(f10.0)') hmod%ST_p

   CALL get_command_argument(3, k_length_str)
   read (k_length_str, '(f10.0)') k_length_real

   k_length = INT(k_length_real)
   CALL get_command_argument(4, a_str)
   read (a_str, '(f10.0)') a

   CALL get_command_argument(5, name_str)

   CALL init_halomod(a, hmod, cosm, verbose)

   ! Allocate k_array
   ALLOCATE (k_array(k_length))

   ! Allocate array for power spectrum
   ALLOCATE (pow_li(k_length, 1), pow_2h(1, 1, k_length, 1), pow_1h(1, 1, k_length, 1), pow_hm(1, 1, k_length, 1))

   input = '../mcmc/data/k_range.dat'

   OPEN (1, file=input)
   DO i = 1, k_length
      READ (1, *) k_str
      READ (k_str, '(f10.0)') k_array(i)
   END DO
   CLOSE (1)

   ! Calculate halo model
   field = field_dmonly
   nf = 1
   CALL calculate_HMx_a(field, nf, k_array, k_length, pow_li(:, 1), pow_2h(:, :, :, 1), pow_1h(:, :, :, 1), pow_hm(:, :, :, 1), hmod, cosm, verbose, response)

   fext = '.dat'
   output = '../mcmc/data/data_'
   output = TRIM(output)//TRIM(name_str)
   output = TRIM(output)//TRIM(fext)
   OPEN (2, file=output)
   DO j = 1, k_length
      WRITE (2, *) pow_hm(1, 1, j, 1)
   END DO

   CLOSE (2)

END PROGRAM
