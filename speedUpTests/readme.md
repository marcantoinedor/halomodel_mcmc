Changes by Marc-Antoine Dor
=============

## How to use this code?

Python scripts in **utils/** are useful to create data from a compiled file in **bin/**.
**get.py** is here to parse the **.dat** files in **data/**



## To use f2py

f2py -c --opt='-Warray-bounds -ffpe-trap=invalid,zero,overflow -fimplicit-none -O3 -std=gnu -ffree-line-length-none -fdefault-real-8 -fdefault-double-8 -lgfortran -lm' --fco
mpiler=gfortran constants.f90 physics.f90 logical_operations.f90 random_numbers.f90 file_info.f90 fix_polynomial.f90 array_operations.f90 table_integer.f90 special_functions.f90 interpolate.f90 s
olve_equations.f90 string_operations.f90 calculus_table.f90 cosmology_functions.f90 HMx.f90 module.f90 -m module