#!/bin/bash
mkdir -p bin/mcmcs

cp fortran/mcmcs/xi_st_CFHT.f90 src/halo_model.f90
make
mv bin/halo_model bin/mcmcs/xi_st_CFHT

cp fortran/mcmcs/xi_amp_CFHT.f90 src/halo_model.f90
make
mv bin/halo_model bin/mcmcs/xi_amp_CFHT

cp fortran/mcmcs/xi_mmin_CFHT.f90 src/halo_model.f90
make
mv bin/halo_model bin/mcmcs/xi_mmin_CFHT

cp fortran/mcmcs/xi_sig8_CFHT.f90 src/halo_model.f90
make
mv bin/halo_model bin/mcmcs/xi_sig8_CFHT
