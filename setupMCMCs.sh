#!/bin/bash
mkdir -p bin/mcmcs

cp old_scripts/mcmcs/xi_st_CFHT.f90 src/halo_model.f90
make
mv bin/halo_model bin/mcmcs/xi_st_CFHT

cp old_scripts/mcmcs/xi_amp_CFHT.f90 src/halo_model.f90
make
mv bin/halo_model bin/mcmcs/xi_amp_CFHT

cp old_scripts/mcmcs/xi_mmin_CFHT.f90 src/halo_model.f90
make
mv bin/halo_model bin/mcmcs/xi_mmin_CFHT

cp old_scripts/mcmcs/xi_sig8_CFHT.f90 src/halo_model.f90
make
mv bin/halo_model bin/mcmcs/xi_sig8_CFHT
