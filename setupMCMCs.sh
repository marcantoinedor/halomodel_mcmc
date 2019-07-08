mkdir bin/mcmcs

cp old_scripts/mcmcs/xi_CFHT.f90 src/halo_model.f90
make
mv bin/halo_model bin/mcmcs/xi_CFHT

cp old_scripts/mcmcs/xi_amp_CFHT.f90 src/halo_model.f90
make
mv bin/halo_model bin/mcmcs/xi_amp_CFHT

cp old_scripts/mcmcs/xi_mmin_CFHT.f90 src/halo_model.f90
make
mv bin/halo_model bin/mcmcs/xi_mmin_CFHT
