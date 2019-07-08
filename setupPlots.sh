
# Basic plots of halo model power spectrum
cp old_scripts/halo_model.demo.f90 src/halo_model.f90
make
mv bin/halo_model bin/halo_model_demo


# Implementation of relative differences in the halo_model quantities (3D power spectrum) varying mmax
cp old_scripts/pow3D/power_mmax.f90 src/halo_model.f90
make
mv bin/halo_model bin/plots/pow3D_mmax

# Varying S&T parameters in halo mass function
cp old_scripts/pow3D/power_st.f90 src/halo_model.f90
make
mv bin/halo_model bin/plots/pow3D_st



# # plots of correlation function varying cosmology
# cp old_scripts/correlation/cosmo_CFHT.f90 src/halo_model
# make
# cp bin/halo_model bin/cosmo_xi_CFHT

