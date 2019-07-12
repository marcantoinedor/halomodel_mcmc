#!/bin/bash
mkdir -p bin/plots

# Basic plots of halo model power spectrum
cp old_scripts/halo_model.demo.f90 src/halo_model.f90
make
mv bin/halo_model bin/halo_model_demo

# Implementation of relative differences (mmax version) in the halo_model quantities (3D power spectrum) varying mmax
cp old_scripts/pow3D/power_mmax.f90 src/halo_model.f90
make
mv bin/halo_model bin/plots/pow3D_mmax

# Implementation of relative differences (mmin version) in the halo_model quantities (3D power spectrum) varying mmax
cp old_scripts/pow3D/power_mmin.f90 src/halo_model.f90
make
mv bin/halo_model bin/plots/pow3D_mmin

# Implementation of relative differences (alpha version) in the halo_model quantities (3D power spectrum) varying mmax
cp old_scripts/pow3D/power_alpha.f90 src/halo_model.f90
make
mv bin/halo_model bin/plots/pow3D_alpha

# 3D power spectrum:  Varying S&T parameters in halo mass function
cp old_scripts/pow3D/power_st.f90 src/halo_model.f90
make
mv bin/halo_model bin/plots/pow3D_st

# 3D power spectrum:  Varying halo code
cp old_scripts/pow3D/power_ihm.f90 src/halo_model.f90
make
mv bin/halo_model bin/plots/pow3D_ihm

# 2D power spectrum, CFHT range:  Varying S&T parameters in halo mass function
cp old_scripts/pow2D/power_st.f90 src/halo_model.f90
make
mv bin/halo_model bin/plots/pow2D_st

# 2D power spectrum, CFHT range:  Varying mmin parameter in halo mass function
cp old_scripts/pow2D/power_mmin.f90 src/halo_model.f90
make
mv bin/halo_model bin/plots/pow2D_mmin

# 2D power spectrum, CFHT range:  Varying alpha parameter in halo mass function
cp old_scripts/pow2D/power_alpha.f90 src/halo_model.f90
make
mv bin/halo_model bin/plots/pow2D_alpha

# 2D power spectrum, CFHT range:  Varying halo code
cp old_scripts/pow2D/power_ihm.f90 src/halo_model.f90
make
mv bin/halo_model bin/plots/pow2D_ihm

# Shear correlation function, CFHT range: Varying alpha parameter in halo mass function
cp old_scripts/correlation/xi_alpha.f90 src/halo_model.f90
make
mv bin/halo_model bin/plots/xi_alpha

# Shear correlation function, CFHT range: Varying mmin parameter in halo mass function
cp old_scripts/correlation/xi_mmin.f90 src/halo_model.f90
make
mv bin/halo_model bin/plots/xi_mmin

# Shear correlation function, CFHT range: Varying S&T parameters in halo mass function
cp old_scripts/correlation/xi_st.f90 src/halo_model.f90
make
mv bin/halo_model bin/plots/xi_st

# Shear correlation function, CFHT range: Varying S&T parameters in halo mass function
cp old_scripts/correlation/xi_ihm.f90 src/halo_model.f90
make
mv bin/halo_model bin/plots/xi_ihm

# Optimisation scripts for number of loop iterations
cp old_scripts/opt/findNa.f90 src/halo_model.f90
make
mv bin/halo_model bin/opt/findNa

# # plots of correlation function varying cosmology
# cp old_scripts/correlation/cosmo_CFHT.f90 src/halo_model
# make
# cp bin/halo_model bin/cosmo_xi_CFHT

