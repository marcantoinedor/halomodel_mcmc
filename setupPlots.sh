#!/bin/bash
mkdir -p bin/plots
mkdir -p src

# Basic plots of halo model power spectrum
cp fortran/halo_model.demo.f90 src/halo_model.f90
make
mv bin/halo_model bin/halo_model_demo

# Implementation of relative differences (mmax version) in the halo_model quantities (3D power spectrum) varying mmax
cp fortran/pow3D/power_mmax.f90 src/halo_model.f90
make
mv bin/halo_model bin/plots/pow3D_mmax

# Implementation of relative differences (mmin version) in the halo_model quantities (3D power spectrum) varying mmax
cp fortran/pow3D/power_mmin.f90 src/halo_model.f90
make
mv bin/halo_model bin/plots/pow3D_mmin

# Implementation of relative differences (alpha version) in the halo_model quantities (3D power spectrum) varying mmax
cp fortran/pow3D/power_alpha.f90 src/halo_model.f90
make
mv bin/halo_model bin/plots/pow3D_alpha

# 3D power spectrum:  Varying S&T parameters in halo mass function
cp fortran/pow3D/power_st.f90 src/halo_model.f90
make
mv bin/halo_model bin/plots/pow3D_st

# 3D power spectrum:  Varying halo code
cp fortran/pow3D/power_ihm.f90 src/halo_model.f90
make
mv bin/halo_model bin/plots/pow3D_ihm

# 2D power spectrum, CFHT range:  Varying S&T parameters in halo mass function
cp fortran/pow2D/power_st.f90 src/halo_model.f90
make
mv bin/halo_model bin/plots/pow2D_st

# 2D power spectrum, CFHT range:  Varying mmin parameter in halo mass function
cp fortran/pow2D/power_mmin.f90 src/halo_model.f90
make
mv bin/halo_model bin/plots/pow2D_mmin

# 2D power spectrum, CFHT range:  Varying alpha parameter in halo mass function
cp fortran/pow2D/power_alpha.f90 src/halo_model.f90
make
mv bin/halo_model bin/plots/pow2D_alpha

# 2D power spectrum, CFHT range:  Varying halo code
cp fortran/pow2D/power_ihm.f90 src/halo_model.f90
make
mv bin/halo_model bin/plots/pow2D_ihm

# Shear correlation function, CFHT range: Varying alpha parameter in halo mass function
cp fortran/correlation/xi_alpha.f90 src/halo_model.f90
make
mv bin/halo_model bin/plots/xi_alpha

# Shear correlation function, CFHT range: Varying mmin parameter in halo mass function
cp fortran/correlation/xi_mmin.f90 src/halo_model.f90
make
mv bin/halo_model bin/plots/xi_mmin

# Shear correlation function, CFHT range: Varying S&T parameters in halo mass function
cp fortran/correlation/xi_st.f90 src/halo_model.f90
make
mv bin/halo_model bin/plots/xi_st

# Shear correlation function, CFHT range: Varying S&T parameters in halo mass function
cp fortran/correlation/xi_ihm.f90 src/halo_model.f90
make
mv bin/halo_model bin/plots/xi_ihm

# Shear correlation function, CFHT range: Varying sig8 parameter in halo mass function
cp fortran/correlation/xi_sig8.f90 src/halo_model.f90
make
mv bin/halo_model bin/plots/xi_sig8

# Optimisation scripts for number of scale factors for interpolation in Limber
cp fortran/opt/findNa.f90 src/halo_model.f90
make
mv bin/halo_model bin/opt/findNa

# Optimisation scripts for number of Bessel modes to consider
cp fortran/opt/findLmax.f90 src/halo_model.f90
make
mv bin/halo_model bin/opt/findLmax

# Optimisation scripts for number of Bessel modes to consider
cp fortran/opt/findLmax_CFHT.f90 src/halo_model.f90
make
mv bin/halo_model bin/opt/findLmax_CFHT

# Optimisation scripts for number of l_length to consider
cp fortran/opt/findL_length.f90 src/halo_model.f90
make
mv bin/halo_model bin/opt/findL_length

# plots of correlation function varying cosmology
cp fortran/correlation/xi_cosmo.f90 src/halo_model.f90
make
mv bin/halo_model bin/plots/xi_cosmo

# Plotting m(nu) and nu(M)
cp fortran/quantities/m_nu.f90 src/halo_model.f90
make
mv bin/halo_model bin/plots/m_nu

# Plotting mass_function
cp fortran/quantities/mass_function_st.f90 src/halo_model.f90
make
mv bin/halo_model bin/plots/mass_function

# Plotting mass_function
cp fortran/quantities/multiplicity_function_st.f90 src/halo_model.f90
make
mv bin/halo_model bin/plots/multiplicity_function
