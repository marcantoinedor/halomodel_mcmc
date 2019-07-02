# Halo model

This repository is an implementation of the MCMC technique to constrain Sheth & Tormen mass function parameters (p,q) from weak lensing data using the halo model.

All theoretical computations are from Alexander Mead's code available here [https://github.com/alexander-mead/library]

To download (clone) this repository, follow the instructions on `github`, I recommend using `>git clone`.

To run this code, you will have to clone the halo model library coded by Alexander Mead, and insert the absolute path of this library in the `Makefile`.
Update regularly this librairy to get the most accurate HM code.

You need a Fortran compiler, and the `Makefile` is currently configured to use `gfortran`, but you can change this and it should (might) work with other compilers, although you may need to change some of the compile flags.

Several directrories also need to be present in the base `halo_model` directory for the install to work correctly. You need `bin/`, `build/`, `debug_build/` and `lib/`. Some of these might be already present when you clone the repository, some of them might be created by the `Makefile`, but some of them you may need to create manually.

## Plots

To make plots of different quantities in the halo model with variations of some parameters (p, q, mmin, mmax), you will need to compile some files in order to create binaries which compute create some data from HM code. All data is stored in `data/` directory and not deleted because some computations take some time. 

## MCMC

Markov Chain Monte-Carlo is a mathematical technique to see errorbars on parameters from some constraints, correlation and others things. 
