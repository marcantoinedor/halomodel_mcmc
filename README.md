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
It is useful, especially to see the correlation between parameters from a fitting process, if there are more than 1 parameter. In this implementation, we have 2 free parameters : p and q. 

### Run MCMC

All MCMC programs are a collaboration between fortran and python languages. The fortran one compute the quantities, given the input parameters and these quantities are compared to the value measured by the data. It communicates thanks to the python library ***subprocess*** which enables to call a process from python and catch the standard output (**stdout**) of this subprocess and the output error (**stderr**).

To run the MCMC routines, you would have to install the ***emcee*** package, available [there](https://emcee.readthedocs.io), a really easy-to-use python package very well documented and which enables a very simple setup for parallel computations for example.

All MCMC routines are in the files such as ***mcmc/run_cluster*.py***. To prepare a run, compilation of fortran sources is needed, but everything is automatized in a shell script: 

```
./setupMCMCS.sh
```

And then you can launch a MCMC calculation with a command like this one : 

```
python3 mcmc/run_cluster_qp.py 1 3
```

Where the first argument is the index of the cosmology, and the second one is the number of threads you want to use in parallel. 

### MCMC plots

### Development