# Halo model

This repository is an implementation of the MCMC technique to constrain Sheth & Tormen mass function parameters (p,q) from weak lensing data using the halo model.

All theoretical computations are from Alexander Mead's code available here [https://github.com/alexander-mead/library]

To download (clone) this repository, follow the instructions on `github`, I recommend using `>git clone`.

To run this code, you will have to clone the halo model library coded by Alexander Mead, and insert the absolute or relative path of this library in the `Makefile`.
Update regularly this librairy to get the most accurate HM code. This release is for the library the 15 of July 2019.

You need a Fortran compiler, and the `Makefile` is currently configured to use `gfortran`, but you can change this and it should (might) work with other compilers, although you may need to change some of the compile flags.

Several directrories also need to be present in the base `halo_model` directory for the install to work correctly. You need `bin/`, `build/`, `debug_build/` and `lib/`. Some of these might be already present when you clone the repository, some of them might be created by the `Makefile`, but some of them you may need to create manually.

## Installation and dependencies

The required dependencies are the following: 

- matplotlib
- scipy
- numpy
- emcee : https://emcee.readthedocs.io/
- corner : https://corner.readthedocs.io/
- colour : https://pypi.org/project/colour/

Add the path to modules inside this repo. You can add this line to your ***.profile*** file in your ***/home*** .

```
export PYTHONPATH="${PYTHONPATH}:~/hm_mcmc/utils/:~/hm_mcmc/mcmc/:~/hm_mcmc/utils/"
```
## Plots

To make plots of different quantities in the halo model with variations of some parameters (p, q, mmin, mmax, alpha), you will need to compile some files in order to create binaries which compute create some data from HM code. All created data is stored in the ***data/*** directory and not deleted because some computations can take some time.

All the scripts to plot the different figures are in ***plots/*** .

Plotting need compiled fortran programs. All the binaries needed for that are created by a shell script : 

```
./setupPlots.sh
```

To test the environment, one thing to do is to run the script ***plots/first_plot.py***

Take care of running all the scripts from the root of the working directory : ***hm_mcmc/*** .


## MCMC

Markov Chain Monte-Carlo is a mathematical technique to see errorbars on parameters from some constraints, correlation and others things. 
It is useful, especially to see the correlation between parameters from a fitting process, if there are more than 1 parameter. In this implementation, we have 2 free parameters : p and q. 

### Run MCMC

All MCMC programs are a collaboration between fortran and python languages. The fortran one compute the quantities, given the input parameters and these quantities are compared to the value measured by the data. It communicates thanks to the python library ***subprocess*** which enables to call a process from python and catch the standard output (**stdout**) of this subprocess and the output error (**stderr**).

To run the MCMC routines, you would have to install the ***emcee*** package, available [there](https://emcee.readthedocs.io), a really easy-to-use python package very well documented and which enables a very simple setup for parallel computations for example.

All MCMC routines are in the files such as ***mcmc/run_cluster_\*.py***. To prepare a run, compilation of fortran sources is needed, but everything is automatized in a shell script: 

```
./setupMCMCs.sh
```

And then you can launch a MCMC calculation with a command like this one : 

```
python3 mcmc/run_cluster_qp.py 1 3
```

Where the first argument is the index of the cosmology, and the second one is the number of threads you want to use in parallel. 

All cosmology availabled are in the Fortran library, in the file ***cosmology_functions.f90***

This script writes down be the following files :
- A file containing the best fit informations : ***mcmc/results/CFHT/fit{icosmo}.txt***
- A numpy array containing all the walkers steps. It will be saved in a 1D, so the MCMCS parameters(dim, walkers, steps) will be necesarry to use this information. Its path is ***mcmc/results/CFHT/chain_\*{icosmo}.npy***

In this MCMC file, one can choose if he wants to find the best fitting parameters only or to run MCMC only.

### Results of MCMC

An example-script is available deal with the data.

### Development