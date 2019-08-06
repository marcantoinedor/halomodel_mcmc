# Halo model

This repository is an implementation of the MCMC technique to constrain Sheth & Tormen mass function parameters (p,q) from weak lensing data using the halo model.

All theoretical computations are from Alexander Mead's code available here [https://github.com/alexander-mead/library]

To download (clone) this repository, follow the instructions on `github`, I recommend using `>git clone`.

To run this code, you will have to clone the halo model library coded by Alexander Mead, and insert the absolute or relative path of this library in the `Makefile`.
Update regularly this library to get the most accurate version of the library. This release is for the library the 31st of July 2019.

You need a Fortran compiler, and the `Makefile` is currently configured to use `gfortran`, but you can change this and it should (might) work with other compilers, although you may need to change some of the compile flags.

Several directrories also need to be present in the base `halo_model` directory for the installation to work correctly. You need `bin/`, `build/`, `debug_build/` and `lib/`. Some of these might be already present when you clone the repository, some of them might be created by the `Makefile`, but some of them you may need to create manually.

# Installation and dependencies

Everything is coded using **python3**.

The required dependencies are the following: 

- matplotlib
- scipy
- numpy
- subprocess (usually installed by default)
- emcee : https://emcee.readthedocs.io/
- pygtc : https://pygtc.readthedocs.io/
- colour : https://pypi.org/project/colour/

In order to make the code easy to read and modify, some separated modules are implemented in python to get data, create data and others actions...

Finally you will need to replace the line 1316 in library following file  ***src/limber.f90*** : 
(where the n(z) histogram of CFHTLenS is hardcoded) in the 

```
input = '/Users/Mead/Physics/data/CFHTLenS/nz.txt'
```

by this one : 

```
input = 'CFHTLenS/nz.txt'
```

If the paths of these python files are not automatically detected, copy/paste this kind of line to your ***.profile*** file in your ***/home*** .

```
export PYTHONPATH="${PYTHONPATH}:your_path_to_repo/utils/:your_path_to_repo/mcmc/:your_path_to_repo/utils/"
```

# Plots

To make plots of different quantities in the halo model with variations of some parameters (p, q, mmin, mmax, alpha), you will need to compile some files in order to create binaries which compute create some data from HM code. All created data is stored in the ***data/*** directory and not deleted because some computations can take some time.

All the scripts to plot the different figures are in ***plots/*** .


## Set up

Plotting need compiled fortran programs. All the binaries needed for that are created by a shell script : 

```
./setupPlots.sh
```

To test the environment, one thing to do is to run the script ***plots/first_plot.py***

Take care of running all the scripts from the root of the working directory : ***hm_mcmc/*** .

## How to use plotting scripts

Scripts will automatically create data if it does nto exist already. Simply run this kind of command:

```
python3 plots/pow3D/differences_alpha.py
```

All plotting scripts allow one optional argument, which is a string.
If the argument is `clean`, then all previous calculated data for this plot will be recalculated. It can be useful if you want to change some paramters in fortran source files for debugging or erase corrupted data without manually deleting all data files :

```
python3 plots/pow3D/differences_alpha.py clean
```

# MCMC

Markov Chain Monte-Carlo is a mathematical technique to see errorbars on parameters from some constraints, correlation and others things. 
It is useful, especially to see the correlation between parameters from a fitting process, if there are more than 1 parameter. In this implementation, we have 2 free parameters : p and q. 

All MCMC programs are a collaboration between fortran and python languages. The fortran one compute the quantities, given the input parameters and these quantities are compared to the value measured by the data. It communicates thanks to the python library ***subprocess*** which enables to call a process from python and catch the standard output (**stdout**) of this subprocess and the output error (**stderr**).

## Set up
To run the MCMC routines, you would have to install the ***emcee*** package, available [there](https://emcee.readthedocs.io), a really easy-to-use python package very well documented and which enables a very simple setup for parallel computations for example.

All MCMC routines are in the files such as ***mcmc/run_cluster_\*.py***. To prepare a run, compilation of fortran sources is needed, but everything is automatized in a shell script: 

```
./setupMCMCs.sh
```

## Run
And then you can launch a MCMC calculation with a command like this one : 

```
python3 mcmc/run_cluster_qp.py 1 3 20
```

Where the first argument is the index of the cosmology, the second is the halo-model code to use( list in ***hmx.f90*** file in Mead's library) and the third one is the number of threads you want to use in parallel.

All cosmologies availabled are in the Fortran library, in the file ***cosmology_functions.f90***

You can change the MCMC parameters (number of walkers and steps) for your run just in the beginning of the python file.


This script writes down be the following files :
- A file containing the best fit informations : ***mcmc/results/{UsedData}/fit{icosmo}.txt***
- A numpy array containing all the walkers steps. It will be saved in a 1D, so the MCMCS parameters(dim, walkers, steps) will be necesarry to use this information. Its path is ***mcmc/results/{UsedData}/chain_\*{icosmo}.npy***


In this MCMC file, one can choose if it wants to find the best fitting parameters only or to run MCMC only. 

## Results

An example-script is available to interpret the data : 

```
python3 mcmc/results_pq.py
```

Please check the arguments it needs in the beginning of the python file. 

# Development

Some files and folders of this project are first steps of development, just to show the way the code has evolved. The concerrning repos are ***speedUpTests*** , ***code_issues*** , ***mcmc/old_mcmcs*** and ***mcmc/first_steps*** .

#  Workspace settings

-  Python formatting used is the autopep8 fiducial formatting, with my values defined in ***.pep8*** file. 

- Fortran one is implemented in ***.vscode/settings.json*** file, using **fpretiffy** formatter manually at each save.

- ***.fortls*** file is to configure the fortran intellisense integrated in vscode by the ***fortran-language-server*** python-package.

- An integrated vscode debugger is also set up in the ***.vscode*** folder.  