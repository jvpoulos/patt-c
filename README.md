# patt-c

This repository provides data and code for reproducing ["Estimating population average treatment effects from experiments with noncompliance"](https://www.degruyter.com/document/doi/10.1515/jci-2018-0035/html).

Please cite the paper if you use this code for academic research:

```
@article{ottoboni2020estimating,
  title={Estimating population average treatment effects from experiments with noncompliance},
  author={Ottoboni, Kellie N and Poulos, Jason V},
  journal={Journal of Causal Inference},
  volume={8},
  number={1},
  pages={108--130},
  year={2020},
  publisher={De Gruyter}
}
```
Description of scripts in code/
------
* `package-list.R` install required packages 
* `main.R` main run list 
  * `main.sh` shell script for main.R
* `simulation.R` runs simulations and produces `results/simulation_res.Rdata`
* `simulation-plots.R` loads `results/simulation_res.Rdata` and saves to `plots/`
* `prepare-ohie.R` merges OHIE data and saves to `data/prepare-ohie.Rdata`
 `prepare-NHIS.R` merges NHIS data and saves to `data/prepare-NHIS.Rdata`
* `prepare-analysis.R` imports NHIS and OHIE datasets and creates outcome vectors and common covariates for the analysis; saves to `data/prepare-analysis.Rdata`
* `analysis.R` produces empirical estimates; change `run.model` to TRUE to train complier and response models; saves to `data/analysis.Rdata`
  * `SuperLearner.R` Super learner helper functions
  * `complier-mod.R` Fit complier model if `run.model` is TRUE
  * `complier-mod-cv.R` Cross-validate accuracy of complier model if `run.model` is TRUE
  * `response-mod.R` Fit response models if `run.model` is TRUE
  * `wtc.R` function for weighted t-test with cluster-bootstrapped SEs
 * `rct-nrt-compare.R` produce estimates for Tables A1 and A2
 * `estimator-compare-plots.R` saves heterogeneous treatment effect estimates to `plots/`    
 * `placebo-test.R` produce estimates for Table A3

Instructions
------
* Clone a copy of the repository to your working directory with the command
```
$ git clone https://github.com/jvpoulos/patt-c
```
* The code uses **R** version 3.5.2 (2018-12-20). To install this **R** version on Ubuntu, use the command 
```
$ sudo apt-get install r-base-core=3.5.2-1xenial
```
* Download and extract pretrained response models to `results/` directory:
  * [response-mod.tar.xz](https://www.dropbox.com/s/d53jcpj9vbo0tex/response-mod.tar.xz?dl=1)
  * [response-mod-patt.tar.xz](https://www.dropbox.com/s/srgvn4oa2ugmf8p/response-mod-patt.tar.xz?dl=1)
* Open `package-list.R` in a script editor
  * Verify that all required packages in `package-list.R`are installed in your **R** library
* Open `main.R` in a script editor
  * Change the file path specified by `repo.directory` to your working directory
  * Change `patient.simulation` to TRUE to run simulation
  * Save your changes to `main.R`
* Make shell file `main.sh` executable from the Linux/Unix command line:
```
$ chmod +x main.sh
```
* Execute the file:
```
$ ./main.sh > main.txt
```
