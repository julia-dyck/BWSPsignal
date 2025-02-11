# BWSPsignal

This repository provides the BWSPsignal R package and a "/simulationstudy" folder to tune and apply the Bayesian Power generalized Weibull Shape Parameter (BPgWSP) test.

The BPgWSP test was developed to perform signal detection for adverse event monitoring based on electronic health records.

The BPgWSP test was developed and presented in

Dyck, J., & Sauzet, O. (2024). **The BPgWSP test: a Bayesian Weibull Shape Parameter signal detection test for adverse drug reactions.** arXiv preprint arXiv:2412.05463.

available on [https://arxiv.org/abs/2412.05463](https://arxiv.org/abs/2412.05463).

## Installation of the BWSPsignal package
You can install the BWSPsignal package with

``` r
# install.packages("devtools") # if not installed, yet
devtools::install_github(repo = "julia-dyck/BWSPsignal")
```

## Reproduction of simulation study
To reproduce the simulation study presented in [Dyck & Sauzet (2024)](https://arxiv.org/abs/2412.05463) fork the repository, go to the release tag v1.0.0 and follow the instructions in "BWSPsignal/simulationstudy/README".

## Tuning of the PgWSP test for your purpose
To tune the PgWSP test for your application you can use the folder in "BWSPsignal/simulationstudy/" as a template, and adjust sample scenarios, and test alternatives accordingly.

