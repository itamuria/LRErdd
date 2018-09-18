# LRErdd
Regression Discontinuity as Local Randomized Experiments

We present the R LRErdd package with a case study. The package includes a set of functions for the design and analysis of Regression Discontinuity Designs as local randomized experiments within the potential outcome approach as formalized in Li et al (2015). 

A sub-set of functions implements the design phase of the study where focus is on the selection of suitable subpopulations for which we can draw valid causal inference. 
These functions provide summary statistics of pre-and post-treatment variables by treatment status, and select suitable subpopulations around the threshold where pre-treatment variables are well balanced between treatment using randomization-based tests with adjustment for multiplicities. Functions for a visual inspection of the results are also provided. 

Finally the LRErdd package includes a set of functions for drawing inference on causal effects for the selected subpopulations using randomization-based modes of inference. Specifically the Fisher Exact $p-$value and Neyman approaches are implemented for the analysis of both sharp and fuzzy RD designs. We illustrate our approach in a study concerning the effects of University grants on student dropout.
                                                   
Li F, Mattei A, Mealli F (2015). Bayesian inference for regression discontinuity designs withapplication to the evaluation of Italian university grants. The Annals of Applied Statistics,9(4), 1906-1931."))