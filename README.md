# Exploring the impact of Propensity Score Weighting for causal inference: a simulation and case study.

#### *Riccardo Bianchini*

### Abstract
Randomised Controlled Trials (RCTs) are considered the gold standard to establish the cause-and-effect relationship between an intervention and an outcome. However, they are not always feasible or the right approach to answer a specific research question. On the other side, observational studies are more practical, but face challenges in addressing confounding variables. Propensity Score (PS) methods offer an effective solution to this by balancing covariates between treatment groups, therefore mitigating the impact of confounding variables and enhancing the validity of causal inferences in observational studies. In this research two different PS weighting methods, Inverse Probability Weighting (IPW) with trimming and Overlap Weighting (OW), were compared in terms of covariate balance and estimation bias, through the use of an exemplary case study and Monte Carlo simulations. Results showed that OW consistently achieved superior covariate balance and lower bias in estimating treatment effect compared to IPW with trimming, across several sample sizes and treatment prevalences. Overall, this investigation underscored the advantage of using OW in addressing exreme propensity scores, and emphasized the importance of choosing the right method to obtain robust and reliable results in observational studies.

## Package information

The package `PW.case.study.simulations` contains all the functions used for analysing the exemplary case study and for building the Monte Carlo simulation approach.

The file `Case_study_analysis.Rmd` reports the analysis conducted on the case study and the obtained results.

All the analysis were conducted using RStudio (v4.1.2).
