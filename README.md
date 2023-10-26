# csmparameter

The purpose of this package is to provide parameter sensitivity analysis and estimation capability for the Decision Support System for Agrotechnology Transfer Cropping Systems Model (DSSAT-CSM). The initial goal for the package will be to provide cross-platform functions for:

- Global sensitivity analysis using the `sensitivity` R package
- Parameter estimation using:

    - `optim()` from the `stats` R package
    - Differential Evolution as impemented in the `DEoptim` R package
    - Generalized Likelihood Uncertainty Estimation (GLUE) as described in He et al. (2010)
    
