# A mother script executing the particular ML analyses

# tools -----

  library(caret)
  library(caretExtra) ## available from https://github.com/PiotrTymoszuk/caretExtra
  library(doParallel)
  library(lmqc)
  library(qgam)

  c('./tools/cov_project_tools.R', 
    './tools/cov_project_globals.R', 
    './tools/clust_tools2.R') %>% 
    source_all(message = TRUE, 
               crash = TRUE)

# analysis scripts ----

  c('./modeling scripts/rf_modeling.R', ## development of the Random Forest models
    './modeling scripts/rf_plots.R', ## plotting of the RF model features
    './modeling scripts/rf_pca.R', ## PCA of the most influential explanatory variables 
    './modeling scripts/psych_soc_measures.R', ## univariate modeling for the most influential features
    './modeling scripts/explained_variance.R', ## fraction explained variance in multi-paramater Poisson modeling
    './modeling scripts/predictions_da.R') %>% ## RF predictions of mental scoring in the depression+/- subsets
    source_all(message = TRUE, 
               crash = TRUE)

# END -----