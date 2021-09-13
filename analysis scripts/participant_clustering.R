# This script tries to find subsets of the study participants in respect
# to the most influential factors identified by RF and PCA

  insert_head()
  
# tools -----
  
  source('./tools/kohonen_tools.R')
  
  library(furrr)
  
# data container -----
  
  partclust <- list()
  
# globals, analysis tables and SOM grids -----
  
  insert_msg('Globals setup')
  
  ## the most influential variables
  
  partclust$parameters <- rforest_plots$pca %>% 
    purrr::map(function(x) x$top_influential$parameter) %>% 
    unlist %>% 
    unique
  
  partclust$variables <- rforest_plots$pca %>% 
    purrr::map(function(x) x$top_influential$variable) %>% 
    unlist %>% 
    unique
  
  ## mental scoring and prevalence variables
  
  partclust$mental_scoring_vars <- rforest$response
  
  partclust$mental_prev_vars <- c('phq_depression_positive', 
                                  'phq_anxiety_positive')
  
  ## analysis table
  
  partclust$analysis_tbl <- cov_data %>% 
    purrr::map(select, 
               ID, 
               all_of(partclust$variables)) %>% 
    purrr::map(column_to_rownames, 
               'ID') %>% 
    purrr::map(function(x) model.matrix(~., x)) %>% 
    purrr::map(as.data.frame) %>% 
    purrr::map(select, 
               all_of(partclust$parameters))
  
  ## som grid
  
  partclust$som_grid <- somgrid(xdim = floor((5*sqrt(nrow(partclust$analysis_tbl$south)))^0.5), 
                                ydim = floor((5*sqrt(nrow(partclust$analysis_tbl$south)))^0.5), 
                                topo = 'hexagonal', 
                                neighbourhood.fct = 'gaussian', 
                                toroidal = T)
  
# Clustering -----
  
  insert_msg('Particpnant clustering')
  
  partclust$clust_results <- partclust$analysis_tbl %>% 
    purrr::map(clust_fcts, 
               som_grid = partclust$som_grid, 
               dist.fcts = 'tanimoto', 
               rlen = 2000,
               hcl_distance = 'euclidean', 
               k = 3, 
               hc_method = 'ward.D2', 
               seed = 123)
  
  detach(package:kohonen)
  
# cluster re-naming -> find a way to fix the node assignment -----
  
  insert_msg('Cluster re-naming')

  partclust$clust_results$north$assignment <- classify_clusters(partclust$clust_results$north, 
                                                                clust_names = c('LR', 'IR', 'HR'))
  
  partclust$clust_results$south$assignment <- classify_clusters(partclust$clust_results$south, 
                                                                clust_names = c('LR', 'IR', 'HR'))
  
# plotting tables with the clustering and mental health factor variables coded as dummies ----
  
  insert_msg('Plotting tables')
  
  ## clustering features
  
  partclust$plot_tbls <- partclust$analysis_tbl %>% ## clustering variables
    purrr::map(rownames_to_column, 
               'ID') %>% 
    purrr::map(as_tibble)
  
  partclust$plot_tbls <- map2(partclust$plot_tbls, 
                              partclust$clust_results %>% 
                                purrr::map(~.x$assignment), 
                              left_join, 
                              by = 'ID')
  
  ## mental health scoring
  
  partclust$plot_tbls_mental <- cov_data %>%
    purrr::map(select, 
              ID, 
              all_of(partclust$mental_scoring_vars))
  
  partclust$plot_tbls_mental <- map2(partclust$plot_tbls_mental, 
                                     partclust$clust_results %>% 
                                       purrr::map(~.x$assignment), 
                                     left_join, 
                                     by = 'ID')
  
  ## depression and anxiety prevalence
  
  partclust$plot_tbls_depr_anx <- cov_data %>%
    purrr::map(select, 
               ID,
               all_of(partclust$mental_prev_vars)) %>% 
    purrr::map(mutate, 
               phq_depression_positive = car::recode(as.character(phq_depression_positive), 
                                                     "'no' = 0; 'yes' = 1") %>% 
                 as.numeric, 
               phq_anxiety_positive = car::recode(as.character(phq_anxiety_positive), 
                                                  "'no' = 0; 'yes' = 1") %>% 
                 as.numeric)
  
  partclust$plot_tbls_depr_anx <- map2(partclust$plot_tbls_depr_anx, 
                                       partclust$clust_results %>% 
                                         purrr::map(~.x$assignment), 
                                       left_join, 
                                       by = 'ID')

# plotting clustering feature presence/absence in the nodes and clusters as a heat map -----
  
  insert_msg('Plotting the clustering feature presence/absence and mental health scoring as heat maps')
  
  ## clustering features
  
  partclust$hm_clust_features <- list(inp_tbl = partclust$plot_tbls, 
                                      plot_title = globals$cohort_labs[names(partclust$plot_tbls)]) %>% 
    pmap(plot_clust_hm, 
         prevalence = T, 
         features = partclust$variables, 
         plot_subtitle = 'Clustering by top influential factors')
  
  ## mental scoring
  
  partclust$hm_mental_scoring <- list(inp_tbl = partclust$plot_tbls_mental, 
                                     plot_title = globals$cohort_labs[names(partclust$plot_tbls)]) %>% 
    pmap(plot_clust_hm, 
         prevalence = F, 
         features = partclust$mental_scoring_vars, 
         plot_subtitle = 'Clustering by top influential factors', 
         scaling_fun = min_max, 
         midpoint = 0.5, 
         scale_name = 'Frac. max.')
  
  ## depression and anxiety prevalence
  
  partclust$hm_depr_anx <- list(inp_tbl = partclust$plot_tbls_depr_anx, 
                                plot_title = globals$cohort_labs[names(partclust$plot_tbls)]) %>% 
    pmap(plot_clust_hm, 
         prevalence = T, 
         features = partclust$mental_prev_vars, 
         plot_subtitle = 'Clustering by top influential factors')

# Calculating the prevalence and scoring within the clusters -----
  
  insert_msg('Calculating the clustering factor prevalence and average mental health scoring in the clusters')
  
  ## clustering features
  
  partclust$prevalence_clust_features <- partclust$plot_tbls %>% 
    purrr::map(get_prevalence, 
               by = 'clust_name', 
               scoring = F, 
               features = partclust$variables)
  
  ## mental scoring
  
  partclust$prevalence_mental_scoring <- partclust$plot_tbls_mental %>% 
    purrr::map(get_prevalence, 
               by = 'clust_name', 
               scoring = T, 
               features = partclust$mental_scoring_vars)
  
  ## mental disorder prevalence
  
  partclust$prevalence_depr_anx <- partclust$plot_tbls_depr_anx %>% 
    purrr::map(get_prevalence, 
               by = 'clust_name', 
               scoring = F, 
               features = partclust$mental_prev_vars)

# plotting clustering feature, depression and anxiety prevalence in the clusters as bar plots -----
  
  insert_msg('Plotting clustering feature prevalence as bar plots')
  
  ## clustering features
  
  partclust$preval_plots_clust_features <- list(clust_prevalence_list = partclust$prevalence_clust_features, 
                                                plot_title = globals$cohort_labs[names(partclust$prevalence_clust_features)]) %>% 
    pmap(plot_prevalence) %>% 
    map(function(x) x + 
          scale_x_continuous(limits = c(0, 115), 
                             breaks = seq(0, 100, by = 20)))
  
  ## mental health disorders
  
  partclust$preval_plots_depr_anx <- list(clust_prevalence_list = partclust$prevalence_depr_anx, 
                                                plot_title = globals$cohort_labs[names(partclust$prevalence_clust_features)]) %>% 
    pmap(plot_prevalence) %>% 
    map(function(x) x + 
          scale_x_continuous(limits = c(0, 60), 
                             breaks = seq(0, 60, by = 10)))
  
# plotting mental health scoring in the clusters -----
  
  insert_msg('Plotting mental health scoring in the clusters')
  
  partclust$mental_score_clust_plots <- list(clust_prevalence_list = partclust$prevalence_mental_scoring, 
                                             plot_title = globals$cohort_labs[names(partclust$prevalence_mental_scoring)]) %>% 
    pmap(plot_scoring, 
         show_points = F)

# END ----

  insert_tail()