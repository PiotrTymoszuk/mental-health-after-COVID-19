# Exports the main figures and supplementary figure references to R markdown file

  insert_head()
  
# Main figures -----
  
  insert_msg('Main figure export')
  
  insert_figure(paper_figures$consort, 
                paper_figures$rf_components, 
                paper_figures$uni_modeling, 
                paper_figures$clustering, 
                paper_figures$clust_mental, 
                paper_figures$pre_cov_da, 
                paper_figures$summary, 
                file = './paper/markdown/figure_chunks.Rmd', 
                ref_names = stri_replace_all(names(paper_figures), fixed = '_', replacement = '-'), 
                captions = c('Study inclusion flow diagram.', 
                             'Random Forest modeling of the mental health and quality of life scoring during COVID-19 convalescence.', 
                             'Association of the most influential factors with the mental health readouts investigated by univariable modeling.', 
                             'Clustering of the study participants by the most influential factors affecting the mental health and quality of life scoring.', 
                             'Mental health and quality of life scoring, depression and anxiety prevalence in the mental disorder risk clusters.', 
                             'Characteristic of baseline features, COVID-19 course and recovery in participants with pre-existing depression or anxiety.', 
                             'Summary of the study results.'))
  
# Supplementary figures -----
  
  insert_msg('Supplementary figure export')
  
  insert_figure(suppl_figures$distribution, 
                suppl_figures$obs_time, 
                suppl_figures$survey, 
                suppl_figures$correlation_responses, 
                suppl_figures$correlation_responses_da, 
                suppl_figures$rforest_anx, 
                suppl_figures$rforest_dpr, 
                suppl_figures$rforest_omh, 
                suppl_figures$rforest_qol, 
                suppl_figures$perc_explained, 
                suppl_figures$rf_performance_da, 
                suppl_figures$clust_qc, 
                suppl_figures$clust_features_summary, 
                suppl_figures$clust_features_heat, 
                file = './paper/markdown/suppl_figure_chunks.Rmd', 
                ref_names = stri_replace_all(names(suppl_figures), fixed = '_', replacement = '-'), 
                captions = c('Value distribution of the mental health and quality of life scores, stress score and physical performance loss.', 
                             'Effect of the observation time on mental health and quality of life scoring.', 
                             'Effect of the survey duration on mental health and quality of life scoring.', 
                             'Inter-correlation of the mental health and quality of life score variables.',
                             'Inter-correlation of the mental health and quality of life score variables in participants with pre-existing depression or anxiety.', 
                             'Random Forest modeling of the anxiety scoring.', 
                             'Random Forest modeling of the depression scoring.', 
                             'Random Forest modeling of the overall mental health scoring.', 
                             'Random Forest modeling of the quality of life scoring.', 
                             'Fraction of mental health and quality of life scoring deviance explained by the common most influential factors.', 
                             'Prediction of mental health and quality of life scoring by Random modeling in participants with pre-existing depression or anxiety.', 
                             'Development of the participant clusters in respect to the common most influential factors for mental health and quality of life scoring.', 
                             'Characteristic of the mental disorder risk clusters.', 
                             'Variables significantly differing between the mental disorder risk clusters.'))
  
# END -----
  
  insert_tail()