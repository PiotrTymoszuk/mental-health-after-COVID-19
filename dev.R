

test <- right_join(cov_data$south, 
                   partclust$clust_results$south$assignment, 
                   by = 'ID')
  
test$sum_symptoms_acute_class

analyze_factor(inp_tbl = clust_char$analysis_tbl$south, 
               variable = 'symptom_free', 
               split_var = 'clust_name')

clust_char$analysis_tbl$south %>% 
  dlply(split_var) %>% 
  map(count_feature, 
      var_to_count = variable, 
      .drop = F)

count_feature(inp_tbl = clust_char$analysis_tbl$south, 
              var_to_count = 'symptom_free', 
              .drop = F)

