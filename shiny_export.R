# Exports the data to be displayed in the dashboard

  insert_head()
  
# exporting ----
  
  insert_msg('App data export')

  save(cov_data, 
       file = 'survey_data.RData')
  
  clust_assignment <- partclust$clust_objects %>% 
    map(extract, 'assignment') %>% 
    map(mutate, 
        ID = observation, 
        clust_name = clust_id) %>% 
    map(select, 
        ID, clust_name)
  
  save(clust_assignment, 
       file = 'clust_assign.RData')
  
# END -----
  
  insert_tail()