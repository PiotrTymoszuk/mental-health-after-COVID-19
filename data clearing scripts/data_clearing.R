# this script performs additional data wrangling tasks: stratification of the symptom sums
# for the risk modeling (acute and persistent)
# source of weights: AGES (https://covid19-dashboard.ages.at/dashboard.html), Ministerio di Saluto
# (https://www.epicentro.iss.it/coronavirus/bollettino/Bollettino-sorveglianza-integrata-COVID-19_7-luglio-2021.pdf)

  insert_head()
  
# data container -----
  
  model_wrangling <- list()

# acute symptom sum stratification by quartiles, merging with the main study tables ----
  
  insert_msg('Stratification of the acute symptom sum')
  
  model_wrangling$sympt_sum_tbls$acute <- cov_data %>% 
    map(strat_quartile, 
        numeric_variable = 'sum_symptoms_acute', 
        new_var_name = 'sum_symptoms_acute_class',
        labels = c('1Q', '2Q', '3Q', '4Q'))
  
# long symptom sum stratification by median and 75th percentile -----
  
  insert_msg('Stratification of the persistent symptom sum')
  
  model_wrangling$sympt_sum_tbls$long <- cov_data %>% 
    map(strat_quartile, 
        numeric_variable = 'sum_symptoms_long', 
        quant_vec = c(0.5, 0.75), 
        new_var_name = 'sum_symptoms_long_class',
        labels = c('0', '1 - 3', '>3'))
  
# merging with the main table ------
  
  insert_msg('Merging with the modeling tables')
  
  cov_data <- map2(cov_data, 
                   model_wrangling$sympt_sum_tbls$acute,
                   left_join, 
                   by = 'ID') %>% 
    map2(., 
         model_wrangling$sympt_sum_tbls$long, 
         left_join, 
         by = 'ID')
  
# reading the data and defining the weights based on age and sex of all Tyrolean and Italian convalescents ------
  
  insert_msg('Defining the modeling weights')

  ## north
  
  model_wrangling$weights$north <- read_delim('./input data/CovidFaelle_Altersgruppe.csv', 
                                              delim = ';')  %>% 
    filter(Bundesland == 'Tirol') %>% 
    mutate(age_strata = Altersgruppe , 
           age_weight_class = paste('age', AltersgruppeID , sep = '_'), 
           sex = ifelse(Geschlecht == 'M', 'male', 'female'), 
           weight_strata_id = paste(sex, age_weight_class, sep = '_'), 
           number_convalescents = AnzahlGeheilt, 
           freq_weight = number_convalescents/sum(number_convalescents)) %>% 
    select(age_weight_class, 
           age_strata, 
           sex, 
           number_convalescents, 
           weight_strata_id, 
           freq_weight)
  
  ## south
  
  model_wrangling$weights$south <- read_excel('./input data/age_sex_dist_italy.xlsx') %>% 
    mutate(number_convalescents_male = male_cases - male_deaths, 
           number_convalescents_female = female_cases - female_deaths, 
           age_weight_class = paste('age', 1:nrow(.) , sep = '_')) %>% 
    gather(key = 'sex', 
           value = 'number_convalescents', 
           number_convalescents_male, 
           number_convalescents_female) %>% 
    mutate(sex = stri_extract(sex, regex = 'male|female'), 
           weight_strata_id = paste(sex, age_weight_class, sep = '_'), 
           freq_weight = number_convalescents/sum(number_convalescents)) %>% 
    select(age_weight_class, 
           age_strata, 
           sex, 
           number_convalescents, 
           weight_strata_id, 
           freq_weight)
  
# merging the weights with the study data -----
  
  ## north
  
  model_wrangling$weight_tbls$north <- cov_data$north %>% 
    select(ID, 
           sex, 
           age) %>% 
    mutate(age_weight_class = cut(age, 
                                  c(-Inf, 4, 14, 24, 34, 44, 54, 64, 74, 84, Inf), 
                                  paste('age', 1:10, sep = '_')), 
           weight_strata_id = paste(sex, age_weight_class, sep = '_')) %>% 
    select(ID, 
           weight_strata_id) %>% 
    left_join(model_wrangling$weights$north %>% 
                select(weight_strata_id, 
                       freq_weight), 
              by = 'weight_strata_id')
  
  ## south
  
  model_wrangling$weight_tbls$south <- cov_data$south %>% 
    select(ID, 
           sex, 
           age) %>% 
    mutate(age_weight_class = cut(age, 
                                  c(-Inf, 9, 19, 29, 39, 49, 59, 69, 79, 89, Inf), 
                                  paste('age', 1:10, sep = '_')), 
           weight_strata_id = paste(sex, age_weight_class, sep = '_')) %>% 
    select(ID, 
           weight_strata_id) %>% 
    left_join(model_wrangling$weights$south %>% 
                select(weight_strata_id, 
                       freq_weight), 
              by = 'weight_strata_id')
  
  ## merging
  
  cov_data <- map2(cov_data, 
                   model_wrangling$weight_tbls, 
                   left_join, 
                   by = 'ID')
  
# calculating the quantile stress classes -------
  
  insert_msg('Calculating the stress score quartiles')
  
  model_wrangling$stress_class <- cov_data %>% 
    map(strat_quartile, 
        numeric_variable = 'stress_score', 
        new_var_name = 'stress_score_class',
        labels = c('1Q', '2Q', '3Q', '4Q'))
  
  cov_data <- map2(cov_data, 
                   model_wrangling$stress_class, 
                   left_join, 
                   by = 'ID')

# calculating the quantile scores for the physical performance loss -----
  
  insert_msg('Calculating the physical performance impairment score')
  
  cov_data <- cov_data %>% 
    map(mutate, 
        perf_impairment_score = car::recode(perf_impairment_class, 
                                            "'0 - 25%' = 0; 
                                            '26 - 50%' = 1; 
                                            '51% - 75%' = 2; 
                                            '76 - 100%' = 3") %>% 
          as.numeric)
  
  
# Acute and Persistent symptom classification as psychosomatic, counting, stratifying by quartiles -----
  
  insert_msg('Psychsomatic acute and persistent symptoms')
  
  model_wrangling$psychosom_sympt <- c('fatigue', 
                                       'fatigue_day', 
                                       'breath_short', 
                                       'dyspnoe', 
                                       'chest_pain', 
                                       'tachycardia', 
                                       'extrasystole', 
                                       'abdominal_pain', 
                                       'nausea', 
                                       'dim_appetite', 
                                       'dizziness', 
                                       'headache', 
                                       'sleep_prob')
  ## counting
  
  model_wrangling$psychosom_acute_sympt_sum <- cov_data %>% 
    map(sum_variables, 
        vars_to_sum = paste(model_wrangling$psychosom_sympt, 
                            'acute', sep = '_'))
  
  model_wrangling$psychosom_long_sympt_sum <- cov_data %>% 
    map(sum_variables, 
        vars_to_sum = paste(model_wrangling$psychosom_sympt, 
                            'long', sep = '_'))
  
  cov_data <- map2(cov_data, 
                   model_wrangling$psychosom_acute_sympt_sum, 
                   function(x, y) mutate(x, psychosom_acute_sympt_sum = y)) %>% 
    map2(., 
         model_wrangling$psychosom_long_sympt_sum, 
         function(x, y) mutate(x, psychosom_long_sympt_sum = y))
  
  ## stratification: acute by quartiles, long by median and 75th percentile
  
  model_wrangling$psychosom_acute_sympt_class <- cov_data %>% 
    map(strat_quartile, 
        numeric_variable = 'psychosom_acute_sympt_sum', 
        labels = c('1Q', '2Q', '3Q', '4Q'), 
        new_var_name = 'psychosom_acute_sympt_class')
  
  model_wrangling$psychosom_long_sympt_class <- list(inp_tbl = cov_data, 
                                                     labels = list(c('0', '1', '2 - 12'), 
                                                                   c('0', '1', '2 - 12'))) %>% 
    pmap(strat_quartile, 
         numeric_variable = 'psychosom_long_sympt_sum', 
         quant_vec = c(0.5, 0.75), 
         new_var_name = 'psychosom_long_sympt_class')
  
  ## merging
  
  cov_data <- map2(cov_data, 
                   model_wrangling$psychosom_acute_sympt_class, 
                   left_join, 
                   by = 'ID') %>% 
    map2(., 
         model_wrangling$psychosom_long_sympt_class, 
         left_join, 
         by = 'ID')
  
# neurocognitive symptoms -----
  
  insert_msg('Neurocognitive symptoms')
  
  model_wrangling$neurocognitive_sympt <- c('confusion', 
                                            'imp_concentration', 
                                            'forgetfulness')
  
  ## symptom counting
  
  model_wrangling$neurocognitive_acute_sympt_sum <- cov_data %>% 
    map(sum_variables, 
        vars_to_sum = paste(model_wrangling$neurocognitive_sympt, 
                            'acute', sep = '_'))
  
  model_wrangling$neurocognitive_long_sympt_sum <- cov_data %>% 
    map(sum_variables, 
        vars_to_sum = paste(model_wrangling$neurocognitive_sympt, 
                            'long', sep = '_'))
  
  cov_data <- map2(cov_data, 
                   model_wrangling$neurocognitive_acute_sympt_sum, 
                   function(x, y) mutate(x, neurocognitive_acute_sympt_sum = y)) %>% 
    map2(., 
         model_wrangling$neurocognitive_long_sympt_sum, 
         function(x, y) mutate(x, neurocognitive_long_sympt_sum = y)) %>% 
    map(mutate, 
        neurocognitive_acute_sympt_class = factor(neurocognitive_acute_sympt_sum), 
        neurocognitive_long_sympt_class = factor(neurocognitive_long_sympt_sum))

# END -----
  
  insert_tail()