# This script provides globals and import functions 

# toolbox ----

  require(readr)
  require(readxl)
  require(tidyverse)
  require(stringi)
  require(soucer)

# Data containers ----

  globals <- list()

# graphics theme -----

  globals$present_scale <- c(yes = 'firebrick4', 
                             no = 'steelblue4')
  
  globals$reg_scale <- c(positive = 'firebrick3', 
                         negative = 'steelblue3', 
                         ns = 'gray60')
  
  globals$common_text <- element_text(size = 8, 
                                      face = 'plain', 
                                      color = 'black')
  
  globals$common_margin <- ggplot2::margin(t = 5, 
                                           l = 4, 
                                           r = 4, 
                                           unit = 'mm')
  
  globals$common_theme <- theme_classic() + theme(axis.text = globals$common_text, 
                                                  axis.title = globals$common_text, 
                                                  plot.title = element_text(size = 8, 
                                                                            face = 'bold'), 
                                                  plot.subtitle = globals$common_text, 
                                                  plot.tag = element_text(size = 8, 
                                                                          face = 'plain', 
                                                                          color = 'black', 
                                                                          hjust = 0, 
                                                                          vjust = 1), 
                                                  plot.tag.position = 'bottom', 
                                                  legend.text = globals$common_text, 
                                                  legend.title = globals$common_text, 
                                                  strip.text = globals$common_text,
                                                  strip.background = element_rect(fill = 'gray95', 
                                                                                  color = 'gray80'), 
                                                  plot.margin = globals$common_margin, 
                                                  panel.grid.major = element_line(color = 'gray90'))

# Co-morbidities -----
  
  globals$comorb <- c('autoimmunity', 
                      'anemia', 
                      'hypertension', 
                      'depression_burnout',
                      'diabetes', 
                      'pre_cov_epilepsy',
                      'dementia', 
                      'surgery_6m_before', 
                      'frequent_flu_like', 
                      'heart_circulation', 
                      'hay_fever', 
                      'malignancy', 
                      'pins_needles_feet', 
                      'gastrointenstinal', 
                      'lung', 
                      'two_plus_infections_antibiotics', 
                      'multiple_sclerosis', 
                      'night_dyspnoe', 
                      'bruxism', 
                      'kidney', 
                      'parkinson', 
                      'insomnia', 
                      'stroke', 
                      'embolism', 
                      'transplantation')

# Daily medication -----
  
  globals$daily_medication <- c('daily_immunosuppression', 
                                'daily_cortison', 
                                'daily_ace_drugs', 
                                'daily_pain_killers', 
                                'daily_anti_coagulation')
  
# self-reported symptoms -------
  
  globals$symptoms <- c('fever', 
                        'ague', 
                        'sore_throat', 
                        'running_nose', 
                        'fatigue', 
                        'dry_cough', 
                        'wet_cough', 
                        'breath_short', 
                        'dyspnoe', 
                        'chest_pain', 
                        'tachycardia', 
                        'extrasystole', 
                        'joint_pain', 
                        'bone_pain', 
                        'muscle_pain', 
                        'abdominal_pain', 
                        'nausea', 
                        'vomiting', 
                        'dim_appetite', 
                        'diarrhea', 
                        'dizziness', 
                        'headache', 
                        'anosmia', 
                        'taste_loss', 
                        'confusion', 
                        'tingle_feet', 
                        'tingle_hands', 
                        'ache_feet', 
                        'ache_hands', 
                        'numb_feet', 
                        'numb_hands', 
                        'unhandiness_walk', 
                        'unhandiness_micromotor', 
                        'sleep_prob', 
                        'fatigue_day', 
                        'imp_concentration', 
                        'forgetfulness', 
                        'epilepsy_covid', 
                        'swelling', 
                        'blue_fingers', 
                        'urticaria', 
                        'blister_rash', 
                        'net_rash', 
                        'red_eyes')

# colors, tiles, descriptions and labels, read from the variable lexicon ------  
  
 globals$var_lexicon <- read_excel('./input data/variable_lexicon.xlsx')
  
# cohort colors and labels -----
  
  globals$cohort_labs <- c(north = 'AT', 
                           south = 'IT')
  
  globals$cohort_colors <- globals$var_lexicon %>% 
    filter(variable == 'cohort') %>% 
    .$level_colors %>% 
    stri_split_fixed(pattern = ', ') %>% 
    unlist %>% 
    set_names(names(globals$cohort_labs))
  
# Risk clusters ----
  
  ## cluster colors and labels
  
  globals$clust_labs <- c('LR', 'IR', 'HR')
  
  globals$clust_colors <- c('darkolivegreen4', 
                            'lightblue4', 
                            'indianred4') %>% 
    set_names(globals$clust_labs)
  
# Responses, modeling variables and their graphical representation -----
  
  ## responses

  globals$response <- globals$var_lexicon %>% 
    filter(response == 'yes') %>% 
    .$variable
  
  ## independent variables
  
  globals$variables <- globals$var_lexicon %>% 
    filter(modeling_variable == 'yes') %>% 
    .$variable
  
  ## prediction types
  
  globals$pred_labs <- c(globals$cohort_labs['north'], 
                         cv = 'CV', 
                         globals$cohort_labs['south'])
  
  globals$pred_colors <- c(globals$cohort_colors['north'], 
                           cv = 'plum4', 
                           globals$cohort_colors['south'])
  
  
  globals$response_labels <- c('mental_health_score' = 'OMH', 
                               'life_quality_score' = 'QoL', 
                               'phq_depression_score' = 'DPR', 
                               'phq_anxiety_score' = 'ANX')
  
  globals$response_colors <- c('mental_health_score' = 'lightskyblue3', 
                               'life_quality_score' = 'darkseagreen3', 
                               'phq_depression_score' = 'cadetblue3', 
                               'phq_anxiety_score' = 'gray60')


# END ----
