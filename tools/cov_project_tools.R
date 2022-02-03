# This script provides tools for the COV-Questionnaire project


# libraries and data ----

   require(ggvenn)
   require(UpSetR)
   require(doParallel)
   require(caret)
   require(meta)
   require(furrr)
   require(ggplotify)
   require(nVennR)
   require(cowplot)
   require(magick)

# Random forest, fit stats and variable importance ------
   
   plot_ml_stats <- function(data, 
                             stat = 'RMSE', 
                             plot_order = c('train', 'cv', 'test'), 
                             plot_title = NULL, 
                             plot_subtitle = NULL, 
                             plot_tag = NULL, 
                             y_lab = 'RMSE') {
      
      ## plots the given statistic for mental health response prediction
      ## data is a fit summary table of caretx objects
      
      data %>% 
         mutate(prediction = factor(prediction, 
                                    levels = plot_order)) %>% 
         filter(.data[['statistic']] == stat) %>% 
         ggplot(aes(x = response, 
                    y = estimate, 
                    color = prediction, 
                    group = prediction, 
                    shape = prediction)) + 
         geom_line() + 
         geom_point(size = 2) + 
         globals$common_theme + 
         theme(axis.title.x = element_blank()) + 
         labs(title = plot_title, 
              subtitle = plot_subtitle, 
              tag = plot_tag, 
              y = y_lab)
      
   }

   extract_rf_importance <- function(caret_model) {
      
      ## extracts a summary table with the impacts
      
      impact_tbl <- varImp(caret_model)$importance %>% 
         rownames_to_column('parameter') %>% 
         set_names(c('parameter', 
                     'delta_mse')) %>% 
         as_tibble
      
      ## extracting the variable and level names
      
      extr_regex <- globals$var_lexicon %>% 
         filter(modeling_variable == 'yes') %>% 
         .$variable %>% 
         paste(collapse = '|')
      
      impact_tbl %>% 
         mutate(variable = stri_extract(parameter, 
                                        regex = extr_regex), 
                level = stri_replace_all(parameter, 
                                         regex = extr_regex, 
                                         replacement = ''), 
                variable_label = translate_var(variable), 
                variable_label = ifelse(level %in% c('', 'yes'), 
                                        variable_label, 
                                        paste(variable_label, level, sep = ': ')))

   }

   plot_bar_rf <- function(inp_tbl, 
                           plot_title = NULL, 
                           plot_subtitle = NULL, 
                           plot_tag = NULL, 
                           x_lab = NULL, 
                           tag_position = 'bottom', 
                           x_transf = 'identity', 
                           fill_color = 'cornsilk') {
      
      ## makes a basic bar plot with delta MSE results obtained by random forest modeling
      
      bar_plot <- inp_tbl %>% 
         ggplot(aes(x = delta_mse, 
                    y = reorder(variable_label, delta_mse))) +
         geom_bar(stat = 'identity', 
                  color = 'black', 
                  fill = fill_color) + 
         scale_x_continuous(trans = x_transf) + 
         globals$common_theme + 
         theme(axis.title.y = element_blank(), 
               plot.tag.position = tag_position) + 
         labs(title = plot_title, 
              subtitle = plot_subtitle, 
              x = x_lab, 
              tag = plot_tag)
      
      return(bar_plot)
      
      
   }
   
   plot_n_venn <- function(data, 
                           subset_names = NULL, 
                           fill_color = unname(overlap$var_colors[overlap_vars]), 
                           plot_title = NULL, 
                           plot_tag = NULL, 
                           legend_position = 'right', 
                           opacity = 0.2, 
                           borderWidth = 2, 
                           labelRegions = FALSE, 
                           fontScale = 3,
                           plot_leg_ratio = c(0.9, 0.1), ...) {
      
      ## plots a Venn diagram employing nVennR
      
      subset_names <- stri_replace(subset_names, fixed = '\n', replacement = ' ')
      
      if(!is.null(subset_names)) set_names(data, subset_names)
      
      temp_file <- sample(LETTERS, 5, replace = TRUE) %>% 
         paste(collapse = '') %>% 
         paste0('.svg')
      
      venn_plot <- plotVenn(sets = data, 
                            showPlot = TRUE, 
                            systemShow = FALSE, 
                            showLegend = FALSE, 
                            outFile = temp_file, 
                            setColors = fill_color, 
                            opacity = opacity, 
                            borderWidth = borderWidth, 
                            labelRegions = labelRegions, 
                            fontScale = fontScale, ...)
      
      gg_plot <- plot_grid(ggdraw() + 
                              draw_image(image_read_svg(temp_file)))
      
      file.remove(temp_file)
      
      gg_plot <- plot_grid(ggdraw() + 
                              draw_text(text = plot_title, 
                                        size = 8, 
                                        fontface = 'bold'), 
                           gg_plot, 
                           ggdraw() + 
                              draw_text(text = plot_tag, 
                                        size = 8, 
                                        fontface = 'plain'), 
                           nrow = 3, 
                           rel_heights = c(0.08, 0.82, 0.06))
      
      ## the legend will be obtain ed from a 'fake plot'
      
      gg_legend <- tibble(subset = factor(subset_names, levels = rev(subset_names)), 
                          n = 1) %>% 
         ggplot(aes(x = subset_names, 
                    y = n, 
                    fill = subset_names)) + 
         geom_bar(stat = 'identity', 
                  alpha = if(2.5*opacity > 1) 1 else 2.5*opacity, 
                  size = borderWidth) + 
         scale_fill_manual(values = rev(tolower(fill_color)), 
                           name = '') + 
         globals$common_theme + 
         theme(legend.position = legend_position)
      
      gg_legend <- get_legend(gg_legend)
      
      switch(legend_position, 
             'right' = plot_grid(gg_plot, 
                                 gg_legend, 
                                 ncol = 2, 
                                 rel_widths = plot_leg_ratio) + 
                theme(plot.margin = globals$common_margin), 
             'bottom' = plot_grid(gg_plot, 
                                  gg_legend, 
                                  nrow = 2, 
                                  rel_heights = plot_leg_ratio) + 
                theme(plot.margin = globals$common_margin))
      
   }

   plot_univar_forest <- function(data_inference,
                                  data_fit, 
                                  variables, 
                                  mod_response = 'life_quality_score', 
                                  plot_title = translate_var(mod_response), 
                                  plot_subtitle = NULL) {
      
      ## plots results of the univariate modeling as a Forest plot with color coding for the cohorts
      ## presents Rsq as a bar plot
      
      ## plot meta
      
      data_list <- list(inference = data_inference, 
                        fit = data_fit) %>%
         map(filter, 
             variable %in% variables, 
             response == mod_response) %>% 
         map(mutate, 
             variable = translate_var(variable), 
             cohort = factor(cohort, c('south', 'north')))
      
      ## ordering by the highest Rsq
      
      plot_order <- data_list$inference %>% 
         arrange(estimate) %>% 
         mutate(plot_order = 1:nrow(.))
      
      data_list <- data_list %>% 
         map(left_join, plot_order[c('variable', 'plot_order', 'cohort')], by = c('variable', 'cohort'))

      ## n numbers         

      plot_tag <- data_list$inference %>% 
         dlply(.(cohort), 
               function(x) min(x$n_complete))
      
      plot_tag <- paste0('\nAT: n = ', 
                         plot_tag$north, 
                         ', IT: n = ', 
                         plot_tag$south)
      
      ## forest plot
      
      forest_plot <- data_list$inference %>% 
         ggplot(aes(x = estimate, 
                    y = reorder(variable, plot_order), 
                    shape = cohort, 
                    color = cohort)) +
         geom_vline(xintercept = 1, 
                    linetype = 'dashed') + 
         geom_errorbarh(aes(xmin = lower_ci, 
                            xmax = upper_ci), 
                        height = 0, 
                        position = position_dodge(0.5), 
                        size = 0.75) + 
         geom_point(size = 2.5, 
                    position = position_dodge(0.5)) + 
         scale_shape_manual(values = c(north = 15, 
                                       south = 16), 
                            labels = globals$cohort_labs, 
                            name = '') + 
         scale_color_manual(values = globals$cohort_colors, 
                            labels = globals$cohort_labs, 
                            name = '') + 
         globals$common_theme + 
         theme(axis.title.y = element_blank(), 
               panel.grid.major = element_line(color = 'gray90')) + 
         labs(title = plot_title, 
              subtitle = plot_subtitle, 
              tag = plot_tag, 
              x = expression('exp '*beta))
      
      rsq_plot <- data_list$fit %>% 
         ggplot(aes(x = adj_rsq, 
                    y = reorder(variable, plot_order), 
                    fill = cohort)) + 
         geom_bar(stat = 'identity', 
                  position = position_dodge(width = 0.9), 
                  color = 'black') + 
         scale_fill_manual(values = globals$cohort_colors, 
                           labels = globals$cohort_labs, 
                           name = '') + 
         scale_x_continuous(breaks = c(0, 0.1, 0.2)) + 
         globals$common_theme + 
         theme(axis.title.y = element_blank()) + 
         labs(title = plot_title, 
              subtitle = plot_subtitle, 
              x = expression('adjusted R'^2))
      
      ## plot panel
      
      list(panel = plot_grid(forest_plot + 
                                theme(legend.position = 'none', 
                                      plot.margin = ggplot2::margin(t = 5, r = 2, b = 0, l = 4, unit = 'mm')), 
                             rsq_plot + 
                                theme(axis.text.y = element_blank(), 
                                      plot.title = element_blank(), 
                                      legend.position = 'none', 
                                      plot.margin = ggplot2::margin(t = 5, r = 4, b = 0, l = 2, unit = 'mm')), 
                             ncol = 2, 
                             align = 'h', 
                             rel_widths = c(0.75, 0.25)), 
           legend = get_legend(rsq_plot + 
                                  theme(legend.position = 'bottom')))
 
   }
   
   plot_frac_dev <- function(lm_analysis_object_north,
                             lm_analysis_object_south, 
                             plot_title = NULL) {
      
      ## plots percent explained deviance
      
      ## plotting table and n number
      
      plotting_tbl <- list(north = lm_analysis_object_north,
                           south = lm_analysis_object_south) %>% 
         map(anova)
      
      n_number <- list(north = lm_analysis_object_north,
                       south = lm_analysis_object_south) %>% 
         map(lmqc::nobs)
      
      unexplained <- plotting_tbl %>% 
         map(filter, variable == 'Residuals') %>% 
         map(~.x$frac_explained)
      
      plotting_tbl <- plotting_tbl %>% 
         map2_dfr(., names(.), ~mutate(.x, cohort = .y)) %>% 
         mutate(cohort = factor(cohort, c('south', 'north')))
      
      plot_subtitle <- paste0('Explained deviance fraction:\nAT: ', 
                              signif(1 - unexplained$north, 2), 
                              ', IT: ', signif(1 - unexplained$south, 2))
      
      ## plot
      
      plotting_tbl %>% 
         filter(!variable %in% c('NULL', 'Residuals')) %>% 
         mutate(variable = translate_var(variable)) %>% 
         ggplot(aes(x = frac_explained, 
                    y = reorder(variable, frac_explained), 
                    fill = cohort)) + 
         geom_bar(stat = 'identity', 
                  color = 'black', 
                  position = position_dodge(width = 0.9)) + 
         scale_fill_manual(values = globals$cohort_colors, 
                           labels = globals$cohort_labs) + 
         globals$common_theme + 
         theme(axis.title.y = element_blank()) + 
         labs(title = plot_title, 
              subtitle = plot_subtitle, 
              tag = paste0('\nAT: n = ', n_number$north, 
                           ', IT: n = ', n_number$south), 
              x = 'fraction of total deviance')
      
   }
   
# GAM modeling -----
   
   model_gam <- function(data_list, 
                         formulas, 
                         family = 'poisson') {
      
      ## simple serial GAM modeling with cubic splines
      
      ## models
      
      models <- data_list %>% 
         map(function(cohort) formulas %>% 
                map(gam, 
                    family = family, 
                    data = cohort))
      
      ## model summaries
      
      test_results <- models %>% 
         map(~map(.x, summary) %>% 
                map2_dfr(., names(.), ~tibble(variable = .y, 
                                              p_value = .x[['s.pv']], 
                                              edf = .x[['edf']], 
                                              rsq = .x[['r.sq']], 
                                              expl_dev = .x[['dev.expl']]))) %>% 
         map2_dfr(., names(.), ~mutate(.x, cohort = .y))
      
      ## p value correction
      
      test_results %>% 
         mutate(p_adjusted = p.adjust(p_value, 'BH'), 
                significance = ifelse(p_adjusted < 0.05, 
                                      ifelse(p_adjusted < 0.001, 
                                             'p < 0.001', 
                                             paste('p =', signif(p_value, 2))), 
                                      paste0('ns (p = ', signif(p_value, 2), ')')), 
                plot_sub = paste(significance, ', R\u00B2 = ', signif(rsq, 2)))
      
      
   }
   
# Feature comparison accessories ------
   
   plot_volcano <- function(data,
                            estimate_var = 'estimate', 
                            eff_index = 'strong_var', 
                            effect_cutoff = 0.3, 
                            plot_title = NULL, 
                            plot_subtitle = NULL, 
                            plot_tag = NULL, 
                            x_lab = 'Effect size', 
                            eff_lab = 'Moderate-to-strong\neffect', 
                            jitter_w = 0.02, 
                            jitter_h = 0.02) {
      
      ## plots a Volcano plot with the results of Kruskal 
      ## or Chi-Squared test.
      
      data %>% 
         ggplot(aes(x = .data[[estimate_var]], 
                    y = -log10(p_adjusted), 
                    fill = .data[[eff_index]])) + 
         geom_hline(yintercept = -log10(0.05), 
                    linetype = 'dashed') + 
         geom_vline(xintercept = effect_cutoff, 
                    linetype = 'dashed') + 
         geom_point(shape = 21, 
                    size = 2, 
                    position = position_jitter(width = jitter_w, 
                                               height = jitter_h), 
                    alpha = 0.5) +
         geom_text_repel(aes(label = plot_lab), 
                         size = 2.75) + 
         scale_fill_manual(values = c('no' = 'gray60', 
                                      'yes' = 'indianred3'), 
                           name = eff_lab) + 
         globals$common_theme +
         labs(title = plot_title, 
              subtitle = plot_subtitle, 
              tag = plot_tag, 
              x  = x_lab, 
              y = expression('-log'[10]*' pFDR'))
      
      
   }
   
   plot_hm <- function(data, 
                       clust_var = 'clust_id', 
                       variables = clust_ft$cmm_factors, 
                       plot_title = NULL, 
                       plot_subtitle = NULL, 
                       plot_tag = NULL, 
                       split_var_type = TRUE) {
      
      ## plots variables of interest as a heat map
      
      plotting_tbl <- data[variables] %>% 
         map_dfc(as.numeric) %>% 
         map_dfc(min_max)
      
      plotting_tbl <- plotting_tbl %>% 
         mutate(clust_id = data[[clust_var]], 
                ID = data$ID) %>% 
         gather(key = 'feature', 
                value = 'expression',
                all_of(variables)) %>% 
         mutate(var_type = ifelse(stri_detect(feature, fixed = 'acute') & !stri_detect(feature, fixed = 'subacute'), 
                                  'acute symptoms', 
                                  ifelse(stri_detect(feature, fixed = 'subacute'), 
                                         'sub. symptoms', 
                                         ifelse(stri_detect(feature, fixed = 'long'), 
                                                'persist. symptoms', 
                                                ifelse(stri_detect(feature, regex =  'phq|mental|quality'), 
                                                       'mental', 
                                                       'other')))), 
                var_type = factor(var_type, c('other', 'mental', 'persist. symptoms', 'sub. symptoms', 'acute symptoms')), 
                feature = translate_var(feature))
      
      hm <- plotting_tbl %>% 
         ggplot(aes(x = ID, 
                    y = feature, 
                    fill = expression)) + 
         geom_tile()
      
      if(split_var_type) {
         
         hm <- hm + 
            facet_grid(var_type ~ clust_id, 
                       scales = 'free', 
                       space = 'free')
         
      } else {
         
         hm <- hm + 
            facet_grid(. ~ clust_id, 
                       scales = 'free', 
                       space = 'free')
         
      }
      
      hm + 
         scale_fill_gradient2(low = 'steelblue', 
                              mid = 'black', 
                              high = 'firebrick', 
                              midpoint = 0.5, 
                              name = 'Feature:\n\n0: min/absent\n1: max/present\n') + 
         globals$common_theme + 
         theme(axis.title.y = element_blank(), 
               axis.text.x = element_blank(), 
               axis.ticks.x = element_blank(), 
               axis.line.x = element_blank()) + 
         labs(title = plot_title, 
              subtitle = plot_subtitle,
              tag = plot_tag, 
              x = 'Partcipant')
      
   }
   
   plot_correlogram <- function(data, 
                                plot_title = NULL, 
                                plot_subtitle = NULL, 
                                plot_tag = NULL, 
                                fill_lab = 'rho', 
                                size_lab = 'abs(rho)') {
      
      data %>% 
         mutate(font_face = ifelse(p_adjusted < 0.05, 'bold', 'plain'), 
                estimate_lab = ifelse(estimate == 1, NA, signif(estimate, 2)))  %>% 
         ggplot(aes(x = translate_var(variable1), 
                    y = translate_var(variable2), 
                    fill = estimate, 
                    size = abs(estimate))) + 
         geom_point(shape = 21) + 
         geom_text(aes(label = estimate_lab, 
                       fontface = font_face), 
                   size = 2.75, 
                   hjust = -0.3) + 
         scale_fill_gradient2(low = 'steelblue', 
                              mid = 'white', 
                              high = 'firebrick', 
                              midpoint = 0, 
                              limits = c(-1, 1), 
                              name = fill_lab) + 
         scale_size_area(limits = c(0, 1), 
                         name = size_lab, 
                         max_size = 4) + 
         globals$common_theme + 
         theme(axis.title = element_blank()) + 
         labs(title = plot_title, 
              subtitle = plot_subtitle,
              tag = plot_tag)
      
      
   }
   
   violin_panel <- function(data, 
                            variables, 
                            clust_var = 'clust_id', 
                            plot_title = NULL, 
                            plot_subtitle = NULL, 
                            plot_tag = NULL, 
                            split_var_type = TRUE, 
                            split_by_cluster = TRUE, 
                            violin_alpha = 0.25, 
                            plot_iqr = TRUE) {
      
      plotting_tbl <- data[variables] %>% 
         map_dfc(as.numeric) %>% 
         map_dfc(min_max)
      
      plotting_tbl <- plotting_tbl %>% 
         mutate(clust_id = data[[clust_var]], 
                ID = data$ID) %>% 
         gather(key = 'feature', 
                value = 'expression',
                all_of(variables)) %>%
         mutate(feature = translate_var(feature), 
                var_type = ifelse(stri_detect(feature, regex = 'acute|long'), 
                                  'symptoms', 
                                  ifelse(stri_detect(feature, fixed = 'score'), 
                                         'mental', 
                                         'pre-CoV')), 
                var_type = factor(var_type, c('pre-CoV', 'mental', 'symptoms')))
      
      median_tbl <- plotting_tbl %>% 
         group_by(feature, clust_id, var_type) %>% 
         summarise(perc25 = quantile(expression, 0.25, na.rm = TRUE), 
                   perc75 = quantile(expression, 0.75, na.rm = TRUE), 
                   expression = median(expression, na.rm = TRUE))
      
      vio_panel <- plotting_tbl %>% 
         ggplot(aes(x = expression, 
                    y = feature, 
                    fill = clust_id, 
                    color = clust_id)) + 
         geom_violin(scale = 'width', 
                     alpha = violin_alpha, 
                     color = 'black')
      
      if(plot_iqr) {
         
         vio_panel <- vio_panel + 
            geom_errorbarh(data = median_tbl, 
                           aes(xmin = perc25, 
                               xmax = perc75), 
                           color = 'orangered3', 
                           height = 0, 
                           position = position_dodge(0.9))
         
      }
      
      vio_panel + 
         geom_point(data = median_tbl, 
                    shape = 23, 
                    size = 2, 
                    color = 'orangered3', 
                    fill = 'orangered3', 
                    position = position_dodge(0.9), 
                    aes(group = clust_id)) +
         globals$common_theme + 
         theme(axis.title.y = element_blank()) + 
         facet_grid(rows = if(split_var_type) vars(var_type) else NULL, 
                    cols = if(split_by_cluster) vars(clust_id) else NULL, 
                    scales = 'free_y', 
                    space = 'free_y') + 
         labs(title = plot_title, 
              subtitle = plot_subtitle, 
              tag = plot_tag, 
              x = 'Quantity, 0: min/absent, 1: max/present')
      
   }
   
# general table and LATEX formatting -----
   
   format_main_tbl <- function(analysis_table) {
      
      ## formats the tables with cohort comparisons

      analysis_table <- analysis_table %>% 
         map_dfc(stri_replace, 
                 regex = 'no:\\s{1}.*\\nyes:\\s{1}', 
                 replacement = '') %>% 
         map_dfc(stri_replace, 
                 regex = 'Mean\\s{1}.*\\nMedian', 
                 replacement = 'Median') %>% 
         map_dfc(stri_replace, 
                 fixed = 'young', 
                 replacement = 'up to 30 years') %>% 
         map_dfc(stri_replace, 
                 fixed = 'middle-aged', 
                 replacement = '31 - 65 years') %>% 
         map_dfc(stri_replace, 
                 fixed = 'elderly', 
                 replacement = '> 65 years') %>% 
         map_dfc(stri_replace, 
                 fixed = 'Chi-squared', 
                 replacement = '\u03C7\u00B2') %>% 
         mutate(variable = translate_var(variable, short = FALSE), 
                test = stri_replace(test, fixed = ' test', replacement = ''))
      
      analysis_table %>% 
         mutate(variable = car::recode(variable, 
                                       "'Pre-CoV depression/anxiety' = 'Depression/anxiety before COVID-19'; 
                                  'Pre-CoV sleep disorders' = 'Sleep disorders before COVID-19';
                                  'Freq. resp. infections' = '> 2 respiratory infections per year'; 
                                  'Freq. bact. Infections' = '> 2 bacterial infections per year'; 
                                  'Observation time' = 'Time between survey and diagnosis'"), 
                variable = stri_replace(variable, 
                                        fixed = 'Sum', 
                                        replacement = 'Number'), 
                variable = stri_replace(variable, 
                                        fixed = 'NC', 
                                        replacement = 'neurocognitive symptoms'), 
                variable = stri_replace(variable, 
                                        fixed = 'persist.', 
                                        replacement = 'persistent'), 
                variable = stri_replace(variable, 
                                        fixed = '#', 
                                        replacement = 'Number of'), 
                variable = car::recode(variable, 
                                       "'OMH score' = 'Overall Mental Health Score'; 
                                  'QoL score' = 'Quality of Life Score';
                                  'DPR score' = 'Depression Score'; 
                                  'DPR+' = 'Depression Screening-positive'; 
                                  'ANX score' = 'Anxiety score'; 
                                  'ANX+' = 'Anxiety Screening-positive'; 
                                  'Stress score' = 'Psychosocial Stress Score'"))
      
   }

# varia -----
   
   mm_inch <- function(input_mm) {
      
      return(0.0393700787 * input_mm)
      
   }
   
   set_rownames <- function(inp_tbl, new_rownames) {
      
      ## sets custom rownames
      
      out_tbl <- inp_tbl %>% 
         as.data.frame
      
      rownames(out_tbl) <- new_rownames
      
      return(out_tbl)      
      
   }
   
   strat_quartile <- function(inp_tbl, 
                              numeric_variable, 
                              quant_vec = c(0.25, 0.5, 0.75), 
                              new_var_name = 'strat_var', 
                              id_index = 'ID', labels = NULL) {
      
      ## stratifies the given variable by its quartiles (or other quantiles...)
      
      if(length(numeric_variable) > 1) {
         
         out_tbl <- numeric_variable %>% 
            map(strat_quartile, 
                inp_tbl = inp_tbl, 
                new_var_name = 'strat_var', 
                id_index = id_index, 
                labels = labels) %>% 
            reduce(left_join, 
                   by = id_index)
         
         if(length(numeric_variable) == length(new_var_name)) {
            
            out_tbl <- out_tbl %>% 
               set_names(c(id_index, 
                           new_var_name))
            
         }
         
         return(out_tbl)
         
      }
      
      cutoffs <- inp_tbl[[numeric_variable]] %>% 
         quantile(quant_vec, na.rm = T)
      
      cutoffs <- c(-Inf, cutoffs, Inf)
      
      out_tbl <- inp_tbl %>% 
         mutate(strat_var = cut(.data[[numeric_variable]], 
                                cutoffs, 
                                labels)) %>% 
         select(all_of(c(id_index, 
                         'strat_var'))) %>% 
         set_names(c(id_index, 
                     new_var_name))
      
      return(out_tbl)
      
   }
   
   min_max <- function(vector) {
      
      ## min max normalization of a numeric vector
      
      stopifnot(is.numeric(vector))
      
      return((vector - min(vector, na.rm = T))/(max(vector, na.rm = T) - min(vector, na.rm = T)))
      
   }
   
   get_tag <- function(plot) {
      
      ## extracts the plot tag and changes it's format for the figure panel
      
      plot_tag <- plot %>% 
         ggplot_build
      
      plot_tag <- plot_tag$plot$labels$tag
      
      return(plot_tag)
      
   }
   
# END ----