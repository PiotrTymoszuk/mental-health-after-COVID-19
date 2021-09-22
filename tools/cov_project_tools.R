# This script provides tools for the COV-Questionnaire project


# libraries and data ----

   c('./tools/sys_tools.R', 
     './tools/plotting_tools.R', 
     './tools/counting_tools.R', 
     './tools/clust_tools.R', 
     './tools/lm_qc_tools.R') %>% 
      walk(source)

   library(ggvenn)
   library(UpSetR)
   library(doParallel)
   library(caret)
   library(meta)
   library(furrr)

# feature frequency or value analysis wrapper ----
   
   analyze_split <- function(inp_data_list, 
                             var_lexicon, 
                             split_var, 
                             labeller, 
                             numeric_colors = NULL, 
                             numeric_y_lab = '# symptoms', 
                             numeric_test = 'u', 
                             show_tag = F, 
                             x_lab = NULL, ...) {
      
      ## this function (a wrapper around counting and plotting function from the 'counting_tools.R')
      ## analyzes the categorical features specified in the variable lexicon in respect to the splitting factor
      
      ## data container
      
      data_cont <- list()
      
      ## input data
      
      inp_data_list <- inp_data_list %>% 
         map(filter, 
             !is.na(.data[[split_var]]))
      
      ## analyses
      
      data_cont$analyses_north <- var_lexicon$variable %>% 
         map(analyze_feature, 
             inp_tbl = inp_data_list$north, 
             split_var = split_var) %>% 
         set_names(var_lexicon$variable)
      
      data_cont$analyses_south <- var_lexicon$variable %>% 
         map(analyze_feature, 
             inp_tbl = inp_data_list$south, 
             split_var = split_var) %>% 
         set_names(var_lexicon$variable)
      
      ## Summary table and correction for multiple testing by Benjamini-Hochberg
      
      ## counts/stats
      
      data_cont$stats$north <- data_cont$analyses_north %>% 
         map_dfr(extract_counts) %>% 
         mutate(cohort = 'north')
      
      data_cont$stats$south <- data_cont$analyses_south %>% 
         map_dfr(extract_counts) %>% 
         mutate(cohort = 'south')
      
      data_cont$stats <- data_cont$stats %>% 
         reduce(rbind)
      
      ## significance
      
      data_cont$signifcance$north <- data_cont$analyses_north %>% 
         map_dfr(extract_test_summary) %>% 
         mutate(cohort = 'north')
      
      data_cont$signifcance$south <- data_cont$analyses_south %>% 
         map_dfr(extract_test_summary) %>% 
         mutate(cohort = 'south')

      data_cont$signifcance <- data_cont$signifcance %>% 
         reduce(rbind) %>% 
         mutate(p_fdr = p.adjust(p_value, 'BH')) %>% 
         dlply(.(cohort), as_tibble)
      
      ## plotting
      
      if(data_cont$analyses_north[[1]]$var_class == 'factor') {
         
         ## significance vectors
         
         data_cont$signifcance_tags <- data_cont$signifcance %>% 
            map(function(x) ifelse(x$p_fdr < 0.05, 
                                   paste('p =', signif(x$p_fdr, 2)), 
                                   paste('ns, p =', signif(x$p_fdr, 2))))
         
         data_cont$plots_north <-  list(analysis_obj = data_cont$analyses_north, 
                                        label = paste('North Tyrol:', var_lexicon$label), 
                                        fill_colors = var_lexicon$level_colors) %>% 
            pmap(plot_analysis, 
                 labeller = labeller, 
                 cust_theme = globals$common_theme + 
                    theme(plot.title.position = 'plot'), 
                 pie = F, 
                 y_lab = '% strata', ...)
         
         data_cont$plots_south <-  list(analysis_obj = data_cont$analyses_south, 
                                        label = paste('South Tyrol:', var_lexicon$label), 
                                        fill_colors = var_lexicon$level_colors) %>% 
            pmap(plot_analysis, 
                 labeller = labeller, 
                 cust_theme = globals$common_theme + 
                    theme(plot.title.position = 'plot'), 
                 pie = F, 
                 y_lab = '% strata', ...) + 
            theme(plot.title.position = 'plot')
         
      } else {
         
         ## significance vectors
         
         data_cont$signifcance_tags <- data_cont$signifcance %>% 
            map(filter, 
                test == numeric_test) %>% 
            map(function(x) ifelse(x$p_fdr < 0.05, 
                                   paste('p =', signif(x$p_fdr, 2)), 
                                   paste('ns, p =', signif(x$p_fdr, 2))))
      
         if(!is.null(numeric_colors)) {
            
            data_cont$plots_north <-  list(analysis_obj = data_cont$analyses_north, 
                                           label = paste('AT:', var_lexicon$label)) %>% 
               pmap(plot_analysis, 
                    labeller = labeller, 
                    cust_theme = globals$common_theme  + 
                       theme(plot.title.position = 'plot'), 
                    y_lab = numeric_y_lab, 
                    x_lab = x_lab, 
                    fill_colors = numeric_colors, ...)
            
            data_cont$plots_south <-  list(analysis_obj = data_cont$analyses_south, 
                                           label = paste('IT:', var_lexicon$label)) %>% 
               pmap(plot_analysis, 
                    labeller = labeller, 
                    cust_theme = globals$common_theme + 
                       theme(plot.title.position = 'plot'), 
                    y_lab = numeric_y_lab, 
                    x_lab = x_lab, 
                    fill_colors = numeric_colors, ...)
            
            
         } else {
            
            data_cont$plots_north <-  list(analysis_obj = data_cont$analyses_north, 
                                           label = paste('AT:', var_lexicon$label)) %>% 
               pmap(plot_analysis, 
                    labeller = labeller, 
                    cust_theme = globals$common_theme + 
                       theme(plot.title.position = 'plot'), 
                    y_lab = numeric_y_lab, 
                    x_lab = x_lab, 
                    fill_colors = globals$cohort_colors[1], ...)
            
            data_cont$plots_south <-  list(analysis_obj = data_cont$analyses_south, 
                                           label = paste('IT:', var_lexicon$label)) %>% 
               pmap(plot_analysis, 
                    labeller = labeller, 
                    cust_theme = globals$common_theme + 
                       theme(plot.title.position = 'plot'), 
                    y_lab = numeric_y_lab, 
                    x_lab = x_lab, 
                    fill_colors = globals$cohort_colors[2], ...)
            
         }

      }

      ## appending with the corrected p values
      
      data_cont$plots_north <- map2(data_cont$plots_north, 
                                    data_cont$signifcance_tags$north, 
                                    function(x, y) x + labs(subtitle = y))
      
      data_cont$plots_south <- map2(data_cont$plots_south, 
                                    data_cont$signifcance_tags$south, 
                                    function(x, y) x + labs(subtitle = y))
      
      ## plot panels
      
      if(show_tag) {
         
         data_cont$plot_panels <- map2(data_cont$plots_north, 
                                       data_cont$plots_south, 
                                       function(x, y) plot_grid(x + theme(legend.position = 'none'), 
                                                                y + theme(legend.position = 'none'), 
                                                                ncol = 2, 
                                                                rel_widths = c(1, 1)))
         
         
      } else {
         
         data_cont$plot_panels <- map2(data_cont$plots_north, 
                                       data_cont$plots_south, 
                                       function(x, y) plot_grid(x + theme(legend.position = 'none', 
                                                                          plot.tag = element_blank()), 
                                                                y + theme(legend.position = 'none', 
                                                                          plot.tag = element_blank()), 
                                                                ncol = 2, 
                                                                rel_widths = c(1, 1)))
         
         
      }
      
      
      ## output
      
      return(data_cont[c('stats', 
                         'signifcance', 
                         'plots_north', 
                         'plots_south', 
                         'plot_panels')])
      
   }

# Random forest training/model development ------
   
   train_cv_rf <- function(response, 
                           train_data, 
                           variables = names(train_data)[names(train_data) != response], 
                           n_folds = 10, 
                           mtry_vec = c(200, 500, 800), ...) {
      
      ## trains a random forest model (the best chosen for the given mtry values)
      ## validated by n_folds cross-validation

      registerDoParallel(detectCores() - 1)
      
      mod_formula <- paste(response, 
                           paste(variables, collapse = '+'), 
                           sep = '~') %>% 
         as.formula
      
      caret_model <- mod_formula %>% 
         caret::train(form = .,  
                      method = 'rf', 
                      data = train_data, 
                      trControl = trainControl(method = 'cv', 
                                               number = n_folds), 
                      tuneGrid = data.frame(mtry = mtry_vec), ...)
      
      stopImplicitCluster()
      
      return(caret_model)
      
   }
   
   predict_cv_rf <- function(caret_model, 
                             response, 
                             new_data) {
      
      ## makes predictions and evaluates them
      
      ## predictions

      pred_tbl <- predict.train(caret_model, 
                                newdata = new_data)
      
      ## updating with the true values and direct fit measures
      
      pred_tbl <- tibble(ID = names(pred_tbl), 
                         .fitted = pred_tbl, 
                         y = new_data[[response]]) %>% 
         mutate(.resid = .fitted - y, 
                .sq.resid = .resid ^ 2, 
                .std.resid = scale(.resid)[, 1], 
                .sq.std.resid = .std.resid ^ 2, 
                .candidate_missfit = ifelse(abs(.std.resid) > qnorm(0.975), 
                                            'yes', 
                                            'no'))
      
      ## fit measured in complete training data set
      
      measures_train <- tibble(mse = mean(pred_tbl$.sq.resid), 
                               mae = mean(abs(pred_tbl$.resid)), 
                               corr_pearson = cor(pred_tbl$y, 
                                                  pred_tbl$.fitted, 
                                                  method = 'pearson'), 
                               corr_spearman = cor(pred_tbl$y, 
                                                   pred_tbl$.fitted, 
                                                   method = 'spearman'), 
                               rsq = 1 - mean(pred_tbl$.sq.resid)/var(pred_tbl$y), 
                               n_complete = nrow(pred_tbl))
      
      ## CV fit measures for the best model
      
      measures_cv <- caret_model$results %>% 
         filter(mtry == caret_model$bestTune$mtry[1]) %>% 
         as_tibble %>% 
         set_names(c('mtry', 
                     'rmse', 
                     'rsq', 
                     'mae', 
                     'rmse_sd', 
                     'rsq_sd', 
                     'mae_sd')) %>% 
         mutate(mse = rmse ^ 2)
      
      return(list(predictions = pred_tbl, 
                  measures_train = measures_train, 
                  measures_cv = measures_cv))
      
   }
   
   extract_rf_summary <- function(caret_model) {
      
      ## extracts a summary table with the impacts
      
      impact_tbl <- caret_model$finalModel$importance %>% 
         as.data.frame %>% 
         rownames_to_column('parameter') %>% 
         set_names(c('parameter', 
                     'delta_mse')) %>% 
         as_tibble
      
      ## extracting the variable and level names
      
      extr_regex <- globals$var_lexicon %>% 
         filter(modeling_variable == 'yes') %>% 
         .$variable %>% 
         paste(collapse = '|')
      
      impact_tbl <- impact_tbl %>% 
         mutate(variable = stri_extract(parameter, 
                                        regex = extr_regex), 
                level = stri_replace_all(parameter, 
                                         regex = extr_regex, 
                                         replacement = ''), 
                variable_label = translate_var(variable))
      return(impact_tbl)
      
   }
   
# Random Forest, linear modeling and Venn plotting of modeling results ----

   plot_bar_rf <- function(inp_tbl, 
                           plot_title = NULL, 
                           plot_subtitle = NULL, 
                           plot_tag = NULL, 
                           x_lab = NULL, tag_position = 'bottom', 
                           x_transf = 'identity', 
                           fill_color = 'cornsilk') {
      
      ## makes a basic bar plot with delta MSE results obtained by random forest modeling
      
      bar_plot <- inp_tbl %>% 
         mutate(plot_label = paste(variable_label, level, sep = ': ')) %>% 
         ggplot(aes(x = delta_mse, 
                    y = reorder(plot_label, delta_mse))) +
         geom_bar(stat = 'identity', 
                  color = 'black', 
                  fill = fill_color) + 
         scale_x_continuous(trans = x_transf) + 
         globals$common_theme + 
         theme(panel.grid.major = element_line(color = 'gray90'), 
               axis.title.y = element_blank(), 
               plot.tag.position = tag_position) + 
         labs(title = plot_title, 
              subtitle = plot_subtitle, 
              x = x_lab, 
              tag = plot_tag)
      
      return(bar_plot)
      
      
   }
   
   
   plot_bar_rf_cohorts <- function(modeling_summary, n_top = 20, 
                                   plot_title = NULL, plot_tag = NULL, 
                                   x_lab = expression(Delta*' MSE'), tag_position = 'bottom', 
                                   x_transf = 'identity') {
      
      ## plots a bar plot of top influential component of the random forest model split by the cohort
      
      require(cowplot)

      ## identifying the top estimates
      
      plotting_tbl <- modeling_summary %>% 
         dlply(.(cohort), 
               top_n, 
               n_top, 
               delta_mse)

      if(is.null(plot_tag)) {
         
         n_numbers <- plotting_tbl %>% 
            map(function(x) x$n_complete[1]) %>%
            map2(globals$cohort_labs[names(plotting_tbl)], 
                 ., 
                 function(x, y) paste('\n', x, ', n = ', y, sep = ''))

      }

      
      cohort_plots <- list(inp_tbl = plotting_tbl, 
                           plot_subtitle = paste(globals$cohort_labs[names(plotting_tbl)], 
                                                 ', ', 
                                                 n_top, ' most influential factors', sep = ''), 
                           plot_tag = n_numbers, 
                           fill_color = globals$cohort_colors[names(plotting_tbl)]) %>% 
         pmap(plot_bar_rf, 
              x_lab = x_lab, 
              plot_title = plot_title, 
              tag_position = tag_position, 
              x_transf = x_transf) %>% 
         map(function(x) x + theme(plot.title.position = 'plot'))
      
      cohort_panel <- plot_grid(cohort_plots$north, 
                                cohort_plots$south + 
                                   labs(title = ''), 
                                ncol = 2, 
                                align = 'hv')
      
      return(cohort_panel)

   }
   
   plot_venn <- function(plotting_lst,  colors = c('blue', 'yellow', 'green', 'red'), 
                         plot_title = NULL, plot_subtitle = NULL, short = T, fct_per_line = 4) {
      
      ## generates a Venn plot to show an overlap between the significant features
      ## identified for different risk responses
      
      ## plot

      venn_plot <- plotting_lst %>% 
         ggvenn(show_percentage = F, 
                fill_color = unname(colors), 
                set_name_size = 2.75, 
                text_size = 2.75) + 
         theme(plot.title = element_text(size = 10, face = 'bold'), 
               plot.subtitle = globals$common_text) + 
         labs(title = plot_title, 
              subtitle = plot_subtitle)
      
      ## feature listing
      
      feat_txt <- plotting_lst %>% 
         reduce(intersect) %>% 
         translate_var(short = short) %>% 
         sort %>% 
         wrap_vector(line_length = fct_per_line)
      
      ## plot panel
      
      venn_panel <- plot_grid(venn_plot, 
                              ggdraw() + 
                                 draw_text(feat_txt, 
                                           hjust = 0, 
                                           x = 0.05, 
                                           size = 8) + 
                                 theme(plot.margin = ggplot2::margin(t = 0, b = 0, r = 2, l = 3)), 
                              ncol = 2, 
                              rel_widths = c(0.4, 0.6)) + 
         theme(plot.margin = globals$common_margin)
      
      return(venn_panel)
      
   }
   
   plot_upset <- function(plotting_lst, 
                          plot_title = '', 
                          plot_subtitle = '', 
                          short = T, 
                          fct_per_line = 4, 
                          label_common = T, 
                          query = NULL) {
      
      ## makes an upset plot to visualize intersections
      
      if(label_common) {
         
         query <- list(list(query = intersects, 
                            params = list(names(plotting_lst)), 
                            color = 'coral3', 
                            active = T))
         
      }
      
      ## upset
      
      upset_plot <- upset(fromList(plotting_lst), 
                          order.by = 'freq', 
                          nsets = 6, 
                          set_size.show = F, 
                          queries = query, 
                          main.bar.color = 'gray40', 
                          mainbar.y.label = '# common variables', 
                          text.scale = 1.2)
      
      ## listing of the commonn factors
      
      feat_txt <- plotting_lst %>% 
         reduce(intersect) %>% 
         translate_var(short = short) %>% 
         stri_sort %>% 
         wrap_vector(line_length = fct_per_line)
      
      ## panel
      
      upset_panel <- plot_grid(upset_plot$Main_bar, 
                               upset_plot$Matrix, 
                               nrow = 2, 
                               align = 'hv', 
                               rel_heights = c(1.5, 0.5)) %>% 
         plot_grid(., 
                   ggdraw() + 
                      draw_text(feat_txt, 
                                hjust = 0, 
                                x = 0.05, 
                                size = 8) + 
                      theme(plot.margin = ggplot2::margin(t = 0, b = 0, r = 2, l = 3)), 
                   ncol = 2, 
                   rel_widths = c(2, 1)) %>% 
         plot_grid(ggdraw() + 
                      draw_text(plot_subtitle, 
                                size = 9, 
                                fontface = 'plain', 
                                hjust = 0, 
                                x = 0.05), 
                   ., 
                   nrow = 2, 
                   rel_heights = c(0.05, 0.95)) %>% 
         plot_grid(ggdraw() + 
                      draw_text(plot_title, 
                                size = 9, 
                                fontface = 'bold', 
                                hjust = 0, 
                                x = 0.05), 
                   ., 
                   nrow = 2, 
                   rel_heights = c(0.05, 0.95))
      
      upset_panel <- upset_panel +
         theme(plot.margin = globals$common_margin)
      
      return(upset_panel)
      
   }
   
   plot_corr <- function(pred_tbl, x_var = 'y', y_var = '.fitted', 
                         x_lab = 'Actual score', y_lab = 'Fitted score', plot_tag = NULL,  
                         plot_title = NULL, plot_subtitle = NULL, rho_method = 'spearman', 
                         violin = F, fill_color = 'steelblue') {
      
      ## makes a correlation plot
      
      
      if(is.null(plot_tag)) {
         
         corr_coeff <- cor(pred_tbl[[x_var]], 
                           pred_tbl[[y_var]], 
                           method = rho_method)
         
         plot_tag = paste('\n\u03C1 = ', 
                          signif(corr_coeff, 2), 
                          ', n = ', 
                          nrow(pred_tbl))
         
      }
      
      axis_scale <- factor(pred_tbl[[x_var]]) %>% 
         levels %>% 
         as.numeric
      
      ## plot
      
      if(!violin) {
         
         corr_plot <- pred_tbl %>% 
            ggplot(aes(x = .data[[x_var]], 
                       y = .data[[y_var]]))
         
      } else {
         
         corr_plot <- pred_tbl %>% 
            ggplot(aes(x = factor(.data[[x_var]]), 
                       y = .data[[y_var]])) + 
            geom_violin(fill = fill_color, 
                        alpha = 0.25)
         
      }
      
      corr_plot <- corr_plot + 
         geom_abline(slope = 1, 
                     intercept = 0, 
                     linetype = 'dashed') + 
         geom_point(size = 2, 
                    shape = 21, 
                    alpha = 0.5, 
                    position = position_jitter(height = 0.05, 
                                               width = 0.05), 
                    fill = fill_color) + 
         scale_x_continuous(breaks = axis_scale, 
                            limits = range(axis_scale)) + 
         scale_y_continuous(breaks = axis_scale, 
                            limits = range(axis_scale)) + 
         globals$common_theme + 
         labs(title = plot_title, 
              subtitle = plot_subtitle, 
              tag = plot_tag, 
              x = x_lab, 
              y = y_lab)
      
      return(corr_plot)
      
   }
   
   rf_pca <- function(rf_summary_lst, 
                      k = 3, 
                      top_factors = 10, 
                      score_fill = 'coral3', 
                      label_obs = T, 
                      scale = F, 
                      ...) {
      
      ## makes a PCA of the impacting factors identified by random forest modeling
      
      ## scaled input table
      
      inp_tbl <- rf_summary_lst %>% 
         map(select, 
             parameter, 
             delta_mse) %>% 
         map2(., 
              names(.), 
              function(x, y) set_names(x, c('parameter', y))) %>% 
         reduce(left_join, 
                by = 'parameter') %>% 
         column_to_rownames('parameter') %>% 
         t %>% 
         as.data.frame
      
      obs <- rownames(inp_tbl)
      
      if(scale) {
         
         inp_tbl <- inp_tbl %>% 
            map(function(x) scale(x)[, 1]) %>%
            map_dfc(function(x) if(all(is.na(x))) NULL else x) %>% ## removal of the invariant variables
            as.data.frame
         
      }
      
      ## naming vectors
      
      present_vars <- rf_summary_lst %>% 
         map(~.x$variable) %>% 
         reduce(c) %>%
         unique
      
      rownames(inp_tbl) <- obs
      parameters <- names(inp_tbl)
      
      vars <- names(inp_tbl) %>% 
         stri_extract(regex = paste(present_vars, collapse = '|'))
      
      levels <- names(inp_tbl) %>% 
         stri_replace(regex = paste(present_vars, collapse = '|'), 
                      replacement = '')
      
      ## PCA
      
      pca_obj <- PCAproj(inp_tbl, 
                         k = k, ...)
      
      ## loadings table
      
      loadings_tbl <- pca_obj$loadings[, ] %>% 
         as_tibble %>% 
         mutate(parameter = parameters, 
                variable = vars, 
                level = levels, 
                dist = sqrt(Comp.1^2 + Comp.2^2))
      
      ## loadings stats
      
      loadings_stats <- pca_obj$sdev^2/sum(pca_obj$sdev^2)
      
      ## loadings plot
      
      loadings_plot <- loadings_tbl %>% 
         arrange(-dist) %>% 
         mutate(dist_ord = 1:nrow(.), 
                var_lab = ifelse(dist_ord <= top_factors, 
                                 paste(translate_var(variable), level, sep = ': '), 
                                 NA))
      
      vct_tbl <- loadings_plot %>% 
         filter(!is.na(var_lab))
      
      loadings_plot <- loadings_plot %>% 
         ggplot(aes(x = Comp.1, 
                    y = Comp.2, 
                    fill = dist)) + 
         geom_segment(data = vct_tbl, 
                      aes(x = 0, 
                          y = 0, 
                          xend = Comp.1, 
                          yend = Comp.2), 
                      color = 'gray60', 
                      arrow = arrow(type = 'closed', 
                                    angle = 15, 
                                    length = unit(8, 'points'))) + 
         geom_point(size = 2, 
                    shape = 21) + 
         geom_label_repel(aes(label = var_lab), 
                          size = 2.4, 
                          box.padding = 0.1, 
                          label.padding = 0.1) +
         scale_fill_gradient2(low = 'steelblue',
                              mid = 'white', 
                              high = 'firebrick', 
                              name = 'Vec. length') + 
         globals$common_theme + 
         theme(panel.grid.major = element_line(color = 'gray90')) + 
         labs(title = 'Factor impact', 
              subtitle = 'PCA, factor loadings', 
              x = paste0('PC1, ', 100 * signif(loadings_stats[1], 2), '%'), 
              y = paste0('PC2, ', 100 * signif(loadings_stats[2], 2), '%'))
      
      ## PC score table
      
      score_tbl <- pca_obj$scores %>% 
         as.data.frame %>% 
         mutate(observation = obs) %>% 
         as_tibble
      
      ## PC score plot
      
      score_plot <- score_tbl %>% 
         mutate(obs_lab = translate_var(observation)) %>% 
         ggplot(aes(x = Comp.1, 
                    y = Comp.2)) + 
         geom_point(size = 2, 
                    shape = 21, 
                    fill = score_fill) + 
         globals$common_theme + 
         theme(panel.grid.major = element_line(color = 'gray90')) + 
         labs(title = 'Observation scores', 
              subtitle = 'PCA, observation scores', 
              x = paste0('PC1, ', 100 * signif(loadings_stats[1], 2), '%'), 
              y = paste0('PC2, ', 100 * signif(loadings_stats[2], 2), '%'))
      
      if(label_obs) {
         
         score_plot <- score_plot + 
            geom_label_repel(aes(label = obs_lab), 
                             size = 2.4, 
                             box.padding = 0.1, 
                             label.padding = 0.1, 
                             fill = score_fill)
         
      }
      
      ## output
      
      return(list(pca_obj = pca_obj, 
                  loadings_tbl = loadings_tbl, 
                  prop_var = loadings_stats, 
                  score_tbl = score_tbl, 
                  score_plot = score_plot, 
                  loadings_plot = loadings_plot, 
                  top_influential = vct_tbl))
      
   }

   plot_analysis_hm <- function(modeling_summary, 
                                plot_title = NULL, plot_subtitle = NULL, 
                                plot_tag = NULL, resp_order = NULL, bubble = F) {
      
      ## displays a significance table in form of a heat map or bubble plot
      ## plotting order by the number of significant responses

      plot_order <- modeling_summary %>% 
         ddply(.(variable), 
               summarise, 
               reg_sum = mean(log(estimate))) %>% 
         arrange(reg_sum) %>% 
         mutate(plot_order = 1:nrow(.)) %>% 
         select(variable, 
                plot_order)
      
      plotting_tbl <- modeling_summary %>% 
         select(variable, 
                level, 
                estimate, 
                lower_ci, 
                upper_ci, 
                response, 
                regulation, 
                cohort) %>% 
         left_join(plot_order, 
                   by = 'variable')

      ## adding variable and response labels
      
      plotting_tbl <- plotting_tbl %>% 
         mutate(variable_label = translate_var(variable), 
                plot_label = ifelse(level != 'yes', 
                                    paste(variable_label, 
                                          level, sep = ': '), 
                                    variable_label), 
                est_label_short = signif(estimate, 2), 
                est_label = paste0(signif(estimate, 2), 
                                   '\n[', 
                                   signif(lower_ci, 2), 
                                   ' - ', 
                                   signif(upper_ci, 2), 
                                   ']'))

      short_labs <- c('mental_health_score' = 'OMH', 
                      'life_quality_score' = 'QoL', 
                      'perf_impairment_score' = 'PPL', 
                      'phq_depression_score' = 'DPR', 
                      'phq_anxiety_score' = 'ANX', 
                      'stress_score' = 'STR')
            
      ## heat map
   
      hm_plot <- plotting_tbl %>% 
         ggplot(aes(x =  response, 
                    y = reorder(plot_label, plot_order), 
                    fill = log(estimate)))
      
      if(bubble) {
         
         hm_plot <- hm_plot + 
            geom_point(aes(size = abs(estimate)), 
                       shape = 21) + 
            geom_text(aes(label = est_label_short), 
                      size = 2.3, 
                      hjust = 0, 
                      vjust = 0.5, 
                      nudge_x = 0.25) + 
            guides(size = F) + 
            globals$common_theme + 
            theme(panel.grid.major = element_line(color = 'gray90'))
         
      } else {
         
         hm_plot <- hm_plot  + 
            geom_tile(color = 'black') + 
            geom_text(aes(label = est_label), 
                      size = 2.3, 
                      lineheight = 0.75, 
                      hjust = 0.5, 
                      vjust = 0.5) + 
            globals$common_theme
         
      }
      
      hm_plot <- hm_plot +  
         scale_fill_gradient2(low = 'steelblue', 
                              mid = 'white', 
                              high = 'coral3', 
                              midpoint = 0, 
                              name = expression('log '*beta)) + 
         facet_grid(. ~ cohort, 
                    labeller = as_labeller(globals$cohort_labs)) + 
         theme(axis.title = element_blank()) + 
         labs(title = plot_title, 
              subtitle = plot_subtitle, 
              tag = plot_tag)
      
      if(is.null(resp_order)) {
         
         hm_plot <- hm_plot +
            scale_x_discrete(labels = short_labs)
         
      } else {
         
         hm_plot <- hm_plot +
            scale_x_discrete(labels = short_labs, 
                             limits = resp_order)
         
      }
      
      return(hm_plot)
      
   }
   
   plot_univar_forest <- function(mod_summary_tbl, 
                                  params_to_plot = psych_analyses$top_parameters, 
                                  response_to_plot = 'life_quality_score', 
                                  plot_title = translate_var(response_to_plot), 
                                  plot_subtitle = NULL) {
      
      ## plots results of the univariate modeling as a Forest plot with color coding for the cohorts
      
      ## plot meta
      
      plotting_tbl <- mod_summary_tbl %>% 
         ddply(.(variable, level), 
               function(x) if(all(x$significant == 'yes')) x else NULL) %>% 
         filter(parameter %in% params_to_plot, 
                response == response_to_plot) %>% 
         mutate(plot_lab = ifelse(level == 'yes', 
                                  translate_var(variable), 
                                  paste(translate_var(variable), level, sep = ': ')), 
                cohort = factor(cohort, c('south', 'north')))
      
      
      plot_tag <- mod_summary_tbl %>% 
         dlply(.(cohort), 
               function(x) min(x$n_complete))
      
      plot_tag <- paste0('\nAT: n = ', 
                         plot_tag$north, 
                         ', IT: n = ', 
                         plot_tag$south)
      
      ## forest plot
      
      forest_plot <- plotting_tbl %>% 
         ggplot(aes(x = estimate, 
                    y = reorder(plot_lab, estimate), 
                    shape = cohort, 
                    color = cohort)) +
         geom_vline(xintercept = 1, 
                    linetype = 'dashed') + 
         geom_errorbarh(aes(xmin = lower_ci, 
                            xmax = upper_ci), 
                        height = 0, 
                        position = position_dodge(0.5)) + 
         geom_point(size = 2, 
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
              x = expression(beta))
      
      return(forest_plot)
      
   }
   
# SOM tools -----
   
   clust_fcts <- function(data, 
                          som_grid, 
                          dist.fcts = 'tanimoto', 
                          rlen = 2000,
                          hcl_distance = 'euclidean', 
                          k = 3, 
                          hc_method = 'ward.D2', 
                          seed = 123) {
      
      ## a combi wrapper for two-step clustering of the study data:
      ## Step 1: som with the provided grid object 
      ## Step 2: hierarchical clustering with the k-branch tree cut
      
      ## SOM and SOM diagnostic plots
      
      kohonen_obj <- fit_som(data = data, 
                             grid = som_grid, 
                             dist.fcts = dist.fcts, 
                             rlen = rlen, 
                             seed = seed)
      
      kohonen_plots <- plot_som(kohonen_object = kohonen_obj)
      
      kohonen_node_ass <- get_node_ass(kohonen_object = kohonen_obj) %>% 
         set_names(c('ID', 'node', 'neuro_dist'))
      
      ## HCL
      
      clust_str <- hcluster_data(inp_tbl = kohonen_obj$codes[[1]], 
                                 distance_method = hcl_distance, 
                                 k = k, 
                                 hc_method = hc_method, 
                                 seed = seed)
      
      clust_ass <- clust_str$clust_assignment %>% 
         mutate(node = stri_replace(variable, fixed = 'V', replacement = '') %>% 
                   factor) %>% 
         select(node, 
                clust_id) %>% 
         left_join(kohonen_node_ass, ., by = 'node')
      
      return(list(kohonen_obj = kohonen_obj, 
                  som_plots = kohonen_plots, 
                  clust_obj = clust_str, 
                  assignment = clust_ass))
      
      
   }
   
   classify_clusters <- function(clust_fcts_results, clust_names = c('LR', 'IR', 'HR')) {
      
      ## assigns the cluster names based on the density of the clustering features
      
      assingment_tbl <- clust_fcts_results$assignment %>% 
         select(ID, clust_id)
      
      feature_clut_tbl <- clust_fcts_results$kohonen_obj$data[[1]] %>% 
         as.data.frame
      
      id_vec <- rownames(feature_clut_tbl)
      
      feature_sums <- id_vec %>% 
         map_dbl(function(x) sum(feature_clut_tbl[x, ], na.rm = T))
      
      feature_sum_tbl <- tibble(ID = id_vec, 
                                feature_sum = feature_sums) %>% 
         left_join(assingment_tbl, by = 'ID') %>% 
         ddply(.(clust_id), 
               summarise, 
               feature_mean = mean(feature_sum, na.rm = T)) %>% 
         arrange(feature_mean) %>% 
         mutate(clust_name = clust_names)
      
      return(left_join(clust_fcts_results$assignment, 
                       feature_sum_tbl[c('clust_id', 'clust_name')], 
                       by = 'clust_id') %>% 
                mutate(clust_name = factor(clust_name, 
                                           levels = c('LR', 'IR', 'HR'))))
      
   }
   
   plot_clust_hm <- function(inp_tbl, 
                             prevalence = T, 
                             features = partclust$variables, 
                             plot_title = NULL, 
                             plot_subtitle = NULL, 
                             scaling_fun = function(x) scale(x)[, 1], 
                             midpoint = 0, 
                             scale_name = 'Z-score') {
      
      ## plots scoring of prevalence of the features in the clusters as a heat map
      
      ## plotting table in the long format
      ## appending with the variable names
      
      plotting_tbl <- inp_tbl %>% 
         gather(key = 'parameter', 
                value = 'present', 
                names(inp_tbl)[!names(inp_tbl) %in% c('ID', 'node', 'neuro_dist', 'clust_id', 'clust_name')]) %>% 
         arrange(clust_name, 
                 present) %>% 
         mutate(plot_order = 1:nrow(.), 
                variable = stri_extract(parameter, 
                                        regex = paste(features, collapse = '|')), 
                level = stri_replace(parameter, 
                                     regex = paste(features, collapse = '|'), 
                                     replacement = ''),
                plot_lab = ifelse(level == 'yes' | level == '', 
                                  translate_var(variable), 
                                  paste(translate_var(variable), 
                                        level, sep = ': '))) %>% 
         filter(complete.cases(.))
      
      ## n numbers
      
      n_numbers <- inp_tbl %>% 
         filter(complete.cases(.)) %>% 
         dlply(.(clust_name), nrow)
      
      plot_tag <- n_numbers %>% 
         map2_chr(., 
                  names(.), 
                  function(x, y) paste0(y, ': n = ', x)) %>% 
         paste(collapse = '\n') %>% 
         paste0('\n', .)
      
      ## plotting
      
      if(prevalence) {
         
         hm_plot <- plotting_tbl %>% 
            ggplot(aes(x = reorder(ID, plot_order), 
                       y = reorder(plot_lab, present), 
                       fill = factor(present))) + 
            scale_fill_manual(values = c('0' = 'steelblue', 
                                         '1' = 'coral3'), 
                              labels = c('0' = 'absent', 
                                         '1' = 'present'), 
                              name = '')
         
      } else {
         
         plotting_tbl <- plotting_tbl %>% 
            ddply(.(plot_lab), 
                  mutate, 
                  present = scaling_fun(present))
         
         hm_plot <- plotting_tbl %>% 
            ggplot(aes(x = reorder(ID, plot_order), 
                       y = plot_lab, 
                       fill = present)) + 
            scale_fill_gradient2(low = 'steelblue', 
                                 mid = 'white', 
                                 high = 'firebrick', 
                                 midpoint = midpoint, 
                                 name = scale_name)
         
      }
      
      hm_plot <- hm_plot +
         geom_tile() +
         facet_grid(. ~ clust_name, 
                    scales = 'free', 
                    space = 'free') + 
         globals$common_theme + 
         theme(axis.text.x = element_blank(), 
               axis.ticks.x = element_blank(), 
               axis.line = element_blank(), 
               axis.title.y = element_blank()) + 
         labs(title = plot_title, 
              subtitle = plot_subtitle, 
              tag = plot_tag, 
              x = 'Participant')
      
      return(hm_plot)
      
   }
   
   get_prevalence <- function(feature_tbl, 
                              features = partclust$variables, 
                              by = 'node', 
                              scoring = F) {
      
      ## for the given feature table containing the SOM node assignment
      ## prevalence of the clustering features is returned, within the SOM node or cluster
      
      features_to_analyse <- names(feature_tbl)[!names(feature_tbl) %in% c('ID', 'node', 'neuro_dist', 'clust_id', 'clust_name')]
      
      pres_tbl <- feature_tbl %>% ## presence
         gather(key = 'parameter', 
                value = 'present', 
                all_of(features_to_analyse)) %>% 
         filter(complete.cases(.))
      
      if(!scoring) {
         
         summ_tbl <- pres_tbl %>% ## counting
            ddply(c(by, 'parameter'), 
                  summarise, 
                  prevalence = sum(present)/length(present), 
                  n = sum(present), 
                  n_total = length(present)) %>% 
            as_tibble
         
         signif_tbl <- feature_tbl %>% 
            map_dfc(function(x) if(is.numeric(x)) factor(x) else x)
         
         p_tbl <- features_to_analyse %>% 
            purrr::map(analyze_feature, 
                       inp_tbl = signif_tbl,
                       split_var = by) %>% 
            map_dfr(extract_test_summary) %>% 
            mutate(parameter = variable, 
                   p_adj = p.adjust(p_value)) %>% 
            select(- variable)

      } else {
         
         summ_tbl <- pres_tbl %>% ## counting
            ddply(c(by, 'parameter'), 
                  summarise, 
                  mean = mean(present, na.rm = T),
                  sd = sd(present, na.rm = T), 
                  median = median(present, na.rm = T), 
                  perc25 = quantile(present, 0.25, na.rm = T), 
                  perc75 = quantile(present, 0.75, na.rm = T), 
                  n_total = length(present)) %>% 
            as_tibble

         p_tbl <- features_to_analyse %>% 
            purrr::map(analyze_feature, 
                       inp_tbl = feature_tbl,
                       split_var = by) %>% 
            map_dfr(extract_test_summary) %>% 
            filter(test == 'kruskal') %>% 
            mutate(parameter = variable, 
                   p_adj = p.adjust(p_value)) %>% 
            select(- variable)
         
      }
      
      pres_tbl <- pres_tbl %>% 
         mutate(variable = stri_extract(parameter, 
                                        regex = paste(features, collapse = '|')), 
                level = stri_replace(parameter, 
                                     regex = paste(features, collapse = '|'), 
                                     replacement = ''),
                plot_lab = ifelse(level == 'yes' | level == '', 
                                  translate_var(variable), 
                                  paste(translate_var(variable), 
                                        level, sep = ': ')))
      
      summ_tbl <- summ_tbl %>% 
         mutate(variable = stri_extract(parameter, 
                                        regex = paste(features, collapse = '|')), 
                level = stri_replace(parameter, 
                                     regex = paste(features, collapse = '|'), 
                                     replacement = ''),
                plot_lab = ifelse(level == 'yes' | level == '', 
                                  translate_var(variable), 
                                  paste(translate_var(variable), 
                                        level, sep = ': ')))
      
      p_tbl <- p_tbl %>% 
         mutate(variable = stri_extract(parameter, 
                                        regex = paste(features, collapse = '|')), 
                level = stri_replace(parameter, 
                                     regex = paste(features, collapse = '|'), 
                                     replacement = ''),
                plot_lab = ifelse(level == 'yes' | level == '', 
                                  translate_var(variable), 
                                  paste(translate_var(variable), 
                                        level, sep = ': ')))
      
      return(list(long_tbl = pres_tbl, 
                  summary = summ_tbl, 
                  test = p_tbl))
      
   }
   
   plot_prevalence <- function(clust_prevalence_list, 
                               plot_title = NULL, 
                               plot_subtitle = NULL) {
      
      ## plots prevalence of the features in the clusters
      
      ## table storing the p value laels
      
      label_tbl <- clust_prevalence_list$summary %>% 
         ddply(.(parameter), 
               summarise, 
               prevalence = max(prevalence)) %>% 
         left_join(clust_prevalence_list$test, ., by = 'parameter') %>% 
         mutate(clust_name = NA, 
                p_lab = ifelse(p_adj < 0.05, 
                               paste('p =', signif(p_adj, 2)), 
                               NA))
      
      ## n numbers
      
      n_numbers <- clust_prevalence_list$summary %>% 
         dlply(.(clust_name), 
               function(x) min(x$n_total))
      
      plot_tag <- n_numbers %>% 
         map2_chr(., 
                  names(.), 
                  function(x, y) paste0(y, ': n = ', x)) %>% 
         paste(collapse = '\n') %>% 
         paste0('\n', .)
      
      ## plot
      
      prev_plot <- clust_prevalence_list$summary %>% 
         ggplot(aes(x = prevalence * 100, 
                    y = reorder(plot_lab, prevalence), 
                    fill = clust_name)) + 
         geom_bar(stat = 'identity', 
                  position = 'dodge', 
                  color = 'black') + 
         geom_text(data = label_tbl, 
                   aes(label = p_lab), 
                   size = 2.4, 
                   nudge_x = 1, 
                   hjust = 0, 
                   vjust = 0.5) + 
         scale_fill_manual(values = globals$clust_colors, 
                           name = 'Cluster') + 
         globals$common_theme + 
         theme(axis.title.y = element_blank(), 
               panel.grid.major = element_line(color = 'gray90')) + 
         labs(title = plot_title, 
              subtitle = plot_subtitle, 
              tag = plot_tag, 
              x = '% within the cluster')
      
      return(prev_plot)
      
   }
   
   plot_scoring <- function(clust_prevalence_list, 
                            plot_title = NULL, 
                            plot_subtitle = NULL, 
                            show_points = T) {
      
      ## plots prevalence of the features in the clusters
      
      ## table storing the p value labels
      
      label_tbl <- clust_prevalence_list$long_tbl %>% 
         ddply(.(parameter), 
               summarise, 
               present = max(present)) %>% 
         left_join(clust_prevalence_list$test, ., by = 'parameter') %>% 
         mutate(clust_name = NA, 
                p_lab = ifelse(p_adj < 0.05, 
                               paste('p =', signif(p_adj, 2)), 
                               'ns'))
      
      ## n numbers
      
      n_numbers <- clust_prevalence_list$summary %>% 
         dlply(.(clust_name), 
               function(x) min(x$n_total))
      
      plot_tag <- n_numbers %>% 
         map2_chr(., 
                  names(.), 
                  function(x, y) paste0(y, ': n = ', x)) %>% 
         paste(collapse = '\n') %>% 
         paste0('\n', .)
      
      ## plot
      
      violin_plot <- clust_prevalence_list$long_tbl %>% 
         ggplot(aes(y = present, 
                    x = plot_lab, 
                    fill = clust_name)) + 
         scale_fill_manual(values = globals$clust_colors, 
                           name = 'Cluster') + 
         facet_wrap(plot_lab ~ ., 
                    scales = 'free', 
                    ncol = 4) + 
         globals$common_theme + 
         theme(axis.title.x = element_blank(), 
               panel.grid.major = element_line(color = 'gray90'), 
               strip.background = element_blank(), 
               strip.text = element_blank()) + 
         labs(title = plot_title, 
              subtitle = plot_subtitle, 
              tag = plot_tag, 
              y = 'Score') + 
         geom_text(data = label_tbl, 
                   aes(label = p_lab, 
                       y = present * 1.05), 
                   size = 2.4, 
                   hjust = 0.5, 
                   vjust = 0)
      
      if(show_points) {
         
         violin_plot <- violin_plot + 
            geom_violin(alpha = 0.25) + 
            geom_point(shape = 21, 
                       size = 2, 
                       alpha = 0.3, 
                       position = position_jitterdodge(jitter.width = 0.2, 
                                                       jitter.height = 0.1, 
                                                       dodge.width = 0.9), 
                       show.legend = F)
         
      } else {
         
         violin_plot <- violin_plot + 
            geom_violin()
         
      }
      
      violin_plot <- violin_plot + 
         geom_errorbar(data = clust_prevalence_list$summary, 
                       aes(y = median, 
                           ymin = perc25, 
                           ymax = perc75), 
                       width = 0.1, 
                       color = 'black', 
                       size = 1, 
                       position = position_dodge(width = 0.9)) +  
         geom_point(data = clust_prevalence_list$summary, 
                    aes(y = median, 
                        group = clust_name), 
                    shape = 23, 
                    size = 3, 
                    color = 'black', 
                    fill = 'orangered2', 
                    position = position_dodge(width = 0.9))
      
      return(violin_plot)
      
   }
   
# Prevalence of other features in the mental disorder risk clusters -----
   
   plot_prev_pie <- function(clust_prev_tbl, 
                             plot_title = NULL, 
                             plot_subtitle = NULL) {
      
      ## plots the prevalence of the features of interest 
      ## a facet of bar plots. 'Intensity' of the categorical 
      # feature defined by the factor levels is color-coded
      
      ## meta
      
      n_numbers <- clust_prev_tbl %>% 
         dlply(.(split_var), 
               function(x) min(x$total_n, na.rm = T))
      
      plot_tag <- n_numbers %>% 
         map2_chr(., 
                  names(.), 
                  function(x, y) paste0(y, ': n = ', x)) %>% 
         paste(collapse = '\n') %>% 
         paste0('\n', .)
      
      ## plotting tbl, setting the level_no fo 'problematic' cases where no min/max normalization was done
      ## by hand to 1 and 0, depending on the percent value
      
      plotting_tbl <- clust_prev_tbl %>% 
         dlply(.(variable, split_var), 
               arrange, 
               strata) %>% 
         map_dfr(function(x) mutate(x, 
                                    level_no = min_max(1:nrow(x)))) %>% 
         as_tibble
      
      plotting_tbl <- plotting_tbl %>% 
         mutate(level_no = ifelse(percent == 100, 1, 
                                  ifelse(percent == 0, 0, level_no)))
      
      ## plot
      
      bar_panel <- plotting_tbl %>% 
         ggplot(aes(x = '', 
                    y = percent, 
                    fill = level_no)) + 
         geom_bar(stat = 'identity', 
                  position = 'stack', 
                  color = 'gray80') + 
         scale_fill_gradient2(low = 'steelblue', 
                              mid = 'white', 
                              high = 'coral3', 
                              midpoint = 0.5, 
                              name = '', 
                              breaks = c(0, 0.25, 0.5, 0.75, 1), 
                              labels = c('Absent/none', 
                                         rep('', 3), 
                                         'Present/many')) + 
         coord_polar(theta = 'y') + 
         facet_grid(var_label ~ split_var) + 
         globals$common_theme + 
         theme(axis.line = element_blank(), 
               axis.ticks = element_blank(), 
               axis.text = element_blank(), 
               axis.title = element_blank(), 
               strip.text.y = element_text(angle = 0), 
               strip.background = element_blank()) + 
         labs(title = plot_title, 
              subtitle = plot_subtitle, 
              tag = plot_tag)
      
      return(bar_panel)
      
   }
   
   
# Formatting of the modeling result tables -----
   
   format_res_tbl <- function(modeling_summary, p_val_var = 'p_adj') {
      
      ## nice format modeling result table
      
      nice_tbl <- modeling_summary %>% 
         left_join(globals$var_lexicon %>% 
                      mutate(response = variable) %>% 
                      select(response, family), 
                   by = 'response') %>% 
         mutate(method = car::recode(family,
                                     "'quasipoisson' = 'GLM Poisson'; 
                                'quasibinomial' = 'logistic regression'")) %>% 
         mutate(cohort = car::recode(cohort, 
                                     "'south' = 'South Tyrol'; 
                                'north' = 'North Tyrol'"), 
                response = translate_var(response, short = F), 
                variable = translate_var(variable, short = F), 
                variable = paste(variable, level, sep = ': '), 
                significance = ifelse(.data[[p_val_var]] < 0.05, 
                                      paste('p =', 
                                            signif(.data[[p_val_var]], 2)), 
                                      'ns'), 
                estimate = signif(estimate, 3), 
                lower_ci = signif(lower_ci, 3), 
                upper_ci = signif(upper_ci, 3))
      
      out_tbl <- try(nice_tbl %>% 
                         select(cohort, 
                                response, 
                                variable, 
                                method, 
                                n_complete, 
                                estimate, 
                                lower_ci, 
                                upper_ci, 
                                significance) %>% 
                         set_names(c('Cohort', 
                                     'Response', 
                                     'Co-variate', 
                                     'Method', 
                                     'N', 
                                     'Exp. estimate', 
                                     '2.5% CI', 
                                     '97.5% CI', 
                                     'Significance')), 
                      silent = T)
      
      if(any(class(out_tbl) == 'try-error')) {
         
         out_tbl <- try(nice_tbl %>% 
                           select(cohort, 
                                  response, 
                                  variable, 
                                  method, 
                                  estimate, 
                                  lower_ci, 
                                  upper_ci, 
                                  significance) %>% 
                           set_names(c('Cohort', 
                                       'Response', 
                                       'Co-variate', 
                                       'Method', 
                                       'Exp. estimate', 
                                       '2.5% CI', 
                                       '97.5% CI', 
                                       'Significance')), 
                        silent = T)
         
      }
         
      return(out_tbl)
      
   }
   
   make_prev_tbl <- function(analysis_list, signif_digits = 3) {
      
      ## summarizes the results of prevalence analysis in a handy table
      
      ## counts
      
      count_tbl <- analysis_list$stats %>% 
         filter(strata == 'yes') %>% 
         mutate(tab_cell = paste(signif(percent, signif_digits), 
                                 '% (', 
                                 total_n, ')', sep = '')) %>% 
         select(variable, split_var, tab_cell, cohort) %>% 
         spread(key = variable, 
                value = tab_cell) %>% 
         arrange(cohort, split_var)
      
      ## significance
      
      signif_tbl <- analysis_list$signifcance %>% 
         reduce(rbind) %>% 
         mutate(variable = stri_replace(variable, 
                                        fixed = 'positive', 
                                        replacement = 'significance')) %>% 
         select(variable, p_fdr, cohort) %>% 
         mutate(p_fdr = ifelse(p_fdr < 0.05, 
                               paste('p =', 
                                     signif(p_fdr, 2)), 
                               'ns')) %>% 
         spread(key = 'variable', 
                value = 'p_fdr')
      
      return(left_join(count_tbl, 
                       signif_tbl, 
                       by = 'cohort'))
      
   }
   
   aggregate_modeling <- function(modeling_summary, return_models = F, ...) {
      
      ## aggregates the results of univariate modeling in the North and South cohort
      ## The inverse variance method is used for pooling
      ## p value adjustment by Benjamini-Hochberg
      
      pooled_models <- modeling_summary %>% 
         filter(level != 'baseline') %>% 
         dlply(.(parameter), 
               function(x) metagen(TE = estimate, 
                                   seTE = se, 
                                   studlab = cohort, 
                                   data = x))
      
      if(return_models) {
         
         return(pooled_models)
         
      }
      
      pooled_summary <- pooled_models %>% 
         map(summary) %>% 
         map2_dfr(., 
                  names(.), 
                  function(x, y) tibble(parameter = y, 
                                        estimate = x$fixed$TE, 
                                        se = x$fixed$seTE, 
                                        lower_ci = x$fixed$lower, 
                                        upper_ci = x$fixed$upper, 
                                        p_value = x$fixed$p, 
                                        Q = x$Q, 
                                        tau = x$tau$TE, 
                                        tau_sq = x$tau2$TE, 
                                        I_sq = x$I2$TE, 
                                        H = x$H$TE)) %>% 
         left_join(modeling_summary[, c('parameter', 
                                        'variable', 
                                        'response', 
                                        'level', 
                                        'cohort')] %>% 
                      filter(cohort == 'north'), 
                   by = 'parameter') %>% 
         mutate(cohort = 'pooled', 
                p_adj = p.adjust(p_value, 'BH')) %>% 
         select(response, 
                variable, 
                level, 
                parameter, 
                estimate, 
                se, 
                lower_ci, 
                upper_ci, 
                p_value, 
                p_adj, 
                cohort)
      
      return(pooled_summary)
      
   }
   

# Text functions ----
   
   split_vec <- function(inp_vector, chunk_size) {
      
      return(split(inp_vector, ceiling(seq_along(inp_vector)/chunk_size)))
      
   }
   
   wrap_vector <- function(txt_vec, line_length = 5) {
      
      split_txt <- split_vec(txt_vec, 
                             line_length) %>% 
         map(paste, 
             collapse = ', ') %>%
         paste(collapse = ',\n')
      
      return(split_txt)
      
   }
   
# general table and LATEX formatting -----
   
   format_main_tbl <- function(analysis_table, hide_mean = T) {
      
      ## formats the tables with TY and STY cohort comparisons
      
      if(hide_mean) {
         
         out_tbl <- analysis_table %>% 
            mutate(strata1 = stri_replace(strata1, 
                                          regex = 'mean\\(SD\\).*\n', 
                                          replacement = ''), 
                   strata2 = stri_replace(strata2, 
                                          regex = 'mean\\(SD\\).*\n', 
                                          replacement = ''))
         
      } else {
         
         out_tbl <- analysis_table
         
      }
      
      out_tbl <- out_tbl %>% 
         mutate(variable = translate_var(variable, short = F), 
                strata1 = stri_replace(strata1, 
                                       fixed = 'complete', 
                                       replacement = ''), 
                strata2 = stri_replace(strata2, 
                                       fixed = 'complete', 
                                       replacement = ''), 
                strata1 = stri_replace(strata1, 
                                       fixed = 'young', 
                                       replacement = 'up to 30 y') %>% 
                   stri_replace(fixed = 'middle-aged', 
                                replacement = '31 - 65 y') %>% 
                   stri_replace(fixed = 'elderly', 
                                replacement = '65 y and more'), 
                strata2 = stri_replace(strata2, 
                                       fixed = 'young', 
                                       replacement = 'up to 30 y') %>% 
                   stri_replace(fixed = 'middle-aged', 
                                replacement = '31 - 65 y') %>% 
                   stri_replace(fixed = 'elderly', 
                                replacement = '65 y and more')) %>% 
         mutate(significance = ifelse(is.na(p_U), 
                                      p_chi, 
                                      p_U), 
                test = ifelse(is.na(p_U), 
                              'Chi', 
                              'U'), 
                significance = p.adjust(significance, 'BH'), 
                significance = ifelse(significance < 0.05, 
                                      paste('p =', signif(significance, 2)), 
                                      'ns')) %>% 
         select(variable, 
                strata1, 
                strata2, 
                test, 
                significance) %>% 
         set_names(c('Variable', 
                     'AT', 
                     'IT', 
                     'Test', 
                     'Significance'))
      
      return(out_tbl)
      
   }
   
   format_line <- function(inp_tbl) {
      
      ## formats the given table to include percent marks and new lines
      
      require(kableExtra)
      
      new_tbl <- inp_tbl %>% 
         map_dfc(stri_replace_all, 
                 fixed = '%', 
                 replacement = '\\%') %>% 
         map_dfc(stri_replace_all, 
                 fixed = 'Chi', 
                 replacement = '$\\chi^2$') %>% 
         map_dfc(stri_replace_all, 
                 fixed = '#', 
                 replacement = 'Number of') %>%
         map_dfc(linebreak)
      
      return(new_tbl)
      
   }

   
# Kappa and factor variable overlap -----
   
   get_kappa <- function(data, variable1, variable2, kappa_only = T) {
      
      ## calculates proportions and unweighted Cohen's Kappa for two variables

      cross_tbl <- CrossTable(data[[variable1]], 
                              data[[variable2]], 
                              chisq = T)
      
      kappa_stat <- Kappa(cross_tbl$t)
      
      kappa_ci <- confint(kappa_stat)
      
      kappa_tbl <- tibble(kappa = kappa_stat[['Unweighted']][1], 
                          se = kappa_stat[['Unweighted']][2]) %>% 
         mutate(z = kappa/se, 
                lower_ci = kappa_ci['Unweighted', 1], 
                upper_ci = kappa_ci['Unweighted', 2], 
                p_value = 2*pnorm(z, lower.tail = F), 
                variable1 = variable1, 
                variable2 = variable2) ## two-tailed z test
      
      if(kappa_only) {
         
         return(kappa_tbl)
         
      } else {
         
         return(list(cross_tbl = cross_tbl, 
                     kappa = kappa_tbl))
         
      }
      
   }
   
   plot_kappa <- function(kappa_table, 
                          plot_title = NULL, 
                          plot_subtitle = NULL, 
                          plot_tag = NULL) {
      
      ## plots kappas between variable pairs as a heat map
      
      kappa_hm <- kappa_table %>% 
         mutate(plot_lab = paste0(signif(kappa, 2), 
                                  '\n[', 
                                  signif(lower_ci, 2), 
                                  ' - ', 
                                  signif(upper_ci, 2), 
                                  ']\np = ', 
                                  signif(p_adj, 2))) %>% 
         ggplot(aes(x = variable2, 
                    y = variable1,
                    fill = kappa)) + 
         geom_tile(color = 'gray40') + 
         geom_text(aes(label = plot_lab), 
                   size = 2.6) + 
         scale_x_discrete(limits = overlap$mental_vars, 
                          labels = overlap$mental_labs) + 
         scale_y_discrete(limits = overlap$mental_vars, 
                          label = overlap$mental_labs) + 
         scale_fill_gradient2(low = 'steelblue', 
                              mid = 'white', 
                              high = 'firebrick', 
                              name = 'Kappa', 
                              midpoint = 0.5, 
                              limits = c(0, 1)) + 
         globals$common_theme + 
         theme(axis.title = element_blank(), 
               panel.grid.major = element_line(color = 'gray90')) + 
         labs(title = plot_title, 
              subtitle = plot_subtitle, 
              tag = plot_tag)
      
      return(kappa_hm)
      
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
   
   strat_quartile <- function(inp_tbl, numeric_variable, quant_vec = c(0.25, 0.5, 0.75), 
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