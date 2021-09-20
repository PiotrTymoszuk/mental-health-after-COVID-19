# this script renders a word file with the figures and supplementary material

  insert_head()

# tools -----

  library(bookdown)
  library(knitr)
  library(rmarkdown)
  library(kableExtra)

# rendering the figures and tables ------

  insert_msg('Rendering the figures and tables')
  
  render('./paper/figures_and_tables.Rmd', 
         output_format = pdf_document2(number_sections = F)) 
  
  render('./paper/figures_and_tables.Rmd', 
         output_format = word_document2(number_sections = F)) 
  
# rendering the supplementary material -----
  
  insert_msg('Rendering the supplementary material')
  
  render('./paper/supplementary_material.Rmd', 
         output_format = pdf_document2(number_sections = F)) 
  
# END ------
  
  insert_tail()