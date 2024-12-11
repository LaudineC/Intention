



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##
##### load or install libraries %%%%%%%%%%%##
#####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##

#------- cran_packages ---------
library(pacman)

rm(list=ls())


p_load(here, 
       tidyverse, 
       tidymodels,                # Tidyverse for data wrangling, ggplot2 etc.
       gtsummary,                 # other package for summary tables and regressions (balance test easy !)
       ggpubr,                    # Nice graphical possibilities for publication ready plots
       sf,                        # Simple features for spatial data
       spData,                    # Spatial data
       flextable,                   # Rmarkdown tables pretty good with word exports (and other formats)
       fixest,
       glmnet, 
       skimr, 
       bonsai, 
       rio, 
       broom, 
       knitr,
       kableExtra, # for nice tables
       modelsummary,
       data.table
       )
#
## Packages for importing, storing and manipulating data
#
#p_load(devtools,                 # Packages from git an others
#       tidyverse,                # Tidyverse for data wrangling, ggplot2 etc.
#       broom,                    # Broom for tidy methods
#       lubridate,                # For Date-time formats
#       readxl,                   # Read write excel files
#       officer                   # Manipulate office documents from Rmarkdown
#       )
#
## Packages used for models and estimations
#
#p_load(fixest,                    # Fast fixed-effect models
#       estimatr,                  # Wrappers of models for treatment effects
#       hdm,                       # High dimensional models : lasso and post-lasso from Chernozukov et al.
#       grf,                       # Generalised random forest from Athey et al.
#       fwildclusterboot,          # Fast wild cluster bootstrap from McKinnon et al (2023)
#       glmnet,                    # Generalised linear model with regularization (lasso, ridge and elasticnets)
#       nnet,                      # Neural network models and multinomial logit
#       multcomp,                  # Multiple comparisons
#       twang                     # Functions for propensity score estimations and weightings + non-response analysis and weighting
#      )
#
#
## Package used for generating, formatting and exporting plots and tables
#
#p_load(modelsummary,              # data and model summary tables and plots
#       gtsummary,                 # other package for summary tables and regressions (balance test easy !)
#       flextable,                 # Rmarkdown tables pretty good with word exports (and other formats)
#       #kableExtra,               # Good package for LaTeX export (and html) but not word documents 
#       viridis,                   # Viridis palettes
#       RColorBrewer,              # Predifined palettes 
#       gglgbtq,                   # color palettes with LGBTQIA flags
#       ggh4x,                     # Hack for some ggplot functions (in particular, useful for adapting facets)
#       cowplot, patchwork,        # Two packages to build figures from ggplot objects
#       gridExtra,                 # additional functions for grids of graphics
#       hrbrthemes,                # Additional thems for ggplot
#       extrafont, extrafontdb     # extra fonts in R and Rmarkdown
#       )
#
#
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##

select <- dplyr::select
summarize <- dplyr::summarize


## Colorblind-friendly palettes
# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# The palette with black:
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


custom.col <- c( "#C4961A","#FFDB6D",
                 "#D16103", "#F4EDCA",  "#52854C",  "#C3D7A4", "#293352","#4E84C4")






# Register Times New Roman (replace with the appropriate font name if necessary)
#font_import(pattern = "Times New Roman")

#GGplot default theme
Maintheme <-   theme_bw()+  
  theme(text = element_text(family = "Times New Roman"),
        legend.position = "bottom",
        plot.title=element_text(hjust = 0.5,margin=unit(c(0, 0, .6, 0), "cm"),size=12, 
                                face="bold",family = "Times New Roman"),
        plot.subtitle=element_text(hjust = 0.2,margin=unit(c(0, 0, .3, 0), "cm"),size=11),
        plot.caption = element_text(hjust = 0, size=10,family = "Times New Roman"),
        #axis.text.x = element_text(angle = 45),
        axis.text.x = element_text(angle = 0),
        axis.title.y = element_text(angle=90),
        strip.background = element_rect(fill = "lightgrey"),
        plot.margin = unit(c(0, 0, 1, 0), "cm")
  )

theme_set(Maintheme)

SourcesStacked <- "stacked database of pairwise comparisons."
SourcesMain <- "endline database."


