
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Using decomposition techniques and machine learning to investigate the determinants of socioeconomic inequalities in early childcare access

------------------------------------------------------------------------

**All this project’s materials are free and open**.

- [Replicate the findings](#replicate)
- [Check out the files glossary for this repository](#glossary)

![Open data](images/data_large_color.png)   ![Open
materials](images/materials_large_color.png)  
![Preregistration](images/preregistered_large_color.png)

------------------------------------------------------------------------

## Abstract

Early childcare has a strong equalizing potential, yet access remains
socioeconomically stratified. This study examines how these
socioeconomic inequalities emerge and widen across three stages of the
early childcare access process: intention to use early childcare during
pregnancy, application, and actual access during the child’s first year.
Using longitudinal data on approximately 2,000 families in France,
collected during pregnancy and followed one year after birth, we
document a progressive widening of gaps along the access pathway.
Compared with high–socioeconomic status (SES) households, low–SES
households are 15 percentage points less likely to intend to use early
childcare, 20 percentage points less likely to apply, and 31 percentage
points less likely to obtain access. To identify the determinants of
these gaps, we combine machine learning for variable selection with
decomposition analyses that quantify the contribution of observable
factors at each stage across a wide range of 40 predictors. At the
intention stage, most of the SES gap is accounted for by differences in
observable characteristics related to resources, constraints, and
available alternatives, with norms contributing little. At subsequent
stages, inequalities increasingly reflect institutional barriers. The
largest disparities emerge at the access stage, where spot
allocation-related factors account for most of the gap, particularly
application timing, employment status, income, and knowledge of the
childcare system. Overall, the results show that socioeconomic
stratification in early childcare access is closely linked to the timing
and design of the access processes, even in systems intended to be
universal.

------------------------------------------------------------------------

This repository contains the code for the revisions of the paper.

Our pre-print is online here:

> 20. 2025. “Using decomposition techniques and machine learning to
>           investigate the determinants of socioeconomic inequalities
>           in early childcare access”“. Accessed July 3, 2026. Online
>           at
>           <https://osf.io/cn3wb/overview?view_only=cbb09270889c4f8a92179ccacec4da23>

## How to download and replicate

To reproduce the findings and re-run the analysis, do the following:

1.  Download and install this repository.
2.  Open `Intention.Rproj` to open an [RStudio
    Project](https://r4ds.had.co.nz/workflow-projects.html).
3.  Within the RStudio Project, open the `Manuscrit.qmd` file and run it
    to replicate the figures and tables of the main paper
4.  Within the RStudio Project, open the `Supplementary Materials.qmd`
    file and run it to replicate the figures and tables of the SI
    Appendix. Note that for convenience (it is much slower), Lasso and
    Elastic nets robustness checks are performed in another file:
    `Lasso_Elastic_Nets.qmd`. Same for the RF with Recursive Feature
    Elimination: `RFE_RFtuning_def.qmd`. Running this code is at your
    own discretion. It may take several days to run on a standard
    computer.

The source code can be found in the `.qmd` and `.R` files.

Please find the [session info below](#session-info).

*Estimated installation time* : If you have R and Rstudio installed,
then adding additional packages required in our R scripts is quick (a
couple of minutes)

*Estimated run time*: Rendering the `Manuscrit.qmd` document takes about
5mins on our machine, both on Mac and PC (see [session
info](#session-info))

## Files glossary

The `codebook.csv` contains all variables of our data set.

The `Data/` folder contains the main data sets.

- `Data/MainDB_analysis.csv` is the main data set used for the analyses.
- `Data/Sapatial Data/txcouv_pe_com_EAJE_assmat.csv` contains data on
  early childcare access in France necessary for

The `RScripts/` folder contains the scripts necessary to that we rely
upon in our analysis scripts. To keep the `Manuscrit.Rmd` document neat
and readable, we wrote all lengthy code bits as external functions.

- `RScripts/AllFunctions.R` contains all functions used in the analysis.
- `RScripts/LoadAllData.R` contains the code to load the data.
- `RScripts/LoadInstallPackages.R` contains the code to install and load
  all necessary packages.

`Figures/` contains the figures used in the manuscript.

`.bib` contains all references for the manuscript and `.csl` the
citation formatting.

`images/` contains images used for this ReadME file.

## Session info

    #> R version 4.5.0 (2025-04-11)
    #> Platform: x86_64-apple-darwin20
    #> Running under: macOS Ventura 13.7.8
    #> 
    #> Matrix products: default
    #> BLAS:   /Library/Frameworks/R.framework/Versions/4.5-x86_64/Resources/lib/libRblas.0.dylib 
    #> LAPACK: /Library/Frameworks/R.framework/Versions/4.5-x86_64/Resources/lib/libRlapack.dylib;  LAPACK version 3.12.1
    #> 
    #> locale:
    #> [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    #> 
    #> time zone: America/Chicago
    #> tzcode source: internal
    #> 
    #> attached base packages:
    #> [1] stats     graphics  grDevices utils     datasets  methods   base     
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] compiler_4.5.0    fastmap_1.2.0     cli_3.6.5         tools_4.5.0      
    #>  [5] htmltools_0.5.8.1 rstudioapi_0.17.1 yaml_2.3.10       rmarkdown_2.29   
    #>  [9] knitr_1.50        xfun_0.52         digest_0.6.37     rlang_1.1.6      
    #> [13] evaluate_1.0.3
