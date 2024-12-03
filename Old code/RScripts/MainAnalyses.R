#----------------------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------ Carbuccia et al. (2024)  --------------------------------------------------------#
#------------------------------------------      Main analysis       --------------------------------------------------------# 
#------------------------------------------   Author: Arthur Heim    --------------------------------------------------------#    
#------------------------------------------         Version 1        --------------------------------------------------------#  
#------------------------------------------         June 2024        --------------------------------------------------------#     
#----------------------------------------------------------------------------------------------------------------------------#
#----------------------------------------------------------------------------------------------------------------------------#


# Instructions and comments for replication
# This file is built with chunk labels than are called from the main Rmarkdown document.
# The file should be run from the R project "Analysis_Notebooks".
# These chunks are to be launched after running the package loading and data wrangling scripts.




#------ BalanceTable ----------
## This chunk construct the balance table of covariates at baseline for the appendix

tabVar <- MainDB  %>% 
  mutate(
    SingleMum1or0 = ifelse(SingleMum == TRUE, 1, 0),
    CoupleCohabiting1or0 = ifelse(CoupleCohabiting == TRUE, 1, 0),
    AgeSup321or0 = ifelse(AgeSup32 == "Yes", 1, 0),
    Active1or0 = ifelse(Act3 == "Active", 1, 0),
    Educ1or0  = ifelse(Educ == "Sup", 1, 0),
    BornFr1or0  = ifelse(FrenchYNBaseline == "France", 1, 0),
    EverUsedECS1or0  = ifelse(UsedECEC == "Oui", 1, 0),
    PlanToUseECS1or0 = ifelse(ECSPlanToBaseline == TRUE, 1, 0),
    ComputerYes1or0 = ifelse(ComputerYN == "Oui", 1, 0),
    HighECSCov1or0 = ifelse(HighLowECECBaseline == "High ECEC covering", 1, 0), 
    DepParis1or0  = ifelse(Dep == "75", 1, 0),
    KnowsCrecheOnly1or0  = ifelse(KnowsCrecheOnly == TRUE, 1, 0),
    DidNotSmoke1or0 = ifelse(SmokePreg == "Did Not Smoke", 1, 0), 
    Primipare1or0 = ifelse(Primipare == TRUE, 1, 0), 
    WorkPlanTo1or0 = ifelse(WorkPlanTo == TRUE, 1, 0), 
    DeprivHolidays1or0 = ifelse(DeprivHolidays != "Pas du tout", 1, 0), NumberChildren = as.numeric(NumberChildren), 
    BabyFemale = ifelse(BabyFemale == TRUE, 1, 0)
  ) %>%  filter(NumberChildren <3) %>%
  select(
    "Assignment" = Assignment,
    "Single parent family" = SingleMum1or0,
    #  "Couple cohabiting" = CoupleCohabiting1or0,
    "Age of the mother" = Age,
    "Number of children" = NumberChildren,
    # "The mother has no child" = Primipare1or0,
    "Born in France" = BornFr1or0,
    "The mother has a post-secondary education" = Educ1or0,  # Strata: Educ: ≤ Bac or higher
    
    #  "The mothers is not born in MENAnorAsia" = BirthNotAsiaMENA1or0,
    "The mother is active" = Active1or0,
    "The mother want to work after maternity leaves" = WorkPlanTo1or0,
    "The family earn less than €2,500 per month" = FmilyEarnLessThan2500,
    #  "The family couldn't afford holidays" = DeprivHolidays1or0,
    "Has a computer" = ComputerYes1or0,
    "The mother is present orientated" = Discount501or0,
    "The mother did not smoke" = DidNotSmoke1or0,
    "The mother wants to breastfeed" = BreastFeedIntend1or0, 
    "Has ever used early childcare" = EverUsedECS1or0,                  # Used: yes/no/ don't wanna answer
    "Want to use early childcare" = PlanToUseECS1or0,                 # Intend to use, block variable
    "Knows early childcare is subsidised" = AffordSubsidies1or0,
    "Knows only daycare" = KnowsCrecheOnly1or0,
    #    "Value socialization" = ValSocialisation,
    "Believe in returns to early childcare" = LikertReturnHK1or0,
    "The mother trusts early childcare" = TrustCreche1or0,
    "The mother lives in Paris" = DepParis1or0,
    "Early childcare coverage is high" = HighECSCov1or0,
    "Child is a girl" = BabyFemale,
  #  StrataWave
  )


summary_baseline_variables_endline <- tabVar %>%
  tbl_summary(
    by = Assignment,
    statistic = list(
      all_continuous() ~ "{mean} ({sd})",
      all_categorical() ~ "{p}% ({n})"),
    digits = all_continuous() ~ 2,
    missing = "no"
  ) %>%
  add_overall() %>% #add_difference() %>% 
  modify_header(label ~ "**Variable**",
                stat_0 ~ "Overall",
                stat_1 ~ "Info only (T1)",
                stat_2 ~ "Info + admin. support (T2)",
                stat_3 ~ "Control") %>%
  modify_spanning_header(c("stat_1", "stat_2","stat_3") ~ "**Assignment group**") %>% 
  add_p(
    pvalue_fun = ~ style_pvalue(.x, digits = 3)
  ) %>% 
  add_q(method = "fdr",
    pvalue_fun = ~ style_pvalue(.x, digits = 3))%>% 
  add_significance_stars(  thresholds = c(0.01, 0.05, 0.1))


summary_baseline_variables_endline %>% 
    as_flex_table() %>% 
  merge_v(part = "header", j = 1) %>% 
  merge_v(part = "header", j = 2) %>% 
  merge_v(part = "header", j = 3) %>% 
  merge_h(part = "header", i = 1) %>% 
  italic(j=1,part="body")|>  
  width(j=c(1),unit = "cm",width=3)|>
  width(j=c(2:7),unit = "cm",width=2)|>
  set_caption(caption = "Balance test of baseline characteristics across assignment groups") %>% 
  add_footer_lines(
"Sources: Baseline database. Proportions and number of observations in parentheses for categorical and dichotomous variables and Pearson's Chi-squared test.
We report averages and standard deviations in parentheses for continuous variables and use a Kruskal-Wallis rank sum test. Q-value control for the false discovery rate.")  %>%  fontsize(i=1,size=10,part="footer") #%>% print()
  


#------ AttentionAction --------

#The idea is to  plot the average difference between high/low SES for 3 outcomes
#outcomes <- c("ECSPlanToBaseline", "ECSApp", "ECSUseYes")

# Simplest, fastest way of doing that: OLS 
DiffControls <- feols(c(ECSPlanToBaseline,ECSApp,ECSUseYes)~Educ2,
                      MainDB %>% filter(Assignment=="Control") %>%mutate(ECSPlanToBaseline = ifelse(ECSPlanToBaseline == TRUE, 1, 0)),
                      #weights = ~WeightBalance,
                      se="hetero")

DiffControlsFr <- feols(c(ECSPlanToBaseline,ECSApp,ECSUseYes)~i(FrenchYNBaseline,ref="France"),
                        MainDB %>% filter(Assignment=="Control") %>%mutate(ECSPlanToBaseline = ifelse(ECSPlanToBaseline == TRUE, 1, 0)),
                        #weights = ~WeightBalance,
                        se="hetero")



res <-  DiffControls %>% modelplot(,draw=FALSE) %>% mutate(Heterogeneity="Baseline education")%>% 
  mutate(Outcome=str_remove(model,'lhs:'),term=ifelse(str_detect(term,'Low-SES'),'SES gap','Mean High SES'))
resb <-  DiffControlsFr %>% modelplot(,draw=FALSE) %>% mutate(Heterogeneity="Country of birth")%>% mutate(Outcome=str_remove(model,'lhs:'),
                                                                                                          term=ifelse(str_detect(term,'Abroad'),'Migrant gap','Mean French'))

res2 <- bind_rows(res,resb) %>% mutate(OutcomeLabel=factor(case_when(str_detect(Outcome,"ECSPlanToBaseline")~"Intend to use \nBaseline",
                                                                     str_detect(Outcome,"ECSApp")~"Apply \nEndline",
                                                                     str_detect(Outcome,"ECSUseYes")~"Access \nEndline"),
                                                           levels=c("Intend to use \nBaseline","Apply \nEndline","Access \nEndline")
))


# same with discount and information
DiffControlsDisc <- feols(c(ECSPlanToBaseline,ECSApp,ECSUseYes)~i(Discount501or0,0),
                            MainDB %>% filter(Assignment=="Control") %>%
                            mutate(ECSPlanToBaseline = ifelse(ECSPlanToBaseline == TRUE, 1, 0)),
                          #weights = ~WeightBalance,
                          se="hetero")

DiffControlsInfo <- feols(c(ECSPlanToBaseline,ECSApp,ECSUseYes)~i(InfoBaseline),MainDB %>% filter(Assignment=="Control") %>%
                            mutate(ECSPlanToBaseline = ifelse(ECSPlanToBaseline == TRUE, 1, 0)),
                          #weights = ~WeightBalance,
                          se="hetero")



res <-  DiffControlsDisc %>% modelplot(,draw=FALSE) %>% mutate(Heterogeneity="Present bias")%>% mutate(Outcome=str_remove(model,'lhs:'),
                                                                                                       term=ifelse(str_detect(term,'Intercept'),'Mean low present bias','Present bias gap'))
resb <-  DiffControlsInfo %>% modelplot(,draw=FALSE) %>% mutate(Heterogeneity="Baseline information")%>% mutate(Outcome=str_remove(model,'lhs:'),
                                                                                                                term=ifelse(str_detect(term,'Low'),'Low information gap','Mean high information group'))

res2b <- bind_rows(res,resb) %>% mutate(OutcomeLabel=factor(case_when(str_detect(Outcome,"ECSPlanToBaseline")~"Intend to use \nBaseline",
                                                                      str_detect(Outcome,"ECSApp")~"Apply \nEndline",
                                                                      str_detect(Outcome,"ECSUseYes")~"Access \nEndline"),
                                                            levels=c("Intend to use \nBaseline","Apply \nEndline","Access \nEndline")
))

# stack the two results and use "Heterogeneity" for facets
Stack.Intend.Gap <- bind_rows(res2,res2b) %>% 
  mutate(termPlot=factor(term,levels=c('SES gap','Migrant gap','Low information gap','Present bias gap')))

Intend.Gap.plot = 
  ggplot(Stack.Intend.Gap %>% filter(str_detect(term,"gap")))+geom_bar(aes(x=OutcomeLabel,y=estimate,fill=OutcomeLabel),stat='identity',alpha=.4)+
  geom_errorbar(aes(x=OutcomeLabel,ymin=conf.low,
                    ymax =conf.high,
                    ,color=OutcomeLabel),width=.3)+
  facet_wrap(~termPlot)+#,scales = "free_y")+
  scale_x_discrete(name = "Outcomes",guide = guide_axis_nested(delim = "\n"))+
  scale_fill_viridis_d("Outcomes",alpha=.8,option="A",end=.6)+
  scale_color_viridis_d("Outcomes",alpha=.8,option="A",end=.6)+
  labs(title='Intention-to-access gaps across sub-groups',
       caption=
"Sources: Control group only. Intention measured at the baseline survey during pregnancy (Q4 2022); application and access 
are measured at the endline survey one year after (Q4 2023).
Notes: Coefficients of OLS regressions of the outcomes on dummies for the group variable of interest. Error bars indicate 
pointwise 95% CI based on heteroskedasticity-robust standard errors. Intention-to-action gap in early childcare application 
and access in the control group across four dimensions of heterogeneity:
- SES gap compares mothers with less than a high school diploma with those with tertiary education.
- Migrant gap compares mothers born abroad with mothers born in France.
- Low information gap compares parents with very low information with those wih knowledge of ECS.
- Present bias gap compares mothers who prefer 50€ in three days to those who prefer 80 € in 3 months."
  )

Intend.Gap.plot




#----- TableITT -----

set.seed(666)


ITT.UseCreche <- ITTSimultaneous(Y="UseCreche",weights = "WeightBalance")
ITT.ECSUseYes <- ITTSimultaneous(Y="ECSUseYes",weights="WeightBalance")
ITT.ECSApp <- ITTSimultaneous(Y="ECSApp",weights="WeightBalance")
ITT.ECSAppCreche <- ITTSimultaneous(Y="AppCreche",weights="WeightBalance")

#Coef Map for clear labels
cm <- c('T1-C'    = 'Information vs Control ',
        'T2-C'    = 'Support + Information vs Control',
        'T2-T1'   = 'Support vs Information',
        "Control mean" = "Mean ctrl. group")

#empty <- list(tidy=as.data.frame())

MainResultTable =
modelsummary(list(
  "Application_Any childcare"=ITT.ECSApp$ModelSummary,
  "Application_Daycare"=ITT.ECSAppCreche$ModelSummary,
  "Access_Any childcare"=ITT.ECSUseYes$ModelSummary,
  "Access_Daycare"=ITT.UseCreche$ModelSummary
),
coef_map = cm,
fmt=fmt_statistic(estimate=2, adj.p.value=3,std.error=2,conf.int=2,"Chi 2"=2,"P-value"=3), 
estimate = '{estimate}{stars} ({std.error})',
statistic = c("conf.int",
              "adj.p.val. = {adj.p.value}"),
#stars=FALSE,
stars = c('*' = .1,'**' = .05, '***' = .01),
gof_map = c(#"Mean of DV",
  "Covariates","Fixed effects","Chi 2","P-value",
  "nobs", "r.squared","adj.r.squared"),
title="Intention-to-treat effect on the main outcomes",
notes=paste("Sources:", SourcesStacked,
            "
*= p<.1, **= p<.05, ***= p<.01 based on point-wise p-value.
Standard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.
Adjusted p-value and confidence intervals account for simultaneous inference using the",ITT.UseCreche$Correction, "method. 
Each column estimates jointly the effects of the program using fully-saturated stacked regressions. Control means estimated separately by OLS.
Joint significance test of null effect using Chi-2 test and p-value are reported at the bottom of the table."),
output = 'flextable') 


MainResultTable %>% 
  merge_at(j=1,i=c(1:3),part="body")|>   
  merge_at(j=1,i=c(4:6),part="body")|>   
  merge_at(j=1,i=c(7:9),part="body") |>   
  merge_at(j=1,i=c(10:11),part="body") |>   
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   # Separate headers
  merge_at(j=1,i=c(1:2),part="header") %>% 
  #merge_v(j = 1,part="header") %>% 
  italic(i = c(2),  part = "header") %>% 
  bold(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                # center
  align(part = "body", align = "center")|>                # center   width(j=1,width=3.5,unit = "cm")|>
  width(j=c(2,3,4,5),width=2.7,unit = "cm")|>
  width(j=c(1),width=2.4,unit = "cm") %>% 
  hline(c(9,11),part="body")



#------ HetEducationT1 ------------


Het.ITT.App.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                               Outcome = "ECSApp",
                                               Heterogeneity = "Educ2",
                                               ITT = TRUE,
                                               Weights = "WeightPS",
                                               clusters = "StrataWave")


Het.ITT.App.Mig <- GroupHeterogeneityFnCTRL(DB = PostDB,
                                            Outcome = "ECSApp",
                                            Heterogeneity= "FrenchYNBaseline",
                                            ITT = TRUE,
                                            Weights = "WeightPS",
                                            clusters = "StrataWave")



Het.ITT.App.Info <- GroupHeterogeneityFnCTRL(DB = PostDB %>% mutate(
  InfoBaseline=ifelse(LevelInfoSubExPost == "Aucun ou très bas","Low information","High information")),
  Outcome = "ECSApp",
  Heterogeneity = "InfoBaseline",
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave")



Het.ITT.App.Discount <- GroupHeterogeneityFnCTRL(DB = PostDB%>% mutate(
  Discount501or0=ifelse(Discount501or0 == 1,"Present Orientated","Future Orientated")),
  Outcome = "ECSApp",
  Heterogeneity = "Discount501or0",
  ITT = TRUE,
  Weights = "WeightPS",
  clusters = "StrataWave")


# Définir les niveaux des facteurs pour un ordre personnalisé
term_levels <- c("T1-C")
heterogeneity_levels <- c("SES", "Migration background", "Level of knowledge", "Present bias")
panel_levels <- c("Control group", "ITT")

# Fusionner les données en un seul DataFrame avec les niveaux de facteur appropriés
DataPlot <- bind_rows(
  Het.ITT.App.Educ2C$ModelSummary0$tidy %>% mutate(Y = "Apply for early childcare", panel = "Control group", Heterogeneity = "SES") %>% filter(term %in% term_levels),
  Het.ITT.App.Educ2C$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ITT", Heterogeneity = "SES") %>% filter(term %in% term_levels),
  Het.ITT.App.Mig$ModelSummary0$tidy %>% mutate(Y = "Apply for early childcare", panel = "Control group", Heterogeneity = "Migration background") %>% filter(term %in% term_levels),
  Het.ITT.App.Mig$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ITT", Heterogeneity = "Migration background") %>% filter(term %in% term_levels),
  Het.ITT.App.Info$ModelSummary0$tidy %>% mutate(Y = "Apply for early childcare", panel = "Control group", Heterogeneity = "Level of knowledge") %>% filter(term %in% term_levels),
  Het.ITT.App.Info$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ITT", Heterogeneity = "Level of knowledge") %>% filter(term %in% term_levels),
  Het.ITT.App.Discount$ModelSummary0$tidy %>% mutate(Y = "Apply for early childcare", panel = "Control group", Heterogeneity = "Present bias") %>% filter(term %in% term_levels),
  Het.ITT.App.Discount$Tidy %>% mutate(Y = "Apply for early childcare", panel = "ITT", Heterogeneity = "Present bias") %>% filter(term %in% term_levels)
) %>%
  mutate(
    term = factor(term, levels = term_levels),
    Heterogeneity = factor(Heterogeneity, levels = heterogeneity_levels),
    panel = factor(panel, levels = panel_levels)
  )


# Create a named vector for the x-axis labels
x_labels <- c(
  "T1-C!Apply for early childcare!SES" = "SES",
  "T1-C!Apply for early childcare!Migration background" = "Migration background",
  "T1-C!Apply for early childcare!Level of knowledge" = "Level of knowledge",
  "T1-C!Apply for early childcare!Present bias" = "Present bias"
)
# Tracer le graphique avec les facteurs ordonnés
DataPlot %>%
  ggplot() +
  geom_pointrange(aes(
    x = interaction(term, Y, Heterogeneity, sep = "!"),
    y = estimate, ymin = point.conf.low,
    ymax = point.conf.high, color = Group
  ), position = position_dodge(.6)) +
  geom_crossbar(aes(
    y = estimate, x = interaction(term, Y, Heterogeneity, sep = "!"),
    fill = Group, ymin = conf.low,
    color = Group, ymax = conf.high
  ), position = position_dodge(.6), alpha = .2, fatten = 2, width = .4) +
  scale_x_discrete(labels = x_labels, name = "Heterogeneity") +
  coord_flip() +
  facet_grid(Heterogeneity ~ panel, scales = "free_y", space = "free_y") +
  scale_fill_brewer("Heterogeneity", palette = "Dark2", limits = c("High-SES", "Low-SES", "France", "Abroad", "Low information","High information","Present Orientated","Future Orientated"))+
  scale_color_brewer("Heterogeneity",palette = "Dark2",limits = c("High-SES", "Low-SES", "France", "Abroad", "Low information","High information","Present Orientated","Future Orientated"))+
  scale_shape_manual("Model:", values = c(4:8)) +
  guides(col = guide_legend(ncol = 2)) + # Ajuste le nombre de colonnes de la légende des couleurs
  theme(legend.position = "right", legend.box = "vertical") + # Ajuste la position de la légende
  geom_hline(aes(yintercept = 0), linetype = c(2)) + # Dashed line for randomization date
  ylab("Estimates") +
  guides(
    col = guide_legend(ncol = 2),
    fill = guide_legend(ncol = 2)
  ) +
  labs(
    title = "Heterogeneous ITT of the information-only treatment \non early childcare application",
    #subtitle = "",
    caption = paste("Sources:", SourcesStacked,
                    "\nStandard errors are cluster-heteroskedasticity robust adjusted at the block x wave level.",
                    "\nPoint indicates the ITT and the error bars indicate pointwise 95% CI.",
                    "\nThe Fixed effect model is estimated with block x wave x subsample fixed effects and inverse probability weighting.")
  )




#------ HetEducAccessCrecheLate --------


Het.LATE.UseCreche.Educ2C <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                      Outcome = "UseCreche",
                                                      Heterogeneity = "Educ2",
                                                      ITT = FALSE,
                                                      Weights = "WeightPS",
                                                      clusters = "StrataWave")



Test.UseCreche.Educ2.Late <- CompareCoefDelta(Model = Het.LATE.UseCreche.Educ2C,
                                         Padjust.Method = "Westfall",
                                         OutcomeLabel="daycare access",
                                         GroupLabel="SES status")

# get the automatic table
Test.UseCreche.Educ2.Late$Summary.Model 


#------ HetIntentionAccessCrecheLate --------


Het.LATE.UseCreche.intend <- GroupHeterogeneityFnCTRL(DB = PostDBT2,
                                                      Outcome = "UseCreche",
                                                      Heterogeneity = "Intention",
                                                      ITT = FALSE,
                                                      Weights = "WeightPS",
                                                      clusters = "StrataWave")



Test.UseCreche.intend.Late <- CompareCoefDelta(Model = Het.LATE.UseCreche.intend,
                                              Padjust.Method = "Westfall",
                                              OutcomeLabel="daycare access",
                                              GroupLabel="intention to use")

# get the automatic table
Test.UseCreche.intend.Late$Summary.Model 




#----- RobustnessITTECSApp -------

#To use the function, the default should work on it's own (recall that there are cross validation so it takes time to run)
ITT.EcsAppT2T1 <- EstPostDoubleLasso(Y="ECSApp",
                                     Z="Z",
                                     Cluster="SubSampleStrata",
                                     FE="SubSampleStrata",
                                     weights = "WeightPS",
                                     SubSample = "T2-T1",
                                     DB=PostDB,
                                     X=X.c)
ITT.EcsAppT2C <- EstPostDoubleLasso(Y="ECSApp",
                                    Z="Z",
                                    Cluster="SubSampleStrata",
                                    FE="SubSampleStrata",
                                    weights = "WeightPS",
                                    SubSample = "T2-C",
                                    DB=PostDB,
                                    X=X.c)

ITT.EcsAppT1C <- EstPostDoubleLasso(Y="ECSApp",
                                    Z="Z",
                                    Cluster="SubSampleStrata",
                                    FE="SubSampleStrata",
                                    weights = "WeightPS",
                                    SubSample = "T1-C",
                                    DB=PostDB,
                                    X=X.c)

#----- RobustnessITTECSAppTable -------

# Let's put that in a modelsummary 

# First define properties of the borders
TopBottom <- fp_border(color = "black", style = "thick", width = 1.2)

modelsummary(list("Information vs Control_Basic"=ITT.EcsAppT1C$`Basic Model`, 
                  "Information vs Control_Pooled"=ITT.EcsAppT1C$`Post Lasso`,
                  "Information vs Control_Lin regression"=ITT.EcsAppT1C$`ITT Post lasso`,
                  "Information + support vs Control_Basic"=ITT.EcsAppT2C$`Basic Model`, 
                  "Information + support vs Control_Pooled"=ITT.EcsAppT2C$`Post Lasso`,
                  "Information + support vs Control_Lin regression"=ITT.EcsAppT2C$`ITT Post lasso`,
                  "Support vs Information_Basic"=ITT.EcsAppT2T1$`Basic Model`, 
                  "Support vs Information_Pooled"=ITT.EcsAppT2T1$`Post Lasso`,
                  "Support vs Information_Lin regression"=ITT.EcsAppT2T1$`ITT Post lasso`),
             coef_map=c("Z"="Intention-to-treat"),
             title="Applied to any childcare solution",
             fmt=fmt_statistic(estimate=3,std.error=3,conf.int=3), 
             estimate = '{estimate}{stars} ({std.error})',
             #estimate = '{estimate} ({std.error})',
             statistic = c("conf.int"),
             #stars=FALSE,
             stars = c('*' = .1,'**' = .05, '***' = .01),
             gof_map = c("Covariates","FE: FE",
                         "nobs", "r.squared","adj.r.squared"),
             notes=paste("Sources: Premiers pas, follow-up databases, pairwise comparisons.
The dependent variable equals 1 if the parent reports applying to any formal childcare.
Basic specification run OLS on a treatment dummy and block x wave fixed effects.
Pooled specifications add the de-meaned covariates selected by a double-lasso regression with lambda minimising the RMSE chosen by 10-fold cross validation for both the outcome and the treatment. We keep covariates from both post-lasso models in the final estimation. 
Lin regression add interactions of the same de-meaned covariates with the treatment variable.
All models use inverse block-specific propensity scores to account for the design.
Cluster-robust standard errors adjusted at the block level in parenthesis ; point-wise 95% confidence intervals in bracklets."), output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   # Separate headers
  merge_at(j=c(1),i=c(1,2),part="header") %>% 
  merge_at(j=c(1),i=c(1,2),part="body") %>% 
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                # center
  align(part = "body", align = "center")|>                # center   width(j=1,width=3.5,unit = "cm")|>
  width(j=c(2:10),width=2.8,unit = "cm")|>
  width(j=c(1),width=1,unit = "cm") %>% 
  border_remove() %>% 
  hline_bottom(border = TopBottom) %>% hline_top(border=TopBottom) %>% 
  hline_top( part = "header", border = TopBottom) %>% 
  hline(2,part="body") %>% 
  hline(j=c(2:4),i=1,part="header") %>% 
  hline(j=c(5:7),i=1,part="header") %>% 
  hline(j=c(8:10),i=1,part="header")




#----- RobustnessITTECSUseYes -------

#To use the function, the default should work on it's own (recall that there are cross validation so it takes time to run)
ITT.ECSUseYesT2T1 <- EstPostDoubleLasso(Y="ECSUseYes",
                                     Z="Z",
                                     Cluster="SubSampleStrata",
                                     FE="SubSampleStrata",
                                     weights = "WeightPS",
                                     SubSample = "T2-T1",
                                     DB=PostDB,
                                     X=X.c)
ITT.ECSUseYesT2C <- EstPostDoubleLasso(Y="ECSUseYes",
                                    Z="Z",
                                    Cluster="SubSampleStrata",
                                    FE="SubSampleStrata",
                                    weights = "WeightPS",
                                    SubSample = "T2-C",
                                    DB=PostDB,
                                    X=X.c)

ITT.ECSUseYesT1C <- EstPostDoubleLasso(Y="ECSUseYes",
                                    Z="Z",
                                    Cluster="SubSampleStrata",
                                    FE="SubSampleStrata",
                                    weights = "WeightPS",
                                    SubSample = "T1-C",
                                    DB=PostDB,
                                    X=X.c)

#----- RobustnessITTECSUseYesTable -------

# Let's put that in a modelsummary 

# First define properties of the borders
TopBottom <- fp_border(color = "black", style = "thick", width = 1.2)

modelsummary(list("Information vs Control_Basic"=ITT.ECSUseYesT1C$`Basic Model`, 
                  "Information vs Control_Pooled"=ITT.ECSUseYesT1C$`Post Lasso`,
                  "Information vs Control_Lin regression"=ITT.ECSUseYesT1C$`ITT Post lasso`,
                  "Information + support vs Control_Basic"=ITT.ECSUseYesT2C$`Basic Model`, 
                  "Information + support vs Control_Pooled"=ITT.ECSUseYesT2C$`Post Lasso`,
                  "Information + support vs Control_Lin regression"=ITT.ECSUseYesT2C$`ITT Post lasso`,
                  "Support vs Information_Basic"=ITT.ECSUseYesT2T1$`Basic Model`, 
                  "Support vs Information_Pooled"=ITT.ECSUseYesT2T1$`Post Lasso`,
                  "Support vs Information_Lin regression"=ITT.ECSUseYesT2T1$`ITT Post lasso`),
             coef_map=c("Z"="Intention-to-treat"),
             title="Accessed any childcare solution",
             fmt=fmt_statistic(estimate=3,std.error=3,conf.int=3), 
             estimate = '{estimate}{stars} ({std.error})',
             #estimate = '{estimate} ({std.error})',
             statistic = c("conf.int"),
             #stars=FALSE,
             stars = c('*' = .1,'**' = .05, '***' = .01),
             gof_map = c("Covariates","FE: FE",
                         "nobs", "r.squared","adj.r.squared"),
             notes=paste("Sources: Premiers pas, follow-up databases, pairwise comparisons.
The dependent variable equals 1 if the parent reports accessing any formal childcare.
Basic specification run OLS on a treatment dummy and block x wave fixed effects.
Pooled specifications add the de-meaned covariates selected by a double-lasso regression with lambda minimising the RMSE chosen by 10-fold cross validation for both the outcome and the treatment. We keep covariates from both post-lasso models in the final estimation. 
Lin regression add interactions of the same de-meaned covariates with the treatment variable.
All models use inverse block-specific propensity scores to account for the design.
Cluster-robust standard errors adjusted at the block level in parenthesis ; point-wise 95% confidence intervals in bracklets."), output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   # Separate headers
  merge_at(j=c(1),i=c(1,2),part="header") %>% 
  merge_at(j=c(1),i=c(1,2),part="body") %>% 
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                # center
  align(part = "body", align = "center")|>                # center   width(j=1,width=3.5,unit = "cm")|>
  width(j=c(2:10),width=2.8,unit = "cm")|>
  width(j=c(1),width=1,unit = "cm") %>% 
  border_remove() %>% 
  hline_bottom(border = TopBottom) %>% hline_top(border=TopBottom) %>% 
  hline_top( part = "header", border = TopBottom) %>% 
  hline(2,part="body") %>% 
  hline(j=c(2:4),i=1,part="header") %>% 
  hline(j=c(5:7),i=1,part="header") %>% 
  hline(j=c(8:10),i=1,part="header")






#----- RobustnessITTECSAppCreche -------

#To use the function, the default should work on it's own (recall that there are cross validation so it takes time to run)
ITT.ECSAppCrecheT2T1 <- EstPostDoubleLasso(Y="AppCreche",
                                        Z="Z",
                                        Cluster="SubSampleStrata",
                                        FE="SubSampleStrata",
                                        weights = "WeightPS",
                                        SubSample = "T2-T1",
                                        DB=PostDB,
                                        X=X.c)
ITT.ECSAppCrecheT2C <- EstPostDoubleLasso(Y="AppCreche",
                                       Z="Z",
                                       Cluster="SubSampleStrata",
                                       FE="SubSampleStrata",
                                       weights = "WeightPS",
                                       SubSample = "T2-C",
                                       DB=PostDB,
                                       X=X.c)

ITT.ECSAppCrecheT1C <- EstPostDoubleLasso(Y="AppCreche",
                                       Z="Z",
                                       Cluster="SubSampleStrata",
                                       FE="SubSampleStrata",
                                       weights = "WeightPS",
                                       SubSample = "T1-C",
                                       DB=PostDB,
                                       X=X.c)

#----- RobustnessITTECSAppCrecheTable -------

# Let's put that in a modelsummary 

# First define properties of the borders
TopBottom <- fp_border(color = "black", style = "thick", width = 1.2)

modelsummary(list("Information vs Control_Basic"=ITT.ECSAppCrecheT1C$`Basic Model`, 
                  "Information vs Control_Pooled"=ITT.ECSAppCrecheT1C$`Post Lasso`,
                  "Information vs Control_Lin regression"=ITT.ECSAppCrecheT1C$`ITT Post lasso`,
                  "Information + support vs Control_Basic"=ITT.ECSAppCrecheT2C$`Basic Model`, 
                  "Information + support vs Control_Pooled"=ITT.ECSAppCrecheT2C$`Post Lasso`,
                  "Information + support vs Control_Lin regression"=ITT.ECSAppCrecheT2C$`ITT Post lasso`,
                  "Support vs Information_Basic"=ITT.ECSAppCrecheT2T1$`Basic Model`, 
                  "Support vs Information_Pooled"=ITT.ECSAppCrecheT2T1$`Post Lasso`,
                  "Support vs Information_Lin regression"=ITT.ECSAppCrecheT2T1$`ITT Post lasso`),
             coef_map=c("Z"="Intention-to-treat"),
             title="Applied for daycare",
             fmt=fmt_statistic(estimate=3,std.error=3,conf.int=3), 
             estimate = '{estimate}{stars} ({std.error})',
             #estimate = '{estimate} ({std.error})',
             statistic = c("conf.int"),
             #stars=FALSE,
             stars = c('*' = .1,'**' = .05, '***' = .01),
             gof_map = c("Covariates","FE: FE",
                         "nobs", "r.squared","adj.r.squared"),
             notes=paste("Sources: Premiers pas, follow-up databases, pairwise comparisons.
The dependent variable equals 1 if the parent applied for daycare.
Basic specification run OLS on a treatment dummy and block x wave fixed effects.
Pooled specifications add the de-meaned covariates selected by a double-lasso regression with lambda minimising the RMSE chosen by 10-fold cross validation for both the outcome and the treatment. We keep covariates from both post-lasso models in the final estimation. 
Lin regression add interactions of the same de-meaned covariates with the treatment variable.
All models use inverse block-specific propensity scores to account for the design.
Cluster-robust standard errors adjusted at the block level in parenthesis ; point-wise 95% confidence intervals in bracklets."), output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   # Separate headers
  merge_at(j=c(1),i=c(1,2),part="header") %>% 
  merge_at(j=c(1),i=c(1,2),part="body") %>% 
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                # center
  align(part = "body", align = "center")|>                # center   width(j=1,width=3.5,unit = "cm")|>
  width(j=c(2:10),width=2.8,unit = "cm")|>
  width(j=c(1),width=1,unit = "cm") %>% 
  border_remove() %>% 
  hline_bottom(border = TopBottom) %>% hline_top(border=TopBottom) %>% 
  hline_top( part = "header", border = TopBottom) %>% 
  hline(2,part="body") %>% 
  hline(j=c(2:4),i=1,part="header") %>% 
  hline(j=c(5:7),i=1,part="header") %>% 
  hline(j=c(8:10),i=1,part="header")






#----- RobustnessITTUseCreche -------

#To use the function, the default should work on it's own (recall that there are cross validation so it takes time to run)
ITT.UseCrecheT2T1 <- EstPostDoubleLasso(Y="UseCreche",
                                           Z="Z",
                                           Cluster="SubSampleStrata",
                                           FE="SubSampleStrata",
                                           weights = "WeightPS",
                                           SubSample = "T2-T1",
                                           DB=PostDB,
                                           X=X.c)
ITT.UseCrecheT2C <- EstPostDoubleLasso(Y="UseCreche",
                                          Z="Z",
                                          Cluster="SubSampleStrata",
                                          FE="SubSampleStrata",
                                          weights = "WeightPS",
                                          SubSample = "T2-C",
                                          DB=PostDB,
                                          X=X.c)

ITT.UseCrecheT1C <- EstPostDoubleLasso(Y="UseCreche",
                                          Z="Z",
                                          Cluster="SubSampleStrata",
                                          FE="SubSampleStrata",
                                          weights = "WeightPS",
                                          SubSample = "T1-C",
                                          DB=PostDB,
                                          X=X.c)

#----- RobustnessITTUseCrecheTable -------

# Let's put that in a modelsummary 

# First define properties of the borders
TopBottom <- fp_border(color = "black", style = "thick", width = 1.2)

modelsummary(list("Information vs Control_Basic"=ITT.UseCrecheT1C$`Basic Model`, 
                  "Information vs Control_Pooled"=ITT.UseCrecheT1C$`Post Lasso`,
                  "Information vs Control_Lin regression"=ITT.UseCrecheT1C$`ITT Post lasso`,
                  "Information + support vs Control_Basic"=ITT.UseCrecheT2C$`Basic Model`, 
                  "Information + support vs Control_Pooled"=ITT.UseCrecheT2C$`Post Lasso`,
                  "Information + support vs Control_Lin regression"=ITT.UseCrecheT2C$`ITT Post lasso`,
                  "Support vs Information_Basic"=ITT.UseCrecheT2T1$`Basic Model`, 
                  "Support vs Information_Pooled"=ITT.UseCrecheT2T1$`Post Lasso`,
                  "Support vs Information_Lin regression"=ITT.UseCrecheT2T1$`ITT Post lasso`),
             coef_map=c("Z"="Intention-to-treat"),
             title="Accessed daycare",
             fmt=fmt_statistic(estimate=3,std.error=3,conf.int=3), 
             estimate = '{estimate}{stars} ({std.error})',
             #estimate = '{estimate} ({std.error})',
             statistic = c("conf.int"),
             #stars=FALSE,
             stars = c('*' = .1,'**' = .05, '***' = .01),
             gof_map = c("Covariates","FE: FE",
                         "nobs", "r.squared","adj.r.squared"),
             notes=paste("Sources: Premiers pas, follow-up databases, pairwise comparisons.
The dependent variable equals 1 if the parent accessed for daycare.
Basic specification run OLS on a treatment dummy and block x wave fixed effects.
Pooled specifications add the de-meaned covariates selected by a double-lasso regression with lambda minimising the RMSE chosen by 10-fold cross validation for both the outcome and the treatment. We keep covariates from both post-lasso models in the final estimation. 
Lin regression add interactions of the same de-meaned covariates with the treatment variable.
All models use inverse block-specific propensity scores to account for the design.
Cluster-robust standard errors adjusted at the block level in parenthesis ; point-wise 95% confidence intervals in bracklets."), output = 'flextable') %>% 
  theme_booktabs()|>
  separate_header(split="_",opts = c("center-hspan")) |>   # Separate headers
  merge_at(j=c(1),i=c(1,2),part="header") %>% 
  merge_at(j=c(1),i=c(1,2),part="body") %>% 
  italic(i = c(1),  part = "header") %>% 
  italic(j = c(1),  part = "body") %>% fontsize(size=9,part="footer")%>% fontsize(size=10,part="body") %>% 
  align(part = "header", align = "center")|>                # center
  align(part = "body", align = "center")|>                # center   width(j=1,width=3.5,unit = "cm")|>
  width(j=c(2:10),width=2.8,unit = "cm")|>
  width(j=c(1),width=1,unit = "cm") %>% 
  border_remove() %>% 
  hline_bottom(border = TopBottom) %>% hline_top(border=TopBottom) %>% 
  hline_top( part = "header", border = TopBottom) %>% 
  hline(2,part="body") %>% 
  hline(j=c(2:4),i=1,part="header") %>% 
  hline(j=c(5:7),i=1,part="header") %>% 
  hline(j=c(8:10),i=1,part="header")

