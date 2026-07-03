
##### Load Data and clean #####%


MainDB <- read_csv(here("Data", "MainDB_analysis.csv"), show_col_types = FALSE)

#---- 2_MainDBMachineLearning ----
dbMachineLearning <- MainDB %>%# we select the variables that we want. Check that there are the same than below. Obsiouvly, there are several variables that were recoded, and we don't want to have both the recoded version and the original one as they are colinear. 
  select(c(Intention, 
           ECSApp, 
           ECSUseYes,
           #AffordCostBaseline, 
           #Educ3, # on laisse educ et educ dad mais je suis pas sûre que ce soit pertinent. Idem pour les variables de revenu
           #Educ3Dad, 
           NormsOpposedN,
           Discount501or0,
           AffordSubsidiesBaseline,
           AffordNMCostWBNegativeBaseline,
           AffordOppCostNegativeBaseline,
           AffordNMCostCareerNegativeBaseline,
           AccessHoursBaseline, 
           #  Primipare, #to check 
           # BirthPlace3, 
           Active,  
           AccessInformalFullTime,
           #Age,
           #SingleMum, 
           #FmlyIncomeBaseline,
           #IncomeBaseline,
           LikertReturnHKBaseline,
           TrustCreche,
           # KnowsCreche,
           ComputerYN ,
           #  HighLowECECBaseline ,
           #     BreastFeedIntend,
           #     BreastFeedLength, 
           #Language3,
           # InformalReciprocity, # Do the parents already take care of the children of others
           # AffordCostDK, # remove because we have PerceivedAffordability
           # AffordCostBaseline,
           #    AccessEasyBaseline,
           NormsBaseline,
           LikertAccessInfoBaseline, # FOR 4
           LikertAccessSoc, # FOR 4
           # DeprivClothes, # DEPRIVATION: do parents struggle to buy clothes?
           # DeprivFood, # DEPRIVATION: do parents struggle to buy food?
           # DeprivBills, # DEPRIVATION: do parents struggle to pay for the bills?
           # DeprivHolidays, # DEPRIVATION: do parents struggle to go in holidays?
           # DeprivHousing,# DEPRIVATION: do parents struggle to pay for housing?              
           #NumberChildren,
           #TrustFam,
           #TrustFriend,
           #   CoupleCohabiting,
           AccessEasy5, 
           NormsScore,
           LevelInfoSubExPost, 
           KnownNbTypeECS, 
           LevelComprehensionExPost, 
           Assignment, 
           ECSIdealAgeEntryBaseline, 
           AccessHoursBaseline, 
           Taux.de.couv.global,
           MotherHealthIndex,
           BabyHealthIndex, 
           Educ,
           Migration4,
           blocksMatWave, 
           # PoliticalOrientation, 
           NumberChildren,
           WorkPlanTo,
           NormsWork,
           NormsHouse, 
           #Income, # remove bc redundant with Opp Costs
           OpportunityCosts,
           #MonthlyDaycareFees
           PerceivedAffordability,
           MovedOut, 
           ActivityCriteriaYes, 
           SocialCriteriaYes,
           RegistrationOnlineOnly
  )) %>% # Recode the variables so that there are in the right format
  
  mutate(
    NormsOpposedN = as.numeric(NormsOpposedN), 
    #  KnowsCreche = as.factor(KnowsCreche),
    # KnowsAssMat = as.factor(KnowsAssMat), 
    IdealEntryAge = as.factor(ifelse(ECSIdealAgeEntryBaseline == "Jamais", "Pendant ses trois premiers mois",
                                     as.character(ECSIdealAgeEntryBaseline))), # too few observation in the Never categort # it causes prb and I am worried it is collinear with the DV. Most ppl that said no also reponded "jusqu'à la maternelle"
    # BirthPlace3 = as.factor(ifelse(BirthPlace3 == "Je ne souhaite pas répondre", "Else", as.character(BirthPlace3))) # same too few Je ne souhaite pas répondre
    AccessHoursBaselineNo = ifelse(AccessHoursBaseline == "Non", TRUE, FALSE), 
    AccessHoursBaselineYes = ifelse((AccessHoursBaseline == "Oui" | AccessHoursBaseline == "Non, mais on prendra un autre mode de garde en plus"), 
                                    TRUE, FALSE),
    LevelComprehensionExPostGoodExcellent = ifelse(LevelComprehensionExPost == "pas de doutes sur la compréhension des questions",
                                                   "Yes", "No" ),
    LevelInfoSubExPost2 = ifelse((LevelInfoSubExPost == "Aucun ou très bas"|LevelInfoSubExPost == "Basique") , 
                                 "Low knowledge", "High knowledge"),
    EntryAgeBeforeOne = ifelse((IdealEntryAge == "Pendant ses trois premiers mois" | IdealEntryAge == "Pendant ses six premiers mois" | 
                                  IdealEntryAge == "Pendant ses neuf premiers mois"), "Before One", "After One"),
    LikertReturnHKBaseline = case_when(
      LikertReturnHKBaseline == "Très en désaccord" ~ "Disagree",
      LikertReturnHKBaseline == "Assez en désaccord" ~ "Disagree",
      LikertReturnHKBaseline == "Je ne sais pas" ~ "Neutral",
      LikertReturnHKBaseline == "Assez d’accord" ~ "Agree",
      LikertReturnHKBaseline == "Très d’accord" ~ "Agree"
    ),
    NormsBaseline = case_when(
      NormsBaseline == "Ne sait pas" ~ "Unknown",
      NormsBaseline == "Aucune" ~ "A minority to none",
      NormsBaseline == "Une minorité" ~ "A minority to none",
      NormsBaseline == "Moitié moitié" ~ "Half-Half",
      NormsBaseline == "La plupart" ~ "Most to All",
      NormsBaseline == "Toutes" ~ "Most to All"
    ), 
    TrustCreche = case_when(
      TrustCreche == "Très en désaccord" ~ "Disagree",
      TrustCreche == "Assez en désaccord" ~ "Disagree",
      TrustCreche == "Je ne sais pas" ~ "Neutral",
      TrustCreche == "Assez d’accord" ~ "Agree",
      TrustCreche == "Très d’accord" ~ "Agree"
    ), 
    OpportunityCosts = as.numeric(OpportunityCosts)
    #MonthlyDaycareFees = as.numeric(MonthlyDaycareFees)
    
  )%>%
  select(-c(AccessHoursBaseline, LevelComprehensionExPost, ECSIdealAgeEntryBaseline, LevelInfoSubExPost, IdealEntryAge)) %>% # remove the original variables
  mutate(across(where(is.logical), as.factor)) %>% 
  mutate(across(where(is.character), as.factor)) # put them in the right format for tidymodels
# Add knowledge variables


#----- For 4 & 5 with social and informational ressources ----
## TO DO BEFORE_GO TO 2 and selevt informational and social resources

MLDummified <- dbMachineLearning %>% 
  mutate(
    TrustEarlyChildcareAgree = ifelse(TrustCreche == "Agree", "Yes", "No"), # the mother trust early childcare 
    TrustEarlyChildcareAgree = ifelse(is.na(TrustEarlyChildcareAgree) == TRUE, "No", as.character(TrustEarlyChildcareAgree)), # the mother trust early childcare
    NormsUseMostToAll = ifelse(NormsBaseline == "Most to All", "Yes", "No"), # Descriptive norms
    ECBenefitChildrenAgree = ifelse(LikertReturnHKBaseline == "Agree", "Yes", "No"), # believe in EC returns
    ECAccessHoursFit = ifelse(AccessHoursBaselineYes == TRUE, "Yes", "No"), # early childcare opening hours fit
    LevelInformation = LevelInfoSubExPost2, # Level of knowledge
    LevelComprehensionGoodExcellent = LevelComprehensionExPostGoodExcellent, # Level of comprehension French
    NegativeImpactSalary = ifelse(AffordOppCostNegativeBaseline == TRUE, "Yes", "No"), # Negative Impact Salary
    NegativeImpactCareer = ifelse(AffordNMCostCareerNegativeBaseline == TRUE, "Yes", "No"), # Negative Impact Career
    NegativeImpactWellBeing = ifelse(AffordNMCostWBNegativeBaseline == TRUE, "Yes", "No"), # Negative Impact Well Being
    Active = ifelse(Active == 'Active', "Yes", "No"), # The mother was active at baseline
    # NobodyOpposed = ifelse(NormsOpposedN == 0, "Yes", "No"), # Nobody opposed to the mother using childcare
    # NobodyOpposed = ifelse(is.na(NobodyOpposed) == TRUE, "No", as.character(NobodyOpposed)), # Nobody opposed to the mother using childcare
    PrescriptiveNorm = NormsOpposedN, # Prescriptive norms
    PresentOriented = ifelse(Discount501or0 == 1, "Yes", "No"), # Present oriented
    KnowsECIsSubsidized = ifelse(AffordSubsidiesBaseline == "Yes", "Yes", "No"), # knows that EC is subsidized
    BelieveAccessingChildcareEasy = ifelse(AccessEasy5 == "Easy", "Yes", "No"), #Maybe check
    # DontKnowECCosts = ifelse(AffordCostDK == TRUE, "Yes", "No"), # Don't know EC costs
    #ProgressiveNorms = ifelse(NormsScore < 2, "Yes", "No"), # Progressive norms: play with that a bit
    # ThinkCostBelow500 = ifelse(AffordCostBaseline < 500, "Yes", "No"), # Think cost below 500
    HasAccessToAComputer = ifelse(ComputerYN == "Oui", "Yes", "No"), # Has access to a computer
    CanAccessScoialResources = ifelse((LikertAccessSoc == "Assez d’accord" | LikertAccessSoc == "Très d’accord"), 
                                      "Yes", "No"), # Can access social resources
    CanAccessInformation = ifelse((LikertAccessInfoBaseline == "Assez d’accord" | LikertAccessInfoBaseline == "Très d’accord"),
                                  "Yes", "No"), # Can access information
    #LeftWing = ifelse(PoliticalOrientation == "Left", "Yes", "No"),# Left wing
    WorkPlanTo = ifelse(WorkPlanTo == TRUE, "Yes", "No"), # Work plan to use childcare
    AttitudesTowardsWorkAndFamily = ifelse((NormsHouse == "D’accord"| NormsHouse == "Assez d ’accord"), "Yes", "No"), # Attitudes towards work
    AttitudesLaborAffectsChildren = ifelse((NormsWork == "D’accord" | NormsWork == "Assez d ’accord" ), "Yes", "No") # Attitudes towards house
  ) %>% select(-c(TrustCreche, 
                  NormsBaseline, 
                  LikertReturnHKBaseline, 
                  AccessHoursBaselineNo, 
                  AccessHoursBaselineYes, 
                  LevelInfoSubExPost2,
                  LevelComprehensionExPostGoodExcellent,
                  AffordOppCostNegativeBaseline, 
                  AffordNMCostWBNegativeBaseline,
                  AffordNMCostCareerNegativeBaseline,
                  NormsOpposedN, 
                  Discount501or0, 
                  #AffordCostDK, 
                  AccessEasy5, 
                  ComputerYN, 
                  #  NormsScore, 
                  KnownNbTypeECS, 
                  AffordSubsidiesBaseline,
                  LikertAccessInfoBaseline, 
                  LikertAccessSoc,
                  Educ, 
                  Migration4,
                  blocksMatWave,
                  # PoliticalOrientation, 
                  #LeftWing,
                  #AffordCostBaseline
                  NormsScore, 
                  NormsHouse, 
                  NormsWork                  # DontKnowECCosts,
                  #AffordCostBaseline
  )) %>% 
  mutate(across(where(is.logical), as.factor)) %>% 
  mutate(across(where(is.character), as.factor)) # put them in the right format for tidymodels


#---------- Oaxaca -------------------


Oaxaca <- dbMachineLearning  %>%  
  mutate(
    TrustEarlyChildcareAgree = ifelse(TrustCreche == "Agree", "Yes", "No"), # the mother trust early childcare 
    TrustEarlyChildcareAgree = ifelse(is.na(TrustEarlyChildcareAgree) == TRUE, "No", as.character(TrustEarlyChildcareAgree)), # the mother trust early childcare
    NormsUseMostToAll = ifelse(NormsBaseline == "Most to All", "Yes", "No"), # Descriptive norms
    ECBenefitChildrenAgree = ifelse(LikertReturnHKBaseline == "Agree", "Yes", "No"), # believe in EC returns
    ECAccessHoursFit = ifelse(AccessHoursBaselineYes == TRUE, "Yes", "No"), # early childcare opening hours fit
    LevelInformation = LevelInfoSubExPost2, # Level of knowledge
    LevelComprehensionGoodExcellent = LevelComprehensionExPostGoodExcellent, # Level of comprehension French
    NegativeImpactSalary = ifelse(AffordOppCostNegativeBaseline == TRUE, "Yes", "No"), # Negative Impact Salary
    NegativeImpactCareer = ifelse(AffordNMCostCareerNegativeBaseline == TRUE, "Yes", "No"), # Negative Impact Career
    NegativeImpactWellBeing = ifelse(AffordNMCostWBNegativeBaseline == TRUE, "Yes", "No"), # Negative Impact Well Being
    Active = ifelse(Active == 'Active', "Yes", "No"), # The mother was active at baseline
    # NobodyOpposed = ifelse(NormsOpposedN == 0, "Yes", "No"), # Nobody opposed to the mother using childcare
    # NobodyOpposed = ifelse(is.na(NobodyOpposed) == TRUE, "No", as.character(NobodyOpposed)), # Nobody opposed to the mother using childcare
    PrescriptiveNorm = NormsOpposedN, # Prescriptive norms
    PresentOriented = ifelse(Discount501or0 == 1, "Yes", "No"), # Present oriented
    KnowsECIsSubsidized = ifelse(AffordSubsidiesBaseline == "Yes", "Yes", "No"), # knows that EC is subsidized
    BelieveAccessingChildcareEasy = ifelse(AccessEasy5 == "Easy", "Yes", "No"), #Maybe check
    #DontKnowECCosts = ifelse(AffordCostDK == TRUE, "Yes", "No"), # Don't know EC costs
    #ProgressiveNorms = ifelse(NormsScore < 2, "Yes", "No"), # Progressive norms: play with that a bit
    # ThinkCostBelow500 = ifelse(AffordCostBaseline < 500, "Yes", "No"), # Think cost below 500
    HasAccessToAComputer = ifelse(ComputerYN == "Oui", "Yes", "No"), # Has access to a computer
    CanAccessScoialResources = ifelse((LikertAccessSoc == "Assez d’accord" | LikertAccessSoc == "Très d’accord"), 
                                      "Yes", "No"), # Can access social resources
    CanAccessInformation = ifelse((LikertAccessInfoBaseline == "Assez d’accord" | LikertAccessInfoBaseline == "Très d’accord"),
                                  "Yes", "No"), # Can access information
    #  LeftWing = ifelse(PoliticalOrientation == "Left", "Yes", "No"),# Left wing
    WorkPlanTo = ifelse(WorkPlanTo == TRUE, "Yes", "No"), # Work plan to use childcare
    AttitudesTowardsWorkAndFamily = ifelse((NormsHouse == "D’accord"| NormsHouse == "Assez d ’accord"), "Yes", "No"), # Attitudes towards work
    AttitudesLaborAffectsChildren = ifelse((NormsWork == "D’accord" | NormsWork == "Assez d ’accord" ), "Yes", "No") # Attitudes towards house
  ) %>% select(-c(TrustCreche, 
                  NormsBaseline, 
                  LikertReturnHKBaseline, 
                  AccessHoursBaselineNo, 
                  AccessHoursBaselineYes, 
                  LevelInfoSubExPost2,
                  LevelComprehensionExPostGoodExcellent,
                  AffordOppCostNegativeBaseline, 
                  AffordNMCostWBNegativeBaseline,
                  AffordNMCostCareerNegativeBaseline,
                  NormsOpposedN, 
                  Discount501or0, 
                  #AffordCostDK, 
                  AccessEasy5, 
                  ComputerYN, 
                  #  NormsScore, 
                  KnownNbTypeECS, 
                  AffordSubsidiesBaseline,
                  LikertAccessInfoBaseline, 
                  LikertAccessSoc,
                  Migration4,
                  blocksMatWave,
                  #PoliticalOrientation, 
                 # LeftWing,
                  NormsScore, 
                  NormsHouse, 
                  NormsWork
                  
                  # DontKnowECCosts,
                  #AffordCostBaseline
  )) %>% 
  mutate(across(where(is.logical), as.factor)) %>% 
  mutate(across(where(is.character), as.factor)) # put them in the right format for tidymodels


#------- Robustness Access other vars ---------------

dbMachineLearning_Access <- MainDB %>%# we select the variables that we want. Check that there are the same than below. Obsiouvly, there are several variables that were recoded, and we don't want to have both the recoded version and the original one as they are colinear. 
  select(c(Intention, 
           ECSApp, 
           ECSUseYes,
           #AffordCostBaseline, 
           #Educ3, # on laisse educ et educ dad mais je suis pas sûre que ce soit pertinent. Idem pour les variables de revenu
           #Educ3Dad, 
           NormsOpposedN,
           Discount501or0,
           AffordSubsidiesBaseline,
           AffordNMCostWBNegativeBaseline,
           AffordOppCostNegativeBaseline,
           AffordNMCostCareerNegativeBaseline,
           AccessHoursBaseline, 
           #  Primipare, #to check 
           # BirthPlace3, 
           Active,  
           AccessInformalFullTime,
           #Age,
           #SingleMum, 
           #FmlyIncomeBaseline,
           #IncomeBaseline,
           LikertReturnHKBaseline,
           TrustCreche,
           # KnowsCreche,
           ComputerYN ,
           #  HighLowECECBaseline ,
           #     BreastFeedIntend,
           #     BreastFeedLength, 
           #Language3,
           # InformalReciprocity, # Do the parents already take care of the children of others
           # AffordCostDK, # remove because we have PerceivedAffordability
           # AffordCostBaseline,
           #    AccessEasyBaseline,
           NormsBaseline,
           LikertAccessInfoBaseline, # FOR 4
           LikertAccessSoc, # FOR 4
           # DeprivClothes, # DEPRIVATION: do parents struggle to buy clothes?
           # DeprivFood, # DEPRIVATION: do parents struggle to buy food?
           # DeprivBills, # DEPRIVATION: do parents struggle to pay for the bills?
           # DeprivHolidays, # DEPRIVATION: do parents struggle to go in holidays?
           # DeprivHousing,# DEPRIVATION: do parents struggle to pay for housing?              
           #NumberChildren,
           #TrustFam,
           #TrustFriend,
           #   CoupleCohabiting,
           AccessEasy5, 
           NormsScore,
           LevelInfoSubExPost, 
           KnownNbTypeECS, 
           LevelComprehensionExPost, 
           Assignment, 
           ECSIdealAgeEntryBaseline, 
           AccessHoursBaseline, 
           Taux.de.couv.global,
           MotherHealthIndex,
           BabyHealthIndex, 
           Educ,
           Migration4,
           blocksMatWave, 
           # PoliticalOrientation, 
           NumberChildren,
           WorkPlanTo,
           NormsWork,
           NormsHouse, 
           Income, # remove bc redundant with Opp Costs
           #OpportunityCosts,
           MonthlyDaycareFees,
           PerceivedAffordability,
           MovedOut, 
           ECSAppTimingBeforeBirth,
           ApplicationWentOnSiteYes,
           ApplicationCalled,
           ApplicationNetworked, 
           ActivityCriteriaYes,
           SocialCriteriaYes,
           RegistrationOnlineOnly
  )) %>% # Recode the variables so that there are in the right format
  
  mutate(
    NormsOpposedN = as.numeric(NormsOpposedN), 
    #  KnowsCreche = as.factor(KnowsCreche),
    # KnowsAssMat = as.factor(KnowsAssMat), 
    IdealEntryAge = as.factor(ifelse(ECSIdealAgeEntryBaseline == "Jamais", "Pendant ses trois premiers mois",
                                     as.character(ECSIdealAgeEntryBaseline))), # too few observation in the Never categort # it causes prb and I am worried it is collinear with the DV. Most ppl that said no also reponded "jusqu'à la maternelle"
    # BirthPlace3 = as.factor(ifelse(BirthPlace3 == "Je ne souhaite pas répondre", "Else", as.character(BirthPlace3))) # same too few Je ne souhaite pas répondre
    AccessHoursBaselineNo = ifelse(AccessHoursBaseline == "Non", TRUE, FALSE), 
    AccessHoursBaselineYes = ifelse((AccessHoursBaseline == "Oui" | AccessHoursBaseline == "Non, mais on prendra un autre mode de garde en plus"), 
                                    TRUE, FALSE),
    LevelComprehensionExPostGoodExcellent = ifelse(LevelComprehensionExPost == "pas de doutes sur la compréhension des questions",
                                                   "Yes", "No" ),
    LevelInfoSubExPost2 = ifelse((LevelInfoSubExPost == "Aucun ou très bas"|LevelInfoSubExPost == "Basique") , 
                                 "Low knowledge", "High knowledge"),
    EntryAgeBeforeOne = ifelse((IdealEntryAge == "Pendant ses trois premiers mois" | IdealEntryAge == "Pendant ses six premiers mois" | 
                                  IdealEntryAge == "Pendant ses neuf premiers mois"), "Before One", "After One"),
    LikertReturnHKBaseline = case_when(
      LikertReturnHKBaseline == "Très en désaccord" ~ "Disagree",
      LikertReturnHKBaseline == "Assez en désaccord" ~ "Disagree",
      LikertReturnHKBaseline == "Je ne sais pas" ~ "Neutral",
      LikertReturnHKBaseline == "Assez d’accord" ~ "Agree",
      LikertReturnHKBaseline == "Très d’accord" ~ "Agree"
    ),
    NormsBaseline = case_when(
      NormsBaseline == "Ne sait pas" ~ "Unknown",
      NormsBaseline == "Aucune" ~ "A minority to none",
      NormsBaseline == "Une minorité" ~ "A minority to none",
      NormsBaseline == "Moitié moitié" ~ "Half-Half",
      NormsBaseline == "La plupart" ~ "Most to All",
      NormsBaseline == "Toutes" ~ "Most to All"
    ), 
    TrustCreche = case_when(
      TrustCreche == "Très en désaccord" ~ "Disagree",
      TrustCreche == "Assez en désaccord" ~ "Disagree",
      TrustCreche == "Je ne sais pas" ~ "Neutral",
      TrustCreche == "Assez d’accord" ~ "Agree",
      TrustCreche == "Très d’accord" ~ "Agree"
    ), 
    # OpportunityCosts = as.numeric(OpportunityCosts),
    MonthlyDaycareFees = as.numeric(MonthlyDaycareFees)
    
  )%>%
  select(-c(AccessHoursBaseline, LevelComprehensionExPost, ECSIdealAgeEntryBaseline, LevelInfoSubExPost, IdealEntryAge)) %>% # remove the original variables
  mutate(across(where(is.logical), as.factor)) %>% 
  mutate(across(where(is.character), as.factor)) # put them in the right format for tidymodels
# Add knowledge variables


MLDummified_Access <- dbMachineLearning_Access %>% 
  mutate(
    TrustEarlyChildcareAgree = ifelse(TrustCreche == "Agree", "Yes", "No"), # the mother trust early childcare 
    TrustEarlyChildcareAgree = ifelse(is.na(TrustEarlyChildcareAgree) == TRUE, "No", as.character(TrustEarlyChildcareAgree)), # the mother trust early childcare
    NormsUseMostToAll = ifelse(NormsBaseline == "Most to All", "Yes", "No"), # Descriptive norms
    ECBenefitChildrenAgree = ifelse(LikertReturnHKBaseline == "Agree", "Yes", "No"), # believe in EC returns
    ECAccessHoursFit = ifelse(AccessHoursBaselineYes == TRUE, "Yes", "No"), # early childcare opening hours fit
    LevelInformation = LevelInfoSubExPost2, # Level of knowledge
    LevelComprehensionGoodExcellent = LevelComprehensionExPostGoodExcellent, # Level of comprehension French
    NegativeImpactSalary = ifelse(AffordOppCostNegativeBaseline == TRUE, "Yes", "No"), # Negative Impact Salary
    NegativeImpactCareer = ifelse(AffordNMCostCareerNegativeBaseline == TRUE, "Yes", "No"), # Negative Impact Career
    NegativeImpactWellBeing = ifelse(AffordNMCostWBNegativeBaseline == TRUE, "Yes", "No"), # Negative Impact Well Being
    Active = ifelse(Active == 'Active', "Yes", "No"), # The mother was active at baseline
    # NobodyOpposed = ifelse(NormsOpposedN == 0, "Yes", "No"), # Nobody opposed to the mother using childcare
    # NobodyOpposed = ifelse(is.na(NobodyOpposed) == TRUE, "No", as.character(NobodyOpposed)), # Nobody opposed to the mother using childcare
    PrescriptiveNorm = NormsOpposedN, # Prescriptive norms
    PresentOriented = ifelse(Discount501or0 == 1, "Yes", "No"), # Present oriented
    KnowsECIsSubsidized = ifelse(AffordSubsidiesBaseline == "Yes", "Yes", "No"), # knows that EC is subsidized
    BelieveAccessingChildcareEasy = ifelse(AccessEasy5 == "Easy", "Yes", "No"), #Maybe check
    # DontKnowECCosts = ifelse(AffordCostDK == TRUE, "Yes", "No"), # Don't know EC costs
    #ProgressiveNorms = ifelse(NormsScore < 2, "Yes", "No"), # Progressive norms: play with that a bit
    # ThinkCostBelow500 = ifelse(AffordCostBaseline < 500, "Yes", "No"), # Think cost below 500
    HasAccessToAComputer = ifelse(ComputerYN == "Oui", "Yes", "No"), # Has access to a computer
    CanAccessScoialResources = ifelse((LikertAccessSoc == "Assez d’accord" | LikertAccessSoc == "Très d’accord"), 
                                      "Yes", "No"), # Can access social resources
    CanAccessInformation = ifelse((LikertAccessInfoBaseline == "Assez d’accord" | LikertAccessInfoBaseline == "Très d’accord"),
                                  "Yes", "No"), # Can access information
    # LeftWing = ifelse(PoliticalOrientation == "Left", "Yes", "No"),# Left wing
    WorkPlanTo = ifelse(WorkPlanTo == TRUE, "Yes", "No"), # Work plan to use childcare
    AttitudesTowardsWorkAndFamily = ifelse((NormsHouse == "D’accord"| NormsHouse == "Assez d ’accord"), "Yes", "No"), # Attitudes towards work
    AttitudesLaborAffectsChildren = ifelse((NormsWork == "D’accord" | NormsWork == "Assez d ’accord" ), "Yes", "No") # Attitudes towards house
  ) %>% select(-c(TrustCreche, 
                  NormsBaseline, 
                  LikertReturnHKBaseline, 
                  AccessHoursBaselineNo, 
                  AccessHoursBaselineYes, 
                  LevelInfoSubExPost2,
                  LevelComprehensionExPostGoodExcellent,
                  AffordOppCostNegativeBaseline, 
                  AffordNMCostWBNegativeBaseline,
                  AffordNMCostCareerNegativeBaseline,
                  NormsOpposedN, 
                  Discount501or0, 
                  #AffordCostDK, 
                  AccessEasy5, 
                  ComputerYN, 
                  #  NormsScore, 
                  KnownNbTypeECS, 
                  AffordSubsidiesBaseline,
                  LikertAccessInfoBaseline, 
                  LikertAccessSoc,
                  Educ, 
                  Migration4,
                  blocksMatWave,
                  # PoliticalOrientation, 
                  #LeftWing,
                  #AffordCostBaseline
                  NormsScore, 
                  NormsHouse, 
                  NormsWork                  # DontKnowECCosts,
                  #AffordCostBaseline
  )) %>% 
  mutate(across(where(is.logical), as.factor)) %>% 
  mutate(across(where(is.character), as.factor)) # put them in the right format for tidymodels


Oaxaca_Access <-  dbMachineLearning_Access  %>%  
  mutate(
    TrustEarlyChildcareAgree = ifelse(TrustCreche == "Agree", "Yes", "No"), # the mother trust early childcare 
    TrustEarlyChildcareAgree = ifelse(is.na(TrustEarlyChildcareAgree) == TRUE, "No", as.character(TrustEarlyChildcareAgree)), # the mother trust early childcare
    NormsUseMostToAll = ifelse(NormsBaseline == "Most to All", "Yes", "No"), # Descriptive norms
    ECBenefitChildrenAgree = ifelse(LikertReturnHKBaseline == "Agree", "Yes", "No"), # believe in EC returns
    ECAccessHoursFit = ifelse(AccessHoursBaselineYes == TRUE, "Yes", "No"), # early childcare opening hours fit
    LevelInformation = LevelInfoSubExPost2, # Level of knowledge
    LevelComprehensionGoodExcellent = LevelComprehensionExPostGoodExcellent, # Level of comprehension French
    NegativeImpactSalary = ifelse(AffordOppCostNegativeBaseline == TRUE, "Yes", "No"), # Negative Impact Salary
    NegativeImpactCareer = ifelse(AffordNMCostCareerNegativeBaseline == TRUE, "Yes", "No"), # Negative Impact Career
    NegativeImpactWellBeing = ifelse(AffordNMCostWBNegativeBaseline == TRUE, "Yes", "No"), # Negative Impact Well Being
    Active = ifelse(Active == 'Active', "Yes", "No"), # The mother was active at baseline
    # NobodyOpposed = ifelse(NormsOpposedN == 0, "Yes", "No"), # Nobody opposed to the mother using childcare
    # NobodyOpposed = ifelse(is.na(NobodyOpposed) == TRUE, "No", as.character(NobodyOpposed)), # Nobody opposed to the mother using childcare
    PrescriptiveNorm = NormsOpposedN, # Prescriptive norms
    PresentOriented = ifelse(Discount501or0 == 1, "Yes", "No"), # Present oriented
    KnowsECIsSubsidized = ifelse(AffordSubsidiesBaseline == "Yes", "Yes", "No"), # knows that EC is subsidized
    BelieveAccessingChildcareEasy = ifelse(AccessEasy5 == "Easy", "Yes", "No"), #Maybe check
    #DontKnowECCosts = ifelse(AffordCostDK == TRUE, "Yes", "No"), # Don't know EC costs
    #ProgressiveNorms = ifelse(NormsScore < 2, "Yes", "No"), # Progressive norms: play with that a bit
    # ThinkCostBelow500 = ifelse(AffordCostBaseline < 500, "Yes", "No"), # Think cost below 500
    HasAccessToAComputer = ifelse(ComputerYN == "Oui", "Yes", "No"), # Has access to a computer
    CanAccessScoialResources = ifelse((LikertAccessSoc == "Assez d’accord" | LikertAccessSoc == "Très d’accord"), 
                                      "Yes", "No"), # Can access social resources
    CanAccessInformation = ifelse((LikertAccessInfoBaseline == "Assez d’accord" | LikertAccessInfoBaseline == "Très d’accord"),
                                  "Yes", "No"), # Can access information
    # LeftWing = ifelse(PoliticalOrientation == "Left", "Yes", "No"),# Left wing
    WorkPlanTo = ifelse(WorkPlanTo == TRUE, "Yes", "No"), # Work plan to use childcare
    AttitudesTowardsWorkAndFamily = ifelse((NormsHouse == "D’accord"| NormsHouse == "Assez d ’accord"), "Yes", "No"), # Attitudes towards work
    AttitudesLaborAffectsChildren = ifelse((NormsWork == "D’accord" | NormsWork == "Assez d ’accord" ), "Yes", "No") # Attitudes towards house
  ) %>% select(-c(TrustCreche, 
                  NormsBaseline, 
                  LikertReturnHKBaseline, 
                  AccessHoursBaselineNo, 
                  AccessHoursBaselineYes, 
                  LevelInfoSubExPost2,
                  LevelComprehensionExPostGoodExcellent,
                  AffordOppCostNegativeBaseline, 
                  AffordNMCostWBNegativeBaseline,
                  AffordNMCostCareerNegativeBaseline,
                  NormsOpposedN, 
                  Discount501or0, 
                  #AffordCostDK, 
                  AccessEasy5, 
                  ComputerYN, 
                  #  NormsScore, 
                  KnownNbTypeECS, 
                  AffordSubsidiesBaseline,
                  LikertAccessInfoBaseline, 
                  LikertAccessSoc,
                  Migration4,
                  blocksMatWave,
                  #PoliticalOrientation, 
                 # LeftWing,
                  NormsScore, 
                  NormsHouse, 
                  NormsWork
                  
                  # DontKnowECCosts,
                  #AffordCostBaseline
  )) %>% 
  mutate(across(where(is.logical), as.factor)) %>% 
  mutate(across(where(is.character), as.factor)) # put them in the right format for tidymodels


#----- Robustness_Controls_ML ----
MLDummified_Controls <- dbMachineLearning %>%
  filter(Assignment == "Control") %>%  
  mutate(
    TrustEarlyChildcareAgree = ifelse(TrustCreche == "Agree", "Yes", "No"), # the mother trust early childcare 
    TrustEarlyChildcareAgree = ifelse(is.na(TrustEarlyChildcareAgree) == TRUE, "No", as.character(TrustEarlyChildcareAgree)), # the mother trust early childcare
    NormsUseMostToAll = ifelse(NormsBaseline == "Most to All", "Yes", "No"), # Descriptive norms
    ECBenefitChildrenAgree = ifelse(LikertReturnHKBaseline == "Agree", "Yes", "No"), # believe in EC returns
    ECAccessHoursFit = ifelse(AccessHoursBaselineYes == TRUE, "Yes", "No"), # early childcare opening hours fit
    LevelInformation = LevelInfoSubExPost2, # Level of knowledge
    LevelComprehensionGoodExcellent = LevelComprehensionExPostGoodExcellent, # Level of comprehension French
    NegativeImpactSalary = ifelse(AffordOppCostNegativeBaseline == TRUE, "Yes", "No"), # Negative Impact Salary
    NegativeImpactCareer = ifelse(AffordNMCostCareerNegativeBaseline == TRUE, "Yes", "No"), # Negative Impact Career
    NegativeImpactWellBeing = ifelse(AffordNMCostWBNegativeBaseline == TRUE, "Yes", "No"), # Negative Impact Well Being
    Active = ifelse(Active == 'Active', "Yes", "No"), # The mother was active at baseline
    # NobodyOpposed = ifelse(NormsOpposedN == 0, "Yes", "No"), # Nobody opposed to the mother using childcare
    # NobodyOpposed = ifelse(is.na(NobodyOpposed) == TRUE, "No", as.character(NobodyOpposed)), # Nobody opposed to the mother using childcare
    PrescriptiveNorm = NormsOpposedN, # Prescriptive norms
    PresentOriented = ifelse(Discount501or0 == 1, "Yes", "No"), # Present oriented
    KnowsECIsSubsidized = ifelse(AffordSubsidiesBaseline == "Yes", "Yes", "No"), # knows that EC is subsidized
    BelieveAccessingChildcareEasy = ifelse(AccessEasy5 == "Easy", "Yes", "No"), #Maybe check
    # DontKnowECCosts = ifelse(AffordCostDK == TRUE, "Yes", "No"), # Don't know EC costs
    #ProgressiveNorms = ifelse(NormsScore < 2, "Yes", "No"), # Progressive norms: play with that a bit
    # ThinkCostBelow500 = ifelse(AffordCostBaseline < 500, "Yes", "No"), # Think cost below 500
    HasAccessToAComputer = ifelse(ComputerYN == "Oui", "Yes", "No"), # Has access to a computer
    CanAccessScoialResources = ifelse((LikertAccessSoc == "Assez d’accord" | LikertAccessSoc == "Très d’accord"), 
                                      "Yes", "No"), # Can access social resources
    CanAccessInformation = ifelse((LikertAccessInfoBaseline == "Assez d’accord" | LikertAccessInfoBaseline == "Très d’accord"),
                                  "Yes", "No"), # Can access information
   # LeftWing = ifelse(PoliticalOrientation == "Left", "Yes", "No"),# Left wing
    WorkPlanTo = ifelse(WorkPlanTo == TRUE, "Yes", "No"), # Work plan to use childcare
    AttitudesTowardsWorkAndFamily = ifelse((NormsHouse == "D’accord"| NormsHouse == "Assez d ’accord"), "Yes", "No"), # Attitudes towards work
    AttitudesLaborAffectsChildren = ifelse((NormsWork == "D’accord" | NormsWork == "Assez d ’accord" ), "Yes", "No") # Attitudes towards house
  ) %>% select(-c(TrustCreche, 
                  NormsBaseline, 
                  LikertReturnHKBaseline, 
                  AccessHoursBaselineNo, 
                  AccessHoursBaselineYes, 
                  LevelInfoSubExPost2,
                  LevelComprehensionExPostGoodExcellent,
                  AffordOppCostNegativeBaseline, 
                  AffordNMCostWBNegativeBaseline,
                  AffordNMCostCareerNegativeBaseline,
                  NormsOpposedN, 
                  Discount501or0, 
                  #AffordCostDK, 
                  AccessEasy5, 
                  ComputerYN, 
                  #  NormsScore, 
                  KnownNbTypeECS, 
                  AffordSubsidiesBaseline,
                  LikertAccessInfoBaseline, 
                  LikertAccessSoc,
                  Educ, 
                  Migration4,
                  blocksMatWave,
                  # PoliticalOrientation, 
                  #LeftWing,
                  #AffordCostBaseline
                  NormsScore, 
                  NormsHouse, 
                  NormsWork,
                  Assignment
                  # DontKnowECCosts,
                  #AffordCostBaseline
  )) %>% 
  mutate(across(where(is.logical), as.factor)) %>% 
  mutate(across(where(is.character), as.factor)) # put them in the right format for tidymodels

#----- Robustness_Controls_Oaxaca ----
Oaxaca_Control <- dbMachineLearning  %>% 
  filter(Assignment == "Control") %>%  
  mutate(
    TrustEarlyChildcareAgree = ifelse(TrustCreche == "Agree", "Yes", "No"), # the mother trust early childcare 
    TrustEarlyChildcareAgree = ifelse(is.na(TrustEarlyChildcareAgree) == TRUE, "No", as.character(TrustEarlyChildcareAgree)), # the mother trust early childcare
    NormsUseMostToAll = ifelse(NormsBaseline == "Most to All", "Yes", "No"), # Descriptive norms
    ECBenefitChildrenAgree = ifelse(LikertReturnHKBaseline == "Agree", "Yes", "No"), # believe in EC returns
    ECAccessHoursFit = ifelse(AccessHoursBaselineYes == TRUE, "Yes", "No"), # early childcare opening hours fit
    LevelInformation = LevelInfoSubExPost2, # Level of knowledge
    LevelComprehensionGoodExcellent = LevelComprehensionExPostGoodExcellent, # Level of comprehension French
    NegativeImpactSalary = ifelse(AffordOppCostNegativeBaseline == TRUE, "Yes", "No"), # Negative Impact Salary
    NegativeImpactCareer = ifelse(AffordNMCostCareerNegativeBaseline == TRUE, "Yes", "No"), # Negative Impact Career
    NegativeImpactWellBeing = ifelse(AffordNMCostWBNegativeBaseline == TRUE, "Yes", "No"), # Negative Impact Well Being
    Active = ifelse(Active == 'Active', "Yes", "No"), # The mother was active at baseline
    # NobodyOpposed = ifelse(NormsOpposedN == 0, "Yes", "No"), # Nobody opposed to the mother using childcare
    # NobodyOpposed = ifelse(is.na(NobodyOpposed) == TRUE, "No", as.character(NobodyOpposed)), # Nobody opposed to the mother using childcare
    PrescriptiveNorm = NormsOpposedN, # Prescriptive norms
    PresentOriented = ifelse(Discount501or0 == 1, "Yes", "No"), # Present oriented
    KnowsECIsSubsidized = ifelse(AffordSubsidiesBaseline == "Yes", "Yes", "No"), # knows that EC is subsidized
    BelieveAccessingChildcareEasy = ifelse(AccessEasy5 == "Easy", "Yes", "No"), #Maybe check
    #DontKnowECCosts = ifelse(AffordCostDK == TRUE, "Yes", "No"), # Don't know EC costs
    #ProgressiveNorms = ifelse(NormsScore < 2, "Yes", "No"), # Progressive norms: play with that a bit
    # ThinkCostBelow500 = ifelse(AffordCostBaseline < 500, "Yes", "No"), # Think cost below 500
    HasAccessToAComputer = ifelse(ComputerYN == "Oui", "Yes", "No"), # Has access to a computer
    CanAccessScoialResources = ifelse((LikertAccessSoc == "Assez d’accord" | LikertAccessSoc == "Très d’accord"), 
                                      "Yes", "No"), # Can access social resources
    CanAccessInformation = ifelse((LikertAccessInfoBaseline == "Assez d’accord" | LikertAccessInfoBaseline == "Très d’accord"),
                                  "Yes", "No"), # Can access information
    #LeftWing = ifelse(PoliticalOrientation == "Left", "Yes", "No"),# Left wing
    WorkPlanTo = ifelse(WorkPlanTo == TRUE, "Yes", "No"), # Work plan to use childcare
    AttitudesTowardsWorkAndFamily = ifelse((NormsHouse == "D’accord"| NormsHouse == "Assez d ’accord"), "Yes", "No"), # Attitudes towards work
    AttitudesLaborAffectsChildren = ifelse((NormsWork == "D’accord" | NormsWork == "Assez d ’accord" ), "Yes", "No") # Attitudes towards house
  ) %>% select(-c(TrustCreche, 
                  NormsBaseline, 
                  LikertReturnHKBaseline, 
                  AccessHoursBaselineNo, 
                  AccessHoursBaselineYes, 
                  LevelInfoSubExPost2,
                  LevelComprehensionExPostGoodExcellent,
                  AffordOppCostNegativeBaseline, 
                  AffordNMCostWBNegativeBaseline,
                  AffordNMCostCareerNegativeBaseline,
                  NormsOpposedN, 
                  Discount501or0, 
                  #AffordCostDK, 
                  AccessEasy5, 
                  ComputerYN, 
                  #  NormsScore, 
                  KnownNbTypeECS, 
                  AffordSubsidiesBaseline,
                  LikertAccessInfoBaseline, 
                  LikertAccessSoc,
                  Migration4,
                  blocksMatWave,
                  #PoliticalOrientation, 
                  #LeftWing,
                  NormsScore, 
                  NormsHouse, 
                  NormsWork,
                  Assignment
                  
                  # DontKnowECCosts,
                  #AffordCostBaseline
  )) %>% 
  mutate(across(where(is.logical), as.factor)) %>% 
  mutate(across(where(is.character), as.factor)) # put them in the right format for tidymodels


#------------- Descriptives --------------
# First part - select variables and do initial transformations
dbMachineLearning_Descriptives <- MainDB %>%
  select(c(Intention, 
           Educ,
           Age,
           ECSApp, 
           ECSUseYes,
           MigrationBackground,
           NormsOpposedN,
           Discount501or0,
           AffordSubsidiesBaseline,
           AffordNMCostWBNegativeBaseline,
           AffordOppCostNegativeBaseline,
           AffordNMCostCareerNegativeBaseline,
           AccessHoursBaseline, 
           Active,  
           AccessInformalFullTime,
           LikertReturnHKBaseline,
           TrustCreche,
           ComputerYN,
           NormsBaseline,
           LikertAccessInfoBaseline,
           LikertAccessSoc,
           AccessEasy5, 
           NormsScore,
           LevelInfoSubExPost, 
           KnownNbTypeECS, 
           LevelComprehensionExPost, 
           Assignment, 
           ECSIdealAgeEntryBaseline, 
           AccessHoursBaseline, 
           Taux.de.couv.global,
           MotherHealthIndex,
           BabyHealthIndex, 
           Educ,
           Migration4,
           blocksMatWave, 
           #PoliticalOrientation, 
           NumberChildren,
           WorkPlanTo,
           NormsWork,
           NormsHouse, 
           Income,
           FmlyIncomeBaseline,
           OpportunityCosts, # Added OpportunityCosts here
           MonthlyDaycareFees,
           PerceivedAffordability,
           MovedOut, 
           ECSAppTimingBeforeBirth,
           ApplicationWentOnSiteYes,
           ApplicationCalled,
           ApplicationNetworked, 
           ActivityCriteriaYes,
           SocialCriteriaYes,
           RegistrationOnlineOnly
  )) %>%
  
  mutate(
    NormsOpposedN = as.numeric(NormsOpposedN),
    IdealEntryAge = as.factor(ifelse(ECSIdealAgeEntryBaseline == "Jamais", "Pendant ses trois premiers mois",
                                     as.character(ECSIdealAgeEntryBaseline))),
    AccessHoursBaselineNo = ifelse(AccessHoursBaseline == "Non", TRUE, FALSE), 
    AccessHoursBaselineYes = ifelse((AccessHoursBaseline == "Oui" | AccessHoursBaseline == "Non, mais on prendra un autre mode de garde en plus"), 
                                    TRUE, FALSE),
    LevelComprehensionExPostGoodExcellent = ifelse(LevelComprehensionExPost == "pas de doutes sur la compréhension des questions",
                                                   "Yes", "No"),
    LevelInfoSubExPost2 = ifelse((LevelInfoSubExPost == "Aucun ou très bas"|LevelInfoSubExPost == "Basique"), 
                                 "Low knowledge", "High knowledge"),
    EntryAgeBeforeOne = ifelse((IdealEntryAge == "Pendant ses trois premiers mois" | IdealEntryAge == "Pendant ses six premiers mois" | 
                                  IdealEntryAge == "Pendant ses neuf premiers mois"), "Before One", "After One"),
    LikertReturnHKBaseline = case_when(
      LikertReturnHKBaseline == "Très en désaccord" ~ "Disagree",
      LikertReturnHKBaseline == "Assez en désaccord" ~ "Disagree",
      LikertReturnHKBaseline == "Je ne sais pas" ~ "Neutral",
      LikertReturnHKBaseline == "Assez d’accord" ~ "Agree",
      LikertReturnHKBaseline == "Très d’accord" ~ "Agree"
    ),
    NormsBaseline = case_when(
      NormsBaseline == "Ne sait pas" ~ "Unknown",
      NormsBaseline == "Aucune" ~ "A minority to none",
      NormsBaseline == "Une minorité" ~ "A minority to none",
      NormsBaseline == "Moitié moitié" ~ "Half-Half",
      NormsBaseline == "La plupart" ~ "Most to All",
      NormsBaseline == "Toutes" ~ "Most to All"
    ), 
    TrustCreche = case_when(
      TrustCreche == "Très en désaccord" ~ "Disagree",
      TrustCreche == "Assez en désaccord" ~ "Disagree",
      TrustCreche == "Je ne sais pas" ~ "Neutral",
      TrustCreche == "Assez d’accord" ~ "Agree",
      TrustCreche == "Très d’accord" ~ "Agree"
    ), 
    OpportunityCosts = as.numeric(OpportunityCosts), # Ensure OpportunityCosts is numeric
    MonthlyDaycareFees = as.numeric(MonthlyDaycareFees),
    FmlyIncome = case_when(
      FmlyIncomeBaseline %in% c("Ne veut pas répondre", "NSP") ~ NA_character_,
      TRUE ~ FmlyIncomeBaseline
    ),
    FmlyIncome = factor(
      FmlyIncome,
      levels = c(
        "- 500",
        "500-700",
        "700-1000",
        "1000-1500",
        "1500-2000",
        "2000-2500",
        "2500-3000",
        "3000-4500",
        "4500-5000",
        "5000-6000",
        "6000-7000",
        "7000-8000",
        "+8000"
      )
    ),
    
    FmlyIncome3 = case_when(
      FmlyIncome %in% c("- 500", "500-700", "700-1000", "1000-1500") ~ "Less than 1500 euros",
      FmlyIncome %in% c("1500-2000", "2000-2500")                    ~ "1500-2500 euros",
      FmlyIncome %in% c("2500-3000", "3000-4500", "4500-5000",
                                "5000-6000", "6000-7000", "7000-8000",
                                "+8000")                                     ~ "More than 2500 euros",
      FmlyIncome %in% c("Ne veut pas répondre", "NSP")               ~ NA_character_,
      TRUE ~ NA_character_
    ),
    FmlyIncome3 = factor(
      FmlyIncome3,
      levels = c("Less than 1500 euros", "1500-2500 euros", "More than 2500 euros")
    )
    
  ) %>%
  select(-c(AccessHoursBaseline, LevelComprehensionExPost, ECSIdealAgeEntryBaseline, LevelInfoSubExPost, IdealEntryAge, FmlyIncome,FmlyIncomeBaseline)) %>%
  mutate(across(where(is.logical), as.factor)) %>% 
  mutate(across(where(is.character), as.factor))

# Create the final Descriptives dataset with all the desired transformations
Descriptives <- dbMachineLearning_Descriptives %>% 
  mutate(
    TrustEarlyChildcareAgree = ifelse(TrustCreche == "Agree", "Yes", "No"),
    TrustEarlyChildcareAgree = ifelse(is.na(TrustEarlyChildcareAgree) == TRUE, "No", as.character(TrustEarlyChildcareAgree)),
    NormsUseMostToAll = ifelse(NormsBaseline == "Most to All", "Yes", "No"),
    ECBenefitChildrenAgree = ifelse(LikertReturnHKBaseline == "Agree", "Yes", "No"),
    ECAccessHoursFit = ifelse(AccessHoursBaselineYes == TRUE, "Yes", "No"),
    LevelInformation = LevelInfoSubExPost2,
    LevelComprehensionGoodExcellent = LevelComprehensionExPostGoodExcellent,
    NegativeImpactSalary = ifelse(AffordOppCostNegativeBaseline == TRUE, "Yes", "No"),
    NegativeImpactCareer = ifelse(AffordNMCostCareerNegativeBaseline == TRUE, "Yes", "No"),
    NegativeImpactWellBeing = ifelse(AffordNMCostWBNegativeBaseline == TRUE, "Yes", "No"),
    Active = ifelse(Active == 'Active', "Yes", "No"),
    PrescriptiveNorm = NormsOpposedN,
    PresentOriented = ifelse(Discount501or0 == 1, "Yes", "No"),
    KnowsECIsSubsidized = ifelse(AffordSubsidiesBaseline == "Yes", "Yes", "No"),
    BelieveAccessingChildcareEasy = ifelse(AccessEasy5 == "Easy", "Yes", "No"),
    HasAccessToAComputer = ifelse(ComputerYN == "Oui", "Yes", "No"),
    CanAccessScoialResources = ifelse((LikertAccessSoc == "Assez d’accord" | LikertAccessSoc == "Très d’accord"), 
                                      "Yes", "No"),
    CanAccessInformation = ifelse((LikertAccessInfoBaseline == "Assez d’accord" | LikertAccessInfoBaseline == "Très d’accord"),
                                  "Yes", "No"),
    #LeftWing = ifelse(PoliticalOrientation == "Left", "Yes", "No"),
    WorkPlanTo = ifelse(WorkPlanTo == TRUE, "Yes", "No"),
    AttitudesTowardsWorkAndFamily = ifelse((NormsHouse == "D’accord"| NormsHouse == "Assez d’accord "), "Yes", "No"),
    AttitudesLaborAffectsChildren = ifelse((NormsWork == "D’accord" | NormsWork == "Assez d’accord " ), "Yes", "No")
  ) %>% 
  # Keep needed variables and remove redundant ones
  select(-c(TrustCreche, 
            NormsBaseline, 
            LikertReturnHKBaseline, 
            AccessHoursBaselineNo, 
            AccessHoursBaselineYes, 
            LevelInfoSubExPost2,
            LevelComprehensionExPostGoodExcellent,
            AffordOppCostNegativeBaseline, 
            AffordNMCostWBNegativeBaseline,
            AffordNMCostCareerNegativeBaseline,
            NormsOpposedN, 
            Discount501or0,
            AccessEasy5, 
            ComputerYN,
            KnownNbTypeECS, 
            AffordSubsidiesBaseline,
            LikertAccessInfoBaseline, 
            LikertAccessSoc,
            Migration4,
            blocksMatWave,
           # LeftWing,
            NormsScore, 
            NormsHouse, 
            NormsWork
  )) %>% 
  mutate(across(where(is.logical), as.factor)) %>% 
  mutate(across(where(is.character), as.factor))



#------- Intensity ---------------

dbMachineLearning_Intensity <- MainDB %>%# we select the variables that we want. Check that there are the same than below. Obsiouvly, there are several variables that were recoded, and we don't want to have both the recoded version and the original one as they are colinear. 
  select(c(Intention, 
           ECSApp, 
           ECSIntensity,
           #AffordCostBaseline, 
           #Educ3, # on laisse educ et educ dad mais je suis pas sûre que ce soit pertinent. Idem pour les variables de revenu
           #Educ3Dad, 
           NormsOpposedN,
           Discount501or0,
           AffordSubsidiesBaseline,
           AffordNMCostWBNegativeBaseline,
           AffordOppCostNegativeBaseline,
           AffordNMCostCareerNegativeBaseline,
           AccessHoursBaseline, 
           #  Primipare, #to check 
           # BirthPlace3, 
           Active,  
           AccessInformalFullTime,
           #Age,
           #SingleMum, 
           #FmlyIncomeBaseline,
           #IncomeBaseline,
           LikertReturnHKBaseline,
           TrustCreche,
           # KnowsCreche,
           ComputerYN ,
           #  HighLowECECBaseline ,
           #     BreastFeedIntend,
           #     BreastFeedLength, 
           #Language3,
           # InformalReciprocity, # Do the parents already take care of the children of others
           # AffordCostDK, # remove because we have PerceivedAffordability
           # AffordCostBaseline,
           #    AccessEasyBaseline,
           NormsBaseline,
           LikertAccessInfoBaseline, # FOR 4
           LikertAccessSoc, # FOR 4
           # DeprivClothes, # DEPRIVATION: do parents struggle to buy clothes?
           # DeprivFood, # DEPRIVATION: do parents struggle to buy food?
           # DeprivBills, # DEPRIVATION: do parents struggle to pay for the bills?
           # DeprivHolidays, # DEPRIVATION: do parents struggle to go in holidays?
           # DeprivHousing,# DEPRIVATION: do parents struggle to pay for housing?              
           #NumberChildren,
           #TrustFam,
           #TrustFriend,
           #   CoupleCohabiting,
           AccessEasy5, 
           NormsScore,
           LevelInfoSubExPost, 
           KnownNbTypeECS, 
           LevelComprehensionExPost, 
           Assignment, 
           ECSIdealAgeEntryBaseline, 
           AccessHoursBaseline, 
           Taux.de.couv.global,
           MotherHealthIndex,
           BabyHealthIndex, 
           Educ,
           Migration4,
           blocksMatWave, 
           # PoliticalOrientation, 
           NumberChildren,
           WorkPlanTo,
           NormsWork,
           NormsHouse, 
           Income, # remove bc redundant with Opp Costs
           #OpportunityCosts,
           MonthlyDaycareFees,
           PerceivedAffordability,
           MovedOut, 
           ECSAppTimingBeforeBirth,
           ApplicationWentOnSiteYes,
           ApplicationCalled,
           ApplicationNetworked, 
           ActivityCriteriaYes,
           SocialCriteriaYes,
           RegistrationOnlineOnly, 
           WorkMotherReduced, 
           WorkMotherFullTime
  )) %>% # Recode the variables so that there are in the right format
  
  mutate(
    NormsOpposedN = as.numeric(NormsOpposedN), 
    #  KnowsCreche = as.factor(KnowsCreche),
    # KnowsAssMat = as.factor(KnowsAssMat), 
    IdealEntryAge = as.factor(ifelse(ECSIdealAgeEntryBaseline == "Jamais", "Pendant ses trois premiers mois",
                                     as.character(ECSIdealAgeEntryBaseline))), # too few observation in the Never categort # it causes prb and I am worried it is collinear with the DV. Most ppl that said no also reponded "jusqu'à la maternelle"
    # BirthPlace3 = as.factor(ifelse(BirthPlace3 == "Je ne souhaite pas répondre", "Else", as.character(BirthPlace3))) # same too few Je ne souhaite pas répondre
    AccessHoursBaselineNo = ifelse(AccessHoursBaseline == "Non", TRUE, FALSE), 
    AccessHoursBaselineYes = ifelse((AccessHoursBaseline == "Oui" | AccessHoursBaseline == "Non, mais on prendra un autre mode de garde en plus"), 
                                    TRUE, FALSE),
    LevelComprehensionExPostGoodExcellent = ifelse(LevelComprehensionExPost == "pas de doutes sur la compréhension des questions",
                                                   "Yes", "No" ),
    LevelInfoSubExPost2 = ifelse((LevelInfoSubExPost == "Aucun ou très bas"|LevelInfoSubExPost == "Basique") , 
                                 "Low knowledge", "High knowledge"),
    EntryAgeBeforeOne = ifelse((IdealEntryAge == "Pendant ses trois premiers mois" | IdealEntryAge == "Pendant ses six premiers mois" | 
                                  IdealEntryAge == "Pendant ses neuf premiers mois"), "Before One", "After One"),
    LikertReturnHKBaseline = case_when(
      LikertReturnHKBaseline == "Très en désaccord" ~ "Disagree",
      LikertReturnHKBaseline == "Assez en désaccord" ~ "Disagree",
      LikertReturnHKBaseline == "Je ne sais pas" ~ "Neutral",
      LikertReturnHKBaseline == "Assez d’accord" ~ "Agree",
      LikertReturnHKBaseline == "Très d’accord" ~ "Agree"
    ),
    NormsBaseline = case_when(
      NormsBaseline == "Ne sait pas" ~ "Unknown",
      NormsBaseline == "Aucune" ~ "A minority to none",
      NormsBaseline == "Une minorité" ~ "A minority to none",
      NormsBaseline == "Moitié moitié" ~ "Half-Half",
      NormsBaseline == "La plupart" ~ "Most to All",
      NormsBaseline == "Toutes" ~ "Most to All"
    ), 
    TrustCreche = case_when(
      TrustCreche == "Très en désaccord" ~ "Disagree",
      TrustCreche == "Assez en désaccord" ~ "Disagree",
      TrustCreche == "Je ne sais pas" ~ "Neutral",
      TrustCreche == "Assez d’accord" ~ "Agree",
      TrustCreche == "Très d’accord" ~ "Agree"
    ), 
    # OpportunityCosts = as.numeric(OpportunityCosts),
    MonthlyDaycareFees = as.numeric(MonthlyDaycareFees),
    WorkMotherFullTime = ifelse(WorkMotherFullTime == 1, "Yes", "No")
    
  )%>%
  select(-c(AccessHoursBaseline, LevelComprehensionExPost, ECSIdealAgeEntryBaseline, LevelInfoSubExPost, IdealEntryAge)) %>% # remove the original variables
  mutate(across(where(is.logical), as.factor)) %>% 
  mutate(across(where(is.character), as.factor)) # put them in the right format for tidymodels
# Add knowledge variables


MLDummified_I <- dbMachineLearning_Intensity %>% 
  mutate(
    TrustEarlyChildcareAgree = ifelse(TrustCreche == "Agree", "Yes", "No"), # the mother trust early childcare 
    TrustEarlyChildcareAgree = ifelse(is.na(TrustEarlyChildcareAgree) == TRUE, "No", as.character(TrustEarlyChildcareAgree)), # the mother trust early childcare
    NormsUseMostToAll = ifelse(NormsBaseline == "Most to All", "Yes", "No"), # Descriptive norms
    ECBenefitChildrenAgree = ifelse(LikertReturnHKBaseline == "Agree", "Yes", "No"), # believe in EC returns
    ECAccessHoursFit = ifelse(AccessHoursBaselineYes == TRUE, "Yes", "No"), # early childcare opening hours fit
    LevelInformation = LevelInfoSubExPost2, # Level of knowledge
    LevelComprehensionGoodExcellent = LevelComprehensionExPostGoodExcellent, # Level of comprehension French
    NegativeImpactSalary = ifelse(AffordOppCostNegativeBaseline == TRUE, "Yes", "No"), # Negative Impact Salary
    NegativeImpactCareer = ifelse(AffordNMCostCareerNegativeBaseline == TRUE, "Yes", "No"), # Negative Impact Career
    NegativeImpactWellBeing = ifelse(AffordNMCostWBNegativeBaseline == TRUE, "Yes", "No"), # Negative Impact Well Being
    Active = ifelse(Active == 'Active', "Yes", "No"), # The mother was active at baseline
    # NobodyOpposed = ifelse(NormsOpposedN == 0, "Yes", "No"), # Nobody opposed to the mother using childcare
    # NobodyOpposed = ifelse(is.na(NobodyOpposed) == TRUE, "No", as.character(NobodyOpposed)), # Nobody opposed to the mother using childcare
    PrescriptiveNorm = NormsOpposedN, # Prescriptive norms
    PresentOriented = ifelse(Discount501or0 == 1, "Yes", "No"), # Present oriented
    KnowsECIsSubsidized = ifelse(AffordSubsidiesBaseline == "Yes", "Yes", "No"), # knows that EC is subsidized
    BelieveAccessingChildcareEasy = ifelse(AccessEasy5 == "Easy", "Yes", "No"), #Maybe check
    # DontKnowECCosts = ifelse(AffordCostDK == TRUE, "Yes", "No"), # Don't know EC costs
    #ProgressiveNorms = ifelse(NormsScore < 2, "Yes", "No"), # Progressive norms: play with that a bit
    # ThinkCostBelow500 = ifelse(AffordCostBaseline < 500, "Yes", "No"), # Think cost below 500
    HasAccessToAComputer = ifelse(ComputerYN == "Oui", "Yes", "No"), # Has access to a computer
    CanAccessScoialResources = ifelse((LikertAccessSoc == "Assez d’accord" | LikertAccessSoc == "Très d’accord"), 
                                      "Yes", "No"), # Can access social resources
    CanAccessInformation = ifelse((LikertAccessInfoBaseline == "Assez d’accord" | LikertAccessInfoBaseline == "Très d’accord"),
                                  "Yes", "No"), # Can access information
    # LeftWing = ifelse(PoliticalOrientation == "Left", "Yes", "No"),# Left wing
    WorkPlanTo = ifelse(WorkPlanTo == TRUE, "Yes", "No"), # Work plan to use childcare
    AttitudesTowardsWorkAndFamily = ifelse((NormsHouse == "D’accord"| NormsHouse == "Assez d ’accord"), "Yes", "No"), # Attitudes towards work
    AttitudesLaborAffectsChildren = ifelse((NormsWork == "D’accord" | NormsWork == "Assez d ’accord" ), "Yes", "No") # Attitudes towards house
  ) %>% select(-c(TrustCreche, 
                  NormsBaseline, 
                  LikertReturnHKBaseline, 
                  AccessHoursBaselineNo, 
                  AccessHoursBaselineYes, 
                  LevelInfoSubExPost2,
                  LevelComprehensionExPostGoodExcellent,
                  AffordOppCostNegativeBaseline, 
                  AffordNMCostWBNegativeBaseline,
                  AffordNMCostCareerNegativeBaseline,
                  NormsOpposedN, 
                  Discount501or0, 
                  #AffordCostDK, 
                  AccessEasy5, 
                  ComputerYN, 
                  #  NormsScore, 
                  KnownNbTypeECS, 
                  AffordSubsidiesBaseline,
                  LikertAccessInfoBaseline, 
                  LikertAccessSoc,
                  Educ, 
                  Migration4,
                  blocksMatWave,
                  # PoliticalOrientation, 
                  #LeftWing,
                  #AffordCostBaseline
                  NormsScore, 
                  NormsHouse, 
                  NormsWork                  # DontKnowECCosts,
                  #AffordCostBaseline
  )) %>% 
  mutate(across(where(is.logical), as.factor)) %>% 
  mutate(across(where(is.character), as.factor)) # put them in the right format for tidymodels


Oaxaca_Intensity <-  dbMachineLearning_Intensity  %>%  
  mutate(
    TrustEarlyChildcareAgree = ifelse(TrustCreche == "Agree", "Yes", "No"), # the mother trust early childcare 
    TrustEarlyChildcareAgree = ifelse(is.na(TrustEarlyChildcareAgree) == TRUE, "No", as.character(TrustEarlyChildcareAgree)), # the mother trust early childcare
    NormsUseMostToAll = ifelse(NormsBaseline == "Most to All", "Yes", "No"), # Descriptive norms
    ECBenefitChildrenAgree = ifelse(LikertReturnHKBaseline == "Agree", "Yes", "No"), # believe in EC returns
    ECAccessHoursFit = ifelse(AccessHoursBaselineYes == TRUE, "Yes", "No"), # early childcare opening hours fit
    LevelInformation = LevelInfoSubExPost2, # Level of knowledge
    LevelComprehensionGoodExcellent = LevelComprehensionExPostGoodExcellent, # Level of comprehension French
    NegativeImpactSalary = ifelse(AffordOppCostNegativeBaseline == TRUE, "Yes", "No"), # Negative Impact Salary
    NegativeImpactCareer = ifelse(AffordNMCostCareerNegativeBaseline == TRUE, "Yes", "No"), # Negative Impact Career
    NegativeImpactWellBeing = ifelse(AffordNMCostWBNegativeBaseline == TRUE, "Yes", "No"), # Negative Impact Well Being
    Active = ifelse(Active == 'Active', "Yes", "No"), # The mother was active at baseline
    # NobodyOpposed = ifelse(NormsOpposedN == 0, "Yes", "No"), # Nobody opposed to the mother using childcare
    # NobodyOpposed = ifelse(is.na(NobodyOpposed) == TRUE, "No", as.character(NobodyOpposed)), # Nobody opposed to the mother using childcare
    PrescriptiveNorm = NormsOpposedN, # Prescriptive norms
    PresentOriented = ifelse(Discount501or0 == 1, "Yes", "No"), # Present oriented
    KnowsECIsSubsidized = ifelse(AffordSubsidiesBaseline == "Yes", "Yes", "No"), # knows that EC is subsidized
    BelieveAccessingChildcareEasy = ifelse(AccessEasy5 == "Easy", "Yes", "No"), #Maybe check
    #DontKnowECCosts = ifelse(AffordCostDK == TRUE, "Yes", "No"), # Don't know EC costs
    #ProgressiveNorms = ifelse(NormsScore < 2, "Yes", "No"), # Progressive norms: play with that a bit
    # ThinkCostBelow500 = ifelse(AffordCostBaseline < 500, "Yes", "No"), # Think cost below 500
    HasAccessToAComputer = ifelse(ComputerYN == "Oui", "Yes", "No"), # Has access to a computer
    CanAccessScoialResources = ifelse((LikertAccessSoc == "Assez d’accord" | LikertAccessSoc == "Très d’accord"), 
                                      "Yes", "No"), # Can access social resources
    CanAccessInformation = ifelse((LikertAccessInfoBaseline == "Assez d’accord" | LikertAccessInfoBaseline == "Très d’accord"),
                                  "Yes", "No"), # Can access information
    # LeftWing = ifelse(PoliticalOrientation == "Left", "Yes", "No"),# Left wing
    WorkPlanTo = ifelse(WorkPlanTo == TRUE, "Yes", "No"), # Work plan to use childcare
    AttitudesTowardsWorkAndFamily = ifelse((NormsHouse == "D’accord"| NormsHouse == "Assez d ’accord"), "Yes", "No"), # Attitudes towards work
    AttitudesLaborAffectsChildren = ifelse((NormsWork == "D’accord" | NormsWork == "Assez d ’accord" ), "Yes", "No") # Attitudes towards house
  ) %>% select(-c(TrustCreche, 
                  NormsBaseline, 
                  LikertReturnHKBaseline, 
                  AccessHoursBaselineNo, 
                  AccessHoursBaselineYes, 
                  LevelInfoSubExPost2,
                  LevelComprehensionExPostGoodExcellent,
                  AffordOppCostNegativeBaseline, 
                  AffordNMCostWBNegativeBaseline,
                  AffordNMCostCareerNegativeBaseline,
                  NormsOpposedN, 
                  Discount501or0, 
                  #AffordCostDK, 
                  AccessEasy5, 
                  ComputerYN, 
                  #  NormsScore, 
                  KnownNbTypeECS, 
                  AffordSubsidiesBaseline,
                  LikertAccessInfoBaseline, 
                  LikertAccessSoc,
                  Migration4,
                  blocksMatWave,
                  #PoliticalOrientation, 
                  # LeftWing,
                  NormsScore, 
                  NormsHouse, 
                  NormsWork
                  
                  # DontKnowECCosts,
                  #AffordCostBaseline
  )) %>% 
  mutate(across(where(is.logical), as.factor)) %>% 
  mutate(across(where(is.character), as.factor)) # put them in the right format for tidymodels

####################################
#### PAPER THEME ################
####################################

library(grid)

paper_theme <- theme_minimal(base_size = 12) +
  theme(
    text = element_text(size = 12),
    strip.text = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    axis.text.y = element_text(size = 12, margin = margin(r = 8)),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 12),
    legend.position = "bottom",
    panel.spacing = unit(1, "lines"),
    plot.caption = element_text(size = 10, hjust = 0, margin = margin(t = 10)),
    plot.title = element_text(hjust = 0, size = 12, face = "bold"),
    plot.subtitle = element_text(hjust = 0, size = 11),
    plot.margin = margin(15, 10, 15, 15)
  )
####################################



