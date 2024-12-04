
##### Load Data and clean #####%


# OLD CODE

#---- LoadSource ----
# Original baseline dataset 
BaselineFull <- read_csv(here("Data","PremiersPas_orginal_withrefusals.csv"))

##create a df with the first two rows that contains the full questions, in case it is needed
details_values <- BaselineFull[1:2,]

##remove these rows from the main df
BaselineFull<- BaselineFull[-c(1:2),]

# Endline clean
#PremiersPasWithoutIds <- read_csv(here("Data","PremiersPasWithoutIds.csv"))

MainDB <- read_csv(here( "Data","PremiersPasWithNonRespondents112024.csv")) %>%    
  mutate(Responded=ifelse(is.na(ECSApp),0,1), ResponseStatus=case_when(Responded==1~"Respondent",                                                                         Responded==2~"NonRespondent")) %>%
  group_by(strata,Which_wave) %>%  
  mutate(StrataWave=cur_group_id() %>% as.factor()) %>%   ungroup() %>%   
  mutate(Educ2=ifelse(as.character(Educ2)=="Bac","Low-SES","High-SES"))

# Baseline clean
data_full <- read_csv(here("Data","BaselinePremiersPas.csv"))

# Include some data that was lost
MainDB <- left_join(MainDB, data_full[, c("ResponseId", "BirthPlace", "BirthPlace2")], by = "ResponseId")


MainDB <- MainDB %>% mutate(BirthNotAsiaMENA = ifelse((as.character(BirthPlace) == "Asie (hors MENA)" | as.character(BirthPlace) == "MENA (Maghreb +Arabie saoudite, etc.)"), "AsiaOrMENA", "NotAsiaNorMENA"),
                            BirthNotAsiaMENA = as.factor(BirthNotAsiaMENA), 
                            BirthNotAsiaMENA1or0 = ifelse((as.character(BirthPlace) == "Asie (hors MENA)" | as.character(BirthPlace) == "MENA (Maghreb +Arabie saoudite, etc.)"), 0, 1), 
                            BirthPlace3 = fct_collapse(BirthPlace, 
                                                       MENAsie = c("Asie (hors MENA)" , "MENA (Maghreb +Arabie saoudite, etc.)"), 
                                                       France = c( "Dans un territoire français d’outre-mer", "En France métropolitaine"), 
                                                       Else = c("Afrique subsaharienne", "Amérique Latine (à partir du Mexique)", "Autre", 
                                                                "Europe/Amérique du Nord, Australie"#, "Je ne souhaite pas répondre" 
                                                       )
                            ), 
                            NormsBaseline=factor(NormsBaseline,
                                                 levels=c("Ne sait pas","Aucune","Une minorité","Moitié moitié","La plupart","Toutes"), ordered = TRUE # how many mothers in their surroundings use childcare 
                            ), 
                            NormsBaselineNum = case_when(
                              NormsBaseline == "Ne sait pas" ~ NA,
                              NormsBaseline == "Aucune" ~ 1,
                              NormsBaseline == "Une minorité" ~ 2,
                              NormsBaseline == "Moitié moitié" ~ 3,
                              NormsBaseline == "La plupart" ~ 4,
                              NormsBaseline == "Toutes" ~ 5
                            ), 
                            NormsBaselineNum = as.numeric(NormsBaselineNum), 
                            NormsBaselineNoone = ifelse(NormsBaseline == "Aucune", 1, 0),
                            NormsBaselineSomeOpposed = ifelse(NormsOpposedN > 4, 1, 0), 
                            AffordOppCostNegativeBaseline = AffordOppCostBaseline == "Négatif", 
                            AffordNMCostCareerNegativeBaseline = AffordNMCostCareerBaseline == "Négatif",
                            AffordNMCostWBNegativeBaseline = AffordNMCostWBBaseline == "Négatif", 
                            LikertReturnHKBaseline=factor(LikertReturnHKBaseline, # believe in childcare returns on investment: is a child that attended childcare better developed in preschool?
                                                          levels=c("Très en désaccord","Assez en désaccord","Je ne sais pas","Assez d’accord","Très d’accord") , ordered = TRUE), # strongly disagree, disagree, don't know, agree, strongly agree
                            LikertReturnHKDisagreeBaseline = ifelse(LikertReturnHKBaseline == "Très en désaccord" | LikertReturnHKBaseline == "Assez en désaccord", 1, 0), 
                            LikertReturnHKAgreeBaseline = ifelse(LikertReturnHKBaseline == "Assez d’accord" | LikertReturnHKBaseline == "Très d’accord", 1, 0), 
                            LikertReturnHStrongAgreeBaseline = ifelse( LikertReturnHKBaseline == "Très d’accord", 1, 0), 
                            TrustCreche = factor(TrustCreche, # believe in childcare returns on investment: is a child that attended childcare better developed in preschool?
                                                 levels=c("Très en désaccord","Assez en désaccord","Je ne sais pas","Assez d’accord","Très d’accord") , ordered = TRUE), # strongly
                            TrustCrecheAgree = ifelse(TrustCreche == "Assez d’accord" | TrustCreche == "Très d’accord", 1, 0), 
                            ECSIdealAgeEntry6 = ifelse(as.character(ECSIdealAgeEntryBaseline ) == "Jamais", "Pendant ses trois premiers mois", as.character(ECSIdealAgeEntryBaseline)), 
                            ECSIdealAgeEntry6 = as.factor(ECSIdealAgeEntry6), 
                            ECSIdealAgeEntryBaseline = factor(ECSIdealAgeEntryBaseline , 
                                                              levels = c(  "Jamais", "Pendant ses trois premiers mois", "Pendant ses six premiers mois", "Pendant ses neuf premiers mois", "Pendant la première année", "Ne sait pas", "Jusqu’à l’entrée à la maternelle" ) , ordered = TRUE),
                            ECSIdealAgeEntryBaseline1yoOrMore = ifelse(as.character(ECSIdealAgeEntryBaseline) == "Pendant la première année" | as.character(ECSIdealAgeEntryBaseline) == "Jusqu’à l’entrée à la maternelle", 1, 0), 
                            LikertAccessInfoBaselineAgree = ifelse((LikertAccessInfoBaseline == "Assez d’accord"| LikertAccessInfoBaseline == "Très d’accord") , 1, 0), 
                            LikertAccessSocAgree = ifelse((LikertAccessSoc == "Assez d’accord"| LikertAccessSoc == "Très d’accord") , 1, 0), 
                            KnownNbTypeECS012 = ifelse(KnownNbTypeECS >= 2, 2, KnownNbTypeECS), 
                            Norms3 = case_when(
                              NormsBaseline == "Ne sait pas" ~ "Half-Half",
                              NormsBaseline == "Aucune" ~ "Low",
                              NormsBaseline == "Une minorité" ~ "Low",
                              NormsBaseline == "Moitié moitié" ~ "Half-Half",
                              NormsBaseline == "La plupart" ~ "High",
                              NormsBaseline == "Toutes" ~ "High"
                            ),
                            Norms3=factor(Norms3,
                                          levels=c("Low","Half-Half","High"), labels = c("Low","Half-Half","High"), , ordered = TRUE # how many mothers in their surroundings use childcare groups in 3
                            ), 
                            Returns3 = case_when(
                              LikertReturnHKBaseline == "Très en désaccord" ~ "Disagree",
                              LikertReturnHKBaseline == "Assez en désaccord" ~ "Disagree",
                              LikertReturnHKBaseline == "Je ne sais pas" ~ "No opinion",
                              LikertReturnHKBaseline == "Assez d’accord" ~ "Agree",
                              LikertReturnHKBaseline == "Très d’accord" ~ "Agree"
                            ),
                            Returns3 = factor(Returns3,
                                              levels=c("Disagree","No opinion","Agree"), labels = c("Disagree","No opinion","Agree"), ordered = TRUE # how many mothers in their surroundings use childcare groups in 3
                            ), 
                            AffordSubsidiesEndline1or0 = ifelse(AffordSubsidiesEndline == "Yes",1,0), 
                            EverUsedECS1or0  = ifelse(UsedECEC != "Non", 1, 0), 
                            EverUsedECEC  = ifelse(UsedECEC != "Non","Yes", "No"),
                            T1=ifelse(Assignment=="T1",1,0),
                            T2=ifelse(Assignment=="T2",1,0), Suivi_administratif1or0=as.numeric(ifelse(Suivi_administratif=="Yes",1,0)), 
                            NumericSubjInfoEndline = as.numeric(as.factor(SubjectiveInfoLevel)), 
                            CompliancePP = case_when(
                              Assignment == "Control" ~ 0,
                              Compliance == "Oui" ~ 1, 
                              .default = 0),
                            InfoBaseline=ifelse(LevelInfoSubExPost == "Aucun ou très bas","Low knowledge","High knowledge"), 
                            WorkMotherBaseline = ifelse(Occupation == "Je travaille (y compris si congé maladie, maternité ou parental)", "Yes", "No"), 
                            WorkMotherBaseline1or0 = ifelse(WorkMotherBaseline == "Yes", 1, 0),
                            HighEduc = ifelse(Educ2 == "High-SES", 1, 0), 
                            FrenchYNBaseline = ifelse(FrenchYNBaseline == "French", "France", "Abroad"), 
                            Dep = as.factor(Dep), 
                            DepCov = case_when(Dep == "75" ~ "High", 
                                               Dep == "93" ~ "Low",
                                               Dep == "94" ~ "Mid"), 
                            
                            DepHighLow = ifelse(Dep == "75", "Higher", "Lower"), # I create a new variable to gain power that is Higher is Paris and Lower otherwise because looking at the 3 departments is not enough power and 93 and 94 have about the same coverage rate
                            HighLowCoverageGlobal = ifelse(TAUXCOUV_COM > median(TAUXCOUV_COM), "High coverage", "Low coverage"),  # I create a variable for 
                            UseCrecheA = case_when(
                              UseCrecheA == 1 ~ 1,
                              UseParentale == 1~ 1,
                              .default = 0 
                            ), 
                            UseCrecheA = as.numeric(UseCrecheA), 
                            OrderedEduc = OrderedEduc.x, 
                            ECSNoChildcareType = as.factor(ECSNoChildcareType), 
                            InformalCareType =  case_when( str_detect(ECSNoChildcareType,  "vous-mêmes") == TRUE ~ "Maternal care",
                                                   str_detect(ECSNoChildcareType, "grands-parents") == TRUE ~ "Grand-parents",
                            (str_detect(ECSNoChildcareType, "conjoint") == TRUE | str_detect(ECSNoChildcareType_5_TEXT,"papa") == TRUE) ~ "Other parent", 
                            (str_detect(ECSNoChildcareType,"voisin") == TRUE| str_detect(ECSNoChildcareType_5_TEXT, "ami") == TRUE |
                               str_detect(ECSNoChildcareType_5_TEXT, "Nounou") == TRUE | 
                               str_detect(ECSNoChildcareType_5_TEXT, "maternelle") == TRUE |
                              str_detect(ECSNoChildcareType_5_TEXT, "Nourrice") == TRUE |
                              str_detect(ECSNoChildcareType_5_TEXT, "Personne") == TRUE |
                              str_detect(ECSNoChildcareType_5_TEXT, "Technicienne") == TRUE |
                               str_detect(ECSNoChildcareType_5_TEXT, "répondre") == TRUE |
                              str_detect(ECSNoChildcareType_5_TEXT, "cousine") == TRUE) ~ "Informal care" ),
                            ChildcareTypeEndline = ifelse(
                              is.na(ECSType6Endline) == FALSE, as.character(ECSType6Endline),as.character(InformalCareType)), 
                            ChildcareTypeBaseline = ifelse(
                              is.na(ECSType7) == FALSE, as.character(ECSType7),"Maternal Care"
                            ), 
                            NormsWork=factor(NormsWork,
                                             levels=c("En désaccord","Assez en désaccord","Je ne sais pas","Assez d’accord","D’accord"), 
                                             ordered = TRUE # how many mothers in their surroundings use childcare 
                            ), 
                            NormsHouse=factor(NormsHouse,
                                             levels=c("En désaccord","Assez en désaccord","Je ne sais pas","Assez d’accord","D’accord"), 
                                             ordered = TRUE # how many mothers in their surroundings use childcare 
                            ), 
                            NormsWorkNum = as.numeric(NormsWork),
                            NormsHouseNum = as.numeric(NormsHouse),
                            NormsScore = (NormsWorkNum + NormsHouseNum)/2,
                            )

Control = MainDB %>% filter(Assignment == "Control")
## In the future, clean this import process to do just as in the Replication file

## Original baseline dataset with the number of refusals
#BaselineFull <- read_csv(here("Data","BaselineWithRefusals.csv")) 
#
#
#
## Importthe main database with both baseline and endline information for all participants
#MainDB <- read_csv(here("Data","MainDatabaseWithoutIds.csv")) %>%   mutate(
#  NormsBaseline=factor(NormsBaseline,
#                       levels=c("Ne sait pas","Aucune","Une minorité","Moitié moitié","La plupart","Toutes"), ordered = TRUE # how many mothers in their surroundings use childcare 
#  ), 
#  Dep = as.factor(Dep) 
#)
#


# Descrtiptive statistics 

## For descriptive stats of the coverage rates in 2021 we used data from https://data.caf.fr/explore/dataset/txcouv_pe_com/information/?disjunctive.annee&refine.annee=2021
## We import the data for the coverage rates in 2021 outside from the cities in the sample
### Import the files
txcouv_pe_com_EAJE_assmat <- read.csv(here("Data", "Spatial data","txcouv_pe_com_EAJE_assmat.csv"),sep=";")%>% filter(Date.référence == "2021") %>% mutate(NationalAvgTauxCouv2021 = round(mean(Taux.de.couv.global, na.rm = TRUE), 1)) 

# Import  shape data for the map at the city level

# Note: here they have data on PMI and EAJE in SSD https://data.iledefrance.fr/explore/?sort=modified&refine.theme=Logement+-+sant%C3%A9+-+social

# Import our map at the city level
gp_shape <- st_read(file.path(here("Data", "Spatial data", 
                                   "RECENSEMENT_COMMUNE_POPULATION.shp")))

#---- MainDBPrep ----



## Convert character variables to factor
#char_vars <- lapply(MainDB, class) %in% c('character','logical')
#MainDB[, char_vars] <- lapply(MainDB[, char_vars], as.factor)
#
#DBResponse <- MainDB %>% select(Responded, one_of(PredVars))%>% as.data.frame()
#
#ps_fit <- ps(Responded ~ . ,## Use all variables
#             data=DBResponse, ## Select just the response and specified predictors
#             estimand='ATE', ## ATE: Generalize to all those invited for the survey, not just responders
#             verbose=FALSE
#)
#
#
## get the weights
#DBResponse$weight <- get.weights(ps_fit, stop.method="es.mean")
#DBResponse$PsRX <- ps_fit$ps$es.mean.ATE
#
#survey_trick <- rbind(
#  data.frame(DBResponse, trt_trick=1, weight_trick=1) # full sample
#  , data.frame(DBResponse, trt_trick=0, weight_trick=DBResponse$weight) %>%
#    filter(Responded==1) # Responders, weighted
#) # Trick dx.wts function to summarize this data set to compare if weighted responders are similar to the full sample (ATT since full sample has trt_trick=1 as trick treatment)
#
#
#
#MainDB$WeightBalance <- DBResponse$weight
#
####### Compute propensity scores of treatment assignments
## I use a multinomial logit of assignment over blocks within each wave (i.e. the exact randomisation design)
## multinom is taken from the package glmnet
#(fit_basic <- multinom(Assignment ~ StrataWave, data = MainDB))
##%>% invisible()
#
#
## Then retrieve the predicted probability
#
#MainDB$predT1 <- predict(fit_basic,MainDB,"probs")[,"T1"]
#MainDB$predT2 <- predict(fit_basic,MainDB,"probs")[,"T2"]
#
## From there, we generate centred treatment dummies
#
#MainDB <- MainDB %>% mutate(Z1=ifelse(Assignment=="T1",1,0),
#                            Z2=ifelse(Assignment=="T2",1,0),
#                            Z1.c=Z1-predT1,
#                            Z2.c=Z2-predT2
#)
#
#
#
#
## Ultimately, we want to test all pairwise comparison of attrition rate across groups. So Thats how we do it
#SampleT1C <- MainDB %>%filter( Assignment %in% c("Control","T1")) %>%
#  mutate(SubSample="T1-C") %>% mutate(Z=ifelse(Assignment=="T1",1,0), 
#                                      Z.c = Z-predT1, 
#                                      WeightPS=Z/predT1+(1-Z)/(1-predT1))
#
#SampleT2C <- MainDB %>%filter( Assignment %in% c("Control","T2"))%>% 
#  mutate(SubSample="T2-C") %>% mutate(Z=ifelse(Assignment=="T2",1,0),
#                                      Z.c=Z-predT2,
#                                      WeightPS=Z/predT2+(1-Z)/(1-predT2))
#SampleT2T1 <- MainDB %>%filter( Assignment %in% c("T2","T1"))%>% mutate(SubSample="T2-T1") %>% mutate(Z=ifelse(Assignment=="T2",1,0), 
#                                                                                                      Z.c=Z-predT2,
#                                                                                                      WeightPS=Z/predT2+(1-Z)/(1-predT2))                                                                          
#
## bind them
#
#StackedDB <- bind_rows(SampleT1C,SampleT2C,SampleT2T1) %>% mutate(Treat=paste("Z=",Z,":",SubSample,sep=""),
#                                                                  SubSampleStrata=paste(SubSample,StrataWave,sep=":")
#)
#
## Propensity score for each pairwise comparison: simple probit
#fit_basicStack <- feglm(Z ~ 1|SubSampleStrata, data = StackedDB,"probit",cluster = ~StrataWave)
#retrieve the prediction


# Prepare all the covariates

#------ PrepCovariates ------
#Here, we build the "\dot{X}" matrix and the interaction with the treatment the we will use in all models for the Lasso
# The idea is to take each variable we consider, to generate dummies, and even centre them by block to mimic the fixed effect regression. We center every variable by block essentially.
# then, for the lasso, all covariates are centred, we can interact them with treatment and therefore keep and ATE interpretation of the coefficient.



## AgeChild1:AgeChild8,Primipare,Age, PregMonth
AgeChildenId <- MainDB %>% select(ResponseId,Primipare,AgeChild1:AgeChild8,Age, PregMonth) %>% 
  mutate_at(vars(Primipare:AgeChild8,Age, PregMonth),~ifelse(is.na(.),0,.)) 
AgeChilden <-  model.matrix(~0+.,AgeChildenId %>% select(-c(ResponseId))) %>% as.data.frame() 


AgeChilden.c <- AgeChilden %>% bind_cols(.,AgeChildenId %>% select(ResponseId)) %>%
  mutate_at(all_of(names(AgeChilden)),~.x-mean(.))%>% ungroup()

#KnowsCreche:KnowsNothing
KnowsId <- MainDB %>% select(ResponseId,KnowsCreche:KnowsCrecheOnly) 
Knows <- model.matrix(~0+.,KnowsId %>% select(-c(ResponseId))) %>% as.data.frame() 
Knows.c <- Knows   %>% bind_cols(.,KnowsId %>% select(ResponseId)) %>%
  mutate_at(all_of(names(Knows)),~.x-mean(.))%>% ungroup()


# Relationship LanguageBaseline, OrderedEduc, Act3, FrenchYNBaseline

SocDemoId <- MainDB %>% select(ResponseId,
                                  Relationship,LanguageBaseline, OrderedEduc, Act3, FrenchYNBaseline,Dep,
                                  "WorkBefore","WorkPlanTo" ,
                                  "WorkPlanNotTo","WorkNoPlan" 
) %>% mutate(Dep=as.factor(Dep))
SocDemo <-  model.matrix(~0+.,SocDemoId %>% select(-c(ResponseId))) %>% as.data.frame() 
SocDemo.c <- SocDemo %>% bind_cols(.,SocDemoId %>% select(ResponseId)) %>% 
  mutate_at(all_of(names(SocDemo)),~.x-mean(.))%>% ungroup()

# FmlyIncomeBaseline, IncomeBaseline

IncomesId <- MainDB %>% select(ResponseId,
                                  FmlyIncomeBaseline, IncomeBaseline) %>% 
  mutate_at(vars(FmlyIncomeBaseline, IncomeBaseline),~as.character(.)) %>% 
  replace_na(list(FmlyIncomeBaseline="NA", IncomeBaseline="NA"))

Incomes <-   model.matrix(~0+.,IncomesId %>% select(-c(ResponseId))) %>% as.data.frame()

Incomes.c <- Incomes %>% bind_cols(.,IncomesId %>%  select(ResponseId)) %>% 
   mutate_at(all_of(names(Incomes)),~.x-mean(.))%>% ungroup()

SubjectiveId <-  MainDB %>% select(ResponseId,
                                      "AccessEasyBaseline", "AccessHoursBaseline" ,"AccessInformalCareBaseline",
                                      "LikertReturnHKBaseline","LikertAccessInfoBaseline"  ,"LikertReturnHKBaseline",
                                      "LikertAccessSoc","NormsBaseline" )

Subjective <- model.matrix(~0+.,SubjectiveId %>% select(-c(ResponseId))) %>% as.data.frame()

Subjective.c <- Subjective %>% bind_cols(.,SubjectiveId %>%  select(ResponseId)) %>% 
  mutate_at(all_of(names(Subjective)),~.x-mean(.))%>% ungroup()


Depriveid <- MainDB %>% select(ResponseId,
                                  "DeprivClothesIrrelevant" ,"DeprivFoodIrrelevant"  ,
                                  "DeprivBillsIrrelevant"   ,"DeprivHolidaysIrrelevant" ,
                                  "DeprivHousingIrrelevant" ,"DeprivClothes" ,
                                  "DeprivFood"              ,"DeprivBills"    ,
                                  "DeprivHolidays"          ,"DeprivHousing"  ,"LevelComprehensionExPost"  ) %>% mutate_at(vars(DeprivClothesIrrelevant:LevelComprehensionExPost),~ifelse(is.na(.),"NA",.))

Deprive <- model.matrix(~0+.,Depriveid %>% select(-c(ResponseId))) %>% as.data.frame()

Deprive.c <- Deprive %>% bind_cols(.,Depriveid %>%  select(ResponseId)) %>% 
 mutate_at(all_of(names(Deprive)),~.x-mean(.)) %>% ungroup()


# Vector of outcome names
Outcomes <- c("ECSApp","ECSUseYes","UseCreche","AppCreche","AppAssMat","Suivi_administratif1_0")
# Vector of secondary outcomes' names
sec.Outcomes <- c("ECSIdealYes","UsePrivée","ECSAppTiming","KnowsAssMatEndline")


# Matrix of centred potential covariates
X.c <-   left_join(AgeChilden.c,Knows.c,by = join_by(ResponseId)) %>% 
  left_join(.,SocDemo.c,by = join_by(ResponseId)) %>% 
  left_join(.,Incomes.c,by = join_by(ResponseId)) %>% 
  left_join(.,Subjective.c,by = join_by(ResponseId)) %>% 
  left_join(.,Deprive.c,by = join_by(ResponseId)) 



# remove unecessary datasets
rm(SocDemo, SocDemo.c, SocDemoId, 
   Subjective, SubjectiveId, Subjective.c,
   AgeChilden, AgeChildenId, AgeChilden.c,
   Deprive, Depriveid, Deprive.c,
   Incomes, IncomesId, Incomes.c,
   Knows, KnowsId, Knows.c) 

#---- MainDBMachineLearning ----
dbMachineLearning <- MainDB %>%# we select the variables that we want. Check that there are the same than below. Obsiouvly, there are several variables that were recoded, and we don't want to have both the recoded version and the original one as they are colinear. 
  select(c(Intention, 
           ECSApp, 
           ECSUseYes,
           AffordCostBaseline, 
           #Educ3, # on laisse educ et educ dad mais je suis pas sûre que ce soit pertinent. Idem pour les variables de revenu
           #Educ3Dad, 
           NormsOpposedN,
           Discount501or0,
           ComputerYN,
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
           AffordCostDK, 
           AffordCostBaseline,
           #    AccessEasyBaseline,
           NormsBaseline,
          # LikertAccessInfoBaseline, # strongly disagree, disagree, don't know, agree, strongly agree
          # LikertAccessSoc, # strongly disagree, disagree, don't know, agree, strongly agree
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
         BabyHealthIndex
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
  )
  
  )%>%
  select(-c(AccessHoursBaseline, LevelComprehensionExPost, ECSIdealAgeEntryBaseline, LevelInfoSubExPost, IdealEntryAge)) %>% # remove the original variables
  mutate(across(where(is.logical), as.factor)) %>% 
  mutate(across(where(is.character), as.factor)) # put them in the right format for tidymodels
# Add knowledge variables

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
    DontKnowECCosts = ifelse(AffordCostDK == TRUE, "Yes", "No"), # Don't know EC costs
    #ProgressiveNorms = ifelse(NormsScore < 2, "Yes", "No"), # Progressive norms: play with that a bit
   # ThinkCostBelow500 = ifelse(AffordCostBaseline < 500, "Yes", "No"), # Think cost below 500
    HasAccessToAComputer = ifelse(ComputerYN == "Oui", "Yes", "No"), # Has access to a computer
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
                  AffordCostDK, 
                  AccessEasy5, 
                  ComputerYN, 
                 # AffordCostBaseline, 
                #  NormsScore, 
                  KnownNbTypeECS, 
                AffordSubsidiesBaseline
                  )) %>% 
  mutate(across(where(is.logical), as.factor)) %>% 
  mutate(across(where(is.character), as.factor)) # put them in the right format for tidymodels

# For now we keep Prohressive norms and ThinkCostBelow500 as numeric because a lot of nas otherwise


# With all
MLDummifiedALL <- dbMachineLearning %>% 
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
    NobodyOpposed = ifelse(NormsOpposedN == 0, "Yes", "No"), # Nobody opposed to the mother using childcare
    NobodyOpposed = ifelse(is.na(NobodyOpposed) == TRUE, "No", as.character(NobodyOpposed)), # Nobody opposed to the mother using childcare
    PresentOriented = ifelse(Discount501or0 == 1, "Yes", "No"), # Present oriented
    KnowsECIsSubsidized = ifelse(AffordSubsidiesBaseline == "Yes", "Yes", "No"), # knows that EC is subsidized
    BelieveAccessingChildcareEasy = ifelse(AccessEasy5 == "Easy", "Yes", "No"), #Maybe check
    DontKnowECCosts = ifelse(AffordCostDK == TRUE, "Yes", "No"), # Don't know EC costs
    ProgressiveNorms = ifelse(NormsScore < 2, "Yes", "No"), # Progressive norms: play with that a bit
    ThinkCostBelow500 = ifelse(AffordCostBaseline < 500, "Yes", "No"), # Think cost below 500
    HasAccessToAComputer = ifelse(ComputerYN == "Oui", "Yes", "No"), # Has access to a computer
    CoverageAboveAverage = ifelse(Taux.de.couv.global > 55, "Yes", "No") # Coverage above average
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
                  AffordCostDK, 
                  AccessEasy5, 
                  ComputerYN, 
                  AffordCostBaseline, 
                  NormsScore, 
                  KnownNbTypeECS, 
                  AffordSubsidiesBaseline, 
                  Taux.de.couv.global
  )) %>% 
  mutate(across(where(is.logical), as.factor)) %>% 
  mutate(across(where(is.character), as.factor)) 
