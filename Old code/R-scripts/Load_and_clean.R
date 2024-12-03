

# ---- Load_and_clean ----

# Cleaned code stops line 644

### WARNING: There are to features to undo depending on the anaylses you want to run 
# Line X: if you work on the intervention, remove miscarriages
# Line X: if you work on the intervention, exclude undocumented mothers
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#######-##-
##### Import and clean the raw survey database %%%%%%%%%%%##
#####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#######-###-

##Import data_tot = full is the raw datasheet with also empty observations (i.e., participants that refused to participate or that did not meet inclusion criteria
data_tot <- read_csv("../Descriptive_statistics/data_full.csv")


## Clean data_tot

#create a df with the first two rows that contains the full questions
details_values <- data_tot[1:2,]


#remove these rows from the main data frame
data_tot <- data_tot[-c(1:2),]


#change the encoding for UTF-8 so that everything is more readable
for (col in colnames(data_tot)){
  Encoding(data_tot[[col]]) <- "UTF-8"
}

#put all missing values as NA
data_tot <- data_tot %>% mutate_all(.,~na_if(.,""))


#convert relevant VARIABLES INTO FACTORS
##NB: pour ajouter les stats sur les heures, prendre le dataset du 24/10 et ajouter "'HOUR_1','HOUR_2','HOUR_3','HOUR_4',"
names <- c('Q3', 'MAT', 'Q11', 'Q5', 'surveyor_id',
           'INTERNETCON', 'DIPLOMA', 
           'PARTNER','BIRTHPLACE', 'BIRTHPLACE2', 'DIPLOMA2', 'OCCUPATION', 'OCCUPATION2',
           'SMOKEYN', 'SMOKEPREG', 'BREASTFEED', 'AfterBirthOccupation',
           'ECSYN','ECS_Used', 'ECS_Subsidies', 'ACCESSBILITY1', 'ACCESSIBILITY2','ECS_Type', 'ECS_Need', 'ECS_Ideal_Entry',
           'INFORMALCARE', 'NORMS', 'COST_Career', 'OPPCOST', 'COST_WB',
           'INFO_RESC', 'SOC_RESC', 'RETURNS', 'COMPUTERYN', 'INCOME', 'FMLYINCOME', 'DISCOUNTING', 
           'LANGUAGE', 'COMP', 'EXP', 'INFO_SUB', 'PATNER', 
           'TRUST_1', 'TRUST_2', 'TRUST_3','TRUST_4', 
           'DEPRIVATION_1', 'DEPRIVATION_2', 'DEPRIVATION_3', 'DEPRIVATION_4', 'DEPRIVATION_5', 'SansPapier', 'ECS_Cost', 'INFORMAL_RCP'
)

data_tot[,names] <- lapply(data_tot[,names] , factor)

#convert relevant VARIABLES INTO numerical ones

names_numerical <- c("Duration (in seconds)", "ZIPCODE_1_TEXT", "Q117_1_TEXT", "RecordedDate", "PREGMONTH_1_TEXT", 
                     "BIRTHDATE#2_1", "BIRTHDATE#3_1", "NBR_KIDS_4_TEXT", "AGE_KIDS#1_1_1", "AGE_KIDS#1_2_1",
                     "AGE_KIDS#1_3_1", "AGE_KIDS#1_4_1", "AGE_KIDS#1_5_1", "AGE_KIDS#1_6_1", "AGE_KIDS#1_7_1", 
                     "AGE_KIDS#1_8_1", "DateArrival_16_TEXT", "DateArrival2_16_TEXT", "LengthBREASTFEED_4_TEXT",
                     "ECS_Date#2_1", "ECS_Cost_1_TEXT", "NUMBER_1_TEXT", "NUMBER2_1_TEXT")
data_tot[,names_numerical] <- lapply(data_tot[,names_numerical] , as.numeric)


## Dealing with the LANGUAGE column

#1.there were a mistake in LANGUAGE for one participant, row 1003, we correct it

# change the language of the participant with error in LANGUAGE code
data_tot$LANGUAGE[1003] <- "français"
data_tot$LANGUAGE[666] <- "arabe"


#2. Some participants are coded as "other" or "Je ne souhaite pas regarder les vidéos" in the LANGUAGE column, they need to be turned into French

# data_tot <- data_tot %>% 
#   mutate(LANGUAGE = fct_collapse (LANGUAGE,
#                                   french = c("français", "Je ne souhaite pas recevoir les vidéos", "Autre"), 
#                                   english = "anglais",
#                                   arabic = "arabe"))


## Delete miscarriages if working on the endline survey
#rows_to_delete <- c(1325, 1046, 470, 961)
#data_tot<- subset(data_tot, !(row.names(data_tot) %in% rows_to_delete))

#######- CREATE NEW DATASETS keeping only completed questionnaires: data_tot_clean

# All the observation that we can keep for Baseline
## We filter individuals who did not fill the mandatory question. INCOME is one.
data_tot_clean <- data_tot %>% 
  drop_na("INCOME")


## Isolate date (StartDate_D) and time (StartDate_T) variables to have proper survey dates 
data_tot_clean$StartDate_D <- as.Date(data_tot_clean$StartDate)
data_tot_clean$StartDate_T <-strftime(data_tot_clean$StartDate, format="%H:%M:%S")


data_tot_clean$StartDate = ymd_hms(data_tot_clean$StartDate)
data_tot_clean$EndDate = ymd_hms(data_tot_clean$EndDate)

#compute mothers' birth-dates into age: AGE variable
data_tot_clean$BirthDate <- paste(data_tot_clean$`BIRTHDATE#3_1`,data_tot_clean$`BIRTHDATE#1_1`,data_tot_clean$`BIRTHDATE#2_1`, sep="-")
data_tot_clean$BirthDate <- ymd(data_tot_clean$BirthDate)

date_ref <-  ymd("2022-12-31")

data_tot_clean$Age <- age_calc(data_tot_clean$BirthDate, date_ref, units = "years")
data_tot_clean$Age = floor(data_tot_clean$Age) 
data_tot_clean$Age <- as.numeric(data_tot_clean$Age)

#create an Early Childcare Structure (ECS) ideal entry date
data_tot_clean$ECSDate <- paste(data_tot_clean$`ECS_Date#1_1`,data_tot_clean$`ECS_Date#2_1`, sep=" ")
data_tot_clean$ECSDate <- myd(data_tot_clean$ECSDate,  truncated = 1)

##put the DECEMBER 2026 observations that coded for "Don't know" as NA
# TO DO: in later versions, this should be replaced by "Ne sait pas encore" instead of NA
data_tot_clean$ECSDate <- 
  na_if(data_tot_clean$ECSDate, ymd("2026-12-01"))



## ZIPCODES : Creation of a variable 'DEP' that contains the department numbers of all participants.
data_tot_clean <- data_tot_clean %>%
  mutate(DEP = substr(ZIPCODE_1_TEXT, 1, 2))
data_tot_clean$DEP <- as.factor(data_tot_clean$DEP)


## Manipulate the number and types of ECS Known in order to do stats on the information level

ECS_df <- data_tot_clean %>% select(ResponseId,ECS_Type, ECS_Known_Y, ECS_Known_N) %>% 
  mutate_at(vars(ECS_Type, ECS_Known_Y, ECS_Known_N),~str_replace_all(.,"Autres types de crèches","Autre")) %>%
 # mutate_at(vars(ECS_Type, ECS_Known_Y, ECS_Known_N),~ifelse(.=="Aucun autre"|.=="Aucun",NA,.)) %>% 
  mutate(ECS_Type=ifelse(ECS_Type=="Je ne souhaite pas répondre","Je ne sais pas encore",ECS_Type)) %>% 
  mutate(ECSKnown=paste(ECS_Known_Y, ECS_Known_N,sep=",")) %>% 
  separate_longer_delim(ECSKnown, delim = ",") %>% # separate the elements in ECSKnown in long format
        mutate(ECSKnown=str_remove_all(ECSKnown,"NA,|,NA|NA")) %>% 
  filter(ECSKnown!="") %>%  #remove NA and if the known element is the one they intend to use.
  mutate(KnowsCreche   =str_detect(ECS_Type,"Crèches")|str_detect(ECSKnown,"Crèches"), #Asked or know any crèche
         KnowsCrecheA  =str_detect(ECS_Type,"associatives")|str_detect(ECSKnown,"associatives"), # Asked or know crèche associatives
         KnowsCrechePub=str_detect(ECS_Type,"Crèches") & str_detect(ECSKnown,"associatives")==FALSE,  # Asked or know public crèches
         KnowsAssMat   =str_detect(ECS_Type,"Assistantes")|str_detect(ECSKnown,"Assistantes"), # Asked or know assistantes maternelles (private childminders)
         KnowsNotYet   =str_detect(ECS_Type,"Je ne sais pas encore"), # Did not know yet which type of childcare they wanted
         KnowsJE       =str_detect(ECS_Type,"Jardin d’enfant")|str_detect(ECSKnown,"Jardin d’enfant"), # Asked or know jardin d'enfant 
         KnowsAutre    =str_detect(ECS_Type,"Autre")|str_detect(ECSKnown,"Autre"), # Asked or know a type of childcare that was not in the list
         KnowsPartage  =str_detect(ECS_Type,"partagée")|str_detect(ECSKnown,"partagée"), # Asked or know shared nannies
         KnowsHG       =str_detect(ECS_Type,"Haltes garderies")|str_detect(ECSKnown,"Haltes garderies"), # Asked or know drop-in daycare
         KnowsNounou   =str_detect(ECS_Type,"Nounou")|str_detect(ECSKnown,"Nounou"), # Asked or know nannies
         KnowsNothing  =(is.na(ECS_Type) & ECS_Known_N=="Aucun")| # Asked or know nothing
           (str_detect(ECS_Type,"souhaite pas|ne sais pas encore") & str_detect(ECS_Known_Y,"Aucun")),
         WantCreche   =str_detect(ECS_Type,"Crèches"), # Want crèches (daycare)
         WantCrecheA  =str_detect(ECS_Type,"associatives"),  # Want associative daycare
         WantAssMat   =str_detect(ECS_Type,"Assistantes"),  # Want private childminders
         WantJE       =str_detect(ECS_Type,"associatives"),  ###### ERROR
         WantAutre    =str_detect(ECS_Type,"Autre|Jardin d’enfant"), # Want other type of care
         WantPartage  =str_detect(ECS_Type,"partagée"), # Want shared nanny
         WantHG       =str_detect(ECS_Type,"Haltes garderies"), # Want drop-in daycae
         WantNounou   =str_detect(ECS_Type,"Nounou"),  # Want nanny
         ) %>% 
   mutate_at(vars(KnowsCreche:WantNounou),~ifelse(is.na(.),FALSE,.)) %>% 
  group_by(ResponseId) %>% 
  summarise_at(vars(KnowsCreche:WantNounou),~ifelse(sum(.)>=1,1,0)) %>% 
  mutate(KnowsCrecheOnly=(KnowsCreche==1|KnowsCrecheA==1|KnowsCrechePub==1) &    # Knows only daycare
           (KnowsAssMat +
            KnowsNotYet +
            KnowsJE     +
            KnowsAutre  +
            KnowsPartage+
            KnowsHG     +
            KnowsNounou )==0,
         KnownNbTypeECS=KnowsCrecheA  +
                        KnowsCrechePub+
                        KnowsAssMat   +
                        KnowsJE       +
                        KnowsAutre    +
                        KnowsPartage  +
                        KnowsHG       +
                        KnowsNounou ,
         Knows0r1ECS=KnownNbTypeECS<=1
                        )


## Merge with data_tot_clean

data_tot_clean <- data_tot_clean %>% left_join(.,ECS_df) %>% 
          mutate(InterviewDate=ymd(StartDate_D),
                 SingleMum=PARTNER %in% c("Non, et je ne suis pas en couple","Je ne sais pas", # the participant is a single parent (mother)
                                          "Je ne souhaite pas répondre"),
                 CoupleCohabiting= PARTNER=="Oui", # The participant is living with her partner
                 Couple=PARTNER %in% c("Oui", "Non, mais je suis en couple"),  # the participant has a partner
                 Relationship=case_when(CoupleCohabiting==TRUE~"Couple cohabitant", # Variable that describes the type of relationship the household has 
                                        Couple==TRUE~"Couple non cohabitant",
                                        SingleMum==TRUE~"Famille monoparentale"),
                 LANGUAGE=ifelse(as.character(LANGUAGE)=="Je ne souhaite pas recevoir les vidéos","français",as.character(LANGUAGE)), # recode the "français" category
                 Language=factor(LANGUAGE,
                                 levels=c("anglais","arabe","français", "Autre")),  # create a Language variable that has 4 levels : english, arabic, french or other
                 PregMonth=PREGMONTH_1_TEXT,                                        # rename the number of months of pregnancy
                 Preg7Less = ifelse(PregMonth <= 7,"7Less","8OR9"),                 # binary variable that has two levels: 7 months of pregnancy or less, and 8 or 9 months of pregnancy
                 SMOKEYN=ifelse(as.character(SMOKEYN)=="Je ne sais pas","Je ne souhaite pas répondre",as.character(SMOKEYN)), 
                 SMOKEPREG=ifelse(as.character(SMOKEPREG)=="Ne sait pas","Vous avez continué à fumer",as.character(SMOKEPREG)), # Recode the variable on whether the mother smoked during pregnancy
                 SmokePreg = case_when(
                 SMOKEYN == "Non" ~ "Did Not Smoke",
                 SMOKEYN == "Je ne souhaite pas répondre" ~ "Don't Want To Answer",
                 SMOKEYN == "Oui" & SMOKEPREG == "Vous avez continué à fumer" ~ "Continued Smoking", 
                 SMOKEYN == "Oui" & SMOKEPREG == "Vous avez continué mais réduit" ~ "Reduced Smoking",
                 SMOKEYN == "Oui" & SMOKEPREG == "Vous avez arrêté complètement" ~ "Stopped Smoking",
                 TRUE~NA),
                 BreastFeedIntend=case_when(as.character(BREASTFEED)=="Non"~"Non",                                         # recode the variable on Breastfeeding intention afterbirth
                                            as.character(BREASTFEED) %in% 
                                              c("Je ne sais pas encore","Je ne souhaite pas répondre")~"Ne sait pas encore",
                                            as.character(LengthBREASTFEED) %in% 
                                              c("Je ne sais pas encore","Je ne souhaite pas répondre")~" Oui, mais pas la durée",
                                            as.character(LengthBREASTFEED)=="Nombre de mois"~"Oui, durée anticipée",
                                            TRUE~NA
                                            ),
                 BreastFeedLength=as.numeric(LengthBREASTFEED_4_TEXT),                                                  # Intended breastfeeding period lenght
                 BreastFeedIntend1or0 = ifelse(BreastFeedIntend %in% c("Oui, mais pas la durée", "Oui, durée anticipée"), 1, 0), # Dummy for breastfeeding intention
                 OrderedEduc=factor(DIPLOMA, levels = c(
                   "N’est jamais allé à l’école (6-8 ans)", "CEP(Certificat d’Etudes Primaires) / fin école primaire (9-11 ans)",     # recode mothers' level of education
                   "BEPC, Brevet élémentaire, Brevet des collèges / fin collège (13-16 ans)",
                   "CAP, BEP",
                   "Bac / fin lycée (16-19 ans)",
                   "Bac +2 (Deug, BTS, DUT) (19-20 ans)",
                   "Bac +3  / fin d’un diplôme de premier cycle universitaire (21-22 ans)",
                   "Bac +4 ou +5 (2nd cycle universitaire, ingénieur, grande école…) (22-24 ans)",
                   "Plus de Bac +5 (24-26 ans)"), ordered = TRUE, labels = 
                     c("No school", "9-11", "Brevet", "CAP", "Bac", "Bac +2", "Bac + 3", "Bac + 4/5", "+ Bac+5")),                 # create a binary variable for post-secondary education "sup" or not "Bac"
                 Educ=fct_collapse(DIPLOMA,
                                      Bac = c("Bac / fin lycée (16-19 ans)", 
                                              "BEPC, Brevet élémentaire, Brevet des collèges / fin collège (13-16 ans)", 
                                              "CAP, BEP", "CEP(Certificat d’Etudes Primaires) / fin école primaire (9-11 ans)",
                                              "N’est jamais allé à l’école (6-8 ans)"),
                                      Sup = c("Bac +2 (Deug, BTS, DUT) (19-20 ans)", 
                                              "Bac +3  / fin d’un diplôme de premier cycle universitaire (21-22 ans)",
                                              "Bac +4 ou +5 (2nd cycle universitaire, ingénieur, grande école…) (22-24 ans)",
                                              "Plus de Bac +5 (24-26 ans)")),
                 Act3= fct_collapse(OCCUPATION,                                   # create a categorical variable for whether the mother is active (i.e. has a job or an activity) or not
                              Active = c("Je travaille (y compris si congé maladie, maternité ou parental)", "Je suis étudiant(e), stagiaire ou fait une formation"), 
                              Inactive = c("Je suis au chômage, cherche un emploi", "Je suis femme au foyer", "Autre", "J’ai une pension de retraite, d’invalidité, etc."), 
                              NSP = c("Ne souhaite pas répondre", "Ne sait pas")),
                 FmlyIncome=factor(FMLYINCOME, levels = c(                        # recode family income in shorter labels
                   "Je ne sais pas",
                   "Je ne souhaite pas répondre",
                   "Moins de 500 euros",
                   "Entre 500 et 700 euros",
                   "Entre 700 et 900 euros ",
                   "Entre 1000 et 1500 euros",
                   "Entre 1500 et 2000 euros",
                   "Entre 2000 et 2500 euros ",
                   "Entre 2500 et 3000 euros",
                   "Entre 3000 et 4500 euros",
                   "Entre 4500 et 5000 euros", 
                   "Entre 5000 et 6000 euros",
                   "Entre 6000 et 7000 euros",
                   "Entre 7000 et 8000 euros",
                   "Plus de 8000"), ordered = TRUE, labels = 
                     c("NSP", "Ne veut pas répondre", "- 500", "500-700", "700-1000", "1000-1500", 
                       "1500-2000", "2000-2500", "2500-3000", "3000-4500", "4500-5000",
                       "5000-6000", "6000-7000", "7000-8000", "+8000")),
                 Income=factor(INCOME, levels = c(
                   "Je ne sais pas", "Je ne souhaite pas répondre",               # recode mothers' income in shorter labels
                   "Moins de 500 euros",
                   "Entre 500 et 700 euros",
                   "Entre 700 et 900 euros ",
                   "Entre 1000 et 1500 euros",
                   "Entre 1500 et 2000 euros",
                   "Entre 2000 et 2500 euros",
                   "Entre 2500 et 3000 euros",
                   "Entre 3000 et 4500 euros",
                   "Entre 4500 et 5000 euros", 
                   "Entre 5000 et 6000 euros",
                   "Plus de 6000 "), ordered = TRUE, labels = 
                     c("NSP", "Ne veut pas répondre", "- 500",
                       "500-700", "700-1000", "1000-1500",
                       "1500-2000", "2000-2500", "2500-3000",
                       "3000-4500", "4500-5000",
                       "5000-6000", "+6000")),
                 ECSType7=fct_collapse(ECS_Type,                                         # create a simpler categorical variable for the type of childcare 
                                             Crèches = c("Autres types de crèches",      
                                                         "Crèches municipales (ou juste «crèches »)", 
                                                         "Crèches associatives"), 
                                             Nounou = c("Nounou à domicile", "Garde partagée"), 
                                             NSP = c("Je ne sais pas encore", "Je ne souhaite pas répondre")),
                 ECSType7=factor(ECSType7, levels = c(
                   "Crèches", "Assistantes maternelles",   # 7 levels : daycare, private childminder, nanny, drop-in daycare, jardin d'enfant 
                   "Nounou",                               # Other or Don't know yet
                   "Haltes garderies",
                   "Jardin d’enfant", "Autre",
                   "NSP", NA)),
                 FrenchYN = fct_collapse(BIRTHPLACE, 
                                         French = c("En France métropolitaine", "Dans un territoire français d’outre-mer"), # binary variable for the migration status 
                                         Else = c("Afrique subsaharienne", "MENA (Maghreb +Arabie saoudite, etc.)",         # Else = the mother is not born in France
                                                  "Europe/Amérique du Nord, Australie", "Asie (hors MENA)", 
                                                  "Amérique Latine (à partir du Mexique)", 
                                                  "Autre", "Je ne souhaite pas répondre" ) ) ,
                 Intention=fct_collapse(ECSYN,
                                           No = c("Je ne sais pas encore", "Non"), # dummy for has the intention to use childcare afterbirth
                                           Yes = "Oui"),
                 ECSUsed =ifelse(is.na(ECS_Used),"Non",as.character(ECS_Used)),    # Has ever used ECS
                 ECSNeed=ifelse(as.character(ECS_Need)=="Ne souhaite pas répondre","Ne sait pas encore",as.character(ECS_Need)), # intensity of childcare needed
                 ECSNeed=factor(ECSNeed,
                                levels=c("A temps plein","De temps en temps","Jamais", "Ne sait pas encore")), # full-time, part-time, never, don't know yet
                 ECSIdealAgeEntry=factor(ECS_Ideal_Entry, levels=c("Jamais","Pendant ses trois premiers mois","Pendant ses six premiers mois", 
                                                                   "Pendant ses neuf premiers mois", "Pendant la première année", "Jusqu’à l’entrée à la maternelle",
                                                                   "Ne sait pas")), # what is the ideal entry age in childcare for parents
                 AffordSubsidies=fct_collapse(ECS_Subsidies, # Knows that the cost of childcare is reduced through subsidies 
                                              Yes = "Oui", 
                                              No_NSP = c("Je ne sais pas", "Non")),
                 AffordSubsidies1or0 = ifelse(AffordSubsidies == "Yes", 1, 0), 
                 COST_WB=ifelse(as.character(COST_WB)=="Je ne souhaite pas répondre","Je ne sais pas encore",as.character(COST_WB)),
                 AffordNMCostWB=factor(COST_WB,
                                       levels=c("Négatif",  "Je ne sais pas encore","Pas d'impact","Positif"), ordered = TRUE # would staying at home have a positive, 
                                       ),                                                                   # negative or null impact on mother's well being
                 OPPCOST=ifelse(as.character(OPPCOST)=="Je ne souhaite pas répondre","Je ne sais pas encore",as.character(OPPCOST)),
                 AffordOppCost=factor(OPPCOST,
                                      levels=c("Négatif",  "Je ne sais pas encore", "Pas d'impact", "Positif"), ordered = TRUE # would staying at home have a positive, 
                 ),                                                                   # negative or null impact on household's income
                 COST_Career=ifelse(as.character(COST_Career)=="Je ne souhaite pas répondre","Je ne sais pas encore",as.character(COST_Career)),
                 AffordNMCostCareer=factor(COST_Career, # would staying at home have a positive, 
                                           levels=c("Négatif", "Pas d'impact", "Je ne sais pas encore","Positif"), ordered = TRUE # negative or null impact on mother's career
                 ),
                 DISCOUNTING=ifelse(as.character(DISCOUNTING)=="Je ne souhaite pas répondre","Je ne sais pas",as.character(DISCOUNTING)),
                 DiscountRate=factor(DISCOUNTING,
                                     levels=c("Je ne sais pas","50€ dans 3 jours","80€ dans 3 mois")), #discount rate 
                 ACCESSBILITY1= ifelse(as.character(ACCESSBILITY1)=="je ne souhaite pas répondre","je ne sais pas",as.character(ACCESSBILITY1)), 
                 AccessEasy=factor(ACCESSBILITY1,levels =c("impossible","quasiment impossible", # Whether accessing a slot in ECS is seen as easy
                                                           "très difficile","assez difficile",
                                                           "je ne sais pas","assez facile",
                                                           "très facile"),ordered = TRUE), 
                 ACCESSIBILITY2= ifelse(as.character(ACCESSIBILITY2)=="Je ne souhaite pas répondre","Je ne sais pas encore",as.character(ACCESSIBILITY2)),
                 AccessHours=factor(ACCESSIBILITY2,levels =c("Oui", # Whether ECS opening hours fit parents working hours
                                                           "Non, mais on prendra un autre mode de garde en plus","Je ne sais pas encore",
                                                           "Non"),ordered = TRUE),
                 AccessInformalCare=factor(INFORMALCARE,levels =c("Oui, autant que nécessaire", # Whether accessing informal care is feasible
                 "Oui, de temps en temps","Oui, pour dépanner / très ponctuellement",
                 "Je ne sais pas encore",
                 "Non"),ordered = TRUE),
                 NORMS=ifelse(as.character(NORMS)=="Ne souhaite pas répondre","Ne sait pas",as.character(NORMS)),
                 Norms=factor(NORMS,
                              levels=c("Ne sait pas","Aucune","Une minorité","Moitié moitié","La plupart","Toutes") # how many mothers in their surroundings use childcare 
                              ),
                 Q117= ifelse(as.character(Q117)=="Je ne souhaite pas répondre","Je ne sais pas",as.character(Q117)),
                 NormsOpposedDK=factor(Q117,levels =c("Nombre",                          # Norms: how many oppose: number or don't know
                                                    "Je ne sais pas"),ordered = TRUE),
                 NormsOpposedN=Q117_1_TEXT,
                 ) %>% mutate_at(vars(INFO_RESC, RETURNS,SOC_RESC),
                                 ~ifelse(as.character(.)=="Je ne souhaite pas répondre",
                                         "Je ne sais pas",as.character(.))) %>% 
                mutate(
                LikertAccessInfo=factor(INFO_RESC, # Is it feasible to access information about childcare
                                        levels=c("Très en désaccord","Assez en désaccord","Je ne sais pas","Assez d’accord","Très d’accord")), # strongly disagree, disagree, don't know, agree, strongly agree
                LikertReturnHK=factor(RETURNS, # believe in childcare returns on investment: is a child that attended childcare better developed in preschool?
                                        levels=c("Très en désaccord","Assez en désaccord","Je ne sais pas","Assez d’accord","Très d’accord")), # strongly disagree, disagree, don't know, agree, strongly agree
                LikertReturnHK1or0 = ifelse(LikertReturnHK %in% c("Assez d’accord", "Très d’accord "), 1, 0),
                LikertAccessSoc=factor(SOC_RESC, # Does the mother have someone that can help with paperwork if needed
                       levels=c("Très en désaccord","Assez en désaccord","Je ne sais pas","Assez d’accord","Très d’accord")), # strongly disagree, disagree, don't know, agree, strongly agree
                TrustFamIrrelevant=as.character(TRUST_1)=="Pas Pertinent", # Put the "Irrelevant" levels aside to analyse the likerts below properly
                TrustFriendIrrelevant=as.character(TRUST_2)=="Pas Pertinent",
                TrustAssMatIrrelevant=as.character(TRUST_3)=="Pas Pertinent",
                TrustCrecheIrrelevant=as.character(TRUST_4)=="Pas Pertinent",
                TrustFam=factor(TRUST_1, # TRUST: Do the parents trust their close family (parents, brother, sister, etc.) to take care of the baby
                                levels=c("Très en désaccord","Assez en désaccord","Je ne sais pas","Assez d’accord","Très d’accord"),
                                ),
                TrustFriend=factor(TRUST_2, # TRUST: Do the parents trust their friends to take care of the baby
                                levels=c("Très en désaccord","Assez en désaccord","Je ne sais pas","Assez d’accord","Très d’accord"),
                ),
                TrustAssMat=factor(TRUST_3, # TRUST: Do the parents trust a private childminder to take care of the baby
                                levels=c("Très en désaccord","Assez en désaccord","Je ne sais pas","Assez d’accord","Très d’accord"),
                ),
                TrustCreche=factor(TRUST_4, # TRUST: Do the parents trust their family to take care of the baby
                                levels=c("Très en désaccord","Assez en désaccord","Je ne sais pas","Assez d’accord","Très d’accord"),
                ),
                TrustCreche1or0 = ifelse(TrustCreche %in% c("Assez d’accord", "Très d’accord"), 1, 0),
                DeprivClothesIrrelevant=as.character(DEPRIVATION_1)=="Pas pertinent", # Put the "Irrelevant" levels aside to analyse the likerts below properly
                DeprivFoodIrrelevant=as.character(DEPRIVATION_2)=="Pas pertinent",
                DeprivBillsIrrelevant=as.character(DEPRIVATION_3)=="Pas pertinent",
                DeprivHolidaysIrrelevant=as.character(DEPRIVATION_4)=="Pas pertinent",
                DeprivHousingIrrelevant=as.character(DEPRIVATION_5)=="Pas pertinent",
                DeprivClothes =factor(DEPRIVATION_1,levels=c("Pas du tout","Un peu","Beaucoup","Je ne souhaite pas répondre")), # DEPRIVATION: do parents struggle to buy clothes?
                DeprivFood    =factor(DEPRIVATION_2,levels=c("Pas du tout","Un peu","Beaucoup","Je ne souhaite pas répondre")), # DEPRIVATION: do parents struggle to buy food?
                DeprivBills   =factor(DEPRIVATION_3,levels=c("Pas du tout","Un peu","Beaucoup","Je ne souhaite pas répondre")), # DEPRIVATION: do parents struggle to pay for the bills?
                DeprivHolidays=factor(DEPRIVATION_4,levels=c("Pas du tout","Un peu","Beaucoup","Je ne souhaite pas répondre")), # DEPRIVATION: do parents struggle to go in holidays?
                DeprivHousing =factor(DEPRIVATION_5,levels=c("Pas du tout","Un peu","Beaucoup","Je ne souhaite pas répondre")), # DEPRIVATION: do parents struggle to pay for housing?
                InformalReciprocity = INFORMAL_RCP, # Do the parents already take care of the children of others
                LevelComprehensionExPost = COMP,    # Surveyor codded ex post the level of comprehension of the participants
                LevelExpressionExPost = EXP,        # Surveyor codded ex post the level of expression of the participants
                LevelInfoSubExPost = recode(data_tot_clean$INFO_SUB, # Surveyor codded ex post the level of information about childcare of the participants
                                            "aucun ou très très bas" = "Aucun ou très bas",
                                            "basique : connait les bases (ex. les crèches, parle de quelque chose comme les nounous à domicile), mais pas le système (ex. pense que si pas de travail c'est impossible, n'a aucune idée des coûts et de comment ça fonctionne)" = "Basique",
                                            "plutôt bonne : semble connaître les subtilités (ex. connait aussi les assistantes maternelles, etc.), mais pas tout" = "Bonne",
                                            "excellente: connais la plupart des subtilités (ex. sait que les subventions dépendent du revenu, que les parents qui ont pas de travail peuvent être prioritaires, mentionne les commissions, etc.)" = "Excellente"),
                LevelPartnerAnsweredExPost = PATNER,  # Surveyor codded ex post whether the father (if present) took into account the point of view of the mother when answering
                AffordCostDK =ECS_Cost %in% c("Je ne sais pas", "Je ne souhaite pas répondre"), # Don't Know the cost if childcare 
                AffordCost = as.numeric(ECS_Cost_1_TEXT), # Families were ask how much do they think childcare would cost them, 
                ComputerYN = ifelse(as.character(COMPUTERYN)=="Je ne sais pas encore","Non",as.character(COMPUTERYN)), # Whether families can access a computer
                NumberChildren = as.numeric(NBR_KIDS_4_TEXT), # Turn the number of children the family already has into a numeric variable
                Primipare = NBR_KIDS_4_TEXT == 0,  # Create a dummy for whether the mother is primipare
                AgeSup32 = ifelse(Age > 32, "Yes", "No"),  # Create a dummy for whether the mother is above median age in the sample
                AccessImpossible = ifelse(AccessEasy == "impossible", 1, 0),  # Create a dummy for whether the mother saw have a place in crèche as impossible
                AccessHard = ifelse(AccessEasy %in% c("quasiment impossible", "très difficile", "assez difficile"), 1, 0), # Create a dummy for whether the mother saw have a place in crèche as hard
                AccessHardtoImpossible = ifelse(AccessEasy %in% c("quasiment impossible", "très difficile", "assez difficile", "impossible"), 1, 0),# Create a dummy for whether the mother saw have a place in crèche as impossible
                AccessEasy1or0 = ifelse(AccessEasy %in% c("assez facile", "très facile"), 1, 0), # Create a dummy for whether the mother saw have a place in crèche as easy
                AccessEasyDK = ifelse(AccessEasy == "je ne sais pas", 1, 0), # Create a dummy for whether the mother doesn't know if have a place in crèche is hard or not
                AccessNotHours = ifelse(AccessHours == "Non", 1, 0),  # Create a dummy for whether the mother doesn't know if have a place in crèche is hard or not
                AccessNotInformalCare = ifelse(AccessInformalCare == "Non", 1, 0), 
                AccessInformalCareAMAP = ifelse(AccessInformalCare == "Oui, autant que nécessaire", 1, 0)
                )
                

                      
  


         
#### Values:
# This question indirectly measured the (parenting) norms of the parents by asking with of a list of values is more important for a child to aquire
# Parents could select two values maximum. Here we create dummies for whether the parent selected one given value or not
# We first pivot longer, and then join the result to the dataframe
Values <- data_tot_clean %>% select(ResponseId,VALUES) %>% separate_longer_delim(VALUES,delim = ",") %>% 
                    mutate(
                      ValGenerosity=as.character(VALUES)=="Générosité / sens du partage",
                      ValIndependence=as.character(VALUES)=="Indépendance / autonomie",
                      ValSocialisation=as.character(VALUES)=="Socialisation",
                      ValObedience=as.character(VALUES)=="Obéissance",
                      ValEducation=as.character(VALUES)=="Bonnes manières / éducation",
                      ValDK=as.character(VALUES)=="Je ne sais pas"|as.character(VALUES)=="Je ne souhaite pas répondre",
                      ValImagination=as.character(VALUES)=="Imagination / créativité",
                      ValNone=as.character(VALUES)=="Aucun des choix"
                    ) %>% select(-VALUES) %>% 
  group_by(ResponseId) %>% mutate_at(vars(ValGenerosity:ValNone),~sum(.)) %>% distinct()


## Merge the values created with the dataframe 
data_tot_clean <- data_tot_clean %>% left_join(.,Values)



### Other covariates


# Rename AGE_KIDS_ for after that coded the age of each older siblings 
names(data_tot_clean)[str_detect(names(data_tot_clean),"AGE_KIDS")] <- paste("AgeChild",c(1:8),sep="")

data_tot_clean <- data_tot_clean %>% 
                          mutate(BirthPlace=case_when(BIRTHPLACE_11_TEXT=="Afghanistan"~"MENA (Maghreb +Arabie saoudite, etc.)", # recode birthplace of the mother
                                                      BIRTHPLACE_11_TEXT=="Serbie"~"Europe/Amérique du Nord, Australie",
                                                      BIRTHPLACE_11_TEXT=="Haiti"|BIRTHPLACE_11_TEXT=="Haïti"~"Afrique subsaharienne",
                                                      !is.na(BIRTHPLACE)~BIRTHPLACE
                          ),
                          BirthPlace2=case_when(BIRTHPLACE2_11_TEXT=="Afghanistan"~"MENA (Maghreb +Arabie saoudite, etc.)",  # recode birthplace of the other parent
                                                BIRTHPLACE2_11_TEXT=="Serbie"~"Europe/Amérique du Nord, Australie",
                                                BIRTHPLACE2_11_TEXT=="Haiti"|BIRTHPLACE2_11_TEXT=="Haïti"|
                                                BIRTHPLACE2_11_TEXT=="comores"|BIRTHPLACE2_11_TEXT=="Comores"~"Afrique subsaharienne",
                                                BIRTHPLACE2_11_TEXT=="Israel"~"Europe/Amérique du Nord, Australie",
                                                !is.na(BIRTHPLACE2)~BIRTHPLACE2),
                          DateArrival_16_TEXT = case_when( # Recode the date of arrival 
                            DateArrival_16_TEXT == "19996" ~ "1996", # 1. Recode the typos in the column containing 
                            DateArrival_16_TEXT == "20002" ~ "2002", # the exact date of arrival for some participants
                            DateArrival_16_TEXT == "20111" ~ "2011",
                            DateArrival_16_TEXT == "20120" ~ "2012",
                            TRUE ~ as.character(DateArrival_16_TEXT)  # Keep the original value if none of the above conditions match
                          ),
                          DateArrival_16_TEXT = as.numeric(DateArrival_16_TEXT),
                          DateArrival = case_when( # 2. Now, put the exact dates of arrival in DateArrival_16_TEXT into the corresponding levels
                            is.na(DateArrival_16_TEXT) ~ DateArrival,
                            DateArrival_16_TEXT >= 2018 | DateArrival == "Je ne sais pas" ~ "Moins de 5 ans",
                            DateArrival_16_TEXT < 2018 & DateArrival_16_TEXT >= 2013 ~ "Entre 5 et 10 ans",
                            DateArrival_16_TEXT < 2013 & DateArrival_16_TEXT >= 2003 ~ "Entre 10 et 20 ans",
                            DateArrival_16_TEXT <= 2003 ~ "Il y a plus de 20 ans",
                            TRUE ~ as.character(DateArrival)  # Convert DateArrival back to character
                          ),
                          DateArrival2_16_TEXT=ifelse(DateArrival2_16_TEXT=="201","2010",DateArrival2_16_TEXT), # Do the same for the date of arrival for the second parent
                          DateArrival2_16_TEXT=as.numeric(DateArrival2_16_TEXT),
                          DateArrival2=case_when(
                                                 DateArrival2_16_TEXT>=2018~"Moins de 5 ans", 
                                                 DateArrival2_16_TEXT<2018 &
                                                 DateArrival2_16_TEXT>=2013 ~"Entre 5 et 10 ans",
                                                 DateArrival2_16_TEXT<2013 &
                                                 DateArrival2_16_TEXT>=2003 ~"Entre 10 et 20 ans",
                                                 DateArrival2_16_TEXT<=2003 ~ "Il y a plus de 20 ans",
                                                 DateArrival2=="Je ne sais pas"|DateArrival2=="Je ne souhaite pas répondre"~"Manquant",
                                                 DateArrival2!="Année"~ as.character(DateArrival2),
                          ),
                          OrderedEduc2=case_when(DIPLOMA2=="Ne souhaite pas répondre"|         # recode the level of education of the second parent
                                                 DIPLOMA2=="Ne sait pas"|                      # 1. deal with missing values
                                                 DIPLOMA2=="Autre. Préciser :"~"Missing",
                                                 !is.na(DIPLOMA2)~DIPLOMA2,
                                                 TRUE~"No Partner"),
                          OrderedEduc2=factor(OrderedEduc2, levels = c(
                            "N’est jamais allé à l’école (6-8 ans)", "CEP(Certificat d’Etudes Primaires) / fin école primaire (9-11 ans)",
                            "BEPC, Brevet élémentaire, Brevet des collèges / fin collège (13-16 ans)", # 2. then recode the levels with shorter labels
                            "CAP, BEP",
                            "Bac / fin lycée (17-19 ans)",
                            "Bac +2 (Deug, BTS, DUT) (19-20 ans)",
                            "Bac +3  / fin d’un diplôme de premier cycle universitaire (21-22 ans)",
                            "Bac +4 ou +5 (2nd cycle universitaire, ingénieur, grande école…) (22-24 ans)",
                            "Plus de Bac +5 (24-26 ans)","Missing","No Partner"), ordered = TRUE, labels = c(
                            "No school", "9-11", "Brevet", "CAP", "Bac", "Bac +2", "Bac + 3", "Bac + 4/5", "+ Bac+5","Missing","No Partner")),        
                          Occupation=ifelse(
                            OCCUPATION %in% c("Autre", # Recode occupation  of the mother: put all the missings and the small categories 
                                              "Ne sait pas",   # into the same level: "Autre"
                                              "Ne souhaite pas répondre",
                                              "J’ai une pension de retraite, d’invalidité, etc."),"Autre",as.character(OCCUPATION)),
                          Occupation = as.factor(Occupation),
                          Occupation2=case_when( # Recode the occupation of the second parent: 
                            OCCUPATION2_6_TEXT=="Maitre de conference"~"travaille (y compris si congé maladie, maternité ou parental)",
                            OCCUPATION2_6_TEXT=="Reconversion"~"est au chômage, cherche un emploi", # 1. deal with the manually written occupations
                            OCCUPATION2 %in% c("Ne sait pas","Ne souhaite pas réponndre","a une pension de retraite, d’invalidité, etc.")~"Autre", # 2. and missing values
                            !is.na(OCCUPATION2)~OCCUPATION2,
                            TRUE~"No Partner"
                          ),
                          Occupation2 = as.factor(Occupation2),
                          WorkBefore = ifelse(Occupation == "Je travaille (y compris si congé maladie, maternité ou parental)", 1, 0), #Dummy for whether the mother worked before being in maternity leaves
                          WorkPlanTo=AfterBirthOccupation=="Oui", # create a dummy for the mothers that want to work
                          WorkPlanNotTo=AfterBirthOccupation=="Non", # create a dummy for the mothers that don't want to work 
                          WorkNoPlan=AfterBirthOccupation %in% c("Oui","Non")==FALSE, # create a dummy for the mothers that don't know yet
                          AfterBirthOccupation=ifelse(AfterBirthOccupation %in% c("Oui","Non")==FALSE,"Ne sait pas",as.character(AfterBirthOccupation)), # recode missing values
                          ECSPlanTo=ECSYN=="Oui", # create a dummy for ppl that want to use ECS before preschool
                          ECSPlanNotTo=ECSYN=="Non", # create a dummy for ppl that don't want to use ECS before preschool
                          ECSNoPlan=ECSYN %in% c("Oui","Non")==FALSE, # create a dummy for ppl that don't know yet if they want to use ECS before preschool
                          NoPhoneNumber=is.na(NUMBER_1_TEXT), # Dummy of ppl that did not leave their phone number
                          FmilyEarnLessThan2500 = ifelse(FmlyIncome %in% c("- 500", "500-700", "700-1000", "1000-1500","1500-2000",  "2000-2500"), 1, 0), 
                          EarnLessThan1500 = ifelse(Income %in% c("- 500", "500-700", "700-1000", "1000-1500"), 1, 0), 
                          Intention1or0 = ifelse(Intention == "Yes", 1, 0)
)

#write_csv(data_tot_clean,"../Data/Full_dataset_PremiersPas_with_undocumented.csv")


CleanedDB <- data_tot_clean %>% 
          select(ResponseId,                                # ID
                 SurveyorId=surveyor_id,                    # Surveyor Id
                 StartDate,                                 # Date of the survey
                 MaternityWard=MAT,                         # Maternity Ward of survey
                 Dep=DEP,                                   # Departement
                 CodePostal=ZIPCODE_1_TEXT,                 # ZipCode
                 DateArrival,                               # Date of arrival in France of the mother
                 DateArrival2,                              # Date of arrival in France of the 2nd parent
                 AgeChild1:AgeChild8,                       # Other children age
                 SansPapier,                                # For filter: Undocumented migrant
                 NoPhoneNumber,                             # # For filter: No Phone -> Out of RCT.
                 BirthDate:ECSNoPlan,                       # All variables created before
                 AfterBirthOccupation,                      # Plans after Birth 
                 WorkBefore,                              # Dummy for whether the mother was working
                 BreastFeedIntend1or0, 
                 FmilyEarnLessThan2500, 
                 EarnLessThan1500,
                 Intention1or0, 
                 Occupation, 
                 Occupation2
         )

###### Remove unexperimental data #######

# 1. Recode non undocumented mother to replace NA 
CleanedDB$SansPapier <- replace(CleanedDB$SansPapier, is.na(CleanedDB$SansPapier), "Non") 

##### This DB constitute the baseline before matching it with the RCT Data
BaselineRCT <- CleanedDB %>% filter(SansPapier == "Non" & NoPhoneNumber==FALSE) # 2. Remove undocumented mothers and missing Phone Numbers

# Creation of subsamples of interest 
#Low-SES mothers
lowSES = CleanedDB %>% 
  filter(Educ == "Bac") 

highSES = CleanedDB %>% 
  filter(Educ == "Sup") 


#Migrant mothers
migrants = CleanedDB %>% 
  filter(FrenchYN == "Else") 

nonmigrants  = CleanedDB %>% 
  filter(FrenchYN == "French") 

# Seine Saint Denis 
SSD = CleanedDB %>% 
  filter(Dep == "93") 

nonSSD = CleanedDB %>% 
  filter(Dep != "93") 




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##
##### Import and clean the randomization data %%%%%%%%%%%##
#####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##

#```{r creation of a database that includes randomization data,  include= FALSE}

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##
##### Import the data from the waves to augment the database  %%%%%%%%%%%##
#####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##
#Import the data of the different waves into a Waves database

Wave1 <- fread("../pre-analysis-Laudine/Databases/Wave1.csv", encoding="UTF-8")
Wave2 <- fread("../pre-analysis-Laudine/Databases/Wave2.csv", encoding="UTF-8")
Wave3 <- fread("../pre-analysis-Laudine/Databases/Wave3.csv", encoding="UTF-8")
Wave4 <- fread("../pre-analysis-Laudine/Databases/Wave4.csv", encoding="UTF-8")
Wave5 <- fread("../pre-analysis-Laudine/Databases/Wave5.csv", encoding="UTF-8")
Wave6 <- fread("../pre-analysis-Laudine/Databases/Wave6.csv", encoding="UTF-8")
Wave7 <- fread("../pre-analysis-Laudine/Databases/Wave7.csv", encoding="UTF-8")

# Add a column "Which_wave" indicating from which wave the data comes
#TO DO: If one find time to do it: do it more elegantly 
Wave1$Which_wave <- "Wave1"
Wave2$Which_wave <- "Wave2"
Wave3$Which_wave <- "Wave3"
Wave4$Which_wave <- "Wave4"
Wave5$Which_wave <- "Wave5"
Wave6$Which_wave <- "Wave6"
Wave7$Which_wave <- "Wave7"

#Create  the Waves file that compile all Waves
Waves <- bind_rows(list(Wave1, Wave2, Wave3, Wave4, Wave5, Wave6, Wave7))


# change the language of the participant responseId R_0FOGPcEx6kj6SFn  that was coded arabic instead of French
Waves$LANGUAGE[Waves$ResponseId=="R_0FOGPcEx6kj6SFn"] <- "français"

# Duplicates 
# Some duplicates were created while doing the first randmisation, we keep their treament status and remove duplicates
DUP <- Waves %>% group_by(ResponseId,WhichPhone) %>% mutate(Dup=n()) %>% filter(Dup>1) %>% 
  filter((ResponseId=="R_jAVU7wqjdpxLPru" & Assignment=="T1")|
           (ResponseId=="R_neTcA1kLr8LlSyv"& Assignment=="T2" & COM=="Villecresnes")|
           (ResponseId=="R_arC2ASj8mir3Nb3"& Assignment=="Control")) %>% distinct() #%>% view()


Waves <- Waves %>% group_by(ResponseId,WhichPhone) %>% mutate(Dup=n()) %>% filter(Dup==1) %>% 
  bind_rows(.,DUP) %>% select(-Dup) %>% ungroup()



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##
##### Creation of an augmented dataframe: data_full %%%%%%%%%%%##
#####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##


#```{r create an augmented main database with randomization data, echo= FALSE}
#######-#######-#######-#######-#######-#######-#######-#######-#######-#######-
###### Creation of a dataset that keeps the best of both worlds ######-
#######-#######-#######-#######-#######-#######-#######-#######-#######-#######-

## In this chunk, we create a data_full data base that keeps both the final 
#cleaned data extracted from the questionnaire (data) AND the additional information 
#about the randomization contained in the Waves data base

#Indeed, some columns, as EDUC for instance are weird in the Waves data set, 
#so we just want to keep the additional information brought by Waves, but extract this additional information
#to augment the main data set: data

#As a reminder, Waves is the data from the randomization, containing mainly the Treatment groups of the participants
# What we want to do in this part is to add to our main database "data", the additional information contained in "Waves"
#### !!!!!! CAUTION : by doing so, we will remove the numbers of the fathers, because we want a one to one correspondence between households and rows

#For the export in excel, the database were put in long format by phone number for mothers and fathers (so that messages could be sent to both)
# To match the treatment variable with the questionnaire, we keep only the important variables and then pivot 
# the database in wide format again with a PhoneMother and PhoneFather column.

CleanWaves <- Waves %>% select(ResponseId,NUMCOM:Which_wave,Mail=Q121,-Dep) %>% mutate(Number=as.character(Number)) 

CleanWaves <- CleanWaves %>%  pivot_wider(id_cols=names(CleanWaves)[names(CleanWaves) %in% c("WhichPhone","Number")==FALSE], 
                                          names_from=WhichPhone,
                                          values_from = Number)

# Export the contact database 
writexl::write_xlsx(CleanWaves,"../Data/Contact_Data_PremiersPas.xslx")

# Remove identifying information 
CleanWaves <- CleanWaves %>% select(-c("Mail":"PhoneFather"),-"Prenom")

# Do the inner join
data_full <- BaselineRCT %>% left_join(.,CleanWaves, by = c("ResponseId"))


# Add a bloc variable Wave x Maternity Ward x Strata of randomisation 
data_full <- data_full %>%
  group_by(Which_wave, MaternityWard, strata) %>%
  mutate(
    group_id = cur_group_id(),  # Store the group IDs temporarily (optional)
    labels = paste(Which_wave, MaternityWard, strata, sep = ' x ')
  ) %>%
  ungroup() %>%
  mutate(blocksMatWaveStrata = factor(as.character(group_id), labels = unique(labels)))

# Add a bloc variable Maternity Ward x Id Wave identifier for descriptive Fixed Effects
data_full <- data_full %>%
  group_by(Which_wave, MaternityWard) %>%
  mutate(
    group_id = cur_group_id(),  # Store the group IDs temporarily (optional)
    labels = paste(Which_wave, MaternityWard, sep = 'x')
  ) %>%
  ungroup() %>%
  mutate(blocksMatWave = factor(as.character(group_id), labels = unique(labels)))

################################################################
################### OFFICIAL CODE STOPS HERE ###################
################################################################



# Check that there are no duplicates
if (sum(duplicated(data_full$ResponseId)) > 0) {
  
  
  print(which(duplicated(data_full$ResponseId)))
  
} else{
  
  print("No duplicates")
  
}

## 12 individuals were lost between recruitment and random assignment.
# We remove those out of the experiment.
data_full <- data_full %>% filter(!is.na(Assignment)) #%>% 
  
immo <- read_csv("../Data/Immobilier/full.csv")

immo <- immo %>% filter(!is.na(nombre_pieces_principales)) %>%
  filter(nature_mutation=="Vente") %>% 
  filter(type_local!="Local industriel. commercial ou assimilé") %>% 
  filter(nombre_pieces_principales>0)

feols(valeur_fonciere~as.factor(nombre_pieces_principales)+surface_terrain+I(surface_terrain^2)|code_commune,
      immo %>% filter(code_departement %in% c("75","91","92","93","94"))
      )


ImmoPrices <- immo %>% filter(code_departement %in% c("75","91","92","93","94")) %>% 
  group_by(code_postal,code_commune,code_departement,nom_commune) %>% 
  summarise(prixMoyen=mean(valeur_fonciere),
            PrixMedian=median(valeur_fonciere),
            surfaceReelleBati=mean(surface_reelle_bati,na.rm=TRUE),
            PrixMoyenM2Bati=mean(valeur_fonciere/surface_reelle_bati,na.rm=TRUE))

data_full <- left_join(data_full %>% mutate_at(vars(NUMCOM,CodePostal),~as.character(.)),ImmoPrices,by=c("COM"="nom_commune","CodePostal"="code_postal")) #%>% 
  #select(ResponseId,prixMoyen,PrixMedian,surfaceReelleBati,PrixMoyenM2Bati) %>% distinct(ResponseId,.keep_all = TRUE)

#data_full <- test %>% left_join(.,data_full)
#
#
write_csv(data_full,"../Data/BaselinePremiersPas.csv")
#
#library(FactoMineR)
#library(FactoInvestigate)
#
#factorsValues <- test %>% select(ValGenerosity:ValNone,KnownNbTypeECS,TAUXCOUV_COM) #%>%
#  mutate_at(vars(KnowsCrecheOnly,Knows0r1ECS))
#factorslikert <- test %>% select(LikertAccessInfo:LikertAccessSoc,) %>% mutate_all(.,~as.numeric(as.factor(.)))
#corrplot::corrplot(cor(as.matrix(factorsValues)))
#
#res.pca = PCA(bind_cols(factorsValues), scale.unit=TRUE, ncp=5,  graph=T) 
#
#res.pca = PCA(factorsValues, scale.unit=TRUE, ncp=4,  
#              graph=T)
#              
#
#Investigate(res.pca)
#
#
#corrplot::corrplot(cor(factorslikert))
#
#f2 <- fa(factors,2,rotation="varimax")   		#factor analyze the resulting item
#
##(f2)                                              #show the result
#load=loadings(f2)
#print(load,sort=TRUE,digits=2,cutoff=0.01)  #show the loadings       
#plot(load)                                 #plot factor 1 by 2
#identify(load,labels=names(msq))           
##put names of selected points onto the figure  -- to stop, click with command key
#plot(f2,labels=names(msq))
#
#
#
#
#
#
#
#
#
#
#
#
#
### compute the age of the baby now for each observation
#pregnancy_length <- 280
#
#
#
### Recoding other predictors
#
#### Whether ppl think that subsidies are avaiblable for ECS
#data_tot_clean$ECS_Sub_YN <-   fct_collapse(data_tot_clean$ECS_Subsidies,
#                                        Yes = "Oui", 
#                                        No_NSP = c("Je ne sais pas", "Non"))
#
## Whether they perceive them as accessible (getting a place)
#data_tot_clean$Access1_binary <-   fct_collapse(data_tot_clean$ACCESSBILITY1,
#                                            Difficult_Access = c("assez difficile", "impossible", "très difficile", "quasiment impossible" ), 
#                                            Not_difficult = c("assez facile", "très facile"), 
#                                            `Dont know` = c("je ne sais pas", "je ne souhaite pas répondre"))
#
## Whether they perceive them as accessible (getting a place)
#data_tot_clean$Access2_binary <-   fct_collapse(data_tot_clean$ACCESSIBILITY2,
#                                                Accessible = c("Oui", "Non, mais on prendra un autre mode de garde en plus"), 
#                                                Not_accessible = c("Non"), 
#                                                `Dont know` = c("Je ne sais pas encore", "Je ne souhaite pas répondre"))
#
## Collapse trust in crèche into 3 levels
#data_tot_clean$Trust_creche <-   fct_collapse(data_tot_clean$TRUST_3,
#                                               Agree = c("Assez d’accord", "Très d’accord"),
#                                               `Dont agree` = c("Très en désaccord", "Assez en désaccord"), 
#                                               `Dont know` = c("Je ne sais pas", "Pas petinent"))
#
#
## Whether they would fit their working hours
#
#
#### Create ECS_Date that is a variable for people wanting to use ECS in September 2023 or before, and those wanting to use ECS after that
#
## Create an empty binary variable
#data_tot_clean$ECS_Date_binary <- NA
#
## Define the cutoff date
#cutoff_date <- as.Date("2023-09-02")
#
## Set values for ECS dates before and after the cutoff
#data_tot_clean$ECS_Date_binary[data_tot_clean$ECSDate < cutoff_date] <- "Before 09.2023"
#data_tot_clean$ECS_Date_binary[data_tot_clean$ECSDate >= cutoff_date] <- "After 09.2023"
#
## Convert the binary variable to a factor
##data_tot_clean$ECS_Date_binary <- factor(data_tot_clean$ECS_Date_binary, levels = c("Before December 2023", "After December 2023"))
#
##head(data_tot_clean$ECS_Date_binary)
### Create a "data" dataset that contains only the valid rows (ie. those with phone numbers)
#data <- data_tot_clean %>% 
#  drop_na(c("NUMBER_1_TEXT"))
#
#
#data$SansPapier <- replace(data$SansPapier, is.na(data$SansPapier), "Non")
#
## SANS PAPIER: TO read if working on the intervention
#data <- subset(data, data$SansPapier == "Non")
#
## Creation of subsamples of interest 
##Low-SES mothers
#lowSES = data_tot_clean %>% 
#  filter(EDUC == "Bac") 
#
#highSES = data_tot_clean %>% 
#  filter(EDUC == "Sup") 
#
#
##Migrant mothers
#migrants = data_tot_clean %>% 
#  filter(FrenchYN == "Else") 
#
#nonmigrants  = data_tot_clean %>% 
#  filter(FrenchYN == "French") 
#
## Seine Saint Denis 
#SSD = data_tot_clean %>% 
#  filter(DEP == "93") 
#
#nonSSD = data_tot_clean %>% 
#  filter(DEP != "93") 
#
#
### Creation of inactive mothers
### TO DO : inclure dnas le code intial 
#
#data_tot_clean$Active <-   fct_collapse(data_tot_clean$OCCUPATION,
#                                        Active = c("Je travaille (y compris si congé maladie, maternité ou parental)", "Je suis étudiant(e), stagiaire ou fait une formation"), 
#                                        Inactive = c("Je suis au chômage, cherche un emploi", "Je suis femme au foyer", "Autre", "J’ai une pension de retraite, d’invalidité, etc."), 
#                                        NSP = c("Ne souhaite pas répondre", "Ne sait pas"))
#
#
#
#inactive = data_tot_clean %>%  
#  filter(Active == "Inactive") 
#
#active = data_tot_clean %>%  
#  filter(Active == "Active") 
#
#
## Rename AGE_KIDS_ for after
#names(data_tot_clean)[str_detect(names(data_tot_clean),"AGE_KIDS")] <- paste("AgeChild",c(1:8),sep="")
#
#test <- data_tot_clean %>% mutate(
#                                  SingleMum=PARTNER %in% c("Non, et je ne suis pas en couple","Je ne sais pas",
#                                                           "Je ne souhaite pas répondre"),
#                                  CoupleCohabiting= PARTNER=="Oui",
#                                  Couple=PARTNER %in% c("Oui", "Non, mais je suis en couple"),
#                                  Relationship=case_when(Couple==TRUE~"Couple",
#                                                         CoupleCohabiting==TRUE~"Couple non cohabitant",
#                                                         SingleMum==TRUE~"Famille monoparentale"),
#                                  BirthPlace=case_when(BIRTHPLACE_11_TEXT=="Afghanistan"~"MENA (Maghreb +Arabie saoudite, etc.)",
#                                               BIRTHPLACE_11_TEXT=="Serbie"~"Europe/Amérique du Nord, Australie",
#                                               BIRTHPLACE_11_TEXT=="Haiti"|BIRTHPLACE_11_TEXT=="Haïti"~"Afrique subsaharienne",
#                                               !is.na(BIRTHPLACE)~BIRTHPLACE
#                                               ),
#                                 BirthPlace2=case_when(BIRTHPLACE2_11_TEXT=="Afghanistan"~"MENA (Maghreb +Arabie saoudite, etc.)",
#                                                       BIRTHPLACE2_11_TEXT=="Serbie"~"Europe/Amérique du Nord, Australie",
#                                                       BIRTHPLACE2_11_TEXT=="Haiti"|BIRTHPLACE2_11_TEXT=="Haïti"|
#                                                       BIRTHPLACE2_11_TEXT=="comores"|BIRTHPLACE2_11_TEXT=="Comores"~"Afrique subsaharienne",
#                                                       BIRTHPLACE2_11_TEXT=="Israel"~"Europe/Amérique du Nord, Australie",
#                                                      !is.na(BIRTHPLACE2)~BIRTHPLACE2),
#                                 DateArrival_16_TEXT=case_when(DateArrival_16_TEXT=="201"~"2010",
#                                                               DateArrival_16_TEXT=="19996"~"1996",
#                                                               DateArrival_16_TEXT=="20002"~"2002",
#                                                               DateArrival_16_TEXT=="20111"~"2011",
#                                                               DateArrival_16_TEXT=="20120"~"2012"),
#                                 DateArrival_16_TEXT=as.numeric(DateArrival_16_TEXT),
#                                 DateArrival=case_when(
#                                             DateArrival_16_TEXT>=2018|DateArrival=="Je ne sais pas"~"Moins de 5 ans", #Je ne sais pas: person recently arrived
#                                             DateArrival_16_TEXT<2018 &
#                                             DateArrival_16_TEXT>=2013 ~"Entre 5 et 10 ans",
#                                             DateArrival_16_TEXT<2013 &
#                                               DateArrival_16_TEXT>=2003 ~"Entre 10 et 20 ans",
#                                             DateArrival_16_TEXT<=2003 ~ "Il y a plus de 20 ans",
#                                             DateArrival!="Année"~ DateArrival,
#                                             ),
#                                  DateArrival2_16_TEXT=ifelse(DateArrival2_16_TEXT=="201","2010",DateArrival2_16_TEXT),
#                                  DateArrival2_16_TEXT=as.numeric(DateArrival2_16_TEXT),
#                                  DateArrival2=case_when(
#                                    DateArrival2_16_TEXT>=2018~"Moins de 5 ans", 
#                                    DateArrival2_16_TEXT<2018 &
#                                    DateArrival2_16_TEXT>=2013 ~"Entre 5 et 10 ans",
#                                    DateArrival2_16_TEXT<2013 &
#                                    DateArrival2_16_TEXT>=2003 ~"Entre 10 et 20 ans",
#                                    DateArrival2_16_TEXT<=2003 ~ "Il y a plus de 20 ans",
#                                    DateArrival2=="Je ne sais pas"|DateArrival2=="Je ne souhaite pas répondre"~"Manquant",
#                                    DateArrival2!="Année"~ DateArrival2,
#                                  ),
#                                 OrderedEduc2=case_when(DIPLOMA2=="Ne souhaite pas répondre"|
#                                                        DIPLOMA2=="Ne sait pas"|
#                                                        DIPLOMA2=="Autre. Préciser :"~"Missing",
#                                                        !is.na(DIPLOMA2)~DIPLOMA2,
#                                                        TRUE~"No Partner"),
#                                 OrderedEduc2=factor(OrderedEduc2, levels = c(
#                                                                      "N’est jamais allé à l’école (6-8 ans)", "CEP(Certificat d’Etudes Primaires) / fin école primaire (9-11 ans)",
#                                   "BEPC, Brevet élémentaire, Brevet des collèges / fin collège (13-16 ans)",
#                                   "CAP, BEP",
#                                   "Bac / fin lycée (17-19 ans)",
#                                   "Bac +2 (Deug, BTS, DUT) (19-20 ans)",
#                                   "Bac +3  / fin d’un diplôme de premier cycle universitaire (21-22 ans)",
#                                   "Bac +4 ou +5 (2nd cycle universitaire, ingénieur, grande école…) (22-24 ans)",
#                                   "Plus de Bac +5 (24-26 ans)","Missing","No Partner"), ordered = TRUE, labels = 
#                                     c("No school", "9-11", "Brevet", "CAP", "Bac", "Bac +2", "Bac + 3", "Bac + 4/5", "+ Bac+5","Missing","No Partner")),        
#                          Occupation=ifelse(
#                                        OCCUPATION %in% c("Autre",
#                                                          "Ne sait pas",
#                                                          "Ne souhaite pas répondre",
#                                                          "J’ai une pension de retraite, d’invalidité, etc."),"Autre",as.character(OCCUPATION)),
#                          Occupation2=case_when(
#                            OCCUPATION2_6_TEXT=="Maitre de conference"~"travaille (y compris si congé maladie, maternité ou parental)",
#                            OCCUPATION2_6_TEXT=="Reconversion"~"est au chômage, cherche un emploi",
#                            OCCUPATION2 %in% c("Ne sait pas","Ne souhaite pas réponndre","a une pension de retraite, d’invalidité, etc.")~"Autre",
#                            !is.na(OCCUPATION2)~OCCUPATION2,
#                            TRUE~"No Partner"
#                          ),
#                          WorkPlanTo=AfterBirthOccupation=="Oui",
#                          WorkPlanNotTo=AfterBirthOccupation=="Non",
#                          WorkNoPlan=AfterBirthOccupation %in% c("Oui","Non")==FALSE,
#                          AfterBirthOccupation=ifelse(AfterBirthOccupation %in% c("Oui","Non")==FALSE,"Ne sait pas",as.character(AfterBirthOccupation)),
#                          ECSPlanTo=ECSYN=="Oui",
#                          ECSPlanNotTo=ECSYN=="Non",
#                          ECSNoPlan=ECSYN %in% c("Oui","Non")==FALSE, 
#                          AgeSup32 = ifelse(Age > 32, "Yes", "No")
#                          )
#                                 
##)
#        
#
#
#
#test <- data_tot_clean %>% 
#  select(
#    ResponseId,
#    ZipCode = ZIPCODE_1_TEXT,                                                  # Zip code
#    StartDate_D,StartDate_T,
#    Language=LANGUAGE,
#    BirthPlace,BirthPlace2,DateArrival,DateArrival2,
#    Age=AGE,
#    OrderedEduc=ordered_educ, OrderedEduc2,
#    FmlyIncome=FMLYINCOME, 
#    Income=INCOME, 
#    Occupation,Occupation2,WorkPlanTo,WorkPlanNotTo,WorkNoPlan,AfterBirthOccupation,
#    FrenchYN, 
#    PregBinary=PREG_BINARY,
#    Active,
#    Partner=PARTNER,
#    Nchildren=NBR_KIDS_4_TEXT,
#    AgeChild1:AgeChild8,
#    ECSPlanTo,ECSPlanNotTo,ECSNoPlan,
#    ECSDate,
#    ECS_1:ECS_11,
#    CrechesMunicipales=Crèches_municipales,
#    Haltes,
#    SingleECSCreche=Single_ECS_Creches,
#    OnlyOneECS=Only_One_ECS,
#    NumECS=Num_ECS,
#    PregnancyDaysLeft=Pregnancy_Days_Left,
#    BabyAge=baby_age,
#    ECSTypeR=ECS_TypeR,
#    ECSSubYN=ECS_Sub_YN,
#    Access2Binary=Access2_binary,
#    TrustCreche=Trust_creche,
#    ECSDateBinary=ECS_Date_binary,
#  )
#
#
#
#
#
#
##```
#
#
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##
###### Import and clean the randomization data %%%%%%%%%%%##
######%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##
#
##```{r creation of a database that includes randomization data,  include= FALSE}
#
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##
###### Import the data from the waves to augment the database  %%%%%%%%%%%##
######%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##
##Import the data of the different waves into a Waves database
#
#Wave1 <- fread("../pre-analysis-Laudine/Databases/Wave1.csv", encoding="UTF-8")
#Wave2 <- fread("../pre-analysis-Laudine/Databases/Wave2.csv", encoding="UTF-8")
#Wave3 <- fread("../pre-analysis-Laudine/Databases/Wave3.csv", encoding="UTF-8")
#Wave4 <- fread("../pre-analysis-Laudine/Databases/Wave4.csv", encoding="UTF-8")
#Wave5 <- fread("../pre-analysis-Laudine/Databases/Wave5.csv", encoding="UTF-8")
#Wave6 <- fread("../pre-analysis-Laudine/Databases/Wave6.csv", encoding="UTF-8")
#Wave7 <- fread("../pre-analysis-Laudine/Databases/Wave7.csv", encoding="UTF-8")
#
## Add a column "Which_wave" indicating from which wave the data comes
##TO DO: If one find time to do it: do it more elegantly 
#Wave1$Which_wave <- "Wave1"
#Wave2$Which_wave <- "Wave2"
#Wave3$Which_wave <- "Wave3"
#Wave4$Which_wave <- "Wave4"
#Wave5$Which_wave <- "Wave5"
#Wave6$Which_wave <- "Wave6"
#Wave7$Which_wave <- "Wave7"
#
##Create  the Waves file that compile all Waves
#Waves <- bind_rows(list(Wave1, Wave2, Wave3, Wave4, Wave5, Wave6, Wave7))
#
#
## change the language of the participant responseId R_0FOGPcEx6kj6SFn  that was coded arabic instead of French
#Waves$LANGUAGE[Waves$ResponseId=="R_0FOGPcEx6kj6SFn"] <- "français"
#
## Duplicates 
#
#DUP <- Waves %>% group_by(ResponseId,WhichPhone) %>% mutate(Dup=n()) %>% filter(Dup>1) %>% 
#  filter((ResponseId=="R_jAVU7wqjdpxLPru" & Assignment=="T1")|
#         (ResponseId=="R_neTcA1kLr8LlSyv"& Assignment=="T2" & COM=="Villecresnes")|
#         (ResponseId=="R_arC2ASj8mir3Nb3"& Assignment=="Control")) %>% distinct() #%>% view()
#
#
#Waves <- Waves %>% group_by(ResponseId,WhichPhone) %>% mutate(Dup=n()) %>% filter(Dup==1) %>% 
#  bind_rows(.,DUP) %>% select(-Dup) %>% ungroup()
#
##```
#
#
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##
###### Creation of an augmented dataframe: data_full %%%%%%%%%%%##
######%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%##
#
#
##```{r create an augmented main database with randomization data, echo= FALSE}
########-#######-#######-#######-#######-#######-#######-#######-#######-#######-
####### Creation of a dataset that keeps the best of both worlds ######-
########-#######-#######-#######-#######-#######-#######-#######-#######-#######-
#
### In this chunk, we create a data_full data base that keeps both the final 
##cleaned data extracted from the questionnaire (data) AND the additional information 
##about the randomization contained in the Waves data base
#
##Indeed, some columns, as EDUC for instance are weird in the Waves data set, 
##so we just want to keep the additional information brought by Waves, but extract this additional information
##to augment the main data set: data
#
##As a reminder, Waves is the data from the randomization, containing mainly the Treatment groups of the participants
## What we want to do in this part is to add to our main database "data", the additional information contained in "Waves"
##### !!!!!! CAUTION : by doing so, we will remove the numbers of the fathers, because we want a one to one correspondence between households and rows
#
##For the export in excel, the database were put in long format by phone number for mothers and fathers (so that messages could be sent to both)
## To match the treatment variable with the questionnaire, we keep only the important variables and then pivot 
## the database in wide format again with a PhoneMother and PhoneFather column.
#
#CleanWaves <- Waves %>% select(ResponseId,NUMCOM:Which_wave,Mail=Q121) %>% mutate(Number=as.character(Number)) 
#
#CleanWaves <- CleanWaves %>%  pivot_wider(id_cols=names(CleanWaves)[names(CleanWaves) %in% c("WhichPhone","Number")==FALSE], 
#              names_from=WhichPhone,
#              values_from = Number)
#
## Export the contact database 
#writexl::write_xlsx(CleanWaves,"../Data/Contact_Data_PremiersPas.xslx")
#
#CleanWaves <- CleanWaves %>% select(-c("Mail":"PhoneFather"),-"Prenom")
#
## Do the inner join
#data_full <- data %>% left_join(.,CleanWaves, by = c("ResponseId"))
#
#
#
## Check that there are no duplicates
#if (sum(duplicated(data_full$ResponseId)) > 0) {
#  
#  
#  print(which(duplicated(data_full$ResponseId)))
#  
#} else{
#  
#  print("No duplicates")
#  
#}
#
### 12 individuals were lost between recruitment and random assignment.
## We remove those out of the experiment.
#data_full <- data_full %>% filter(!is.na(Assignment)) %>% 
#  
#
#
#
##Write the file with full data
##writexl::write_xlsx(data_full, "../Data/all_waves_rand/All_participants_full.xlsx", col_names = TRUE)
#
#rm(list=setdiff(ls(),c("data_full","select","summarize","cbbPalette","cbPalette")))
#
## Table Contact
#Contact <- data_full %>% 
#
#
#
###### Cleaning
#
#
#PremiersPas <- data_full 
#
#
#
#
#
#
#
#
#
#
##```
#
##```{r cleaning of the data_full database, echo = FALSE}
########-#######-#######-#######-#######-#######-#######-#######-
########- Cleaning the data_full database #######-#######-#
########-#######-#######-#######-#######-#######-#######-#######-#
#
### In this chunk, we clean the most complete data base up to date: data_full, that includes all variables from the questionnaire but also data from the randomization
#
##convert VARIABLES INTO FACTORS
##TO DO: to continue
#names <- c('ID', 'Assignment', 'Which_wave'
#)
#
#data_full[,names] <- lapply(data_full[,names] , factor)
#
##convert VARIABLES INTO numerical ones
##TO DO: to continue
#names_numerical <- c("TAUXCOUV_COM" )
#data_full[,names_numerical] <- lapply(data_full[,names_numerical] , as.numeric)
#



#```



# ----



# ---- FirstGraph ----


