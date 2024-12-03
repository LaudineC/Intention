

# ---- Load_and_clean ----

# Cleaned code stops line 644

### WARNING: There are to features to undo depending on the anaylses you want to run 
# Line X: if you work on the intervention, remove miscarriages
# Line X: if you work on the intervention, exclude undocumented mothers
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#######-##-
##### Import and clean the raw survey database %%%%%%%%%%%##
#####%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%#######-###-

##Import data_full, the baseline dataset
data_full <- read_csv("../Data/BaselinePremiersPas.csv")


##Import data_tot_clean with excluded participants (the ones that were undocumented or did not leave their phone number)
data_tot_clean <- read_csv("../Data/Full_dataset_PremiersPas_with_undocumented.csv") 



#convert relevant VARIABLES INTO FACTORS
##NB: pour ajouter les stats sur les heures, prendre le dataset du 24/10 et ajouter "'HOUR_1','HOUR_2','HOUR_3','HOUR_4',"
names <- c("ResponseId", "SurveyorId", "MaternityWard", "Dep", "CodePostal", "DateArrival",
           "DateArrival2", "SansPapier", "Relationship", "Language", "Preg7Less", "SmokePreg", 
           "BreastFeedIntend", "OrderedEduc", "Educ", "Act3", "FmlyIncome", "Income", "ECSType7", "FrenchYN", 
           "Intention.x", "ECSUsed", "ECSNeed", "ECSIdealAgeEntry", "AffordSubsidies", "AffordNMCostWB", "AffordOppCost", 
           "AffordNMCostCareer", "DiscountRate", "AccessEasy", "AccessHours", "AccessInformalCare", "Norms", "NormsOpposedDK",
           "LikertAccessInfo", "LikertReturnHK", "LikertAccessSoc", "TrustFam", "TrustFriend", "TrustAssMat", "TrustCreche", 
           "DeprivClothes", "DeprivFood", "DeprivBills", "DeprivHolidays", "DeprivHousing", "InformalReciprocity", 
           "LevelComprehensionExPost", "LevelExpressionExPost", "LevelInfoSubExPost", "LevelPartnerAnsweredExPost", 
           "ComputerYN", "AgeSup32", "BirthPlace", "BirthPlace2", "OrderedEduc2", "Occupation", "Occupation2", 
           "AfterBirthOccupation", "NUMCOM", "COM", "REG", "HighLowECEC", "Educ2", "Intention.y", "UsedECEC", "IntendUse",
           "Assignment", "Which_wave", "labels", "blocksMatWaveStrata", "blocksMatWave", "code_commune", "code_departement"
           
)

data_full[,names] <- lapply(data_full[,names] , factor)

#convert relevant VARIABLES INTO numerical ones


names_numerical <- c("AgeChild1", "AgeChild2", "AgeChild3", "AgeChild4", "AgeChild5", "AgeChild6", "AgeChild7", "AgeChild8",
                     "Age", "KnowsCreche", "KnowsCrecheA", "KnowsCrechePub", "KnowsAssMat", "KnowsNotYet", "KnowsJE", "KnowsAutre",
                     "KnowsPartage", "KnowsHG", "KnowsNounou", "KnowsNothing", "WantCreche", "WantCrecheA", "WantAssMat", "WantJE", 
                     "WantAutre", "WantPartage", "WantHG", "WantNounou", "KnownNbTypeECS", "PregMonth", "BreastFeedLength",
                     "BreastFeedIntend1or0", "AffordSubsidies1or0", "NormsOpposedN", "LikertReturnHK1or0", "TrustCreche1or0",
                     "AffordCost", "NumberChildren", "AccessImpossible", "AccessHard", "AccessHardtoImpossible", "AccessEasy1or0", 
                     "AccessEasyDK", "AccessNotHours", "AccessNotInformalCare", "AccessInformalCareAMAP", "ValGenerosity", 
                     "ValIndependence", "ValSocialisation", "ValObedience", "ValEducation", "ValDK", "ValImagination", "ValNone", 
                     "WorkBefore", "FmilyEarnLessThan2500", "EarnLessThan1500", "Intention1or0", "CODDEP", "CODREG", "PTOT", 
                     "TAUXCOUV_COM", "TAUXCOUV_EPCI", "TAUXCOUV_DEP", "TAUXCOUV_NAT", "n", "strata", "group_id", "prixMoyen", 
                     "PrixMedian", "surfaceReelleBati", "PrixMoyenM2Bati")
data_full[,names_numerical] <- lapply(data_full[,names_numerical] , as.numeric)

# Order relevant variables


data_full <- data_full %>%  mutate(
         OrderedEduc=factor(OrderedEduc, levels =
             c("No school", "9-11", "Brevet", "CAP", "Bac", "Bac +2", "Bac + 3", "Bac + 4/5", "+ Bac+5"), ordered = TRUE),
         Income = factor(Income, 
                         levels = c("NSP","- 500","500-700", "1000-1500", 
                                    "1500-2000", "2000-2500", "2500-3000", "3000-4500", "4500-5000",  "5000-6000", "Ne veut pas répondre" ), ordered = TRUE),
         FmlyIncome = factor(FmlyIncome, 
                             levels =c("NSP", "- 500", "500-700", "700-1000", "1000-1500", 
                                       "1500-2000", "2000-2500", "2500-3000", "3000-4500", "4500-5000",
                                       "5000-6000", "6000-7000", "7000-8000", "+8000", "Ne veut pas répondre"), ordered = TRUE),
          AffordNMCostWBOrdered=factor(AffordNMCostWB,
                               levels=c("Négatif",  "Je ne sais pas encore","Pas d'impact","Positif"), ordered = TRUE # would staying at home have a positive, 
         ),                                                                   # negative or null impact on mother's well being
         AffordOppCost=factor(AffordOppCost,
                              levels=c("Négatif",  "Je ne sais pas encore", "Pas d'impact", "Positif"), ordered = TRUE # would staying at home have a positive, 
         ),                                                                   # negative or null impact on household's income
         AffordNMCostCareer=factor(AffordNMCostCareer, # would staying at home have a positive, 
                                   levels=c("Négatif", "Pas d'impact", "Je ne sais pas encore","Positif"), ordered = TRUE # negative or null impact on mother's career
         ),
         AccessEasy=factor(AccessEasy,levels =c("impossible","quasiment impossible", # Whether accessing a slot in ECS is seen as easy
                                                   "très difficile","assez difficile",
                                                   "je ne sais pas","assez facile",
                                                   "très facile"),ordered = TRUE), 
         AccessHours=factor(AccessHours,levels =c("Oui", # Whether ECS opening hours fit parents working hours
                                                     "Non, mais on prendra un autre mode de garde en plus","Je ne sais pas encore",
                                                     "Non"),ordered = TRUE),
         AccessInformalCare=factor(AccessInformalCare,levels =c("Oui, autant que nécessaire", # Whether accessing informal care is feasible
                                                          "Oui, de temps en temps","Oui, pour dépanner / très ponctuellement",
                                                          "Je ne sais pas encore",
                                                          "Non"),ordered = TRUE),
         Norms=factor(Norms,
                      levels=c("Ne sait pas","Aucune","Une minorité","Moitié moitié","La plupart","Toutes"), ordered = TRUE # how many mothers in their surroundings use childcare 
         ),
     LikertAccessInfo=factor(LikertAccessInfo, # Is it feasible to access information about childcare
                            levels=c("Très en désaccord","Assez en désaccord","Je ne sais pas","Assez d’accord","Très d’accord"), ordered = TRUE), # strongly disagree, disagree, don't know, agree, strongly agree
    LikertReturnHK=factor(LikertReturnHK, # believe in childcare returns on investment: is a child that attended childcare better developed in preschool?
                          levels=c("Très en désaccord","Assez en désaccord","Je ne sais pas","Assez d’accord","Très d’accord") , ordered = TRUE), # strongly disagree, disagree, don't know, agree, strongly agree
    LikertAccessSoc=factor(LikertAccessSoc, # Does the mother have someone that can help with paperwork if needed
                           levels=c("Très en désaccord","Assez en désaccord","Je ne sais pas","Assez d’accord","Très d’accord"), ordered = TRUE), # strongly disagree, disagree, don't know, agree, strongly agree
    TrustFam=factor(TrustFam, # TRUST: Do the parents trust their close family (parents, brother, sister, etc.) to take care of the baby
                    levels=c("Très en désaccord","Assez en désaccord","Je ne sais pas","Assez d’accord","Très d’accord"),  ordered = TRUE
    ),
    TrustFriend=factor(TrustFriend, # TRUST: Do the parents trust their friends to take care of the baby
                       levels=c("Très en désaccord","Assez en désaccord","Je ne sais pas","Assez d’accord","Très d’accord"), ordered = TRUE
    ),
    TrustAssMat=factor(TrustAssMat, # TRUST: Do the parents trust a private childminder to take care of the baby
                       levels=c("Très en désaccord","Assez en désaccord","Je ne sais pas","Assez d’accord","Très d’accord"), ordered = TRUE
    ),
    TrustCreche=factor(TrustCreche, # TRUST: Do the parents trust their family to take care of the baby
                       levels=c("Très en désaccord","Assez en désaccord","Je ne sais pas","Assez d’accord","Très d’accord"), ordered = TRUE
    ),
    DeprivClothes =factor(DeprivClothes,levels=c("Pas du tout","Un peu","Beaucoup","Je ne souhaite pas répondre"), ordered = TRUE), # DEPRIVATION: do parents struggle to buy clothes?
    DeprivFood    =factor(DeprivFood,levels=c("Pas du tout","Un peu","Beaucoup","Je ne souhaite pas répondre"), ordered = TRUE), # DEPRIVATION: do parents struggle to buy food?
    DeprivBills   =factor(DeprivBills,levels=c("Pas du tout","Un peu","Beaucoup","Je ne souhaite pas répondre"), ordered = TRUE), # DEPRIVATION: do parents struggle to pay for the bills?
    DeprivHolidays=factor(DeprivHolidays,levels=c("Pas du tout","Un peu","Beaucoup","Je ne souhaite pas répondre"), ordered = TRUE), # DEPRIVATION: do parents struggle to go in holidays?
    DeprivHousing =factor(DeprivHousing,levels=c("Pas du tout","Un peu","Beaucoup","Je ne souhaite pas répondre"), ordered = TRUE), # DEPRIVATION: do parents struggle to pay for housing?
    InformalReciprocity = factor(InformalReciprocity, 
                                 levels = c( "Non", " Je ne souhaite pas répondre", "Oui, pour dépanner / très ponctuellement",
                                              "Oui, de temps en temps", " Oui, très souvent"), ordered = TRUE), # Do the parents already take care of the children of others
OrderedEduc2=factor(OrderedEduc2, levels  = c(
  "No school", "9-11", "Brevet", "CAP", "Bac", "Bac +2", "Bac + 3", "Bac + 4/5", "+ Bac+5","Missing","No Partner"), ordered = TRUE),        

)





#```



# ----



# ---- FirstGraph ----


