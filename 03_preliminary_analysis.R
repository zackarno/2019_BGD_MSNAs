
#preliminary analysis done  by 5 September.
#health, nutrition, food security/livelihoods, protection.

#based on HH characterstics - secondary analysis.
## (gender_hoh, hh_size, age_groups) ---> November

## MSNI- MSNA focal points to finishi bilateral discussions with sectors 28 Aug.
## agreeing on 1-6 scale
## decide om individual thesholds of need per indicator (2 or 3 classes)
## sectoral combination tables done.

# By 28 AUG- CONTACT HQ ABOUT THE NEW 1-6 SCALE
# SECTORAL LSGS BY 8 SEPT.
# 14 SEPT. MET WITH SECTORS WITH RESULTS
# 14,15 RUN MSNI
# 24 SEPT - JOINT ANALYSIS WORKSHOP- MSNI DONE


rm(list=ls())
population<-c("Host","Refugee")[2]
data_process<-c("checking", "cleaning", "analysis")[3]
# write_output<-c("yes","no")[1]

library(dplyr)
library(GISutils)
# detach("package:butteR", unload = TRUE)
detach("package:sp", unload = TRUE)

library(nngeo)
library(dplyr)
# remove.packages(pkgs = "GISutils")
# remove.packages(pkgs = "raster")
# remove.packages(pkgs = "sp")
# library(sp)
library(hypegrammaR)
library(koboquest)
library(stringr)
library(lubridate)
library(rgdal)
library(sf)
library(anytime)
library(srvyr)
library(forcats)
# source("Functions/kobo_factorize.R")
source("Functions/ActivatePaths.R")
source("Functions/make_composite_indicators_bgd_msna_2019.R")

# source("Functions/remove_concat_select_multiple.R")


HH_kobo_questions<-read.csv(survey_path,stringsAsFactors = FALSE)
HH_kobo_choices<-read.csv(choices_path, stringsAsFactors = FALSE)
HH<-read.csv(HH_path, stringsAsFactors = FALSE, na.strings=c("", " ", NA))
Indiv<-read.csv(Indiv_path, stringsAsFactors = FALSE, na.strings=c("", " ", NA))

dap1<- read.csv("Inputs/DAPs/MSNA_DAP_BasicAnalysis_simple_18Sept.csv", stringsAsFactors = FALSE, na.strings=c("", " ", NA))
dap2<-read.csv("Inputs/DAPs/MSNA_DAP_BasicAnalysis_SUBSET_ind_gender_18Sept.csv", stringsAsFactors = FALSE, na.strings=c("", " ", NA))



pop<- read.csv(pop_path, stringsAsFactors = FALSE, na.strings=c("", " ", NA))

if(population=="Refugee"){
  dap1_hh<-dap1 %>% 
    filter(dataset %in% c("ref_only", "both")) %>% 
    filter(level=="household")
  dap1_indiv<-dap1 %>% 
    filter(dataset %in% c("ref_only", "both")) %>% 
    filter(level=="individual")
  
  dap2_indiv <-dap2 %>% 
    filter(dataset %in% c("ref_only", "both")) 
  }

if(population=="Host"){
  dap1_hh<-dap1 %>% 
    filter(dataset %in% c("host_only", "both")) %>% 
    filter(level=="household")
  dap1_indiv<-dap1 %>% 
    filter(dataset %in% c("host_only", "both")) %>% 
    filter(level=="individual")
  
  dap2_indiv <-dap2%>% 
    filter(dataset %in% c("host_only", "both")) 
}




# FILTER YES CONSENT AND MAKE COMPOSITE INDICATORS ------------------------

HH_yes_consent<- HH %>% filter(informed_consent=="yes")
Indiv<-Indiv %>% filter(X_submission__uuid %in% HH_yes_consent$X_uuid)

composite_indicators<-make_composite_indicators_bgd_msna_2019(hh_data = HH_yes_consent,  individual_data = indiv_yes_consent,population = population)

HH_with_composite<-HH_yes_consent %>% left_join(composite_indicators$household_composites,by="X_uuid")
Indiv_with_composite<-Indiv %>% left_join(composite_indicators$individual_composites,by=c("X_submission__uuid"))

HH_with_composite<-butteR::remove_concat_select_multiple(HH_with_composite,questionnaire = HH_kobo_questionnaire)

HH_data_factorized<-butteR::questionnaire_factorize_categorical(HH_with_composite,questionnaire = HH_kobo_questionnaire,return_full_data = TRUE)

HH_data_factorized<-HH_data_factorized %>%  #REMOVE COLUMNS THAT ARE ALL NAS
  select_if(~!all(is.na(.)))

# CLEAN POPULATION DATA ---------------------------------------------------

if (population=="Refugee") {
  
  pop_cleaned<-pop %>% 
    filter(!is.na(Camp)& is.na(Block)) %>% 
    mutate(
      !!(sf_strata):=str_replace(Camp, "Total","") %>% trimws(),
      Total.Families=as.numeric(Total.Families)
    ) %>% 
    select({sf_strata}, Total.Families,Total.Individuals)}
# else{ 
  # pop[[sf_strata]]<-if_else(pop[[sf_strata]] =="Teknaf Sadar" , "Teknaf",pop[[sf_strata]]) 
  # }

# spatial cleaning should not be necessary on final data set --------------
if(data_process=="Checking"){ 



HH_spatial<-st_as_sf(HH, coords= long_lat, crs=sf::st_crs(strata_spatial))
boundary_gdb<- "Inputs/GIS"
strata_spatial<-sf::st_read(boundary_gdb,"190310_Outline_Rohingya_Refugee_Camp_A1", stringsAsFactors=FALSE)
strata_spatial_utm<- sf::st_transform(strata_spatial, 32646)
HH_spatial<-sf::st_as_sf(HH,coords= long_lat, crs=sf::st_crs(strata_spatial))
HH_spatial_utm<- sf::st_transform(HH_spatial, 32646)
HH_spatial<-sf::st_join(HH_spatial_utm,strata_spatial_utm, st_is_within_distance, dist=10)
HH_cleaned<-HH_spatial %>% as.data.frame() %>% 
  select(camp_id=New_Camp_N, everything())
  } else { HH_cleaned<- HH_data_factorized}

#  which(HH_spatial$New_Camp_N %>% unique() %in% pop_cleaned$camp_id==FALSE)
#  (HH_spatial$New_Camp_N %>% unique())[which(HH_spatial$New_Camp_N %>% unique() %in% pop_cleaned$camp_id==FALSE)]



# WEIGHT AND CREATE SVY DESIGN OBJECTS ------------------------------------

#REMOVE STRATA THAT WERE NOT YET ASSESSED


# HH_cleaned<-HH_cleaned %>%
#   group_by(!!sym(strata)) %>%
#   filter(n()>10) %>%
#   ungroup()

HH_cleaned %>% 
  group_by(!!sym(strata)) %>% 
  count()


  
# weighting<-map_to_weighting(sampling.frame = pop, 
#                             data.stratum.column = "camp_id",
#                             data = HH_cleaned, 
#                             sampling.frame.population.column ="Total.Families",
#                             sampling.frame.stratum.column = "camp_id")



weighting<-map_to_weighting(sampling.frame = pop_cleaned, 
                            data.stratum.column = strata,
                            data = HH_cleaned, 
                            sampling.frame.population.column =sf_pop,
                            sampling.frame.stratum.column = sf_strata)



HH_cleaned$camp_name %>% unique() %>% length()
HH_cleaned %>% group_by(camp_name) %>% count()
HH_cleaned[[strata]][which(HH_cleaned[[strata]] %in%pop[[sf_strata]] ==FALSE)]
pop_cleaned[[sf_strata]][which(pop_cleaned[[sf_strata]] %in% HH_cleaned[[strata]]==FALSE)]
#MAKE DESIGN OBJECTS
HH_svy_ob<-map_to_design(data=HH_cleaned, weighting_function = weighting)


Indiv_with_relevant_HH_data<-Indiv_with_composite %>% left_join(HH_svy_ob$variables %>% 
                      mutate(weights= weights(HH_svy_ob)) %>% 
                      select(X_uuid,{strata},respondent_gender,weights), by=c("X_submission__uuid"="X_uuid"))

ind_svy_ob<-survey::svydesign(ids = ~ 1,
                  strata =  ~ camp_name,
                  weights= ~weights,
                  data = Indiv_with_relevant_HH_data)






# run dap -----------------------------------------------------------------

variables_to_analyze<-dap1_hh$variable

basic_analysis_overall_nas_subset_with_ci<-butteR::mean_proportion_table(design = HH_svy_ob, 
                                                                         list_of_variables = variables_to_analyze,
                                                                         aggregation_level = NULL,
                                                                         round_to = 2,return_confidence = TRUE,na_replace = FALSE)
basic_analysis_overall_nas_subset<-butteR::mean_proportion_table(design = HH_svy_ob, 
                                                                         list_of_variables = variables_to_analyze,
                                                                         aggregation_level = NULL,
                                                                         round_to = 2,return_confidence = FALSE,na_replace = FALSE)
basic_analysis_by_camp_nas_subset_with_ci<-butteR::mean_proportion_table(design = HH_svy_ob, 
                                                list_of_variables = variables_to_analyze,
                                                aggregation_level = "camp_name",
                                                round_to = 2,return_confidence = TRUE,na_replace = FALSE)
basic_analysis_by_camp_nas_subset<-butteR::mean_proportion_table(design = HH_svy_ob, 
                                                                         list_of_variables = variables_to_analyze,
                                                                         aggregation_level = "camp_name",
                                                                         round_to = 2,return_confidence = FALSE,na_replace = FALSE)
basic_analysis_by_resp_gender_nas_subset<-butteR::mean_proportion_table(design = HH_svy_ob, 
                                                                 list_of_variables = variables_to_analyze,
                                                                 aggregation_level = "respondent_gender",
                                                                 round_to = 2,return_confidence = FALSE,na_replace = FALSE)
basic_analysis_by_resp_gender_nas_subset_with_ci<-butteR::mean_proportion_table(design = HH_svy_ob, 
                                                                        list_of_variables = variables_to_analyze,
                                                                        aggregation_level = "respondent_gender",
                                                                        round_to = 2,return_confidence = TRUE,na_replace = FALSE)

variables_to_analyze<-dap1_indiv$variable %>% trimws()
Indiv_with_relevant_HH_data
variables_to_analyze[which(variables_to_analyze %in% (Indiv_with_relevant_HH_data %>%  colnames())==FALSE)] %>% dput()



variables_to_analyze_good<-c(variables_to_analyze[which(variables_to_analyze %in% (Indiv_with_relevant_HH_data %>%  colnames()))],"ind_why_notreatment.treatment_expensive") 

ind_svy_ob$variables<- butteR::questionnaire_factorize_categorical(data = ind_svy_ob$variables, questionnaire = HH_kobo_questionnaire,return_full_data = TRUE)

sapply(ind_svy_ob$variables, class)

asdf<-butteR::mean_proportion_table(design = ind_svy_ob, 
                              list_of_variables = variables_to_analyze_good,
                              aggregation_level = NULL,
                              round_to = 2,return_confidence = TRUE,na_replace = FALSE)



debugonce(butteR::mean_proportion_table)

# output weights for Mark -------------------------------------------------


HH_svy_ob$variables$weights<-HH_svy_ob %>% weights()
HH_svy_ob$variables %>% nrow()
weight_table<-HH_svy_ob$variables %>%
  group_by(!!sym(strata)) %>%
  summarise(weights=unique(weights),
            number_per_union=n()) %>% data.frame() 
# write.csv(weight_table, "Outputs/HC_MSNA2019_Weights_17sept2019.csv")
# bla<-srvyr::as_survey(HH_svy_ob)


# REMOVE VARIABLES NOT NEEEDE FOR ANALYSIS --------------------------------


remove_other<-HH_svy_ob$variables %>% select(ends_with("_other")) %>% colnames()
HH_svy_ob$variables$I.HH_CHAR.education_level.HH
HH_svy_ob$variables$I.FSL.host_community_food_assistance_only.HH

problem_composites<-c("I.FSL.host_community_food_assistance_only.HH")

cols_to_scrap<-c("X_id", "X_uuid", "X_submission_time", "X_validation_status", "referral_ID","end_note", "X", "survey_date", "survey_start", "deviceid", "end_survey", strata,
"instance_name", "audit", "enumerator_id", "camp_name", "intro_text","informed_consent","datearrival_bgd", "datearrival_shelter","cooking_fuel_other",remove_other ,problem_composites)

analyze_these<-HH_svy_ob$variables[ , -which(names(HH_svy_ob$variables) %in% cols_to_scrap)] %>% colnames()


# RUN BUTTER --------------------------------------------------------------
HH_svy_ob$variables$upazilla_name
gimme<-butteR::mean_proportion_table(HH_svy_ob,analyze_these,aggregation_level = c("upazilla_name","I.HH_CHAR.gender_hoh.HH"),return_confidence = TRUE,na_replace = TRUE)



aggregation_level = NULL
setdiff(analyze_these, aggregation_level) %>% length()
analyze_these %>% length()
HH_svy_ob$variables$I.HH_CHAR.gender_hoh.HH

# debugonce(butteR::return_select_multiple_groups)
asdf<-butteR::return_select_multiple_groups(data=HH_svy_ob, questionnaire = HH_kobo_questionnaire)











#TRY RUNNING ANALYSIS ON ALL DATA 
############################################################################################################
#JUST REPLACE CATEGORICAL VARIABLES IN DATA SET WTIH KOBO FACOTRIZED CATEGORICAL
HH_categorical_data_kobo_factorized<-GISutils::kobo_factorize_categorical(HH_cleaned,HH_kobo_questionnaire,return_full_data = TRUE)

#REMOVE WEIRD COLUMNS LIKE UUID, AND OTHER INDEXES
HH_cleaned_trimmed<-HH_categorical_data_kobo_factorized %>%
  #REMOVE COLUMNS THAT ARE GREATER 95% DISTINCT VALUES
  select_if(~n_distinct(.)<(0.95*nrow(HH_cleaned))) %>% 
  #REMOVE COLUMNS THAT ARE ALL NAS
  select_if(~!all(is.na(.))) %>%  
  #REMOVE A FEW MORE
  select(-deviceid, -enumerator_id,-X_submission_time) 

HH_cleaned_trimmed<-HH_cleaned_trimmed %>% 
  group_by(camp_id) %>% 
  filter(n()>10)

#MAKE DESIGN OBJECT
HH_svy_full<-map_to_design(data=HH_cleaned_trimmed, weighting_function = weighting)

#REMOVE THE FEW REMAINING COLUMNS THAT WILL CAUSE A PROBLEM


HH_svy_full$variables<- GISutils::remove_concat_select_multiple(data = HH_svy_full$variables,
                                                                questionnaire = HH_kobo_questionnaire
                                                                  )

HH_svy_full$variables
#go to srvyr script

columns_to_analyze<- HH_svy_full$variables %>% 
  dplyr::select(-starts_with("rank"),-camp_name, -camp_id) %>% colnames()

HH_svy_full$variables<-HH_svy_full$variables %>% 
  mutate_if(sapply(HH_svy_full$variables, is.character),as.factor)

HH_svy_full$variables %>% 
  select(hoh_gender,breastfeeding_women) %>% 
  summarise_all(mean(.,na.rm=TRUE))
svyexp %>% 
  select(hoh_gender,breastfeeding_women) %>% 
  summarise_all(survey_mean(.,na.rm=TRUE, proportion=TRUE))

HH$hoh_gender
length(columns_to_analyze)
debugonce(meanprop_improved)
# library(gtools)
library(srvyr)
?srvyr::all_vars()
?srvyr::survey_mean
svyexp %>% class()
svyexp[["tbl_svy"]]<-as_survey(HH_svy_full)
srvyr::survey_mean(.svy=as_survey( HH_svy_full), 
                   x = columns_to_analyze[50])
 %>% srvyr::all_varsl(
  survey_mean(.)
)

for(i in 1: length(columns_to_analyze[50:70])){
  colname<-
  
}
svyexp %>% 
  summarise(asdf=survey_mean(improvement.improve_access, proportion=TRUE,vartype="ci")
  )

as_survey(HH_svy_full) %>% 
  summarise(asdf=survey_mean(improvement.improve_access, proportion=TRUE,vartype="ci")
            )


asdf<-meanprop_improved(data = HH_svy_full,
                variable_to_analyze = columns_to_analyze[50:70],by_strata1 = "camp_id", by_strata2 = "Upazila",
                multiple_combined_dissagregations = "hoh_gender", na.action = FALSE 
                  )

suckit<-GISutils::MeanPropSurvey2(data = HH_svy_full,
                         variable_to_analyze = columns_to_analyze[50:70],by_strata1 = "camp_id", by_strata2 = "Upazila",
                         multiple_combined_dissagregations = "hoh_gender", na.action = FALSE 
)
suckit[[1]]
asdf[[1]][1:nrow(asdf[[1]]),1:10]
asdf[[1]]
asdfs<-MeanPropSurvey(data =HH_svy_full,
                      variable_to_analyze =  columns_to_analyze,
                      strata1 = "camp_id",
                      strata2="Upazila",by_variables ="hoh_gender", na.action = FALSE )


?map_to_summary_table
?map_to_summary_statistic
?map_to_case

zsumstats<-function(dependent.var.names){
  rtable<-list()
  mycase<-map_to_case(hypothesis.type = "direct_reporting",dependent.var.type = "categorical")  
  for ( i in 1: length(dependent.var.names)){
    variable_of_interest<-dependent.var.names[i]
    rtable[[i]]<-map_to_summary_statistic(design = HH_svy_full, dependent.var= variable_of_interest, case = mycase)
  }}
suckit<-zsumstats(columns_to_analyze[50:70])
suckit
mean_with_confints("hoh_gender", design=HH_svy_full)
percent_with_confints_select_one("wood_access.adult_female",design=HH_svy_full)
percent_with_confints_select_one_groups(dependent.var="hoh_gender", independent.var = "camp_id",design =HH_svy_full)
HH_kobo_questionnaire$question_is_select_multiple("wood_access.adult_female")
HH_kobo_questionnaire$question_is_categorical ("wood_access.adult_female")
#TESTS
#############################


# HH_svy_categorical$variables$shelter_purchased_reason2<-HH_svy_categorical$variables$shelter_purchased_reason %>% as.character()
t1<-svymean(formula("~shelter_purchased_reason"), HH_svy_categorical,na.rm=TRUE, var.test= "ci",
            level=0.95)
t2<-survey::svyciprop(formula("~shelter_purchased_reason2"), HH_svy_categorical,na.rm=TRUE, vartype= "ci")
confint(t1)
t1df<-t1 %>% data.frame()
t1df %>% 
  mutate(
    l_ci=mean-1.96*SE,
    h_ci=mean+1.96*SE
  )
sd
#############################
#RUN ANALYSIS ON ONLY CATEGORICAL
###########################################################################################################

#REMOVE THE FEW REMAINING COLUMNS THAT WILL CAUSE A PROBLEM
columns_to_analyze<- HH_svy_categorical$variables %>% 
  select(-starts_with("rank"),-camp_name, -camp_id) %>% colnames()




#should i reinvent mean prop svy?
inspect_list<-list()
inspect_list2<-list()

for ( i in 1:length(columns_to_analyze)){
  print(i)
  variable_of_interest<-columns_to_analyze[i]
  print(variable_of_interest)
  svy_formula<-paste0("~",variable_of_interest)
  svy_by_formula<-"~camp_id"
  print(svy_formula)
  inspect_list[[i]]<-svymean(formula(svy_formula),HH_svy_categorical, na.rm=TRUE,keepvar=TRUE, vartype= "ci")
  inspect_list2[[i]]<-svyby(formula(svy_formula),
                            by=formula(svy_by_formula),HH_svy_categorical,svymean, na.rm=TRUE,keepvar=TRUE, vartype= "ci")
  #TO GET CI'S SHOULD GATHER AND THEN SPREAD THE DATA
}

inspect_list
inspect_list2[[1]] %>% colnames()

#THE ABOVE WORKS WELL-- LETS SEE IF WE CAN DO THE WHOLE DATA SET NOT JUST CATEGORICAL
###########################################################################################################
#categorical correlation matrix
##############################################################
data_cat<- GISutils::kobo_factorize_categorical(return_full_data = FALSE, 
                                                      data = HH_svy_full$variables, questionnaire = HH_kobo_questionnaire)

library(factoextra)
library(FactoMineR)

res.mca <- MCA(data_cat, graph = FALSE)

asdf<-fviz_mca_biplot(res.mca, 
                repel = TRUE, # Avoid text overlapping (slow if many point)
                ggtheme = theme_minimal())
windows();asdf
library(psych)
library(polycor)
library(ca)
plot(ca(data_cat))
?polychoric
poly_values<- polychoric(data_cat)
items_polychoric = poly_values$rho

poly.example <- cor.ci(sim.poly(nvar = 10,n = 100)$items,n.iter = 10,poly = TRUE)
poly.example
print(corr.test(poly.example$rho), short=FALSE)  

#should i reinvent mean prop svy?
inspect_list<-list()
inspect_list2<-list()
suckit<-list()

for ( i in 1: length(columns_to_analyze)){
  print(i)
  variable_of_interest<-columns_to_analyze[i]
  suckit[[i]]<-HH_svy_full$variables %>% group_by(camp_id) %>% n_distinct({variable_of_interest})
  print(variable_of_interest)
  svy_formula<-paste0("~",variable_of_interest)
  svy_by_formula<-"~camp_id"
  print(svy_formula)
  if (length(levels(HH_svy_full$variables[,variable_of_interest]))<1){
    print(paste0(variable_of_interest, " has < 1 level: Skipping"))
    
  }
  if (length(levels(HH_svy_full$variables[,variable_of_interest]))>1){
  inspect_list[[i]]<-svymean(formula(svy_formula),
                             HH_svy_full, 
                             na.rm=TRUE,keepvar=TRUE)
  
  inspect_list2[[i]]<-svyby(formula(svy_formula),
                            by=formula(svy_by_formula),
                            HH_svy_full,
                            svymean, 
                            # na.rm=TRUE,
                            # na.rm.all = TRUE, 
                            # na.rm.by = TRUE,
                            keepvar=TRUE, 
                            vartype= "ci")
  #TO GET CI'S SHOULD GATHER AND THEN SPREAD THE DATA
}}

suckit[[6]]
HH_svy_full$variables$cooking_fuel_other

svymean(formula(svy_formula),HH_svy_full, vartype="ci")
svyby(formula(svy_formula), by=formula(svy_by_formula),HH_svy_full,
      svymean, 
      na.rm.by = TRUE, 
      na.rm=TRUE,
      na.rm.all = TRUE,
      keepvar=TRUE, 
      vartype= "ci")




# HH_spatial<-sf::st_join(HH_spatial, strata_spatial)
# nn<-st_is_within_distance(HH_spatial_utm, strata_spatial_utm,dist = 10)
# HH_spatial_nn<-nngeo::st_nn(x = HH_spatial, strata_spatial)
# HH_spatial %>% filter(is.na(New_Camp_N.x)) %>% nrow()
# HH_cleaned<-HH_spatial %>% as.data.frame() %>% 
#   mutate(
#     camp_id=if_else(is.na(New_Camp_N.x), New_Camp_N.y, New_Camp_N.x)
#   ) %>% 
#   select(camp_id,everything(), -(New_Camp_N.x:geometry))
