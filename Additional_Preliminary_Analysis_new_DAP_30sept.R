


rm(list=ls())
user<-c("zack", "mehedi")[1]
population<-c("Host","Refugee")[2]
data_process<-c("checking", "cleaning", "analysis")[3]
analysis_phase<-c("basic","relationship_testing")[1]
# write_output<-c("yes","no")[1]

library(dplyr)
library(hypegrammaR)
library(koboquest)
library(stringr)
library(lubridate)
library(rgdal)
library(sf)
library(anytime)
library(srvyr)
library(forcats)
library(gtools)
# library(nngeo)

source("Functions/ActivatePaths.R")
# source("Functions/make_composite_indicators_bgd_msna_2019.R")
# source("Functions/make_composite_indicators_bgd_msna_2019_mk.R")
source("Functions/make_composite_indicators_bgd_msna_2019_mk2.R")


# LOAD IN DATA AND ODK TOOLS ----------------------------------------------
HH<-read.csv(HH_path, stringsAsFactors = FALSE, na.strings=c("", " ", NA))
Indiv<-read.csv(Indiv_path, stringsAsFactors = FALSE, na.strings=c("", " ", NA))

HH_kobo_questions<-read.csv(survey_path,stringsAsFactors = FALSE)
HH_kobo_choices<-read.csv(choices_path, stringsAsFactors = FALSE)
HH_kobo_questionnaire<-koboquest::load_questionnaire(HH,questions = HH_kobo_questions,choices = HH_kobo_choices, choices.label.column.to.use = "label..english")

Indiv_kobo_questionnaire<-koboquest::load_questionnaire(Indiv,questions = HH_kobo_questions,choices = HH_kobo_choices, choices.label.column.to.use = "label..english")

#LOAD DAP
dap<- read.csv("Inputs/DAPs/MSNA_DAP_BasicAnalysis_All_Changes_Together_30SepMM.csv", stringsAsFactors = FALSE, na.strings=c("", " ", NA))


#LOAD POPULATION DATA
pop<- read.csv(pop_path, stringsAsFactors = FALSE, na.strings=c("", " ", NA))


# SPLIT DAP INTO DIFFERNT TYPES OF ANLAYSES -------------------------------

dap_break_downs<-list()
dap_break_downs$dap_basic_hh<-dap %>%
  filter(is.na(disaggregation)) %>% 
  filter(dataset %in% c(dap_population_name,"both")) %>% 
  filter(level=="household")

dap_break_downs$dap_basic_indiv<-dap %>%
  filter(is.na(disaggregation)) %>% 
  filter(dataset %in% c(dap_population_name, "both")) %>% 
  filter(level=="individual")

dap_break_downs$dap_subsets_hh <-dap %>%
  filter(!is.na(subset.1)) %>% 
  filter(dataset %in% c(dap_population_name, "both")) %>%
  filter(level=="household") %>% 
  group_by(subset.1)

dap_break_downs$dap_subsets_indiv <-dap %>%
  filter(!is.na(subset.1)) %>% 
  filter(dataset %in% c(dap_population_name, "both")) %>%
  filter(level=="individual") %>% 
  group_by(subset.1)

dap_break_downs$dap_dissagregations_idiv<-dap %>% 
  filter(!is.na(disaggregation)) %>% 
  filter(dataset %in% c(dap_population_name, "both")) %>%
  filter(level=="individual")
dap_break_downs$dap_composite_hh<-dap %>% 
  filter(!is.na(subset.2)) %>% 
  filter(dataset %in% c(dap_population_name, "both")) %>%
  filter(level=="household")

dap_break_downs$dap_composite_indiv<-dap %>% 
  filter(!is.na(subset.2)) %>% 
  filter(dataset %in% c(dap_population_name, "both")) %>%
  filter(level=="individual")

dap_break_downs$for_na_replace<-dap %>% 
  filter(!is.na(na_replace)) %>% 
  filter(dataset %in% c(dap_population_name, "both"))
# dap %>% filter(!is.na(na_replace))

# dap %>%
#   filter(!is.na(na_replace))


# FILTER YES CONSENT AND MAKE COMPOSITE INDICATORS ------------------------

HH_yes_consent<- HH %>% filter(informed_consent=="yes")
Indiv<-Indiv %>% filter(X_submission__uuid %in% HH_yes_consent$X_uuid)


#RUN COMPOSITE INDICATOR SCRIPT- THANKS MEHEDI!
composite_indicators<-make_composite_indicators_bgd_msna_2019(hh_data = HH_yes_consent,  individual_data = Indiv,population = population)
# JOIN COMPOSITE INDICATORS TO APPROPRIATE DATA SETS
HH_with_composite<-HH_yes_consent %>% left_join(composite_indicators$household_composites,by="X_uuid")



Indiv_with_composite<-Indiv %>% left_join(composite_indicators$individual_composites,by="X_index")



HH_data_factorized<-HH_with_composite

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
if(population=="Host"){
  pop_cleaned<-pop
  pop_cleaned[[sf_strata]]<-if_else(pop_cleaned[[sf_strata]] =="Teknaf Sadar" , "Teknaf",pop_cleaned[[sf_strata]])
}

weighting<-map_to_weighting(sampling.frame = pop_cleaned, 
                            data.stratum.column = strata,
                            data = HH_data_factorized, 
                            sampling.frame.population.column =sf_pop,
                            sampling.frame.stratum.column = sf_strata)


# HH_data_factorized %>% group_by(!!sym(strata)) %>% count()


#MAKE DESIGN OBJECTS
HH_svy_ob<-map_to_design(data=HH_data_factorized, weighting_function = weighting)


Indiv_with_relevant_HH_data<-Indiv_with_composite %>% left_join(HH_svy_ob$variables %>% 
                                                                  mutate(weights= weights(HH_svy_ob)) %>% 
                                                                  select(X_uuid,{strata},respondent_gender,weights), by=c("X_submission__uuid"="X_uuid"))

ind_svy_ob<-survey::svydesign(ids = ~ 1,
                              strata =  formula(paste0("~",strata)),
                              weights= ~weights,
                              data = Indiv_with_relevant_HH_data)





#recode for boda counts
#rank columns
rank_columns<-HH_svy_ob$variables %>% select(starts_with("rank")) %>% colnames() %>% dput()

rank_cols_recoded<-sapply(HH_svy_ob$variables[,rank_columns], function(x) ifelse(x==1,3, ifelse(3,1,x))) %>% data.frame()
rank_cols_recoded<-sapply(rank_cols_recoded, function(x)ifelse(is.na(x),0,x)) %>% data.frame()
colnames(rank_cols_recoded)<- paste0(rank_columns, "_recoded")
HH_svy_ob$variables<-data.frame(HH_svy_ob$variables, rank_cols_recoded)


variables_to_analyze<-dap_break_downs$dap_basic_hh$variable  %>% trimws()

#extract variables to analyze from DAP and add any that were forgotten in DAP
variables_to_analyze<-c(variables_to_analyze,
                        HH_svy_ob$variables %>% select(starts_with("hh_coping_mechanism."),
                                                       starts_with("debt_reason."),
                                                       starts_with("unsafe_reason_female.")) %>% colnames(),colnames(rank_cols_recoded))

#remove this one
variables_to_analyze<-variables_to_analyze[variables_to_analyze!="I.HH_CHAR.childheaded_households.HH"]


# analyze data ------------------------------------------------------------

basic_analysis_without_cis<-list()
basic_analysis_without_cis[["overall"]]<-butteR::mean_proportion_table(design = HH_svy_ob, 
                                                                       list_of_variables = variables_to_analyze,
                                                                       aggregation_level = NULL,
                                                                       round_to = 2,return_confidence = FALSE,na_replace = FALSE,
                                                                       questionnaire = HH_kobo_questionnaire)
variables_to_analyze[1]
basic_analysis_without_cis[["by_strata"]]<-butteR::mean_proportion_table(design = HH_svy_ob, 
                                                                         list_of_variables = variables_to_analyze,
                                                                         aggregation_level = strata,
                                                                         round_to = 2,return_confidence = FALSE,na_replace = FALSE,
                                                                         questionnaire = HH_kobo_questionnaire)

basic_analysis_without_cis[["by_respondent_gender"]]<-butteR::mean_proportion_table(design = HH_svy_ob, 
                                                                                    list_of_variables = variables_to_analyze,
                                                                                    aggregation_level = "respondent_gender",
                                                                                    round_to = 2,return_confidence = FALSE,na_replace = FALSE,
                                                                                    questionnaire = HH_kobo_questionnaire)


#write output
if ( write_output=="yes"){
  for(i in 1:length(basic_analysis_without_cis)){
    type_of_analysis<-names(basic_analysis_without_cis)[i]
    date_for_title<-stringr::str_replace_all(Sys.Date(),"-","_")
    name_of_file<-paste0(date_for_title,"_",population,"_HH_DAP_Simple_",type_of_analysis,"_", ".csv")
    write.csv(basic_analysis_without_cis[[type_of_analysis]],paste0("Outputs/",name_of_file) )
  }
  
  
}



# na replace analysis -----------------------------------------------------

#specify variables where NA is not a true NA
variables_to_analyze<-dap_break_downs$for_na_replace$variable %>% trimws()

#forgot one variable in the DAP
if (population=="Refugee"){
  variables_to_analyze<-c(variables_to_analyze, HH_svy_ob$variables %>% select(starts_with("debt_reason.")) %>% colnames())
  
}

basic_analysis_without_cis<-list()
basic_analysis_without_cis[["overall_na_replace"]]<-butteR::mean_proportion_table(design = HH_svy_ob, 
                                                                       list_of_variables = variables_to_analyze,
                                                                       aggregation_level = NULL,
                                                                       round_to = 2,return_confidence = FALSE,na_replace = TRUE,
                                                                       questionnaire = HH_kobo_questionnaire)

basic_analysis_without_cis[["by_strata_na_replace"]]<-butteR::mean_proportion_table(design = HH_svy_ob, 
                                                                         list_of_variables = variables_to_analyze,
                                                                         aggregation_level = strata,
                                                                         round_to = 2,return_confidence = FALSE,na_replace = TRUE,
                                                                         questionnaire = HH_kobo_questionnaire)

basic_analysis_without_cis[["by_respondent_gender_na_replace"]]<-butteR::mean_proportion_table(design = HH_svy_ob, 
                                                                                    list_of_variables = variables_to_analyze,
                                                                                    aggregation_level = "respondent_gender",
                                                                                    round_to = 2,return_confidence = FALSE,na_replace = TRUE,
                                                                                    questionnaire = HH_kobo_questionnaire)

if(write_output=="yes"){
  for(i in 1:length(basic_analysis_without_cis)){
    type_of_analysis<-names(basic_analysis_without_cis)[i]
    date_for_title<-stringr::str_replace_all(Sys.Date(),"-","_")
    name_of_file<-paste0(date_for_title,"_",population,"_HH_DAP_Simple_",type_of_analysis,"_", ".csv")
    write.csv(basic_analysis_without_cis[[type_of_analysis]],paste0("Outputs/",name_of_file) )
  }
}



# Indivdual Level Analysis ------------------------------------------------

# DAP 1

#indiv vars to anlyze simply
variables_to_analyze<-dap_break_downs$dap_basic_indiv$variable %>% trimws() %>% unique()

#indiv vars to analyze by ind_gender
analyze_by_ind_gender<-dap_break_downs$dap_dissagregations_idiv$variable %>% trimws()

basic_analysis_without_cis<-list()

basic_analysis_without_cis[["overall"]]<-butteR::mean_proportion_table(design = ind_svy_ob, 
                                                                       list_of_variables = variables_to_analyze,
                                                                       aggregation_level = NULL,
                                                                       round_to = 2,return_confidence = FALSE,na_replace = FALSE,
                                                                       questionnaire = Indiv_kobo_questionnaire)


basic_analysis_without_cis[["by_strata"]]<-butteR::mean_proportion_table(design = ind_svy_ob, 
                                                                         list_of_variables = variables_to_analyze,
                                                                         aggregation_level = strata,
                                                                         round_to = 2,return_confidence = FALSE,na_replace = FALSE,
                                                                         questionnaire = Indiv_kobo_questionnaire)

basic_analysis_without_cis[["by_respondent_gender"]]<-butteR::mean_proportion_table(design = ind_svy_ob, 
                                                                                    list_of_variables = variables_to_analyze,
                                                                                    aggregation_level = "respondent_gender",
                                                                                    round_to = 2,return_confidence = FALSE,na_replace = FALSE,
                                                                                    questionnaire = Indiv_kobo_questionnaire)

basic_analysis_without_cis[["by_ind_gender"]]<-butteR::mean_proportion_table(design = ind_svy_ob, 
                                                                                    list_of_variables = analyze_by_ind_gender,
                                                                                    aggregation_level = "ind_gender",
                                                                                    round_to = 2,return_confidence = FALSE,na_replace = FALSE,
                                                                                    questionnaire = Indiv_kobo_questionnaire)

if (write_outut=="yes"){
  for(i in 1:length(basic_analysis_without_cis)){
    type_of_analysis<-names(basic_analysis_without_cis)[i]
    date_for_title<-stringr::str_replace_all(Sys.Date(),"-","_")
    name_of_file<-paste0(date_for_title,"_",population,"_Indiv_DAP_Simple_",type_of_analysis,"_", ".csv")
    write.csv(basic_analysis_without_cis[[type_of_analysis]],paste0("Outputs/",name_of_file) )
  }
}



# INDIVIDUAL SUBSET BY EDUCATION AGE GROUPS -------------------------------

# single subsets ----------------------------------------------------------
ind_svy_ob$variables<-ind_svy_ob$variables %>% 
  mutate(ind_gender=if_else(ind_gender=="male", "male", "female", missing=NULL))

dap_break_downs$dap_single_subsets_indiv<-dap_break_downs$dap_subsets_indiv %>% 
  mutate(variable_subset.1= paste0(variable , "_&_",subset.1)) %>% 
  distinct(.,variable_subset.1, .keep_all = TRUE)

subset_groups<-split(dap_break_downs$dap_single_subsets_indiv,dap_break_downs$dap_single_subsets_indiv$subset.1)

# list_of_surveys_by_subset %>% names()
analyzed_overall<-list()
analyzed_by_camp<-list()
analyzed_by_respondent_gender<-list()
overall_analysis<-list()
by_strata<-list()
by_resp_gender<-list()
# overall_binded<-list()
names(subset_groups)
#dont want to subset each one by everything-- only variable by variable
for (i in 1: length(subset_groups)){
  print(paste0 ("i =" ,i))
  subset_in_DAP<-subset_groups[[i]]
  print("wtf")
  name_of_subset_in_DAP<-names(subset_groups)[i]
  print("hello")
  ind_svy_ob$variables$I.EDU.age_group_education.INDV
  variables_to_analyze<-subset_in_DAP$variable %>% trimws()
  list_of_surveys_by_subset<-split(ind_svy_ob$variables, ind_svy_ob$variables[[name_of_subset_in_DAP]])
  
  print("ASD")
  # list_of_subsets[lapply(list_of_subsets, length) > 0]
  for(j in 1 : length(list_of_surveys_by_subset)){
    name_of_subset_survey<-names(list_of_surveys_by_subset)[j]
    
    print(paste0 ("GROUP =",subset_in_DAP," ",name_of_subset_survey))
    new_design<-survey::svydesign(ids = ~ 1,
                                  strata = formula(paste0("~",strata)),
                                  weights= ~weights,
                                  data = list_of_surveys_by_subset[[name_of_subset_survey]])
    # analyzed_overall[[subset_group]]
    analyzed_overall[[paste0(name_of_subset_in_DAP,"_", name_of_subset_survey)]]<-  butteR::mean_proportion_table(design = new_design,
                                                                                                                  list_of_variables = variables_to_analyze,
                                                                                                                  aggregation_level = NULL,
                                                                                                                  round_to = 2,return_confidence = FALSE,na_replace = FALSE, questionnaire = Indiv_kobo_questionnaire)
    # analyzed_by_camp[[subset_group]]
    analyzed_by_camp[[paste0(name_of_subset_in_DAP,"_", name_of_subset_survey)]]<-  butteR::mean_proportion_table(design = new_design,
                                                                                                                  list_of_variables = variables_to_analyze,
                                                                                                                  aggregation_level = strata,
                                                                                                                  round_to = 2,return_confidence = FALSE,na_replace = FALSE,questionnaire = Indiv_kobo_questionnaire)
    # analyzed_by_respondent_gender[[subset_group]]
    analyzed_by_respondent_gender[[paste0(name_of_subset_in_DAP,"_", name_of_subset_survey)]]<-  butteR::mean_proportion_table(design = new_design,
                                                                                                                               list_of_variables = variables_to_analyze,
                                                                                                                               aggregation_level = "respondent_gender",
                                                                                                                               round_to = 2,return_confidence = FALSE,na_replace = FALSE,questionnaire = Indiv_kobo_questionnaire)
    
    # date_for_title<-stringr::str_replace_all(Sys.Date(),"-","_")
  }
  overall_analysis[[name_of_subset_in_DAP]]<-analyzed_overall
  by_strata[[name_of_subset_in_DAP]]<-analyzed_by_camp
  by_resp_gender[[name_of_subset_in_DAP]]<-analyzed_by_respondent_gender
}


subsets_together_overall<-do.call("smartbind", overall_analysis$ind_gender) 
subsets_together_by_strata<-do.call("smartbind", sapply(by_strata$ind_gender, as.data.frame))
subsets_together_by_resp_gender<-do.call("smartbind", sapply(by_resp_gender$ind_gender, as.data.frame))



if (population=="Host"){
  write.csv(subsets_together_overall,"Outputs/2019_10_02_Host_Single_Subsets_Overall.csv")
  write.csv(subsets_together_by_strata,"Outputs/2019_10_02_Host_Single_Subsets_By_Strata.csv")
  write.csv(subsets_together_by_resp_gender,"Outputs/2019_10_02_Host_Single_Subsets_By_Resp_gender.csv")
}
if(population=="Refugee"){
  write.csv(subsets_together_overall,"Outputs/2019_10_02_Refugee_Single_Subsets_Overall.csv")
  write.csv(subsets_together_by_strata,"Outputs/2019_10_02_Refugee_Single_Subsets_By_Strata.csv")
  write.csv(subsets_together_by_resp_gender,"Outputs/2019_10_02_Refugee_Single_Subsets_By_Resp_gender.csv")
  
}

# Double Subsets ----------------------------------------------------------



dap_break_downs$dap_double_subsets_indiv<-dap_break_downs$dap_subsets_indiv %>% 
  filter(!is.na(subset.2)) %>% 
  mutate(variable_subset.1= paste0(variable , "_&_",subset.1),
         variable_subset.1_subset.2=paste0(variable, "_&_", subset.1,"_&_", subset.2)) 
  

subset_groups1<-split(dap_break_downs$dap_double_subsets_indiv,dap_break_downs$dap_double_subsets_indiv$subset.1)

analyzed_overall<-list()
analyzed_by_camp<-list()
analyzed_by_respondent_gender<-list()
overall_analysis<-list()
by_strata<-list()
by_resp_gender<-list()
names(subset_groups1)

for (i in 1: length(subset_groups1)){
  print(paste0 ("i =" ,i))
  subset_in_DAP<-subset_groups1[[i]]
  name_of_subset_in_DAP<-names(subset_groups1)[i]
  print("yoshi")
  ind_svy_ob$variables$double_subset<-paste0(ind_svy_ob$variables[[name_of_subset_in_DAP]],"_&_", ind_svy_ob$variables[["ind_gender"]])
  print("yoshi2")
  variables_to_analyze<-subset_in_DAP$variable %>% trimws()
  ind_svy_ob$variables<-ind_svy_ob$variables[!is.na(ind_svy_ob$variables[[name_of_subset_in_DAP]]),]
  list_of_surveys_by_subset<-split(ind_svy_ob$variables, ind_svy_ob$variables[["double_subset"]])
  print("ASD")
  # list_of_subsets[lapply(list_of_subsets, length) > 0]
  for(j in 1 : length(list_of_surveys_by_subset)){
    name_of_subset_survey<-names(list_of_surveys_by_subset)[j]
    
    print(paste0 ("GROUP =",subset_in_DAP," ",name_of_subset_survey))
    new_design<-survey::svydesign(ids = ~ 1,
                                  strata = formula(paste0("~",strata)),
                                  weights= ~weights,
                                  data = list_of_surveys_by_subset[[name_of_subset_survey]])
    analyzed_overall[[paste0(name_of_subset_in_DAP,"_", name_of_subset_survey)]]<-  butteR::mean_proportion_table(design = new_design,
                                                           list_of_variables = variables_to_analyze,
                                                           aggregation_level = NULL,
                                                           round_to = 2,return_confidence = FALSE,na_replace = FALSE, questionnaire = Indiv_kobo_questionnaire)
    analyzed_by_camp[[paste0(name_of_subset_in_DAP,"_", name_of_subset_survey)]]<-  butteR::mean_proportion_table(design = new_design,
                                                           list_of_variables = variables_to_analyze,
                                                           aggregation_level = strata,
                                                           round_to = 2,return_confidence = FALSE,na_replace = FALSE,questionnaire = Indiv_kobo_questionnaire)
    analyzed_by_respondent_gender[[paste0(name_of_subset_in_DAP,"_", name_of_subset_survey)]]<-  butteR::mean_proportion_table(design = new_design,
                                                                        list_of_variables = variables_to_analyze,
                                                                        aggregation_level = "respondent_gender",
                                                                        round_to = 2,return_confidence = FALSE,na_replace = FALSE,questionnaire = Indiv_kobo_questionnaire)
    
  }
  overall_analysis[[name_of_subset_in_DAP]]<-analyzed_overall
  by_strata[[name_of_subset_in_DAP]]<-analyzed_by_camp
  by_resp_gender[[name_of_subset_in_DAP]]<-analyzed_by_respondent_gender
}




subsets_together_overall1<-do.call("smartbind", overall_analysis[[1]]) %>% tibble::rownames_to_column()
subsets_together_overall2<-do.call("smartbind", overall_analysis[[2]]) %>% tibble::rownames_to_column()
subsets_together_overall3<-do.call("smartbind", list(subsets_together_overall1,subsets_together_overall2))


# subsets_together_overall1<-do.call("smartbind", overall_analysis$I.EDU.age_group_formal_education.INDV) %>% tibble::rownames_to_column()
# subsets_together_overall2<-do.call("smartbind", overall_analysis$I.EDU.age_group_nonformal_education.INDV) %>% tibble::rownames_to_column()
# subsets_together_overall3<-do.call("smartbind", list(subsets_together_overall1,subsets_together_overall2))

subsets_together_by_strata1<-do.call("smartbind", sapply(by_strata[[1]], as.data.frame))%>% tibble::rownames_to_column()
subsets_together_by_strata2<-do.call("smartbind", sapply(by_strata[[2]], as.data.frame))%>% tibble::rownames_to_column()
subsets_together_by_strata3<-do.call("smartbind", list(subsets_together_by_strata1,subsets_together_by_strata2))

subsets_together_by_resp_gender1<-do.call("smartbind",
                                         sapply(by_resp_gender[[1]],
                                                as.data.frame))%>% tibble::rownames_to_column()
subsets_together_by_resp_gender2<-do.call("smartbind",
                                         sapply(by_resp_gender[[2]],
                                                as.data.frame))%>% tibble::rownames_to_column()
subsets_together_by_resp_gender3<-do.call("smartbind", list(subsets_together_by_resp_gender1,subsets_together_by_resp_gender2))


if (population=="Host"){
  write.csv(subsets_together_overall3,"Outputs/2019_10_02_Host_Double_Subsets_Overall.csv")
  write.csv(subsets_together_by_strata3,"Outputs/2019_10_02_Host_Double_Subsets_By_Strata.csv")
  write.csv(subsets_together_by_resp_gender3,"Outputs/2019_10_02_Host_Double_Subsets_By_Resp_gender.csv")
}
if(population=="Refugee"){
  write.csv(subsets_together_overall3,"Outputs/2019_10_02_Refugee_Double_Subsets_Overall.csv")
  write.csv(subsets_together_by_strata3,"Outputs/2019_10_02_Refugee_Double_Subsets_Strata.csv")
  write.csv(subsets_together_by_resp_gender3,"Outputs/2019_10_02_Refugee_Double_Subsets_By_Resp_gender.csv")
  
}







#######################################################



marital_status_by_gender_hoh<-svyby(formula = ~hoh_marital, by = ~I.HH_CHAR.gender_hoh.HH,design = HH_svy_ob,FUN = svymean, na.rm=TRUE)

# HH_srv<-as_survey(HH_svy_ob)
marital_status_by_gender_hoh<-HH_srv %>% 
  group_by(hoh_marital,I.HH_CHAR.gender_hoh.HH) %>% 
  summarise(asdf=survey_mean(na.rm=TRUE))

ind_srv<-as_survey(ind_svy_ob)
ind_srv$variables$I.INDV_CHAR.age_groups_demographics.INDV

ind_svy_ob$variables$I.INDV_CHAR.age_groups_demographics.INDV<-as.factor(ind_svy_ob$variables$I.INDV_CHAR.age_groups_demographics.INDV)
ind_svy_ob$variables$ind_gender<-as.factor(ind_svy_ob$variables$ind_gender)

svyby(formula = ~I.INDV_CHAR.age_groups_demographics.INDV, by = ~ind_gender,design = ind_svy_ob,FUN = svyciprop)
svyby(formula = ~ind_gender, by = ~I.INDV_CHAR.age_groups_demographics.INDV,design = ind_svy_ob,FUN = svymean)

# svytable(~I.INDV_CHAR.age_groups_demographics.INDV+ind_gender, ind_svy_ob) %>% prop.table

# marital_status_by_gender_hoh<-ind_srv %>% 
#   group_by(ind_gender,I.INDV_CHAR.age_groups_demographics.INDV) %>% 
#   summarise(asdf=survey_mean(na.rm=TRUE))


# WITH CONFIDENC INTERVALS IF YOU WANT
# basic_analysis_with_cis<-list()
# basic_analysis_with_cis[["overall"]]<-butteR::mean_proportion_table(design = HH_svy_ob,
#                                                                        list_of_variables = variables_to_analyze,
#                                                                        aggregation_level = NULL,
#                                                                        round_to = 2,return_confidence = TRUE,na_replace = FALSE)


# basic_analysis_with_cis[["by_strata"]]<-butteR::mean_proportion_table(design = HH_svy_ob, 
#                                                                          list_of_variables = variables_to_analyze,
#                                                                          aggregation_level = strata,
#                                                                          round_to = 2,return_confidence = TRUE,na_replace = FALSE)
# basic_analysis_with_cis[["by_respondent_gender"]]<-butteR::mean_proportion_table(design = HH_svy_ob, 
#                                                                                     list_of_variables = variables_to_analyze,
#                                                                                     aggregation_level = "respondent_gender",
#                                                                                     round_to = 2,
#                                                                                     return_confidence = TRUE,na_replace = FALSE)




asdf<-overall_analysis[unlist(stringr::str_detect(names(analyzed_overall),"I.EDU.age_group_education.INDV"))]
overall_analysis[stringr::str_detect(names(analyzed_overall),"I.EDU.age_group_education.INDV")]
subset_by_ind_gender<-dap_break_downs$dap_subsets_indiv %>% filter(subset=="ind_gender") 
variables_to_analyze<-subset_by_ind_gender$variable %>% trimws()

#WE WANT TO SUBSET THIS BY GENDER

# ind_svy_ob$variables<-ind_svy_ob$variables %>% filter(ind_gender!= "other")
# gender_subsets<-split(ind_svy_ob$variables, ind_svy_ob$variables$ind_gender)

male_hoh<-HH_svy_ob %>% subset(I.HH_CHAR.gender_hoh.HH=="male") %>% as_survey()
female_hoh<-HH_svy_ob %>% subset(I.HH_CHAR.gender_hoh.HH=="female") %>% as_survey()

hoh_marital_male_hoh<-male_hoh %>% 
  group_by(hoh_marital) %>% 
  summarise(
    hoh_marital_male_hoh=survey_mean(na.rm = TRUE)
  )

hoh_marital_female_hoh<-female_hoh %>% 
  group_by(hoh_marital) %>% 
  summarise(
    hoh_marital_female_hoh=survey_mean(na.rm = TRUE)
  )





if(population=="Refugee"){
  write.csv(hoh_marital_male_hoh, "Outputs/Refugee/2019_09_24_Refugee_marital_status_subset_male_hoh.csv")
  write.csv(hoh_marital_female_hoh, "Outputs/Refugee/2019_09_24_Refugee_marital_status_female_hoh.csv")
}
if(population=="Host"){
  write.csv(hoh_marital_male_hoh, "Outputs/Host/2019_09_24_Host_marital_status_subset_male_hoh.csv")
  write.csv(hoh_marital_female_hoh, "Outputs/Host/2019_09_24_Host_marital_status_female_hoh.csv")
}
analyzed_overall<-list()
analyzed_by_respondent_gender<-list()
analyzed_by_camp<-list()
# ind_svy_ob$variables<-butteR::questionnaire_factorize_categorical(ind_svy_ob$variables,questionnaire = Indiv_kobo_questionnaire,return_full_data = TRUE)
for(subset_group in names(gender_subsets)){
  new_design<-survey::svydesign(ids = ~ 1,
                                strata = formula(paste0("~",strata)),
                                weights= ~weights,
                                data = gender_subsets[[subset_group]])
  analyzed_overall[[subset_group]]<-  butteR::mean_proportion_table(design = new_design, 
                                                                    list_of_variables = variables_to_analyze,
                                                                    aggregation_level = NULL,
                                                                    round_to = 2,return_confidence = FALSE,na_replace = FALSE,
                                                                    questionnaire = Indiv_kobo_questionnaire)
  print(paste0 (subset_group , " overall done"))
  analyzed_by_camp[[subset_group]]<-  butteR::mean_proportion_table(design = new_design, 
                                                                    list_of_variables = variables_to_analyze,
                                                                    aggregation_level = strata,
                                                                    round_to = 2,return_confidence = FALSE,na_replace = FALSE,
                                                                    questionnaire = Indiv_kobo_questionnaire)
  print(paste0 (subset_group , " by camp done"))
  analyzed_by_respondent_gender[[subset_group]]<-  butteR::mean_proportion_table(design = new_design, 
                                                                                 list_of_variables = variables_to_analyze,
                                                                                 aggregation_level = "respondent_gender",
                                                                                 round_to = 2,return_confidence = FALSE,na_replace = FALSE,
                                                                                 questionnaire = Indiv_kobo_questionnaire)
  print(paste0 (subset_group , " by resp gender"))
}

analysis_by_ind_gender<-list(overall=analyzed_overall,by_strata=analyzed_by_camp,by_resp_gender=analyzed_by_respondent_gender)
# ind_svy_ob$variables$ind_formal_learning_prev %>% class()
# Indiv_kobo_questionnaire$question_get_question_label("ind_formal_learning_prev")
# Indiv_kobo_questionnaire$question_get_choices("ind_formal_learning_prev")

# asdf$ind_formal_learning_prev %>% class()
for(analysis_name in names(analysis_by_ind_gender)){
  male_ind_gender<-analysis_by_ind_gender[[analysis_name]][["male"]]
  female_ind_gender<-analysis_by_ind_gender[[analysis_name]][["female"]]
  date_for_title<-stringr::str_replace_all(Sys.Date(),"-","_")
  name_of_file_male<-paste0(date_for_title,"_",population,"_Indiv_SUBSET_by_ind_gender_male_",analysis_name, ".csv")
  name_of_file_female<-paste0(date_for_title,"_",population,"_Indiv_SUBSET_by_ind_gender_female_",analysis_name, ".csv")
  write.csv(male_ind_gender,paste0("Outputs/",name_of_file_male) )
  write.csv(female_ind_gender,paste0("Outputs/",name_of_file_female) )
}


overall_analysis[[which([[stringr::str_detect(names(analyzed_overall),"I.EDU.age_group_education.INDV")]])]]

analyzed_overall$`I.EDU.age_group_education.INDV_3-4`
analyzed_overall[[]]
asdf<-purrr::map(analyzed_overall, ~ unlist(.[1:4]))
analyzed_overall$ind_gender_male
analyzed_overall$`12-17`
overall_binded$I.EDU.age_group_education.INDV
analyzed_overall
sapply(overall_binded,function(x) x[2])
  # all_analysis[[name_of_subset_in_DAP]]<-list(analyzed_overall,analyzed_by_camp,analyzed_by_respondent_gender)
  # overall_analysis[[name_of_subset_in_DAP]][,i]<-analyzed_overall
  # by_strata[[name_of_subset_in_DAP]][[i]]<-analyzed_by_camp
  # by_resp_gender[[name_of_subset_in_DAP]][[i]]<- analyzed_by_respondent_gender
  # write.csv(analyzed_overall[[subset_group]],paste0(date_for_title,"_OVERALL_",name_of_subset_in_DAP,"_",subset_group, "_SUBSET.csv"))
  # write.csv(analyzed_by_camp[[subset_group]],paste0(date_for_title,"_BY_STRATA_",name_of_subset_in_DAP,"_",subset_group, "_SUBSET.csv"))
  # write.csv( analyzed_by_respondent_gender[[subset_group]],paste0("Outputs/subsets/",date_for_title,"_BY_RESP_GENDER_",name_of_subset_in_DAP,"_",subset_group, "_SUBSET.csv"))


overall_analysis$I.EDU.age_group_formal_education.INDV
overall_analysis$ind_gender
overall_analysis$I.EDU.age_group_formal_education.INDV
overall_analysis$ind_gender 
overall_analysis$I.EDU.age_group_formal_education.INDV %>% length()
do.call("rbind", overall_analysis)
length(overall_analysis$I.EDU.age_group_formal_education.INDV)
for(i in 1:length(basic_analysis_without_cis)){
  type_of_analysis<-names(basic_analysis_without_cis)[i]
  date_for_title<-stringr::str_replace_all(Sys.Date(),"-","_")
  name_of_file<-paste0(date_for_title,"_",population,"_Indiv_DAP_Simple_",type_of_analysis,"_", ".csv")
  write.csv(basic_analysis_without_cis[[type_of_analysis]],paste0("Outputs/",name_of_file) )
}
for(i in 1: length())
overall_analysis$I.EDU.age_group_formal_education.INDV
asdfg$I.EDU.age_group_formal_education.INDV
analyzed_overall
asdfg$I.EDU.age_group_formal_education.INDV$
names(asdfg)
names(asdfg$I.EDU.age_group_formal_education.INDV)
names(asdfg$I.EDU.age_group_nonformal_education.INDV)
names(asdfg$I.EDU.age_group_formal_education.INDV$`5-11`$`12-17`)
asdfg$I.EDU.age_group_formal_education.INDV
asdfg$I.EDU.age_group_formal_education.INDV %>% names()
asdfg$ind_gender$I.EDU.age_group_formal_education.INDV
ind_svy_ob$variables$I.EDU.age_group_formal_education.INDV %>% unique()
all_analysis$I.EDU.age_group_nonformal_education.INDV
all_analysis$I.EDU.age_group_nonformal_education.INDV[[1]]
all_analysis[[1]]
all_analysis[[1]][[2]][[3]]
analyzed_overall %>% names()
analyzed_overall$female
# 
# 
# # Table of weights  -------------------------------------------------
# 
# 
# HH_svy_ob$variables$weights<-HH_svy_ob %>% weights()
# HH_svy_ob$variables %>% nrow()
# weight_table<-HH_svy_ob$variables %>%
#   group_by(!!sym(strata)) %>%
#   summarise(weights=unique(weights),
#             number_per_union=n()) %>% data.frame() 
# # write.csv(weight_table, "Outputs/HC_MSNA2019_Weights_17sept2019.csv")
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
# 
# 
# 
