

rm(list=ls())
user<-c("zack", "mehedi")[1]
population<-c("Host","Refugee")[1]
data_process<-c("checking", "cleaning", "analysis")[3]
# write_output<-c("yes","no")[1]
# butteR::mean_proportion_table
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
# library(nngeo)

 
source("Functions/ActivatePaths.R")
source("Functions/make_composite_indicators_bgd_msna_2019.R")


# LOAD IN DATA AND ODK TOOLS ----------------------------------------------

HH<-read.csv(HH_path, stringsAsFactors = FALSE, na.strings=c("", " ", NA))
Indiv<-read.csv(Indiv_path, stringsAsFactors = FALSE, na.strings=c("", " ", NA))

HH_kobo_questions<-read.csv(survey_path,stringsAsFactors = FALSE)
HH_kobo_choices<-read.csv(choices_path, stringsAsFactors = FALSE)
HH_kobo_questionnaire<-koboquest::load_questionnaire(HH,questions = HH_kobo_questions,choices = HH_kobo_choices, choices.label.column.to.use = "label..english")
Indiv_kobo_questionnaire<-koboquest::load_questionnaire(Indiv,questions = HH_kobo_questions,choices = HH_kobo_choices, choices.label.column.to.use = "label..english")
#LOAD DAP
dap<- read.csv("Inputs/DAPs/MSNA_DAP_BasicAnalysis_All_Changes_Together_19SepMM.csv", stringsAsFactors = FALSE, na.strings=c("", " ", NA))


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
  filter(!is.na(subset)) %>% 
  filter(dataset %in% c(dap_population_name, "both")) %>%
  filter(level=="household") %>% 
  group_by(subset)
dap_break_downs$dap_subsets_indiv <-dap %>%
  filter(!is.na(subset)) %>% 
  filter(dataset %in% c(dap_population_name, "both")) %>%
  filter(level=="individual") %>% 
  group_by(subset)
dap_break_downs$dap_composite_hh<-dap %>% 
  filter(!is.na(subset)) %>% 
  filter(dataset %in% c(dap_population_name, "both")) %>%
  filter(level=="household")
dap_break_downs$dap_composite_indiv<-dap %>% 
  filter(!is.na(subset)) %>% 
  filter(dataset %in% c(dap_population_name, "both")) %>%
  filter(level=="individual")






# FILTER YES CONSENT AND MAKE COMPOSITE INDICATORS ------------------------

HH_yes_consent<- HH %>% filter(informed_consent=="yes")
Indiv<-Indiv %>% filter(X_submission__uuid %in% HH_yes_consent$X_uuid)

#RUN COMPOSITE INDICATOR SCRIPT- THANKS MEHEDI!
composite_indicators<-make_composite_indicators_bgd_msna_2019(hh_data = HH_yes_consent,  individual_data = Indiv,population = population)

# JOIN COMPOSITE INDICATORS TO APPROPRIATE DATA SETS
HH_with_composite<-HH_yes_consent %>% left_join(composite_indicators$household_composites,by="X_uuid")
Indiv_with_composite<-Indiv %>% left_join(composite_indicators$individual_composites,by=c("X_submission__uuid"))

# GET RID OF CONCATENAT
HH_with_composite<-butteR::remove_concat_select_multiple(HH_with_composite,questionnaire = HH_kobo_questionnaire)
Indiv_with_composite<- butteR::remove_concat_select_multiple(Indiv_with_composite,questionnaire = Indiv_kobo_questionnaire)

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
if(population=="Host"){
  pop_cleaned<-pop
  pop_cleaned[[sf_strata]]<-if_else(pop_cleaned[[sf_strata]] =="Teknaf Sadar" , "Teknaf",pop_cleaned[[sf_strata]])
}

weighting<-map_to_weighting(sampling.frame = pop_cleaned, 
                            data.stratum.column = strata,
                            data = HH_data_factorized, 
                            sampling.frame.population.column =sf_pop,
                            sampling.frame.stratum.column = sf_strata)


HH_data_factorized %>% group_by(!!sym(strata)) %>% count()


#MAKE DESIGN OBJECTS
HH_svy_ob<-map_to_design(data=HH_data_factorized, weighting_function = weighting)


Indiv_with_relevant_HH_data<-Indiv_with_composite %>% left_join(HH_svy_ob$variables %>% 
                      mutate(weights= weights(HH_svy_ob)) %>% 
                      select(X_uuid,{strata},respondent_gender,weights), by=c("X_submission__uuid"="X_uuid"))

ind_svy_ob<-survey::svydesign(ids = ~ 1,
                  strata =  formula(paste0("~",strata)),
                  weights= ~weights,
                  data = Indiv_with_relevant_HH_data)



# run dap -----------------------------------------------------------------
variables_to_analyze<-dap_break_downs$dap_basic_hh$variable  %>% trimws()
dap_break_downs$dap_basic_hh$variable
HH_svy_ob$variables$I.HH_CHAR.childheaded_households.HH<-forcats::fct_expand(HH_svy_ob$variables$I.HH_CHAR.childheaded_households.HH, "yes")


#NINA WANTS THE DATA WITHOUT CI
basic_analysis_without_cis<-list()
basic_analysis_without_cis[["overall"]]<-butteR::mean_proportion_table(design = HH_svy_ob, 
                                                                 list_of_variables = variables_to_analyze,
                                                                 aggregation_level = NULL,
                                                                 round_to = 2,return_confidence = FALSE,na_replace = FALSE)

basic_analysis_without_cis[["by_strata"]]<-butteR::mean_proportion_table(design = HH_svy_ob, 
                                                                 list_of_variables = variables_to_analyze,
                                                                 aggregation_level = strata,
                                                                 round_to = 2,return_confidence = FALSE,na_replace = FALSE)


basic_analysis_without_cis[["by_respondent_gender"]]<-butteR::mean_proportion_table(design = HH_svy_ob, 
                                                                        list_of_variables = variables_to_analyze,
                                                                        aggregation_level = "respondent_gender",
                                                                        round_to = 2,return_confidence = FALSE,na_replace = FALSE)


for(i in 1:length(basic_analysis_without_cis)){
  type_of_analysis<-names(basic_analysis_without_cis)[i]
  date_for_title<-stringr::str_replace_all(Sys.Date(),"-","_")
  name_of_file<-paste0(date_for_title,"_",population,"_HH_DAP_Simple_",type_of_analysis,"_", ".csv")
  write.csv(basic_analysis_without_cis[[type_of_analysis]],paste0("Outputs/",name_of_file) )
}


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



# Indivdual Level Analysis ------------------------------------------------

# DAP 1

variables_to_analyze<-dap_break_downs$dap_basic_indiv$variable %>% trimws()

#JUST ADDING ONE VARIABLE CLASS INTEGER SO THAT BUTTER WILL RUN (NEED TO UPDATE THIS IN THE butteR package)
variables_to_analyze_good<- c(variables_to_analyze)#,"ind_why_notreatment.treatment_expensive")

#FACTORIZE THE INDIVIDUAL DATA -- WILL NOT ACTUALLY USE THE QUESTIONNAIRE- SO WARNING DOESNT MATTER
ind_svy_ob$variables<- butteR::questionnaire_factorize_categorical(data = ind_svy_ob$variables, questionnaire = HH_kobo_questionnaire,return_full_data = TRUE)


basic_analysis_without_cis<-list()

basic_analysis_without_cis[["overall"]]<-butteR::mean_proportion_table(design = ind_svy_ob, 
                                                                       list_of_variables = variables_to_analyze_good,
                                                                       aggregation_level = NULL,
                                                                       round_to = 2,return_confidence = FALSE,na_replace = FALSE)
basic_analysis_without_cis[["by_strata"]]<-butteR::mean_proportion_table(design = ind_svy_ob, 
                                                                         list_of_variables = variables_to_analyze_good,
                                                                         aggregation_level = strata,
                                                                         round_to = 2,return_confidence = FALSE,na_replace = FALSE)
basic_analysis_without_cis[["by_respondent_gender"]]<-butteR::mean_proportion_table(design = ind_svy_ob, 
                                                                                    list_of_variables = variables_to_analyze_good,
                                                                                    aggregation_level = "respondent_gender",
                                                                                    round_to = 2,return_confidence = FALSE,na_replace = FALSE)
basic_analysis_without_cis$overall

for(i in 1:length(basic_analysis_without_cis)){
  type_of_analysis<-names(basic_analysis_without_cis)[i]
  date_for_title<-stringr::str_replace_all(Sys.Date(),"-","_")
  name_of_file<-paste0(date_for_title,"_",population,"_Indiv_DAP_Simple_",type_of_analysis,"_", ".csv")
  write.csv(basic_analysis_without_cis[[type_of_analysis]],paste0("Outputs/",name_of_file) )
}




subset_by_ind_gender<-dap_break_downs$dap_subsets_indiv %>% filter(subset=="ind_gender") 
variables_to_analyze<-subset_by_ind_gender$variable %>% trimws()


#WE WANT TO SUBSET THIS BY GENDER
gender_subsets<-split(ind_svy_ob$variables, ind_svy_ob$variables$ind_gender)

analyzed_overall<-list()
analyzed_by_respondent_gender<-list()
analyzed_by_camp<-list()
for(subset_group in names(gender_subsets)){
  new_design<-survey::svydesign(ids = ~ 1,
                    strata = formula(paste0("~",strata)),
                    weights= ~weights,
                    data = gender_subsets[[subset_group]])
  analyzed_overall[[subset_group]]<-  butteR::mean_proportion_table(design = new_design, 
                                                                    list_of_variables = variables_to_analyze,
                                                                    aggregation_level = NULL,
                                                                    round_to = 2,return_confidence = FALSE,na_replace = FALSE)
  analyzed_by_camp[[subset_group]]<-  butteR::mean_proportion_table(design = new_design, 
                                                                    list_of_variables = variables_to_analyze,
                                                                    aggregation_level = strata,
                                                                    round_to = 2,return_confidence = FALSE,na_replace = FALSE)
  analyzed_by_respondent_gender[[subset_group]]<-  butteR::mean_proportion_table(design = new_design, 
                                                                                 list_of_variables = variables_to_analyze,
                                                                                 aggregation_level = "respondent_gender",
                                                                                 round_to = 2,return_confidence = FALSE,na_replace = FALSE)}

analysis_by_ind_gender<-list(overall=analyzed_overall,by_strata=analyzed_by_camp,by_resp_gender=analyzed_by_respondent_gender)

for(analysis_name in names(analysis_by_ind_gender)){
  male_ind_gender<-analysis_by_ind_gender[[analysis_name]][["male"]]
  female_ind_gender<-analysis_by_ind_gender[[analysis_name]][["female"]]
  date_for_title<-stringr::str_replace_all(Sys.Date(),"-","_")
  name_of_file_male<-paste0(date_for_title,"_",population,"_Indiv_SUBSET_by_ind_gender_male_",type_of_analysis, ".csv")
  name_of_file_female<-paste0(date_for_title,"_",population,"_Indiv_SUBSET_by_ind_gender_female_",analysis_name, ".csv")
  write.csv(male_ind_gender,paste0("Outputs/",name_of_file_male) )
  write.csv(female_ind_gender,paste0("Outputs/",name_of_file_female) )
}




# Table of weights  -------------------------------------------------


HH_svy_ob$variables$weights<-HH_svy_ob %>% weights()
HH_svy_ob$variables %>% nrow()
weight_table<-HH_svy_ob$variables %>%
  group_by(!!sym(strata)) %>%
  summarise(weights=unique(weights),
            number_per_union=n()) %>% data.frame() 
# write.csv(weight_table, "Outputs/HC_MSNA2019_Weights_17sept2019.csv")
















