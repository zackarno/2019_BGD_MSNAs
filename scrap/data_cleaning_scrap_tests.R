
rm(list=ls())
library(dplyr)
library(stringr)
library(lubridate)
population<-c("Host", "Refugee")[2]
data_process<-c("checking", "cleaning", "analysis")[3]

source("Functions/ActivatePaths.R")



household<-read.csv(HH_path, stringsAsFactors = FALSE,na.strings=c("", " ", NA))


# household<-read.csv("Inputs/Refugee/data_cleaning/FINAL R1 Data Cleaning Refugee MSNA 2019 2 Sep SURVEYS TO DELETE MM .csv")
individual<-read.csv(Indiv_path,stringsAsFactors = FALSE,na.strings=c("", " ", NA))
if(population=="Refugee"){
to_delete<-read.csv("Inputs/Refugee/data_cleaning/cleaning_logs/FINAL_R2_Data_Cleaning_Refugee_MSNA_2019_17_Sep_SURVEYS_TO_DELETE_MM.csv", stringsAsFactors = FALSE,na.strings=c("", " ", NA))
cleaning_log<-read.csv("Inputs/Refugee/data_cleaning/cleaning_logs/FINAL_R2_Data_Cleaning_Refugee_MSNA_2019_17_Sep_SURVEYS_TO_MODIFY_MM.csv", stringsAsFactors = FALSE,na.strings=c("", " ", NA))
}

if(population=="Host"){
  to_delete<-read.csv("Inputs/Host_Community/04_data_cleaning/cleaning_logs/FINAL R3 Data Cleaning HC MSNA 2019 UPDATED 15 Sep SURVEYS TO DELETE MM .csv", stringsAsFactors = FALSE,na.strings=c("", " ", NA))
  cleaning_log<-read.csv("Inputs/Host_Community/04_data_cleaning/cleaning_logs/FINAL R3 Data Cleaning HC MSNA 2019 UPDATED 15 Sep SURVEYS TO MODIFY MM .csv", stringsAsFactors = FALSE,na.strings=c("", " ", NA))}


colnames(cleaning_log)<-colnames(cleaning_log) %>% tolower()
cleaning_log$indicator<-cleaning_log$question %>% trimws()
cleaning_log$new_value<-cleaning_log$new_value %>% trimws()

to_delete$UUID<-to_delete$UUID %>% trimws()

# a.	REFUGEE DATASET ONLY: add a column named shelter_purchased_material.bamboo_unspecified after shelter_purchased_material.barok_bamboo
# i.	Could it be automatically coded to mirror the barok_bamboo question, where: 1) if barok_bamboo = blank/NA, then bamboo_unspecified should also be coded as such and 2) if barok_bamboo = any value (0 or 1), the new bamboo_unspecified == 0? Mark will indicate in his cleaning log when bamboo_unspecified should be turned to 1

########################################################################
#USEFUL TO CONVERT NON COMPATIBLE EMAIL LISTS
#########################################################################
# from_email<-readClipboard()
# str_replace_all(from_email," ", ",") %>% str_split(",") %>% dput()
#########################################################################

#REMOVE COLUMNS
if (population== "Refugee"){
  HH_additional_cols_to_remove<-c("referral", "referral.general_protection", "referral.gbv", 
                               "referral.cp", "referral_ID","NFI_own", "hh_coping_mechanism", "debt_reason", 
                               "Settlement", "District", "X", "Union")
  
  indiv_addition_cols_to_remove<-c("ind_formal_learning_number", "ind_formal_notattend_number", "ind_help_daily_number", "ind_need_treatment", "X")
  
   
  individual<- individual %>% 
    select(X_submission__uuid,X_index, everything())
  #MIGHT NEED TO CHANGE
  unsafe_col<-"feel_unsafe_female.none"
  
  

  #add a column named shelter_purchased_material.bamboo_unspecified after shelter_purchased_material.barok_bamboo
  # i.	Could it be automatically coded to mirror the barok_bamboo question, where: 1) if barok_bamboo = blank/NA, then bamboo_unspecified should also be coded as such and 2) if barok_bamboo = any value (0 or 1), the new bamboo_unspecified == 0? Mark will indicate in his cleaning log when bamboo_unspecified should be turned to 1
  
  household<-household %>%
    mutate( 
      shelter_purchased_material.bamboo_unspecified= ifelse(is.na(shelter_purchased_material.barok_bamboo),NA,0)
    )%>%
    select(everything(),
           shelter_purchased_material.barok_bamboo,
           shelter_purchased_material.bamboo_unspecified,
           everything()) 
  
  
  }

if (population=="Host"){
  HH_additional_cols_to_remove<-c("enumerator_org",
                                  "end_note",
                                  "OBJECTID",
                                  "OBJECTID_1", 
                                  "GEOCODE11",
                                  "UpzCode",
                                  "A4remark", 
                                  "adm4_en",
                                  "adm3_en",
                                  "reported_date")
  
  
  unsafe_col<-"feel_unsafe_female.none"

}

household$Upazila<-str_replace(household$Upazila, "Ukhia", "Ukhiya")



HH_remove_cols<-  c("deviceid", "instance_name", "audit", "intro_text", 
                "hh_text", "repeat_introduction", "female_reprodage_count", "illness_HH_count", 
                "under2_HH_count", "under5_HH_count", "over_5_HH_count", "under_18_HH_count", 
                "over_18_HH_count", "ind_help_daily_count", "formal_school_count", 
                "formal_school_NOT_count", "food_list", "past_30_days_spend", 
                "married_mobility", "unmarried_mobility", "rank_priority_needs_label", 
                "X_id", "X_submission_time", "X_validation_status","enumerator_id", "survey_start", "end_note",
                "end_survey", "X",  HH_additional_cols_to_remove)



# "NFI_own"
INDIV_remove_cols<-c("under2_HH", "under5_HH", "over_5_HH", "under_18_HH", 
"over_18_HH", "num_female_reprod", "X_submission__id", 
"X_submission__submission_time", "X_submission__validation_status", 
"X_parent_table_name", indiv_addition_cols_to_remove)


household_columns_removed<-household %>% dplyr::select(-HH_remove_cols)
individual_columns_removed<-individual %>% dplyr::select(-INDIV_remove_cols)

# i.	If feel_unsafe_female == ‘none’ (or alternatively if “feel_unsafe_female.none” == 1), could the following 13 columns all be turned to NA / blank? 
unsafe_reason_cols<-c("unsafe_reason_female", "unsafe_reason_female.criminal_group", 
                      "unsafe_reason_female.violence", "unsafe_reason_female.bully_harass", 
                      "unsafe_reason_female.abduction", "unsafe_reason_female.forced_work", 
                      "unsafe_reason_female.study_work_outside", "unsafe_reason_female.lack_of_light", 
                      "unsafe_reason_female.intra_community_tension", "unsafe_reason_female.road_accident", 
                      "unsafe_reason_female.environmental_risk", "unsafe_reason_female.none", 
                      "unsafe_reason_female.dntknow_prefer", "unsafe_reason_female.other", 
                       "unsafe_reason_female_other")


HH_cleaned1<-household_columns_removed
HH_cleaned1[,unsafe_reason_cols]<-sapply(HH_cleaned1[,unsafe_reason_cols], 
       function(x) ifelse(HH_cleaned1[,unsafe_col]==1,NA,x) %>% as.numeric)

names(HH_cleaned1)[names(HH_cleaned1) == "work_mobility"] <- "married_work_mobility"
names(HH_cleaned1)[names(HH_cleaned1) == "market_mobility"] <- "married_market_mobility"

#####################################################################################################################3
raw_to_clean <- function(ds,cl_log,data_set_level){
  for(i in 1:nrow(cl_log)){
    print(i)
    print(cl_log$indicator[i])
    print(cl_log[i,]$indicator)
    if(data_set_level=="HH"){
    ds[,cl_log[i,]$indicator][ds[,"X_uuid"]==cl_log[i,]$uuid] <- cl_log[i,]$new_value
    }
    if(data_set_level=="INDIV"){
      ds[,cl_log[i,]$indicator][ds[,"X_index"]==cl_log[i,]$ind_index] <- cl_log[i,]$new_value
    }
  }
  # ds[,indicator][ds[,"X_uuid"]==uuid] <- new_value  
  return(ds)
}


cleaning_log_fixed<-cleaning_log %>% filter(!is.na(question)) %>% 
  mutate(dataset_level=if_else(is.na(ind_index),"hh", "ind")) 

HH_cleaning_log<-cleaning_log_fixed  %>% 
  filter(dataset_level=="hh")

Individual_cleaning_log<-cleaning_log_fixed  %>% 
  filter(dataset_level=="ind")

#check
HH_cleaning_log$uuid[HH_cleaning_log$uuid %in%HH_cleaned1$X_uuid==FALSE]
#remove uuids using to_delet
HH_cleaned2<- HH_cleaned1 %>% filter(X_uuid %in% trimws(to_delete$UUID)==FALSE)

HH_cleaned2$X_uuid%in% to_delete$UUID %>% which()
HH_cleaned1$X_uuid %in% to_delete$UUID %>% which() %>% length()


HH_cleaning_log$indicator[which(HH_cleaning_log$indicator %in% colnames(HH_cleaned2)==FALSE)]



HH_cleaned_final<-raw_to_clean(HH_cleaned2, HH_cleaning_log,data_set_level = "HH" )
Indiv_cleaned_1<-raw_to_clean(individual_columns_removed, Individual_cleaning_log,data_set_level = "INDIV" )
Indiv_cleaned_final<- Indiv_cleaned_1 %>% filter(X_submission__uuid %in% HH_cleaned_final$X_uuid)


HH_cleaned_final$reported_date

HH_cleaned_final$survey_date<-HH_cleaned_final$reported_date
HH_cleaned_final$camp_name <-HH_cleaned_final$New_Camp_N

HH_cleaned_final<- HH_cleaned_final %>% select(-reported_date, -New_Camp_N)
HH_cleaned_final$camp_name
# dir.create("D:\\Dropbox\\REACH_BGD\\REACH\\Ongoing\\70DQR - Joint MSNAs\\in-depth MSNAs\\02 Workplan and Data Collection\\01_HostCommunity/03_data_cleaning")
if(population=="Refugee"){
write.csv(HH_cleaned_final,"D:\\Dropbox\\REACH_BGD\\REACH\\Ongoing\\70DQR - Joint MSNAs\\in-depth MSNAs\\02 Workplan and Data Collection\\02_Refugee\\03_data_cleaning\\20190915_HH_Refugee_Cleaned_20190917.csv")
write.csv(Indiv_cleaned_final,"D:\\Dropbox\\REACH_BGD\\REACH\\Ongoing\\70DQR - Joint MSNAs\\in-depth MSNAs\\02 Workplan and Data Collection\\02_Refugee\\03_data_cleaning\\20190915_Indiv_Refugee_Cleaned_20190917.csv")}

if (population=="Host"){
write.csv(Indiv_cleaned_final,"D:\\Dropbox\\REACH_BGD\\REACH\\Ongoing\\70DQR - Joint MSNAs\\in-depth MSNAs\\02 Workplan and Data Collection\\01_HostCommunity/03_data_cleaning/20190909_Indiv_HostCommunity_Cleaned_20190915.csv")
write.csv(HH_cleaned_final,"D:\\Dropbox\\REACH_BGD\\REACH\\Ongoing\\70DQR - Joint MSNAs\\in-depth MSNAs\\02 Workplan and Data Collection\\01_HostCommunity/03_data_cleaning/20190909_HH_HostCommunity_Cleaned_20190915.csv")}


#############################################################################################################



