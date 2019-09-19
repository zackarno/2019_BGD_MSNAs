
# library -----------------------------------------------------------------

library(dplyr)
library(anytime)
library(lubridate)
library(AMR)
library(forcats) 

population<-c("Host","Refugee")[1]
write <- c("yes","no")[1]


# Load datasets -----------------------------------------------------------


if ( population == "Refugee"){
  HH<-read.csv("Inputs/Refugee/02_data_collection/HH_Refugee.csv", header = TRUE, sep = ",")
  INDV<-read.csv("Inputs/Refugee/02_data_collection/INDIV_Refugee_MSNA.csv", header = TRUE, sep = ",")   
}

if ( population == "Host"){
  HH<-read.csv("Inputs/Host_Community/02_data_collection/HH_HC_MSNA.csv", header = TRUE, sep = ",")
  INDV<-read.csv("Inputs/Host_Community/02_data_collection/INDIV_HC_MSNA.csv", header = TRUE, sep = ",")   
}


# Household to Household --------------------------------------------------



enough_water_without_drinking <- HH[,c("enough_water.cooking", "enough_water.washing_bathing", 
                                       "enough_water.domestic_purpose")]
medical_expenses_grater_than_zero <- c("501_1000_bdt","1_500_bdt","1001_2000_bdt","2001_5000_bdt","5001_plus_bdt")

# WASH.visible_faeces_stagwater_waste_accom <-HH[,c("stagnant_water", "visible_faeces", "visible_waste")]
# WASH.visible_faeces_stagwater_waste_accom_tf <- WASH.visible_faeces_stagwater_waste_accom == "yes"



hh_to_hh<- mutate(HH,
                  I.HH_CHAR.size.HH = if_else(hh_size < 5, "small","large", missing = NULL),

                  I.HH_CHAR.gender_hoh.HH = if_else(respondent_hoh == "yes",  HH$respondent_gender, HH$hoh_gender, NULL ),
                  
                  I.HH_CHAR.age_hoh.HH = if_else(respondent_hoh == "yes", true = HH$respondent_age, false = HH$hoh_age, missing = NULL ),
                  
                  
                  I.P.married_women_no_work_market_alone.HH = if_else( work_mobility == "" & market_mobility == "","",
                                                                      if_else(work_mobility == "never_alone" & market_mobility == "never_alone","yes","no")),
                  
                  I.P.unmarried_women_no_work_market_alone.HH = if_else( unmarried_work_mobility == "" & unmarried_market_mobility == "","",
                                                                         if_else(unmarried_work_mobility == "never_alone" & unmarried_market_mobility == "never_alone" , "yes","no")),
                  
                  enough_water_without_drinking_rowsum = rowSums(enough_water_without_drinking),
                  I.WASH.enough_water_all_needs.HH = if_else( enough_water_without_drinking_rowsum + enough_water.drinking == 4 , "yes","no",NULL),
                  I.WASH.enough_drinking_only_andnot_oneother.HH = if_else(enough_water.drinking == 1 & enough_water_without_drinking_rowsum ==2 ,"yes","no",NULL),
                  I.WASH.enough_drinking_only_andnot_twoothers.HH = if_else(enough_water.drinking == 1 & enough_water_without_drinking_rowsum ==1 ,"yes","no",NULL),
                  I.WASH.enough_drinking_only_andnot_threeothers.HH= if_else(enough_water.drinking == 1 & enough_water_without_drinking_rowsum == 0 ,"yes","no",NULL),
                  
                  # WASH.visible_faeces_stagwater_waste_accom_tf_rowsum = rowSums(WASH.visible_faeces_stagwater_waste_accom_tf),
                  # I.WASH.visible_faeces_stagwater_waste_accom.HH = if_else ( WASH.visible_faeces_stagwater_waste_accom_tf_rowsum ==1  ,"yes","no",NULL ),
                  
                  I.WASH.visible_faeces_stagwater_waste_accom.HH = if_else(stagnant_water == "" & visible_faeces == "" & visible_waste == "", "NA",
                                                                           if_else(stagnant_water == "yes" & visible_faeces == "yes" & visible_waste == "yes", "yes","no",NULL)),
                  
                  
                  
                  I.FCS_score = (cereals_tubers*2 + pulses_nuts_seeds*3 +
                                 vegetables*1 + fruits*1 +dairy*4 +
                                 meat_fish*4 + oil_fats*.5 +sweets*.5 +
                                 spices_condiments*0),
                  I.FSL.food_consumption_score.HH = if_else(I.FCS_score > 42, "Acceptable",
                                                            if_else (I.FCS_score >=28 ,  "Borderline",
                                                                    if_else(I.FCS_score < 28 , "poor","no" ))),
                  I.HEALTH.health_expenses_reported.HH = if_else(exp_medical %in% medical_expenses_grater_than_zero | health_coping.pay_care == 1, "yes","no",NULL ),
                  
                  I.HEALTH.debt_pay_health.HH = if_else(health_coping.ghealth_debt == 1 | debt_reason.pay_health_expenses ==1 , "yes","no", NULL),
                  
                  
                  
                  
              )  #%>% select(starts_with("I."))


if (population == "Refugee"){
  
  Some_primary<- c ("elementary_1", "elementary_2", "elementary_3", "elementary_4","kindergarten")
  Primary_and_above<- c("elementary_5","middle_6", "middle_7", "middle_8", "middle_9", "high_10", "high_11", "tertiary")
  no_formal_education <- c("noedu","madrassa")
  
  Language <- hh_to_hh[,c("language.chittagonian","language.burmese" ,"language.english","language.rohingya","language.bangla","language.arabic","language.other")]
  
  firewood_womenchildren <- hh_to_hh[,c("wood_access.adult_female","wood_access.girls","wood_access.boys")]
  
  
  FSL.begging_borrowing_bartering_support_food_included <- hh_to_hh[, c("food_source.support_relatives","food_source.exchange", "food_source.borrowed", 
                                                                        "food_source.begging")]
  FSL.begging_borrowing_bartering_support_food_excluded <- hh_to_hh[, c("food_source.own_production","food_source.gather", "food_source.hunt_fish",
                                                                        "food_source.assistance_distribution", "food_source.assistance_voucher",
                                                                        "food_source.army" )]
  
  
  
  SNFI.no_shelter_improvements_reason <-c("no_money","no_labor","dnk_items","market_unreachable")
  
  FSL.refugee_food_assistance_only_included <- hh_to_hh[,c("food_source.army", 
                                                  "food_source.assistance_distribution", 
                                                  "food_source.assistance_voucher")]
  
  FSL.refugee_food_assistance_only_excluded <- hh_to_hh[,c("food_source.purchase","food_source.support_relatives", "food_source.exchange", "food_source.borrowed", 
                                                           "food_source.begging", "food_source.gather", "food_source.hunt_fish", 
                                                           "food_source.own_production")]
  
  cooking_fuel_1 <- hh_to_hh[,c("cooking_fuel.purchased_firewood", "cooking_fuel.collected_firewood", 
                        "cooking_fuel.lpg_gas_cylinder", "cooking_fuel.kerosene_stove", 
                        "cooking_fuel.dung_cakes", "cooking_fuel.other", "cooking_fuel.dntknow_prefer")]

  safety_issues_barriers_key_facilities_included_education <- HH[,c("education_barrier.center_unsafe", 
                                                                    "education_barrier.way_unsafe")]
  safety_issues_barriers_key_facilities_included_market <- HH[,c("market_problems.market_unsafe_way", 
                                                                 "market_problems.market_unsafe")]
  
  P.awareness_community_protection_atleast_two <- HH[,c("comm_mechanisms.health", 
                                                        "comm_mechanisms.education", "comm_mechanisms.safety", "comm_mechanisms.community_mob", 
                                                        "comm_mechanisms.natural_disasters", "comm_mechanisms.disabilities", 
                                                        "comm_mechanisms.child_protect", "comm_mechanisms.dispute_resolution", "comm_mechanisms_other")]
  
  exp_shelter_materials_greater_than_zero <- c("1_500_bdt", "501_1000_bdt","1001_2000_bdt","2001_5000_bdt","5001_plus_bdt") #excluding dntknow_prefer
  
  
  HH_to_HH <- hh_to_hh %>% 
    mutate(
      language_rowsum = rowSums(Language,na.rm = TRUE),
      I.HH_CHAR.language_fluency.HH= if_else(language_rowsum==1, "1",
                                             if_else(language_rowsum == 2,"Bilingual",
                                                     if_else(language_rowsum>2,"Multilingual","", missing = NULL))),
      
      I.HH_CHAR.datearrival_bgd.HH = if_else(anydate(datearrival_bgd) < dmy("01/01/2016")  , true = "pre_2016",
                                             if_else(anydate(datearrival_bgd) %in%  dmy("01/01/2016"):dmy("31/07/2017"),"Jan 2016- July 2017",
                                                     if_else(anydate(HH$datearrival_bgd) > dmy("31/07/2017")  , true = "after_july_2017", false = " ", missing = NULL))),
      
      I.HH_CHAR.datearrival_shelter.HH = if_else(anydate(datearrival_shelter) < dmy("01/08/2017")  , true = "pre_Aug_2017",
                                                 if_else(anydate(datearrival_shelter) %in%  dmy("01/08/2017"):dmy("30/06/2018"), true = "Aug2017-June2018",
                                                         if_else(anydate(datearrival_shelter) %in%  dmy("01/07/2018"):dmy("31/12/2018"), true = "July-Dec 2018",
                                                                 if_else(anydate(datearrival_shelter) %in%  dmy("01/01/2019"):dmy("31/03/2019"), true = "Jan-March 2019 ",
                                                                         if_else(anydate(datearrival_shelter) > dmy("31/03/2019")  , true = "April 2019_Present", false = " ", missing = NULL))))),
      
      
      I.HH_CHAR.education_level.HH = if_else(edu_highest %in% Some_primary, "Some primary", 
                                             if_else( edu_highest %in% Primary_and_above, "Primary and above",
                                                      if_else(edu_highest %in% no_formal_education, "No formal Education","",missing = NULL ))),
      
      firewood_womenchildren_rowsum = rowSums(firewood_womenchildren),
      I.SNFI.firewood_womenchildren.HH = if_else(firewood_womenchildren_rowsum >=1, "yes", "no",NULL),
      
      
      I.SNFI.min_floormat.HH =if_else(floormat >= hh_size, true = "yes",false = "no", missing = NULL),
      I.SNFI.min_blanket.HH = if_else( blanket>= hh_size, true = "yes",false = "no", missing = NULL ),
     
      cooking_fuel_rowsum = rowSums(cooking_fuel_1),
      I.SNFI.lpg_only.HH = if_else(cooking_fuel_rowsum == 1 & cooking_fuel.lpg_gas_cylinder == 1, "yes","no",NULL),
      
      I.SNFI.use_firewood_any.HH = if_else(cooking_fuel.purchased_firewood == 1 | cooking_fuel.collected_firewood == 1, "yes","no" ),
      
      I.SNFI.no_shelter_improvements_reason.HH = if_else( improvement_reason == "", "NA",  
                                                          if_else( improvement_reason %in% SNFI.no_shelter_improvements_reason ,"yes","no",NULL)),
      
      FSL.refugee_food_assistance_only_excluded_rowsum = rowSums(FSL.refugee_food_assistance_only_excluded),
      FSL.refugee_food_assistance_only_included_rowsum = rowSums(FSL.refugee_food_assistance_only_included),
      
      I.FSL.refugee_food_assistance_only.HH = if_else(FSL.refugee_food_assistance_only_included_rowsum >= 1 & 
                                                        FSL.refugee_food_assistance_only_excluded_rowsum == 0 , "yes","no",NULL),
      
      FSL.begging_borrowing_bartering_support_food_included_rowsum = rowSums(FSL.begging_borrowing_bartering_support_food_included),
      FSL.begging_borrowing_bartering_support_food_excluded_rowsum = rowSums(FSL.begging_borrowing_bartering_support_food_excluded),
      I.FSL.begging_borrowing_bartering_support_food.HH = if_else(FSL.begging_borrowing_bartering_support_food_included_rowsum >=1 &
                                                                    FSL.begging_borrowing_bartering_support_food_excluded_rowsum == 0, "yes","no",NULL),
      
      safety_issues_barriers_key_facilities_included_education_rowsum = rowSums(safety_issues_barriers_key_facilities_included_education),
      safety_issues_barriers_key_facilities_included_market_rowsum = rowSums(safety_issues_barriers_key_facilities_included_market),

      I.P.safety_issues_barriers_key_facilities.HH_education = if_else(safety_issues_barriers_key_facilities_included_education_rowsum >=1 , "Education_unsafe","no",NULL),
      I.P.safety_issues_barriers_key_facilities.HH_market= if_else(safety_issues_barriers_key_facilities_included_market_rowsum >=1 , "Market_unsafe","no" ,NULL),
 
      #below pb, have to check 
      P.awareness_community_protection_atleast_two_rowsum = rowSums(P.awareness_community_protection_atleast_two,na.rm = T),
      I.P.awareness_community_protection_atleast_two.HH = if_else(P.awareness_community_protection_atleast_two_rowsum >= 2,"yes",
                                                                  if_else(P.awareness_community_protection_atleast_two_rowsum == 0, "None","no",NULL)), #what is 1, None and No same or not??
      
      I.FSL.debt_pay_food.HH = if_else(food_source.borrowed == 1 | debt_reason.buy_food == 1, "yes", "no", NULL),
      
      I.SNFI.pay_shelter_materials_improvements.HH = if_else(exp_shelter_materials %in% exp_shelter_materials_greater_than_zero | shelter_purchased == "yes" , "yes","no", NULL)
      
      
    ) %>% select(starts_with("I."),"X_uuid","informed_consent")
}


if (population == "Host") {
  
  Some_primary<- c ("1", "2", "3", "4")
  Primary_and_above<- c("5","6", "7", "8", "9", "10", "11", "12", "above_12_tertiary_edu")
  no_formal_education <- c("none","madrasah_only")
  
  safety_issues_barriers_key_facilities_included_education <- hh_to_hh[,c("education_barrier.school_unsafe", 
                                                                    "education_barrier.way_unsafe")]
  safety_issues_barriers_key_facilities_included_market <- hh_to_hh[,c("market_problems.market_unsafe_way", 
                                                                 "market_problems.market_unsafe")]
  
  SNFI.no_shelter_improvements_reason <-hh_to_hh[,c("improvement_reason.no_money", 
                                                    "improvement_reason.no_labour",
                                                    "improvement_reason.no_item_acess")]
  
  
  
  firewood_womenchildren <- c("adult_female","girls","boys")
  
  FSL.begging_borrowing_bartering_support_food_included <- hh_to_hh[, c("food_source.borrow","food_source.support_friends_relatives",
                                                                  "food_source.begging_scavenging","food_source.barter_exchange")]
  
  FSL.begging_borrowing_bartering_support_food_excluded <- hh_to_hh[, c("food_source.purchase_cash", "food_source.purchase_credit",
                                                                        "food_source.food_assistance_food_card","food_source.gathering", 
                                                                        "food_source.hunting_fishing", "food_source.own_production")]

                                                                        
  
  
  FSL.refugee_food_assistance_only_included <- hh_to_hh[,c("food_source.army_distributing","food_source.food_assistance_food_card")] #need to confirm
  
  FSL.refugee_food_assistance_only_excluded <- hh_to_hh[,c("food_source.purchase_cash", "food_source.purchase_credit",
                                                           "food_source.support_friends_relatives", "food_source.barter_exchange", 
                                                           "food_source.borrow", "food_source.begging_scavenging", "food_source.gathering", 
                                                           "food_source.hunting_fishing", "food_source.own_production")] #need to confirm and should be included other or not 
  
  
  cooking_fuel <- hh_to_hh[,c("cooking_fuel.purchased_firewood", "cooking_fuel.collected_firewood",
                        "cooking_fuel.biogas","cooking_fuel.induction","cooking_fuel.dried_leaf_hay",
                        "cooking_fuel.lpg_gas_cylinder", "cooking_fuel.kerosene_stove", 
                        "cooking_fuel.dung_cakes", "cooking_fuel.other")]
  cooking_fuel_rowsum <- rowSums(cooking_fuel)
  
  P.awareness_community_protection_atleast_two <- hh_to_hh[,c("comm_mechanisms.health", "comm_mechanisms.education", 
                                                        "comm_mechanisms.safety_security", "comm_mechanisms.community_mobilization", 
                                                        "comm_mechanisms.drr_preparation", "comm_mechanisms.support_disable", 
                                                        "comm_mechanisms.protect_children", "comm_mechanisms.resolve_dispute")]
  

HH_to_HH <- hh_to_hh %>% mutate(
    
    I.SNFI.lpg_only.HH = if_else( cooking_fuel_rowsum == 1 & cooking_fuel.lpg_gas_cylinder == 1, "yes","no",NULL),
    safety_issues_barriers_key_facilities_included_education_rowsum = rowSums(safety_issues_barriers_key_facilities_included_education),
    safety_issues_barriers_key_facilities_included_market_rowsum = rowSums(safety_issues_barriers_key_facilities_included_market),
    
    I.P.safety_issues_barriers_key_facilities.HH_education = if_else(safety_issues_barriers_key_facilities_included_education_rowsum >=1 , "Education_unsafe","no",NULL),
    I.P.safety_issues_barriers_key_facilities.HH_market= if_else(safety_issues_barriers_key_facilities_included_market_rowsum >=1 , "Market_unsafe","no",NULL ),
    
    SNFI.no_shelter_improvements_reason_rowsum = rowSums(SNFI.no_shelter_improvements_reason),
    I.SNFI.no_shelter_improvements_reason.HH = if_else( SNFI.no_shelter_improvements_reason_rowsum >= 1 ,"yes","no",NULL),
    
    
    
    P.awareness_community_protection_atleast_two_rowsum = rowSums(P.awareness_community_protection_atleast_two),
    I.P.awareness_community_protection_atleast_two.HH = if_else(P.awareness_community_protection_atleast_two_rowsum >= 2,"yes",
                                                                if_else(P.awareness_community_protection_atleast_two_rowsum == 0,"None","no",NULL)), #are "none" and "no" same ??
    
    FSL.begging_borrowing_bartering_support_food_included_rowsum = rowSums(FSL.begging_borrowing_bartering_support_food_included),
    FSL.begging_borrowing_bartering_support_food_excluded_rowsum = rowSums(FSL.begging_borrowing_bartering_support_food_excluded),
    I.FSL.begging_borrowing_bartering_support_food.HH = if_else(FSL.begging_borrowing_bartering_support_food_included_rowsum >=1  &
                                                                  FSL.begging_borrowing_bartering_support_food_excluded_rowsum == 0, "yes","no",NULL),
    
    FSL.refugee_food_assistance_only_excluded_rowsum = rowSums(FSL.refugee_food_assistance_only_excluded),
    FSL.refugee_food_assistance_only_included_rowsum = rowSums(FSL.refugee_food_assistance_only_included),
    I.FSL.host_community_food_assistance_only.HH = if_else(FSL.refugee_food_assistance_only_included_rowsum >= 1 
                                                           & FSL.refugee_food_assistance_only_excluded_rowsum == 0 , "yes","no",NULL),
    
    I.HH_CHAR.education_level.HH = if_else(edu_highest %in% Some_primary, "Some primary", 
                                           if_else( edu_highest %in% Primary_and_above, "Primary and above",
                                                    if_else(edu_highest %in% no_formal_education, "No formal Education","",missing = NULL ))),
    
    
    I.FSL.debt_pay_food.HH = if_else(food_source.borrow == 1 | debt_reason.buy_food == 1, "yes", "no", NULL),
    
  
    I.SNFI.firewood_womenchildren.HH = if_else(wood_access %in% firewood_womenchildren, "yes", "no",NULL),
    
    
    
   
    I.FSL.without_livelihood_assets.HH = if_else(livestock =="yes" | agricultural_land =="yes" | fishing_gear =="yes"  ,"no",
                                                 if_else( livestock == ""  & agricultural_land == "" & fishing_gear == "" ,"", "yes"))
    
    
  )%>% select(starts_with("I."),"X_uuid","informed_consent")
}

# new_dataset_3 <- new_dataset %>%  select("wood_access.adult_female","wood_access.girls","wood_access.boys","I.SNFI.firewood_womenchildren.HH","wood_access.adult_male") 



# Individual to Household -------------------------------------------------


if (population == "Refugee"){ HEALTH.safety_concerns_access_health <- INDV[,c("ind_why_notreatment.safety_concerns","ind_why_notreatment.safety_concerns_night")] }#for Refugee
if (population == "Host"){ HEALTH.safety_concerns_access_health <- INDV[,c("ind_why_notreatment.safety_concerns","ind_why_notreatment.night_safety")] }#for host


indv_to_indv <- INDV %>% 
  mutate(
  HEALTH.safety_concerns_access_health_sum = rowSums(HEALTH.safety_concerns_access_health ),
  HEALTH.safety_concerns_access_health_sum_explicit_na = fct_explicit_na(as.factor(HEALTH.safety_concerns_access_health_sum), na_level = "NA"),

  #   age_under_5 = if_else(ind_age <5 , "yes","no"),
  #   age_under_18 = if_else(ind_age <18 , "yes","no"),
  #   age_over_60= if_else(ind_age >= 60 , "yes","no")
  ) %>%
  group_by(X_submission__uuid) %>%
  summarise(
    dependents = (length(ind_age[ind_age<15]) + length(ind_age[ind_age>64])),
    non_dependent = (length(ind_age[ind_age %in% 15:64])),
    I.HH_CHAR.dependency_ratio.INDVHH  = (dependents/non_dependent),
    I.HH_CHAR.dependency_ratio_classification.HH = if_else(I.HH_CHAR.dependency_ratio.INDVHH  >= 1.5, "High", "Low", NULL),
    I.HH.CP.sep_unaccom_minor_atleast_one.INDVHH = if_else(any(ind_new_hh_member == "yes"),true = "yes",false = "no",missing = "no"),
    
    I.HEALTH.indillness_atleast.INDVHH = if_else(any(ind_illness == "yes"), "yes","no"),
    I.HEALTH.indhelp_atleast.INDVHH = if_else(any(ind_help_daily == "yes" & ind_age >= 5 ),"yes","no"),
    I.HEALTH.indhelp_atleast_elderly.INDVHH = if_else(any(ind_help_daily == "yes" & ind_age >= 60),"yes","no"),
    # I.HEALTH.indillness_atleast.INDVHH = if_else(any(ind_illness == "yes"), "yes",
    #                                              if_else(all(ind_illness == ""), "","no")),
    
    I.FSL.livelihoods_atleast_one.INDVHH = if_else(any(ind_work == "yes" & ind_age >= 5),"yes","no"),
    I.FSL.livelihoods_atleast_one_child.INDVHH = if_else(any(ind_work == "yes" & ind_age < 18),"yes","no"),
    
    I.HEALTH.safety_concerns_access_health_any.INDVHH = if_else(all(HEALTH.safety_concerns_access_health_sum_explicit_na == "NA"),"NA", 
                                                                if_else(any(HEALTH.safety_concerns_access_health_sum_explicit_na == 1),"yes","no")),
    I.P.safety_issues_barriers_key_facilities.INDVHH = if_else(all(HEALTH.safety_concerns_access_health_sum_explicit_na == "NA"),"NA", 
                                                                if_else(any(HEALTH.safety_concerns_access_health_sum_explicit_na == 1),"Health_unsafe","no"))
    
    ) 
  
INDV_tO_INDV <- indv_to_indv
    

if (write == "yes"){
  if(population == "Refugee"){
hh_to_hh_path <- "recoding_output_file/hh_to_hh_refugee.csv"
indv_to_indv_path <- "recoding_output_file/indv_to_indv_refugee.csv"

write.csv(HH_to_HH, hh_to_hh_path , row.names = F)
write.csv(INDV_tO_INDV, indv_to_indv_path , row.names = F)
  }
  if (population == "Host") {
    hh_to_hh_path <- "recoding_output_file/hh_to_hh_HC.csv"
    indv_to_indv_path <- "recoding_output_file/indv_to_indv_HC.csv"
    
    write.csv(HH_to_HH, hh_to_hh_path , row.names = F)
    write.csv(INDV_tO_INDV, indv_to_indv_path , row.names = F)
  }
}
