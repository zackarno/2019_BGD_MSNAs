# asdf<-recode_HEALTH_severity_bgd2019(hh_data = ref,individual_data = ref_indiv, population = "Refugee")
# asdf %>%filter(informed_consent=="yes") %>% 
  # select(starts_with("int.health"),starts_with("sev.health"), starts_with("sev_score.health")) %>% data.frame


# t(starts_with('int.health')) %>% colnames() %>% dput()
# asdf<-recode_HEALTH_severity_bgd2019(hh_data = host,individual_data = host_indiv, population = "Host")
# individual_data=ref_indiv
# hh_data=ref
# hh_data=ref
# individual_data=ref_indiv
recode_HEALTH_severity_bgd2019<- function(hh_data,individual_data, population) {


# conditional variabables -------------------------------------------------
  if(population=="Host"){
    ind_why_notreatment_night_safety=sym("ind_why_notreatment.night_safety")
      
  }else{
    ind_why_notreatment_night_safety=sym("ind_why_notreatment.safety_concerns_night")
    hh_data<-hh_data %>%  mutate(
      int.health.comm_no_health_worker= if_else(comm_health_worker=="no",1,0),
      
    )
  }



  # common intermediate variables -------------------------------------------

  HH_rec_step1<-hh_data%>% 
    mutate(

      #HEALTH
      int.health.pay =  if_else(exp_medical != "0_bdt"| health_coping.pay_care,1,0,missing=0),
      int.health.debt= if_else(health_coping.ghealth_debt|debt_reason.pay_health_expenses ,1,0,missing=0),
      int.health.priority_needs= if_else(hh_priority_needs.access_health_facilities==1 ,1,0),
      int.health.wash_diar_under5= if_else(under5_diarrhea>0 ,1,0, missing=0),
      int.health.preg_no_anc = if_else(pregnant_woman=="yes" & pregnant_women_anc ==0 ,1,0),
      
      #COPING
      

    ) 
  # Individual intermediate aggregations ------------------------------------
  indiv<-individual_data
  indiv_to_indiv<-indiv %>% 
    mutate(
      int.health.sick_no_treatment= if_else(ind_illness=="yes" & treatment_sought=="no",1,0,missing=0),
      int.health.sick_gov_ngo_treatment=if_else(ind_illness=="yes" & 
                                                  (treatment_location.ngo_clinic|treatment_location.govt_clinic ) ,1,0,missing=0),
    
      int.health.sick_pharmacy_or_traditional=if_else(ind_illness=="yes" &
                                                        (treatment_location.pharmacy|treatment_location.healer ) ,1,0,missing=0),
      int.health.sick_private= if_else(ind_illness=="yes" & treatment_location.private_clinic  ,1,0,missing=0),
      int.health.sick_no_treatment_not_safety= if_else(ind_illness=="yes" &  treatment_sought=="no" & ind_why_notreatment.safety_concerns==0 & !!sym(ind_why_notreatment_night_safety)==0,1,0,missing=0),
      

      
      int.health.sick_no_treatment_safety= if_else(ind_illness=="yes" &  treatment_sought=="no" & (ind_why_notreatment.safety_concerns==1 | !!sym(ind_why_notreatment_night_safety)==1),1,0,missing=0),
      int.health.birthed_not_clinic= if_else(ind_birth_place %in% c("home", "midwife_house"),1,0),
      int.health.adult_requiring_assistance= if_else(ind_age>17 & ind_help_daily=="yes",1,0),
      int.health.anyone_requiring_assistance= if_else(ind_help_daily=="yes",1,0, missing=0),
      int.health.smoking_everyday= if_else(ind_age>17 & ind_smoke=="everyday",1,0,missing = 0),
      int.health.smoking_everyday_somedays= if_else(ind_age>17 & ind_smoke %in% c("everyday","some_days"),1,0,missing = 0)
      
      
    )
  
  at_least_one <- function(variable) {
    if_else(sum(variable,na.rm = TRUE)>0,1,0)
    
  }
  all_fam <- function(variable) {
    if_else(mean(variable,na.rm = TRUE)==1,1,0)
    
  }
  
  indiv_to_hh<-indiv_to_indiv %>% 
    group_by(X_submission__uuid) %>% 
    summarise(
      int.health.sick_no_treatment_atleast= at_least_one(int.health.sick_no_treatment),
      int.health.sick_gov_ngo_treatment_atleast= at_least_one(int.health.sick_gov_ngo_treatment),
      int.health.sick_pharmacy_or_traditional_atleast= at_least_one(int.health.sick_pharmacy_or_traditional),
      int.health.sick_private_atleast= at_least_one(int.health.sick_private),
      int.health.sick_no_treatment_not_safety_atleast= at_least_one(int.health.sick_no_treatment_not_safety),
      int.health.sick_no_treatment_safety_atleast= at_least_one(int.health.sick_no_treatment_safety),
      int.health.birthed_not_clinic_atleast= at_least_one(int.health.birthed_not_clinic),
      int.health.smoking_everyday_atleast=at_least_one(int.health.smoking_everyday),
      int.health.anyone_requiring_assistance_atleast=at_least_one(int.health.anyone_requiring_assistance),
      #ALL
      int.health.adult_requiring_assistance_all= all_fam(int.health.adult_requiring_assistance),
      int.health.smoking_everyday_all= all_fam(int.health.smoking_everyday),
      int.health.sick_none=if_else(mean(ind_illness=="no",na.rm = TRUE)==1,1,0)
      
      
    )
  HH_rec_step1<-HH_rec_step1 %>% left_join(indiv_to_hh, by= c("X_uuid"="X_submission__uuid"))
  
    
  
  int.health.names.to_combine<-c( "int.health.priority_needs", 
     "int.health.preg_no_anc", 
     "int.health.sick_pharmacy_or_traditional_atleast", 
     "int.health.sick_private_atleast", 
     "int.health.birthed_not_clinic_atleast", 
     "int.health.comm_no_health_worker"
  )
  
int.health.names.to_combine<-names(HH_rec_step1)[names(HH_rec_step1)%in%int.health.names.to_combine %>%which()]
number.health.names<-length(int.health.names.to_combine)

 names_for_sev3<- c("int.health.pay", "int.health.wash_diar_under5", "int.health.priority_needs", 
    "int.health.preg_no_anc", "int.health.sick_pharmacy_or_traditional_atleast", 
    "int.health.sick_private_atleast", "int.health.anyone_requiring_assistance_atleast",
    "int.health.smoking_everyday_atleast","int.health.comm_no_health_worker"
  )

 names_for_sev3<-names(HH_rec_step1)[names(HH_rec_step1)%in%names_for_sev3 %>%which()]

  HH_rec_step2<-HH_rec_step1 %>% 
    mutate(
      number_health_access= rowSums(.[int.health.names.to_combine], na.rm = TRUE),
      
      sev.health.s5= if_else(int.health.sick_no_treatment_safety_atleast==1 |
                               (int.health.adult_requiring_assistance_all==1&int.health.debt==1),1,0) ,
      
      int.sums.health= if_else(rowSums(.[c("int.health.debt","int.health.sick_no_treatment_not_safety_atleast","int.health.smoking_everyday_all")])>1,1,0),
      
      sev.health.s4=if_else(int.health.adult_requiring_assistance_all==1|
                              (int.sums.health==1)|
                              number_health_access==number.health.names,1,0),
      sev.health.s3= if_else(rowSums(.[names_for_sev3],na.rm = TRUE)>2,1,0),
      # sev.health.s2=if_else(rowSums(.[names_for_sev3],na.rm = TRUE)>0&
                              # rowSums(.[names_for_sev3],na.rm = TRUE)<3,1,0),
      sev.health.s2= int.health.birthed_not_clinic_atleast==0 |int.health.preg_no_anc==1| int.health.sick_gov_ngo_treatment_atleast==1,
      sev.health.s1=is.na(int.health.birthed_not_clinic_atleast) & is.na(int.health.preg_no_anc) & int.health.sick_none==1,
      
      sev_score.health.total=if_else(sev.health.s5==1,5,
                                   if_else(sev.health.s4==1,4,
                                           if_else(sev.health.s3==1,3,
                                                   if_else(sev.health.s2==1, 2,1)))),
      sev_score.health.total_over_3= if_else(sev_score.health.total>3,1,0)
    )
  
    


  final<-HH_rec_step2
  
}

